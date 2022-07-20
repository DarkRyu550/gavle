use std::cell::Cell;
use std::num::NonZeroUsize;

/** This trait specifies the interface for the access lock state machine.
 *
 * # Design Rationale
 * A state machine is a very direct way to model this sort of access control
 * interface in a very generic way that also composes fairly well. We gives up
 * scope-based automatic resource management, but, in exchange, we gain a few
 * key advantages.
 *
 * First of all, we get rid of hacks like having every composition function
 * return some form of custom `impl Trait` structure. Secondly, this simplifies
 * the locking strategy for types which have collections of arbitrary numbers of
 * objects, which would otherwise require an extra allocation for every lock
 * operation.
 *
 * # Downsides and Limitations
 * The obvious downside to this strategy is that the safety and predictability
 * of this trait depends heavily on both correct implementation and correct use.
 *
 * One very important limitation is the lack of atomicity of locking operations.
 * In structures which compose these locks, by default, the failure to lock one
 * substructure won't prevent previously successful locks in the same invocation
 * from going into effect.
 *
 * The only proper way to deal with these failures, therefore, is to panic right
 * away, rather than trying to raise an error. Seeing as mutable resource
 * aliasing should be considered a grave programming error, anyway, panicking
 * here shouldn't end up being that big of an issue, and be a fair tradeoff for
 * what we gain.
 */
pub trait AccessLock {
	/** The current number of active read locks on this object. */
	fn write_locks(&self) -> usize;

	/** The current number of active read locks on this object. */
	fn read_locks(&self) -> usize;

	/** Signal that the caller wants to perform writing operations to the unsafe
	 * resource or set of unsafe resources guarded by this lock.
	 *
	 * Calling this function a second time right after another call to it
	 * succeeds is a no-op.
	 *
	 * # Panic
	 * This function will panic if it this resource is currently already
	 * aliased, either mutably or immutably.
	 */
	fn acquire_write(&self);

	/** Signal that the caller will no longer perform writing operations to the
	 * unsafe resource.
	 *
	 * # Panic
	 * This function will panic if the resource isn't currently in the writable
	 * state. */
	fn release_write(&self);

	/** Signal that the caller wants to perform reading operations to the unsafe
	 * resource or set of unsafe resources guarded by this lock.
	 *
	 * Calling this function multiple times results in the number of readers
	 * being increased, once per call. As a result, every call to this function
	 * must be paired with a call to [its corresponding release function].
	 *
	 * # Panic
	 * This function will panic if the resource is currently mutably aliased.
	 *
	 * [its corresponding release function]: release_read
	 */
	fn acquire_read(&self);

	/** Signal that the caller will no longer perform reading operations to the
	 * unsafe resource.
	 *
	 * # Panic
	 * This function will panic if the resource is not currently read-only
	 * aliased by any clients. */
	fn release_read(&self);

	/** Guarded version of the [`acquire_write`] function, for automatic
	 * release of the lease on exit from scope. */
	fn acquire_write_guarded(&self) -> WriteGuard
		where Self: Sized {

		self.acquire_write();
		WriteGuard(self)
	}

	/** Guarded version of the [`acquire_read`] function, for automatic
	 * release of the lease on exit from scope. */
	fn acquire_read_guarded(&self) -> ReadGuard
		where Self: Sized {

		self.acquire_read();
		ReadGuard(self)
	}
}

/** Guard for the write acquisition. Automatically releases the write lock when
 * it gets dropped from scope. */
pub struct WriteGuard<'a>(&'a dyn AccessLock);
impl<'a> Drop for WriteGuard<'a> {
	fn drop(&mut self) {
		self.0.release_write();
	}
}

/** Guard for the read acquisition. Automatically releases the read lock when
 * it gets dropped from scope. */
pub struct ReadGuard<'a>(&'a dyn AccessLock);
impl<'a> Drop for ReadGuard<'a> {
	fn drop(&mut self) {
		self.0.release_read();
	}
}

/** Implementation of [the access lock] for a single unitary resource. It is
 * indented to be the single building block atop which more complex access locks
 * are built.
 *
 * This structure should be used directly as a backing implementation of
 * [`AccessLock`] for structures that only need to guard a single resource.
 * Especially since rolling a custom access lock implementation can lead to much
 * grief given the very bare-bones state machine design of the interface.
 *
 * [the access lock]: AccessLock
 */
#[derive(Debug, Eq, PartialEq)]
pub struct UnitAccessLock {
	/** Current state of the lock. */
	lock: Cell<AccessLockState>
}
impl UnitAccessLock {
	/** Creates a new unit access lock in the idle state. */
	pub fn new() -> Self {
		Self {
			lock: Cell::new(AccessLockState::Idle)
		}
	}
}
impl Default for UnitAccessLock {
	fn default() -> Self {
		Self::new()
	}
}
impl AccessLock for UnitAccessLock {
	fn write_locks(&self) -> usize {
		match self.lock.get() {
			AccessLockState::Write => 1,
			_ => 0
		}
	}
	fn read_locks(&self) -> usize {
		match self.lock.get() {
			AccessLockState::Read { clients } => clients.get(),
			_ => 0
		}
	}
	fn acquire_write(&self) {
		let state = self.lock.get();
		let next = match state {
			AccessLockState::Idle => AccessLockState::Write,
			AccessLockState::Write => AccessLockState::Write,
			AccessLockState::Read { clients } =>
				panic!("tried to acquire a write lease to a resource that is \
					currently being read from by {} clients", clients),
		};

		let old = self.lock.replace(next);
		if old != state {
			panic!("inconsistency between read and write of cell!")
		}
	}

	fn release_write(&self) {
		let state = self.lock.get();
		let next = match state {
			AccessLockState::Write => AccessLockState::Idle,
			_ => panic!("tried to relinquish a write lease to a resource that \
				is currently not being written to")
		};

		let old = self.lock.replace(next);
		if old != state {
			panic!("inconsistency between read and write of cell!")
		}
	}

	fn acquire_read(&self) {
		let state = self.lock.get();
		let next = match state {
			AccessLockState::Idle => AccessLockState::Read { clients: NonZeroUsize::new(1).unwrap() },
			AccessLockState::Read { clients } =>
				AccessLockState::Read {
					clients: NonZeroUsize::new(clients.get() + 1).unwrap()
				},
			AccessLockState::Write =>
				panic!("tried to acquire a read lease to a resource that is \
					currently being written to"),
		};

		let old = self.lock.replace(next);
		if old != state {
			panic!("inconsistency between read and write of cell!")
		}
	}

	fn release_read(&self) {
		let state = self.lock.get();
		let next = match state {
			AccessLockState::Read { clients }
				if clients.get() == 1 => AccessLockState::Idle,
			AccessLockState::Read { clients } =>
				AccessLockState::Read {
					clients: NonZeroUsize::new(clients.get() - 1).unwrap()
				},
			_ =>
				panic!("tried to relinquish a read lease to a resource that is \
					currently not being read from"),
		};

		let old = self.lock.replace(next);
		if old != state {
			panic!("inconsistency between read and write of cell!")
		}
	}
}

/** States of the unit access lock. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum AccessLockState {
	/** No readers or writers. */
	Idle,
	/** The lock is reserved for the exclusive use of a single client with
	 * read and write access. */
	Write,
	/** The lock is reserved for read-only access by one or many clients. */
	Read {
		clients: NonZeroUsize,
	}
}