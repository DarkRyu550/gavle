use glow::{HasContext, Context};
use std::rc::Rc;
use std::ops::{RangeBounds, Bound, Deref, DerefMut};
use std::cell::RefCell;
use crate::access::{AccessLock, UnitAccessLock};
use std::convert::TryFrom;
use crate::Information;

/** States the mapping of the buffer can take on. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) enum MapState {
	Mapped,
	Unmapped,
}
impl Default for MapState {
	fn default() -> Self {
		Self::Unmapped
	}
}

/** Inner shared structure of the buffer. */
#[derive(Debug)]
pub struct InnerBuffer {
	/** Shared handle to the underlying context. */
	pub(crate) context: Rc<Context>,
	/** Shared information on the context. */
	pub(crate) information: Rc<Information>,
	/** Shared OpenGL pipeline lock. */
	pub(crate) pipeline: Rc<RefCell<()>>,
	/** Name of the buffer, used to identify it to the implementation. */
	pub(crate) buffer: <Context as HasContext>::Buffer,
	/** Access control structure. */
	pub(crate) access: UnitAccessLock,
	/** State of the buffer mapping. */
	pub(crate) map: RefCell<MapState>,
	/** Length of the buffer, in bytes. */
	pub(crate) len: u32,
}
impl Drop for InnerBuffer {
	fn drop(&mut self) {
		unsafe {
			/* Safe because we own this buffer and `Rc` doesn't let this hop
			 * over thread boundaries.
			 *
			 * We can also trust that we won't be deleting this buffer while
			 * it's still in use due to the mutability requirement the functions
			 * that use buffers place on instances of this structure. */
			let _atomic = self.access.acquire_write_guarded();

			self.context.delete_buffer(self.buffer)
		}
	}
}
impl AccessLock for InnerBuffer {
	fn write_locks(&self) -> usize {
		self.access.write_locks()
	}
	fn read_locks(&self) -> usize {
		self.access.read_locks()
	}
	fn acquire_write(&self) {
		self.access.acquire_write()
	}
	fn release_write(&self) {
		self.access.release_write()
	}
	fn acquire_read(&self) {
		self.access.acquire_read()
	}
	fn release_read(&self) {
		self.access.release_read()
	}
}

/** The trait that describes functionality shared between all buffer types. */
pub trait Buffer {
	/** Length of this buffer, in bytes. */
	fn len(&self) -> u32;

	/** Get the raw handle of this buffer. */
	unsafe fn as_raw_handle(&self) -> <Context as HasContext>::Buffer;

	/** Get a range of this buffer. */
	fn slice<R>(&self, range: R) -> BufferSlice
		where R: RangeBounds<u32>;
}

/** This macro instances buffers from a common buffer code given the buffer
 * type they belong to in OpenGL. */
macro_rules! instance_buffers {
	($(
		$(#[$outer:meta])*
		pub struct $name:ident: $target:expr;
	)+) => {$(
		$(#[$outer])*
		pub struct $name {
			/** Inner access-controlled version of this structure. */
			pub(crate) inner: Rc<InnerBuffer>,
		}
		impl $name {
			/** Enum value of the binding slot that should be used for this type
			 * of buffer. */
			pub(crate) const GL_BIND: u32 = $target;
		}
		impl Buffer for $name {
			/** Length of this buffer, in bytes. */
			fn len(&self) -> u32 {
				self.inner.len
			}

			/** Get the raw handle of this buffer. */
			unsafe fn as_raw_handle(&self) -> <Context as HasContext>::Buffer {
				self.inner.buffer
			}

			/** Get a range of this buffer. */
			fn slice<R>(&self, range: R) -> BufferSlice
				where R: RangeBounds<u32> {

				let offset = match range.start_bound() {
					Bound::Unbounded => 0,
					Bound::Excluded(val) => val.checked_add(1)
						.expect("lower range bound value would overflow u32 range"),
					Bound::Included(val) => *val
				};

				let length = match range.end_bound() {
					Bound::Unbounded => self.len() - offset,
					Bound::Excluded(val) => {
						let val = *val;

						if val > self.len() {
							panic!("upper range bound {} is greater than the \
								length {} of the buffer",
								val, self.len())
						}
						if offset > val {
							panic!("lower range bound {} is greater than upper \
								range bound {}",
								offset, val)
						}

						val.checked_sub(offset).unwrap()
					},
					Bound::Included(val) => {
						let val = *val;

						if val >= self.len() {
							panic!("upper range bound ={} is greater than the \
								length {} of the buffer",
								val, self.len())
						}
						if offset > val {
							panic!("lower range bound {} is greater than upper \
								range bound {}",
								offset, val)
						}

						val.checked_sub(offset).unwrap()
							.checked_add(1)
							.expect("upper range bound overflows u32 range")
					},
				};

				BufferSlice {
					buffer: &self.inner,
					target: $target,
					offset,
					length
				}
			}
		}
		impl AccessLock for $name {
			fn write_locks(&self) -> usize {
				self.inner.write_locks()
				}
			fn read_locks(&self) -> usize {
				self.inner.read_locks()
			}
			fn acquire_write(&self) {
				self.inner.acquire_write()
			}
			fn release_write(&self) {
				self.inner.release_write()
			}
			fn acquire_read(&self) {
				self.inner.acquire_read()
			}
			fn release_read(&self) {
				self.inner.release_read()
			}
		}
	)+}
}
instance_buffers! {
	#[derive(Debug)]
	#[doc = "A buffer type that may be used for vertex storage."]
	pub struct VertexBuffer: glow::ARRAY_BUFFER;
	#[derive(Debug)]
	#[doc = "A buffer type that may be used for index storage."]
	pub struct IndexBuffer: glow::ELEMENT_ARRAY_BUFFER;
	#[derive(Debug)]
	#[doc = "A buffer that that may be used for uniform block storage."]
	pub struct UniformBuffer: glow::UNIFORM_BUFFER;
}

/** Usage classes for buffers. This helps optimize the usage of the buffers. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum BufferProfile {
	/** This buffer will be used for upload of data from the host to the device.
	 * It will seldom be updated but it will read from many times. */
	StaticUpload,
	/** This buffer will be used for upload of data from the host to the device.
	 * It will be both updated and read from frequently. */
	DynamicUpload,
	/** This buffer will be used by operations that take data on the device and
	 * save it on the device. It will seldom e updated but it will be read from
	 * many times. */
	StaticDevice,
	/** This buffer will be used by operations that take data on the device and
	 * save it on the device. It will be both updated and read from frequently.
	 */
	DynamicDevice,
}
impl BufferProfile {
	/** Get the OpenGL enum value for the current variant. */
	pub(crate) fn as_opengl(&self) -> u32 {
		match self {
			Self::StaticUpload  => glow::STATIC_DRAW,
			Self::DynamicUpload => glow::DYNAMIC_DRAW,
			Self::StaticDevice  => glow::STATIC_COPY,
			Self::DynamicDevice => glow::DYNAMIC_COPY,
		}
	}
}

/** Descriptor for the creation of a buffer. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BufferDescriptor {
	/** Length of the desired buffer, in bytes. */
	pub size: u32,
	/** Usage profile of the desired buffer.
	 *
	 * Keep in mind that, as in OpenGL, this are hints rather than requirements.
	 * Uploading to a device-local buffer is still allowed, albeit it might be
	 * slow compared to doing this to an upload buffer. */
	pub profile: BufferProfile,
}

/** Read-only slice over the range of a buffer. */
#[derive(Debug, Copy, Clone)]
pub struct BufferSlice<'a> {
	/** Underlying buffer. */
	buffer: &'a InnerBuffer,
	/** Buffer bind target. */
	target: u32,
	/** Beginning offset of the slice, inclusive. */
	offset: u32,
	/** Length of the slice. */
	length: u32,
}
impl<'a> BufferSlice<'a> {
	/** Tries to map this buffer read-only and fails if the buffer has already
	 * been mapped.
	 *
	 * # Panic
	 * This function will panic if the buffer can't be locked immutably or if
	 * the pipeline can't be locked mutably. */
	pub fn try_map(&self) -> Result<BufferView, BufferRemap> {
		let buffer_lock = self.buffer.acquire_read_guarded();
		let pipeline_lock = self.buffer.pipeline.borrow_mut();

		let mut map = self.buffer.map.borrow_mut();
		*map = match *map {
			MapState::Mapped => return Err(BufferRemap),
			MapState::Unmapped => MapState::Mapped,
		};

		let len = self.length;
		let gl = self.buffer.context.as_ref();
		let data = if len == 0 {
			/* Empty slice. */
			BufferData::Empty { nothing: [] }
		} else if self.buffer.information.capabilities.buffer_mapping {
			let ptr = unsafe {
				gl.bind_buffer(self.target, Some(self.buffer.buffer));
				let ptr = gl.map_buffer_range(
					self.target,
					i32::try_from(self.offset).unwrap(),
					i32::try_from(len).unwrap(),
					{
						let access = glow::MAP_READ_BIT | glow::MAP_WRITE_BIT;
						let access = access | glow::MAP_FLUSH_EXPLICIT_BIT;
						access
					});

				/* Check for mapping errors. */
				if ptr.is_null() {
					panic!("opengl failed to map the buffer {:?} to memory: \
					0x{:08x}", self.buffer.buffer, gl.get_error());
				}

				ptr
			};

			BufferData::Mapped {
				data: ptr,
				len: usize::try_from(len).unwrap(),
				mutated: false
			}
		} else {
			let mut buf = vec![0; usize::try_from(len).unwrap()];
			unsafe {
				gl.get_buffer_sub_data(
					self.target,
					i32::try_from(self.offset).unwrap(),
					&mut buf);
			}

			BufferData::Mirrored {
				storage: buf.into_boxed_slice(),
				mutated: false
			}
		};

		Ok(BufferView {
			slice: *self,
			data,
			_pipeline_lock: pipeline_lock,
			_buffer_lock: buffer_lock
		})
	}

	/** Tries to map this buffer read-write and fails if the buffer has already
	 * been mapped.
	 *
	 * When calling this function, the user may choose how the initial contents
	 * of the mapped writable buffer will be available. See the
	 * [documentation of the load operations] for details on how the choice
	 * affects the initial state of the buffer and what the performance
	 * characteristics of each operation are.
	 *
	 * # Panic
	 * This function will panic if the buffer can't be locked mutable or if the
	 * pipeline can't be locked mutably.
	 *
	 * [documentation of the load operations]: BufferLoadOp
	 */
	pub fn try_map_mut(
		&self,
		op: BufferLoadOp) -> Result<BufferViewMut, BufferRemap> {

		let buffer_lock = self.buffer.acquire_write_guarded();
		let pipeline_lock = self.buffer.pipeline.borrow_mut();

		let mut map = self.buffer.map.borrow_mut();
		*map = match *map {
			MapState::Mapped => return Err(BufferRemap),
			MapState::Unmapped => MapState::Mapped,
		};

		let len = self.length;
		let gl = self.buffer.context.as_ref();
		let data = if len == 0 {
			/* This is an empty buffer. */
			BufferData::Empty { nothing: [] }
		} else if self.buffer.information.capabilities.buffer_mapping {
			let ptr = unsafe {
				gl.bind_buffer(self.target, Some(self.buffer.buffer));
				let ptr = gl.map_buffer_range(
					self.target,
					i32::try_from(self.offset).unwrap(),
					i32::try_from(len).unwrap(),
					{
						let access = glow::MAP_READ_BIT | glow::MAP_WRITE_BIT;
						let access = access | glow::MAP_FLUSH_EXPLICIT_BIT;
						access
					});

				/* Check for mapping errors. */
				if ptr.is_null() {
					panic!("opengl failed to map the buffer {:?} to memory: \
					0x{:08x}", self.buffer.buffer, gl.get_error());
				}

				ptr
			};
			match op {
				BufferLoadOp::Clear(val) => unsafe {
					/* Clear the mapped range. */
					std::ptr::write_bytes(
						ptr,
						val,
						usize::try_from(len).unwrap())
				},
				_ => {
					/* For mapped buffers, Load is the same as doing nothing,
					 * and DontCare is by definition, the same as Load. */
				}
			}
			BufferData::Mapped {
				data: ptr,
				len: usize::try_from(len).unwrap(),
				mutated: false
			}
		} else {
			unsafe {
				/* All operations after this rely on the buffer being bound,
				 * regardless of whether we load it now or not. */
				gl.bind_buffer(self.target, Some(self.buffer.buffer));
			}

			let buf = match op {
				BufferLoadOp::Clear(val) =>
					/* Create an empty buffer with the given initial value. */
					vec![val; usize::try_from(len).unwrap()],
				BufferLoadOp::DontCare =>
					/* Create an empty buffer that is zero-initialized. */
					vec![0; usize::try_from(len).unwrap()],
				BufferLoadOp::Load => unsafe {
					/* Download the buffer data from the device. */
					let mut buf = vec![0; usize::try_from(len).unwrap()];
					gl.get_buffer_sub_data(
						self.target,
						i32::try_from(self.offset).unwrap(),
						&mut buf);
					buf
				}
			};
			BufferData::Mirrored {
				storage: buf.into_boxed_slice(),
				mutated: false
			}
		};

		Ok(BufferViewMut {
			slice: *self,
			data,
			_pipeline_lock: pipeline_lock,
			_buffer_lock: buffer_lock
		})
	}
}

/** The operations that can be used to initialize the memory contents in the
 * host-visible part of a mutable mapped buffer slice. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BufferLoadOp {
	/** The initial contents of the mapped buffer range will be undefined.
	 *
	 * # Performance
	 * This is the fastest option, as the implementation is free to select
	 * the path of least resistance given the characteristics and capabilities
	 * of the underlying implementation. The downside to this option, however,
	 * is that the whole range must be written to on every mapping in order to
	 * keep the results defined in every platform. */
	DontCare,
	/** The initial contents of the mapped buffer will be cleared to the given
	 * value.
	 *
	 * # Performance
	 * This is a fast option, albeit less so than [`Self::DontCare`] in some
	 * systems (namely, those that support direct memory mapping). However, even
	 * in the systems in which this option performs a little slower, this is
	 * still the fastest option that leaves the contents of the buffer the same
	 * on every platform. */
	Clear(u8),
	/** The initial contents of the mapped buffer will mirror its contents in
	 * device memory. This behaves exactly like a full read-write memory map of
	 * the buffer.
	 *
	 * # Performance
	 * On systems that support direct memory mapping, this option is as fast as
	 * any of the others. However, on systems that don't, the library has to
	 * emulate the behavior of the full memory map in software, by downloading
	 * the contents of the buffer on every map, which is rather slow.
	 *
	 * It is preferable that users avoid this operation as much as possible, by
	 * either mapping only the parts they intend to change with more lenient
	 * operations, or by mapping larger buffers at once.
	 */
	Load,
}

/** Error type for when buffer map requests are made to buffers which are
 * already mapped. */
#[derive(Debug)]
pub struct BufferRemap;
impl std::fmt::Display for BufferRemap {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "tried to map a buffer which is already mapped")
	}
}
impl std::error::Error for BufferRemap {}

#[derive(Debug, thiserror::Error)]
pub enum BufferError {
	#[error("could not create buffer object: {what}")]
	CreationFailed {
		what: String
	},
}

/** Depending on which implementation we're running, buffers may or may not be
 * memory-mappable. This enum implements a common structure for the logic of
 * both implementations that allow memory mapping and those that forbid it. */
enum BufferData {
	/** The buffer is mapped to host memory. */
	Mapped {
		/** The pointer to the data. */
		data: *mut u8,
		/** The length of the mapped region. */
		len: usize,
		/** Whether the storage has been accessed mutably. */
		mutated: bool,
	},
	/** The buffer is not mapped to the host memory.
	 *
	 * It is download into host memory and then uploaded back onto device memory
	 * when the changes made to it are complete. */
	Mirrored {
		/** The backing storage in host memory. */
		storage: Box<[u8]>,
		/** Whether the storage has been accessed mutably. */
		mutated: bool,
	},
	/** The buffer has a length of zero and isn't backed by anything. */
	Empty {
		/** Ah yes, the _nothing array_.
		 *
		 * We need a valid slice for empty buffers, just as we would need for
		 * any of the other valid buffer storage types. This is actually a
		 * pretty good way to do it, despite how silly it looks. */
		nothing: [u8; 0]
	},
	/** This reference has been terminated. */
	Terminated
}
impl BufferData {
	/** Flushes and unmaps the buffer. */
	fn finish(&mut self, slice: BufferSlice<'_>) {
		let this = std::mem::replace(self, Self::Terminated);

		match this {
			Self::Mapped { len, mutated, .. } => unsafe {
				let gl = slice.buffer.context.as_ref();
				if mutated {
					gl.flush_mapped_buffer_range(
						slice.target,
						i32::try_from(slice.offset).unwrap(),
						i32::try_from(len).unwrap());
				}
				gl.unmap_buffer(slice.target);
			},
			Self::Mirrored { storage, mutated } => unsafe {
				let gl = slice.buffer.context.as_ref();
				if mutated {
					gl.buffer_sub_data_u8_slice(
						slice.target,
						i32::try_from(slice.offset).unwrap(),
						&*storage)
				}
			},
			Self::Terminated | Self::Empty { .. } => { /* No-op. */ }
		}
	}
}
impl AsRef<[u8]> for BufferData {
	fn as_ref(&self) -> &[u8] {
		match self {
			Self::Mapped { len, data, .. } => unsafe {
				std::slice::from_raw_parts(*data, *len)
			},
			Self::Mirrored { storage, .. } => &*storage,
			Self::Empty { nothing } => &nothing[..],
			Self::Terminated =>
				panic!("called as_ref() on a terminated BufferData"),
		}
	}
}
impl AsMut<[u8]> for BufferData {
	fn as_mut(&mut self) -> &mut [u8] {
		match self {
			Self::Mapped { len, data, mutated } => {
				*mutated = true;
				unsafe {
					std::slice::from_raw_parts_mut(*data, *len)
				}
			},
			Self::Mirrored { storage, mutated } => {
				*mutated = true;
				&mut *storage
			},
			Self::Empty { nothing } => &mut nothing[..],
			Self::Terminated =>
				panic!("called as_mut() on a terminated BufferData")
		}
	}
}
impl Deref for BufferData {
	type Target = [u8];
	fn deref(&self) -> &Self::Target {
		self.as_ref()
	}
}
impl DerefMut for BufferData {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.as_mut()
	}
}

/** Immutable memory mapped range of a buffer. */
pub struct BufferView<'a> {
	/** The slice of the buffer this  */
	slice: BufferSlice<'a>,
	/** The mapped data in this buffer. */
	data: BufferData,
	/** The lock on the pipeline. */
	_pipeline_lock: std::cell::RefMut<'a, ()>,
	/** The lock on the buffer. */
	_buffer_lock: crate::access::ReadGuard<'a>,
}
impl<'a> AsRef<[u8]> for BufferView<'a> {
	fn as_ref(&self) -> &[u8] {
		self.data.as_ref()
	}
}
impl<'a> Deref for BufferView<'a> {
	type Target = [u8];
	fn deref(&self) -> &Self::Target {
		self.as_ref()
	}
}
impl<'a> Drop for BufferView<'a> {
	fn drop(&mut self) {
		self.data.finish(self.slice);

		let gl = self.slice.buffer.context.as_ref();
		unsafe {
			gl.bind_buffer(self.slice.target, None);
		}

		let mut map = self.slice.buffer.map.borrow_mut();
		*map = MapState::Unmapped;
	}
}

/** Mutable memory mapped range of a buffer. */
pub struct BufferViewMut<'a> {
	/** The slice of the buffer this  */
	slice: BufferSlice<'a>,
	/** The mapped data in this buffer. */
	data: BufferData,
	/** The lock on the pipeline. */
	_pipeline_lock: std::cell::RefMut<'a, ()>,
	/** The lock on the buffer. */
	_buffer_lock: crate::access::WriteGuard<'a>,
}
impl<'a> AsRef<[u8]> for BufferViewMut<'a> {
	fn as_ref(&self) -> &[u8] {
		self.data.as_ref()
	}
}
impl<'a> AsMut<[u8]> for BufferViewMut<'a> {
	fn as_mut(&mut self) -> &mut [u8] {
		self.data.as_mut()
	}
}
impl<'a> Deref for BufferViewMut<'a> {
	type Target = [u8];
	fn deref(&self) -> &Self::Target {
		self.as_ref()
	}
}
impl<'a> DerefMut for BufferViewMut<'a> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.as_mut()
	}
}
impl<'a> Drop for BufferViewMut<'a> {
	fn drop(&mut self) {
		self.data.finish(self.slice);

		let gl = self.slice.buffer.context.as_ref();
		unsafe {
			gl.bind_buffer(self.slice.target, None);
		}

		let mut map = self.slice.buffer.map.borrow_mut();
		*map = MapState::Unmapped;
	}
}
