use glow::{Context, HasContext};
use std::rc::Rc;
use std::borrow::Cow;
use crate::access::{AccessLock, UnitAccessLock};

#[derive(Debug)]
pub(crate) struct InnerShader {
	/** The underlying context handle. */
	pub(crate) context: Rc<Context>,
	/** The access control structure. */
	pub(crate) access: UnitAccessLock,
	/** The name of the underlying shader object. */
	pub(crate) shader: <Context as HasContext>::Shader,
}
impl Drop for InnerShader {
	fn drop(&mut self) {
		unsafe {
			/* Safe because we own this buffer and `Rc` doesn't let this hop
			 * over thread boundaries.
			 *
			 * We can also trust that we won't be deleting this buffer while
			 * it's still in use due to the mutability requirement the functions
			 * that use buffers place on instances of this structure. */
			let _atom = self.access.acquire_write_guarded();
			self.context.delete_shader(self.shader)
		}
	}
}

/** Source of a shader module. */
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ShaderSource<'a> {
	/** Module represented as GLSL source code.
	 *
	 * The source will be fed to the driver for compilation and validation,
	 * which may result in small CPU usage spikes, as parsing GLSL directly is
	 * expensive. */
	Glsl(Cow<'a, str>)
}

/**
 This macro instances shader objects from a common code. It works like absolute
 black magic to me so don't ask me how this compiles and works!

 Usage is as follows:
 ```rust
 instance_shaders! {
 	pub struct ShaderA;
 	pub struct ShaderB;
 }
 ```
*/
macro_rules! instance_shaders {
	(
		$(
			$(#[$outer:meta])*
			pub struct $name:ident: $kind:expr;
		)+
	) => {$(
		$(#[$outer])*
		pub struct $name {
			pub(crate) inner: Rc<InnerShader>,
		}
		impl $name {
			/** Value of the type of this shader in OpenGL. */
			#[allow(dead_code)]
			pub(crate) const GL_TYPE: u32 = $kind;

			/** Get the raw handle of this shader. */
			#[allow(dead_code)]
			pub unsafe fn as_raw_handle(&self) -> <Context as HasContext>::Shader {
				self.inner.shader
			}
		}
		impl AccessLock for $name {
			fn write_locks(&self) -> usize { self.inner.access.write_locks() }
			fn read_locks(&self) -> usize { self.inner.access.read_locks() }
			fn acquire_write(&self) { self.inner.access.acquire_write() }
			fn release_write(&self) { self.inner.access.release_write() }
			fn acquire_read(&self)  { self.inner.access.acquire_read()  }
			fn release_read(&self)  { self.inner.access.release_read()  }
		}
		)+}
}

instance_shaders! {
	#[doc = "An instanced and successfully compiled vertex shader object."]
	#[derive(Debug)]
	pub struct VertexShader: glow::VERTEX_SHADER;
	#[doc = "An instanced and successfully compiled fragment shader object."]
	#[derive(Debug)]
	pub struct FragmentShader: glow::FRAGMENT_SHADER;
	#[doc = "An instanced and successfully compiled compute shader object."]
	#[doc = ""]
	#[doc = "# Support"]
	#[doc = "Keep in mind that compute shaders are only supported in OpenGL "]
	#[doc = "ES 3.1 and above."]
	#[derive(Debug)]
	pub struct ComputeShader: glow::COMPUTE_SHADER;
}

#[derive(Debug, thiserror::Error)]
pub enum ShaderError {
	#[error("could not create shader object: {what}")]
	CreationFailed {
		what: String
	},
	#[error("could not compile shader object: {what}")]
	CompilationFailed {
		what: String
	}
}
