use smallvec::SmallVec;
use crate::texture::Texture;
use crate::access::{UnitAccessLock, AccessLock};
use glow::{HasContext, Context};
use std::rc::Rc;

/** The backing structure used for custom framebuffers. */
#[derive(Debug)]
pub(crate) struct InnerFramebuffer {
	/** Shared context. */
	pub(crate) context: Rc<Context>,
	/** Access control lock. */
	pub(crate) access: UnitAccessLock,
	/** Color attachments. */
	pub(crate) color_attachments: SmallVec<[Texture; 32]>,
	/** Depth stencil attachment. */
	pub(crate) depth_stencil: Option<Texture>,
	/** Underlying named framebuffer object. */
	pub(crate) framebuffer: <Context as HasContext>::Framebuffer,
	/** The operation to perform on the color attachment when it is loaded. */
	pub(crate) color_load_op: LoadOp<Color>,
	/** The operation to perform on the depth attachment when it is loaded. */
	pub(crate) depth_load_op: LoadOp<f32>,
	/** The operation to perform on the stencil attachment when it is loaded. */
	pub(crate) stencil_load_op: LoadOp<u8>,
}
impl AccessLock for InnerFramebuffer {
	fn write_locks(&self) -> usize {
		let a = self.access.write_locks();
		for texture in &self.color_attachments { assert_eq!(texture.write_locks(), a) }
		for texture in &self.depth_stencil     { assert_eq!(texture.write_locks(), a) }

		a
	}
	fn read_locks(&self) -> usize {
		let a = self.access.read_locks();
		for texture in &self.color_attachments { assert_eq!(texture.read_locks(), a) }
		for texture in &self.depth_stencil     { assert_eq!(texture.read_locks(), a) }

		a
	}
	fn acquire_write(&self) {
		self.access.acquire_write();
		for texture in &self.color_attachments { texture.acquire_write(); }
		for texture in &self.depth_stencil     { texture.acquire_write(); }
	}
	fn release_write(&self) {
		self.access.release_write();
		for texture in &self.color_attachments { texture.release_write(); }
		for texture in &self.depth_stencil     { texture.release_write(); }
	}
	fn acquire_read(&self) {
		self.access.acquire_read();
		for texture in &self.color_attachments { texture.acquire_read(); }
		for texture in &self.depth_stencil     { texture.acquire_read(); }
	}
	fn release_read(&self) {
		self.access.release_read();
		for texture in &self.color_attachments { texture.release_read(); }
		for texture in &self.depth_stencil     { texture.release_read(); }
	}
}
impl Drop for InnerFramebuffer {
	fn drop(&mut self) {
		unsafe {
			let _atom = self.access.acquire_write_guarded();
			self.context.delete_framebuffer(self.framebuffer);
		}
	}
}

/** This type hides the fact that the framebuffer is an enum. Clients shouldn't
 * know this. */
#[derive(Debug)]
pub(crate) enum FramebufferVariants {
	/** The default framebuffer, which renders to the display. Equivalent to a
	 * framebuffer created in Vulkan by using an image from the swapchain, plus
	 * an extra depth and stencil attachment.
	 *
	 * Because OpenGL does not expose any details on the attachments of the
	 * buffer it uses to render to the screen, we need to create a custom
	 * variant that basically says "this will end up on the screen, trust me".
	 */
	Default {
		/** The operation to perform on the color attachment when it is loaded. */
		color_load_op: LoadOp<Color>,
		/** The operation to perform on the depth attachment when it is loaded. */
		depth_load_op: LoadOp<f32>,
		/** The operation to perform on the stencil attachment when it is loaded. */
		stencil_load_op: LoadOp<u8>,
	},
	/** This is a real framebuffer object. Because of the nature of the API, all
	 * custom framebuffer objects are used exclusively for off-screen rendering.
	 *
	 * This is also the only variant that is access controlled.
	 */
	Custom {
		inner: Rc<InnerFramebuffer>
	}
}

#[derive(Debug)]
pub struct Framebuffer {
	/** The actual framebuffer variants structure. */
	pub(crate) variants: FramebufferVariants
}
impl Framebuffer {
	/** Bind this framebuffer for use in OpenGL.
	 *
	 * This function does not perform any load or clear operations. Assuming
	 * that those have already been done. */
	pub(crate) unsafe fn bind(&self, gl: &Context) {
		match &self.variants {
			FramebufferVariants::Default { .. } => {
				gl.bind_framebuffer(glow::FRAMEBUFFER, None);
			},
			FramebufferVariants::Custom { inner } => {
				gl.bind_framebuffer(glow::FRAMEBUFFER, Some(inner.framebuffer));
			}
		};
	}

	/** Bind this framebuffer for use in OpenGL.
	 *
	 * This function also performs any required clear operations in all of the
	 * attachments of the framebuffer, if needed. */
	pub(crate) unsafe fn bind_and_load(&self, gl: &Context) {
		let (color, depth, stencil) = match &self.variants {
			FramebufferVariants::Default {
				color_load_op,
				depth_load_op,
				stencil_load_op } => {

				gl.bind_framebuffer(glow::FRAMEBUFFER, None);
				(*color_load_op, *depth_load_op, *stencil_load_op)
			},
			FramebufferVariants::Custom { inner } => {
				gl.bind_framebuffer(glow::FRAMEBUFFER, Some(inner.framebuffer));

				(
					inner.color_load_op,
					inner.depth_load_op,
					inner.stencil_load_op
				)
			}
		};

		let mut mask = 0;
		if let LoadOp::Clear(color) = color {
			gl.clear_color(color.red, color.green, color.blue, color.alpha);
			mask |= glow::COLOR_BUFFER_BIT;
		}
		if let LoadOp::Clear(depth) = depth {
			gl.clear_depth_f32(depth);
			mask |= glow::DEPTH_BUFFER_BIT;
		}
		if let LoadOp::Clear(stencil) = stencil {
			gl.clear_stencil(i32::from(stencil));
			mask |= glow::STENCIL_BUFFER_BIT;
		}
		if mask != 0 {
			gl.clear(mask);
		}
	}
}
impl AccessLock for Framebuffer {
	fn write_locks(&self) -> usize {
		if let FramebufferVariants::Custom { inner } = &self.variants {
			inner.write_locks()
		} else { 0 }
	}
	fn read_locks(&self) -> usize {
		if let FramebufferVariants::Custom { inner } = &self.variants {
			inner.read_locks()
		} else { 0 }
	}

	fn acquire_write(&self) {
		if let FramebufferVariants::Custom { inner } = &self.variants {
			inner.acquire_write()
		}
	}
	fn release_write(&self) {
		if let FramebufferVariants::Custom { inner } = &self.variants {
			inner.release_write()
		}
	}
	fn acquire_read(&self) {
		if let FramebufferVariants::Custom { inner } = &self.variants {
			inner.acquire_read()
		}
	}
	fn release_read(&self) {
		if let FramebufferVariants::Custom { inner } = &self.variants {
			inner.release_read()
		}
	}
}

/** Descriptor used for the default framebuffer. */
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct DefaultFramebufferDescriptor {
	/** The operation to perform on the color attachment when it is loaded. */
	pub color_load_op: LoadOp<Color>,
	/** The operation to perform on the depth attachment when it is loaded. */
	pub depth_load_op: LoadOp<f32>,
	/** The operation to perform on the stencil attachment when it is loaded. */
	pub stencil_load_op: LoadOp<u8>,
}

/** Descriptor for a new, custom framebuffer. */
#[derive(Debug, Copy, Clone)]
pub struct FramebufferDescriptor<'a> {
	/** The color attachments of the render pass. */
	pub color_attachments: &'a [FramebufferColorAttachmentDescriptor<'a>],
	/** The depth and stencil attachment of the render pass, if any. */
	pub depth_stencil_attachment: Option<FramebufferDepthStencilAttachmentDescriptor<'a>>
}

/** Descriptor for a color attachment in a custom framebuffer. */
#[derive(Debug, Copy, Clone)]
pub struct FramebufferColorAttachmentDescriptor<'a> {
	/** Texture that will be used as the color attachment. */
	pub attachment: &'a Texture,
	/** The operation to perform on the attachment when it is loaded. */
	pub load_op: LoadOp<Color>
}

/** Descriptor for a depth-stencil attachment in a custom framebuffer. */
#[derive(Debug, Copy, Clone)]
pub struct FramebufferDepthStencilAttachmentDescriptor<'a> {
	/** Texture that will be used as the depth and stencil attachment. */
	pub attachment: &'a Texture,
	/** The operation to perform on the depth attachment when it is loaded. */
	pub depth_load_op: LoadOp<f32>,
	/** The operation to perform on the stencil attachment when it is loaded. */
	pub stencil_load_op: LoadOp<u8>,
}

/** Operation to be performed on the loading of an attachment. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum LoadOp<T> {
	/** Clear the attachment with the given value. */
	Clear(T),
	/** Keep the values in the attachment as they are. */
	Load,
}

/** RGBA color value with components expressed as 32-bit floating point
 * numbers. */
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Color {
	/** Amount of the red component, in the `[0; 1]` range. */
	pub red: f32,
	/** Amount of the green component, in the `[0; 1]` range. */
	pub green: f32,
	/** Amount of the blue component, in the `[0; 1]` range. */
	pub blue: f32,
	/** Amount of the alpha component, in the `[0; 1]` range. */
	pub alpha: f32,
}

#[derive(Debug, thiserror::Error)]
pub enum FramebufferError {
	#[error("could not create framebuffer object: {what}")]
	CreationError {
		what: String
	},
}
