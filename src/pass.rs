use crate::pipeline::RenderPipeline;
use std::rc::Rc;
use glow::{Context, HasContext};
use crate::buffer::{VertexBuffer, IndexBuffer};
use std::ops::Range;
use crate::binding::UniformGroup;
use crate::access::AccessLock;
use crate::framebuffer::Framebuffer;
use std::convert::TryFrom;
use crate::{Information, Color, Buffer};

pub struct RenderPass<'a> {
	/** Shared graphics context. */
	pub(crate) context: Rc<Context>,
	/** Shared context information. */
	pub(crate) information: Rc<Information>,
	/** Global graphics state lock. */
	pub(crate) _lock: std::cell::RefMut<'a, ()>,
	/** Whether the pipeline has already been set up for calls.
	 *
	 * Because for the hole lifetime of this render pass we own a lock to the
	 * global OpenGL state machine, wew can safely assume that, once we set up
	 * the pipeline, we won't have to ever do it again. */
	pub(crate) general_setup: bool,
	/** Whether the buffers and vertex array have been set up. */
	pub(crate) draw_buffers_setup: bool,
	/** Whether the stencil state has been set up. */
	pub(crate) stencil_setup: bool,
	/** Whether the blending state has been set up. */
	pub(crate) blending_setup: bool,
	/** Whether the framebuffer state has been set loaded. */
	pub(crate) framebuffer_loaded: bool,
	/** Reference to the pipeline object used in this pass. */
	pub(crate) pipeline: &'a RenderPipeline,
	/** Reference to a vertex buffer, if any. */
	pub(crate) vertex: Option<&'a VertexBuffer>,
	/** Reference to an index buffer, if any. */
	pub(crate) index: Option<&'a IndexBuffer>,
	/** Reference to a uniform binding group, if any. */
	pub(crate) bind: Option<&'a UniformGroup>,
	/** Framebuffer connected to the attachments. */
	pub(crate) framebuffer: &'a Framebuffer,
	/** Stencil reference value to be used during render operations. */
	pub(crate) stencil_reference: u8,
	/** Color blend constant value to be used during render operations. */
	pub(crate) color_blend_constant: Color,
}
impl<'a> RenderPass<'a> {
	/** Sets the vertex buffer to be used for this dispatch. */
	pub fn set_vertex_buffer(&mut self, buffer: &'a VertexBuffer) {
		let old = self.vertex.replace(buffer);

		/* We can compare inner buffers to check whether the buffer is the
		 * the same or not. */
		let updated = match old {
			Some(old) if !Rc::ptr_eq(&buffer.inner, &old.inner) => true,
			Some(_) => false,
			None => true,
		};
		self.draw_buffers_setup = !updated;
	}

	/** Sets the index buffer to be used for this dispatch. */
	pub fn set_index_buffer(&mut self, buffer: &'a IndexBuffer) {
		let old = self.index.replace(buffer);

		/* We can compare inner buffers to check whether the buffer is the
		 * the same or not. */
		let updated = match old {
			Some(old) if !Rc::ptr_eq(&buffer.inner, &old.inner) => true,
			Some(_) => false,
			None => true,
		};
		self.draw_buffers_setup = !updated;
	}

	/** Sets the uniform bind group to be used for this dispatch. */
	pub fn set_bind_group(&mut self, group: &'a UniformGroup) {
		let old = self.bind.replace(group);

		let updated = match old {
			Some(old) if !std::ptr::eq(old as *const _, group as *const _) =>
				true,
			Some(_) => false,
			None => true,
		};
		self.general_setup = !updated;
	}

	/** Set the viewport to be used for all subsequent draw commands. */
	pub fn set_viewport(&mut self, viewport: Viewport) {
		/* Clamp both the width and the height to the maximum value allowed by
		 * the context before we actually pass this call on to OpenGL. */

		let mut width = viewport.width;
		if let Some(max_width) = self.information.limits.max_viewport_width {
			if viewport.width > max_width {
				warn!("Clamping requested viewport width ({}) to the maximum ({})",
					viewport.width,
					max_width);
				width = max_width
			}
		}
		let mut height = viewport.height;
		if let Some(max_height) = self.information.limits.max_viewport_height {
			if viewport.height > max_height {
				warn!("Clamping requested viewport height ({}) to the maximum ({})",
					viewport.height,
					max_height);
				height = max_height
			}
		}

		/* Now that we've made sure the parameters are valid, pass them on. */
		let gl = self.context.as_ref();
		unsafe {
			gl.viewport(
				viewport.x,
				viewport.y,
				i32::try_from(width)
					.expect("the viewport width must fit in an i32"),
				i32::try_from(height)
					.expect("the viewport height must fit in an i32"))
		}
	}

	/** Sets the blend color as used by some of the blending modes. */
	pub fn set_blend_color(&mut self, color: Color) {
		self.color_blend_constant = color;
		self.blending_setup = false;
	}

	/** Set the reference value for stencil operations. */
	pub fn set_stencil_reference(&mut self, reference: u8) {
		self.stencil_reference = reference;
		self.stencil_setup = false;
	}

	pub fn set_pipeline(&mut self, pipeline: &'a RenderPipeline) {
		self.pipeline = pipeline;
		self.general_setup = false;
	}

	/** Perform the setup of the pipeline for subsequent render command, if
	 * required. Importantly, this function does not control the stencil state.
	 */
	unsafe fn ensure_setup(&mut self) {
		let gl = self.context.as_ref();
		if !self.framebuffer_loaded {
			self.framebuffer.bind_and_load(gl);
			self.framebuffer_loaded = true;
		}

		if !self.general_setup {
			self.framebuffer.bind(gl);
			self.pipeline.bind(gl);

			let vertex = self.vertex.map(|vertex| vertex.as_raw_handle());
			let index = self.index.map(|index| index.as_raw_handle());
			if let Some(binder) = &self.bind {
				binder.bind(
					gl,
					&self.information.features,
					&self.pipeline.inner.program)
			}

			gl.bind_buffer(glow::ARRAY_BUFFER, vertex);
			gl.bind_buffer(glow::ELEMENT_ARRAY_BUFFER, index);

			self.general_setup = true;
		}

		if !self.draw_buffers_setup {
			self.pipeline.vertex_array_setup(
				gl,
				self.vertex,
				self.index);
			self.draw_buffers_setup = true;
		}

		if !self.stencil_setup {
			self.pipeline.stencil_setup(gl, self.stencil_reference);
			self.stencil_setup = true;
		}

		if !self.blending_setup {
			self.pipeline.blending_setup(gl, self.color_blend_constant);
			self.blending_setup = true;
		}
	}

	/** Actually performs the dispatch set up in this structure. */
	pub fn draw_indexed(
		&mut self,
		indices: Range<u32>,
		instances: u32) {

		/* Lock the pipeline.
		 *
		 * We don't actually use the result from this lock, because the FFI does
		 * not require us to actually have a mutable borrow to the context. This
		 * is done for the sake of internal consistency rather than FFI safety.
		 */
		let _atoms = (
			self.pipeline.acquire_read_guarded(),
			self.vertex.as_ref().map(|buffer| buffer.acquire_read_guarded()),
			self.index.as_ref().map(|buffer| buffer.acquire_read_guarded()),
			self.bind.as_ref().map(|bind| bind.acquire_read_guarded()));
		self.pipeline.framebuffer_acquire_write(
			&self.framebuffer,
			/* We have to be strict if demanded by the current context. */
			!self.information.features.readonly_framebuffer_feedback);

		let check_i32 = |val|
			i32::try_from(val).expect("value does not fit in an i32, as is \
				required by the opengl interface");

		/* Make sure the pipeline is setup correctly. */
		unsafe {
			self.ensure_setup();
		}

		let gl = self.context.as_ref();
		unsafe {
			gl.draw_elements_instanced(
				self.pipeline.drawing_mode(),
				check_i32(indices.end) - check_i32(indices.start),
				self.pipeline.index_type(),
				check_i32(indices.start * self.pipeline.index_len()),
				check_i32(instances))
		}

		self.pipeline.framebuffer_release_write(
			&self.framebuffer,
			/* We have to be strict if demanded by the current context. */
			!self.information.features.readonly_framebuffer_feedback);
	}
}

/** Specification of a viewport. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Viewport {
	/** Offset of the point of origin in the horizontal axis. */
	pub x: i32,
	/** Offset of the point of origin in the vertical axis. */
	pub y: i32,
	/** Width of the view frame in physical display units.
	 *
	 * # Size limitation
	 * For a valid viewport, this must be smaller than [`i32::MAX_VALUE`],
	 * otherwise, OpenGL will just halt and catch fire. */
	pub width: u32,
	/** Height of the view frame in physical display units.
	 *
	 * # Size limitation
	 * For a valid viewport, this must be smaller than [`i32::MAX_VALUE`],
	 * otherwise, OpenGL will just halt and catch fire. */
	pub height: u32,
}

/** Descriptor for starting a new render pass. */
pub struct RenderPassDescriptor<'a> {
	/** The pipeline that will be used for the render pass. */
	pub pipeline: &'a RenderPipeline,
	/** The framebuffer that will receive the results of the render pass. */
	pub framebuffer: &'a Framebuffer,
}

