use crate::shader::{VertexShader, FragmentShader};
use std::rc::Rc;
use glow::{HasContext, Context};
use std::borrow::Cow;
use crate::access::{AccessLock, UnitAccessLock};
use crate::{VertexBuffer, IndexBuffer, Framebuffer, FramebufferVariants, Color};
use std::convert::TryFrom;
use std::collections::HashMap;
use std::cell::Cell;

/** Wrapper around a shader program used in a render pipeline. */
#[derive(Debug)]
pub(crate) struct RenderProgram {
	/** The name of program in the context. */
	pub(crate) program: <Context as HasContext>::Program,
	/** Set of active vertex attributes exposed by the program. */
	pub(crate) attributes: HashMap<String, ActiveBinding>,
	/** Set of active uniforms exposed by the program. */
	pub(crate) uniforms: HashMap<String, ActiveBinding>,
}
impl RenderProgram {
	/** Creates a new instance of this structure from the given raw program
	 * handle, probing it for information with the given context reference. */
	pub unsafe fn new(
		gl: &Context,
		program: <Context as HasContext>::Program) -> Self {

		let attributes = 0..gl.get_active_attributes(program);
		let uniforms = 0..gl.get_active_uniforms(program);

		Self {
			program,
			attributes: attributes.into_iter()
				.filter_map(|index| gl.get_active_attribute(program, index))
				.map(|attribute| (
					attribute.name,
					ActiveBinding {
						kind: attribute.atype,
						size: u32::try_from(attribute.size).unwrap()
					}
				))
				.collect(),
			uniforms: uniforms.into_iter()
				.filter_map(|index| gl.get_active_uniform(program, index))
				.map(|attribute| (
					attribute.name,
					ActiveBinding {
						kind: attribute.utype,
						size: u32::try_from(attribute.size).unwrap()
					}
				))
				.collect(),
		}
	}
}

/** Information on an active shader program binding.
 *
 * Both uniform and attribute bindings use this structure, since the types used
 * to express information on them is the same. However, the valid variants of
 * this structure change depending on whether this is an attribute or a uniform
 * binding. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct ActiveBinding {
	/** Type of this bind. Either a uniform type enum value or an attribute type
	 * enum value.
	 *
	 * Taken directly from OpenGL and not further type checked, the type value
	 * may not be always valid. */
	pub kind: u32,
	/** The size of this binding, in bytes. */
	pub size: u32,
}

#[derive(Debug)]
pub(crate) struct InnerRenderPipeline {
	/** Handle to the shared context. */
	pub(crate) context: Rc<Context>,
	/** Access control structure. */
	pub(crate) access: UnitAccessLock,
	/** Shader program, linked from the shaders specified in the descriptor. */
	pub(crate) program: RenderProgram,
	/** Vertex Array Object specifying the layout of the vertex buffer. */
	pub(crate) vao: Cell<Option<<Context as HasContext>::VertexArray>>,
	/** Layout of the vertex buffer. */
	pub(crate) vertex_layout: OwnedVertexBufferLayout,
	/** Reference to the vertex shader used in this pipeline. */
	pub(crate) vertex_shader: VertexShader,
	/** Reference to the fragment shader used in this pipeline, if any. */
	pub(crate) fragment_shader: Option<FragmentShader>,
	/** State information for the primitive assembler. */
	pub(crate) primitive_state: PrimitiveState,
	/** The effect of draw calls on the depth and stencil aspects of the output
	 * target, if any. */
	pub(crate) depth_stencil: Option<DepthStencilState>,
	/** The operations to be applied to the color targets of this pipeline. */
	pub(crate) color_target_state: ColorTargetState
}
impl Drop for InnerRenderPipeline {
	fn drop(&mut self) {
		unsafe {
			let _atom = self.access.acquire_write_guarded();
			self.context.delete_program(self.program.program);
			if let Some(vao) = self.vao.replace(None) {
				self.context.delete_vertex_array(vao);
			}
		}
	}
}

pub struct RenderPipeline {
	/** Shared inner version of this render pipeline object. */
	pub(crate) inner: Rc<InnerRenderPipeline>
}
impl AccessLock for RenderPipeline {
	fn write_locks(&self) -> usize {
		0
	}
	fn read_locks(&self) -> usize {
		let a = self.inner.vertex_shader.read_locks();
		if let Some(fragment_shader) = &self.inner.fragment_shader {
			assert_eq!(a, fragment_shader.read_locks());
		}
		assert_eq!(a, self.inner.access.read_locks());

		a
	}
	fn acquire_write(&self) {
		panic!("tried to perform a write lock operation on a pipeline. \
			pipelines are read-only objects");
	}
	fn release_write(&self) {
		panic!("tried to perform a write lock operation on a pipeline. \
			pipelines are read-only objects");
	}
	fn acquire_read(&self) {
		self.inner.vertex_shader.acquire_read();
		if let Some(fragment_shader) = &self.inner.fragment_shader {
			fragment_shader.acquire_read();
		}
		self.inner.access.acquire_read();
	}
	fn release_read(&self) {
		self.inner.vertex_shader.release_read();
		if let Some(fragment_shader) = &self.inner.fragment_shader {
			fragment_shader.release_read();
		}
		self.inner.access.release_read();
	}
}
impl RenderPipeline {
	/** Bind this pipeline for use in OpenGL. */
	pub(crate) unsafe fn bind(&self, gl: &Context) {
		gl.use_program(Some(self.inner.program.program));

		/* Set up culling. */
		match self.inner.primitive_state.front_face {
			FrontFace::Ccw => gl.front_face(glow::CCW),
			FrontFace::Cw => gl.front_face(glow::CW)
		}
		match self.inner.primitive_state.cull_mode {
			CullMode::None => gl.disable(glow::CULL_FACE),
			CullMode::Back => {
				gl.enable(glow::CULL_FACE);
				gl.cull_face(glow::BACK)
			},
			CullMode::Front => {
				gl.enable(glow::CULL_FACE);
				gl.cull_face(glow::FRONT)
			}
		}

		/* Set up depth testing. */
		if let Some(ds) = self.inner.depth_stencil {
			gl.enable(glow::DEPTH_TEST);
			gl.depth_mask(ds.depth_write_enabled);
			gl.depth_func(ds.depth_compare.as_opengl());
		} else {
			gl.disable(glow::DEPTH_TEST)
		}

		/* Set up color masking. */
		gl.color_mask(
			self.inner.color_target_state.write_mask.contains(ColorWrite::RED),
			self.inner.color_target_state.write_mask.contains(ColorWrite::GREEN),
			self.inner.color_target_state.write_mask.contains(ColorWrite::BLUE),
			self.inner.color_target_state.write_mask.contains(ColorWrite::ALPHA));
	}

	/** Checks whether the depth aspect is written to by this pipeline. */
	fn depth_write_enabled(&self) -> bool {
		if let Some(ds) = self.inner.depth_stencil {
			ds.depth_write_enabled
		} else {
			false
		}
	}

	/** Checks whether the stencil aspect is written to by this pipeline. */
	fn stencil_write_enabled(&self) -> bool {
		if let Some(ds) = self.inner.depth_stencil {
			let masked = ds.stencil.write_mask == 0;

			let kept_pass = ds.stencil.pass_op == StencilOperation::Keep;
			let kept_fail = ds.stencil.fail_op == StencilOperation::Keep;
			let kept_dfal = ds.stencil.depth_fail_op == StencilOperation::Keep;

			let kept = match ds.stencil.compare {
				CompareFunction::Always =>
					/* We can ignore what the fail operation does if the test is
					 * never set to fail. We only check for the other two. */
					kept_dfal && kept_pass,
				CompareFunction::Never =>
					/* We can ignore what both the pass and depth fail
					 * operations do, because the test is never going to pass in
					 * the first place. We only check for what the fail
					 * operation does. */
					kept_fail,
				_ =>
					/* The compare function doesn't let us disregard any of the
					 * operations, so they all must be set to keep. */
					kept_pass && kept_fail && kept_dfal
			};

			/* We consider writing to the stencil aspect to be enabled if both
			 * the write mask is non-zero and any of the used operations are set
			 * to write to the stencil buffer. */
			!kept && !masked
		} else {
			false
		}
	}

	/** Lock the given framebuffer knowing what the pipeline intends to do with
	 * it. This is a more costly operation than locking the whole of the
	 * framebuffer at once, but it allows for the framebuffer and the shaders
	 * to share resources when the pipeline is not gonna be writing to them.
	 *
	 * When using this function, the analogous functions in the framebuffer must
	 * not be called. Releases must be done through the
	 * [`framebuffer_release_write()`] function in the pipeline.
	 * */
	pub(crate) fn framebuffer_acquire_write(&self, fb: &Framebuffer, strict: bool) {
		let fb = match fb.variants {
			FramebufferVariants::Default { .. } =>
				/* The default framebuffer is managed directly by OpenGL. */
				return,
			FramebufferVariants::Custom { ref inner } => &**inner
		};
		fb.access.acquire_write();

		/* Check the depth-stencil attachment of the framebuffer. */
		if self.depth_write_enabled() || self.stencil_write_enabled() {
			for texture in &fb.depth_stencil { texture.acquire_write() }
		} else {
			for texture in &fb.depth_stencil {
				if !strict {
					texture.acquire_read()
				} else {
					/* Catch valid texture feedback uses that become invalid in
					 * strict mode, and panic if any were found. */
					if texture.read_locks() > 0 {
						panic!("Tried to use a framebuffer with an active \
							texture feedback loop in strict mode. This has \
							most likely happened because you are trying to use \
							this feature in a host that does not have \
							Features::readonly_framebuffer_feedback.");
					}
					texture.acquire_write()
				}
			}
		}

		/* We don't know how color attachments behave since we don't have access
		 * to the shaders. We can't assume anything, so all color attachments
		 * are always marked as being write targets. */
		for texture in &fb.color_attachments { texture.acquire_write() }
	}

	/** Unlocks a framebuffer previously locked using the
	 * [`framebuffer_acquire_write()`] function. */
	pub(crate) fn framebuffer_release_write(&self, fb: &Framebuffer, strict: bool) {
		let fb = match fb.variants {
			FramebufferVariants::Default { .. } =>
				/* The default framebuffer is managed directly by OpenGL. */
				return,
			FramebufferVariants::Custom { ref inner } => &**inner
		};
		fb.access.release_write();

		if self.depth_write_enabled() || self.stencil_write_enabled() || strict {
			for texture in &fb.depth_stencil { texture.release_write() }
		} else {
			for texture in &fb.depth_stencil { texture.release_read() }
		}

		for texture in &fb.color_attachments { texture.release_write() }
	}

	/** Sets up the stencil state of the pipeline.
	 *
	 * This part of the setup requires an external reference value and thus it
	 * is done separately from the rest of the setup, which is done in the
	 * [`bind()`] function. */
	pub(crate) unsafe fn stencil_setup(&self, gl: &Context, reference: u8) {
		if let Some(DepthStencilState { stencil, .. }) = self.inner.depth_stencil {
			gl.enable(glow::STENCIL_TEST);
			gl.stencil_mask(u32::from(stencil.write_mask));
			gl.stencil_func(
				stencil.compare.as_opengl(),
				i32::from(reference),
				u32::from(stencil.read_mask));
			gl.stencil_op(
				stencil.fail_op.as_opengl(),
				stencil.depth_fail_op.as_opengl(),
				stencil.pass_op.as_opengl())
		} else {
			gl.disable(glow::STENCIL_TEST);
		}
	}

	/** Sets up the blending state of the pipeline.
	 *
	 * This part of the setup requires an external reference value and thus it
	 * is done separately from the rest of the setup, which is done in the
	 * [`bind()`] function. */
	pub(crate) unsafe fn blending_setup(&self, gl: &Context, constant: Color) {
		let state = &self.inner.color_target_state;

		let alpha_required = !state.alpha_blend.may_be_skipped();
		let color_required = !state.color_blend.may_be_skipped();
		let required = alpha_required || color_required;

		if required {
			gl.enable(glow::BLEND);
			gl.blend_color(
				constant.red,
				constant.green,
				constant.blue,
				constant.alpha);

			/* Set up the blend factors. */
			gl.blend_func_separate(
				state.color_blend.src_factor.as_opengl(),
				state.color_blend.dst_factor.as_opengl(),
				state.alpha_blend.src_factor.as_opengl(),
				state.alpha_blend.dst_factor.as_opengl());

			/* Set up the blend equations. */
			gl.blend_equation_separate(
				state.color_blend.operation.as_opengl(),
				state.alpha_blend.operation.as_opengl());
		} else {
			gl.disable(glow::BLEND);
		}
	}

	/** Sets up the vertex array state of the pipeline.
	 *
	 * This part of the setup requires previous knowledge of exactly which
	 * buffers are going to be used for drawing. This forces us to configure the
	 * VAO right before its use in the pipeline. */
	pub(crate) unsafe fn vertex_array_setup(
		&self,
		gl: &Context,
		vertex_buffer: Option<&VertexBuffer>,
		index_buffer: Option<&IndexBuffer>) {

		/* Create a new VAO and delete the old one. */
		let vao = gl.create_vertex_array()
			.expect("could not create clean vertex array for pipeline \
				setup");
		if let Some(old) = self.inner.vao.replace(Some(vao)) {
			gl.delete_vertex_array(old);
		}

		/* Bind the new vertex array so that we get a clean namespace right
		 * away, even if we error out. */
		gl.bind_vertex_array(Some(vao));


		/* Expecting to use attributes from a non-existent vertex buffer is
		 * a bug, so we panic right away. */
		if vertex_buffer.is_none()
			&& self.inner.vertex_layout.attributes.len() != 0 {

			panic!("tried to use a non-empty vertex buffer layout with no \
				vertex buffer to be bound")
		}

		let vertex_buffer = vertex_buffer.map(|buffer| buffer.inner.buffer);
		let index_buffer = index_buffer.map(|buffer| buffer.inner.buffer);

		/* Bind the buffer, then set up all of the vertex attributes to point to
		 * it in the right places. We have to do this with the target buffer
		 * bound to `ARRAY_BUFFER`, otherwise the implementation would likely
		 * assume us to be giving it a location in host memory. */

		gl.bind_buffer(glow::ARRAY_BUFFER, vertex_buffer);
		for attribute in &self.inner.vertex_layout.attributes {
			if let None = self.inner.program.attributes.get(attribute.binding.as_ref()) {
				trace!("tried to bind to the inactive attribute \"{}\". data \
					for this attribute will be missing",
					attribute.binding);

				continue
			}

			let binding = gl.get_attrib_location(
				self.inner.program.program,
				&attribute.binding)
				.expect("could not find binding previously determined to \
					be active");

			let kind = attribute.kind.as_opengl();
			let count = attribute.components as _;

			let offset = i32::try_from(attribute.offset)
				.expect("invalid vertex attribute offset");
			let stride = i32::try_from(self.inner.vertex_layout.array_stride)
				.expect("invalid vertex buffer stride");

			gl.enable_vertex_attrib_array(binding);
			gl.vertex_attrib_pointer_f32(
				binding,
				count,
				kind,
				false,
				stride,
				offset)
		}

		/* Binding to `ELEMENT_ARRAY_BUFFER` by itself is enough to make the
		 * VAO point to it. */
		gl.bind_buffer(glow::ELEMENT_ARRAY_BUFFER, index_buffer);
	}

	/** OpenGL drawing mode for this pipeline. */
	pub(crate) fn drawing_mode(&self) -> u32 {
		match self.inner.primitive_state.topology {
			PrimitiveTopology::LineList      => glow::LINES,
			PrimitiveTopology::LineStrip     => glow::LINE_STRIP,
			PrimitiveTopology::PointList     => glow::POINTS,
			PrimitiveTopology::TriangleList  => glow::TRIANGLES,
			PrimitiveTopology::TriangleStrip => glow::TRIANGLE_STRIP
		}
	}

	/** OpenGL type used for model indices in this pipeline. */
	pub(crate) fn index_type(&self) -> u32 {
		match self.inner.primitive_state.index_format {
			IndexFormat::Uint16 => glow::UNSIGNED_SHORT,
			IndexFormat::Uint32 => glow::UNSIGNED_INT
		}
	}

	/** Number of bytes used by every model index in this pipeline. */
	pub(crate) fn index_len(&self) -> u32 {
		match self.inner.primitive_state.index_format {
			IndexFormat::Uint16 => 2,
			IndexFormat::Uint32 => 4
		}
	}
}

#[derive(Copy, Clone)]
pub struct RenderPipelineDescriptor<'a> {
	/** Description of the vertex processing stage of this pipeline. */
	pub vertex: VertexState<'a>,
	/** State description for the primitive assembler. */
	pub primitive_state: PrimitiveState,
	/** Object referring to the fragment program that will be used to process
	 * the fragments fed into this pipeline, if any.
	 *
	 * The fragment stage may be omitted from the pipeline if only the side
	 * effects of the other stages are desired. Such as when calculating light
	 * maps from the point of view of the light source. */
	pub fragment: Option<FragmentState<'a>>,
	/** The effect of draw calls on the depth and stencil aspects of the output
	 * target, if any. */
	pub depth_stencil: Option<DepthStencilState>,
}

/** Describes the depth and stencil aspects in a render pipeline. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DepthStencilState {
	/** If disabled, depth will not be written to. */
	pub depth_write_enabled: bool,
	/** Comparison function used to compare depth values in the depth test. */
	pub depth_compare: CompareFunction,
	/** Stencil state. */
	pub stencil: StencilState,
}

/** Describes stencil state in a render pipeline.
 * If you are not using stencil state, set this to `StencilState::IGNORE`. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct StencilState {
	/** Stencil values are AND-ed with this mask when writing to the stencil
	 * buffer. */
	pub write_mask: u8,
	/** Stencil values are AND-ed with this mask when reading from the reference
	 * or from the buffer. */
	pub read_mask: u8,
	/** Comparison function that determines if the fail_op or pass_op is used
	 * on the stencil buffer. */
	pub compare: CompareFunction,
	/** Operation that is preformed when stencil test fails. */
	pub fail_op: StencilOperation,
	/** Operation that is performed when depth test fails but stencil test
	 * succeeds. */
	pub depth_fail_op: StencilOperation,
	/** Operation that is performed when stencil test success. */
	pub pass_op: StencilOperation,
}
impl StencilState {
	/** Ignore the stencil state. */
	pub const IGNORE: Self = Self {
		write_mask: 0xff,
		read_mask: 0xff,
		compare: CompareFunction::Always,
		fail_op: StencilOperation::Keep,
		depth_fail_op: StencilOperation::Keep,
		pass_op: StencilOperation::Keep
	};
}

/** Operation to perform on the stencil value. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum StencilOperation {
	/** Keep stencil value unchanged. */
	Keep,
	/** Set stencil value to zero. */
	Zero,
	/** Replace stencil value with value provided in most recent call to [`RenderPass::set_stencil_reference`]. */
	Replace,
	/** Bitwise inverts stencil value. */
	Invert,
	/** Increments stencil value by one, clamping on overflow. */
	IncrementClamp,
	/** Decrements stencil value by one, clamping on underflow. */
	DecrementClamp,
	/** Increments stencil value by one, wrapping on overflow. */
	IncrementWrap,
	/** Decrements stencil value by one, wrapping on underflow. */
	DecrementWrap,
}
impl StencilOperation {
	/** Get the OpenGL enum value for the current variant. */
	fn as_opengl(&self) -> u32 {
		match self {
			Self::Keep => glow::KEEP,
			Self::Zero => glow::ZERO,
			Self::Replace => glow::REPLACE,
			Self::IncrementClamp => glow::INCR,
			Self::IncrementWrap => glow::INCR_WRAP,
			Self::DecrementClamp => glow::DECR,
			Self::DecrementWrap => glow::DECR_WRAP,
			Self::Invert => glow::INVERT
		}
	}
}

/** Comparison function used for depth and stencil operations. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CompareFunction {
	/** Function never passes. */
	Never,
	/** Function passes if new value less than existing value. */
	Less,
	/** Function passes if new value is equal to existing value. */
	Equal,
	/** Function passes if new value is less than or equal to existing value. */
	LessEqual,
	/** Function passes if new value is greater than existing value. */
	Greater,
	/** Function passes if new value is not equal to existing value. */
	NotEqual,
	/** Function passes if new value is greater than or equal to existing value. */
	GreaterEqual,
	/** Function always passes. */
	Always,
}
impl CompareFunction {
	/** Get the OpenGL enum value for the current variant. */
	fn as_opengl(&self) -> u32 {
		match self {
			Self::Equal => glow::EQUAL,
			Self::Always => glow::ALWAYS,
			Self::Greater => glow::GREATER,
			Self::GreaterEqual => glow::GEQUAL,
			Self::Less => glow::LESS,
			Self::LessEqual => glow::LEQUAL,
			Self::NotEqual => glow::NOTEQUAL,
			Self::Never => glow::NEVER
		}
	}
}

/** Describes the fragment process in a render pipeline. */
#[derive(Copy, Clone)]
pub struct FragmentState<'a> {
	/** The compiled shader module for this stage. */
	pub shader: &'a FragmentShader,
	/** The color operations to be applied to all of the color targets.
	 *
	 * Normally this would be an array of valid color target states, one for
	 * each target in the framebuffer. However, the implementation of OpenGL ES
	 * backing this library does not support multiple color target states. */
	pub targets: ColorTargetState
}

/** Describes the color state of a render pipeline. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ColorTargetState {
	/** The alpha blending that is used for this pipeline. */
	pub alpha_blend: BlendState,
	/** The color blending that is used for this pipeline. */
	pub color_blend: BlendState,
	/** Mask which enables or disables writes to different target channels. */
	pub write_mask: ColorWrite
}

bitflags::bitflags! {
	/// Color write mask. Disabled color channels will not be written to.
    #[repr(transparent)]
    pub struct ColorWrite: u32 {
        /** Enable red channel writes. */
        const RED = 1;
        /** Enable green channel writes. */
        const GREEN = 2;
        /** Enable blue channel writes. */
        const BLUE = 4;
        /** Enable alpha channel writes. */
        const ALPHA = 8;
        /** Enable red, green, and blue channel writes. */
        const COLOR = 7;
        /** Enable writes to all channels. */
        const ALL = 15;
    }
}

/** Describes the blend state of a pipeline.
 *
 * Alpha blending is very complicated: see the OpenGL or Vulkan spec for more
 * information. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BlendState {
	/** Multiplier for the source, which is produced by the fragment shader. */
	pub src_factor: BlendFactor,
	/** Multiplier for the destination, which is stored in the target. */
	pub dst_factor: BlendFactor,
	/** The binary operation applied to the source and destination, multiplied
	 * by their respective factors. */
	pub operation: BlendOperation,
}
impl BlendState {
	/** Default blending state that replaces destination with the source. */
	pub const REPLACE: Self = BlendState {
		src_factor: BlendFactor::One,
		dst_factor: BlendFactor::Zero,
		operation: BlendOperation::Add,
	};

	/** Whether the operations described by this blending state have any
	 * noticeable effect when compared to leaving blending disabled.
	 *
	 * This function lets us check whether enabling the blend operation is
	 * really required. Which speeds up a few operations. */
	pub(crate) fn may_be_skipped(&self) -> bool {
		*self == Self::REPLACE
	}
}


/** Alpha blend factor.
 *
 * Alpha blending is very complicated: see the OpenGL or Vulkan spec for more
 * information. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BlendFactor {
	Zero,
	One,
	SrcColor,
	OneMinusSrcColor,
	SrcAlpha,
	OneMinusSrcAlpha,
	DstColor,
	OneMinusDstColor,
	DstAlpha,
	OneMinusDstAlpha,
	SrcAlphaSaturated,
	BlendColor,
	OneMinusBlendColor,
}
impl BlendFactor {
	/** Get the OpenGL enum value for the current variant. */
	fn as_opengl(&self) -> u32 {
		match self {
			Self::Zero => glow::ZERO,
			Self::One => glow::ONE,
			Self::SrcColor => glow::SRC_COLOR,
			Self::OneMinusSrcColor => glow::ONE_MINUS_SRC_COLOR,
			Self::DstColor => glow::DST_COLOR,
			Self::OneMinusDstColor => glow::ONE_MINUS_DST_COLOR,
			Self::SrcAlpha => glow::SRC_ALPHA,
			Self::OneMinusSrcAlpha => glow::ONE_MINUS_SRC_ALPHA,
			Self::DstAlpha => glow::DST_ALPHA,
			Self::OneMinusDstAlpha => glow::ONE_MINUS_DST_ALPHA,
			Self::BlendColor => glow::CONSTANT_COLOR,
			Self::OneMinusBlendColor => glow::ONE_MINUS_CONSTANT_COLOR,
			Self::SrcAlphaSaturated =>
				/* Use the same as SrcAlpha. */
				glow::SRC_ALPHA
		}
	}
}

/** Alpha blend operation.
 *
 * Alpha blending is very complicated: see the OpenGL or Vulkan spec for more
 * information. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum BlendOperation {
	Add,
	Subtract,
	ReverseSubtract,
	Min,
	Max,
}
impl BlendOperation {
	/** Get the OpenGL enum value for the current variant. */
	fn as_opengl(&self) -> u32 {
		match self {
			Self::Add => glow::FUNC_ADD,
			Self::Subtract => glow::FUNC_SUBTRACT,
			Self::ReverseSubtract => glow::FUNC_REVERSE_SUBTRACT,
			Self::Min => glow::MIN,
			Self::Max => glow::MAX,
		}
	}
}
impl Default for BlendOperation {
	fn default() -> Self {
		Self::Add
	}
}

#[derive(Copy, Clone)]
/** Description of the vertex processing stage of a pipeline. */
pub struct VertexState<'a> {
	/** Object referring to the vertex program that will be used to process the
	 * vertices fed into this pipeline. */
	pub shader: &'a VertexShader,
	/** The layout of the vertex buffer used in this pipeline.
	 *
	 * # Single element
	 * OpenGL ES 3.0 does not offer support for multiple vertex buffers in a
	 * programmable render pipeline. Thus, we have a single layout that maps to
	 * the one single VAO we are afforded for render commands.
	 */
	pub buffer: &'a VertexBufferLayout<'a>,
}

/** Description of the layout of a vertex buffer. */
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VertexBufferLayout<'a> {
	/** The stride, in bytes, between elements of this buffer. */
	pub array_stride: u32,
	/** The list of attributes which comprise a single vertex. */
	pub attributes: &'a [VertexAttribute<'a>]
}

/** Owned version of the [`VertexBufferLayout`] structure. */
#[derive(Debug)]
pub(crate) struct OwnedVertexBufferLayout {
	pub(crate) array_stride: u32,
	pub(crate) attributes: Vec<VertexAttribute<'static>>,
}
impl<'a> From<&'_ VertexBufferLayout<'a>> for OwnedVertexBufferLayout {
	fn from(layout: &VertexBufferLayout<'a>) -> Self {
		Self {
			array_stride: layout.array_stride,
			attributes: layout.attributes.iter()
				.map(|attribute| VertexAttribute {
					kind: attribute.kind,
					components: attribute.components,
					offset: attribute.offset,
					binding: Cow::Owned(attribute.binding.to_string())
				})
				.collect()
		}
	}
}

/** Description of a single vertex attribute. */
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VertexAttribute<'a> {
	/** Specifies the data type of each component in the vertex attribute. */
	pub kind: VertexType,
	/** Specifies the number of components in the vertex attribute. */
	pub components: VertexComponents,
	/** Offset of this attribute from the start of a vertex. */
	pub offset: u32,
	/** Binding to the shader. This is the name given to the input parameter in
	 * the shader code.
	 *
	 * # The reason for using variable names
	 * The OpenGL ES version of GLSL offers no support for the use of
	 * `layout(location)`, leaving us with two options: Using shader variable
	 * names or relying on automatic assignment.
	 *
	 * Automatic assignment is completely unpredictable and arbitrary in how it
	 * assigns indices to entries in the shader and it may assign different
	 * indices to the same shader, linked two different times. Thus, automatic
	 * assignment is effectively useless.
	 *
	 * This leaves us with using variable names as the only viable option.
	 */
	pub binding: Cow<'a, str>
}
impl<'a> VertexAttribute<'a> {
	/** Length in bytes of this attribute, in the buffer. */
	pub fn len(&self) -> u32 {
		let component = match self.kind {
			VertexType::I8  | VertexType::U8  => 1,
			VertexType::I16 | VertexType::U16 => 2,
			VertexType::F16 => 2,
			VertexType::F32 => 4,
		};
		let multiplier = self.components as u32;

		component * multiplier
	}
}

/** Data types a vertex attribute may be of. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum VertexType {
	/** Signed 8-bit integer. Corresponds to `GL_BYTE`. */
	I8,
	/** Unsigned 8-bit integer. Corresponds to `GL_UNSIGNED_BYTE`. */
	U8,
	/** Signed 16-bit integer. Corresponds to `GL_SHORT`. */
	I16,
	/** Unsigned 16-bit integer. Corresponds to `GL_UNSIGNED_SHORT`. */
	U16,
	/** Signed 16-bit floating point number. Corresponds to `GL_HALF_FLOAT`. */
	F16,
	/** Signed 32-bit floating point number. Corresponds to `GL_FLOAT`. */
	F32
}
impl VertexType {
	/** Returns the OpenGL enum the current variant is equivalent to. */
	pub fn as_opengl(&self) -> u32 {
		match self {
			Self::I8 => glow::BYTE,
			Self::U8 => glow::UNSIGNED_BYTE,
			Self::I16 => glow::SHORT,
			Self::U16 => glow::UNSIGNED_SHORT,
			Self::F16 => glow::HALF_FLOAT,
			Self::F32 => glow::FLOAT
		}
	}
}

/** Describes the state of primitive assembly and rasterization in a render
 * pipeline. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PrimitiveState {
	/** The primitive topology used to interpret vertices. */
	pub topology: PrimitiveTopology,
	/** Format of the indices. */
	pub index_format: IndexFormat,
	/** The face to consider the front for the purpose of culling and stencil
	 * operations. */
	pub front_face: FrontFace,
	/** The face culling mode. */
	pub cull_mode: CullMode,
	/** Controls the way each polygon is rasterized. */
	pub polygon_mode: PolygonMode,
}

/** Type of drawing modes for polygons. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum PolygonMode {
	/** Polygons will be filled. */
	Fill,
}

/** Type of faces to be culled. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CullMode {
	/** No faces will be culled. */
	None,
	/** Front faces will be culled. */
	Front,
	/** Back faces will be culled. */
	Back,
}

/** Winding order which classifies the "front" face. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum FrontFace {
	/** Triangles with vertices in counter clockwise order are considered the
	 * front face.
	 *
	 * This is the default with right handed coordinate spaces. */
	Ccw,
	/** Triangles with vertices in clockwise order are considered the front
	 * face.
	 *
	 * This is the default with left handed coordinate spaces.
	 */
	Cw

}

/** Primitive type the input mesh is composed of. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum PrimitiveTopology {
	/** Vertex data is a list of points. Each vertex is a new point. */
	PointList,
	/** Vertex data is a list of lines. Each pair of vertices composes a new
	 * line.
	 *
	 * Vertices `0 1 2 3` create two lines `0 1` and `2 3`. */
	LineList,
	/** Vertex data is a strip of lines. Each set of two adjacent vertices form
	 * a line.
	 *
	 * Vertices `0 1 2 3` create three lines `0 1`, `1 2`, and `2 3`. */
	LineStrip,
	/** Vertex data is a list of triangles. Each set of 3 vertices composes a
	 * new triangle.
	 *
	 * Vertices `0 1 2 3 4 5` create two triangles `0 1 2` and `3 4 5`.
	 */
	TriangleList,
	/** Vertex data is a triangle strip. Each set of three adjacent vertices
	 * form a triangle.
	 *
	 * Vertices `0 1 2 3 4 5` creates four triangles `0 1 2`, `2 1 3`, `3 2 4`,
	 * and `4 3 5`. */
	TriangleStrip,
}

/** Number of components a vertex attribute may have. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum VertexComponents {
	/** Single component vertex attribute. */
	One = 1,
	/** Dual component vertex attribute. */
	Two = 2,
	/** Three component vertex attribute. */
	Three = 3,
	/** Found component vertex attribute. */
	Four = 4
}

/** Data types an index may have. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum IndexFormat {
	/** Unsigned 16-bit integer. Corresponds to `GL_UNSIGNED_SHORT`. */
	Uint16,
	/** Unsigned 32-bit integer. Corresponds to `GL_UNSIGNED_INT`. */
	Uint32,
}

#[derive(Debug, thiserror::Error)]
pub enum RenderPipelineError {
	#[error("Failed to create shader program: {what}")]
	ProgramCreationFailed {
		what: String
	},
	#[error("Failed to link shader program: {what}")]
	ProgramLinkFailed {
		what: String
	},
	#[error("Failed to create a new vertex array object: {what}")]
	VertexArrayObjectCreationFailed {
		what: String
	},
	#[error("Attribute binding name is missing from shader program: {binding}")]
	AttributeBindingMissing {
		/** Name of the binding in the shader that is missing. */
		binding: String,
	}
}
