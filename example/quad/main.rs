use winit::dpi::PhysicalSize;
use winit::event_loop::{EventLoop, ControlFlow};
use winit::window::{WindowBuilder, WindowId};
use gavle::*;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::time::Duration;
use winit::event::{Event, WindowEvent, ElementState, VirtualKeyCode};
use std::num::NonZeroU8;

/** Vertex type. */
#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, bytemuck::Pod, bytemuck::Zeroable)]
pub struct Vertex {
	position: [f32; 4],
	texture: [f32; 2],
}
impl Vertex {
	/** Layout of buffers using this vertex type. */
	pub const LAYOUT: &'static VertexBufferLayout<'static> = &VertexBufferLayout {
		array_stride: 6 * 4,
		attributes: &[
			VertexAttribute {
				kind: VertexType::F32,
				components: VertexComponents::Four,
				offset: 0,
				binding: Cow::Borrowed("position")
			},
			VertexAttribute {
				kind: VertexType::F32,
				components: VertexComponents::Two,
				offset: 16,
				binding: Cow::Borrowed("texture_uv")
			}
		]
	};

	/** Vertex list used to produce the model of a texture mapped cube. */
	pub const CUBE_VERTICES: &'static [Self] = &[
		Self { position: [-1.0, -1.0, -1.0,  1.0], texture: [0.0, 1.0] },
		Self { position: [ 1.0, -1.0, -1.0,  1.0], texture: [1.0, 1.0] },
		Self { position: [-1.0,  1.0, -1.0,  1.0], texture: [0.0, 0.0] },
		Self { position: [ 1.0,  1.0, -1.0,  1.0], texture: [1.0, 0.0] },
		Self { position: [-1.0, -1.0,  1.0,  1.0], texture: [1.0, 1.0] },
		Self { position: [ 1.0, -1.0,  1.0,  1.0], texture: [0.0, 1.0] },
		Self { position: [-1.0,  1.0,  1.0,  1.0], texture: [1.0, 0.0] },
		Self { position: [ 1.0,  1.0,  1.0,  1.0], texture: [0.0, 0.0] },
	];

	/** Indices for drawing the cube, in triangle list mode. */
	pub const CUBE_INDICES: &'static [u32] = &[
		0, 2, 1, /* Front. */
		1, 2, 3,
		1, 3, 5, /* Right. */
		5, 3, 7,
		4, 6, 5, /* Back. */
		5, 6, 7,
		0, 2, 4, /* Left. */
		4, 2, 6,
		0, 1, 5, /* Top. */
		4, 0, 5,
		2, 3, 7, /* Bottom. */
		6, 2, 7
	];

	/** Generate the index buffer data for the cube. */
	pub fn cube_index_bytes() -> &'static [u8] {
		bytemuck::cast_slice(Self::CUBE_INDICES)
	}

	/** Generate the vertex buffer data for the cube. */
	pub fn cube_vertex_bytes() -> &'static [u8] {
		bytemuck::cast_slice(Self::CUBE_VERTICES)
	}
}

/** Platform-agnostic function that runs the real game code. */
fn run<F, G, W>(
	device: Device,
	event_loop: EventLoop<()>,
	mut window_id: W,
	mut swap: F,
	mut delta: G) -> !
	where F: 'static + FnMut(),
		  G: 'static + FnMut() -> Duration,
		  W: 'static + FnMut() -> WindowId {

	/* Load in the texture data. */
	let texture = image::load_from_memory_with_format(
		include_bytes!("goat.png"),
		image::ImageFormat::Png)
		.expect("could not load texture");
	let texture = texture.to_rgba8();

	let texture_width = texture.width();
	let texture_height = texture.height();
	let texture = texture.into_raw();

	let vertex_shader = device.create_vertex_shader(
		ShaderSource::Glsl(Cow::Borrowed(include_str!("quad.vert"))))
		.unwrap();
	let fragment_shader = device.create_fragment_shader(
		ShaderSource::Glsl(Cow::Borrowed(include_str!("quad.frag"))))
		.unwrap();
	let texture = device.create_texture_with_data(
		&TextureDescriptor {
			extent: TextureExtent::D2 { width: texture_width, height: texture_height },
			format: TextureFormat::Rgba8Unorm,
			mip: Mipmap::Automatic { filter: FilterType::Lanczos3 }
		},
		&texture)
		.unwrap();

	let vertices = device.create_vertex_buffer_with_data(
		&BufferDescriptor {
			size: u32::try_from(Vertex::cube_vertex_bytes().len()).unwrap(),
			profile: BufferProfile::StaticUpload
		},
		&Vertex::cube_vertex_bytes()).unwrap();
	let indices = device.create_index_buffer_with_data(
		&BufferDescriptor {
			size: u32::try_from(Vertex::cube_index_bytes().len()).unwrap(),
			profile: BufferProfile::StaticUpload
		},
		&Vertex::cube_index_bytes()).unwrap();

	let mut modelview = Matrix4::identity();
	let projection = Matrix4::perspective(1.0, 100.0);
	let combined = [modelview.transpose(), projection.transpose()];

	let matrices = device.create_uniform_buffer_with_data(
		&BufferDescriptor {
			size: u32::try_from(bytemuck::bytes_of(&combined).len()).unwrap(),
			profile: BufferProfile::DynamicUpload
		},
		&bytemuck::bytes_of(&combined))
		.unwrap();

	let uniforms = device.create_uniform_bind_group(
		&UniformGroupDescriptor {
			entries: &[
				UniformGroupEntry {
					binding: "goat".into(),
					kind: UniformBind::Texture {
						texture: &texture,
						far: TextureFilter::Linear,
						near: TextureFilter::Linear,
						anisotropy_clamp: Some(NonZeroU8::new(16).unwrap()),
					}
				},
				UniformGroupEntry {
					binding: "matrices".into(),
					kind: UniformBind::Buffer {
						buffer: &matrices
					}
				}
			]
		});

	let pipeline = device.create_render_pipeline(
		&RenderPipelineDescriptor {
			vertex: VertexState {
				shader: &vertex_shader,
				buffer: Vertex::LAYOUT
			},
			primitive_state: PrimitiveState {
				topology: PrimitiveTopology::TriangleList,
				index_format: IndexFormat::Uint32,
				front_face: FrontFace::Ccw,
				cull_mode: CullMode::None,
				polygon_mode: PolygonMode::Fill
			},
			fragment: Some(FragmentState {
				shader: &fragment_shader,
				targets: ColorTargetState {
					alpha_blend: BlendState::REPLACE,
					color_blend: BlendState::REPLACE,
					write_mask: ColorWrite::all()
				}
			}),
			depth_stencil: Some(DepthStencilState {
				depth_write_enabled: true,
				depth_compare: CompareFunction::Less,
				stencil: StencilState::IGNORE,
			})
		}).unwrap();
	let framebuffer = device.default_framebuffer(
		&DefaultFramebufferDescriptor {
			color_load_op: LoadOp::Clear(Color {
				red: 0.0,
				green: 0.0,
				blue: 0.0,
				alpha: 1.0
			}),
			depth_load_op: LoadOp::Clear(f32::INFINITY),
			stencil_load_op: LoadOp::Clear(0xff),
		});

	let mut viewport = Viewport {
		x: 0,
		y: 0,
		width: 800,
		height: 600
	};
	event_loop.run(move |event, _, control| {
		let mut pass_off = false;
		*control = ControlFlow::Poll;

		match event {
			Event::WindowEvent { event, window_id: target_window_id }
				if window_id() == target_window_id => {

				match event {
					WindowEvent::CloseRequested => *control = ControlFlow::Exit,
					WindowEvent::Resized(PhysicalSize { width, height }) => {
						viewport = Viewport {
							x: 0,
							y: 0,
							width,
							height
						};
					},
					WindowEvent::KeyboardInput { input, .. }
						if input.state == ElementState::Pressed => {
						if let Some(VirtualKeyCode::Escape) = input.virtual_keycode {
							*control = ControlFlow::Exit
						}
					}
					_ => {}
				}
			},
			Event::MainEventsCleared => pass_off = true,
			_ => {}
		}
		if !pass_off { return }

		let delta = delta();

		/* Update the rotation of the cube. */
		modelview =
			Matrix4::rotate_y(2.0 * std::f32::consts::PI * delta.as_secs_f32() * -0.5)
				* modelview;
		let modelview = Matrix4::translate(0.0, 0.0, -5.0) * modelview;
		{
			let slice = matrices.slice(..);
			let mut map = slice
				.try_map_mut(BufferLoadOp::DontCare)
				.unwrap();

			let combined = [modelview.transpose(), projection.transpose()];
			map.as_mut().copy_from_slice(bytemuck::bytes_of(&combined));
		}
		let mut pass = device.start_render_pass(&RenderPassDescriptor {
			pipeline: &pipeline,
			framebuffer: &framebuffer
		});
		pass.set_viewport(viewport);
		pass.set_index_buffer(&indices);
		pass.set_vertex_buffer(&vertices);
		pass.set_bind_group(&uniforms);
		pass.draw_indexed(
			0..u32::try_from(Vertex::CUBE_INDICES.len()).unwrap(),
			1);

		swap();
	})
}

/** Creates a new window and event loop pair. */
fn window() -> (EventLoop<()>, WindowBuilder) {
	let event_loop = winit::event_loop::EventLoop::new();
	let window = winit::window::WindowBuilder::default()
		.with_title("cube")
		.with_resizable(true)
		.with_inner_size(PhysicalSize {
			width: 800,
			height: 600
		});

	(event_loop, window)
}

#[cfg(not(target_arch = "wasm32"))]
fn main() {
	env_logger::init();
	let (event_loop, window_builder) = window();

	let windowed_context = glutin::ContextBuilder::new()
		.with_gl(glutin::GlRequest::Specific(glutin::Api::OpenGlEs, (3, 0)))
		.with_gl_profile(glutin::GlProfile::Core)
		.with_vsync(false)
		.with_multisampling(8)
		.build_windowed(window_builder, &event_loop)
		.expect("could not initialize opengl context");

	let context = match unsafe { windowed_context.make_current() } {
		Ok(context) => context,
		Err((_, what)) =>
			panic!("could not use the created opengl context: {}", what)
	};

	let device = gavle::Device::new_from_context(unsafe {
		gavle::Context::from_loader_function(|proc| {
			context.get_proc_address(proc) as *const _
		})
	}).unwrap();

	let (context, window) = unsafe { context.split() };

	use std::time::Instant;
	let mut now = Instant::now();
	let mut frames = 0u32;
	let mut dnow = Instant::now();

	run(
		device,
		event_loop,
		move || window.id(),
		move || context.swap_buffers().unwrap(),
		move || {
			let ndnow = Instant::now();
			let delta = ndnow.duration_since(dnow);
			dnow = ndnow;

			frames += 1;
			let elapsed = now.elapsed();
			if elapsed >= Duration::from_secs(1) {
				let fps = f64::from(frames) / elapsed.as_secs_f64();
				log::info!("FPS: {:.02}", fps);

				now = Instant::now();
				frames = 0;
			}

			delta
		});
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen::prelude::wasm_bindgen(start)]
pub fn wasm_start() {
	main()
}

#[cfg(target_arch = "wasm32")]
fn main() {
	std::panic::set_hook(Box::new(console_error_panic_hook::hook));

	console_log::init_with_level(log::Level::Trace)
		.expect("could not initialize logger");

	let (event_loop, window_builder) = window();
	let window = window_builder.build(&event_loop)
		.expect("could not create window");

	let canvas = winit::platform::web::WindowExtWebSys::canvas(&window);
	web_sys::window()
		.expect("no window element")
		.document()
		.expect("no document element")
		.body()
		.expect("document has no body")
		.append_child(&canvas)
		.expect("could not append canvas to body");

	use wasm_bindgen::JsCast;
	let context = canvas.get_context("webgl2")
		.unwrap()
		.unwrap()
		.dyn_into::<web_sys::WebGl2RenderingContext>()
		.unwrap();
	let context = gavle::Context::from_webgl2_context(context);

	run(
		Device::new_from_context(context).unwrap(),
		event_loop,
		move || window.id(),
		move || {},
		move || Duration::from_secs_f64(0.01666666666))
}

/** Matrix type. */
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
struct Matrix4([f32; 16]);
impl Matrix4 {
	pub fn translate(x: f32, y: f32, z: f32) -> Self { Self([
		1.0, 0.0, 0.0,   x,
		0.0, 1.0, 0.0,   y,
		0.0, 0.0, 1.0,   z,
		0.0, 0.0, 0.0, 1.0,
	])}

	pub fn identity() -> Self {
		Self([
			1.0, 0.0, 0.0, 0.0,
			0.0, 1.0, 0.0, 0.0,
			0.0, 0.0, 1.0, 0.0,
			0.0, 0.0, 0.0, 1.0,
		])
	}

	pub fn perspective(n: f32, f: f32) -> Self {
		Self([
			1.0, 0.0,  0.0,                0.0,
			0.0, 1.0,  0.0,                0.0,
			0.0, 0.0, -(f + n) / (f - n), -2.0 * f * n / (f - n),
			0.0, 0.0, -1.0,                0.0
		])
	}

	pub fn rotate_y(y: f32) -> Self {
		Self([
			f32::cos(y), 0.0, f32::sin(y), 0.0,
			0.0, 1.0, 0.0, 0.0,
			-f32::sin(y), 0.0, f32::cos(y), 0.0,
			0.0, 0.0, 0.0, 1.0
		])
	}

	pub fn transpose(mut self) -> Self {
		let a = |i: usize, j: usize| i * 4 + j;

		for i in 0..4 {
			for j in 0..i {
				let x = self.0[a(i, j)];
				let y = self.0[a(j, i)];

				self.0[a(i, j)] = y;
				self.0[a(j, i)] = x;
			}
		}

		self
	}
}
impl std::ops::Mul for Matrix4 {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self::Output {
		let a = |i: usize, j: usize| self.0[i * 4 + j];
		let b = |i: usize, j: usize| rhs.0[i * 4 + j];

		Self([
			(a(0, 0) * b(0, 0)) + (a(0, 1) * b(1, 0)) + (a(0, 2) * b(2, 0)) + (a(0, 3) * b(3, 0)),
			(a(0, 0) * b(0, 1)) + (a(0, 1) * b(1, 1)) + (a(0, 2) * b(2, 1)) + (a(0, 3) * b(3, 1)),
			(a(0, 0) * b(0, 2)) + (a(0, 1) * b(1, 2)) + (a(0, 2) * b(2, 2)) + (a(0, 3) * b(3, 2)),
			(a(0, 0) * b(0, 3)) + (a(0, 1) * b(1, 3)) + (a(0, 2) * b(2, 3)) + (a(0, 3) * b(3, 3)),
			(a(1, 0) * b(0, 0)) + (a(1, 1) * b(1, 0)) + (a(1, 2) * b(2, 0)) + (a(1, 3) * b(3, 0)),
			(a(1, 0) * b(0, 1)) + (a(1, 1) * b(1, 1)) + (a(1, 2) * b(2, 1)) + (a(1, 3) * b(3, 1)),
			(a(1, 0) * b(0, 2)) + (a(1, 1) * b(1, 2)) + (a(1, 2) * b(2, 2)) + (a(1, 3) * b(3, 2)),
			(a(1, 0) * b(0, 3)) + (a(1, 1) * b(1, 3)) + (a(1, 2) * b(2, 3)) + (a(1, 3) * b(3, 3)),
			(a(2, 0) * b(0, 0)) + (a(2, 1) * b(1, 0)) + (a(2, 2) * b(2, 0)) + (a(2, 3) * b(3, 0)),
			(a(2, 0) * b(0, 1)) + (a(2, 1) * b(1, 1)) + (a(2, 2) * b(2, 1)) + (a(2, 3) * b(3, 1)),
			(a(2, 0) * b(0, 2)) + (a(2, 1) * b(1, 2)) + (a(2, 2) * b(2, 2)) + (a(2, 3) * b(3, 2)),
			(a(2, 0) * b(0, 3)) + (a(2, 1) * b(1, 3)) + (a(2, 2) * b(2, 3)) + (a(2, 3) * b(3, 3)),
			(a(3, 0) * b(3, 0)) + (a(3, 1) * b(1, 0)) + (a(3, 2) * b(2, 0)) + (a(3, 3) * b(3, 0)),
			(a(3, 0) * b(3, 1)) + (a(3, 1) * b(1, 1)) + (a(3, 2) * b(2, 1)) + (a(3, 3) * b(3, 1)),
			(a(3, 0) * b(3, 2)) + (a(3, 1) * b(1, 2)) + (a(3, 2) * b(2, 2)) + (a(3, 3) * b(3, 2)),
			(a(3, 0) * b(3, 3)) + (a(3, 1) * b(1, 3)) + (a(3, 2) * b(2, 3)) + (a(3, 3) * b(3, 3)),
		])
	}
}
