use glow::{Context, HasContext};
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::borrow::Cow;
use std::collections::HashSet;
use std::str::FromStr;

/** Queries for a parameter with an `i32` result, checking whether it is
 * supported and, if it is not, returns `None`. */
unsafe fn checked_get_parameter_i32(
	gl: &Context,
	parameter: u32) -> Option<i32> {

	let val = gl.get_parameter_i32(parameter);
	match gl.get_error() {
		glow::NO_ERROR => Some(val),
		glow::INVALID_ENUM => None,
		what =>
			panic!("unexpected glError() value after glGet(0x{:08x}): \
				0x{:08x}",
				parameter,
				what)
	}
}

/** Information on a context. */
#[derive(Debug, Clone, PartialEq)]
pub struct Information {
	/** Version and profile of the current context. */
	pub version: Version,
	/** Capabilities of this context. */
	pub capabilities: Capabilities,
	/** Limits of this context. */
	pub limits: Limits,
	/** Features of this context. */
	pub features: Features
}
impl Information {
	/** Minimum supported version of the OpenGL Core specification. */
	const MIN_CORE: Release = Release { major: 4, minor: 3 };

	/** Minimum supported version of the OpenGL ES specification. */
	const MIN_ES: Release = Release { major: 3, minor: 0 };

	/** Minimum supported version of the WebGL specification. */
	const MIN_WEB: Release = Release { major: 2, minor: 0 };

	/** Collect information on the given context and check whether it is
	 * supported by the Gavle implementation or not. */
	pub fn collect(context: &Context) -> Result<Self, UnsupportedContext> {
		let gl = context;
		let (version, major, minor) = unsafe {(
			gl.get_parameter_string(glow::VERSION),
			checked_get_parameter_i32(gl, glow::MAJOR_VERSION),
			checked_get_parameter_i32(gl, glow::MINOR_VERSION),
		)};
		debug!("Reported OpenGL Version String: {}", version);
		debug!("Reported OpenGL Version: {:?}.{:?}", major, minor);

		/* Parse the version string. */
		let version = Version::parse(&version)
			.map_err(|_| UnsupportedContext::InvalidVersion(version.clone()))?;

		/* Check if the release value given to us by the dedicated function
		 * matches that of the version string. */
		let dedicated = (
			major.map(|major| u32::try_from(major)),
			minor.map(|minor| u32::try_from(minor)));
		match dedicated {
			(Some(Ok(major)), Some(Ok(minor))) => {
				let release = Release { major, minor };
				if release != version.release {
					return Err(UnsupportedContext::MismatchedRelease {
						string: (version.release.major, version.release.minor),
						dedicated: (major, minor)
					})
				}
			},
			(None, None) => warn!("implementation does not support dedicated \
				version query targets. we will rely solely on the version \
				string, which may not be as accurate"),
			_ => return Err(UnsupportedContext::InvalidRelease(major, minor))
		}

		/* Check if the release is supported. */
		match version.profile {
			Profile::Core if version.release >= Self::MIN_CORE => {},
			Profile::Es   if version.release >= Self::MIN_ES   => {},
			Profile::Web  if version.release >= Self::MIN_WEB  => {},
			_ => return Err(UnsupportedContext::UnsupportedRelease {
				profile: version.profile,
				release: (version.release.major, version.release.minor)
			})
		}

		/* Enumerate all of the available extensions. */
		let mut extensions = HashSet::new();
		let _ = unsafe { Extension::enumerate(gl, &mut extensions) }?;

		debug!("Discovered {} extensions: ", extensions.len());
		for extension in &extensions {
			debug!("    - {}", extension)
		}

		/* Gather capability information. */
		let capabilities = Capabilities {
			buffer_mapping: version.profile != Profile::Web,
		};
		let limits = Limits::collect(context)?;
		let features = Features {
			sampler_anisotropy:
				extensions.contains(&Extension::EXT_TEXTURE_FILTER_ANISOTROPIC),
			readonly_framebuffer_feedback:
				   version.profile == Profile::Core
				&& version.release >= Release { major: 4, minor: 5 }
		};

		/* Check whether the limits are available for all of the available
		 * optional features. */
		if features.sampler_anisotropy && limits.max_sampler_anisotropy.is_none() {
			return Err(UnsupportedContext::MissingMaxSamplerAnisotropy)
		}

		Ok(Self {
			version,
			capabilities,
			limits,
			features
		})
	}
}

/** Normalized extension name. */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Extension {
	/** The name of the vendor responsible for this extension. */
	vendor: Cow<'static, str>,
	/** The name of the extension itself. */
	extension: Cow<'static, str>,
}
impl Extension {
	/** Support for anisotropic filtering in texture samplers.
	 *
	 * Registry entry:
	 * https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_filter_anisotropic.txt.
	 */
	pub const EXT_TEXTURE_FILTER_ANISOTROPIC: Self =
		Self {
			vendor: Cow::Borrowed("EXT"),
			extension: Cow::Borrowed("texture_filter_anisotropic")
		};

	/** Support for explicit memory barriers.
	 *
	 * Registry entry:
	 * https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_barrier.txt.
	 */
	pub const ARB_TEXTURE_BARRIER: Self =
		Self {
			vendor: Cow::Borrowed("ARB"),
			extension: Cow::Borrowed("texture_barrier")
		};
}
/** Implements parsing for OpenGL extension strings.
 *
 * Keep in mind that this parser is implemented with a very strict, albeit
 * unofficial idea of what a valid OpenGL extension name string is. This is due
 * to how the OpenGL ES specification does not define what is a valid string and
 * what is not.
 *
 * This leaves us with strictly demanding something that looks like what most
 * implementations are doing, in the form of an optional, all uppercase "GL_"
 * suffix, followed by an all upper case "<vendor>_" field, followed by mixed
 * case "<extension>" field. */
impl FromStr for Extension {
	type Err = ExtensionParseError;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		/* Make sure the string only contains valid characters. */
		let valid = s.chars()
			.find(|c| {
				let valid =
					    c.is_ascii_alphanumeric()
					|| *c == '_';
				!valid
			})
			.is_none();
		if !valid {
			return Err(Self::Err::InvalidCharacters)
		}

		/* Ignore the GL_ prefix, if it's present. */
		let s = s.strip_prefix("GL_").unwrap_or(s);

		/* Everything up to the first underscore is the vendor name. */
		let (vendor, extension) = s.split_once("_")
			.ok_or(Self::Err::NotEnoughFields)?;

		if &vendor.to_ascii_uppercase() != vendor {
			return Err(Self::Err::NonUppercaseVendor)
		}

		Ok(Self {
			vendor: Cow::Owned(vendor.to_string()),
			extension: Cow::Owned(extension.to_string())
		})
	}
}
impl Extension {
	/** Enumerate all of the available extensions using the given context handle. */
	unsafe fn enumerate(
		gl: &glow::Context,
		target: &mut impl Extend<Extension>) -> Result<usize, UnsupportedContext> {

		let extension_count = gl.get_parameter_i32(glow::NUM_EXTENSIONS);
		let num_supported = match gl.get_error() {
			glow::INVALID_ENUM => false,
			glow::NO_ERROR => true,
			glow::INVALID_VALUE =>
				panic!("glGetv(0x{:08x}) is out of range",
					glow::NUM_EXTENSIONS),
			what =>
				panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
					glow::NUM_EXTENSIONS,
					what)
		};

		let mut count = 0usize;
		if num_supported {
			for index in 0..extension_count {
				let extension = gl.get_parameter_indexed_string(
					glow::EXTENSIONS,
					index.try_into().unwrap());
				match gl.get_error() {
					glow::INVALID_ENUM =>
						return Err(UnsupportedContext::ExtensionEnumerationFailed),
					glow::NO_ERROR => {},
					glow::INVALID_VALUE =>
						panic!("glGetv(0x{:08x}, index: {}) is out of range",
							glow::NUM_EXTENSIONS,
							index),
					what =>
						panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
							glow::NUM_EXTENSIONS,
							what)
				}

				let extension = extension.trim().parse()
					.expect("Invalid extension name");
				target.extend(std::iter::once(extension));

				count += 1;
			}
		} else {
			warn!("Probing the extension count with GL_NUM_EXTENSIONS is not \
				supported. Falling back to pulling the combined extension \
				string using glGetString(GL_EXTENSIONS)");
			let combined = gl.get_parameter_string(glow::EXTENSIONS);
			let direct_supported  = match gl.get_error() {
				glow::INVALID_ENUM => false,
				glow::NO_ERROR => true,
				glow::INVALID_VALUE =>
					panic!("glGetv(0x{:08x}) is out of range",
						glow::NUM_EXTENSIONS),
				what =>
					panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
						glow::NUM_EXTENSIONS,
						what)
			};

			if direct_supported {
				let iterator = combined.split_ascii_whitespace()
					.map(|slice| slice.to_string())
					.map(|string| string.parse().expect("Invalid extension name"))
					.inspect(|_| count += 1);
				target.extend(iterator);
			} else {
				warn!("Probing the extension count with GL_EXTENSIONS is not \
					supported. We're probably running under WebGL, so, we're \
					falling back to glow::HasContext::supported_extensions()");
				let iterator = gl.supported_extensions()
					.iter()
					.map(|name| name.parse()
						.expect("Invalid extension name"));

				target.extend(iterator);
			}
		}

		Ok(count)
	}
}
impl std::fmt::Display for Extension {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "GL_{}_{}", &self.vendor, &self.extension)
	}
}

/** Errors that may occur during the parsing of an extension name. */
#[derive(Debug, thiserror::Error)]
pub enum ExtensionParseError {
	/** This error occurs when the string naming an extension contains invalid,
	 * unrecognized characters. */
	#[error("Invalid characters in extension name")]
	InvalidCharacters,
	/** This error occurs when a given extension string doesn't have enough
	 * fields to name both the vendor and the extension. */
	#[error("Not enough fields for a valid extension name")]
	NotEnoughFields,
	/** This error occurs when the name of the vendor is not all in the upper
	 * case, as is required by our parser, in spite of the lack of an official
	 * requirement for that. */
	#[error("The vendor name is not in upper case letters")]
	NonUppercaseVendor,
}

/** Capabilities of a given context.
 *
 * None of these limit what the user may do with the API, instead, these
 * capabilities are meant to allow the library to internally select a faster
 * code path whenever the implementation supports it. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Capabilities {
	/** Whether the context supports direct mapping of buffers to host memory. */
	pub buffer_mapping: bool,
}

/** Features of a given context.
 *
 * Unlike with the capabilities, features are limiting, and using features which
 * are not supported on the target context is considered an error. Therefore, it
 * is important for users to understand these features and limitations before
 * enabling something. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Features {
	/** Whether anisotropic filtering is supported by the context. */
	pub sampler_anisotropy: bool,
	/** Whether it's possible to attach a read-only framebuffer element to an
	 * active texture sampler slot.
	 *
	 * This allows for what is considered in older OpenGL contexts to be a
	 * framebuffer feedback loop to be constructed, but it is not a widely
	 * supported feature. */
	pub readonly_framebuffer_feedback: bool,
}

/** Limits on the amount of elements a given context supports. */
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Limits {
	/** Maximum number of texture units available to the user for a given draw
	 * command. This is the maximum number of texture attachments a bind group
	 * is allowed to have. */
	pub max_textures: u32,
	/** The maximum extent of each of the axes in a 1D or 2D texture, measured
	 * in pixels. */
	pub max_texture_size: u32,
	/** The maximum extent of each of the axes in a 3D texture, measured in
	 * pixels. */
	pub max_texture_size_3d: u32,
	/** The maximum number of layers allowed in a 2D array texture. The maximum
	 * size of the individual layers is [`max_texture_size`]. */
	pub max_texture_layers: u32,
	/** Maximum number of uniform blocks available to the user for a given draw
	 * command. This is the maximum number of uniform buffers a bind group
	 * is allowed to have. */
	pub max_uniform_block_bindings: u32,
	/** The maximum size of a single bound uniform block. Uniform blocks larger
	 * than this number cannot be used in bind groups. */
	pub max_uniform_block_size: u32,
	/** The maximum number of color attachments a framebuffer is allowed to
	 * have. */
	pub max_framebuffer_color_attachments: u32,
	/** The maximum width of a framebuffer attachment, measured in pixels. */
	pub max_framebuffer_attachment_width: Option<u32>,
	/** The maximum height of a framebuffer attachment, measured in pixels. */
	pub max_framebuffer_attachment_height: Option<u32>,
	/** The maximum width of the viewport at any given time. */
	pub max_viewport_width: Option<u32>,
	/** The maximum height of the viewport at any given time. */
	pub max_viewport_height: Option<u32>,
	/** The maximum value of allowed for the anisotropy clamp. */
	pub max_sampler_anisotropy: Option<f32>,
}
impl Limits {
	fn collect(gl: &Context) -> Result<Self, UnsupportedContext> {
		let try_ensure_u32_indexed = |param: u32, index: u32| {
			let value = unsafe {
				let val = gl.get_parameter_indexed_i32(param, index);
				match gl.get_error() {
					glow::INVALID_ENUM => return Ok(None),
					glow::NO_ERROR => {},
					glow::INVALID_VALUE =>
						panic!("glGetv(0x{:08x}, index: {}) is out of range",
							param,
							index),
					what =>
						panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
							param,
							what)
				}
				val
			};
			u32::try_from(value)
				.map(|value| Some(value))
				.map_err(|_| UnsupportedContext::InvalidParameter {
					value,
					parameter: param
				})
		};
		let try_ensure_u32 = |param: u32| {
			let value = unsafe {
				let val = gl.get_parameter_i32(param);
				match gl.get_error() {
					glow::INVALID_ENUM => return Ok(None),
					glow::NO_ERROR => {},
					what =>
						panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
							param,
							what)
				}
				val
			};
			u32::try_from(value)
				.map(|value| Some(value))
				.map_err(|_| UnsupportedContext::InvalidParameter {
					value,
					parameter: param
				})
		};
		let ensure_u32 = |param: u32| {
			let value = unsafe {
				let val = gl.get_parameter_i32(param);
				match gl.get_error() {
					glow::INVALID_ENUM => return Err(
						UnsupportedContext::UnsupportedParameter {
							parameter: param
						}),
					glow::NO_ERROR => {},
					what =>
						panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
							param,
							what)
				}
				val
			};
			u32::try_from(value)
				.map_err(|_| UnsupportedContext::InvalidParameter {
					value,
					parameter: param
				})
		};
		let try_ensure_f32 = |param: u32| {
			let value = unsafe {
				let val = gl.get_parameter_f32(param);
				match gl.get_error() {
					glow::INVALID_ENUM => return Ok(None),
					glow::NO_ERROR => {},
					what =>
						panic!("glGet(0x{:08x}) returned error code 0x{:08x}",
							param,
							what)
				}
				val
			};
			Ok(Some(value))
		};

		Ok(Self {
			/* Texture limits block. */
			max_textures: ensure_u32(glow::MAX_COMBINED_TEXTURE_IMAGE_UNITS)?,
			max_texture_size: ensure_u32(glow::MAX_TEXTURE_SIZE)?,
			max_texture_size_3d: ensure_u32(glow::MAX_3D_TEXTURE_SIZE)?,
			max_texture_layers: ensure_u32(glow::MAX_ARRAY_TEXTURE_LAYERS)?,

			/* Uniform buffer limits block. */
			max_uniform_block_bindings: ensure_u32(glow::MAX_UNIFORM_BUFFER_BINDINGS)?,
			max_uniform_block_size: ensure_u32(glow::MAX_UNIFORM_BLOCK_SIZE)?,

			/* Framebuffer limits block. */
			max_framebuffer_color_attachments: ensure_u32(glow::MAX_COLOR_ATTACHMENTS)?,
			max_framebuffer_attachment_width: try_ensure_u32(glow::MAX_FRAMEBUFFER_WIDTH)?,
			max_framebuffer_attachment_height: try_ensure_u32(glow::MAX_FRAMEBUFFER_HEIGHT)?,
			max_viewport_width: try_ensure_u32_indexed(glow::MAX_VIEWPORT_DIMS, 0)?,
			max_viewport_height: try_ensure_u32_indexed(glow::MAX_VIEWPORT_DIMS, 1)?,
			max_sampler_anisotropy: try_ensure_f32(glow::MAX_TEXTURE_MAX_ANISOTROPY_EXT)?,
		})
	}
}

/** Version information of a context. */
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Version {
	/** Implementation profile of the context. */
	pub profile: Profile,
	/** Release number of the implementation profile. */
	pub release: Release,
	/** Vendor specific information included in the string, if any. */
	pub vendor: String,
}
impl Version {
	/** Try to parse version information from a version string. */
	fn parse(string: &str) -> Result<Self, &str> {
		let (profile, string) = Profile::parse(string)?;
		let (release, string) = Release::parse(string)?;
		let vendor = string.trim().to_string();

		Ok(Self { profile, release, vendor })
	}
}

/** Types of OpenGL implementation profile and their version numbers. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Profile {
	/** This is running a desktop OpenGL implementation. */
	Core,
	/** This is running an OpenGL ES implementation. */
	Es,
	/** This is running a WebGL implementation. */
	Web
}
impl Profile {
	/** Try to parse an implementation profile from a version string. */
	fn parse(string: &str) -> Result<(Self, &str), &str> {
		let string = string.trim_start();

		const WEB_SIGNATURE: &'static str = "WebGL ";
		const ES_SIGNATURE: &'static str = "OpenGL ES ";

		if string.is_empty() {
			/* Empty version strings are invalid by definition. */
			Err(string)
		} else if string.starts_with(WEB_SIGNATURE) {
			Ok((
				Self::Web,
				string.split_at(WEB_SIGNATURE.len()).1
			))
		} else if string.starts_with(ES_SIGNATURE) {
			Ok((
				Self::Es,
				string.split_at(ES_SIGNATURE.len()).1
			))
		} else if string.chars().next().unwrap().is_numeric() {
			/* Core just requires a numeric character here. */
			Ok((
				Self::Core,
				string
			))
		} else {
			/* Invalid version string. */
			Err(string)
		}
	}
}
impl std::fmt::Display for Profile {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Profile::Core =>
				write!(f, "OpenGL"),
			Profile::Es =>
				write!(f, "OpenGL ES"),
			Profile::Web =>
				write!(f, "WebGL")
		}
	}
}

/** Release information of an OpenGL implementation. This is the "version" part
 * of the version string. */
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Release {
	/** Major version of this release. */
	pub major: u32,
	/** Minor version of this release. */
	pub minor: u32,
}
impl Release {
	/** Try to parse release information from a version string. */
	fn parse(string: &str) -> Result<(Self, &str), &str> {
		let (major, minor) = string.split_once(".")
			.ok_or(string)?;

		let split = minor
			.chars()
			.enumerate()
			.find_map(|(index, char)| if char.is_ascii() && char.is_numeric() {
				None
			} else {
				Some(index)
			});
		let (minor, next) = match split {
			Some(split) => minor.split_at(split),
			None => (minor, "")
		};

		/* There might still be revision information that we don't care about.
		 *
		 * Since that is technically still release information, we should
		 * consume it here so that the next stage of the parse doesn't have to
		 * deal with release information we don't find useful.
		 */
		let cutoff = next
			.chars()
			.enumerate()
			.find_map(|(index, char)| {
				if char.is_ascii() && (char.is_numeric() || char == '.') {
					None
				} else {
					Some(index)
				}
			});
		let next = match cutoff {
			Some(cutoff) => next.split_at(cutoff).1,
			None => ""
		};

		let result = Self {
			major: u32::from_str_radix(major, 10).map_err(|_| string)?,
			minor: u32::from_str_radix(minor, 10).map_err(|_| string)?,
		};

		Ok((result, next))
	}
}
impl PartialOrd for Release {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		match self.major.partial_cmp(&other.major) {
			None | Some(Ordering::Equal) => {},
			Some(ordering) => return Some(ordering),
		}
		self.minor.partial_cmp(&other.minor)
	}
}

impl Ord for Release {
	fn cmp(&self, other: &Self) -> Ordering {
		match self.major.cmp(&other.major) {
			Ordering::Equal => {},
			ordering => return ordering,
		}
		self.minor.cmp(&other.minor)
	}
}

/** Error type used when the context used to create [the Gavle device] is not
 * supported by the library as a whole. This should only be the case for
 * somewhat old contexts.
 *
 * The minimum required OpenGL versions for this library are.
 * - `OpenGL Core 4.0`
 * - `OpenGL ES 3.0`
 * - `WebGL 2`
 *
 */
#[derive(Debug, thiserror::Error)]
pub enum UnsupportedContext {
	#[error("the version string \"{0}\" is invalid")]
	InvalidVersion(String),
	#[error("the release {0:?}.{1:?} is invalid")]
	InvalidRelease(Option<i32>, Option<i32>),
	#[error("the release given by the version string ({string:?}) \
		differ from the one gathered with dedicated calls ({dedicated:?})")]
	MismatchedRelease {
		/** The release number given by the version string. */
		string: (u32, u32),
		/** The release number given by the dedicated calls. */
		dedicated: (u32, u32)
	},
	#[error("the valued returned by glGet(0x{parameter:08x}) is invalid: \
		{value}")]
	InvalidParameter {
		/** Returned invalid parameter value. */
		value: i32,
		/** OpenGL parameter enum value. */
		parameter: u32,
	},
	#[error("the required paremeter 0x{parameter:08x} is not supported")]
	UnsupportedParameter {
		/** OpenGL parameter enum value. */
		parameter: u32,
	},
	#[error("{profile} {release:?} is not supported")]
	UnsupportedRelease {
		/** The profile to which belongs the unsupported release. */
		profile: Profile,
		/** The exact number of the unsupported release. */
		release: (u32, u32)
	},
	#[error("extension enumeration is not supported")]
	ExtensionEnumerationFailed,
	#[error("sampler anisotropy is available, however, the implementation does \
		not provide us with a maximum sampler anisotropy")]
	MissingMaxSamplerAnisotropy,
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn profile() {
		assert_eq!(
			Profile::parse("4.6.0 NVIDIA 457.51"),
			Ok((Profile::Core, "4.6.0 NVIDIA 457.51")));
		assert_eq!(Profile::parse("OpenGL ES 3.0"), Ok((Profile::Es, "3.0")));
		assert_eq!(Profile::parse("WebGL 2.0"), Ok((Profile::Web, "2.0")))
	}

	#[test]
	fn release() {
		assert_eq!(
			Release::parse("4.6.0 NVIDIA 457.51"),
			Ok((Release { major: 4, minor: 6 }, " NVIDIA 457.51")));
		assert_eq!(Release::parse("3.0"), Ok((Release { major: 3, minor: 0 }, "")));
		assert_eq!(Release::parse("2.0"), Ok((Release { major: 2, minor: 0 }, "")));
	}

	#[test]
	fn version() {
		assert_eq!(
			Version::parse("4.6 NVIDIA 457.51"),
			Ok(Version {
				profile: Profile::Core,
				release: Release { major: 4, minor: 6 },
				vendor: "NVIDIA 457.51".to_string()
			}));
		assert_eq!(
			Version::parse("OpenGL ES 3.0"),
			Ok(Version {
				profile: Profile::Es,
				release: Release { major: 3, minor: 0 },
				vendor: "".to_string()
			}));
		assert_eq!(
			Version::parse("WebGL 2.0"),
			Ok(Version {
				profile: Profile::Web,
				release: Release { major: 2, minor: 0 },
				vendor: "".to_string()
			}));
	}
}
