# Gavle

Gavle is an OpenGL wrapper crate modeled after wgpu.

This crate aims to bring many of the benefits of stateless graphics APIs such
as WebGPU directly to OpenGL, so that projects that, for whatever reason, do 
not have the choice of a more modern API may still enjoy many of the workflow 
benefits that come with using them.

## Goals

The primary goal of this crate is to facilitate development workflow by removing
all direct interaction between user code and the global state in OpenGL, as well
as by tracking ownership and mutability of objects in device memory. This allows
gavle to also catch many would-be bugs by raising hard errors in instances where
they would happen.

This crate also takes the road of being more oppinionated about which features 
it provides and how it provides them, electing to support only a small but - 
hopefully - robust set of them. The reason is two-fold: Firstly, because 
supporting the whole kitchen sink in a consistent and safe way is impractical
for any project of this size, as demonstrated by Glium, and, secondly, because
it would jeopardize the next goal: that of interoperability.

This crate is designed so as to allow users to transparently run the same 
graphics code in every major platform Rust supports that also supports some form 
of OpenGL. It can run on top of WebGL, OpenGL ES, as well as standard OpenGL
and necessitates no more than a recompile and some glue code providing it with a 
valid context.

## Non-goals

Unfortunately, though expectedly, this crate has to leave out many of the 
features that make modern OpenGL viable for high-level projects - chief among
which being SSBOs and compute shaders - as they are simply not available in 
WebGL. If you need these features, I would heavily discourage the use of this 
crate.

## Features
This create supports the following features:
- [X] Ownership and mutability tracking for all GPU objects.
- [X] Garbage collected GPU objects.
- [X] Memory-mapped GPU buffer editing.
- [X] Full support for WebGL and OpenGL ES.

## Acknowledgements
Many of the structures in this crate were lifted off WebGPU and I feel it's
important to state that without the efforts that were put into that project this
crate would certainly have much different ergonomics than the ones it does 
currently. 

## On Versioning
This crate was first written as part of a larger project which is still ongoing
and has not yet been published, and, thus, the stage of developemnt this crate
is in should probably not be gauged from its version number alone.