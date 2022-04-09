Xtreme3D v3
===========
Xtreme3D is a cross-language 3D graphics engine based on GLScene. It has a rich feature set, including custom shaders, wide variety of asset formats support, built-in effects, physics, collision and ray casting.

Xtreme3D originally was created for Game Maker. It makes possible to create full-featured 3D games with modern graphics and reasonable framerate in a tool that was intended mainly for 2D games and provides limited built-in 3D capabilities. Original Xtreme3D was a closed-source project, and it was abandoned by the author. Xtreme3D 2.0.2.0 (2006) was the last official release. So we in Russian Xtreme3D community made an attempt to rewrite the engine, maintaining the API as close to original as possible. This work began in 2009 and was completed in 2016. This project is not connected in any way with the original Xtreme3D developer, all code was written from scratch.

The engine is written in Delphi and based on a modified version of [GLScene](https://sourceforge.net/projects/glscene) 1.0.0.0714. The code should be compiled with Delphi 7 (other Delphi versions are not tested).

You can use Xtreme3D with almost any programming language. We already provide [Python](https://github.com/xtreme3d/xtreme3d/tree/master/bindings/python), [D](https://github.com/xtreme3d/xtreme3d/tree/master/bindings/dlang) and [LuaJIT](https://github.com/xtreme3d/xtreme3d/tree/master/bindings/lua) bindings which are automatically generated from engine's source code.

Visit Xtreme3D site at [https://xtreme3d.ru](https://xtreme3d.ru).

Screenshots
-----------
[![Screenshot3](/screenshots/pbr-thumb.jpg)](/screenshots/pbr.jpg)
[![Screenshot1](/screenshots/shadows-thumb.jpg)](/screenshots/shadows.jpg)
[![Screenshot4](/screenshots/water2-thumb.jpg)](/screenshots/water2.jpg)
[![Screenshot4](/screenshots/bumpshadows-thumb.jpg)](/screenshots/bumpshadows.jpg)
[![Screenshot4](/screenshots/x3d-3.0-rain-thumb.jpg)](/screenshots/x3d-3.0-rain.jpg)
[![Screenshot4](/screenshots/darkarts-thumb.jpg)](/screenshots/darkarts.jpg)

Features
--------
* Extremely lightweight. It's just a DLL of 2 MB in size (plus three optional DLLs - ode.dll, freetype.dll, OpenFBX.dll)
* Can be used with any language that supports DLLs and stdcall convension
* Scene graph with object hierarchy/pivoting
* Built-in primitives (such as box, sphere, cylinder, teapot, etc.)
* Materials and textures. Materials can be loaded from script files
* GLSL shaders
* Built-in easy to use shaders, including cel shader, bump/parallax shader, phong shader, multimaterial shader, etc.
* Multitexturing. Materials can have up to 8 texture slots accessible from shaders. Fixed-pipeline multipass multitexturing is also supported
* Popular image formats support for textures (BMP, JPG, PNG, TGA, DDS)
* Various 3D model formats support (3DS, OBJ, LWO, B3D, MD2, MD3, MD5, SMD, FBX and many other)
* Procedural mesh construction from vertex/normal/texcoord/index lists - you are free to design your own model format
* Model animation, per-vertex and skeletal
* Lightmapping and dynamic lighting
* Terrain with automatic LOD and real-time editing
* Dynamic water rendering
* Dynamic soft shadows
* Static and dynamic cubemaps
* Particle system for fire-like and lighting-like effects
* Lensflare effect
* 2D shapes, including rectangle, circle, line, and custom 2D mesh
* 2D sprites and text with TTF and Unicode support
* 3D text and billboards
* Skyboxes
* Procedural skydome with sun, stars and animated day/night cycle
* Procedural trees
* Lines and curves rendering (Cubic, Bezier, NURBS)
* Curves can be used to control object movement
* Discrete LOD system
* Offscreen rendering via p-buffers or FBOs
* Collision detection between bounding spheres, oriented bounding boxes and meshes
* Raycasting
* Built-in game-oriented collision systems (DCE, FPSManager)
* Integrated rigid body physics using ODE engine
* [Kraft](https://github.com/BeRo1985/kraft) physics engine integration
* Ragdolls
* Octree and quadtree to speed-up rendering and collision tests
* Loading resources from GLScene PAK archives
* Keyboard and mouse input
* Window creation and management
* ...plus a lot more!

Documentation
-------------
Xtreme3D is fully documented in Russian, the help file is [here](https://github.com/xtreme3d/xtreme3d/tree/master/doc-ru). Machine-translated English version is also [available](https://github.com/xtreme3d/xtreme3d/tree/master/doc-en) (thanks to Bill Collins for making it).

Asset Creation
--------------
For static geometry with lightmaps (such as levels), we recommend using B3D (Blitz3D) format. You can use B3D exporter for Blender to create models in this format - find it in tools/blender-b3d-exporter.

For animated geometry, we recommend either SMD (Half-Life) or MD5 (Doom 3) formats. You can use MD5 exporter for Blender to create models in this format - find it in tools/blender-md5-exporter.

License
-------
Xtreme3D 3.x and corresponding GLScene fork are both distributed under Mozilla Public License 1.1. This license allows you to create commercial closed-source applications with the engine.

ODE is distributed under BSD-style license.

FreeType is distributed under FreeType License (FTL).

OpenFBX is distributed under MIT license.
