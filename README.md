Xtreme3D v3
===========
Xtreme3D is a 3D graphics engine for Game Maker 8. It makes possible to create full-featured 3D games with modern graphics and reasonable framerate in Game Maker, a game design tool that was intended mainly for 2D games and provides only limited 3D capabilities.

Original Xtreme3D was a closed-source project, and it was abandoned by the author. Xtreme3D 2.0.2.0 (2006) was the last official release. So we in Russian Xtreme3D community made an attempt to rewrite the engine, maintaining the API as close to original as possible. This work began back in 2009 and was completed in 2016. This project is not connected in any way with the original Xtreme3D developer, all code was written from scratch.

The engine is written in Delphi and based on a modified version of GLScene 1.0.0.0714. The code is compatible with Delphi 7 and higher.

Visit our Russian Xtreme3D site at http://xtreme3d.narod.ru

Screenshots
-----------
[![Screenshot3](/screenshots/pbr-thumb.jpg)](/screenshots/pbr.jpg)
[![Screenshot1](/screenshots/shadows-thumb.jpg)](/screenshots/shadows.jpg)
[![Screenshot4](/screenshots/water2-thumb.jpg)](/screenshots/water2.jpg)
[![Screenshot4](/screenshots/bumpshadows-thumb.jpg)](/screenshots/bumpshadows.jpg)
[![Screenshot4](/screenshots/x3d-3.0-rain-thumb.jpg)](/screenshots/x3d-3.0-rain.jpg)
[![Screenshot4](/screenshots/superdemo-thumb.jpg)](/screenshots/superdemo.jpg)

Features
--------
* Scene graph with object hierarchy/pivoting
* Built-in primitives (such as box, sphere, cylinder, teapot, etc.)
* Materials and textures. Materials can be loaded from script files
* GLSL shaders
* Built-in easy to use shaders, including cel shader, bump/parallax shader, phong shader, multimaterial shader, etc.
* Multitexturing. Materials can have up to 8 texture slots accessible from shaders. Fixed-pipeline multipass multitexturing is also supported
* Popular image formats support for textures (BMP, JPG, PNG, TGA, DDS)
* Various 3D model formats support (3DS, OBJ, LWO, B3D, MD2, MD3, MD5, SMD and many other)
* Procedural mesh construction from vertex/normal/texcoord/index lists - you are free to design your own model format
* Model animation, per-vertex and skeletal
* Lightmapping and dynamic lighting
* Terrain rendering with automatic LOD
* Dynamic water rendering
* Dynamic soft shadows
* Static and dynamic cubemaps
* Particle system for fire-like and lighting-like effects
* Lensflare effect
* 2D shapes, including rectangle, circle, line, and custom 2D mesh
* 2D sprites and text with Unicode support
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
* Rigid body physics using ODE engine
* Ragdolls
* Octree and quadtree to speed-up rendering and collision tests
* Loading resources from Quake 2 PAK archives
* + a lot more

Asset Creation
--------------
For static geometry with lightmaps (such as levels), we recommend using B3D (Blitz3D) format. You can use B3D exporter for Blender to create models in this format - find it in tools/blender-b3d-exporter.

For animated geometry, we recommend either SMD (Half-Life) or MD5 (Doom 3) formats. You can use MD5 exporter for Blender to create models in this format - find it in tools/blender-md5-exporter.

License
-------
Xtreme3D 3.x and corresponding GLScene fork are both distributed under Mozilla Public License 1.1. This license allows you to create commercial closed-source applications with the engine.

ODE is distributed under BSD-style license.

FreeType is distributed under FreeType License (FTL).
