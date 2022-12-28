Xtreme3D v4
===========
Xtreme3D is a cross-language 3D graphics engine based on [GLScene](https://github.com/GLScene/GLScene). It has a rich feature set, including custom shaders, wide variety of asset formats support, built-in effects, physics, collision and ray casting.

Xtreme3D originally was created for Game Maker. It makes possible to create full-featured 3D games with modern graphics and reasonable framerate in a tool that was intended mainly for 2D games and provides limited built-in 3D capabilities. Original Xtreme3D was a closed-source project, and it was abandoned by the author. Xtreme3D 2.0.2.0 (2006) was the last official release. So we in Russian Xtreme3D community made an attempt to rewrite the engine, maintaining the API as close to original as possible. This work began in 2009 and was completed in 2016. This project is not connected in any way with the original Xtreme3D developer, all code was written from scratch.

The engine is written in Delphi 10 and based on GLScene 2.1. The code can be compiled with free Delphi Community Edition.

Visit Xtreme3D site at [https://xtreme3d.ru](https://xtreme3d.ru).

Features
--------
* Can be used with any language that supports DLLs and stdcall convension
* Scene graph with object hierarchy/pivoting
* Built-in primitives (such as box, sphere, cylinder, teapot, etc.)
* Materials and textures. Materials can be loaded from script files
* GLSL shaders
* A number of built-in shaders
* Popular image formats support for textures (BMP, JPG, PNG, TGA, DDS)
* Various 3D model formats support (3DS, OBJ, LWO, B3D, MD2, MD3, MD5, SMD and many others)
* Procedural mesh construction from vertex/normal/texcoord/index lists - you are free to design your own model format
* Model animation, per-vertex and skeletal
* Lightmapping and dynamic lighting
* Terrain with automatic LOD and real-time editing
* Dynamic water rendering
* Static and dynamic cubemaps
* Particle system for fire-like and lighting-like effects
* Lensflare effect
* 2D sprites, billboards
* Skyboxes
* Procedural skydome with sun, stars and animated day/night cycle
* Procedural trees
* Lines and curves rendering (Cubic, Bezier, NURBS)
* Curves can be used to control object movement
* Discrete LOD system
* Offscreen rendering
* Collision detection between bounding spheres, oriented bounding boxes and meshes
* Raycasting
* Built-in game-oriented collision systems (DCE, FPSManager)
* Integrated rigid body physics using ODE engine
* Ragdolls
* Octree and quadtree to speed-up rendering and collision tests
* Loading resources from GLScene PAK archives
* Keyboard and mouse input
* Window creation and management

License
-------
Xtreme3D 3.x and corresponding GLScene fork are both distributed under Mozilla Public License 1.1. This license allows you to create commercial closed-source applications with the engine.

ODE is distributed under BSD-style license.
