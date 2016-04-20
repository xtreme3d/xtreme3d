Xtreme3D v3
===========
Once upon a time, there was a great 3D graphics engine for Game Maker - Xtreme3D. It made possible to create full-featured 3D games with modern graphics  and reasonable framerate in a game design tool that was intended mainly for 2D games and provided only limited 3D capabilities. However, original Xtreme3D was a closed-source project, and it was abandoned by the author. Xtreme3D 2.0.2.0 (2006) was the last official release. So we in Russian Xtreme3D community made an attempt to rewrite the engine, maintaining the API as close to original as possible. This work began back in 2009 and now is close to its completion.

This project is not connected in any way with the original Xtreme3D developer. All code was written from scratch.

The engine is written in Delphi and based on a modified version of GLScene 1.0.0.0714. It is not finished yet, the following functionality is missing: Shadowvolume, Mirror, Navigator, Tree, Trail, DCE, FPS, Partition. The code is compatible with Delphi 7 and higher (Lazarus support is  also theoretically possible with some minor changes, we will return to this when the codebase will be ready).

Visit our Russian Xtreme3D site at http://xtreme3d.narod.ru

Features
--------
* Scene hierarchy
* Built-in primitives (such as box, sphere, cylinder, teapot, etc.)
* Materials and textures. Materials can be loaded from script files
* Various 3D model formats support (3DS, OBJ, LWO, B3D, MD2, MD3, SMD and others)
* Model animation (per-vertex and skeletal)
* Lightmapping and dynamic lighting
* Terrain rendering with automatic LOD
* Dynamic water rendering
* Static and dynamic cubemaps
* GLSL shaders
* Built-in shaders, including cel shader, bump shader, multimaterial shader, etc.
* Particle system
* Lensflare effect
* 2D sprites and text
* 3D text
* Skyboxes
* Procedural skydome with Sun, stars and day/night cycle
* Dynamic shadows

License
-------
GLScene is distributed under Mozilla Public License 1.1. 

Xtreme3D v3 is distributed under GNU Lesser General Public License 2.1. It allows you to create commercial closed-source applications with the engine if you  don't modify the source code and only use compiled version of the library for linking.
