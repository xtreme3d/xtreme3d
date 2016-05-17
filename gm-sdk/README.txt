Xtreme3D 3.0 alpha SDK
======================
Xtreme3D is a 3D graphics engine for Game Maker 8. Originally it was created by a programmer known as Xception, but he abandoned his project, and we in Russian Xtreme3D community decided to rewrite Xtreme3D from scratch.
The engine is written in Delphi and based on a modified version of GLScene 1.0.0.0714. It works upon OpenGL and supports a wide variety of graphics technologies, including scene hierarchy, materials and textures, GLSL shaders, static and dynamic cubemaps, lightmapping, particles, dynamic water rendering, procedural skydome and trees, terrain rendering with LOD, shadow volumes, 3D text and a lot more. It also supports all popular 3D model formats, such as OBJ, 3DS, MD2, MD3, MD5, SMD, B3D and others. Xtreme3D is free and open source (distributed under LGPL).

This SDK includes only compiled version of the engine (xtreme3d.dll). The source code can be found at https://github.com/xtreme3d/xtreme3d
Also visit our website at http://xtreme3d.narod.ru

Demos
-----
demo.gmk - demonstrates sea water shader, animated MD5 model, B3D scene with lightmap, procedural tree, skybox, lensflare and first person camera with collision detection.

Asset Creation
--------------
For static geometry with lightmaps (such as levels), we recommend using B3D (Blitz3D) format. You can use B3D exporter for Blender to create models in this format - find it in tools/blender-b3d-exporter.
For animated geometry, we recommend either SMD (Half-Life) or MD5 (Doom 3) formats. You can use MD5 exporter for Blender to create models in this format - find it in tools/blender-md5-exporter.

Credits
-------
Xtreme3D 3.0 is mainly written by Timur Gafarov aka Gecko. DCE functions are written by Rutraple, Grid functions are written by Ghost.

License
-------
GLScene is distributed under Mozilla Public License 1.1. 
Xtreme3D 3.0 is distributed under GNU Lesser General Public License 2.1. It allows you to create commercial closed-source applications with the engine if you don't modify the source code and only use compiled version of the library for linking.
