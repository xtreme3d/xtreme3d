Xtreme3D source code
====================
Xtreme3D is written in Delphi 11 and based on a slightly modified version of [GLScene](https://github.com/GLScene/GLScene) 2.2. All necessary dependencies are included. The resulting form of the engine is `xtreme3d.dll`.

`bin` directory contains additional dynamic libraries that are needed for the engine to work correctly. Put them alongside with compiled `xtreme3d.dll` in your application's directory.

`Addons` folder contains additional modules that are not part of the core GLScene, including Kraft physics, SDL2 integration, CSM and LOD formats support, shadow maps support and other.
