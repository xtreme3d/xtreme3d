Xtreme3D for C
==============
C binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). Comes with a demo application that can be built with CMake.

Usage
-----
Install [CMake](https://cmake.org/). Go to the `build` directory and run `cmake ..`. Then use VSCode to build generated project.

After building, do not forget to copy `xtreme3d.dll`, `ode64s.dll`, `SDL2.dll`, `SDL2_ttf.dll` to `Release` directory before running the application.

Binding Generator
-----------------
`include/xtreme3d.h` and `xtreme3d.def` can be automatically regenerated from the Xtreme3D source code. Use `genbinding.py` for that (given the repo directory structure remains unchanged).

To generate `lib/xtreme3d.lib`, use `makelib.bat`. Modify path to the `lib.exe` if needed.
