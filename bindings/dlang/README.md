Xtreme3D for D
==============
This is Derelict D binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It can be automatically generated from Xtreme3D source code using genbinding.py script. 

The demo uses SDL2 to create a window and handle user input.

Usage
-----
If you have Python installed, just run `genbinding.py` script. 

Assuming Xtreme3D repository structure unchanged, it will analyze Xtreme3D source code and generate two files: `derelict/xtreme3d/xtreme3d_function_headers.txt` and `derelict/xtreme3d/xtreme3d_function_bindings.txt`, which are statically included in Derelict binding. We update these files after API changes, so it's actually not necessary to run the script if you are using release version of the source code. 

Then you can use Dub to compile the demo application: `dub build`. Do not forget to copy `xtreme3d.dll` (and also `ode.dll`, `freetype.dll` if needed) to application directory before running.
