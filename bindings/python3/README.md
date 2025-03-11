Xtreme3D for Python
===================
This is a Python 3 binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It is tested with Python 3.9 64-bit.

Basic usage
-----------
To run a demo:

`python test.pyw`

You can run *.pyw file directly from Windows Explorer if you don't need the command line window.

Do not forget to copy `xtreme3d.dll`, `ode64s.dll`, `SDL2.dll`, `SDL2_ttf.dll` to application directory before running.

Deployment
----------
To distribute your game, you can build a standalone executable from your script, so that it won't require Python installation on user sude to run.

First, install [cx_Freeze](https://pypi.org/project/cx-Freeze/). Then use `build.bat`:

`build test.pyw`

Make sure you have write access to working directory before running `build.bat`. It will create a directory named `build/exe.win-amd64-3.9` containing the bindle ready for distribution, including the `data` folder and all DLLs. If you use any additional files in your application, if copy them to the `data` folder, or modify `setup.py` to include other folders. It will also use `icon.ico` to create a `app.exe` - you can replace it with your own icon.

Binding Generator
-----------------
The binding script (`xtreme3d/x3dfuncs.py`) can be automatically regenerated from the Xtreme3D source code. Use `genbinding.py` for that (given the repo directory structure remains unchanged).
