Xtreme3D for Python
===================
This is a Python binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It is compatible and tested with Python 2.7 (only 32-bit!). Python 3.x is not supported.

The demo uses PySDL2 to create a window and handle user input. 

Usage
-----
1. Basic usage

To run a demo:

`python demoname.py`

Do not forget to copy `xtreme3d.dll` (and also `ode.dll`, `freetype.dll`, `OpenFBX.dll` if needed) to application directory before running.

2. Deployment

You can build a standalone executable from any SDK demo or custom application, so that it won't require Python installation to run. Use `build.bat` script for that:

`build demoname.py`

Make sure you have write access to current directory before running the script.
It will create a directory named `build/exe.win32-2.7` containing all the files needed to distribute the application, including the `data` folder and all DLLs. If you use any additional files in your application, copy them there manually, or modify `setup.py`.
It will also use `icon.ico` to create a `app.exe` - you can replace it with your own icon.
`build/exe.win32-2.7` directory can then be archived or used to make an installer with third-party software.
