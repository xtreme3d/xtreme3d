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

If you have [PyInstaller](https://www.pyinstaller.org), you can build a standalone executable from any SDK demo or custom application, so that it won't require Python to run. Use `build.bat` script for that:

`build demoname.py`

Make sure you have write access to current directory before running the script.
It will create a directory named `dist/app` containing all the files needed to distribute the application, including the `data` folder and all DLLs. If you use any additional files in your application, copy them there manually.
It will also use `icon.ico` to create a `app.exe` - you can replace it with your own icon. You can rename `app.exe` to your liking.
`dist/app` directory can then be archived or used to make an installer with third-party software.

Note: if you run `build.bat` second time, it will delete `dist/app` directory, so any changes you made to it will be lost. Please, copy your build to other location if you want to keep it.
