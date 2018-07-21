Xtreme3D for Python
===================
This is a Python binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It is compatible with Python 2.7 and 3.2+ (only 32-bit!).

The demo uses PySDL2 to create a window and handle user input. 

Usage
-----
Run `python app.py`. You can also use PyInstaller to make a standalone application: `pyinstaller app.py`.

Do not forget to copy `xtreme3d.dll` (and also `ode.dll`, `freetype.dll`, `OpenFBX.dll` if needed) to application directory before running.
