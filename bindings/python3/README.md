Xtreme3D for Python
===================
This is a Python 3 binding for [Xtreme3D engine](https://github.com/xtreme3d/xtreme3d). It is tested with Python 3.9 64-bit.

Basic usage
-----------
To run a demo:

`python app.pyw`

You can run *.pyw file directly from Windows Explorer if you don't need the command line window.

Deployment
----------
To distribute your game, you can build a standalone executable from your script, so that it won't require Python installation on user sude to run.

There are two supported deployment workflows - using cx_Freeze and using PyInstaller.

### cx_Freeze workflow:

`build app.pyw`

Make sure you have write access to working directory before running `build.bat`. It will create a directory named `build/exe.win-amd64-3.9` containing the bindle ready for distribution, including the `data` folder and all DLLs. If you use any additional files in your application, modify `setup.py` to include other folders. It will also use `icon.ico` to create a `app.exe` - you can replace it with your own icon.

### PyInstaller workflow:

`build_pyinstaller app.spec`

Make sure you have write access to working directory before running `build_pyinstaller.bat`. It will create a directory named `dist/app` containing the bindle ready for distribution, including the `data` folder and all DLLs. If you use any additional files in your application, modify `app.spec` to include other folders. It will also use `icon.ico` and `splash.jpg` to create a `app.exe` - you can replace it with your own icon and splash screen image.

Warning: do not run `build_pyinstaller app.pyw`! It will overwrite `app.spec`.

When building with PyInstaller

Mods/plugins
------------
The example script shows how to add a modding system to the game using [PluginBase](https://github.com/mitsuhiko/pluginbase) framework. Put your Python scripts to `plugins` folder, and the core application will run `setup` and `update` functions from them. They have access to Xtreme3D functions as well as the application's data. Plugins can create and manipulate Xtreme3D objects, keep their own state, and update their logics. This will work in the bundle: users can add their own plugins to modify the game at runtime.
