import sys
from os.path import join
from glob import glob
from distutils.sysconfig import get_python_lib
from cx_Freeze import setup, Executable

base = None
if (sys.platform == "win32"):
    base = "Win32GUI"

setup(
    name = 'xreme3d',
    version = '0.0.1',
    description = 'Xtreme3D',
    options = {
        'build_exe': {
        'packages': ['os', 'sys', 'ctypes', 'json'],
        'include_files': ['data', 'xtreme3d.dll', 'ode64s.dll', 'SDL2.dll', 'SDL2_ttf.dll'],
        'include_msvcr': True,
    }},
    executables = [Executable(script='test.py', icon='icon.ico', base=base)]
)
