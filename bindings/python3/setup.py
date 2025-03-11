import sys
from os.path import join
from glob import glob
from distutils.sysconfig import get_python_lib
from cx_Freeze import setup, Executable

base = None
if (sys.platform == 'win32'):
    base = 'Win32GUI'

mainScript = 'test.pyw'

if '--script' in sys.argv:
    index = sys.argv.index('--script')
    sys.argv.pop(index)
    mainScript = sys.argv.pop(index)

executable = Executable(script = mainScript, icon = 'icon.ico', base = base)

exclude = ['tkinter', 'tcl', 'tk', 'sqlite3', 'xml', 'xmlrpc', 'http', 'html', 'urllib', 'email', 'unittest', 'xtreme3d.dll', 'ode64s.dll', 'SDL2.dll', 'SDL2_ttf.dll']
include = ['data', 'plugins', 'xtreme3d.dll', 'ode64s.dll', 'SDL2.dll', 'SDL2_ttf.dll']

setup(
    name = 'Xtreme3D SDK Demo',
    version = '1.0.0',
    description = 'Xtreme3D SDK Demo',
    options = {
        'build_exe': {
            'packages': ['os', 'sys', 'ctypes', 'json'],
            'excludes': exclude,
            'include_files': include,
            'include_msvcr': True,
            'optimize': 2
        }
    },
    executables = [executable]
)
