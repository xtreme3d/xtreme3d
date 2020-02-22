import sys
from cx_Freeze import setup, Executable

mainScript = 'csm.py'

if '--script' in sys.argv:
    index = sys.argv.index('--script')
    sys.argv.pop(index)
    mainScript = sys.argv.pop(index)

executable = Executable(script=mainScript, icon='icon.ico', base = 'Win32GUI')

include = [
    'codepages', 
    'data',
    'freetype.dll',
    'ode.dll',
    'OpenFBX.dll',
    'SDL2.dll',
    'xtreme3d.dll'
]

setup(
    name = 'Xtreme3D SDK Demo',
    version = '1.0.0',
    description = 'Xtreme3D SDK Demo',
    options = {
        'build_exe': {
            'packages': ['os', 'sys', 'time', 'random', 'ctypes'],
            'include_files': include,
            'include_msvcr': True,
            'excludes': ['tkinter', 'tcl', 'tk', 'sqlite3'],
            'optimize': 2
        }
    },
    executables = [executable]
)