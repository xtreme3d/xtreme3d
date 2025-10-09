# -*- mode: python ; coding: utf-8 -*-

a = Analysis(
    ['app.pyw'],
    pathex=[],
    binaries=[
        ('xtreme3d.dll', '.'),
        ('ode64s.dll', '.'),
        ('SDL2.dll', '.'),
        ('SDL2_ttf.dll', '.'),
        ('SDL2_mixer.dll', '.'),
        ('libgme.dll', '.'),
        ('libopus-0.dll', '.'),
        ('libopusfile-0.dll', '.'),
        ('libxmp.dll', '.'),
        ('settings.ini', '.')
    ],
    datas=[],
    hiddenimports=['clr'],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=['check_win_ver.py'],
    excludes=[],
    noarchive=False,
    optimize=0,
)
pyz = PYZ(a.pure)
splash = Splash(
    'splash.jpg',
    binaries=a.binaries,
    datas=a.datas,
    text_pos=None,
    text_size=12,
    minify_script=True,
    always_on_top=True,
)

exe = EXE(
    pyz,
    a.scripts,
    splash,
    [],
    exclude_binaries=True,
    name='app',
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
    icon=['icon.ico'],
)
coll = COLLECT(
    exe,
    a.binaries,
    a.datas,
    splash.binaries,
    strip=False,
    upx=True,
    upx_exclude=[],
    name='app',
)

from distutils.dir_util import copy_tree
copy_tree("data", "dist/app/data")
copy_tree("plugins", "dist/app/plugins")
