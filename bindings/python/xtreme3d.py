import ctypes

xtreme3d_dll_filename = 'xtreme3d.dll'

# Colors
c_black = 0.0
c_dkgray = 4210752.0
c_gray = 8421504.0
c_ltgray = 12632256.0
c_white = 16777215.0
c_aqua = 16776960.0
c_blue = 16711680.0
c_fuchsia = 16711935.0
c_green = 32768.0
c_lime = 65280.0
c_maroon = 128.0
c_navy = 8388608.0
c_olive = 32896.0
c_purple = 8388736.0
c_red = 255.0
c_silver = 12632256.0
c_teal = 8421376.0
c_yellow = 65535.0
c_orange = 33023.0

# Antialiasing
aaDefault = 0.0;
aaNone = 1.0;
aa2x = 2.0;
aa2xHQ = 3.0;
aa4x = 4.0;
aa4xHQ = 5.0;

# Light source type
lsSpot = 0.0;
lsOmni = 1.0;
lsParallel = 2.0;

x3d = ctypes.windll.LoadLibrary(xtreme3d_dll_filename)

# Engine
x3d.EngineCreate.argtypes = []
x3d.EngineCreate.restype = ctypes.c_double
x3d.Update.argtypes = [ctypes.c_double]
x3d.Update.restype = ctypes.c_double

# Viewer
x3d.ViewerCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerCreate.restype = ctypes.c_double
x3d.ViewerRender.argtypes = [ctypes.c_double]
x3d.ViewerRender.restype = ctypes.c_double
x3d.ViewerSetBackgroundColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetBackgroundColor.restype = ctypes.c_double
x3d.ViewerSetCamera.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetCamera.restype = ctypes.c_double
x3d.ViewerSetAntiAliasing.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetAntiAliasing.restype = ctypes.c_double

# Camera
x3d.CameraCreate.argtypes = [ctypes.c_double]
x3d.CameraCreate.restype = ctypes.c_double

# Light
x3d.LightCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightCreate.restype = ctypes.c_double

# Dummycube
x3d.DummycubeCreate.argtypes = [ctypes.c_double]
x3d.DummycubeCreate.restype = ctypes.c_double

# Primitives
x3d.CubeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CubeCreate.restype = ctypes.c_double

# Object
x3d.ObjectSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetPosition.restype = ctypes.c_double
x3d.ObjectTurn.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectTurn.restype = ctypes.c_double
x3d.ObjectSetMaterial.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ObjectSetMaterial.restype = ctypes.c_double

# Material
x3d.MaterialLibraryCreate.argtypes = []
x3d.MaterialLibraryCreate.restype = ctypes.c_double
x3d.MaterialLibraryActivate.argtypes = [ctypes.c_double]
x3d.MaterialLibraryActivate.restype = ctypes.c_double
x3d.MaterialCreate.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialCreate.restype = ctypes.c_double

# Window
x3d.WindowCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.WindowCreate.restype = ctypes.c_double
x3d.WindowCenter.argtypes = [ctypes.c_double]
x3d.WindowCenter.restype = ctypes.c_double

# Engine
def EngineCreate():
    return x3d.EngineCreate()
    
def Update(dt):
    return x3d.Update(dt)

# Viewer
def ViewerCreate(x, y, w, h, windowhandle):
    return x3d.ViewerCreate(x, y, w, h, windowhandle)
    
def ViewerRender(viewer):
    return x3d.ViewerRender(viewer)
    
def ViewerSetBackgroundColor(viewer, color):
    return x3d.ViewerSetBackgroundColor(viewer, color)
    
def ViewerSetCamera(viewer, camera):
    return x3d.ViewerSetCamera(viewer, camera)

def ViewerSetAntiAliasing(viewer, aa):
    return x3d.ViewerSetAntiAliasing(viewer, aa)

# Camera
def CameraCreate(parent):
    return x3d.CameraCreate(parent)

# Light
def LightCreate(ls, parent):
    return x3d.LightCreate(ls, parent)

# Dummycube
def DummycubeCreate(parent):
    return x3d.DummycubeCreate(parent)

# Primitives
def CubeCreate(width, height, depth, parent):
    return x3d.CubeCreate(width, height, depth, parent)
    
# Object
def ObjectSetPosition(object, x, y, z):
    return x3d.ObjectSetPosition(object, x, y, z)

def ObjectTurn(object, angle):
    return x3d.ObjectTurn(object, angle)

def ObjectSetMaterial(object, material):
    return x3d.ObjectSetMaterial(object, material)

# Material
def MaterialLibraryCreate():
    return x3d.MaterialLibraryCreate()
    
def MaterialLibraryActivate(matlib):
    return x3d.MaterialLibraryActivate(matlib)

def MaterialCreate(name, texfilename):
    return x3d.MaterialCreate(name, texfilename)

# Window
def WindowCreate(x, y, width, height, resizeable):
    return x3d.WindowCreate(x, y, width, height, resizeable)

def WindowCenter(window):
    return x3d.WindowCenter(window)
