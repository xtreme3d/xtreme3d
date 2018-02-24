import ctypes
import time
import sdl2
from xtreme3d import *
    
def windowHandle(sdlwnd):    
    info = sdl2.SDL_SysWMinfo()
    sdl2.SDL_GetWindowWMInfo(sdlwnd, ctypes.byref(info)) 
    return info.info.win.window
    
sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
window = sdl2.SDL_CreateWindow(b'Xtreme3D',
                                   sdl2.SDL_WINDOWPOS_CENTERED,
                                   sdl2.SDL_WINDOWPOS_CENTERED, 
                                   800, 600,
                                   sdl2.SDL_WINDOW_SHOWN)

EngineCreate()
viewer = ViewerCreate(0, 0, 800, 600, windowHandle(window))
ViewerSetBackgroundColor(viewer, c_dkgray)
ViewerSetAntiAliasing(viewer, aa4xHQ);

matlib = MaterialLibraryCreate()
MaterialLibraryActivate(matlib)

back = DummycubeCreate(0)
scene = DummycubeCreate(0)
front = DummycubeCreate(0)

light = LightCreate(lsOmni, scene)
ObjectSetPosition(light, 2, 2, 2);

camera = CameraCreate(scene)
ViewerSetCamera(viewer, camera)
ObjectSetPosition(camera, 0, 0, 2);

cube = CubeCreate(1, 1, 1, scene)
MaterialCreate('mCube', 'data/stone.png');
ObjectSetMaterial(cube, 'mCube')

def updateLogic(dt):
    ObjectTurn(cube, dt * 45)

    Update(dt)
    ViewerRender(viewer)

fps = 60
lastFrameTime = 0
running = True

event = sdl2.SDL_Event()
while running:

    while sdl2.SDL_PollEvent(ctypes.byref(event)) != 0:
        if event.type == sdl2.SDL_QUIT:
            running = False
        elif event.type == sdl2.SDL_KEYDOWN:
            if event.key.keysym.sym == sdl2.SDLK_ESCAPE:
                running = False

    currentTime = time.time()
    dt = currentTime - lastFrameTime
    lastFrameTime = currentTime

    updateLogic(dt)

    sleepTime = 1.0 / fps - (currentTime - lastFrameTime)
    if sleepTime > 0:
        time.sleep(sleepTime)

sdl2.SDL_DestroyWindow(window)
sdl2.SDL_Quit()
