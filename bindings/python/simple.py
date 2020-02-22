import os.path
import time
import ctypes
import sdl2
from xtreme3d import *
from keycodes import *
    
def windowHandle(sdlwnd):    
    info = sdl2.SDL_SysWMinfo()
    sdl2.SDL_GetWindowWMInfo(sdlwnd, ctypes.byref(info)) 
    return info.info.win.window
    
windowWidth = 1280
windowHeight = 720
    
sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
window = sdl2.SDL_CreateWindow('Xtreme3D',
                                sdl2.SDL_WINDOWPOS_CENTERED,
                                sdl2.SDL_WINDOWPOS_CENTERED, 
                                windowWidth, windowHeight,
                                sdl2.SDL_WINDOW_SHOWN)

EngineCreate()
viewer = ViewerCreate(0, 0, windowWidth, windowHeight, windowHandle(window))
ViewerSetBackgroundColor(viewer, c_dkgray)
ViewerSetAntiAliasing(viewer, aa4xHQ)
ViewerSetLighting(viewer, True)
ViewerEnableFog(viewer, True)
ViewerSetFogDistance(viewer, 50, 100)
ViewerEnableVSync(viewer, vsmSync)

matlib = MaterialLibraryCreate()
MaterialLibraryActivate(matlib)

MaterialCreate('mPlane', 'data/ground.jpg')

bump = BumpShaderCreate()
BumpShaderSetDiffuseTexture(bump, '')
BumpShaderSetNormalTexture(bump, '')
BumpShaderSetMaxLights(bump, 3)
BumpShaderUseParallax(bump, False)
BumpShaderSetShadowBlurRadius(bump, 2)
BumpShaderUseAutoTangentSpace(bump, True)

MaterialCreate('mStone', '')
MaterialLoadTextureEx('mStone', 'data/stone.png', 0)
MaterialLoadTextureEx('mStone', 'data/stone-normal.png', 1)
MaterialSetShininess('mStone', 32)
MaterialSetAmbientColor('mStone', c_dkgray, 1)
MaterialSetDiffuseColor('mStone', c_ltgray, 1)
MaterialSetSpecularColor('mStone', c_white, 1)
MaterialSetShader('mStone', bump)

back = DummycubeCreate(0)
scene = DummycubeCreate(0)
front = DummycubeCreate(0)

light = LightCreate(lsOmni, scene)
ObjectSetPosition(light, 2, 4, 2)
LightSetDiffuseColor(light, c_white)
LightSetSpecularColor(light, c_white)

shadowCasters = DummycubeCreate(scene)

plane = ShadowplaneCreate(15, 15, 3, 3, shadowCasters, light, c_black, 0.7, scene)
ObjectSetPosition(plane, 0, -2, 0)
ObjectPitch(plane, 90)
ObjectSetMaterial(plane, 'mPlane')

cube = CubeCreate(1, 1, 1, shadowCasters)
ObjectSetMaterial(cube, 'mStone')

camPos = DummycubeCreate(scene)
ObjectSetPosition(camPos, 0, 1, 5)
camera = CameraCreate(camPos)
CameraSetViewDepth(camera, 500)
CameraSetFocal(camera, 80)
ViewerSetCamera(viewer, camera)

font = WindowsBitmapfontCreate('Arial', 20, 0, 128)
text = HUDTextCreate(font, 'Xtreme3D 3.6', front)
HUDTextSetColor(text, c_white, 0.5)
ObjectSetPosition(text, 20, 20, 0)

keyPressed = [False] * 512
mouseX = 0
mouseY = 0

mx = windowWidth / 2
my = windowHeight / 2
sdl2.SDL_WarpMouseInWindow(window, mx, my)
sdl2.SDL_ShowCursor(0)

def updateLogic(dt):
    deltax = (mx - mouseX) / 3.0;
    deltay = (my - mouseY) / 3.0;
    sdl2.SDL_WarpMouseInWindow(window, mx, my);
    ObjectRotate(camera, deltay, 0, 0);
    ObjectRotate(camPos, 0, -deltax, 0);
    
    if keyPressed[KEY_W]: ObjectMove(camPos, -10 * dt)
    if keyPressed[KEY_A]: ObjectStrafe(camPos, 10 * dt)
    if keyPressed[KEY_D]: ObjectStrafe(camPos, -10 * dt)
    if keyPressed[KEY_S]: ObjectMove(camPos, 10 * dt)

    ObjectTurn(cube, dt * 45)

    framerate = int(ViewerGetFramesPerSecond(viewer))
    HUDTextSetText(text, str(framerate));

    Update(dt)
    
def render():
    ViewerRender(viewer)

screenNum = 0
def makeScreenshot():
    global screenNum
    screenNum += 1
    saved = False;
    while not saved:
        filename = 'screenshot' + str(screenNum) + '.bmp'
        if not os.path.exists(filename):
            ViewerRenderToFile(viewer, filename)
            saved = True
        else:
            screenNum += 1

fixedTimeStep = 1.0 / 60.0
timer = 0
lastTime = 0

running = True

event = sdl2.SDL_Event()
while running:

    while sdl2.SDL_PollEvent(ctypes.byref(event)) != 0:
        if event.type == sdl2.SDL_QUIT:
            running = False
        elif event.type == sdl2.SDL_KEYDOWN:
            keyPressed[event.key.keysym.scancode] = True
            if event.key.keysym.scancode == KEY_ESCAPE:
                running = False
            elif event.key.keysym.scancode == KEY_F12:
                makeScreenshot()
        elif event.type == sdl2.SDL_KEYUP:
            keyPressed[event.key.keysym.scancode] = False
        elif event.type == sdl2.SDL_MOUSEMOTION:
            mouseX = event.motion.x
            mouseY = event.motion.y

    currentTime = sdl2.SDL_GetTicks()
    elapsedTime = currentTime - lastTime
    lastTime = currentTime
    dt = elapsedTime * 0.001

    timer += dt
    if (timer >= fixedTimeStep):
        timer -= fixedTimeStep;
        updateLogic(fixedTimeStep)
        
    render()

sdl2.SDL_DestroyWindow(window)
sdl2.SDL_Quit()
