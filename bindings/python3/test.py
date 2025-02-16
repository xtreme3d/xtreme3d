import os.path
import time
import random
import ctypes
from xtreme3d import *

EngineCreate()
EngineShowLoadingErrors(True)
EngineSetCulling(vcNone)
EngineSetObjectsSorting(osNone)
EngineSetMaxLights(8)

windowWidth = 1280
windowHeight = 720

win = WindowCreate(0, 0, windowWidth, windowHeight, False)
WindowCenter(win)
WindowSetTitle(win, b'Xtreme3D 4.0')

viewer = ViewerCreate(0, 0, windowWidth, windowHeight, WindowGetHandle(win))
ViewerSetBackgroundColor(viewer, c_gray)
ViewerSetLighting(viewer, True)
ViewerEnableFog(viewer, True)
ViewerSetFogColor(viewer, c_gray)
ViewerSetFogDistance(viewer, 0, 50)
ViewerSetAntiAliasing(viewer, csa8xHQ) # CSAA
ViewerEnableVSync(viewer, vsmSync)
ViewerSetAutoRender(viewer, False)

matlib = MaterialLibraryCreate()
MaterialLibraryActivate(matlib)

back = DummycubeCreate(0)
scene = DummycubeCreate(0)
front = DummycubeCreate(0)

camPos = DummycubeCreate(scene)

camera = CameraCreate(camPos)
ViewerSetCamera(viewer, camera)
ObjectSetPosition(camPos, 0, 1.8, 3)
CameraSetViewDepth(camera, 1000)
CameraSetFocal(camera, 100)
CameraSetNearPlaneBias(camera, 0.2)

light1 = LightCreate(lsOmni, scene)
LightSetAmbientColor(light1, c_black)
LightSetDiffuseColor(light1, c_white)
LightSetSpecularColor(light1, c_white)
ObjectSetPosition(light1, 1, 3, 1)

plane = PlaneCreate(0, 20, 20, 10, 10, scene)
ObjectPitch(plane, 90)
MaterialCreate(b'mPlane', b'data/ground.jpg')
ObjectSetMaterial(plane, b'mPlane')

cube = CubeCreate(1, 1, 1, scene)
ObjectSetPosition(cube, 0, 0.5, 0)

mouselookActive = True
mbLeftReleased = True
pmx = MouseGetPositionX()
pmy = MouseGetPositionY()
mx = WindowGetPosition(win, 0) + windowWidth* 0.5
my = WindowGetPosition(win, 1) + windowHeight * 0.5
MouseSetPosition(mx, my)
MouseShowCursor(not mouselookActive);

running = True
timer = 0.0
dt = 1.0 / 60.0 #0.015

while(running):
    WindowDispatch()
    timeStep = EngineGetTimeStep()
    timer += timeStep
    if timer >= dt:
        timer -= dt
        escPressed = False
        if mouselookActive and WindowIsActive(win):
            mx = WindowGetPosition(win, 0) + windowWidth * 0.5
            my = WindowGetPosition(win, 1) + windowHeight * 0.5
            deltax = (mx - MouseGetPositionX()) / 6
            deltay = (my - MouseGetPositionY()) / 6
            ObjectRotate(camera, deltay, 0, 0)
            ObjectRotate(camPos, 0, -deltax, 0)
            MouseSetPosition(mx, my)

            if KeyIsPressed(ord('W')):
                ObjectMove(camPos, -5 * dt)
            if KeyIsPressed(ord('A')):
                ObjectStrafe(camPos, 5 * dt)
            if KeyIsPressed(ord('D')):
                ObjectStrafe(camPos, -5 * dt)
            if KeyIsPressed(ord('S')):
                ObjectMove(camPos, 5 * dt)
            if KeyIsPressed(27): # vk_escape
                escPressed = True
        
        if MouseIsPressed(1) and WindowIsActive(win): # mb_left
            if mbLeftReleased:
                mbLeftReleased = False
                mouselookActive = not mouselookActive
                if not mouselookActive:
                    MouseSetPosition(pmx, pmy)
                else:
                    pmx = MouseGetPositionX()
                    pmy = MouseGetPositionY()
                    mx = WindowGetPosition(win, 0) + windowWidth * 0.5
                    my = WindowGetPosition(win, 1) + windowHeight * 0.5
                    MouseSetPosition(mx, my)
                MouseShowCursor(not mouselookActive)
        else:
            mbLeftReleased = True
        
        EngineUpdate(dt)
        ViewerRender(viewer)
        
        if escPressed:
            running = False
        else:
            running = WindowIsShowing(win)
