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

win = WindowCreate(0, 0, 1280, 720, False)
WindowCenter(win)

viewer = ViewerCreate(0, 0, 1280, 720, WindowGetHandle(win))
ViewerSetBackgroundColor(viewer, c_gray)
ViewerSetLighting(viewer, True)
ViewerEnableFog(viewer, True)
ViewerSetFogColor(viewer, c_gray)
ViewerSetFogDistance(viewer, 0, 50)
# ViewerSetAntiAliasing(viewer, csa8xHQ) #CSAA
ViewerEnableVSync(viewer, vsmNoSync)
ViewerSetAutoRender(viewer, False)

running = True
timer = 0.0
fixedDelta = 1.0 / 60.0

while(running):
    dt = EngineGetTimeStep()
    timer += dt
    if timer >= fixedDelta:
        EngineUpdate(dt)
        ViewerRender(viewer)
        WindowDispatch()
        running = WindowIsShowing(win)
        timer = 0.0
