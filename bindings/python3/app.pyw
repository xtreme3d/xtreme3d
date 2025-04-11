import os.path
import time
import random
import ctypes
from xtreme3d import *
from pluginbase import PluginBase

pluginBase = PluginBase(package = 'editorPlugins')

class App:
    def __init__(self):
        self.x3d = x3d
        
        EngineCreate()
        EngineShowLoadingErrors(True)
        EngineSetCulling(vcNone)
        EngineSetObjectsSorting(osNone)
        EngineSetMaxLights(8)
        
        AudioInit()
        
        self.windowWidth = 1280
        self.windowHeight = 720
        
        self.window = WindowCreate(0, 0, self.windowWidth, self.windowHeight, False)
        WindowCenter(self.window)
        WindowSetTitle(self.window, b'Xtreme3D 4.0')

        self.viewer = ViewerCreate(0, 0, self.windowWidth, self.windowHeight, WindowGetHandle(self.window))
        ViewerSetBackgroundColor(self.viewer, c_gray)
        ViewerSetLighting(self.viewer, True)
        ViewerEnableFog(self.viewer, True)
        ViewerSetFogColor(self.viewer, c_gray)
        ViewerSetFogDistance(self.viewer, 0, 50)
        ViewerSetAntiAliasing(self.viewer, csa8xHQ) # CSAA
        ViewerEnableVSync(self.viewer, vsmSync)
        ViewerSetAutoRender(self.viewer, False)

        self.matlib = MaterialLibraryCreate()
        MaterialLibraryActivate(self.matlib)

        self.back = DummycubeCreate(0)
        self.scene = DummycubeCreate(0)
        self.front = DummycubeCreate(0)

        self.camPos = DummycubeCreate(self.scene)
        self.camera = CameraCreate(self.camPos)
        ViewerSetCamera(self.viewer, self.camera)
        ObjectSetPosition(self.camPos, 0, 1.8, 3)
        CameraSetViewDepth(self.camera, 1000)
        CameraSetFocal(self.camera, 100)
        CameraSetNearPlaneBias(self.camera, 0.2)

        self.light1 = LightCreate(lsOmni, self.scene)
        LightSetAmbientColor(self.light1, c_black)
        LightSetDiffuseColor(self.light1, c_white)
        LightSetSpecularColor(self.light1, c_white)
        ObjectSetPosition(self.light1, 1, 3, 1)

        self.plane = PlaneCreate(0, 20, 20, 10, 10, self.scene)
        ObjectPitch(self.plane, 90)
        MaterialCreate(b'mPlane', b'data/ground.jpg')
        ObjectSetMaterial(self.plane, b'mPlane')

        self.mouselookActive = True
        self.mbLeftReleased = True
        self.pmx = MouseGetPositionX()
        self.pmy = MouseGetPositionY()
        self.mx = WindowGetPosition(self.window, 0) + self.windowWidth* 0.5
        self.my = WindowGetPosition(self.window, 1) + self.windowHeight * 0.5
        MouseSetPosition(self.mx, self.my)
        MouseShowCursor(not self.mouselookActive);
        
        self.running = True
        self.timer = 0.0
        self.dt = 1.0 / 60.0
        
        self.music = MusicLoad(b'data/robocop3.xm')
        MusicPlay(self.music, -1)
        
        self.pluginSource = pluginBase.make_plugin_source(
            searchpath = ['./plugins'],
            identifier = 'game')
        self.plugins = []
        for pluginName in self.pluginSource.list_plugins():
            plugin = self.pluginSource.load_plugin(pluginName)
            self.plugins.append(plugin)
            plugin.setup(self)

    def run(self):
        while(self.running):
            WindowDispatch()
            timeStep = EngineGetTimeStep()
            self.timer += timeStep
            if self.timer >= self.dt:
                self.timer -= self.dt
                self.update(self.dt)
                for plugin in self.plugins:
                    plugin.update(self, self.dt)
                self.render(self.dt)
    
    def update(self, dt):
        escPressed = False
        if self.mouselookActive and WindowIsActive(self.window):
            self.mx = WindowGetPosition(self.window, 0) + self.windowWidth * 0.5
            self.my = WindowGetPosition(self.window, 1) + self.windowHeight * 0.5
            deltax = (self.mx - MouseGetPositionX()) / 6
            deltay = (self.my - MouseGetPositionY()) / 6
            ObjectRotate(self.camera, deltay, 0, 0)
            ObjectRotate(self.camPos, 0, -deltax, 0)
            MouseSetPosition(self.mx, self.my)

        if KeyIsPressed(ord('W')):
            ObjectMove(self.camPos, -5 * dt)
        if KeyIsPressed(ord('A')):
            ObjectStrafe(self.camPos, 5 * dt)
        if KeyIsPressed(ord('D')):
            ObjectStrafe(self.camPos, -5 * dt)
        if KeyIsPressed(ord('S')):
            ObjectMove(self.camPos, 5 * dt)
        if KeyIsPressed(27): # vk_escape
            escPressed = True
            
        if MouseIsPressed(1) and WindowIsActive(self.window): # mb_left
            if self.mbLeftReleased:
                self.mbLeftReleased = False
                self.mouselookActive = not self.mouselookActive
                if not self.mouselookActive:
                    MouseSetPosition(self.pmx, self.pmy)
                    MouseShowCursor(True)
                else:
                    self.pmx = MouseGetPositionX()
                    self.pmy = MouseGetPositionY()
                    self.mx = WindowGetPosition(self.window, 0) + self.windowWidth * 0.5
                    self.my = WindowGetPosition(self.window, 1) + self.windowHeight * 0.5
                    MouseSetPosition(self.mx, self.my)
                    MouseShowCursor(False)
            else:
                self.mbLeftReleased = True
        
        if escPressed:
            self.running = False
        else:
            self.running = WindowIsShowing(self.window)

    def render(self, dt):
        EngineUpdate(self.dt)
        ViewerRender(self.viewer)

app = App()
app.run()
AudioClose()
