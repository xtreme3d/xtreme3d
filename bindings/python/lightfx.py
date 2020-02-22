import os.path
import time
import random
import ctypes
import sdl2
from framework import *

"""
  LightFX Demo
  -------------------------
  Author: Gecko
  Xtreme3D version: 3.8
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
  This demo shows how to use LightFX system that handles multiple light sources 
  (up to 8 lsOmni or lsSpot lights per object).
"""

class MyApplication(Framework):
    screenNum = 0

    def start(self):
        EngineCreate()
        EngineSetMaxLights(8)
        
        self.viewer = ViewerCreate(0, 0, self.windowWidth, self.windowHeight, windowHandle(self.window))
        ViewerSetBackgroundColor(self.viewer, c_dkgray)
        ViewerSetAntiAliasing(self.viewer, aa4xHQ)
        ViewerSetLighting(self.viewer, True)
        ViewerEnableFog(self.viewer, True)
        ViewerSetFogColor(self.viewer, c_dkgray)
        ViewerSetFogDistance(self.viewer, 50, 100)
        ViewerEnableVSync(self.viewer, vsmSync)
        ViewerSetAutoRender(self.viewer, False)

        self.matlib = MaterialLibraryCreate()
        MaterialLibraryActivate(self.matlib)

        self.bump = BumpShaderCreate()
        BumpShaderSetDiffuseTexture(self.bump, '')
        BumpShaderSetNormalTexture(self.bump, '')
        BumpShaderSetHeightTexture(self.bump, '')
        BumpShaderSetMaxLights(self.bump, 8)
        BumpShaderUseAutoTangentSpace(self.bump, True)
        BumpShaderUseParallax(self.bump, True)
        
        MaterialCreate('mStone', 'data/room/stone.png')
        MaterialLoadTextureEx('mStone', 'data/room/stone-normal.png', 1)
        MaterialLoadTextureEx('mStone', 'data/room/stone-height.png', 2)
        MaterialSetShininess('mStone', 32)
        MaterialSetAmbientColor('mStone', c_black, 1)
        MaterialSetDiffuseColor('mStone', c_white, 1)
        MaterialSetSpecularColor('mStone', c_ltgray, 1)
        MaterialSetShader('mStone', self.bump)
        
        MaterialCreate('mPlane', 'data/misc/tiles.jpg')

        self.back = DummycubeCreate(0)
        self.scene = DummycubeCreate(0)
        self.front = DummycubeCreate(0)
        
        gridSize = 4;
        
        for y in range(-gridSize, gridSize+1):
            for x in range(-gridSize, gridSize+1):
                p = PlaneCreate(1, 3, 3, 1, 1, self.scene)
                ObjectSetPosition(p, x * 3, 0, y * 3)
                ObjectPitch(p, 90)
                ObjectSetMaterial(p, 'mStone')
                LightFXCreate(p)
                
                li = LightCreate(lsOmni, self.scene)
                col = MakeColorRGBFloat(random.randrange(255) / 255.0, random.randrange(255) / 255.0, random.randrange(255) / 255.0)
                LightSetAmbientColor(li, c_black)
                LightSetDiffuseColor(li, col)
                LightSetSpecularColor(li, col)
                LightSetAttenuation(li, 2.0, 0.0, 0.8)
                ObjectSetPosition(li, x * 3, 1, y * 3)

        self.camPos = DummycubeCreate(self.scene)
        ObjectSetPosition(self.camPos, 0, 1.8, 5)
        self.camera = CameraCreate(self.camPos)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        ViewerSetCamera(self.viewer, self.camera)
        
        s1 = SphereCreate(0.5, 16, 8, self.camPos)
        ObjectSetPosition(s1, 0, -1.8 + 0.5, -2)
        ObjectSetMaterial(s1, 'mStone')
        LightFXCreate(s1);

        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12);
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_white, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.setMouseToCenter()
        
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
        elif key == KEY_F12:
            self.makeScreenshot()
        
    def update(self, dt):
        deltax = (self.halfWindowWidth - self.mouseX) / 3.0;
        deltay = (self.halfWindowHeight - self.mouseY) / 3.0;
        self.setMouseToCenter();
        ObjectRotate(self.camera, deltay, 0, 0);
        ObjectRotate(self.camPos, 0, -deltax, 0);
        
        if self.keyPressed[KEY_W]: ObjectMove(self.camPos, -10 * dt)
        if self.keyPressed[KEY_A]: ObjectStrafe(self.camPos, 10 * dt)
        if self.keyPressed[KEY_D]: ObjectStrafe(self.camPos, -10 * dt)
        if self.keyPressed[KEY_S]: ObjectMove(self.camPos, 10 * dt)

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        HUDTextSetText(self.text, 'FPS: ' + str(framerate));

        Update(dt)
    
    def render(self):    
        ViewerRender(self.viewer)
        
    def makeScreenshot(self):
        self.screenNum += 1
        saved = False;
        while not saved:
            filename = 'screenshot' + str(self.screenNum) + '.bmp'
            if not os.path.exists(filename):
                ViewerRenderToFile(self.viewer, filename)
                saved = True
            else:
                self.screenNum += 1

app = MyApplication(1280, 720, 'Xtreme3D 3.8 LightFX Demo')
app.run()
