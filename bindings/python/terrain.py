import os.path
import time
import random
import ctypes
import sdl2
import math;
from framework import *

"""
  Dynamic Terrain Editing Demo
  -------------------------
  Author: Gecko
  Xtreme3D version: 3.8+
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
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

        self.back = DummycubeCreate(0)
        self.scene = DummycubeCreate(0)
        self.front = DummycubeCreate(0)
        
        self.shadowCasters = DummycubeCreate(self.scene)
        
        self.lightPivot = DummycubeCreate(self.scene)
        self.light = LightCreate(lsParallel, self.lightPivot)
        LightSetAmbientColor(self.light, c_black)
        LightSetDiffuseColor(self.light, c_white)
        LightSetSpecularColor(self.light, c_white)
        ObjectPitch(self.light, -45);
        
        self.camPos = DummycubeCreate(self.shadowCasters)
        ObjectSetPosition(self.camPos, 0, 1.8, 5)
        self.camera = CameraCreate(self.camPos)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        ViewerSetCamera(self.viewer, self.camera)
        
        self.hds = BmpHDSCreateEmpty(256, 256, 0.5)
        BmpHDSSetInfiniteWarp(self.hds, 0)
        self.terrain=TerrainCreate(self.scene)
        TerrainSetHeightData(self.terrain, self.hds)
        TerrainSetTileSize(self.terrain, 32)
        TerrainSetTilesPerTexture(self.terrain, 8)
        TerrainSetQualityDistance(self.terrain, 100)
        TerrainSetQualityStyle(self.terrain, hrsFullGeometry)
        TerrainSetMaxCLodTriangles(self.terrain, 10000)
        TerrainSetCLodPrecision(self.terrain, 50)
        TerrainSetOcclusionFrameSkip(self.terrain, 0)
        TerrainSetOcclusionTesselate(self.terrain, totTesselateIfVisible)
        ObjectSetScale(self.terrain, 1, 1, 0.1)
        ObjectRotate(self.terrain,90,0,0)
        
        self.heightmap = [[0.5 for x in range(256)] for y in range(256)] 
        self.drawHeight(30, 30, 10, 2, 0.5, 0.2)
        self.drawHeight(40, 30, 10, 2, 0.5, 0.2)
        
        BmpHDSSave(self.hds, 'heightmap.bmp')
        MaterialCreate('mTerrainTexture1', 'data/terrain/rock.jpg');
        MaterialSetTextureWrap('mTerrainTexture1', 1);
        MaterialCreate('mTerrainTexture2', 'data/terrain/grass.png');
        MaterialSetTextureWrap('mTerrainTexture2', 1);
        MaterialCreate('mTerrainMask', 'heightmap.bmp');
        MaterialCreate('mTerrainNormalmap', 'heightmap.bmp');
        MaterialSetTextureFormat('mTerrainNormalmap', tfNormalMap);
        
        self.vp1 = textRead('data/shaders/terrain/terrain-vp.glsl')
        self.fp1 = textRead('data/shaders/terrain/terrain-fp.glsl')
        terrShader = GLSLShaderCreate(self.vp1, self.fp1)
        terrParamTexture1 = GLSLShaderCreateParameter(terrShader, 'texture1')
        GLSLShaderSetParameterTexture(terrParamTexture1, 'mTerrainTexture1', 0)
        terrParamTexture2 = GLSLShaderCreateParameter(terrShader, 'texture2')
        GLSLShaderSetParameterTexture(terrParamTexture2, 'mTerrainTexture2', 1)
        terrParamMask = GLSLShaderCreateParameter(terrShader, 'mask')
        GLSLShaderSetParameterTexture(terrParamMask, 'mTerrainMask', 2)
        terrParamNormalmap = GLSLShaderCreateParameter(terrShader, 'normalmap')
        GLSLShaderSetParameterTexture(terrParamNormalmap, 'mTerrainNormalmap', 3)
        terrParamInvViewMatrix = GLSLShaderCreateParameter(terrShader, 'invViewMatrix')
        GLSLShaderSetParameterInvViewMatrix(terrParamInvViewMatrix)
        terrParamViewMatrix = GLSLShaderCreateParameter(terrShader, 'viewMatrix')
        GLSLShaderSetParameterViewMatrix(terrParamViewMatrix)

        MaterialCreate('mTerrain', '')
        MaterialSetShader('mTerrain', terrShader)
        ObjectSetMaterial(self.terrain, 'mTerrain')
                
        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12)
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_white, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.setMouseToCenter()
    
    def drawHeight(self, cx, cy, radius, offset, power, opacity):
        diameter = int(radius + radius)
        for y in range(-radius, radius):
            for x in range(-radius, radius):
                px = int(cx + x)
                py = int(cy + y)
                d = self.distance(px, py, cx, cy)
                h = self.clamp((radius - d) / (radius - offset), 0.0, 1.0)
                hDest = math.pow(h, power) * opacity
                hSrc = self.heightmap[px][py]
                h = hSrc + hDest
                self.heightmap[px][py] = h;
                BmpHDSSetHeight(self.hds, px, py, h)
   
    def distance(self, x1, y1, x2, y2):
        dx = x1 - x2
        dy = y1 - y2
        d = dx * dx + dy * dy
        if d < 0.000001:
            return 0
        else:
            return math.sqrt(dx * dx + dy * dy)
            
    def clamp(self, n, smallest, largest): 
        return max(smallest, min(n, largest))
    
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
        elif key == KEY_F12:
            self.makeScreenshot()
    
    def onMouseButtonUp(self, button):
        if button == MB_LEFT or button == MB_RIGHT:
            BmpHDSSave(self.hds, 'heightmap.bmp')
            MaterialLoadTexture('mTerrainNormalmap', 'heightmap.bmp');
            MaterialLoadTexture('mTerrainMask', 'heightmap.bmp');
    
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
        
        ObjectSetPositionY(self.camPos, TerrainGetHeightAtObjectPosition(self.terrain, self.camPos) + 2);
        
        px = ObjectGetAbsolutePosition(self.camPos, 0) + ObjectGetAbsoluteDirection(self.camera, 0) * 5
        py = ObjectGetAbsolutePosition(self.camPos, 1) + ObjectGetAbsoluteDirection(self.camera, 1) * 5
        pz = ObjectGetAbsolutePosition(self.camPos, 2) + ObjectGetAbsoluteDirection(self.camera, 2) * 5
        hx = TerrainGetHDSPosition(self.terrain, px, py, pz, 0)
        hy = TerrainGetHDSPosition(self.terrain, px, py, pz, 1)
        
        if self.mouseButtonPressed[MB_LEFT]:
            self.drawHeight(hx, hy, 5, 1, 0.5, 0.005)
        if self.mouseButtonPressed[MB_RIGHT]:
            self.drawHeight(hx, hy, 5, 1, 0.5, -0.005)
        
        if self.keyPressed[KEY_UP]:   ObjectPitch(self.light, -30 * dt)
        if self.keyPressed[KEY_DOWN]: ObjectPitch(self.light,  30 * dt)
        if self.keyPressed[KEY_LEFT]: ObjectTurn(self.lightPivot, -30 * dt)
        if self.keyPressed[KEY_RIGHT]: ObjectTurn(self.lightPivot, 30 * dt)

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        
        HUDTextSetText(self.text, 
            'Xtreme3D 3\r' + 'FPS: '+ str(framerate) +
            '\rPress <LMB> to increase height' +
            '\rPress <RMB> to lower height')

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

app = MyApplication(1280, 720, 'Xtreme3D 3 Dynamic Terrain Demo')
app.run()
