import os.path
import time
import random
import ctypes
import sdl2
from framework import *

"""
  Cascaded Shadow Maps Demo
  -------------------------
  Author: Gecko
  Xtreme3D version: 3.8+
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
  This demo shows how cascaded shadow maps (CSM) can be implemented with Xtreme3D.
  It is a technique to shadow large areas with good quality and reasonable performance cost.
  The main idea behind CSM is rendering several shadow maps (called cascades) with different projection sizes
  and then combine (weight-blend) them in fragment shader based on fragment's coordinates.
  This demo uses simpler variant of CSM: three squared cascades created around the camera, with the camera at the center. 
  Each cascade use the same buffer resolution, but they cover more and more area.
  See 'shaders/csm/csm-fp.glsl' to see how shadow lookup is actually done.
"""

class CSMApplication(Framework):
    screenNum = 0

    def start(self):
        EngineCreate()

        self.viewer = ViewerCreate(0, 0, self.windowWidth, self.windowHeight, windowHandle(self.window))
        ViewerSetBackgroundColor(self.viewer, 0xb4c9f6)
        ViewerSetAntiAliasing(self.viewer, aa4xHQ)
        ViewerSetLighting(self.viewer, True)
        ViewerEnableFog(self.viewer, True)
        ViewerSetFogColor(self.viewer, 0xb4c9f6)
        ViewerSetFogDistance(self.viewer, 50, 100)
        ViewerEnableVSync(self.viewer, vsmSync)
        ViewerSetAutoRender(self.viewer, False)

        self.matlib = MaterialLibraryCreate()
        MaterialLibraryActivate(self.matlib)

        self.back = DummycubeCreate(0)
        self.scene = DummycubeCreate(0)
        self.front = DummycubeCreate(0)

        self.camPos = DummycubeCreate(self.scene)
        ObjectSetPosition(self.camPos, 2, 1, 0)
        ObjectTurn(self.camPos, -90)

        self.camera = CameraCreate(self.camPos)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        CameraSetNearPlaneBias(self.camera, 0.2)
        ViewerSetCamera(self.viewer, self.camera)

        self.shadowCasters = DummycubeCreate(self.scene)

        self.shadowPos = DummycubeCreate(self.scene)
        self.shadowCam = CameraCreate(self.shadowPos)
        CameraSetViewDepth(self.shadowCam, 500)
        ObjectSetPosition(self.shadowCam, 0, 10, 10)
        CameraSetTargetObject(self.shadowCam, self.camPos)
            
        self.shadowRes = 1024
        self.sm = ShadowMapCreate(self.shadowRes, self.viewer, self.shadowCasters)
        ShadowMapSetCamera(self.sm, self.shadowCam)
        ShadowMapSetProjectionSize(self.sm, 3)
        ShadowMapSetZClippingPlanes(self.sm, -10.0, 300.0)

        self.sm2 = ShadowMapCreate(self.shadowRes, self.viewer, self.shadowCasters)
        ShadowMapSetCamera(self.sm2, self.shadowCam)
        ShadowMapSetProjectionSize(self.sm2, 15)
        ShadowMapSetZClippingPlanes(self.sm2, -10.0, 300.0)

        self.sm3 = ShadowMapCreate(self.shadowRes, self.viewer, self.shadowCasters)
        ShadowMapSetCamera(self.sm3, self.shadowCam)
        ShadowMapSetProjectionSize(self.sm3, 100)
        ShadowMapSetZClippingPlanes(self.sm3, -10.0, 300.0)

        MaterialCreate('mGround', 'data/misc/tiles.jpg')
        MaterialSetOptions('mGround', 1, 1)
        MaterialSetTextureWrap('mGround', True)
        self.vp1 = textRead('data/shaders/csm/csm-vp.glsl')
        self.fp1 = textRead('data/shaders/csm/csm-fp.glsl')
        
        self.csmShader = GLSLShaderCreate(self.vp1, self.fp1)
        
        self.csmParamTexture = GLSLShaderCreateParameter(self.csmShader, 'texture')
        GLSLShaderSetParameterTexture(self.csmParamTexture, 'mGround', 0)
        
        self.csmParamShadowmap = GLSLShaderCreateParameter(self.csmShader, 'shadowMap1')
        GLSLShaderSetParameterShadowTexture(self.csmParamShadowmap, self.sm, 1)
        
        self.csmParamShadowmap2 = GLSLShaderCreateParameter(self.csmShader, 'shadowMap2')
        GLSLShaderSetParameterShadowTexture(self.csmParamShadowmap2, self.sm2, 2)
        
        self.csmParamShadowmap3 = GLSLShaderCreateParameter(self.csmShader, 'shadowMap3')
        GLSLShaderSetParameterShadowTexture(self.csmParamShadowmap3, self.sm3, 3)
        
        self.csmParamShadowMatrix = GLSLShaderCreateParameter(self.csmShader, 'shadowMatrix1')
        GLSLShaderSetParameterShadowMatrix(self.csmParamShadowMatrix, self.sm)
        
        self.csmParamShadowMatrix2 = GLSLShaderCreateParameter(self.csmShader, 'shadowMatrix2')
        GLSLShaderSetParameterShadowMatrix(self.csmParamShadowMatrix2, self.sm2)
        
        self.csmParamShadowMatrix3 = GLSLShaderCreateParameter(self.csmShader, 'shadowMatrix3')
        GLSLShaderSetParameterShadowMatrix(self.csmParamShadowMatrix3, self.sm3)
        
        self.csmParamShadowmapSize = GLSLShaderCreateParameter(self.csmShader, 'shadowMapSize')
        GLSLShaderSetParameter2f(self.csmParamShadowmapSize, self.shadowRes, self.shadowRes)
        
        self.usePCF = True
        self.csmParamUsePCF = GLSLShaderCreateParameter(self.csmShader, 'usePCF')
        GLSLShaderSetParameter1i(self.csmParamUsePCF, self.usePCF)
        
        self.drawCascades = False
        self.csmParamDrawCascades = GLSLShaderCreateParameter(self.csmShader, 'drawCascades')
        GLSLShaderSetParameter1i(self.csmParamDrawCascades, self.drawCascades)
        MaterialSetShader('mGround', self.csmShader)

        self.plane = PlaneCreate(0, 100, 100, 50, 50, self.scene)
        ObjectSetMaterial(self.plane, 'mGround')
        ObjectPitch(self.plane, 90)

        self.sphere = SphereCreate(0.25, 20, 10, self.shadowCasters)
        ObjectSetPosition(self.sphere, 20, 0.25, 1)
            
        self.cylinder = CylinderCreate(0.25, 0.25, 3, 16, 4, 1, self.shadowCasters)
        ObjectSetPosition(self.cylinder, 0, 1.5, -4)
        ObjectSetMaterial(self.cylinder, 'mGround')

        self.torus = TorusCreate(1, 10, 35, 10, self.shadowCasters) 
        ObjectSetPosition(self.torus, 0, 4, 2)
        ObjectPitch(self.torus, 90)
        ObjectSetMaterial(self.torus, 'mGround')

        self.cub = CubeCreate(10, 10, 2, self.shadowCasters) 
        ObjectSetPosition(self.cub, 0, 5, -10)
        ObjectSetMaterial(self.cub, 'mGround')

        self.cub2 = CubeCreate(10, 2, 2, self.shadowCasters) 
        ObjectSetPosition(self.cub2, 10, 9, -10)
        ObjectSetMaterial(self.cub2, 'mGround')

        self.cub3 = CubeCreate(10, 10, 2, self.shadowCasters) 
        ObjectSetPosition(self.cub3, 20, 5, -10)
        ObjectSetMaterial(self.cub3, 'mGround')

        self.cub4 = CubeCreate(3, 3, 1, self.shadowCasters) 
        ObjectSetPosition(self.cub4, 0, 1.5, -1.5)
        ObjectSetMaterial(self.cub4, 'mGround')

        self.matlib2 = MaterialLibraryCreate()
        MaterialLibrarySetTexturePaths(self.matlib2, 'data/hellknight');
        MaterialLibraryActivate(self.matlib2)

        MaterialCreate('mHellknight', 'diffuse.png')
        MaterialSetShininess('mHellknight', 32)
        MaterialSetAmbientColor('mHellknight', c_ltgray, 1)
        MaterialSetDiffuseColor('mHellknight', c_white, 1)
        MaterialSetSpecularColor('mHellknight', c_ltgray, 1)

        self.hk = ActorCreate('data/hellknight/hellknight.md5mesh', self.matlib2, self.shadowCasters)
        ActorAddObject(self.hk, 'data/hellknight/idle.md5anim')
        ActorAddObject(self.hk, 'data/hellknight/attack.md5anim')
        ActorSwitchToAnimation(self.hk, 0, True)
        ObjectSetScale(self.hk, 0.012, 0.012, 0.012)
        ObjectSetPosition(self.hk, 0, 0, 0)
        ObjectSetMaterial(self.hk, 'mHellknight')
            
        MaterialLibraryActivate(self.matlib);

        self.light = LightCreate(lsParallel, self.scene)
        LightSetAmbientColor(self.light, c_dkgray)
        LightSetDiffuseColor(self.light, c_white)
        LightSetSpecularColor(self.light, c_white)
        ObjectPitch(self.light, -135)

        self.light2 = LightCreate(lsOmni, self.scene)
        LightSetAmbientColor(self.light2, 0x005000)
        LightSetDiffuseColor(self.light2, c_green)
        LightSetSpecularColor(self.light2, c_green)
        LightSetAttenuation(self.light2, 1, 0.5, 0.1)
        ObjectSetPosition(self.light2, 0, 1.5, -3)

        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12)
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_black, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.setMouseToCenter()
        
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
            
        elif key == KEY_F12:
            self.makeScreenshot()
            
        elif key == KEY_C:
            self.drawCascades = not self.drawCascades
            GLSLShaderSetParameter1i(self.csmParamDrawCascades, self.drawCascades)
            
        elif key == KEY_P:
            self.usePCF = not self.usePCF
            GLSLShaderSetParameter1i(self.csmParamUsePCF, self.usePCF);
            
    def onMouseButtonDown(self, button):
        if button == MB_LEFT:
            c = CubeCreate(0.5, 0.5, 0.5, self.shadowCasters); 
            ObjectSetPositionOfObject(c, self.camera);
            ObjectSetRotation(c, random.randrange(45), random.randrange(45), random.randrange(45));
            ObjectSetMaterial(c, 'mGround');
        
    def update(self, dt):
        deltax = (self.halfWindowWidth - self.mouseX) / 3.0;
        deltay = (self.halfWindowHeight - self.mouseY) / 3.0;
        self.setMouseToCenter();
        ObjectRotate(self.camera, deltay, 0, 0);
        ObjectRotate(self.camPos, 0, -deltax, 0);
        
        if self.keyPressed[KEY_W]: ObjectMove(self.camPos, -5 * dt)
        if self.keyPressed[KEY_A]: ObjectStrafe(self.camPos, 5 * dt)
        if self.keyPressed[KEY_D]: ObjectStrafe(self.camPos, -5 * dt)
        if self.keyPressed[KEY_S]: ObjectMove(self.camPos, 5 * dt)
        
        if self.keyPressed[KEY_PAGEUP]: ObjectLift(self.camPos, 4 * dt);
        if self.keyPressed[KEY_PAGEDOWN]: ObjectLift(self.camPos, -4 * dt);

        ObjectSetPositionOfObject(self.shadowPos, self.camPos)

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        
        HUDTextSetText(self.text, 
            'Xtreme3D 3\r' + 'FPS: '+ str(framerate) +
            '\rPress <C> to switch cascades visualization' +
            '\rPress <P> to switch PCF (soft shadows)' +
            '\rPress <LMB> to create a cube')
            
        Update(dt)
    
    def render(self):
        ObjectHide(self.front)
        ShadowMapRender(self.sm)
        ShadowMapRender(self.sm2)
        ShadowMapRender(self.sm3)
        ObjectShow(self.front)

        GLSLShaderSetParameterShadowMatrix(self.csmParamShadowMatrix, self.sm)
        GLSLShaderSetParameterShadowMatrix(self.csmParamShadowMatrix2, self.sm2)
        GLSLShaderSetParameterShadowMatrix(self.csmParamShadowMatrix3, self.sm3)

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

app = CSMApplication(1280, 720, 'Xtreme3D 3 Cascaded Shadow Maps Demo')
app.run()
