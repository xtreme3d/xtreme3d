import os.path
import time
import random
import ctypes
import sdl2
from framework import *

"""
  Water
  -------------------------------
  Author: Gecko
  Xtreme3D version: 3.8+
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
  This demo shows how to create realistic water in Xtreme3D. It features reflection,
  refraction, Fresnel effect, subsurface scattering effect, animated waves, and chromatic aberration.
"""

class CSMApplication(Framework):
    screenNum = 0

    def start(self):
        EngineCreate()

        self.bgColor = MakeColorRGB(142, 200, 232)

        self.viewer = ViewerCreate(0, 0, self.windowWidth, self.windowHeight, windowHandle(self.window))
        ViewerSetBackgroundColor(self.viewer, self.bgColor)
        ViewerSetAntiAliasing(self.viewer, aa4xHQ)
        ViewerSetLighting(self.viewer, True)
        ViewerEnableFog(self.viewer, False)
        ViewerSetFogColor(self.viewer, self.bgColor)
        ViewerSetFogDistance(self.viewer, 100, 200)
        ViewerEnableVSync(self.viewer, vsmSync)
        ViewerSetAutoRender(self.viewer, False)

        self.matlib = MaterialLibraryCreate()
        MaterialLibraryActivate(self.matlib)

        self.backscene = DummycubeCreate(0)
        self.back = DummycubeCreate(self.backscene)
        self.scene = DummycubeCreate(self.backscene)
        self.front = DummycubeCreate(0)

        self.reflectionPlane = ClipPlaneCreate(self.scene)
        ClipPlaneSetPlane(self.reflectionPlane, 0, 0, 0, 0, 1, 0)

        self.cube = CubeCreate(1, 20, 1, self.reflectionPlane)
        ObjectSetPosition(self.cube, 0, 0, -20)

        self.levelMlib = MaterialLibraryCreate()
        MaterialLibrarySetTexturePaths(self.levelMlib, 'data/b3d')
        self.ffm1 = FreeformCreate('data/b3d/level.b3d', self.levelMlib, self.levelMlib, self.reflectionPlane)
        FreeformBuildOctree(self.ffm1)
        ObjectSetPosition(self.ffm1, 0, -0.5, 0)

        self.matlib2 = MaterialLibraryCreate()
        MaterialLibrarySetTexturePaths(self.matlib2, 'data/hellknight')
        MaterialLibraryActivate(self.matlib2)
        self.bump = BumpShaderCreate()
        BumpShaderSetDiffuseTexture(self.bump, '')
        BumpShaderSetNormalTexture(self.bump, '')
        BumpShaderSetMaxLights(self.bump, 3)
        BumpShaderUseAutoTangentSpace(self.bump, True)
        MaterialCreate('mHellknight', 'diffuse.png')
        MaterialCreate('mHellknightNormal', 'normal.png')
        MaterialSetSecondTexture('mHellknight', 'mHellknightNormal')
        MaterialSetShininess('mHellknight', 32)
        MaterialSetAmbientColor('mHellknight', c_gray, 1)
        MaterialSetDiffuseColor('mHellknight', c_white, 1)
        MaterialSetSpecularColor('mHellknight', c_ltgray, 1)
        MaterialSetShader('mHellknight', self.bump)
        self.hk = ActorCreate('data/hellknight/hellknight.md5mesh', self.matlib2, self.reflectionPlane)
        ActorAddObject(self.hk, 'data/hellknight/idle.md5anim')
        ActorAddObject(self.hk, 'data/hellknight/attack.md5anim')
        ActorSwitchToAnimation(self.hk, 0, True)
        ObjectSetScale(self.hk, 0.04, 0.04, 0.04)
        ObjectSetPosition(self.hk, 0, -0.5, 0)
        ObjectSetMaterial(self.hk, 'mHellknight')
        MaterialLibraryActivate(self.matlib)

        self.camPos = DummycubeCreate(self.scene)
        ObjectSetPosition(self.camPos, 0, 2, 5)

        self.camera = CameraCreate(self.camPos)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        CameraSetNearPlaneBias(self.camera, 0.2)
        ViewerSetCamera(self.viewer, self.camera)

        self.waterCamPos = DummycubeCreate(self.scene)
        ObjectSetPosition(self.waterCamPos, 0, -2, 5)
        self.waterCamera = CameraCreate(self.waterCamPos)
        CameraSetViewDepth(self.waterCamera, 500)
        CameraSetFocal(self.waterCamera, 80)
        CameraSetNearPlaneBias(self.waterCamera, 0.2)

        self.lightPivot = DummycubeCreate(self.scene);
        self.light=LightCreate(lsParallel, self.lightPivot);
        self.ambCol = MakeColorRGBFloat(0.3, 0.5, 0.7);
        LightSetAmbientColor(self.light, self.ambCol);
        LightSetDiffuseColor(self.light, c_white);
        LightSetSpecularColor(self.light, c_white);
        ObjectPitch(self.light, -20);
        ObjectTurn(self.lightPivot, 70);
        ObjectSetPosition(self.lightPivot, 0, 20, 0);

        MaterialCreate('mGround', 'data/b3d/ground.jpg')
        self.bottom = PlaneCreate(0, 100, 100, 50, 50, self.scene)
        ObjectSetPositionY(self.bottom, -4)
        ObjectPitch(self.bottom, 90)
        ObjectSetMaterial(self.bottom, 'mGround')

        MaterialCreate('mSkyTop','data/sky/sky_top.tga')
        MaterialCreate('mSkyBottom','data/sky/sky_bottom.tga')
        MaterialCreate('mSkyLeft','data/sky/sky_left.tga')
        MaterialCreate('mSkyRight','data/sky/sky_right.tga')
        MaterialCreate('mSkyFront','data/sky/sky_front.tga')
        MaterialCreate('mSkyBack','data/sky/sky_back.tga')
        self.sky=SkyboxCreate(self.back)
        SkyboxSetMaterial(self.sky, sbmTop, 'mSkyTop')
        SkyboxSetMaterial(self.sky, sbmBottom, 'mSkyBottom')
        SkyboxSetMaterial(self.sky, sbmLeft, 'mSkyLeft')
        SkyboxSetMaterial(self.sky, sbmRight, 'mSkyRight')
        SkyboxSetMaterial(self.sky, sbmFront, 'mSkyFront')
        SkyboxSetMaterial(self.sky, sbmBack, 'mSkyBack')

        self.vp2 = textRead('data/shaders/water/position-vp.glsl')
        self.fp2 = textRead('data/shaders/water/position-fp.glsl')
        self.posShader = GLSLShaderCreate(self.vp2, self.fp2)
        MaterialCreate('mPositionWriter', '')
        MaterialSetOptions('mPositionWriter', 1, 1)
        MaterialSetTextureWrap('mPositionWriter', True)
        MaterialSetShader('mPositionWriter', self.posShader)

        # reflection color buffer
        self.fbo1 = FBOCreate(self.windowWidth, self.windowHeight, self.viewer)
        FBOSetCamera(self.fbo1, self.waterCamera)

        # refraction color buffer
        self.fbo2 = FBOCreate(self.windowWidth, self.windowHeight, self.viewer)
        FBOSetCamera(self.fbo2, self.camera)

        # refraction position buffer
        self.fbo3 = FBOCreate(self.windowWidth, self.windowHeight, self.viewer)
        FBOSetCamera(self.fbo3, self.camera)
        FBOSetColorTextureFormat(self.fbo3, tfRGBAFloat16)
        FBOSetOverrideMaterial(self.fbo3, self.matlib, 'mPositionWriter')

        MaterialCreate('mWater', '')
        MaterialSetOptions('mWater', 1, 1)
        MaterialSetTextureWrap('mWater', True)

        MaterialCreate('mWaterNormal1','data/water/water-norm-001.png')
        MaterialCreate('mWaterNormal2','data/water/water-norm-002.png')

        self.vp1 = textRead('data/shaders/water/water-vp.glsl')
        self.fp1 = textRead('data/shaders/water/water-fp.glsl')
        self.waterShader = GLSLShaderCreate(self.vp1, self.fp1)
        
        self.waterParamViewMatrix = GLSLShaderCreateParameter(self.waterShader, 'viewMatrix')
        GLSLShaderSetParameterViewMatrix(self.waterParamViewMatrix)
        
        self.waterParamInvViewMatrix = GLSLShaderCreateParameter(self.waterShader, 'invViewMatrix')
        GLSLShaderSetParameterInvViewMatrix(self.waterParamInvViewMatrix)
        
        self.waterParamReflectionTexture = GLSLShaderCreateParameter(self.waterShader, 'reflectionMap')
        GLSLShaderSetParameterFBOColorTexture(self.waterParamReflectionTexture, self.fbo1, 0)
        
        self.waterParamRefractionTexture = GLSLShaderCreateParameter(self.waterShader, 'refractionMap')
        GLSLShaderSetParameterFBOColorTexture(self.waterParamRefractionTexture, self.fbo2, 1)
        
        self.waterParamDepth = GLSLShaderCreateParameter(self.waterShader, 'depthMap')
        GLSLShaderSetParameterFBODepthTexture(self.waterParamDepth, self.fbo2, 2)
        
        self.waterParamNormalmap1 = GLSLShaderCreateParameter(self.waterShader, 'normalMap1')
        GLSLShaderSetParameterTexture(self.waterParamNormalmap1, 'mWaterNormal1', 3)
        
        self.waterParamNormalmap2 = GLSLShaderCreateParameter(self.waterShader, 'normalMap2')
        GLSLShaderSetParameterTexture(self.waterParamNormalmap2, 'mWaterNormal2', 4)
        
        self.waterParamPositionMap = GLSLShaderCreateParameter(self.waterShader, 'positionMap')
        GLSLShaderSetParameterFBOColorTexture(self.waterParamPositionMap, self.fbo3, 5)
        
        self.waterParamViewSize = GLSLShaderCreateParameter(self.waterShader, 'viewSize')
        GLSLShaderSetParameter2f(self.waterParamViewSize, self.windowWidth, self.windowHeight)
        
        self.waterParamScrollTime = GLSLShaderCreateParameter(self.waterShader, 'scrollTime')
        self.scrollTime = 0.0
        GLSLShaderSetParameter1f(self.waterParamScrollTime, self.scrollTime)
        
        MaterialSetShader('mWater', self.waterShader)

        self.water = PlaneCreate(0, 100, 100, 50, 50, self.scene)
        ObjectPitch(self.water, 90)
        ObjectSetMaterial(self.water, 'mWater')

        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12)
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_white, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.setMouseToCenter()
        
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
        elif key == KEY_RETURN:
            MovementSetActivePath(self.m, 0)
            MovementStart(self.m)
        elif key == KEY_F12:
            self.makeScreenshot()
            
    def onMouseButtonDown(self, button):
        pass
        
    def update(self, dt):
        deltax = (self.halfWindowWidth - self.mouseX) / 3.0
        deltay = (self.halfWindowHeight - self.mouseY) / 3.0
        self.setMouseToCenter()
        ObjectRotate(self.camera, deltay, 0, 0)
        ObjectRotate(self.camPos, 0, -deltax, 0)
        ObjectRotate(self.waterCamera, -deltay, 0, 0)
        ObjectRotate(self.waterCamPos, 0, -deltax, 0)
        
        if self.keyPressed[KEY_W]: ObjectMove(self.camPos, -10 * dt)
        if self.keyPressed[KEY_A]: ObjectStrafe(self.camPos, 10 * dt)
        if self.keyPressed[KEY_D]: ObjectStrafe(self.camPos, -10 * dt)
        if self.keyPressed[KEY_S]: ObjectMove(self.camPos, 10 * dt)
        
        if self.keyPressed[KEY_PAGEUP]: ObjectLift(self.camPos, 4 * dt);
        if self.keyPressed[KEY_PAGEDOWN]: ObjectLift(self.camPos, -4 * dt);
        
        camPosX = ObjectGetPosition(self.camPos, 0)
        camPosY = ObjectGetPosition(self.camPos, 1)
        camPosZ = ObjectGetPosition(self.camPos, 2)
        ObjectSetPosition(self.waterCamPos, camPosX, -camPosY, camPosZ)

        self.scrollTime += dt;
        GLSLShaderSetParameter1f(self.waterParamScrollTime, self.scrollTime)

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        
        HUDTextSetText(self.text, 'Xtreme3D 3\r' + 'FPS: ' + str(framerate));
            
        Update(dt)
    
    def render(self):
        ObjectHide(self.water)
        ClipPlaneEnable(self.reflectionPlane, True)
        ClipPlaneSetPlane(self.reflectionPlane, 0, -0.2, 0, 0, 1, 0)
        FBORenderObject(self.fbo1, self.backscene)
        ClipPlaneSetPlane(self.reflectionPlane, 0, 0.2, 0, 0, -1, 0)
        FBORenderObject(self.fbo2, self.backscene)
        FBORenderObject(self.fbo3, self.backscene)
        ClipPlaneEnable(self.reflectionPlane, False)
        ObjectShow(self.water)

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

app = CSMApplication(1280, 720, 'Xtreme3D 3 Water Demo')
app.run()
