import os.path
import time
import random
import ctypes
import sdl2
from framework import *

"""
  Bump/parallax mapping + shadows
  -------------------------------
  Author: Gecko
  Xtreme3D version: 3.8
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
  This demo shows how to use Xtreme3D's built-in BumpShader with parallax mapping, shadows,
  and multiple lights. It also demonstrates use of Movement object to define curved path
  for a light source.
"""

class CSMApplication(Framework):
    screenNum = 0

    def start(self):
        EngineCreate()

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

        self.shadowCam = CameraCreate(self.scene)
        CameraSetViewDepth(self.shadowCam, 500)
        self.target = DummycubeCreate(self.scene)
        ObjectSetPosition(self.target, 0, -1, 0)
        CameraSetTargetObject(self.shadowCam, self.target)
        ObjectSetPosition(self.shadowCam, 0, 10, 0)
        ObjectPitch(self.shadowCam, -70)
        ObjectTurn(self.shadowCam, -20)

        self.shadowRes = 1024
        self.sm = ShadowMapCreate(self.shadowRes, self.viewer, self.shadowCasters)
        ShadowMapSetCamera(self.sm, self.shadowCam)
        ShadowMapSetProjectionSize(self.sm, 5)
        ShadowMapSetZClippingPlanes(self.sm, -10.0, 200.0)

        self.bump = BumpShaderCreate()
        BumpShaderSetDiffuseTexture(self.bump, '')
        BumpShaderSetNormalTexture(self.bump, '')
        BumpShaderSetHeightTexture(self.bump, '')
        BumpShaderSetMaxLights(self.bump, 3)
        BumpShaderUseAutoTangentSpace(self.bump, True)
        BumpShaderUseParallax(self.bump, True)
        BumpShaderSetShadowMap(self.bump, self.sm)
        BumpShaderSetShadowBlurRadius(self.bump, 2)

        self.col_ambient = 0x303030

        MaterialCreate('mStone', 'data/room/stone.png')
        MaterialLoadTextureEx('mStone', 'data/room/stone-normal.png', 1)
        MaterialLoadTextureEx('mStone', 'data/room/stone-height.png', 2)
        MaterialSetShininess('mStone', 32)
        MaterialSetAmbientColor('mStone', self.col_ambient, 1)
        MaterialSetDiffuseColor('mStone', c_white, 1)
        MaterialSetSpecularColor('mStone', c_ltgray, 1)
        MaterialSetShader('mStone', self.bump)

        self.camPos = DummycubeCreate(self.scene)
        ObjectSetPosition(self.camPos, 2, 1, 0)
        ObjectTurn(self.camPos, -90)

        self.camera = CameraCreate(self.camPos)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        CameraSetNearPlaneBias(self.camera, 0.2)
        ViewerSetCamera(self.viewer, self.camera)

        self.ffm = FreeformCreate('data/room/room.b3d', self.matlib, self.matlib, self.scene)
        FreeformGenTangents(self.ffm)
        ObjectSetMaterial(self.ffm, 'mStone')
        FreeformUseMeshMaterials(self.ffm, False)

        self.sphere = SphereCreate(0.25, 16, 8, self.shadowCasters)
        ObjectSetPosition(self.sphere, 1, 0.25, 1)
        ObjectSetMaterial(self.sphere, 'mStone')
            
        self.cylinder = CylinderCreate(0.25, 0.25, 0.5, 16, 4, 1, self.shadowCasters)
        ObjectSetPosition(self.cylinder, -1, 0.25, 1)
        ObjectSetMaterial(self.cylinder, 'mStone')

        self.torus = TorusCreate(0.1, 0.25, 16, 8, self.shadowCasters) 
        ObjectSetPosition(self.torus, 0, 0.25, 1)
        ObjectPitch(self.torus, 90)
        ObjectSetMaterial(self.torus, 'mStone')

        self.matlib2 = MaterialLibraryCreate()
        MaterialLibrarySetTexturePaths(self.matlib2, 'data/hellknight') 
        MaterialLibraryActivate(self.matlib2)

        MaterialCreate('mHellknight', 'diffuse.png')
        MaterialCreate('mHellknightNormal', 'normal.png')
        MaterialSetSecondTexture('mHellknight', 'mHellknightNormal')
        MaterialSetShininess('mHellknight', 32)
        MaterialSetAmbientColor('mHellknight', self.col_ambient, 1)
        MaterialSetDiffuseColor('mHellknight', c_white, 1)
        MaterialSetSpecularColor('mHellknight', c_ltgray, 1)
        MaterialSetShader('mHellknight', self.bump)

        self.hk = ActorCreate('data/hellknight/hellknight.md5mesh', self.matlib2, self.shadowCasters)
        ActorAddObject(self.hk, 'data/hellknight/idle.md5anim')
        ActorAddObject(self.hk, 'data/hellknight/attack.md5anim')
        ActorSwitchToAnimation(self.hk, 0, True)
        ObjectSetScale(self.hk, 0.012, 0.012, 0.012)
        ObjectSetPosition(self.hk, 0, 0, 0)
        ObjectSetMaterial(self.hk, 'mHellknight')
        
        LightFXCreate(self.sphere);
            
        MaterialLibraryActivate(self.matlib)

        self.movingObject = DummycubeCreate(self.scene)
        ObjectSetPosition(self.movingObject, 2, 1.5, 2)

        MaterialCreate('mFlare', 'data/misc/flare.png')
        MaterialSetOptions('mFlare', True, True)
        MaterialSetBlendingMode('mFlare', bmAdditive)

        self.spr = SpriteCreate('mFlare', 0.7, 0.7, self.movingObject)
        self.tr = TrailCreate(self.movingObject, self.scene);
        TrailSetMarkWidth(self.tr, 0.1)
        TrailSetLimits(self.tr, 300, 1.0)

        self.m = MovementCreate(self.movingObject)
        self.pa = MovementAddPath(self.m)
        self.node = MovementPathAddNode(self.pa)
        MovementPathNodeSetPosition(self.node, 2, 1.5, 2)
        self.node = MovementPathAddNode(self.pa)
        MovementPathNodeSetPosition(self.node, -2, 1.5, 2)
        self.node = MovementPathAddNode(self.pa)
        MovementPathNodeSetPosition(self.node, -2, 0.5, -2)
            
        self.pa2 = MovementAddPath(self.m)
        self.node = MovementPathAddNode(self.pa2)
        MovementPathNodeSetPosition(self.node, -2, 0.5, -2)
        self.node = MovementPathAddNode(self.pa2)
        MovementPathNodeSetPosition(self.node, 2, 0.5, -2)
        self.node = MovementPathAddNode(self.pa2)
        MovementPathNodeSetPosition(self.node, 2, 1.5, 2)

        MovementSetActivePath(self.m, 0)
        MovementAutoStartNextPath(self.m, True)
        MovementStart(self.m)

        self.light=LightCreate(lsOmni, self.sphere)
        LightSetAmbientColor(self.light,c_black)
        LightSetDiffuseColor(self.light,c_orange)
        LightSetSpecularColor(self.light,c_orange)
        LightSetAttenuation(self.light, 1, 0.3, 0.0)
        ObjectSetPosition(self.light, 1, 1, 1)

        self.light2=LightCreate(lsOmni, self.sphere)
        LightSetAmbientColor(self.light2,c_black)
        LightSetDiffuseColor(self.light2,c_aqua)
        LightSetSpecularColor(self.light2,c_aqua)
        LightSetAttenuation(self.light2, 1, 0.3, 0.0)
        ObjectSetPosition(self.light2,-1, 1, 0)

        self.light3=LightCreate(lsOmni, self.movingObject)
        LightSetAmbientColor(self.light3,c_black)
        LightSetDiffuseColor(self.light3,c_ltgray)
        LightSetSpecularColor(self.light3,c_ltgray)
        LightSetAttenuation(self.light3, 1, 0.3, 0.0)

        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12)
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_white, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.setMouseToCenter()
        
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
        elif key == KEY_RETURN:
            MovementSetActivePath(self.m, 0);
            MovementStart(self.m)
        elif key == KEY_F12:
            self.makeScreenshot()
            
    def onMouseButtonDown(self, button):
        pass
        
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
        
        ObjectSetPositionOfObject(self.shadowCam, self.movingObject)

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        
        HUDTextSetText(self.text, 'Xtreme3D 3.8\r' + 'FPS: ' + str(framerate) + '\rPress <Enter> to restart light movement');
            
        Update(dt)
    
    def render(self):
        ObjectHide(self.front)
        ShadowMapRender(self.sm)
        ObjectShow(self.front)

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

app = CSMApplication(1280, 720, 'Xtreme3D 3.8 Bump/Parallax Mapping + Shadows Demo')
app.run()
