import os.path
import time
import ctypes
import sdl2
from framework import *

"""
  Interactive UI Demo
  -------------------
  Author: Gecko
  Xtreme3D version: 3.8
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
  This demo shows how implement an interactive user interface like in Doom 3.
  Stand near the screen and move the cursor by rotating camera.
"""

class MyApplication(Framework):
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
        
        self.uiViewer = ViewerCreate(0, 0, 256, 256, windowHandle(self.window))
        ViewerSetBackgroundColor(self.uiViewer, c_black)
        ViewerSetLighting(self.uiViewer, False)
        ViewerEnableFog(self.uiViewer, False)
        ViewerSetAutoRender(self.uiViewer, False)
        ViewerSetVisible(self.uiViewer, False)

        self.matlib = MaterialLibraryCreate()
        MaterialLibraryActivate(self.matlib)

        MaterialCreate('mPlane', 'data/misc/tiles.jpg')
        MaterialCreate('mStone', 'data/room/stone.png')

        self.objects = DummycubeCreate(0)
        self.back = DummycubeCreate(self.objects)
        self.scene = DummycubeCreate(self.objects)
        self.front = DummycubeCreate(self.objects)
        
        self.interactive = DummycubeCreate(0)
        ObjectHide(self.interactive)

        self.light = LightCreate(lsOmni, self.scene)
        ObjectSetPosition(self.light, 2, 4, 2)

        self.shadowCasters = DummycubeCreate(self.scene)

        self.plane = ShadowplaneCreate(15, 15, 3, 3, self.shadowCasters, self.light, c_black, 0.7, self.scene)
        ObjectSetPosition(self.plane, 0, -2, 0)
        ObjectPitch(self.plane, 90)
        ObjectSetMaterial(self.plane, 'mPlane')
        
        MaterialCreate('mScreen', '')
        
        self.screenPlaneWidth = 1.0
        self.screenPlaneHeight = 1.0
        self.screenPlane = PlaneCreate(1, self.screenPlaneWidth, self.screenPlaneHeight, 1, 1, self.scene)
        ObjectSetPosition(self.screenPlane, 0, 1, 0)
        ObjectSetMaterial(self.screenPlane, 'mScreen')

        self.camPos = DummycubeCreate(self.scene)
        ObjectSetPosition(self.camPos, 0, 1, 5)
        self.camera = CameraCreate(self.camPos)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        ViewerSetCamera(self.viewer, self.camera)

        self.screenBufferWidth = 256
        self.screenBufferHeight = 256
        self.screenBuffer = FBOCreate(self.screenBufferWidth, self.screenBufferHeight, self.uiViewer)
        FBOSetCamera(self.screenBuffer, self.camera)
        ViewerSetCamera(self.uiViewer, self.camera)
        
        self.vp1 = textRead('data/shaders/ui/ui-vp.glsl')
        self.fp1 = textRead('data/shaders/ui/ui-fp.glsl')
        self.screenShader = GLSLShaderCreate(self.vp1, self.fp1)
        self.screenParamBuffer = GLSLShaderCreateParameter(self.screenShader, 'screenTexture')
        GLSLShaderSetParameterFBOColorTexture(self.screenParamBuffer, self.screenBuffer, 0)
        MaterialSetShader('mScreen', self.screenShader)
        
        MaterialCreate('mGrid', 'data/misc/grid.png')
        MaterialSetTextureFilter('mGrid', miLinear, maLinear)
        
        MaterialCreate('mCursor', 'data/misc/cursor.png')
        MaterialSetBlendingMode('mCursor', bmTransparency)
        MaterialSetTextureFilter('mCursor', miLinear, maLinear)
        
        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12)
        
        self.uiBackground = HUDSpriteCreate('mGrid', 256, 256, self.interactive)
        ObjectSetPosition(self.uiBackground, 128, 128, 0)
                
        self.uiButton = HUDShapeRectangleCreate(128, 32, self.interactive)
        self.uiButtonX = 128
        self.uiButtonY = 128
        self.uiButtonWidth = 128
        self.uiButtonHeight = 32
        self.uiButtonMouseOn = False
        self.uiButtonActive = False
        ObjectSetPosition(self.uiButton, 128, 128, 0)
        HUDShapeSetColor(self.uiButton, c_yellow, 1.0)
        
        self.uiText = HUDTextCreate(self.font, 'Press Me', self.interactive)
        HUDTextSetColor(self.uiText, c_black, 1.0)
        ObjectSetPosition(self.uiText, 92, 122, 0)
        
        self.cursorWidth = 32
        self.cursorHeight = 32
        self.uiCursor = HUDSpriteCreate('mCursor', 32, 32, self.interactive)
        ObjectSetPosition(self.uiCursor, 50, 50, 0)
        
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_white, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.setMouseToCenter()
        
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
        elif key == KEY_F12:
            self.makeScreenshot()
            
    def onMouseButtonDown(self, button):
        if button == MB_LEFT:
            if self.uiButtonMouseOn:
                self.uiButtonActive = not self.uiButtonActive
            
    def uiCursorOverArea(self, cx, cy, x, y, w, h):
        return cx > (x - w * 0.5) and cy > (y - h * 0.5) and cx < (x + w * 0.5) and cy < (y + h * 0.5)
        
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
        
        if ObjectGetDistance(self.camera, self.screenPlane) < 1.5:
            ObjectSceneRaycast(self.camera, self.screenPlane)
            rx = ObjectGetCollisionPosition(0) - ObjectGetAbsolutePosition(self.screenPlane, 0)
            ry = ObjectGetCollisionPosition(1) - ObjectGetAbsolutePosition(self.screenPlane, 1)
            rz = ObjectGetCollisionPosition(2) - ObjectGetAbsolutePosition(self.screenPlane, 2)
            cx = (rx / self.screenPlaneWidth + 0.5) * self.screenBufferWidth
            cy = (1.0 - (ry / self.screenPlaneHeight + 0.5)) * self.screenBufferHeight
            ObjectSetPosition(self.uiCursor, cx + self.cursorWidth * 0.5, cy + self.cursorHeight * 0.5, 0)
            
            if self.uiCursorOverArea(cx, cy, self.uiButtonX, self.uiButtonY, self.uiButtonWidth, self.uiButtonHeight):
                if self.uiButtonActive:
                    HUDShapeSetColor(self.uiButton, c_fuchsia, 1.0)
                    HUDTextSetColor(self.uiText, c_white, 1.0)
                else:
                    HUDShapeSetColor(self.uiButton, c_white, 1.0)
                    HUDTextSetColor(self.uiText, c_black, 1.0)
                self.uiButtonMouseOn = True
            else:
                if self.uiButtonActive:
                    HUDShapeSetColor(self.uiButton, c_red, 1.0)
                    HUDTextSetColor(self.uiText, c_white, 1.0)
                else:
                    HUDShapeSetColor(self.uiButton, c_yellow, 1.0)
                    HUDTextSetColor(self.uiText, c_black, 1.0)
                self.uiButtonMouseOn = False

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        HUDTextSetText(self.text, 'FPS: ' + str(framerate));

        Update(dt)
    
    def render(self):
        ViewerSetVisible(self.uiViewer, True)
        ObjectShow(self.interactive)
        FBORenderObjectEx(self.screenBuffer, self.interactive, True, True, False, False)
        ObjectHide(self.interactive)
        ViewerSetVisible(self.uiViewer, False)
    
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

app = MyApplication(1280, 720, 'Xtreme3D 3.8 Interactive GUI Demo')
app.run()
