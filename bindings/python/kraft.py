import os.path
import time
import ctypes
import sdl2
import math
from framework import *

"""
  Kraft Physics Demo
  ------------------
  Author: Gecko
  Xtreme3D version: 3.8
  Python version (tested): 2.7.11
  License: Public Domain (or CC-0)
  -------------------------
  This demo shows how to use Kraft physics engine -
  creating static and dynamic bodies, static mesh, and character controller.
"""

def distance(x1, y1, z1, x2, y2, z2):
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2
    return math.sqrt(dx * dx + dy * dy + dz * dz)

class MyApplication(Framework):
    screenNum = 0
    onGround = False

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
        
        MaterialCreate('mCrate', 'data/misc/crate.png')
        MaterialSetAmbientColor('mCrate', c_ltgray, 1.0)

        self.back = DummycubeCreate(0)
        self.scene = DummycubeCreate(0)
        self.front = DummycubeCreate(0)
        
        self.light = LightCreate(lsOmni, self.scene)
        ObjectSetPosition(self.light, 5, 8, 5)
        
        self.kraft = KraftCreate()
        
        self.levelGeometry = DummycubeCreate(self.scene)
        self.shadowCasters = DummycubeCreate(self.levelGeometry)
        
        self.levelMlib = MaterialLibraryCreate()
        MaterialLibrarySetTexturePaths(self.levelMlib, 'data/b3d')
        self.level = FreeformCreate('data/b3d/level.b3d', self.levelMlib, self.levelMlib, self.scene)
        FreeformBuildOctree(self.level)
        ObjectSetPosition(self.level, 0, -0.5, 0)
        
        self.levelRb = KraftCreateRigidBody(self.kraft, 1)
        KraftObjectSetRigidBody(self.level, self.levelRb)
        KraftCreateShapeMesh(self.levelRb, self.level)
        KraftRigidBodyFinish(self.levelRb)
        
        self.planePivot = DummycubeCreate(self.levelGeometry)
        ObjectSetPosition(self.planePivot, 0, -2, 0)
        self.rb = KraftCreateRigidBody(self.kraft, 1)
        self.sPlane = KraftCreateShapePlane(self.rb, 0, 1, 0, 0)
        KraftRigidBodyFinish(self.rb)

        cubes = []
        for i in range(0, 2):
            cube = CubeCreate(1, 1, 1, self.shadowCasters)
            ObjectSetPosition(cube, 0, 2 + i * 2.5, 0)
            ObjectSetMaterial(cube, 'mCrate')
            rb2 = KraftCreateRigidBody(self.kraft, 2)
            KraftObjectSetRigidBody(cube, rb2)
            sCube1 = KraftCreateShapeBox(rb2, 0.5, 0.5, 0.5)
            KraftShapeSetDensity(sCube1, 200.0)
            KraftRigidBodyFinish(rb2)
            cubes.append(rb2) 
            
        j = KraftCreateJointBallSocket(cubes[0], cubes[1])
        KraftJointSetAnchor1(j, 0, 2, 0)
        KraftJointSetAnchor2(j, 0, -2, 0)
        KraftJointSetHingeAxis1(j, 1, 0, 0)
        KraftJointSetHingeAxis2(j, 1, 0, 0)
        
        self.characterRadius = 1.0
        self.character = DummycubeCreate(self.scene)
        ObjectSetPosition(self.character, 0, 6, 5)
        self.rb3 = KraftCreateRigidBody(self.kraft, 2)
        KraftRigidBodySetGravity(self.rb3, 0, -1, 0, 0)
        KraftObjectSetRigidBody(self.character, self.rb3)
        self.sSphere = KraftCreateShapeSphere(self.rb3, self.characterRadius)
        KraftShapeSetDensity(self.sSphere, 1.0)
        KraftShapeSetFriction(self.sSphere, 0.0)
        KraftShapeSetRayCastable(self.sSphere, False)
        KraftRigidBodyFinish(self.rb3)

        self.camPos = DummycubeCreate(self.scene)        
        self.camera = CameraCreate(self.camPos)
        ObjectSetPosition(self.camera, 0, 0.7, 0)
        CameraSetViewDepth(self.camera, 500)
        CameraSetFocal(self.camera, 80)
        CameraSetNearPlaneBias(self.camera, 0.5)
        ViewerSetCamera(self.viewer, self.camera)
        
        self.font = TTFontCreate('data/NotoSans-Regular.ttf', 12)
        self.text = HUDTextCreate(self.font, '', self.front)
        HUDTextSetColor(self.text, c_white, 1.0)
        ObjectSetPosition(self.text, 20, 20, 0)
        
        self.mouseLook = True
        
        self.setMouseToCenter()
        
    def onKeyDown(self, key):
        if key == KEY_ESCAPE:
            self.running = False
        elif key == KEY_F12:
            self.makeScreenshot()
        
    def onMouseButtonDown(self, button):
        self.mouseLook = not self.mouseLook
        sdl2.SDL_ShowCursor(not self.mouseLook)
        self.setMouseToCenter()
    
    verticalSpeed = 0  

    def update(self, dt):
        if self.mouseLook:
            deltax = (self.halfWindowWidth - self.mouseX) / 3.0
            deltay = (self.halfWindowHeight - self.mouseY) / 3.0
            self.setMouseToCenter()
            ObjectRotate(self.camera, deltay, 0, 0)
            ObjectRotate(self.camPos, 0, -deltax, 0)
        
        px = ObjectGetAbsolutePosition(self.character, 0)
        py = ObjectGetAbsolutePosition(self.character, 1)
        pz = ObjectGetAbsolutePosition(self.character, 2)
        dx = ObjectGetAbsoluteDirection(self.camPos, 0) * self.characterRadius
        dy = ObjectGetAbsoluteDirection(self.camPos, 1) * self.characterRadius
        dz = ObjectGetAbsoluteDirection(self.camPos, 2) * self.characterRadius
        self.onGround = False
        characterMarginWidth = 0.5
        
        avgHeight = 0
        hitNormal = 1
        if KraftRayCast(self.kraft, px, py, pz, 0, -1, 0, 100):
            avgHeight = KraftGetRayHitPosition(1)
            hitNormal = KraftGetRayHitNormal(1)
            minDist = self.characterRadius + 0.001
            minDist2 = self.characterRadius + 0.2
            if abs(py - avgHeight) <= minDist:
                self.onGround = True
            elif abs(py - avgHeight) <= minDist2:
                if KraftRayCast(self.kraft, px + dx, py + dy + 10, pz + dz, 0, -1, 0, 100):
                    avgHeight = (avgHeight + KraftGetRayHitPosition(1)) * 0.5
                    minDist = self.characterRadius + characterMarginWidth
                    if abs(py - avgHeight) <= minDist:
                        self.onGround = True
                        
        if self.onGround:
            self.verticalSpeed = 0
        else:
            self.verticalSpeed = self.verticalSpeed - 20 * dt
        
        moveSpd = 0
        strafeSpd = 0
        if self.keyPressed[KEY_W]: moveSpd = -7
        if self.keyPressed[KEY_S]: moveSpd = 7
        if self.keyPressed[KEY_A]: strafeSpd = -7
        if self.keyPressed[KEY_D]: strafeSpd = 7
        if self.keyPressed[KEY_SPACE] and self.onGround: 
            svx = KraftRigidBodyGetLinearVelocity(self.rb3, 0) 
            svz = KraftRigidBodyGetLinearVelocity(self.rb3, 2) 
            self.verticalSpeed = 7
        
        vy = self.verticalSpeed   
        vx = ObjectGetAbsoluteDirection(self.camPos, 0) * moveSpd + ObjectGetAbsoluteRight(self.camPos, 0) * strafeSpd
        vz = ObjectGetAbsoluteDirection(self.camPos, 2) * moveSpd + ObjectGetAbsoluteRight(self.camPos, 2) * strafeSpd        

        if abs(vy) < 0.1:
            vy = 0
        if abs(vx) < 0.1:
            vx = 0
        if abs(vz) < 0.1:
            vz = 0
            
        svx = KraftRigidBodyGetLinearVelocity(self.rb3, 0) 
        svy = KraftRigidBodyGetLinearVelocity(self.rb3, 1) 
        svz = KraftRigidBodyGetLinearVelocity(self.rb3, 2) 
        
        dvx = vx - svx
        dvy = vy - svy
        dvz = vz - svz
        
        velocitySmoothCoef = 1.0
            
        KraftRigidBodySetLinearVelocity(self.rb3, svx + dvx * velocitySmoothCoef, vy, svz + dvz * velocitySmoothCoef) 
        KraftRigidBodySetAngularVelocity(self.rb3, 0, 0, 0)

        framerate = int(ViewerGetFramesPerSecond(self.viewer))
        HUDTextSetText(self.text, 'FPS: ' + str(framerate))
        
        KraftStep(self.kraft, dt)

        Update(dt)
        
        ObjectSetAbsolutePosition(self.camPos,
           ObjectGetAbsolutePosition(self.character, 0),
           ObjectGetAbsolutePosition(self.character, 1),
           ObjectGetAbsolutePosition(self.character, 2))
        
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

app = MyApplication(1280, 720, 'Xtreme3D 3.8 Kraft Demo')
app.run()
