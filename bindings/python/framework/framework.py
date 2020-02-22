import time
import ctypes
import sdl2
from keycodes import *

def windowHandle(sdlwnd):    
    info = sdl2.SDL_SysWMinfo()
    sdl2.SDL_GetWindowWMInfo(sdlwnd, ctypes.byref(info)) 
    return info.info.win.window
    
def textRead(filename):
    f = open(filename, 'r')
    return f.read()

class Framework:    
    windowWidth = 640
    windowHeight = 480
    window = None
    
    keyPressed = [False] * 512
    mouseButtonPressed = [False] * 255
    mouseX = 0
    mouseY = 0
    
    halfWindowWidth = 0
    halfWindowHeight = 0

    running = True
    fixedTimeStep = 1.0 / 60.0
    timer = 0
    lastTime = 0

    def __init__(self, w, h, title):
        self.windowWidth = w
        self.windowHeight = h
        self.windowTitle = title
        
        self.halfWindowWidth = self.windowWidth / 2
        self.halfWindowHeight = self.windowHeight / 2
    
        sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)
        self.window = sdl2.SDL_CreateWindow(self.windowTitle,
                      sdl2.SDL_WINDOWPOS_CENTERED,
                      sdl2.SDL_WINDOWPOS_CENTERED, 
                      self.windowWidth, self.windowHeight,
                      sdl2.SDL_WINDOW_SHOWN)
                      
        sdl2.SDL_WarpMouseInWindow(self.window, self.halfWindowWidth, self.halfWindowHeight)
        sdl2.SDL_ShowCursor(0)
        self.start()
        
    def start(self):
        pass

    def update(self, dt):
        pass
    
    def render(self):
        pass
    
    def onKeyDown(self, key):
        pass
            
    def onKeyUp(self, key):
        pass
    
    def onMouseButtonDown(self, button):
        pass
        
    def onMouseButtonUp(self, button):
        pass
        
    def setMouse(self, x, y):
        sdl2.SDL_WarpMouseInWindow(self.window, x, y)
    
    def setMouseToCenter(self):
        self.setMouse(self.halfWindowWidth, self.halfWindowHeight)
        
    def run(self):
        event = sdl2.SDL_Event()
        while self.running:
            while sdl2.SDL_PollEvent(ctypes.byref(event)) != 0:
                if event.type == sdl2.SDL_QUIT:
                    self.running = False
                elif event.type == sdl2.SDL_KEYDOWN:
                    self.keyPressed[event.key.keysym.scancode] = True
                    self.onKeyDown(event.key.keysym.scancode)
                elif event.type == sdl2.SDL_KEYUP:
                    self.keyPressed[event.key.keysym.scancode] = False
                    self.onKeyUp(event.key.keysym.scancode)
                elif event.type == sdl2.SDL_MOUSEBUTTONDOWN:
                    self.mouseButtonPressed[event.button.button] = True
                    self.onMouseButtonDown(event.button.button)
                elif event.type == sdl2.SDL_MOUSEBUTTONUP:
                    self.mouseButtonPressed[event.button.button] = False
                    self.onMouseButtonUp(event.button.button)
                elif event.type == sdl2.SDL_MOUSEMOTION:
                    self.mouseX = event.motion.x
                    self.mouseY = event.motion.y

            self.currentTime = sdl2.SDL_GetTicks()
            elapsedTime = self.currentTime - self.lastTime
            self.lastTime = self.currentTime
            dt = elapsedTime * 0.001

            self.timer += dt
            if (self.timer >= self.fixedTimeStep):
                self.timer -= self.fixedTimeStep
                self.update(self.fixedTimeStep)
                
            self.render()

        sdl2.SDL_DestroyWindow(self.window)
        sdl2.SDL_Quit()
