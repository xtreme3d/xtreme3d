module main;

import std.stdio;
import std.conv;
import std.string;
import std.math;
import std.algorithm;
import std.file;

import derelict.sdl2.sdl;
import derelict.xtreme3d.xtreme3d;

double windowHandle(SDL_Window* wnd)
{
    SDL_SysWMinfo winInfo;
    SDL_GetWindowWMInfo(wnd, &winInfo);
    auto hwnd = winInfo.info.win.window;
    return cast(double)(cast(int)hwnd);
}

void main()
{
    DerelictSDL2.load();
    DerelictXtreme3D.load();

    if (SDL_Init(SDL_INIT_EVERYTHING) == -1)
        throw new Exception("Failed to init SDL: " ~ to!string(SDL_GetError()));
 
    uint windowWidth = 1280;
    uint windowHeight = 720;

    SDL_Window* window1 = null;
    window1 = SDL_CreateWindow("Xtreme3D Application", 100, 100, windowWidth, windowHeight, SDL_WINDOW_SHOWN);
    if (window1 == null)
        throw new Exception("Failed to create window: " ~ to!string(SDL_GetError()));
    SDL_GL_SetSwapInterval(1);

    EngineCreate();
    
    double matlib = MaterialLibraryCreate();
    MaterialLibraryActivate(matlib);
    
    double view = ViewerCreate(0, 0, windowWidth, windowHeight, windowHandle(window1));
    ViewerSetBackgroundColor(view, c_dkgray);
    ViewerSetAntiAliasing(view, aa4xHQ);
    ViewerSetLighting(view, true);
    ViewerEnableFog(view, true);
    ViewerSetFogDistance(view, 50, 100);
    ViewerEnableVSync(view, vsmSync);
    
    double back = DummycubeCreate(0);
    double scene = DummycubeCreate(0);
    double front = DummycubeCreate(0);
    
    double camPos = DummycubeCreate(scene);
    ObjectSetPosition(camPos, 0, 1, 0);
    double camera = CameraCreate(camPos);
    CameraSetViewDepth(camera, 500);
    CameraSetFocal(camera, 80);
    ViewerSetCamera(view, camera);
    
    double light = LightCreate(lsOmni, scene);
    ObjectSetPosition(light, 5, 8, 0);
    LightSetDiffuseColor(light, c_white);
    LightSetSpecularColor(light, c_white);
    
    double light2 = LightCreate(lsOmni, scene);
    ObjectSetPosition(light2, -5, 8, 0);
    LightSetDiffuseColor(light2, c_aqua);
    LightSetSpecularColor(light2, c_white);
    
    double shadowCasters = DummycubeCreate(scene);
    
    double plane = ShadowplaneCreate(15, 15, 3, 3, shadowCasters, light, c_black, 0.7, scene); 
    ObjectSetPosition(plane, 0, -3, -5);
    ObjectPitch(plane, 90);
    
    double matlib2 = MaterialLibraryCreate();
    MaterialLibrarySetTexturePaths(matlib2, "data/hellknight".strPtr);
    double hk = ActorCreate("data/hellknight/hellknight.md5mesh".strPtr, matlib2, shadowCasters);
    ActorAddObject(hk, "data/hellknight/idle2.md5anim".strPtr);
    ActorSwitchToAnimation(hk, 0, true);
    ObjectSetScale(hk, 0.05, 0.05, 0.05);
    ObjectSetPosition(hk, 0, -3, -5);
    
    MaterialLibraryActivate(matlib2);
    
    MaterialCreate("mHellknightDiffuse".strPtr, "diffuse.png".strPtr);
    MaterialCreate("mHellknightNormal".strPtr, "normal.png".strPtr);
    
    string vp1 = readText("data/hellknight/bump-vp.glsl");
    string fp1 = readText("data/hellknight/bump-fp.glsl");
    
    double bump = GLSLShaderCreate(vp1.strPtr, fp1.strPtr);
    double diffuseMapParam = GLSLShaderCreateParameter(bump, "diffuseMap".strPtr);
    GLSLShaderSetParameterTexture(diffuseMapParam, "mHellknightDiffuse".strPtr, 0);
    double normalMapParam = GLSLShaderCreateParameter(bump, "normalMap".strPtr);
    GLSLShaderSetParameterTexture(normalMapParam, "mHellknightNormal".strPtr, 1);
    double maxNumLightsParam = GLSLShaderCreateParameter(bump, "maxNumLights".strPtr);
    GLSLShaderSetParameter1i(maxNumLightsParam, 2);
    
    MaterialCreate("mHellknight".strPtr, "".strPtr);
    MaterialSetShininess("mHellknight".strPtr, 16);
    MaterialSetAmbientColor("mHellknight".strPtr, c_dkgray, 1);
    MaterialSetDiffuseColor("mHellknight".strPtr, c_white, 1);
    MaterialSetSpecularColor("mHellknight".strPtr, c_gray, 1);
    MaterialSetShader("mHellknight".strPtr, bump);

    ObjectSetMaterial(hk, "mHellknight".strPtr);
    
    double font = WindowsBitmapfontCreate("Arial".strPtr, 20, 0, 128);
    double text = HUDTextCreate(font, "Xtreme3D 3.0".strPtr, front);
    HUDTextSetColor(text, c_white, 0.5);
    ObjectSetPosition(text, 20, 20, 0);
    
    bool[512] keyPressed = false;
    int mouseX = 0;
    int mouseY = 0;
    
    int currentTime = 0;
    int lastTime = 0;
    double deltaTime = 0.0;
    
    int mx = windowWidth / 2;
    int my = windowHeight / 2;
    SDL_WarpMouseInWindow(window1, mx, my);
    SDL_ShowCursor(0);
    
    char[15] fpsStr;
 
    bool running = true;
    while(running)
    {
        currentTime = SDL_GetTicks();
        auto elapsedTime = currentTime - lastTime;
        lastTime = currentTime;
        deltaTime = cast(double)(elapsedTime) * 0.001;

        SDL_Event event;
        while (SDL_PollEvent(&event))
        {
            switch(event.type)
            {
                case SDL_QUIT:
                    running = false;
                    break;
 
                case SDL_KEYDOWN:
                    keyPressed[event.key.keysym.sym] = true;
                    switch(event.key.keysym.sym)
                    {
                        case SDLK_ESCAPE:
                            running = false;
                            break;
                        default:
                            break;
                    }
                    break;
                    
                case SDL_KEYUP:
                    keyPressed[event.key.keysym.sym] = false;
                    break;
                    
                case SDL_MOUSEMOTION:
                    mouseX = event.motion.x;
                    mouseY = event.motion.y;

                    break;
                
                default:
                    break;
            } 
        }
        
        double deltax = cast(double)(mx - mouseX) / 3.0;
        double deltay = cast(double)(my - mouseY) / 3.0;
        SDL_WarpMouseInWindow(window1, mx, my);

        ObjectRotate(camera, deltay, 0, 0);
        ObjectRotate(camPos, 0, -deltax, 0);
        
        if (keyPressed['w']) ObjectMove(camPos, -10 * deltaTime);
        if (keyPressed['a']) ObjectStrafe(camPos, 10 * deltaTime);
        if (keyPressed['d']) ObjectStrafe(camPos, -10 * deltaTime);
        if (keyPressed['s']) ObjectMove(camPos, 10 * deltaTime);
        
        int framerate = cast(int)ViewerGetFramesPerSecond(view);
        sprintf(fpsStr.ptr, "FPS %d", framerate);
        HUDTextSetText(text, fpsStr.ptr);
        
        Update(deltaTime);
        ViewerRender(view);
    }
    
    EngineDestroy();

    SDL_DestroyWindow(window1);
    SDL_Quit();
}

