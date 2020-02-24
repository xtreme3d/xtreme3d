local ffi = require("ffi")
local x3d = require("x3d")
local utils = require("utils")

windowWidth = 1280
windowHeight = 720

x3d.EngineCreate()
local window = x3d.WindowCreate(0, 0, windowWidth, windowHeight, 1)
x3d.WindowCenter(window)

local viewer = x3d.ViewerCreate(0, 0, windowWidth, windowHeight, x3d.WindowGetHandle(window))
x3d.ViewerSetBackgroundColor(viewer, 4210752.0)
x3d.ViewerEnableVSync(viewer, 0)

local matlib = x3d.MaterialLibraryCreate()
x3d.MaterialLibraryActivate(matlib)

local back = x3d.DummycubeCreate(0)
local scene = x3d.DummycubeCreate(0)
local front = x3d.DummycubeCreate(0)

local light = x3d.LightCreate(1, scene)
x3d.ObjectSetPosition(light, 2, 4, 2)

local bump = x3d.BumpShaderCreate()
x3d.BumpShaderSetDiffuseTexture(bump, utils.cstr(""))
x3d.BumpShaderSetNormalTexture(bump, utils.cstr(""))
x3d.BumpShaderSetMaxLights(bump, 3)
x3d.BumpShaderUseParallax(bump, 0)
x3d.BumpShaderSetShadowBlurRadius(bump, 2)
x3d.BumpShaderUseAutoTangentSpace(bump, 1)

local cube = x3d.CubeCreate(1, 1, 1, scene)
x3d.MaterialCreate(utils.cstr("mStone"), utils.cstr(""))
x3d.MaterialSetShininess(utils.cstr("mStone"), 32)
x3d.MaterialSetAmbientColor(utils.cstr("mStone"), 4210752.0, 1)
x3d.MaterialSetDiffuseColor(utils.cstr("mStone"), 12632256.0, 1)
x3d.MaterialSetSpecularColor(utils.cstr("mStone"), 16777215.0, 1)
x3d.MaterialLoadTextureEx(utils.cstr("mStone"), utils.cstr("data/stone.png"), 0)
x3d.MaterialLoadTextureEx(utils.cstr("mStone"), utils.cstr("data/stone-normal.png"), 1)
x3d.MaterialSetShader(utils.cstr("mStone"), bump)
x3d.ObjectSetMaterial(cube, utils.cstr("mStone"))

local camPos = x3d.DummycubeCreate(scene)
x3d.ObjectSetPosition(camPos, 0, 1, 5)

local camera = x3d.CameraCreate(camPos)
x3d.CameraSetViewDepth(camera, 500)
x3d.CameraSetFocal(camera, 80)
x3d.ViewerSetCamera(viewer, camera)

local fixedTimeStep = 1.0 / 60.0
local running = 1
local timer = 0

local mx = x3d.WindowGetPosition(window, 0) + windowWidth / 2
local my = x3d.WindowGetPosition(window, 1) + windowHeight / 2

x3d.MouseSetPosition(mx, my)

function update(dt)
    x3d.WindowDispatch()
    
    if (x3d.KeyIsPressed(27) == 1) then
        running = 0
    end
    
    local mouseX = x3d.MouseGetPositionX();
    local mouseY = x3d.MouseGetPositionY();
    local deltax = (mx - mouseX) / 3.0
    local deltay = (my - mouseY) / 3.0
    x3d.MouseSetPosition(mx, my);
    x3d.ObjectRotate(camera, deltay, 0, 0);
    x3d.ObjectRotate(camPos, 0, -deltax, 0);
    
    if (x3d.KeyIsPressed(87) == 1) then x3d.ObjectMove(camPos, -10 * dt) end
    if (x3d.KeyIsPressed(65) == 1) then x3d.ObjectStrafe(camPos, 10 * dt) end
    if (x3d.KeyIsPressed(68) == 1) then x3d.ObjectStrafe(camPos, -10 * dt) end
    if (x3d.KeyIsPressed(83) == 1) then x3d.ObjectMove(camPos, 10 * dt) end
    
    local newWidth = x3d.WindowGetSize(window, 0)
    local newHeight = x3d.WindowGetSize(window, 1)
    if (newWidth ~= windowWidth or 
        newHeight ~= windowHeight) then
        windowWidth = newWidth
        windowHeight = newHeight
        mx = x3d.WindowGetPosition(window, 0) + windowWidth / 2
        my = x3d.WindowGetPosition(window, 1) + windowHeight / 2
        x3d.ViewerResize(viewer, 0, 0, windowWidth, windowHeight);
    end
    
    x3d.ObjectTurn(cube, dt * 45)
    
    x3d.Update(fixedTimeStep)
    x3d.ViewerRender(viewer)
end

while (running == 1) do
    running = x3d.WindowIsShowing(window)
    
    local dt = x3d.EngineGetTimeStep();
    timer = timer + dt;
    if (timer >= fixedTimeStep) then
        timer = timer - fixedTimeStep
        update(dt)
    end
end

x3d.WindowDestroy(window)
