macro c_black = 0.0;
macro c_dkgray = 4210752.0;
macro c_gray = 8421504.0;
macro c_ltgray = 12632256.0;
macro c_white = 16777215.0;
macro c_aqua = 16776960.0;
macro c_blue = 16711680.0;
macro c_fuchsia = 16711935.0;
macro c_green = 32768.0;
macro c_lime = 65280.0;
macro c_maroon = 128.0;
macro c_navy = 8388608.0;
macro c_olive = 32896.0;
macro c_purple = 8388736.0;
macro c_red = 255.0;
macro c_silver = 12632256.0;
macro c_teal = 8421376.0;
macro c_yellow = 65535.0;
macro c_orange = 33023.0;

global.windowWidth = 1280;
global.windowHeight = 720;

//global.ShowConsole(false);
global.EngineCreate();
//global.EngineShowLoadingErrors(1);
//global.EngineSetCulling(vcNone);
//global.EngineSetObjectsSorting(osNone);
//global.EngineSetMaxLights(8);

const window = global.WindowCreate(0, 0, global.windowWidth, global.windowHeight, false);
global.WindowCenter(window);
global.WindowSetBackgroundColor(window, c_black);
global.WindowSetTitle(window, "Xtreme3D 4.0");

const viewer = global.ViewerCreate(0, 0, global.windowWidth, global.windowHeight, global.WindowGetHandle(window));
global.ViewerSetBackgroundColor(viewer, c_gray);
//global.ViewerSetLighting(self.viewer, true);
//global.ViewerEnableFog(self.viewer, true);
//global.ViewerSetFogColor(self.viewer, c_gray);
//global.ViewerSetFogDistance(self.viewer, 0, 50);
//global.ViewerSetAntiAliasing(self.viewer, csa8xHQ);
//global.ViewerEnableVSync(self.viewer, vsmSync);
global.ViewerSetAutoRender(viewer, false);

let timer = 0;
const dt = 1.0 / 60.0;
let running = true;
while(running)
{
    global.WindowDispatch(window);
    running = global.WindowIsActive(window);
    if (!running) break;
    
    let timeStep = global.EngineGetTimeStep();
    timer += timeStep;
    if (timer >= dt)
    {
        timer -= dt;
        update(dt);
    }
}

func update(dt)
{
    global.EngineUpdate(dt);
    global.ViewerRender(viewer);
}
