module x3dfuncs;

import x3dsignatures;

__gshared
{
    // Engine
    f_EngineCreate EngineCreate;
    f_EngineUpdate EngineUpdate;
    f_EngineGetTimeStep EngineGetTimeStep;
    
    // Viewer
    f_ViewerCreate ViewerCreate;
    f_ViewerSetBackgroundColor ViewerSetBackgroundColor;
    f_ViewerSetAutoRender ViewerSetAutoRender;
    f_ViewerRender ViewerRender;
    
    // Window
    f_WindowCreate WindowCreate;
    f_WindowSetBackgroundColor WindowSetBackgroundColor;
    f_WindowCenter WindowCenter;
    f_WindowResize WindowResize;
    f_WindowDispatch WindowDispatch;
    f_WindowIsActive WindowIsActive;
    f_WindowGetHandle WindowGetHandle;
}
