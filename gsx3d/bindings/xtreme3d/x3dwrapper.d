module x3dwrapper;

version(Windows)
{
    import core.sys.windows.windows;
}

import gscript;
import x3dfuncs;

GsDynamic x3d_ShowConsole(GsDynamic[] args)
{
    version(Windows)
    {
        if (args[1].asNumber)
            ShowWindow(GetConsoleWindow(), SW_SHOW);
        else
            ShowWindow(GetConsoleWindow(), SW_HIDE);
    }
    return GsDynamic(1);
}

GsDynamic x3d_EngineCreate(GsDynamic[] args)
{
    return GsDynamic(EngineCreate());
}

GsDynamic x3d_EngineUpdate(GsDynamic[] args)
{
    return GsDynamic(EngineUpdate(args[1].asNumber));
}

GsDynamic x3d_EngineGetTimeStep(GsDynamic[] args)
{
    return GsDynamic(EngineGetTimeStep());
}

GsDynamic x3d_ViewerCreate(GsDynamic[] args)
{
    return GsDynamic(ViewerCreate(
        args[1].asNumber,
        args[2].asNumber,
        args[3].asNumber,
        args[4].asNumber,
        args[5].asNumber
    ));
}

GsDynamic x3d_ViewerSetBackgroundColor(GsDynamic[] args)
{
    return GsDynamic(ViewerSetBackgroundColor(
        args[1].asNumber,
        args[2].asNumber
    ));
}

GsDynamic x3d_ViewerSetAutoRender(GsDynamic[] args)
{
    return GsDynamic(ViewerSetAutoRender(
        args[1].asNumber,
        args[2].asNumber
    ));
}

GsDynamic x3d_ViewerRender(GsDynamic[] args)
{
    return GsDynamic(ViewerRender(args[1].asNumber));
}

GsDynamic x3d_WindowCreate(GsDynamic[] args)
{
    return GsDynamic(WindowCreate(
        args[1].asNumber,
        args[2].asNumber,
        args[3].asNumber,
        args[4].asNumber,
        args[5].asNumber
    ));
}

GsDynamic x3d_WindowSetBackgroundColor(GsDynamic[] args)
{
    return GsDynamic(WindowSetBackgroundColor(
        args[1].asNumber,
        args[2].asNumber
    ));
}

GsDynamic x3d_WindowCenter(GsDynamic[] args)
{
    return GsDynamic(WindowCenter(args[1].asNumber));
}

GsDynamic x3d_WindowResize(GsDynamic[] args)
{
    return GsDynamic(WindowResize(
        args[1].asNumber,
        args[2].asNumber,
        args[3].asNumber,
        args[4].asNumber,
        args[5].asNumber
    ));
}

GsDynamic x3d_WindowDispatch(GsDynamic[] args)
{
    return GsDynamic(WindowDispatch(args[1].asNumber));
}

GsDynamic x3d_WindowIsActive(GsDynamic[] args)
{
    return GsDynamic(WindowIsActive(args[1].asNumber));
}

GsDynamic x3d_WindowGetHandle(GsDynamic[] args)
{
    return GsDynamic(WindowGetHandle(args[1].asNumber));
}
