/**
 * Xtreme3D binding for D + GScript3
 */
module xtreme3d;

import linker;
import x3dfuncs;
import x3dwrapper;
import gscript;

version(Windows)
{
    pragma(lib, "user32");
    
    import core.sys.windows.windows;
    
    enum CP_UTF8 = 65001;
    
    static this()
    {
        // Set console code page to UTF-8
        SetConsoleCP(CP_UTF8);
        SetConsoleOutputCP(CP_UTF8);
    }
}

__gshared
{
    private SharedLib libx3d;
}

void bindSymbol(void** symbolPtr, const(char)* name) @nogc nothrow
{
    pragma(inline, true);
    *symbolPtr = getFunctionPointer(libx3d, name);
}

void x3dInit() @nogc nothrow
{
    version(Windows)
    {
        libx3d = openLibrary("xtreme3d.dll");
        
        bindSymbol(cast(void**)&EngineCreate, "EngineCreate");
        bindSymbol(cast(void**)&EngineUpdate, "EngineUpdate");
        bindSymbol(cast(void**)&EngineGetTimeStep, "EngineGetTimeStep");
        
        bindSymbol(cast(void**)&ViewerCreate, "ViewerCreate");
        bindSymbol(cast(void**)&ViewerSetBackgroundColor, "ViewerSetBackgroundColor");
        bindSymbol(cast(void**)&ViewerSetAutoRender, "ViewerSetAutoRender");
        bindSymbol(cast(void**)&ViewerRender, "ViewerRender");
        
        bindSymbol(cast(void**)&WindowCreate, "WindowCreate");
        bindSymbol(cast(void**)&WindowSetBackgroundColor, "WindowSetBackgroundColor");
        bindSymbol(cast(void**)&WindowCenter, "WindowCenter");
        bindSymbol(cast(void**)&WindowResize, "WindowResize");
        bindSymbol(cast(void**)&WindowDispatch, "WindowDispatch");
        bindSymbol(cast(void**)&WindowIsActive, "WindowIsActive");
        bindSymbol(cast(void**)&WindowGetHandle, "WindowGetHandle");
    }
}

void x3dBindFunctions(GsVirtualMachine vm)
{
    vm.set("ShowConsole", GsDynamic(&x3d_ShowConsole));
    
    vm.set("EngineCreate", GsDynamic(&x3d_EngineCreate));
    vm.set("EngineUpdate", GsDynamic(&x3d_EngineUpdate));
    vm.set("EngineGetTimeStep", GsDynamic(&x3d_EngineGetTimeStep));
    
    vm.set("ViewerCreate", GsDynamic(&x3d_ViewerCreate));
    vm.set("ViewerSetBackgroundColor", GsDynamic(&x3d_ViewerSetBackgroundColor));
    vm.set("ViewerSetAutoRender", GsDynamic(&x3d_ViewerSetAutoRender));
    vm.set("ViewerRender", GsDynamic(&x3d_ViewerRender));
    
    vm.set("WindowCreate", GsDynamic(&x3d_WindowCreate));
    vm.set("WindowSetBackgroundColor", GsDynamic(&x3d_WindowSetBackgroundColor));
    vm.set("WindowCenter", GsDynamic(&x3d_WindowCenter));
    vm.set("WindowResize", GsDynamic(&x3d_WindowResize));
    vm.set("WindowDispatch", GsDynamic(&x3d_WindowDispatch));
    vm.set("WindowIsActive", GsDynamic(&x3d_WindowIsActive));
    vm.set("WindowGetHandle", GsDynamic(&x3d_WindowGetHandle));
}
