module x3dsignatures;

extern(C) @nogc nothrow:

alias f_EngineCreate = double function();
alias f_EngineUpdate = double function(double dt);
alias f_EngineGetTimeStep = double function();

alias f_ViewerCreate = double function(double x, double y, double width, double height, double windowHandle);
alias f_ViewerSetBackgroundColor = double function(double viewer, double color);
alias f_ViewerSetAutoRender = double function(double viewer, double mode);
alias f_ViewerRender = double function(double viewer);

alias f_WindowCreate = double function(double x, double y, double width, double height, double resizeable);
alias f_WindowSetBackgroundColor = double function(double window, double color);
alias f_WindowCenter = double function(double window);
alias f_WindowResize = double function(double window, double x, double y, double width, double height);
alias f_WindowDispatch = double function(double window);
alias f_WindowIsActive = double function(double window);
alias f_WindowGetHandle = double function(double window);
