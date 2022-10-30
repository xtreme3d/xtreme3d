function ViewerCreate(top, left, width, height: integer; winHandle: Pointer): Pointer; cdecl;
var
    windowHandle: HWND;
    viewer: TGLSceneViewer;
begin
    windowHandle := HWND(winHandle);
    if IsWindow(windowHandle) then begin
        viewer := TGLSceneViewer.Create(nil);
        viewer.Top := top;
        viewer.Left := left;
        viewer.Width := width;
        viewer.Height := height;
        viewer.Enabled := false;
        viewer.Visible := true;
        viewer.Buffer.Lighting := true;
        viewer.ParentWindow := windowHandle;
        viewer.Buffer.ContextOptions := [roDoubleBuffer, roStencilBuffer, roRenderToWindow];
        //viewer.AutoRender := False;
        result := viewer;
    end
    else begin
        ShowMessage('Invalid parent window handle');
        result := nil;
    end;
end;

function ViewerRender(viewer: Pointer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.Render();
    result := 1;
end;

function ViewerSetBackgroundColor(viewer: Pointer; color: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.BackgroundColor := TColor(color);
    result := 1;
end;
