function ViewerCreate(top, left, width, height, winHandle: real): real; cdecl;
var
    windowHandle: HWND;
    viewer: TGLSceneViewer;
begin
    windowHandle := HWND(RealToPtr(winHandle));
    if IsWindow(windowHandle) then begin
        viewer := TGLSceneViewer.Create(nil);
        viewer.Top := Trunc(top);
        viewer.Left := Trunc(left);
        viewer.Width := Trunc(width);
        viewer.Height := Trunc(height);
        viewer.Enabled := false;
        viewer.Visible := true;
        viewer.Buffer.Lighting := true;
        viewer.ParentWindow := windowHandle;
        viewer.Buffer.ContextOptions := [roDoubleBuffer, roStencilBuffer, roRenderToWindow, roTwoSideLighting];
        result := ObjToReal(viewer);
    end
    else begin
        ShowMessage('Invalid parent window handle');
        result := 0.0;
    end;
end;

function ViewerSetCamera(viewer, camera: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Camera := TGLCamera(RealToPtr(camera));
    result := 1.0;
end;

function ViewerEnableVSync(viewer, vsm: real): real; cdecl;
begin
    if vsm = 0 then TGLSceneViewer(RealToPtr(viewer)).VSync := vsmSync;
    if vsm = 1 then TGLSceneViewer(RealToPtr(viewer)).VSync := vsmNoSync;
    result := 1.0;
end;

function ViewerRender(viewer: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.Render();
    result := 1.0;
end;

function ViewerBeginRender(viewer: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.BeginRender;
    result:=1;
end;

function ViewerClear(viewer, clearColor, clearDepth, clearStencil: real): real; cdecl;
var
    mask: GLbitfield;
begin
    mask := 0;
    if clearColor > 0   then mask := GL_COLOR_BUFFER_BIT;
    if clearDepth > 0   then mask := mask or GL_DEPTH_BUFFER_BIT;
    if clearStencil > 0 then mask := mask or GL_STENCIL_BUFFER_BIT;
    TGLSceneViewer(RealToPtr(viewer)).Buffer.Clear(mask);
    result:=1;
end;

function ViewerRenderObject(viewer, obj: real): real; cdecl;
begin
    if obj > 0 then
        TGLSceneViewer(RealToPtr(viewer)).Buffer.RenderBaseObject(TGLBaseSceneObject(RealToPtr(obj)));
    result:=1;
end;

function ViewerEndRender(viewer: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.EndRender;
    result:=1;
end;

function ViewerRenderToFile(v: real; fname: PAnsiChar): real; cdecl;
var
    viewer: TGLSceneViewer;
    filename, ext: string;
    bmp: TBitmap;
    png: TPNGObject;
begin
    viewer := TGLSceneViewer(RealToPtr(v));
    filename := StrConv(fname);
    bmp := viewer.Buffer.CreateSnapShotBitmap;
    ext := ExtractFileExt(filename);
    if CompareText(ext, '.png') = 0 then begin
      png := TPNGObject.Create;
      png.Assign(bmp);
      png.CreateAlpha;
      png.SaveToFile(filename);
      png.Free;
    end
    else begin
      bmp.SaveToFile(filename)
    end;
    bmp.Free;
    result := 1.0;
end;

function ViewerResize(v, left, top, width, height: real): real; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(RealToPtr(v));
    viewer.Top := Trunc(top);
    viewer.Left := Trunc(left);
    viewer.Width := Trunc(width);
    viewer.Height := Trunc(height);
    result := 1.0;
end;

function ViewerSetVisible(viewer, mode: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Visible := boolean(Trunc(mode));
    result := 1.0;
end;

function ViewerGetPixelColor(viewer, x, y: real): real; cdecl;
var
    color: TColor;
begin
    color := TGLSceneViewer(RealToPtr(viewer)).Buffer.GetPixelColor(Trunc(x), Trunc(y));
    result := Integer(color);
end;

function ViewerGetPixelDepth(viewer, x, y: real): real; cdecl;
begin
    result := TGLSceneViewer(RealToPtr(viewer)).Buffer.GetPixelDepth(Trunc(x), Trunc(y));
end;

function ViewerSetLighting(viewer, mode: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.Lighting := boolean(Trunc(mode));
    result := 1.0;
end;

function ViewerSetBackgroundColor(viewer, color: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.BackgroundColor := TColor(Trunc(color));
    result := 1.0;
end;

function ViewerSetAmbientColor(viewer, color: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.AmbientColor.AsWinColor := TColor(Trunc(color));
    result := 1.0;
end;

function ViewerEnableFog(viewer, mode: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.FogEnable := boolean(Trunc(mode));
    result := 1.0;
end;

function ViewerSetFogColor(viewer, color: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).Buffer.FogEnvironment.FogColor.AsWinColor := TColor(Trunc(color));
    result := 1.0;
end;

function ViewerSetFogDistance(v, fstart, fend: real): real; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(RealToPtr(v));
    viewer.Buffer.FogEnvironment.FogStart := fstart;
    viewer.Buffer.FogEnvironment.FogEnd := fend;
    result := 1.0;
end;

function ViewerScreenToWorld(viewer, x, y, ind: real): real; cdecl;
var
    stw: TAffineVector;
begin
    stw := TGLSceneViewer(RealToPtr(viewer)).Buffer.ScreenToWorld(Trunc(x), Trunc(y));
    result := stw.V[Trunc(ind)];
end;

function ViewerWorldToScreen(viewer, x, y, z, ind: real): real; cdecl;
var
    wts: TAffineVector;
begin
    SetVector(wts, 0, 0, 0);
    wts := TGLSceneViewer(RealToPtr(viewer)).Buffer.WorldToScreen(AffineVectorMake(x, y, z));
    result := wts.V[Trunc(ind)];
end;

function ViewerCopyToTexture(viewer: real; mtrl: PAnsiChar): real; cdecl;
var
    mat: TGLLibMaterial;
    buf: TGLSceneBuffer;
begin
    mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
    buf := TGLSceneViewer(RealToPtr(viewer)).Buffer;
    buf.CopyToTexture(mat.Material.Texture);
    result := 1.0;
end;

function ViewerGetPickedObject(viewer, x, y: real): real; cdecl;
var
    obj: TGLBaseSceneObject;
begin
    obj := TGLSceneViewer(RealToPtr(viewer)).Buffer.GetPickedObject(Trunc(x), Trunc(y));
    result := ObjToReal(obj);
end;

function ViewerGetPickedObjectsList(viewer, list, x, y, w, h, num: real): real; cdecl;
var
    rect: TRect;
begin
    rect := TRect.Create(Trunc(x), Trunc(y), Trunc(w), Trunc(h));
    TGLSceneViewer(RealToPtr(viewer)).Buffer.PickObjects(rect, TGLPickList(RealToPtr(list)), Trunc(num));
    result := list;
end;

function ViewerScreenToVector(viewer, x, y, ind: real): real; cdecl;
var
    vec: TGLVector;
begin
    vec := TGLSceneViewer(RealToPtr(viewer)).Buffer.ScreenToVector(Trunc(x), Trunc(y));
    result := vec.V[Trunc(ind)];
end;

function ViewerVectorToScreen(viewer, x, y, z, ind: real): real; cdecl;
var
    dvec, rvec: TAffineVector;
begin
    SetVector(dvec, x, y, z);
    rvec := TGLSceneViewer(RealToPtr(viewer)).Buffer.VectorToScreen(dvec);
    result := rvec.V[Trunc(ind)];
end;

function ViewerPixelToDistance(viewer, x, y: real): real; cdecl;
begin
    result := TGLSceneViewer(RealToPtr(viewer)).Buffer.PixelToDistance(Trunc(x), Trunc(y));
end;

function ViewerSetAntiAliasing(v, aa: real): real; cdecl;
var
    viewer: TGLSceneViewer;
    aaMode: integer;
begin
    viewer := TGLSceneViewer(RealToPtr(v));
    aaMode := Trunc(aa);
    if aaMode = 0 then viewer.Buffer.AntiAliasing := aaDefault;
    if aaMode = 1 then viewer.Buffer.AntiAliasing := aaNone;
    if aaMode = 2 then viewer.Buffer.AntiAliasing := aa2x;
    if aaMode = 3 then viewer.Buffer.AntiAliasing := aa2xHQ;
    if aaMode = 4 then viewer.Buffer.AntiAliasing := aa4x;
    if aaMode = 5 then viewer.Buffer.AntiAliasing := aa4xHQ;
    if aaMode = 6 then viewer.Buffer.AntiAliasing := aa6x;
    if aaMode = 7 then viewer.Buffer.AntiAliasing := aa8x;
    if aaMode = 8 then viewer.Buffer.AntiAliasing := aa16x;
    if aaMode = 9 then viewer.Buffer.AntiAliasing := csa8x;
    if aaMode = 10 then viewer.Buffer.AntiAliasing := csa8xHQ;
    if aaMode = 11 then viewer.Buffer.AntiAliasing := csa16x;
    if aaMode = 12 then viewer.Buffer.AntiAliasing := csa16xHQ;
    result := 1.0;
end;

function ViewerGetGLSLSupported(viewer:real): real; cdecl;
var
   v: TGLSceneViewer;
begin
   v := TGLSceneViewer(RealToPtr(viewer));
   if (IsExtensionSupported(v, 'GL_ARB_shader_objects') and
       IsExtensionSupported(v, 'GL_ARB_vertex_shader') and
       IsExtensionSupported(v, 'GL_ARB_fragment_shader')) then
       Result := 1
   else
       Result := 0;
end;

function ViewerGetFBOSupported(viewer:real): real; cdecl;
var
   v: TGLSceneViewer;
begin
   v := TGLSceneViewer(RealToPtr(viewer));
   if IsExtensionSupported(v, 'GL_ARB_framebuffer_object') then
       Result := 1
   else
       Result := 0;
end;

function ViewerGetVBOSupported(viewer:real): real; cdecl;
var
   v: TGLSceneViewer;
begin
   v := TGLSceneViewer(RealToPtr(viewer));
   if IsExtensionSupported(v, 'GL_ARB_vertex_buffer_object') then
       Result := 1
   else
       Result := 0;
end;

function ViewerGetSize(viewer, index: real): real; cdecl;
var
    v: TGLSceneViewer;
begin
    v := TGLSceneViewer(RealToPtr(viewer));
    if index = 0 then
        result := v.Width
    else
        result := v.Height;
end;

function ViewerGetPosition(viewer, index: real): real; cdecl;
var
    v: TGLSceneViewer;
begin
    v := TGLSceneViewer(RealToPtr(viewer));
    if index = 0 then
        result := v.Left
    else
        result := v.Top;
end;

function ViewerIsOpenGLExtensionSupported(viewer: real; ext: PAnsiChar): real; cdecl;
var
    v: TGLSceneViewer;
begin
    v := TGLSceneViewer(RealToPtr(viewer));
    result := integer(IsExtensionSupported(v, String(AnsiString(ext))));
end;

function ViewerGetFramesPerSecond(viewer: real): real; cdecl;
begin
    result := TGLSceneViewer(RealToPtr(viewer)).FramesPerSecond;
end;

function ViewerResetPerformanceMonitor(viewer: real): real; cdecl;
begin
    TGLSceneViewer(RealToPtr(viewer)).ResetPerformanceMonitor;
    result := 1.0;
end;

function ViewerPixelRayToWorld(viewer, x, y, ind: real): real; cdecl;
var
    vec: TAffineVector;
begin
    vec := TGLSceneViewer(RealToPtr(viewer)).Buffer.PixelRayToWorld(Trunc(x), Trunc(y));
    result := vec.V[Trunc(ind)];
end;

function ViewerShadeModel(v, ind: real): real; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(RealToPtr(v));
    if Trunc(ind) = 0 then viewer.Buffer.ShadeModel := smFlat;
    if Trunc(ind) = 1 then viewer.Buffer.ShadeModel := smSmooth;
    result := 1.0;
end;

function ViewerSetAutoRender(v, mode: real): real; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(RealToPtr(v));
    viewer.AutoRender := Boolean(Trunc(mode));
    result := 1.0;
end;
