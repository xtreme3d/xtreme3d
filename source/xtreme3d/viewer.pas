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

function ViewerSetCamera(viewer, camera: Pointer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Camera := TGLCamera(camera);
    result := 1;
end;

function ViewerEnableVSync(viewer: Pointer; vsm: integer): integer; cdecl;
begin
    if vsm = 0 then TGLSceneViewer(viewer).VSync := vsmSync;
    if vsm = 1 then TGLSceneViewer(viewer).VSync := vsmNoSync;
    result := 1;
end;

function ViewerRender(viewer: Pointer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.Render();
    result := 1;
end;

function ViewerRenderToFile(viewer: Pointer; fname: PAnsiChar): integer; cdecl;
var
    bmp: TBitmap;
begin
    bmp := TGLSceneViewer(viewer).Buffer.CreateSnapShotBitmap;
    bmp.SaveToFile(String(AnsiString(fname)));
    bmp.Free;
    result := 1;
end;

{
// TODO
function ViewerRenderToFilePNG(v: Pointer; fname: PAnsiChar): integer; cdecl;
var
    viewer: TGLSceneViewer;
    bmp: TBitmap;
    bufw, bufb: TBitmap;
    png: TPNGObject;
    i, j: integer;
    pw, pb, pr, pa: pByteArray;
    f: single;
    oldColor: TColor;
begin
    viewer := TGLSceneViewer(v);
    oldColor := viewer.Buffer.BackgroundColor;
    viewer.Buffer.BackgroundColor := $ffffff;
    viewer.Buffer.Render;
    bufw := viewer.Buffer.CreateSnapShotBitmap;

    viewer.Buffer.BackgroundColor := 0;
    viewer.Buffer.Render;
    bufb := viewer.Buffer.CreateSnapShotBitmap;

    viewer.Buffer.BackgroundColor := oldColor;
    viewer.Buffer.Render;
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf32bit;
    bmp.Transparent := true;
    bmp.Width := viewer.Width;
    bmp.Height := viewer.Height;

    for j := 0 to bufw.Height - 1 do begin
        pw := bufw.ScanLine[j];
        pb := bufb.ScanLine[j];
        pr := bmp.ScanLine[j];
        for i := 0 to bufw.Width - 1 do begin
            // alpha
            pr[i * 4 + 3] := pb[i * 4 + 1] - pw[i * 4 + 1] + 255;
            // color
            f := 255 / pr[i * 4 + 3];
            pr[i * 4] := round(ClampValue( pb[i * 4] * f, 0, 255));
            pr[i * 4 + 1] := round(ClampValue( pb[i * 4 + 1] * f, 0, 255));
            pr[i * 4 + 2] := round(ClampValue( pb[i * 4 + 2] * f, 0, 255));
        end;
    end;
    png := TPNGObject.Create;
    png.Assign(bmp);
    png.CreateAlpha;
    for j := 0 to png.Height - 1 do begin
        pr := bmp.ScanLine[j];
        pa := png.AlphaScanline[j];
        for i := 0 to png.Width - 1 do begin
           pa[i] := pr[i * 4 + 3];
        end;
    end;
    png.SaveToFile(String(AnsiString(fname)));
    png.Free;
    bmp.Free;
    result := 1;
end;
}

{
// TODO
function ViewerRenderEx(viewer, obj, clear, swap, updateFPS: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.SimpleRender2(TGLBaseSceneObject(trunc64(obj)),
    True,
    Boolean(trunc64(updateFPS)),
    Boolean(trunc64(clear)),
    Boolean(trunc64(clear)),
    Boolean(trunc64(clear)),
    Boolean(trunc64(swap)));
  result:=1;
end;
}

function ViewerResize(v: Pointer; left, top, width, height: integer): integer; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(v);
    viewer.Top := top;
    viewer.Left := left;
    viewer.Width := width;
    viewer.Height := height;
    result := 1;
end;

function ViewerSetVisible(viewer: Pointer; mode: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Visible := boolean(mode);
    result := 1;
end;

function ViewerGetPixelColor(viewer: Pointer; x, y: integer): integer; cdecl;
var
    color: TColor;
begin
    color := TGLSceneViewer(viewer).Buffer.GetPixelColor(x, y);
    result := Integer(color);
end;

function ViewerGetPixelDepth(viewer: Pointer; x, y: integer): real; cdecl;
begin
    result := TGLSceneViewer(viewer).Buffer.GetPixelDepth(x, y);
end;

function ViewerSetLighting(viewer: Pointer; mode: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.Lighting := boolean(mode);
    result := 1;
end;

function ViewerSetBackgroundColor(viewer: Pointer; color: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.BackgroundColor := TColor(color);
    result := 1;
end;

function ViewerSetAmbientColor(viewer: Pointer; color: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.AmbientColor.AsWinColor := TColor(color);
    result := 1;
end;

function ViewerEnableFog(viewer: Pointer; mode: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.FogEnable := boolean(mode);
    result := 1;
end;

function ViewerSetFogColor(viewer: Pointer; color: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.FogEnvironment.FogColor.AsWinColor := TColor(color);
    result := 1;
end;

function ViewerSetFogDistance(viewer: Pointer; fstart, fend: real): integer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.FogEnvironment.FogStart := fstart;
    TGLSceneViewer(viewer).Buffer.FogEnvironment.FogEnd := fend;
    result := 1;
end;

function ViewerScreenToWorld(viewer: Pointer; x, y, ind: integer): real; cdecl;
var
    stw: TAffineVector;
begin
    stw := TGLSceneViewer(viewer).Buffer.ScreenToWorld(x, y);
    result := stw.V[ind];
end;

function ViewerWorldToScreen(viewer: Pointer; x, y, z: real; ind: integer): real; cdecl;
var
    wts: TAffineVector;
begin
    SetVector(wts, 0, 0, 0);
    wts := TGLSceneViewer(viewer).Buffer.WorldToScreen(AffineVectorMake(x, y, z));
    result := wts.V[ind];
end;

function ViewerCopyToTexture(viewer: Pointer; mtrl: PAnsiChar): integer; cdecl;
var
    mat: TGLLibMaterial;
    buf: TGLSceneBuffer;
begin
    mat := matlib.Materials.GetLibMaterialByName(String(AnsiString(mtrl)));
    buf := TGLSceneViewer(viewer).Buffer;
    buf.CopyToTexture(mat.Material.Texture);
    result := 1;
end;

function ViewerGetPickedObject(viewer: Pointer; x, y: integer): Pointer; cdecl;
var
    obj: TGLBaseSceneObject;
begin
    obj := TGLSceneViewer(viewer).Buffer.GetPickedObject(x, y);
    result := obj;
end;

function ViewerGetPickedObjectsList(viewer, list: Pointer; x, y, w, h, num: integer): Pointer; cdecl;
begin
    TGLSceneViewer(viewer).Buffer.PickObjects(TRect.Create(x, y, w, h), TGLPickList(list), num);
    result := list;
end;

function ViewerScreenToVector(viewer: Pointer; x, y, ind: integer): real; cdecl;
var
    vec: TGLVector;
begin
    vec := TGLSceneViewer(viewer).Buffer.ScreenToVector(x, y);
    result := vec.V[ind];
end;

function ViewerVectorToScreen(viewer: Pointer; x, y, z: real; ind: integer): real; cdecl;
var
    dvec, rvec: TAffineVector;
begin
    SetVector(dvec, x, y, z);
    rvec := TGLSceneViewer(viewer).Buffer.VectorToScreen(dvec);
    result := rvec.V[ind];
end;

function ViewerPixelToDistance(viewer: Pointer; x, y: integer): real; cdecl;
begin
    result := TGLSceneViewer(viewer).Buffer.PixelToDistance(x, y);
end;

function ViewerSetAntiAliasing(v: Pointer; aa: integer): integer; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(v);
    if aa = 0 then viewer.Buffer.AntiAliasing := aaDefault;
    if aa = 1 then viewer.Buffer.AntiAliasing := aaNone;
    if aa = 2 then viewer.Buffer.AntiAliasing := aa2x;
    if aa = 3 then viewer.Buffer.AntiAliasing := aa2xHQ;
    if aa = 4 then viewer.Buffer.AntiAliasing := aa4x;
    if aa = 5 then viewer.Buffer.AntiAliasing := aa4xHQ;
    if aa = 6 then viewer.Buffer.AntiAliasing := aa6x;
    if aa = 7 then viewer.Buffer.AntiAliasing := aa8x;
    if aa = 8 then viewer.Buffer.AntiAliasing := aa16x;
    if aa = 9 then viewer.Buffer.AntiAliasing := csa8x;
    if aa = 10 then viewer.Buffer.AntiAliasing := csa8xHQ;
    if aa = 11 then viewer.Buffer.AntiAliasing := csa16x;
    if aa = 12 then viewer.Buffer.AntiAliasing := csa16xHQ;
    result := 1;
end;

{
// TODO:
function ViewerGetGLSLSupported(viewer:real): real; cdecl;
begin
   if (GL_ARB_shader_objects and
       GL_ARB_vertex_shader and
       GL_ARB_fragment_shader) then
       Result := 1
   else
       Result := 0;
end;

function ViewerGetFBOSupported(viewer: real): real; cdecl;
begin
   if GL_ARB_framebuffer_object then
       Result := 1
   else
       Result := 0;
end;

function ViewerGetVBOSupported(viewer:real): real; cdecl;
begin
   if GL_ARB_vertex_buffer_object then
       Result := 1
   else
       Result := 0;
end;
}

{
// TODO
function ViewerSetAutoRender(viewer: Pointer; mode: integer): integer; cdecl;
begin
    TGLSceneViewer(viewer).AutoRender := Boolean(mode);
    result := 1;
end;
}

{
// TODO
function ViewerSetOverrideMaterial(viewer, mlb: Pointer; mtrl: PAnsiChar): integer; cdecl;
var
    v: TGLSceneViewer;
    mlib: TGLMaterialLibrary;
    mat: TGLLibMaterial;
begin
    v := TGLSceneViewer(viewer);
    v.Buffer.OverrideMaterial := nil;
    if Length(mtrl) > 0 then
    begin
        mlib := TGLMaterialLibrary(mlb);
        mat := mlib.Materials.GetLibMaterialByName(String(mtrl));
        v.Buffer.OverrideMaterial := mat;
    end;
    result := 1;
end;
}

function ViewerGetSize(viewer: Pointer; index: integer): integer; cdecl;
var
    v: TGLSceneViewer;
begin
    v := TGLSceneViewer(viewer);
    if index = 0 then
        result := v.Width
    else
        result := v.Height;
end;

function ViewerGetPosition(viewer: Pointer; index: integer): integer; cdecl;
var
    v: TGLSceneViewer;
begin
    v := TGLSceneViewer(viewer);
    if index = 0 then
        result := v.Left
    else
        result := v.Top;
end;

function ViewerIsOpenGLExtensionSupported(viewer: Pointer; ext: PAnsiChar): integer; cdecl;
var
    v: TGLSceneViewer;
begin
    v := TGLSceneViewer(viewer);
    result := integer(IsExtensionSupported(v, String(AnsiString(ext))));
end;

function ViewerGetFramesPerSecond(viewer: Pointer): real; cdecl;
begin
    result := TGLSceneViewer(viewer).FramesPerSecond;
end;

function ViewerResetPerformanceMonitor(viewer: Pointer): integer; cdecl;
begin
    TGLSceneViewer(viewer).ResetPerformanceMonitor;
    result := 1;
end;

function ViewerPixelRayToWorld(viewer: Pointer; x, y, ind: integer): real; cdecl;
var
    vec: TAffineVector;
begin
    vec := TGLSceneViewer(viewer).Buffer.PixelRayToWorld(x, y);
    result := vec.V[ind];
end;

function ViewerShadeModel(v: Pointer; ind: integer): integer; cdecl;
var
    viewer: TGLSceneViewer;
begin
    viewer := TGLSceneViewer(v);
    if ind = 0 then viewer.Buffer.ShadeModel := smFlat;
    if ind = 1 then viewer.Buffer.ShadeModel := smSmooth;
    result := 1;
end;
