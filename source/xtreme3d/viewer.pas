function ViewerCreate(top,left,w,h,pw: real): real; cdecl;
var
  Handle: HWnd;
  GLSceneViewer1: TGLSceneViewer;
begin
  Handle := trunc64(pw);
  if IsWindow(Handle) then
    begin
      GLSceneViewer1:=TGLSceneViewer.Create(nil);
      GLSceneViewer1.Top:=trunc64(top);
      GLSceneViewer1.Left:=trunc64(left);
      GLSceneViewer1.Width:=trunc64(w);
      GLSceneViewer1.Height:=trunc64(h);
      GLSceneViewer1.Enabled:=false;
      GLSceneViewer1.Visible:=true;
      GLSceneViewer1.Buffer.Lighting:=true;
      GLSceneViewer1.ParentWindow:=Handle;
      GLSceneViewer1.Buffer.ContextOptions := [roDoubleBuffer, roStencilBuffer, roRenderToWindow];
      GLSceneViewer1.AutoRender := False;
      result:=Integer(GLSceneViewer1);
    end
  else
    begin
      ShowMessage('Invalid parent window handle');
      result:=0;
    end;
end;

function ViewerSetCamera(viewer,camera: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Camera:=TGLCamera(trunc64(camera));
  result:=1;
end;

function ViewerEnableVSync(viewer,vsm: real): real; cdecl;
begin
  if vsm=0 then TGLSceneViewer(trunc64(viewer)).VSync:=vsmSync;
  if vsm=1 then TGLSceneViewer(trunc64(viewer)).VSync:=vsmNoSync;
  result:=1;
end;

function ViewerRender(viewer: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.Render();
  result:=1;
end;

function ViewerRenderToFile(viewer:real; fname:pchar): real; cdecl;
var
  bmp: TBitmap;
begin
  bmp := TGLSceneViewer(trunc64(viewer)).Buffer.CreateSnapShotBitmap;
  bmp.SaveToFile(String(fname));
  bmp.Free;
  result:=1;
end;

function ViewerRenderToFilePNG(viewer:real; fname:pchar): real; cdecl;
var
  bmp: TBitmap;
  bufw,bufb: TBitmap;
  png: TPNGObject;
  i,j: integer;
  pw,pb,pr,pa: PByteArray;
  f: single;
  oldColor: TColor;
begin  
	oldColor:=TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor;
    TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor := $ffffff;
    TGLSceneViewer(trunc64(viewer)).Buffer.Render;
    bufw := TGLSceneViewer(trunc64(viewer)).Buffer.CreateSnapShotBitmap;

    TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor := 0;
    TGLSceneViewer(trunc64(viewer)).Buffer.Render;
    bufb := TGLSceneViewer(trunc64(viewer)).Buffer.CreateSnapShotBitmap;

	TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor := oldColor;
    TGLSceneViewer(trunc64(viewer)).Buffer.Render;
	bmp := TBitmap.Create;
    bmp.PixelFormat := pf32bit;
    bmp.Transparent := true;
    bmp.Width := TGLSceneViewer(trunc64(viewer)).Width;
    bmp.Height := TGLSceneViewer(trunc64(viewer)).Height;

    for j := 0 to bufw.Height - 1 do begin
      pw := bufw.ScanLine[j];
      pb := bufb.ScanLine[j];
      pr := bmp.ScanLine[j];
      for i := 0 to bufw.Width - 1 do begin
      // alpha
        pr[i * 4 + 3] := pb[i * 4 + 1] - pw[i * 4 + 1] + 255;
      // color
        f := 255 / pr[i * 4 + 3];
        pr[i * 4] := round( clampValue( pb[i * 4] * f, 0, 255 ));
        pr[i * 4 + 1] := round(clampValue( pb[i * 4 + 1] * f, 0, 255));
        pr[i * 4 + 2] := round(clampValue( pb[i * 4 + 2] * f, 0, 255));
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
    png.SaveToFile(String(fname));
    png.Free;
	bmp.Free;
  result:=1;
end;

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

function ViewerResize(viewer,left,top,w,h:real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Top:=trunc64(top);
  TGLSceneViewer(trunc64(viewer)).Left:=trunc64(left);
  TGLSceneViewer(trunc64(viewer)).Width:=trunc64(w);
  TGLSceneViewer(trunc64(viewer)).Height:=trunc64(h);
  result:=1;
end;

function ViewerSetVisible(viewer,mode:real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Visible:=boolean(trunc64(mode));
  result:=1;
end;

function ViewerGetPixelColor(viewer,x,y:real): real; cdecl;
var
  color: TColor;
begin
  color:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPixelColor(trunc64(x),trunc64(y));
  result:=Integer(color);
end;

function ViewerGetPixelDepth(viewer,x,y:real): real; cdecl;
var
  depth: real;
begin
  depth:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPixelDepth(trunc64(x),trunc64(y));
  result:=depth;
end;

function ViewerSetLighting(viewer,mode:real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.Lighting:=boolean(trunc64(mode));
  result:=1;
end;

function ViewerSetBackgroundColor(viewer,color: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor:=TColor(trunc64(color));
  result:=1;
end;

function ViewerSetAmbientColor(viewer,color: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.AmbientColor.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function ViewerEnableFog(viewer,mode: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnable:=boolean(trunc64(mode));
  result:=1;
end;

function ViewerSetFogColor(viewer,color: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnvironment.FogColor.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function ViewerSetFogDistance(viewer,fstart,fend: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnvironment.FogStart:=fstart;
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnvironment.FogEnd:=fend;
  result:=1;
end;

function ViewerScreenToWorld(viewer,x,y,ind:real): real; cdecl;
var
  stw: TAffineVector;
begin
  stw:=TGLSceneViewer(trunc64(viewer)).Buffer.ScreenToWorld(trunc64(x),trunc64(y));
  result:=stw[trunc64(ind)];
end;

function ViewerWorldToScreen(viewer,x,y,z,ind:real): real; cdecl;
var
  wts: TAffineVector;
begin
  SetVector(wts,0,0,0);
  wts:=TGLSceneViewer(trunc64(viewer)).Buffer.WorldToScreen(AffineVectorMake(x, y, z));
  result:=wts[trunc64(ind)];
end;

function ViewerCopyToTexture(viewer:real; mtrl:pchar): real; cdecl;
var
  mat: TGLLibMaterial;
  buf: TGLSceneBuffer;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  buf := TGLSceneViewer(trunc64(viewer)).Buffer;
  //buf.CopyToTexture(mat.Material.Texture);
  buf.CopyToTexture(mat.Material.Texture, 0, 0, buf.Width, buf.Height, 0, 0, 0, true);
  result:=1;
end;

function ViewerGetPickedObject(viewer,x,y:real):real; cdecl;
var
  obj:TGLBaseSceneObject;
begin
  obj:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPickedObject(trunc64(x),trunc64(y));
  result:=Integer(obj);
end;

function ViewerGetPickedObjectsList(viewer,x,y,w,h,num,ind:real):real; cdecl;
var
  obj:TGLBaseSceneObject;
  plist:TGLPickList;
begin
  plist:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPickedObjects(trunc64(x),trunc64(y),trunc64(w),trunc64(h),trunc64(num));
  if plist.Count>0 then
     obj:=plist.Hit[trunc64(ind)]
  else obj:=nil;
  result:=Integer(obj);
end;

function ViewerScreenToVector(viewer,x,y,ind:real):real; cdecl;
var
  vec: TVector;
begin
  vec:=TGLSceneViewer(trunc64(viewer)).Buffer.ScreenToVector(trunc64(x),trunc64(y));
  result:=vec[trunc64(ind)];
end;

function ViewerVectorToScreen(viewer,x,y,z,ind:real):real; cdecl;
var
  dvec,rvec: TAffineVector;
begin
  SetVector(dvec,x,y,z);
  rvec:=TGLSceneViewer(trunc64(viewer)).Buffer.VectorToScreen(dvec);
  result:=rvec[trunc64(ind)];
end;

function ViewerPixelToDistance(viewer,x,y:real):real; cdecl;
var
  dist: real;
begin
  dist:=TGLSceneViewer(trunc64(viewer)).Buffer.PixelToDistance(trunc64(x),trunc64(y));
  result:=dist;
end;

function ViewerSetAntiAliasing(viewer,aa: real): real; cdecl;
begin
  if trunc64(aa)=0 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aaDefault;
  if trunc64(aa)=1 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aaNone;
  if trunc64(aa)=2 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa2x;
  if trunc64(aa)=3 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa2xHQ;
  if trunc64(aa)=4 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa4x;
  if trunc64(aa)=5 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa4xHQ;
  result:=1;
end;

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

function ViewerSetAutoRender(viewer, mode: real): real; cdecl;
begin
  TGLSceneViewer(trunc64(viewer)).AutoRender := Boolean(trunc64(mode));
  result:=1;
end;

function ViewerSetOverrideMaterial(viewer, mlb: real; mtrl: pchar): real; cdecl;
var
  v: TGLSceneViewer;
  mlib: TGLMaterialLibrary;
  mat: TGLLibMaterial;
begin
  v := TGLSceneViewer(trunc64(viewer));
  v.Buffer.OverrideMaterial := nil;
  if Length(mtrl) > 0 then
  begin
    mlib := TGLMaterialLibrary(trunc64(mlb));
    mat := mlib.Materials.GetLibMaterialByName(String(mtrl)); 
    v.Buffer.OverrideMaterial := mat;
  end;
  result:=1;
end;

function ViewerGetSize(viewer, index: real): real; cdecl;
var
  v: TGLSceneViewer;
begin
  v := TGLSceneViewer(trunc64(viewer));
  if trunc64(index) = 0 then
    result := v.Width
  else
    result := v.Height;
end;

function ViewerGetPosition(viewer, index: real): real; cdecl;
var
  v: TGLSceneViewer;
begin
  v := TGLSceneViewer(trunc64(viewer));
  if trunc64(index) = 0 then
    result := v.Left
  else
    result := v.Top;
end;

function ViewerIsOpenGLExtensionSupported(viewer: real; ext: pchar): real; cdecl;
var
  v: TGLSceneViewer;
begin
  v := TGLSceneViewer(trunc64(viewer));
  result := Integer(IsExtensionSupported(v, String(ext)));
end;

function ViewerGetFramesPerSecond(viewer: real): real; cdecl;
var
  v: TGLSceneViewer;
  fps: real;
begin
  v := TGLSceneViewer(trunc64(viewer));
  fps := v.FramesPerSecond;
  result := fps;
end;

function ViewerResetPerformanceMonitor(viewer: real): real; cdecl;
var
  v: TGLSceneViewer;
begin
  v := TGLSceneViewer(trunc64(viewer));
  v.ResetPerformanceMonitor;
  result := 1.0;
end;

function ViewerPixelRayToWorld(viewer,x, y,ind: real): real; cdecl;
var
vec: TAffineVector;
begin
  vec:=TGLSceneViewer(trunc64(viewer)).Buffer.PixelRayToWorld(trunc64(x),trunc64(y));
  result:=vec[trunc64(ind)];
end;

function ViewerShadeModel(viewer,ind: real): real; cdecl;
begin
if (ind=0) then
  TGLSceneViewer(trunc64(viewer)).Buffer.ShadeModel:=smFlat;
if (ind=1) then
   TGLSceneViewer(trunc64(viewer)).Buffer.ShadeModel:=smSmooth;
result:=1;
end;
