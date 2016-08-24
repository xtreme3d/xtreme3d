function ViewerCreate(top,left,w,h,pw: real): real; stdcall;
var
  GLSceneViewer1: TGLSceneViewer;
begin
  GLSceneViewer1:=TGLSceneViewer.Create(nil);
  GLSceneViewer1.Top:=trunc64(top);
  GLSceneViewer1.Left:=trunc64(left);
  GLSceneViewer1.Width:=trunc64(w);
  GLSceneViewer1.Height:=trunc64(h);
  GLSceneViewer1.Enabled:=false;
  GLSceneViewer1.Visible:=true;
  GLSceneViewer1.Buffer.Lighting:=true;
  GLSceneViewer1.ParentWindow:=trunc64(pw);
  GLSceneViewer1.Buffer.ContextOptions := [roDoubleBuffer, roStencilBuffer, roRenderToWindow];
  GLSceneViewer1.AutoRender := False; 
  result:=Integer(GLSceneViewer1);
end;

function ViewerSetCamera(viewer,camera: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Camera:=TGLCamera(trunc64(camera));
  result:=1;
end;

function ViewerEnableVSync(viewer,vsm: real): real; stdcall;
begin
  if vsm=0 then TGLSceneViewer(trunc64(viewer)).VSync:=vsmSync;
  if vsm=1 then TGLSceneViewer(trunc64(viewer)).VSync:=vsmNoSync;
  result:=1;
end;

function ViewerRender(viewer: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.Render();
  result:=1;
end;

function ViewerRenderToFile(viewer:real; fname:pchar): real; stdcall;
var
  bmp: TBitmap;
begin
  bmp := TGLSceneViewer(trunc64(viewer)).Buffer.CreateSnapShotBitmap;
  bmp.SaveToFile(String(fname));
  bmp.Free;
  result:=1;
end;

function ViewerResize(viewer,top,left,w,h:real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Top:=trunc64(top);
  TGLSceneViewer(trunc64(viewer)).Left:=trunc64(left);
  TGLSceneViewer(trunc64(viewer)).Width:=trunc64(w);
  TGLSceneViewer(trunc64(viewer)).Height:=trunc64(h);
  result:=1;
end;

function ViewerSetVisible(viewer,mode:real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Visible:=boolean(trunc64(mode));
  result:=1;
end;

function ViewerGetPixelColor(viewer,x,y:real): real; stdcall;
var
  color: TColor;
begin
  color:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPixelColor(trunc64(x),trunc64(y));
  result:=Integer(color);
end;

function ViewerGetPixelDepth(viewer,x,y:real): real; stdcall;
var
  depth: real;
begin
  depth:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPixelDepth(trunc64(x),trunc64(y));
  result:=depth;
end;

function ViewerSetLighting(viewer,mode:real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.Lighting:=boolean(trunc64(mode));
  result:=1;
end;

function ViewerSetBackgroundColor(viewer,color: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor:=TColor(trunc64(color));
  result:=1;
end;

function ViewerSetAmbientColor(viewer,color: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.AmbientColor.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function ViewerEnableFog(viewer,mode: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnable:=boolean(trunc64(mode));
  result:=1;
end;

function ViewerSetFogColor(viewer,color: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnvironment.FogColor.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function ViewerSetFogDistance(viewer,fstart,fend: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnvironment.FogStart:=fstart;
  TGLSceneViewer(trunc64(viewer)).Buffer.FogEnvironment.FogEnd:=fend;
  result:=1;
end;

function ViewerScreenToWorld(viewer,x,y,ind:real): real; stdcall;
var
  stw: TAffineVector;
begin
  stw:=TGLSceneViewer(trunc64(viewer)).Buffer.ScreenToWorld(trunc64(x),trunc64(y));
  result:=stw[trunc64(ind)];
end;

function ViewerWorldToScreen(viewer,x,y,z,ind:real): real; stdcall;
var
  vec: TAffineVector;
  wts: TAffineVector;
begin
  SetVector(vec,x,y,z);
  wts:=TGLSceneViewer(trunc64(viewer)).Buffer.WorldToScreen(vec);
  result:=wts[trunc64(ind)];
end;

function ViewerCopyToTexture(viewer:real; mtrl:pchar): real; stdcall;
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

function ViewerGetFramesPerSecond(viewer:real):real; stdcall;
var
  fps: real;
begin
  fps:=TGLSceneViewer(trunc64(viewer)).FramesPerSecond;
  result:=fps;
end;

function ViewerGetPickedObject(viewer,x,y:real):real; stdcall;
var
  obj:TGLBaseSceneObject;
begin
  obj:=TGLSceneViewer(trunc64(viewer)).Buffer.GetPickedObject(trunc64(x),trunc64(y));
  result:=Integer(obj);
end;

function ViewerGetPickedObjectsList(viewer,x,y,w,h,num,ind:real):real; stdcall;
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

function ViewerScreenToVector(viewer,x,y,ind:real):real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLSceneViewer(trunc64(viewer)).Buffer.ScreenToVector(trunc64(x),trunc64(y));
  result:=vec[trunc64(ind)];
end;

function ViewerVectorToScreen(viewer,x,y,z,ind:real):real; stdcall;
var
  dvec,rvec: TAffineVector;
begin
  SetVector(dvec,x,y,z);
  rvec:=TGLSceneViewer(trunc64(viewer)).Buffer.VectorToScreen(dvec);
  result:=rvec[trunc64(ind)];
end;

function ViewerPixelToDistance(viewer,x,y:real):real; stdcall;
var
  dist: real;
begin
  dist:=TGLSceneViewer(trunc64(viewer)).Buffer.PixelToDistance(trunc64(x),trunc64(y));
  result:=dist;
end;

function ViewerSetAntiAliasing(viewer,aa: real): real; stdcall;
begin
  if trunc64(aa)=0 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aaDefault;
  if trunc64(aa)=1 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aaNone;
  if trunc64(aa)=2 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa2x;
  if trunc64(aa)=3 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa2xHQ;
  if trunc64(aa)=4 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa4x;
  if trunc64(aa)=5 then TGLSceneViewer(trunc64(viewer)).Buffer.AntiAliasing:=aa4xHQ;
  result:=1;
end;

function ViewerGetVBOSupported(viewer:real): real; stdcall;
begin
   if GL_ARB_vertex_buffer_object then
       Result := 1
   else
       Result := 0;
end;

function ViewerGetGLSLSupported(viewer:real): real; stdcall;
begin
   if (GL_ARB_shader_objects and
       GL_ARB_vertex_shader and
       GL_ARB_fragment_shader) then
       Result := 1
   else
       Result := 0;
end;

function ViewerSetAutoRender(viewer, mode: real): real; stdcall;
begin
  TGLSceneViewer(trunc64(viewer)).AutoRender := Boolean(trunc64(mode));
  result:=1;
end;
