function FBOCreate(w, h, parent: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  if not (parent = 0) then
        fbor := TGLFBORendererEx.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
        fbor := TGLFBORendererEx.Create(scene.Objects);
  fbor.Width := Trunc(w);
  fbor.Height := Trunc(h);
  fbor.PostGenerateMipmap := False;
  result := PtrToReal(fbor);
end;

function FBOSetActive(fbo, mode: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.Active := Boolean(Trunc(mode));
  result := 1;
end;

function FBOSetAspect(fbo, aspect: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.Aspect := aspect;
  result := 1;
end;

function FBOSetPickableTarget(fbo, mode: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.PickableTarget := Boolean(Trunc(mode));
  result := 1;
end;

function FBOSetSize(fbo, width, height: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.Width := Trunc(width);
  fbor.Height := Trunc(height);
  result := 1;
end;

function FBOSetCamera(fbo, cam: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.Camera := TGLCamera(RealToPtr(cam));
  result := 1;
end;

function FBOSetRootObject(fbo, obj: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.RootObject := TGLBaseSceneObject(RealToPtr(obj));
  result := 1;
end;

function FBOSetBackgroundColor(fbo, color: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.BackgroundColor.AsWinColor := TColor(Trunc(color));
  result := 1;
end;

function FBOSetEnabledRenderBuffers(fbo, depth, stencil: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.EnabledRenderBuffers := [];
  if Boolean(Trunc(depth)) = true then
    fbor.EnabledRenderBuffers := fbor.EnabledRenderBuffers + [erbDepth];
  if Boolean(Trunc(stencil)) = true then
    fbor.EnabledRenderBuffers := fbor.EnabledRenderBuffers + [erbStencil];
  result := 1;
end;

function FBOSetSceneScaleFactor(fbo, scaleFactor: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.SceneScaleFactor := scaleFactor;
  result := 1;
end;

function FBOSetTargetVisibility(fbo, tv: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  if tv = 0 then fbor.TargetVisibility := tvDefault;
  if tv = 1 then fbor.TargetVisibility := tvFBOOnly;
  result := 1;
end;

function FBOSetMaterialLibrary(fbo, matlib: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.MaterialLibrary := TGLAbstractMaterialLibrary(RealToPtr(matlib));
  result := 1;
end;

function FBOSetColorTextureName(fbo: real; name: PAnsiChar): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.ColorTextureName := StrConv(name);
  result := 1;
end;

function FBOSetDepthTextureName(fbo: real; name: PAnsiChar): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.DepthTextureName := StrConv(name);
  result := 1;
end;

function FBOSetClearOptions(fbo, clearColor, clearDepth, clearStencil, useBufferBackground: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  fbor.ClearOptions := [];
  if Boolean(Trunc(clearColor)) = true then
    fbor.ClearOptions := fbor.ClearOptions + [coColorBufferClear];
  if Boolean(Trunc(clearDepth)) = true then
    fbor.ClearOptions := fbor.ClearOptions + [coDepthBufferClear];
  if Boolean(Trunc(clearStencil)) = true then
    fbor.ClearOptions := fbor.ClearOptions + [coStencilBufferClear];
  if Boolean(Trunc(useBufferBackground)) = true then
    fbor.ClearOptions := fbor.ClearOptions + [coUseBufferBackground];
  result := 1;
end;

function FBOSetStencilPrecision(fbo, sp: real): real; cdecl;
var
  fbor: TGLFBORendererEx;
begin
  fbor := TGLFBORendererEx(RealToPtr(fbo));
  if sp = 0 then fbor.StencilPrecision := spDefault;
  if sp = 1 then fbor.StencilPrecision := sp1bit;
  if sp = 2 then fbor.StencilPrecision := sp4bits;
  if sp = 3 then fbor.StencilPrecision := sp8bits;
  if sp = 4 then fbor.StencilPrecision := sp16bits;
  result := 1;
end;

