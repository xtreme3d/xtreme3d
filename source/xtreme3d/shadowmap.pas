function ShadowMapCreate(size, viewer, caster: real): real; stdcall;
var
  v: TGLSceneViewer;
  sm: TGLShadowMap;
begin
  if not GL_ARB_framebuffer_object then
  begin
      ShowMessage('GL_ARB_frame_buffer_object required');
      result := 0;
      Exit;
  end;
  v := TGLSceneViewer(trunc64(viewer));
  sm := TGLShadowMap.Create;
  sm.Width := trunc64(size);
  sm.Height := trunc64(size);
  sm.MainBuffer := v.Buffer;
  sm.Caster := TGLBaseSceneObject(trunc64(caster));
  result:=integer(sm);
end;

function ShadowMapSetCamera(shadowmap, cam: real): real; stdcall;
var
  sm: TGLShadowMap;
begin
  sm:=TGLShadowMap(trunc64(shadowmap));
  sm.ShadowCamera := TGLCamera(trunc64(cam));
  result:=1;
end;

function ShadowMapSetCaster(shadowmap, caster: real): real; stdcall;
var
  sm: TGLShadowMap;
begin
  sm:=TGLShadowMap(trunc64(shadowmap));
  sm.Caster := TGLBaseSceneObject(trunc64(caster));
  result:=1;
end;

function ShadowMapSetProjectionSize(shadowmap, size: real): real; stdcall;
var
  sm: TGLShadowMap;
begin
  sm:=TGLShadowMap(trunc64(shadowmap));
  sm.ProjectionSize := size;
  result:=1;
end;

function ShadowMapSetZScale(shadowmap, scale: real): real; stdcall;
var
  sm: TGLShadowMap;
begin
  sm:=TGLShadowMap(trunc64(shadowmap));
  sm.ZScale := scale;
  result:=1;
end;

function ShadowMapSetZClippingPlanes(shadowmap, znear, zfar: real): real; stdcall;
var
  sm: TGLShadowMap;
begin
  sm:=TGLShadowMap(trunc64(shadowmap));
  sm.ZNear := znear;
  sm.ZFar := zfar;
  result:=1;
end;

function ShadowMapRender(shadowmap: real): real; stdcall;
var
  sm: TGLShadowMap;
begin
  sm:=TGLShadowMap(trunc64(shadowmap));
  sm.Render();
  result:=1;
end;
