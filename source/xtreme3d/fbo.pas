function FBOCreate(w, h, viewer: real): real; stdcall;
var
  fbo: TGLFBO;
  v: TGLSceneViewer;
begin
  if not GL_ARB_framebuffer_object then
  begin
      ShowMessage('GL_ARB_frame_buffer_object required');
      result := 0;
      Exit;
  end;
  v := TGLSceneViewer(trunc64(viewer));
  fbo := TGLFBO.Create;
  fbo.Width := trunc64(w);
  fbo.Height := trunc64(h);
  fbo.MainBuffer := v.Buffer;
  result := integer(fbo);
end;

function FBOSetCamera(fbo, cam: real): real; stdcall;
var
  fb: TGLFBO;
begin
  fb := TGLFBO(trunc64(fbo));
  fb.Camera := TGLCamera(trunc64(cam));
  result := 1;
end;

function FBORenderObject(fbo, obj: real): real; stdcall;
var
  fb: TGLFBO;
begin
  fb := TGLFBO(trunc64(fbo));
  fb.RenderObject := TGLBaseSceneObject(trunc64(obj));
  fb.Render();
  result := 1;
end;

function FBOSetViewer(fbo, viewer: real): real; stdcall;
var
  fb: TGLFBO;
  v: TGLSceneViewer;
begin
  fb := TGLFBO(trunc64(fbo));
  v := TGLSceneViewer(trunc64(viewer));
  fb.MainBuffer := v.Buffer;
  result := 1;
end;
