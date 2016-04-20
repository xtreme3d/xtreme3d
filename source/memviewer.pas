function MemoryViewerCreate(w,h: real): real; stdcall;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer.Create(scene);
  mv.Width:=trunc64(w);
  mv.Height:=trunc64(h);
  mv.Buffer.Lighting:=false;
  result:=integer(mv);
end;

function MemoryViewerSetCamera(mview,cam: real): real; stdcall;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(trunc64(mview));
  mv.Camera:=TGLCamera(trunc64(cam));
  result:=1;
end;

function MemoryViewerRender(mview: real): real; stdcall;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(trunc64(mview));
  mv.Render;
  result:=1;
end;