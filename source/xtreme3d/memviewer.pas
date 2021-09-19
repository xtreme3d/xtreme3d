function MemoryViewerCreate(w,h: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer.Create(scene);
  mv.Width:=trunc64(w);
  mv.Height:=trunc64(h);
  mv.Buffer.ContextOptions := [roStencilBuffer];
  result:=integer(mv);
end;

function MemoryViewerSetCamera(mview,cam: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(trunc64(mview));
  mv.Camera:=TGLCamera(trunc64(cam));
  result:=1;
end;

function MemoryViewerRender(mview: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(trunc64(mview));
  mv.Render;
  result:=1;
end;

function MemoryViewerSetViewport(mview, x, y, w, h: real): real; cdecl;
var
  mv: TGLMemoryViewer;
  mat:TGLLibMaterial;
begin
  mv:=TGLMemoryViewer(trunc64(mview));
  mv.Buffer.SetViewPort(trunc64(x), trunc64(y), trunc64(w), trunc64(h));
  result:=1;
end;

function MemoryViewerCopyToTexture(mview: real; matname: pchar): real; cdecl;
var
  mv: TGLMemoryViewer;
  mat:TGLLibMaterial;
begin
  mv:=TGLMemoryViewer(trunc64(mview));
  mat:=matlib.Materials.GetLibMaterialByName(String(matname));
  Assert(mat.Material.Texture.IsHandleAllocated);
  mv.CopyToTexture(mat.Material.Texture);
  result:=1;
end;