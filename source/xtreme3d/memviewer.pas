function MemoryViewerCreate(w,h: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer.Create(scene);
  mv.Width:=trunc(w);
  mv.Height:=trunc(h);
  mv.Buffer.ContextOptions := [roStencilBuffer];
  result:=ObjToReal(mv);
end;

function MemoryViewerSetCamera(mview,cam: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(RealToPtr(mview));
  mv.Camera:=TGLCamera(RealToPtr(cam));
  result:=1;
end;

function MemoryViewerRender(mview: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(RealToPtr(mview));
  mv.Render;
  result:=1;
end;

function MemoryViewerSetViewport(mview, x, y, w, h: real): real; cdecl;
var
  mv: TGLMemoryViewer;
begin
  mv:=TGLMemoryViewer(RealToPtr(mview));
  mv.Buffer.SetViewPort(trunc(x), trunc(y), trunc(w), trunc(h));
  result:=1;
end;

function MemoryViewerCopyToTexture(mview: real; matname: PAnsiChar): real; cdecl;
var
  mv: TGLMemoryViewer;
  mat:TGLLibMaterial;
begin
  mv:=TGLMemoryViewer(RealToPtr(mview));
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(matname));
  Assert(mat.Material.Texture.IsHandleAllocated);
  mv.CopyToTexture(mat.Material.Texture);
  result:=1;
end;
