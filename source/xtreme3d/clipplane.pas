function ClipPlaneCreate(parent: real): real; cdecl;
var
  cp: TGLClipPlane;
begin
  if not (parent = 0) then
    cp := TGLClipPlane.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    cp := TGLClipPlane.CreateAsChild(scene.Objects);
  result := Integer(cp);
end;

function ClipPlaneEnable(cplane, mode: real): real; cdecl;
var
  cp: TGLClipPlane;
begin
  cp := TGLClipPlane(RealToPtr(cplane));
  cp.ClipPlaneEnabled := Boolean(Trunc(mode));
  result := 1;
end;

function ClipPlaneSetPlane(cplane, px, py, pz, nx, ny, nz: real): real; cdecl;
var
  cp: TGLClipPlane;
begin
  cp := TGLClipPlane(RealToPtr(cplane));
  cp.SetClipPlane(AffineVectorMake(px, py, pz), AffineVectorMake(nx, ny, nz));
  result := 1;
end;
