function ClipPlaneCreate(parent: real): real; stdcall;
var
  cp: TGLClipPlane;
begin
  if not (parent = 0) then
    cp := TGLClipPlane.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    cp := TGLClipPlane.CreateAsChild(scene.Objects);
  result := Integer(cp);
end;

function ClipPlaneEnable(cplane, mode: real): real; stdcall;
var
  cp: TGLClipPlane;
begin
  cp := TGLClipPlane(trunc64(cplane));
  cp.ClipPlaneEnabled := Boolean(trunc64(mode));
  result := 1;
end;

function ClipPlaneSetPlane(cplane, px, py, pz, nx, ny, nz: real): real; stdcall;
var
  cp: TGLClipPlane;
begin
  cp := TGLClipPlane(trunc64(cplane));
  cp.SetClipPlane(AffineVectorMake(px, py, pz), AffineVectorMake(nx, ny, nz));
  result := 1;
end;
