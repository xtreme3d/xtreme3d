function SkyboxCreate(parent: real): real; cdecl;
var
  sb: TGLSkyBox;
begin
  if not (parent=0) then
    sb := TGLSkyBox.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    sb := TGLSkyBox.CreateAsChild(scene.Objects);
  sb.MaterialLibrary := matlib;
  result := ObjToReal(sb);
end;

function SkyboxSetMaterial(skybox, sbm: real; matname: PAnsiChar): real; cdecl;
var
  sb: TGLSkyBox;
begin
  sb := TGLSkyBox(RealToPtr(skybox));
  if sbm = 0 then sb.MatNameTop := String(matname);
  if sbm = 1 then sb.MatNameBottom := String(matname);
  if sbm = 2 then sb.MatNameLeft := String(matname);
  if sbm = 3 then sb.MatNameRight := String(matname);
  if sbm = 4 then sb.MatNameFront := String(matname);
  if sbm = 5 then sb.MatNameBack := String(matname);
  if sbm = 6 then sb.MatNameClouds := String(matname); 
  result := 1;
end;

function SkyboxSetClouds(skybox, offset, size: real): real; cdecl;
var
  sb: TGLSkyBox;
begin
  sb := TGLSkyBox(RealToPtr(skybox));
  sb.CloudsPlaneOffset := offset;
  sb.CloudsPlaneSize := size;
  result := 1;
end;

function SkyboxSetStyle(skybox, sbs: real): real; cdecl;
var
  sb: TGLSkyBox;
begin
  sb := TGLSkyBox(RealToPtr(skybox));
  if sbs = 0 then sb.Style := sbsFull;
  if sbs = 1 then sb.Style := sbsTopHalf;
  if sbs = 2 then sb.Style := sbsBottomHalf;
  if sbs = 3 then sb.Style := sbTopTwoThirds;
  if sbs = 4 then sb.Style := sbsTopHalfClamped;
  result := 1;
end;
