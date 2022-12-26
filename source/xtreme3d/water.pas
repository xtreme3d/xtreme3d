function WaterCreate(parent: real): real; cdecl;
var
  water: TGLWaterPlane;
begin
  if not (parent=0) then
    water:=TGLWaterPlane.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    water:=TGLWaterPlane.CreateAsChild(scene.Objects);
  result:=ObjToReal(water);
end;

function WaterCreateRandomRipple(water: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).CreateRippleRandom;
  result:=1;
end;

function WaterCreateRippleAtGridPosition(water, x, y: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).CreateRippleAtGridPos(trunc(x), trunc(y));
  result:=1;
end;

function WaterCreateRippleAtWorldPosition(water, x, y, z: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).CreateRippleAtWorldPos(x, y, z);
  result:=1;
end;

function WaterCreateRippleAtObjectPosition(water, obj: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).CreateRippleAtWorldPos(TGLBaseSceneObject(trunc(obj)).AbsolutePosition);
  result:=1;
end;

function WaterSetMask(water: real; material: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(String(AnsiString(material)));
  with mat.Material.Texture.Image as TGLPictureImage do
      TGLWaterPlane(RealToPtr(water)).Mask := Picture;
  result:=1;
end;

function WaterSetActive(water, mode: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).Active := Boolean(trunc(mode));
  result:=1;
end;

function WaterReset(water: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).Reset;
  result:=1;
end;

function WaterSetRainTimeInterval(water, interval: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).RainTimeInterval := trunc(interval);
  result:=1;
end;

function WaterSetRainForce(water, force: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).RainForce := force;
  result:=1;
end;

function WaterSetViscosity(water, viscosity: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).Viscosity := viscosity;
  result:=1;
end;

function WaterSetElastic(water, elastic: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).Elastic := elastic;
  result:=1;
end;

function WaterSetResolution(water, res: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).Resolution := trunc(res);
  result:=1;
end;

{
function WaterSetLinearWaveHeight(water, height: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).SmoothWaveHeight := height;
  result:=1;
end;
}

{
function WaterSetLinearWaveFrequency(water, freq: real): real; cdecl;
begin
  TGLWaterPlane(RealToPtr(water)).SmoothWaveFrequency := freq;
  result:=1;
end;
}