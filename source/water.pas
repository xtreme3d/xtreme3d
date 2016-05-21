function WaterCreate(parent: real): real; stdcall;
var
  water: TGLWaterPlane;
begin
  if not (parent=0) then
    water:=TGLWaterPlane.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    water:=TGLWaterPlane.CreateAsChild(scene.Objects);
  result:=Integer(water);
end;

function WaterCreateRandomRipple(water: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).CreateRippleRandom;
  result:=1;
end;

function WaterCreateRippleAtGridPosition(water, x, y: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).CreateRippleAtGridPos(trunc64(x), trunc64(y));
  result:=1;
end;

function WaterCreateRippleAtWorldPosition(water, x, y, z: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).CreateRippleAtWorldPos(x, y, z);
  result:=1;
end;

function WaterCreateRippleAtObjectPosition(water, obj: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).CreateRippleAtWorldPos(TGLBaseSceneObject(trunc64(obj)).AbsolutePosition);
  result:=1;
end;

function WaterSetMask(water: real; material: pchar): real; stdcall;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(String(material));
  with mat.Material.Texture.Image as TGLPictureImage do
      TGLWaterPlane(trunc64(water)).Mask := Picture;
  result:=1;
end;

function WaterSetActive(water, mode: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).Active := Boolean(trunc64(mode));
  result:=1;
end;

function WaterReset(water: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).Reset;
  result:=1;
end;

function WaterSetRainTimeInterval(water, interval: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).RainTimeInterval := trunc64(interval);
  result:=1;
end;

function WaterSetRainForce(water, force: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).RainForce := force;
  result:=1;
end;

function WaterSetViscosity(water, viscosity: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).Viscosity := viscosity;
  result:=1;
end;

function WaterSetElastic(water, elastic: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).Elastic := elastic;
  result:=1;
end;

function WaterSetResolution(water, res: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).Resolution := trunc64(res);
  result:=1;
end;

function WaterSetLinearWaveHeight(water, height: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).SmoothWaveHeight := height;
  result:=1;
end;

function WaterSetLinearWaveFrequency(water, freq: real): real; stdcall;
begin
  TGLWaterPlane(trunc64(water)).SmoothWaveFrequency := freq;
  result:=1;
end;