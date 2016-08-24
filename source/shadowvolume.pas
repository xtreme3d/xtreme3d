function ShadowvolumeCreate(parent: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  if not (parent=0) then
    sv := TGLShadowVolume.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    sv := TGLShadowVolume.CreateAsChild(scene.Objects);
  result := Integer(sv);
end;

function ShadowvolumeSetActive(shadowvolume, active: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.Active := Boolean(trunc64(active));
  result := 1.0;
end;

function ShadowvolumeAddLight(shadowvolume, light: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.Lights.AddCaster(TGLBaseSceneObject(trunc64(light)));
  result := 1.0;
end;

function ShadowvolumeRemoveLight(shadowvolume, light: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.Lights.RemoveCaster(TGLBaseSceneObject(trunc64(light)));
  result := 1.0;
end;

function ShadowvolumeAddOccluder(shadowvolume, obj: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.Occluders.AddCaster(TGLBaseSceneObject(trunc64(obj)));
  result := 1.0;
end;

function ShadowvolumeRemoveOccluder(shadowvolume, obj: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.Occluders.RemoveCaster(TGLBaseSceneObject(trunc64(obj)));
  result := 1.0;
end;

function ShadowvolumeSetDarkeningColor(shadowvolume, color, alpha: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.DarkeningColor.AsWinColor := TColor(trunc64(color));
  sv.DarkeningColor.Alpha := alpha;
  result := 1.0;
end;

function ShadowvolumeSetMode(shadowvolume, svm: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  if svm = 0 then sv.Mode := svmAccurate;
  if svm = 1 then sv.Mode := svmDarkening;
  if svm = 2 then sv.Mode := svmOff;
  result := 1.0;
end;

function ShadowvolumeSetOptions(
  shadowvolume, showvolumes, cachesilhouettes,
  scissorclips, worldscissorclip: real): real; stdcall;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(trunc64(shadowvolume));
  sv.Options := [];
  if showvolumes = 1      then sv.Options := sv.Options + [svoShowVolumes];
  if cachesilhouettes = 1 then sv.Options := sv.Options + [svoCacheSilhouettes];
  if scissorclips = 1     then sv.Options := sv.Options + [svoScissorClips];
  if worldscissorclip = 1 then sv.Options := sv.Options + [svoWorldScissorClip];
  result := 1.0;
end;
