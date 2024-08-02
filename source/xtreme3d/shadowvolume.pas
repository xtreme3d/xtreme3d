function ShadowvolumeCreate(parent: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  if not (parent=0) then
    sv := TGLShadowVolume.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    sv := TGLShadowVolume.CreateAsChild(scene.Objects);
  result := ObjToReal(sv);
end;

function ShadowvolumeSetActive(shadowvolume, active: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.Active := Boolean(trunc(active));
  result := 1.0;
end;

function ShadowvolumeAddLight(shadowvolume, light: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.Lights.AddCaster(TGLBaseSceneObject(RealToPtr(light)));
  result := 1.0;
end;

function ShadowvolumeRemoveLight(shadowvolume, light: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.Lights.RemoveCaster(TGLBaseSceneObject(RealToPtr(light)));
  result := 1.0;
end;

function ShadowvolumeAddOccluder(shadowvolume, obj: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.Occluders.AddCaster(TGLBaseSceneObject(RealToPtr(obj)));
  result := 1.0;
end;

function ShadowvolumeRemoveOccluder(shadowvolume, obj: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.Occluders.RemoveCaster(TGLBaseSceneObject(RealToPtr(obj)));
  result := 1.0;
end;

function ShadowvolumeSetDarkeningColor(shadowvolume, color, alpha: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.DarkeningColor.AsWinColor := TColor(trunc(color));
  sv.DarkeningColor.Alpha := alpha;
  result := 1.0;
end;

function ShadowvolumeSetMode(shadowvolume, svm: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  if svm = 0 then sv.Mode := svmAccurate;
  if svm = 1 then sv.Mode := svmDarkening;
  if svm = 2 then sv.Mode := svmOff;
  result := 1.0;
end;

function ShadowvolumeSetOptions(
  shadowvolume, showvolumes, cachesilhouettes,
  scissorclips, worldscissorclip: real): real; cdecl;
var
  sv: TGLShadowVolume;
begin
  sv := TGLShadowVolume(RealToPtr(shadowvolume));
  sv.Options := [];
  if showvolumes = 1      then sv.Options := sv.Options + [svoShowVolumes];
  if cachesilhouettes = 1 then sv.Options := sv.Options + [svoCacheSilhouettes];
  if scissorclips = 1     then sv.Options := sv.Options + [svoScissorClips];
  if worldscissorclip = 1 then sv.Options := sv.Options + [svoWorldScissorClip];
  result := 1.0;
end;
