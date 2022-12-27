function MirrorCreate(target, parent: real): real; cdecl;
var
  mi: TGLMirror;
begin
  if not (parent=0) then
    mi := TGLMirror.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    mi := TGLMirror.CreateAsChild(scene.Objects);
  mi.MirrorObject := TGLBaseSceneObject(RealToPtr(target));
  Result := Integer(mi);
end;

function MirrorSetObject(mirror, target: real): real; cdecl;
var
  mi: TGLMirror;
begin
  mi := TGLMirror(RealToPtr(mirror));
  mi.MirrorObject := TGLBaseSceneObject(RealToPtr(target));
  Result := 1.0;
end;

function MirrorSetOptions(mirror, stencil, opaque, planeclipping, clearzbuffer: real): real; cdecl;
var
  mi: TGLMirror;
begin
  mi := TGLMirror(RealToPtr(mirror));
  mi.MirrorOptions := [];
  if stencil = 1       then mi.MirrorOptions := mi.MirrorOptions + [moUseStencil];
  if opaque = 1        then mi.MirrorOptions := mi.MirrorOptions + [moOpaque];
  if planeclipping = 1 then mi.MirrorOptions := mi.MirrorOptions + [moMirrorPlaneClip];
  if clearzbuffer = 1  then mi.MirrorOptions := mi.MirrorOptions + [moClearZBuffer];
  Result := 1.0;
end;

function MirrorSetShape(mirror, ms: real): real; cdecl;
var
  mi: TGLMirror;
begin
  mi := TGLMirror(RealToPtr(mirror));
  if ms = 0 then mi.Shape := msRect;
  if ms = 1 then mi.Shape := msDisk;
  Result := 1.0;
end;

function MirrorSetDiskOptions(mirror, radius, slices: real): real; cdecl;
var
  mi: TGLMirror;
begin
  mi := TGLMirror(RealToPtr(mirror));
  mi.Radius := radius;
  mi.Slices := trunc(slices);
  Result := 1.0;
end;
