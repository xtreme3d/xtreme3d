function TrailCreate(obj, parent: real): real; cdecl;
var
  t: TGLTrail;
begin
  if not (parent=0) then
    t := TGLTrail.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    t := TGLTrail.CreateAsChild(scene.Objects);
  t.TrailObject := TGLBaseSceneObject(RealToPtr(obj));
  result := ObjToReal(t);
end;

function TrailSetObject(trail, obj: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.TrailObject := TGLBaseSceneObject(RealToPtr(obj));
  result := 1.0;
end;

function TrailSetAlpha(trail, alpha, fade: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.Alpha := alpha;
  t.AlphaFade := Boolean(trunc(fade));
  result := 1.0;
end;

function TrailSetLimits(trail, vl, tl: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.VertLimit := trunc(vl);
  t.TimeLimit := tl;
  result := 1.0;
end;

function TrailSetMinDistance(trail, distance: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.MinDistance := distance;
  result := 1.0;
end;

function TrailSetUVScale(trail, scale: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.UVScale := scale;
  result := 1.0;
end;

function TrailSetMarkStyle(trail, ms: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  if ms = 0 then t.MarkStyle := msUp;
  if ms = 1 then t.MarkStyle := msDirection;
  if ms = 2 then t.MarkStyle := msFaceCamera;
  result := 1.0;
end;

function TrailSetMarkWidth(trail, width: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.MarkWidth := width;
  result := 1.0;
end;

function TrailSetEnabled(trail, mode: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.Enabled := Boolean(trunc(mode));
  result := 1.0;
end;

function TrailClearMarks(trail: real): real; cdecl;
var
  t: TGLTrail;
begin
  t := TGLTrail(RealToPtr(trail));
  t.ClearMarks;
  result := 1.0;
end;
