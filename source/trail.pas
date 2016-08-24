function TrailCreate(obj, parent: real): real; stdcall;
var
  t: TGLTrail;
begin
  if not (parent=0) then
    t := TGLTrail.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    t := TGLTrail.CreateAsChild(scene.Objects);
  t.TrailObject := TGLBaseSceneObject(trunc64(obj));
  result := Integer(t);
end;

function TrailSetObject(trail, obj: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.TrailObject := TGLBaseSceneObject(trunc64(obj));
  result := 1.0;
end;

function TrailSetAlpha(trail, alpha, fade: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.Alpha := alpha;
  t.AlphaFade := Boolean(trunc64(fade));
  result := 1.0;
end;

function TrailSetLimits(trail, vl, tl: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.VertLimit := trunc64(vl);
  t.TimeLimit := tl;
  result := 1.0;
end;

function TrailSetMinDistance(trail, distance: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.MinDistance := distance;
  result := 1.0;
end;

function TrailSetUVScale(trail, scale: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.UVScale := scale;
  result := 1.0;
end;

function TrailSetMarkStyle(trail, ms: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  if ms = 0 then t.MarkStyle := msUp;
  if ms = 1 then t.MarkStyle := msDirection;
  if ms = 2 then t.MarkStyle := msFaceCamera;
  result := 1.0;
end;

function TrailSetMarkWidth(trail, width: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.MarkWidth := width;
  result := 1.0;
end;

function TrailSetEnabled(trail, mode: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.Enabled := Boolean(trunc64(mode));
  result := 1.0;
end;

function TrailClearMarks(trail: real): real; stdcall;
var
  t: TGLTrail;
begin
  t := TGLTrail(trunc64(trail));
  t.ClearMarks;
  result := 1.0;
end;
