function ObjectSetPosition(obj, x, y, z: real): real; cdecl;
var
    ob: TGLBaseSceneObject;
begin
    ob := TGLBaseSceneObject(RealToPtr(obj));
    ob.Position.X := x;
    ob.Position.Y := y;
    ob.Position.Z := z;
    result:=1;
end;

function ObjectRotate(obj, p, t, r: real): real; cdecl;
var
    ob: TGLBaseSceneObject;
begin
    ob := TGLBaseSceneObject(RealToPtr(obj));
    ob.Roll(r);
    ob.Turn(t);
    ob.Pitch(p);
    result := 1;
end;

function ObjectMove(obj,spd:real): real; cdecl;
var
    ob:TGLBaseSceneObject;
begin
    ob:=TGLBaseSceneObject(RealToPtr(obj));
    ob.Move(spd);
    result:=1;
end;

function ObjectLift(obj,spd:real): real; cdecl;
var
    ob:TGLBaseSceneObject;
begin
    ob:=TGLBaseSceneObject(RealToPtr(obj));
    ob.Lift(spd);
    result:=1;
end;

function ObjectStrafe(obj,spd:real): real; cdecl;
var
    ob:TGLBaseSceneObject;
begin
    ob:=TGLBaseSceneObject(RealToPtr(obj));
    ob.Slide(spd);
    result:=1;
end;
