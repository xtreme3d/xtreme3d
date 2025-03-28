function ObjectHide(obj: real): real; cdecl;
begin
    TGLBaseSceneObject(RealToPtr(obj)).Visible := false;
    Result:=1;
end;

function ObjectShow(obj: real): real; cdecl;
begin
    TGLBaseSceneObject(RealToPtr(obj)).Visible := true;
    Result:=1;
end;

function ObjectIsVisible(obj: real): real; cdecl;
begin
    Result:=Integer(TGLBaseSceneObject(RealToPtr(obj)).Visible);
end;

function ObjectCopy(obj, parent: real): real; cdecl;
var
  obj1, obj2, par: TGLBaseSceneObject;
begin
  obj1 := TGLBaseSceneObject(RealToPtr(obj));
  if not (parent=0) then
    par := TGLBaseSceneObject(RealToPtr(parent))
  else
    par := scene.Objects;
  obj2 := TGLBaseSceneObject(obj1.NewInstance).CreateAsChild(par);
  result:=ObjToReal(obj2);
end;

function ObjectDestroy(obj: real): real; cdecl;
begin
    TObject(RealToPtr(obj)).Free;
    result:=1;
end;

function ObjectDestroyChildren(obj: real): real; cdecl;
begin
    TGLBaseSceneObject(RealToPtr(obj)).DeleteChildren;
    result:=1;
end;

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

function ObjectGetPosition(obj, ind: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  result := sobj.Position.AsVector.V[Trunc(ind)];
end;

function ObjectGetAbsolutePosition(obj, ind: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  result := sobj.AbsolutePosition.V[Trunc(ind)];
end;

function ObjectSetPositionOfObject(obj1, obj2: real): real; cdecl;
var
  sobj1, sobj2: TGLBaseSceneObject;
begin
  sobj1 := TGLBaseSceneObject(RealToPtr(obj1));
  sobj2 := TGLBaseSceneObject(RealToPtr(obj2));
  sobj1.AbsolutePosition := sobj2.AbsolutePosition;
  result := 1;
end;

function ObjectAlignWithObject(obj1, obj2: real): real; cdecl;
var
  sobj1, sobj2: TGLBaseSceneObject;
begin
  sobj1 := TGLBaseSceneObject(RealToPtr(obj1));
  sobj2 := TGLBaseSceneObject(RealToPtr(obj2));
  sobj1.Rotation.AsVector := sobj2.Rotation.AsVector;
  result := 1;
end;

function ObjectSetPositionX(obj,posx: real): real; cdecl;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(RealToPtr(obj));
  Object1.Position.X:=posx;
  result:=1;
end;

function ObjectSetPositionY(obj,posy: real): real; cdecl;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(RealToPtr(obj));
  Object1.Position.Y:=posy;
  result:=1;
end;

function ObjectSetPositionZ(obj,posz: real): real; cdecl;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(RealToPtr(obj));
  Object1.Position.Z:=posz;
  result:=1;
end;

function ObjectGetPositionX(obj: real): real; cdecl;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(RealToPtr(obj));
  result:=Object1.Position.X;
end;

function ObjectGetPositionY(obj: real): real; cdecl;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(RealToPtr(obj));
  result:=Object1.Position.Y;
end;

function ObjectGetPositionZ(obj: real): real; cdecl;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(RealToPtr(obj));
  result:=Object1.Position.Z;
end;

function ObjectSetAbsolutePosition(obj, x, y, z: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  sobj.AbsolutePosition := VectorMake(x, y, z, 1.0);
  result := 1;
end;

function ObjectSetDirection(obj, x, y, z: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  sobj.Direction.SetVector(x, y, z);
  result := 1;
end;

function ObjectGetDirection(obj, ind: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  result := sobj.Direction.DirectVector.V[Trunc(ind)];
end;

function ObjectSetAbsoluteDirection(obj, x, y, z: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  sobj.AbsoluteDirection := VectorMake(x, y, z, 0.0);
  result := 1;
end;

function ObjectGetAbsoluteDirection(obj, ind: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  result := sobj.AbsoluteDirection.V[Trunc(ind)];
end;

function ObjectGetPitch(obj:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
  p: real;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  p:=GLObject.PitchAngle;
  result:=p;
end;

function ObjectGetTurn(obj:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
  p: real;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  p:=GLObject.TurnAngle;
  result:=p;
end;

function ObjectGetRoll(obj:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
  p: real;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  p:=GLObject.RollAngle;
  result:=p;
end;

function ObjectSetRotation(obj, x, y, z: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  sobj.Rotation.SetVector(x, y, z);
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

function ObjectTranslate(obj,x,y,z:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  GLObject.Translate(x, y, z);
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

function ObjectScale(obj,x,y,z:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  GLObject.Scale.X := GLObject.Scale.X + x;
  GLObject.Scale.Y := GLObject.Scale.Y + y;
  GLObject.Scale.Z := GLObject.Scale.Z + z;
  result:=1;
end;

function ObjectSetScale(obj,x,y,z:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  GLObject.Scale.SetVector(x,y,z);
  result:=1;
end;

function ObjectGetScale(obj, ind: real): real; cdecl;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  result := sobj.Scale.AsVector.V[Trunc(ind)];
end;

function ObjectSetUpVector(obj,x,y,z:real): real; cdecl;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(RealToPtr(obj));
  GLObject.Up.SetVector(x, y, z);
  result:=1;
end;

function ObjectPointToObject(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  object1.PointTo(object2, object1.Up.AsVector);
  result:=1;
end;

function ObjectShowAxes(obj, mode: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj));
  object1.ShowAxes := Boolean(RealToPtr(mode));
  result:=1;
end;

function ObjectGetGroundHeight(obj, target: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
  rstart, rdir, ipoint, inorm: TGLVector;
  bestDistance: Single;
  res: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj));
  object2:=TGLBaseSceneObject(RealToPtr(target));
  rstart := VectorMake(object1.AbsolutePosition.V[0], 1000, object1.AbsolutePosition.V[2], 1);
  rdir := VectorMake(0, -1, 0, 0);
  ipoint := VectorMake(0, -MaxSingle, 0, 0);
  
  bestDistance := MaxSingle;
  res := RecursiveRaycast(object1, object2, rstart, rdir, ipoint, inorm, bestDistance);
  if res <> nil then
    result:=ipoint.V[1]
  else
    result:=0;
end;

function ObjectSceneRaycast(obj, target: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
  res: TGLBaseSceneObject;
  rstart, rdir: TGLVector;
  bestDistance: Single;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj));
  object2:=TGLBaseSceneObject(RealToPtr(target));
  rstart := object1.AbsolutePosition;
  rdir := object1.AbsoluteDirection;
  bestDistance := MaxSingle;
  res := RecursiveRaycast(object1, object2, rstart, rdir, collisionPoint, collisionNormal, bestDistance);
  if res <> nil then
    result:=ObjToReal(res)
  else
    result:=0;
end;

function ObjectRaycast(obj, target: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
  res: Boolean;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj));
  object2:=TGLBaseSceneObject(RealToPtr(target));
  res := Raycast(object1, object2, collisionPoint, collisionNormal);
  result:=Integer(res);
end;

// TODO: ObjectMoveToLastRaycastPosition(obj)

function ObjectSetMaterial(obj: real; mat: PAnsiChar): real; cdecl;
var
  object1:TGLSCeneObject;
begin
  object1:=TGLSceneObject(RealToPtr(obj));
  object1.Material.MaterialLibrary:=matlib;
  object1.Material.LibMaterialName:=String(AnsiString(mat));
  result:=1;
end;

function ObjectGetMaterial(obj: real): PAnsiChar; cdecl;
var
  object1:TGLSCeneObject;
begin
  object1:=TGLSceneObject(RealToPtr(obj));
  result:=PAnsiChar(AnsiString(object1.Material.LibMaterialName));
end;

function ObjectGetDistance(obj, target: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj));
  object2:=TGLBaseSceneObject(RealToPtr(target));
  result:=VectorDistance(object1.AbsolutePosition, object2.AbsolutePosition);
end;

function ObjectCheckCubeVsCube(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  result := Integer(FastCheckCubeVsCube(object1, object2));
end;

function ObjectCheckSphereVsSphere(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  result := Integer(FastCheckSphereVsSphere(object1, object2));
end;

function ObjectCheckSphereVsCube(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  result := Integer(FastCheckSphereVsCube(object1, object2));
end;

function ObjectCheckCubeVsFace(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  result := Integer(FastCheckCubeVsFace(object1, object2));
end;

function ObjectCheckFaceVsFace(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  result := Integer(FastCheckFaceVsFace(object1, object2));
end;

function ObjectIsPointInObject(obj1, x, y, z: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  p: TGLVector;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  p := VectorMake(x, y, z, 1);
  if object1.PointInObject(p) then
      result := 1
  else
      result := 0;
end;

function ObjectSetCulling(obj1, vc: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  culling: Integer;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  culling := Trunc(vc);
  if culling = 0 then object1.VisibilityCulling := vcNone;
  if culling = 1 then object1.VisibilityCulling := vcInherited;
  if culling = 2 then object1.VisibilityCulling := vcObjectBased;
  if culling = 3 then object1.VisibilityCulling := vcHierarchical;
  result := 1;
end;

function ObjectSetName(obj1: real; name: PAnsiChar): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.Name := String(AnsiString(name));
  result := 1;
end;

function ObjectGetName(obj1: real): PAnsiChar; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := PAnsiChar(AnsiString(object1.Name));
end;

function ObjectGetClassName(obj1: real): PAnsiChar; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := PAnsiChar(AnsiString(object1.ClassName));
end;

function ObjectSetTag(obj1, tag: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.Tag := Trunc(tag);
  result := 1;
end;

function ObjectGetTag(obj1: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.Tag;
end;

function ObjectGetParent(obj1: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := ObjToReal(object1.Parent);
end;

function ObjectGetChildCount(obj1: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.Count;
end;

function ObjectGetChild(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := ObjToReal(object1.Children[Trunc(ind)]);
end;

function ObjectGetIndex(obj1: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.Index;
end;

function ObjectFindChild(obj1: real; name: PAnsiChar): real; cdecl;
var
  object1: TGLBaseSceneObject;
  n: String;
  i: Integer;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  n := StrConv(name);

  for i := 0 to object1.Count do
  begin
     if AnsiCompareText(object1.Children[i].Name, n) = 0 then
     begin
         result := ObjToReal(object1.Children[i]);
         Exit;
     end;
  end;
  
  result := 0;
end;

function ObjectGetBoundingSphereRadius(obj1: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.BoundingSphereRadius;
end;

function ObjectGetAbsoluteUp(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.AbsoluteUp.V[Trunc(ind)];
end;

function ObjectSetAbsoluteUp(obj1, x, y, z: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.AbsoluteUp := VectorMake(x, y, z, 0);
  result := 1;
end;

function ObjectGetAbsoluteRight(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.AbsoluteRight.V[Trunc(ind)];
end;

// Returns the Absolute X Vector expressed in object's local coordinates
function ObjectGetAbsoluteXVector(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.AbsoluteXVector.V[Trunc(ind)];
end;

// Returns the Absolute Y Vector expressed in object's local coordinates
function ObjectGetAbsoluteYVector(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.AbsoluteYVector.V[Trunc(ind)];
end;

// Returns the Absolute Z Vector expressed in object's local coordinates
function ObjectGetAbsoluteZVector(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.AbsoluteZVector.V[Trunc(ind)];
end;

function ObjectGetRight(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.Right.V[Trunc(ind)];
end;

function ObjectMoveChildUp(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.MoveChildUp(Trunc(ind));
  result := 1;
end;

function ObjectMoveChildDown(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.MoveChildDown(Trunc(ind));
  result := 1;
end;

function ObjectSetParent(obj1, obj2: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  object1.Parent := object2;
  result := 1;
end;

// keep - copy obj2's children to obj1
function ObjectRemoveChild(obj1, obj2, keep: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  object1.Remove(object2, Boolean(Trunc(keep)));
  result := 1;
end;

function ObjectMoveObjectAround(obj1, obj2, p, t: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object2:=TGLBaseSceneObject(RealToPtr(obj2));
  object1.MoveObjectAround(object2, p, t);
  result := 1;
end;

function ObjectPitch(obj1, angle: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.Pitch(angle);
  result := 1;
end;

function ObjectTurn(obj1, angle: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.Turn(angle);
  result := 1;
end;

function ObjectRoll(obj1, angle: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.Roll(angle);
  result := 1;
end;

function ObjectGetUp(obj1, ind: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  result := object1.Up.AsVector.V[Trunc(ind)];
end;

function ObjectRotateAbsolute(obj1, x, y, z: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  object1.RotateAbsolute(x, y, z);
  result := 1;
end;

function ObjectRotateAbsoluteVector(obj1, x, y, z, angle: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  v: TAffineVector;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  v.V[0] := x;
  v.V[1] := y;
  v.V[2] := z;
  object1.RotateAbsolute(v, angle);
  result := 1;
end;

// Sets object's local matrix column
function ObjectSetMatrixColumn(obj1, ind, x, y, z, w: real): real; cdecl;
var
  object1: TGLBaseSceneObject;
  i: Integer;
  m: TMatrix4f;
begin
  object1:=TGLBaseSceneObject(RealToPtr(obj1));
  i := Trunc(ind);
  m := object1.Matrix^;
  m.V[i].V[0] := x;
  m.V[i].V[1] := y;
  m.V[i].V[2] := z;
  m.V[i].V[3] := w;
  object1.SetMatrix(m);
  result := 1;
end;

function ObjectExportMatrix(obj1,obj2: real): real; cdecl;
var
  GLObject1:TGLSCeneObject;
  GLObject2:TGLSceneObject;
  m: TMatrix4f;
begin
  GLObject1:=TGLSceneObject(RealToPtr(obj1));
  m:=GLObject1.Matrix^;
  GLObject2:=TGLSceneObject(RealToPtr(obj2));
  GLObject2.SetMatrix(m);
  result:=1;
end;

function ObjectExportAbsoluteMatrix(obj1,obj2: real): real; cdecl;
var
  GLObject1:TGLSCeneObject;
  GLObject2:TGLSceneObject;
  m: TMatrix4f;
begin
  GLObject1:=TGLSceneObject(RealToPtr(obj1));
  m:=GLObject1.AbsoluteMatrix;
  GLObject2:=TGLSceneObject(RealToPtr(obj2));
  GLObject2.SetMatrix(m);
  result:=1;
end;

function ObjectInFrustum(obj, viewer: real): real; cdecl;
var
  obj1: TGLBaseSceneObject;
  mvp: TGLMatrix;
  frustum: TFrustum;
  v: TGLSceneViewer;
begin
  obj1 := TGLBaseSceneObject(RealToPtr(obj));
  v := TGLSceneViewer(RealToPtr(viewer));
  mvp := MatrixMultiply(MatrixMultiply(v.Buffer.ModelMatrix, v.Buffer.ViewMatrix), v.Buffer.ProjectionMatrix);
  frustum := ExtractFrustumFromModelViewProjection(mvp);
  result := Integer(not IsVolumeClipped(
    AffineVectorMake(obj1.AbsolutePosition), obj1.BoundingSphereRadius, frustum));
end;

function ObjectFindByName(name: PAnsiChar): real; cdecl;
begin
  result:=ObjToReal(scene.FindSceneObject(StrConv(name)));
end;

function ObjectIgnoreDepthBuffer(obj, mode: real): real; cdecl;
var
  obj1: TGLBaseSceneObject;
begin
  obj1 := TGLBaseSceneObject(RealToPtr(obj));
  if Boolean(Trunc(mode))=true then
    obj1.ObjectStyle := obj1.ObjectStyle + [osIgnoreDepthBuffer]
  else
    obj1.ObjectStyle := obj1.ObjectStyle + [osIgnoreDepthBuffer];
  result:=1;
end;

function ObjectIsPicked(obj, viewer, x, y: real): real; cdecl;
var
  viewer1: TGLSceneViewer;
  obj1: TGLBaseSceneObject;
  pkList : TGLPickList;
  rect: TRect;
begin
  viewer1 := TGLSceneViewer(RealToPtr(viewer));
  obj1 := TGLBaseSceneObject(RealToPtr(obj));
  rect := TRect.Create(Trunc(x)-1, Trunc(y)-1, Trunc(x)+1, Trunc(y)+1);
  result:=0;
  pkList := viewer1.Buffer.GetPickedObjects(rect);
  if pkList.Count > 0 then begin
    if pkList.FindObject(obj1) > -1 then result:=1;
  end;
end;

function ObjectStructureChanged(obj: real): real; cdecl;
var
  GLObject1: TGLSCeneObject;
begin
  GLObject1 := TGLSceneObject(RealToPtr(obj));
  GLObject1.StructureChanged;
  result:=1;
end;

function ObjectClearStructureChanged(obj: real): real; cdecl;
var
  GLObject1: TGLSCeneObject;
begin
  GLObject1 := TGLSceneObject(RealToPtr(obj));
  GLObject1.ClearStructureChanged;
  result:=1;
end;

function ObjectNotifyChange(obj: real): real; cdecl;
var
  GLObject1:TGLSCeneObject;
begin
  GLObject1:=TGLSceneObject(RealToPtr(obj));
  GLObject1.NotifyChange(GLObject1);
  result:=1;
end;
