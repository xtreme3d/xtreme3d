function ObjectHide(obj: real): real; stdcall;
begin
    TGLBaseSceneObject(trunc64(obj)).Visible := false;
    Result:=1;
end;

function ObjectShow(obj: real): real; stdcall;
begin
    TGLBaseSceneObject(trunc64(obj)).Visible := true;
    Result:=1;
end;

function ObjectIsVisible(obj: real): real; stdcall;
begin
    Result:=Integer(TGLBaseSceneObject(trunc64(obj)).Visible);
end;

function ObjectCopy(obj, parent: real): real; stdcall;
var
    obj1, obj2: TGLBaseSceneObject;
begin
    obj1 := TGLBaseSceneObject(trunc64(obj));
    if not (parent=0) then
        obj2:=TGLBaseSceneObject.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
    else
        obj2:=TGLBaseSceneObject.CreateAsChild(scene.Objects);
    obj2.Assign(obj1);
    result:=Integer(obj2);
end;

function ObjectDestroy(obj: real): real; stdcall;
begin
    TGLBaseSceneObject(trunc64(obj)).Free;
    result:=1;
end;

function ObjectDestroyChildren(obj: real): real; stdcall;
begin
    TGLBaseSceneObject(trunc64(obj)).DeleteChildren;
    result:=1;
end;

function ObjectSetPosition(obj,x,y,z:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Position.X:=x;
  GLObject.Position.Y:=y;
  GLObject.Position.Z:=z;
  result:=1;
end;

function ObjectGetPosition(obj, ind: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  result := sobj.Position.AsVector[trunc64(ind)];
end;

function ObjectGetAbsolutePosition(obj, ind: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  result := sobj.AbsolutePosition[trunc64(ind)];
end;

function ObjectSetPositionOfObject(obj1, obj2: real): real; stdcall;
var
  sobj1, sobj2: TGLBaseSceneObject;
begin
  sobj1 := TGLBaseSceneObject(trunc64(obj1));
  sobj2 := TGLBaseSceneObject(trunc64(obj2));
  sobj1.AbsolutePosition := sobj2.AbsolutePosition;
  result := 1;
end;

function ObjectAlignWithObject(obj1, obj2: real): real; stdcall;
var
  sobj1, sobj2: TGLBaseSceneObject;
begin
  sobj1 := TGLBaseSceneObject(trunc64(obj1));
  sobj2 := TGLBaseSceneObject(trunc64(obj2));
  sobj1.Rotation.AsVector := sobj2.Rotation.AsVector;
  result := 1;
end;

function ObjectSetPositionX(obj,posx: real): real; stdcall;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(trunc64(obj));
  Object1.Position.X:=posx;
  result:=1;
end;

function ObjectSetPositionY(obj,posy: real): real; stdcall;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(trunc64(obj));
  Object1.Position.Y:=posy;
  result:=1;
end;

function ObjectSetPositionZ(obj,posz: real): real; stdcall;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(trunc64(obj));
  Object1.Position.Z:=posz;
  result:=1;
end;

function ObjectGetPositionX(obj: real): real; stdcall;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(trunc64(obj));
  result:=Object1.Position.X;
end;

function ObjectGetPositionY(obj: real): real; stdcall;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(trunc64(obj));
  result:=Object1.Position.Y;
end;

function ObjectGetPositionZ(obj: real): real; stdcall;
var
  Object1: TGLBaseSceneObject;
begin
  Object1:=TGLBaseSceneObject(trunc64(obj));
  result:=Object1.Position.Z;
end;

function ObjectSetAbsolutePosition(obj, x, y, z: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  sobj.AbsolutePosition := VectorMake(x, y, z, 1.0);
  result := 1;
end;

function ObjectSetDirection(obj, x, y, z: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  sobj.Direction.SetVector(x, y, z);
  result := 1;
end;

function ObjectGetDirection(obj, ind: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  result := sobj.Direction.DirectVector[trunc64(ind)];
end;

function ObjectSetAbsoluteDirection(obj, x, y, z: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  sobj.AbsoluteDirection := VectorMake(x, y, z, 0.0);
  result := 1;
end;

function ObjectGetAbsoluteDirection(obj, ind: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  result := sobj.AbsoluteDirection[trunc64(ind)];
end;

function ObjectGetPitch(obj:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
  p: real;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  p:=GLObject.PitchAngle;
  result:=p;
end;

function ObjectGetTurn(obj:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
  p: real;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  p:=GLObject.TurnAngle;
  result:=p;
end;

function ObjectGetRoll(obj:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
  p: real;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  p:=GLObject.RollAngle;
  result:=p;
end;

function ObjectSetRotation(obj, x, y, z: real): real; stdcall;
var
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  sobj.Rotation.SetVector(x, y, z);
  result := 1;
end;

function ObjectMove(obj,spd:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Move(spd);
  result:=1;
end;

function ObjectLift(obj,spd:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Lift(spd);
  result:=1;
end;

function ObjectStrafe(obj,spd:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Slide(spd);
  result:=1;
end;

function ObjectTranslate(obj,x,y,z:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Translate(x, y, z);
  result:=1;
end;

function ObjectRotate(obj,p,t,r:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Roll(r);
  GLObject.Turn(t);
  GLObject.Pitch(p);
  result:=1;
end;

function ObjectScale(obj,x,y,z:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Scale.X := GLObject.Scale.X + x;
  GLObject.Scale.Y := GLObject.Scale.Y + y;
  GLObject.Scale.Z := GLObject.Scale.Z + z;
  result:=1;
end;

function ObjectSetScale(obj,x,y,z:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Scale.SetVector(x,y,z);
  result:=1;
end;

function ObjectSetUpVector(obj,x,y,z:real): real; stdcall;
var
  GLObject:TGLBaseSceneObject;
begin
  GLObject:=TGLBaseSceneObject(trunc64(obj));
  GLObject.Up.SetVector(x, y, z);
  result:=1;
end;

function ObjectPointToObject(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  object1.PointTo(object2, object1.Up.AsVector);
  result:=1;
end;

// unimplemented: ObjectAlignToCamera

// ObjectGetAtXY = ViewerGetPickedObject

function ObjectShowAxes(obj, mode: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj));
  object1.ShowAxes := Boolean(trunc64(mode));
  result:=1;
end;

function Raycast(
  obj, target: TGLBaseSceneObject;
  var isecPoint, isecNorm: TVector): Boolean;
var
  rstart, rdir, ipoint, inorm: TVector;
begin
  rstart := obj.AbsolutePosition;
  rdir := obj.AbsoluteDirection;
  if target.RayCastIntersect(rstart, rdir, @ipoint, @inorm) then
  begin
    isecPoint := ipoint;
    isecNorm := inorm;
    result := True;
  end
  else
    result := False;
end;

function RecursiveRaycast(
  obj, target: TGLBaseSceneObject;
  rstart, rdir: TVector;
  var isecPoint, isecNorm: TVector;
  var bestDistance: Single): TGLBaseSceneObject;
var
  ip, ipoint, inorm: TVector;
  bestObject: TGLBaseSceneObject;
  resObj: TGLBaseSceneObject;
  d: Single;
  i: Integer;
begin
  bestObject := nil;

  if target.RayCastIntersect(rstart, rdir, @ipoint, @inorm) then
  begin
    d := VectorDistance(rstart, ipoint);
    if d < bestDistance then
    begin
      isecPoint := ipoint;
      isecNorm := inorm;
      bestDistance := d;
      bestObject := target;
    end;
  end;

  for i := 0 to target.Count-1 do
  begin
    resObj := RecursiveRaycast(obj, target.Children[i], rstart, rdir, ipoint, inorm, bestDistance);
    if resObj <> nil then
    begin
      isecPoint := ipoint;
      isecNorm := inorm;
      bestObject := resObj;
    end;
  end;

  result := bestObject;
end;

function ObjectGetGroundHeight(obj, target: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
  rstart, rdir, ipoint, inorm: TVector;
  bestDistance: Single;
  i: Integer;
  maxh, h: Single;
  res: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj));
  object2:=TGLBaseSceneObject(trunc64(target));
  rstart := VectorMake(object1.AbsolutePosition[0], 1000, object1.AbsolutePosition[2], 1);
  rdir := VectorMake(0, -1, 0, 0);
  ipoint := VectorMake(0, -MaxSingle, 0, 0);
  
  bestDistance := MaxSingle;
  res := RecursiveRaycast(object1, object2, rstart, rdir, ipoint, inorm, bestDistance);
  if res <> nil then
    result:=ipoint[1]
  else
    result:=0;
end;

function ObjectSceneRaycast(obj, target: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
  res: TGLBaseSceneObject;
  rstart, rdir: TVector;
  bestDistance: Single;
begin
  object1:=TGLBaseSceneObject(trunc64(obj));
  object2:=TGLBaseSceneObject(trunc64(target));
  //collisionPoint := VectorMake(0, 0, 0, 0);
  //collisionNormal := VectorMake(0, 0, 0, 0);
  rstart := object1.AbsolutePosition;
  rdir := object1.AbsoluteDirection;
  bestDistance := MaxSingle;
  res := RecursiveRaycast(object1, object2, rstart, rdir, collisionPoint, collisionNormal, bestDistance);
  if res <> nil then
    result:=Integer(res)
  else
    result:=0;
end;

function ObjectRaycast(obj, target: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
  res: Boolean;
begin
  object1:=TGLBaseSceneObject(trunc64(obj));
  object2:=TGLBaseSceneObject(trunc64(target));
  //collisionPoint := VectorMake(0, 0, 0, 0);
  //collisionNormal := VectorMake(0, 0, 0, 0);
  res := Raycast(object1, object2, collisionPoint, collisionNormal);
  result:=Integer(res);
end;

function ObjectGetCollisionPosition(ind: real): real; stdcall;
begin
  result := collisionPoint[trunc64(ind)];
end;

function ObjectGetCollisionNormal(ind: real): real; stdcall;
begin
  result := collisionNormal[trunc64(ind)];
end;

//TODO:
//ObjectResetCollision()
//ObjectMoveToCollision(obj)

// ObjectUseObjectColor is removed
// ObjectSetDiffuseColor is removed

function ObjectSetMaterial(obj:real; mat:pchar): real; stdcall;
var
  object1:TGLSCeneObject;
  //ffm: TGLFreeForm;
  //mesh1: TMeshObject;
  //fg1: TFaceGroup;
  //mi, fgi: Integer;
begin
  object1:=TGLSceneObject(trunc64(obj));
  object1.Material.MaterialLibrary:=matlib;
  object1.Material.LibMaterialName:=mat;
{
  if object1.ClassType = TGLFreeForm then
  begin
    ffm := TGLFreeForm(trunc64(obj));
    //ffm.UseMeshMaterials := false;
    for mi:=0 to ffm.MeshObjects.Count-1 do begin
      mesh1 := ffm.MeshObjects[mi];

      for fgi:=0 to mesh1.FaceGroups.Count-1 do begin
        fg1 := mesh1.FaceGroups[fgi];
        fg1.MaterialName := String(mat);
      end;
    end;
  end;
 }
  result:=1;
end;

function ObjectGetDistance(obj, target: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj));
  object2:=TGLBaseSceneObject(trunc64(target));
  result:=VectorDistance(object1.AbsolutePosition, object2.AbsolutePosition);
end;

function ObjectCheckCubeVsCube(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  result := Integer(FastCheckCubeVsCube(object1, object2));
end;

function ObjectCheckSphereVsSphere(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  result := Integer(FastCheckSphereVsSphere(object1, object2));
end;

function ObjectCheckSphereVsCube(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  result := Integer(FastCheckSphereVsCube(object1, object2));
end;

function ObjectCheckCubeVsFace(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  result := Integer(FastCheckCubeVsFace(object1, object2));
end;

function ObjectCheckFaceVsFace(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  result := Integer(FastCheckFaceVsFace(object1, object2));
end;

function ObjectIsPointInObject(obj1, x, y, z: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  p: TVector;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  p := VectorMake(x, y, z, 1);
  if object1.PointInObject(p) then
      result := 1
  else
      result := 0;
end;

// Change: vc now can be vcNone, vcInherited, vcObjectBased, vcHierarchical (0, 1, 2, 3)
function ObjectSetCulling(obj1, vc: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  culling: Integer;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  culling := trunc64(vc);
  if culling = 0 then object1.VisibilityCulling := vcNone;
  if culling = 1 then object1.VisibilityCulling := vcInherited;
  if culling = 2 then object1.VisibilityCulling := vcObjectBased;
  if culling = 3 then object1.VisibilityCulling := vcHierarchical;
  result := 1;
end;

function ObjectSetName(obj1: real; name: pchar): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.Name := String(name);
  result := 1;
end;

function ObjectGetName(obj1: real): pchar; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := pchar(object1.Name);
end;

function ObjectGetClassName(obj1: real): pchar; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := pchar(String(object1.ClassName));
end;

// ObjectSetID is now ObjectSetTag
function ObjectSetTag(obj1, tag: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.Tag := trunc64(tag);
  result := 1;
end;

// ObjectGetID is now ObjectGetTag
function ObjectGetTag(obj1: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.Tag;
end;

function ObjectGetParent(obj1: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := Integer(object1.Parent);
end;

function ObjectGetChildCount(obj1: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.Count;
end;

function ObjectGetChild(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := Integer(object1.Children[trunc64(ind)]);
end;

function ObjectGetIndex(obj1: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.Index;
end;

function ObjectFindChild(obj1: real; name: pchar): real; stdcall;
var
  object1: TGLBaseSceneObject;
  n: String;
  i: Integer;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  n := String(name);

  for i := 0 to object1.Count do
  begin
     if AnsiCompareText(object1.Children[i].Name, n) = 0 then
     begin
         result := Integer(object1.Children[i]);
         Exit;
     end;
  end;
  
  result := 0;
end;

function ObjectGetBoundingSphereRadius(obj1: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.BoundingSphereRadius;
end;

function ObjectGetAbsoluteUp(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.AbsoluteUp[trunc64(ind)];
end;

function ObjectSetAbsoluteUp(obj1, x, y, z: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.AbsoluteUp := VectorMake(x, y, z, 0);
  result := 1;
end;

function ObjectGetAbsoluteRight(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.AbsoluteRight[trunc64(ind)];
end;

// Returns the Absolute X Vector expressed in object's local coordinates
function ObjectGetAbsoluteXVector(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.AbsoluteXVector[trunc64(ind)];
end;

// Returns the Absolute Y Vector expressed in object's local coordinates
function ObjectGetAbsoluteYVector(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.AbsoluteYVector[trunc64(ind)];
end;

// Returns the Absolute Z Vector expressed in object's local coordinates
function ObjectGetAbsoluteZVector(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.AbsoluteZVector[trunc64(ind)];
end;

function ObjectGetRight(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.Right[trunc64(ind)];
end;

function ObjectMoveChildUp(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.MoveChildUp(trunc64(ind));
  result := 1;
end;

function ObjectMoveChildDown(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.MoveChildDown(trunc64(ind));
  result := 1;
end;

function ObjectSetParent(obj1, obj2: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  object1.Parent := object2;
  result := 1;
end;

// keep - copy obj2's children to obj1 
function ObjectRemoveChild(obj1, obj2, keep: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  object1.Remove(object2, Boolean(trunc64(keep)));
  result := 1;
end;

function ObjectMoveObjectAround(obj1, obj2, p, t: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  object2: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object2:=TGLBaseSceneObject(trunc64(obj2));
  object1.MoveObjectAround(object2, p, t);
  result := 1;
end;

function ObjectPitch(obj1, angle: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.Pitch(angle);
  result := 1;
end;

function ObjectTurn(obj1, angle: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.Turn(angle);
  result := 1;
end;

function ObjectRoll(obj1, angle: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.Roll(angle);
  result := 1;
end;

// ObjectGetUpVector = ObjectGetUp
function ObjectGetUp(obj1, ind: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  result := object1.Up.AsVector[trunc64(ind)];
end;

// Unimplemented: ObjectStructureChanged

function ObjectRotateAbsolute(obj1, x, y, z: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  object1.RotateAbsolute(x, y, z);
  result := 1;
end;

function ObjectRotateAbsoluteVector(obj1, x, y, z, angle: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  v: TAffineVector;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  v[0] := x;
  v[1] := y;
  v[2] := z;
  object1.RotateAbsolute(v, angle);
  result := 1;
end;

// Sets object's local matrix column
function ObjectSetMatrixColumn(obj1, ind, x, y, z, w: real): real; stdcall;
var
  object1: TGLBaseSceneObject;
  i: Integer;
  m: TMatrix;
begin
  object1:=TGLBaseSceneObject(trunc64(obj1));
  i := trunc64(ind);
  m := object1.Matrix;
  m[i][0] := x;
  m[i][1] := y;
  m[i][2] := z;
  m[i][3] := w;
  object1.Matrix := m;
  result := 1;
end;

function ObjectExportMatrix(obj1,obj2: real): real; stdcall;
var
  GLObject1:TGLSCeneObject;
  GLObject2:TGLSceneObject;
  m: TMatrix4f;
begin
  GLObject1:=TGLSceneObject(trunc64(obj1));
  m:=GLObject1.Matrix;
  GLObject2:=TGLSceneObject(trunc64(obj2));
  GLObject2.Matrix:=m;
  result:=1;
end;

function ObjectExportAbsoluteMatrix(obj1,obj2: real): real; stdcall;
var
  GLObject1:TGLSCeneObject;
  GLObject2:TGLSceneObject;
  m: TMatrix4f;
begin
  GLObject1:=TGLSceneObject(trunc64(obj1));
  m:=GLObject1.AbsoluteMatrix;
  GLObject2:=TGLSceneObject(trunc64(obj2));
  GLObject2.Matrix:=m;
  result:=1;
end;
