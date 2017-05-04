// Object
function lua_ObjectHide(const Args: TLuaArgs): TLuaArg;
begin
  ObjectHide(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectShow(const Args: TLuaArgs): TLuaArg;
begin
  ObjectShow(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectIsVisible(const Args: TLuaArgs): TLuaArg;
var
  r: Boolean;
begin
  r := Boolean(trunc64(ObjectIsVisible(Args[0].AsDouble)));
  result := LuaArg(r);
end;

function lua_ObjectCopy(const Args: TLuaArgs): TLuaArg;
var
  r: double;
begin
  r := ObjectCopy(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(r);
end;

function lua_ObjectDestroy(const Args: TLuaArgs): TLuaArg;
begin
  ObjectDestroy(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectDestroyChildren(const Args: TLuaArgs): TLuaArg;
begin
  ObjectDestroyChildren(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetPosition(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetPosition(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetPosition(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetPosition(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetAbsolutePosition(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetAbsolutePosition(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectSetPositionOfObject(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetPositionOfObject(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectAlignWithObject(const Args: TLuaArgs): TLuaArg;
begin
  ObjectAlignWithObject(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetPositionX(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetPositionX(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetPositionY(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetPositionY(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetPositionZ(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetPositionZ(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetPositionX(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetPositionX(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetPositionY(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetPositionY(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetPositionZ(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetPositionZ(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectSetAbsolutePosition(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetAbsolutePosition(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetDirection(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetDirection(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetDirection(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetDirection(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectSetAbsoluteDirection(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetAbsoluteDirection(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetAbsoluteDirection(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetAbsoluteDirection(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetPitch(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetPitch(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetTurn(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetTurn(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetRoll(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetRoll(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectSetRotation(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetRotation(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectMove(const Args: TLuaArgs): TLuaArg;
begin
  ObjectMove(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectLift(const Args: TLuaArgs): TLuaArg;
begin
  ObjectLift(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectStrafe(const Args: TLuaArgs): TLuaArg;
begin
  ObjectStrafe(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectTranslate(const Args: TLuaArgs): TLuaArg;
begin
  ObjectTranslate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectRotate(const Args: TLuaArgs): TLuaArg;
begin
  ObjectRotate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectScale(const Args: TLuaArgs): TLuaArg;
begin
  ObjectScale(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetScale(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetScale(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetUpVector(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetUpVector(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectPointToObject(const Args: TLuaArgs): TLuaArg;
begin
  ObjectPointToObject(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectShowAxes(const Args: TLuaArgs): TLuaArg;
begin
  ObjectShowAxes(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetGroundHeight(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectGetGroundHeight(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectSceneRaycast(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectSceneRaycast(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectRaycast(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectRaycast(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectGetCollisionPosition(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectGetCollisionPosition(Args[0].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectGetCollisionNormal(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectGetCollisionNormal(Args[0].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectSetMaterial(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetMaterial(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(1.0);
end;

function lua_ObjectGetDistance(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectGetDistance(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectCheckCubeVsCube(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectCheckCubeVsCube(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectCheckSphereVsSphere(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectCheckSphereVsSphere(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectCheckSphereVsCube(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectCheckSphereVsCube(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectCheckCubeVsFace(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectCheckCubeVsFace(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectCheckFaceVsFace(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectCheckFaceVsFace(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectIsPointInObject(const Args: TLuaArgs): TLuaArg;
var
  h: double;
begin
  h := ObjectIsPointInObject(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(h);
end;

function lua_ObjectSetCulling(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetCulling(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetName(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetName(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(1.0);
end;

function lua_ObjectGetName(const Args: TLuaArgs): TLuaArg;
var
  n: pchar;
begin
  n := ObjectGetName(Args[0].AsDouble);
  result := LuaArg(String(n));
end;

function lua_ObjectGetClassName(const Args: TLuaArgs): TLuaArg;
var
  n: pchar;
begin
  n := ObjectGetClassName(Args[0].AsDouble);
  result := LuaArg(String(n));
end;

function lua_ObjectSetTag(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetTag(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetTag(const Args: TLuaArgs): TLuaArg;
var
  t: double;
begin
  t := ObjectGetTag(Args[0].AsDouble);
  result := LuaArg(t);
end;

function lua_ObjectGetParent(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ObjectGetParent(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_ObjectGetChildCount(const Args: TLuaArgs): TLuaArg;
var
  cc: double;
begin
  cc := ObjectGetChildCount(Args[0].AsDouble);
  result := LuaArg(cc);
end;

function lua_ObjectGetChild(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ObjectGetChild(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ObjectGetIndex(const Args: TLuaArgs): TLuaArg;
var
  i: double;
begin
  i := ObjectGetIndex(Args[0].AsDouble);
  result := LuaArg(i);
end;

function lua_ObjectFindChild(const Args: TLuaArgs): TLuaArg;
var
  i: double;
begin
  i := ObjectFindChild(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(i);
end;

function lua_ObjectGetBoundingSphereRadius(const Args: TLuaArgs): TLuaArg;
var
  r: double;
begin
  r := ObjectGetBoundingSphereRadius(Args[0].AsDouble);
  result := LuaArg(r);
end;

function lua_ObjectGetAbsoluteUp(const Args: TLuaArgs): TLuaArg;
var
  u: double;
begin
  u := ObjectGetAbsoluteUp(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(u);
end;

function lua_ObjectSetAbsoluteUp(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetAbsoluteUp(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetAbsoluteRight(const Args: TLuaArgs): TLuaArg;
var
  r: double;
begin
  r := ObjectGetAbsoluteRight(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(r);
end;

function lua_ObjectGetAbsoluteXVector(const Args: TLuaArgs): TLuaArg;
var
  x: double;
begin
  x := ObjectGetAbsoluteXVector(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(x);
end;

function lua_ObjectGetAbsoluteYVector(const Args: TLuaArgs): TLuaArg;
var
  y: double;
begin
  y := ObjectGetAbsoluteYVector(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(y);
end;

function lua_ObjectGetAbsoluteZVector(const Args: TLuaArgs): TLuaArg;
var
  z: double;
begin
  z := ObjectGetAbsoluteZVector(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(z);
end;

function lua_ObjectGetRight(const Args: TLuaArgs): TLuaArg;
var
  r: double;
begin
  r := ObjectGetRight(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(r);
end;

function lua_ObjectMoveChildUp(const Args: TLuaArgs): TLuaArg;
begin
  ObjectMoveChildUp(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectMoveChildDown(const Args: TLuaArgs): TLuaArg;
begin
  ObjectMoveChildDown(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetParent(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetParent(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectRemoveChild(const Args: TLuaArgs): TLuaArg;
begin
  ObjectRemoveChild(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectMoveObjectAround(const Args: TLuaArgs): TLuaArg;
begin
  ObjectMoveObjectAround(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectPitch(const Args: TLuaArgs): TLuaArg;
begin
  ObjectPitch(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectTurn(const Args: TLuaArgs): TLuaArg;
begin
  ObjectTurn(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectRoll(const Args: TLuaArgs): TLuaArg;
begin
  ObjectRoll(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectGetUp(const Args: TLuaArgs): TLuaArg;
var
  u: double;
begin
  u := ObjectGetUp(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(u);
end;

function lua_ObjectRotateAbsolute(const Args: TLuaArgs): TLuaArg;
begin
  ObjectRotateAbsolute(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectRotateAbsoluteVector(const Args: TLuaArgs): TLuaArg;
begin
  ObjectRotateAbsoluteVector(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectSetMatrixColumn(const Args: TLuaArgs): TLuaArg;
begin
  ObjectSetMatrixColumn(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectExportMatrix(const Args: TLuaArgs): TLuaArg;
begin
  ObjectExportMatrix(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ObjectExportAbsoluteMatrix(const Args: TLuaArgs): TLuaArg;
begin
  ObjectExportAbsoluteMatrix(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;
