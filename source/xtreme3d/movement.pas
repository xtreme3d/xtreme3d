function MovementCreate(obj: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  mov: TGLMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  mov := GetOrCreateMovement(ob);
  result := ObjToReal(mov);
end;

function MovementStart(movement: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(RealToPtr(movement));
  mov.StartPathTravel;
  result := 1.0;
end;

function MovementStop(movement: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(RealToPtr(movement));
  mov.StopPathTravel;
  result := 1.0;
end;

// Switches to next movement when the current will end
// and continues moving. Movement will stop when no more paths left
function MovementAutoStartNextPath(movement, mode: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(RealToPtr(movement));
  mov.AutoStartNextPath := Boolean(trunc(mode));
  result := 1.0;
end;

function MovementAddPath(movement: real): real; cdecl;
var
  mov: TGLMovement;
  path: TGLMovementPath;
begin
  mov := TGLMovement(RealToPtr(movement));
  path := mov.AddPath;
  result := ObjToReal(path);
end;

// After switching active path, MovementStart should be called to start movement
function MovementSetActivePath(movement,ind: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(RealToPtr(movement));
  mov.ActivePathIndex := trunc(ind);
  result := 1.0;
end;

function MovementPathSetSplineMode(path, lsm: real): real; cdecl;
var
  mpath: TGLMovementPath;
begin
  mpath := TGLMovementPath(RealToPtr(path));
  if lsm = 0 then mpath.PathSplineMode := lsmLines;
  if lsm = 1 then mpath.PathSplineMode := lsmCubicSpline; // default mode
  if lsm = 2 then mpath.PathSplineMode := lsmBezierSpline;
  if lsm = 3 then mpath.PathSplineMode := lsmNURBSCurve;
  if lsm = 4 then mpath.PathSplineMode := lsmSegments;
  result := 1.0;
end;

function MovementPathAddNode(path: real): real; cdecl;
var
  mpath: TGLMovementPath;
  node: TGLPathNode;
begin
  mpath := TGLMovementPath(RealToPtr(path));
  node := mpath.AddNode;
  node.Speed := 1.0;
  result := ObjToReal(node);
end;

function MovementPathNodeSetPosition(node, x, y, z: real): real; cdecl;
var
  pnode: TGLPathNode;
begin
  pnode := TGLPathNode(RealToPtr(node));
  pnode.X := x;
  pnode.Y := y;
  pnode.Z := z;
  result := 1.0;
end;

function MovementPathNodeSetRotation(node, x, y, z: real): real; cdecl;
var
  pnode: TGLPathNode;
begin
  pnode := TGLPathNode(RealToPtr(node));
  pnode.PitchAngle := x;
  pnode.TurnAngle := y;
  pnode.RollAngle := z;
  result := 1.0;
end;

function MovementPathNodeSetSpeed(node, speed: real): real; cdecl;
var
  pnode: TGLPathNode;
begin
  pnode := TGLPathNode(RealToPtr(node));
  pnode.Speed := speed;
  result := 1.0;
end;

function MovementPathShow(pat,vis: real): real; cdecl;
var
 path: TGLMovementPath;
begin
  path := TGLMovementPath(RealToPtr(pat));
  path.ShowPath := Boolean(trunc(vis));
  Result := 1;
end;

function MovementPathSetLoop(pat,loopn: real): real; cdecl;
var
 path: TGLMovementPath;
begin
  path := TGLMovementPath(RealToPtr(pat));
  path.Looped := Boolean(trunc(loopn));
  Result := 1;
end;

function MovementPathDeleteNode(pat,node: real): real; cdecl;
var
 path: TGLMovementPath;
 nod: TGLPathNode;
begin
  path := TGLMovementPath(RealToPtr(pat));
  nod := TGLPathNode(trunc(node));
  path.DeleteNode(nod);
  Result := 1;
end;
