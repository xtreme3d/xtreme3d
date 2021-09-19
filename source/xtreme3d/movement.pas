function MovementCreate(obj: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  mov: TGLMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  mov := GetOrCreateMovement(ob);
  result := Integer(mov);
end;

function MovementStart(movement: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(trunc64(movement));
  mov.StartPathTravel;
  result := 1.0;
end;

function MovementStop(movement: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(trunc64(movement));
  mov.StopPathTravel;
  result := 1.0;
end;

// Switches to next movement when the current will end
// and continues moving. Movement will stop when no more paths left
function MovementAutoStartNextPath(movement, mode: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(trunc64(movement));
  mov.AutoStartNextPath := Boolean(trunc64(mode));
  result := 1.0;
end;

function MovementAddPath(movement: real): real; cdecl;
var
  mov: TGLMovement;
  path: TGLMovementPath;
begin
  mov := TGLMovement(trunc64(movement));
  path := mov.AddPath;
  result := Integer(path);
end;

// After switching active path, MovementStart should be called
// to start movement
function MovementSetActivePath(movement,ind: real): real; cdecl;
var
  mov: TGLMovement;
begin
  mov := TGLMovement(trunc64(movement));
  mov.ActivePathIndex := trunc64(ind);
  result := 1.0;
end;

function MovementPathSetSplineMode(path, lsm: real): real; cdecl;
var
  mpath: TGLMovementPath;
begin
  mpath := TGLMovementPath(trunc64(path));
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
  mpath := TGLMovementPath(trunc64(path));
  node := mpath.AddNode;
  node.Speed := 1.0;
  result := Integer(node);
end;

function MovementPathNodeSetPosition(node, x, y, z: real): real; cdecl;
var
  pnode: TGLPathNode;
begin
  pnode := TGLPathNode(trunc64(node));
  pnode.X := x;
  pnode.Y := y;
  pnode.Z := z;
  result := 1.0;
end;

function MovementPathNodeSetRotation(node, x, y, z: real): real; cdecl;
var
  pnode: TGLPathNode;
begin
  pnode := TGLPathNode(trunc64(node));
  pnode.PitchAngle := x;
  pnode.TurnAngle := y;
  pnode.RollAngle := z;
  result := 1.0;
end;

function MovementPathNodeSetSpeed(node, speed: real): real; cdecl;
var
  pnode: TGLPathNode;
begin
  pnode := TGLPathNode(trunc64(node));
  pnode.Speed := speed;
  result := 1.0;
end;

function MovementPathShow(pat,vis: real): real; cdecl;
var
 path: TGLMovementPath;
begin
  path := TGLMovementPath(trunc64(pat));
  path.ShowPath := Boolean(trunc64(vis));
  Result := 1;
end;

function MovementPathSetLoop(pat,loopn: real): real; cdecl;
var
 path: TGLMovementPath;
begin
  path := TGLMovementPath(trunc64(pat));
  path.Looped := Boolean(trunc64(loopn));
  Result := 1;
end;

function MovementPathDeleteNode(pat,node: real): real; cdecl;
var
 path: TGLMovementPath;
 nod: TGLPathNode;
begin
  path := TGLMovementPath(trunc64(pat));
  nod := TGLPathNode(trunc64(node));
  path.DeleteNode(nod);
  Result := 1;
end;

