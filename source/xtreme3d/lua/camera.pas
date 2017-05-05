// Camera
function lua_CameraCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := CameraCreate(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_CameraSetStyle(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetStyle(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraSetFocal(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetFocal(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraSetSceneScale(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetSceneScale(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraScaleScene(const Args: TLuaArgs): TLuaArg;
begin
  CameraScaleScene(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraSetViewDepth(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetViewDepth(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraSetTargetObject(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetTargetObject(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraMoveAroundTarget(const Args: TLuaArgs): TLuaArg;
begin
  CameraMoveAroundTarget(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraSetDistanceToTarget(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetDistanceToTarget(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraGetDistanceToTarget(const Args: TLuaArgs): TLuaArg;
var
  d: double;
begin
  d := CameraGetDistanceToTarget(Args[0].AsDouble);
  result := LuaArg(d);
end;

function lua_CameraCopyToTexture(const Args: TLuaArgs): TLuaArg;
begin
  CameraCopyToTexture(Args[0].AsDouble, pchar(Args[1].AsString), Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraGetNearPlane(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := CameraGetNearPlane(Args[0].AsDouble);
  result := LuaArg(p);
end;

function lua_CameraSetNearPlaneBias(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetNearPlaneBias(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraAbsoluteVectorToTarget(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraAbsoluteVectorToTarget(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraAbsoluteRightVectorToTarget(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraAbsoluteRightVectorToTarget(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraAbsoluteUpVectorToTarget(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraAbsoluteUpVectorToTarget(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraZoomAll(const Args: TLuaArgs): TLuaArg;
begin
  CameraZoomAll(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraScreenDeltaToVector(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraScreenDeltaToVector(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, 
    Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble, Args[7].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraScreenDeltaToVectorXY(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraScreenDeltaToVectorXY(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, 
    Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraScreenDeltaToVectorXZ(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraScreenDeltaToVectorXZ(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, 
    Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraScreenDeltaToVectorYZ(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraScreenDeltaToVectorYZ(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, 
    Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraAbsoluteEyeSpaceVector(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := CameraAbsoluteEyeSpaceVector(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, 
    Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_CameraSetAutoLeveling(const Args: TLuaArgs): TLuaArg;
begin
  CameraSetAutoLeveling(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraMoveInEyeSpace(const Args: TLuaArgs): TLuaArg;
begin
  CameraMoveInEyeSpace(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraMoveTargetInEyeSpace(const Args: TLuaArgs): TLuaArg;
begin
  CameraMoveTargetInEyeSpace(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CameraPointInFront(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := CameraPointInFront(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(p);
end;

function lua_CameraGetFieldOfView(const Args: TLuaArgs): TLuaArg;
var
  f: double;
begin
  f := CameraGetFieldOfView(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(f);
end;


