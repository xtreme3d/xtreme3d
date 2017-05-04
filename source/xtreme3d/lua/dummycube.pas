// Dummycube
function lua_DummycubeCreate(const Args: TLuaArgs): TLuaArg;
var
  d: double;
begin
  d := DummycubeCreate(Args[0].AsDouble);
  result := LuaArg(d);
end;

function lua_DummycubeAmalgamate(const Args: TLuaArgs): TLuaArg;
begin
  DummycubeAmalgamate(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_DummycubeSetCameraMode(const Args: TLuaArgs): TLuaArg;
begin
  DummycubeSetCameraMode(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_DummycubeSetVisible(const Args: TLuaArgs): TLuaArg;
begin
  DummycubeSetVisible(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_DummycubeSetEdgeColor(const Args: TLuaArgs): TLuaArg;
begin
  DummycubeSetEdgeColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_DummycubeSetCubeSize(const Args: TLuaArgs): TLuaArg;
begin
  DummycubeSetCubeSize(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;
