// HUDShape
function lua_HUDShapeRectangleCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDShapeRectangleCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDShapeCircleCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDShapeCircleCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                            Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDShapeLineCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDShapeLineCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                          Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDShapeMeshCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDShapeMeshCreate(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDShapeSetSize(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeSetSize(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeScale(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeScale(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeSetRotation(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeSetRotation(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeRotate(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeRotate(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeSetColor(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeSetColor(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeSetOrigin(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeSetOrigin(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeCircleSetRadius(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeCircleSetRadius(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeCircleSetSlices(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeCircleSetSlices(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeCircleSetAngles(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeCircleSetAngles(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeLineSetPoints(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeLineSetPoints(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                        Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeLineSetWidth(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeLineSetWidth(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeMeshAddVertex(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeMeshAddVertex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                        Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeMeshAddTriangle(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeMeshAddTriangle(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeMeshSetVertex(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeMeshSetVertex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_HUDShapeMeshSetTexCoord(const Args: TLuaArgs): TLuaArg;
begin
  HUDShapeMeshSetTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;
