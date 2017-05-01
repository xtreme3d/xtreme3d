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

function lua_ObjectTranslate(const Args: TLuaArgs): TLuaArg;
begin
  ObjectTranslate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_KeyIsPressed(const Args: TLuaArgs): TLuaArg;
begin
  result := LuaArg(IsKeyDown(Args[0].AsInteger));
end;
