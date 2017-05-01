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

function lua_ObjectTranslate(const Args: TLuaArgs): TLuaArg;
begin
  ObjectTranslate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_KeyIsPressed(const Args: TLuaArgs): TLuaArg;
begin
  result := LuaArg(IsKeyDown(Args[0].AsInteger));
end;
