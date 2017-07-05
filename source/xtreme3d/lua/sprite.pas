// Sprite
function lua_HUDSpriteCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDSpriteCreate(pchar(Args[0].AsString), Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDSpriteCreateEx(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDSpriteCreateEx(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble,
                         Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble);
  result := LuaArg(c);
end;

function lua_SpriteCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpriteCreate(pchar(Args[0].AsString), Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_SpriteCreateEx(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpriteCreateEx(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble,
                      Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble);
  result := LuaArg(c);
end;

function lua_SpriteSetSize(const Args: TLuaArgs): TLuaArg;
begin
  SpriteSetSize(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SpriteScale(const Args: TLuaArgs): TLuaArg;
begin
  SpriteScale(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SpriteSetRotation(const Args: TLuaArgs): TLuaArg;
begin
  SpriteSetRotation(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SpriteRotate(const Args: TLuaArgs): TLuaArg;
begin
  SpriteRotate(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SpriteMirror(const Args: TLuaArgs): TLuaArg;
begin
  SpriteMirror(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SpriteNoZWrite(const Args: TLuaArgs): TLuaArg;
begin
  SpriteNoZWrite(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SpriteSetBounds(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpriteSetBounds(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_SpriteSetBoundsUV(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpriteSetBoundsUV(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_SpriteSetOrigin(const Args: TLuaArgs): TLuaArg;
begin
  SpriteSetOrigin(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;
