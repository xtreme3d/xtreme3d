// Font & Text
function lua_BmpfontCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := BmpfontCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble,
                     Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble, Args[7].AsDouble);
  result := LuaArg(c);
end;

function lua_BmpfontLoad(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := BmpfontLoad(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_TTFontCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := TTFontCreate(pchar(Args[0].AsString), Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_TTFontSetLineGap(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := TTFontSetLineGap(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_WindowsBitmapfontCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := WindowsBitmapfontCreate(pchar(Args[0].AsString), Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FlatTextCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FlatTextCreate(Args[0].AsDouble, pchar(Args[1].AsString), Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_FlatTextSetFont(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FlatTextSetFont(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FlatTextSetColor(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FlatTextSetColor(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_FlatTextSetText(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FlatTextSetText(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_HUDTextCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDTextCreate(Args[0].AsDouble, pchar(Args[1].AsString), Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDTextSetRotation(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDTextSetRotation(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDTextSetFont(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDTextSetFont(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDTextSetColor(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDTextSetColor(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_HUDTextSetText(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := HUDTextSetText(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_SpaceTextCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpaceTextCreate(Args[0].AsDouble, pchar(Args[1].AsString), Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_SpaceTextSetExtrusion(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpaceTextSetExtrusion(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_SpaceTextSetFont(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpaceTextSetFont(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_SpaceTextSetText(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SpaceTextSetText(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;
