// Input
function lua_KeyIsPressed(const Args: TLuaArgs): TLuaArg;
begin
  result := LuaArg(IsKeyDown(Args[0].AsInteger));
end;
