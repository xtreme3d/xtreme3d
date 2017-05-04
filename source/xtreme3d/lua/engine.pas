// Engine
function lua_EngineCreate(const Args: TLuaArgs): TLuaArg;
begin
  EngineCreate();
  result := LuaArg(1.0);
end;

function lua_EngineDestroy(const Args: TLuaArgs): TLuaArg;
begin
  EngineDestroy();
  result := LuaArg(1.0);
end;

function lua_EngineSetObjectsSorting(const Args: TLuaArgs): TLuaArg;
begin
  EngineSetObjectsSorting(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_EngineSetCulling(const Args: TLuaArgs): TLuaArg;
begin
  EngineSetCulling(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SetPakArchive(const Args: TLuaArgs): TLuaArg;
begin
  SetPakArchive(pchar(Args[0].AsString));
  result := LuaArg(1.0);
end;

function lua_Update(const Args: TLuaArgs): TLuaArg;
begin
  Update(Args[0].AsDouble);
  result := LuaArg(1.0);
end;
