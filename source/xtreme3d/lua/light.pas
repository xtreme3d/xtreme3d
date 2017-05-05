// Light
function lua_LightCreate(const Args: TLuaArgs): TLuaArg;
var
  l: double;
begin
  l := LightCreate(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(l);
end;

function lua_LightSetAmbientColor(const Args: TLuaArgs): TLuaArg;
begin
  LightSetAmbientColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetDiffuseColor(const Args: TLuaArgs): TLuaArg;
begin
  LightSetDiffuseColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetSpecularColor(const Args: TLuaArgs): TLuaArg;
begin
  LightSetSpecularColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetAttenuation(const Args: TLuaArgs): TLuaArg;
begin
  LightSetAttenuation(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetShining(const Args: TLuaArgs): TLuaArg;
begin
  LightSetShining(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetSpotCutoff(const Args: TLuaArgs): TLuaArg;
begin
  LightSetSpotCutoff(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetSpotExponent(const Args: TLuaArgs): TLuaArg;
begin
  LightSetSpotExponent(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetSpotDirection(const Args: TLuaArgs): TLuaArg;
begin
  LightSetSpotDirection(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(1.0);
end;

function lua_LightSetStyle(const Args: TLuaArgs): TLuaArg;
begin
  LightSetStyle(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

