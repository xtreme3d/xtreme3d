// Primitives
function lua_PlaneCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := PlaneCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                   Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_CubeCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := CubeCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_CubeSetNormalDirection(const Args: TLuaArgs): TLuaArg;
begin
  CubeSetNormalDirection(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_SphereCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := SphereCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_SphereSetAngleLimits(const Args: TLuaArgs): TLuaArg;
begin
  SphereSetAngleLimits(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                       Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(1.0);
end;

function lua_CylinderCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := CylinderCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                      Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble);
  result := LuaArg(c);
end;

function lua_ConeCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ConeCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                  Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_AnnulusCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := AnnulusCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                     Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble);
  result := LuaArg(c);
end;

function lua_TorusCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := TorusCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                   Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_DiskCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := DiskCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                  Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble, Args[6].AsDouble);
  result := LuaArg(c);
end;

function lua_FrustrumCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FrustrumCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble,
                      Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_DodecahedronCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := DodecahedronCreate(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_IcosahedronCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := IcosahedronCreate(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_TeapotCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := TeapotCreate(Args[0].AsDouble);
  result := LuaArg(c);
end;
