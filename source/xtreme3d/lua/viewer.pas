// Viewer
function lua_ViewerCreate(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := ViewerCreate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_ViewerSetCamera(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetCamera(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerEnableVSync(const Args: TLuaArgs): TLuaArg;
begin
  ViewerEnableVSync(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerRender(const Args: TLuaArgs): TLuaArg;
begin
  ViewerRender(Args[0].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerRenderToFile(const Args: TLuaArgs): TLuaArg;
begin
  ViewerRenderToFile(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(1.0);
end;

function lua_ViewerRenderEx(const Args: TLuaArgs): TLuaArg;
begin
  ViewerRenderEx(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerResize(const Args: TLuaArgs): TLuaArg;
begin
  ViewerResize(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerSetVisible(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetVisible(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerGetPixelColor(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ViewerGetPixelColor(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ViewerGetPixelDepth(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ViewerGetPixelDepth(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ViewerSetLighting(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetLighting(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerSetBackgroundColor(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetBackgroundColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerSetAmbientColor(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetAmbientColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerEnableFog(const Args: TLuaArgs): TLuaArg;
begin
  ViewerEnableFog(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerSetFogColor(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetFogColor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerSetFogDistance(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetFogDistance(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerScreenToWorld(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := ViewerScreenToWorld(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(v);
end;

function lua_ViewerWorldToScreen(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := ViewerWorldToScreen(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_ViewerCopyToTexture(const Args: TLuaArgs): TLuaArg;
begin
  ViewerCopyToTexture(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(1.0);
end;

function lua_ViewerGetFramesPerSecond(const Args: TLuaArgs): TLuaArg;
var
  f: double;
begin
  f := ViewerGetFramesPerSecond(Args[0].AsDouble);
  result := LuaArg(f);
end;

function lua_ViewerGetPickedObject(const Args: TLuaArgs): TLuaArg;
var
  o: double;
begin
  o := ViewerGetPickedObject(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(o);
end;

function lua_ViewerGetPickedObjectsList(const Args: TLuaArgs): TLuaArg;
var
  o: double;
begin
  o := ViewerGetPickedObjectsList(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, 
    Args[5].AsDouble, Args[6].AsDouble);
  result := LuaArg(o);
end;

function lua_ViewerScreenToVector(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := ViewerScreenToVector(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(v);
end;

function lua_ViewerVectorToScreen(const Args: TLuaArgs): TLuaArg;
var
  v: double;
begin
  v := ViewerVectorToScreen(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(v);
end;

function lua_ViewerPixelToDistance(const Args: TLuaArgs): TLuaArg;
var
  d: double;
begin
  d := ViewerPixelToDistance(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(d);
end;

function lua_ViewerSetAntiAliasing(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetAntiAliasing(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerGetGLSLSupported(const Args: TLuaArgs): TLuaArg;
var
  s: double;
begin
  s := ViewerGetGLSLSupported(Args[0].AsDouble);
  result := LuaArg(s);
end;

function lua_ViewerGetFBOSupported(const Args: TLuaArgs): TLuaArg;
var
  s: double;
begin
  s := ViewerGetFBOSupported(Args[0].AsDouble);
  result := LuaArg(s);
end;

function lua_ViewerGetVBOSupported(const Args: TLuaArgs): TLuaArg;
var
  s: double;
begin
  s := ViewerGetVBOSupported(Args[0].AsDouble);
  result := LuaArg(s);
end;

function lua_ViewerSetAutoRender(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetAutoRender(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(1.0);
end;

function lua_ViewerSetOverrideMaterial(const Args: TLuaArgs): TLuaArg;
begin
  ViewerSetOverrideMaterial(Args[0].AsDouble, Args[1].AsDouble, pchar(Args[2].AsString));
  result := LuaArg(1.0);
end;

function lua_ViewerGetSize(const Args: TLuaArgs): TLuaArg;
var
  s: double;
begin
  s := ViewerGetSize(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(s);
end;

function lua_ViewerGetPosition(const Args: TLuaArgs): TLuaArg;
var
  p: double;
begin
  p := ViewerGetPosition(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(p);
end;

function lua_ViewerIsOpenGLExtensionSupported(const Args: TLuaArgs): TLuaArg;
var
  s: double;
begin
  s := ViewerIsOpenGLExtensionSupported(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(s);
end;
