function ShadowplaneCreate(width, height, xtiles, ytiles, target, light, color, alpha, parent: real): real; cdecl;
var
  sp: TGLShadowPlane;
begin
  if not (parent=0) then
    sp := TGLShadowPlane.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    sp := TGLShadowPlane.CreateAsChild(scene.Objects);
  sp.Width := width;
  sp.Height := height;
  sp.XTiles := trunc(xtiles);
  sp.YTiles := trunc(ytiles);
  sp.ShadowingObject := TGLBaseSceneObject(RealToPtr(target));
  sp.ShadowedLight := TGLLightSource(RealToPtr(light));
  sp.ShadowColor.AsWinColor := TColor(trunc(color));
  sp.ShadowColor.Alpha := alpha;
  sp.ShadowOptions := [spoUseStencil, spoScissor];
  sp.Style := [psTileTexture];
  result := ObjToReal(sp);
end;

function ShadowplaneSetLight(shadowplane, light: real): real; cdecl;
var
  sp: TGLShadowPlane;
begin
  sp := TGLShadowPlane(RealToPtr(shadowplane));
  sp.ShadowedLight := TGLLightSource(RealToPtr(light));
  result := 1.0;
end;

function ShadowplaneSetObject(shadowplane, target: real): real; cdecl;
var
  sp: TGLShadowPlane;
begin
  sp := TGLShadowPlane(RealToPtr(shadowplane));
  sp.ShadowingObject := TGLBaseSceneObject(RealToPtr(target));
  result := 1.0;
end;

function ShadowplaneSetOptions(shadowplane, stencil, scissor, transparent, ignorez: real): real; cdecl;
var
  sp: TGLShadowPlane;
begin
  sp := TGLShadowPlane(RealToPtr(shadowplane));
  sp.ShadowOptions := [];
  if Boolean(Trunc(stencil)) = true then
    sp.ShadowOptions := sp.ShadowOptions + [spoUseStencil];
  if Boolean(Trunc(scissor)) = true then
    sp.ShadowOptions := sp.ShadowOptions + [spoScissor];
  if Boolean(Trunc(transparent)) = true then
    sp.ShadowOptions := sp.ShadowOptions + [spoTransparent];
  if Boolean(Trunc(ignorez)) = true then
    sp.ShadowOptions := sp.ShadowOptions + [spoIgnoreZ];
  result := 1.0;
end;
