function ShadowplaneCreate(
  width, height,
  xtiles, ytiles,
  target, light, color, alpha,
  parent: real): real; stdcall;
var
  sp: TGLShadowPlane;
begin
  if not (parent=0) then
    sp := TGLShadowPlane.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    sp := TGLShadowPlane.CreateAsChild(scene.Objects);
  sp.Width := width;
  sp.Height := height;
  sp.XTiles := trunc64(xtiles);
  sp.YTiles := trunc64(ytiles);
  sp.ShadowingObject := TGLBaseSceneObject(trunc64(target));
  sp.ShadowedLight := TGLLightSource(trunc64(light));
  sp.ShadowColor.AsWinColor := TColor(trunc64(color));
  sp.ShadowColor.Alpha := alpha;
  sp.ShadowOptions := [spoUseStencil, spoScissor];
  sp.Style := [psTileTexture];
  result := Integer(sp);
end;

function ShadowplaneSetLight(shadowplane, light: real): real; stdcall;
var
  sp: TGLShadowPlane;
begin
  sp := TGLShadowPlane(trunc64(shadowplane));
  sp.ShadowedLight := TGLLightSource(trunc64(light));
  result := 1.0;
end;

function ShadowplaneSetObject(shadowplane, target: real): real; stdcall;
var
  sp: TGLShadowPlane;
begin
  sp := TGLShadowPlane(trunc64(shadowplane));
  sp.ShadowingObject := TGLBaseSceneObject(trunc64(target));
  result := 1.0;
end;

function ShadowplaneSetOptions(shadowplane,
  stencil, scissor, transparent, ignorez: real): real; stdcall;
var
  sp: TGLShadowPlane;
begin
  sp := TGLShadowPlane(trunc64(shadowplane));

       if (stencil = 1) and (scissor = 1) and (transparent = 1) then
      sp.ShadowOptions := [spoUseStencil, spoScissor, spoTransparent]
  else if (stencil = 1) and (scissor = 1) and (transparent = 0) then
      sp.ShadowOptions := [spoUseStencil, spoScissor]
  else if (stencil = 1) and (scissor = 0) and (transparent = 1) then
      sp.ShadowOptions := [spoUseStencil, spoTransparent]
  else if (stencil = 0) and (scissor = 1) and (transparent = 1) then
      sp.ShadowOptions := [spoScissor, spoTransparent]
  else if (stencil = 1) and (scissor = 0) and (transparent = 0) then
      sp.ShadowOptions := [spoUseStencil]
  else if (stencil = 0) and (scissor = 1) and (transparent = 0) then
      sp.ShadowOptions := [spoScissor]
  else if (stencil = 0) and (scissor = 0) and (transparent = 1) then
      sp.ShadowOptions := [spoTransparent]
  else if (stencil = 0) and (scissor = 0) and (transparent = 0) then
      sp.ShadowOptions := [];
      
  sp.NoZWrite := Boolean(trunc64(ignorez));
  result := 1.0;
end;
