function HUDSpriteCreate(mtrl: PAnsiChar; w,h,parent: real): real; cdecl;
var
  GLHUDSprite1: TGLHUDSprite;
begin
  if not (parent=0) then
    GLHUDSprite1:=TGLHUDSprite.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLHUDSprite1:=TGLHUDSprite.CreateAsChild(scene.Objects);
  GLHUDSprite1.SetSize(w, h);
  GLHUDSprite1.Material.MaterialLibrary:=matlib;
  GLHUDSprite1.Material.LibMaterialName:=StrConv(mtrl);
  result:=ObjToReal(GLHUDSprite1);
end;

function HUDSpriteGetMouseOver(sprite, viewer: real): real; cdecl;
var
  spr: TGLHUDSprite;
  v: TGLSceneViewer;
  spriteX, spriteY: single;
  mouseInSprite: Boolean;
  mouse: TPoint;
begin
  spr := TGLHUDSprite(RealToPtr(sprite));
  v := TGLSceneViewer(RealToPtr(viewer));
  GetCursorPos(mouse);
  ScreenToClient(v.ParentWindow, mouse);
  spriteX := spr.AbsolutePosition.V[0] - spr.Width * 0.5;
  spriteY := spr.AbsolutePosition.V[1] - spr.Height * 0.5;
  mouseInSprite :=
    (mouse.X > spriteX) and
    (mouse.X < (spriteX + spr.Width)) and
    (mouse.Y > spriteY) and
    (mouse.Y < (spriteY + spr.Height));
  result := Integer(mouseInSprite);
end;

function HUDSpriteXTiles(sprite,xtls: real): real; cdecl;
var
 spr: TGLHUDSprite;
begin
  spr := TGLHUDSprite(RealToPtr(sprite));
  spr.XTiles:=Trunc(xtls);
  Result := 1;
end;

function HUDSpriteYTiles(sprite,ytls: real): real; cdecl;
var
 spr: TGLHUDSprite;
begin
  spr := TGLHUDSprite(RealToPtr(sprite));
  spr.YTiles:=Trunc(ytls);
  Result := 1;
end;

function SpriteCreate(mtrl: PAnsiChar; w,h,parent: real): real; cdecl;
var
  GLSprite1: TGLSprite;
begin
  if not (parent=0) then
    GLSprite1:=TGLSprite.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLSprite1:=TGLSprite.CreateAsChild(scene.Objects);
  GLSprite1.SetSize(w, h);
  GLSprite1.Material.MaterialLibrary:=matlib;
  GLSprite1.Material.LibMaterialName:=StrConv(mtrl);
  result:=ObjToReal(GLSprite1);
end;

function SpriteSetSize(sprite,w,h: real): real; cdecl;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(RealToPtr(sprite));
  GLSprite1.SetSize(w, h);
  result:=1;
end;

function SpriteGetSize(sprite, type_val: real): real; cdecl;
var
  spr: TGLSprite;
begin
  spr := TGLSprite(RealToPtr(sprite));
  result := 0.0;
  if (type_val = 0) then
    result := spr.Width;
  if (type_val = 1) then
    result := spr.Height;
end;

function SpriteScale(sprite,u,v: real): real; cdecl;
var
  GLSprite1: TGLSprite;
  w,h: real;
begin
  GLSprite1:=TGLSprite(RealToPtr(sprite));
  w:=GLSprite1.Width;
  h:=GLSprite1.Height;
  GLSprite1.SetSize(w+u,h+v);
  result:=1;
end;

function SpriteSetRotation(sprite,angle: real): real; cdecl;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(RealToPtr(sprite));
  GLSprite1.Rotation:=angle;
  result:=1;
end;

function SpriteRotate(sprite,angle: real): real; cdecl;
var
  GLSprite1: TGLSprite;
  rot: real;
begin
  GLSprite1:=TGLSprite(RealToPtr(sprite));
  rot:=GLSprite1.Rotation;
  GLSprite1.Rotation:=rot+angle;
  result:=1;
end;

function SpriteMirror(sprite,u,v: real): real; cdecl;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(RealToPtr(sprite));
  GLSprite1.MirrorU:=boolean(Trunc(u));
  GLSprite1.MirrorV:=boolean(Trunc(v));
  result:=1;
end;

function SpriteCreateEx(w, h, left, top, right, bottom, parent: real): real; cdecl;
var
  spr: TGLSprite;
begin
  if not (parent=0) then
    spr:=TGLSprite.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    spr:=TGLSprite.CreateAsChild(scene.Objects);
  spr.SetSize(w, h);
  spr.UVLeft := left;
  spr.UVTop := 1.0 - bottom;
  spr.UVRight := right;
  spr.UVBottom := 1.0 - top;
  result := ObjToReal(spr);
end;

function HUDSpriteCreateEx(w, h, left, top, right, bottom, parent: real): real; cdecl;
var
  spr: TGLHUDSprite;
begin
  if not (parent=0) then
    spr:=TGLHUDSprite.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    spr:=TGLHUDSprite.CreateAsChild(scene.Objects);
  spr.SetSize(w, h);
  spr.UVLeft := left;
  spr.UVTop := 1.0 - bottom;
  spr.UVRight := right;
  spr.UVBottom := 1.0 - top;
  result:= ObjToReal(spr);
end;

function SpriteSetBounds(sprite, left, top, right, bottom: real): real; cdecl;
var
  spr: TGLSprite;
  tw, th: Single;
  mat: TGLLibMaterial;
begin
  spr := TGLSprite(RealToPtr(sprite));
  mat := TGLMaterialLibrary(spr.Material.MaterialLibrary).Materials.GetLibMaterialByName(spr.Material.LibMaterialName);
  if mat <> nil then
  begin
    if mat.Material.Texture <> nil then
    begin
      if mat.Material.Texture.Image.Width > 0 then
      begin
        tw := mat.Material.Texture.Image.Width;
        th := mat.Material.Texture.Image.Height;
        spr.UVLeft := left / tw;
        spr.UVTop := (th - bottom) / th;
        spr.UVRight := right / tw;
        spr.UVBottom := (th - top) / th;
      end;
    end;
  end;
  result := 1;
end;

function SpriteSetBoundsUV(sprite, left, top, right, bottom: real): real; cdecl;
var
  spr: TGLSprite;
begin
  spr := TGLSprite(RealToPtr(sprite));
  spr.UVLeft := left;
  spr.UVTop := 1.0 - bottom;
  spr.UVRight := right;
  spr.UVBottom := 1.0 - top;
  result := 1;
end;

function SpriteSetOrigin(sprite, x, y: real): real; cdecl;
var
  spr: TGLSprite;
begin
  spr := TGLSprite(RealToPtr(sprite));
  spr.OriginX := x;
  spr.OriginY := y;
  result := 1;
end;

