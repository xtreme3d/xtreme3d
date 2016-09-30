function HUDSpriteCreate(mtrl: pchar; w,h,parent: real): real; stdcall;
var
  GLHUDSprite1: TGLHUDSprite;
begin
  if not (parent=0) then
    GLHUDSprite1:=TGLHUDSprite.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLHUDSprite1:=TGLHUDSprite.CreateAsChild(scene.Objects);
  GLHUDSprite1.SetSize(trunc64(w),trunc64(h));
  GLHUDSprite1.Material.MaterialLibrary:=matlib;
  GLHUDSprite1.Material.LibMaterialName:=mtrl;
  result:=Integer(GLHUDSprite1);
end;

function SpriteCreate(mtrl: pchar; w,h,parent: real): real; stdcall;
var
  GLSprite1: TGLSprite;
begin
  if not (parent=0) then
    GLSprite1:=TGLSprite.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLSprite1:=TGLSprite.CreateAsChild(scene.Objects);
  GLSprite1.SetSize(trunc64(w),trunc64(h));
  GLSprite1.Material.MaterialLibrary:=matlib;
  GLSprite1.Material.LibMaterialName:=mtrl;
  result:=Integer(GLSprite1);
end;

function SpriteSetSize(sprite,w,h: real): real; stdcall;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(trunc64(sprite));
  GLSprite1.SetSize(trunc64(w),trunc64(h));
  result:=1;
end;

function SpriteScale(sprite,u,v: real): real; stdcall;
var
  GLSprite1: TGLSprite;
  w,h: real;
begin
  GLSprite1:=TGLSprite(trunc64(sprite));
  w:=GLSprite1.Width;
  h:=GLSprite1.Height;
  GLSprite1.SetSize(w+u,h+v);
  result:=1;
end;

function SpriteSetRotation(sprite,angle: real): real; stdcall;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(trunc64(sprite));
  GLSprite1.Rotation:=angle;
  result:=1;
end;

function SpriteRotate(sprite,angle: real): real; stdcall;
var
  GLSprite1: TGLSprite;
  rot: real;
begin
  GLSprite1:=TGLSprite(trunc64(sprite));
  rot:=GLSprite1.Rotation;
  GLSprite1.Rotation:=rot+angle;
  result:=1;
end;

function SpriteMirror(sprite,u,v: real): real; stdcall;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(trunc64(sprite));
  GLSprite1.MirrorU:=boolean(trunc64(u));
  GLSprite1.MirrorV:=boolean(trunc64(v));
  result:=1;
end;

function SpriteNoZWrite(sprite,mode: real): real; stdcall;
var
  GLSprite1: TGLSprite;
begin
  GLSprite1:=TGLSprite(trunc64(sprite));
  GLSprite1.NoZWrite:=boolean(trunc64(mode));
  result:=1;
end;

function SpriteCreateEx(w, h, left, top, right, bottom, parent: real): real; stdcall;
var
  spr: TGLSprite;
begin
  if not (parent=0) then
    spr:=TGLSprite.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    spr:=TGLSprite.CreateAsChild(scene.Objects);
  spr.SetSize(trunc64(w),trunc64(h));
  //spr.Material.MaterialLibrary:=matlib;
  //spr.Material.LibMaterialName:=mtrl;
  spr.UVLeft := left;
  spr.UVTop := 1.0 - bottom;
  spr.UVRight := right;
  spr.UVBottom := 1.0 - top;
  result := Integer(spr);
end;

function HUDSpriteCreateEx(w, h, left, top, right, bottom, parent: real): real; stdcall;
var
  spr: TGLHUDSprite;
begin
  if not (parent=0) then
    spr:=TGLHUDSprite.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    spr:=TGLHUDSprite.CreateAsChild(scene.Objects);
  spr.SetSize(trunc64(w),trunc64(h));
  //spr.Material.MaterialLibrary:=matlib;
  //spr.Material.LibMaterialName:=mtrl;
  spr.UVLeft := left;
  spr.UVTop := 1.0 - bottom;
  spr.UVRight := right;
  spr.UVBottom := 1.0 - top;
  result:= Integer(spr);
end;

function SpriteSetBounds(sprite, left, top, right, bottom: real): real; stdcall;
var
  spr: TGLSprite;
  tw, th: Single;
  mat: TGLLibMaterial;
begin
  spr := TGLSprite(trunc64(sprite));
  mat:=spr.Material.MaterialLibrary.Materials.GetLibMaterialByName(spr.Material.LibMaterialName);
  if mat.Material.Texture <> nil then
  begin
    tw := mat.Material.Texture.Image.Width;
    th := mat.Material.Texture.Image.Height;
    spr.UVLeft := left / tw;
    spr.UVTop := (th - bottom) / th; 
    spr.UVRight := right / tw;
    spr.UVBottom := (th - top) / th;
  end;
  result := 1;
end;

function SpriteSetBoundsUV(sprite, left, top, right, bottom: real): real; stdcall;
var
  spr: TGLSprite;
begin
  spr := TGLSprite(trunc64(sprite));
  spr.UVLeft := left;
  spr.UVTop := 1.0 - bottom;
  spr.UVRight := right;
  spr.UVBottom := 1.0 - top;
  result := 1;
end;

function SpriteSetOrigin(sprite, x, y: real): real; stdcall;
var
  spr: TGLSprite;
begin
  spr := TGLSprite(trunc64(sprite));
  spr.OriginX := x;
  spr.OriginY := y;
  result := 1;
end;
