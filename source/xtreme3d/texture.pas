function TextureExLoad(textureItem: real; filename: PAnsiChar): real; cdecl;
var
  item: TGLTextureExItem;
  tex: TGLTexture;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  tex := TGLTexture.Create(scene);

  try
    tex.Image.LoadFromFile(StrConv(filename));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('MaterialLoadTextureEx:' + #13#10 + E.Message);
      result := 0;
      exit;
    end;
  end;

  tex.Disabled := False;
  item.Texture := tex;
  result := 1;
end;

function TextureExSetFromMaterial(textureItem: real; mtrl: PAnsiChar): real; cdecl;
var
  item: TGLTextureExItem;
  mat: TGLLibMaterial;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  item.Texture := mat.Material.Texture;
  result := 1;
end;

function TextureExGenerate(textureItem, w, h: real): real; cdecl;
var
  item: TGLTextureExItem;
  tex: TGLTexture;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  tex := TGLTexture.Create(scene);
  tex.ImageClassName := TGLBlankImage.ClassName;
  with tex.Image as TGLBlankImage do begin
    Width := Trunc(w);
    Height := Trunc(h);
  end;
  tex.Disabled := False;
  item.Texture := tex;
  result := 1;
end;

function TextureExDelete(textureItem: real): real; cdecl;
var
  item: TGLTextureExItem;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  item.Free;
  result := 1;
end;

function TextureExSetTextureScale(textureItem, x, y: real): real; cdecl;
var
  item: TGLTextureExItem;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  item.TextureScale.X := x;
  item.TextureScale.Y := y;
  result := 1;
end;

function TextureExSetTextureOffset(textureItem, x, y: real): real; cdecl;
var
  item: TGLTextureExItem;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  item.TextureOffset.X := x;
  item.TextureOffset.Y := y;
  result := 1;
end;

function TextureExEnable(textureItem, mode: real): real; cdecl;
var
  item: TGLTextureExItem;
begin
  item := TGLTextureExItem(RealToPtr(textureItem));
  item.Texture.Disabled := not Boolean(Trunc(mode));
  result := 1;
end;
