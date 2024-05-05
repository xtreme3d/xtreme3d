function MaterialLibraryCreate: real; cdecl;
var
  mlib:TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary.Create(scene);
  result:=ObjToReal(mlib);
end;

function MaterialLibraryActivate(mlib: real): real; cdecl;
begin
  matlib:=TGLMaterialLibrary(RealToPtr(mlib));
  result:=1;
end;

function MaterialLibrarySetTexturePaths(mlb: real; path: PAnsiChar): real; cdecl;
var
  mlib:TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary(RealToPtr(mlb));
  mlib.TexturePaths:=String(AnsiString(path));
  result:=1;
end;

function MaterialLibraryClear(mlb: real): real; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary(RealToPtr(mlb));
  mlib.Materials.Clear;
  result:=1;
end;

function MaterialLibraryDeleteUnused(mlb: real): real; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary(RealToPtr(mlb));
  mlib.Materials.DeleteUnusedMaterials;
  result:=1;
end;

function MaterialLibraryHasMaterial(matlib: real; name: PAnsiChar): real; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib := TGLMaterialLibrary(RealToPtr(matlib));
  if mlib.LibMaterialByName(StrConv(name)) = nil then
      Result := 0
  else
      Result := 1;
end;

function MaterialLibraryLoadScript(matlib: real; filename: PAnsiChar): real; cdecl;
var
  mlib: TGLMaterialLibrary;
  list: TStringList;
  scripter: TGLMaterialScripter;
begin
  mlib := TGLMaterialLibrary(RealToPtr(matlib));
  list := TStringList.Create;
  list.Text := LoadStringFromFile(StrConv(filename));
  scripter := TGLMaterialScripter.Create(scene);
  scripter.MaterialLibrary := mlib;
  scripter.Script := list;
  scripter.CompileScript;
  scripter.Free;
  list.Free;
  Result := 1;
end;

function MaterialLibraryGetTextureByName(matlib: real; name: PAnsiChar): real; cdecl;
var
  mlib: TGLMaterialLibrary;
  tex: TGLTexture;
begin
  mlib := TGLMaterialLibrary(RealToPtr(matlib));
  tex := mlib.TextureByName(StrConv(name));
  if tex = nil then Result := 0
  else Result := PtrToReal(tex);
end;

function MaterialCreate(mtrl, fname: PAnsiChar): real; cdecl;
begin
  try
    matlib.AddTextureMaterial(StrConv(mtrl), StrConv(fname), true);
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('MaterialCreate:' + #13#10 + E.Message);
    end;
  end;
  result:=1;
end;

function MaterialDestroy(mtrl: PAnsiChar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Free;
  result := 1;
end;

function MaterialAddCubeMap(mtrl: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat := matlib.AddTextureMaterial(StrConv(mtrl), '', true);
   mat.Material.Texture.ImageClassName:=TGLCubeMapImage.ClassName;
   mat.Material.Texture.MappingMode := tmmCubeMapReflection;
   mat.Material.Texture.MinFilter:=miLinear;
   mat.Material.Texture.TextureWrap:=twNone;
   result:=1;
end;

function MaterialCubeMapLoadImage(mtrl, filename: PAnsiChar; ind: real): real; cdecl;
var
  mat:TGLLibMaterial;
  target:TGLCubeMapTarget;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  with mat.Material.Texture.Image as TGLCubeMapImage do begin
    if ind=0 then target:=cmtPX
    else if ind=1 then target:=cmtNX
    else if ind=2 then target:=cmtPY
    else if ind=3 then target:=cmtNY
    else if ind=4 then target:=cmtPZ
    else if ind=5 then target:=cmtNZ
    else target := cmtPX;
    
    try
      Picture[target].LoadFromFile(StrConv(filename));
    except
      On E: Exception do
      begin
        if showLoadingErrors then
          ShowMessage('MaterialCubeMapLoadImage:' + #13#10 + E.Message);
      end;
    end;
    
  end;
  result:=1;
end;

function MaterialCubeMapGenerate(mtrl: PAnsiChar; res: real): real; cdecl;
var
  mat:TGLLibMaterial;
  bmp:TBitmap;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.texture.ImageClassName := TGLCubeMapImage.ClassName;
  with mat.Material.Texture.Image as TGLCubeMapImage do begin
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf32bit;
    bmp.HandleType := bmDIB;
    bmp.Height := Trunc(res);
    bmp.Width := Trunc(res);
    Picture[cmtPX].Assign(bmp);
    Picture[cmtNX].Assign(bmp);
    Picture[cmtPY].Assign(bmp);
    Picture[cmtNY].Assign(bmp);
    Picture[cmtPZ].Assign(bmp);
    Picture[cmtNZ].Assign(bmp);
  end;
  result:=1;
end;

function MaterialCubeMapFromScene(mtrl: PAnsiChar; viewer, camera, res: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  memviewer.Camera := TGLCamera(RealToPtr(camera));
  memviewer.Width := Trunc(res);
  memviewer.Height := Trunc(res);
  memviewer.Buffer.BackgroundColor :=
    TGLSceneViewer(RealToPtr(viewer)).Buffer.BackgroundColor;
  memviewer.RenderCubeMapTextures(mat.Material.Texture);
  Result := 1;
end;

function MaterialSetName(mtrl, name: PAnsiChar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Name := StrConv(name);
  result := 1;
end;

function MaterialSetShininess(mtrl: PAnsiChar; shin: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.FrontProperties.Shininess:=Trunc(shin);
  result:=1;
end;

function MaterialSetAmbientColor(mtrl: PAnsiChar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.FrontProperties.Ambient.AsWinColor:=TColor(Trunc(col));
  mat.Material.FrontProperties.Ambient.Alpha := alpha;
  result:=1;
end;

function MaterialSetDiffuseColor(mtrl: PAnsiChar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.FrontProperties.Diffuse.AsWinColor:=TColor(Trunc(col));
  mat.Material.FrontProperties.Diffuse.Alpha := alpha;
  result:=1;
end;

function MaterialSetSpecularColor(mtrl: PAnsiChar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.FrontProperties.Specular.AsWinColor:=TColor(Trunc(col));
  mat.Material.FrontProperties.Specular.Alpha := alpha;
  result:=1;
end;

function MaterialSetEmissionColor(mtrl: PAnsiChar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.FrontProperties.Emission.AsWinColor:=TColor(Trunc(col));
  mat.Material.FrontProperties.Emission.Alpha := alpha;
  result:=1;
end;

function MaterialGetColor(mtrl: PAnsiChar; index: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if (index=0) then result:=mat.Material.FrontProperties.Ambient.AsWinColor
  else if (index=1) then result:=mat.Material.FrontProperties.Diffuse.AsWinColor
  else if (index=2) then result:=mat.Material.FrontProperties.Specular.AsWinColor
  else if (index=3) then result:=mat.Material.FrontProperties.Emission.AsWinColor
  else result := 0;
end;

function MaterialGetAlpha(mtrl: PAnsiChar; index: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if (index=0) then result:=mat.Material.FrontProperties.Ambient.Alpha
  else if (index=1) then result:=mat.Material.FrontProperties.Diffuse.Alpha
  else if (index=2) then result:=mat.Material.FrontProperties.Specular.Alpha
  else if (index=3) then result:=mat.Material.FrontProperties.Emission.Alpha
  else result := 0;
end;

function MaterialSetBlendingMode(mtrl: PAnsiChar; bm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if bm=0 then mat.Material.BlendingMode:=bmOpaque
  else if bm=1 then mat.Material.BlendingMode:=bmTransparency
  else if bm=2 then mat.Material.BlendingMode:=bmAdditive
  else if bm=3 then mat.Material.BlendingMode:=bmAlphaTest50
  else if bm=4 then mat.Material.BlendingMode:=bmAlphaTest100
  else if bm=5 then mat.Material.BlendingMode:=bmModulate;
  result:=1;
end;

function MaterialSetTextureMode(mtrl: PAnsiChar; tm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tm=0 then mat.Material.Texture.TextureMode:=tmDecal
  else if tm=1 then mat.Material.Texture.TextureMode:=tmModulate
  else if tm=2 then mat.Material.Texture.TextureMode:=tmBlend
  else if tm=3 then mat.Material.Texture.TextureMode:=tmReplace;
  result:=1;
end;

function MaterialSetTextureMappingMode(mtrl: PAnsiChar; tmm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tmm=0 then mat.Material.Texture.MappingMode:=tmmUser
  else if tmm=1 then mat.Material.Texture.MappingMode:=tmmObjectLinear
  else if tmm=2 then mat.Material.Texture.MappingMode:=tmmEyeLinear
  else if tmm=3 then mat.Material.Texture.MappingMode:=tmmSphere
  else if tmm=4 then mat.Material.Texture.MappingMode:=tmmCubeMapReflection
  else if tmm=5 then mat.Material.Texture.MappingMode:=tmmCubeMapNormal
  else if tmm=6 then mat.Material.Texture.MappingMode:=tmmCubeMapLight0
  else if tmm=7 then mat.Material.Texture.MappingMode:=tmmCubeMapCamera;
  result:=1;
end;

function MaterialSetPolygonMode(mtrl: PAnsiChar; pm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if pm=0 then mat.Material.PolygonMode := pmFill
  else if pm=1 then mat.Material.PolygonMode := pmLines
  else if pm=2 then mat.Material.PolygonMode := pmPoints;
  result:=1;
end;

function MaterialSetTextureImageAlpha(mtrl: PAnsiChar; tia: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tia=0 then mat.Material.Texture.ImageAlpha := tiaDefault
  else if tia=1 then mat.Material.Texture.ImageAlpha := tiaAlphaFromIntensity
  else if tia=2 then mat.Material.Texture.ImageAlpha := tiaSuperBlackTransparent
  else if tia=3 then mat.Material.Texture.ImageAlpha := tiaLuminance
  else if tia=4 then mat.Material.Texture.ImageAlpha := tiaLuminanceSqrt
  else if tia=5 then mat.Material.Texture.ImageAlpha := tiaOpaque
  else if tia=6 then mat.Material.Texture.ImageAlpha := tiaTopLeftPointColorTransparent
  else if tia=7 then mat.Material.Texture.ImageAlpha := tiaInverseLuminance
  else if tia=8 then mat.Material.Texture.ImageAlpha := tiaInverseLuminanceSqrt
  else if tia=9 then mat.Material.Texture.ImageAlpha := tiaBottomRightPointColorTransparent;
  result:=1;
end;

function MaterialSetTextureScale(mtrl: PAnsiChar; u, v: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.TextureScale.X := u;
  mat.TextureScale.Y := v;
  result:=1;
end;

function MaterialSetTextureOffset(mtrl: PAnsiChar; u, v: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.TextureOffset.X := u;
  mat.TextureOffset.Y := v;
  result:=1;
end;

function MaterialSetTextureFilter(mtrl: PAnsiChar; mi, ma: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if mi=0 then mat.Material.Texture.MinFilter := miNearest
  else if mi=1 then mat.Material.Texture.MinFilter := miLinear
  else if mi=2 then mat.Material.Texture.MinFilter := miNearestMipmapNearest
  else if mi=3 then mat.Material.Texture.MinFilter := miLinearMipmapNearest
  else if mi=4 then mat.Material.Texture.MinFilter := miNearestMipmapLinear
  else if mi=5 then mat.Material.Texture.MinFilter := miLinearMipmapLinear
  else if ma=0 then mat.Material.Texture.MagFilter := maNearest
  else if ma=1 then mat.Material.Texture.MagFilter := maLinear;
  result:=1;
end;

function MaterialEnableTexture(mtrl: PAnsiChar; mode: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if mode=0 then mat.Material.Texture.Enabled := false
  else mat.Material.Texture.Enabled := true;
  result:=1;
end;

function MaterialGetCount(): real; cdecl;
begin
  result:=matlib.Materials.Count;
end;

function MaterialGetName(ind: real): PAnsiChar; cdecl;
begin
  result:=PAnsiChar(AnsiString(matlib.Materials.Items[Trunc(ind)].Name));
end;

function MaterialSetFaceCulling(mtrl: PAnsiChar; fc: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if fc=0 then mat.Material.FaceCulling := fcBufferDefault
  else if fc=1 then mat.Material.FaceCulling := fcCull
  else if fc=2 then mat.Material.FaceCulling := fcNoCull;
  result:=1;
end;

function MaterialSetSecondTexture(mtrl, mtrl2: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Texture2Name:=StrConv(mtrl2);
  result:=1;
end;

function MaterialSetTextureFormat(mtrl: PAnsiChar; tf: real): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tf=0 then mat.Material.Texture.TextureFormat := tfDefault
  else if tf=1 then mat.Material.Texture.TextureFormat := tfRGB
  else if tf=2 then mat.Material.Texture.TextureFormat := tfRGBA
  else if tf=3 then mat.Material.Texture.TextureFormat := tfRGB16
  else if tf=4 then mat.Material.Texture.TextureFormat := tfRGBA16
  else if tf=5 then mat.Material.Texture.TextureFormat := tfAlpha
  else if tf=6 then mat.Material.Texture.TextureFormat := tfLuminance
  else if tf=7 then mat.Material.Texture.TextureFormat := tfLuminanceAlpha
  else if tf=8 then mat.Material.Texture.TextureFormat := tfIntensity
  else if tf=9 then mat.Material.Texture.TextureFormat := tfNormalMap
  else if tf=10 then mat.Material.Texture.TextureFormat := tfRGBAFloat16
  else if tf=11 then mat.Material.Texture.TextureFormat := tfRGBAFloat32
  else if tf=12 then mat.Material.Texture.TextureFormat := tfExtended;
  result:=1;
end;

function MaterialSetTextureFormatEx(mtrl: PAnsiChar; tfex: real): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.TextureFormatEx := TGLInternalFormat(Ord(Trunc(tfex)));
end;

function MaterialSetTextureCompression(mtrl: PAnsiChar; tc: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tc=0 then mat.Material.Texture.Compression := tcDefault
  else if tc=1 then mat.Material.Texture.Compression := tcNone
  else if tc=2 then mat.Material.Texture.Compression := tcStandard
  else if tc=3 then mat.Material.Texture.Compression := tcHighQuality
  else if tc=4 then mat.Material.Texture.Compression := tcHighSpeed;
  result:=1;
end;

function MaterialTextureRequiredMemory(mtrl: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  result:=mat.Material.Texture.TextureImageRequiredMemory;
end;

function MaterialSetFilteringQuality(mtrl: PAnsiChar; tf: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tf=0 then mat.Material.Texture.FilteringQuality := tfIsotropic
  else if tf=1 then mat.Material.Texture.FilteringQuality := tfAnisotropic;
  result:=1;
end;

function MaterialSetShader(mtrl: PAnsiChar; shd: real): real; cdecl;
var
  mat:TGLLibMaterial;
  shad: TGLShader;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if shd > 0 then
  begin
    shad := TGLShader(RealToPtr(shd));
    mat.Shader := shad;
  end
  else
    mat.Shader := nil;
  result := 1;
end;

function MaterialSaveTexture(mtrl, fname: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.Image.SaveToFile(StrConv(fname));
  result:=1;
end;

function MaterialSetOptions(mtrl: PAnsiChar; op1,op2: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if (op1=1) and (op2=1) then mat.Material.MaterialOptions:=[moIgnoreFog,moNoLighting]
  else if (op1=1) and (op2=0) then mat.Material.MaterialOptions:=[moIgnoreFog]
  else if (op1=0) and (op2=1) then mat.Material.MaterialOptions:=[moNoLighting]
  else if (op1=0) and (op2=0) then mat.Material.MaterialOptions:=[];
  result:=1;
end;

function MaterialSetTextureWrap(mtrl: PAnsiChar; wrap: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.TextureWrap := TGLTextureWrap(Trunc(wrap));
  result:=1;
end;

function MaterialSetTextureWrapS(mtrl: PAnsiChar; wrap: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.TextureWrapS := TGLSeparateTextureWrap(Trunc(wrap));
  result:=1;
end;

function MaterialSetTextureWrapT(mtrl: PAnsiChar; wrap: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.TextureWrapT := TGLSeparateTextureWrap(Trunc(wrap));
  result:=1;
end;

function MaterialSetTextureWrapR(mtrl: PAnsiChar; wrap: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.TextureWrapR := TGLSeparateTextureWrap(Trunc(wrap));
  result:=1;
end;

function MaterialGenTexture(mtrl: PAnsiChar; w, h: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.Texture.ImageClassName := TGLBlankImage.ClassName;
  with mat.Material.Texture.Image as TGLBlankImage do begin
    Width := Trunc(w);
    Height := Trunc(h);
  end;
  result:=1;
end;

function MaterialSetTexture(mtrl, mtrl2: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
  mat2:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat2:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl2));
  mat.Material.Texture := mat2.Material.Texture;
  result:=1;
end;

function MaterialGetTextureWidth(mtrl: PAnsiChar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  result := 0;
  if mat.Material.Texture <> nil then
    result := mat.Material.Texture.Image.Width;
end;

function MaterialGetTextureHeight(mtrl: PAnsiChar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  result := 0;
  if mat.Material.Texture <> nil then
    result := mat.Material.Texture.Image.Height;
end;

function MaterialLoadTexture(mtrl, filename: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  
  try
    mat.Material.Texture.Image.LoadFromFile(StrConv(filename));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('MaterialLoadTexture:' + #13#10 + E.Message);
    end;
  end;
  
  result:=1;
end;

// Redesigned TextureEx system
function MaterialAddTextureEx(mtrl: PAnsiChar; index: real): real; cdecl;
var
  mat: TGLLibMaterial;
  item: TGLTextureExItem;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  item := mat.Material.TextureEx.Add();
  item.TextureIndex := Trunc(index);
  result := ObjToReal(item);
end;

function MaterialTextureExClear(mtrl: PAnsiChar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.TextureEx.Clear;
  result := 1;
end;

function MaterialHasTextureEx(mtrl: PAnsiChar; index: real): real; cdecl;
var
  mat: TGLLibMaterial;
  tex: TGLTexture;
  i: Integer;
begin
  mat := matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  result := 0;
  for i := 0 to mat.Material.TextureEx.Count - 1 do
    if mat.Material.TextureEx.Items[i].TextureIndex = Trunc(index) then
       result := 1;
end;

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

function MaterialNoiseCreate(mtrl: PAnsiChar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.Add();
  mat.Name:=StrConv(mtrl);
  mat.Material.Texture.Disabled:=false;
  mat.Material.Texture.Image:=TGLProcTextureNoise.Create(nil);
  result:=1;
end;

function MaterialNoiseSetDimensions(mtrl: PAnsiChar; w, h: real): real; cdecl;
begin
  with TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(StrConv(mtrl)).Material.Texture.Image) do
  begin
    Width:=Trunc(w);
    Height:=Trunc(h);
  end;
  result:=1;
end;

function MaterialNoiseAnimate(mtrl: PAnsiChar; speed: real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(StrConv(mtrl)).Material.Texture.Image).NoiseAnimate(speed);
  result:=1;
end;

function MaterialNoiseSetMinCut(mtrl: PAnsiChar; m: real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(StrConv(mtrl)).Material.Texture.Image).MinCut:=Trunc(m);
  result:=1;
end;

function MaterialNoiseSetSharpness(mtrl: PAnsiChar; s: real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(StrConv(mtrl)).Material.Texture.Image).NoiseSharpness:=s;
  result:=1;
end;

function MaterialNoiseSetSeamless(mtrl: PAnsiChar; mode: real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(StrConv(mtrl)).Material.Texture.Image).Seamless:=boolean(Trunc(mode));
  result:=1;
end;

function MaterialNoiseRandomSeed(mtrl: PAnsiChar; s: real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(StrConv(mtrl)).Material.Texture.Image).NoiseRandSeed:=Trunc(s);
  result:=1;
end;

{
function MaterialCullFrontFaces(mtrl: PAnsiChar; culff: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.CullFrontFaces := Boolean(Trunc(culff));
  result:=1;
end;
}

function MaterialSetDepthWrite(mtrl: PAnsiChar; mode: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.DepthProperties.DepthWrite := Boolean(Trunc(mode));
  result:=1;
end;

function MaterialSetDepthTest(mtrl: PAnsiChar; mode: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  mat.Material.DepthProperties.DepthTest := Boolean(Trunc(mode));
  result:=1;
end;

{
function MaterialSetTextureExFromLibrary(material1: pchar; matlib2: real; material2: pchar; index: real): real; cdecl;
var
  mat1, mat2: TGLLibMaterial;
  mlib: TGLMaterialLibrary;
begin
  mat1 := matlib.Materials.GetLibMaterialByName(String(material1));
  mlib := TGLMaterialLibrary(RealToPtr(matlib2));
  mat2 := mlib.Materials.GetLibMaterialByName(String(material2));
  mat1.Material.SetTextureN(RealToPtr(index), mat2.Material.Texture);
  result := 1.0;
end;
}

function MaterialGetNameFromLibrary(matlib, index: real): PAnsiChar; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib := TGLMaterialLibrary(RealToPtr(matlib));
  result := PAnsiChar(AnsiString(mlib.Materials.Items[Trunc(index)].Name));
end;

function MaterialSetTextureCompareMode(mtrl: PAnsiChar; tcm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(StrConv(mtrl));
  if tcm = 0 then
    mat.Material.Texture.TextureCompareMode := tcmNone
  else if tcm = 1 then
    mat.Material.Texture.TextureCompareMode := tcmCompareRtoTexture;
  result:=1;
end;


