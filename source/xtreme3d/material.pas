function MaterialLibraryCreate: real; cdecl;
var
  mlib:TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary.Create(scene);
  result:=Integer(mlib);
end;

function MaterialLibraryActivate(mlib:real): real; cdecl;
begin
  matlib:=TGLMaterialLibrary(trunc64(mlib));
  result:=1;
end;

function MaterialLibrarySetTexturePaths(mlb: real; path: pchar): real; cdecl;
var
  mlib:TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary(trunc64(mlb));
  mlib.TexturePaths:=String(path);
  result:=1;
end;

function MaterialLibraryClear(mlb: real): real; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary(trunc64(mlb));
  mlib.Materials.Clear;
  result:=1;
end;

function MaterialLibraryDeleteUnused(mlb: real): real; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib:=TGLMaterialLibrary(trunc64(mlb));
  mlib.Materials.DeleteUnusedMaterials;
  result:=1;
end;

function MaterialLibraryHasMaterial(matlib: real; name: pchar): real; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib := TGLMaterialLibrary(trunc64(matlib));
  if mlib.LibMaterialByName(String(name)) = nil then
      Result := 0
  else
      Result := 1;
end;

function MaterialLibraryLoadScript(matlib: real; filename: pchar): real; cdecl;
var
  mlib: TGLMaterialLibrary;
  list: TStringList;
  scripter: TGLMaterialScripter;
begin
  mlib := TGLMaterialLibrary(trunc64(matlib));
  list := TStringList.Create;
  list.Text := LoadStringFromFile2(String(filename));
  scripter := TGLMaterialScripter.Create(scene);
  scripter.MaterialLibrary := mlib;
  scripter.Script := list;
  scripter.CompileScript;
  scripter.Free;
  list.Free;
  Result := 1;
end;

function MaterialCreate(mtrl,fname: pchar): real; cdecl;
begin  
  try
    matlib.AddTextureMaterial(String(mtrl),fname,true);
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('MaterialCreate:' + #13#10 + 'Error loading ' + String(fname) + #13#10 + E.Message);
    end;
  end;
  result:=1;
end;

function MaterialDestroy(mtrl: pchar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
  mat.Free;
  result := 1;
end;

function MaterialAddCubeMap(mtrl: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat := matlib.AddTextureMaterial(String(mtrl), '', true);
   mat.Material.Texture.ImageClassName:=TGLCubeMapImage.ClassName;
   mat.Material.Texture.MappingMode := tmmCubeMapReflection;
   mat.Material.Texture.MinFilter:=miLinear;
   mat.Material.Texture.TextureWrap:=twNone;
   result:=1;
end;

function MaterialCubeMapLoadImage(mtrl, texture: pchar; ind: real): real; cdecl;
var
  mat:TGLLibMaterial;
  target:TGLCubeMapTarget;
begin
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  with mat.Material.Texture.Image as TGLCubeMapImage do begin
    if ind=0 then target:=cmtPX;
    if ind=1 then target:=cmtNX;
    if ind=2 then target:=cmtPY;
    if ind=3 then target:=cmtNY;
    if ind=4 then target:=cmtPZ;
    if ind=5 then target:=cmtNZ;
    
    try
      Picture[target].LoadFromFile(String(texture));
    except
      On E: Exception do
      begin
        if showLoadingErrors then
          ShowMessage('MaterialCubeMapLoadImage:' + #13#10 + 'Error loading ' + String(texture) + #13#10 + E.Message);
      end;
    end;
    
  end;
  result:=1;
end;

function MaterialCubeMapGenerate(mtrl: pchar; res: real): real; cdecl;
var
  mat:TGLLibMaterial;
  bmp:TBitmap;
begin
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  mat.Material.texture.ImageClassName := TGLCubeMapImage.ClassName;
  with mat.Material.Texture.Image as TGLCubeMapImage do begin
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf32bit;
    bmp.HandleType := bmDIB;
    bmp.Height := trunc64(res);
    bmp.Width := trunc64(res);
    Picture[cmtPX].Assign(bmp);
    Picture[cmtNX].Assign(bmp);
    Picture[cmtPY].Assign(bmp);
    Picture[cmtNY].Assign(bmp);
    Picture[cmtPZ].Assign(bmp);
    Picture[cmtNZ].Assign(bmp);
  end;
  result:=1;
end;

function MaterialCubeMapFromScene(mtrl: pchar; viewer, camera, res: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  memviewer.Camera := TGLCamera(trunc64(camera));
  memviewer.Width := trunc64(res);
  memviewer.Height := trunc64(res);
  memviewer.Buffer.BackgroundColor :=
    TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor;
  memviewer.RenderCubeMapTextures(mat.Material.Texture);
  Result := 1;
end;

function MaterialSetName(mtrl, name: pchar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
  mat.Name := String(name);
  result := 1;
end;

function MaterialSetShininess(mtrl: pchar; shin: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.FrontProperties.Shininess:=trunc64(shin);
  result:=1;
end;

function MaterialSetAmbientColor(mtrl: pchar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.FrontProperties.Ambient.AsWinColor:=TColor(trunc64(col));
  mat.Material.FrontProperties.Ambient.Alpha := alpha;
  result:=1;
end;

function MaterialSetDiffuseColor(mtrl: pchar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.FrontProperties.Diffuse.AsWinColor:=TColor(trunc64(col));
  mat.Material.FrontProperties.Diffuse.Alpha := alpha;
  result:=1;
end;

function MaterialSetSpecularColor(mtrl: pchar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.FrontProperties.Specular.AsWinColor:=TColor(trunc64(col));
  mat.Material.FrontProperties.Specular.Alpha := alpha;
  result:=1;
end;

function MaterialSetEmissionColor(mtrl: pchar; col, alpha: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.FrontProperties.Emission.AsWinColor:=TColor(trunc64(col));
  mat.Material.FrontProperties.Emission.Alpha := alpha;
  result:=1;
end;

function MaterialGetColor(mtrl: pchar; index: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if (index=0) then
    result:=mat.Material.FrontProperties.Ambient.AsWinColor;
  if (index=1) then
    result:=mat.Material.FrontProperties.Diffuse.AsWinColor;
  if (index=2) then
    result:=mat.Material.FrontProperties.Specular.AsWinColor;
  if (index=3) then
    result:=mat.Material.FrontProperties.Emission.AsWinColor;	
end;

function MaterialGetAlpha(mtrl: pchar; index: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if (index=0) then
    result:=mat.Material.FrontProperties.Ambient.Alpha;
  if (index=1) then
    result:=mat.Material.FrontProperties.Diffuse.Alpha;
  if (index=2) then
    result:=mat.Material.FrontProperties.Specular.Alpha;
  if (index=3) then
    result:=mat.Material.FrontProperties.Emission.Alpha;	
end;

function MaterialSetBlendingMode(mtrl: pchar; bm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if bm=0 then mat.Material.BlendingMode:=bmOpaque;
  if bm=1 then mat.Material.BlendingMode:=bmTransparency;
  if bm=2 then mat.Material.BlendingMode:=bmAdditive;
  if bm=3 then mat.Material.BlendingMode:=bmAlphaTest50;
  if bm=4 then mat.Material.BlendingMode:=bmAlphaTest100;
  if bm=5 then mat.Material.BlendingMode:=bmModulate;
  result:=1;
end;

function MaterialSetTextureMode(mtrl: pchar; tm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if tm=0 then mat.Material.Texture.TextureMode:=tmDecal;
  if tm=1 then mat.Material.Texture.TextureMode:=tmModulate;
  if tm=2 then mat.Material.Texture.TextureMode:=tmBlend;
  if tm=3 then mat.Material.Texture.TextureMode:=tmReplace;
  result:=1;
end;

function MaterialSetTextureMappingMode(mtrl: pchar; tmm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if tmm=0 then mat.Material.Texture.MappingMode:=tmmUser;
  if tmm=1 then mat.Material.Texture.MappingMode:=tmmObjectLinear;
  if tmm=2 then mat.Material.Texture.MappingMode:=tmmEyeLinear;
  if tmm=3 then mat.Material.Texture.MappingMode:=tmmSphere;
  if tmm=4 then mat.Material.Texture.MappingMode:=tmmCubeMapReflection;
  if tmm=5 then mat.Material.Texture.MappingMode:=tmmCubeMapNormal;
  if tmm=6 then mat.Material.Texture.MappingMode:=tmmCubeMapLight0;
  if tmm=7 then mat.Material.Texture.MappingMode:=tmmCubeMapCamera;
  result:=1;
end;

function MaterialSetPolygonMode(mtrl: pchar; pm: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if pm=0 then mat.Material.FrontProperties.PolygonMode := pmFill;
  if pm=1 then mat.Material.FrontProperties.PolygonMode := pmLines;
  if pm=2 then mat.Material.FrontProperties.PolygonMode := pmPoints;
  result:=1;
end;

function MaterialSetTextureImageAlpha(mtrl: pchar; tia: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if tia=0 then mat.Material.Texture.ImageAlpha := tiaDefault;
  if tia=1 then mat.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
  if tia=2 then mat.Material.Texture.ImageAlpha := tiaSuperBlackTransparent;
  if tia=3 then mat.Material.Texture.ImageAlpha := tiaLuminance;
  if tia=4 then mat.Material.Texture.ImageAlpha := tiaLuminanceSqrt;
  if tia=5 then mat.Material.Texture.ImageAlpha := tiaOpaque;
  if tia=6 then mat.Material.Texture.ImageAlpha := tiaTopLeftPointColorTransparent;
  if tia=7 then mat.Material.Texture.ImageAlpha := tiaInverseLuminance;
  if tia=8 then mat.Material.Texture.ImageAlpha := tiaInverseLuminanceSqrt;
  result:=1;
end;

function MaterialSetTextureScale(mtrl: pchar; u, v: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.TextureScale.X := u;
  mat.TextureScale.Y := v;
  result:=1;
end;

function MaterialSetTextureOffset(mtrl: pchar; u, v: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.TextureOffset.X := u;
  mat.TextureOffset.Y := v;
  result:=1;
end;

function MaterialSetTextureFilter(mtrl: pchar; mi, ma: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if mi=0 then mat.Material.Texture.MinFilter := miNearest;
  if mi=1 then mat.Material.Texture.MinFilter := miLinear;
  if mi=2 then mat.Material.Texture.MinFilter := miNearestMipmapNearest;
  if mi=3 then mat.Material.Texture.MinFilter := miLinearMipmapNearest;
  if mi=4 then mat.Material.Texture.MinFilter := miNearestMipmapLinear;
  if mi=5 then mat.Material.Texture.MinFilter := miLinearMipmapLinear;
  if ma=0 then mat.Material.Texture.MagFilter := maNearest;
  if ma=1 then mat.Material.Texture.MagFilter := maLinear;
  result:=1;
end;

function MaterialEnableTexture(mtrl: pchar; mode: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if mode=0 then
      mat.Material.Texture.Enabled := false
  else
      mat.Material.Texture.Enabled := true;
  result:=1;
end;

function MaterialGetCount(): real; cdecl;
begin
  result:=matlib.Materials.Count;
end;

function MaterialGetName(ind: real): pchar; cdecl;
begin
  result:=pchar(matlib.Materials.Items[trunc64(ind)].Name);
end;

function MaterialSetFaceCulling(mtrl: pchar; fc: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if fc=0 then mat.Material.FaceCulling := fcBufferDefault;
  if fc=1 then mat.Material.FaceCulling := fcCull;
  if fc=2 then mat.Material.FaceCulling := fcNoCull;
  result:=1;
end;

function MaterialSetSecondTexture(mtrl, mtrl2: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Texture2Name:=String(mtrl2);
  result:=1;
end;

function MaterialSetTextureFormat(mtrl: pchar; tf: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if tf=0 then mat.Material.Texture.TextureFormat := tfDefault;
  if tf=1 then mat.Material.Texture.TextureFormat := tfRGB;
  if tf=2 then mat.Material.Texture.TextureFormat := tfRGBA;
  if tf=3 then mat.Material.Texture.TextureFormat := tfRGB16;
  if tf=4 then mat.Material.Texture.TextureFormat := tfRGBA16;
  if tf=5 then mat.Material.Texture.TextureFormat := tfAlpha;
  if tf=6 then mat.Material.Texture.TextureFormat := tfLuminance;
  if tf=7 then mat.Material.Texture.TextureFormat := tfLuminanceAlpha;
  if tf=8 then mat.Material.Texture.TextureFormat := tfIntensity;
  if tf=9 then mat.Material.Texture.TextureFormat := tfNormalMap;
  if tf=10 then mat.Material.Texture.TextureFormat := tfRGBAFloat16;
  if tf=11 then mat.Material.Texture.TextureFormat := tfRGBAFloat32;
  result:=1;
end;

function MaterialSetTextureCompression(mtrl: pchar; tc: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if tc=0 then mat.Material.Texture.Compression := tcDefault;
  if tc=1 then mat.Material.Texture.Compression := tcNone;
  if tc=2 then mat.Material.Texture.Compression := tcStandard;
  if tc=3 then mat.Material.Texture.Compression := tcHighQuality;
  if tc=4 then mat.Material.Texture.Compression := tcHighSpeed;
  result:=1;
end;

function MaterialTextureRequiredMemory(mtrl: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  result:=mat.Material.Texture.TextureImageRequiredMemory;
end;

function MaterialSetFilteringQuality(mtrl: pchar; tf: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if tf=0 then mat.Material.Texture.FilteringQuality := tfIsotropic;
  if tf=1 then mat.Material.Texture.FilteringQuality := tfAnisotropic;
  result:=1;
end;

function MaterialAddTextureEx(mtrl, tex: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
  mat2:TGLLibMaterial;
  item:TGLTextureExItem;
begin
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  mat2:=matlib.Materials.GetLibMaterialByName(String(tex));
  item := mat.Material.TextureEx.Add;
  item.Texture := mat2.Material.Texture;
  result:=1;
end;

function MaterialTextureExClear(mtrl: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.TextureEx.Clear;
  result:=1;
end;

function MaterialTextureExDelete(mtrl: pchar; ind: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.TextureEx.Items[trunc64(ind)].Free;
  result:=1;
end;

function MaterialSetShader(mtrl: pchar; shd: real): real; cdecl;
var
  mat:TGLLibMaterial;
  shad: TGLShader;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  if shd > 0 then
  begin
    shad := TGLShader(trunc64(shd));
    mat.Shader := shad;
  end
  else
    mat.Shader := nil;
  result := 1;
end;

function MaterialSaveTexture(mtrl,fname: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.Texture.Image.SaveToFile(fname);
  result:=1;
end;

function MaterialSetOptions(mtrl: pchar; op1,op2: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if (op1=1) and (op2=1) then mat.Material.MaterialOptions:=[moIgnoreFog,moNoLighting];
  if (op1=1) and (op2=0) then mat.Material.MaterialOptions:=[moIgnoreFog];
  if (op1=0) and (op2=1) then mat.Material.MaterialOptions:=[moNoLighting];
  if (op1=0) and (op2=0) then mat.Material.MaterialOptions:=[];
  result:=1;
end;

function MaterialSetTextureWrap(mtrl: pchar; wrap: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if wrap = 0 then
    mat.Material.Texture.TextureWrap := twNone
  else
    mat.Material.Texture.TextureWrap := twBoth;
  result:=1;
end;

function MaterialGenTexture(mtrl: pchar; w, h: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.Texture.ImageClassName := TGLBlankImage.ClassName;
  with mat.Material.Texture.Image as TGLBlankImage do begin
    Width := trunc64(w);
    Height := trunc64(h);
  end;
  result:=1;
end;

function MaterialSetTexture(mtrl, mtrl2: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
  mat2:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat2:=matlib.Materials.GetLibMaterialByName(mtrl2);
  mat.Material.Texture := mat2.Material.Texture;
  result:=1;
end;

function MaterialGetTextureWidth(mtrl: pchar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  result := 0;
  if mat.Material.Texture <> nil then
    result := mat.Material.Texture.Image.Width;
end;

function MaterialGetTextureHeight(mtrl: pchar): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  result := 0;
  if mat.Material.Texture <> nil then
    result := mat.Material.Texture.Image.Height;
end;

function MaterialLoadTexture(mtrl, filename: pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  
  try
    mat.Material.Texture.Image.LoadFromFile(String(filename));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('MaterialLoadTexture:' + #13#10 + 'Error loading ' + String(filename) + #13#10 + E.Message);
    end;
  end;
  
  result:=1;
end;

function MaterialLoadTextureEx(mtrl, filename: pchar; index: real): real; cdecl;
var
  mat: TGLLibMaterial;
  tex: TGLTexture;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  tex := TGLTexture.Create(mat.Material);
  
  try
    tex.Image.LoadFromFile(String(filename));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('MaterialLoadTextureEx:' + #13#10 + 'Error loading ' + String(filename) + #13#10 + E.Message);
    end;
  end;
  
  tex.Disabled := False;
  mat.Material.SetTextureN(trunc64(index), tex);
  result:=1;
end;

function MaterialSetTextureEx(mtrl, mtrl2: pchar; index: real): real; cdecl;
var
  mat: TGLLibMaterial;
  mat2: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  mat2 := matlib.Materials.GetLibMaterialByName(mtrl2);
  mat.Material.SetTextureN(trunc64(index), mat2.Material.Texture);
  result := 1;
end;

function MaterialGenTextureEx(mtrl: pchar; index, w, h: real): real; cdecl;
var
  mat: TGLLibMaterial;
  tex: TGLTexture;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  tex := TGLTexture.Create(mat.Material);
  tex.ImageClassName := TGLBlankImage.ClassName;
  with tex.Image as TGLBlankImage do begin
    Width := trunc64(w);
    Height := trunc64(h);
  end;
  tex.Disabled := False;
  mat.Material.SetTextureN(trunc64(index), tex);
  result := 1;
end;

function MaterialEnableTextureEx(mtrl: pchar; index, mode: real): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.GetTextureN(trunc64(index)).Disabled := not Boolean(trunc64(mode));
  result := 1;
end;

function MaterialHasTextureEx(mtrl: pchar; index: real): real; cdecl;
var
  mat: TGLLibMaterial;
  tex: TGLTexture;
begin
  mat := matlib.Materials.GetLibMaterialByName(mtrl);
  tex := mat.Material.GetTextureN(trunc64(index));
  if tex = nil then
    result := 0
  else 
    result := 1;
end;

function MaterialNoiseCreate(mtrl:pchar): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.Add();
  mat.Name:=mtrl;
  mat.Material.Texture.Disabled:=false;
  mat.Material.Texture.Image:=TGLProcTextureNoise.Create(nil);
  result:=1;
end;

function MaterialNoiseSetDimensions(mtrl:pchar; w,h:real): real; cdecl;
begin
  with TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(mtrl).Material.Texture.Image) do
  begin
    Width:=trunc64(w);
    Height:=trunc64(h);
  end;
  result:=1;
end;

function MaterialNoiseAnimate(mtrl:pchar; speed:real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(mtrl).Material.Texture.Image).NoiseAnimate(speed);
  result:=1;
end;

function MaterialNoiseSetMinCut(mtrl:pchar; m:real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(mtrl).Material.Texture.Image).MinCut:=trunc64(m);
  result:=1;
end;

function MaterialNoiseSetSharpness(mtrl:pchar; s:real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(mtrl).Material.Texture.Image).NoiseSharpness:=s;
  result:=1;
end;

function MaterialNoiseSetSeamless(mtrl:pchar; mode:real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(mtrl).Material.Texture.Image).Seamless:=boolean(trunc64(mode));
  result:=1;
end;

function MaterialNoiseRandomSeed(mtrl:pchar; s:real): real; cdecl;
begin
  TGLProcTextureNoise(matlib.Materials.GetLibMaterialByName(mtrl).Material.Texture.Image).NoiseRandSeed:=trunc64(s);
  result:=1;
end;

function MaterialCullFrontFaces(mtrl: pchar; culff: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.CullFrontFaces := Boolean(trunc64(culff));
  result:=1;
end;

function MaterialSetZWrite(mtrl: pchar; zwrite: real): real; cdecl;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.ZWrite := Boolean(trunc64(zwrite));
  result:=1;
end;

function MaterialSetTextureExFromLibrary(material1: pchar; matlib2: real; material2: pchar; index: real): real; cdecl;
var
  mat1, mat2: TGLLibMaterial;
  mlib: TGLMaterialLibrary;
begin
  mat1 := matlib.Materials.GetLibMaterialByName(String(material1));
  mlib := TGLMaterialLibrary(trunc64(matlib2));
  mat2 := mlib.Materials.GetLibMaterialByName(String(material2));
  mat1.Material.SetTextureN(trunc64(index), mat2.Material.Texture);
  result := 1.0;
end;

function MaterialGetNameFromLibrary(matlib, index: real): pchar; cdecl;
var
  mlib: TGLMaterialLibrary;
begin
  mlib := TGLMaterialLibrary(trunc64(matlib));
  result := pchar(mlib.Materials.Items[trunc64(index)].Name);
end;
