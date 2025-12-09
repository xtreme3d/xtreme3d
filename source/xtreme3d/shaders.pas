function ShaderEnable(shader,mode: real): real; cdecl;
var
  GLShader: TGLShader;
begin
  GLShader:=TGLShader(RealToPtr(shader));
  GLShader.Enabled:=boolean(Trunc(mode));
  result:=1;
end;

//Cel Shader
function CelShaderCreate(): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader.Create(scene);
  result:=ObjToReal(GLCel1);
end;

function CelShaderSetLineColor(shader,col: real): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(RealToPtr(shader));
  GLCel1.OutlineColor.AsWinColor:=TColor(Trunc(col));
  result:=1;
end;

function CelShaderSetLineWidth(shader,width: real): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(RealToPtr(shader));
  GLCel1.OutlineWidth:=width;
  result:=1;
end;

function CelShaderSetOptions(shader,outlines,textured: real): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(RealToPtr(shader));
  if (outlines=1) and (textured=1) then GLCel1.CelShaderOptions:=[csoOutlines, csoTextured];
  if (outlines=1) and (textured=0) then GLCel1.CelShaderOptions:=[csoOutlines];
  if (outlines=0) and (textured=1) then GLCel1.CelShaderOptions:=[             csoTextured];
  if (outlines=0) and (textured=0) then GLCel1.CelShaderOptions:=[                        ];
  result:=1;
end;

// Multimaterial Shader
function MultiMaterialShaderCreate(matlib: real): real; cdecl;
var
  shader: TGLMultiMaterialShader;
begin
  shader := TGLMultiMaterialShader.Create(scene);
  shader.MaterialLibrary := TGLMaterialLibrary(RealToPtr(matlib));
  result:=ObjToReal(shader);
end;

// HiddenLine Shader
function HiddenLineShaderCreate(): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader.Create(scene);
  result := ObjToReal(sh);
end;

function HiddenLineShaderSetLineSmooth(shader, mode: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(RealToPtr(shader));
  sh.LineSmooth := Boolean(RealToPtr(mode));
  result := 1;
end;

function HiddenLineShaderSetSolid(shader, mode: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(RealToPtr(shader));
  sh.Solid := Boolean(RealToPtr(mode));
  result := 1;
end;

function HiddenLineShaderSetSurfaceLit(shader, mode: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(RealToPtr(shader));
  sh.SurfaceLit := Boolean(RealToPtr(mode));
  result := 1;
end;

function HiddenLineShaderSetFrontLine(shader, width, col, pattern, forceMat: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(RealToPtr(shader));
  sh.FrontLine.Width := width;
  sh.FrontLine.Color.AsWinColor := TColor(Trunc(col));
  sh.FrontLine.Pattern := Trunc(pattern);
  sh.FrontLine.ForceMaterial := Boolean(Trunc(forceMat));
  result := 1;
end;

function HiddenLineShaderSetBackLine(shader, width, col, pattern, forceMat: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(RealToPtr(shader));
  sh.BackLine.Width := width;
  sh.BackLine.Color.AsWinColor := TColor(Trunc(col));
  sh.BackLine.Pattern := Trunc(pattern);
  sh.BackLine.ForceMaterial := Boolean(Trunc(forceMat));
  result := 1;
end;

// Outline Shader
function OutlineShaderCreate(smooth: real): real; cdecl;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader.Create(scene);
  sh.LineSmooth := Boolean(Trunc(smooth));
  result := ObjToReal(sh);
end;

function OutlineShaderSetLineColor(shader, col: real): real; cdecl;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader(RealToPtr(shader));
  sh.LineColor.AsWinColor := TColor(Trunc(col));
  result := 1;
end;

function OutlineShaderSetLineWidth(shader, width: real): real; cdecl;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader(RealToPtr(shader));
  sh.LineWidth := width; 
  result := 1;
end;

//Texture Combine Shader
function TexCombineShaderCreate(matlib: real): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader.Create(scene);
  GLTexCombineShader.MaterialLibrary:=TGLMaterialLibrary(RealToPtr(matlib));
  GLTexCombineShader.Enabled:=true;
  result:=ObjToReal(GLTexCombineShader);
end;

function TexCombineShaderAddCombiner(tcs: real; str: PAnsiChar): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(RealToPtr(tcs));
  GLTexCombineShader.Combiners.Add(String(AnsiString(str)));
  result:=1;
end;

function TexCombineShaderMaterial3(tcs: real; m3: PAnsiChar): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(RealToPtr(tcs));
  GLTexCombineShader.LibMaterial3Name:=String(AnsiString(m3));
  result:=1;
end;

function TexCombineShaderMaterial4(tcs: real; m4: PAnsiChar): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(RealToPtr(tcs));
  GLTexCombineShader.LibMaterial4Name:=String(AnsiString(m4));
  result:=1;
end;

// GLSL shader
function GLSLShaderCreate(vp, fp: PAnsiChar): real; cdecl;
var
  shader: TGLSLShader;
begin
  shader := TGLSLShader.Create(scene);
  shader.SetPrograms(vp, fp);
  result := ObjToReal(shader);
end;

function GLSLShaderSetLogger(shader, logger: real): real; cdecl;
var
  shadr: TGLSLShader;
begin
  shadr := TGLSLShader(RealToPtr(shader));
  shadr.logger := TLogSession(RealToPtr(logger));
  result := 1.0;
end;

function GLSLShaderCreateParameter(glsl: real; name: PAnsiChar): real; cdecl;
var
  shader: TGLSLShader;
  param: TGLSLShaderParameter;
begin
  shader := TGLSLShader(RealToPtr(glsl));
  param := shader.Param.AddUniform(String(AnsiString(name)));
  result := ObjToReal(param);
end;

function GLSLShaderSetParameter1i(par: real; val: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniform1i;
  param.UniformInteger := trunc(val);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter1f(par, x: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniform1f;
  param.UniformVector[0] := x;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter2f(par, x, y: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniform2f;
  param.UniformVector[0] := x;
  param.UniformVector[1] := y;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter3f(par, x, y, z: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniform3f;
  param.UniformVector[0] := x;
  param.UniformVector[1] := y;
  param.UniformVector[2] := z;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter4f(par, x, y, z, w: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniform4f;
  param.UniformVector[0] := x;
  param.UniformVector[1] := y;
  param.UniformVector[2] := z;
  param.UniformVector[3] := w;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterTexture(par: real; mtrl: PAnsiChar; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  matName: String;
  mat: TGLLibMaterial;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformTexture2D;
  matName := StrConv(mtrl);
  if Length(matName) > 0 then
  begin
    mat := matlib.Materials.GetLibMaterialByName(matName);
    param.Texture := mat.Material.Texture;
    param.TextureTarget := cGLTexTypeToGLEnum[param.Texture.Image.NativeTextureTarget];
  end;
  param.UniformTexture := trunc(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterSecondTexture(par: real; mtrl: PAnsiChar; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  matName: String;
  mat: TGLLibMaterial;
  mat2: TGLLibMaterial;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformSecondTexture2D;
  matName := StrConv(mtrl);
  if Length(matName) > 0 then
  begin
    mat := matlib.Materials.GetLibMaterialByName(matName);
    mat2 := matlib.Materials.GetLibMaterialByName(mat.Texture2Name);
    param.Texture := mat2.Material.Texture;
  end;
  param.UniformTexture := trunc(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterShadowTexture(par, shadowmap, texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformShadowTexture;
  param.ShadowMap := TGLShadowMap(RealToPtr(shadowmap));
  param.UniformTexture := trunc(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterShadowMatrix(par, shadowmap: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformShadowMatrix;
  param.ShadowMap := TGLShadowMap(RealToPtr(shadowmap));
  param.UniformMatrix := TGLShadowMap(RealToPtr(shadowmap)).ShadowMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterMatrix(par, obj: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformMatrix4f;
  param.UniformMatrix := sobj.AbsoluteMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterInvMatrix(par, obj: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(RealToPtr(obj));
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformMatrix4f;
  param.UniformMatrix := sobj.InvAbsoluteMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterFBOColorTexture(par, fbo, texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformFBOColorTexture;
  param.FBORenderer := TGLFBORenderer(RealToPtr(fbo));
  param.UniformTexture := trunc(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterFBODepthTexture(par, fbo, texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformFBODepthTexture;
  param.FBORenderer := TGLFBORenderer(RealToPtr(fbo));
  param.UniformTexture := trunc(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterViewMatrix(par: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformViewMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterInvViewMatrix(par: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformInvViewMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterHasTextureEx(par, slot: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(RealToPtr(par));
  param.UniformType := uniformHaveTexture;
  param.UniformInteger := trunc(slot);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderForceDisableStencilTest(shader, mode: real): real; cdecl;
var
  shadr: TGLSLShader;
begin
  shadr := TGLSLShader(RealToPtr(shader));
  shadr.ForceDisableStencilTest := Boolean(Trunc(mode));
  result := 1;
end;

function GLSLShaderSetOptions(shader, lightingEnabled, fogEnabled: real): real; cdecl;
var
  shadr: TGLSLShader;
begin
  shadr := TGLSLShader(RealToPtr(shader));
  shadr.LightingEnabled := Boolean(Trunc(lightingEnabled));
  shadr.FogEnabled := Boolean(Trunc(fogEnabled));
  result := 1;
end;


function PhongShaderCreate: real; cdecl;
var
  phong: TGLSLShader;
  paramDiffTex: TGLSLShaderParameter;
  paramUseTexture: TGLSLShaderParameter;
  paramMaxLights: TGLSLShaderParameter;
  paramFogEnabled: TGLSLShaderParameter;
  paramLightingEnabled: TGLSLShaderParameter;
begin
  phong := TGLSLShader.Create(scene);
  phong.SetPrograms(phongVertexProgram, phongFragmentProgram);

  paramDiffTex := phong.Param.AddUniform('diffuseMap');
  paramDiffTex.UniformType := uniformTexture2D;
  paramDiffTex.UniformTexture := 0;
  paramDiffTex.Initialized := True;

  paramUseTexture := phong.Param.AddUniform('useTexture');
  paramUseTexture.UniformType := uniform1i;
  paramUseTexture.UniformInteger := 0;
  paramUseTexture.Initialized := True;

  paramMaxLights := phong.Param.AddUniform('maxNumLights');
  paramMaxLights.UniformType := uniform1i;
  paramMaxLights.UniformInteger := 1;
  paramMaxLights.Initialized := True;

  paramFogEnabled := phong.Param.AddUniform('fogEnabled');
  paramFogEnabled.UniformType := uniformFogEnabled;
  paramFogEnabled.Initialized := True;

  paramLightingEnabled := phong.Param.AddUniform('lightingEnabled');
  paramLightingEnabled.UniformType := uniformLightingEnabled;
  paramLightingEnabled.Initialized := True;

  result := ObjToReal(phong);
end;

function PhongShaderUseTexture(shader, mode: real): real; cdecl;
var
  phong: TGLSLShader;
  paramUseTexture: TGLSLShaderParameter;
begin
  phong := TGLSLShader(RealToPtr(shader));
  paramUseTexture := phong.Param.Items[1];
  paramUseTexture.UniformInteger := trunc(mode);
  result:=1;
end;

function PhongShaderSetMaxLights(shader, maxlights: real): real; cdecl;
var
  phong: TGLSLShader;
  paramMaxLights: TGLSLShaderParameter;
begin
  phong := TGLSLShader(RealToPtr(shader));
  paramMaxLights := phong.Param.Items[2];
  paramMaxLights.UniformInteger := trunc(maxlights);
  result:=1;
end;

function BumpShaderCreate: real; cdecl;
var
  bump: TGLSLShader;
  paramMaxLights: TGLSLShaderParameter;
  paramUseParallax: TGLSLShaderParameter;
  paramParallaxHeight: TGLSLShaderParameter;
  paramShadowMap: TGLSLShaderParameter;
  paramShadowMatrix: TGLSLShaderParameter;
  paramUseShadowMap: TGLSLShaderParameter;
  paramShadowMapSize: TGLSLShaderParameter;
  paramShadowBlurRadius: TGLSLShaderParameter;
  paramUseAutoTangentSpace: TGLSLShaderParameter;
  paramFogEnabled: TGLSLShaderParameter;
  paramLightingEnabled: TGLSLShaderParameter;
begin
  bump := TGLSLShader.Create(scene);
  bump.SetPrograms(bumpVertexProgram, bumpFragmentProgram);

  bump.Param.AddUniform('diffuseMap');
  bump.Param.AddUniform('normalMap');
  bump.Param.AddUniform('heightMap');

  paramMaxLights := bump.Param.AddUniform('maxNumLights');
  paramMaxLights.UniformType := uniform1i;
  paramMaxLights.UniformInteger := 8;
  paramMaxLights.Initialized := True;

  paramUseParallax := bump.Param.AddUniform('useParallax');
  paramUseParallax.UniformType := uniform1i;
  paramUseParallax.UniformInteger := 0;
  paramUseParallax.Initialized := True;

  paramParallaxHeight := bump.Param.AddUniform('parallaxHeight');
  paramParallaxHeight.UniformType := uniform1f;
  paramParallaxHeight.UniformVector[0] := 0.03;
  paramParallaxHeight.Initialized := True;

  paramShadowMap := bump.Param.AddUniform('shadowMap');
  paramShadowMap.UniformType := uniformShadowTexture;
  paramShadowMap.UniformTexture := 7;
  paramShadowMap.ShadowMap := nil;
  paramShadowMap.Initialized := True;

  paramShadowMatrix := bump.Param.AddUniform('shadowMatrix');
  paramShadowMatrix.UniformType := uniformShadowMatrix;
  paramShadowMatrix.ShadowMap := nil;
  paramShadowMatrix.Initialized := True;

  paramUseShadowMap := bump.Param.AddUniform('useShadowMap');
  paramUseShadowMap.UniformType := uniform1i;
  paramUseShadowMap.UniformInteger := 0;
  paramUseShadowMap.Initialized := True;

  paramShadowMapSize := bump.Param.AddUniform('shadowMapSize');
  paramShadowMapSize.UniformType := uniform2f;
  paramShadowMapSize.UniformVector[0] := 1.0;
  paramShadowMapSize.UniformVector[1] := 1.0;
  paramShadowMapSize.Initialized := True;

  paramShadowBlurRadius := bump.Param.AddUniform('shadowBlurRadius');
  paramShadowBlurRadius.UniformType := uniform1f;
  paramShadowBlurRadius.UniformVector[0] := 0.0;
  paramShadowBlurRadius.Initialized := True;

  paramUseAutoTangentSpace := bump.Param.AddUniform('useAutoTangentSpace');
  paramUseAutoTangentSpace.UniformType := uniform1i;
  paramUseAutoTangentSpace.UniformInteger := 1;
  paramUseAutoTangentSpace.Initialized := True;

  paramFogEnabled := bump.Param.AddUniform('fogEnabled');
  paramFogEnabled.UniformType := uniformFogEnabled;
  paramFogEnabled.Initialized := True;

  paramLightingEnabled := bump.Param.AddUniform('lightingEnabled');
  paramLightingEnabled.UniformType := uniformLightingEnabled;
  paramLightingEnabled.Initialized := True;

  result := ObjToReal(bump);
end;

function BumpShaderSetDiffuseTexture(shader: real; mtrl: PAnsiChar): real; cdecl;
var
  bump: TGLSLShader;
  matName: String;
  mat: TGLLibMaterial;
  paramDiffTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  matName := StrConv(mtrl);
  paramDiffTex := bump.Param.Items[0];
  paramDiffTex.UniformType := uniformTexture2D;
  if Length(matName) > 0 then
  begin
    mat:=matlib.Materials.GetLibMaterialByName(matName);
    paramDiffTex.Texture := mat.Material.Texture;
  end;
  paramDiffTex.UniformTexture := 0;
  paramDiffTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetNormalTexture(shader: real; mtrl: PAnsiChar): real; cdecl;
var
  bump: TGLSLShader;
  matName: String;
  mat: TGLLibMaterial;
  paramNormTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  matName := StrConv(mtrl);
  paramNormTex := bump.Param.Items[1];
  paramNormTex.UniformType := uniformTexture2D;
  if Length(matName) > 0 then
  begin
    mat:=matlib.Materials.GetLibMaterialByName(matName);
    paramNormTex.Texture := mat.Material.Texture;
  end;
  paramNormTex.UniformTexture := 1;
  paramNormTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetHeightTexture(shader: real; mtrl: PAnsiChar): real; cdecl;
var
  bump: TGLSLShader;
  matName: String;
  mat: TGLLibMaterial;
  paramHeightTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  matName := StrConv(mtrl);
  paramHeightTex := bump.Param.Items[2];
  paramHeightTex.UniformType := uniformTexture2D;
  if Length(matName) > 0 then
  begin
    mat:=matlib.Materials.GetLibMaterialByName(matName);
    paramHeightTex.Texture := mat.Material.Texture;
  end;
  paramHeightTex.UniformTexture := 2;
  paramHeightTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetMaxLights(shader, maxlights: real): real; cdecl;
var
  bump: TGLSLShader;
  paramMaxLights: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  paramMaxLights := bump.Param.Items[3];
  paramMaxLights.UniformInteger := trunc(maxlights);
  result:=1;
end;

function BumpShaderUseParallax(shader, mode: real): real; cdecl;
var
  bump: TGLSLShader;
  paramUseParallax: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  paramUseParallax := bump.Param.Items[4];
  paramUseParallax.UniformInteger := trunc(mode);
  result:=1;
end;

function BumpShaderSetParallaxOffset(shader, height: real): real; cdecl;
var
  bump: TGLSLShader;
  paramParallaxHeight: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  paramParallaxHeight := bump.Param.Items[5];
  paramParallaxHeight.UniformVector[0] := height;
  result:=1;
end;

function BumpShaderSetShadowMap(shader, shadowmap: real): real; cdecl;
var
  bump: TGLSLShader;
  sm: TGLShadowMap;
  paramShadowMap: TGLSLShaderParameter;
  paramShadowMatrix: TGLSLShaderParameter;
  paramUseShadowMap: TGLSLShaderParameter;
  paramShadowMapSize: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  paramShadowMap := bump.Param.Items[6];
  paramShadowMatrix := bump.Param.Items[7];
  paramUseShadowMap := bump.Param.Items[8];
  paramShadowMapSize := bump.Param.Items[9];

  if shadowmap <> 0 then
  begin
    sm := TGLShadowMap(RealToPtr(shadowmap));
    paramShadowMap.ShadowMap := sm;
    paramShadowMatrix.ShadowMap := sm;
    paramUseShadowMap.UniformInteger := 1;
    paramShadowMapSize.UniformVector[0] := sm.FBO.Width;
    paramShadowMapSize.UniformVector[1] := sm.FBO.Height;
  end
  else
  begin
    paramShadowMap.ShadowMap := nil;
    paramShadowMatrix.ShadowMap := nil;
    paramUseShadowMap.UniformInteger := 0;
    paramShadowMapSize.UniformVector[0] := 1.0;
    paramShadowMapSize.UniformVector[1] := 1.0;
  end;
  result:=1;
end;

function BumpShaderSetShadowBlurRadius(shader, radius: real): real; cdecl;
var
  bump: TGLSLShader;
  paramShadowBlurRadius: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  paramShadowBlurRadius := bump.Param.Items[10];
  paramShadowBlurRadius.UniformVector[0] := radius;
  result:=1;
end;

function BumpShaderUseAutoTangentSpace(shader, mode: real): real; cdecl;
var
  bump: TGLSLShader;
  paramUseAutoTangentSpace: TGLSLShaderParameter;
begin
  bump := TGLSLShader(RealToPtr(shader));
  paramUseAutoTangentSpace := bump.Param.Items[11];
  paramUseAutoTangentSpace.UniformInteger := trunc(mode);
  result:=1;
end;
