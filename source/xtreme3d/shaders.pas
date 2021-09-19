function ShaderEnable(shader,mode: real): real; cdecl;
var
  GLShader: TGLShader;
begin
  GLShader:=TGLShader(trunc64(shader));
  GLShader.Enabled:=boolean(trunc64(mode));
  result:=1;
end;

function PhongShaderCreate(): real; cdecl;
var
  phong: TGLSLShader;
  paramDiffTex: TGLSLShaderParameter;
  paramUseTexture: TGLSLShaderParameter;
  paramMaxLights: TGLSLShaderParameter;
  paramFogEnabled: TGLSLShaderParameter;
  paramLightingEnabled: TGLSLShaderParameter;
begin
  if not
    (GL_ARB_shader_objects and
     GL_ARB_vertex_shader and
     GL_ARB_fragment_shader) then begin
      ShowMessage('GL_ARB_shader_objects, GL_ARB_vertex_shader, GL_ARB_fragment_shader required');
      result := 0;
      Exit;
  end;
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
  
  result := integer(phong);
end;

function PhongShaderUseTexture(shader, mode: real): real; cdecl;
var
  phong: TGLSLShader;
  paramUseTexture: TGLSLShaderParameter;
begin
  phong := TGLSLShader(trunc64(shader));
  paramUseTexture := phong.Param.Items[1];
  paramUseTexture.UniformInteger := trunc64(mode);
  result:=1;
end;

function PhongShaderSetMaxLights(shader, maxlights: real): real; cdecl;
var
  phong: TGLSLShader;
  paramMaxLights: TGLSLShaderParameter;
begin
  phong := TGLSLShader(trunc64(shader));
  paramMaxLights := phong.Param.Items[2];
  paramMaxLights.UniformInteger := trunc64(maxlights);
  result:=1;
end;

function BumpShaderCreate(): real; cdecl;
var
  bump: TGLSLShader;
  paramDiffTex: TGLSLShaderParameter;
  paramNormTex: TGLSLShaderParameter;
  paramHeightTex: TGLSLShaderParameter;
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
  if not
    (GL_ARB_shader_objects and
     GL_ARB_vertex_shader and
     GL_ARB_fragment_shader) then begin
      ShowMessage('GL_ARB_shader_objects, GL_ARB_vertex_shader, GL_ARB_fragment_shader required');
      result := 0;
      Exit;
  end;
  bump := TGLSLShader.Create(scene);
  bump.SetPrograms(bumpVertexProgram, bumpFragmentProgram);
  paramDiffTex := bump.Param.AddUniform('diffuseMap');
  paramNormTex := bump.Param.AddUniform('normalMap');
  paramHeightTex := bump.Param.AddUniform('heightMap');
  paramMaxLights := bump.Param.AddUniform('maxNumLights');
  paramMaxLights.UniformType := uniform1i;
  paramMaxLights.UniformInteger := 1;
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
  //paramShadowMatrix.UniformMatrix := TGLShadowMap(trunc64(shadowmap)).ShadowMatrix;
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
  paramUseAutoTangentSpace.UniformInteger := 0;
  paramUseAutoTangentSpace.Initialized := True;
  
  paramFogEnabled := bump.Param.AddUniform('fogEnabled');
  paramFogEnabled.UniformType := uniformFogEnabled;
  paramFogEnabled.Initialized := True;
  
  paramLightingEnabled := bump.Param.AddUniform('lightingEnabled');
  paramLightingEnabled.UniformType := uniformLightingEnabled;
  paramLightingEnabled.Initialized := True;

  result := integer(bump);
end;

function BumpShaderSetDiffuseTexture(shader: real; mtrl: pchar): real; cdecl;
var
  bump: TGLSLShader;
  mat: TGLLibMaterial;
  paramDiffTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramDiffTex := bump.Param.Items[0];
  paramDiffTex.UniformType := uniformTexture2D;
  if Length(mtrl) > 0 then
  begin
    mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
    paramDiffTex.Texture := mat.Material.Texture;
  end;
  paramDiffTex.UniformTexture := 0;
  paramDiffTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetNormalTexture(shader: real; mtrl: pchar): real; cdecl;
var
  bump: TGLSLShader;
  mat: TGLLibMaterial;
  paramNormTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramNormTex := bump.Param.Items[1];
  paramNormTex.UniformType := uniformTexture2D;
  if Length(mtrl) > 0 then
  begin
    mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
    paramNormTex.Texture := mat.Material.Texture;
  end;
  paramNormTex.UniformTexture := 1;
  paramNormTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetHeightTexture(shader: real; mtrl: pchar): real; cdecl;
var
  bump: TGLSLShader;
  mat: TGLLibMaterial;
  paramHeightTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramHeightTex := bump.Param.Items[2];
  paramHeightTex.UniformType := uniformTexture2D;
  if Length(mtrl) > 0 then
  begin
    mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
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
  bump := TGLSLShader(trunc64(shader));
  paramMaxLights := bump.Param.Items[3];
  paramMaxLights.UniformInteger := trunc64(maxlights);
  result:=1;
end;

function BumpShaderUseParallax(shader, mode: real): real; cdecl;
var
  bump: TGLSLShader;
  paramUseParallax: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramUseParallax := bump.Param.Items[4];
  paramUseParallax.UniformInteger := trunc64(mode);
  result:=1;
end;

function BumpShaderSetParallaxOffset(shader, height: real): real; cdecl;
var
  bump: TGLSLShader;
  paramParallaxHeight: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
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
  bump := TGLSLShader(trunc64(shader));
  if shadowmap <> 0 then
    sm := TGLShadowMap(trunc64(shadowmap));
  paramShadowMap := bump.Param.Items[6];
  paramShadowMatrix := bump.Param.Items[7];
  paramUseShadowMap := bump.Param.Items[8];
  paramShadowMapSize := bump.Param.Items[9];
  
  if shadowmap <> 0 then
  begin
    paramShadowMap.ShadowMap := sm;
    paramShadowMatrix.ShadowMap := sm;
    paramUseShadowMap.UniformInteger := 1;
    paramShadowMapSize.UniformVector[0] := sm.Width;
    paramShadowMapSize.UniformVector[1] := sm.Height;
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
  bump := TGLSLShader(trunc64(shader));
  paramShadowBlurRadius := bump.Param.Items[10];
  paramShadowBlurRadius.UniformVector[0] := radius;
  result:=1;
end;

function BumpShaderUseAutoTangentSpace(shader, mode: real): real; cdecl;
var
  bump: TGLSLShader;
  paramUseAutoTangentSpace: TGLSLShaderParameter; 
begin
  bump := TGLSLShader(trunc64(shader));
  paramUseAutoTangentSpace := bump.Param.Items[11];
  paramUseAutoTangentSpace.UniformInteger := trunc64(mode);
  result:=1;
end;

// BumpShaderSetMethod is no longer available, bump shader is now GLSL-only
// BumpShaderSetSpecularMode is no longer available, only Blinn-Phong specular is supported
// BumpShaderSetSpace is no longer available, only tangent space is supported
// BumpShaderSetOptions is no longer available

//Cel Shader
function CelShaderCreate(): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader.Create(scene);
  result:=integer(GLCel1);
end;

function CelShaderSetLineColor(shader,col: real): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(trunc64(shader));
  GLCel1.OutlineColor.AsWinColor:=TColor(trunc64(col));
  result:=1;
end;

function CelShaderSetLineWidth(shader,width: real): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(trunc64(shader));
  GLCel1.OutlineWidth:=width;
  result:=1;
end;

function CelShaderSetOptions(shader,outlines,textured: real): real; cdecl;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(trunc64(shader));
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
  shader.MaterialLibrary := TGLMaterialLibrary(trunc64(matlib));
  result:=integer(shader);
end;

// HiddenLine Shader
function HiddenLineShaderCreate(): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader.Create(scene);
  result := integer(sh);
end;

function HiddenLineShaderSetLineSmooth(shader, mode: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.LineSmooth := Boolean(trunc64(mode)); 
  result := 1;
end;

function HiddenLineShaderSetSolid(shader, mode: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.Solid := Boolean(trunc64(mode)); 
  result := 1;
end;

function HiddenLineShaderSetSurfaceLit(shader, mode: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.SurfaceLit := Boolean(trunc64(mode));
  result := 1;
end;

function HiddenLineShaderSetFrontLine(shader, width, col, pattern, forceMat: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.FrontLine.Width := width;
  sh.FrontLine.Color.AsWinColor := TColor(trunc64(col));
  sh.FrontLine.Pattern := trunc64(pattern);
  sh.FrontLine.ForceMaterial := Boolean(trunc64(forceMat));
  result := 1;
end;

function HiddenLineShaderSetBackLine(shader, width, col, pattern, forceMat: real): real; cdecl;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.BackLine.Width := width;
  sh.BackLine.Color.AsWinColor := TColor(trunc64(col));
  sh.BackLine.Pattern := trunc64(pattern);
  sh.BackLine.ForceMaterial := Boolean(trunc64(forceMat));
  result := 1;
end;

// Outline Shader
function OutlineShaderCreate(smooth: real): real; cdecl;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader.Create(scene);
  sh.LineSmooth := Boolean(trunc64(smooth));
  result := integer(sh);
end;

function OutlineShaderSetLineColor(shader, col: real): real; cdecl;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader(trunc64(shader));
  sh.LineColor.AsWinColor := TColor(trunc64(col)); 
  result := 1;
end;

function OutlineShaderSetLineWidth(shader, width: real): real; cdecl;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader(trunc64(shader));
  sh.LineWidth := width; 
  result := 1;
end;

//Texture Combine Shader
function TexCombineShaderCreate(matlib: real): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader.Create(scene);
  GLTexCombineShader.MaterialLibrary:=TGLMaterialLibrary(trunc64(matlib));
  GLTexCombineShader.Enabled:=true;
  result:=integer(GLTexCombineShader);
end;

function TexCombineShaderAddCombiner(tcs: real; str: pchar): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(trunc64(tcs));
  GLTexCombineShader.Combiners.Add(str);
  result:=1;
end;

function TexCombineShaderMaterial3(tcs: real; m3: pchar): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(trunc64(tcs));
  GLTexCombineShader.LibMaterial3Name:=m3;
  result:=1;
end;

function TexCombineShaderMaterial4(tcs: real; m4: pchar): real; cdecl;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(trunc64(tcs));
  GLTexCombineShader.LibMaterial4Name:=m4;
  result:=1;
end;

// GLSL shader
function GLSLShaderCreate(vp, fp: pchar): real; cdecl;
var
  shader: TGLSLShader;
begin
  if not
    (GL_ARB_shader_objects and
     GL_ARB_vertex_shader and
     GL_ARB_fragment_shader) then begin
      ShowMessage('GL_ARB_shader_objects, GL_ARB_vertex_shader, GL_ARB_fragment_shader required');
      result := 0;
      Exit;
  end;

  shader := TGLSLShader.Create(scene);
  shader.SetPrograms(vp, fp);
  result:=integer(shader);
end;

function GLSLShaderCreateParameter(glsl: real; name: pchar): real; cdecl;
var
  shader: TGLSLShader;
  param: TGLSLShaderParameter;
begin
  shader := TGLSLShader(trunc64(glsl));
  param := shader.Param.AddUniform(String(name));
  result := integer(param);
end;

function GLSLShaderSetParameter1i(par: real; val: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniform1i;
  param.UniformInteger := trunc64(val);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter1f(par, x: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniform1f;
  param.UniformVector[0] := x;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter2f(par, x, y: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
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
  param := TGLSLShaderParameter(trunc64(par));
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
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniform4f;
  param.UniformVector[0] := x;
  param.UniformVector[1] := y;
  param.UniformVector[2] := z;
  param.UniformVector[3] := w;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterTexture(par: real; mtrl: pchar; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  mat: TGLLibMaterial;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformTexture2D;
  if Length(mtrl) > 0 then
  begin
    mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
    param.Texture := mat.Material.Texture;
  end;
  param.UniformTexture := trunc64(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterSecondTexture(par: real; mtrl: pchar; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  mat: TGLLibMaterial;
  mat2: TGLLibMaterial;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformSecondTexture2D;
  if Length(mtrl) > 0 then
  begin
    mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
    mat2 := matlib.Materials.GetLibMaterialByName(mat.Texture2Name);
    param.Texture := mat2.Material.Texture;
  end;
  param.UniformTexture := trunc64(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterShadowTexture(par, shadowmap: real; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformShadowTexture;
  param.ShadowMap := TGLShadowMap(trunc64(shadowmap));
  param.UniformTexture := trunc64(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterShadowMatrix(par, shadowmap: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformShadowMatrix;
  param.ShadowMap := TGLShadowMap(trunc64(shadowmap));
  param.UniformMatrix := TGLShadowMap(trunc64(shadowmap)).ShadowMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterMatrix(par, obj: real): real; cdecl;
var
  param: TGLSLShaderParameter;
  sobj: TGLBaseSceneObject;
begin
  sobj := TGLBaseSceneObject(trunc64(obj));
  param := TGLSLShaderParameter(trunc64(par));
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
  sobj := TGLBaseSceneObject(trunc64(obj));
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformMatrix4f;
  param.UniformMatrix := sobj.InvAbsoluteMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterFBOColorTexture(par, fbo: real; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformFBOColorTexture;
  param.FBO := TGLFBO(trunc64(fbo));
  param.UniformTexture := trunc64(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterFBODepthTexture(par, fbo: real; texUnit: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformFBODepthTexture;
  param.FBO := TGLFBO(trunc64(fbo));
  param.UniformTexture := trunc64(texUnit);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterViewMatrix(par: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformViewMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterInvViewMatrix(par: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformInvViewMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterHasTextureEx(par, slot: real): real; cdecl;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformHaveTexture;
  param.UniformInteger := trunc64(slot);
  param.Initialized := True;
  result := 1;
end;

