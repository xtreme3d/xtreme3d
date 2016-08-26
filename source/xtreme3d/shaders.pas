function ShaderEnable(shader,mode: real): real; stdcall;
var
  GLShader: TGLShader;
begin
  GLShader:=TGLShader(trunc64(shader));
  GLShader.Enabled:=boolean(trunc64(mode));
  result:=1;
end;

function PhongShaderCreate(): real; stdcall;
var
  GLPhong1: TGLPhongShader;
begin
  GLPhong1:=TGLPhongShader.Create(scene);
  result:=integer(GLPhong1);
end;

function BumpShaderCreate(): real; stdcall;
var
  bump: TGLSLShader;
  paramDiffTex: TGLSLShaderParameter;
  paramNormTex: TGLSLShaderParameter;
  paramHeightTex: TGLSLShaderParameter;
  paramMaxLights: TGLSLShaderParameter;
  paramUseParallax: TGLSLShaderParameter;
  paramParallaxHeight: TGLSLShaderParameter; 
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
  result := integer(bump);
end;

function BumpShaderSetDiffuseTexture(shader: real; mtrl: pchar): real; stdcall;
var
  bump: TGLSLShader;
  mat: TGLLibMaterial;
  paramDiffTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  paramDiffTex := bump.Param.Items[0];
  paramDiffTex.UniformType := uniformTexture2D;
  paramDiffTex.Texture := mat.Material.Texture;
  paramDiffTex.UniformTexture := 0;
  paramDiffTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetNormalTexture(shader: real; mtrl: pchar): real; stdcall;
var
  bump: TGLSLShader;
  mat: TGLLibMaterial;
  paramNormTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  paramNormTex := bump.Param.Items[1];
  paramNormTex.UniformType := uniformTexture2D;
  paramNormTex.Texture := mat.Material.Texture;
  paramNormTex.UniformTexture := 1;
  paramNormTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetHeightTexture(shader: real; mtrl: pchar): real; stdcall;
var
  bump: TGLSLShader;
  mat: TGLLibMaterial;
  paramHeightTex: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  mat:=matlib.Materials.GetLibMaterialByName(String(mtrl));
  paramHeightTex := bump.Param.Items[2];
  paramHeightTex.UniformType := uniformTexture2D;
  paramHeightTex.Texture := mat.Material.Texture;
  paramHeightTex.UniformTexture := 2;
  paramHeightTex.Initialized := True;
  result:=1;
end;

function BumpShaderSetMaxLights(shader, maxlights: real): real; stdcall;
var
  bump: TGLSLShader;
  paramMaxLights: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramMaxLights := bump.Param.Items[3];
  paramMaxLights.UniformInteger := trunc64(maxlights);
  result:=1;
end;

function BumpShaderUseParallax(shader, mode: real): real; stdcall;
var
  bump: TGLSLShader;
  paramUseParallax: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramUseParallax := bump.Param.Items[4];
  paramUseParallax.UniformInteger := trunc64(mode);
  result:=1;
end;

function BumpShaderSetParallaxOffset(shader, height: real): real; stdcall;
var
  bump: TGLSLShader;
  paramParallaxHeight: TGLSLShaderParameter;
begin
  bump := TGLSLShader(trunc64(shader));
  paramParallaxHeight := bump.Param.Items[5];
  paramParallaxHeight.UniformVector[0] := height;
  result:=1;
end;

// BumpShaderSetMethod is no longer available, bump shader is now GLSL-only
// BumpShaderSetSpecularMode is no longer available, only Blinn-Phong specular is supported
// BumpShaderSetSpace is no longer available, only tangent space is supported
// BumpShaderSetOptions is no longer available

//Cel Shader
function CelShaderCreate(): real; stdcall;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader.Create(scene);
  result:=integer(GLCel1);
end;

function CelShaderSetLineColor(shader,col: real): real; stdcall;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(trunc64(shader));
  GLCel1.OutlineColor.AsWinColor:=TColor(trunc64(col));
  result:=1;
end;

function CelShaderSetLineWidth(shader,width: real): real; stdcall;
var
  GLCel1: TGLCelShader;
begin
  GLCel1:=TGLCelShader(trunc64(shader));
  GLCel1.OutlineWidth:=width;
  result:=1;
end;

function CelShaderSetOptions(shader,outlines,textured: real): real; stdcall;
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
function MultiMaterialShaderCreate(matlib: real): real; stdcall;
var
  shader: TGLMultiMaterialShader;
begin
  shader := TGLMultiMaterialShader.Create(scene);
  shader.MaterialLibrary := TGLMaterialLibrary(trunc64(matlib));
  result:=integer(shader);
end;

// HiddenLine Shader
function HiddenLineShaderCreate(): real; stdcall;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader.Create(scene);
  result := integer(sh);
end;

function HiddenLineShaderSetLineSmooth(shader, mode: real): real; stdcall;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.LineSmooth := Boolean(trunc64(mode)); 
  result := 1;
end;

function HiddenLineShaderSetSolid(shader, mode: real): real; stdcall;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.Solid := Boolean(trunc64(mode)); 
  result := 1;
end;

function HiddenLineShaderSetSurfaceLit(shader, mode: real): real; stdcall;
var
  sh: TGLHiddenLineShader;
begin
  sh := TGLHiddenLineShader(trunc64(shader));
  sh.SurfaceLit := Boolean(trunc64(mode));
  result := 1;
end;

function HiddenLineShaderSetFrontLine(shader, width, col, pattern, forceMat: real): real; stdcall;
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

function HiddenLineShaderSetBackLine(shader, width, col, pattern, forceMat: real): real; stdcall;
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
function OutlineShaderCreate(smooth: real): real; stdcall;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader.Create(scene);
  sh.LineSmooth := Boolean(trunc64(smooth));
  result := integer(sh);
end;

function OutlineShaderSetLineColor(shader, col: real): real; stdcall;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader(trunc64(shader));
  sh.LineColor.AsWinColor := TColor(trunc64(col)); 
  result := 1;
end;

function OutlineShaderSetLineWidth(shader, width: real): real; stdcall;
var
  sh: TGLOutlineShader;
begin
  sh := TGLOutlineShader(trunc64(shader));
  sh.LineWidth := width; 
  result := 1;
end;

//Texture Combine Shader
function TexCombineShaderCreate(matlib: real): real; stdcall;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader.Create(scene);
  GLTexCombineShader.MaterialLibrary:=TGLMaterialLibrary(trunc64(matlib));
  GLTexCombineShader.Enabled:=true;
  result:=integer(GLTexCombineShader);
end;

function TexCombineShaderAddCombiner(tcs: real; str: pchar): real; stdcall;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(trunc64(tcs));
  GLTexCombineShader.Combiners.Add(str);
  result:=1;
end;

function TexCombineShaderMaterial3(tcs: real; m3: pchar): real; stdcall;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(trunc64(tcs));
  GLTexCombineShader.LibMaterial3Name:=m3;
  result:=1;
end;

function TexCombineShaderMaterial4(tcs: real; m4: pchar): real; stdcall;
var
  GLTexCombineShader: TGLTexCombineShader;
begin
  GLTexCombineShader:=TGLTexCombineShader(trunc64(tcs));
  GLTexCombineShader.LibMaterial4Name:=m4;
  result:=1;
end;

// GLSL shader
function GLSLShaderCreate(vp, fp: pchar): real; stdcall;
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

function GLSLShaderCreateParameter(glsl: real; name: pchar): real; stdcall;
var
  shader: TGLSLShader;
  param: TGLSLShaderParameter;
begin
  shader := TGLSLShader(trunc64(glsl));
  param := shader.Param.AddUniform(String(name));
  result := integer(param);
end;

function GLSLShaderSetParameter1i(par: real; val: real): real; stdcall;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniform1i;
  param.UniformInteger := trunc64(val);
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter1f(par, x: real): real; stdcall;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniform1f;
  param.UniformVector[0] := x;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameter2f(par, x, y: real): real; stdcall;
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

function GLSLShaderSetParameter3f(par, x, y, z: real): real; stdcall;
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

function GLSLShaderSetParameter4f(par, x, y, z, w: real): real; stdcall;
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

function GLSLShaderSetParameterTexture(par: real; mtrl: pchar; texUnit: real): real; stdcall;
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

function GLSLShaderSetParameterShadowTexture(par, shadowmap: real; texUnit: real): real; stdcall;
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

function GLSLShaderSetParameterShadowMatrix(par, shadowmap: real): real; stdcall;
var
  param: TGLSLShaderParameter;
begin
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformMatrix4f;
  param.UniformMatrix := TGLShadowMap(trunc64(shadowmap)).ShadowMatrix;
  param.Initialized := True;
  result := 1;
end;

function GLSLShaderSetParameterMatrix(par, obj: real): real; stdcall;
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

function GLSLShaderSetParameterInvMatrix(par, obj: real): real; stdcall;
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
