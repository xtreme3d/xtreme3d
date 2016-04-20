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
  GLBump1: TGLBumpShader;
begin
  GLBump1:=TGLBumpShader.Create(scene);
  result:=integer(GLBump1);
end;

function BumpShaderSetMethod(shader,bm: real): real; stdcall;
var
  GLBump1: TGLBumpShader;
begin
  GLBump1:=TGLBumpShader(trunc64(shader));
  if trunc64(bm)=0 then GLBump1.BumpMethod:=bmDot3TexCombiner;
  if trunc64(bm)=1 then GLBump1.BumpMethod:=bmBasicARBFP;
  result:=1;
end;

function BumpShaderSetSpecularMode(shader,sm: real): real; stdcall;
var
  GLBump1: TGLBumpShader;
begin
  GLBump1:=TGLBumpShader(trunc64(shader));
  if trunc64(sm)=0 then GLBump1.SpecularMode:=smOff;
  if trunc64(sm)=1 then GLBump1.SpecularMode:=smBlinn;
  if trunc64(sm)=2 then GLBump1.SpecularMode:=smPhong;
  result:=1;
end;

function BumpShaderSetSpace(shader,bs: real): real; stdcall;
var
  GLBump1: TGLBumpShader;
begin
  GLBump1:=TGLBumpShader(trunc64(shader));
  if trunc64(bs)=0 then GLBump1.BumpSpace:=bsObject;
  if trunc64(bs)=1 then GLBump1.BumpSpace:=bsTangentExternal;
  if trunc64(bs)=2 then GLBump1.BumpSpace:=bsTangentQuaternion;
  result:=1;
end;

function BumpShaderSetParallaxOffset(shader,offs: real): real; stdcall;
var
  GLBump1: TGLBumpShader;
begin
  GLBump1:=TGLBumpShader(trunc64(shader));
  GLBump1.ParallaxOffset:=offs;
  result:=1;
end;

function BumpShaderSetOptions(shader,dt2,stc,la,pm: real): real; stdcall;
var
  GLBump1: TGLBumpShader;
begin
  GLBump1:=TGLBumpShader(trunc64(shader));
  if (dt2=1) and (stc=1) and (la=1) and (pm=1) then GLBump1.BumpOptions:=[boDiffuseTexture2,boUseSecondaryTexCoords,boLightAttenuation,boParallaxMapping];
  if (dt2=1) and (stc=1) and (la=1) and (pm=0) then GLBump1.BumpOptions:=[boDiffuseTexture2,boUseSecondaryTexCoords,boLightAttenuation                  ];
  if (dt2=1) and (stc=1) and (la=0) and (pm=0) then GLBump1.BumpOptions:=[boDiffuseTexture2,boUseSecondaryTexCoords                                     ];
  if (dt2=1) and (stc=0) and (la=0) and (pm=0) then GLBump1.BumpOptions:=[boDiffuseTexture2                                                             ];
  if (dt2=0) and (stc=1) and (la=1) and (pm=1) then GLBump1.BumpOptions:=[                  boUseSecondaryTexCoords,boLightAttenuation,boParallaxMapping];
  if (dt2=0) and (stc=0) and (la=1) and (pm=1) then GLBump1.BumpOptions:=[                                          boLightAttenuation,boParallaxMapping];
  if (dt2=0) and (stc=0) and (la=0) and (pm=1) then GLBump1.BumpOptions:=[                                                             boParallaxMapping];
  if (dt2=1) and (stc=0) and (la=1) and (pm=1) then GLBump1.BumpOptions:=[boDiffuseTexture2,                        boLightAttenuation,boParallaxMapping];
  if (dt2=1) and (stc=1) and (la=0) and (pm=1) then GLBump1.BumpOptions:=[boDiffuseTexture2,boUseSecondaryTexCoords,                   boParallaxMapping];
  if (dt2=1) and (stc=0) and (la=1) and (pm=0) then GLBump1.BumpOptions:=[boDiffuseTexture2,                        boLightAttenuation                  ];
  if (dt2=0) and (stc=1) and (la=0) and (pm=1) then GLBump1.BumpOptions:=[                  boUseSecondaryTexCoords,                   boParallaxMapping];
  if (dt2=1) and (stc=0) and (la=0) and (pm=1) then GLBump1.BumpOptions:=[boDiffuseTexture2,                                           boParallaxMapping];
  if (dt2=0) and (stc=1) and (la=1) and (pm=0) then GLBump1.BumpOptions:=[                  boUseSecondaryTexCoords,boLightAttenuation                  ];
  if (dt2=0) and (stc=1) and (la=0) and (pm=0) then GLBump1.BumpOptions:=[                  boUseSecondaryTexCoords                                     ];
  if (dt2=0) and (stc=0) and (la=1) and (pm=0) then GLBump1.BumpOptions:=[                                          boLightAttenuation                  ];
  if (dt2=0) and (stc=0) and (la=0) and (pm=0) then GLBump1.BumpOptions:=[                                                                              ];
  result:=1;
end;

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
  mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
  param := TGLSLShaderParameter(trunc64(par));
  param.UniformType := uniformTexture2D;
  param.Texture := mat.Material.Texture;
  param.UniformTexture := trunc64(texUnit);
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
