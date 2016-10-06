unit GLSLShader;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLTexture, GLUserShader, OpenGL1x, GLUtils,
  GLShadowMap, GLFBO;

type
  TGLSLShaderParameterType = (
      uniform1i,
      uniform1f,
      uniform2f,
      uniform3f,
      uniform4f, 
      uniformMatrix4f,
      uniformTexture2D,
      uniformSecondTexture2D,
      uniformShadowTexture,
      uniformFBOColorTexture,
      uniformFBODepthTexture,
      uniformShadowMatrix
    );

  TGLSLShader = class;

  TGLSLShaderParameter = class(TCollectionItem)
    protected
      FName: String;
      FInitialized: Boolean;
      FTexture: TGLTexture;
      FShadowMap: TGLShadowMap;
      FFBO: TGLFBO;
      FUniformLocation: GLint;
      FUniformType: TGLSLShaderParameterType;
      FUniformInteger: Integer;
      FUniformTexture: Integer;
      FUniformVector: array[0..3] of Single;
      FUniformMatrix: TMatrix;
      procedure SetVecElem(index: Integer; const val: Single);
      function GetVecElem(index: Integer): Single;
    public
      constructor Create(ACollection: TCollection); override;
      procedure Init(paramType: TGLSLShaderParameterType; name: String);
      procedure Bind(mat: TGLLibMaterial; shader: TGLSLShader);
      procedure Unbind(shader: TGLSLShader);
      property UniformVector[index: Integer]: Single read GetVecElem write SetVecElem;
      property UniformMatrix: TMatrix4f read FUniformMatrix write FUniformMatrix;
      property UniformType: TGLSLShaderParameterType read FUniformType write FUniformType;
      property Name: String read FName write FName;
      property UniformInteger: Integer read FUniformInteger write FUniformInteger;
      property UniformTexture: Integer read FUniformTexture write FUniformTexture;
      property Texture: TGLTexture read FTexture write FTexture;
      property ShadowMap: TGLShadowMap read FShadowMap write FShadowMap;
      property FBO: TGLFBO read FFBO write FFBO;
      property Initialized: Boolean read FInitialized write FInitialized;
  end;

  TGLSLShaderParameters = class(TCollection)
    protected
      FShader: TGLSLShader;
      FParamCount: Integer;
      procedure SetItems(index: Integer; const val: TGLSLShaderParameter);
      function GetItems(index: Integer): TGLSLShaderParameter;
    public
      constructor Create(AOwner: TGLSLShader);
      property Items[index: Integer]: TGLSLShaderParameter read GetItems write SetItems; default;
      function AddUniform(name: String):   TGLSLShaderParameter;
      function AddUniform1i(name: String): TGLSLShaderParameter;
      function AddUniform1f(name: String): TGLSLShaderParameter;
      function AddUniform2f(name: String): TGLSLShaderParameter;
      function AddUniform3f(name: String): TGLSLShaderParameter;
      function AddUniform4f(name: String): TGLSLShaderParameter;
      function AddUniformTexture2D(name: String): TGLSLShaderParameter;
      function AddUniformSecondTexture2D(name: String): TGLSLShaderParameter;
      function AddUniformShadowTexture(name: String): TGLSLShaderParameter;
      function AddUniformFBOColorTexture(name: String): TGLSLShaderParameter;
      function AddUniformFBODepthTexture(name: String): TGLSLShaderParameter;
      procedure Bind(mat: TGLLibMaterial);
      procedure Unbind;
  end;

  TGLSLShader = class(TGLShader)
    private
      Parameters: TGLSLShaderParameters;
      vps: pchar;
      fps: pchar;
      haveSrc: Boolean;
      shaderSane: Boolean;
      shaderProg: GLenum;
      shaderVert: GLenum;
      shaderFrag: GLenum;
    public
      constructor Create(AOwner: TComponent); override;
      procedure SetPrograms(vp, fp: PChar);
    protected
      procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
      procedure DoInitialize; override;
    published
      property Param: TGLSLShaderParameters read Parameters;
      property Prog: GLenum read shaderProg;
  end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('GLScene Shaders', [TGLSLShader]);
end;

constructor TGLSLShaderParameter.Create(ACollection: TCollection);
begin
  inherited;
  FUniformInteger := 0;
  FUniformTexture := 0;
  FUniformVector[0] := 0.0;
  FUniformVector[1] := 0.0;
  FUniformVector[2] := 0.0;
  FUniformVector[3] := 0.0;
  FUniformMatrix := IdentityHmgMatrix;
  FShadowMap := nil;
  FInitialized := False;
end;

procedure TGLSLShaderParameter.SetVecElem(index: Integer; const val: Single);
begin
  FUniformVector[index] := val;
end;

function TGLSLShaderParameter.GetVecElem(index: Integer): Single;
begin
  Result := FUniformVector[index];
end;

procedure TGLSLShaderParameter.Init(paramType: TGLSLShaderParameterType; name: String);
begin
  FUniformType := paramType;
  FName := name;
end;

procedure TGLSLShaderParameter.Bind(mat: TGLLibMaterial; shader: TGLSLShader);
begin
  if not FInitialized then
    Exit;
    
  if FUniformLocation = 0 then
    FUniformLocation := glGetUniformLocationARB(shader.Prog, PChar(FName));
    
  if FUniformType = uniform1i then
    glUniform1iARB(FUniformLocation, FUniformInteger);

  if FUniformType = uniformTexture2D then
  begin
    if FTexture <> Nil then
    begin    
      glActiveTextureARB(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      glBindTexture(FTexture.Image.NativeTextureTarget, FTexture.Handle);
      glActiveTextureARB(GL_TEXTURE0_ARB);
    end;
    glUniform1iARB(FUniformLocation, FUniformTexture);
  end;
  
  if FUniformType = uniformSecondTexture2D then
  begin
    if FTexture <> Nil then
    begin    
      glActiveTextureARB(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      glBindTexture(FTexture.Image.NativeTextureTarget, FTexture.Handle);
      glActiveTextureARB(GL_TEXTURE0_ARB);
    end;
    glUniform1iARB(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformShadowTexture then
  begin
    if FShadowMap <> Nil then
    begin
      glActiveTextureARB(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      glBindTexture(GL_TEXTURE_2D, FShadowMap.DepthTextureHandle);
      glActiveTextureARB(GL_TEXTURE0_ARB);
    end;
    glUniform1iARB(FUniformLocation, FUniformTexture);
  end;
  
  if FUniformType = uniformFBOColorTexture then
  begin
    if FFBO <> Nil then
    begin
      glActiveTextureARB(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      glBindTexture(GL_TEXTURE_2D, FFBO.ColorTextureHandle);
      glActiveTextureARB(GL_TEXTURE0_ARB);
    end;
    glUniform1iARB(FUniformLocation, FUniformTexture);
  end;
  
  if FUniformType = uniformFBODepthTexture then
  begin
    if FFBO <> Nil then
    begin
      glActiveTextureARB(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      glBindTexture(GL_TEXTURE_2D, FFBO.DepthTextureHandle);
      glActiveTextureARB(GL_TEXTURE0_ARB);
    end;
    glUniform1iARB(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformShadowMatrix then
  begin
    if FShadowMap <> Nil then
    begin
      FUniformMatrix := FShadowMap.ShadowMatrix;
    end;
    glUniformMatrix4fvARB(FUniformLocation, 1, false, @FUniformMatrix[0]);
  end;

  if FUniformType = uniform1f then
    glUniform1fARB(FUniformLocation, FUniformVector[0]);

  if FUniformType = uniform2f then
    glUniform2fARB(FUniformLocation,
      FUniformVector[0], FUniformVector[1]);

  if FUniformType = uniform3f then
    glUniform3fARB(FUniformLocation,
      FUniformVector[0], FUniformVector[1], FUniformVector[2]);
      
  if FUniformType = uniform4f then
    glUniform4fARB(FUniformLocation,
      FUniformVector[0], FUniformVector[1], FUniformVector[2], FUniformVector[3]);

  if FUniformType = uniformMatrix4f then
    glUniformMatrix4fvARB(FUniformLocation, 1, false, @FUniformMatrix[0]);
end;

procedure TGLSLShaderParameter.Unbind(shader: TGLSLShader);
begin
  if not FInitialized then
    Exit;
  if FUniformType = uniformTexture2D then
  begin
    if FTexture <> Nil then
    begin
      glActiveTextureARB(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      glBindTexture(FTexture.Image.NativeTextureTarget, 0);
      glActiveTextureARB(GL_TEXTURE0_ARB);
    end;
  end;
end;

constructor TGLSLShaderParameters.Create(AOwner: TGLSLShader);
begin
  inherited Create(TGLSLShaderParameter);
  FShader:=AOwner;
  FParamCount:=0;
end;

procedure TGLSLShaderParameters.SetItems(index: Integer; const val: TGLSLShaderParameter);
begin
	inherited Items[index]:=val;
end;

function TGLSLShaderParameters.GetItems(index: Integer): TGLSLShaderParameter;
begin
	Result:=TGLSLShaderParameter(inherited Items[index]);
end;

function TGLSLShaderParameters.AddUniform(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Name := name;
  param.Initialized := False;
  Result := param;
end;

function TGLSLShaderParameters.AddUniform1i(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniform1i, name);
  param.Initialized := True;
  Result := param;
end;

function TGLSLShaderParameters.AddUniform1f(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniform1f, name);
  param.Initialized := True;
  Result := param;
end;

function TGLSLShaderParameters.AddUniform2f(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniform2f, name);
  param.Initialized := True;
  Result := param;
end;

function TGLSLShaderParameters.AddUniform3f(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniform3f, name);
  param.Initialized := True;
  Result := param;
end;

function TGLSLShaderParameters.AddUniform4f(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniform4f, name);
  param.Initialized := True;
  Result := param;
end;

function TGLSLShaderParameters.AddUniformTexture2D(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniformTexture2D, name);
  param.Initialized := False;
  Result := param;
end;

function TGLSLShaderParameters.AddUniformSecondTexture2D(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniformSecondTexture2D, name);
  param.Initialized := False;
  Result := param;
end;

function TGLSLShaderParameters.AddUniformShadowTexture(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniformShadowTexture, name);
  param.Initialized := False;
  Result := param;
end;

function TGLSLShaderParameters.AddUniformFBOColorTexture(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniformFBOColorTexture, name);
  param.Initialized := False;
  Result := param;
end;

function TGLSLShaderParameters.AddUniformFBODepthTexture(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniformFBODepthTexture, name);
  param.Initialized := False;
  Result := param;
end;

procedure TGLSLShaderParameters.Bind(mat: TGLLibMaterial);
var
  i: Integer;
  p: TGLSLShaderParameter;
begin
  for i := 0 to Count-1 do
  begin
    p := TGLSLShaderParameter(inherited Items[i]);
    p.Bind(mat, FShader);
  end;
end;

procedure TGLSLShaderParameters.Unbind;
var
  i: Integer;
  p: TGLSLShaderParameter;
begin
  for i := 0 to Count-1 do
  begin
    p := TGLSLShaderParameter(inherited Items[i]);
    p.Unbind(FShader);
  end;
end;

constructor TGLSLShader.Create(AOwner:Tcomponent);
begin
   inherited;
   Parameters := TGLSLShaderParameters.Create(self);
   ShaderStyle := ssLowLevel;
   Enabled := true;
   haveSrc := false;
   shaderSane := false;
end;

procedure TGLSLShader.DoInitialize;
var
   pl: Integer;
   infobufferlen: Integer;
   infobuffer: array[0..1000] of Char;
begin
   if not haveSrc then
       Exit;
       
    //glDisable(GL_STENCIL_TEST);
       
   shaderProg := glCreateProgramObjectARB();
   shaderVert := glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
   shaderFrag := glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);

   pl:=-1;
   glShaderSourceARB(shaderVert, 1, @vps, @pl);
   glShaderSourceARB(shaderFrag, 1, @fps, @pl);

   glCompileShaderARB(shaderVert);
   glCompileShaderARB(shaderFrag);
   glAttachObjectARB(shaderProg, shaderVert);
   glAttachObjectARB(shaderProg, shaderFrag);
   glLinkProgramARB(shaderProg);

   infobufferlen := 0;
   FillChar(infobuffer, 1000, 0);

   glGetInfoLogARB(shaderVert, 999, @infobufferlen, infobuffer);
   if infobuffer[0] <> #0 then begin
       ShowMessage('GLSL vertex shader error: ' + #13#10 + String(infobuffer));
       Exit;
   end;

   infobufferlen := 0;
   FillChar(infobuffer, 1000, 0);

   glGetInfoLogARB(shaderFrag, 999, @infobufferlen, infobuffer);
   if infobuffer[0] <> #0 then begin
       ShowMessage('GLSL fragment shader error: ' + #13#10 + String(infobuffer));
       Exit;
   end;

   shaderSane:=true;
   
   //glEnable(GL_STENCIL_TEST);
end;

procedure TGLSLShader.SetPrograms(vp, fp: PChar);
begin
    vps := vp;
    fps := fp;
    haveSrc := true;
end;

procedure TGLSLShader.DoApply(var rci: TRenderContextInfo; Sender : TObject);
var
  mat: TGLLibMaterial;
begin
  mat := TGLLibMaterial(Sender);
  if not (csDesigning in ComponentState) and shaderSane then
  begin
      //glDisable(GL_STENCIL_TEST);
      glUseProgramObjectARB(shaderProg);
      Parameters.Bind(mat);
      //glEnable(GL_STENCIL_TEST);
  end;
end;

function TGLSLShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if not (csDesigning in ComponentState) and shaderSane then
  begin
      //glDisable(GL_STENCIL_TEST);
      Parameters.Unbind;
      glUseProgramObjectARB(0);
     // glEnable(GL_STENCIL_TEST);
  end;
  Result:=False;
end;

end.
