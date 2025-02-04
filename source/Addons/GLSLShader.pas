unit GLSLShader;

interface
{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,
  GLS.Context,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.Material,
  GLS.Utils,
  GLS.RenderContextInfo,
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.PipelineTransformation,
  GLS.FBORenderer,
  GLShadowMap,
  TypInfo,
  Variants;

const
  // GL_ARB_seamless_cube_map required 
  GL_TEXTURE_CUBE_MAP_SEAMLESS = $884F;

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
      uniformShadowMatrix,
      uniformViewMatrix,
      uniformInvViewMatrix,
      uniformHaveTexture,
      uniformMaterialProperty,
      uniformFogEnabled,
      uniformLightingEnabled
    );
  
  TGLSLShader = class;

  TGLSLShaderParameter = class(TCollectionItem)
    protected
      FName: String;
      FInitialized: Boolean;
      FTexture: TGLTexture;
      FTextureTarget: TGLEnum;
      FShadowMap: TGLShadowMap;
      FFBORenderer: TGLFBORenderer;
      FUniformLocation: Integer;
      FUniformType: TGLSLShaderParameterType;
      FUniformInteger: Integer;
      FUniformTexture: Integer;
      FUniformVector: array[0..3] of Single;
      FUniformMatrix: TMatrix4f;
      FMaterialPropertyName: String;
      procedure SetVecElem(index: Integer; const val: Single);
      function GetVecElem(index: Integer): Single;
    public
      constructor Create(ACollection: TCollection); override;
      procedure Init(paramType: TGLSLShaderParameterType; name: String);
      procedure Bind(mat: TGLLibMaterial; shader: TGLSLShader; var rci: TGLRenderContextInfo);
      procedure Unbind(shader: TGLSLShader);
      property UniformVector[index: Integer]: Single read GetVecElem write SetVecElem;
      property UniformMatrix: TMatrix4f read FUniformMatrix write FUniformMatrix;
      property UniformType: TGLSLShaderParameterType read FUniformType write FUniformType;
      property Name: String read FName write FName;
      property UniformInteger: Integer read FUniformInteger write FUniformInteger;
      property UniformTexture: Integer read FUniformTexture write FUniformTexture;
      property Texture: TGLTexture read FTexture write FTexture;
      property TextureTarget: TGLEnum read FTextureTarget write FTextureTarget;
      property ShadowMap: TGLShadowMap read FShadowMap write FShadowMap;
      property FBORenderer: TGLFBORenderer read FFBORenderer write FFBORenderer;
      property MaterialPropertyName: String read FMaterialPropertyName write FMaterialPropertyName;
      property Initialized: Boolean read FInitialized write FInitialized;
      function GetMaterialField(mat: TGLLibMaterial; name: string): variant;
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
      function AddUniformViewMatrix(name: String): TGLSLShaderParameter;
      procedure Bind(mat: TGLLibMaterial; var rci: TGLRenderContextInfo);
      procedure Unbind;
  end;

  TGLSLShader = class(TGLShader)
    private
      Parameters: TGLSLShaderParameters;
      vps: PAnsiChar;
      fps: PAnsiChar;
      haveSrc: Boolean;
      shaderSane: Boolean;
      shaderProg: GLenum;
      shaderVert: GLenum;
      shaderFrag: GLenum;
      FFogEnabled: Boolean;
      FLightingEnabled: Boolean;
      FForceDisableStencilTest: Boolean;
      FStencilTestState: Boolean;
    public
      constructor Create(AOwner: TComponent); override;
      procedure SetPrograms(vp, fp: PAnsiChar);
    protected
      procedure DoApply(var rci : TGLRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TGLRenderContextInfo) : Boolean; override;
      procedure DoInitialize(var rci: TGLRenderContextInfo; Sender: TObject); override;
    published
      property Param: TGLSLShaderParameters read Parameters;
      property Prog: GLenum read shaderProg;
      property FogEnabled: Boolean read FFogEnabled write FFogEnabled;
      property LightingEnabled: Boolean read FLightingEnabled write FLightingEnabled;
      property ForceDisableStencilTest: Boolean read FForceDisableStencilTest write FForceDisableStencilTest;
  end;

procedure Register;

implementation

uses
  GLS.State;

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

procedure TGLSLShaderParameter.Init(paramType: TGLSLShaderParameterType; name: string);
begin
   FUniformType := paramType;
   FName := name;
end;

function TGLSLShaderParameter.GetMaterialField(mat: TGLLibMaterial; name: string): variant;
begin
   result := GetPropValue(mat, name);
end;

procedure TGLSLShaderParameter.Bind(mat: TGLLibMaterial; shader: TGLSLShader; var rci: TGLRenderContextInfo);
var
  propValue: variant;
  i, hasTexture: integer;
begin
  if not FInitialized then
    Exit;

  if FUniformLocation = 0 then
    FUniformLocation := gl.GetUniformLocation(shader.Prog, PAnsiChar(AnsiString(FName)));

  if FUniformType = uniform1i then
    gl.Uniform1i(FUniformLocation, FUniformInteger);

  if FUniformType = uniformTexture2D then
  begin
    if Assigned(FTexture) then
    begin
      gl.ActiveTexture(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      gl.BindTexture(cGLTexTypeToGLEnum[FTexture.Image.NativeTextureTarget], FTexture.Handle);
      gl.ActiveTexture(GL_TEXTURE0_ARB);
    end;
    gl.Uniform1i(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformSecondTexture2D then
  begin
    if Assigned(FTexture) then
    begin
      gl.ActiveTexture(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
      gl.BindTexture(cGLTexTypeToGLEnum[FTexture.Image.NativeTextureTarget], FTexture.Handle);
      gl.ActiveTexture(GL_TEXTURE0_ARB);
    end;
    gl.Uniform1i(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformShadowTexture then
  begin
    if FShadowMap <> Nil then
    begin
       FTexture := TGLMaterialLibrary(FShadowMap.FBO.MaterialLibrary).TextureByName(FShadowMap.FBO.DepthTextureName);
       if FTexture <> Nil then
       begin
         gl.ActiveTexture(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
         gl.BindTexture(cGLTexTypeToGLEnum[FTexture.Image.NativeTextureTarget], FTexture.Handle);
         gl.TexParameteri(cGLTexTypeToGLEnum[FTexture.Image.NativeTextureTarget], GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
         gl.ActiveTexture(GL_TEXTURE0_ARB);
       end;
    end;
    gl.Uniform1i(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformFBOColorTexture then
  begin
    if FFBORenderer <> Nil then
    begin
      FTexture := TGLMaterialLibrary(FFBORenderer.MaterialLibrary).TextureByName(FFBORenderer.ColorTextureName);
      if FTexture <> Nil then
      begin
        gl.ActiveTexture(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
        gl.BindTexture(cGLTexTypeToGLEnum[FTexture.Image.NativeTextureTarget], FTexture.Handle);
        gl.ActiveTexture(GL_TEXTURE0_ARB);
      end;
    end;
    gl.Uniform1i(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformFBODepthTexture then
  begin
    if FFBORenderer <> Nil then
    begin
      FTexture := TGLMaterialLibrary(FFBORenderer.MaterialLibrary).TextureByName(FFBORenderer.DepthTextureName);
      if FTexture <> Nil then
      begin
        gl.ActiveTexture(GL_TEXTURE0_ARB + GLUint(FUniformTexture));
        gl.BindTexture(cGLTexTypeToGLEnum[FTexture.Image.NativeTextureTarget], FTexture.Handle);
        gl.ActiveTexture(GL_TEXTURE0_ARB);
      end;
    end;
    gl.Uniform1i(FUniformLocation, FUniformTexture);
  end;

  if FUniformType = uniformShadowMatrix then
  begin
    if FShadowMap <> Nil then
      FUniformMatrix := FShadowMap.ShadowMatrix;
    gl.UniformMatrix4fv(FUniformLocation, 1, false, @FUniformMatrix);
  end;

  if FUniformType = uniformViewMatrix then
  begin
    FUniformMatrix := rci.PipelineTransformation.ModelViewMatrix^;
    gl.UniformMatrix4fv(FUniformLocation, 1, false, @FUniformMatrix);
  end;

  if FUniformType = uniformInvViewMatrix then
  begin
    FUniformMatrix := rci.PipelineTransformation.ModelViewMatrix^;
    InvertMatrix(FUniformMatrix);
    gl.UniformMatrix4fv(FUniformLocation, 1, false, @FUniformMatrix);
  end;

  if FUniformType = uniformHaveTexture then
  begin
    hasTexture := 0;
    for i := 0 to mat.Material.TextureEx.Count - 1 do begin
      if mat.Material.TextureEx.Items[i].TextureIndex = FUniformInteger then begin
       hasTexture := 1;
       break;
      end;
    end;
    glUniform1iARB(FUniformLocation, hasTexture);
  end;

  if FUniformType = uniformFogEnabled then
  begin
    gl.Uniform1i(FUniformLocation, Integer(shader.FogEnabled));
  end;

  if FUniformType = uniformLightingEnabled then
  begin
    gl.Uniform1i(FUniformLocation, Integer(shader.LightingEnabled));
  end;

  {
  // TODO
  if FUniformType = uniformMaterialProperty then
  begin
    //FUniformMatrix := rci.modelViewMatrix^;
    //InvertMatrix(FUniformMatrix);
    //glUniformMatrix4fvARB(FUniformLocation, 1, false, @FUniformMatrix[0]);
    propValue := GetMaterialField(mat, FMaterialPropertyName);
    if (VarType(propValue) and varTypeMask) = varSingle then
    begin
    end;
  end;
  }

  if FUniformType = uniform1f then
    gl.Uniform1f(FUniformLocation, FUniformVector[0]);

  if FUniformType = uniform2f then
    gl.Uniform2f(FUniformLocation,
      FUniformVector[0], FUniformVector[1]);

  if FUniformType = uniform3f then
    gl.Uniform3f(FUniformLocation,
      FUniformVector[0], FUniformVector[1], FUniformVector[2]);

  if FUniformType = uniform4f then
    gl.Uniform4f(FUniformLocation,
      FUniformVector[0], FUniformVector[1], FUniformVector[2], FUniformVector[3]);

  if FUniformType = uniformMatrix4f then
    gl.UniformMatrix4fv(FUniformLocation, 1, false, @FUniformMatrix);
end;

procedure TGLSLShaderParameter.Unbind(shader: TGLSLShader);
begin
  if not FInitialized then
    Exit;
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
  param.TextureTarget := GL_TEXTURE_2D;
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

function TGLSLShaderParameters.AddUniformViewMatrix(name: String): TGLSLShaderParameter;
var
  param: TGLSLShaderParameter;
begin
  param := Add as TGLSLShaderParameter;
  param.Init(uniformViewMatrix, name);
  param.Initialized := True;
  Result := param;
end;


procedure TGLSLShaderParameters.Bind(mat: TGLLibMaterial; var rci: TGLRenderContextInfo);
var
  i: Integer;
  p: TGLSLShaderParameter;
begin
  for i := 0 to Count-1 do
  begin
    p := TGLSLShaderParameter(inherited Items[i]);
    p.Bind(mat, FShader, rci);
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
   FForceDisableStencilTest := false;
end;

procedure TGLSLShader.DoInitialize;
var
   p: PAnsiChar;
   pl: Integer;
   success: Integer;
   maxLength: Integer;
   log: AnsiString;
begin
   if not haveSrc then
       Exit;

   shaderProg := gl.CreateProgram();
   shaderVert := gl.CreateShader(GL_VERTEX_SHADER_ARB);
   shaderFrag := gl.CreateShader(GL_FRAGMENT_SHADER_ARB);

   pl := -1;
   p := PAnsiChar(AnsiString(vps));
   gl.ShaderSource(shaderVert, 1, @p, nil);
   p := PAnsiChar(AnsiString(fps));
   gl.ShaderSource(shaderFrag, 1, @p, nil);

   gl.CompileShader(shaderVert);
   gl.CompileShader(shaderFrag);
   gl.AttachShader(shaderProg, shaderVert);
   gl.AttachShader(shaderProg, shaderFrag);
   gl.LinkProgram(shaderProg);

   shaderSane := true;

   success := 0;
   gl.GetObjectParameteriv(shaderVert, GL_OBJECT_COMPILE_STATUS_ARB, @success);
   if success = 0 then
   begin
     shaderSane := false;
     maxLength := 0;
     gl.GetObjectParameteriv(shaderVert, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
     SetLength(log, maxLength);
     gl.GetInfoLog(shaderVert, maxLength, @maxLength, @log[1]);
     SetLength(log, maxLength);
     ShowMessage('GLSL vertex shader error: ' + #13#10 + String(log));
   end;

   success := 0;
   gl.GetObjectParameteriv(shaderFrag, GL_OBJECT_COMPILE_STATUS_ARB, @success);
   if success = 0 then
   begin
     shaderSane := false;
     maxLength := 0;
     gl.GetObjectParameteriv(shaderFrag, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
     SetLength(log, maxLength);
     gl.GetInfoLog(shaderVert, maxLength, @maxLength, @log[1]);
     SetLength(log, maxLength);
     ShowMessage('GLSL fragment shader error: ' + #13#10 + String(log));
   end;

   // TODO: make this switchable
   gl.Enable(GL_TEXTURE_CUBE_MAP_SEAMLESS);
end;

procedure TGLSLShader.SetPrograms(vp, fp: PAnsiChar);
begin
    vps := vp;
    fps := fp;
    haveSrc := true;
end;

procedure TGLSLShader.DoApply(var rci: TGLRenderContextInfo; Sender : TObject);
var
  mat: TGLLibMaterial;
begin
  mat := TGLLibMaterial(Sender);
  if not (csDesigning in ComponentState) and shaderSane then
  begin
      FFogEnabled := gl.IsEnabled(GL_FOG);
      FLightingEnabled := gl.IsEnabled(GL_LIGHTING);
      if FForceDisableStencilTest then begin
        FStencilTestState := gl.IsEnabled(GL_STENCIL_TEST);
        gl.Disable(GL_STENCIL_TEST);
      end;
      gl.UseProgram(shaderProg);
      Parameters.Bind(mat, rci);
  end;
end;

function TGLSLShader.DoUnApply(var rci: TGLRenderContextInfo): Boolean;
begin
  if not (csDesigning in ComponentState) and shaderSane then
  begin
      Parameters.Unbind;
      gl.UseProgram(0);
      if FForceDisableStencilTest then begin
        if FStencilTestState then
          gl.Enable(GL_STENCIL_TEST);
      end;
  end;
  Result:=False;
end;

end.

