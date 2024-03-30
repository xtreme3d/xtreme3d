unit GLShadowMap;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.RenderContextInfo,
  GLS.FBORenderer;

type

  TGLShadowMap = class(TComponent)
    private
        { Private Declarations }
    protected
        { Protected Declarations }
        FShadowMatrix: TMatrix4f;
        FZScale: Single;
        FFBO: TGLFBORenderer;
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent; fbo: TGLFBORenderer) overload;
        destructor Destroy; override;
        property ShadowMatrix: TMatrix4f read FShadowMatrix;
        property ZScale: Single read FZScale write FZScale;
        property FBO: TGLFBORenderer read FFBO write FFBO;
        procedure LightFBORendererBeforeRender(Sender: TObject; var rci: TGLRenderContextInfo);
        procedure LightFBORendererAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
  end;

implementation

constructor TGLShadowMap.Create(AOwner: TComponent; fbo: TGLFBORenderer);
begin
  inherited Create(AOwner);
  FZScale := 1.0;
  FShadowMatrix := IdentityHmgMatrix;
  FFBO := fbo;
  FFBO.BeforeRender := LightFBORendererBeforeRender;
  FFBO.AfterRender := LightFBORendererAfterRender;
end;

destructor TGLShadowMap.Destroy;
begin
  inherited Destroy;
end;

procedure TGLShadowMap.LightFBORendererBeforeRender(Sender: TObject; var rci: TGLRenderContextInfo);
var
  FInvCameraMatrix: TGLMatrix;
  FBiasMatrix: TMatrix4f;
begin
  // get the modelview and projection matrices from the light's "camera"
  FInvCameraMatrix := rci.PipelineTransformation.InvModelViewMatrix^;
  FShadowMatrix := MatrixMultiply(CreateScaleMatrix(AffineVectorMake(ZScale, ZScale, ZScale)), FInvCameraMatrix);
  FShadowMatrix := MatrixMultiply(FShadowMatrix, rci.PipelineTransformation.ModelViewMatrix^);
  FShadowMatrix := MatrixMultiply(FShadowMatrix, rci.PipelineTransformation.ProjectionMatrix^);
  FBiasMatrix := CreateScaleAndTranslationMatrix(VectorMake(0.5, 0.5, 0.5), VectorMake(0.5, 0.5, 0.5));
  FShadowMatrix := MatrixMultiply(FShadowMatrix, FBiasMatrix);

  // push geometry back a bit, prevents false self-shadowing
  {
  with rci.GLStates do
  begin
    Enable(stPolygonOffsetFill);
    PolygonOffsetFactor := 2;
    PolygonOffsetUnits := 2;
  end;
  }
end;

procedure TGLShadowMap.LightFBORendererAfterRender(Sender: TObject; var rci: TGLRenderContextInfo);
begin
  //rci.GLStates.Disable(stPolygonOffsetFill);
end;

end.
