unit GLShadowCamera;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.RenderContextInfo,
  GLS.FBORenderer,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Context;

type

  TGLShadowCamera = class(TGLCamera)
    private
        { Private Declarations }
    protected
        { Protected Declarations }
        FProjectionSize: Single;
        FZNear: Single;
        FZFar: Single;
    public
        { Public Declarations }
        constructor Create(aOwner: TComponent); override;
        property ProjectionSize: Single read FProjectionSize write FProjectionSize;
        property ZNear: Single read FZNear write FZNear;
        property ZFar: Single read FZFar write FZFar;
        procedure ApplyShadowProjection(const viewport: TRectangle; width, height: Integer; DPI: Integer; var viewPortRadius: Single);
  end;

implementation

constructor TGLShadowCamera.Create(aOwner: TComponent);
begin
  inherited;
  CameraStyle := csCustom;
  FProjectionSize := 20.0;
  FZNear := -1000.0;
  FZFar := 1000.0;
  OnCustomPerspective := ApplyShadowProjection;
end;

procedure TGLShadowCamera.ApplyShadowProjection(const viewport: TRectangle; width, height: Integer; DPI: Integer; var viewPortRadius: Single);
var
  mat: TGLMatrix;
begin
  //viewPortRadius := FZFar;
  mat := CreateOrthoMatrix(-FProjectionSize, FProjectionSize, -FProjectionSize, FProjectionSize, FZNear, FZFar);
  with CurrentGLContext.PipelineTransformation do
      SetProjectionMatrix(MatrixMultiply(mat, ProjectionMatrix^));
end;

end.
