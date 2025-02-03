unit GLShadowCamera;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  GLS.VectorTypes,
  GLS.VectorGeometry,
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
  OnCustomPerspective := ApplyShadowProjection;
  CameraStyle := csCustom;
  FProjectionSize := 5.0;
  FZNear := -100.0;
  FZFar := 100.0;
end;

procedure TGLShadowCamera.ApplyShadowProjection(const viewport: TRectangle; width, height: Integer; DPI: Integer; var viewPortRadius: Single);
var
  mat: TGLMatrix;
begin
  mat := CreateOrthoMatrix(-FProjectionSize, FProjectionSize, -FProjectionSize, FProjectionSize, FZNear, FZFar);
  with CurrentGLContext.PipelineTransformation do
      SetProjectionMatrix(mat);
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

RegisterClasses([TGLShadowCamera]);

end.
