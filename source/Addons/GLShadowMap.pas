unit GLShadowMap;

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
  GLS.Context,
  GLShadowCamera;

type

  TGLShadowMap = class(TComponent)
    private
        { Private Declarations }
    protected
        { Protected Declarations }
        FShadowMatrix: TMatrix4f;
        FFBO: TGLFBORenderer;
        FViewer: TGLSceneViewer;
        FShadowCamera: TGLShadowCamera;
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent; fbo: TGLFBORenderer; viewer: TGLSceneViewer; camera: TGLShadowCamera) overload;
        destructor Destroy; override;
        property ShadowMatrix: TMatrix4f read FShadowMatrix;
        property FBO: TGLFBORenderer read FFBO write FFBO;
        property Viewer: TGLSceneViewer read FViewer write FViewer;
        property ShadowCamera: TGLShadowCamera read FShadowCamera write FShadowCamera;
        procedure Update;
  end;

implementation

constructor TGLShadowMap.Create(AOwner: TComponent; fbo: TGLFBORenderer; viewer: TGLSceneViewer; camera: TGLShadowCamera);
begin
  inherited Create(AOwner);
  FShadowMatrix := IdentityHmgMatrix;
  FFBO := fbo;
  FViewer := viewer;
  FShadowCamera := camera;
end;

destructor TGLShadowMap.Destroy;
begin
  inherited Destroy;
end;

function cameraGetViewMatrix(camera: TGLCamera): TMatrix4f;
var
  v, d, v2: TGLVector;
  absPos: TGLVector;
  LM, mat: TGLMatrix;
begin
  if Assigned(camera.TargetObject) then begin
    v := camera.TargetObject.AbsolutePosition;
    absPos := camera.AbsolutePosition;
    VectorSubtract(v, absPos, d);
    NormalizeVector(d);
    LM := CreateLookAtMatrix(absPos, v, camera.Up.AsVector);
  end
  else begin
    if Assigned(camera.Parent) then
      mat := camera.Parent.AbsoluteMatrix
    else
      mat := IdentityHmgMatrix;
    absPos := camera.AbsolutePosition;
    v := VectorTransform(camera.Direction.AsVector, mat);
    d := VectorTransform(camera.Up.AsVector, mat);
    v2 := VectorAdd(absPos, v);
    LM := CreateLookAtMatrix(absPos, v2, d);
  end;
  result := LM;
end;

procedure TGLShadowMap.Update();
var
  FInvCameraMatrix, FShadowCameraMatrix: TGLMatrix;
  FProjectionMatrix, FBiasMatrix: TMatrix4f;
begin
  FProjectionMatrix := CreateOrthoMatrix(
    -FShadowCamera.ProjectionSize, FShadowCamera.ProjectionSize,
    -FShadowCamera.ProjectionSize, FShadowCamera.ProjectionSize,
     FShadowCamera.ZNear, FShadowCamera.ZFar);

  FInvCameraMatrix := cameraGetViewMatrix(viewer.Camera);
  InvertMatrix(FInvCameraMatrix);
  FShadowCameraMatrix := cameraGetViewMatrix(FShadowCamera);
  //InvertMatrix(FShadowCameraMatrix);
  //FShadowMatrix := MatrixMultiply(FInvCameraMatrix, CreateScaleMatrix(AffineVectorMake(ZScale, ZScale, ZScale)));

  {
  FShadowMatrix := CreateScaleAndTranslationMatrix(VectorMake(0.5, 0.5, 0.5), VectorMake(0.5, 0.5, 0.5));
  FShadowMatrix := MatrixMultiply(FProjectionMatrix, FShadowMatrix);
  FShadowMatrix := MatrixMultiply(FShadowCameraMatrix, FShadowMatrix);
  FShadowMatrix := MatrixMultiply(FInvCameraMatrix, FShadowMatrix);
  }

  FShadowMatrix := MatrixMultiply(FInvCameraMatrix, FShadowCameraMatrix);
  FShadowMatrix := MatrixMultiply(FShadowMatrix, FProjectionMatrix);
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

end.
