unit GLFBORendererEx;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.Texture,
  GLS.Material,
  GLS.FBORenderer,
  GLS.State,
  GLS.Context,
  GLS.RenderContextInfo,
  GLS.Utils;

type

  TGLFBORendererEx = class(TGLFBORenderer)
    private
        { Private Declarations }
    protected
        { Protected Declarations }
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent); override;
        procedure OnBeforeRender(fbo: TObject; var rci: TGLRenderContextInfo);
        procedure OnAfterRender(fbo: TObject; var rci: TGLRenderContextInfo);
  end;

implementation

constructor TGLFBORendererEx.Create(AOwner: TComponent);
begin
  inherited;
  BeforeRender := OnBeforeRender;
  AfterRender := OnAfterRender;
end;

procedure TGLFBORendererEx.OnBeforeRender(fbo: TObject; var rci: TGLRenderContextInfo);
var
  buffer: TGLSceneBuffer;
begin
  with rci.GLStates do
  begin
    // push geometry back a bit, prevents false self-shadowing
    //Enable(stPolygonOffsetFill);
    //SetPolygonOffset(0.0, 1.0);
    //Disable(stCullFace);
    //Enable(stCullFace);
    //CullFaceMode := cmFront;
    //DepthFunc := cfLEqual;
  end;
end;

procedure TGLFBORendererEx.OnAfterRender(fbo: TObject; var rci: TGLRenderContextInfo);
begin
  with rci.GLStates do
  begin
    //SetPolygonOffset(0.0, 0.0);
    //Disable(stPolygonOffsetFill);
    //Enable(stCullFace);
    //CullFaceMode := cmBack;
    //DepthFunc := cfLess;
  end;
end;

end.
