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
begin
  // push geometry back a bit, prevents false self-shadowing
  with rci.GLStates do
  begin
    Enable(stPolygonOffsetFill);
    PolygonOffsetFactor := 1;
    PolygonOffsetUnits := 1;
  end;
  gl.CullFace(GL_FRONT);
end;

procedure TGLFBORendererEx.OnAfterRender(fbo: TObject; var rci: TGLRenderContextInfo);
begin
  with rci.GLStates do
  begin
    Disable(stPolygonOffsetFill);
  end;
  gl.CullFace(GL_BACK);
end;

end.
