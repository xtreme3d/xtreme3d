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
        FShadowMapMode: Boolean;
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent); override;
        property ShadowMapMode: Boolean read FShadowMapMode write FShadowMapMode;
        procedure OnBeforeRender(fbo: TObject; var rci: TGLRenderContextInfo);
        procedure OnAfterRender(fbo: TObject; var rci: TGLRenderContextInfo);
  end;

implementation

constructor TGLFBORendererEx.Create(AOwner: TComponent);
begin
  inherited;
  FShadowMapMode := false;
  BeforeRender := OnBeforeRender;
  AfterRender := OnAfterRender;
end;

procedure TGLFBORendererEx.OnBeforeRender(fbo: TObject; var rci: TGLRenderContextInfo);
begin
  with rci.GLStates do
  begin
    if FShadowMapMode then begin
      Enable(stPolygonOffsetFill);
      SetPolygonOffset(3.0, 0.0);
      Disable(stCullFace);
    end;
  end;
end;

procedure TGLFBORendererEx.OnAfterRender(fbo: TObject; var rci: TGLRenderContextInfo);
begin
  with rci.GLStates do
  begin
    if FShadowMapMode then begin
      SetPolygonOffset(0.0, 0.0);
      Disable(stPolygonOffsetFill);
      Enable(stCullFace);
    end;
  end;
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

RegisterClasses([TGLFBORendererEx]);

end.
