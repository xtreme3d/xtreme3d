unit GLClippingPlane;

// TODO: multiple clip planes support

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils;

type

  TGLClipPlane = class(TGLSceneObject)
    private
        { Private Declarations }
        FRendering : Boolean;
    protected
        { Protected Declarations }
        FClipPlane: TDoubleHmgPlane;
        FClipPlaneEnabled: Boolean;
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
        procedure SetClipPlane(point, normal: TAffineVector);
        property ClipPlaneEnabled: Boolean read FClipPlaneEnabled write FClipPlaneEnabled;
  end;

implementation

constructor TGLClipPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClipPlaneEnabled := False;
  SetClipPlane(AffineVectorMake(0, 0, 0), AffineVectorMake(0, 1, 0));
end;

destructor TGLClipPlane.Destroy;
begin
  inherited Destroy;
end;

procedure TGLClipPlane.SetClipPlane(point, normal: TAffineVector);
begin
  SetPlane(FClipPlane, PlaneMake(point, normal));
end;

procedure TGLClipPlane.DoRender(var rci : TRenderContextInfo;
                                renderSelf, renderChildren : Boolean);
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      if FClipPlaneEnabled then
      begin
        glEnable(GL_CLIP_PLANE0);
        glClipPlane(GL_CLIP_PLANE0, @FClipPlane);
      end;

      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);

      if FClipPlaneEnabled then
      begin
         glDisable(GL_CLIP_PLANE0);
      end;
   finally
      FRendering:=False;
   end;
end;

end.
