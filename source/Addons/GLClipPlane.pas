unit GLClipPlane;

// TODO: multiple clip planes support

interface

uses
  Winapi.OpenGL,
  Classes,
  GLS.Context,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.Texture,
  GLS.Utils,
  GLS.RenderContextInfo;

type

  TGLClipPlane = class(TGLSceneObject)
    private
        { Private Declarations }
        FRendering : Boolean;
    protected
        { Protected Declarations }
        FClipPlane: TDoubleHmgPlane;
        FClipPlaneEnabled: Boolean;
        FClipPlaneID: TGLEnum;
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren : Boolean); override;
        procedure SetClipPlane(point, normal: TAffineVector);
        property ClipPlaneEnabled: Boolean read FClipPlaneEnabled write FClipPlaneEnabled;
        property ClipPlaneID: TGLEnum read FClipPlaneID;
  end;

implementation

var
  MaxClipPlaneID: TGLEnum;

constructor TGLClipPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClipPlaneEnabled := False;
  FClipPlaneID := MaxClipPlaneID;
  MaxClipPlaneID := MaxClipPlaneID + 1;
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

procedure TGLClipPlane.DoRender(var rci: TGLRenderContextInfo;
                                renderSelf, renderChildren : Boolean);
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      if FClipPlaneEnabled and (FClipPlaneID < 6) then
      begin
        gl.Enable(GL_CLIP_PLANE0 + FClipPlaneID);
        gl.ClipPlane(GL_CLIP_PLANE0 + FClipPlaneID, @FClipPlane);
      end;

      if renderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, rci);

      if FClipPlaneEnabled and (FClipPlaneID < 6) then
      begin
         gl.Disable(GL_CLIP_PLANE0 + FClipPlaneID);
      end;
   finally
      FRendering:=False;
   end;
end;

end.
