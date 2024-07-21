unit GLHUDText2;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.BitmapFont,
  GLS.HUDObjects,
  GLS.Objects,
  GLS.RenderContextInfo,
  GLS.Context,
  GLS.State;

type

TGLHUDText2 = class(TGLHUDText)
  private
    FFont: TGLFont;
  protected
    procedure SetFont(const val: TGLFont);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;
  published
    property Font: TGLFont read FFont write SetFont;
end;

implementation

constructor TGLHUDText2.Create(AOwner: TComponent);
begin
   inherited;
end;

procedure TGLHUDText2.SetFont(const val: TGLFont);
begin
   if val<>FFont then begin
      if Assigned(FFont) then
         FFont.UnRegisterUser(Self);
      FFont:=val;
      if Assigned(FFont) then begin
         FFont.RegisterUser(Self);
         FFont.FreeNotification(Self);
      end;
      StructureChanged;
   end;
end;

procedure TGLHUDText2.DoRender(var rci : TGLRenderContextInfo; renderSelf, renderChildren: Boolean);
var
   f, X, Y, Z: Single;
begin
  if Assigned(FFont) and (Text <> '') then
  begin
    X := Position.X;
    Y := Position.Y;
    Z := Position.Z;

    rci.GLStates.PolygonMode := pmFill;
    // Prepare matrices
    gl.MatrixMode(GL_MODELVIEW);
    gl.PushMatrix;
    gl.LoadMatrixf(@TGLSceneBuffer(rci.buffer).BaseProjectionMatrix);
    f := rci.renderDPI / 96;
    gl.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);
    gl.Translatef(X * f - rci.viewPortSize.cx / 2, rci.viewPortSize.cy / 2 - Y * f, Z);
    if Rotation <> 0 then
      gl.Rotatef(Rotation, 0, 0, 1);
    gl.Scalef(Scale.DirectX * f, Scale.DirectY * f, 1);
    gl.MatrixMode(GL_PROJECTION);
    gl.PushMatrix;
    gl.LoadIdentity;
    rci.GLStates.Disable(stDepthTest);
    // render text
    if Assigned(FFont) then
      FFont.RenderString(rci, Text, Alignment, Layout, ModulateColor.Color)
    else
      BitmapFont.RenderString(rci, Text, Alignment, Layout, ModulateColor.Color);
    // restore state
    rci.GLStates.Enable(stDepthTest);
    gl.PopMatrix;
    gl.MatrixMode(GL_MODELVIEW);
    gl.PopMatrix;
  end;
  if Count > 0 then
    Self.renderChildren(0, Count - 1, rci);
end;

end.
