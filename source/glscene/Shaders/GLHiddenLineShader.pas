{: GLHiddenLineShader<p>

   A shader that renders hidden (back-faced) lines differently from visible
   (front) lines. Polygon offset is used to displace fragments depths a little
   so that there is no z-fighting in rendering the same geometry multiple times.<p>

   <b>History : </b><font size=-1><ul>
      <li>25/09/04 - NelC - Fixed bug of disabled blend (thx Carlos)   
      <li>05/02/04 - NelC - Fixed memory leak in TGLHiddenLineShader.Destroy (thx Achim Hammes)
      <li>13/12/03 - NelC - Added SurfaceLit, ShadeModel
      <li>05/12/03 - NelC - Added ForceMaterial
      <li>03/12/03 - NelC - Creation. Modified from the HiddenLineShader in
                            the multipass demo.
   </ul></font>
}
unit GLHiddenLineShader;

interface

uses
  Classes, GLTexture, OpenGL1x, GLMisc, GLCrossPlatform, GLScene;

type
  TGLLineSettings = class(TGLUpdateAbleObject)
	   private
      { Private Declarations }
       FColor   : TGLColor;
       FWidth   : Single;
       FPattern : TGLushort;

       FForceMaterial : Boolean;

       procedure SetPattern(const value: TGLushort);
       procedure SetColor(const v : TGLColor);
       procedure SetWidth(const Value: Single);
       procedure SetForceMaterial(v : boolean);

     public
			{ Public Declarations }
       constructor Create(AOwner: TPersistent); override;
       destructor Destroy; override;
       procedure Apply(var rci : TRenderContextInfo);
       procedure UnApply(var rci : TRenderContextInfo);

     published
			{ Published Declarations }
       property Width : Single read FWidth write SetWidth;
       property Color : TGLColor read FColor write SetColor;
       property Pattern : TGLushort read FPattern write SetPattern default $FFFF;
       {: Set ForceMaterial to true to enforce the application of the line settings
          for objects that sets their own color, line width and pattern. }
       property ForceMaterial : Boolean read FForceMaterial write SetForceMaterial default false;
  end;

  TGLHiddenLineShader = class(TGLShader)
    private
      FPassCount    : integer;

      FLineSmooth : Boolean;
      FSolid      : Boolean;

      FBackGroundColor : TGLColor;

      FFrontLine : TGLLineSettings;
      FBackLine  : TGLLineSettings;

      FLighting : Boolean;
      FShadeModel : TGLShadeModel;

      procedure SetlineSmooth(v : boolean);
      procedure SetSolid(v : boolean);
      procedure SetBackgroundColor(AColor: TGLColor);
      procedure SetLighting(v : boolean);
      procedure SetShadeModel(const val : TGLShadeModel);

    protected
      procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;

    public
			{ Public Declarations }
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      { Published Declarations }
      property FrontLine : TGLLineSettings read FFrontLine write FFrontLine;
      property BackLine : TGLLineSettings read FBackLine write FBackLine;
      {: Line smoothing control }
      property LineSmooth : Boolean read FlineSmooth write SetlineSmooth default false;
      {: Solid controls if you can see through the front-line wireframe. }
      property Solid : Boolean read FSolid write SetSolid default false;
      {: Color used for solid fill. }
      property BackgroundColor: TGLColor read FBackgroundColor write SetBackgroundColor;
      {: When Solid is True, determines if lighting or background color is used. }
      property SurfaceLit : Boolean read FLighting write SetLighting default true;
      {: Shade model.<p>
         Default is "Smooth".<p> }
      property ShadeModel : TGLShadeModel read FShadeModel write SetShadeModel default smDefault;
  end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TGLHiddenLineShader]);
end;

// ------------------
// ------------------ TGLLineSettings ------------------
// ------------------

// Create
//
constructor TGLLineSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FColor:=TGLColor.Create(Self);
  FColor.Initialize(clrGray20);
  FWidth:=2;
  Pattern:=$FFFF;
  ForceMaterial:=false;
end;

// Destroy
//
destructor TGLLineSettings.Destroy;
begin
  FColor.Free;
  inherited;
end;

// SetPattern
//
procedure TGLLineSettings.SetPattern(const value: TGLushort);
begin
   if FPattern<>value then begin
      FPattern:=Value;
      NotifyChange(self);
   end;
end;

// SetColor
//
procedure TGLLineSettings.SetColor(const v: TGLColor);
begin
   FColor.Color:=v.Color;
   NotifyChange(Self);
end;

// SetWidth
//
procedure TGLLineSettings.SetWidth(const Value: Single);
begin
  FWidth := Value;
  NotifyChange(Self);
end;

var IgnoreMatSave : boolean;

// Apply
//
procedure TGLLineSettings.Apply(var rci : TRenderContextInfo);
begin
  glLineWidth(Width);
  glColor4fv(Color.AsAddress);
  if Pattern<>$FFFF then begin
      glEnable(GL_LINE_STIPPLE);
      glLineStipple(1, Pattern);
    end
  else
    glDisable(GL_LINE_STIPPLE);

  if ForceMaterial then begin
    IgnoreMatSave:=rci.ignoreMaterials;
    rci.ignoreMaterials:=true;
  end;
end;

// UnApply
//
procedure TGLLineSettings.UnApply(var rci : TRenderContextInfo);
begin
  if ForceMaterial then rci.ignoreMaterials:=IgnoreMatSave;
end;

// SetForceMaterial
//
procedure TGLLineSettings.SetForceMaterial(v: boolean);
begin
   if FForceMaterial<>v then begin
      FForceMaterial:=v;
      NotifyChange(self);
   end;
end;

// ------------------
// ------------------ TGLHiddenLineShader ------------------
// ------------------

// Create
//
constructor TGLHiddenLineShader.Create(AOwner : TComponent);
begin
  inherited;
  FFrontLine := TGLLineSettings.Create(self);
  FBackLine  := TGLLineSettings.Create(self);
  FSolid:=false;

  FBackgroundColor:=TGLColor.Create(Self);
  FBackgroundColor.Initialize(clrBtnFace);

  FLineSmooth:=False;
  FLighting:=true;
  FShadeModel:=smDefault;
end;

// Destroy
//
destructor TGLHiddenLineShader.Destroy;
begin
  FFrontLine.Free;
  FBackLine.Free;
  FBackgroundColor.Free;
  inherited;
end;

// DoApply
//
procedure TGLHiddenLineShader.DoApply(var rci: TRenderContextInfo; Sender : TObject);
begin
   FPassCount:=1;

   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_POLYGON_BIT or
                GL_HINT_BIT or GL_DEPTH_BUFFER_BIT or GL_LINE_BIT or GL_LIGHTING_BIT);

   if solid then begin
       // draw filled front faces in first pass
       glPolygonMode(GL_FRONT, GL_FILL);
       glCullFace(GL_BACK);

       if FLighting then begin
           case ShadeModel of
             smDefault, smSmooth : glShadeModel(GL_SMOOTH);
             smFlat : glShadeModel(GL_FLAT);
           end
         end
       else
         begin
           glDisable(GL_LIGHTING);
           glColor4fv(FBackgroundColor.AsAddress); // use background color
         end;
       // enable and adjust polygon offset
       glEnable(GL_POLYGON_OFFSET_FILL);
     end
   else begin
       glDisable(GL_LIGHTING);
       // draw back lines in first pass
       FBackLine.Apply(rci);
       glCullFace(GL_FRONT);
       GLPolygonMode(GL_BACK, GL_LINE);
       // enable and adjust polygon offset
       glEnable(GL_POLYGON_OFFSET_LINE);
     end;

   glPolygonOffset(1, 2);
end;

// DoUnApply
//
function TGLHiddenLineShader.DoUnApply(var rci: TRenderContextInfo): Boolean;

  procedure SetLineSmoothBlend;
  begin
    if LineSmooth then begin
       glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
       glEnable(GL_LINE_SMOOTH);
    end else begin
       glDisable(GL_LINE_SMOOTH);
    end;

    if LineSmooth or (FBackLine.FColor.Alpha<1) or (FFrontLine.FColor.Alpha<1) then begin
       glEnable(GL_BLEND);
       glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    end else begin
       glDisable(GL_BLEND);
    end;
  end;

begin
   case FPassCount of
      1 : begin
            // draw front line in 2nd pass
            FPassCount:=2;

            FBackLine.UnApply(rci);
            FFrontLine.Apply(rci);

            SetLineSmoothBlend;

            if solid and FLighting then
              glDisable(GL_LIGHTING);

            GLPolygonMode(GL_FRONT, GL_LINE);
            glCullFace(GL_BACK);

            if solid then
              glDisable(GL_POLYGON_OFFSET_FILL)
            else
              glDisable(GL_POLYGON_OFFSET_LINE);

            Result:=True;
          end;
      2 : begin
            FFrontLine.UnApply(rci);
            glPopAttrib;
            Result:=false;
          end;
   else
      Assert(False);
      Result:=False;
   end;
end;

// SetBackgroundColor
//
procedure TGLHiddenLineShader.SetBackgroundColor(AColor: TGLColor);
begin
   FBackgroundColor.Color:=AColor.Color;
   NotifyChange(Self);
end;

// SetlineSmooth
//
procedure TGLHiddenLineShader.SetlineSmooth(v: boolean);
begin
   if FlineSmooth<>v then begin
      FlineSmooth:=v;
      NotifyChange(self);
   end;
end;

// SetLighting
//
procedure TGLHiddenLineShader.SetLighting(v: boolean);
begin
   if FLighting<>v then begin
      FLighting:=v;
      NotifyChange(self);
   end;
end;

// SetSolid
//
procedure TGLHiddenLineShader.SetSolid(v: boolean);
begin
   if FSolid<>v then begin
      FSolid:=v;
      NotifyChange(self);
   end;
end;

// SetShadeModel
//
procedure TGLHiddenLineShader.SetShadeModel(const val : TGLShadeModel);
begin
   if FShadeModel<>val then begin
      FShadeModel:=val;
      NotifyChange(Self);
   end;
end;

end.


