{: GLBlur<p>

	Applies a blur effect over the viewport.<p>

	<b>History : </b><font size=-1><ul>
        <li>11/06/04 - Mrqzzz - Creation
   </ul></font>
}
unit GLBlur;

interface

uses
   Classes, GLScene, VectorGeometry, GLMisc, StdCtrls, GLObjects, GLBitmapFont,
   GLTexture,GLHudObjects;

type
    TGLBlurPreset = (pNone,pGlossy,pBeastView,pOceanDepth,pDream,pOverBlur);

type
    TGLBlur = class (TGLHUDSprite)
      private
       FViewer : TGLMemoryViewer;
       OldTime :  Double;
       FDoingMemView : boolean;
    FBlurDeltaTime: Double;
    FBlurTop: single;
    FBlurBottom: single;
    FBlurLeft: single;
    FBlurRight: single;
    FRenderHeight: integer;
    FRenderWidth: integer;
    FPreset: TGLBlurPreset;
    procedure DoMemView(baseObject: TGLBaseSceneObject);
    procedure SetBlurDeltaTime(const Value: Double);
    procedure SetBlurBottom(const Value: single);
    procedure SetBlurLeft(const Value: single);
    procedure SetBlurRight(const Value: single);
    procedure SetBlurTop(const Value: single);
    procedure SetRenderHeight(const Value: integer);
    procedure SetRenderWidth(const Value: integer);
    procedure UpdateImageSettings;
    procedure SetPreset(const Value: TGLBlurPreset);
      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure DoProgress(const progressTime : TProgressTimes); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
      published
         property BlurDeltaTime : Double read FBlurDeltaTime write SetBlurDeltaTime;
         property BlurLeft:single read FBlurLeft write SetBlurLeft;
         property BlurTop:single read FBlurTop write SetBlurTop;
         property BlurRight:single read FBlurRight write SetBlurRight;
         property BlurBottom:single read FBlurBottom write SetBlurBottom;
         property RenderWidth:integer read FRenderWidth write SetRenderWidth;
         property RenderHeight:integer read FRenderHeight write SetRenderHeight;
         property Preset : TGLBlurPreset read FPreset write SetPreset stored false;
    end;

implementation

uses SysUtils, OpenGL1x, GLGraphics, XOpenGL, GLState;

constructor TGLBlur.Create(AOwner: TComponent);
begin
  inherited;
  FBlurDeltaTime := 0.02;
  FBlurTop := 0.01;
  FBlurLeft := 0.01;
  FBlurRight := 0.01;
  FBlurBottom := 0.01;
  FRenderHeight := 256;
  FRenderWidth  := 256;
  OldTime := 0.0;
  FViewer := TGLMemoryViewer.create(self);
  FPreset := pNone;
  Material.Texture.Disabled := false;
  // init texture
  Material.Texture.ImageClassName := 'TGLBlankImage';
  TGLBlankImage(Material.Texture.Image).Width := FViewer.Width;
  TGLBlankImage(Material.Texture.Image).Height := FViewer.Height;
  Material.MaterialOptions := [moNoLighting];
  Material.Texture.TextureMode := TMModulate;  
  Material.BlendingMode := bmAdditive;
end;

destructor TGLBlur.Destroy;
begin
     FViewer.Free;
     inherited;
end;


procedure TGLBlur.UpdateImageSettings;
begin
     with TGLBlankImage(Material.Texture.Image) do
     begin
          Width := RenderWidth;
          Height := Renderheight;
     end;

     with FViewer do
     begin
          Width := RenderWidth;
          Height := Renderheight;
     end;
end;

procedure TGLBlur.DoProgress(const progressTime : TProgressTimes);
begin
     inherited;
     OldTime := OldTime + progressTime.deltaTime;
     if self.Visible and (OldTime >= FBlurDeltaTime) then //(progressTime.newTime - OldTime > FBlurDeltaTime) then
     begin
          OldTime := 0.0; //progressTime.newTime;
          if Self.Parent is TGLBaseSceneObject then
             DoMemView(Self.Parent);
     end;
end;


procedure TGLBlur.DoMemView(baseObject: TGLBaseSceneObject);
var
   OldFocalLength:single;
   refsiz:single;
begin
     if FViewer.Camera<>Scene.CurrentGLCamera then
        FViewer.Camera := Scene.CurrentGLCamera;

     if FViewer.Camera<>nil then
     begin
          FDoingMemView := true;

          //SCene.RenderScene(FViewer.Buffer,FViewer.Width,FViewer.Height,dsRendering,baseObject);
          FViewer.Camera.BeginUpdate;

          OldFocalLength := FViewer.Camera.FocalLength;

          // CALCULATE SCALED FOCAL LENGTH FOR VIEWER
          if Scene.CurrentBuffer.Width>SCene.CurrentBuffer.height then
             refsiz := SCene.CurrentBuffer.Width
          else
              refsiz := SCene.CurrentBuffer.height;

          FViewer.Camera.FocalLength := FViewer.Camera.FocalLength*FViewer.Buffer.Width/refsiz;


          if FViewer.Buffer.BackgroundColor<>SCene.CurrentBuffer.BackgroundColor then
             FViewer.Buffer.BackgroundColor := SCene.CurrentBuffer.BackgroundColor;
          
          FViewer.Render(baseObject);
          FViewer.CopyToTexture(self.Material.Texture);


          FViewer.Camera.FocalLength := OldFocalLength;
          FViewer.Camera.EndUpdate;
          FDoingMemView := false;
     end;
end;



procedure TGLBlur.DoRender(var rci : TRenderContextInfo;
                              renderSelf, renderChildren : Boolean);
var
	vx, vy, vx1, vy1, f : Single;
        offsx,offsy:single;
        MaxMeasure : integer;
begin

    if rci.ignoreMaterials then Exit;
  	Material.Apply(rci);
    repeat
      if AlphaChannel<>1 then
         rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel);
      // Prepare matrices
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
      if rci.renderDPI=96 then
         f:=1
      else
         f:=rci.renderDPI/96;
      glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);

      // center of viewport:
      glTranslatef(0,0, Position.Z);

      if Rotation<>0 then
         glRotatef(Rotation, 0, 0, 1);
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      //glDisable(GL_CULL_FACE);
      glDepthMask(False);


     // calculate offsets in order to keep the quad a square centered in the view
     if rci.viewPortSize.cx>rci.viewPortSize.cy then
     begin
          offsx := 0;
          offsy := (rci.viewPortSize.cx-rci.viewPortSize.cy)*0.5;
          MaxMeasure := rci.viewPortSize.cx;
     end
     else
     begin
          offsx := (rci.viewPortSize.cy-rci.viewPortSize.cx)*0.5;
          offsy := 0;
          MaxMeasure := rci.viewPortSize.cy;
     end;

      // precalc coordinates
      vx:=-rci.viewPortSize.cx*0.5*f;     vx1:=vx+rci.viewPortSize.cx*f;
      vy:=+rci.viewPortSize.cy*0.5*f;     vy1:=vy-rci.viewPortSize.cy*f;


      vx := vx - offsx; vx1 := vx1 + offsx;
      vy := vy + offsy; vy1 := vy1 - offsy;

      // Cause the radial scaling
      if FDoingMemView then
      begin
           vx := vx - FBlurLeft*MaxMeasure;    vx1 := vx1 + FBlurRight*MaxMeasure;
           vy := vy + FBlurTop*MaxMeasure;    vy1 := vy1 - FBlurBottom*MaxMeasure;
      end;

      // issue quad
      glBegin(GL_QUADS);
         glNormal3fv(@YVector);
         xglTexCoord2f(0, 0);           glVertex2f( vx, vy1);
         xglTexCoord2f(XTiles, 0);      glVertex2f(vx1, vy1);
         xglTexCoord2f(XTiles, YTiles); glVertex2f(vx1,  vy);
         xglTexCoord2f(0, YTiles);      glVertex2f( vx,  vy);
      glEnd;

      // restore state
      glDepthMask(True);
      glPopAttrib;
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
   until not Material.UnApply(rci);
   if Count>0 then
      Self.RenderChildren(0, Count-1, rci);
end;


procedure TGLBlur.SetBlurDeltaTime(const Value: Double);
begin
  FBlurDeltaTime := Value;
end;

procedure TGLBlur.SetBlurBottom(const Value: single);
begin
  FBlurBottom := Value;
end;

procedure TGLBlur.SetBlurLeft(const Value: single);
begin
  FBlurLeft := Value;
end;

procedure TGLBlur.SetBlurRight(const Value: single);
begin
  FBlurRight := Value;
end;

procedure TGLBlur.SetBlurTop(const Value: single);
begin
  FBlurTop := Value;
end;

procedure TGLBlur.SetRenderHeight(const Value: integer);
begin
  FRenderHeight := Value;
  UpdateImageSettings;
end;

procedure TGLBlur.SetRenderWidth(const Value: integer);
begin
  FRenderWidth := Value;
  UpdateImageSettings;
end;

procedure TGLBlur.SetPreset(const Value: TGLBlurPreset);
begin
     FPreset := Value;

     case FPreset of
     pNone       : begin
                       // do nothing
                  end;
     pGlossy     : begin
                      Material.BlendingMode := bmAdditive;
                      Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 0.7);
                      BlurTop := 0.02;
                      BlurLeft := 0.02;
                      BlurRight := 0.02;
                      BlurBottom := 0.02;
                      BlurDeltaTime := 0.02;

                  end;
     pBeastView  : begin
                      Material.BlendingMode := bmAdditive;
                      Material.FrontProperties.Diffuse.SetColor(1, 0, 0, 0.8);
                      BlurTop := 0.001;
                      BlurLeft := 0.03;
                      BlurRight := 0.03;
                      BlurBottom := 0.001;
                      BlurDeltaTime := 0.02;

                  end;
     pOceanDepth : begin
                      Material.BlendingMode := bmTransparency;
                      Material.FrontProperties.Diffuse.SetColor(0.2, 0.2, 1, 0.99);
                      BlurTop := 0.04;
                      BlurLeft := 0.02;
                      BlurRight := 0.02;
                      BlurBottom := 0.04;
                      BlurDeltaTime := 0.02;
                  end;
     pDream      : begin
                      Material.BlendingMode := bmTransparency;
                      Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 0.992);
                      BlurTop := 0.02;
                      BlurLeft := 0.02;
                      BlurRight := 0.02;
                      BlurBottom := 0.02;
                      BlurDeltaTime := 0.1;
                  end;
     pOverBlur  : begin
                      Material.BlendingMode := bmAdditive;
                      Material.FrontProperties.Diffuse.SetColor(0.95,0.95, 0.95, 0.98);
                      BlurTop := 0.01;
                      BlurLeft := 0.01;
                      BlurRight := 0.01;
                      BlurBottom := 0.01;
                      BlurDeltaTime := 0.02;
                  end;
     end;

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   // class registrations
   RegisterClass(TGLBlur);



end.
