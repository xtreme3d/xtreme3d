// GLTexLensFlare
{: Texture-based Lens flare object.<p>

	<b>History : </b><font size=-1><ul>
      <li>25/09/03 - EG - Creation from GLLensFlare split
	</ul></font><p>
}
unit GLTexLensFlare;

interface

uses
   Classes, GLScene, VectorGeometry, GLObjects, GLTexture, OpenGL1x, GLMisc, GLContext;

type

   // TGLTextureLensFlare
   //
   TGLTextureLensFlare = class(TGLBaseSceneObject)
      private
         { Private Declarations }
         FSize: integer;
         FCurrSize: Single;
         FNumSecs: integer;
         FAutoZTest: boolean;
         //used for internal calculation
         FDeltaTime : Double;
         FImgSecondaries: TGLTexture;
         FImgRays: TGLTexture;
         FImgRing: TGLTexture;
         FImgGlow: TGLTexture;
         FSeed: Integer;
         procedure SetImgGlow(const Value: TGLTexture);
         procedure SetImgRays(const Value: TGLTexture);
         procedure SetImgRing(const Value: TGLTexture);
         procedure SetImgSecondaries(const Value: TGLTexture);
         procedure SetSeed(const Value: Integer);
      protected
         { Protected Declarations }
         procedure SetSize(aValue: integer);
         procedure SetNumSecs(aValue: integer);
         procedure SetAutoZTest(aValue: boolean);
      public
         { Public Declarations }    
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure BuildList(var rci: TRenderContextInfo); override;
         procedure DoProgress(const progressTime : TProgressTimes); override;
      published
         { Public Declarations }
         //: MaxRadius of the flare.
         property Size: integer read FSize write SetSize default 50;
         //: Random seed
         property Seed : Integer read FSeed write SetSeed;
         //: Number of secondary flares.
         property NumSecs: integer read FNumSecs write SetNumSecs default 8;
         //: Number of segments used when rendering circles.
         //property Resolution: integer read FResolution write SetResolution default 64;
         property AutoZTest: boolean read FAutoZTest write SetAutoZTest default True;
         // The Textures
         property ImgGlow: TGLTexture read FImgGlow write SetImgGlow;
         property ImgRays: TGLTexture read FImgRays write SetImgRays;
         property ImgRing: TGLTexture read FImgRing write SetImgRing;
         property ImgSecondaries: TGLTexture read FImgSecondaries write SetImgSecondaries;

         property ObjectsSorting;
         property Position;
         property Visible;
         property OnProgress;
         property Behaviours;
         property Effects;
   end;

implementation

// ------------------
// ------------------ TGLTextureLensFlare ------------------
// ------------------

constructor TGLTextureLensFlare.Create;
begin
  inherited;
  Randomize;
  FSeed := Random(2000)+465;
  
  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FCurrSize := FSize;
  FNumSecs := 8;
  FAutoZTest := True;

  FImgRays := TGLTexture.Create(Self);
  FImgSecondaries := TGLTexture.Create(Self);
  FImgRing := TGLTexture.Create(Self);
  FImgGlow := TGLTexture.Create(Self);
end;

procedure TGLTextureLensFlare.SetSize(aValue: integer);
begin
  if FSize <> aValue then
  begin
    FSize := aValue;
    FCurrSize := FSize;
    StructureChanged;
  end;
end;


procedure TGLTextureLensFlare.SetNumSecs(aValue: integer);
begin
  if FNumSecs <> aValue then
  begin
    FNumSecs := aValue;
    StructureChanged;
  end;
end;

// SetAutoZTest
//
procedure TGLTextureLensFlare.SetAutoZTest(aValue: boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

// BuildList
//
procedure TGLTextureLensFlare.BuildList(var rci: TRenderContextInfo);
var
  v, rv, screenPos, posVector : TAffineVector;
  depth, rnd : Single;
  flag : Boolean;
  i : Integer;
begin
  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := Scene.CurrentBuffer.WorldToScreen(v);
    if (screenPos[0] < rci.viewPortSize.cx) and (screenPos[0] >= 0)
      and (screenPos[1] < rci.viewPortSize.cy) and (screenPos[1] >= 0) then
    begin
      if FAutoZTest then
      begin
        depth := Scene.CurrentBuffer.GetPixelDepth(Round(ScreenPos[0]),
          Round(rci.viewPortSize.cy - ScreenPos[1]));
        // but is it behind something?
        if screenPos[2] >= 1 then
          flag := (depth >= 1)
        else
          flag := (depth >= screenPos[2]);
      end
      else
        flag := True;
    end
    else
      flag := False;
  end
  else
    flag := False;

  MakeVector(posVector,
    screenPos[0] - rci.viewPortSize.cx / 2,
    screenPos[1] - rci.viewPortSize.cy / 2,0);

  // make the glow appear/disappear progressively

  if Flag then
    if FCurrSize < FSize then
       FCurrSize := FCurrSize + FDeltaTime * 200{FSize * 4};
  if not Flag then
    if FCurrSize > 0 then
       FCurrSize := FCurrSize - FDeltaTime * 200{FSize * 4};
  if FCurrSize <= 0 then Exit;


  // Prepare matrices
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);


  glPushAttrib(GL_ENABLE_BIT);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);  
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE,GL_ONE);

  //Rays and Glow on Same Position
  glPushMatrix;
  glTranslatef(posVector[0], posVector[1], posVector[2]);

  if not ImgGlow.Disabled and Assigned(ImgGlow.Image) then
  begin
        ImgGlow.Apply(rci);
        glbegin(GL_QUADS);
          glTexCoord2f(0,0);glVertex3f(-FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,0);glVertex3f( FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,1);glVertex3f( FCurrSize, FCurrSize,0);
          glTexCoord2f(0,1);glVertex3f(-FCurrSize, FCurrSize,0);
        glend;
        ImgGlow.UnApply(rci);
  end;

  if not ImgRays.Disabled and Assigned(ImgRays.Image) then
  begin
        ImgRays.Apply(rci);
        glbegin(GL_QUADS);
          glTexCoord2f(0,0);glVertex3f(-FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,0);glVertex3f( FCurrSize,-FCurrSize,0);
          glTexCoord2f(1,1);glVertex3f( FCurrSize, FCurrSize,0);
          glTexCoord2f(0,1);glVertex3f(-FCurrSize, FCurrSize,0);
        glend;
        ImgRays.UnApply(rci);
  end;
  glPopMatrix;

  if not ImgRing.Disabled and Assigned(ImgRing.Image) then
  begin
        glPushMatrix;
          glTranslatef(posVector[0]*1.1, posVector[1]*1.1, posVector[2]);
          ImgRing.Apply(rci);
          glbegin(GL_QUADS);
            glTexCoord2f(0,0);glVertex3f(-FCurrSize,-FCurrSize,0);
            glTexCoord2f(1,0);glVertex3f( FCurrSize,-FCurrSize,0);
            glTexCoord2f(1,1);glVertex3f( FCurrSize, FCurrSize,0);
            glTexCoord2f(0,1);glVertex3f(-FCurrSize, FCurrSize,0);
          glend;
          ImgRing.UnApply(rci);
        glPopMatrix;
  end;

  if not ImgSecondaries.Disabled and Assigned(ImgSecondaries.Image) then
  begin
        RandSeed := FSeed;
        glPushMatrix;
          ImgSecondaries.Apply(rci);
          for i := 1 to FNumSecs do
          begin
             rnd := 2 * Random - 1;
             v:=PosVector;
             if rnd<0 then
                ScaleVector(V, rnd)
             else ScaleVector(V, 0.8*rnd);
             glPushMatrix;
               glTranslatef(v[0], v[1],v[2]);

               rnd := random*0.5+0.1;
               glbegin(GL_QUADS);
                 glTexCoord2f(0,0);glVertex3f(-FCurrSize*rnd,-FCurrSize*rnd,0);
                 glTexCoord2f(1,0);glVertex3f( FCurrSize*rnd,-FCurrSize*rnd,0);
                 glTexCoord2f(1,1);glVertex3f( FCurrSize*rnd, FCurrSize*rnd,0);
                 glTexCoord2f(0,1);glVertex3f(-FCurrSize*rnd, FCurrSize*rnd,0);
               glend;
             glPopMatrix
          end;
          ImgSecondaries.UnApply(rci);
        glPopMatrix;
  end;

   // restore state

  glPopAttrib;
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;


// DoProgress
//
procedure TGLTextureLensFlare.DoProgress(const progressTime: TProgressTimes);
begin
  FDeltaTime:=progressTime.deltaTime;
  inherited;  
end;

procedure TGLTextureLensFlare.SetImgGlow(const Value: TGLTexture);
begin
  FImgGlow.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRays(const Value: TGLTexture);
begin
  FImgRays.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRing(const Value: TGLTexture);
begin
  FImgRing.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgSecondaries(const Value: TGLTexture);
begin
  FImgSecondaries.Assign(Value);
  StructureChanged;
end;


destructor TGLTextureLensFlare.Destroy;
begin   
  FImgRays.Free;
  FImgSecondaries.Free;
  FImgRing.Free;
  FImgGlow.Free;
  inherited;
end;

procedure TGLTextureLensFlare.SetSeed(const Value: Integer);
begin
  FSeed := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClasses([TGLTextureLensFlare]);

end.
