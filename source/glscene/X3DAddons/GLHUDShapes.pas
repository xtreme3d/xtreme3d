unit GLHUDShapes;

{$R-}

interface

{$i GLScene.inc}

uses Classes, VectorGeometry, GLScene, GLTexture, GLMisc, OpenGL1x, SysUtils,
   VectorLists, GLCrossPlatform, GLContext;

type

  THUDShapeType = (hstRectangle, hstCircle, hstLine, hstMesh);

	TGLHUDShape = class(TGLSceneObject)
		private
		  FWidth: TGLFloat;
		  FHeight: TGLFloat;
		  FRotation: TGLFloat;
      FOriginX: TGLFloat;
      FOriginY: TGLFloat;
      FVertices: TAffineVectorList;
      FTexCoords: TAffineVectorList;
      FVertexIndices: TIntegerList;
      FShapeType: THUDShapeType;
      FNumSlices: Integer;
      FStartAngle: Single;
      FEndAngle: Single;
      FPoint1X: Single;
      FPoint1Y: Single;
      FPoint2X: Single;
      FPoint2Y: Single;
      FLineWidth: Single;
      FColor: TGLColor;

		protected
		  procedure SetWidth(const val: TGLFloat);
      procedure SetHeight(const val: TGLFloat);
      procedure SetRotation(const val: TGLFloat);
      procedure SetVertices(const val: TAffineVectorList);
      procedure SetTexCoords(const val: TAffineVectorList);
      procedure SetVertexIndices(const val: TIntegerList);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
      procedure DoRender(var rci: TRenderContextInfo; renderSelf, renderChildren: Boolean); override;
      function AxisAlignedDimensionsUnscaled: TVector; override;

			procedure SetSize(const width, height: TGLFloat);
			procedure SetSquareSize(const size : TGLFloat);

		published
			{ Published Declarations }
			property Width: TGLFloat read FWidth write SetWidth;
			property Height: TGLFloat read FHeight write SetHeight;
      property Rotation: TGLFloat read FRotation write SetRotation;

      property OriginX: TGLFloat read FOriginX write FOriginX;
      property OriginY: TGLFloat read FOriginY write FOriginY;

      property Vertices: TAffineVectorList read FVertices write SetVertices;
      property TexCoords: TAffineVectorList read FTexCoords write SetTexCoords;
      property VertexIndices: TIntegerList read FVertexIndices write SetVertexIndices;

      property ShapeType: THUDShapeType read FShapeType write FShapeType;
      
      property NumSlices: Integer read FNumSlices write FNumSlices;
      property StartAngle: Single read FStartAngle write FStartAngle;
      property EndAngle: Single read FEndAngle write FEndAngle;

      property Point1X: Single read FPoint1X write FPoint1X;
      property Point1Y: Single read FPoint1Y write FPoint1Y;
      property Point2X: Single read FPoint2X write FPoint2X;
      property Point2Y: Single read FPoint2Y write FPoint2Y;
      property LineWidth: Single read FLineWidth write FLineWidth;

      property Color: TGLColor read FColor write FColor;
	end;

implementation

uses GLStrings, Spline, XOpenGL, GLState;

constructor TGLHUDShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FWidth := 1; //2;
  FHeight := 1; //2;
  FOriginX := 0.0;
  FOriginY := 0.0;
  FVertices := TAffineVectorList.Create;
  FTexCoords := TAffineVectorList.Create;
  FVertexIndices := TIntegerList.Create;
  FShapeType := hstRectangle;
  FNumSlices := 16;
  FStartAngle := 0;
  FEndAngle := 360;
  FPoint1X := 0;
  FPoint1Y := 0;
  FPoint2X := 0;
  FPoint2Y := 0;
  FLineWidth := 1;
  Color := TGLColor.Create(Self);
  Color.SetColor(1, 1, 1, 1);
end;

destructor TGLHUDShape.Destroy;
begin
   FVertices.Free;
   FTexCoords.Free;
   FVertexIndices.Free;
   Color.Free;
   inherited;
end;

procedure TGLHUDShape.Assign(Source: TPersistent);
begin
  if Source is TGLHUDShape then begin
    FWidth := TGLHUDShape(Source).FWidth;
    FHeight := TGLHUDShape(Source).FHeight;
    FRotation := TGLHUDShape(Source).FRotation;
    FOriginX := TGLHUDShape(Source).FOriginX;
    FOriginY := TGLHUDShape(Source).FOriginY;
    FVertices.Assign(TGLHUDShape(Source).FVertices);
    FTexCoords.Assign(TGLHUDShape(Source).FTexCoords);
    FVertexIndices.Assign(TGLHUDShape(Source).FVertexIndices);
    FShapeType := TGLHUDShape(Source).FShapeType;
    FNumSlices := TGLHUDShape(Source).FNumSlices;
    FStartAngle := TGLHUDShape(Source).FStartAngle;
    FEndAngle := TGLHUDShape(Source).FEndAngle;
    FPoint1X := TGLHUDShape(Source).FPoint1X;
    FPoint1Y := TGLHUDShape(Source).FPoint1Y;
    FPoint2X := TGLHUDShape(Source).FPoint2X;
    FPoint2Y := TGLHUDShape(Source).FPoint2Y;
    FLineWidth := TGLHUDShape(Source).FLineWidth;
  end;
  inherited Assign(Source);
end;

function TGLHUDShape.AxisAlignedDimensionsUnscaled: TVector;
begin
   Result[0] := 0.5 * Abs(FWidth);
   Result[1] := 0.5 * Abs(FHeight);
   Result[2] := 0.5 * Abs(FWidth);
end;

procedure TGLHUDShape.DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
var
  f : Single;
  tv1, tv2, tv3: TAffineVector;
  uv1, uv2, uv3: TAffineVector;
  i, c: Integer;
  angleR, endAngleR, stepAngleR, x, y: Single;
  mat: TGLLibMaterial;
begin
  if rci.ignoreMaterials then Exit;
  
  Material.Apply(rci);

  mat := Material.MaterialLibrary.LibMaterialByName(Material.LibMaterialName);

  if mat <> nil then
  begin
    if mat.Material.Texture.Image.Width > 0 then
      glColor4fv(Color.AsAddress);
  end
  else
    glColor4fv(Color.AsAddress);

  repeat
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
    if rci.renderDPI=96 then
       f:=1
    else f:=rci.renderDPI/96;
    glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);
    glTranslatef(f*Position.X-rci.viewPortSize.cx*0.5,
                 rci.viewPortSize.cy*0.5-f*Position.Y, Position.Z);
    if Rotation<>0 then
       glRotatef(Rotation, 0, 0, 1);
    glTranslatef(-OriginX, OriginY, 0.0);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
      
    glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(False);

    if FShapeType = hstRectangle then
    begin
      glScalef(Width * f * 0.5, Height * f * 0.5, 1.0);
      glBegin(GL_QUADS);
         glNormal3fv(@YVector);
         xglTexCoord2f(0, 0); glVertex2f(-1, -1);
         xglTexCoord2f(1, 0); glVertex2f(+1, -1);
         xglTexCoord2f(1, 1); glVertex2f(+1, +1);
         xglTexCoord2f(0, 1); glVertex2f(-1, +1);
      glEnd;
    end
    else if FShapeType = hstCircle then
    begin
      if FStartAngle < FEndAngle then
      begin
        angleR := DegToRad(FStartAngle);
        endAngleR := DegToRad(FEndAngle);
        stepAngleR := (2.0 * PI) / FNumSlices;
        glScalef(Width * f * 0.5, Height * f * 0.5, 1.0);
        glBegin(GL_TRIANGLE_FAN);
        xglTexCoord2f(0.5, 0.5); glVertex2f(0, 0);
        for i := 0 to FNumSlices+1 do
        begin
          if angleR < endAngleR then
          begin
            x := cos(angleR);
            y := sin(angleR);
            xglTexCoord2f((x + 1) * 0.5, (y + 1) * 0.5);
            glVertex2f(x, y);
            angleR := angleR + stepAngleR;
          end
          else
          begin
            angleR := endAngleR;
            x := cos(angleR);
            y := sin(angleR);
            xglTexCoord2f((x + 1) * 0.5, (y + 1) * 0.5);
            glVertex2f(x, y);
            break;
          end;
        end;
        glEnd();
      end;
    end
    else if FShapeType = hstLine then
    begin
      glScalef(Width * f, Height * f, 1.0);
      glScalef(1.0, -1.0, 1.0);
      glLineWidth(FLineWidth);
      glBegin(GL_LINES);
        glVertex2f(Point1X, Point1Y);
        glVertex2f(Point2X, Point2Y);
      glEnd();
    end
    else if FShapeType = hstMesh then
    begin
      if (FVertexIndices.Count > 0) and (FVertices.Count > 0) then
      begin
        if ((FVertexIndices.Count mod 3) = 0) then
        begin
          glScalef(Width * f, Height * f, 1.0);
          glMatrixMode(GL_MODELVIEW);
          glScalef(1.0, -1.0, 1.0);
          c := Round(FVertexIndices.Count / 3);
          for i:=0 to c-1 do
          begin
            tv1 := FVertices[FVertexIndices[i*3]];
            tv2 := FVertices[FVertexIndices[i*3+1]];
            tv3 := FVertices[FVertexIndices[i*3+2]];

            if (FTexCoords.Count = FVertices.Count) then
            begin
              uv1 := FTexCoords[FVertexIndices[i*3]];
              uv2 := FTexCoords[FVertexIndices[i*3+1]];
              uv3 := FTexCoords[FVertexIndices[i*3+2]];
            end
            else
            begin
              uv1 := AffineVectorMake(0, 0, 0);
              uv2 := AffineVectorMake(0, 0, 0);
              uv3 := AffineVectorMake(0, 0, 0);
            end;

            glBegin(GL_TRIANGLES);
              glNormal3fv(@YVector);
              xglTexCoord2f(uv1[0], uv1[1]); glVertex2f(tv1[0], tv1[1]);
              xglTexCoord2f(uv2[0], uv2[1]); glVertex2f(tv2[0], tv2[1]);
              xglTexCoord2f(uv3[0], uv3[1]); glVertex2f(tv3[0], tv3[1]);
            glEnd();

          end;
        end;
      end;
    end;

    // restore state
    glDepthMask(True);
    glPopAttrib;
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  until not Material.UnApply(rci);
  if Count>0 then
    Self.RenderChildren(0, Count-1, rci);
end;

// SetWidth
//
procedure TGLHUDShape.SetWidth(const val : TGLFloat);
begin
	if FWidth<>val then begin
		FWidth:=val;
		NotifyChange(Self);
	end;
end;

// SetHeight
//
procedure TGLHUDShape.SetHeight(const val : TGLFloat);
begin
	if FHeight<>val then begin
		FHeight:=val;
		NotifyChange(Self);
	end;
end;

// SetRotation
//
procedure TGLHUDShape.SetRotation(const val : TGLFloat);
begin
	if FRotation<>val then begin
		FRotation:=val;
		NotifyChange(Self);
	end;
end;

// SetSize
//
procedure TGLHUDShape.SetSize(const width, height : TGLFloat);
begin
	FWidth:=width;
	FHeight:=height;
   NotifyChange(Self);
end;

// SetSquareSize
//
procedure TGLHUDShape.SetSquareSize(const size : TGLFloat);
begin
	FWidth:=size;
	FHeight:=size;
   NotifyChange(Self);
end;

procedure TGLHUDShape.SetVertices(const val : TAffineVectorList);
begin
   FVertices.Assign(val);
end;

procedure TGLHUDShape.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

procedure TGLHUDShape.SetVertexIndices(const val : TIntegerList);
begin
   FVertexIndices.Assign(val);
end;

initialization

   RegisterClasses([TGLHUDShape]);

end.
