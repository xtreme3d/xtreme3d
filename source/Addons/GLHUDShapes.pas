unit GLHUDShapes;

{$R-}

interface

{$I GLScene.inc}

uses
  Math,
  System.Classes,
  System.SysUtils,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.Scene,
  GLS.Color,
  GLS.Texture,
  GLS.Material,
  GLS.OpenGLTokens,
  GLS.Context,
  GLS.RenderContextInfo;

type
  THUDShapeType = (hstRectangle, hstCircle, hstLine, hstMesh);

  TGLHUDShape = class(TGLSceneObject)
    private
      FWidth: TGLFloat;
      FHeight: TGLFloat;
      FRotation: TGLFloat;
      FOriginX: TGLFloat;
      FOriginY: TGLFloat;
      FVertices: TGLAffineVectorList;
      FTexCoords: TGLAffineVectorList;
      FVertexIndices: TGLIntegerList;
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
      procedure SetVertices(const val: TGLAffineVectorList);
      procedure SetTexCoords(const val: TGLAffineVectorList);
      procedure SetVertexIndices(const val: TGLIntegerList);

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean); override;
      function AxisAlignedDimensionsUnscaled: TGLVector; override;
      procedure SetSize(const width, height: TGLFloat);
      procedure SetSquareSize(const size: TGLFloat);

    published
      property Width: TGLFloat read FWidth write SetWidth;
      property Height: TGLFloat read FHeight write SetHeight;
      property Rotation: TGLFloat read FRotation write SetRotation;
      property OriginX: TGLFloat read FOriginX write FOriginX;
      property OriginY: TGLFloat read FOriginY write FOriginY;
      property Vertices: TGLAffineVectorList read FVertices write SetVertices;
      property TexCoords: TGLAffineVectorList read FTexCoords write SetTexCoords;
      property VertexIndices: TGLIntegerList read FVertexIndices write SetVertexIndices;
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

constructor TGLHUDShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FWidth := 1;
  FHeight := 1;
  FOriginX := 0.0;
  FOriginY := 0.0;
  FVertices := TGLAffineVectorList.Create;
  FTexCoords := TGLAffineVectorList.Create;
  FVertexIndices := TGLIntegerList.Create;
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

function TGLHUDShape.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result.V[0] := 0.5 * Abs(FWidth);
  Result.V[1] := 0.5 * Abs(FHeight);
  Result.V[2] := 0.5 * Abs(FWidth);
end;

procedure TGLHUDShape.DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);
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

  mat := Material.GetLibMaterial;

  if mat <> nil then begin
    if mat.Material.Texture.Image.Width > 0 then
      gl.Color4fv(Color.AsAddress);
  end
  else
    gl.Color4fv(Color.AsAddress);

  repeat
    gl.MatrixMode(GL_MODELVIEW);
    gl.PushMatrix;
    gl.LoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
    if rci.renderDPI = 96 then
       f := 1
    else f := rci.renderDPI / 96.0;
    gl.Scalef(2.0 / rci.viewPortSize.cx, 2.0 / rci.viewPortSize.cy, 1.0);
    gl.Translatef(f * Position.X - rci.viewPortSize.cx * 0.5,
                 rci.viewPortSize.cy * 0.5 - f * Position.Y, Position.Z);
    if Rotation <> 0 then
       gl.Rotatef(Rotation, 0.0, 0.0, 1.0);
    gl.Translatef(-OriginX, OriginY, 0.0);

    gl.MatrixMode(GL_PROJECTION);
    gl.PushMatrix;
    gl.LoadIdentity;
    gl.MatrixMode(GL_MODELVIEW);

    gl.PushAttrib(GL_ENABLE_BIT);
    gl.Disable(GL_DEPTH_TEST);
    gl.Disable(GL_LIGHTING);
    gl.Disable(GL_CULL_FACE);
    gl.Enable(GL_BLEND);
    gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    gl.DepthMask(False);

    if FShapeType = hstRectangle then begin
      gl.Scalef(Width * f * 0.5, Height * f * 0.5, 1.0);
      gl.Begin_(GL_QUADS);
         gl.Normal3fv(@YVector);
         gl.TexCoord2f(0, 0); gl.Vertex2f(-1, -1);
         gl.TexCoord2f(1, 0); gl.Vertex2f(+1, -1);
         gl.TexCoord2f(1, 1); gl.Vertex2f(+1, +1);
         gl.TexCoord2f(0, 1); gl.Vertex2f(-1, +1);
      gl.End_;
    end
    else if FShapeType = hstCircle then begin
      if FStartAngle < FEndAngle then begin
        angleR := DegToRad(FStartAngle);
        endAngleR := DegToRad(FEndAngle);
        stepAngleR := (2.0 * PI) / FNumSlices;
        gl.Scalef(Width * f * 0.5, Height * f * 0.5, 1.0);
        gl.Begin_(GL_TRIANGLE_FAN);
        gl.TexCoord2f(0.5, 0.5); glVertex2f(0, 0);
        for i := 0 to FNumSlices+1 do begin
          if angleR < endAngleR then begin
            x := cos(angleR);
            y := sin(angleR);
            gl.TexCoord2f((x + 1) * 0.5, (y + 1) * 0.5);
            gl.Vertex2f(x, y);
            angleR := angleR + stepAngleR;
          end
          else begin
            angleR := endAngleR;
            x := cos(angleR);
            y := sin(angleR);
            gl.TexCoord2f((x + 1) * 0.5, (y + 1) * 0.5);
            gl.Vertex2f(x, y);
            break;
          end;
        end;
        gl.End_();
      end;
    end
    else if FShapeType = hstLine then begin
      gl.Scalef(Width * f, Height * f, 1.0);
      gl.Scalef(1.0, -1.0, 1.0);
      gl.LineWidth(FLineWidth);
      gl.Begin_(GL_LINES);
        gl.Vertex2f(Point1X, Point1Y);
        gl.Vertex2f(Point2X, Point2Y);
      gl.End_();
    end
    else if FShapeType = hstMesh then begin
      if (FVertexIndices.Count > 0) and (FVertices.Count > 0) then begin
        if ((FVertexIndices.Count mod 3) = 0) then begin
          gl.Scalef(Width * f, Height * f, 1.0);
          gl.MatrixMode(GL_MODELVIEW);
          gl.Scalef(1.0, -1.0, 1.0);
          c := Round(FVertexIndices.Count / 3);
          for i:=0 to c-1 do begin
            tv1 := FVertices[FVertexIndices[i*3]];
            tv2 := FVertices[FVertexIndices[i*3+1]];
            tv3 := FVertices[FVertexIndices[i*3+2]];

            if (FTexCoords.Count = FVertices.Count) then begin
              uv1 := FTexCoords[FVertexIndices[i*3]];
              uv2 := FTexCoords[FVertexIndices[i*3+1]];
              uv3 := FTexCoords[FVertexIndices[i*3+2]];
            end
            else begin
              uv1 := AffineVectorMake(0, 0, 0);
              uv2 := AffineVectorMake(0, 0, 0);
              uv3 := AffineVectorMake(0, 0, 0);
            end;

            gl.Begin_(GL_TRIANGLES);
              gl.Normal3fv(@YVector);
              gl.TexCoord2f(uv1.V[0], uv1.V[1]); gl.Vertex2f(tv1.V[0], tv1.V[1]);
              gl.TexCoord2f(uv2.V[0], uv2.V[1]); gl.Vertex2f(tv2.V[0], tv2.V[1]);
              gl.TexCoord2f(uv3.V[0], uv3.V[1]); gl.Vertex2f(tv3.V[0], tv3.V[1]);
            gl.End_();
          end;
        end;
      end;
    end;

    // restore state
    gl.DepthMask(True);
    gl.PopAttrib;
    gl.MatrixMode(GL_PROJECTION);
    gl.PopMatrix;
    gl.MatrixMode(GL_MODELVIEW);
    gl.PopMatrix;
  until not Material.UnApply(rci);

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

// SetWidth
//
procedure TGLHUDShape.SetWidth(const val: TGLFloat);
begin
	if FWidth<>val then begin
		FWidth:=val;
		NotifyChange(Self);
	end;
end;

// SetHeight
//
procedure TGLHUDShape.SetHeight(const val: TGLFloat);
begin
	if FHeight<>val then begin
		FHeight:=val;
		NotifyChange(Self);
	end;
end;

// SetRotation
//
procedure TGLHUDShape.SetRotation(const val: TGLFloat);
begin
	if FRotation<>val then begin
		FRotation:=val;
		NotifyChange(Self);
	end;
end;

// SetSize
//
procedure TGLHUDShape.SetSize(const width, height: TGLFloat);
begin
	FWidth:=width;
	FHeight:=height;
   NotifyChange(Self);
end;

// SetSquareSize
//
procedure TGLHUDShape.SetSquareSize(const size: TGLFloat);
begin
	FWidth:=size;
	FHeight:=size;
   NotifyChange(Self);
end;

procedure TGLHUDShape.SetVertices(const val: TGLAffineVectorList);
begin
   FVertices.Assign(val);
end;

procedure TGLHUDShape.SetTexCoords(const val: TGLAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

procedure TGLHUDShape.SetVertexIndices(const val: TGLIntegerList);
begin
   FVertexIndices.Assign(val);
end;

initialization

   RegisterClasses([TGLHUDShape]);

end.

