
function HUDShapeRectangleCreate(w, h, parent: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.SetSize(w, h);
  shp.ShapeType := hstRectangle;
  result := Integer(shp);
end;

function HUDShapeCircleCreate(radius, slices, startAng, endAng, parent: real): real; stdcall;
var
  shp: TGLHUDShape;
  i: Integer;
  angle, stepAngle, x, y: Single;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.SetSize(radius * 2, radius * 2);
  shp.ShapeType := hstCircle;
  shp.NumSlices := trunc64(slices);
  shp.StartAngle := startAng;
  shp.EndAngle := endAng;
  result := Integer(shp);
end;

function HUDShapeLineCreate(x1, y1, x2, y2, parent: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.Point1X := x1;
  shp.Point1Y := y1;
  shp.Point2X := x2;
  shp.Point2Y := y2;
  shp.SetSize(2, 2);
  shp.ShapeType := hstLine;
  result := Integer(shp);
end;

function HUDShapeMeshCreate(parent: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.SetSize(1, 1);
  shp.ShapeType := hstMesh;
  result := Integer(shp);
end;

function HUDShapeSetRotation(shape, angle: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.Rotation := angle;
  result := 1;
end;

function HUDShapeRotate(shape, angle: real): real; stdcall;
var
  shp: TGLHUDShape;
  rot: real;
begin
  shp := TGLHUDShape(trunc64(shape));
  rot := shp.Rotation;
  shp.Rotation := rot + angle;
  result := 1;
end;

function HUDShapeSetColor(shape, col, alpha: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.Color.AsWinColor := TColor(trunc64(col));
  shp.Color.Alpha := alpha;
  result := 1;
end;

function HUDShapeSetOrigin(shape, x, y: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.OriginX := x;
  shp.OriginY := y;
  result := 1;
end;

function HUDShapeSetSize(shape, w, h: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.SetSize(w, h);
  result := 1;
end;

function HUDShapeScale(shape, u, v: real): real; stdcall;
var
  shp: TGLHUDShape;
  w, h: real;
begin
  shp := TGLHUDShape(trunc64(shape));
  w := shp.Width;
  h := shp.Height;
  shp.SetSize(w + u, h + v);
  result := 1;
end;

function HUDShapeCircleSetRadius(shape, radius: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.SetSize(radius * 2, radius * 2);
  result := 1;
end;

function HUDShapeCircleSetSlices(shape, slices: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.NumSlices := trunc64(slices);
  result := 1;
end;

function HUDShapeCircleSetAngles(shape, startAng, endAng: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.StartAngle := startAng;
  shp.EndAngle := endAng;
  result := 1;
end;

function HUDShapeLineSetPoints(shape, x1, y1, x2, y2: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.Point1X := x1;
  shp.Point1Y := y1;
  shp.Point2X := x2;
  shp.Point2Y := y2;
  result := 1;
end;

function HUDShapeLineSetWidth(shape, w: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.LineWidth := w;
  result := 1;
end;

function HUDShapeMeshAddVertex(shape, x, y, u, v: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.Vertices.Add(x, y);
  shp.TexCoords.Add(u, v);
  result := shp.Vertices.Count - 1;
end;

function HUDShapeMeshAddTriangle(shape, v1, v2, v3: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.VertexIndices.Add(trunc64(v1), trunc64(v2), trunc64(v3));
  result := shp.VertexIndices.Count - 1;
end;

function HUDShapeMeshSetVertex(shape, index, x, y: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.Vertices.List[trunc64(index)][0] := x;
  shp.Vertices.List[trunc64(index)][1] := y;
  result := 1.0;
end;

function HUDShapeMeshSetTexCoord(shape, index, u, v: real): real; stdcall;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(trunc64(shape));
  shp.TexCoords.List[trunc64(index)][0] := u;
  shp.TexCoords.List[trunc64(index)][1] := v;
  result := 1.0;
end;
