
function HUDShapeRectangleCreate(w, h, parent: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.SetSize(w, h);
  shp.ShapeType := hstRectangle;
  result := Integer(shp);
end;

function HUDShapeCircleCreate(radius, slices, startAng, endAng, parent: real): real; cdecl;
var
  shp: TGLHUDShape;
  i: Integer;
  angle, stepAngle, x, y: Single;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.SetSize(radius * 2, radius * 2);
  shp.ShapeType := hstCircle;
  shp.NumSlices := Trunc(slices);
  shp.StartAngle := startAng;
  shp.EndAngle := endAng;
  result := Integer(shp);
end;

function HUDShapeLineCreate(x1, y1, x2, y2, parent: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.Point1X := x1;
  shp.Point1Y := y1;
  shp.Point2X := x2;
  shp.Point2Y := y2;
  shp.SetSize(1, 1);
  shp.ShapeType := hstLine;
  result := ObjToReal(shp);
end;

function HUDShapeMeshCreate(parent: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  if not (parent = 0) then
    shp := TGLHUDShape.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    shp := TGLHUDShape.CreateAsChild(scene.Objects);
  shp.SetSize(1, 1);
  shp.ShapeType := hstMesh;
  result := ObjToReal(shp);
end;

function HUDShapeSetRotation(shape, angle: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.Rotation := angle;
  result := 1;
end;

function HUDShapeRotate(shape, angle: real): real; cdecl;
var
  shp: TGLHUDShape;
  rot: real;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  rot := shp.Rotation;
  shp.Rotation := rot + angle;
  result := 1;
end;

function HUDShapeSetColor(shape, col, alpha: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.Color.AsWinColor := TColor(Trunc(col));
  shp.Color.Alpha := alpha;
  result := 1;
end;

function HUDShapeSetOrigin(shape, x, y: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.OriginX := x;
  shp.OriginY := y;
  result := 1;
end;

function HUDShapeSetSize(shape, w, h: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.SetSize(w, h);
  result := 1;
end;

function HUDShapeScale(shape, u, v: real): real; cdecl;
var
  shp: TGLHUDShape;
  w, h: real;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  w := shp.Width;
  h := shp.Height;
  shp.SetSize(w + u, h + v);
  result := 1;
end;

function HUDShapeCircleSetRadius(shape, radius: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.SetSize(radius * 2, radius * 2);
  result := 1;
end;

function HUDShapeCircleSetSlices(shape, slices: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.NumSlices := Trunc(slices);
  result := 1;
end;

function HUDShapeCircleSetAngles(shape, startAng, endAng: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.StartAngle := startAng;
  shp.EndAngle := endAng;
  result := 1;
end;

function HUDShapeLineSetPoints(shape, x1, y1, x2, y2: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.Point1X := x1;
  shp.Point1Y := y1;
  shp.Point2X := x2;
  shp.Point2Y := y2;
  result := 1;
end;

function HUDShapeLineSetWidth(shape, w: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.LineWidth := w;
  result := 1;
end;

function HUDShapeMeshAddVertex(shape, x, y, u, v: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.Vertices.Add(x, y);
  shp.TexCoords.Add(u, v);
  result := shp.Vertices.Count - 1;
end;

function HUDShapeMeshAddTriangle(shape, v1, v2, v3: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.VertexIndices.Add(Trunc(v1), Trunc(v2), Trunc(v3));
  result := shp.VertexIndices.Count - 1;
end;

function HUDShapeMeshSetVertex(shape, index, x, y: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.Vertices.List[Trunc(index)].V[0] := x;
  shp.Vertices.List[Trunc(index)].V[1] := y;
  result := 1.0;
end;

function HUDShapeMeshSetTexCoord(shape, index, u, v: real): real; cdecl;
var
  shp: TGLHUDShape;
begin
  shp := TGLHUDShape(RealToPtr(shape));
  shp.TexCoords.List[Trunc(index)].V[0] := u;
  shp.TexCoords.List[Trunc(index)].V[1] := v;
  result := 1.0;
end;
