function PolygonCreate(parent: real): real; stdcall;
var
  poly: TGLPolygon;
begin
  if not (parent=0) then
    poly := TGLPolygon.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    poly := TGLPolygon.CreateAsChild(scene.Objects);
  result := Integer(poly);
end;

function PolygonAddVertex(polygon, x, y, z: real): real; stdcall;
var
  poly: TGLPolygon;
begin
  poly := TGLPolygon(trunc64(polygon));
  poly.AddNode(x, y, z);
  result := poly.Nodes.Count-1;
end;

function PolygonSetVertexPosition(polygon, vertex, x, y, z: real): real; stdcall;
var
  poly: TGLPolygon;
begin
  poly := TGLPolygon(trunc64(polygon));
  poly.Nodes[trunc64(vertex)].AsVector := VectorMake(x, y, z, 1.0);
  result := 1;
end;

function PolygonDeleteVertex(polygon, vertex: real): real; stdcall;
var
  poly: TGLPolygon;
begin
  poly := TGLPolygon(trunc64(polygon));
  poly.Nodes.Delete(trunc64(vertex));
  result := 1;
end;
