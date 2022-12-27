function OctreeCreate(maxdepth, leafthreshold, growgravy, culling: real): real; cdecl;
var
  octree: TGLOctreeSpacePartition;
begin
  octree := TGLOctreeSpacePartition.Create();
  octree.MaxTreeDepth := trunc(maxdepth);
  octree.LeafThreshold := trunc(leafthreshold);
  octree.GrowGravy := growgravy;
  if culling = 0 then octree.CullingMode := cmFineCulling;
  if culling = 1 then octree.CullingMode := cmGrossCulling;
  result := ObjToReal(octree);
end;

function QuadtreeCreate(maxdepth, leafthreshold, growgravy, culling: real): real; cdecl;
var
  quadtree: TGLQuadtreeSpacePartition;
begin
  quadtree := TGLQuadtreeSpacePartition.Create();
  quadtree.MaxTreeDepth := trunc(maxdepth);
  quadtree.LeafThreshold := trunc(leafthreshold);
  quadtree.GrowGravy := growgravy;
  if culling = 0 then quadtree.CullingMode := cmFineCulling;
  if culling = 1 then quadtree.CullingMode := cmGrossCulling;
  result := ObjToReal(quadtree);
end;

function PartitionDestroy(tree: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  t.Free;
  result := 1.0;
end;

function PartitionAddLeaf(tree, obj: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  tobj: TGLSceneObj;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  tobj := TGLSceneObj.CreateObj(t, TGLBaseSceneObject(RealToPtr(obj)));
  result := ObjToReal(tobj);
end;

function PartitionLeafChanged(leaf: real): real; cdecl;
var
  pleaf: TGLSpacePartitionLeaf;
begin
  pleaf := TGLSpacePartitionLeaf(RealToPtr(leaf));
  pleaf.Changed;
  result := 1.0;
end;

function PartitionQueryFrustum(tree, viewer: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  v: TGLSceneViewer;
  mvp: TGLMatrix;
  frustum : TFrustum;
begin
  v := TGLSceneViewer(RealToPtr(viewer));
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  mvp := MatrixMultiply(MatrixMultiply(v.Buffer.ModelMatrix, v.Buffer.ViewMatrix), v.Buffer.ProjectionMatrix);
  frustum := ExtractFrustumFromModelViewProjection(mvp);
  result := t.QueryFrustum(frustum);
end;

function PartitionQueryLeaf(tree, leaf: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  pleaf: TGLSpacePartitionLeaf;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  pleaf := TGLSpacePartitionLeaf(RealToPtr(leaf));
  result := t.QueryLeaf(pleaf);
end;

function PartitionQueryAABB(tree, obj: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  o: TGLBaseSceneObject;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  o := TGLBaseSceneObject(RealToPtr(obj));
  result := t.QueryAABB(o.AxisAlignedBoundingBox);
end;

function PartitionQueryBSphere(tree, obj: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  o: TGLBaseSceneObject;
  bs: TBSphere;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  o := TGLBaseSceneObject(RealToPtr(obj));
  bs.Center := AffineVectorMake(o.AbsolutePosition);
  bs.Radius := o.BoundingSphereRadius;
  result := t.QueryBSphere(bs);
end;

function PartitionGetNodeTests(tree: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  result := t.QueryNodeTests;
end;

function PartitionGetNodeCount(tree: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  result := t.GetNodeCount;
end;

function PartitionGetResult(tree, ind: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  sobj: TGLSceneObj;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  sobj := t.QueryResult[trunc(ind)] as TGLSceneObj;
  result := ObjToReal(sobj.Obj);
end;

function PartitionGetResultCount(tree: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  result := t.QueryResult.Count;
end;

function PartitionResultShow(tree: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  sobj: TGLSceneObj;
  i: Integer;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  for i := 0 to t.QueryResult.Count-1 do
  begin
    sobj := t.QueryResult[i] as TGLSceneObj;
    sobj.Obj.Visible := true;
  end;
  result := 1.0;
end;

function PartitionResultHide(tree: real): real; cdecl;
var
  t: TGLSectoredSpacePartition;
  sobj: TGLSceneObj;
  i: Integer;
begin
  t := TGLSectoredSpacePartition(RealToPtr(tree));
  for i := 0 to t.QueryResult.Count-1 do
  begin
    sobj := t.QueryResult[i] as TGLSceneObj;
    sobj.Obj.Visible := false;
  end;
  result := 1.0;
end;
