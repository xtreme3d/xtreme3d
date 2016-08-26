function OctreeCreate(maxdepth, leafthreshold, growgravy, culling: real): real; stdcall;
var
  octree: TOctreeSpacePartition;
begin
  octree := TOctreeSpacePartition.Create();
  octree.MaxTreeDepth := trunc64(maxdepth);
  octree.LeafThreshold := trunc64(leafthreshold);
  octree.GrowGravy := growgravy;
  if culling = 0 then octree.CullingMode := cmFineCulling;
  if culling = 1 then octree.CullingMode := cmGrossCulling;
  result := Integer(octree);
end;

function QuadtreeCreate(maxdepth, leafthreshold, growgravy, culling: real): real; stdcall;
var
  quadtree: TQuadtreeSpacePartition;
begin
  quadtree := TQuadtreeSpacePartition.Create();
  quadtree.MaxTreeDepth := trunc64(maxdepth);
  quadtree.LeafThreshold := trunc64(leafthreshold);
  quadtree.GrowGravy := growgravy;
  if culling = 0 then quadtree.CullingMode := cmFineCulling;
  if culling = 1 then quadtree.CullingMode := cmGrossCulling;
  result := Integer(quadtree);
end;

function PartitionDestroy(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  t.Free;
  result := 1.0;
end;

function PartitionAddLeaf(tree, obj: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  tobj: TSceneObj;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  tobj := TSceneObj.CreateObj(t, TGLBaseSceneObject(trunc64(obj)));
  result := Integer(tobj);
end;

function PartitionLeafChanged(leaf: real): real; stdcall;
var
  pleaf: TSpacePartitionLeaf;
begin
  pleaf := TSpacePartitionLeaf(trunc64(leaf));
  pleaf.Changed;
  result := 1.0;
end;

function PartitionQueryFrustum(tree, viewer: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  v: TGLSceneViewer;
  mvp: TMatrix;
  frustum : TFrustum;
begin
  v := TGLSceneViewer(trunc64(viewer));
  t := TSectoredSpacePartition(trunc64(tree));
  mvp := MatrixMultiply(v.Buffer.ModelViewMatrix, v.Buffer.ProjectionMatrix);
  frustum := ExtractFrustumFromModelViewProjection(mvp);
  result := t.QueryFrustum(frustum);
end;

function PartitionQueryLeaf(tree, leaf: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  pleaf: TSpacePartitionLeaf;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  pleaf := TSpacePartitionLeaf(trunc64(leaf));
  result := t.QueryLeaf(pleaf);
end;

function PartitionQueryAABB(tree, obj: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  o: TGLBaseSceneObject;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  o := TGLBaseSceneObject(trunc64(obj));
  result := t.QueryAABB(o.AxisAlignedBoundingBox);
end;

function PartitionQueryBSphere(tree, obj: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  o: TGLBaseSceneObject;
  bs: TBSphere;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  o := TGLBaseSceneObject(trunc64(obj));
  bs.Center := AffineVectorMake(o.AbsolutePosition);
  bs.Radius := o.BoundingSphereRadius;
  result := t.QueryBSphere(bs);
end;

function PartitionGetNodeTests(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  result := t.QueryNodeTests;
end;

function PartitionGetNodeCount(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  result := t.GetNodeCount;
end;

function PartitionGetResult(tree, ind: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  sobj: TSceneObj;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  sobj := t.QueryResult[trunc64(ind)] as TSceneObj;
  result := Integer(sobj.Obj);
end;

function PartitionGetResultCount(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  result := t.QueryResult.Count;
end;

function PartitionResultShow(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  sobj: TSceneObj;
  i: Integer;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  for i := 0 to t.QueryResult.Count-1 do
  begin
    sobj := t.QueryResult[i] as TSceneObj;
    sobj.Obj.Visible := true;
  end;
  result := 1.0;
end;

function PartitionResultHide(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  sobj: TSceneObj;
  i: Integer;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  for i := 0 to t.QueryResult.Count-1 do
  begin
    sobj := t.QueryResult[i] as TSceneObj;
    sobj.Obj.Visible := false;
  end;
  result := 1.0;
end;
