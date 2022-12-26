function TreeCreate(parent: real): real; cdecl;
var
  t: TGLTree;
begin
  if not (parent = 0) then
    t := TGLTree.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    t := TGLTree.CreateAsChild(scene.Objects);
  t.RebuildTree;
  result := ObjToReal(t);
end;

function TreeSetMaterials(tree: real; mfront, mback, mbranch: pchar): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.MaterialLibrary := matlib;
  t.LeafMaterialName := String(mfront);
  t.LeafBackMaterialName := String(mback);
  t.BranchMaterialName := String(mbranch);
  result := 1.0;
end;

function TreeSetBranchFacets(tree, facets: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.BranchFacets := trunc(facets);
  result := 1.0;
end;

function TreeBuildMesh(tree, parent: real): real; cdecl;
var
  t: TGLTree;
  ffm: TGLFreeForm;
  mesh1: TGLMeshObject;
  mi: Integer;
begin
  t := TGLTree(RealToPtr(tree));
  if not (parent = 0) then
    ffm := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    ffm := TGLFreeForm.CreateAsChild(scene.Objects);
  ffm.MaterialLibrary := t.MaterialLibrary;
  ffm.LightmapLibrary := t.MaterialLibrary;
  t.BuildMesh(ffm);

  for mi:=0 to ffm.MeshObjects.Count-1 do begin
      mesh1 := ffm.MeshObjects[mi];
      //mesh1.BuildTangentSpace();
      //GenMeshTangents(mesh1);
  end;

  ffm.BuildOctree;
  result := Integer(ffm);
end;

function TreeSetBranchNoise(tree, noise: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.BranchNoise := noise;
  result := 1.0;
end;

function TreeSetBranchAngle(tree, angle: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.BranchAngle := angle;
  result := 1.0;
end;

function TreeSetBranchSize(tree, size: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.BranchSize := size;
  result := 1.0;
end;

function TreeSetBranchRadius(tree, radius: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.BranchRadius := radius;
  result := 1.0;
end;

function TreeSetBranchTwist(tree, twist: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.BranchTwist := twist;
  result := 1.0;
end;

function TreeSetDepth(tree, depth: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.Depth := trunc(depth);
  result := 1.0;
end;

function TreeSetLeafSize(tree, leafsize: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.LeafSize := leafsize;
  result := 1.0;
end;

function TreeSetLeafThreshold(tree, threshold: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.LeafThreshold := threshold;
  result := 1.0;
end;

function TreeSetSeed(tree, seed: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(RealToPtr(tree));
  t.Seed := trunc(seed);
  result := 1.0;
end;
