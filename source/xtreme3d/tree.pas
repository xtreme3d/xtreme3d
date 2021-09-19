function TreeCreate(parent: real): real; cdecl;
var
  t: TGLTree;
begin
  if not (parent = 0) then
    t := TGLTree.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    t := TGLTree.CreateAsChild(scene.Objects);
  t.RebuildTree;
  result := Integer(t);
end;

function TreeSetMaterials(tree: real; mfront, mback, mbranch: pchar): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
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
  t := TGLTree(trunc64(tree));
  t.BranchFacets := trunc64(facets);
  result := 1.0;
end;

function TreeBuildMesh(tree, parent: real): real; cdecl;
var
  t: TGLTree;
  ffm: TGLFreeForm;
  mesh1: TMeshObject;
  mi: Integer;
begin
  t := TGLTree(trunc64(tree));
  if not (parent = 0) then
    ffm := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
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
  t := TGLTree(trunc64(tree));
  t.BranchNoise := noise;
  result := 1.0;
end;

function TreeSetBranchAngle(tree, angle: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.BranchAngle := angle;
  result := 1.0;
end;

function TreeSetBranchSize(tree, size: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.BranchSize := size;
  result := 1.0;
end;

function TreeSetBranchRadius(tree, radius: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.BranchRadius := radius;
  result := 1.0;
end;

function TreeSetBranchTwist(tree, twist: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.BranchTwist := twist;
  result := 1.0;
end;

function TreeSetDepth(tree, depth: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.Depth := trunc64(depth);
  result := 1.0;
end;

function TreeSetLeafSize(tree, leafsize: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.LeafSize := leafsize;
  result := 1.0;
end;

function TreeSetLeafThreshold(tree, threshold: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.LeafThreshold := threshold;
  result := 1.0;
end;

function TreeSetSeed(tree, seed: real): real; cdecl;
var
  t: TGLTree;
begin
  t := TGLTree(trunc64(tree));
  t.Seed := trunc64(seed);
  result := 1.0;
end;
