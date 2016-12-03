function FreeformCreate(fname: pchar; matl1,matl2,parent: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  ml: TGLMaterialLibrary;
  ml2: TGLMaterialLibrary;
  mesh1: TMeshObject;
  mi: Integer;
begin
  ml:=TGLMaterialLibrary(trunc64(matl1));
  ml2:=TGLMaterialLibrary(trunc64(matl2));
  if not (parent=0) then
    GLFreeForm1:=TGLFreeForm.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLFreeForm1:=TGLFreeForm.CreateAsChild(scene.Objects);
  GLFreeForm1.MaterialLibrary:=ml;
  GLFreeForm1.LightmapLibrary:=ml2;
  GLFreeForm1.LoadFromFile(fname);
  
  {
  // TODO: make separate functions for this
  
  for mi:=0 to GLFreeForm1.MeshObjects.Count-1 do begin
      mesh1 := GLFreeForm1.MeshObjects[mi];
      if (mesh1.Vertices.Count > 0) and (mesh1.TexCoords.Count > 0) then
        GenMeshTangents(mesh1);
  end;
  
  GLFreeForm1.BuildOctree;
  }
  
  result:=Integer(GLFreeForm1);
end;

// MeshCountObjects = FreeformMeshObjectsCount
function FreeformMeshObjectsCount(ff: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  mobj: integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  mobj:=GLFreeForm1.MeshObjects.Count;
  result:=mobj;
end;

// MeshSetVisible = FreeformMeshSetVisible
function FreeformMeshSetVisible(ff,mesh,mode: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  GLFreeForm1.MeshObjects[trunc64(mesh)].Visible:=boolean(trunc64(mode));
  GLFreeForm1.StructureChanged;
  result:=1;
end;

function FreeformMeshSetSecondCoords(ff1,mesh1,ff2,mesh2: real): real; stdcall;
var
  GLFreeForm1,GLFreeForm2: TGLFreeForm;
  tl: TAffineVectorList;
  sc: TTexPointList;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff1));
  GLFreeForm2:=TGLFreeForm(trunc64(ff2));
  tl:=GLFreeForm2.MeshObjects[trunc64(mesh2)].TexCoords;
  sc:=TTexPointList(tl);
  GLFreeForm1.MeshObjects[trunc64(mesh1)].LightMapTexCoords:=sc;
  result:=1;
end;

function FreeformMeshTriangleCount(ff,mesh: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  tri: integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  tri:=GLFreeForm1.MeshObjects[trunc64(mesh)].TriangleCount;
  result:=tri;
end;

function FreeformMeshFaceGroupsCount(ff,mesh: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  fgr: integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  fgr:=GLFreeForm1.MeshObjects[trunc64(mesh)].FaceGroups.Count;
  result:=fgr;
end;

function FreeformMeshFaceGroupTriangleCount(ff,mesh,fgr: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  tri: integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  tri:=GLFreeForm1.MeshObjects[trunc64(mesh)].FaceGroups[trunc64(fgr)].TriangleCount;
  result:=tri;
end;

// Change: explosion creation API changed
function FreeformCreateExplosionFX(ff1, enable: real): real; stdcall;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
  cache: TMeshObjectList;
begin
  ffm:=TGLFreeForm(trunc64(ff1));
  result:=0;
  if ffm.Effects.Count > 0 then
      Exit;
  cache:=TMeshObjectList.Create;
  cache.Assign(ffm.MeshObjects);
  ffm.TagObject := cache;
  exp:= TGLBExplosionFX.Create(ffm.Effects);
  exp.MaxSteps:= 0;
  exp.Speed:= 0.1;
  exp.Enabled:=Boolean(trunc64(enable));
  result:=Integer(exp);
end;

function FreeformExplosionFXReset(ff1: real): real; stdcall;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
begin
  ffm:=TGLFreeForm(trunc64(ff1));
  exp:=TGLBExplosionFx(ffm.effects.items[0]);
  exp.Reset;
  exp.Enabled:=false;
  ffm.MeshObjects.Assign(TMeshObjectList(ffm.TagObject));
  ffm.StructureChanged;
  result := 1;
end;

function FreeformExplosionFXEnable(ff1, mode: real): real; stdcall;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
begin
  ffm:=TGLFreeForm(trunc64(ff1));
  exp:=TGLBExplosionFx(ffm.effects.items[0]);
  exp.Enabled:=Boolean(trunc64(mode));
  result := 1;
end;

function FreeformExplosionFXSetSpeed(ff1, speed: real): real; stdcall;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
begin
  ffm:=TGLFreeForm(trunc64(ff1));
  exp:=TGLBExplosionFx(ffm.effects.items[0]);
  exp.Speed := speed;
  result := 1;
end;

function FreeformSphereSweepIntersect(freeform, obj, radius, vel: real): real; stdcall;
var
  ffm: TGLFreeForm;
  ob: TGLBaseSceneObject;
  start, dir: TVector;
  res: Boolean;
begin
  ffm := TGLFreeForm(trunc64(freeform));
  ob := TGLBaseSceneObject(trunc64(obj));
  start := ob.Position.AsVector;
  dir := ob.Direction.AsVector;
  res := ffm.OctreeSphereSweepIntersect(start, dir, vel, radius);
  result := Integer(res);
end;

function FreeformPointInMesh(freeform, x, y, z: real): real; stdcall;
var
  ffm: TGLFreeForm;
  p: TVector;
  res: Boolean;
begin
  ffm := TGLFreeForm(trunc64(freeform));
  p := VectorMake(x, y, z);
  res := ffm.OctreePointInMesh(p);
  result := Integer(res);
end;

function FreeformMeshSetMaterial(ff,mesh: real; material: pchar): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  i: Integer;
  me: TMeshObject;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  me := GLFreeForm1.MeshObjects[trunc64(mesh)];
  for i:=0 to me.FaceGroups.Count-1 do
  begin
    me.FaceGroups[i].MaterialName := String(material); 
  end;
  GLFreeForm1.StructureChanged;
  result:=1;
end;

function FreeformUseMeshMaterials(ff,mode: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  GLFreeForm1.UseMeshMaterials:=boolean(trunc64(mode));
  result:=1;
end;

function FreeformToFreeforms(freeform, parent: real): real; stdcall;
var
  ffm, ffm2: TGLFreeForm;
  mi, fgi, vi, tci: Integer;
  mesh, mesh2: TMeshObject;
  fg: TFGVertexIndexList;
  fg2: TFGVertexNormalTexIndexList;
  centroid: TAffineVector;
  one: TAffineVector;
  divisor: Single;
begin
  ffm := TGLFreeForm(trunc64(freeform));
  one := AffineVectorMake(1, 1, 1);
  
  for mi:=0 to ffm.MeshObjects.Count-1 do begin
    mesh := ffm.MeshObjects[mi];

    if not (parent=0) then
      ffm2 := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
    else
      ffm2 := TGLFreeForm.CreateAsChild(scene.Objects);
    mesh2 := TMeshObject.CreateOwned(ffm2.MeshObjects);
    
    ffm2.MaterialLibrary:=ffm.MaterialLibrary;
    ffm2.LightmapLibrary:=ffm.LightmapLibrary;
    ffm2.UseMeshMaterials:=True;

    // Calc centroid
    centroid := AffineVectorMake(0, 0, 0);
    for vi:=0 to mesh.Vertices.Count-1 do begin
      mesh2.Vertices.Add(mesh.Vertices[vi]);
      centroid := VectorAdd(centroid, mesh2.Vertices[vi]);
    end;
    ffm2.Position.AsAffineVector := VectorDivide(centroid, mesh2.Vertices.Count);

    // Reposition vertices
    for vi:=0 to mesh2.Vertices.Count-1 do begin
      mesh2.Vertices[vi] := VectorSubtract(mesh2.Vertices[vi], ffm2.Position.AsAffineVector);
    end;

    for vi:=0 to mesh.Normals.Count-1 do begin
      mesh2.Normals.Add(mesh.Normals[vi]);
    end;

    for vi:=0 to mesh.TexCoords.Count-1 do begin
      mesh2.TexCoords.Add(mesh.TexCoords[vi]);
    end;

    for vi:=0 to mesh.LightMapTexCoords.Count-1 do begin
      mesh2.LightMapTexCoords.Add(mesh.LightMapTexCoords[vi]);
    end;

    mesh2.Mode := momFaceGroups;
    for fgi:=0 to mesh.FaceGroups.Count-1 do begin
      fg := TFGVertexIndexList(mesh.FaceGroups[fgi]);
      fg2 := TFGVertexNormalTexIndexList.CreateOwned(mesh2.FaceGroups);
      fg2.Mode := fgmmTriangles;
      fg2.VertexIndices := fg.VertexIndices;
      fg2.NormalIndices := fg.VertexIndices;
      fg2.TexCoordIndices := fg.VertexIndices;
      fg2.MaterialName := fg.MaterialName;
      fg2.LightMapIndex := fg.LightMapIndex;
      fg2.PrepareMaterialLibraryCache(ffm2.MaterialLibrary);
    end;

  end;
  result := 1.0;
end;

function FreeformMeshFaceGroupSetMaterial(ff,mesh,fg: real; matname: pchar): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  faceGroup := TFGVertexNormalTexIndexList(meshObj.FaceGroups[trunc64(fg)]);

  faceGroup.MaterialName := String(matname);
  faceGroup.PrepareMaterialLibraryCache(freeform.MaterialLibrary);
  freeform.StructureChanged;
  result:=1;
end;

function FreeformMeshFaceGroupGetMaterial(ff,mesh,fgroup: real): pchar; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  me: TMeshObject;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  me := GLFreeForm1.MeshObjects[trunc64(mesh)];
  result:=pchar(me.FaceGroups[trunc64(fgroup)].MaterialName);
end;

function FreeformCreateEmpty(matlib1, matlib2, parent: real): real; stdcall;
var
  freeform: TGLFreeForm;
  ml: TGLMaterialLibrary;
  ml2: TGLMaterialLibrary;
begin
  ml := TGLMaterialLibrary(trunc64(matlib1));
  ml2 := TGLMaterialLibrary(trunc64(matlib2));
  if not (parent=0) then
    freeform := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    freeform := TGLFreeForm.CreateAsChild(scene.Objects);
  freeform.MaterialLibrary := ml;
  freeform.LightmapLibrary := ml2;
  freeform.UseMeshMaterials := True;
  result:=Integer(freeform);
end;

function FreeformAddMesh(ff: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := TMeshObject.CreateOwned(freeform.MeshObjects);
  meshObj.Mode := momFaceGroups;
  result := freeform.MeshObjects.Count - 1;
end;

function FreeformMeshAddFaceGroup(ff, mesh: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  faceGroup := TFGVertexNormalTexIndexList.CreateOwned(meshObj.FaceGroups);
  result := meshObj.FaceGroups.Count - 1;
end;

function FreeformMeshAddVertex(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  result := meshObj.Vertices.Add(x, y, z);
end;

function FreeformMeshAddNormal(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  result := meshObj.Normals.Add(x, y, z);
end;

function FreeformMeshAddTexCoord(ff, mesh, u, v: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  result := meshObj.TexCoords.Add(u, v);
end;

function FreeformMeshAddSecondTexCoord(ff, mesh, u, v: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  result := meshObj.LightMapTexCoords.Add(u, v);
end;

function FreeformMeshAddTangent(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  meshObj.Tangents.Add(x, y, z, 1.0);
  result := 1.0;
end;

function FreeformMeshAddBinormal(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  meshObj.Binormals.Add(x, y, z, 1.0);
  result := 1.0;
end;

function FreeformMeshFaceGroupAddTriangle(ff, mesh, fg, i1, i2, i3: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  faceGroup := TFGVertexNormalTexIndexList(meshObj.FaceGroups[trunc64(fg)]);
  faceGroup.VertexIndices.Add(trunc64(i1), trunc64(i2), trunc64(i3));
  faceGroup.NormalIndices.Add(trunc64(i1), trunc64(i2), trunc64(i3));
  faceGroup.TexCoordIndices.Add(trunc64(i1), trunc64(i2), trunc64(i3));
  result := faceGroup.VertexIndices.Count - 1;
end;

function FreeformMeshGenNormals(ff, mesh: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  indices: TIntegerList;
  i: Integer;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  if (meshObj.Vertices.Count > 0) and (meshObj.FaceGroups.Count > 0) then
  begin
    indices := TIntegerList.Create;
    for i:=0 to meshObj.FaceGroups.Count-1 do
    begin
      indices.Add(TFGVertexNormalTexIndexList(meshObj.FaceGroups[i]).VertexIndices);
    end;
    meshObj.BuildNormals(indices, momTriangles);
    indices.Free;
  end;
  result := 1.0;
end;

function FreeformMeshGenTangents(ff, mesh: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  if (meshObj.Vertices.Count > 0) and (meshObj.TexCoords.Count > 0) then
    GenMeshTangents(meshObj);
  result := 1.0;
end;

// MeshCountVertices = FreeformMeshVerticesCount
function FreeformMeshVerticesCount(ff, mesh: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  result := meshObj.Vertices.Count;
end;

function FreeformMeshTranslate(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  v: TAffineVector;
  m: TMatrix;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  v := AffineVectorMake(x, y, z);
  m := CreateTranslationMatrix(v);
  meshObj.Vertices.TransformAsPoints(m);
  result := 1.0;
end;

// MeshRotate = FreeformMeshRotate
function FreeformMeshRotate(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  m: TMatrix;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  
  m := MatrixMultiply(
    MatrixMultiply(
      CreateRotationMatrixX(DegToRad(x)),
      CreateRotationMatrixY(DegToRad(y))),
    CreateRotationMatrixZ(DegToRad(x)));
  if meshObj.Vertices.Count > 0 then
    meshObj.Vertices.TransformAsVectors(m);
  if meshObj.Normals.Count > 0 then
    meshObj.Normals.TransformAsVectors(m);
  if (meshObj.Vertices.Count > 0) and (meshObj.TexCoords.Count > 0) then
    GenMeshTangents(meshObj);
  result := 1.0;
end;

function FreeformMeshScale(ff, mesh, x, y, z: real): real; stdcall;
var
  freeform: TGLFreeForm;
  meshObj: TMeshObject;
  v: TAffineVector;
  m: TMatrix;
begin
  freeform := TGLFreeForm(trunc64(ff));
  meshObj := freeform.MeshObjects[trunc64(mesh)];
  v := AffineVectorMake(x, y, z);
  m := CreateScaleMatrix(v);
  meshObj.Vertices.TransformAsPoints(m);
  result := 1.0;
end;

function FreeformSave(ff: real; filename: pchar): real; stdcall;
var
  freeform: TGLFreeForm;
begin
  freeform := TGLFreeForm(trunc64(ff));
  freeform.SaveToFile(String(filename));
  result := 1.0;
end;

// Unimplemented:
// MeshOptimize
// MeshSmoothFaces
