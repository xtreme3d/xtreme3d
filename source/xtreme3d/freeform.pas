function FreeformCreate(fname: PAnsiChar; matl1,matl2,parent: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  ml: TGLMaterialLibrary;
  ml2: TGLMaterialLibrary;
begin
  ml:=TGLMaterialLibrary(RealToPtr(matl1));
  ml2:=TGLMaterialLibrary(RealToPtr(matl2));
  if not (parent=0) then
    GLFreeForm1:=TGLFreeForm.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLFreeForm1:=TGLFreeForm.CreateAsChild(scene.Objects);
  GLFreeForm1.MaterialLibrary:=ml;
  GLFreeForm1.LightmapLibrary:=ml2;
  GLFreeForm1.IgnoreMissingTextures := True;
  GLFreeForm1.AutoScaling.X := 1.0;
  GLFreeForm1.AutoScaling.Y := 1.0;
  GLFreeForm1.AutoScaling.Z := 1.0;
  
  try
    GLFreeForm1.LoadFromFile(StrConv(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('FreeformCreate:' + #13#10 + E.Message);
    end;
  end;
  
  result:=ObjToReal(GLFreeForm1);
end;

function FreeformGenTangents(ff: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  mesh1: TGLMeshObject;
  mi: Integer;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  for mi:=0 to GLFreeForm1.MeshObjects.Count-1 do begin
      mesh1 := GLFreeForm1.MeshObjects[mi];
      if (mesh1.Vertices.Count > 0) and (mesh1.TexCoords.Count > 0) then
        GenMeshTangents(mesh1);
  end;
  GLFreeForm1.StructureChanged;
  GLFreeForm1.NotifyChange(nil);
  result:=1.0;
end;

function FreeformMeshObjectsCount(ff: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  mobj: integer;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  mobj:=GLFreeForm1.MeshObjects.Count;
  result:=mobj;
end;

function FreeformMeshSetVisible(ff,mesh,mode: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  GLFreeForm1.MeshObjects[Trunc(mesh)].Visible:=boolean(Trunc(mode));
  GLFreeForm1.StructureChanged;
  result:=1;
end;

function FreeformMeshSetSecondCoords(ff1,mesh1,ff2,mesh2: real): real; cdecl;
var
  GLFreeForm1,GLFreeForm2: TGLFreeForm;
  tl: TGLAffineVectorList;
  sc: TGLAffineVectorList;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff1));
  GLFreeForm2:=TGLFreeForm(RealToPtr(ff2));
  tl:=GLFreeForm2.MeshObjects[Trunc(mesh2)].TexCoords;
  sc:=TGLAffineVectorList(tl);
  GLFreeForm1.MeshObjects[Trunc(mesh1)].LightMapTexCoords:=sc;
  result:=1;
end;

function FreeformMeshTriangleCount(ff,mesh: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  tri: integer;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  tri:=GLFreeForm1.MeshObjects[Trunc(mesh)].TriangleCount;
  result:=tri;
end;

function FreeformMeshObjectGetName(ff,mesh: real): PAnsiChar; cdecl;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  result:=PAnsiChar(AnsiString(GLFreeForm1.MeshObjects.Items[Trunc(mesh)].Name));
end;

function FreeformMeshObjectSetName(ff,mesh: real; name: PAnsiChar): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  GLFreeForm1.MeshObjects.Items[Trunc(mesh)].Name:=String(AnsiString(name));
  result:=1;
end;

function FreeformMeshObjectDestroy(ff,mesh: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  GLFreeForm1.MeshObjects.Items[Trunc(mesh)].Destroy;
  result:=1;
end;

function FreeformMeshFaceGroupsCount(ff,mesh: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  fgr: integer;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  fgr:=GLFreeForm1.MeshObjects[Trunc(mesh)].FaceGroups.Count;
  result:=fgr;
end;

function FreeformMeshFaceGroupTriangleCount(ff,mesh,fgr: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  tri: integer;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  tri:=GLFreeForm1.MeshObjects[Trunc(mesh)].FaceGroups[Trunc(fgr)].TriangleCount;
  result:=tri;
end;

// Change: explosion creation API changed
function FreeformCreateExplosionFX(ff1, enable: real): real; cdecl;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
  cache: TGLMeshObjectList;
begin
  ffm:=TGLFreeForm(RealToPtr(ff1));
  result:=0;
  if ffm.Effects.Count > 0 then
      Exit;
  cache:=TGLMeshObjectList.Create;
  cache.Assign(ffm.MeshObjects);
  ffm.TagObject := cache;
  exp:= TGLBExplosionFX.Create(ffm.Effects);
  exp.MaxSteps:= 0;
  exp.Speed:= 0.1;
  exp.Enabled:=Boolean(Trunc(enable));
  result:=ObjToReal(exp);
end;

function FreeformExplosionFXReset(ff1: real): real; cdecl;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
begin
  ffm:=TGLFreeForm(RealToPtr(ff1));
  exp:=TGLBExplosionFx(ffm.effects.items[0]);
  exp.Reset;
  exp.Enabled:=false;
  ffm.MeshObjects.Assign(TGLMeshObjectList(ffm.TagObject));
  ffm.StructureChanged;
  result := 1;
end;

function FreeformExplosionFXEnable(ff1, mode: real): real; cdecl;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
begin
  ffm:=TGLFreeForm(RealToPtr(ff1));
  exp:=TGLBExplosionFx(ffm.effects.items[0]);
  exp.Enabled:=Boolean(RealToPtr(mode));
  result := 1;
end;

function FreeformExplosionFXSetSpeed(ff1, speed: real): real; cdecl;
var
  ffm: TGLFreeForm;
  exp: TGLBExplosionFx;
begin
  ffm:=TGLFreeForm(RealToPtr(ff1));
  exp:=TGLBExplosionFx(ffm.effects.items[0]);
  exp.Speed := speed;
  result := 1;
end;

function FreeformSphereSweepIntersect(freeform, obj, radius, vel: real): real; cdecl;
var
  ffm: TGLFreeForm;
  ob: TGLBaseSceneObject;
  start, dir: TGLVector;
  res: Boolean;
begin
  ffm := TGLFreeForm(RealToPtr(freeform));
  ob := TGLBaseSceneObject(RealToPtr(obj));
  start := ob.Position.AsVector;
  dir := ob.Direction.AsVector;
  res := ffm.OctreeSphereSweepIntersect(start, dir, vel, radius);
  result := Integer(res);
end;

function FreeformPointInMesh(freeform, x, y, z: real): real; cdecl;
var
  ffm: TGLFreeForm;
  p: TGLVector;
  res: Boolean;
begin
  ffm := TGLFreeForm(RealToPtr(freeform));
  p := VectorMake(x, y, z);
  res := ffm.OctreePointInMesh(p);
  result := Integer(res);
end;

function FreeformMeshSetMaterial(ff,mesh: real; material: PAnsiChar): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  i: Integer;
  me: TGLMeshObject;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  me := GLFreeForm1.MeshObjects[Trunc(mesh)];
  for i:=0 to me.FaceGroups.Count-1 do
  begin
    me.FaceGroups[i].MaterialName := String(AnsiString(material));
  end;
  GLFreeForm1.StructureChanged;
  result:=1;
end;

function FreeformUseMeshMaterials(ff,mode: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  GLFreeForm1.UseMeshMaterials:=boolean(Trunc(mode));
  result:=1;
end;

function FreeformToFreeforms(freeform, parent: real): real; cdecl;
var
  ffm, ffm2: TGLFreeForm;
  mi, fgi, vi: Integer;
  mesh, mesh2: TGLMeshObject;
  fg: TFGVertexIndexList;
  fg2: TFGVertexIndexList;
  centroid: TAffineVector;
  one: TAffineVector;
begin
  ffm := TGLFreeForm(RealToPtr(freeform));
  one := AffineVectorMake(1, 1, 1);
  
  for mi:=0 to ffm.MeshObjects.Count-1 do begin
    mesh := ffm.MeshObjects[mi];

    if not (parent=0) then
      ffm2 := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
      ffm2 := TGLFreeForm.CreateAsChild(scene.Objects);
    mesh2 := TGLMeshObject.CreateOwned(ffm2.MeshObjects);
    
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
      fg2 := TFGVertexIndexList.CreateOwned(mesh2.FaceGroups);
      fg2.Mode := fgmmTriangles;
      fg2.VertexIndices := fg.VertexIndices;
      //fg2.NormalIndices := fg.VertexIndices;
      //fg2.TexCoordIndices := fg.VertexIndices;
      fg2.MaterialName := fg.MaterialName;
      fg2.LightMapIndex := fg.LightMapIndex;
      fg2.PrepareMaterialLibraryCache(ffm2.MaterialLibrary);
    end;

  end;
  result := 1.0;
end;

function FreeformMeshFaceGroupSetMaterial(ff,mesh,fg: real; matname: PAnsiChar): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  faceGroup := TFGVertexNormalTexIndexList(meshObj.FaceGroups[Trunc(fg)]);

  faceGroup.MaterialName := String(AnsiString(matname));
  faceGroup.PrepareMaterialLibraryCache(freeform.MaterialLibrary);
  freeform.StructureChanged;
  result:=1;
end;

function FreeformMeshFaceGroupGetMaterial(ff,mesh,fgroup: real): PAnsiChar; cdecl;
var
  GLFreeForm1: TGLFreeForm;
  me: TGLMeshObject;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  me := GLFreeForm1.MeshObjects[Trunc(mesh)];
  result:=PAnsiChar(AnsiString(me.FaceGroups[Trunc(fgroup)].MaterialName));
end;

function FreeformCreateEmpty(matlib1, matlib2, parent: real): real; cdecl;
var
  freeform: TGLFreeForm;
  ml: TGLMaterialLibrary;
  ml2: TGLMaterialLibrary;
begin
  ml := TGLMaterialLibrary(RealToPtr(matlib1));
  ml2 := TGLMaterialLibrary(RealToPtr(matlib2));
  if not (parent=0) then
    freeform := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    freeform := TGLFreeForm.CreateAsChild(scene.Objects);
  freeform.MaterialLibrary := ml;
  freeform.LightmapLibrary := ml2;
  freeform.UseMeshMaterials := True;
  result:=ObjToReal(freeform);
end;

function FreeformAddMesh(ff: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := TGLMeshObject.CreateOwned(freeform.MeshObjects);
  meshObj.Mode := momFaceGroups;
  result := freeform.MeshObjects.Count - 1;
end;

function FreeformMeshAddFaceGroup(ff, mesh: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  TFGVertexNormalTexIndexList.CreateOwned(meshObj.FaceGroups);
  result := meshObj.FaceGroups.Count - 1;
end;

function FreeformMeshAddVertex(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  result := meshObj.Vertices.Add(x, y, z);
end;

function FreeformMeshAddNormal(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  result := meshObj.Normals.Add(x, y, z);
end;

function FreeformMeshAddTexCoord(ff, mesh, u, v: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  result := meshObj.TexCoords.Add(u, v);
end;

function FreeformMeshAddSecondTexCoord(ff, mesh, u, v: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  result := meshObj.LightMapTexCoords.Add(u, v);
end;

function FreeformMeshAddTangent(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  meshObj.Tangents.Add(x, y, z, 1.0);
  result := 1.0;
end;

function FreeformMeshAddBinormal(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  meshObj.Binormals.Add(x, y, z, 1.0);
  result := 1.0;
end;

function FreeformMeshFaceGroupAddTriangle(ff, mesh, fg, i1, i2, i3: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  faceGroup := TFGVertexNormalTexIndexList(meshObj.FaceGroups[Trunc(fg)]);
  faceGroup.VertexIndices.Add(Trunc(i1), Trunc(i2), Trunc(i3));
  faceGroup.NormalIndices.Add(Trunc(i1), Trunc(i2), Trunc(i3));
  faceGroup.TexCoordIndices.Add(Trunc(i1), Trunc(i2), Trunc(i3));
  result := faceGroup.VertexIndices.Count - 1;
end;

function FreeformMeshGenNormals(ff, mesh: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
  indices: TGLIntegerList;
  i: Integer;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  if (meshObj.Vertices.Count > 0) and (meshObj.FaceGroups.Count > 0) then
  begin
    indices := TGLIntegerList.Create;
    for i:=0 to meshObj.FaceGroups.Count-1 do
    begin
      indices.Add(TFGVertexNormalTexIndexList(meshObj.FaceGroups[i]).VertexIndices);
    end;
    meshObj.BuildNormals(indices, momTriangles);
    indices.Free;
  end;
  result := 1.0;
end;

function FreeformMeshGenTangents(ff, mesh: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  if (meshObj.Vertices.Count > 0) and (meshObj.TexCoords.Count > 0) then
    GenMeshTangents(meshObj);
  result := 1.0;
end;

function FreeformMeshVerticesCount(ff, mesh: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  result := meshObj.Vertices.Count;
end;

function FreeformMeshTranslate(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
  v: TAffineVector;
  m: TGLMatrix;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  v := AffineVectorMake(x, y, z);
  m := CreateTranslationMatrix(v);
  meshObj.Vertices.TransformAsPoints(m);
  result := 1.0;
end;

function FreeformMeshRotate(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
  m: TGLMatrix;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  
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

function FreeformMeshScale(ff, mesh, x, y, z: real): real; cdecl;
var
  freeform: TGLFreeForm;
  meshObj: TGLMeshObject;
  v: TAffineVector;
  m: TGLMatrix;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  meshObj := freeform.MeshObjects[Trunc(mesh)];
  v := AffineVectorMake(x, y, z);
  m := CreateScaleMatrix(v);
  meshObj.Vertices.TransformAsPoints(m);
  result := 1.0;
end;

function FreeformSave(ff: real; filename: PAnsiChar): real; cdecl;
var
  freeform: TGLFreeForm;
begin
  freeform := TGLFreeForm(RealToPtr(ff));
  freeform.SaveToFile(StrConv(filename));
  result := 1.0;
end;

function FreeformMeshGetVertex(ff, mesh, v, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  result := ffm.MeshObjects[Trunc(mesh)].Vertices.Items[Trunc(v)].V[Trunc(index)];
end;

function FreeformMeshGetNormal(ff, mesh, n, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  result := ffm.MeshObjects[Trunc(mesh)].Normals.Items[Trunc(n)].V[Trunc(index)];
end;

function FreeformMeshGetTexCoord(ff, mesh, t, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  result := ffm.MeshObjects[Trunc(mesh)].TexCoords.Items[Trunc(t)].V[Trunc(index)];
end;

function FreeformMeshGetSecondTexCoord(ff, mesh, t, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
  i: Integer;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  i := Trunc(index);
  if i = 0 then
    result := ffm.MeshObjects[Trunc(mesh)].LightMapTexCoords.Items[Trunc(t)].X
  else if i = 1 then
    result := ffm.MeshObjects[Trunc(mesh)].LightMapTexCoords.Items[Trunc(t)].Y
  else
    result := 0;
end;

function FreeformMeshGetTangent(ff, mesh, t, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  result := ffm.MeshObjects[Trunc(mesh)].Tangents.Items[Trunc(t)].V[Trunc(index)];
end;

function FreeformMeshGetBinormal(ff, mesh, b, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  result := ffm.MeshObjects[Trunc(mesh)].Binormals.Items[Trunc(b)].V[Trunc(index)];
end;

function FreeformMeshFaceGroupGetIndex(ff, mesh, fg, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
  faceGroup: TFGVertexIndexList;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  faceGroup := TFGVertexIndexList(ffm.MeshObjects[Trunc(mesh)].FaceGroups[Trunc(fg)]);
  result := faceGroup.VertexIndices[Trunc(index)];
end;

function FreeformMeshSetVertex(ff, mesh, v, x, y, z: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MeshObjects[Trunc(mesh)].Vertices[Trunc(v)] := AffineVectorMake(x, y, z);
  result := 1.0;
end;

function FreeformMeshSetNormal(ff, mesh, n, x, y, z: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MeshObjects[Trunc(mesh)].Normals[Trunc(n)] := AffineVectorMake(x, y, z);
  result := 1.0;
end;

function FreeformMeshSetTexCoord(ff, mesh, t, u, v: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MeshObjects[Trunc(mesh)].TexCoords[Trunc(t)] := AffineVectorMake(u, v, 1); 
  result := 1.0;
end;

function FreeformMeshSetSecondTexCoord(ff, mesh, t, u, v: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MeshObjects[Trunc(mesh)].LightMapTexCoords.Items[Trunc(t)] := AffineVectorMake(u, v, 1); //TexPointMake(u, v);
  result := 1.0;
end;

function FreeformMeshSetTangent(ff, mesh, t, x, y, z: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MeshObjects[Trunc(mesh)].Tangents[Trunc(t)] := VectorMake(x, y, z, 0.0);
  result := 1.0;
end;

function FreeformMeshSetBinormal(ff, mesh, b, x, y, z: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MeshObjects[Trunc(mesh)].Binormals[Trunc(b)] := VectorMake(x, y, z, 0.0);
  result := 1.0;
end;

function FreeformMeshFaceGroupSetIndex(ff, mesh, fg, index, i: real): real; cdecl;
var
  ffm: TGLFreeForm;
  faceGroup: TFGVertexIndexList;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  faceGroup := TFGVertexIndexList(ffm.MeshObjects[Trunc(mesh)].FaceGroups[Trunc(fg)]);
  faceGroup.VertexIndices[Trunc(index)] := Trunc(i);
  result := 1.0;
end;

function FreeformBuildOctree(ff: real): real; cdecl;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(RealToPtr(ff));
  GLFreeForm1.BuildOctree;
  result:=1.0;
end;

function FreeformMeshFaceGroupGetLightmapIndex(ff, mesh, fg: real): real; cdecl;
var
  ffm: TGLFreeForm;
  meshObj: TGLMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  meshObj := ffm.MeshObjects[Trunc(mesh)];
  faceGroup := TFGVertexNormalTexIndexList(meshObj.FaceGroups[Trunc(fg)]);
  result := faceGroup.LightMapIndex;
end;

function FreeformMeshFaceGroupSetLightmapIndex(ff, mesh, fg, index: real): real; cdecl;
var
  ffm: TGLFreeForm;
  meshObj: TGLMeshObject;
  faceGroup: TFGVertexNormalTexIndexList;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  meshObj := ffm.MeshObjects[Trunc(mesh)];
  faceGroup := TFGVertexNormalTexIndexList(meshObj.FaceGroups[Trunc(fg)]);
  faceGroup.LightMapIndex := Trunc(index);
  result := 1.0;
end;

function FreeformSetMaterialLibraries(ff, matlib, lmmatlib: real): real; cdecl;
var
  ffm: TGLFreeForm;
begin
  ffm := TGLFreeForm(RealToPtr(ff));
  ffm.MaterialLibrary := TGLMaterialLibrary(Trunc(matlib));
  ffm.LightmapLibrary := TGLMaterialLibrary(Trunc(lmmatlib));
  result := 1.0;
end;

function BaseMeshBuildSilhouetteConnectivityData(obj: real): real; cdecl;
var
    baseMesh: TGLBaseMesh;
begin
    baseMesh := TGLBaseMesh(RealToPtr(obj));
    baseMesh.BuildSilhouetteConnectivityData;
    result := 1.0;
end;

