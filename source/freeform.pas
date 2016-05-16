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
  
  for mi:=0 to GLFreeForm1.MeshObjects.Count-1 do begin
      mesh1 := GLFreeForm1.MeshObjects[mi];
      //mesh1.BuildTangentSpace();
      GenMeshTangents(mesh1);
  end;
  
  GLFreeForm1.BuildOctree;
  
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

function FreeformFaceGroupsCount(ff,mesh: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  fgr: integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  fgr:=GLFreeForm1.MeshObjects[trunc64(mesh)].FaceGroups.Count;
  result:=fgr;
end;

function FreeformFaceGroupTriangleCount(ff,mesh,fgr: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  tri: integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  tri:=GLFreeForm1.MeshObjects[trunc64(mesh)].FaceGroups[trunc64(fgr)].TriangleCount;
  result:=tri;
end;

// TODO: this seems not working
function FreeformSetLightmapsFromFreeform(ff1, ff2: real): real; stdcall;
var
  GLFreeForm1,GLFreeForm2: TGLFreeForm;
  mesh1, mesh2: TMeshObject;
  fg1, fg2: TFaceGroup;
  tc2: TAffineVectorList;
  sc: TTexPointList;
  tcc2, ti, mi, fgi, mati: Integer;
  lmapLib: TGLMaterialLibrary;
  lmapMat: String;
  mat: TGLLibMaterial;
  v: TAffineVector;
  t: TTexPoint;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff1));
  GLFreeForm2:=TGLFreeForm(trunc64(ff2));

  lmapLib := GLFreeForm2.LightmapLibrary;
  GLFreeForm1.LightmapLibrary := lmapLib;

  for mi:=0 to GLFreeForm1.MeshObjects.Count-1 do begin
    mesh1 := GLFreeForm1.MeshObjects[mi];
    mesh2 := GLFreeForm2.MeshObjects[mi];

    mesh1.LightMapTexCoords.Count := 0;
    for ti:=0 to mesh2.TexCoords.Count-1 do begin
      v := mesh2.TexCoords[ti];
      mesh1.LightMapTexCoords.Add(v[0], v[1]);
    end;

    for fgi:=0 to mesh1.FaceGroups.Count-1 do begin
      fg1 := mesh1.FaceGroups[fgi];
      fg2 := mesh2.FaceGroups[fgi];

      lmapMat := fg2.MaterialName;

      for mati:=0 to lmapLib.Materials.Count-1 do begin
        mat := lmapLib.Materials[mati];

        if ansicomparestr(mat.Name, lmapMat) = 0 then begin
          fg1.LightMapIndex := mati;
          
          Break;
        end;
      end;
    end;
  end;

  result:=1;
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
    end;

  end;
  result := 1.0;
end;

// Unimplemented:
// MeshRotate
// MeshCountVertices
// MeshOptimize
// MeshSmoothFaces


{
function FreeformCreateEmpty(matl1, matl2, parent: real): real; stdcall;
var
  ffm: TGLFreeForm;
  ml: TGLMaterialLibrary;
  ml2: TGLMaterialLibrary;
begin
  ml:=TGLMaterialLibrary(trunc64(matl1));
  ml2:=TGLMaterialLibrary(trunc64(matl2));
  if not (parent=0) then
    ffm := TGLFreeForm.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    ffm := TGLFreeForm.CreateAsChild(scene.Objects);
  ffm.MaterialLibrary:=ml;
  ffm.LightmapLibrary:=ml2;
  result:=Integer(ffm);
end;

function FreeformAddMesh(ff: real): real; stdcall;
var
  ffm: TGLFreeForm;
  mesh: TMeshObject;
begin
  ffm := TGLFreeForm(trunc64(ff));
  mesh := TMeshObject.CreateOwned(ffm.MeshObjects);
  result := ffm.MeshObjects.Count-1;
end;

function FreeformMeshAddFacegroup(ff, m: real; matname: pchar): real; stdcall;
var
  ffm: TGLFreeForm;
  mesh: TMeshObject;
  fgroup: TFaceGroup;
begin
  ffm := TGLFreeForm(trunc64(ff));
  mesh := ffm.MeshObjects[trunc64(m)];
  fgroup := TFaceGroup.CreateOwned(mesh.FaceGroups);
  fgroup.MaterialName := String(matname);
  result := mesh.FaceGroups.Count-1;
end;
}