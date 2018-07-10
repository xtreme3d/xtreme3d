unit GLFileFBX;

interface

{$I GLScene.inc}

uses Dialogs, Graphics, Classes, SysUtils, 
     GLVectorFileObjects, ApplicationFileIO, VectorLists, VectorGeometry,
     GLTexture, PersistentClasses, GLGraphics, OpenGL1x, OpenFBX;
     
type

  RealArray = array [0..0] of Real;
  PRealArray = ^RealArray;
    
    TGLFBXVectorFile = class(TVectorFile)
    public
      class function Capabilities: TDataFileCapabilities; override;
      procedure LoadFromStream(aStream: TStream); override;
    end;
    
implementation

class function TGLFBXVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLFBXVectorFile.LoadFromStream(aStream: TStream);
var
  lmapLib: TGLMaterialLibrary;
  matLib: TGLMaterialLibrary;
  buf: array of Byte;

  pScene, pMesh: Pointer;
  numVerts: Integer;
  verts, norms: PRealArray;

  objPos, objRot, objScale: TAffineVector;
  objTrans: TMatrix;

  meshObj: TMeshObject;
  faceGroup: TFGVertexIndexList;
  mi, fgi, vi, ti: Integer;
  i0, i1, i2: Integer;
  index0, index1, index2: Cardinal;
  fileExt: string;
begin
  lmaplib := Owner.LightmapLibrary;
  matlib := Owner.MaterialLibrary;

  if not IsOpenFBXInitialized then
  begin
    if not InitOpenFBX('OpenFBX.dll') then
    begin
      ShowMessage('OpenFBX.dll required');
      Exit;
    end;
  end;

  SetLength(buf, aStream.Size);
  aStream.ReadBuffer(Pointer(buf)^, aStream.Size);

  fileExt := ExtractFileExt(ResourceName);

  pScene := fbxSceneLoad(Pointer(buf), Length(buf));

  owner.MeshObjects.Clear;

  for mi := 0 to fbxSceneGetMeshCount(pScene)-1 do
  begin
    pMesh := fbxSceneGetMesh(pScene, mi);

    meshObj := TMeshObject.CreateOwned(owner.MeshObjects);
    meshObj.Mode := momFaceGroups;
    faceGroup := TFGVertexIndexList.CreateOwned(meshObj.FaceGroups);

    numVerts := fbxMeshGetVertexCount(pMesh);
    verts := PRealArray(fbxMeshGetVertices(pMesh));
    norms := PRealArray(fbxMeshGetNormals(pMesh));

    for ti := 0 to numVerts-1 do
    begin
      meshObj.Vertices.Add(
        verts^[ti*3+0],
        verts^[ti*3+1],
        verts^[ti*3+2]);

      meshObj.Normals.Add(
        norms^[ti*3+0],
        norms^[ti*3+1],
        norms^[ti*3+2]);

      meshObj.TexCoords.Add(
        0,
        0);
      meshObj.LightMapTexCoords.Add(
        0,
        0);
    end;

    objPos := AffineVectorMake(
      fbxObjectGetLocalPosition(pMesh, 0),
      fbxObjectGetLocalPosition(pMesh, 1),
      fbxObjectGetLocalPosition(pMesh, 2));

    objRot := AffineVectorMake(
      fbxObjectGetLocalRotation(pMesh, 0),
      fbxObjectGetLocalRotation(pMesh, 1),
      fbxObjectGetLocalRotation(pMesh, 2));

    objScale := AffineVectorMake(
      fbxObjectGetLocalScaling(pMesh, 0),
      fbxObjectGetLocalScaling(pMesh, 1),
      fbxObjectGetLocalScaling(pMesh, 2));

    objTrans := MatrixMultiply(
      MatrixMultiply(
        CreateRotationMatrixX(DegToRad(objRot[0])),
        CreateRotationMatrixY(DegToRad(objRot[1]))),
      CreateRotationMatrixZ(DegToRad(objRot[2])));
    meshObj.Vertices.TransformAsVectors(objTrans);
    meshObj.Normals.TransformAsVectors(objTrans);
    
    objTrans := CreateScaleAndTranslationMatrix(VectorMake(objScale), VectorMake(objPos));
    meshObj.Vertices.TransformAsPoints(objTrans);
    meshObj.Normals.TransformAsVectors(objTrans);

    for ti := 0 to numVerts div 3 do
    begin
      faceGroup.VertexIndices.Add(ti*3, ti*3+1, ti*3+2);
    end;

    faceGroup.Mode := fgmmTriangles;
    
  end

  {  
  for mi := 0 to csm.numMeshes-1 do
  begin
    meshObj := TMeshObject.CreateOwned(owner.MeshObjects);
    meshObj.Mode := momFaceGroups;

    startIdx := 0;

    for fgi := 0 to csm.meshes[mi].numFacegroups-1 do
    begin
      for vi := 0 to csm.meshes[mi].facegroups[fgi].numVertices-1 do
      begin
        meshObj.Vertices.Add(
          csm.meshes[mi].facegroups[fgi].vertices[vi].v.X,
          csm.meshes[mi].facegroups[fgi].vertices[vi].v.Y,
          csm.meshes[mi].facegroups[fgi].vertices[vi].v.Z);
        meshObj.Normals.Add(
          csm.meshes[mi].facegroups[fgi].vertices[vi].n.X,
          csm.meshes[mi].facegroups[fgi].vertices[vi].n.Y,
          csm.meshes[mi].facegroups[fgi].vertices[vi].n.Z);
        meshObj.TexCoords.Add(
          csm.meshes[mi].facegroups[fgi].vertices[vi].texCoord1.X,
          csm.meshes[mi].facegroups[fgi].vertices[vi].texCoord1.Y);
        meshObj.LightMapTexCoords.Add(
          csm.meshes[mi].facegroups[fgi].vertices[vi].texCoord2.X,
          csm.meshes[mi].facegroups[fgi].vertices[vi].texCoord2.Y);
      end;

      faceGroup := TFGVertexIndexList.CreateOwned(meshObj.FaceGroups);
      for ti := 0 to csm.meshes[mi].facegroups[fgi].numTriangles-1 do
      begin
        faceGroup.VertexIndices.Add(
          csm.meshes[mi].facegroups[fgi].triangles[ti].i3 + startIdx,
          csm.meshes[mi].facegroups[fgi].triangles[ti].i2 + startIdx,
          csm.meshes[mi].facegroups[fgi].triangles[ti].i1 + startIdx);
      end;

      matName := csm.meshes[mi].facegroups[fgi].textureFilename;
      if matlib.LibMaterialByName(matName) = nil then
      begin
        mat := matlib.AddTextureMaterial(matName, matName);
        mat.Material.MaterialOptions := [moNoLighting];
        mat.Material.Texture.TextureMode := tmModulate;
      end;
      faceGroup.MaterialName := matName;
      lightmapID := csm.meshes[mi].facegroups[fgi].lightmapID;
      if lightmapID > 0 then
        faceGroup.LightMapIndex := csm.lightmaps[lightmapID-1].material.Index
      else
        matlib.LibMaterialByName(matName).Material.MaterialOptions := [];
      faceGroup.Mode := fgmmTriangles;

      startIdx := startIdx + csm.meshes[mi].facegroups[fgi].numVertices;
    end;
  end;
  }
end;

initialization
    RegisterVectorFileFormat('fbx', 'FBX File Format', TGLFBXVectorFile);
end.
