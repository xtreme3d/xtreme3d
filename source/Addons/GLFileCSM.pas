{
  GLFileCSM
  Cartography Shop 4 CSM format loader

	History :
	  04/09/16 - Gecko - Unit created.
}
unit GLFileCSM;

interface

{$I GLScene.inc}

uses VCL.Dialogs, VCL.Graphics, System.Classes, System.SysUtils,
     GLS.VectorFileObjects, GLS.ApplicationFileIO, GLS.VectorLists, GLS.VectorGeometry,
     GLS.Texture, GLS.Material, GLS.PersistentClasses, GLS.Graphics;
     
type
    TCSMColor = record
      R: Integer;
      G: Integer;
      B: Integer;
    end;

    TCSMVec2 = record
      X: Single;
      Y: Single;
    end;

    TCSMVec3 = record
      X: Single;
      Y: Single;
      Z: Single;
    end;

    TCSMGroup = record
      flags: Integer;
      parentGroupID: Integer;
      props: String;
      color: TCSMColor;
    end;

    TCSMVisGroup = record
      name: String;
      flags: Integer;
      color: TCSMColor;
    end;

    TCSMLightmap = record
      width: Integer;
      height: Integer;
      name: String;
      data: array of Byte;
      bitmap: TBitmap;
      material: TGLLibMaterial;
    end;

    TCSMVertex = record
      v: TCSMVec3;
      n: TCSMVec3;
      color: TCSMColor;
      texCoord1: TCSMVec3;
      texCoord2: TCSMVec3;
    end;

    TCSMTriangle = record
      i1: Integer;
      i2: Integer;
      i3: Integer;
    end;

    TCSMLine = record
      i1: Integer;
      i2: Integer;
    end;

    TCSMFacegroup = record
      flags: Integer;
      textureFilename: String;
      lightmapID: Integer;
      texOffset: TCSMVec2;
      texScale: TCSMVec2;
      texRotation: Single;
      numVertices: Integer;
      numTriangles: Integer;
      numLines: Integer;
      vertices: array of TCSMVertex;
      triangles: array of TCSMTriangle;
      lines: array of TCSMLine;
    end;

    TCSMMesh = record
      flags: Integer;
      groupID: Integer;
      props: String;
      color: TCSMColor;
      position: TCSMVec3;
      visGroupID: Integer;
      numFacegroups: Integer;
      facegroups: array of TCSMFacegroup;
    end;

    TCSM = record
      ver: Integer;
      numGroups: Integer;
      groups: array of TCSMGroup;
      hasVisGroups: Boolean;
      numVisGroups: Integer;
      visGroups: array of TCSMVisGroup;
      numLightmaps: Integer;
      lightmaps: array of TCSMLightmap;
      numMeshes: Integer;
      meshes: array of TCSMMesh; 
    end;
    
    TGLCSMVectorFile = class(TGLVectorFile)
    public
      class function Capabilities: TGLDataFileCapabilities; override;
      procedure LoadFromStream(aStream: TStream); override;
    end;
    
implementation

function StreamReadString(aStream: TStream): String;
var
  startPos: Int64;
  buf: array of Byte;
  str: String;
  bufSize: Integer;
  ch: Byte;
  i: Integer;
begin
  startPos := aStream.Position;
  i := 0;
  while (i < aStream.Size) do
  begin
    aStream.Read(ch, 1);
    i := i + 1;
    if ch = 0 then
      break;
  end;
  aStream.Position := startPos;
  bufSize := i;

  SetLength(buf, bufSize);
  aStream.Read(buf[0], bufSize);

  if bufSize < 2 then
    Result := ''
  else
  begin
    SetString(str, PAnsiChar(@buf[0]), bufSize-1);
    Result := str;
  end;
end;

procedure CSMReadGroups(var csm: TCSM; aStream: TStream);
var
  i: Integer;
begin
  aStream.Read(csm.numGroups, 4);
  if (csm.numGroups > 0) then
  begin
    SetLength(csm.groups, csm.numGroups);

    for i := 0 to csm.numGroups-1 do
    begin
      aStream.Read(csm.groups[i].flags, 4);
      aStream.Read(csm.groups[i].parentGroupID, 4);
      csm.groups[i].props := StreamReadString(aStream);
      aStream.Read(csm.groups[i].color, SizeOf(TCSMColor));
    end;
  end;
end;

procedure CSMReadVisGroups(var csm: TCSM; aStream: TStream);
var
  i: Integer;
begin
  aStream.Read(csm.numVisGroups, 4);
  if (csm.numVisGroups > 0) then
  begin
    SetLength(csm.visGroups, csm.numVisGroups);

    for i := 0 to csm.numVisGroups-1 do
    begin
      csm.visGroups[i].name := StreamReadString(aStream);
      aStream.Read(csm.visGroups[i].flags, 4);
      aStream.Read(csm.visGroups[i].color, SizeOf(TCSMColor));
    end;
  end;
end;

procedure CSMReadLightmaps(var csm: TCSM; aStream: TStream);
var
  i, x, y, pixi, dataSize: Integer;
  bitmap: TBitmap;
  pos: Int64;
  p: PByteArray;
begin
  aStream.Read(csm.numLightmaps, 4);
  if (csm.numLightmaps > 0) then
  begin
    SetLength(csm.lightmaps, csm.numLightmaps);

    for i := 0 to csm.numLightmaps-1 do
    begin
      aStream.Read(csm.lightmaps[i].width, 4);
      aStream.Read(csm.lightmaps[i].height, 4);
      dataSize := csm.lightmaps[i].width * csm.lightmaps[i].height * 4;
      SetLength(csm.lightmaps[i].data, dataSize);
      aStream.Read(csm.lightmaps[i].data[0], dataSize);

      bitmap := TBitmap.Create;
      bitmap.Width := csm.lightmaps[i].width;
      bitmap.Height := csm.lightmaps[i].height;
      bitmap.PixelFormat := pf24bit;

      for y := 0 to csm.lightmaps[i].height-1 do   
      begin
        p := bitmap.ScanLine[y];
        for x := 0 to csm.lightmaps[i].width-1 do
        begin
          pixi := (y * csm.lightmaps[i].width + x) * 4;
          p[x*3+0] := csm.lightmaps[i].data[pixi+2]; // B
          p[x*3+1] := csm.lightmaps[i].data[pixi+1]; // G
          p[x*3+2] := csm.lightmaps[i].data[pixi+0]; // R
        end;
      end;

      csm.lightmaps[i].bitmap := bitmap;
    end;
  end;
end;

procedure CSMReadVertices(var csm: TCSM; var fgroup: TCSMFacegroup; aStream: TStream);
var
  i: Integer;
begin
  SetLength(fgroup.vertices, fgroup.numVertices);
  for i := 0 to fgroup.numVertices-1 do
  begin
    aStream.Read(fgroup.vertices[i].v.X, 4);
    aStream.Read(fgroup.vertices[i].v.Y, 4);
    aStream.Read(fgroup.vertices[i].v.Z, 4);
    
    aStream.Read(fgroup.vertices[i].n, SizeOf(TCSMVec3));
    aStream.Read(fgroup.vertices[i].color, SizeOf(TCSMColor));
    aStream.Read(fgroup.vertices[i].texCoord1, SizeOf(TCSMVec3));
    aStream.Read(fgroup.vertices[i].texCoord2, SizeOf(TCSMVec3));
  end;
end;

procedure CSMReadTriangles(var csm: TCSM; var fgroup: TCSMFacegroup; aStream: TStream);
var
  i: Integer;
begin
  SetLength(fgroup.triangles, fgroup.numTriangles);
  for i := 0 to fgroup.numTriangles-1 do
  begin
    aStream.Read(fgroup.triangles[i], SizeOf(TCSMTriangle));
  end;
end;

procedure CSMReadLines(var csm: TCSM; var fgroup: TCSMFacegroup; aStream: TStream);
var
  i: Integer;
begin
  SetLength(fgroup.lines, fgroup.numLines);
  for i := 0 to fgroup.numLines-1 do
  begin
    aStream.Read(fgroup.lines[i], SizeOf(TCSMLine));
  end;
end;

procedure CSMReadFacegroups(var csm: TCSM; var mesh: TCSMMesh; aStream: TStream);
var
  i: Integer;
begin
  aStream.Read(mesh.numFacegroups, 4);
  SetLength(mesh.facegroups, mesh.numFacegroups);
  for i := 0 to mesh.numFacegroups-1 do
  begin
    aStream.Read(mesh.facegroups[i].flags, 4);
    mesh.facegroups[i].textureFilename := StreamReadString(aStream);
    aStream.Read(mesh.facegroups[i].lightmapID, 4);
    aStream.Read(mesh.facegroups[i].texOffset, SizeOf(TCSMVec2));
    aStream.Read(mesh.facegroups[i].texScale, SizeOf(TCSMVec2));
    aStream.Read(mesh.facegroups[i].texRotation, 4);
    aStream.Read(mesh.facegroups[i].numVertices, 4);
    aStream.Read(mesh.facegroups[i].numTriangles, 4);
    aStream.Read(mesh.facegroups[i].numLines, 4);

    CSMReadVertices(csm, mesh.facegroups[i], aStream);
    CSMReadTriangles(csm, mesh.facegroups[i], aStream);
    CSMReadLines(csm, mesh.facegroups[i], aStream);
  end;
end;

procedure CSMReadMeshes(var csm: TCSM; aStream: TStream);
var
  i: Integer;
begin
  aStream.Read(csm.numMeshes, 4);
  if (csm.numMeshes > 0) then
  begin
    SetLength(csm.meshes, csm.numMeshes);
    for i := 0 to csm.numMeshes-1 do
    begin
      aStream.Read(csm.meshes[i].flags, 4);
      aStream.Read(csm.meshes[i].groupId, 4);
      csm.meshes[i].props := StreamReadString(aStream);
      aStream.Read(csm.meshes[i].color.R, 4);
      aStream.Read(csm.meshes[i].color.G, 4);
      aStream.Read(csm.meshes[i].color.B, 4);
      aStream.Read(csm.meshes[i].position, SizeOf(TCSMVec3));
      csm.meshes[i].visGroupID := 0;
      if csm.hasVisGroups then
        aStream.Read(csm.meshes[i].visGroupID, 4);

      CSMReadFacegroups(csm, csm.meshes[i], aStream);
    end;
  end;
end;

class function TGLCSMVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLCSMVectorFile.LoadFromStream(aStream: TStream);
var
  csm: TCSM;
  hasVisGroups: Boolean;
  lmapLib: TGLMaterialLibrary;
  matLib: TGLMaterialLibrary;
  mat: TGLLibMaterial;
  i, mi, fgi, vi, ti: Integer;
  startIdx: Integer;
  meshObj: TGLMeshObject;
  faceGroup: TFGVertexIndexList;
  matName: String;
  lightmapID: Integer;
begin
  lmaplib := Owner.LightmapLibrary;
  matlib := Owner.MaterialLibrary;
  
  try
    aStream.Read(csm.ver, 4);
    if (csm.ver <> 4) and (csm.ver <> 5) then
    begin
       raise Exception.Create('Invalid file or unsupported version');
    end;
    
    CSMReadGroups(csm, aStream);

    csm.numVisGroups := 0;
    csm.hasVisGroups := (csm.ver = 5);
    if csm.hasVisGroups then
    begin
      CSMReadVisGroups(csm, aStream);
    end;

    CSMReadLightmaps(csm, aStream);

    for i := 0 to Length(csm.lightmaps)-1 do
    begin
      csm.lightmaps[i].name := 'CSMLightmap' + IntToStr(i);
      mat := lmaplib.AddTextureMaterial(csm.lightmaps[i].name, '');
      mat.Material.Texture.TextureFormat := tfRGB;
      mat.Material.Texture.ImageClassName := TGLBlankImage.ClassName;
      mat.Material.Texture.TextureMode := tmReplace;
      mat.Material.MaterialOptions := [moNoLighting];

      with mat.Material.Texture.Image as TGLBlankImage do
      begin
        Width := csm.lightmaps[i].width;
        Height := csm.lightmaps[i].height;
        GetBitmap32.Assign(csm.lightmaps[i].bitmap);
        NotifyChange(self);
      end;

      csm.lightmaps[i].material := mat;
    end;

    CSMReadMeshes(csm, aStream);
    
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;

  owner.MeshObjects.Clear;
    
  for mi := 0 to csm.numMeshes-1 do
  begin
    meshObj := TGLMeshObject.CreateOwned(owner.MeshObjects);
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
end;

initialization
    RegisterVectorFileFormat('csm', 'Cartography Shop 4 CSM File Format', TGLCSMVectorFile);
end.
