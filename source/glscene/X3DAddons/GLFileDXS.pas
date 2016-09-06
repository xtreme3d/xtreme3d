{
  GLFileDXS
  DeleD DXS format loader

	History :
	  06/09/16 - Gecko - Unit created.
}
unit GLFileDXS;

interface

{$I GLScene.inc}

uses Dialogs, Graphics, Classes, SysUtils,
     GLVectorFileObjects, ApplicationFileIO, VectorLists, VectorGeometry,
     GLTexture, PersistentClasses, GLGraphics, OpenGL1x, XMLDoc, XMLIntf;
     
type    
    TGLDXSVectorFile = class(TVectorFile)
    public
      class function Capabilities: TDataFileCapabilities; override;
      procedure LoadFromStream(aStream: TStream); override;
    end;
    
implementation

class function TGLDXSVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

function XMLGetChildByAttribute(node: IXMLNode; attribName: String; attribStr: String): IXMLNode;
var
  i: Integer;
  childNode: IXMLNode;
begin
  Result := nil;
  for i := 0 to node.ChildNodes.Count-1 do
  begin
    childNode := node.ChildNodes[i];
    if childNode.HasAttribute(attribName) then
    begin
      if childNode.Attributes[attribName] = attribStr then
      begin
        Result := childNode;
        break;
      end;
    end;
  end;
end;

function DXSGetMaterialNodeByID(materialsNode: IXMLNode; id: String): IXMLNode;
var
  ci, mi: Integer;
  categoryNode: IXMLNode;
begin
  Result := nil;
  for ci := 0 to materialsNode.ChildNodes.Count-1 do
  begin
    categoryNode := materialsNode.ChildNodes[ci];
    for mi := 0 to categoryNode.ChildNodes.Count-1 do
    begin
      if categoryNode.ChildNodes[mi].Attributes['id'] = id then
      begin
        Result := categoryNode.ChildNodes[mi];
        break;
      end; 
    end;
  end;
end;

function safeFloat(sStringFloat: String): double;
var
  dReturn : double;
begin
  if (DecimalSeparator = ',') then
    sStringFloat := StringReplace(sStringFloat, '.', ',', [rfIgnoreCase, rfReplaceAll]);
  dReturn := strToFloat(sStringFloat);
  result := dReturn;
end;

procedure TGLDXSVectorFile.LoadFromStream(aStream: TStream);
var
  lmapLib: TGLMaterialLibrary;
  matLib: TGLMaterialLibrary;
  mat: TGLLibMaterial;
  meshObj: TMeshObject;
  faceGroup: TFGVertexIndexList;
  doc: TXMLDocument;
  sceneNode: IXMLNode;
  primitivesNode: IXMLNode;
  materialsNode: IXMLNode;
  matNode: IXMLNode;
  matName: String;
  matTextureFileName: String;
  primNode: IXMLNode;
  verticesNode: IXMLNode;
  vertexNode: IXMLNode;
  newVertexNode: IXMLNode;
  polygonsNode: IXMLNode;
  polyNode: IXMLNode;
  vindexNode: IXMLNode;
  numPrimitives: Integer;
  numVertices: Integer;
  numPolygons: Integer;
  pi, vi, pli: Integer;
  vx, vy, vz: Single;
  vindex: array[0..2] of Integer;
  pInd: Integer;
  i: Integer;
  indices: TIntegerList;
  matIndex: String;
  u0, v0, u1, v1: Single;
begin
  lmaplib := Owner.LightmapLibrary;
  matlib := Owner.MaterialLibrary;

  owner.MeshObjects.Clear;

  doc := TXMLDocument.Create(owner);
  try
    doc.LoadFromStream(aStream);
    
    sceneNode := doc.DocumentElement;
    primitivesNode := sceneNode.ChildNodes['primitives'];
    numPrimitives := primitivesNode.ChildNodes.Count;
    materialsNode := sceneNode.ChildNodes['materials'];

    for pi := 0 to numPrimitives-1 do
    begin
      meshObj := TMeshObject.CreateOwned(owner.MeshObjects);
      meshObj.Mode := momFaceGroups;
      
      primNode := primitivesNode.ChildNodes[pi];
      verticesNode := primNode.ChildNodes['vertices'];
      polygonsNode := primNode.ChildNodes['polygons'];

      numVertices := verticesNode.ChildNodes.Count;
      for vi := 0 to numVertices-1 do
      begin
        vertexNode := verticesNode.ChildNodes[vi];
        vx := safeFloat(vertexNode.Attributes['x']);
        vy := safeFloat(vertexNode.Attributes['y']);
        vz := safeFloat(vertexNode.Attributes['z']);
        meshObj.Vertices.Add(vx, vy, vz);
        meshObj.Normals.Add(0, 0, 0);
        meshObj.TexCoords.Add(0, 0);
        meshObj.LightmapTexCoords.Add(0, 0);
        vertexNode.Attributes['gl_ind'] := meshObj.Vertices.Count-1; 
      end;

      numPolygons := polygonsNode.ChildNodes.Count;
      for pli := 0 to numPolygons-1 do
      begin
        polyNode := polygonsNode.ChildNodes[pli];

        if polyNode.ChildNodes.Count > 3 then
           raise Exception.Create('Non-triangle polygons are not supported, please triangulate');

        matIndex := polyNode.Attributes['mid'];
        matNode := DXSGetMaterialNodeByID(materialsNode, matIndex);

        faceGroup := TFGVertexIndexList.CreateOwned(meshObj.FaceGroups);
        faceGroup.Mode := fgmmTriangles;
        matName := matNode.Attributes['name'];
        if matlib.LibMaterialByName(matName) = nil then
        begin
          // TODO: use empty string if no such node in matNode
          //matTextureFileName := '';
          matTextureFileName := matNode.ChildNodes['layer'].ChildNodes['texture'].Attributes['file'];
          mat := matlib.AddTextureMaterial(matName, matTextureFileName);
        end;
        faceGroup.MaterialName := matName;

        // TODO: lightmaps

        for pInd := 0 to 2 do
        begin
          vindexNode := polyNode.ChildNodes[pInd];
          vertexNode := XMLGetChildByAttribute(verticesNode, 'id', vindexNode.Attributes['vid']);

          if vertexNode <> nil then
          begin
            if vindexNode.HasAttribute('u1') and vindexNode.HasAttribute('v1') then
            begin
              if vertexNode.HasAttribute('u0') and vindexNode.HasAttribute('v0') and
                 vertexNode.HasAttribute('u1') and vindexNode.HasAttribute('v1') then
              begin
                if (vertexNode.Attributes['u0'] = vindexNode.Attributes['u0']) and
                   (vertexNode.Attributes['v0'] = vindexNode.Attributes['v0']) and
                   (vertexNode.Attributes['u1'] = vindexNode.Attributes['u1']) and
                   (vertexNode.Attributes['v1'] = vindexNode.Attributes['v1']) then
                begin
                  // UVs match, use existing vertex
                  vindex[pInd] := vertexNode.Attributes['gl_ind'];
                end
                else
                begin
                  // UVs don't match, append new vertex
                  newVertexNode := verticesNode.AddChild('vertex');
                  newVertexNode.Attributes['x'] := vertexNode.Attributes['x'];
                  newVertexNode.Attributes['y'] := vertexNode.Attributes['y'];
                  newVertexNode.Attributes['z'] := vertexNode.Attributes['z'];
                  newVertexNode.Attributes['u0'] := vindexNode.Attributes['u0'];
                  newVertexNode.Attributes['v0'] := vindexNode.Attributes['v0'];
                  newVertexNode.Attributes['u1'] := vindexNode.Attributes['u1'];
                  newVertexNode.Attributes['v1'] := vindexNode.Attributes['v1'];
                  vx := safeFloat(newVertexNode.Attributes['x']);
                  vy := safeFloat(newVertexNode.Attributes['y']);
                  vz := safeFloat(newVertexNode.Attributes['z']);
                  meshObj.Vertices.Add(vx, vy, vz);
                  u0 := safeFloat(vindexNode.Attributes['u0']);
                  v0 := safeFloat(vindexNode.Attributes['v0']);
                  u1 := safeFloat(vindexNode.Attributes['u1']);
                  v1 := safeFloat(vindexNode.Attributes['v1']);
                  meshObj.TexCoords.Add(u0, v0);
                  meshObj.LightmapTexCoords.Add(u1, v1);
                  newVertexNode.Attributes['gl_ind'] := meshObj.Vertices.Count-1;
                  vindex[pInd] := newVertexNode.Attributes['gl_ind'];
                end;
              end
              else
              begin
                // Vertex doesn't have UVs, store them
                vertexNode.Attributes['u0'] := vindexNode.Attributes['u0'];
                vertexNode.Attributes['v0'] := vindexNode.Attributes['v0'];
                vertexNode.Attributes['u1'] := vindexNode.Attributes['u1'];
                vertexNode.Attributes['v1'] := vindexNode.Attributes['v1'];
                u0 := safeFloat(vindexNode.Attributes['u0']);
                v0 := safeFloat(vindexNode.Attributes['v0']);
                u1 := safeFloat(vindexNode.Attributes['u1']);
                v1 := safeFloat(vindexNode.Attributes['v1']);
                vindex[pInd] := vertexNode.Attributes['gl_ind'];
                meshObj.TexCoords.Items[vindex[pInd]] := AffineVectorMake(u0, v0, 0);
                meshObj.LightmapTexCoords.Items[vindex[pInd]] := TexPointMake(u1, v1);
              end;
            end
            else // no lightmap coords
            begin
              if vertexNode.HasAttribute('u0') and vindexNode.HasAttribute('v0') then
              begin
                if (vertexNode.Attributes['u0'] = vindexNode.Attributes['u0']) and
                   (vertexNode.Attributes['v0'] = vindexNode.Attributes['v0']) then
                begin
                  // UVs match, use existing vertex
                  vindex[pInd] := vertexNode.Attributes['gl_ind'];
                end
                else
                begin
                  // UVs don't match, append new vertex
                  newVertexNode := verticesNode.AddChild('vertex');
                  newVertexNode.Attributes['x'] := vertexNode.Attributes['x'];
                  newVertexNode.Attributes['y'] := vertexNode.Attributes['y'];
                  newVertexNode.Attributes['z'] := vertexNode.Attributes['z'];
                  newVertexNode.Attributes['u0'] := vindexNode.Attributes['u0'];
                  newVertexNode.Attributes['v0'] := vindexNode.Attributes['v0'];
                  vx := safeFloat(newVertexNode.Attributes['x']);
                  vy := safeFloat(newVertexNode.Attributes['y']);
                  vz := safeFloat(newVertexNode.Attributes['z']);
                  meshObj.Vertices.Add(vx, vy, vz);
                  u0 := safeFloat(vindexNode.Attributes['u0']);
                  v0 := safeFloat(vindexNode.Attributes['v0']);
                  meshObj.TexCoords.Add(u0, v0);
                  newVertexNode.Attributes['gl_ind'] := meshObj.Vertices.Count-1;
                  vindex[pInd] := newVertexNode.Attributes['gl_ind'];
                end;
              end
              else
              begin
                // Vertex doesn't have UVs, store them
                vertexNode.Attributes['u0'] := vindexNode.Attributes['u0'];
                vertexNode.Attributes['v0'] := vindexNode.Attributes['v0'];
                u0 := safeFloat(vindexNode.Attributes['u0']);
                v0 := safeFloat(vindexNode.Attributes['v0']);
                vindex[pInd] := vertexNode.Attributes['gl_ind'];
                meshObj.TexCoords.Items[vindex[pInd]] := AffineVectorMake(u0, v0, 0);
              end;
            end;
          end;          
        end;

        // TODO: group polygons by material
        faceGroup.VertexIndices.Add(vindex[2], vindex[1], vindex[0]);
        
      end;

      indices := TIntegerList.Create;
      for i:=0 to meshObj.FaceGroups.Count-1 do
      begin
        indices.Add(TFGVertexIndexList(meshObj.FaceGroups[i]).VertexIndices);
      end;
      meshObj.BuildNormals(indices, momTriangles);
      indices.Free;
      
    end;
  except
    On E: Exception do
    begin
      ShowMessage(E.Message);
      owner.MeshObjects.Clear;
    end;
  end;

  doc.Free();
 end;

initialization
    RegisterVectorFileFormat('dxs', 'DeleD DXS File Format', TGLDXSVectorFile);
end.
