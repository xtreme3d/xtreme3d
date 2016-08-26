//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFile3DS<p>

	3DStudio 3DS vector file format implementation.<p>

	<b>History :</b><font size=-1><ul>
      <li>09/12/04 - LR - Add Integer cast line 94 for Linux
      <li>25/10/04 - SG - Added lightmap (3DS IllumMap) support
      <li>05/06/03 - SG - Separated from GLVectorFileObjects.pas
	</ul></font>
}
unit GLFile3DS;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, GLTexture, ApplicationFileIO,
  VectorGeometry, File3DS, Types3DS;

type
   // TGL3DSVectorFile
   //
   {: The 3DStudio vector file.<p>
      Uses 3DS import library by Mike Lischke (http://www.lishcke-online.de).<p>
      A 3DS file may contain material information and require textures when
      loading. Only the primary texture map is used by GLScene, transparency,
      bump mapping, etc. are ignored as of now. }
   TGL3DSVectorFile = class (TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;
         procedure LoadFromStream(aStream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGL3DSVectorFile ------------------
// ------------------

// Capabilities
//
class function TGL3DSVectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGL3DSVectorFile.LoadFromStream(aStream: TStream);
type
   TSmoothIndexEntry = array[0..31] of Cardinal;
   PSmoothIndexArray = ^TSmoothIndexArray;
   TSmoothIndexArray = array[0..MaxInt shr 8] of TSmoothIndexEntry;
var
   Marker: PByteArray;
   CurrentVertexCount: Integer;
   SmoothIndices: PSmoothIndexArray;
   mesh : TMeshObject;
   hasLightmap : Boolean;

   //--------------- local functions -------------------------------------------

   function GetOrAllocateMaterial(materials : TMaterialList; const name : String) : String;
   var
      material : PMaterial3DS;
      specColor : TVector;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
   begin
      material:=Materials.MaterialByName[Name];
      Assert(Assigned(material));
      if GetOwner is TGLBaseMesh then begin
         matLib:=TGLBaseMesh(GetOwner).MaterialLibrary;
         if Assigned(matLib) then begin
            Result:=name;
            libMat:=matLib.Materials.GetLibMaterialByName(name);
            if not Assigned(libMat) then begin
               libMat:=matLib.Materials.Add;
               libMat.Name:=name;
               with libMat.Material.FrontProperties do begin
                  Ambient.Color:=VectorMake(material.Ambient.R, material.Ambient.G, material.Ambient.B, 1);
                  Diffuse.Color:=VectorMake(material.Diffuse.R, material.Diffuse.G, material.Diffuse.B, 1);
                  specColor:=VectorMake(material.Specular.R, material.Specular.G, material.Specular.B, 1);
                  ScaleVector(specColor, 1 - material.Shininess);
                  Specular.Color:=specColor;
                  Shininess:=MaxInteger(0, Integer(Round((1 - material.ShinStrength) * 128)));
               end;
               if Trim(material.Texture.Map.NameStr)<>'' then begin
                  try
                     with libMat.Material.Texture do begin
                        Image.LoadFromFile(material.Texture.Map.NameStr);
                        Disabled:=False;
                        TextureMode:=tmModulate;
                     end;
                  except
                     on E: ETexture do begin
                        if not Owner.IgnoreMissingTextures then
                           raise;
                     end;
                  end;
               end;
            end;
         end else Result:='';
      end else Result:='';
   end;

   function GetOrAllocateLightMap(materials : TMaterialList; const name : String) : Integer;
   var
      material : PMaterial3DS;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
   begin
      Result:=-1;
      material:=Materials.MaterialByName[Name];
      Assert(Assigned(material));
      if GetOwner is TGLBaseMesh then begin
         matLib:=TGLBaseMesh(GetOwner).LightmapLibrary;
         if Assigned(matLib) then begin
            if Trim(material.IllumMap.Map.NameStr)<>'' then begin
               libMat:=matLib.Materials.GetLibMaterialByName(material.IllumMap.Map.NameStr);
               if not Assigned(libMat) then begin
                  libMat:=matLib.Materials.Add;
                  libMat.Name:=material.IllumMap.Map.NameStr;
                  try
                     with libMat.Material.Texture do begin
                        Image.LoadFromFile(material.IllumMap.Map.NameStr);
                        Disabled:=False;
                        TextureMode:=tmModulate;
                     end;
                  except
                     on E: ETexture do begin
                        if not Owner.IgnoreMissingTextures then
                           raise;
                     end;
                  end;
               end;
               Result:=libmat.Index;
               hasLightMap:=True;
            end;
         end;
      end;
   end;

   //----------------------------------------------------------------------

   function IsVertexMarked(P: Pointer; Index: Integer): Boolean; assembler;
      // tests the Index-th bit, returns True if set else False
   asm
                     BT [EAX], EDX
                     SETC AL
   end;

   //---------------------------------------------------------------------------

   function MarkVertex(P: Pointer; Index: Integer): Boolean; assembler;
      // sets the Index-th bit and return True if it was already set else False
   asm
                     BTS [EAX], EDX
                     SETC AL
   end;

   //---------------------------------------------------------------------------

   procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: Cardinal; P: Pointer);
      // Stores new vertex index (NewIndex) into the smooth index array of vertex ThisIndex
      // using field SmoothingGroup, which must not be 0.
      // For each vertex in the vertex array (also for duplicated vertices) an array of 32 cardinals
      // is maintained (each for one possible smoothing group. If a vertex must be duplicated because
      // it has no smoothing group or a different one then the index of the newly created vertex is
      // stored in the SmoothIndices to avoid loosing the conjunction between not yet processed vertices
      // and duplicated vertices.
      // Note: Only one smoothing must be assigned per vertex. Some available models break this rule and
      //       have more than one group assigned to a face. To make the code fail safe the group ID
      //       is scanned for the lowest bit set.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index (convert flag into an index)
                   MOV EDX, [P]                  // get address of index array
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, EDX
                   LEA EDX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV [EDX], ECX
                   POP EBX
   end;

   //---------------------------------------------------------------------------

   function GetSmoothIndex(ThisIndex, SmoothingGroup: Cardinal; P: Pointer): Integer;
      // Retrieves the vertex index for the given index and smoothing group.
      // This redirection is necessary because a vertex might have been duplicated.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, ECX
                   LEA ECX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV EAX, [ECX]
                   POP EBX
   end;

   //---------------------------------------------------------------------------

   procedure DuplicateVertex(Index: Integer);
      // extends the vector and normal array by one entry and duplicates the vertex data given by Index
      // the marker and texture arrays will be extended too, if necessary
   begin
      // enhance vertex array
      with mesh.Vertices do Add(Items[index]);
      mesh.Normals.Add(NullVector);
      // enhance smooth index array
      ReallocMem(SmoothIndices, (CurrentVertexCount + 1) * SizeOf(TSmoothIndexEntry));
      FillChar(SmoothIndices[CurrentVertexCount], SizeOf(TSmoothIndexEntry), $FF);
      // enhance marker array
      if (CurrentVertexCount div 8) <> ((CurrentVertexCount + 1) div 8) then begin
         ReallocMem(Marker, ((CurrentVertexCount + 1) div 8) + 1);
         Marker[(CurrentVertexCount div 8) + 1]:=0;
      end;
      with mesh.TexCoords do if Count>0 then Add(Items[index]);
      Inc(CurrentVertexCount);
   end;

   //--------------- end local functions ---------------------------------------

var
  iMaterial, i, j : Integer;
  aFaceGroup : TFGVertexIndexList;
  Face, Vertex, TargetVertex: Integer;
  SmoothingGroup: Cardinal;
  CurrentIndex: Word;
  Vector1, Vector2, Normal : TAffineVector;
  standardNormalsOrientation : Boolean;
begin
   with TFile3DS.Create do try
      LoadFromStream(aStream);
      // determine front face winding
      { TODO : better face winding }
      standardNormalsOrientation:=not (NormalsOrientation=mnoDefault);
      for i:=0 to Objects.MeshCount-1 do with PMesh3DS(Objects.Mesh[I])^ do begin
         if IsHidden or (NVertices<3) then Continue;
         hasLightMap:=False;
         mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
         mesh.Name:=PMesh3DS(Objects.Mesh[I])^.NameStr;
         with mesh do begin
            Mode:=momFaceGroups;
            // make a copy of the vertex data, this must always be available
            Vertices.Capacity:=NVertices;
            Normals.AddNulls(NVertices);
            if NTextVerts>0 then begin
               TexCoords.Capacity:=NVertices;
               for j:=0 to NVertices-1 do begin
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
                  TexCoords.Add(PTexPoint(@TextArray[j])^);
               end;
            end else begin
               for j:=0 to NVertices-1 do
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
            end;
         end;
         // allocate memory for the smoothindices and the marker array
         CurrentVertexCount:=NVertices;
         Marker:=AllocMem((NVertices div 8) + 1); // one bit for each vertex
         GetMem(SmoothIndices, NVertices * SizeOf(TSmoothIndexEntry));

         if SmoothArray=nil then begin
            // no smoothing groups to consider
            for face:=0 to NFaces-1 do with FaceArray[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  CurrentIndex:=FaceRec[Vertex];
                  // already been touched?
                  if IsVertexMarked(Marker, CurrentIndex) then begin
                     // already touched vertex must be duplicated
                     DuplicateVertex(CurrentIndex);
                     FaceRec[Vertex]:=CurrentVertexCount-1;
                     mesh.Normals[CurrentVertexCount-1]:=Normal;
                  end else begin
                     // not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end else begin
            // smoothing groups are to be considered
            for Face:=0 to NFaces-1 do with FaceArray[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               SmoothingGroup:=SmoothArray[Face];
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  currentIndex:=FaceRec[Vertex];
                  // Has vertex already been touched?
                  if IsVertexMarked(Marker, currentIndex) then begin
                     // check smoothing group
                     if SmoothingGroup = 0 then begin
                        // no smoothing then just duplicate vertex
                        DuplicateVertex(CurrentIndex);
                        FaceRec[Vertex]:=CurrentVertexCount - 1;
                        mesh.Normals[CurrentVertexCount - 1]:=Normal;
                        // mark new vertex also as touched
                        MarkVertex(Marker, CurrentVertexCount - 1);
                     end else begin
                        // this vertex must be smoothed, check if there's already
                        // a (duplicated) vertex for this smoothing group
                        TargetVertex:=GetSmoothIndex(CurrentIndex, SmoothingGroup, SmoothIndices);
                        if TargetVertex < 0 then begin
                           // vertex has not yet been duplicated for this smoothing
                           // group, so do it now
                           DuplicateVertex(CurrentIndex);
                           FaceRec[Vertex]:=CurrentVertexCount - 1;
                           mesh.Normals[CurrentVertexCount - 1]:=Normal;
                           StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           StoreSmoothIndex(CurrentVertexCount - 1, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           // mark new vertex also as touched
                           MarkVertex(Marker, CurrentVertexCount - 1);
                        end else begin
                           // vertex has already been duplicated,
                           // so just add normal vector to other vertex...
                           mesh.Normals[TargetVertex]:=VectorAdd(mesh.Normals[TargetVertex], Normal);
                           // ...and tell which new vertex has to be used from now on
                           FaceRec[Vertex]:=TargetVertex;
                        end;
                     end;
                  end else begin
                     // vertex not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     // initialize smooth indices for this vertex
                     FillChar(SmoothIndices[CurrentIndex], SizeOf(TSmoothIndexEntry), $FF);
                     if SmoothingGroup <> 0 then
                        StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentIndex, SmoothIndices);
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end;
         FreeMem(Marker);
         FreeMem(SmoothIndices);

         Assert(mesh.Vertices.Count=CurrentVertexCount);

         // and normalize the Normals array
         mesh.Normals.Normalize;

         // now go for each material group
         // if there's no face to material assignment then just copy the
         // face definitions and rely on the default texture of the scene object
         if (NMats = 0) or (not vGLVectorFileObjectsAllocateMaterials) then begin
            aFaceGroup:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
            with aFaceGroup do begin
               MaterialName:='';
               // copy the face list
               for j:=0 to NFaces-1 do begin
                  Add(FaceArray[J].V1);
                  Add(FaceArray[J].V2);
                  Add(FaceArray[J].V3);
               end;
            end;
         end else begin
            for iMaterial:=0 to NMats - 1 do begin
               aFaceGroup:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
               with aFaceGroup do begin
                  MaterialName:=GetOrAllocateMaterial(Materials, MatArray[iMaterial].NameStr);
                  LightMapIndex:=GetOrAllocateLightMap(Materials, MatArray[iMaterial].NameStr);
                  // copy all vertices belonging to the current face into our index array,
                  // there won't be redundant vertices since this would mean a face has more than one
                  // material
                  // NFaces is the one from FaceGroup
                  with MatArray[iMaterial] do for j:=0 to NFaces - 1 do begin
                     Add(FaceArray[FaceIndex[J]].V1);
                     Add(FaceArray[FaceIndex[J]].V2);
                     Add(FaceArray[FaceIndex[J]].V3);
                  end;
               end;
            end;
         end;
         if hasLightMap then
            for j:=0 to mesh.TexCoords.Count-1 do
               mesh.LightMapTexCoords.Add(mesh.TexCoords[j][0], mesh.TexCoords[j][1]);
      end;
   finally
      Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('3ds', '3D Studio files', TGL3DSVectorFile);
   RegisterVectorFileFormat('prj', '3D Studio project files', TGL3DSVectorFile);

end.