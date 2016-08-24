{: GLFileB3D<p>

	B3D VectorFile class<p>

	<b>History :</b><font size=-1><ul>
	   <li>22/12/05 - Mathx - Added to the GLScene Project.
	</ul></font>
}
unit GLFileB3D;

interface

uses
  Classes, SysUtils, Dialogs, GLVectorFileObjects, ApplicationFileIO, FileB3D, TypesB3D;

type
  TGLB3DVectorFile = class(TVectorFile)
  public
    class function Capabilities : TDataFileCapabilities; override;
    procedure LoadFromStream(aStream : TStream); override;
  end;


implementation

uses
  GLTexture, VectorGeometry, VectorLists;


//------------------------------ TGLB3DVectorFile ------------------------------
// Capabilities
class function TGLB3DVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;



// LoadFromStream
procedure TGLB3DVectorFile.LoadFromStream(aStream : TStream);

var
  b3d: TFileB3D;
  s: string;
  mo: TMeshObject;
  i, j : Integer;
  faceGroup: TFGVertexIndexList;
  //lightmapBmp : TGLBitmap;
  Node: PNODEChunk;
  B3DMat: TB3DMaterial;
  B3DTex: TB3DTexture;
  B3DLightTex: TB3DTexture;
  Vertex: PVertexData;
  Triangles: PTRISChunk;
  v, v1: TAffineVector;
  Matrix : TMatrix;
  MatLib : TGLMaterialLibrary;
  LightLib : TGLMaterialLibrary;
  RotQuat: TQuaternion;
  RotMat: TMatrix;

  function GetOrAllocateMaterial(MaterialNum : integer; aMat: TB3DMaterial; aTex: TB3DTexture; aLightmap: TB3DTexture) : String;
  var
    libMat : TGLLibMaterial;
    texName : string;
    lightName: string;
  begin
    if GetOwner is TGLBaseMesh then
    begin
      matLib := TGLBaseMesh(GetOwner).MaterialLibrary;
      LightLib := TGLBaseMesh(GetOwner).LightmapLibrary;
      // got a linked material library?
      if Assigned(matLib) then
      begin
        Result := aMat.GetMaterialName;
        //add base material
        libMat := matLib.Materials.GetLibMaterialByName(Result);
        if not Assigned(libMat) then
        begin
          if Assigned(aTex) then
            texName := aTex.GetTextureName
          else
            texName := '';

          if not FileExists(texName) then
            texName := ExtractFileName(texName);
          if texName<>'' then
            libMat := matLib.AddTextureMaterial(Result + IntToStr(MaterialNum), texName, false)
          else
          begin
            libMat := matLib.Materials.Add;
            libMat.Name := result + IntToStr(MaterialNum);
          end;

          libmat.Material.FrontProperties.Diffuse.Red := aMat.MaterialData.red;
          libmat.Material.FrontProperties.Diffuse.Green := aMat.MaterialData.green;
          libmat.Material.FrontProperties.Diffuse.Blue := aMat.MaterialData.blue;
          libmat.Material.FrontProperties.Diffuse.Alpha := aMat.MaterialData.alpha;
          libmat.Material.FrontProperties.Shininess := Round(aMat.MaterialData.shininess*100.0);
          libmat.Material.MaterialOptions := [moNoLighting];

          if aMat.MaterialData.alpha <> 1 then begin
              libmat.Material.FaceCulling := fcNoCull;
              libmat.Material.BlendingMode := bmTransparency;
          end;

          if Assigned(aTex) then
          begin

            libMat.TextureOffset.AsAffineVector := AffineVectorMake(aTex.TextureData.x_pos,
              aTex.TextureData.y_pos, 0);

            libMat.TextureScale.AsAffineVector := AffineVectorMake(aTex.TextureData.x_scale,
              aTex.TextureData.y_scale, 1);

            if aTex.TextureData.flags = 2 then begin
               libmat.Material.FaceCulling := fcNoCull;
               libmat.Material.BlendingMode := bmTransparency;
            end;

            if aMat.MaterialData.alpha <> 1 then begin
               libmat.Material.Texture.ImageAlpha := tiaAlphaFromIntensity;
               libmat.Material.Texture.TextureFormat := tfRGBA;
               libmat.Material.Texture.TextureMode := tmModulate;
            end;

           end;
        end;
        //add lightmap material
        if (Assigned(LightLib)) and (Assigned(aLightmap)) then
        begin
          LightName := aLightmap.GetTextureName;
          libMat := LightLib.Materials.GetLibMaterialByName(LightName);
          if not Assigned(libMat) then
          begin
            if not FileExists(LightName) then
              LightName := ExtractFileName(LightName);
            libMat := LightLib.AddTextureMaterial(LightName, LightName, false);
            libMat.Material.Texture.TextureMode := tmReplace;
            if Assigned(aLightMap) then
            begin
              libMat.TextureOffset.AsAffineVector := AffineVectorMake(aLightMap.TextureData.x_pos,
                aLightMap.TextureData.y_pos, 0);
              libMat.TextureScale.AsAffineVector := AffineVectorMake(aLightMap.TextureData.x_scale,
                aLightMap.TextureData.y_scale, 1);
            end;
          end;
          //modify the material lightmap index
          aMat.MaterialData.texture_id[1] := libMat.Index;
        end;
      end else
        Result:='';
    end else
      Result:='';
  end;

begin
  b3d := TFileB3D.Create;
  try
    //first, load the b3d model sturctures from stream
    b3d.LoadFromStream(aStream);
    //then add all the materials and lightmaps from b3d structures
    for I:=0 to b3d.Materials.Count-1 do
    begin
      B3DMat := TB3DMaterial(b3d.Materials.Objects[I]);
      B3DTex := nil;
      B3DLightTex := nil;
      //check if there is one texture layer
      if B3DMat.MaterialData.n_texs>0 then
      begin
        if B3DMat.MaterialData.texture_id[0]>=0 then
          B3DTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[0]]);
      end;
      
      //ShowMessage(IntToStr(B3DMat.MaterialData.n_texs));
      if B3DMat.MaterialData.n_texs>1 then
      begin
        //ShowMessage(IntToStr(B3DMat.MaterialData.texture_id[1]));
        if B3DMat.MaterialData.texture_id[1]>=0 then
          B3DLightTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[1]]);
      end;
      
        //check if there are two texture layer
        //if B3DMat.MaterialData.n_texs>1 then
        //begin     
          //if B3DMat.MaterialData.texture_id[3]>=0 then
          //  B3DLightTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[3]])
          //else if B3DMat.MaterialData.texture_id[2]>=0 then
          // B3DLightTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[2]])
          //else 
          //if B3DMat.MaterialData.texture_id[1]>=0 then
            //B3DLightTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[1]]);
        //end;
         {
         if B3DMat.MaterialData.texture_id[0]>=0 then
          B3DTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[0]]);
        //check if there are two texture layer
        if B3DMat.MaterialData.n_texs>1 then
         //why lightmap in some case on channel 2?
         if B3DMat.MaterialData.texture_id[1]>=0 then
            B3DLightTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[1]])
          else
          //check if there are three texture layer
          if B3DMat.MaterialData.n_texs>2 then
           if B3DMat.MaterialData.texture_id[2]>=0 then
               B3DLightTex := TB3DTexture(b3d.Textures.Objects[B3DMat.MaterialData.texture_id[2]]);
          }
      GetOrAllocateMaterial(I, B3DMat, B3DTex, B3DLightTex);
    end;

    if GetOwner is TGLBaseMesh then
     (GetOwner as TGLBaseMesh).NormalsOrientation := mnoDefault;


      Node := b3d.Nodes.NodeData;
      while Node<>nil do begin
        if Node^.meshes<>nil then begin
          mo := TMeshObject.CreateOwned(Owner.MeshObjects);

          SetString(s, Node^.name, strlen(Node^.name));
//          if Pos('16', s)>1 then
//             Pos('17', s);
          mo.Name := s;
          mo.Mode := momFaceGroups;
          //add all the vertices, normals, colors and texture-coords(including the lightmap texture)
          Vertex := Node^.meshes^.vertices.vertices;
          while Assigned(Vertex) do
          begin
            //W3D modif inversed z
            mo.Vertices.Add(AffineVectorMake(Vertex^.y, Vertex^.x, Vertex^.z));
            if (Node^.meshes^.vertices.flags and 1)>0 then
              mo.Normals.Add(VectorNormalize(AffineVectorMake(Vertex^.ny, Vertex^.nx, Vertex^.nz)));

            if (Node^.meshes^.vertices.flags and 2)>0 then begin
              mo.Colors.Add(VectorMake(Vertex^.red, Vertex^.green, Vertex^.blue, Vertex^.alpha));
            end;

            case Node^.meshes^.vertices.tex_coord_sets of
              1:
              begin
                case Node^.meshes^.vertices.tex_coord_set_size of
                  2: mo.TexCoords.Add(Vertex^.tex_coords[0], -Vertex^.tex_coords[1], 0);
                  3: mo.TexCoords.Add(Vertex^.tex_coords[0], -Vertex^.tex_coords[1], Vertex^.tex_coords[2]);
                end;
              end;
              2: //lightmap tex_coord included
              begin
                case Node^.meshes^.vertices.tex_coord_set_size of
                  2: mo.TexCoords.Add(Vertex^.tex_coords[0], -Vertex^.tex_coords[1], 0);
                  3: mo.TexCoords.Add(Vertex^.tex_coords[0], -Vertex^.tex_coords[1], Vertex^.tex_coords[2]);
                end;
                //mo.LightMapTexCoords.Add(Vertex^.tex_coords[0], -Vertex^.tex_coords[1]);
                mo.LightMapTexCoords.Add(
                  Vertex^.tex_coords[Node^.meshes^.vertices.tex_coord_set_size],
                  -Vertex^.tex_coords[Node^.meshes^.vertices.tex_coord_set_size+1]);
              end;
            end;
            Vertex := Vertex^.next;
          end;
          //add facegroups
          Triangles := Node^.meshes^.triangles;
          while Assigned(Triangles) do
          begin
            FaceGroup := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
            if Triangles^.brush_id>=0 then
            begin
              FaceGroup.MaterialName := b3d.Materials[Triangles^.brush_id] + InttoStr(Triangles^.brush_id);
              FaceGroup.LightMapIndex := TB3DMaterial(b3d.Materials.Objects[Triangles^.brush_id]).MaterialData.texture_id[1];
            end else
            begin
              FaceGroup.MaterialName := '';
              FaceGroup.LightMapIndex := -1;
            end;
            for J:=0 to Length(Triangles^.vertex_id)-1 do
              FaceGroup.VertexIndices.Add(Triangles^.vertex_id[J]);
            while FaceGroup.VertexIndices.Count mod 3<>0 do
              FaceGroup.VertexIndices.Delete(FaceGroup.VertexIndices.Count-1);
            Triangles := Triangles.next;
            FaceGroup.Reverse;

          end;

          RotQuat := QuaternionMake([Node^.rotation[2], Node^.rotation[1], Node^.rotation[3]], Node^.rotation[0]);
          RotMat := QuaternionToMatrix(RotQuat);
          mo.Vertices.TransformAsVectors(RotMat);

{          mo.SetPosition( Node^.Position[1], Node^.Position[0], Node^.Position[2]);
          mo.SetScale( Node^.Scale[1], Node^.Scale[0], Node^.Scale[2]);
 }
          if Pos('ENT_',UpperCase(mo.name))=0 then
             v := AffineVectorMake(Node^.position[1],Node^.position[0], Node^.position[2])
          else begin
             v := AffineVectorMake(0.0,0.0,0.0);
          end;

          v1 := AffineVectorMake(Node^.Scale[1],Node^.Scale[0], Node^.Scale[2]);
          matrix := CreateScaleAndTranslationMatrix(VectorMake(v1), VectorMake(v));
          mo.Vertices.TransformAsPoints(matrix);

          RotMat := CreateRotationMatrixZ(DegToRad(90));
          mo.Vertices.TransformAsVectors(RotMat);
          mo.Normals.TransformAsVectors(RotMat);
        end;
        Node := Node^.next;
    end;
  finally
    b3d.free;
  end;
end;

initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterVectorFileFormat('b3d', 'Blitz 3D model files', TGLB3DVectorFile);

end.

