//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileMDC<p>
  
    Code for loading animated MDC files into GLScene FreeForms 
    and Actors.<p>

    This file format uses in Return To Castle Wolfenstein instead 
    of MD3 files. It has got all MD3 features (such as Tag frames) 
    plus very small data!<p>
    
    Original code by Osman Turan (osmanturancom@yahoo.com)<p>

	<b>History :</b><font size=-1><ul>
      <li>11/05/04 - SG - Added to CVS
      <li>07/02/04 - OT - Creation (Osman Turan)
	</ul></font>
}
unit GLFileMDC;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, GLMisc, GLTexture, ApplicationFileIO,
  VectorGeometry, TypesMDC;

type

  TGLMDCVectorFile = class (TVectorFile)
    public
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
// ------------------ TGLMDCVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLMDCVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLMDCVectorFile.LoadFromStream(aStream : TStream);

type
  PPackedNormal = ^TPackedNormal;
  TPackedNormal = array[0..1] of Byte;

var
  i,j,k,
  numVerts,
  numtris     : Integer;
  mesh        : TMorphableMeshObject;
  faceGroup   : TFGIndexTexCoordList;
  morphTarget : TMeshMorphTarget;

  function UnpackNormal(pn: TPackedNormal) : TAffineVector;
  var
    lat,lng : single;
  begin
    // The MDC normal is a latitude/longitude value that needs
    // to be calculated into cartesian space.
    lat:=(pn[0])*(2*pi)/255;
    lng:=(pn[1])*(2*pi)/255;
    Result[0] := cos(lat)*sin(lng);
    Result[1] := sin(lat)*sin(lng);
    Result[2] := cos(lng);
  end;

  procedure AllocateMaterial(meshname:string);
  var
    LibMat : TGLLibMaterial;
  begin
    // If a material library is assigned to the actor/freeform the
    // mesh name will be added as a material.
    if Assigned(Owner.MaterialLibrary) then with Owner.MaterialLibrary do begin
      if Assigned(Materials.GetLibMaterialByName(meshname)) then exit;
      LibMat:=Materials.Add;
      LibMat.name:=meshname;
      LibMat.Material.Texture.Disabled:=False;
    end;
  end;

var
  fileheader: TMDCFileHeader;
  surfheader: TMDCSurfaceHeader;
  borderframes: array of TMDCBorderFrame;
  baseframetable, compframetable: array of Word;
  baseframe: TMDCBaseFrame;
  compframe: TMDCCompFrame;
  xyz, normal: TAffineVector;
  st: array of array[0..1] of Single;
  triangles: array of TMDCTriangle;
  frameOffset: Cardinal;
begin
  aStream.Read(fileheader, SizeOf(fileheader));
  Assert(fileheader.Ident=MDCFILE_IDENTITY,  'Incorrect MDC file Ident');
  Assert(fileheader.Version=MDCFILE_VERSION, 'Incorrect MDC version number');

  try
    aStream.Seek(fileheader.OffsetBorderFrames, soFromBeginning);
    SetLength(borderframes, fileheader.NumFrames);
    aStream.Read(borderframes[0], SizeOf(TMDCBorderFrame) * fileheader.NumFrames);

    frameOffset := fileheader.OffsetSurfaces;

    for i:=0 to fileheader.NumSurfaces-1 do
    begin
      //read header
      aStream.Seek(frameOffset, soFromBeginning);
      aStream.Read(surfheader, SizeOf(TMDCSurfaceHeader));

      //triangles for this surface
      SetLength(triangles, surfheader.NumTriangles);
      aStream.Seek(frameOffset + surfheader.OffsetTriangles, soFromBeginning);
      aStream.Read(triangles[0], SizeOf(TMDCTriangle) * surfheader.NumTriangles);

      //texture coordinates for this surface
      SetLength(st, surfheader.NumVertices);
      aStream.Seek(frameOffset + surfheader.OffsetTexCoords, soFromBeginning);
      aStream.Read(st[0], 2 * SizeOf(Single) * surfheader.NumVertices);

      //base frame table for this surface (for only loading)
      SetLength(baseframetable, fileheader.NumFrames);
      aStream.Seek(frameOffset + surfheader.OffsetFrameBaseFrames, soFromBeginning);
      aStream.Read(baseframetable[0], SizeOf(Word) * fileheader.NumFrames);
      //compressed frame table for this surface (for only loading)
      SetLength(compframetable, fileheader.NumFrames);
      aStream.Seek(frameOffset + surfheader.OffsetFrameCompFrames, soFromBeginning);
      aStream.Read(compframetable[0], SizeOf(Word) * fileheader.NumFrames);

      mesh := TMorphableMeshObject.CreateOwned(Owner.MeshObjects);
      //easiest way to convert a char array to string ;)
      mesh.Name := Trim(PChar(@surfheader.Name[0]));
      with mesh do
      begin
        Mode:=momFaceGroups;
        faceGroup:=TFGIndexTexCoordList.CreateOwned(FaceGroups);
        with faceGroup do
        begin
          AllocateMaterial(mesh.Name);
          MaterialName:=mesh.Name;
          numTris := surfheader.numTriangles;
          VertexIndices.Capacity := numTris * 3;
          TexCoords.Capacity := numTris * 3;
          // Get the vertex indices and texture coordinates
          for j:=0 to surfheader.numTriangles-1 do
          begin
            Add(triangles[j, 0],
                st[ triangles[j, 0] ][0],
                1-st[ triangles[j, 0] ][1]);
            Add(triangles[j, 2],
                st[ triangles[j, 2] ][0],
                1-st[ triangles[j, 2] ][1]);
            Add(triangles[j, 1],
                st[ triangles[j, 1] ][0],
                1-st[ triangles[j, 1] ][1]);
          end;
        end;

        // Get the mesh data for each morph frame
        for j:=0 to fileheader.NumFrames-1 do
        begin
          morphTarget := TMeshMorphTarget.CreateOwned(MorphTargets);
          morphTarget.Name := Trim(PChar(@surfheader.Name[0]))+'['+IntToStr(j)+']';
          numVerts := surfheader.NumVertices;
          morphTarget.Vertices.Capacity := numVerts;

          //base frames
          SetLength(baseframe.BaseVertices, surfheader.NumVertices);
          aStream.Seek(frameOffset + surfheader.OffsetBaseVerts +
                       baseframetable[j] * surfheader.NumVertices * 8, soFromBeginning);
          aStream.Read(baseframe.BaseVertices[0], SizeOf(TMDCBaseVertex) * surfheader.NumVertices);

          //compressed frames
          if compframetable[j] <> $FFFF then //is there a valid frame?
          begin
            SetLength(compframe.CompVertices, surfheader.NumVertices);
            aStream.Seek(frameOffset + surfheader.OffsetCompVerts +
                         compframetable[j] * surfheader.NumVertices * 4, soFromBeginning);
            aStream.Read(compframe.CompVertices[0], SizeOf(TMDCCompVertex) * surfheader.NumVertices);
          end;

          for k := 0 to surfheader.NumVertices-1 do
          begin
            xyz[0] :=
              (baseframe.BaseVertices[k, 0] * MDC_BASEVERTEX_FACTOR) +
               borderframes[j].localorigin[0];
            xyz[1] :=
              (baseframe.BaseVertices[k, 1] * MDC_BASEVERTEX_FACTOR) +
               borderframes[j].localorigin[1];
            xyz[2] :=
              (baseframe.BaseVertices[k, 2] * MDC_BASEVERTEX_FACTOR) +
               borderframes[j].localorigin[2];
            normal := UnpackNormal(PPackedNormal(@baseframe.BaseVertices[k, 3])^);

            if compframetable[j] <> $FFFF then
            begin
              xyz[0] := xyz[0] + ((compframe.CompVertices[k, 0]-128) * MDC_COMPVERTEX_FACTOR);
              xyz[1] := xyz[1] + ((compframe.CompVertices[k, 1]-128) * MDC_COMPVERTEX_FACTOR);
              xyz[2] := xyz[2] + ((compframe.CompVertices[k, 2]-128) * MDC_COMPVERTEX_FACTOR);
              //FIXME:
              //I'm sure compframe.CompVertices[3] points a packed normal.
              //And it must be add the current normal like xyz.
              //But, I don't know a way to unpacked this value
              //I found a precalculated normal list in RTCW 1.41 mod source (q_math.c)
              //
              //NUMVERTEXNORMALS = 162
              //vec3_t bytedirs[NUMVERTEXNORMALS] = {
              //  {-0.525731, 0.000000, 0.850651}, (...)
              //
              //But, I had noticed some compframe.CompVertices[3] value is bigger
              //than NUMVERTEXNORMALS constant. So, there must be another list.
              //Can you find it?
              //Osman Turan (osmanturancom@yahoo.com)
            end;

            //all id Sofware based games uses Z axis as up instead of Y. So, convert them
            morphTarget.Vertices.Add(
              xyz[0],
              xyz[2],
              -xyz[1]);
            morphTarget.Normals.Add(
              normal[0],
              normal[2],
              -normal[1]);
          end;
        end;
      end;

      frameOffset := frameOffset + surfheader.OffsetEnd;

      if mesh.MorphTargets.Count>0 then
        mesh.MorphTo(0);
    end;
  finally
  //save memory free space
    borderframes := nil;
    baseframetable := nil;
    compframetable := nil;
    st := nil;
    triangles := nil;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('mdc', 'MDC files', TGLMDCVectorFile);

end.
