unit GLFileLOD;

{: GLFileLOD<p>
   LODka 3d models editor files

     History:
   date 12 jul 2007 - Added skeleton weights support.             // 12 jul 2007
                      Don't load invisible FaceGroups and Meshes. // 12 jul 2007
   date 13 feb 2007 - Added colliders support.
   date  7 nov 2006 - Changed ReadMesh1 call. Small bugfix.
   date  1 oct 2006 - Added light schemes support.
   date  5 aug 2006 - Bugfix. Loading animations in TGLFreeForm.
   date  7 jun 2006 - Add physics support.
   date  1 may 2006 - Add skeleton support.
   date 23 feb 2006 - Creation.
}

interface

uses
  Classes, SysUtils, GLVectorFileObjects, GLTexture, ApplicationFileIO,
  VectorGeometry
  , ULODkaUtilsA1
  ;

type
    // TGLLODVectorFile
  TGLLODVectorFile = class(TVectorFile)
  public
    { Public Declarations }
    class function Capabilities : TDataFileCapabilities; override;

    procedure LoadFromStream(aStream : TStream); override;
  end;

var
    // Additional text information from LOD-file.
  LODFileTextInfo : string = '';

    // Ligth schemes
  LightSchemes         : TLightOptAss;
  isLigthSchemeLoaded  : Boolean;
    // Physics
  EnablePhysicsLoading : Boolean = False;
  isPhysLoaded         : Boolean = False;
    // Main phisic arrays.
  LODBodys             : TLOD1Bodys;
  LODGeoms             : TLOD1Geoms;
  LODJoints            : TLOD1Joints;
    // Additional FaceGroups information.
  GropusEx1            : TLODGroupsExs;
  
    // Enable/disable loading of disabled Meshes.      // 12 jul 2007
  isLoadDisabledMeshes     : Boolean = True;           // 12 jul 2007
    // Enable/disable loading of disabled FaceGroups.  // 12 jul 2007
  isLoadDisabledFaceGroups : Boolean = True;           // 12 jul 2007

    // Additional animations information.
  LODAnimExDatas1      : TLODAnimExDatas1;
    // Colliders arrays.
  SkelCollidersList    : TSkelCollidersList;

implementation

  // Capabilities
  //
class function TGLLODVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

  // LoadFromStream
  //
procedure TGLLODVectorFile.LoadFromStream(aStream : TStream);
var
  LightSchemeChunk,
  PHS1Chunk,
  Inf1Chunk,
  MainChunk, Chunk2Level, MaterChunk,
  SkelChunk, AnimsChunk, Anim1Chunk,
  CollListChunk, CollChunk           : TLODChunk;
  MeshChunk                          : array of TLODChunk;
  Pos1, colInd,
  FirstMesh, mo1, i, c2, an1         : Integer;
  s1                                 : string;
  isMeshLoaded, isSkeletonLoaded     : Boolean;
begin
  if Owner = nil then exit;
  with aStream do begin
    try
      SetLength(s1, Length(LODkaSignature1));
      Read(s1[1],  Length(LODkaSignature1));
      if CompareText(s1, LODkaSignature1) <> 0 then begin
        Assert(False, 'Not valid *.LOD file.');
        exit;
      end;

      Read(MainChunk, SizeOf(TLODChunk));
      if MainChunk.ID <> 'LOD1' then begin
        Assert(False, 'Wrong version of *.LOD file loader. Try to download'
         +' a new version from LODka''s site');
        exit;
      end;

      isLigthSchemeLoaded := False;
      SetLength(LightSchemes, 0);
      isPhysLoaded     := False;
      LODFileTextInfo  := '';
      isSkeletonLoaded := False;
      isMeshLoaded     := False;
      FirstMesh        := Owner.MeshObjects.Count;
      for c2 := 0 to MainChunk.Count -1 do begin
        Read(Chunk2Level, SizeOf(TLODChunk));


          // Light schemes.
        if Chunk2Level.ID = LightAllSceneSign then begin // 'LGS1'
          LightSchemeChunk := Chunk2Level;
          SetLength(LightSchemes, LightSchemeChunk.Count);
          for i := 0 to Length(LightSchemes) -1 do begin
            isLigthSchemeLoaded
              := LoadLightInSceneFromStream(aStream, LightSchemes[i]);
            if not isLigthSchemeLoaded then begin
              Assert(False, 'Wrong light scheme !');
              exit;
            end;
          end;
        end else

          // Physics.
        if Chunk2Level.ID = 'PHS1' then begin
          PHS1Chunk := Chunk2Level;
          if     EnablePhysicsLoading
             and (PHS1Chunk.Count = 2)
          then begin
            LoadPhysicsFromStream1(aStream, LODBodys, LODGeoms, LODJoints);
            isPhysLoaded := True;
          end else begin
            Position := Position +PHS1Chunk.Size;
          end;
        end else

          // LODFileTextInfo.
        if Chunk2Level.ID = 'INF1' then begin
          Inf1Chunk := Chunk2Level;
          if Inf1Chunk.Count = 1 then begin
            SetLength(LODFileTextInfo, Inf1Chunk.Size);
            Read(LODFileTextInfo[1], Inf1Chunk.Size);
          end else begin
            Position := Position +Inf1Chunk.Size;
          end;
        end else

          // Skeleton.
        if Chunk2Level.ID = 'SKL1' then begin
          if not isMeshLoaded then begin
            SkelChunk := Chunk2Level;
            ReadSkeleton1(aStream, Owner, Chunk2Level.Count);
            isSkeletonLoaded := True;
          end else begin
            Position := Position +Chunk2Level.Size;
          end;
        end else

          // Colliders.
        if Chunk2Level.ID = 'CLL1' then begin
          if isSkeletonLoaded then begin
              // Read list of colliders list.
            CollListChunk := Chunk2Level;
            SetLength(SkelCollidersList, CollListChunk.Count);
            for colInd := 0 to CollListChunk.Count -1 do begin
                // Read chunk of colliders list.
              Read(CollChunk, SizeOf(TLODChunk));
              if CollChunk.ID = 'COL1' then begin
                  // Read colliders list.
                if CollChunk.Count > 0 then
                  LoadCollidersFromStream(aStream,
                    SkelCollidersList[colInd],
                    CollChunk.Count);
              end else begin
                  // Not colliders list chunk.
                Position := Position +CollChunk.Size;
              end;
            end;
          end else begin
              // Skeleton is not loaded.
            Position := Position +Chunk2Level.Size;
          end;
        end else

          // Animations.
        if Chunk2Level.ID = 'ANS1' then begin
          if isSkeletonLoaded then begin
            Pos1       := Position;
            AnimsChunk := Chunk2Level;
            SetLength(LODAnimExDatas1, AnimsChunk.Count);
            for an1 := 0 to AnimsChunk.Count -1 do begin
              if     (an1 > 0)
                 and not (Owner is TGLActor)
              then begin
                Position := Pos1 +Chunk2Level.Size;
                break;
              end;
              Read(Anim1Chunk,  SizeOf(TLODChunk));
              ReadAnim1(aStream, Owner, an1, Anim1Chunk.Count,
                SkelChunk.Count
                , LODAnimExDatas1[an1]
              );
            end;
          end else begin
            Position := Position +Chunk2Level.Size;
          end;
        end else

          // Material Library
        if Chunk2Level.ID = 'MAL1' then begin
          Read(MaterChunk,     SizeOf(TLODChunk));
          ReadMat1(aStream, Owner.MaterialLibrary);
        end else

          // Mesh Library
        if Chunk2Level.ID = 'MSL1' then begin
          isMeshLoaded := True;
          SetLength(MeshChunk, Chunk2Level.Count);
          for mo1 := 0 to Chunk2Level.Count -1 do begin
            Read(MeshChunk[mo1], SizeOf(TLODChunk));
            ReadMesh1(aStream, Owner, GropusEx1, isSkeletonLoaded,
              SkelChunk.Count, 
              isLoadDisabledMeshes, isLoadDisabledFaceGroups); // 12 jul 2007
          end;
        end else begin
          Position := Position +Chunk2Level.Size;
        end;
      end;

      if isSkeletonLoaded then begin
        Owner.Skeleton.RootBones.PrepareGlobalMatrices;
        for i := FirstMesh to Owner.MeshObjects.Count -1 do
          TSkeletonMeshObject(Owner.MeshObjects[i]).PrepareBoneMatrixInvertedMeshes;
      end;
    except
      Assert(False, 'Something wrong in GLFileLOD.pas .');
    end;
  end;
end;

initialization

  RegisterVectorFileFormat('lod', 'LODka 3d models editor files',
    TGLLODVectorFile);

end.