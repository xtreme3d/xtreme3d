unit ULODkaUtilsA1;

{: ULODkaUtils<p>
   LODka 3d models editor files

     History:
   date 12 jul 2007 - Added skeleton weights support.             // 12 jul 2007
                      Don't load invisible FaceGroups and Meshes. // 12 jul 2007
   date 13 feb 2007 - Added colliders support.
   date 20 nov 2006 - BugFix. Loading second textrue.
   date  7 nov 2006 - Added sectond texture support. Small bugfix.
   date  1 oct 2006 - Added light schemes support.
   date 30 aug 2006 - FaceGroups type = 2. Added smooth groups. Changed range
                      of VertexIndices, NormalsIndices and TexCoordIndices
                      from ( Min..255  256..65535  65535..Max )
                      to   ( Min..256  257..65536  65536..Max ). Thanks DDMZ.
   date  5 aug 2006 - Bugfix. Loading animations in TGLFreeForm.
   date 27 jun 2006 - Added some physics parametrs.
   date  7 jun 2006 - Added physics support.
   date  1 may 2006 - Added skeleton support.
   date 23 feb 2006 - Creation.
}

interface

{$DEFINE UseSkeleton}
{$DEFINE UseSkeletonWeight}

uses
  Windows, System.SysUtils, System.Classes, GLS.VectorFileObjects, GLS.Texture,
  GLS.VectorGeometry, GLS.VectorTypes, GLS.Scene, GLS.Material;

const
  LODkaSignature1   = 'LODka3D1';
  LightSceneSign    = 'LIG1';
  LightAllSceneSign = 'LGS1';


type
    // Chunk header.
  TLODChunk = packed record
    ID    : array [0..3] of Char;
    Size  : Integer;
    Count : Integer;
  end;


      // Physics
    // Phisics body types.
  TBodyTypeA = ( bdtNone, bdtBox, bdtSphere, bdtCylinder, bdtCapsule
    , bdtCone, bdtPlane, bdtTriMesh, bdtCCylinder, bdtRay);

    // Phisics joint types.
  TJointTypeA = (jtaNone, jtaBall, jtaSlider, jtaHinge, jtaHinge2, jtaUniversal
    , jtaCorkscrew, jtaUpVector
    );

    // Physics bodies.
  TLOD1Body = packed record
    Type1       : TBodyTypeA;
    Name        : string;
    Position    : TAffineVector;
    Scale       : TAffineVector;
    Rotation    : TQuaternion;
    Axis        : Integer;
    Mass        : Single;
    MassDensity : Single;
    TagFloat    : Single;
    Enabled     : Boolean;

    isGravity        : Boolean;
    isFiniteRot      : Boolean;
    isAutoDisable    : Boolean;
    isAutoDisableDef : Boolean;
    ADLinearThr      : Single;
    ADAngularThr     : Single;
    ADisableTime     : Single;
    ADSteps          : Integer;
  end;
  TLOD1Bodys = array of TLOD1Body;

    // Physics geoms ( colliders in Newton Dynamics ).
  TLOD1Geom = packed record
    Type1       : TBodyTypeA;
    Name        : string;
    BodyIndex   : Integer;
    Position    : TAffineVector;
    Scale       : TAffineVector;
    Rotation    : TQuaternion;
    Axis        : Integer;
    Mass        : Single;
    MassDensity : Single;
    TagFloat    : Single;
    MeshIndex   : array of Integer;
    Enabled     : Boolean;
  end;
  TLOD1Geoms = array of TLOD1Geom;

    // Physics joints.
  TLOD1Joint = packed record
    JointTypeA          : TJointTypeA;
    BodyIndex           : array [0..1] of Integer;
    Axis                : array [0..1] of TQuaternion;
    Position            : TAffineVector;
    dParamLoStop,       dParamHiStop, dParamVel,  dParamFMax,
    dParamFudgeFactor,  dParamBounce, dParamCFM,  dParamStopERP,
    dParamStopCFM,      dParamSuspensionERP,
    dParamSuspensionCFM, TagFloat : Single;
  end;
  TLOD1Joints = array of TLOD1Joint;




     // Animation
  TLODAnimationMode = (
    aamLODNone, aamLODPlayOnce, aamLODLoop, aamLODBounceForward,
    aamLODBounceBackward, aamLODLoopBackward, aamLODExternal);

    // Animations additional information. Intervals and Modes.
  TLODAnimExData1 = packed record
    Interval : Integer;
    Mode     : TLODAnimationMode;
  end;
  TLODAnimExDatas1 = array of TLODAnimExData1;


      // Skeleton colliders type.
    TSkelCollTypeA = (sctSphere, sctCapsule, sctCylinder, sctBox);

      // Skeleton colliders axis info.
    TSkelColJointInfo = packed record
      Axis               : TQuaternion;
      MinAngle, MaxAngle : Single;
    end;

      // 12 jul 2007 begin
      // Vertex bone weight ( bones count 1-127 ).
    TVertexBoneWeightBA = packed record
      BoneID : Byte;
      Weight : Single;
    end;
      // Vertex bone weight ( bones count 128..32767 ).
    TVertexBoneWeightWA = packed record
      BoneID : Word;
      Weight : Single;
    end;
      // 12 jul 2007 end

    // One collider.
  PSkelCollider = ^TSkelCollider;
  TSkelCollider = packed record
    Name         : string;
    BoneID       : SmallInt;
    SkelCollType : TSkelCollTypeA;
    Matrix       : TMatrix4f;
    Scale        : TAffineVector;
    Tag          : Integer;

    TagFloat     : Single;
    Visible      : Boolean;
    JointInfo    : array [0..1] of TSkelColJointInfo;
    // Visible      : Boolean;
  end;
    // Colliders list.
  TSkelColliders = record
    Name  : string;
    Items : array of TSkelCollider;
  end;
    // List of colliders list.
  PSkelCollidersList = ^TSkelCollidersList;
  TSkelCollidersList = array of TSkelColliders;



    // Materials
  TLODSkelSave = (lodSSnone, lodSkSaveV1);

  TMaterialSaveType = (
    mstNotSave,
    mstOnlyNames,
    mstFourColors,
    mstTwoTextures
  );

    TMaterialSaveRecA = packed record
      MaterialName, FileName : string;
    end;

    TMaterialSaveRecAEx = packed record
      Ambient,  Diffuse,
      Emission, Specular, EnvColor   : TGLVector;
      Shininess                      : TGLShininess;
      ImageBrightness, ImageGamma,
      NormalMapScale                 : Single;
      BlendingMode                   : TGLBlendingMode;
      ImageAlpha                     : TGLTextureImageAlpha;
      TextureMode                    : TGLTextureMode;
      TextureWrap                    : TGLTextureWrap;
      TextureFormat                  : TGLTextureFormat;
      TextureEnabled                 : Boolean;
      Enabled                        : Boolean;
    end;

    TMaterialSaveRecAEx2 = packed record
      Ambient,  Diffuse,
      Emission, Specular, EnvColor   : TGLVector;
      Shininess                      : TGLShininess;
      ImageBrightness, ImageGamma,
      NormalMapScale                 : Single;
      BlendingMode                   : TGLBlendingMode;
      ImageAlpha                     : TGLTextureImageAlpha;
      TextureMode                    : TGLTextureMode;
      TextureWrap                    : TGLTextureWrap;
      TextureFormat                  : TGLTextureFormat;
      TextureEnabled                 : Boolean;
      Enabled                        : Boolean;
      MappingMode                    : TGLTextureMappingMode;
    end;

  TMaterialSaveRecM = packed record
    Names  : TMaterialSaveRecA;
    Params : TMaterialSaveRecAEx;
  end;

    // Additiona iformation about each FaceGroup.
    TLODGroupsEx = packed record
      Name    : string;
      Enabled : Boolean;
    end;

  TLODGroupsExs  = array of TLODGroupsEx;
  TLODGroupsExss = array of TLODGroupsExs;

  function  ReadLODChunk(aStream : TStream; var aLODChunk : TLODChunk): Boolean;

  procedure ReadMat1(aStream : TStream; aMatLib : TGLMaterialLibrary);

  procedure ReadMesh1(aStream: TStream
    ; aBaseMesh      : TGLBaseMesh
    ; var aGropusEx  : TLODGroupsExs
    ; isLoadSkeleton : Boolean
    ; aBonesCount    : Integer = 0
    ; isLoadDisabledMeshs1      : Boolean = True // 12 jul 2007
    ; isLoadDisabledFaceGroups1 : Boolean = True // 12 jul 2007
  );

  procedure ReadSkeleton1(aStream : TStream; aBaseMesh : TGLBaseMesh
    ; BonesCount    : Integer
    );

  procedure ReadAnim1(aStream: TStream; aBaseMesh : TGLBaseMesh
    ; anAnimNumber : Integer; aFramesCount : Integer
    ; aBonesCount : Integer
    ; var aLODAnimExData : TLODAnimExData1
  );

    // Physics.
  function LoadPhysicsFromStream1(aStream : TStream;
    var aLODBodys  : TLOD1Bodys;
    var aLODGeoms  : TLOD1Geoms;
    var aLODJoints : TLOD1Joints
  ) : Boolean;


type
    // Lightings.
      // One light source.
    TLightOptA = packed record
      Enabled                      : Boolean;
      LigthStyle                   : TGLLightStyle;
      Position, Direction          : TAffineVector;
      Ambient, Diffuse, Specular   : TGLVector;
      SpotCutOff, SpotExponent,
      ConstAtt, LinearAtt, QuadAtt : Single;
    end;
     // TLightStyle = (lsSpot, lsOmni, lsParallel); // GLScene's light styles.

    // All light sourcies in scene ( 8 lightsources and name ) .
  TLightOptAs = packed record
    Name  : string;
    Items : array [0..7] of TLightOptA;
  end;

  TLightOptAss = array of TLightOptAs;


    // Read one light scheme from stream.
  function LoadLightInSceneFromStream(aStream : TStream;
    var LightOptAs1 : TLightOptAs) : Boolean;

    // Set data ftom TLightOptA to TGLLightSource
  procedure  SetDataToLigthSource(var aLigthSource : TGLLightSource;
    aLightOptA : TLightOptA);


    // Load Colliders From Stream.
  function LoadCollidersFromStream(aStream : TStream;
    var   aSkelColliders  : TSkelColliders;
    const aCollidersCount : Integer): Boolean;


    // ------- Utils -------

    // Copy colliders from aSource to aDest.
  procedure CopyColliders(var aDest : TSkelColliders;
    const aSource :TSkelColliders);
    // Copy colliders list from aSource to aDest.
  procedure CopyCollidersList(var aDest : TSkelCollidersList;
    const aSource :TSkelCollidersList);
    // Append colliders list from aSource to aDest.
  procedure AppendCollidersList(var aDest : TSkelCollidersList;
    const aSource :TSkelCollidersList);


    // Copy physics Body arrays.
  function CopyPhysBodyArrays(
    var   aDestLODBodys   : TLOD1Bodys;
    const aSourceLODBodys : TLOD1Bodys) : Boolean;

    // Copy physics Geoms arrays.
  function CopyPhysGeomArrays(
    var   aDestLODGeoms   : TLOD1Geoms;
    const aSourceLODGeoms : TLOD1Geoms) : Boolean;

    // Copy physics Joints arrays.
  function CopyPhysJointsArrays(
    var   aDestLODJoints   : TLOD1Joints;
    const aSourceLODJoints : TLOD1Joints) : Boolean;

var
  isBuildNormalsIfNecessary : Boolean = True;

implementation

function InRange(const AValue, AMin, AMax: Integer): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function ReadLODChunk(aStream: TStream; var aLODChunk : TLODChunk): Boolean;
begin
  Result := aStream.Position < aStream.Size;
  if Result then
    aStream.Read(aLODChunk, 12);
end;

procedure ReadMat1(aStream: TStream; aMatLib : TGLMaterialLibrary);
var
  MatSType   : TMaterialSaveType;
  MatSave    : array of TMaterialSaveRecA;
  MatSaveEx1 : array of TMaterialSaveRecAEx;
  MatSaveEx2 : array of TMaterialSaveRecAEx2;

  procedure PutArraysToMatLib;
  var
    ps1,
    i      : Integer;
    MatLib : TGLMaterialLibrary;
    LbMat  : TGLLibMaterial;
    sFileName, sTextrue2Name : string;
  begin
    MatLib := aMatLib;
    if MatLib <> nil then
      for i := 0 to Length(MatSave) -1 do begin
        ps1 := Pos(':', MatSave[i].FileName);
        if ps1 = 0 then begin
          sFileName     := MatSave[i].FileName;
          sTextrue2Name := '';
        end else begin
          sFileName     := Copy(MatSave[i].FileName, 1, ps1 -1);
          sTextrue2Name := Copy(MatSave[i].FileName, ps1 +1, MaxInt);
        end;
        LbMat := MatLib.Materials.GetLibMaterialByName(
          MatSave[i].MaterialName);
        if LbMat = nil then begin
          //if FileExists(sFileName) then begin
          if Length(sFileName) > 0 then begin
            LbMat := MatLib.AddTextureMaterial(MatSave[i].MaterialName,
              sFileName);
            LbMat.Texture2Name := sTextrue2Name;
          end else begin
            LbMat      := MatLib.Materials.Add;
            LbMat.Name := MatSave[i].MaterialName;
            LbMat.Texture2Name := sTextrue2Name;
          end;
        end else begin
          //if FileExists(sFileName) then begin
          if Length(sFileName) > 0 then begin
            LbMat.Material.Texture.Image.LoadFromFile(
              sFileName);
            LbMat.Material.Texture.Disabled := False;
          end;
          LbMat.Texture2Name := sTextrue2Name;
        end;
      end;
  end;

  procedure PutArraysToMatLibEx;
  var
    k2, i : Integer;
    LbMat : TGLLibMaterial;
  begin
    if aMatLib = nil then exit;
    k2 := Length(MatSaveEx1);
    if Length(MatSave) < k2 then begin
      beep;
      k2 := Length(MatSave);
    end;
    for i := 0 to k2 -1 do begin
      if i > Length(MatSaveEx2) -1 then begin
        // beep;
      end;
      if not MatSaveEx1[i].Enabled then continue;
      LbMat := aMatLib.Materials.GetLibMaterialByName(
        MatSave[i].MaterialName);
      if LbMat <> nil then begin
        with LbMat.Material do begin
          BlendingMode                    := MatSaveEx1[i].BlendingMode;
          FrontProperties.Ambient.Color   := MatSaveEx1[i].Ambient;
          FrontProperties.Diffuse.Color   := MatSaveEx1[i].Diffuse;
          FrontProperties.Emission.Color  := MatSaveEx1[i].Emission;
          FrontProperties.Specular.Color  := MatSaveEx1[i].Specular;
          FrontProperties.Shininess       := MatSaveEx1[i].Shininess;
          Texture.Disabled                := not MatSaveEx1[i].TextureEnabled;
          Texture.EnvColor.Color          := MatSaveEx1[i].EnvColor;
          Texture.ImageBrightness         := MatSaveEx1[i].ImageBrightness;
          Texture.ImageGamma              := MatSaveEx1[i].ImageGamma;
          Texture.NormalMapScale          := MatSaveEx1[i].NormalMapScale;
          Texture.ImageAlpha              := MatSaveEx1[i].ImageAlpha;
          Texture.TextureMode             := MatSaveEx1[i].TextureMode;
          Texture.TextureWrap             := MatSaveEx1[i].TextureWrap;
          Texture.TextureFormat           := MatSaveEx1[i].TextureFormat;
        end;
      end;
    end;
  end;

  procedure PutArraysToMatLibEx2;
  var
    k2, i : Integer;
    LbMat : TGLLibMaterial;
  begin
    if aMatLib = nil then exit;
    k2 := Length(MatSaveEx2);
    if Length(MatSave) < k2 then begin
      beep;
      k2 := Length(MatSave);
    end;
    for i := 0 to k2 -1 do begin
      if i > Length(MatSaveEx2) -1 then begin
        beep;
      end;
      if not MatSaveEx2[i].Enabled then continue;
      LbMat := aMatLib.Materials.GetLibMaterialByName(
        MatSave[i].MaterialName);
      if LbMat <> nil then begin
        with LbMat.Material do begin
          BlendingMode                    := MatSaveEx2[i].BlendingMode;
          FrontProperties.Ambient.Color   := MatSaveEx2[i].Ambient;
          FrontProperties.Diffuse.Color   := MatSaveEx2[i].Diffuse;
          FrontProperties.Emission.Color  := MatSaveEx2[i].Emission;
          FrontProperties.Specular.Color  := MatSaveEx2[i].Specular;
          FrontProperties.Shininess       := MatSaveEx2[i].Shininess;
          Texture.Disabled                := not MatSaveEx2[i].TextureEnabled;
          Texture.EnvColor.Color          := MatSaveEx2[i].EnvColor;
          Texture.ImageBrightness         := MatSaveEx2[i].ImageBrightness;
          Texture.ImageGamma              := MatSaveEx2[i].ImageGamma;
          Texture.NormalMapScale          := MatSaveEx2[i].NormalMapScale;
          Texture.ImageAlpha              := MatSaveEx2[i].ImageAlpha;
          Texture.TextureMode             := MatSaveEx2[i].TextureMode;
          Texture.TextureWrap             := MatSaveEx2[i].TextureWrap;
          Texture.TextureFormat           := MatSaveEx2[i].TextureFormat;
          Texture.MappingMode             := MatSaveEx2[i].MappingMode;
        end;
      end;
    end;
  end;

var
  i, MatCnt, kLen2 : Integer;
begin
  with aStream do begin
    Read(MatSType, SizeOf(TMaterialSaveType));
    if Integer(MatSType) > Integer(mstTwoTextures) then begin
      Assert(False, 'Wrong material type. Download a new version of ULODkaUtils file.');
    end;
    case MatSType of
      mstNotSave : begin
        end;
      mstOnlyNames : begin
          Read(MatCnt, SizeOf(Integer));
          SetLength(MatSave, MatCnt);
          for i := 0 to MatCnt -1 do begin
            Read(kLen2, SizeOf(Integer));
            SetLength(MatSave[i].MaterialName, kLen2);
            if kLen2 > 0 then
              Read(MatSave[i].MaterialName[1], kLen2);

            Read(kLen2, SizeOf(Integer));
            SetLength(MatSave[i].FileName, kLen2);
            if kLen2 > 0 then
              Read(MatSave[i].FileName[1], kLen2);
          end;
          PutArraysToMatLib;
        end;
      mstFourColors : begin
          Read(MatCnt, SizeOf(Integer));
          SetLength(MatSave,    MatCnt);
          SetLength(MatSaveEx1, MatCnt);
          for i := 0 to MatCnt -1 do begin
            Read(kLen2, SizeOf(Integer));
            SetLength(MatSave[i].MaterialName, kLen2);
            if kLen2 > 0 then
              Read(MatSave[i].MaterialName[1], kLen2);
            Read(kLen2, SizeOf(Integer));
            SetLength(MatSave[i].FileName, kLen2);
            if kLen2 > 0 then
              Read(MatSave[i].FileName[1], kLen2);

            kLen2 := SizeOf(TMaterialSaveRecAEx);
            if kLen2 > 0 then
              Read(MatSaveEx1[i], kLen2);
          end;
          PutArraysToMatLib;
          PutArraysToMatLibEx;
        end;

      mstTwoTextures : begin
          Read(MatCnt, SizeOf(Integer));
          SetLength(MatSave,    MatCnt);
          SetLength(MatSaveEx2, MatCnt);
          for i := 0 to MatCnt -1 do begin
            Read(kLen2, SizeOf(Integer));
            SetLength(MatSave[i].MaterialName, kLen2);
            if kLen2 > 0 then
              Read(MatSave[i].MaterialName[1], kLen2);

            Read(kLen2, SizeOf(Integer));
            SetLength(MatSave[i].FileName, kLen2);
            if kLen2 > 0 then
              Read(MatSave[i].FileName[1], kLen2);

            kLen2 := SizeOf(TMaterialSaveRecAEx2);
            if kLen2 > 0 then
              Read(MatSaveEx2[i], kLen2);
          end;
          PutArraysToMatLib;
          PutArraysToMatLibEx2;
        end;
    end;
  end;
end;


procedure ReadMesh1(aStream: TStream
    ; aBaseMesh      : TGLBaseMesh
    ; var aGropusEx  : TLODGroupsExs
    ; isLoadSkeleton : Boolean
    ; aBonesCount    : Integer = 0
    ; isLoadDisabledMeshs1      : Boolean = True // 12 jul 2007
    ; isLoadDisabledFaceGroups1 : Boolean = True // 12 jul 2007
  );

  function FindLigthMapIndex(aName : string): Integer;
  begin
    if aBaseMesh.LightmapLibrary = nil then begin
      Result := -1;
      exit;
    end;
    for Result := 0 to aBaseMesh.LightmapLibrary.Materials.Count -1 do
      if CompareText(aName, aBaseMesh.LightmapLibrary.Materials[Result].Name) = 0 then
        exit;
    Result := -1;
  end;


type
  TTwoSingle = array [0..1] of Single;
var
  mMode2          : Byte;
  fMode1          : TGLFaceGroupMeshMode;
  vertCnt, normCnt,
  BoneCnt, BoneWidthCnt,
  txcrCnt, n1,
  i, kCnt, fg1    : Integer;
  isSkeletonExists,
  isVisible       : Boolean;
  af2             : array of TAffineVector;
  sMName, sFName  : string;
  Mesh            : TGLMeshObject;
  SMeshO          : TGLSkeletonMeshObject;
  av1             : TAffineVector;

  aIndI       : array of Integer;
  aIndW       : array of Word;
  aIndB       : array of Byte;
  aInd2S      : array of TTwoSingle;

  {$IFDEF UseSkeleton}
  BnWidB      : array of Byte;
  BnWidW      : array of Word;
  {$ENDIF}
  SmoothCnt, FgType : Integer;


  TextureCnt      : Byte;
  TextureCoordCnt : array [0..1] of Integer;
  MatNamesLen     : array [0..1] of Integer;
  TxIndLen        : array [0..1] of Integer;
  FGrLM           : array of array of Integer;
  VrCnt, NrCnt, Tx1Cnt, Tx2Cnt, FgCnt, tr1, tr2, fgInd : Integer;
  fgVNT           : TFGVertexNormalTexIndexList;
  fgV             : TFGVertexIndexList;

    // Bones weight
  BonesPerVertix1, k2, ptInd1, BnWInd1 : Integer;
  BoneWB : array of TVertexBoneWeightBA;
  BoneWW : array of TVertexBoneWeightWA;
begin
  with aStream do begin

    aStream.Read(kCnt, SizeOf(Integer));
    if kCnt > 0 then begin
      SetLength(sMName, kCnt);
      if kCnt > 0 then
        aStream.Read(sMName[1], kCnt);
    end;

    aStream.Read(isSkeletonExists, SizeOf(Boolean));

    if isLoadSkeleton then begin
      SMeshO := TGLSkeletonMeshObject.CreateOwned(aBaseMesh.MeshObjects);
      Mesh   := SMeshO;
    end else begin
      Mesh   := TGLMeshObject.CreateOwned(aBaseMesh.MeshObjects);
    end;

    with Mesh do begin
      Name := sMName;
      Mode := momFaceGroups;

      aStream.Read(mMode2, SizeOf(TGLMeshObjectMode));
      if not InRange(mMode2, 2, 3) then begin
        beep;
        Assert(False, 'Unknown mesh mode : ' +IntToStr(mMode2) +'.');
      end;

      aStream.Read(isVisible, SizeOf(Boolean));
      Visible := isVisible;

        // Load vetices.
      aStream.Read(vertCnt, SizeOf(Integer));
      if vertCnt > 0 then begin
        SetLength(af2, vertCnt);
        aStream.Read(af2[0], vertCnt *SizeOf(TAffineVector));
        for i := 0 to vertCnt -1 do
          Vertices.Add(af2[i]);
      end;

        // Load normals.
      aStream.Read(normCnt, SizeOf(Integer));
      if isLoadSkeleton then begin
        aStream.Position := aStream.Position +(normCnt *SizeOf(TAffineVector));
        for i := 0 to vertCnt -1 do
          Normals.Add(NullVector);
      end else

      begin
        if normCnt > 0 then begin
          SetLength(af2, normCnt);
          aStream.Read(af2[0], normCnt *SizeOf(TAffineVector));
          for i := 0 to normCnt -1 do
            Normals.Add(af2[i]);
        end;
      end;

        // Load TextureCoords.
      if mMode2 = 3 then begin
        aStream.Read(TextureCnt, SizeOf(Byte));
        Assert(InRange(TextureCnt, 1, 2), 'Wrong texture per mesh number : '
          +IntToStr(TextureCnt) +'.');
        aStream.Read(TextureCoordCnt[0], SizeOf(Integer) *TextureCnt);
          // first texture.
        if TextureCnt > 0 then begin
          if TextureCoordCnt[0] > 0 then begin
            SetLength(aInd2S, TextureCoordCnt[0]);
            aStream.Read(aInd2S[0], TextureCoordCnt[0] *SizeOf(TTwoSingle));
            for i := 0 to TextureCoordCnt[0] -1 do
              TexCoords.Add(aInd2S[i][0], aInd2S[i][1], 0);
          end;
        end;
          // second texture.
        if TextureCnt > 1 then begin
          if TextureCoordCnt[1] > 0 then begin
            SetLength(aInd2S, TextureCoordCnt[1]);
            aStream.Read(aInd2S[0], TextureCoordCnt[1] *SizeOf(TTwoSingle));
            for i := 0 to TextureCoordCnt[1] -1 do
              LightMapTexCoords.Add(aInd2S[i][0], aInd2S[i][1]);
          end;
        end;
      end else begin
        aStream.Read(txcrCnt, SizeOf(Integer));
        if txcrCnt > 0 then begin
          SetLength(aInd2S, txcrCnt);
          aStream.Read(aInd2S[0], txcrCnt *SizeOf(TTwoSingle));
          for i := 0 to txcrCnt -1 do
            TexCoords.Add(aInd2S[i][0], aInd2S[i][1], 0);
        end;
      end;

        // Load AddWeightedBone.
      {$IFDEF UseSkeleton}
      aStream.Read(BoneCnt, SizeOf(Integer));
      if BoneCnt > 0 then begin
        if isLoadSkeleton then begin
          if aBonesCount < 255 then begin
            SetLength(BnWidB, BoneCnt);
            aStream.Read(BnWidB[0], BoneCnt *SizeOf(Byte));
            for i := 0 to BoneCnt -1 do
              if InRange(BnWidB[i], 0, aBonesCount -1)
                then SMeshO.AddWeightedBone(BnWidB[i], 1)
                else SMeshO.AddWeightedBone(0, 1);
          end else begin
            SetLength(BnWidW, BoneCnt);
            aStream.Read(BnWidW[0], BoneCnt *SizeOf(Word));
            for i := 0 to BoneCnt -1 do
              if InRange(BnWidW[i], 0, aBonesCount -1)
                then SMeshO.AddWeightedBone(BnWidW[i], 1)
                else SMeshO.AddWeightedBone(0, 1);
          end;
        end else begin
          if aBonesCount < 255 then begin
            Position := Position +BoneCnt *SizeOf(Byte);
          end else begin
            Position := Position +BoneCnt *SizeOf(Word);
          end;
        end;
          // BoneWidthCnt must be zero.
        Read(BoneWidthCnt, SizeOf(Integer));
      // end;


      // 12 jul 2007 begin
      {$IFDEF UseSkeletonWeight}
      end else begin
          // Read Bones weight type.
        aStream.Read(BoneWidthCnt, SizeOf(Integer));
        if BoneWidthCnt = 1 then begin
          // First type.
            // Read number of bones per vertex. And set it.
          aStream.Read(BonesPerVertix1, SizeOf(Integer));
            // Set array size.
          SMeshO.BonesPerVertex         := BonesPerVertix1;
          SMeshO.VerticeBoneWeightCount := vertCnt; // BoneCnt;
            // Read bones, weight data.
          if aBonesCount <= 127 then begin
            // Bones count 1 - 127
              // Read length of BoneWB and BoneWB data.
            aStream.Read(k2, SizeOf(Integer));
            SetLength(BoneWB, k2);
            if k2 > 0 then begin
              aStream.Read(BoneWB[0], k2 *SizeOf(TVertexBoneWeightBA));
                // Move data from BoneWB â SMeshO.VerticesBonesWeights.
              ptInd1  := 0;
              BnWInd1 := 0;
              for i := 0 to k2 -1 do begin
                  // Read data for the bone. 
                SMeshO.VerticesBonesWeights[ptInd1][BnWInd1].BoneID 
                  := Byte(BoneWB[i].BoneID) and $7F;
                SMeshO.VerticesBonesWeights[ptInd1][BnWInd1].Weight 
                  := BoneWB[i].Weight;
                if     ((BoneWB[i].BoneID and $80) = 0)
                   and (BnWInd1 < BonesPerVertix1 -1) 
                       // And that is not last bone for that vetrex.  
                then begin
                  BnWInd1 := BnWInd1 +1;
                end else begin
                  ptInd1  := ptInd1 +1;
                  BnWInd1 := 0;
                end;
              end;
            end;
          end else begin
            // Bones count 128 - 32767.
              // Read length of BoneWB and BoneWB data.
            aStream.Read(k2, SizeOf(Integer));
            SetLength(BoneWW, k2);
            if k2 > 0 then begin
              aStream.Read(BoneWW[0], k2 *SizeOf(TVertexBoneWeightBA));
                // Move data from BoneWB â SMeshO.VerticesBonesWeights.
              ptInd1  := 0;
              BnWInd1 := 0;
              for i := 0 to k2 -1 do begin
                  // Read data for the bone. 
                SMeshO.VerticesBonesWeights[ptInd1][BnWInd1].BoneID 
                  := Word(BoneWW[i].BoneID) and $7FFF;
                SMeshO.VerticesBonesWeights[ptInd1][BnWInd1].Weight 
                  := BoneWW[i].Weight;
                if     ((BoneWW[i].BoneID and $8000) = 0)
                   and (BnWInd1 < BonesPerVertix1 -1)
                       // And that is not last bone for that vetrex.  
                then begin
                  BnWInd1 := BnWInd1 +1;
                end else begin
                  ptInd1  := ptInd1 +1;
                  BnWInd1 := 0;
                end;
              end;
            end;
          end;
        end;
      {$ENDIF} // NEW16 end
      end;
      {$ELSE}
        // No bones. Must be zero.
      aStream.Read(BoneCnt, SizeOf(Integer));
        // No bones weight. Must be zero.
      aStream.Read(BoneWidthCnt, SizeOf(Integer));
      {$ENDIF}

      (*
      {$ELSE}
      aStream.Read(BoneCnt, SizeOf(Integer));
      {$ENDIF}

      aStream.Read(BoneWidthCnt, SizeOf(Integer));
      *)
      // 12 jul 2007 end



        // Load FaceGroups number, and FaceGroups.
      aStream.Read(kCnt, SizeOf(Integer));
      SetLength(aGropusEx, kCnt);
      for fg1 := 0 to kCnt -1 do begin
        SetLength(FGrLM, fg1 +1);

          // Load FaceGroup type.
        aStream.Read(FgType, SizeOf(Integer));
        if (FgType < 1) or (FgType > 2) then begin
          beep;
          Assert(False, 'Unknown FaceGroups format');
        end;

        with TFGVertexNormalTexIndexList.CreateOwned(FaceGroups) do begin
          aStream.Read(fMode1, SizeOf(TGLFaceGroupMeshMode));
          Mode := fMode1;

            // Load additional information about FaceGroup.
          aStream.Read(kCnt, SizeOf(Integer));
          SetLength(sFName, kCnt);
          if kCnt > 0 then
            aStream.Read(sFName[1], kCnt);
          aGropusEx[fg1].Name := sFName;
          aStream.Read(isVisible, SizeOf(Boolean));
          aGropusEx[fg1].Enabled := isVisible;

            // Load matetial names.
          if mMode2 = 3 then begin
            aStream.Read(MatNamesLen[0], SizeOf(Integer) *TextureCnt);
              // First texture material name.
            if TextureCnt > 0 then begin
              SetLength(sFName, MatNamesLen[0]);
              if MatNamesLen[0] > 0 then
                aStream.Read(sFName[1], MatNamesLen[0]);
              MaterialName := sFName;
            end;
              // Second texture material name.
            if TextureCnt > 1 then begin
              SetLength(sFName, MatNamesLen[1]);
              if MatNamesLen[1] > 0 then
                aStream.Read(sFName[1], MatNamesLen[1]);
              LightMapIndex := FindLigthMapIndex(sFName);
            end;
          end else begin
            aStream.Read(kCnt, SizeOf(Integer));
            SetLength(sFName, kCnt);
            if kCnt > 0 then
              aStream.Read(sFName[1], kCnt);
            MaterialName := sFName;
          end;

            // Load VertexIndices.
          aStream.Read(kCnt, SizeOf(Integer));
          if FgType >= 2 then begin
            if kCnt > 0 then
              if vertCnt <= High(Byte) +1 then begin
                SetLength(aIndB, kCnt);
                aStream.Read(aIndB[0], SizeOf(Byte) *kCnt);
                for i := 0 to kCnt -1 do
                  VertexIndices.Add(aIndB[i]);
              end else if vertCnt <= High(Word) +1 then begin
                SetLength(aIndW, kCnt);
                aStream.Read(aIndW[0], SizeOf(Word) *kCnt);
                for i := 0 to kCnt -1 do
                  VertexIndices.Add(aIndW[i]);
              end else begin
                SetLength(aIndI, kCnt);
                aStream.Read(aIndI[0], SizeOf(Integer) *kCnt);
                for i := 0 to kCnt -1 do
                  VertexIndices.Add(aIndI[i]);
              end;
          end else begin
            if kCnt > 0 then
              if vertCnt <= High(Byte) then begin
                SetLength(aIndB, kCnt);
                aStream.Read(aIndB[0], SizeOf(Byte) *kCnt);
                for i := 0 to kCnt -1 do
                  VertexIndices.Add(aIndB[i]);
              end else if vertCnt <= High(Word) then begin
                SetLength(aIndW, kCnt);
                aStream.Read(aIndW[0], SizeOf(Word) *kCnt);
                for i := 0 to kCnt -1 do
                  VertexIndices.Add(aIndW[i]);
              end else begin
                SetLength(aIndI, kCnt);
                aStream.Read(aIndI[0], SizeOf(Integer) *kCnt);
                for i := 0 to kCnt -1 do
                  VertexIndices.Add(aIndI[i]);
              end;
          end;

          if FgType >= 2 then begin
              // Ignore smooth groups.
            aStream.Read(SmoothCnt, SizeOf(Integer));
            if SmoothCnt > 0 then
              aStream.Position := aStream.Position +SizeOf(Integer) *SmoothCnt;
          end;

            // Load NormalIndices.
          aStream.Read(kCnt, SizeOf(Integer));
          if FgType >= 2 then begin
            if kCnt > 0 then
              if normCnt <= High(Byte) +1 then begin
                SetLength(aIndB, kCnt);
                aStream.Read(aIndB[0], SizeOf(Byte) *kCnt);
                for i := 0 to kCnt -1 do
                  NormalIndices.Add(aIndB[i]);
              end else if normCnt <= High(Word) +1 then begin
                SetLength(aIndW, kCnt);
                aStream.Read(aIndW[0], SizeOf(Word) *kCnt);
                for i := 0 to kCnt -1 do
                  NormalIndices.Add(aIndW[i]);
              end else begin
                SetLength(aIndI, kCnt);
                aStream.Read(aIndI[0], SizeOf(Integer) *kCnt);
                for i := 0 to kCnt -1 do
                  NormalIndices.Add(aIndI[i]);
              end;
          end else begin
            if kCnt > 0 then
              if normCnt <= High(Byte) then begin
                SetLength(aIndB, kCnt);
                aStream.Read(aIndB[0], SizeOf(Byte) *kCnt);
                for i := 0 to kCnt -1 do
                  NormalIndices.Add(aIndB[i]);
              end else if normCnt <= High(Word) then begin
                SetLength(aIndW, kCnt);
                aStream.Read(aIndW[0], SizeOf(Word) *kCnt);
                for i := 0 to kCnt -1 do
                  NormalIndices.Add(aIndW[i]);
              end else begin
                SetLength(aIndI, kCnt);
                aStream.Read(aIndI[0], SizeOf(Integer) *kCnt);
                for i := 0 to kCnt -1 do
                  NormalIndices.Add(aIndI[i]);
              end;
          end;

            // Calculate custom NormalIndeces for TGLActor.
          if isLoadSkeleton then begin
            NormalIndices.Clear;
            for i := 0 to (VertexIndices.Count div 3) -1 do begin
              av1 := CalcPlaneNormal(
                Vertices[ VertexIndices[i *3 +0] ],
                Vertices[ VertexIndices[i *3 +1] ],
                Vertices[ VertexIndices[i *3 +2] ]  );

              Normals.TranslateItem(VertexIndices[i *3 +0], av1);
              Normals.TranslateItem(VertexIndices[i *3 +1], av1);
              Normals.TranslateItem(VertexIndices[i *3 +2], av1);

              NormalIndices.Add(VertexIndices[i *3 +0]);
              NormalIndices.Add(VertexIndices[i *3 +1]);
              NormalIndices.Add(VertexIndices[i *3 +2]);
            end;
            if VertexIndices.Count <> NormalIndices.Count then begin
              beep;
            end;
          end else
          begin

            if     (kCnt    = 0)
               and (normCnt = 0)
               and isBuildNormalsIfNecessary
            then begin
              for i := 0 to (VertexIndices.Count div 3) -1 do begin

                av1 := CalcPlaneNormal( Vertices[ VertexIndices[i *3 +0] ],
                                        Vertices[ VertexIndices[i *3 +1] ],
                                        Vertices[ VertexIndices[i *3 +2] ]  );
                n1 := Normals.FindOrAdd(av1);

                NormalIndices.Add(n1);
                NormalIndices.Add(n1);
                NormalIndices.Add(n1);
              end;
              if VertexIndices.Count <> NormalIndices.Count then begin
                beep;
              end;
            end;
          end;


            // Load TexCoordIndices.
          if mMode2 = 3 then begin
            aStream.Read(TxIndLen[0], SizeOf(Integer)  *TextureCnt);
              // First texture TexCoordIndices.
            if TextureCnt > 0 then begin
              if TxIndLen[0] > 0 then
                if TextureCoordCnt[0] <= High(Byte) +1 then begin
                  SetLength(aIndB, TxIndLen[0]);
                  aStream.Read(aIndB[0], SizeOf(Byte) *TxIndLen[0]);
                  for i := 0 to TxIndLen[0] -1 do
                    TexCoordIndices.Add(aIndB[i]);
                end else if TextureCoordCnt[0] <= High(Word) +1 then begin
                  SetLength(aIndW, TxIndLen[0]);
                  aStream.Read(aIndW[0], SizeOf(Word) *TxIndLen[0]);
                  for i := 0 to TxIndLen[0] -1 do
                    TexCoordIndices.Add(aIndW[i]);
                end else begin
                  SetLength(aIndI, TxIndLen[0]);
                  aStream.Read(aIndI[0], SizeOf(Integer) *TxIndLen[0]);
                  for i := 0 to TxIndLen[0] -1 do
                    TexCoordIndices.Add(aIndI[i]);
                end;
            end;
              // Second texture TexCoordIndices.
            if TextureCnt > 1 then begin
              if TxIndLen[1] > 0 then
                if TextureCoordCnt[1] <= High(Byte) +1 then begin
                  SetLength(aIndB, TxIndLen[1]);
                  aStream.Read(aIndB[0], SizeOf(Byte) *TxIndLen[1]);
                  SetLength(FGrLM[fg1], TxIndLen[1]);
                  for i := 0 to TxIndLen[1] -1 do
                    FGrLM[fg1][i] := aIndB[i];
                end else if TextureCoordCnt[1] <= High(Word) +1 then begin
                  SetLength(aIndW, TxIndLen[1]);
                  aStream.Read(aIndW[0], SizeOf(Word) *TxIndLen[1]);
                  SetLength(FGrLM[fg1], TxIndLen[1]);
                  for i := 0 to TxIndLen[1] -1 do
                    FGrLM[fg1][i] := aIndW[i];
                end else begin
                  SetLength(aIndI, TxIndLen[1]);
                  aStream.Read(aIndI[0], SizeOf(Integer) *TxIndLen[1]);
                  SetLength(FGrLM[fg1], TxIndLen[1]);
                  for i := 0 to TxIndLen[1] -1 do
                    FGrLM[fg1][i] := aIndI[i];
                end;
            end;
          end else begin
            aStream.Read(kCnt, SizeOf(Integer));
            if FgType >= 2 then begin
              if kCnt > 0 then
                if txcrCnt <= High(Byte) +1 then begin
                  SetLength(aIndB, kCnt);
                  aStream.Read(aIndB[0], SizeOf(Byte) *kCnt);
                  for i := 0 to kCnt -1 do
                    TexCoordIndices.Add(aIndB[i]);
                end else if txcrCnt <= High(Word) +1 then begin
                  SetLength(aIndW, kCnt);
                  aStream.Read(aIndW[0], SizeOf(Word) *kCnt);
                  for i := 0 to kCnt -1 do
                    TexCoordIndices.Add(aIndW[i]);
                end else begin
                  SetLength(aIndI, kCnt);
                  aStream.Read(aIndI[0], SizeOf(Integer) *kCnt);
                  for i := 0 to kCnt -1 do
                    TexCoordIndices.Add(aIndI[i]);
                end;
            end else begin
              if kCnt > 0 then
                if txcrCnt <= High(Byte) then begin
                  SetLength(aIndB, kCnt);
                  aStream.Read(aIndB[0], SizeOf(Byte) *kCnt);
                  for i := 0 to kCnt -1 do
                    TexCoordIndices.Add(aIndB[i]);
                end else if txcrCnt <= High(Word) then begin
                  SetLength(aIndW, kCnt);
                  aStream.Read(aIndW[0], SizeOf(Word) *kCnt);
                  for i := 0 to kCnt -1 do
                    TexCoordIndices.Add(aIndW[i]);
                end else begin
                  SetLength(aIndI, kCnt);
                  aStream.Read(aIndI[0], SizeOf(Integer) *kCnt);
                  for i := 0 to kCnt -1 do
                    TexCoordIndices.Add(aIndI[i]);
                end;
            end;
          end;
          if     not isLoadDisabledFaceGroups1  // 12 jul 2007
             and not aGropusEx[fg1].Enabled     // 12 jul 2007
          then Free;                            // 12 jul 2007
        end;
      end;

      if TextureCnt > 1 then begin
        // Convert TFGVertexNormalTexIndexList to TFGVertexIndexList because
        //  TFGVertexNormalTexIndexList doesn't support second texture ( now ).
        VrCnt  := Vertices.Count;
        NrCnt  := Normals.Count;
        Tx1Cnt := TexCoords.Count;
        Tx2Cnt := LightMapTexCoords.Count;
        FgCnt  := FaceGroups.Count;
        tr2 := 0;
        for fgInd := 0 to FgCnt -1 do begin
          fgVNT             := FaceGroups[fgInd] as TFGVertexNormalTexIndexList;
          fgV               := TFGVertexIndexList.CreateOwned(FaceGroups);
          fgV.Mode          := fgmmTriangles;
          fgV.MaterialName  := fgVNT.MaterialName;
          fgV.LightMapIndex := fgVNT.LightMapIndex;
          for tr1 := 0 to fgVNT.VertexIndices.Count -1 do begin
            Vertices.Add(  Vertices[ fgVNT.VertexIndices[tr1]]  );
            Normals.Add(   Normals[  fgVNT.NormalIndices[tr1]]  );
            TexCoords.Add( TexCoords[fgVNT.TexCoordIndices[tr1] ]);
            LightMapTexCoords.Add(LightMapTexCoords[FGrLM[FgInd][tr1]]);
            fgV.VertexIndices.Add(tr2);
            tr2 := tr2 +1;
          end;
        end;
        if VrCnt > 0 then
          Vertices.DeleteItems(         0, VrCnt);
        if NrCnt > 0 then
          Normals.DeleteItems(          0, NrCnt);
        if Tx1Cnt > 0 then
          TexCoords.DeleteItems(        0, Tx1Cnt);
        if Tx2Cnt > 0 then
          LightMapTexCoords.DeleteItems(0, Tx2Cnt);
        for fgInd := 0 to FgCnt -1 do begin
          fgVNT := (FaceGroups[0] as TFGVertexNormalTexIndexList);
          FaceGroups.Delete(0);
          fgVNT.Free;
        end;
      end;

      if     not isLoadDisabledMeshs1 // 12 jul 2007
         and Visible                  // 12 jul 2007
      then Mesh.Free;                 // 12 jul 2007

    end;

  end;
end;



procedure ReadSkeleton1(aStream : TStream; aBaseMesh : TGLBaseMesh
  ; BonesCount    : Integer
  );
type
  TLODSkelSave = (lodSSnone, lodSkSaveV1);

    TSkelBone = record
      Name         : string;
      ID, ParentID : Smallint;
    end;
  TSkelBones = array of TSkelBone;

var
  Bones : TSkelBones;

  procedure WriteBonesToActor(aSkeletone : TGLSkeleton);
  var
    Bone1, BoneParent : TGLSkeletonBone;
    i                 : integer;
  begin
    if aSkeletone.RootBones.Count = 0 then begin
      for i := 0 to Length(Bones) -1 do begin
        if Bones[i].ParentID = -1 then begin
          Bone1 := TGLSkeletonBone.CreateOwned( aSkeletone.RootBones);
        end else begin
          BoneParent := aSkeletone.RootBones.BoneByID(Bones[i].ParentID);
          if Assigned(BoneParent) then begin
            Bone1 := TGLSkeletonBone.CreateOwned( BoneParent );
          end else begin
            Assert(False, 'Error ! Parent bone number ( '
             +IntToStr(Bones[i].ParentID) +' ) is not valid.');
             continue;
          end;
        end;
        if Assigned(Bone1) then begin
          Bone1.Name   := Bones[i].Name;
          Bone1.BoneID := Bones[i].ID;
        end;
      end;
    end else begin
      Assert(False, 'Error ! Skeletone is already exisxts');
    end;
  end;

var
  i, k1     : integer;
  SkelType2 : TLODSkelSave;
begin
  with aStream do begin
    Read(SkelType2, SizeOf(TLODSkelSave));
    if Integer(SkelType2) > Integer(lodSkSaveV1) then begin
      Assert(False, 'Wrong skeletone type. Download a new version of the'
        +' loader from LODka3D site.');
      exit;
    end;
    SetLength(Bones, BonesCount);
    for i := 0 to BonesCount -1 do begin
      Read(Bones[i].ID, SizeOf(SmallInt));
      Read(Bones[i].ParentID, SizeOf(SmallInt));
      Read(k1, SizeOf(Integer));
      SetLength(Bones[i].Name, k1);
      Read(Bones[i].Name[1], k1);
    end;
  end;
  WriteBonesToActor(aBaseMesh.Skeleton);
end;


procedure ReadAnim1(aStream: TStream; aBaseMesh : TGLBaseMesh
  ; anAnimNumber : Integer; aFramesCount : Integer
  ; aBonesCount : Integer
  ; var aLODAnimExData : TLODAnimExData1
  );
type
  TSkelPos2 = record
    Position, Rotation : TAffineVector;
  end;
  TSkelAnim2 = record
    Name  : string;
    Items : array of TSkelPos2;
  end;
  TLODAnimType1 = (lodATnone, lodATSaveV1);
var
  Anim1     : TSkelAnim2;
  AnimType1 : TLODAnimType1;
  ActAnim   : TGLActorAnimation;

  k1, firstFrame, BonesCnt,
  fr1, BoneNumb             : Integer;
  Frame1                    : TGLSkeletonFrame;
begin
  BonesCnt := aBonesCount;
  with aStream do begin
    AnimType1 := lodATSaveV1;
    Read(AnimType1, SizeOf(TLODAnimType1));
    if AnimType1 <> lodATSaveV1 then begin
      Assert(False,
        'Wrong skeleton type. Download a new version of ULODkaUtils file.');
      exit;
    end;

    Read(aLODAnimExData, SizeOf(TLODAnimExData1));

    Read(k1, SizeOf(Integer));
    SetLength(Anim1.Name, k1);
    Read(Anim1.Name[1], k1);

    SetLength(Anim1.Items, aFramesCount *BonesCnt);
    if aFramesCount *BonesCnt > 0 then
      Read(Anim1.Items[0], SizeOf(TAffineVector) *2 *aFramesCount *BonesCnt)
    else begin
      beep;
      exit;
    end;
  end;

  FirstFrame := aBaseMesh.Skeleton.Frames.Count;
  k1 := 0;
  for fr1 := 0 to aFramesCount -1 do begin
    Frame1      := TGLSkeletonFrame.CreateOwned(aBaseMesh.Skeleton.Frames);
    Frame1.Name := 'a' +IntToStr(fr1 +FirstFrame);
    for BoneNumb := 0 to BonesCnt -1 do begin
      Frame1.Position.Add(Anim1.Items[k1].Position);
      Frame1.Rotation.Add(Anim1.Items[k1].Rotation);
      k1 := k1 +1;
    end;
  end;

  if aBaseMesh is TGLActor then begin
    ActAnim            := TGLActor(aBaseMesh).Animations.Add;
    ActAnim.Name       := Anim1.Name;
    ActAnim.Reference  := aarSkeleton;
    ActAnim.StartFrame := FirstFrame;
    ActAnim.EndFrame   := TGLActor(aBaseMesh).Skeleton.Frames.Count -1;
  end;
end;











  // Physics.
function LoadPhysicsFromStream1(aStream : TStream;
  var aLODBodys  : TLOD1Bodys;
  var aLODGeoms  : TLOD1Geoms;
  var aLODJoints : TLOD1Joints
) : Boolean;
type
    // Temp records.
  TSaveLens = packed record
    BnLen, GnLen, MsLen, BdLen, GmLen, JnLen : Integer;
  end;

  TLOD01Body = packed record
    Type1       : TBodyTypeA;
    Position    : TAffineVector;
    Scale       : TAffineVector;
    Rotation    : TQuaternion;
    Axis        : Integer;
    Mass        : Single;
    MassDensity : Single;
    TagFloat    : Single;
    Enabled     : Boolean;

    isGravity        : Boolean;
    isFiniteRot      : Boolean;
    isAutoDisable    : Boolean;
    isAutoDisableDef : Boolean;
    ADLinearThr      : Single;
    ADAngularThr     : Single;
    ADisableTime     : Single;
    ADSteps          : Integer;
  end;
  TLOD01Bodys = array of TLOD01Body;

  TLOD01Geom = packed record
    Type1       : TBodyTypeA;
    BodyIndex   : Integer;
    Position    : TAffineVector;
    Scale       : TAffineVector;
    Rotation    : TQuaternion;
    Axis        : Integer;
    Mass        : Single;
    MassDensity : Single;
    MeshLink,
    MeshCount   : Integer;
    TagFloat    : Single;
    Enabled     : Boolean;
  end;
  TLOD01Geoms = array of TLOD01Geom;

  TLOD01BodyGeomsName = string;

  TLOD1MeshLinksList = array of Integer;

    procedure MakePhysRecs(
      Bodys01           : TLOD01Bodys;
      Geoms01           : TLOD01Geoms;
      Joints01          : TLOD1Joints;

      BodysNames01      : TLOD01BodyGeomsName;
      GeomsNames01      : TLOD01BodyGeomsName;
      MeshLinks01       : TLOD1MeshLinksList;

      var aLOD1Bodys02  : TLOD1Bodys;
      var aLOD1Geoms02  : TLOD1Geoms;
      var aLOD1Joints02 : TLOD1Joints
      );
    var
      i, b1, g1, j1, k1, j2  : Integer;
      BNamesList, GNamesList : TStringList;
    begin
      b1 := Length(Bodys01);
      SetLength(aLOD1Bodys02,  b1);
      g1 := Length(Geoms01);
      SetLength(aLOD1Geoms02,  g1);
      j1 := Length(Joints01);
      SetLength(aLOD1Joints02, j1);

      BNamesList := TStringList.Create;
      GNamesList := TStringList.Create;
      try
        BNamesList.Text := BodysNames01;
        GNamesList.Text := GeomsNames01;

        for i := 0 to b1 -1 do begin
          aLOD1Bodys02[i].Type1 := Bodys01[i].Type1;
          if i <= BNamesList.Count -1
            then aLOD1Bodys02[i].Name := BNamesList[i]
            else aLOD1Bodys02[i].Name := '';
          aLOD1Bodys02[i].Position    := Bodys01[i].Position;
          aLOD1Bodys02[i].Scale       := Bodys01[i].Scale;
          aLOD1Bodys02[i].Rotation    := Bodys01[i].Rotation;
          aLOD1Bodys02[i].Axis        := Bodys01[i].Axis;
          aLOD1Bodys02[i].Mass        := Bodys01[i].Mass;
          aLOD1Bodys02[i].MassDensity := Bodys01[i].MassDensity;
          aLOD1Bodys02[i].TagFloat    := Bodys01[i].TagFloat;
          aLOD1Bodys02[i].Enabled     := Bodys01[i].Enabled;

          aLOD1Bodys02[i].isGravity        := Bodys01[i].isGravity;
          aLOD1Bodys02[i].isFiniteRot      := Bodys01[i].isFiniteRot;
          aLOD1Bodys02[i].isAutoDisable    := Bodys01[i].isAutoDisable;
          aLOD1Bodys02[i].isAutoDisableDef := Bodys01[i].isAutoDisableDef;
          aLOD1Bodys02[i].ADLinearThr      := Bodys01[i].ADLinearThr;
          aLOD1Bodys02[i].ADAngularThr     := Bodys01[i].ADAngularThr;
          aLOD1Bodys02[i].ADisableTime     := Bodys01[i].ADisableTime;
          aLOD1Bodys02[i].ADSteps          := Bodys01[i].ADSteps;
        end;

        for i := 0 to g1 -1 do begin
          aLOD1Geoms02[i].Type1 := Geoms01[i].Type1;
          if i <= GNamesList.Count -1
            then aLOD1Geoms02[i].Name := GNamesList[i]
            else aLOD1Geoms02[i].Name := '';
          aLOD1Geoms02[i].BodyIndex   := Geoms01[i].BodyIndex;
          aLOD1Geoms02[i].Position    := Geoms01[i].Position;
          aLOD1Geoms02[i].Scale       := Geoms01[i].Scale;
          aLOD1Geoms02[i].Rotation    := Geoms01[i].Rotation;
          aLOD1Geoms02[i].Axis        := Geoms01[i].Axis;
          aLOD1Geoms02[i].Mass        := Geoms01[i].Mass;
          aLOD1Geoms02[i].MassDensity := Geoms01[i].MassDensity;
          aLOD1Geoms02[i].TagFloat    := Geoms01[i].TagFloat;
          aLOD1Geoms02[i].Enabled     := Geoms01[i].Enabled;
          SetLength(aLOD1Geoms02[i].MeshIndex, 0);
          for j2 := 0 to Geoms01[i].MeshCount -1 do
            if InRange(Geoms01[i].MeshLink +j2, 0, Length(MeshLinks01) -1) then begin
              k1 := Length(aLOD1Geoms02[i].MeshIndex);
              SetLength(aLOD1Geoms02[i].MeshIndex, k1 +1);
              aLOD1Geoms02[i].MeshIndex[k1] := MeshLinks01[ Geoms01[i].MeshLink + j2];
            end;
        end;

        for i := 0 to j1 -1 do begin
          aLOD1Joints02[i].JointTypeA := Joints01[i].JointTypeA;
          for k1 := 0 to 1 do begin
            aLOD1Joints02[i].BodyIndex[k1] := Joints01[i].BodyIndex[k1];
            aLOD1Joints02[i].Axis[k1]      := Joints01[i].Axis[k1];
          end;
          aLOD1Joints02[i].Position            := Joints01[i].Position;
          aLOD1Joints02[i].dParamLoStop        := Joints01[i].dParamLoStop;
          aLOD1Joints02[i].dParamHiStop        := Joints01[i].dParamHiStop;
          aLOD1Joints02[i].dParamVel           := Joints01[i].dParamVel;
          aLOD1Joints02[i].dParamFMax          := Joints01[i].dParamFMax;
          aLOD1Joints02[i].dParamFudgeFactor   := Joints01[i].dParamFudgeFactor;
          aLOD1Joints02[i].dParamBounce        := Joints01[i].dParamBounce;
          aLOD1Joints02[i].dParamCFM           := Joints01[i].dParamCFM;
          aLOD1Joints02[i].dParamStopERP       := Joints01[i].dParamStopERP;
          aLOD1Joints02[i].dParamStopCFM       := Joints01[i].dParamStopCFM;
          aLOD1Joints02[i].dParamSuspensionERP := Joints01[i].dParamSuspensionERP;
          aLOD1Joints02[i].dParamSuspensionCFM := Joints01[i].dParamSuspensionCFM;
          aLOD1Joints02[i].TagFloat            := Joints01[i].TagFloat;
        end;

      finally
        BNamesList.Free;
        GNamesList.Free;
      end;
    end;

var
  SaveType1   : Byte;
  Bodys1      : TLOD01Bodys;
  Geoms1      : TLOD01Geoms;
  Joints1     : TLOD1Joints;
  BodysNames1 : TLOD01BodyGeomsName;
  GeomsNames1 : TLOD01BodyGeomsName;
  MeshLinks1  : TLOD1MeshLinksList;
  Lens1       : TSaveLens;
begin
  with aStream do begin
    try
        // Type.
      Read(SaveType1, SizeOf(SaveType1));
      if SaveType1 <> 1 then begin
        Result := False;
        exit;
      end;

      Read(Lens1, SizeOf(TSaveLens));
      SetLength(BodysNames1, Lens1.BnLen);
      SetLength(GeomsNames1, Lens1.GnLen);
      SetLength(MeshLinks1,  Lens1.MsLen);
      SetLength(Bodys1,      Lens1.BdLen);
      SetLength(Geoms1,      Lens1.GmLen);
      SetLength(Joints1,     Lens1.JnLen);
      if Lens1.BnLen > 0 then
        Read(BodysNames1[1], Lens1.BnLen);
      if Lens1.GnLen > 0 then
        Read(GeomsNames1[1], Lens1.GnLen);
      if Lens1.MsLen > 0 then
        Read(MeshLinks1[0],  Lens1.MsLen *SizeOf(Integer));
      if Lens1.BdLen > 0 then
        Read(Bodys1[0],  Lens1.BdLen *SizeOf(TLOD01Body));
      if Lens1.GmLen > 0 then
        Read(Geoms1[0],  Lens1.GmLen *SizeOf(TLOD01Geom));
      if Lens1.JnLen > 0 then
        Read(Joints1[0], Lens1.JnLen *SizeOf(TLOD1Joint));
      MakePhysRecs(
        Bodys1, Geoms1, Joints1,
        BodysNames1, GeomsNames1, MeshLinks1,
        aLODBodys, aLODGeoms, aLODJoints );
      Result := True;
    except
      Result := False;
    end;
  end;
end;


  // Read one light scheme from stream.
function LoadLightInSceneFromStream(aStream: TStream;
  var LightOptAs1: TLightOptAs): Boolean;
var
  k1        : Integer;
  LODChunk1 : TLODChunk;
begin
  try
    with aStream do begin
      Read(LODChunk1, SizeOf(TLODChunk));
      Result :=    (CompareText(LODChunk1.ID, LightSceneSign) = 0)
                and (LODChunk1.Count = 8);
      if not Result then begin
        Assert(False, 'Wrong light scheme LOD-file format !');
        exit;
      end;

        // Read 1) Name length,  2) Name,  3) 8 Items.
      Read(k1, SizeOf(Integer));
      SetLength(LightOptAs1.Name, k1);
      Read(LightOptAs1.Name[1], k1);
      Read(LightOptAs1.Items[0], SizeOf(TLightOptA) *Length(LightOptAs1.Items));
      Result := True;
    end;
  except
    Result := False;
  end;
end;


  // Set data ftom TLightOptA to TGLLightSource
procedure  SetDataToLigthSource(var aLigthSource : TGLLightSource;
  aLightOptA : TLightOptA);
begin
  aLigthSource.Shining              := aLightOptA.Enabled;
  aLigthSource.Ambient.Color        := aLightOptA.Ambient;
  aLigthSource.Diffuse.Color        := aLightOptA.Diffuse;
  aLigthSource.Specular.Color       := aLightOptA.Specular;
  aLigthSource.Position.SetPoint(aLightOptA.Position);
  aLigthSource.SpotDirection.SetVector(aLightOptA.Direction);
  aLigthSource.SpotCutOff           := aLightOptA.SpotCutOff;
  aLigthSource.SpotExponent         := aLightOptA.SpotExponent;
  aLigthSource.ConstAttenuation     := aLightOptA.ConstAtt;
  aLigthSource.LinearAttenuation    := aLightOptA.LinearAtt;
  aLigthSource.QuadraticAttenuation := aLightOptA.QuadAtt;
  case Integer(aLightOptA.LigthStyle) of
    0 : aLigthSource.LightStyle     := lsSpot;
    1 : aLigthSource.LightStyle     := lsOmni;
    2 : aLigthSource.LightStyle     := lsParallel;
  else
    aLigthSource.LightStyle         := lsSpot;
  end;
end;









  // Load Colliders From Stream.
function LoadCollidersFromStream(aStream : TStream;
  var   aSkelColliders  : TSkelColliders;
  const aCollidersCount : Integer): Boolean;
type
  TCollidersInfoRec1 = packed record
    BoneID       : SmallInt;
    SkelCollType : TSkelCollTypeA;
    Matrix       : TMatrix4f;
    Tag          : Integer;
    TagFloat     : Single;
    Visible      : Boolean;
    JointInfo    : array [0..1] of TSkelColJointInfo;
  end;
  {
  TCollidersInfoRec1 = packed record
    BoneID       : SmallInt;
    SkelCollType : TSkelCollTypeA;
    Matrix       : TMatrix;
    Tag          : Integer;
    Visible      : Boolean;
  end;
  }
var
  namesLen, n1, i,
  nameListLen, colInd, nameLen, BoneNameLen : Integer;
  M1         : TMatrix4f;
    // Colliders names length.
  ColNameLen : array of Integer;
  ColNames   : string;
  SaveRec1   : array of TCollidersInfoRec1;
begin
  Result := False;
  if aStream = nil then exit;
  with aStream do
    try
        // Length of name of colliders list and name of colliders list.
      Read(nameListLen, SizeOf(Integer));
      SetLength(aSkelColliders.Name, nameListLen);
      if nameListLen > 0 then
        Read(aSkelColliders.Name[1], nameListLen);

      if aCollidersCount = 0 then begin
        Result := True;
        exit;
      end;

        // List of names length.
      SetLength(ColNameLen, aCollidersCount);
      if aCollidersCount > 0 then
        Read(ColNameLen[0], SizeOf(Integer) *aCollidersCount);

        // Length of collider names line.
      namesLen := 0;
      for colInd := 0 to aCollidersCount -1 do
        namesLen := namesLen +ColNameLen[colInd];

        // Read colliders names as one line.
      SetLength(ColNames, namesLen);
      Read(ColNames[1], namesLen);

        // Read colliders data to TCollidersInfoRec1.
      SetLength(SaveRec1, aCollidersCount);
      Read(SaveRec1[0], SizeOf(TCollidersInfoRec1) *aCollidersCount);

        // Set number of colliders.
      SetLength(aSkelColliders.Items, aCollidersCount);

        // Copy data from SaveRec1 to aSkelColliders.
      n1 := 1;
      for colInd := 0 to aCollidersCount -1 do begin
        aSkelColliders.Items[colInd].Name := Copy(ColNames, n1, ColNameLen[colInd]);
        n1 := n1 +ColNameLen[colInd];
        aSkelColliders.Items[colInd].BoneID       := SaveRec1[colInd].BoneID;
        aSkelColliders.Items[colInd].SkelCollType := SaveRec1[colInd].SkelCollType;
        aSkelColliders.Items[colInd].Matrix       := SaveRec1[colInd].Matrix;
        aSkelColliders.Items[colInd].Tag          := SaveRec1[colInd].Tag;
        aSkelColliders.Items[colInd].Visible      := SaveRec1[colInd].Visible;

        aSkelColliders.Items[colInd].TagFloat     := SaveRec1[colInd].TagFloat;

          // Axeses.
        CopyMemory(@aSkelColliders.Items[colInd].JointInfo[0],
          @SaveRec1[colInd].JointInfo[0], SizeOf(TSkelColJointInfo) *2);

          // Calc Scale.
          // (needs test)
        for i := 0 to 2 do begin
          aSkelColliders.Items[colInd].Scale.v[i]     := aSkelColliders.Items[colInd].Matrix.v[i].v[3];
          aSkelColliders.Items[colInd].Matrix.v[i].v[3] := 0;
        end;
      end;
      Result := True;
    except
      Result := False;
    end;
end;


  // Copy colliders from aSource to aDest.
procedure CopyColliders(var aDest : TSkelColliders; const aSource :TSkelColliders);
var
  colInd2, colLen2 : Integer;
begin
  aDest.Name := aSource.Name;
  colLen2 := Length(aSource.Items);
  SetLength(aDest.Items, colLen2);
  for colInd2 := 0 to colLen2 -1 do begin
    aDest.Items[colInd2].Name         := aSource.Items[colInd2].Name;
    aDest.Items[colInd2].BoneID       := aSource.Items[colInd2].BoneID;
    aDest.Items[colInd2].SkelCollType := aSource.Items[colInd2].SkelCollType;
    aDest.Items[colInd2].Matrix       := aSource.Items[colInd2].Matrix;
    aDest.Items[colInd2].Scale        := aSource.Items[colInd2].Scale;
    aDest.Items[colInd2].Tag          := aSource.Items[colInd2].Tag;
    aDest.Items[colInd2].TagFloat     := aSource.Items[colInd2].TagFloat;
    aDest.Items[colInd2].Visible      := aSource.Items[colInd2].Visible;
    CopyMemory(@aDest.Items[  colInd2].JointInfo[0],
               @aSource.Items[colInd2].JointInfo[0],
               SizeOf(TSkelColJointInfo) *2);
  end;
end;


  // Copy colliders list from aSource to aDest.
procedure CopyCollidersList(var aDest : TSkelCollidersList;
  const aSource :TSkelCollidersList);
var
  colInd, colLen : Integer;
begin
  colLen := Length(aSource);
  SetLength(aDest, colLen);
  for colInd := 0 to colLen -1 do
    CopyColliders(aDest[colInd], aSource[colInd]);
end;

  // Append colliders list from aSource to aDest.
procedure AppendCollidersList(var aDest : TSkelCollidersList;
  const aSource :TSkelCollidersList);
var
  StartInd, colInd, colLen : Integer;
begin
  StartInd := Length(aDest);
  colLen   := Length(aSource);
  SetLength(aDest, StartInd +colLen);
  for colInd := 0 to colLen -1 do
    CopyColliders(aDest[StartInd +colInd], aSource[colInd]);
end;


  // Copy physics Body arrays.
function CopyPhysBodyArrays(
    var   aDestLODBodys   : TLOD1Bodys;
    const aSourceLODBodys : TLOD1Bodys) : Boolean;
var
  bdInd, bdCnt : Integer;
begin
  bdCnt := Length(aSourceLODBodys);
  SetLength(aDestLODBodys, bdCnt);
  for bdInd := 0 to bdCnt -1 do begin
    aDestLODBodys[bdInd].Type1         := aSourceLODBodys[bdInd].Type1;
    aDestLODBodys[bdInd].Name          := aSourceLODBodys[bdInd].Name;
    aDestLODBodys[bdInd].Position      := aSourceLODBodys[bdInd].Position;
    aDestLODBodys[bdInd].Scale         := aSourceLODBodys[bdInd].Scale;
    aDestLODBodys[bdInd].Rotation      := aSourceLODBodys[bdInd].Rotation;
    aDestLODBodys[bdInd].Axis          := aSourceLODBodys[bdInd].Axis;
    aDestLODBodys[bdInd].Mass          := aSourceLODBodys[bdInd].Mass;
    aDestLODBodys[bdInd].MassDensity   := aSourceLODBodys[bdInd].MassDensity;
    aDestLODBodys[bdInd].TagFloat      := aSourceLODBodys[bdInd].TagFloat;
    aDestLODBodys[bdInd].Enabled       := aSourceLODBodys[bdInd].Enabled;
    aDestLODBodys[bdInd].isGravity     := aSourceLODBodys[bdInd].isGravity;
    aDestLODBodys[bdInd].isFiniteRot   := aSourceLODBodys[bdInd].isFiniteRot;
    aDestLODBodys[bdInd].isAutoDisable := aSourceLODBodys[bdInd].isAutoDisable;
    aDestLODBodys[bdInd].isAutoDisableDef := aSourceLODBodys[bdInd].isAutoDisableDef;
    aDestLODBodys[bdInd].ADLinearThr   := aSourceLODBodys[bdInd].ADLinearThr;
    aDestLODBodys[bdInd].ADAngularThr  := aSourceLODBodys[bdInd].ADAngularThr;
    aDestLODBodys[bdInd].ADisableTime  := aSourceLODBodys[bdInd].ADisableTime;
    aDestLODBodys[bdInd].ADSteps       := aSourceLODBodys[bdInd].ADSteps;
  end;
end;

  // Copy physics Geoms arrays.
function CopyPhysGeomArrays(
    var   aDestLODGeoms   : TLOD1Geoms;
    const aSourceLODGeoms : TLOD1Geoms) : Boolean;
var
  gmInd, gmCnt, msInd, msCnt : Integer;
begin
  gmCnt := Length(aSourceLODGeoms);
  SetLength(aDestLODGeoms, gmCnt);
  for gmInd := 0 to gmCnt -1 do begin
    aDestLODGeoms[gmInd].Type1       := aSourceLODGeoms[gmInd].Type1;
    aDestLODGeoms[gmInd].Name        := aSourceLODGeoms[gmInd].Name;
    aDestLODGeoms[gmInd].BodyIndex   := aSourceLODGeoms[gmInd].BodyIndex;
    aDestLODGeoms[gmInd].Position    := aSourceLODGeoms[gmInd].Position;
    aDestLODGeoms[gmInd].Scale       := aSourceLODGeoms[gmInd].Scale;
    aDestLODGeoms[gmInd].Rotation    := aSourceLODGeoms[gmInd].Rotation;
    aDestLODGeoms[gmInd].Axis        := aSourceLODGeoms[gmInd].Axis;
    aDestLODGeoms[gmInd].Mass        := aSourceLODGeoms[gmInd].Mass;
    aDestLODGeoms[gmInd].MassDensity := aSourceLODGeoms[gmInd].MassDensity;
    aDestLODGeoms[gmInd].TagFloat    := aSourceLODGeoms[gmInd].TagFloat;
    msCnt := Length(aSourceLODGeoms[gmInd].MeshIndex);
    SetLength(aDestLODGeoms[gmInd].MeshIndex, msCnt);
    for msInd := 0 to msCnt -1 do
      aDestLODGeoms[gmInd].MeshIndex[msInd] := aSourceLODGeoms[gmInd].MeshIndex[msInd];
    aDestLODGeoms[gmInd].Enabled := aSourceLODGeoms[gmInd].Enabled;
  end;
end;

  // Copy physics Joints arrays.
function CopyPhysJointsArrays(
  var   aDestLODJoints   : TLOD1Joints;
  const aSourceLODJoints : TLOD1Joints) : Boolean;
var
  jnCnt : Integer;
begin
  jnCnt := Length(aSourceLODJoints);
  SetLength(aDestLODJoints, jnCnt);
  if jnCnt > 0
    then CopyMemory(@aDestLODJoints[0], @aSourceLODJoints[0],
                    SizeOf(TLOD1Joint) *jnCnt);
end;

end.