//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLVectorFileObjects<p>

	Vector File related objects for GLScene<p>

  <b>History :</b><font size=-1><ul>
    <li>19/12/04 - PhP - Added capabilities function
    <li>28/10/03 - SG - Partly implemented skeletal animation,
                        asynchronous animations will fail however.
    <li>03/06/03 - EG - Added header, now self-registers
	</ul></font>
}
unit GLFileMS3D;

interface

uses
  Classes, SysUtils, GLVectorFileObjects,  VectorTypes, GLTexture, VectorGeometry,
  VectorLists, ApplicationFileIO;

type
  // TGLMS3DVectorFile
  //
  {: The MilkShape vector file.<p>
     By Mattias Fagerlund, mattias@cambrianlabs.com. Yada yada. Eric rules! }
  TGLMS3DVectorFile = class(TVectorFile)
  public
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream : TStream); override;
  end;

implementation

uses
  TypesMS3D;

{ TGLMS3DVectorFile }

// capabilities
//
class function TGLMS3DVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

// loadfromstream
//
procedure TGLMS3DVectorFile.LoadFromStream(aStream: TStream);
var
  // GLScene
  i, j, k, itemp: integer;
  wtemp : word;
  NormalID : integer;
  TexCoordID : integer;
  MO : TMeshObject;
  FaceGroup : TFGVertexNormalTexIndexList;

  GroupList : TList;
  GLLibMaterial : TGLLibMaterial;

  // Milkshape 3d
  ms3d_header : TMS3DHeader;
  nNumVertices : word;

  ms3d_vertices : PMS3DVertexArray;
  nNumTriangles : word;

  ms3d_triangle : TMS3DTriangle;
  ms3d_triangles : PMS3DTriangleArray;

  nNumGroups : word;
  Group : TMS3DGroup;
  nNumMaterials : word;
  ms3d_material : TMS3DMaterial;

  fAnimationFPS : single;
  fCurrentTime : single;
  iTotalFrames : integer;

  nNumJoints : word;
  ms3d_joints : PMS3DJointArray;

  bonelist : TStringList;
  bone : TSkeletonBone;
  frame : TSkeletonFrame;
  rot, pos : TVector3f;

  procedure AddFaceVertex(ID: integer);
  begin
    // Add the normal to the normals list
    NormalID := MO.Normals.Add(ms3d_triangle.vertexNormals[ID].v);
    // Add the texCoord
    TexCoordID := MO.TexCoords.Add(ms3d_triangle.s[ID], -ms3d_triangle.t[ID]);
    // Add the vertex to the vertex list
    FaceGroup.Add(ms3d_triangle.vertexIndices[ID], NormalID, TexCoordID);
  end;

  function AddRotations(rot, baserot : TAffineVector) : TAffineVector;
  var
    mat1,mat2,rmat : TMatrix;
    s,c : Single;
    Trans : TTransformations;
  begin
    mat1:=IdentityHMGMatrix;
    mat2:=IdentityHMGMatrix;

    SinCos(rot[0],s,c);
    rmat:=CreateRotationMatrixX(s,c);
    mat1:=MatrixMultiply(mat1,rmat);
    SinCos(rot[1],s,c);
    rmat:=CreateRotationMatrixY(s,c);
    mat1:=MatrixMultiply(mat1,rmat);
    SinCos(rot[2],s,c);
    rmat:=CreateRotationMatrixZ(s,c);
    mat1:=MatrixMultiply(mat1,rmat);

    SinCos(baserot[0],s,c);
    rmat:=CreateRotationMatrixX(s,c);
    mat2:=MatrixMultiply(mat2,rmat);
    SinCos(baserot[1],s,c);
    rmat:=CreateRotationMatrixY(s,c);
    mat2:=MatrixMultiply(mat2,rmat);
    SinCos(baserot[2],s,c);
    rmat:=CreateRotationMatrixZ(s,c);
    mat2:=MatrixMultiply(mat2,rmat);

    mat1:=MatrixMultiply(mat1,mat2);
    if MatrixDecompose(mat1,Trans) then
      SetVector(Result,Trans[ttRotateX],Trans[ttRotateY],Trans[ttRotateZ])
    else
      Result:=NullVector;
  end;

begin
  GroupList := TList.Create;
  FaceGroup := nil;
  ms3d_vertices := nil;
  ms3d_triangles := nil;
  ms3d_joints := nil;

  try
    // First comes the header.
    aStream.ReadBuffer(ms3d_header, sizeof(TMS3DHeader));

    Assert(ms3d_header.version = 4, Format('The MilkShape3D importer can only handle MS3D files of version 4, this is version ', [ms3d_header.id]));

    // Then comes the number of vertices
    aStream.ReadBuffer(nNumVertices, sizeof(nNumVertices));

    // Create the vertex list
    if Owner is TGLActor then begin
      MO := TSkeletonMeshObject.CreateOwned(Owner.MeshObjects);
      TSkeletonMeshObject(MO).BonesPerVertex:=1;
    end else
      MO := TMeshObject.CreateOwned(Owner.MeshObjects);
    MO.Mode := momFaceGroups;

    // Then comes nNumVertices * sizeof (ms3d_vertex_t)

    ms3d_vertices := AllocMem(sizeof(TMS3DVertex) * nNumVertices);
    aStream.ReadBuffer(ms3d_vertices^, sizeof(TMS3DVertex) * nNumVertices);

    for i := 0 to nNumVertices - 1 do
      with ms3d_vertices[i] do  begin
        // Add the vertex to the vertexlist
        MO.Vertices.Add(vertex.v);
        if Owner is TGLActor then
          TSkeletonMeshObject(MO).AddWeightedBone(Byte(BoneID),1);
      end;

    // number of triangles
    aStream.ReadBuffer(nNumTriangles, sizeof(nNumTriangles));

    // nNumTriangles * sizeof (ms3d_triangle_t)
    ms3d_triangles := AllocMem(sizeof(TMS3DTriangle) * nNumTriangles);
    aStream.ReadBuffer(ms3d_triangles^, sizeof(TMS3DTriangle) * nNumTriangles);

    // Don't do anything with the triangles / faces just yet

    // number of groups
    aStream.ReadBuffer(nNumGroups, sizeof(nNumGroups));

    // nNumGroups * sizeof (ms3d_group_t)
    for i := 0 to nNumGroups - 1 do
    begin
      // Read the first part of the group
      Group := TMS3DGroup.Create;
      GroupList.Add(Group);
      aStream.ReadBuffer(Group.Flags, sizeof(Group.Flags));
      aStream.ReadBuffer(Group.name, sizeof(Group.name));
      aStream.ReadBuffer(Group.numtriangles, sizeof(Group.numtriangles));


      for j := 0 to Group.numtriangles - 1 do
      begin
        aStream.ReadBuffer(wtemp, sizeof(wtemp));
        itemp := wtemp;
        Group.triangleIndices.Add(pointer(itemp));
      end;

      aStream.ReadBuffer(Group.materialIndex, sizeof(Group.materialIndex));

      // if materialindex=-1, then there is no material, and all faces should
      // be added to a base VIL
      if Group.materialIndex = char(-1) then
      begin
        // If there's no base VIL, create one!
        if FaceGroup = nil then
          FaceGroup := TFGVertexNormalTexIndexList.CreateOwned(MO.FaceGroups);

        for j := 0 to Group.numtriangles - 1 do
        begin
          ms3d_triangle := ms3d_triangles[integer(Group.triangleIndices[j])];

          AddFaceVertex(0);
          AddFaceVertex(1);
          AddFaceVertex(2);
        end;
      end;
    end;
    // number of materials
    aStream.ReadBuffer(nNumMaterials, sizeof(nNumMaterials));
    // nNumMaterials * sizeof (ms3d_material_t)
    for i := 0 to nNumMaterials-1 do
    begin
      aStream.ReadBuffer(ms3d_material, sizeof(TMS3DMaterial));

      // Create the material, if there's a materiallibrary!
      if Assigned(Owner.MaterialLibrary) then
      begin
        if FileExists(ms3d_material.texture) then
          GLLibMaterial := Owner.MaterialLibrary.AddTextureMaterial(ms3d_material.name, ms3d_material.texture)
        else begin
          if not Owner.IgnoreMissingTextures then
            Exception.Create('Texture file not found: '+ms3d_material.texture);
          GLLibMaterial := Owner.MaterialLibrary.Materials.Add;
          GLLibMaterial.Name := ms3d_material.name;
        end;
        GLLibMaterial.Material.FrontProperties.Emission.Color := ms3d_material.emissive;
        GLLibMaterial.Material.FrontProperties.Ambient.Color := ms3d_material.ambient;
        GLLibMaterial.Material.FrontProperties.Diffuse.Color := ms3d_material.diffuse;
        GLLibMaterial.Material.FrontProperties.Specular.Color := ms3d_material.specular;

        // Shinintess is 0 to 128 in both MS3D and GLScene. Why not 0 to 127? Odd.
        GLLibMaterial.Material.FrontProperties.Shininess := round(ms3d_material.shininess);

        // ms3d_material.transparency is allready set as alpha channel on all
        // colors above
        if ms3d_material.transparency<1 then
          GLLibMaterial.Material.BlendingMode := bmTransparency;//}

        // Create a new face group and add all triangles for this material
        // here. We must cycle through all groups that have this material
        FaceGroup := TFGVertexNormalTexIndexList.CreateOwned(MO.FaceGroups);
        FaceGroup.MaterialName := GLLibMaterial.Name;//}

        for j := 0 to GroupList.Count-1 do
        begin
          Group := TMS3DGroup(GroupList[j]);
          if Group.materialIndex = char(i) then
            for k := 0 to Group.numtriangles - 1 do
            begin
              ms3d_triangle := ms3d_triangles[integer(Group.triangleIndices[k])];

              AddFaceVertex(0);
              AddFaceVertex(1);
              AddFaceVertex(2);
            end;
        end;
      end;
    end;

    // save some keyframer data
    aStream.ReadBuffer(fAnimationFPS, sizeof(fAnimationFPS));
    aStream.ReadBuffer(fCurrentTime, sizeof(fCurrentTime));
    aStream.ReadBuffer(iTotalFrames, sizeof(iTotalFrames));

    // NOTE NOTE NOTE!
    // From here on, the info isn't used at all, it's only read so that a future
    // enhancement of the loader can also use animations!

    // number of joints
    aStream.ReadBuffer(nNumJoints, sizeof(nNumJoints));

    // nNumJoints * sizeof (ms3d_joint_t)
    ms3d_joints := AllocMem(sizeof(TMS3DJoint) * nNumJoints);

    // We have to read the joints one by one!
    for i := 0 to nNumJoints-1 do
    begin
      // Read the joint base
      aStream.ReadBuffer(ms3d_joints[i].Base, sizeof(TMS3DJointBase));

      if ms3d_joints[i].base.numKeyFramesRot>0 then
      begin
        //     ms3d_keyframe_rot_t keyFramesRot[numKeyFramesRot];      // local animation matrices
        // Allocate memory for the rotations
        ms3d_joints[i].keyFramesRot := AllocMem(sizeof(TMS3DKeyframeRotation) * ms3d_joints[i].base.numKeyFramesRot);

        // Read the rotations
        aStream.ReadBuffer(ms3d_joints[i].keyFramesRot^, sizeof(TMS3DKeyframeRotation) * ms3d_joints[i].base.numKeyFramesRot);
      end else
        ms3d_joints[i].keyFramesRot := nil;

      if ms3d_joints[i].base.numKeyFramesTrans>0 then
      begin
        //     ms3d_keyframe_pos_t keyFramesTrans[numKeyFramesTrans];  // local animation matrices
        // Allocate memory for the translations
        ms3d_joints[i].keyFramesTrans := AllocMem(sizeof(TMS3DKeyframePosition) * ms3d_joints[i].base.numKeyFramesTrans);

        // Read the translations
        aStream.ReadBuffer(ms3d_joints[i].keyFramesTrans^, sizeof(TMS3DKeyframePosition) * ms3d_joints[i].base.numKeyFramesTrans);
      end else
        ms3d_joints[i].keyFramesTrans := nil;
    end;

    // Right here, it's time to use the joints - this from the ms3d spec;

    // ***
    // Mesh Transformation:
    //
    // 0. Build the transformation matrices from the rotation and position
    // 1. Multiply the vertices by the inverse of local reference matrix (lmatrix0)
    // 2. then translate the result by (lmatrix0 * keyFramesTrans)
    // 3. then multiply the result by (lmatrix0 * keyFramesRot)
    //
    // For normals skip step 2.
    //
    //
    //
    // NOTE:  this file format may change in future versions!
    //
    //
    // - Mete Ciragan
    // ***

    if (Owner is TGLActor) and (nNumJoints>0) then
    begin

      // Bone names are added to a list initally to sort out parents
      bonelist:=TStringList.Create;
      for i := 0 to nNumJoints-1 do
        bonelist.Add(ms3d_joints[i].Base.Name);

      // Find parent bones and add their children
      for i := 0 to nNumJoints-1 do begin
        j:=bonelist.IndexOf(ms3d_joints[i].Base.ParentName);
        if j = -1 then
          bone:=TSkeletonBone.CreateOwned(Owner.Skeleton.RootBones)
        else
          bone:=TSkeletonBone.CreateOwned(Owner.Skeleton.RootBones.BoneByID(j));
        bone.Name:=ms3d_joints[i].Base.Name;
        bone.BoneID:=i;
      end;
      bonelist.Free;

      // Set up the base pose
      frame:=TSkeletonFrame.CreateOwned(Owner.Skeleton.Frames);
      for i := 0 to nNumJoints-1 do
      begin
        pos:=ms3d_joints[i].Base.Position.V;
        rot:=ms3d_joints[i].Base.Rotation.V;
        frame.Position.Add(pos);
        frame.Rotation.Add(rot);
      end;

      // Now load the animations
      for i:=0 to nNumJoints-1 do
      begin
        if ms3d_joints[i].Base.NumKeyFramesTrans = ms3d_joints[i].Base.NumKeyFramesRot then
        begin
          for j:=0 to ms3d_joints[i].Base.NumKeyFramesTrans-1 do
          begin
            if (j+1) = Owner.Skeleton.Frames.Count then
              frame:=TSkeletonFrame.CreateOwned(Owner.Skeleton.Frames)
            else
              frame:=Owner.Skeleton.Frames[j+1];

            // Set rootbone values to base pose
            if ms3d_joints[i].Base.ParentName = '' then begin
              pos:=ms3d_joints[i].Base.Position.V;
              rot:=ms3d_joints[i].Base.Rotation.V;
            end else begin
              pos:=ms3d_joints[i].KeyFramesTrans[j].Position.V;
              AddVector(pos,ms3d_joints[i].Base.Position.V);
              rot:=ms3d_joints[i].KeyFramesRot[j].Rotation.V;
              rot:=AddRotations(rot,ms3d_joints[i].Base.Rotation.V);
            end;
            frame.Position.Add(pos);
            frame.Rotation.Add(rot);
          end;
        end;
      end;

      Owner.Skeleton.RootBones.PrepareGlobalMatrices;
      TSkeletonMeshObject(MO).PrepareBoneMatrixInvertedMeshes;
    end;

  finally
    if Assigned(ms3d_vertices) then
      FreeMem(ms3d_vertices);

    if Assigned(ms3d_triangles) then
      FreeMem(ms3d_triangles);

    if Assigned(ms3d_joints) then
    begin
      // Free the internal storage of the joint
      for i := 0 to nNumJoints-1 do
      begin
        if Assigned(ms3d_joints[i].keyFramesRot) then
          FreeMem(ms3d_joints[i].keyFramesRot);

        if Assigned(ms3d_joints[i].keyFramesTrans) then
          FreeMem(ms3d_joints[i].keyFramesTrans);
      end;

      FreeMem(ms3d_joints);
    end;

    // Finalize
    for i := 0 to GroupList.Count-1 do
      TMS3DGroup(GroupList[i]).Free;

    GroupList.Free;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('ms3d', 'MilkShape3D files', TGLMS3DVectorFile);

end.
