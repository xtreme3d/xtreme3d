{: GLVectorFileObjects<p>

	Vector File related objects for GLScene<p>

	<b>History :</b><font size=-1><ul>
	    <li>05/12/05 - PhP - fixed TFGIndexTexCoordList.BuildList (thanks fig) 
      <li>10/11/05 - Mathx - Added LastLoadedFilename to TGLBaseMesh (RFE 955083).
      <li>09/11/05 - Mathx - Added isSwitchingAnimation to TGLActor.
      <li>05/09/05 - Mathx - Fixed TSkeletonMeshObject read/write filer (thanks to Zapology)
      <li>04/07/05 - Mathx - Protection against picking mode texture mapping errors
      <li>27/01/05 - Mathx - BuildOctree can now specify an (optional) TreeDepth.
      <li>11/01/05 - SG - Another fix for TGLBaseMesh.Assign (dikoe Kenguru)
      <li>11/01/05 - SG - Fix for TGLBaseMesh.Assign when assigning actors
      <li>26/11/04 - MRQZZZ - by Uwe Raabe : fixed TBaseMeshObject.BuildNormals
      <li>26/11/04 - MRQZZZ - Added "Rendered" property to TGLBaseMesh in order to prevent rendering of the GLBaseMesh but allowing the rendering of it's children
      <li>25/11/04 - SG - Fixed memory leak in TMeshObject (dikoe Kenguru)
      <li>24/11/04 - MF - Added OctreePointInMesh
      <li>03/10/04 - MRQZZZ - Fixed memory leak (FAutoScaling.Free) in TGLBaseMesh.Destroy; (thanks Jan Zizka)
      <li>24/09/04 - SG - Added GetTriangleData/SetTriangleData functions,
                          Added TexCoordsEx, Binormals, Tangents,
                          Added BuildTangentSpace function (experimental).
      <li>23/07/04 - SG - Added fgmmQuad case for TFGVertexIndexList.TraingleCount
                          (Thanks fig).
      <li>18/07/04 - LR - Suppress Consts in uses
      <li>20/06/04 - MRQZZZ - Added AutoScaling property to GLBaseMesh to scale a mesh after loading (like Autocentering) 
      <li>30/03/04 - EG - Added TSkeletonBoneList.BoneCount
      <li>23/03/04 - SG - External positions added to skeleton blended lerps.
                          AutoUpdate flag added to skeleton collider list.
      <li>09/03/04 - SG - TFGIndexTexCoordList.BuildList can now use per vertex color
      <li>29/01/04 - SG - Fix for ApplyCurrentSkeletonFrame with multiple bones per vertex. 
                          Mesh reassembles correctly now (tested up to 4 bones per vertex).
      <li>03/12/03 - SG - Added TSkeletonCollider and TSkeletonColliderList
                          Added Colliders (TSkeletonColliderList) to TSkeleton
      <li>24/10/03 - SG - Various fixes for multiple bones per vertex
      <li>21/09/03 - MRQZZZ - Added "aamLoopBackward" to AnimationMode property
      <li>19/09/03 - EG - "Lighmap" -&gt; "LightMap"
      <li>01/09/03 - SG - Added skeleton frame conversion methods to convert between
                          Rotations and Quaternions.
      <li>27/08/03 - SG - Fixed AddWeightedBone for multiple bones per vertex
      <li>13/08/03 - SG - Added quaternion transforms for skeletal animation
      <li>12/08/03 - SG - Fixed a tiny bug in TSkeleton.MorphMesh
      <li>08/07/03 - EG - Fixed puny bug in skeletal normals transformation 
      <li>05/06/03 - SG - Split SMD, MD2, 3DS, PLY, TIN and GTS code into separate units,
                          FileFormats\GLFile???.pas
      <li>16/05/03 - SG - Fixed OpenGL error caused by glColorMaterial in TMeshObject.BuildList
      <li>08/05/03 - DanB - added OctreeAABBIntersect (Matheus Degiovani)
      <li>07/05/03 - SG - Added TGLSMDVectorFile.SaveToFile method and [read,write] capabilities
      <li>17/04/03 - SG - Added TMeshObjectList.FindMeshByName method
      <li>01/04/03 - SG - Fixed TGLBaseMesh.Assign
      <li>13/02/03 - DanB - added AxisAlignedDimensionsUnscaled
      <li>03/02/03 - EG - Faster PrepareBuildList logic
      <li>31/01/03 - EG - Added MaterialCache logic
      <li>30/01/03 - EG - Fixed color array enable/disable (Nelson Chu),
                          Normals extraction and extraction standardization
      <li>27/01/03 - EG - Assign support, fixed MorphableMeshObjects persistence
      <li>16/01/03 - EG - Updated multiples Bones per vertex transformation code,
                          now makes use of CVAs 
      <li>14/01/03 - EG - Added DisableOpenGLArrays
      <li>09/01/03 - EG - Added Clear methods for MeshObjects
      <li>25/11/02 - EG - Colors and TexCoords lists now disabled if ignoreMaterials is true
      <li>23/10/02 - EG - Faster .GTS and .PLY imports (parsing)
      <li>22/10/02 - EG - Added actor options, fixed skeleton normals transform (thx Marcus)
      <li>21/10/02 - EG - Read support for .GTS (GNU Triangulated Surface library) 
      <li>18/10/02 - EG - FindExtByIndex (Adem)
      <li>17/10/02 - EG - TGLSTLVectorFile moved to new GLFileSTL unit
      <li>04/09/02 - EG - Fixed TGLBaseMesh.AxisAlignedDimensions
      <li>23/08/02 - EG - Added TGLBaseMesh.Visible
      <li>23/07/02 - EG - TGLBaseMesh.LoadFromStream fix (D. Angilella)
      <li>13/07/02 - EG - AutoCenter on barycenter
      <li>22/03/02 - EG - TGLAnimationControler basics now functional
      <li>13/03/02 - EG - Octree support (experimental)
      <li>18/02/02 - EG - Fixed persistence of skeletal meshes
      <li>04/01/02 - EG - Added basic RayCastIntersect implementation
      <li>17/12/01 - EG - Upgraded TGLActor.Synchronize (smooth transitions support)
      <li>30/11/01 - EG - Added smooth transitions (based on Mrqzzz code)
      <li>14/09/01 - EG - Use of vFileStreamClass
      <li>18/08/01 - EG - Added TriangleCount methods, STL export, PLY import
      <li>15/08/01 - EG - FaceGroups can now be rendered by material group
                          (activate with RenderingOption "moroGroupByMaterial")
      <li>14/08/01 - EG - Added TSkeletonBoneList and support for skeleton with
                          multiple root bones, updated SMD loader
      <li>13/08/01 - EG - Improved/fixed SMD loader
      <li>12/08/01 - EG - Completely rewritten handles management,
                          Fixed TActorAnimation.Assign,
                          Fixed persistence
      <li>08/08/01 - EG - Added TGLBaseMesh.AxisAlignedDimensions
      <li>19/07/01 - EG - AutoCentering is now a property of TGLBaseMesh,
                          3DS loader no longer auto-centers,
                          Added ExtractTriangles and related methods
      <li>18/07/01 - EG - VisibilityCulling compatibility changes
      <li>19/06/01 - EG - StrToFloat outlawed and replaced by StrToFloatDef
      <li>25/03/01 - EG - Added TGLAnimationControler
      <li>18/03/01 - EG - Added basic Skeleton structures & SMD importer
      <li>16/03/01 - EG - Introduced new PersistentClasses
      <li>15/03/01 - EG - Fix in TActorAnimation.SetEndFrame (thx David Costa)
      <li>08/03/01 - EG - TGL3DSVectorFile now loads materials for TGLBaseMesh
      <li>26/02/01 - EG - Added TBaseMeshObject & BuildNormals, MD2 normals auto-builded
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>15/01/01 - EG - Added Translate methods
      <li>10/01/01 - EG - Fixed in TGLBaseMesh.DoRender for RenderChildren states
      <li>08/01/01 - EG - Fixed TGLBaseMesh.BuildList messup of attrib states
      <li>22/12/00 - EG - Fixed non-interpolated TGLActor animation (was freezing),
                          Fixed TGLBaseMesh.DoRender messup of attrib states
      <li>18/12/00 - EG - TFGIndexTexCoordList now supports normals (automatically),
                          NormalsOrientation code moved to TGLBaseMesh
      <li>11/12/00 - EG - Fix for NormalOrientation (3DS importer)
      <li>06/12/00 - EG - Added PrepareBuildList mechanism
      <li>08/10/00 - EG - Removed TGLOBJVectorFile, use GLFileOBJ instead
      <li>13/08/00 - EG - Enhancements for Portal Rendering support,
                          Added utility methods & triangle fans
      <li>10/08/00 - EG - Added CurrentAnimation, fixed TMeshObject.GetExtents
      <li>21/07/00 - EG - Vastly improved memory use and mechanisms for MD2/TGLActor
      <li>19/07/00 - EG - Introduced enhanced mesh structure
      <li>16/07/00 - EG - Made use of new TDataFile class
      <li>15/07/00 - EG - FreeForm can now handle 3DS files with multiple textures,
                          Added TGLBaseMesh.GetExtents
      <li>28/06/00 - EG - Support for "ObjectStyle"
      <li>23/06/00 - EG - Reversed "t" texture coord for MD2,
                          TActorAnimations can now load/save
      <li>21/06/00 - EG - Added frame change events to TGLActor,
                          Added TActorAnimations collection
      <li>19/06/00 - EG - Completed smooth movement interpolation for TGLActor
      <li>07/06/00 - EG - TVectorFile now longers assumes a TGLFreeForm as Owner,
                          Added generic TVectorFile.LoadFromFile
      <li>26/05/00 - EG - Removed dependency to GLObjects,
                          TGLFreeForm now may use InterleavedArrays instead of
                          IndexedArrays (better BuildList compatibility)
      <li>22/04/00 - EG - Fixed Material handlings in TGLFreeForm, inverted CCW/CW
                          convention for 3DS Release3
		 <li>11/04/00 - EG - Removed unnecessary code in finalization (thanks Uwe)
	   <li>09/02/00 - EG - Creation from split of GLObjects,
                          fixed class registrations and formats unregistration
	</ul></font>
}
unit GLVectorFileObjects;

interface

uses Classes, GLScene, OpenGL1x, VectorGeometry, SysUtils, GLMisc, GLTexture,
   GLMesh, VectorLists, PersistentClasses, Octree, GeometryBB,
   ApplicationFileIO, GLSilhouette;

type

   TMeshObjectList = class;
   TFaceGroups = class;

   // TMeshAutoCentering
   //
   TMeshAutoCentering = (macCenterX, macCenterY, macCenterZ, macUseBarycenter);
   TMeshAutoCenterings = set of TMeshAutoCentering;

   // TMeshObjectMode
   //
   TMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

   // TBaseMeshObject
   //
   {: A base class for mesh objects.<p>
      The class introduces a set of vertices and normals for the object but
      does no rendering of its own. }
   TBaseMeshObject = class (TPersistentObject)
      private
         { Private Declarations }
         FName : String;
         FVertices : TAffineVectorList;
         FNormals : TAffineVectorList;
         FVisible : Boolean;

      protected
         { Protected Declarations }
         procedure SetVertices(const val : TAffineVectorList);
         procedure SetNormals(const val : TAffineVectorList);

         procedure ContributeToBarycenter(var currentSum : TAffineVector;
                                          var nb : Integer); dynamic;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Clears all mesh object data, submeshes, facegroups, etc. }
         procedure Clear; dynamic;

         {: Translates all the vertices by the given delta. }
         procedure Translate(const delta : TAffineVector); dynamic;
         {: Builds (smoothed) normals for the vertex list.<p>
            If normalIndices is nil, the method assumes a bijection between
            vertices and normals sets, and when performed, Normals and Vertices
            list will have the same number of items (whatever previously was in
            the Normals list is ignored/removed).<p>
            If normalIndices is defined, normals will be added to the list and
            their indices will be added to normalIndices. Already defined
            normals and indices are preserved.<p>
            The only valid modes are currently momTriangles and momTriangleStrip
            (ie. momFaceGroups not supported). }
         procedure BuildNormals(vertexIndices : TIntegerList; mode : TMeshObjectMode;
                                normalIndices : TIntegerList = nil);
         {: Extracts all mesh triangles as a triangles list.<p>
            The resulting list size is a multiple of 3, each group of 3 vertices
            making up and independant triangle.<br>
            The returned list can be used independantly from the mesh object
            (all data is duplicated) and should be freed by caller.<p>
            If texCoords is specified, per vertex texture coordinates will be
            placed there, when available. }
         function ExtractTriangles(texCoords : TAffineVectorList = nil;
                                   normals : TAffineVectorList = nil) : TAffineVectorList; dynamic;

         property Name : String read FName write FName;
         property Visible : Boolean read FVisible write FVisible;
         property Vertices : TAffineVectorList read FVertices write SetVertices;
         property Normals : TAffineVectorList read FNormals write SetNormals;
   end;

   TSkeletonFrameList = class;

   TSkeletonFrameTransform = (sftRotation,sftQuaternion);

	// TSkeletonFrame
	//
   {: Stores position and rotation for skeleton joints.<p>
      If you directly alter some values, make sure to call FlushLocalMatrixList
      so that the local matrices will be recalculated (the call to Flush does
      not recalculate the matrices, but marks the current ones as dirty). }
	TSkeletonFrame = class (TPersistentObject)
	   private
	      { Private Declarations }
         FOwner : TSkeletonFrameList;
         FName : String;
         FPosition : TAffineVectorList;
         FRotation : TAffineVectorList;
         FQuaternion : TQuaternionList;
         FLocalMatrixList : PMatrixArray;
         FTransformMode : TSkeletonFrameTransform;

	   protected
	      { Protected Declarations }
         procedure SetPosition(const val : TAffineVectorList);
         procedure SetRotation(const val : TAffineVectorList);
         procedure SetQuaternion(const val : TQuaternionList);

	   public
	      { Public Declarations }
         constructor CreateOwned(aOwner : TSkeletonFrameList);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TSkeletonFrameList read FOwner;
         property Name : String read FName write FName;
         {: Position values for the joints. }
         property Position : TAffineVectorList read FPosition write SetPosition;
         {: Rotation values for the joints. }
         property Rotation : TAffineVectorList read FRotation write SetRotation;
         {: Quaternions are an alternative to Euler rotations to build the
            global matrices for the skeleton bones. }
         property Quaternion : TQuaternionList read FQuaternion write SetQuaternion;
         {: TransformMode indicates whether to use Rotation or Quaternion to build
            the local transform matrices. }
         property TransformMode : TSkeletonFrameTransform read FTransformMode write FTransformMode;

         {: Calculate or retrieves an array of local bone matrices.<p>
            This array is calculated on the first call after creation, and the
            first call following a FlushLocalMatrixList. Subsequent calls return
            the same arrays. }
         function LocalMatrixList : PMatrixArray;
         {: Flushes (frees) then LocalMatrixList data.<p>
            Call this function to allow a recalculation of local matrices. }
         procedure FlushLocalMatrixList;
         //: As the name states; Convert Quaternions to Rotations or vice-versa.
         procedure ConvertQuaternionsToRotations(KeepQuaternions : Boolean = True);
         procedure ConvertRotationsToQuaternions(KeepRotations : Boolean = True);
	end;

   // TSkeletonFrameList
   //
   {: A list of TSkeletonFrame objects. }
   TSkeletonFrameList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetSkeletonFrame(Index: Integer) : TSkeletonFrame;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TPersistent);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         //: As the name states; Convert Quaternions to Rotations or vice-versa.
         procedure ConvertQuaternionsToRotations(
           KeepQuaternions : Boolean = True; SetTransformMode : Boolean = True);
         procedure ConvertRotationsToQuaternions(
           KeepRotations : Boolean = True; SetTransformMode : Boolean = True);

         property Owner : TPersistent read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TSkeletonFrame read GetSkeletonFrame; default;
   end;

   TSkeleton = class;
   TSkeletonBone = class;

	// TSkeletonBoneList
	//
   {: A list of skeleton bones.<p> }
	TSkeletonBoneList = class (TPersistentObjectList)
	   private
	      { Private Declarations }
         FSkeleton : TSkeleton;     // not persistent

	   protected
	      { Protected Declarations }
         FGlobalMatrix : TMatrix;
         
         function GetSkeletonBone(Index: Integer) : TSkeletonBone;
         procedure AfterObjectCreatedByReader(Sender : TObject); override;

	   public
	      { Public Declarations }
	      constructor CreateOwned(aOwner : TSkeleton);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property Skeleton : TSkeleton read FSkeleton;
         property Items[Index: Integer] : TSkeletonBone read GetSkeletonBone; default;

         {: Returns a bone by its BoneID, nil if not found. }
         function BoneByID(anID : Integer) : TSkeletonBone; virtual;
         {: Returns a bone by its Name, nil if not found. }
         function BoneByName(const aName : String) : TSkeletonBone; virtual;
         {: Number of bones (including all children and self). }
         function BoneCount : Integer;

         //: Render skeleton wireframe
         procedure BuildList(var mrci : TRenderContextInfo); virtual; abstract;
         procedure PrepareGlobalMatrices; virtual;
	end;

	// TSkeletonRootBoneList
	//
   {: This list store skeleton root bones exclusively.<p> }
	TSkeletonRootBoneList = class (TSkeletonBoneList)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         //: Render skeleton wireframe
         procedure BuildList(var mrci : TRenderContextInfo); override;

         property GlobalMatrix : TMatrix read FGlobalMatrix write FGlobalMatrix;
   end;

	// TSkeletonBone
	//
   {: A skeleton bone or node and its children.<p>
      This class is the base item of the bones hierarchy in a skeletal model.
      The joint values are stored in a TSkeletonFrame, but the calculated bone
      matrices are stored here. }
	TSkeletonBone = class (TSkeletonBoneList)
	   private
	      { Private Declarations }
         FOwner : TSkeletonBoneList;    // indirectly persistent
         FBoneID : Integer;
         FName : String;
         FColor : Cardinal;

	   protected
	      { Protected Declarations }
         function GetSkeletonBone(Index: Integer) : TSkeletonBone;
         procedure SetColor(const val : Cardinal);

	   public
	      { Public Declarations }
	      constructor CreateOwned(aOwner : TSkeletonBoneList);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         //: Render skeleton wireframe
         procedure BuildList(var mrci : TRenderContextInfo); override;

         property Owner : TSkeletonBoneList read FOwner;
         property Name : String read FName write FName;
         property BoneID : Integer read FBoneID write FBoneID;
         property Color : Cardinal read FColor write SetColor;
         property Items[Index: Integer] : TSkeletonBone read GetSkeletonBone; default;

         {: Returns a bone by its BoneID, nil if not found. }
         function BoneByID(anID : Integer) : TSkeletonBone; override;
         function BoneByName(const aName : String) : TSkeletonBone; override;

         {: Calculates the global matrix for the bone and its sub-bone.<p>
            Call this function directly only the RootBone. }
         procedure PrepareGlobalMatrices; override;
         {: Global Matrix for the bone in the current frame.<p>
            Global matrices must be prepared by invoking PrepareGlobalMatrices
            on the root bone. }
         property GlobalMatrix : TMatrix read FGlobalMatrix;

         {: Free all sub bones and reset BoneID and Name. }
         procedure Clean; override;
	end;

   TSkeletonColliderList = class;

   // TSkeletonCollider
   //
   {: A general class storing the base level info required for skeleton
      based collision methods. This class is meant to be inherited from
      to create skeleton driven Verlet Constraints, ODE Geoms, etc.
      Overriden classes should be named as TSCxxxxx. }
   TSkeletonCollider = class(TPersistentObject)
      private
         { Private Declarations }
         FOwner : TSkeletonColliderList;
         FBone : TSkeletonBone;
         FBoneID : Integer;
         FLocalMatrix, FGlobalMatrix : TMatrix;
         FAutoUpdate : Boolean;

      protected
         { Protected Declarations }
         procedure SetBone(const val : TSkeletonBone);
         procedure SetLocalMatrix(const val : TMatrix);

      public
         { Public Declarations }
         constructor Create; override;
         constructor CreateOwned(AOwner : TSkeletonColliderList);
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;
         {: This method is used to align the colliders and their
            derived objects to their associated skeleton bone.
            Override to set up descendant class alignment properties. }
         procedure AlignCollider; virtual;

         property Owner : TSkeletonColliderList read FOwner;
         //: The bone that this collider associates with.
         property Bone : TSkeletonBone read FBone write SetBone;
         {: Offset and orientation of the collider in the associated
            bone's space. }
         property LocalMatrix : TMatrix read FLocalMatrix write SetLocalMatrix;
         {: Global offset and orientation of the collider. This
            gets set in the AlignCollider method. }
         property GlobalMatrix : TMatrix read FGlobalMatrix;
         property AutoUpdate : Boolean read FAutoUpdate write FAutoUpdate;
   end;

   // TSkeletonColliderList
   //
   {: List class for storing TSkeletonCollider objects. }
   TSkeletonColliderList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetSkeletonCollider(index : Integer) : TSkeletonCollider;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TPersistent);
         destructor Destroy; override;

         procedure ReadFromFiler(reader : TVirtualReader); override;
         procedure Clear; override;
         {: Calls AlignCollider for each collider in the list. }
         procedure AlignColliders;

         property Owner : TPersistent read FOwner;
         property Items[Index: Integer] : TSkeletonCollider read GetSkeletonCollider; default;
   end;

   TGLBaseMesh = class;

   // TBlendedLerpInfo
   //
   {: Small structure to store a weighted lerp for use in blending. }
   TBlendedLerpInfo = record
      frameIndex1, frameIndex2 : Integer;
      lerpFactor : Single;
      weight : Single;
      externalPositions : TAffineVectorList;
      externalRotations : TAffineVectorList;
      externalQuaternions : TQuaternionList;
   end;

	// TSkeleton
	//
   {: Main skeleton object.<p>
      This class stores the bones hierarchy and animation frames.<br>
      It is also responsible for maintaining the "CurrentFrame" and allowing
      various frame blending operations. }
	TSkeleton = class (TPersistentObject)
	   private
	      { Private Declarations }
         FOwner : TGLBaseMesh;
         FRootBones : TSkeletonRootBoneList;
         FFrames : TSkeletonFrameList;
         FCurrentFrame : TSkeletonFrame; // not persistent
         FBonesByIDCache : TList;
         FColliders : TSkeletonColliderList;

	   protected
	      { Protected Declarations }
         procedure SetRootBones(const val : TSkeletonRootBoneList);
         procedure SetFrames(const val : TSkeletonFrameList);
         function GetCurrentFrame : TSkeletonFrame;
         procedure SetCurrentFrame(val : TSkeletonFrame);
         procedure SetColliders(const val : TSkeletonColliderList);

	   public
	      { Public Declarations }
         constructor CreateOwned(AOwner : TGLBaseMesh);
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TGLBaseMesh read FOwner;
         property RootBones : TSkeletonRootBoneList read FRootBones write SetRootBones;
         property Frames : TSkeletonFrameList read FFrames write SetFrames;
         property CurrentFrame : TSkeletonFrame read GetCurrentFrame write SetCurrentFrame;
         property Colliders : TSkeletonColliderList read FColliders write SetColliders;

         procedure FlushBoneByIDCache;
         function BoneByID(anID : Integer) : TSkeletonBone;
         function BoneByName(const aName : String) : TSkeletonBone;
         function BoneCount : Integer;

         procedure MorphTo(frameIndex : Integer); overload;
         procedure MorphTo(frame : TSkeletonFrame); overload;
         procedure Lerp(frameIndex1, frameIndex2 : Integer;
                        lerpFactor : Single);
         procedure BlendedLerps(const lerpInfos : array of TBlendedLerpInfo);

         {: Linearly removes the translation component between skeletal frames.<p>
            This function will compute the translation of the first bone (index 0)
            and linearly subtract this translation in all frames between startFrame
            and endFrame. Its purpose is essentially to remove the 'slide' that
            exists in some animation formats (f.i. SMD). }
         procedure MakeSkeletalTranslationStatic(startFrame, endFrame : Integer);
         {: Removes the absolute rotation component of the skeletal frames.<p>
            Some formats will store frames with absolute rotation information,
            if this correct if the animation is the "main" animation.<br>
            This function removes that absolute information, making the animation
            frames suitable for blending purposes. }
         procedure MakeSkeletalRotationDelta(startFrame, endFrame : Integer);

         {: Applies current frame to morph all mesh objects. }
         procedure MorphMesh(normalize : Boolean);

         {: Copy bone rotations from reference skeleton. }
         procedure Synchronize(reference : TSkeleton);
         {: Release bones and frames info. }
         procedure Clear;
	end;

   // TMeshObjectRenderingOption
   //
   {: Rendering options per TMeshObject.<p>
   <ul>
   <li>moroGroupByMaterial : if set, the facegroups will be rendered by material
      in batchs, this will optimize rendering by reducing material switches, but
      also implies that facegroups will not be rendered in the order they are in
      the list.
   </ul> }
   TMeshObjectRenderingOption = (moroGroupByMaterial);
   TMeshObjectRenderingOptions = set of TMeshObjectRenderingOption;

   // TMeshObject
   //
   {: Base mesh class.<p>
      Introduces base methods and properties for mesh objects.<p>
      Subclasses are named "TMOxxx". }
   TMeshObject = class (TBaseMeshObject)
      private
         { Private Declarations }
         FOwner : TMeshObjectList;
         FTexCoords : TAffineVectorList; // provision for 3D textures
         FLightMapTexCoords : TTexPointList; // reserved for 2D surface needs
         FColors : TVectorList;
         FFaceGroups: TFaceGroups;
         FMode : TMeshObjectMode;
         FRenderingOptions : TMeshObjectRenderingOptions;
         FArraysDeclared : Boolean; // not persistent
         FLightMapArrayEnabled : Boolean; // not persistent
         FLastLightMapIndex : Integer; // not persistent
         FTexCoordsEx : TList;
         FBinormalsTexCoordIndex : Integer;
         FTangentsTexCoordIndex : Integer;
         FLastXOpenGLTexMapping : Cardinal;

      protected
         { Protected Declarations }
         procedure SetTexCoords(const val : TAffineVectorList);
         procedure SetLightmapTexCoords(const val : TTexPointList);
         procedure SetColors(const val : TVectorList);

         procedure DeclareArraysToOpenGL(var mrci : TRenderContextInfo;
                                         evenIfAlreadyDeclared : Boolean = False);
         procedure DisableOpenGLArrays(var mrci : TRenderContextInfo);

         procedure EnableLightMapArray(var mrci : TRenderContextInfo);
         procedure DisableLightMapArray(var mrci : TRenderContextInfo);

         procedure SetTexCoordsEx(index : Integer; const val : TVectorList);
         function GetTexCoordsEx(index : Integer) : TVectorList;

         procedure SetBinormals(const val : TVectorList);
         function GetBinormals : TVectorList;
         procedure SetBinormalsTexCoordIndex(const val : Integer);
         procedure SetTangents(const val : TVectorList);
         function GetTangents : TVectorList;
         procedure SetTangentsTexCoordIndex(const val : Integer);

      public
         { Public Declarations }
         {: Creates, assigns Owner and adds to list. } 
         constructor CreateOwned(AOwner : TMeshObjectList);
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure Clear; override;

         function ExtractTriangles(texCoords : TAffineVectorList = nil;
                                   normals : TAffineVectorList = nil) : TAffineVectorList; override;
         {: Returns number of triangles in the mesh object. }
         function TriangleCount : Integer; dynamic;

         procedure PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
         procedure DropMaterialLibraryCache;
         
         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); virtual;
         //: Similar to regular scene object's BuildList method
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         //: The extents of the object (min and max coordinates)
         procedure GetExtents(var min, max : TAffineVector); dynamic;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         function PointInObject(const aPoint : TAffineVector) : Boolean; virtual;

         //: Returns the triangle data for a given triangle
         procedure GetTriangleData(tri : Integer; list : TAffineVectorList; 
            var v0, v1, v2 : TAffineVector); overload;
         procedure GetTriangleData(tri : Integer; list : TVectorList; 
            var v0, v1, v2 : TVector); overload;

         //: Sets the triangle data of a given triangle
         procedure SetTriangleData(tri : Integer; list : TAffineVectorList; 
            const v0, v1, v2 : TAffineVector); overload;
         procedure SetTriangleData(tri : Integer; list : TVectorList; 
            const v0, v1, v2 : TVector); overload;

         {: Build the tangent space from the mesh object's vertex, normal 
            and texcoord data, filling the binormals and tangents where
            specified. }
         procedure BuildTangentSpace(
            buildBinormals : Boolean = True; 
            buildTangents : Boolean = True);

         property Owner : TMeshObjectList read FOwner;
         property Mode : TMeshObjectMode read FMode write FMode;
         property TexCoords : TAffineVectorList read FTexCoords write SetTexCoords;
         property LightMapTexCoords : TTexPointList read FLightMapTexCoords write SetLightMapTexCoords;
         property Colors : TVectorList read FColors write SetColors;
         property FaceGroups : TFaceGroups read FFaceGroups;
         property RenderingOptions : TMeshObjectRenderingOptions read FRenderingOptions write FRenderingOptions;

         {: The TexCoords Extension is a list of vector lists that are used 
            to extend the vertex data applied during rendering.<p>
            
            The lists are applied to the GL_TEXTURE0_ARB + index texture 
            environment. This means that if TexCoordsEx 0 or 1 have data it
            will override the TexCoords or LightMapTexCoords repectively. 
            Lists are created on demand, meaning that if you request
            TexCoordsEx[4] it will create the list up to and including 4.
            The extensions are only applied to the texture environment if
            they contain data. }
         property TexCoordsEx[index : Integer] : TVectorList read GetTexCoordsEx write SetTexCoordsEx;

         {: A TexCoordsEx list wrapper for binormals usage, 
            returns TexCoordsEx[BinormalsTexCoordIndex]. }
         property Binormals : TVectorList read GetBinormals write SetBinormals;
         {: A TexCoordsEx list wrapper for tangents usage, 
            returns TexCoordsEx[BinormalsTexCoordIndex]. }
         property Tangents : TVectorList read GetTangents write SetTangents;
         //: Specify the texcoord extension index for binormals (default = 2)
         property BinormalsTexCoordIndex : Integer read FBinormalsTexCoordIndex write SetBinormalsTexCoordIndex;
         //: Specify the texcoord extension index for tangents (default = 3)
         property TangentsTexCoordIndex : Integer read FTangentsTexCoordIndex write SetTangentsTexCoordIndex;

   end;

   // TMeshObjectList
   //
   {: A list of TMeshObject objects. }
   TMeshObjectList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TGLBaseMesh;

      protected
         { Protected Declarations }
         function GetMeshObject(Index: Integer) : TMeshObject;

      public
         { Public Declarations }
         constructor CreateOwned(aOwner : TGLBaseMesh);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
         procedure DropMaterialLibraryCache;

         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); virtual;
         //: Similar to regular scene object's BuildList method
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         procedure MorphTo(morphTargetIndex : Integer);
         procedure Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                        lerpFactor : Single);
         function MorphTargetCount : Integer;

         procedure GetExtents(var min, max : TAffineVector);
         procedure Translate(const delta : TAffineVector);
         function ExtractTriangles(texCoords : TAffineVectorList = nil;
                                   normals : TAffineVectorList = nil) : TAffineVectorList;
         {: Returns number of triangles in the meshes of the list. }
         function TriangleCount : Integer;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;
         
         function FindMeshByName(MeshName : String) : TMeshObject;

         property Owner : TGLBaseMesh read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TMeshObject read GetMeshObject; default;
   end;

   TMeshObjectListClass = class of TMeshObjectList;

   TMeshMorphTargetList = class;

   // TMeshMorphTarget
   //
   {: A morph target, stores alternate lists of vertices and normals. }
   TMeshMorphTarget = class (TBaseMeshObject)
      private
         { Private Declarations }
         FOwner : TMeshMorphTargetList;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshMorphTargetList);
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         property Owner : TMeshMorphTargetList read FOwner;
   end;

   // TMeshMorphTargetList
   //
   {: A list of TMeshMorphTarget objects. }
   TMeshMorphTargetList = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetMeshMorphTarget(Index: Integer) : TMeshMorphTarget;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TPersistent);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure Translate(const delta : TAffineVector);

         property Owner : TPersistent read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TMeshMorphTarget read GetMeshMorphTarget; default;
   end;

   // TMorphableMeshObject
   //
   {: Mesh object with support for morph targets.<p>
      The morph targets allow to change vertices and normals according to pre-
      existing "morph targets". }
   TMorphableMeshObject = class (TMeshObject)
      private
         { Private Declarations }
         FMorphTargets : TMeshMorphTargetList;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure Clear; override;

         procedure Translate(const delta : TAffineVector); override;

         procedure MorphTo(morphTargetIndex : Integer);
         procedure Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                        lerpFactor : Single);

         property MorphTargets : TMeshMorphTargetList read FMorphTargets;
   end;

   // TVertexBoneWeight
   //
   TVertexBoneWeight = packed record
      BoneID : Integer;
      Weight : Single;
   end;

   TVertexBoneWeightArray = array [0..MaxInt shr 4] of TVertexBoneWeight;
   PVertexBoneWeightArray = ^TVertexBoneWeightArray;
   TVerticesBoneWeights = array [0..MaxInt shr 3] of PVertexBoneWeightArray;
   PVerticesBoneWeights = ^TVerticesBoneWeights;
   TVertexBoneWeightDynArray = array of TVertexBoneWeight;

	// TSkeletonMeshObject
	//
   {: A mesh object with vertice bone attachments.<p>
      The class adds per vertex bone weights to the standard morphable mesh.<br>
      The TVertexBoneWeight structures are accessed via VerticesBonesWeights,
      they must be initialized by adjusting the BonesPerVertex and
      VerticeBoneWeightCount properties, you can also add vertex by vertex
      by using the AddWeightedBone method.<p>
      When BonesPerVertex is 1, the weight is ignored (set to 1.0). }
	TSkeletonMeshObject = class (TMorphableMeshObject)
	   private
	      { Private Declarations }
         FVerticesBonesWeights : PVerticesBoneWeights;
         FVerticeBoneWeightCount, FVerticeBoneWeightCapacity : Integer;
         FBonesPerVertex : Integer;
         FLastVerticeBoneWeightCount, FLastBonesPerVertex : Integer; // not persistent
         FBoneMatrixInvertedMeshes : TList; // not persistent

	   protected
	      { Protected Declarations }
         procedure SetVerticeBoneWeightCount(const val : Integer);
         procedure SetVerticeBoneWeightCapacity(const val : Integer);
         procedure SetBonesPerVertex(const val : Integer);
	      procedure ResizeVerticesBonesWeights;

	   public
	      { Public Declarations }
	      constructor Create; override;
         destructor Destroy; override;

	      procedure WriteToFiler(writer : TVirtualWriter); override;
	      procedure ReadFromFiler(reader : TVirtualReader); override;

	      procedure Clear; override;

         property VerticesBonesWeights : PVerticesBoneWeights read FVerticesBonesWeights;
         property VerticeBoneWeightCount : Integer read FVerticeBoneWeightCount write SetVerticeBoneWeightCount;
         property VerticeBoneWeightCapacity : Integer read FVerticeBoneWeightCapacity write SetVerticeBoneWeightCapacity;
         property BonesPerVertex : Integer read FBonesPerVertex write SetBonesPerVertex;

         function FindOrAdd(boneID : Integer;
                            const vertex, normal : TAffineVector) : Integer; overload;
         function FindOrAdd(const boneIDs : TVertexBoneWeightDynArray;
                            const vertex, normal : TAffineVector) : Integer; overload;

         procedure AddWeightedBone(aBoneID : Integer; aWeight : Single);
         procedure AddWeightedBones(const boneIDs : TVertexBoneWeightDynArray);
         procedure PrepareBoneMatrixInvertedMeshes;
         procedure ApplyCurrentSkeletonFrame(normalize : Boolean);

	end;

   // TFaceGroup
   //
   {: Describes a face group of a TMeshObject.<p>
      Face groups should be understood as "a way to use mesh data to render
      a part or the whole mesh object".<p>
      Subclasses implement the actual behaviours, and should have at least
      one "Add" method, taking in parameters all that is required to describe
      a single base facegroup element. }
   TFaceGroup = class (TPersistentObject)
      private
         { Private Declarations }
         FOwner : TFaceGroups;
         FMaterialName : String;
         FMaterialCache : TGLLibMaterial;
         FLightMapIndex : Integer;
         FRenderGroupID : Integer; // NOT Persistent, internal use only (rendering options)

	   protected
	      { Protected Declarations }
         procedure AttachLightmap(lightMap : TGLTexture; var mrci : TRenderContextInfo);
         procedure AttachOrDetachLightmap(var mrci : TRenderContextInfo);

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TFaceGroups); virtual;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
         procedure DropMaterialLibraryCache;

         procedure BuildList(var mrci : TRenderContextInfo); virtual; abstract;

         {: Add to the list the triangles corresponding to the facegroup.<p>
            This function is used by TMeshObjects ExtractTriangles to retrieve
            all the triangles in a mesh. }
         procedure AddToTriangles(aList : TAffineVectorList;
                                  aTexCoords : TAffineVectorList = nil;
                                  aNormals : TAffineVectorList = nil); dynamic;
         {: Returns number of triangles in the facegroup. }
         function TriangleCount : Integer; dynamic; abstract;
         {: Reverses the rendering order of faces.<p>
            Default implementation does nothing }
         procedure Reverse; dynamic;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         property Owner : TFaceGroups read FOwner write FOwner;
         property MaterialName : String read FMaterialName write FMaterialName;
         property MaterialCache : TGLLibMaterial read FMaterialCache;
         {: Index of lightmap in the lightmap library. }
         property LightMapIndex : Integer read FLightMapIndex write FLightMapIndex;
   end;

   // TFaceGroupMeshMode
   //
   {: Known descriptions for face group mesh modes.<p>
      - fgmmTriangles : issue all vertices with GL_TRIANGLES.<br>
      - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP.<br>
      - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
         the same normal for all vertices of a triangle.<br>
      - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN.<br>
      - fgmmQuads : issue all vertices with GL_QUADS. }
   TFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles,
                         fgmmTriangleFan, fgmmQuads);

   // TFGVertexIndexList
   //
   {: A face group based on an indexlist.<p>
      The index list refers to items in the mesh object (vertices, normals, etc.),
      that are all considered in sync, the render is obtained issueing the items
      in the order given by the vertices.<p> }
   TFGVertexIndexList = class (TFaceGroup)
      private
         { Private Declarations }
         FVertexIndices : TIntegerList;
         FMode : TFaceGroupMeshMode;

      protected
         { Protected Declarations }
         procedure SetVertexIndices(const val : TIntegerList);

         procedure AddToList(source, destination : TAffineVectorList;
                             indices : TIntegerList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure AddToTriangles(aList : TAffineVectorList;
                                  aTexCoords : TAffineVectorList = nil;
                                  aNormals : TAffineVectorList = nil); override;
         function TriangleCount : Integer; override;
         procedure Reverse; override;

         procedure Add(idx : Integer);
         procedure GetExtents(var min, max : TAffineVector);
         {: If mode is strip or fan, convert the indices to triangle list indices. }
         procedure ConvertToList;

         //: Return the normal from the 1st three points in the facegroup
         function  GetNormal : TAffineVector;

         property Mode : TFaceGroupMeshMode read FMode write FMode;
         property VertexIndices : TIntegerList read FVertexIndices write SetVertexIndices;
   end;

   // TFGVertexNormalTexIndexList
   //
   {: Adds normals and texcoords indices.<p>
      Allows very compact description of a mesh. The Normals ad TexCoords
      indices are optionnal, if missing (empty), VertexIndices will be used. }
   TFGVertexNormalTexIndexList = class (TFGVertexIndexList)
      private
         { Private Declarations }
         FNormalIndices : TIntegerList;
         FTexCoordIndices : TIntegerList;

      protected
         { Protected Declarations }
         procedure SetNormalIndices(const val : TIntegerList);
         procedure SetTexCoordIndices(const val : TIntegerList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure AddToTriangles(aList : TAffineVectorList;
                                  aTexCoords : TAffineVectorList = nil;
                                  aNormals : TAffineVectorList = nil); override;

         procedure Add(vertexIdx, normalIdx, texCoordIdx : Integer);

         property NormalIndices : TIntegerList read FNormalIndices write SetNormalIndices;
         property TexCoordIndices : TIntegerList read FTexCoordIndices write SetTexCoordIndices;
   end;

   // TFGIndexTexCoordList
   //
   {: Adds per index texture coordinates to its ancestor.<p>
      Per index texture coordinates allows having different texture coordinates
      per triangle, depending on the face it is used in. }
   TFGIndexTexCoordList = class (TFGVertexIndexList)
      private
         { Private Declarations }
         FTexCoords : TAffineVectorList;

      protected
         { Protected Declarations }
         procedure SetTexCoords(const val : TAffineVectorList);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure AddToTriangles(aList : TAffineVectorList;
                                  aTexCoords : TAffineVectorList = nil;
                                  aNormals : TAffineVectorList = nil); override;

         procedure Add(idx : Integer; const texCoord : TAffineVector); overload;
         procedure Add(idx : Integer; const s, t : Single); overload;

         property TexCoords : TAffineVectorList read FTexCoords write SetTexCoords;
   end;

   // TFaceGroups
   //
   {: A list of TFaceGroup objects. }
   TFaceGroups = class (TPersistentObjectList)
      private
         { Private Declarations }
         FOwner : TMeshObject;

      protected
         { Protected Declarations }
         function GetFaceGroup(Index: Integer) : TFaceGroup;

      public
         { Public Declarations }
         constructor CreateOwned(AOwner : TMeshObject);
         destructor Destroy; override;

			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
         procedure DropMaterialLibraryCache;
         
         property Owner : TMeshObject read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TFaceGroup read GetFaceGroup; default;

         procedure AddToTriangles(aList : TAffineVectorList;
                                  aTexCoords : TAffineVectorList = nil;
                                  aNormals : TAffineVectorList = nil);

         {: Material Library of the owner TGLBaseMesh. }
         function MaterialLibrary : TGLMaterialLibrary;
         {: Sort faces by material.<p>
            Those without material first in list, followed by opaque materials,
            then transparent materials. }
         procedure SortByMaterial;
   end;

   // TMeshNormalsOrientation
   //
   {: Determines how normals orientation is defined in a mesh.<p>
      - mnoDefault : uses default orientation<br>
      - mnoInvert : inverse of default orientation<br>
      - mnoAutoSolid : autocalculate to make the mesh globally solid<br>
      - mnoAutoHollow : autocalculate to make the mesh globally hollow<br> }
   TMeshNormalsOrientation = (mnoDefault, mnoInvert); //, mnoAutoSolid, mnoAutoHollow);

   // TVectorFile
   //
   {: Abstract base class for different vector file formats.<p>
      The actual implementation for these files (3DS, DXF..) must be done
      seperately. The concept for TVectorFile is very similar to TGraphic
      (see Delphi Help). }
   TVectorFile = class (TDataFile)
      private
         { Private Declarations }
         FNormalsOrientation : TMeshNormalsOrientation;

      protected
         { Protected Declarations }
         procedure SetNormalsOrientation(const val : TMeshNormalsOrientation); virtual;

      public
         { Public Declarations }
         constructor Create(AOwner: TPersistent); override;

         function Owner : TGLBaseMesh;

         property NormalsOrientation : TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation;
   end;

   TVectorFileClass = class of TVectorFile;

   // TGLGLSMVectorFile
   //
   {: GLSM (GLScene Mesh) vector file.<p>
      This corresponds to the 'native' GLScene format, and object persistence
      stream, which should be the 'fastest' of all formats to load, and supports
      all of GLScene features. }
   TGLGLSMVectorFile = class (TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;

         procedure LoadFromStream(aStream : TStream); override;
         procedure SaveToStream(aStream : TStream); override;
   end;

   // TGLBaseMesh
   //
   {: Base class for mesh objects. }
   TGLBaseMesh = class(TGLSceneObject)
      private
         { Private Declarations }
         FNormalsOrientation : TMeshNormalsOrientation;
         FMaterialLibrary : TGLMaterialLibrary;
         FLightmapLibrary : TGLMaterialLibrary;
         FAxisAlignedDimensionsCache : TVector;
         FUseMeshMaterials : Boolean;
         FOverlaySkeleton : Boolean;
         FIgnoreMissingTextures : Boolean;
         FAutoCentering : TMeshAutoCenterings;
         FAutoScaling: TGLCoordinates;
         FMaterialLibraryCachesPrepared : Boolean;
         FConnectivity : TObject;
         FLastLoadedFilename: string;

      protected
         { Protected Declarations }
         FMeshObjects : TMeshObjectList;     // a list of mesh objects
         FSkeleton : TSkeleton;              // skeleton data & frames
         procedure SetUseMeshMaterials(const val : Boolean);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetLightmapLibrary(const val : TGLMaterialLibrary);
         procedure SetNormalsOrientation(const val : TMeshNormalsOrientation);
         procedure SetOverlaySkeleton(const val : Boolean);
         procedure SetAutoScaling(const Value: TGLCoordinates);
         procedure DestroyHandle; override;

         {: Invoked after creating a TVectorFile and before loading.<p>
            Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.<br>
            Allows to adjust/transfer subclass-specific features. }
         procedure PrepareVectorFile(aFile : TVectorFile); dynamic;

         {: Invoked after a mesh has been loaded/added.<p>
            Triggered by LoadFromFile/Stream and AddDataFromFile/Stream.<br>
            Allows to adjust/transfer subclass-specific features. }
         procedure PrepareMesh; dynamic;

         {: Recursively propagated to mesh object and facegroups.<p>
            Notifies that they all can establish their material library caches. }
         procedure PrepareMaterialLibraryCache;
         {: Recursively propagated to mesh object and facegroups.<p>
            Notifies that they all should forget their material library caches. }
         procedure DropMaterialLibraryCache;

         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list,
            MaterialLibraryCache can be assumed to having been prepared if materials
            are active. Default behaviour is to prepare build lists for the
            meshobjects. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); dynamic;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         function AxisAlignedDimensionsUnscaled : TVector;override;

         procedure BuildList(var rci : TRenderContextInfo); override;
			procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
         procedure StructureChanged; override;
         {: Notifies that geometry data changed, but no re-preparation is needed.<p>
            Using this method will usually be faster, but may result in incorrect
            rendering, reduced performance and/or invalid bounding box data
            (ie. invalid collision detection). Use with caution. } 
         procedure StructureChangedNoPrepare;

         {: BEWARE! Utterly inefficient implementation! }
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;
         function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;

         {: This method allows fast shadow volumes for GLActors.<p>
            If your actor/mesh doesn't change, you don't need to call this.
            It basically caches the connectivity data.}
         procedure BuildSilhouetteConnectivityData;

         property MeshObjects : TMeshObjectList read FMeshObjects;
         property Skeleton : TSkeleton read FSkeleton;

         {: Computes the extents of the mesh.<p> }
         procedure GetExtents(var min, max : TAffineVector);
         {: Computes the barycenter of the mesh.<p> }
         function GetBarycenter : TAffineVector;
         {: Invoked after a mesh has been loaded.<p>
            Should auto-center according to the AutoCentering property. }
         procedure PerformAutoCentering; dynamic;
         {: Invoked after a mesh has been loaded.<p>
            Should auto-scale the vertices of the meshobjects to AutoScaling the property. }
         procedure PerformAutoScaling; dynamic;
         {: Loads a vector file.<p>
            A vector files (for instance a ".3DS") stores the definition of
            a mesh as well as materials property.<p>
            Loading a file replaces the current one (if any). }
         procedure LoadFromFile(const filename : String); dynamic;
         {: Loads a vector file from a stream.<p>
            See LoadFromFile.<br>
            The filename attribute is required to identify the type data you're
            streaming (3DS, OBJ, etc.) }
         procedure LoadFromStream(const filename : String; aStream : TStream); dynamic;
         {: Saves to a vector file.<p>
            Note that only some of the vector files formats can be written to
            by GLScene. }
         procedure SaveToFile(const fileName : String); dynamic;
         {: Saves to a vector file in a stream.<p>
            Note that only some of the vector files formats can be written to
            by GLScene. }
         procedure SaveToStream(const fileName : String; aStream : TStream); dynamic;

         {: Loads additionnal data from a file.<p>
            Additionnal data could be more animation frames or morph target.<br>
            The VectorFile importer must be able to handle addition of data
            flawlessly. }
         procedure AddDataFromFile(const filename : String); dynamic;
         {: Loads additionnal data from stream.<p>
            See AddDataFromFile. }
         procedure AddDataFromStream(const filename : String; aStream : TStream); dynamic;

         {: Returns the filename of the last loaded file, or a blank string if not
            file was loaded (or if the mesh was dinamically built). This does not
            take into account the data added to the mesh (through AddDataFromFile)
            or saved files.}
         function LastLoadedFilename: string;

         {: Determines if a mesh should be centered and how.<p>
            AutoCentering is performed <b>only</b> after loading a mesh, it has
            no effect on already loaded mesh data or when adding from a file/stream.<br>
            If you want to alter mesh data, use direct manipulation methods
            (on the TMeshObjects). }
         property AutoCentering : TMeshAutoCenterings read FAutoCentering write FAutoCentering default [];

         {: Scales vertices to a AutoScaling.<p>
            AutoScaling is performed <b>only</b> after loading a mesh, it has
            no effect on already loaded mesh data or when adding from a file/stream.<br>
            If you want to alter mesh data, use direct manipulation methods
            (on the TMeshObjects). }
         property AutoScaling : TGLCoordinates read FAutoScaling write FAutoScaling;

         {: Material library where mesh materials will be stored/retrieved.<p>
            If this property is not defined or if UseMeshMaterials is false,
            only the FreeForm's material will be used (and the mesh's materials
            will be ignored. }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         {: Defines wether materials declared in the vector file mesh are used.<p>
            You must also define the MaterialLibrary property. }
         property UseMeshMaterials : Boolean read FUseMeshMaterials write SetUseMeshMaterials default True;
         {: LightMap library where lightmaps will be stored/retrieved.<p>
            If this property is not defined, lightmaps won't be used.
            Lightmaps currently *always* use the second texture unit (unit 1),
            and may interfere with multi-texture materials. }
         property LightmapLibrary : TGLMaterialLibrary read FLightmapLibrary write SetLightmapLibrary;
         {: If True, exceptions about missing textures will be ignored.<p>
            Implementation is up to the file loader class (ie. this property
            may be ignored by some loaders) }
         property IgnoreMissingTextures : Boolean read FIgnoreMissingTextures write FIgnoreMissingTextures default False;

         {: Normals orientation for owned mesh.<p> }
         property NormalsOrientation : TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation default mnoDefault;

         {: Request rendering of skeleton bones over the mesh. }
         property OverlaySkeleton : Boolean read FOverlaySkeleton write SetOverlaySkeleton default False;

   end;

   // TGLFreeForm
   //
   {: Container objects for a vector file mesh.<p>
      FreeForms allows loading and rendering vector files (like 3DStudio
      ".3DS" file) in GLScene. Meshes can be loaded with the LoadFromFile
      method.<p>
      A FreeForm may contain more than one mesh, but they will all be handled
      as a single object in a scene. }
   TGLFreeForm = class (TGLBaseMesh)
      private
         { Private Declarations }
         FOctree : TOctree;

      protected
         { Protected Declarations }
         function GetOctree : TOctree;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         function OctreeRayCastIntersect(const rayStart, rayVector : TVector;
                                         intersectPoint : PVector = nil;
                                         intersectNormal : PVector = nil) : Boolean;
         function OctreeSphereSweepIntersect(const rayStart, rayVector : TVector;
                                        const velocity, radius: Single;
                                        intersectPoint : PVector = nil;
                                        intersectNormal : PVector = nil) : Boolean;
         function OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
         {: Returns true if Point is inside the free form - this will only work
         properly on closed meshes. Requires that Octree has been prepared.}
         function OctreePointInMesh(const Point : TVector) : boolean;
         function OctreeAABBIntersect(const AABB: TAABB; objMatrix,invObjMatrix: TMatrix; triangles:TAffineVectorList=nil): boolean;
//         TODO:  function OctreeSphereIntersect

         {: Octree support *experimental*.<p>
            Use only if you understand what you're doing! }
         property Octree : TOctree read GetOctree;
         procedure BuildOctree(TreeDepth: integer = 3);
                                                
      published
         { Published Declarations }
         property AutoCentering;
         property AutoScaling;
         property MaterialLibrary;
         property LightmapLibrary;
         property UseMeshMaterials;
         property NormalsOrientation;
   end;

   // TGLActorOption
   //
   {: Miscellanious actor options.<p>
      <ul>
      <li>aoSkeletonNormalizeNormals : if set the normals of a skeleton-animated
          mesh will be normalized, this is not required if no normals-based texture
          coordinates generation occurs, and thus may be unset to improve performance.
      </ul> }
   TGLActorOption = (aoSkeletonNormalizeNormals);
   TGLActorOptions = set of TGLActorOption;

const
   cDefaultGLActorOptions = [aoSkeletonNormalizeNormals];

type

   TGLActor = class;

   // TActorAnimationReference
   //
   TActorAnimationReference = (aarMorph, aarSkeleton, aarNone);

	// TActorAnimation
	//
   {: An actor animation sequence.<p>
      An animation sequence is a named set of contiguous frames that can be used
      for animating an actor. The referred frames can be either morph or skeletal
      frames (choose which via the Reference property).<p>
      An animation can be directly "played" by the actor by selecting it with
      SwitchAnimation, and can also be "blended" via a TGLAnimationControler. }
	TActorAnimation = class (TCollectionItem)
	   private
	      { Private Declarations }
         FName : String;
         FStartFrame : Integer;
         FEndFrame : Integer;
         FReference : TActorAnimationReference;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         function FrameCount : Integer;
         procedure SetStartFrame(const val : Integer);
         procedure SetEndFrame(const val : Integer);
         procedure SetReference(val : TActorAnimationReference);
         procedure SetAsString(const val : String);
         function GetAsString : String;

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         property AsString : String read GetAsString write SetAsString;

         function OwnerActor : TGLActor;
         
         {: Linearly removes the translation component between skeletal frames.<p>
            This function will compute the translation of the first bone (index 0)
            and linearly subtract this translation in all frames between startFrame
            and endFrame. Its purpose is essentially to remove the 'slide' that
            exists in some animation formats (f.i. SMD). }
         procedure MakeSkeletalTranslationStatic;
         {: Removes the absolute rotation component of the skeletal frames.<p>
            Some formats will store frames with absolute rotation information,
            if this correct if the animation is the "main" animation.<br>
            This function removes that absolute information, making the animation
            frames suitable for blending purposes. }
         procedure MakeSkeletalRotationDelta;

	   published
	      { Published Declarations }
         property Name : String read FName write FName;
         {: Index of the initial frame of the animation. }
         property StartFrame : Integer read FStartFrame write SetStartFrame;
         {: Index of the final frame of the animation. }
         property EndFrame : Integer read FEndFrame write SetEndFrame;
         {: Indicates if this is a skeletal or a morph-based animation. }
         property Reference : TActorAnimationReference read FReference write SetReference default aarMorph;
	end;

   TActorAnimationName = String;

	// TActorAnimations
	//
   {: Collection of actor animations sequences. }
	TActorAnimations = class (TCollection)
	   private
	      { Private Declarations }
	      FOwner : TGLActor;

	   protected
	      { Protected Declarations }
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TActorAnimation);
	      function GetItems(index : Integer) : TActorAnimation;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TGLActor);
         function Add: TActorAnimation;
	      function FindItemID(ID: Integer): TActorAnimation;
	      function FindName(const aName : String) : TActorAnimation;
         function FindFrame(aFrame : Integer; aReference : TActorAnimationReference) : TActorAnimation;

	      procedure SetToStrings(aStrings : TStrings);
         procedure SaveToStream(aStream : TStream);
         procedure LoadFromStream(aStream : TStream);
         procedure SaveToFile(const fileName : String);
         procedure LoadFromFile(const fileName : String);

	      property Items[index : Integer] : TActorAnimation read GetItems write SetItems; default;
         function Last : TActorAnimation;
   end;

	// TGLBaseAnimationControler
	//
   {: Base class for skeletal animation control.<p> }
   TGLBaseAnimationControler = class (TComponent)
	   private
	      { Private Declarations }
         FEnabled : Boolean;
         FActor : TGLActor;

	   protected
	      { Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetEnabled(const val : Boolean);
         procedure SetActor(const val : TGLActor);

         procedure DoChange; virtual;
         function Apply(var lerpInfo : TBlendedLerpInfo) : Boolean; virtual;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         { Published Declarations }
         property Enabled : Boolean read FEnabled write SetEnabled default True;
         property Actor : TGLActor read FActor write SetActor;
	end;

	// TGLAnimationControler
	//
   {: Controls the blending of an additionnal skeletal animation into an actor.<p>
      The animation controler allows animating an actor with several animations
      at a time, for instance, you could use a "run" animation as base animation
      (in TGLActor), blend an animation that makes the arms move differently
      depending on what the actor is carrying, along with an animation that will
      make the head turn toward a target. }
   TGLAnimationControler = class (TGLBaseAnimationControler)
	   private
	      { Private Declarations }
         FAnimationName : TActorAnimationName;
         FRatio : Single;

	   protected
	      { Protected Declarations }
         procedure SetAnimationName(const val : TActorAnimationName);
         procedure SetRatio(const val : Single);

         procedure DoChange; override;
         function Apply(var lerpInfo : TBlendedLerpInfo) : Boolean; override;

      published
         { Published Declarations }
         property AnimationName : String read FAnimationName write SetAnimationName;
         property Ratio : Single read FRatio write SetRatio;
	end;

   // TActorFrameInterpolation
   //
   {: Actor frame-interpolation mode.<p>
      - afpNone : no interpolation, display CurrentFrame only<br>
      - afpLinear : perform linear interpolation between current and next frame }
   TActorFrameInterpolation = (afpNone, afpLinear);

   // TActorActionMode
   //
   {: Defines how an actor plays between its StartFrame and EndFrame.<p>
      <ul>
      <li>aamNone : no animation is performed
      <li>aamPlayOnce : play from current frame to EndFrame, once end frame has
         been reached, switches to aamNone
      <li>aamLoop : play from current frame to EndFrame, once end frame has
         been reached, sets CurrentFrame to StartFrame
      <li>aamBounceForward : play from current frame to EndFrame, once end frame
         has been reached, switches to aamBounceBackward
      <li>aamBounceBackward : play from current frame to StartFrame, once start
         frame has been reached, switches to aamBounceForward
      <li>aamExternal : Allows for external animation control
      </ul> }
   TActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward,
                          aamBounceBackward,aamLoopBackward, aamExternal);

   // TGLActor
   //
   {: Mesh class specialized in animated meshes.<p>
      The TGLActor provides a quick interface to animated meshes based on morph
      or skeleton frames, it is capable of performing frame interpolation and
      animation blending (via TGLAnimationControler components). }
   TGLActor = class (TGLBaseMesh)
      private
         { Private Declarations }
         FStartFrame, FEndFrame : Integer;
         FReference : TActorAnimationReference;
         FCurrentFrame : Integer;
         FCurrentFrameDelta : Single;
         FFrameInterpolation : TActorFrameInterpolation;
         FInterval : Integer;
         FAnimationMode : TActorAnimationMode;
         FOnFrameChanged : TNotifyEvent;
         FOnEndFrameReached, FOnStartFrameReached : TNotifyEvent;
         FAnimations : TActorAnimations;
         FTargetSmoothAnimation : TActorAnimation;
         FControlers : TList;
         FOptions : TGLActorOptions;

      protected
         { Protected Declarations }
         procedure SetCurrentFrame(val : Integer);
         procedure SetStartFrame(val : Integer);
         procedure SetEndFrame(val : Integer);
         procedure SetReference(val : TActorAnimationReference);
         procedure SetAnimations(const val : TActorAnimations);
         function  StoreAnimations : Boolean;
         procedure SetOptions(const val : TGLActorOptions);

         procedure PrepareMesh; override;
         procedure PrepareBuildList(var mrci : TRenderContextInfo); override;

         procedure RegisterControler(aControler : TGLBaseAnimationControler);
         procedure UnRegisterControler(aControler : TGLBaseAnimationControler);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;

			procedure DoProgress(const progressTime : TProgressTimes); override;

         procedure LoadFromStream(const filename : String; aStream : TStream); override;

	      procedure SwitchToAnimation(anAnimation : TActorAnimation; smooth : Boolean = False); overload;
	      procedure SwitchToAnimation(const animationName : String; smooth : Boolean = False); overload;
	      procedure SwitchToAnimation(animationIndex : Integer; smooth : Boolean = False); overload;
         function CurrentAnimation : String;

         {: Synchronize self animation with an other actor.<p>
            Copies Start/Current/End Frame values, CurrentFrameDelta,
            AnimationMode and FrameInterpolation. }
         procedure Synchronize(referenceActor : TGLActor);

         function  NextFrameIndex : Integer;

         procedure NextFrame(nbSteps : Integer = 1);
         procedure PrevFrame(nbSteps : Integer = 1);

         function FrameCount : Integer;

         {: Indicates whether the actor is currently swithing animations (with
            smooth interpolation).}
         function isSwitchingAnimation: boolean;

      published
         { Published Declarations }
         property StartFrame : Integer read FStartFrame write SetStartFrame default 0;
         property EndFrame : Integer read FEndFrame write SetEndFrame default 0;

         {: Reference Frame Animation mode.<p>
            Allows specifying if the model is primarily morph or skeleton based. }
         property Reference : TActorAnimationReference read FReference write FReference default aarMorph;

         {: Current animation frame. }
         property CurrentFrame : Integer read FCurrentFrame write SetCurrentFrame default 0;
         {: Value in the [0; 1] range expressing the delta to the next frame.<p> }
         property CurrentFrameDelta : Single read FCurrentFrameDelta write FCurrentFrameDelta;
         {: Frame interpolation mode (afpNone/afpLinear). }
         property FrameInterpolation : TActorFrameInterpolation read FFrameInterpolation write FFrameInterpolation default afpLinear;

         {: See TActorAnimationMode.<p> }
         property AnimationMode : TActorAnimationMode read FAnimationMode write FAnimationMode default aamNone;
         {: Interval between frames, in milliseconds. }
         property Interval : Integer read FInterval write FInterval;
         {: Actor and animation miscellanious options. }
         property Options : TGLActorOptions read FOptions write SetOptions default cDefaultGLActorOptions; 

         {: Triggered after each CurrentFrame change. }
         property OnFrameChanged : TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
         {: Triggered after EndFrame has been reached by progression or "nextframe" }
         property OnEndFrameReached : TNotifyEvent read FOnEndFrameReached write FOnEndFrameReached;
         {: Triggered after StartFrame has been reached by progression or "nextframe" }
         property OnStartFrameReached : TNotifyEvent read FOnStartFrameReached write FOnStartFrameReached;

         {: Collection of animations sequences. }
         property Animations : TActorAnimations read FAnimations write SetAnimations stored StoreAnimations;

         property AutoCentering;
         property MaterialLibrary;
         property LightmapLibrary;
         property UseMeshMaterials;
         property NormalsOrientation;
         property OverlaySkeleton;
   end;

   // TVectorFileFormat
   //
   TVectorFileFormat = class
      public
         VectorFileClass : TVectorFileClass;
         Extension       : String;
         Description     : String;
         DescResID       : Integer;
   end;

   // TVectorFileFormatsList
   //
   {: Stores registered vector file formats. }
   TVectorFileFormatsList = class (TPersistentObjectList)
      public
         { Public Declarations }
         destructor Destroy; override;

         procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TVectorFileClass);
         function FindExt(ext : string) : TVectorFileClass;
         function FindFromFileName(const fileName : String) : TVectorFileClass;
         procedure Remove(AClass: TVectorFileClass);
         procedure BuildFilterStrings(vectorFileClass : TVectorFileClass;
                                      var descriptions, filters : String;
                                      formatsThatCanBeOpened : Boolean = True;
                                      formatsThatCanBeSaved : Boolean = False);
         function FindExtByIndex(index : Integer;
                                 formatsThatCanBeOpened : Boolean = True;
                                 formatsThatCanBeSaved : Boolean = False) : String;
   end;

   EInvalidVectorFile = class(Exception);

//: Read access to the list of registered vector file formats
function GetVectorFileFormats : TVectorFileFormatsList;
//: A file extension filter suitable for dialog's 'Filter' property
function VectorFileFormatsFilter : String;
//: A file extension filter suitable for a savedialog's 'Filter' property
function VectorFileFormatsSaveFilter : String;
{: Returns an extension by its index in the vector files dialogs filter.<p>
   Use VectorFileFormatsFilter to obtain the filter. }
function VectorFileFormatExtensionByIndex(index : Integer) : String;

procedure RegisterVectorFileFormat(const aExtension, aDescription: String;
                                   aClass : TVectorFileClass);
procedure UnregisterVectorFileClass(aClass : TVectorFileClass);


var
   vGLVectorFileObjectsAllocateMaterials : Boolean = True; // Mrqzzz : Flag to avoid loading materials (useful for IDE Extentions or scene editors)

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLStrings, XOpenGL, GLCrossPlatform, MeshUtils, GLState, GLUtils,
  GLBaseMeshSilhouette;

var
   vVectorFileFormats : TVectorFileFormatsList;
   vNextRenderGroupID : Integer = 1;

const
   cAAFHeader = 'AAF';

// GetVectorFileFormats
//
function GetVectorFileFormats: TVectorFileFormatsList;
begin
   if not Assigned(vVectorFileFormats)then
      vVectorFileFormats:=TVectorFileFormatsList.Create;
   Result:=vVectorFileFormats;
end;

// VectorFileFormatsFilter
//
function VectorFileFormatsFilter : String;
var
   f : String;
begin
   GetVectorFileFormats.BuildFilterStrings(TVectorFile, Result, f);
end;

// VectorFileFormatsSaveFilter
//
function VectorFileFormatsSaveFilter : String;
var
   f : String;
begin
   GetVectorFileFormats.BuildFilterStrings(TVectorFile, Result, f, False, True);
end;

// RegisterVectorFileFormat
//
procedure RegisterVectorFileFormat(const AExtension, ADescription: String; AClass: TVectorFileClass);
begin
   RegisterClass(AClass);
	GetVectorFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterVectorFileClass
//
procedure UnregisterVectorFileClass(AClass: TVectorFileClass);
begin
	if Assigned(vVectorFileFormats) then
		vVectorFileFormats.Remove(AClass);
end;

// VectorFileFormatExtensionByIndex
//
function VectorFileFormatExtensionByIndex(index : Integer) : String;
begin
   Result:=GetVectorFileFormats.FindExtByIndex(index);
end;

// TVectorFileFormatsList.Destroy
//
destructor TVectorFileFormatsList.Destroy;
begin
   Clean;
   inherited;
end;

// Add
//
procedure TVectorFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
                                     AClass: TVectorFileClass);
var
   newRec : TVectorFileFormat;
begin
   newRec:=TVectorFileFormat.Create;
   with newRec do begin
      Extension:=AnsiLowerCase(Ext);
      VectorFileClass:=AClass;
      Description:=Desc;
      DescResID:=DescID;
   end;
   inherited Add(newRec);
end;

// FindExt
//
function TVectorFileFormatsList.FindExt(ext : String) : TVectorFileClass;
var
   i : Integer;
begin
   ext:=AnsiLowerCase(ext);
   for i:=Count-1 downto 0 do with TVectorFileFormat(Items[I]) do begin
      if Extension=ext then begin
         Result:=VectorFileClass;
         Exit;
      end;
   end;
   Result:=nil;
end;

// FindFromFileName
//
function TVectorFileFormatsList.FindFromFileName(const fileName : String) : TVectorFileClass;
var
   ext : String;
begin
   ext:=ExtractFileExt(Filename);
   System.Delete(ext, 1, 1);
   Result:=FindExt(ext);
   if not Assigned(Result) then
      raise EInvalidVectorFile.CreateFmt(glsUnknownExtension,
                                         [ext, 'GLFile'+UpperCase(ext)]);
end;

// Remove
//
procedure TVectorFileFormatsList.Remove(AClass: TVectorFileClass);
var
   i : Integer;
begin
   for i:=Count-1 downto 0 do begin
      if TVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
         DeleteAndFree(i);
   end;
end;

// BuildFilterStrings
//
procedure TVectorFileFormatsList.BuildFilterStrings(
                                       vectorFileClass : TVectorFileClass;
                                       var descriptions, filters : String;
                                       formatsThatCanBeOpened : Boolean = True;
                                       formatsThatCanBeSaved : Boolean = False);
var
   k, i : Integer;
   p : TVectorFileFormat;
begin
   descriptions:='';
   filters:='';
   k:=0;
   for i:=0 to Count-1 do begin
      p:=TVectorFileFormat(Items[i]);
      if     p.VectorFileClass.InheritsFrom(vectorFileClass) and (p.Extension<>'')
         and (   (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities))
              or (formatsThatCanBeSaved and (dfcWrite in p.VectorFileClass.Capabilities))) then begin
         with p do begin
            if k<>0 then begin
               descriptions:=descriptions+'|';
               filters:=filters+';';
            end;
            if (Description='') and (DescResID<>0) then
               Description:=LoadStr(DescResID);
            FmtStr(descriptions, '%s%s (*.%s)|*.%2:s',
                   [descriptions, Description, Extension]);
            filters:=filters+'*.'+Extension;
            Inc(k);
         end;
      end;
   end;
   if (k>1) and (not formatsThatCanBeSaved) then
      FmtStr(descriptions, '%s (%s)|%1:s|%s',
             [glsAllFilter, filters, descriptions]);
end;

// FindExtByIndex
//
function TVectorFileFormatsList.FindExtByIndex(index : Integer;
                                       formatsThatCanBeOpened : Boolean = True;
                                       formatsThatCanBeSaved : Boolean = False) : String;
var
   i : Integer;
   p : TVectorFileFormat;
begin
   Result:='';
   if index>0 then begin
      for i:=0 to Count-1 do begin
         p:=TVectorFileFormat(Items[i]);
         if    (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities))
            or (formatsThatCanBeSaved and (dfcWrite in p.VectorFileClass.Capabilities)) then begin
            if index=1 then begin
               Result:=p.Extension;
               Break;
            end else Dec(index);
         end;
      end;
   end;
end;

// ------------------
// ------------------ TBaseMeshObject ------------------
// ------------------

// Create
//
constructor TBaseMeshObject.Create;
begin
   FVertices:=TAffineVectorList.Create;
   FNormals:=TAffineVectorList.Create;
   FVisible:=True;
   inherited Create;
end;

// Destroy
//
destructor TBaseMeshObject.Destroy;
begin
   FNormals.Free;
   FVertices.Free;
   inherited;
end;

// WriteToFiler
//
procedure TBaseMeshObject.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(1);  // Archive Version 1, added FVisible
      WriteString(FName);
      FVertices.WriteToFiler(writer);
      FNormals.WriteToFiler(writer);
      WriteBoolean(FVisible);
   end;
end;

// ReadFromFiler
//
procedure TBaseMeshObject.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion in [0..1] then with reader do begin
      FName:=ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
      if archiveVersion>=1 then
         FVisible:=ReadBoolean
      else FVisible:=True;
   end else RaiseFilerException(archiveVersion);
end;

// Clear
//
procedure TBaseMeshObject.Clear;
begin
   FNormals.Clear;
   FVertices.Clear;
end;

// ContributeToBarycenter
//
procedure TBaseMeshObject.ContributeToBarycenter(var currentSum : TAffineVector;
                                                 var nb : Integer);
begin
   AddVector(currentSum, FVertices.Sum);
   nb:=nb+FVertices.Count;
end;

// Translate
//
procedure TBaseMeshObject.Translate(const delta : TAffineVector);
begin
   FVertices.Translate(delta);
end;

// BuildNormals
//
procedure TBaseMeshObject.BuildNormals(vertexIndices : TIntegerList; mode : TMeshObjectMode;
                                       normalIndices : TIntegerList = nil);
var
   i, base : Integer;
   n : TAffineVector;
   newNormals : TIntegerList;

   function TranslateNewNormal(vertexIndex : Integer; const delta :
       TAffineVector): Integer;
   var
      pv : PAffineVector;
   begin
      result := newNormals[vertexIndex];
      if result < base then begin
         result := Normals.Add(NullVector);
         newNormals[vertexIndex]:=result;
      end;
      pv:=@Normals.List[result];
      AddVector(pv^, delta);
   end;

begin
   if not Assigned(normalIndices) then begin
      // build bijection
      Normals.Clear;
      Normals.Count:=Vertices.Count;
      case mode of
         momTriangles : begin
            i:=0; while i<=vertexIndices.Count-3 do with Normals do begin
               with Vertices do begin
                  CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                  Items[vertexIndices[i+2]], n);
               end;
               with Normals do begin
                  TranslateItem(vertexIndices[i+0], n);
                  TranslateItem(vertexIndices[i+1], n);
                  TranslateItem(vertexIndices[i+2], n);
               end;
               Inc(i, 3);
            end;
         end;
         momTriangleStrip : begin
            i:=0; while i<=vertexIndices.Count-3 do with Normals do begin
               with Vertices do begin
                  if (i and 1)=0 then
                     CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                     Items[vertexIndices[i+2]], n)
                  else CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+2]],
                                       Items[vertexIndices[i+1]], n);
               end;
               with Normals do begin
                  TranslateItem(vertexIndices[i+0], n);
                  TranslateItem(vertexIndices[i+1], n);
                  TranslateItem(vertexIndices[i+2], n);
               end;
               Inc(i, 1);
            end;
         end;
      else
         Assert(False);
      end;
      Normals.Normalize;
   end else begin
      // add new normals
      base:=Normals.Count;
      newNormals:=TIntegerList.Create;
      newNormals.AddSerie(-1, 0, Vertices.Count);
      case mode of
         momTriangles : begin
            i:=0; while i<=vertexIndices.Count-3 do begin
               with Vertices do begin
                  CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                  Items[vertexIndices[i+2]], n);
               end;
               normalIndices.Add(TranslateNewNormal(vertexIndices[i+0], n));
               normalIndices.Add(TranslateNewNormal(vertexIndices[i+1], n));
               normalIndices.Add(TranslateNewNormal(vertexIndices[i+2], n));
               Inc(i, 3);
            end;
         end;
         momTriangleStrip : begin
            i:=0; while i<=vertexIndices.Count-3 do begin
               with Vertices do begin
                  if (i and 1)=0 then
                     CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                     Items[vertexIndices[i+2]], n)
                  else CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+2]],
                                       Items[vertexIndices[i+1]], n);
               end;
               normalIndices.Add(TranslateNewNormal(vertexIndices[i+0], n));
               normalIndices.Add(TranslateNewNormal(vertexIndices[i+1], n));
               normalIndices.Add(TranslateNewNormal(vertexIndices[i+2], n));
               Inc(i, 1);
            end;
         end;
      else
         Assert(False);
      end;
      for i:=base to Normals.Count-1 do
         NormalizeVector(Normals.List[i]);
      newNormals.Free;
   end;
end;


// ExtractTriangles
//
function TBaseMeshObject.ExtractTriangles(texCoords : TAffineVectorList = nil;
                                          normals : TAffineVectorList = nil) : TAffineVectorList;
begin
   Result:=TAffineVectorList.Create;
   if (Vertices.Count mod 3)=0 then begin
      Result.Assign(Vertices);
      if Assigned(normals) then
         normals.Assign(Self.Normals);
   end;
end;

// SetVertices
//
procedure TBaseMeshObject.SetVertices(const val : TAffineVectorList);
begin
   FVertices.Assign(val);
end;

// SetNormals
//
procedure TBaseMeshObject.SetNormals(const val : TAffineVectorList);
begin
   FNormals.Assign(val);
end;

// ------------------
// ------------------ TSkeletonFrame ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonFrame.CreateOwned(aOwner : TSkeletonFrameList);
begin
   FOwner:=aOwner;
   aOwner.Add(Self);
   Create;
end;

// Create
//
constructor TSkeletonFrame.Create;
begin
	inherited Create;
   FPosition:=TAffineVectorList.Create;
   FRotation:=TAffineVectorList.Create;
   FQuaternion:=TQuaternionList.Create;
   FTransformMode:=sftRotation;
end;

// Destroy
//
destructor TSkeletonFrame.Destroy;
begin
   FlushLocalMatrixList;
   FRotation.Free;
   FPosition.Free;
   FQuaternion.Free;
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeletonFrame.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(1); // Archive Version 1
      WriteString(FName);
      FPosition.WriteToFiler(writer);
      FRotation.WriteToFiler(writer);
      FQuaternion.WriteToFiler(writer);
      WriteInteger(Integer(FTransformMode));
   end;
end;

// ReadFromFiler
//
procedure TSkeletonFrame.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if (archiveVersion=0) or (archiveVersion=1) then with reader do begin
      FName:=ReadString;
      FPosition.ReadFromFiler(reader);
      FRotation.ReadFromFiler(reader);
      if (archiveVersion=1) then begin
        FQuaternion.ReadFromFiler(reader);
        FTransformMode:=TSkeletonFrameTransform(ReadInteger);
      end;
	end else RaiseFilerException(archiveVersion);
   FlushLocalMatrixList;
end;

// SetPosition
//
procedure TSkeletonFrame.SetPosition(const val : TAffineVectorList);
begin
   FPosition.Assign(val);
end;

// SetRotation
//
procedure TSkeletonFrame.SetRotation(const val : TAffineVectorList);
begin
   FRotation.Assign(val);
end;

// SetQuaternion
//
procedure TSkeletonFrame.SetQuaternion(const val : TQuaternionList);
begin
   FQuaternion.Assign(val);
end;

// LocalMatrixList
//
function TSkeletonFrame.LocalMatrixList : PMatrixArray;
var
   i : Integer;
   s, c : Single;
   mat, rmat : TMatrix;
   quat : TQuaternion;
begin
   if not Assigned(FLocalMatrixList) then begin
      case FTransformMode of
         sftRotation : begin
            FLocalMatrixList:=AllocMem(SizeOf(TMatrix)*Rotation.Count);
            for i:=0 to Rotation.Count-1 do begin
               if Rotation[i][0]<>0 then begin
                  SinCos(Rotation[i][0], s, c);
                  mat:=CreateRotationMatrixX(s, c);
               end else mat:=IdentityHmgMatrix;
               if Rotation[i][1]<>0 then begin
                  SinCos(Rotation[i][1], s, c);
                  rmat:=CreateRotationMatrixY(s, c);
                  mat:=MatrixMultiply(mat, rmat);
               end;
               if Rotation[i][2]<>0 then begin
                  SinCos(Rotation[i][2], s, c);
                  rmat:=CreateRotationMatrixZ(s, c);
                  mat:=MatrixMultiply(mat, rmat);
               end;
               mat[3][0]:=Position[i][0];
               mat[3][1]:=Position[i][1];
               mat[3][2]:=Position[i][2];
               FLocalMatrixList[i]:=mat;
            end;
         end;
         sftQuaternion : begin
            FLocalMatrixList:=AllocMem(SizeOf(TMatrix)*Quaternion.Count);
            for i:=0 to Quaternion.Count-1 do begin
               quat:=Quaternion[i];
               mat:=QuaternionToMatrix(quat);
               mat[3][0]:=Position[i][0];
               mat[3][1]:=Position[i][1];
               mat[3][2]:=Position[i][2];
               mat[3][3]:=1;
               FLocalMatrixList[i]:=mat;
            end;
         end;
      end;
   end;
   Result:=FLocalMatrixList;
end;

// FlushLocalMatrixList
//
procedure TSkeletonFrame.FlushLocalMatrixList;
begin
   if Assigned(FLocalMatrixList) then begin
      FreeMem(FLocalMatrixList);
      FLocalMatrixList:=nil;
   end;
end;

// ConvertQuaternionsToRotations
//
procedure TSkeletonFrame.ConvertQuaternionsToRotations(KeepQuaternions : Boolean = True);
var
  i : integer;
  t : TTransformations;
  m : TMatrix;
begin
  Rotation.Clear;
  for i:=0 to Quaternion.Count-1 do begin
    m:=QuaternionToMatrix(Quaternion[i]);
    if MatrixDecompose(m,t) then
      Rotation.Add(t[ttRotateX],t[ttRotateY],t[ttRotateZ])
    else
      Rotation.Add(NullVector);
  end;
  if not KeepQuaternions then
    Quaternion.Clear;
end;

// ConvertRotationsToQuaternions
//
procedure TSkeletonFrame.ConvertRotationsToQuaternions(KeepRotations : Boolean = True);
var
  i : integer;
  mat,rmat : TMatrix;
  s,c : Single;
begin
  Quaternion.Clear;
  for i:= 0 to Rotation.Count-1 do begin
    mat:=IdentityHmgMatrix;
    SinCos(Rotation[i][0], s, c);
    rmat:=CreateRotationMatrixX(s, c);
    mat:=MatrixMultiply(mat, rmat);
    SinCos(Rotation[i][1], s, c);
    rmat:=CreateRotationMatrixY(s, c);
    mat:=MatrixMultiply(mat, rmat);
    SinCos(Rotation[i][2], s, c);
    rmat:=CreateRotationMatrixZ(s, c);
    mat:=MatrixMultiply(mat, rmat);
    Quaternion.Add(QuaternionFromMatrix(mat));
  end;
  if not KeepRotations then
    Rotation.Clear;
end;

// ------------------
// ------------------ TSkeletonFrameList ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonFrameList.CreateOwned(AOwner : TPersistent);
begin
   FOwner:=AOwner;
   Create;
end;

// Destroy
//
destructor TSkeletonFrameList.Destroy;
begin
   Clear;
   inherited;
end;

// ReadFromFiler
//
procedure TSkeletonFrameList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// Clear
//
procedure TSkeletonFrameList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetSkeletonFrame
//
function TSkeletonFrameList.GetSkeletonFrame(Index: Integer): TSkeletonFrame;
begin
   Result:=TSkeletonFrame(List^[Index]);
end;

// ConvertQuaternionsToRotations
//
procedure TSkeletonFrameList.ConvertQuaternionsToRotations(KeepQuaternions : Boolean = True; SetTransformMode : Boolean = True);
var
  i : integer;
begin
  for i:=0 to Count-1 do begin
    Items[i].ConvertQuaternionsToRotations(KeepQuaternions);
    if SetTransformMode then
      Items[i].TransformMode:=sftRotation;
  end;
end;

// ConvertRotationsToQuaternions
//
procedure TSkeletonFrameList.ConvertRotationsToQuaternions(KeepRotations : Boolean = True; SetTransformMode : Boolean = True);
var
  i : integer;
begin
  for i:=0 to Count-1 do begin
    Items[i].ConvertRotationsToQuaternions(KeepRotations);
    if SetTransformMode then
      Items[i].TransformMode:=sftQuaternion;
  end;
end;

// ------------------
// ------------------ TSkeletonBoneList ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonBoneList.CreateOwned(aOwner : TSkeleton);
begin
   FSkeleton:=aOwner;
	Create;
end;

// Create
//
constructor TSkeletonBoneList.Create;
begin
	inherited;
   FGlobalMatrix:=IdentityHmgMatrix;
end;

// Destroy
//
destructor TSkeletonBoneList.Destroy;
begin
   Clean;
	inherited;
end;

// WriteToFiler
//
procedure TSkeletonBoneList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      // nothing, yet
   end;
end;

// ReadFromFiler
//
procedure TSkeletonBoneList.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      // nothing, yet
	end else RaiseFilerException(archiveVersion);
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// AfterObjectCreatedByReader
//
procedure TSkeletonBoneList.AfterObjectCreatedByReader(Sender : TObject);
begin
   with (Sender as TSkeletonBone) do begin
      FOwner:=Self;
      FSkeleton:=Self.Skeleton;
   end;
end;

// GetSkeletonBone
//
function TSkeletonBoneList.GetSkeletonBone(Index : Integer) : TSkeletonBone;
begin
   Result:=TSkeletonBone(List^[Index]);
end;

// BoneByID
//
function TSkeletonBoneList.BoneByID(anID : Integer) : TSkeletonBone;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to Count-1 do begin
      Result:=Items[i].BoneByID(anID);
      if Assigned(Result) then Break;
   end;
end;

// BoneByName
//
function TSkeletonBoneList.BoneByName(const aName : String) : TSkeletonBone;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to Count-1 do begin
      Result:=Items[i].BoneByName(aName);
      if Assigned(Result) then Break;
   end;
end;

// BoneCount
//
function TSkeletonBoneList.BoneCount : Integer;
var
   i : Integer;
begin
   Result:=1;
   for i:=0 to Count-1 do
      Inc(Result, Items[i].BoneCount);
end;

// PrepareGlobalMatrices
//
procedure TSkeletonBoneList.PrepareGlobalMatrices;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].PrepareGlobalMatrices;
end;

// ------------------
// ------------------ TSkeletonRootBoneList ------------------
// ------------------

// WriteToFiler
//
procedure TSkeletonRootBoneList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      // nothing, yet
   end;
end;

// ReadFromFiler
//
procedure TSkeletonRootBoneList.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      // nothing, yet
	end else RaiseFilerException(archiveVersion);
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// BuildList
//
procedure TSkeletonRootBoneList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   // root node setups and restore OpenGL stuff
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_LIGHTING);
   glColor3f(1, 1, 1);
   // render root-bones
   for i:=0 to Count-1 do
      Items[i].BuildList(mrci);
   glPopAttrib;
end;

// ------------------
// ------------------ TSkeletonBone ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonBone.CreateOwned(aOwner : TSkeletonBoneList);
begin
   FOwner:=aOwner;
   aOwner.Add(Self);
   FSkeleton:=aOwner.Skeleton;
	Create;
end;

// Create
//
constructor TSkeletonBone.Create;
begin
   FColor:=$FFFFFFFF; // opaque white
	inherited;
end;

// Destroy
//
destructor TSkeletonBone.Destroy;
begin
   if Assigned(Owner) then
      Owner.Remove(Self);
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeletonBone.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteString(FName);
      WriteInteger(FBoneID);
      WriteInteger(FColor);
   end;
end;

// ReadFromFiler
//
procedure TSkeletonBone.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      FName:=ReadString;
      FBoneID:=ReadInteger;
      FColor:=ReadInteger;
	end else RaiseFilerException(archiveVersion);
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// BuildList
//
procedure TSkeletonBone.BuildList(var mrci : TRenderContextInfo);

   procedure IssueColor(color : Cardinal);
   begin
      glColor4f(GetRValue(color)/255, GetGValue(color)/255, GetBValue(color)/255,
                ((color shr 24) and 255)/255);
   end;

var
   i : Integer;
begin
   // point for self
   glPointSize(5);
   glBegin(GL_POINTS);
      IssueColor(Color);
      glVertex3fv(@GlobalMatrix[3][0]);
   glEnd;
   glPointSize(1);
   // parent-self bone line
   if Owner is TSkeletonBone then begin
      glBegin(GL_LINES);
         glVertex3fv(@TSkeletonBone(Owner).GlobalMatrix[3][0]);
         glVertex3fv(@GlobalMatrix[3][0]);
      glEnd;
   end;
   // render sub-bones
   for i:=0 to Count-1 do
      Items[i].BuildList(mrci);
end;

// GetSkeletonBone
//
function TSkeletonBone.GetSkeletonBone(Index: Integer): TSkeletonBone;
begin
   Result:=TSkeletonBone(List^[Index]);
end;

// SetColor
//
procedure TSkeletonBone.SetColor(const val : Cardinal);
begin
   FColor:=val;
end;

// BoneByID
//
function TSkeletonBone.BoneByID(anID : Integer) : TSkeletonBone;
begin
   if BoneID=anID then
      Result:=Self
   else Result:=inherited BoneByID(anID);
end;

// BoneByName
//
function TSkeletonBone.BoneByName(const aName : String) : TSkeletonBone;
begin
   if Name=aName then
      Result:=Self
   else Result:=inherited BoneByName(aName);
end;

// Clean
//
procedure TSkeletonBone.Clean;
begin
   BoneID:=0;
   Name:='';
   inherited;
end;

// PrepareGlobalMatrices
//
procedure TSkeletonBone.PrepareGlobalMatrices;
begin
   FGlobalMatrix:=MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList[BoneID],
                                 TSkeletonBoneList(Owner).FGlobalMatrix);
   inherited;
end;

// ------------------
// ------------------ TSkeletonCollider ------------------
// ------------------

// Create
//
constructor TSkeletonCollider.Create;
begin
   inherited;
   FLocalMatrix:=IdentityHMGMatrix;
   FGlobalMatrix:=IdentityHMGMatrix;
   FAutoUpdate:=True;
end;

// CreateOwned
//
constructor TSkeletonCollider.CreateOwned(AOwner: TSkeletonColliderList);
begin
   Create;
   FOwner:=AOwner;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// WriteToFiler
//
procedure TSkeletonCollider.WriteToFiler(writer: TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      if Assigned(FBone) then
         WriteInteger(FBone.BoneID)
      else
         WriteInteger(-1);
      Write(FLocalMatrix,SizeOf(TMatrix));
   end;
end;

// ReadFromFiler
//
procedure TSkeletonCollider.ReadFromFiler(reader: TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FBoneID:=ReadInteger;
      Read(FLocalMatrix,SizeOf(TMatrix));
   end else RaiseFilerException(archiveVersion);
end;

// AlignCollider
//
procedure TSkeletonCollider.AlignCollider;
var
   mat : TMatrix;
begin
   if Assigned(FBone) then begin
      if Owner.Owner is TSkeleton then
         if TSkeleton(Owner.Owner).Owner is TGLBaseSceneObject then
            mat:=MatrixMultiply(FBone.GlobalMatrix,TGLBaseSceneObject(TSkeleton(Owner.Owner).Owner).AbsoluteMatrix)
         else
            mat:=FBone.GlobalMatrix;
         MatrixMultiply(FLocalMatrix,mat,FGlobalMatrix);
   end else
      FGlobalMatrix:=FLocalMatrix;
end;

// SetBone
//
procedure TSkeletonCollider.SetBone(const val: TSkeletonBone);
begin
   if val<>FBone then
      FBone:=val;
end;

// SetMatrix
//
procedure TSkeletonCollider.SetLocalMatrix(const val: TMatrix);
begin
   FLocalMatrix:=val;
end;

// ------------------
// ------------------ TSkeletonColliderList ------------------
// ------------------

// CreateOwned
//
constructor TSkeletonColliderList.CreateOwned(AOwner: TPersistent);
begin
   Create;
   FOwner:=AOwner;
end;

// Destroy
//
destructor TSkeletonColliderList.Destroy;
begin
   Clear;
   inherited;
end;

// GetSkeletonCollider
//
function TSkeletonColliderList.GetSkeletonCollider(index : Integer): TSkeletonCollider;
begin
   Result:=TSkeletonCollider(inherited Get(index));
end;

// ReadFromFiler
//
procedure TSkeletonColliderList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do begin
      Items[i].FOwner:=Self;
      if (Owner is TSkeleton) and (Items[i].FBoneID<>-1) then
         Items[i].Bone:=TSkeleton(Owner).BoneByID(Items[i].FBoneID);
   end;
end;

// Clear
//
procedure TSkeletonColliderList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Items[i].FOwner:=nil;
      Items[i].Free;
   end;
   inherited;
end;

// AlignColliders
//
procedure TSkeletonColliderList.AlignColliders;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if Items[i].AutoUpdate then
         Items[i].AlignCollider;
end;

// ------------------
// ------------------ TSkeleton ------------------
// ------------------

// CreateOwned
//
constructor TSkeleton.CreateOwned(AOwner : TGLBaseMesh);
begin
   FOwner:=aOwner;
   Create;
end;

// Create
//
constructor TSkeleton.Create;
begin
	inherited Create;
   FRootBones:=TSkeletonRootBoneList.CreateOwned(Self);
   FFrames:=TSkeletonFrameList.CreateOwned(Self);
   FColliders:=TSkeletonColliderList.CreateOwned(Self);
end;

// Destroy
//
destructor TSkeleton.Destroy;
begin
   FlushBoneByIDCache;
   FCurrentFrame.Free;
   FFrames.Free;
   FRootBones.Free;
   FColliders.Free;
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeleton.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      if FColliders.Count > 0 then
        WriteInteger(1) // Archive Version 1 : with colliders
      else
        WriteInteger(0); // Archive Version 0
      FRootBones.WriteToFiler(writer);
      FFrames.WriteToFiler(writer);
      if FColliders.Count > 0 then
        FFrames.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TSkeleton.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if (archiveVersion=0) or (archiveVersion=1) then with reader do begin
      FRootBones.ReadFromFiler(reader);
      FFrames.ReadFromFiler(reader);
      if (archiveVersion=1) then 
        FColliders.ReadFromFiler(reader);
	end else RaiseFilerException(archiveVersion);
end;

// SetRootBones
//
procedure TSkeleton.SetRootBones(const val : TSkeletonRootBoneList);
begin
   FRootBones.Assign(val);
end;

// SetFrames
//
procedure TSkeleton.SetFrames(const val : TSkeletonFrameList);
begin
   FFrames.Assign(val);
end;

// GetCurrentFrame
//
function TSkeleton.GetCurrentFrame : TSkeletonFrame;
begin
   if not Assigned(FCurrentFrame) then
      FCurrentFrame:=TSkeletonFrame(FFrames.Items[0].CreateClone);
   Result:=FCurrentFrame;
end;

// SetCurrentFrame
//
procedure TSkeleton.SetCurrentFrame(val : TSkeletonFrame);
begin
   if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
   FCurrentFrame:=TSkeletonFrame(val.CreateClone);
end;

// SetColliders
//
procedure TSkeleton.SetColliders(const val : TSkeletonColliderList);
begin
   FColliders.Assign(val);
end;

// FlushBoneByIDCache
//
procedure TSkeleton.FlushBoneByIDCache;
begin
   FBonesByIDCache.Free;
   FBonesByIDCache:=nil;
end;

// BoneByID
//
function TSkeleton.BoneByID(anID : Integer) : TSkeletonBone;

   procedure CollectBones(bone : TSkeletonBone);
   var
      i : Integer;
   begin
      if bone.BoneID>=FBonesByIDCache.Count then
         FBonesByIDCache.Count:=bone.BoneID+1;
      FBonesByIDCache[bone.BoneID]:=bone;
      for i:=0 to bone.Count-1 do
         CollectBones(bone[i]);
   end;

var
   i : Integer;
begin
   if not Assigned(FBonesByIDCache) then begin
      FBonesByIDCache:=TList.Create;
      for i:=0 to RootBones.Count-1 do
         CollectBones(RootBones[i]);
   end;
   Result:=TSkeletonBone(FBonesByIDCache[anID])
end;

// BoneByName
//
function TSkeleton.BoneByName(const aName : String) : TSkeletonBone;
begin
   Result:=RootBones.BoneByName(aName);
end;

// BoneCount
//
function TSkeleton.BoneCount : Integer;
begin
   Result:=RootBones.BoneCount;
end;

// MorphTo
//
procedure TSkeleton.MorphTo(frameIndex : Integer);
begin
   CurrentFrame:=Frames[frameIndex];
end;

// MorphTo
//
procedure TSkeleton.MorphTo(frame: TSkeletonFrame);
begin
   CurrentFrame:=frame;
end;

// Lerp
//
procedure TSkeleton.Lerp(frameIndex1, frameIndex2 : Integer; lerpFactor : Single);
begin
   if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
   FCurrentFrame:=TSkeletonFrame.Create;
   FCurrentFrame.TransformMode:=Frames[frameIndex1].TransformMode;
   with FCurrentFrame do begin
      Position.Lerp(Frames[frameIndex1].Position,
                    Frames[frameIndex2].Position, lerpFactor);
      case TransformMode of
         sftRotation   : Rotation.AngleLerp(Frames[frameIndex1].Rotation,
                                          Frames[frameIndex2].Rotation, lerpFactor);
         sftQuaternion : Quaternion.Lerp(Frames[frameIndex1].Quaternion,
                                         Frames[frameIndex2].Quaternion, lerpFactor);
      end;
   end;
end;

// BlendedLerps
//
procedure TSkeleton.BlendedLerps(const lerpInfos : array of TBlendedLerpInfo);
var
   i, n : Integer;
   blendPositions : TAffineVectorList;
   blendRotations : TAffineVectorList;
   blendQuaternions : TQuaternionList;
begin
   n:=High(lerpInfos)-Low(lerpInfos)+1;
   Assert(n>=1);
   i:=Low(lerpInfos);
   if n=1 then begin
      // use fast lerp (no blend)
      with lerpInfos[i] do
         Lerp(frameIndex1, frameIndex2, lerpFactor);
   end else begin
      if Assigned(FCurrentFrame) then
         FCurrentFrame.Free;
      FCurrentFrame:=TSkeletonFrame.Create;
      FCurrentFrame.TransformMode:=Frames[lerpInfos[i].frameIndex1].TransformMode;
      with FCurrentFrame do begin
         blendPositions:=TAffineVectorList.Create;
         // lerp first item separately
         Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
                       Frames[lerpInfos[i].frameIndex2].Position,
                       lerpInfos[i].lerpFactor);
         if lerpInfos[i].weight<>1 then
            Position.Scale(lerpInfos[i].weight);

         Inc(i);
         // combine the other items
         while i<=High(lerpInfos) do begin
            if not Assigned(lerpInfos[i].externalPositions) then begin
               blendPositions.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
                                   Frames[lerpInfos[i].frameIndex2].Position,
                                   lerpInfos[i].lerpFactor);
               Position.AngleCombine(blendPositions, 1);
            end else
               Position.Combine(lerpInfos[i].externalPositions, 1);
            Inc(i);
         end;
         blendPositions.Free;

         i:=Low(lerpInfos);
         case TransformMode of
            sftRotation : begin
               blendRotations:=TAffineVectorList.Create;
               // lerp first item separately
               Rotation.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
                                  Frames[lerpInfos[i].frameIndex2].Rotation,
                                  lerpInfos[i].lerpFactor);
               Inc(i);
               // combine the other items
               while i<=High(lerpInfos) do begin
                  if not Assigned(lerpInfos[i].externalRotations) then begin
                     blendRotations.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
                                              Frames[lerpInfos[i].frameIndex2].Rotation,
                                              lerpInfos[i].lerpFactor);
                     Rotation.AngleCombine(blendRotations, 1);
                  end else
                     Rotation.AngleCombine(lerpInfos[i].externalRotations, 1);
                  Inc(i);
               end;
               blendRotations.Free;
            end;

            sftQuaternion : begin
               blendQuaternions:=TQuaternionList.Create;
               // Initial frame lerp
               Quaternion.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
                               Frames[lerpInfos[i].frameIndex2].Quaternion,
                               lerpInfos[i].lerpFactor);
               Inc(i);
               // Combine the lerped frames together
               while i<=High(lerpInfos) do begin
                  if not Assigned(lerpInfos[i].externalQuaternions) then begin
                     blendQuaternions.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
                                           Frames[lerpInfos[i].frameIndex2].Quaternion,
                                           lerpInfos[i].lerpFactor);
                     Quaternion.Combine(blendQuaternions, 1);
                  end else
                     Quaternion.Combine(lerpInfos[i].externalQuaternions, 1);
                  Inc(i);
               end;
               blendQuaternions.Free;
            end;
         end;
      end;
   end;
end;

// MakeSkeletalTranslationStatic
//
procedure TSkeleton.MakeSkeletalTranslationStatic(startFrame, endFrame : Integer);
var
   delta : TAffineVector;
   i : Integer;
   f : Single;
begin
   if endFrame<=startFrame then Exit;
   delta:=VectorSubtract(Frames[endFrame].Position[0], Frames[startFrame].Position[0]);
   f:=-1/(endFrame-startFrame);
   for i:=startFrame to endFrame do
      Frames[i].Position[0]:=VectorCombine(Frames[i].Position[0], delta,
                                           1, (i-startFrame)*f);
end;

// MakeSkeletalRotationDelta
//
procedure TSkeleton.MakeSkeletalRotationDelta(startFrame, endFrame : Integer);
var
   i, j : Integer;
   v : TAffineVector;
begin
   if endFrame<=startFrame then Exit;
   for i:=startFrame to endFrame do begin
      for j:=0 to Frames[i].Position.Count-1 do begin
         Frames[i].Position[j]:=NullVector;
         v:=VectorSubtract(Frames[i].Rotation[j],
                           Frames[0].Rotation[j]);
         if VectorNorm(v)<1e-6 then
            Frames[i].Rotation[j]:=NullVector
         else Frames[i].Rotation[j]:=v;
      end;
   end;
end;

// MorphMesh
//
procedure TSkeleton.MorphMesh(normalize : Boolean);
var
   i : Integer;
   mesh : TBaseMeshObject;
begin
   if Owner.MeshObjects.Count>0 then begin
      RootBones.PrepareGlobalMatrices;
      if Colliders.Count>0 then
         Colliders.AlignColliders;
      for i:=0 to Owner.MeshObjects.Count-1 do begin
         mesh:=Owner.MeshObjects.Items[i];
         if mesh is TSkeletonMeshObject then
            TSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end;
   end;
end;

// Synchronize
//
procedure TSkeleton.Synchronize(reference : TSkeleton);
begin
   CurrentFrame.Assign(reference.CurrentFrame);
   MorphMesh(True);
end;

// Clear
//
procedure TSkeleton.Clear;
begin
   FlushBoneByIDCache;
   RootBones.Clean;
   Frames.Clear;
   FCurrentFrame.Free;
   FCurrentFrame:=nil;
   FColliders.Clear;
end;

// ------------------
// ------------------ TMeshObject ------------------
// ------------------

// CreateOwned
//
constructor TMeshObject.CreateOwned(AOwner : TMeshObjectList);
begin
   FOwner:=AOwner;
   Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Create
//
constructor TMeshObject.Create;
begin
   FMode:=momTriangles;
   FTexCoords:=TAffineVectorList.Create;
   FLightMapTexCoords:=TTexPointList.Create;
   FColors:=TVectorList.Create;
   FFaceGroups:=TFaceGroups.CreateOwned(Self);
   FTexCoordsEx:=TList.Create;
   FTangentsTexCoordIndex:=2;
   FBinormalsTexCoordIndex:=3;
   inherited;
end;

// Destroy
//
destructor TMeshObject.Destroy;
var
   i : Integer;
begin
   FFaceGroups.Free;
   FColors.Free;
   FTexCoords.Free;
   FLightMapTexCoords.Free;
   for i:=0 to FTexCoordsEx.Count-1 do
      TVectorList(FTexCoordsEx[i]).Free;
   FTexCoordsEx.Free;
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// WriteToFiler
//
procedure TMeshObject.WriteToFiler(writer : TVirtualWriter);
var
   i : Integer;
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(2);        // Archive Version 2
      FTexCoords.WriteToFiler(writer);
      FLightMapTexCoords.WriteToFiler(writer);
      FColors.WriteToFiler(writer);
      FFaceGroups.WriteToFiler(writer);
      WriteInteger(Integer(FMode));
      WriteInteger(SizeOf(FRenderingOptions));
      Write(FRenderingOptions, SizeOf(FRenderingOptions));
      WriteInteger(FTexCoordsEx.Count);
      for i:=0 to FTexCoordsEx.Count-1 do
         TexCoordsEx[i].WriteToFiler(writer);
      WriteInteger(BinormalsTexCoordIndex);
      WriteInteger(TangentsTexCoordIndex);
   end;
end;

// ReadFromFiler
//
procedure TMeshObject.ReadFromFiler(reader : TVirtualReader);
var
   i, Count, archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion in [0..2] then with reader do begin
      FTexCoords.ReadFromFiler(reader);
      if archiveVersion>=1 then
         FLightMapTexCoords.ReadFromFiler(reader)
      else FLightMapTexCoords.Clear;
      FColors.ReadFromFiler(reader);
      FFaceGroups.ReadFromFiler(reader);
      FMode:=TMeshObjectMode(ReadInteger);
      if ReadInteger<>SizeOf(FRenderingOptions) then Assert(False);
      Read(FRenderingOptions, SizeOf(FRenderingOptions));
      if archiveVersion>=2 then begin
         Count:=ReadInteger;
         for i:=0 to Count-1 do
            TexCoordsEx[i].ReadFromFiler(reader);
         BinormalsTexCoordIndex:=ReadInteger;
         TangentsTexCoordIndex:=ReadInteger;
      end;
   end else RaiseFilerException(archiveVersion);
end;

// Clear;
//
procedure TMeshObject.Clear;
var
   i : Integer;
begin
   inherited;
   FFaceGroups.Clear;
   FColors.Clear;
   FTexCoords.Clear;
   FLightMapTexCoords.Clear;
   for i:=0 to FTexCoordsEx.Count-1 do
      TexCoordsEx[i].Clear;
end;

// ExtractTriangles
//
function TMeshObject.ExtractTriangles(texCoords : TAffineVectorList = nil;
                                      normals : TAffineVectorList = nil) : TAffineVectorList;
begin
   case mode of
      momTriangles : begin
         Result:=inherited ExtractTriangles;
         if Assigned(texCoords) then
            texCoords.Assign(Self.TexCoords);
         if Assigned(normals) then
            normals.Assign(Self.Normals);
      end;
      momTriangleStrip : begin
         Result:=TAffineVectorList.Create;
         ConvertStripToList(Vertices, Result);
         if Assigned(texCoords) then
            ConvertStripToList(Self.TexCoords, texCoords);
         if Assigned(normals) then
            ConvertStripToList(Self.Normals, normals);
      end;
      momFaceGroups : begin
         Result:=TAffineVectorList.Create;
         FaceGroups.AddToTriangles(Result, texCoords, normals);
      end;
   else
      Result:=nil;
      Assert(False);
   end;
end;

// TriangleCount
//
function TMeshObject.TriangleCount : Integer;
var
   i : Integer;
begin
   case Mode of
      momTriangles :
         Result:=(Vertices.Count div 3);
      momTriangleStrip : begin
         Result:=Vertices.Count-2;
         if Result<0 then Result:=0;
      end;
      momFaceGroups : begin
         Result:=0;
         for i:=0 to FaceGroups.Count-1 do
            Result:=Result+FaceGroups[i].TriangleCount;
      end;
   else
      Result:=0;
      Assert(False);
   end;
end;

// PrepareMaterialLibraryCache
//
procedure TMeshObject.PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
begin
   FaceGroups.PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//
procedure TMeshObject.DropMaterialLibraryCache;
begin
   FaceGroups.DropMaterialLibraryCache;
end;

// GetExtents
//
procedure TMeshObject.GetExtents(var min, max : TAffineVector);
begin
   FVertices.GetExtents(min, max);
end;

// Prepare
//
procedure TMeshObject.Prepare;
var
   i : Integer;
begin
   for i:=0 to FaceGroups.Count-1 do
      FaceGroups[i].Prepare;
end;

// PointInObject
//
function TMeshObject.PointInObject(const aPoint : TAffineVector) : Boolean;
var
   min, max : TAffineVector;
begin
   GetExtents(min, max);
   Result:=    (aPoint[0]>=min[0]) and (aPoint[1]>=min[1]) and (aPoint[2]>=min[2])
           and (aPoint[0]<=max[0]) and (aPoint[1]<=max[1]) and (aPoint[2]<=max[2]);
end;

// SetTexCoords
//
procedure TMeshObject.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

// SetLightmapTexCoords
//
procedure TMeshObject.SetLightmapTexCoords(const val : TTexPointList);
begin
   FLightMapTexCoords.Assign(val);
end;

// SetColors
//
procedure TMeshObject.SetColors(const val : TVectorList);
begin
   FColors.Assign(val);
end;

// SetTexCoordsEx
//
procedure TMeshObject.SetTexCoordsEx(index : Integer; const val : TVectorList);
begin
   TexCoordsEx[index].Assign(val);
end;

// GetTexCoordsEx
//
function TMeshObject.GetTexCoordsEx(index : Integer) : TVectorList;
var
   i : Integer;
begin
   if index>FTexCoordsEx.Count-1 then
      for i:=FTexCoordsEx.Count-1 to index do
         FTexCoordsEx.Add(TVectorList.Create);
   Result:=TVectorList(FTexCoordsEx[index]);
end;

// SetBinormals
//
procedure TMeshObject.SetBinormals(const val : TVectorList);
begin
   Binormals.Assign(val);
end;

// GetBinormals
//
function TMeshObject.GetBinormals : TVectorList;
begin
   Result:=TexCoordsEx[BinormalsTexCoordIndex];
end;

// SetBinormalsTexCoordIndex
//
procedure TMeshObject.SetBinormalsTexCoordIndex(const val : Integer);
begin
   Assert(val>=0);
   if val<>FBinormalsTexCoordIndex then begin
      FBinormalsTexCoordIndex:=val;
   end;
end;

// SetTangents
//
procedure TMeshObject.SetTangents(const val : TVectorList);
begin
   Tangents.Assign(val);
end;

// GetTangents
//
function TMeshObject.GetTangents : TVectorList;
begin
   Result:=TexCoordsEx[TangentsTexCoordIndex];
end;

// SetTangentsTexCoordIndex
//
procedure TMeshObject.SetTangentsTexCoordIndex(const val : Integer);
begin
   Assert(val>=0);
   if val<>FTangentsTexCoordIndex then begin
      FTangentsTexCoordIndex:=val;
   end;
end;

// GetTriangleData
//
procedure TMeshObject.GetTriangleData(tri : Integer; 
   list : TAffineVectorList; var v0, v1, v2 : TAffineVector);
var
   i, LastCount, Count : Integer;
   fg : TFGVertexIndexList;
begin
   case Mode of
      momTriangles : begin
         v0:=list[3*tri];
         v1:=list[3*tri+1];
         v2:=list[3*tri+2];
      end;
      momTriangleStrip : begin
         v0:=list[tri];
         v1:=list[tri+1];
         v2:=list[tri+2];
      end;
      momFaceGroups : begin
         Count:=0;
         for i:=0 to FaceGroups.Count-1 do begin
            LastCount:=Count;
            fg:=TFGVertexIndexList(FaceGroups[i]);
            Count:=Count+fg.TriangleCount;
            if Count>tri then begin
               Count:=tri-LastCount;
               case fg.Mode of
                  fgmmTriangles, fgmmFlatTriangles : begin
                     v0:=list[fg.VertexIndices[3*Count]];
                     v1:=list[fg.VertexIndices[3*Count+1]];
                     v2:=list[fg.VertexIndices[3*Count+2]];
                  end;
                  fgmmTriangleStrip : begin
                     v0:=list[fg.VertexIndices[Count]];
                     v1:=list[fg.VertexIndices[Count+1]];
                     v2:=list[fg.VertexIndices[Count+2]];
                  end;
                  fgmmTriangleFan : begin
                     v0:=list[fg.VertexIndices[0]];
                     v1:=list[fg.VertexIndices[Count+1]];
                     v2:=list[fg.VertexIndices[Count+2]];
                  end;
                  fgmmQuads : begin
                     if Count mod 2 = 0 then begin
                        v0:=list[fg.VertexIndices[4*(Count div 2)]];
                        v1:=list[fg.VertexIndices[4*(Count div 2)+1]];
                        v2:=list[fg.VertexIndices[4*(Count div 2)+2]];
                     end else begin
                        v0:=list[fg.VertexIndices[4*(Count div 2)]];
                        v1:=list[fg.VertexIndices[4*(Count div 2)+2]];
                        v2:=list[fg.VertexIndices[4*(Count div 2)+3]];
                     end;
                  end;
               else
                  Assert(False);
               end;
               break;
            end;
         end;
         
      end;
   else
      Assert(False);
   end;
end;

// GetTriangleData
//
procedure TMeshObject.GetTriangleData(tri : Integer; 
   list : TVectorList; var v0, v1, v2 : TVector);
var
   i, LastCount, Count : Integer;
   fg : TFGVertexIndexList;
begin
   case Mode of
      momTriangles : begin
         v0:=list[3*tri];
         v1:=list[3*tri+1];
         v2:=list[3*tri+2];
      end;
      momTriangleStrip : begin
         v0:=list[tri];
         v1:=list[tri+1];
         v2:=list[tri+2];
      end;
      momFaceGroups : begin
         Count:=0;
         for i:=0 to FaceGroups.Count-1 do begin
            LastCount:=Count;
            fg:=TFGVertexIndexList(FaceGroups[i]);
            Count:=Count+fg.TriangleCount;
            if Count>tri then begin
               Count:=tri-LastCount;
               case fg.Mode of
                  fgmmTriangles, fgmmFlatTriangles : begin
                     v0:=list[fg.VertexIndices[3*Count]];
                     v1:=list[fg.VertexIndices[3*Count+1]];
                     v2:=list[fg.VertexIndices[3*Count+2]];
                  end;
                  fgmmTriangleStrip : begin
                     v0:=list[fg.VertexIndices[Count]];
                     v1:=list[fg.VertexIndices[Count+1]];
                     v2:=list[fg.VertexIndices[Count+2]];
                  end;
                  fgmmTriangleFan : begin
                     v0:=list[fg.VertexIndices[0]];
                     v1:=list[fg.VertexIndices[Count+1]];
                     v2:=list[fg.VertexIndices[Count+2]];
                  end;
                  fgmmQuads : begin
                     if Count mod 2 = 0 then begin
                        v0:=list[fg.VertexIndices[4*(Count div 2)]];
                        v1:=list[fg.VertexIndices[4*(Count div 2)+1]];
                        v2:=list[fg.VertexIndices[4*(Count div 2)+2]];
                     end else begin
                        v0:=list[fg.VertexIndices[4*(Count div 2)]];
                        v1:=list[fg.VertexIndices[4*(Count div 2)+2]];
                        v2:=list[fg.VertexIndices[4*(Count div 2)+3]];
                     end;
                  end;
               else
                  Assert(False);
               end;
               break;
            end;
         end;
         
      end;
   else
      Assert(False);
   end;
end;

// SetTriangleData
//
procedure TMeshObject.SetTriangleData(tri : Integer; 
   list : TAffineVectorList; const v0, v1, v2 : TAffineVector);
var
   i, LastCount, Count : Integer;
   fg : TFGVertexIndexList;
begin
   case Mode of
      momTriangles : begin
         list[3*tri]:=v0;
         list[3*tri+1]:=v1;
         list[3*tri+2]:=v2;
      end;
      momTriangleStrip : begin
         list[tri]:=v0;
         list[tri+1]:=v1;
         list[tri+2]:=v2;
      end;
      momFaceGroups : begin
         Count:=0;
         for i:=0 to FaceGroups.Count-1 do begin
            LastCount:=Count;
            fg:=TFGVertexIndexList(FaceGroups[i]);
            Count:=Count+fg.TriangleCount;
            if Count>tri then begin
               Count:=tri-LastCount;
               case fg.Mode of
                  fgmmTriangles, fgmmFlatTriangles : begin
                     list[fg.VertexIndices[3*Count]]:=v0;
                     list[fg.VertexIndices[3*Count+1]]:=v1;
                     list[fg.VertexIndices[3*Count+2]]:=v2;
                  end;
                  fgmmTriangleStrip : begin
                     list[fg.VertexIndices[Count]]:=v0;
                     list[fg.VertexIndices[Count+1]]:=v1;
                     list[fg.VertexIndices[Count+2]]:=v2;
                  end;
                  fgmmTriangleFan : begin
                     list[fg.VertexIndices[0]]:=v0;
                     list[fg.VertexIndices[Count+1]]:=v1;
                     list[fg.VertexIndices[Count+2]]:=v2;
                  end;
                  fgmmQuads : begin
                     if Count mod 2 = 0 then begin
                        list[fg.VertexIndices[4*(Count div 2)]]:=v0;
                        list[fg.VertexIndices[4*(Count div 2)+1]]:=v1;
                        list[fg.VertexIndices[4*(Count div 2)+2]]:=v2;
                     end else begin
                        list[fg.VertexIndices[4*(Count div 2)]]:=v0;
                        list[fg.VertexIndices[4*(Count div 2)+2]]:=v1;
                        list[fg.VertexIndices[4*(Count div 2)+3]]:=v2;
                     end;
                  end;
               else
                  Assert(False);
               end;
               break;
            end;
         end;
         
      end;
   else
      Assert(False);
   end;
end;

// SetTriangleData
//
procedure TMeshObject.SetTriangleData(tri : Integer; 
   list : TVectorList; const v0, v1, v2 : TVector);
var
   i, LastCount, Count : Integer;
   fg : TFGVertexIndexList;
begin
   case Mode of
      momTriangles : begin
         list[3*tri]:=v0;
         list[3*tri+1]:=v1;
         list[3*tri+2]:=v2;
      end;
      momTriangleStrip : begin
         list[tri]:=v0;
         list[tri+1]:=v1;
         list[tri+2]:=v2;
      end;
      momFaceGroups : begin
         Count:=0;
         for i:=0 to FaceGroups.Count-1 do begin
            LastCount:=Count;
            fg:=TFGVertexIndexList(FaceGroups[i]);
            Count:=Count+fg.TriangleCount;
            if Count>tri then begin
               Count:=tri-LastCount;
               case fg.Mode of
                  fgmmTriangles, fgmmFlatTriangles : begin
                     list[fg.VertexIndices[3*Count]]:=v0;
                     list[fg.VertexIndices[3*Count+1]]:=v1;
                     list[fg.VertexIndices[3*Count+2]]:=v2;
                  end;
                  fgmmTriangleStrip : begin
                     list[fg.VertexIndices[Count]]:=v0;
                     list[fg.VertexIndices[Count+1]]:=v1;
                     list[fg.VertexIndices[Count+2]]:=v2;
                  end;
                  fgmmTriangleFan : begin
                     list[fg.VertexIndices[0]]:=v0;
                     list[fg.VertexIndices[Count+1]]:=v1;
                     list[fg.VertexIndices[Count+2]]:=v2;
                  end;
                  fgmmQuads : begin
                     if Count mod 2 = 0 then begin
                        list[fg.VertexIndices[4*(Count div 2)]]:=v0;
                        list[fg.VertexIndices[4*(Count div 2)+1]]:=v1;
                        list[fg.VertexIndices[4*(Count div 2)+2]]:=v2;
                     end else begin
                        list[fg.VertexIndices[4*(Count div 2)]]:=v0;
                        list[fg.VertexIndices[4*(Count div 2)+2]]:=v1;
                        list[fg.VertexIndices[4*(Count div 2)+3]]:=v2;
                     end;
                  end;
               else
                  Assert(False);
               end;
               break;
            end;
         end;
         
      end;
   else
      Assert(False);
   end;
end;

// BuildTangentSpace
//
procedure TMeshObject.BuildTangentSpace(
   buildBinormals : Boolean = True; 
   buildTangents : Boolean = True);
var
   i,j        : Integer;
   v,n,t      : array[0..2] of TAffineVector;
   tangent,
   binormal   : array[0..2] of TVector;
   vt,tt      : TAffineVector;
   interp,dot : Single;

   procedure SortVertexData(sortidx : Integer);
   begin
      if t[0][sortidx]<t[1][sortidx] then begin
         vt:=v[0];   tt:=t[0];
         v[0]:=v[1]; t[0]:=t[1];
         v[1]:=vt;   t[1]:=tt;
      end;
      if t[0][sortidx]<t[2][sortidx] then begin
         vt:=v[0];   tt:=t[0];
         v[0]:=v[2]; t[0]:=t[2];
         v[2]:=vt;   t[2]:=tt;
      end;
      if t[1][sortidx]<t[2][sortidx] then begin
         vt:=v[1];   tt:=t[1];
         v[1]:=v[2]; t[1]:=t[2];
         v[2]:=vt;   t[2]:=tt;
      end;
   end;

begin
   Tangents.Clear;
   Binormals.Clear;
   if buildTangents then Tangents.Count:=Vertices.Count;
   if buildBinormals then Binormals.Count:=Vertices.Count;
   for i:=0 to TriangleCount-1 do begin
      // Get triangle data
      GetTriangleData(i,Vertices,v[0],v[1],v[2]);
      GetTriangleData(i,Normals,n[0],n[1],n[2]);
      GetTriangleData(i,TexCoords,t[0],t[1],t[2]);

      for j:=0 to 2 do begin
         // Compute tangent
         if buildTangents then begin
            SortVertexData(1);

            if (t[2][1]-t[0][1]) = 0 then interp:=1
            else interp:=(t[1][1]-t[0][1])/(t[2][1]-t[0][1]);

            vt:=VectorLerp(v[0],v[2],interp);
            interp:=t[0][0]+(t[2][0]-t[0][0])*interp;
            vt:=VectorSubtract(vt,v[1]);
            if t[1][0]<interp then vt:=VectorNegate(vt);
            dot:=VectorDotProduct(vt,n[j]);
            vt[0]:=vt[0]-n[j][0]*dot;
            vt[1]:=vt[1]-n[j][1]*dot;
            vt[2]:=vt[2]-n[j][2]*dot;
            tangent[j]:=VectorMake(VectorNormalize(vt),0);
         end;

         // Compute Bi-Normal
         if buildBinormals then begin
            SortVertexData(0);

            if (t[2][0]-t[0][0]) = 0 then interp:=1
            else interp:=(t[1][0]-t[0][0])/(t[2][0]-t[0][0]);

            vt:=VectorLerp(v[0],v[2],interp);
            interp:=t[0][1]+(t[2][1]-t[0][1])*interp;
            vt:=VectorSubtract(vt,v[1]);
            if t[1][1]<interp then vt:=VectorNegate(vt);
            dot:=VectorDotProduct(vt,n[j]);
            vt[0]:=vt[0]-n[j][0]*dot;
            vt[1]:=vt[1]-n[j][1]*dot;
            vt[2]:=vt[2]-n[j][2]*dot;
            binormal[j]:=VectorMake(VectorNormalize(vt),0);
         end;
      end;
      
      if buildTangents then SetTriangleData(i,Tangents,tangent[0],tangent[1],tangent[2]);
      if buildBinormals then SetTriangleData(i,Binormals,binormal[0],binormal[1],binormal[2]);
   end;
end;

// DeclareArraysToOpenGL
//
procedure TMeshObject.DeclareArraysToOpenGL(var mrci : TRenderContextInfo; evenIfAlreadyDeclared : Boolean = False);
var
   i : Integer;
   currentMapping : Cardinal;
begin
   if evenIfAlreadyDeclared or (not FArraysDeclared) then begin
      if Vertices.Count>0 then begin
         glEnableClientState(GL_VERTEX_ARRAY);
         glVertexPointer(3, GL_FLOAT, 0, Vertices.List);
      end else glDisableClientState(GL_VERTEX_ARRAY);
      if not mrci.ignoreMaterials then begin
         if Normals.Count>0 then begin
            glEnableClientState(GL_NORMAL_ARRAY);
            glNormalPointer(GL_FLOAT, 0, Normals.List);
         end else glDisableClientState(GL_NORMAL_ARRAY);
         if (Colors.Count>0) and (not mrci.ignoreMaterials) then begin
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4, GL_FLOAT, 0, Colors.List);
         end else glDisableClientState(GL_COLOR_ARRAY);
         if TexCoords.Count>0 then begin
            xglEnableClientState(GL_TEXTURE_COORD_ARRAY);
            xglTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), TexCoords.List);
         end else xglDisableClientState(GL_TEXTURE_COORD_ARRAY);
         if GL_ARB_multitexture then begin
            if LightMapTexCoords.Count>0 then begin
               glClientActiveTextureARB(GL_TEXTURE1_ARB);
               glTexCoordPointer(2, GL_FLOAT, SizeOf(TTexPoint), LightMapTexCoords.List);
               glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            end;
            for i:=0 to FTexCoordsEx.Count-1 do begin
               if TexCoordsEx[i].Count>0 then begin
                  glClientActiveTextureARB(GL_TEXTURE0_ARB + i);
                  glTexCoordPointer(4, GL_FLOAT, SizeOf(TVector), TexCoordsEx[i].List);
                  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
               end;
            end;
            glClientActiveTextureARB(GL_TEXTURE0_ARB);
         end;
      end else begin
         glDisableClientState(GL_NORMAL_ARRAY);
         glDisableClientState(GL_COLOR_ARRAY);
         xglDisableClientState(GL_TEXTURE_COORD_ARRAY);
      end;
      if GL_EXT_compiled_vertex_array and (LightMapTexCoords.Count=0) then
         glLockArraysEXT(0, vertices.Count);
      FLastLightMapIndex:=-1;
      FArraysDeclared:=True;
      FLightMapArrayEnabled:=False;
      if mrci.drawState <> dsPicking then
      	 FLastXOpenGLTexMapping:=xglGetBitWiseMapping;
   end else begin
      if not mrci.ignoreMaterials and not (mrci.drawState = dsPicking) then
         if TexCoords.Count>0 then begin
         currentMapping:=xglGetBitWiseMapping;
            if FLastXOpenGLTexMapping<>currentMapping then begin
               xglEnableClientState(GL_TEXTURE_COORD_ARRAY);
               xglTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), TexCoords.List);
               FLastXOpenGLTexMapping:=currentMapping;
            end;
         end;
   end;
end;

// DisableOpenGLArrays
//
procedure TMeshObject.DisableOpenGLArrays(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   if FArraysDeclared then begin
      DisableLightMapArray(mrci);
      if GL_EXT_compiled_vertex_array and (LightMapTexCoords.Count=0) then
         glUnLockArraysEXT;
      if Vertices.Count>0 then
         glDisableClientState(GL_VERTEX_ARRAY);
      if not mrci.ignoreMaterials then begin
         if Normals.Count>0 then
            glDisableClientState(GL_NORMAL_ARRAY);
         if (Colors.Count>0) and (not mrci.ignoreMaterials) then
            glDisableClientState(GL_COLOR_ARRAY);
         if TexCoords.Count>0 then
            xglDisableClientState(GL_TEXTURE_COORD_ARRAY);
         if GL_ARB_multitexture then begin
            if LightMapTexCoords.Count>0 then begin
               glClientActiveTextureARB(GL_TEXTURE1_ARB);
               glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            end;
            for i:=0 to FTexCoordsEx.Count-1 do begin
               if TexCoordsEx[i].Count>0 then begin
                  glClientActiveTextureARB(GL_TEXTURE0_ARB + i);
                  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
               end;
            end;
            glClientActiveTextureARB(GL_TEXTURE0_ARB);
         end;
      end;
      FArraysDeclared:=False;
   end;
end;

// EnableLightMapArray
//
procedure TMeshObject.EnableLightMapArray(var mrci : TRenderContextInfo);
begin
   if GL_ARB_multitexture and (not mrci.ignoreMaterials) then begin
      Assert(FArraysDeclared);
      if not FLightMapArrayEnabled then begin
         glActiveTextureARB(GL_TEXTURE1_ARB);
         glEnable(GL_TEXTURE_2D);
         glActiveTextureARB(GL_TEXTURE0_ARB);
         FLightMapArrayEnabled:=True;
      end;
   end;
end;

// DisableLightMapArray
//
procedure TMeshObject.DisableLightMapArray(var mrci : TRenderContextInfo);
begin
   if GL_ARB_multitexture and FLightMapArrayEnabled then begin
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);
      FLightMapArrayEnabled:=False;
   end;
end;

// PrepareMaterials
//
procedure TMeshObject.PrepareBuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   if (Mode=momFaceGroups) and Assigned(mrci.materialLibrary) then begin
      for i:=0 to FaceGroups.Count-1 do with TFaceGroup(FaceGroups.List[i]) do begin
         if MaterialCache<>nil then
            MaterialCache.PrepareBuildList;
      end;
   end;
end;

// BuildList
//
procedure TMeshObject.BuildList(var mrci : TRenderContextInfo);
var
   i, j, groupID, nbGroups : Integer;
   gotNormals, gotTexCoords, gotColor : Boolean;
   gotTexCoordsEx : array of Boolean;
   libMat : TGLLibMaterial;
   fg : TFaceGroup;
begin
   FArraysDeclared:=False;
   FLastXOpenGLTexMapping:=0;
   gotColor:=(Vertices.Count=Colors.Count);
   if gotColor then begin
      glPushAttrib(GL_ENABLE_BIT);
      glEnable(GL_COLOR_MATERIAL);
      glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
      mrci.GLStates.ResetGLMaterialColors;
   end;
   case Mode of
      momTriangles, momTriangleStrip : if Vertices.Count>0 then begin
         DeclareArraysToOpenGL(mrci);
         gotNormals:=(Vertices.Count=Normals.Count);
         gotTexCoords:=(Vertices.Count=TexCoords.Count);
         SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
         for i:=0 to FTexCoordsEx.Count-1 do
            gotTexCoordsEx[i]:=(TexCoordsEx[i].Count>0) and GL_ARB_multitexture;
         if Mode=momTriangles then
            glBegin(GL_TRIANGLES)
         else glBegin(GL_TRIANGLE_STRIP);
         for i:=0 to Vertices.Count-1 do begin
            if gotNormals   then glNormal3fv(@Normals.List[i]);
            if gotColor     then glColor4fv(@Colors.List[i]);
            if FTexCoordsEx.Count>0 then begin
               if gotTexCoordsEx[0] then 
                  glMultiTexCoord4fvARB(GL_TEXTURE0_ARB, @TexCoordsEx[0].List[i])
               else if gotTexCoords then
                  xglTexCoord2fv(@TexCoords.List[i]);
               for j:=1 to FTexCoordsEx.Count-1 do
                  if gotTexCoordsEx[j] then
                     glMultiTexCoord4fvARB(GL_TEXTURE0_ARB+j, @TexCoordsEx[j].List[i]);
            end else begin
               if gotTexCoords then
                  xglTexCoord2fv(@TexCoords.List[i]);
            end;
            glVertex3fv(@Vertices.List[i]);
         end;
         glEnd;
      end;
      momFaceGroups : begin
         if Assigned(mrci.materialLibrary) then begin
            if moroGroupByMaterial in RenderingOptions then begin
               // group-by-material rendering, reduces material switches,
               // but alters rendering order
               groupID:=vNextRenderGroupID;
               Inc(vNextRenderGroupID);
               for i:=0 to FaceGroups.Count-1 do begin
                  if FaceGroups[i].FRenderGroupID<>groupID then begin
                     libMat:=FaceGroups[i].FMaterialCache;
                     if Assigned(libMat) then
                        libMat.Apply(mrci);
                     repeat
                        for j:=i to FaceGroups.Count-1 do with FaceGroups[j] do begin
                           if (FRenderGroupID<>groupID)
                                 and (FMaterialCache=libMat) then begin
                              FRenderGroupID:=groupID;
                              BuildList(mrci);
                           end;
                        end;
                     until (not Assigned(libMat)) or (not libMat.UnApply(mrci));
                  end;
               end;
            end else begin
               // canonical rendering (regroups only contiguous facegroups)
               i:=0;
               nbGroups:=FaceGroups.Count;
               while i<nbGroups do begin
                  libMat:=FaceGroups[i].FMaterialCache;
                  if Assigned(libMat) then begin
                     libMat.Apply(mrci);
                     repeat
                        j:=i;
                        while j<nbGroups do begin
                           fg:=FaceGroups[j];
                           if fg.MaterialCache<>libMat then Break;
                           fg.BuildList(mrci);
                           Inc(j);
                        end;
                     until not libMat.UnApply(mrci);
                     i:=j;
                  end else begin
                     FaceGroups[i].BuildList(mrci);
                     Inc(i);
                  end;
               end;
            end;
            // restore faceculling
            if (stCullFace in mrci.GLStates.States) then begin
               if not mrci.bufferFaceCull then
                  mrci.GLStates.UnSetGLState(stCullFace);
            end else begin
               if mrci.bufferFaceCull then
                  mrci.GLStates.SetGLState(stCullFace);
            end;
         end else for i:=0 to FaceGroups.Count-1 do
            FaceGroups[i].BuildList(mrci);
      end;
   else
      Assert(False);
   end;
   if gotColor then glPopAttrib;
   DisableOpenGLArrays(mrci);
end;

// ------------------
// ------------------ TMeshObjectList ------------------
// ------------------

// CreateOwned
//
constructor TMeshObjectList.CreateOwned(aOwner : TGLBaseMesh);
begin
   FOwner:=AOwner;
   Create;
end;

// Destroy
//
destructor TMeshObjectList.Destroy;
begin
   Clear;
   inherited;
end;

// ReadFromFiler
//
procedure TMeshObjectList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
   mesh : TMeshObject;
begin
   inherited;
   for i:=0 to Count-1 do begin
      mesh:=Items[i];
      mesh.FOwner:=Self;
      if mesh is TSkeletonMeshObject then
         TSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
   end;
end;

// PrepareMaterialLibraryCache
//
procedure TMeshObjectList.PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TMeshObject(List[i]).PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//
procedure TMeshObjectList.DropMaterialLibraryCache;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TMeshObject(List[i]).DropMaterialLibraryCache;
end;

// PrepareBuildList
//
procedure TMeshObjectList.PrepareBuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do
      if Visible then
         PrepareBuildList(mrci);
end;

// BuildList
//
procedure TMeshObjectList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do
      if Visible then
         BuildList(mrci);
end;

// MorphTo
//
procedure TMeshObjectList.MorphTo(morphTargetIndex : Integer);
var
   i : Integer;
begin
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

// Lerp
//
procedure TMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                               lerpFactor : Single);
var
   i : Integer;
begin
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2, lerpFactor);
end;

// MorphTargetCount
//
function TMeshObjectList.MorphTargetCount : Integer;
var
   i : Integer;
begin
   Result:=MaxInt;
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      with TMorphableMeshObject(Items[i]) do
         if Result>MorphTargets.Count then
            Result:=MorphTargets.Count;
   if Result=MaxInt then
      Result:=0;
end;

// Clear
//
procedure TMeshObjectList.Clear;
var
   i : Integer;
begin
   DropMaterialLibraryCache;
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetMeshObject
//
function TMeshObjectList.GetMeshObject(Index: Integer): TMeshObject;
begin
   Result:=TMeshObject(List^[Index]);
end;

// GetExtents
//
procedure TMeshObjectList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   lMin, lMax : TAffineVector;
const
   cBigValue : Single = 1e30;
   cSmallValue : Single = -1e30;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      GetMeshObject(i).GetExtents(lMin, lMax);
      for k:=0 to 2 do begin
          if lMin[k]<min[k] then min[k]:=lMin[k];
          if lMax[k]>max[k] then max[k]:=lMax[k];
      end;
   end;
end;

// Translate
//
procedure TMeshObjectList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      GetMeshObject(i).Translate(delta);
end;

// ExtractTriangles
//
function TMeshObjectList.ExtractTriangles(texCoords : TAffineVectorList = nil;
                                          normals : TAffineVectorList = nil) : TAffineVectorList;
var
   i : Integer;
   objTris : TAffineVectorList;
   objTexCoords : TAffineVectorList;
   objNormals : TAffineVectorList;
begin
   Result:=TAffineVectorList.Create;
   if Assigned(texCoords) then
      objTexCoords:=TAffineVectorList.Create
   else objTexCoords:=nil;
   if Assigned(normals) then
      objNormals:=TAffineVectorList.Create
   else objNormals:=nil;
   try
      for i:=0 to Count-1 do begin
         objTris:=GetMeshObject(i).ExtractTriangles(objTexCoords, objNormals);
         try
            Result.Add(objTris);
            if Assigned(texCoords) then begin
               texCoords.Add(objTexCoords);
               objTexCoords.Count:=0;
            end;
            if Assigned(normals) then begin
               normals.Add(objNormals);
               objNormals.Count:=0;
            end;
         finally
            objTris.Free;
         end;
      end;
   finally
      objTexCoords.Free;
      objNormals.Free;
   end;
end;

// TriangleCount
//
function TMeshObjectList.TriangleCount : Integer;
var
   i : Integer;
begin
   Result:=0;
   for i:=0 to Count-1 do
      Result:=Result+Items[i].TriangleCount;
end;

// Prepare
//
procedure TMeshObjectList.Prepare;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Prepare;
end;

// FindMeshByName
//
function TMeshObjectList.FindMeshByName(MeshName : String) : TMeshObject;
var
   i : integer;
begin
   Result:=nil;
   for i:=0 to Count-1 do
      if Items[i].Name = MeshName then begin
        Result:=Items[i];
        break;
      end;
end;


// ------------------
// ------------------ TMeshMorphTarget ------------------
// ------------------

// CreateOwned
//
constructor TMeshMorphTarget.CreateOwned(AOwner : TMeshMorphTargetList);
begin
   FOwner:=AOwner;
   Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Destroy
//
destructor TMeshMorphTarget.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// WriteToFiler
//
procedure TMeshMorphTarget.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      //nothing
   end;
end;

// ReadFromFiler
//
procedure TMeshMorphTarget.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      //nothing
   end else RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TMeshMorphTargetList ------------------
// ------------------

// CreateOwned
//
constructor TMeshMorphTargetList.CreateOwned(AOwner : TPersistent);
begin
   FOwner:=AOwner;
   Create;
end;

// Destroy
//
destructor TMeshMorphTargetList.Destroy;
begin
   Clear;
   inherited;
end;

// ReadFromFiler
//
procedure TMeshMorphTargetList.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// Translate
//
procedure TMeshMorphTargetList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Translate(delta);
end;

// Clear
//
procedure TMeshMorphTargetList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetMeshMorphTarget
//
function TMeshMorphTargetList.GetMeshMorphTarget(Index: Integer): TMeshMorphTarget;
begin
   Result:=TMeshMorphTarget(List^[Index]);
end;

// ------------------
// ------------------ TMorphableMeshObject ------------------
// ------------------

// Create
//
constructor TMorphableMeshObject.Create;
begin
   inherited;
   FMorphTargets:=TMeshMorphTargetList.CreateOwned(Self);
end;

// Destroy
//
destructor TMorphableMeshObject.Destroy;
begin
   FMorphTargets.Free;
   inherited;
end;

// WriteToFiler
//
procedure TMorphableMeshObject.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FMorphTargets.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TMorphableMeshObject.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FMorphTargets.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// Clear;
//
procedure TMorphableMeshObject.Clear;
begin
   inherited;
   FMorphTargets.Clear;
end;

// Translate
//
procedure TMorphableMeshObject.Translate(const delta : TAffineVector);
begin
   inherited;
   MorphTargets.Translate(delta);
end;

// MorphTo
//
procedure TMorphableMeshObject.MorphTo(morphTargetIndex : Integer);
begin
   if (morphTargetIndex=0) and (MorphTargets.Count=0) then Exit;
   Assert(Cardinal(morphTargetIndex)<Cardinal(MorphTargets.Count));
   with MorphTargets[morphTargetIndex] do begin
      if Vertices.Count>0 then
         Self.Vertices.Assign(Vertices);
      if Normals.Count>0 then
         Self.Normals.Assign(Normals);
   end;
end;

// Lerp
//
procedure TMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                                    lerpFactor : Single);
var
   mt1, mt2 : TMeshMorphTarget;
begin
   Assert((Cardinal(morphTargetIndex1)<Cardinal(MorphTargets.Count))
          and (Cardinal(morphTargetIndex2)<Cardinal(MorphTargets.Count)));
   if lerpFactor=0 then
      MorphTo(morphTargetIndex1)
   else if lerpFactor=1 then
      MorphTo(morphTargetIndex2)
   else begin
      mt1:=MorphTargets[morphTargetIndex1];
      mt2:=MorphTargets[morphTargetIndex2];
      if mt1.Vertices.Count>0 then
         Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      if mt1.Normals.Count>0 then begin
         Normals.Lerp(mt1.Normals, mt2.Normals, lerpFactor);
         Normals.Normalize;
      end;
   end;
end;

// ------------------
// ------------------ TSkeletonMeshObject ------------------
// ------------------

// Create
//
constructor TSkeletonMeshObject.Create;
begin
   FBoneMatrixInvertedMeshes:=TList.Create;
	inherited Create;
end;

// Destroy
//
destructor TSkeletonMeshObject.Destroy;
begin
   Clear;
   FBoneMatrixInvertedMeshes.Free;
	inherited Destroy;
end;

// WriteToFiler
//
procedure TSkeletonMeshObject.WriteToFiler(writer : TVirtualWriter);
var
   i : Integer;
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteInteger(FVerticeBoneWeightCount);
      WriteInteger(FBonesPerVertex);
      WriteInteger(FVerticeBoneWeightCapacity);
      for i:=0 to FVerticeBoneWeightCount-1 do
         Write(FVerticesBonesWeights[i][0], FBonesPerVertex*SizeOf(TVertexBoneWeight));
   end;
end;

// ReadFromFiler
//
procedure TSkeletonMeshObject.ReadFromFiler(reader : TVirtualReader);
var
	archiveVersion, i : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
	if archiveVersion=0 then with reader do begin
      FVerticeBoneWeightCount:=ReadInteger;
      FBonesPerVertex:=ReadInteger;
      FVerticeBoneWeightCapacity := ReadInteger;
      ResizeVerticesBonesWeights;
      for i:=0 to FVerticeBoneWeightCount-1 do
         Read(FVerticesBonesWeights[i][0], FBonesPerVertex*SizeOf(TVertexBoneWeight));
	end else RaiseFilerException(archiveVersion);
end;

// Clear
//
procedure TSkeletonMeshObject.Clear;
var
   i : Integer;
begin
   inherited;
   FVerticeBoneWeightCount:=0;
   FBonesPerVertex:=0;
   ResizeVerticesBonesWeights;
   for i:=0 to FBoneMatrixInvertedMeshes.Count-1 do
      TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
   FBoneMatrixInvertedMeshes.Clear;
end;

// SetVerticeBoneWeightCount
//
procedure TSkeletonMeshObject.SetVerticeBoneWeightCount(const val : Integer);
begin
   if val<>FVerticeBoneWeightCount then begin
      FVerticeBoneWeightCount:=val;
      if FVerticeBoneWeightCount>FVerticeBoneWeightCapacity then
         VerticeBoneWeightCapacity:=FVerticeBoneWeightCount+16;
      FLastVerticeBoneWeightCount:=FVerticeBoneWeightCount;
   end;
end;

// SetVerticeBoneWeightCapacity
//
procedure TSkeletonMeshObject.SetVerticeBoneWeightCapacity(const val : Integer);
begin
   if val<>FVerticeBoneWeightCapacity then begin
      FVerticeBoneWeightCapacity:=val;
      ResizeVerticesBonesWeights;
   end;
end;

// SetBonesPerVertex
//
procedure TSkeletonMeshObject.SetBonesPerVertex(const val : Integer);
begin
   if val<>FBonesPerVertex then begin
      FBonesPerVertex:=val;
      ResizeVerticesBonesWeights;
   end;
end;

// ResizeVerticesBonesWeights
//
procedure TSkeletonMeshObject.ResizeVerticesBonesWeights;
var
   n, m, i, j : Integer;
   newArea : PVerticesBoneWeights;
begin
   n:=BonesPerVertex*VerticeBoneWeightCapacity;
   if n=0 then begin
      // release everything
      if Assigned(FVerticesBonesWeights) then begin
         FreeMem(FVerticesBonesWeights[0]);
         FreeMem(FVerticesBonesWeights);
         FVerticesBonesWeights:=nil;
      end;
   end else begin
      // allocate new area
      GetMem(newArea, VerticeBoneWeightCapacity*SizeOf(PVertexBoneWeightArray));
      newArea[0]:=AllocMem(n*SizeOf(TVertexBoneWeight));
      for i:=1 to VerticeBoneWeightCapacity-1 do
         newArea[i]:=PVertexBoneWeightArray(Integer(newArea[0])+i*SizeOf(TVertexBoneWeight)*BonesPerVertex);
      // transfer old data
      if FLastVerticeBoneWeightCount<VerticeBoneWeightCount then
         n:=FLastVerticeBoneWeightCount
      else n:=VerticeBoneWeightCount;
      if FLastBonesPerVertex<BonesPerVertex then
         m:=FLastBonesPerVertex
      else m:=BonesPerVertex;
      for i:=0 to n-1 do
         for j:=0 to m-1 do newArea[i][j]:=VerticesBonesWeights[i][j];
      // release old area and switch to new
      if Assigned(FVerticesBonesWeights) then begin
         FreeMem(FVerticesBonesWeights[0]);
         FreeMem(FVerticesBonesWeights);
      end;
      FVerticesBonesWeights:=newArea;
   end;
   FLastBonesPerVertex:=FBonesPerVertex;
end;

// AddWeightedBone
//
procedure TSkeletonMeshObject.AddWeightedBone(aBoneID : Integer; aWeight : Single);
begin
   if BonesPerVertex<1 then BonesPerVertex:=1;
   VerticeBoneWeightCount:=VerticeBoneWeightCount+1;
   with VerticesBonesWeights[VerticeBoneWeightCount-1][0] do begin
      BoneID:=aBoneID;
      Weight:=aWeight;
   end;
end;

// AddWeightedBones
//
procedure TSkeletonMeshObject.AddWeightedBones(const boneIDs : TVertexBoneWeightDynArray);
var
   i : Integer;
   n : Integer;
begin
   n:=Length(boneIDs);
   if BonesPerVertex<n then BonesPerVertex:=n;
   VerticeBoneWeightCount:=VerticeBoneWeightCount+1;
   for i:=0 to n-1 do begin
      with VerticesBonesWeights[VerticeBoneWeightCount-1][i] do begin
         BoneID:=boneIDs[i].BoneID;
         Weight:=boneIDs[i].Weight;
      end;
   end;
end;

// FindOrAdd
//
function TSkeletonMeshObject.FindOrAdd(boneID : Integer;
                                       const vertex, normal : TAffineVector) : Integer;
var
   i : Integer;
   dynArray : TVertexBoneWeightDynArray;
begin
   if BonesPerVertex>1 then begin
      SetLength(dynArray, 1);
      dynArray[0].BoneID:=boneID;
      dynArray[0].Weight:=1;
      Result:=FindOrAdd(dynArray, vertex, normal);
      Exit;
   end;
   Result:=-1;
   for i:=0 to Vertices.Count-1 do
      if (VerticesBonesWeights[i][0].BoneID=boneID)
            and VectorEquals(Vertices.List[i], vertex)
            and VectorEquals(Normals.List[i], normal) then begin
         Result:=i;
         Break;
      end;
   if Result<0 then begin
      AddWeightedBone(boneID, 1);
      Vertices.Add(vertex);
      Result:=Normals.Add(normal);
   end;
end;

// FindOrAdd
//
function TSkeletonMeshObject.FindOrAdd(const boneIDs : TVertexBoneWeightDynArray;
                                       const vertex, normal : TAffineVector) : Integer;
var
   i, j : Integer;
   bonesMatch : Boolean;
begin
   Result:=-1;
   for i:=0 to Vertices.Count-1 do begin
      bonesMatch:=True;
      for j:=0 to High(boneIDs) do begin
         if    (boneIDs[j].BoneID<>VerticesBonesWeights[i][j].BoneID)
            or (boneIDs[j].Weight<>VerticesBonesWeights[i][j].Weight) then begin
            bonesMatch:=False;
            Break;
         end;
      end;
      if bonesMatch and VectorEquals(Vertices[i], vertex)
                    and VectorEquals(Normals[i], normal) then begin
         Result:=i;
         Break;
      end;
   end;
   if Result<0 then begin
      AddWeightedBones(boneIDs);
      Vertices.Add(vertex);
      Result:=Normals.Add(normal);
   end;
end;

// PrepareBoneMatrixInvertedMeshes
//
procedure TSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
   i, k, boneIndex : Integer;
   invMesh : TBaseMeshObject;
   invMat : TMatrix;
   bone : TSkeletonBone;
   p : TVector;
begin
   // cleanup existing stuff
   for i:=0 to FBoneMatrixInvertedMeshes.Count-1 do
      TBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
   FBoneMatrixInvertedMeshes.Clear;
   // calculate
   for k:=0 to BonesPerVertex-1 do begin
      invMesh:=TBaseMeshObject.Create;
      FBoneMatrixInvertedMeshes.Add(invMesh);
      invMesh.Vertices:=Vertices;
      invMesh.Normals:=Normals;
      for i:=0 to Vertices.Count-1 do begin
         boneIndex:=VerticesBonesWeights[i][k].BoneID;
         bone:=Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
         // transform point
         MakePoint(p, Vertices[i]);
         invMat:=bone.GlobalMatrix;
         InvertMatrix(invMat);
         p:=VectorTransform(p, invMat);
         invMesh.Vertices[i]:=PAffineVector(@p)^;
         // transform normal
         SetVector(p, Normals[i]);
         invMat:=bone.GlobalMatrix;
         invMat[3]:=NullHmgPoint;
         InvertMatrix(invMat);
         p:=VectorTransform(p, invMat);
         invMesh.Normals[i]:=PAffineVector(@p)^;
      end;
   end;
end;

// ApplyCurrentSkeletonFrame
//
procedure TSkeletonMeshObject.ApplyCurrentSkeletonFrame(normalize : Boolean);
var
   i, j, boneID : Integer;
   refVertices, refNormals : TAffineVectorList;
   n, nt : TVector;
   bone : TSkeletonBone;
   skeleton : TSkeleton;
   tempvert,
   tempnorm : TAffineVector;
begin
   with TBaseMeshObject(FBoneMatrixInvertedMeshes[0]) do begin
      refVertices:=Vertices;
      refNormals:=Normals;
   end;
   skeleton:=Owner.Owner.Skeleton;
   n[3]:=0;
   if BonesPerVertex=1 then begin
      // simple case, one bone per vertex
      for i:=0 to refVertices.Count-1 do begin
         boneID:=VerticesBonesWeights[i][0].BoneID;
         bone:=skeleton.BoneByID(boneID);
         Vertices.List[i]:=VectorTransform(refVertices.List[i], bone.GlobalMatrix);
         PAffineVector(@n)^:=refNormals.List[i];
         nt:=VectorTransform(n, bone.GlobalMatrix);
         Normals.List[i]:=PAffineVector(@nt)^;
      end;
   end else begin
      // multiple bones per vertex
      for i:=0 to refVertices.Count-1 do begin
         Vertices.List[i]:=NullVector;
         Normals.List[i]:=NullVector;
         for j:=0 to BonesPerVertex-1 do begin
            with TBaseMeshObject(FBoneMatrixInvertedMeshes[j]) do begin
               refVertices:=Vertices;
               refNormals:=Normals;
            end;
            tempvert:=NullVector;
            tempnorm:=NullVector;
            if VerticesBonesWeights[i][j].Weight<>0 then begin
               boneID:=VerticesBonesWeights[i][j].BoneID;
               bone:=skeleton.BoneByID(boneID);
               CombineVector(tempvert,
                             VectorTransform(refVertices.List[i], bone.GlobalMatrix),
                             VerticesBonesWeights[i][j].Weight);
               PAffineVector(@n)^:=refNormals.List[i];
               n:=VectorTransform(n, bone.GlobalMatrix);
               CombineVector(tempnorm,
                             PAffineVector(@n)^,
                             VerticesBonesWeights[i][j].Weight);
            end;
            AddVector(Vertices.List[i],tempvert);
            AddVector(Normals.List[i],tempnorm);
         end;
      end;
   end;
   if normalize then
      Normals.Normalize;
end;

// ------------------
// ------------------ TFaceGroup ------------------
// ------------------

// CreateOwned
//
constructor TFaceGroup.CreateOwned(AOwner : TFaceGroups);
begin
   FOwner:=AOwner;
   FLightMapIndex:=-1;
   Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Destroy
//
destructor TFaceGroup.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// WriteToFiler
//
procedure TFaceGroup.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      if FLightMapIndex<0 then begin
         WriteInteger(0);  // Archive Version 0
         WriteString(FMaterialName);
      end else begin
         WriteInteger(1);  // Archive Version 1, added FLightMapIndex
         WriteString(FMaterialName);
         WriteInteger(FLightMapIndex);
      end;
   end;
end;

// ReadFromFiler
//
procedure TFaceGroup.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion in [0..1] then with reader do begin
      FMaterialName:=ReadString;
      if archiveVersion>=1 then
         FLightMapIndex:=ReadInteger
      else FLightMapIndex:=-1;
   end else RaiseFilerException(archiveVersion);
end;

// AttachLightmap
//
procedure TFaceGroup.AttachLightmap(lightMap : TGLTexture; var mrci : TRenderContextInfo);
begin
   if GL_ARB_multitexture then with lightMap do begin
      Assert(Image.NativeTextureTarget=GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE1_ARB);

      mrci.GLStates.SetGLCurrentTexture(1, GL_TEXTURE_2D, Handle);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      glActiveTextureARB(GL_TEXTURE0_ARB);
   end;
end;

// AttachOrDetachLightmap
//
procedure TFaceGroup.AttachOrDetachLightmap(var mrci : TRenderContextInfo);
var
   libMat : TGLLibMaterial;
begin
   if GL_ARB_multitexture then begin
      if (not mrci.ignoreMaterials) and Assigned(mrci.lightmapLibrary) then begin
         if Owner.Owner.FLastLightMapIndex<>lightMapIndex then begin
            Owner.Owner.FLastLightMapIndex:=lightMapIndex;
            if lightMapIndex>=0 then begin
               // attach and activate lightmap
               Assert(lightMapIndex<mrci.lightmapLibrary.Materials.Count);
               libMat:=mrci.lightmapLibrary.Materials[lightMapIndex];
               AttachLightmap(libMat.Material.Texture, mrci);
               Owner.Owner.EnableLightMapArray(mrci);
            end else begin
               // desactivate lightmap
               Owner.Owner.DisableLightMapArray(mrci);
            end;
         end;
      end;
   end;
end;

// PrepareMaterialLibraryCache
//
procedure TFaceGroup.PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
begin
   if (FMaterialName<>'') and (matLib<>nil) then
      FMaterialCache:=matLib.Materials.GetLibMaterialByName(FMaterialName)
   else FMaterialCache:=nil;
end;

// DropMaterialLibraryCache
//
procedure TFaceGroup.DropMaterialLibraryCache;
begin
   FMaterialCache:=nil;
end;

// AddToTriangles
//
procedure TFaceGroup.AddToTriangles(aList : TAffineVectorList;
                                    aTexCoords : TAffineVectorList = nil;
                                    aNormals : TAffineVectorList = nil);
begin
   // nothing
end;

// Reverse
//
procedure TFaceGroup.Reverse;
begin
   // nothing
end;

// Prepare
//
procedure TFaceGroup.Prepare;
begin
   // nothing
end;

// ------------------
// ------------------ TFGVertexIndexList ------------------
// ------------------

// Create
//
constructor TFGVertexIndexList.Create;
begin
   inherited;
   FVertexIndices:=TIntegerList.Create;
   FMode:=fgmmTriangles;
end;

// Destroy
//
destructor TFGVertexIndexList.Destroy;
begin
   FVertexIndices.Free;
   inherited;
end;

// WriteToFiler
//
procedure TFGVertexIndexList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FVertexIndices.WriteToFiler(writer);
      WriteInteger(Integer(FMode));
   end;
end;

// ReadFromFiler
//
procedure TFGVertexIndexList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FVertexIndices.ReadFromFiler(reader);
      FMode:=TFaceGroupMeshMode(ReadInteger);
   end else RaiseFilerException(archiveVersion);
end;

// SetIndices
//
procedure TFGVertexIndexList.SetVertexIndices(const val : TIntegerList);
begin
   FVertexIndices.Assign(val);
end;

// BuildList
//
procedure TFGVertexIndexList.BuildList(var mrci : TRenderContextInfo);
const
   cFaceGroupMeshModeToOpenGL : array [TFaceGroupMeshMode] of Integer =
         (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES, GL_TRIANGLE_FAN, GL_QUADS);

begin
   if VertexIndices.Count=0 then Exit;
   Owner.Owner.DeclareArraysToOpenGL(mrci, False);
   AttachOrDetachLightmap(mrci);
   glDrawElements(cFaceGroupMeshModeToOpenGL[Mode], VertexIndices.Count,
                  GL_UNSIGNED_INT, VertexIndices.List);
end;

// AddToList
//
procedure TFGVertexIndexList.AddToList(source, destination : TAffineVectorList;
                                       indices : TIntegerList);
var
   i, n : Integer;
begin
   if not Assigned(destination) then Exit;
   if indices.Count<3 then Exit;
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : begin
         n:=(indices.Count div 3)*3;
         if source.Count>0 then begin
            destination.AdjustCapacityToAtLeast(destination.Count+n);
            for i:=0 to n-1 do
               destination.Add(source[indices.List[i]]);
         end else destination.AddNulls(destination.Count+n);
      end;
      fgmmTriangleStrip : begin
         if source.Count>0 then
            ConvertStripToList(source, indices, destination)
         else destination.AddNulls(destination.Count+(indices.Count-2)*3);
      end;
      fgmmTriangleFan : begin
         n:=(indices.Count-2)*3;
         if source.Count>0 then begin
            destination.AdjustCapacityToAtLeast(destination.Count+n);
            for i:=2 to VertexIndices.Count-1 do begin
               destination.Add(source[indices.List[0]],
                               source[indices.List[i-1]],
                               source[indices.List[i]]);
            end;
         end else destination.AddNulls(destination.Count+n);
      end;
      fgmmQuads : begin
         n:=indices.Count div 4;
         if source.Count>0 then begin
            destination.AdjustCapacityToAtLeast(destination.Count+n*6);
            i:=0;
            while n>0 do begin
               destination.Add(source[indices.List[i]],
                               source[indices.List[i+1]],
                               source[indices.List[i+2]]);
               destination.Add(source[indices.List[i]],
                               source[indices.List[i+2]],
                               source[indices.List[i+3]]);
               Inc(i, 4);
               Dec(n);
            end;
         end else destination.AddNulls(destination.Count+n*6);
      end;
   else
      Assert(False);
   end;
end;

// AddToTriangles
//
procedure TFGVertexIndexList.AddToTriangles(aList : TAffineVectorList;
                                            aTexCoords : TAffineVectorList = nil;
                                            aNormals : TAffineVectorList = nil);
var
   mo : TMeshObject;
begin
   mo:=Owner.Owner;
   AddToList(mo.Vertices,  aList,      VertexIndices);
   AddToList(mo.TexCoords, aTexCoords, VertexIndices);
   AddToList(mo.Normals,   aNormals,   VertexIndices);
end;

// TriangleCount
//
function TFGVertexIndexList.TriangleCount : Integer;
begin
   case Mode of
      fgmmTriangles, fgmmFlatTriangles :
         Result:=VertexIndices.Count div 3;
      fgmmTriangleFan, fgmmTriangleStrip : begin
         Result:=VertexIndices.Count-2;
         if Result<0 then Result:=0;
      end;
      fgmmQuads:
        result:=VertexIndices.Count div 2;
   else
      Result:=0;
      Assert(False);
   end;
end;

// Reverse
//
procedure TFGVertexIndexList.Reverse;
begin
   VertexIndices.Reverse;
end;

// Add
//
procedure TFGVertexIndexList.Add(idx : Integer);
begin
   FVertexIndices.Add(idx);
end;

// GetExtents
//
procedure TFGVertexIndexList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
   ref : PFloatArray;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to VertexIndices.Count-1 do begin
      ref:=Owner.Owner.Vertices.ItemAddress[VertexIndices[i]];
      for k:=0 to 2 do begin
         f:=ref[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// ConvertToList
//
procedure TFGVertexIndexList.ConvertToList;
var
   i : Integer;
   bufList : TIntegerList;
begin
   if VertexIndices.Count>=3 then begin
      case Mode of
         fgmmTriangleStrip : begin
            bufList:=TIntegerList.Create;
            try
               ConvertStripToList(VertexIndices, bufList);
               VertexIndices:=bufList;
            finally
               bufList.Free;
            end;
            FMode:=fgmmTriangles;
         end;
         fgmmTriangleFan : begin
            bufList:=TIntegerList.Create;
            try
               for i:=0 to VertexIndices.Count-3 do
                  bufList.Add(VertexIndices[0], VertexIndices[i], VertexIndices[i+1]);
               VertexIndices:=bufList;
            finally
               bufList.Free;
            end;
            FMode:=fgmmTriangles;
         end;
      end;
   end;
end;

// GetNormal
//
function TFGVertexIndexList.GetNormal : TAffineVector;
begin
   if VertexIndices.Count<3 then
      Result:=NullVector
   else with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[VertexIndices[0]], Items[VertexIndices[1]],
                      Items[VertexIndices[2]], Result);
end;

// ------------------
// ------------------ TFGVertexNormalTexIndexList ------------------
// ------------------

// Create
//
constructor TFGVertexNormalTexIndexList.Create;
begin
   inherited;
   FNormalIndices:=TIntegerList.Create;
   FTexCoordIndices:=TIntegerList.Create;
end;

// Destroy
//
destructor TFGVertexNormalTexIndexList.Destroy;
begin
   FTexCoordIndices.Free;
   FNormalIndices.Free;
   inherited;
end;

// WriteToFiler
//
procedure TFGVertexNormalTexIndexList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FNormalIndices.WriteToFiler(writer);
      FTexCoordIndices.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TFGVertexNormalTexIndexList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FNormalIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// SetNormalIndices
//
procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val : TIntegerList);
begin
   FNormalIndices.Assign(val);
end;

// SetTexCoordIndices
//
procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val : TIntegerList);
begin
   FTexCoordIndices.Assign(val);
end;

// BuildList
//
procedure TFGVertexNormalTexIndexList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   vertexPool : PAffineVectorArray;
   normalPool : PAffineVectorArray;
   texCoordPool : PAffineVectorArray;
   normalIdxList, texCoordIdxList, vertexIdxList : PIntegerVector;
begin
   Assert(    ((TexCoordIndices.Count=0) or (VertexIndices.Count<=TexCoordIndices.Count))
          and ((NormalIndices.Count=0) or (VertexIndices.Count<=NormalIndices.Count)));
   vertexPool:=Owner.Owner.Vertices.List;
   normalPool:=Owner.Owner.Normals.List;
   texCoordPool:=Owner.Owner.TexCoords.List;
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : glBegin(GL_TRIANGLES);
      fgmmTriangleStrip :                glBegin(GL_TRIANGLE_STRIP);
      fgmmTriangleFan   :                glBegin(GL_TRIANGLE_FAN);
   else
      Assert(False);
   end;
   vertexIdxList:=VertexIndices.List;
   if NormalIndices.Count>0 then
      normalIdxList:=NormalIndices.List
   else normalIdxList:=vertexIdxList;
   if TexCoordIndices.Count>0 then
      texCoordIdxList:=TexCoordIndices.List
   else texCoordIdxList:=vertexIdxList;
   if Assigned(texCoordPool) then begin
      for i:=0 to VertexIndices.Count-1 do begin
         glNormal3fv(@normalPool[normalIdxList[i]]);
         xglTexCoord2fv(@texCoordPool[texCoordIdxList[i]]);
         glVertex3fv(@vertexPool[vertexIdxList[i]]);
      end;
   end else begin
      for i:=0 to VertexIndices.Count-1 do begin
         glNormal3fv(@normalPool[normalIdxList[i]]);
         glVertex3fv(@vertexPool[vertexIdxList[i]]);
      end;
   end;
   glEnd;
end;

// AddToTriangles
//
procedure TFGVertexNormalTexIndexList.AddToTriangles(aList : TAffineVectorList;
                                                     aTexCoords : TAffineVectorList = nil;
                                                     aNormals : TAffineVectorList = nil);
begin
   AddToList(Owner.Owner.Vertices,  aList,      VertexIndices);
   AddToList(Owner.Owner.TexCoords, aTexCoords, TexCoordIndices);
   AddToList(Owner.Owner.Normals,   aNormals,   NormalIndices);
end;

// Add
//
procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx : Integer);
begin
   inherited Add(vertexIdx);
   FNormalIndices.Add(normalIdx);
   FTexCoordIndices.Add(texCoordIdx);
end;

// ------------------
// ------------------ TFGIndexTexCoordList ------------------
// ------------------

// Create
//
constructor TFGIndexTexCoordList.Create;
begin
   inherited;
   FTexCoords:=TAffineVectorList.Create;
end;

// Destroy
//
destructor TFGIndexTexCoordList.Destroy;
begin
   FTexCoords.Free;
   inherited;
end;

// WriteToFiler
//
procedure TFGIndexTexCoordList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      FTexCoords.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TFGIndexTexCoordList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FTexCoords.ReadFromFiler(reader);
   end else RaiseFilerException(archiveVersion);
end;

// SetTexCoords
//
procedure TFGIndexTexCoordList.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

// BuildList
//
procedure TFGIndexTexCoordList.BuildList(var mrci : TRenderContextInfo);
var
  i, k: integer;
  texCoordPool: PAffineVectorArray;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  indicesPool: PIntegerArray;
  colorPool: PVectorArray;
  gotColor: boolean;
   
begin
  Assert(VertexIndices.Count = TexCoords.Count);
  texCoordPool := TexCoords.List;
  vertexPool := Owner.Owner.Vertices.List;
  indicesPool := @VertexIndices.List[0];
  colorPool := @Owner.Owner.Colors.List[0];
  gotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);
  case Mode of
    fgmmTriangles: glBegin(GL_TRIANGLES);
    fgmmFlatTriangles: glBegin(GL_TRIANGLES);
    fgmmTriangleStrip: glBegin(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: glBegin(GL_TRIANGLE_FAN);
    fgmmQuads: glBegin(GL_QUADS);
  else
    Assert(False);
  end;
  if Owner.Owner.Normals.Count = Owner.Owner.Vertices.Count then begin
    normalPool := Owner.Owner.Normals.List;
    for i := 0 to VertexIndices.Count - 1 do begin
       xglTexCoord2fv(@texCoordPool[i]);
       k := indicesPool[i];
       if gotColor then 
         glColor4fv(@colorPool[k]);
       glNormal3fv(@normalPool[k]);
       glVertex3fv(@vertexPool[k]);
    end;
  end 
  else begin
    for i := 0 to VertexIndices.Count - 1 do begin
      xglTexCoord2fv(@texCoordPool[i]);
      if gotColor then 
        glColor4fv(@colorPool[indicesPool[i]]);
      glVertex3fv(@vertexPool[indicesPool[i]]);
    end;
  end;
  glEnd;
end;

// AddToTriangles
//
procedure TFGIndexTexCoordList.AddToTriangles(aList : TAffineVectorList;
                                              aTexCoords : TAffineVectorList = nil;
                                              aNormals : TAffineVectorList = nil);
var
   i, n : Integer;
   texCoordList : TAffineVectorList;
begin
   AddToList(Owner.Owner.Vertices,  aList,      VertexIndices);
   AddToList(Owner.Owner.Normals,   aNormals,   VertexIndices);
   texCoordList:=Self.TexCoords;
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : begin
         if Assigned(aTexCoords) then begin
            n:=(VertexIndices.Count div 3)*3;
            aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count+n);
            for i:=0 to n-1 do
               aTexCoords.Add(texCoordList[i]);
         end;
      end;
      fgmmTriangleStrip : begin
         if Assigned(aTexCoords) then
            ConvertStripToList(aTexCoords, texCoordList);
      end;
      fgmmTriangleFan : begin
         if Assigned(aTexCoords) then begin
            aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count+(VertexIndices.Count-2)*3);
            for i:=2 to VertexIndices.Count-1 do begin
               aTexCoords.Add(texCoordList[0],
                              texCoordList[i-1],
                              texCoordList[i]);
            end;
         end;
      end;
   else
      Assert(False);
   end;
end;

// Add (affine)
//
procedure TFGIndexTexCoordList.Add(idx : Integer; const texCoord : TAffineVector);
begin
   TexCoords.Add(texCoord);
   inherited Add(idx);
end;

// Add (s, t)
//
procedure TFGIndexTexCoordList.Add(idx : Integer; const s, t : Single);
begin
   TexCoords.Add(s, t, 0);
   inherited Add(idx);
end;

// ------------------
// ------------------ TFaceGroups ------------------
// ------------------

// CreateOwned
//
constructor TFaceGroups.CreateOwned(AOwner : TMeshObject);
begin
   FOwner:=AOwner;
   Create;
end;

// Destroy
//
destructor TFaceGroups.Destroy;
begin
   Clear;
   inherited;
end;

// ReadFromFiler
//
procedure TFaceGroups.ReadFromFiler(reader : TVirtualReader);
var
   i : Integer;
begin
   inherited;
   for i:=0 to Count-1 do Items[i].FOwner:=Self;
end;

// Clear
//
procedure TFaceGroups.Clear;
var
   i : Integer;
   fg : TFaceGroup;
begin
   for i:=0 to Count-1 do begin
      fg:=GetFaceGroup(i);
      if Assigned(fg) then begin
         fg.FOwner:=nil;
         fg.Free;
      end;
   end;
   inherited;
end;

// GetFaceGroup
//
function TFaceGroups.GetFaceGroup(Index: Integer): TFaceGroup;
begin
   Result:=TFaceGroup(List^[Index]);
end;

// PrepareMaterialLibraryCache
//
procedure TFaceGroups.PrepareMaterialLibraryCache(matLib : TGLMaterialLibrary);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TFaceGroup(List[i]).PrepareMaterialLibraryCache(matLib);
end;

// DropMaterialLibraryCache
//
procedure TFaceGroups.DropMaterialLibraryCache;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TFaceGroup(List[i]).DropMaterialLibraryCache;
end;

// AddToTriangles
//
procedure TFaceGroups.AddToTriangles(aList : TAffineVectorList;
                                     aTexCoords : TAffineVectorList = nil;
                                     aNormals : TAffineVectorList = nil);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].AddToTriangles(aList, aTexCoords, aNormals);
end;

// MaterialLibrary
//
function TFaceGroups.MaterialLibrary : TGLMaterialLibrary;
var
   mol : TMeshObjectList;
   bm : TGLBaseMesh;
begin
   if Assigned(Owner) then begin
      mol:=Owner.Owner;
      if Assigned(mol) then begin
         bm:=mol.Owner;
         if Assigned(bm) then begin
            Result:=bm.MaterialLibrary;;
            Exit;
         end;
      end;
   end;
   Result:=nil;
end;

// CompareMaterials
//
function CompareMaterials(item1, item2 : TObject) : Integer;

   function MaterialIsOpaque(fg : TFaceGroup) : Boolean;
   var
      libMat : TGLLibMaterial;
   begin
      libMat:=fg.MaterialCache;
      Result:=(not Assigned(libMat)) or (not libMat.Material.Blended);
   end;

var
   fg1, fg2 : TFaceGroup;
   opaque1, opaque2 : Boolean;
begin
   fg1:=TFaceGroup(item1);
   opaque1:=MaterialIsOpaque(fg1);
   fg2:=TFaceGroup(item2);
   opaque2:=MaterialIsOpaque(fg2);
   if opaque1=opaque2 then begin
      Result:=CompareStr(fg1.MaterialName, fg2.MaterialName);
      if Result=0 then
         result:=fg1.LightMapIndex-fg2.LightMapIndex;
   end else if opaque1 then
      Result:=-1
   else Result:=1;
end;

// SortByMaterial
//
procedure TFaceGroups.SortByMaterial;
begin
   PrepareMaterialLibraryCache(Owner.Owner.Owner.MaterialLibrary);
   Sort(CompareMaterials);
end;

// ------------------
// ------------------ TVectorFile ------------------
// ------------------

// Create
//
constructor TVectorFile.Create(AOwner: TPersistent);
begin
   Assert(AOwner is TGLBaseMesh);
   inherited;
end;

// Owner
//
function TVectorFile.Owner : TGLBaseMesh;
begin
   Result:=TGLBaseMesh(GetOwner);
end;

// SetNormalsOrientation
//
procedure TVectorFile.SetNormalsOrientation(const val : TMeshNormalsOrientation);
begin
   FNormalsOrientation:=val;
end;

// ------------------
// ------------------ TGLGLSMVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLGLSMVectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TGLGLSMVectorFile.LoadFromStream(aStream : TStream);
begin
   Owner.MeshObjects.LoadFromStream(aStream);
end;

// SaveToStream
//
procedure TGLGLSMVectorFile.SaveToStream(aStream : TStream);
begin
   Owner.MeshObjects.SaveToStream(aStream);
end;

// ------------------
// ------------------ TGLBaseMesh ------------------
// ------------------

// Create
//
constructor TGLBaseMesh.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDoesTemperWithColorsOrFaceWinding];
   if FMeshObjects=nil then
      FMeshObjects:=TMeshObjectList.CreateOwned(Self);
   if FSkeleton=nil then
      FSkeleton:=TSkeleton.CreateOwned(Self);
   FUseMeshMaterials:=True;
   FAutoCentering:=[];
   FAxisAlignedDimensionsCache[0]:=-1;
   FAutoScaling:=TGLCoordinates.CreateInitialized(Self, XYZWHmgVector, csPoint);   
end;

// Destroy
//
destructor TGLBaseMesh.Destroy;
begin
   FConnectivity.Free;
   DropMaterialLibraryCache;
   FSkeleton.Free;
   FMeshObjects.Free;
   FAutoScaling.Free;
   inherited Destroy;
end;

// Assign
//
procedure TGLBaseMesh.Assign(Source: TPersistent);
begin
   if Source is TGLBaseMesh then begin
      FSkeleton.Clear;
      FNormalsOrientation:=TGLBaseMesh(Source).FNormalsOrientation;
      FMaterialLibrary:=TGLBaseMesh(Source).FMaterialLibrary;
      FLightmapLibrary:=TGLBaseMesh(Source).FLightmapLibrary;
      FAxisAlignedDimensionsCache:=TGLBaseMesh(Source).FAxisAlignedDimensionsCache;
      FUseMeshMaterials:=TGLBaseMesh(Source).FUseMeshMaterials;
      FOverlaySkeleton:=TGLBaseMesh(Source).FOverlaySkeleton;
      FIgnoreMissingTextures:=TGLBaseMesh(Source).FIgnoreMissingTextures;
      FAutoCentering:=TGLBaseMesh(Source).FAutoCentering;
      FAutoScaling.Assign(TGLBaseMesh(Source).FAutoScaling);
      FSkeleton.Assign(TGLBaseMesh(Source).FSkeleton);
      FSkeleton.RootBones.PrepareGlobalMatrices;
      FMeshObjects.Assign(TGLBaseMesh(Source).FMeshObjects);
   end;
   inherited Assign(Source);
end;

// LoadFromFile
//
procedure TGLBaseMesh.LoadFromFile(const filename : String);
var
   fs : TStream;
begin
   FLastLoadedFilename:= '';
   if fileName<>'' then begin
      fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyWrite);
      try
         LoadFromStream(fileName, fs);
         FLastLoadedFilename:= filename;
      finally
         fs.Free;
      end;
   end;
end;

// LoadFromStream
//
procedure TGLBaseMesh.LoadFromStream(const fileName : String; aStream : TStream);
var
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   FLastLoadedFilename:= '';
   if fileName<>'' then begin
      MeshObjects.Clear;
      Skeleton.Clear;
      vectorFileClass:=GetVectorFileFormats.FindFromFileName(filename);
      newVectorFile:=VectorFileClass.Create(Self);
      try
         newVectorFile.ResourceName:=filename;
         PrepareVectorFile(newVectorFile);
         if Assigned(Scene) then Scene.BeginUpdate;
         try
            newVectorFile.LoadFromStream(aStream);
            FLastLoadedFilename:= filename;
         finally
            if Assigned(Scene) then Scene.EndUpdate;
         end;
      finally
         newVectorFile.Free;
      end;
      PerformAutoScaling;
      PerformAutoCentering;
      PrepareMesh;
   end;
end;

// SaveToFile
//
procedure TGLBaseMesh.SaveToFile(const filename : String);
var
   fs : TStream;
begin
   if fileName<>'' then begin
      fs:=CreateFileStream(fileName, fmCreate);
      try
         SaveToStream(fileName, fs);
      finally
         fs.Free;
      end;
   end;
end;

// SaveToStream
//
procedure TGLBaseMesh.SaveToStream(const fileName : String; aStream : TStream);
var
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   if fileName<>'' then begin
      vectorFileClass:=GetVectorFileFormats.FindFromFileName(filename);
      newVectorFile:=VectorFileClass.Create(Self);
      try
         newVectorFile.ResourceName:=filename;
         PrepareVectorFile(newVectorFile);
         newVectorFile.SaveToStream(aStream);
      finally
         newVectorFile.Free;
      end;
   end;
end;

// AddDataFromFile
//
procedure TGLBaseMesh.AddDataFromFile(const filename : String);
var
   fs : TStream;
begin
   if fileName<>'' then begin
      fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyWrite);
      try
         AddDataFromStream(fileName, fs);
      finally
         fs.Free;
      end;
   end;
end;

// AddDataFromStream
//
procedure TGLBaseMesh.AddDataFromStream(const filename : String; aStream : TStream);
var
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   if fileName <> '' then begin
      vectorFileClass:=GetVectorFileFormats.FindFromFileName(filename);
      newVectorFile:=VectorFileClass.Create(Self);
      newVectorFile.ResourceName:=filename;
      PrepareVectorFile(newVectorFile);
      try
         if Assigned(Scene) then Scene.BeginUpdate;
         newVectorFile.LoadFromStream(aStream);
         if Assigned(Scene) then Scene.EndUpdate;
      finally
         NewVectorFile.Free;
      end;
      PrepareMesh;
   end;
end;

// GetExtents
//
procedure TGLBaseMesh.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   lMin, lMax : TAffineVector;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to MeshObjects.Count-1 do begin
      TMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
      for k:=0 to 2 do begin
          if lMin[k]<min[k] then min[k]:=lMin[k];
          if lMax[k]>max[k] then max[k]:=lMax[k];
      end;
   end;
end;

// GetBarycenter
//
function TGLBaseMesh.GetBarycenter : TAffineVector;
var
   i, nb : Integer;
begin
   Result:=NullVector;
   nb:=0;
   for i:=0 to MeshObjects.Count-1 do
      TMeshObject(MeshObjects[i]).ContributeToBarycenter(Result, nb);
   if nb>0 then
      ScaleVector(Result, 1/nb);
end;

// LastLoadedFilename
//
function TGLBaseMesh.LastLoadedFilename: string;
begin
   result:= FLastLoadedFilename;
end;


// SetMaterialLibrary
//
procedure TGLBaseMesh.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   if FMaterialLibrary<>val then begin
      if FMaterialLibraryCachesPrepared then
         DropMaterialLibraryCache;
      if Assigned(FMaterialLibrary) then begin
         DestroyHandle;
         FMaterialLibrary.RemoveFreeNotification(Self);
      end;
      FMaterialLibrary:=val;
      if Assigned(FMaterialLibrary) then
         FMaterialLibrary.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetMaterialLibrary
//
procedure TGLBaseMesh.SetLightmapLibrary(const val : TGLMaterialLibrary);
begin
   if FLightmapLibrary<>val then begin
      if Assigned(FLightmapLibrary) then begin
         DestroyHandle;
         FLightmapLibrary.RemoveFreeNotification(Self);
      end;
      FLightmapLibrary:=val;
      if Assigned(FLightmapLibrary) then
         FLightmapLibrary.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetNormalsOrientation
//
procedure TGLBaseMesh.SetNormalsOrientation(const val : TMeshNormalsOrientation);
begin
   if val<>FNormalsOrientation then begin
      FNormalsOrientation:=val;
      StructureChanged;
   end;
end;

// SetOverlaySkeleton
//
procedure TGLBaseMesh.SetOverlaySkeleton(const val : Boolean);
begin
   if FOverlaySkeleton<>val then begin
      FOverlaySkeleton:=val;
      NotifyChange(Self);
   end;
end;

// SetAutoScaling
//
procedure TGLBaseMesh.SetAutoScaling(const Value: TGLCoordinates);
begin
   FAutoScaling.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

// Notification
//
procedure TGLBaseMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FMaterialLibrary then
         MaterialLibrary:=nil
      else if AComponent=FLightmapLibrary then
         LightmapLibrary:=nil;
   end;
   inherited;
end;

// AxisAlignedDimensions
//
{
function TGLBaseMesh.AxisAlignedDimensions : TVector;
var
   dMin, dMax : TAffineVector;
begin
   if FAxisAlignedDimensionsCache[0]<0 then begin
      MeshObjects.GetExtents(dMin, dMax);
      FAxisAlignedDimensionsCache[0]:=MaxFloat(Abs(dMin[0]), Abs(dMax[0]));
      FAxisAlignedDimensionsCache[1]:=MaxFloat(Abs(dMin[1]), Abs(dMax[1]));
      FAxisAlignedDimensionsCache[2]:=MaxFloat(Abs(dMin[2]), Abs(dMax[2]));
   end;
   SetVector(Result, FAxisAlignedDimensionsCache);
   ScaleVector(Result,Scale.AsVector);  //added by DanB
end;
}
// AxisAlignedDimensionsUnscaled
//
function TGLBaseMesh.AxisAlignedDimensionsUnscaled : TVector;
var
   dMin, dMax : TAffineVector;
begin
   if FAxisAlignedDimensionsCache[0]<0 then begin
      MeshObjects.GetExtents(dMin, dMax);
      FAxisAlignedDimensionsCache[0]:=MaxFloat(Abs(dMin[0]), Abs(dMax[0]));
      FAxisAlignedDimensionsCache[1]:=MaxFloat(Abs(dMin[1]), Abs(dMax[1]));
      FAxisAlignedDimensionsCache[2]:=MaxFloat(Abs(dMin[2]), Abs(dMax[2]));
   end;
   SetVector(Result, FAxisAlignedDimensionsCache);
end;

// DestroyHandle
//
procedure TGLBaseMesh.DestroyHandle;
begin
   if Assigned(FMaterialLibrary) then
      MaterialLibrary.DestroyHandles;
   if Assigned(FLightmapLibrary) then
      LightmapLibrary.DestroyHandles;
   inherited;
end;

// PrepareVectorFile
//
procedure TGLBaseMesh.PrepareVectorFile(aFile : TVectorFile);
begin
   aFile.NormalsOrientation:=NormalsOrientation;
end;

// PerformAutoCentering
//
procedure TGLBaseMesh.PerformAutoCentering;
var
   delta, min, max : TAffineVector;
begin
   if macUseBarycenter in AutoCentering then begin
      delta:=VectorNegate(GetBarycenter);
   end else begin
      GetExtents(min, max);
      if macCenterX in AutoCentering then
         delta[0]:=-0.5*(min[0]+max[0])
      else delta[0]:=0;
      if macCenterY in AutoCentering then
         delta[1]:=-0.5*(min[1]+max[1])
      else delta[1]:=0;
      if macCenterZ in AutoCentering then
         delta[2]:=-0.5*(min[2]+max[2])
      else delta[2]:=0;
   end;
   MeshObjects.Translate(delta);
end;

// PerformAutoScaling
//
procedure TGLBaseMesh.PerformAutoScaling;
var
   i: integer;
   vScal : TAffineFltVector;
begin
     if (FAutoScaling.DirectX<>1) or (FAutoScaling.DirectY<>1) or (FAutoScaling.DirectZ<>1) then
     begin
          MakeVector(vScal,FAutoScaling.DirectX,FAutoScaling.DirectY,FAutoScaling.DirectZ);
          for i := 0 to MeshObjects.Count-1 do
          begin
               MeshObjects[i].Vertices.Scale(vScal);
          end;
     end;
end;


// PrepareMesh
//
procedure TGLBaseMesh.PrepareMesh;
begin
   StructureChanged;
end;

// PrepareMaterialLibraryCache
//
procedure TGLBaseMesh.PrepareMaterialLibraryCache;
begin
   if FMaterialLibraryCachesPrepared then
      DropMaterialLibraryCache;
   MeshObjects.PrepareMaterialLibraryCache(FMaterialLibrary);
   FMaterialLibraryCachesPrepared:=True;
end;

// DropMaterialLibraryCache
//
procedure TGLBaseMesh.DropMaterialLibraryCache;
begin
   if FMaterialLibraryCachesPrepared then begin
      MeshObjects.DropMaterialLibraryCache;
      FMaterialLibraryCachesPrepared:=False;
   end;
end;

// PrepareBuildList
//
procedure TGLBaseMesh.PrepareBuildList(var mrci : TRenderContextInfo);
begin
   MeshObjects.PrepareBuildList(mrci);
   if LightmapLibrary<>nil then
      LightmapLibrary.Materials.PrepareBuildList
end;

// SetUseMeshMaterials
//
procedure TGLBaseMesh.SetUseMeshMaterials(const val : Boolean);
begin
   if val<>FUseMeshMaterials then begin
      FUseMeshMaterials:=val;
      if FMaterialLibraryCachesPrepared and (not val) then
         DropMaterialLibraryCache;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TGLBaseMesh.BuildList(var rci : TRenderContextInfo);
begin
   MeshObjects.BuildList(rci);
end;

// DoRender
//
procedure TGLBaseMesh.DoRender(var rci : TRenderContextInfo;
                               renderSelf, renderChildren : Boolean);
begin
   if Assigned(LightmapLibrary) then
      xglForbidSecondTextureUnit;
   if renderSelf then begin
      // set winding
      case FNormalsOrientation of
         mnoDefault : ;// nothing
         mnoInvert : rci.GLStates.InvertGLFrontFace;
      else
         Assert(False);
      end;
      if not rci.ignoreMaterials then begin
         if UseMeshMaterials and Assigned(MaterialLibrary) then begin
            rci.materialLibrary:=MaterialLibrary;
            if not FMaterialLibraryCachesPrepared then
               PrepareMaterialLibraryCache;
         end else rci.materialLibrary:=nil;
         if Assigned(LightmapLibrary) then
            rci.lightmapLibrary:=LightmapLibrary
         else rci.lightmapLibrary:=nil;
         if    rci.amalgamating
            or not (ListHandleAllocated or (osDirectDraw in ObjectStyle)) then
            PrepareBuildList(rci);
         Material.Apply(rci);
         repeat
            if (osDirectDraw in ObjectStyle) or rci.amalgamating then
               BuildList(rci)
            else glCallList(GetHandle(rci));
         until not Material.UnApply(rci);
         rci.materialLibrary:=nil;
      end else begin
         if (osDirectDraw in ObjectStyle) or rci.amalgamating then
            BuildList(rci)
         else glCallList(GetHandle(rci));
      end;
      if FNormalsOrientation<>mnoDefault then
         rci.GLStates.InvertGLFrontFace;
   end;
   if Assigned(LightmapLibrary) then
      xglAllowSecondTextureUnit;
   if renderChildren and (Count>0) then
      Self.RenderChildren(0, Count-1, rci);
end;

// StructureChanged
//
procedure TGLBaseMesh.StructureChanged;
begin
   FAxisAlignedDimensionsCache[0]:=-1;
   DropMaterialLibraryCache;
   MeshObjects.Prepare;
   inherited;
end;

// StructureChangedNoPrepare
//
procedure TGLBaseMesh.StructureChangedNoPrepare;
begin
   inherited StructureChanged;
end;

// RayCastIntersect
//
function TGLBaseMesh.RayCastIntersect(const rayStart, rayVector : TVector;
                                    intersectPoint : PVector = nil;
                                    intersectNormal : PVector = nil) : Boolean;
var
   i : Integer;
   tris : TAffineVectorList;
   locRayStart, locRayVector, iPoint, iNormal : TVector;
   d, minD : Single;
begin
   // BEWARE! Utterly inefficient implementation!
   tris:=MeshObjects.ExtractTriangles;
   try
      SetVector(locRayStart,  AbsoluteToLocal(rayStart));
      SetVector(locRayVector, AbsoluteToLocal(rayVector));
      minD:=-1;
      i:=0; while i<tris.Count do begin
         if RayCastTriangleIntersect(locRayStart, locRayVector,
                                     tris.List[i], tris.List[i+1], tris.List[i+2],
                                     @iPoint, @iNormal) then begin
            d:=VectorDistance2(locRayStart, iPoint);
            if (d<minD) or (minD<0) then begin
               minD:=d;
               if intersectPoint<>nil then
                  intersectPoint^:=iPoint;
               if intersectNormal<>nil then
                  intersectNormal^:=iNormal;
            end;
         end;
         Inc(i, 3);
      end;
   finally
      tris.Free;
   end;
   Result:=(minD>=0);
   if Result then begin
      if intersectPoint<>nil then
         SetVector(intersectPoint^,  LocalToAbsolute(intersectPoint^));
      if intersectNormal<>nil then begin
         SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         if NormalsOrientation=mnoInvert then
            NegateVector(intersectNormal^);
      end;
   end;
end;

// GenerateSilhouette
//
function TGLBaseMesh.GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette;
var
   mc : TGLBaseMeshConnectivity;
   sil : TGLSilhouette;
begin
   sil:=nil;
   if Assigned(FConnectivity) then begin
      mc:=TGLBaseMeshConnectivity(FConnectivity);
      mc.CreateSilhouette(silhouetteParameters,
                              sil,
                              true);
   end else begin
      mc:=TGLBaseMeshConnectivity.CreateFromMesh(Self);
      try
         mc.CreateSilhouette(silhouetteParameters,
                                 sil,
                                 true);
      finally
         mc.Free;
      end;
   end;
   Result:=sil;
end;

// BuildSilhouetteConnectivityData
//
procedure TGLBaseMesh.BuildSilhouetteConnectivityData;
var
   i, j : Integer;
   mo : TMeshObject;
begin
   FreeAndNil(FConnectivity);
   // connectivity data works only on facegroups of TFGVertexIndexList class
   for i:=0 to MeshObjects.Count-1 do begin
      mo:=(MeshObjects[i] as TMeshObject);
      if mo.Mode<>momFacegroups then Exit;
      for j:=0 to mo.FaceGroups.Count-1 do
         if not mo.FaceGroups[j].InheritsFrom(TFGVertexIndexList) then Exit;
   end;
   FConnectivity:=TGLBaseMeshConnectivity.CreateFromMesh(Self);
end;

// ------------------
// ------------------ TGLFreeForm ------------------
// ------------------

// Create
//
constructor TGLFreeForm.Create(AOwner:TComponent);
begin
   inherited;
   FUseMeshMaterials:=True;
end;

// Destroy
//
destructor TGLFreeForm.Destroy;
begin
   FOctree.Free;
   inherited Destroy;
end;

// GetOctree
//
function TGLFreeForm.GetOctree : TOctree;
begin
//   if not Assigned(FOctree) then     //If auto-created, can never use "if Assigned(GLFreeform1.Octree)"
//     FOctree:=TOctree.Create;        //moved this code to BuildOctree
   Result:=FOctree;
end;

// BuildOctree
//
procedure TGLFreeForm.BuildOctree(TreeDepth: integer = 3);
var
   emin, emax : TAffineVector;
   tl : TAffineVectorList;
begin
   if not Assigned(FOctree) then        //moved here from GetOctree
      FOctree:=TOctree.Create;

   GetExtents(emin, emax);
   tl:=MeshObjects.ExtractTriangles;
   try
      with Octree do begin
         DisposeTree;
         InitializeTree(emin, emax, tl, TreeDepth);
      end;
   finally
      tl.Free;
   end;
end;

// OctreeRayCastIntersect
//
function TGLFreeForm.OctreeRayCastIntersect(const rayStart, rayVector : TVector;
                                          intersectPoint : PVector = nil;
                                          intersectNormal : PVector = nil) : Boolean;
var
   locRayStart, locRayVector : TVector;
begin
   Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');
   SetVector(locRayStart,  AbsoluteToLocal(rayStart));
   SetVector(locRayVector, AbsoluteToLocal(rayVector));
   Result:=Octree.RayCastIntersect(locRayStart, locRayVector,
                                       intersectPoint, intersectNormal);
   if Result then begin
      if intersectPoint<>nil then
         SetVector(intersectPoint^,  LocalToAbsolute(intersectPoint^));
      if intersectNormal<>nil then begin
         SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         if NormalsOrientation=mnoInvert then
            NegateVector(intersectNormal^);
      end;
   end;
end;

// OctreePointInMesh
//
function TGLFreeForm.OctreePointInMesh(const Point: TVector): boolean;
const
  cPointRadiusStep = 10000;
var
  rayStart, rayVector, hitPoint, hitNormal : TVector;
  BRad : double;
  HitCount : integer;
  hitDot : double;
begin
  Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');

  result := false;

  // Makes calculations sligthly faster by ignoring cases that are guaranteed
  // to be outside the object
  if not PointInObject(Point) then
    exit;

  BRad := BoundingSphereRadius;

  // This could be a fixed vector, but a fixed vector could have a systemic
  // bug on an non-closed mesh, making it fail constantly for one or several
  // faces.
  rayVector := VectorMake(2*random-1, 2*random-1, 2*random-1);
  rayStart := VectorAdd(VectorScale(rayVector, -BRad), Point);

  HitCount := 0;

  while OctreeRayCastIntersect(rayStart, rayVector, @hitPoint, @hitNormal) do begin
    // Are we past our taget?
    if VectorDotProduct(rayVector, VectorSubtract(Point, hitPoint))<0 then begin
      result := HitCount>0;
      exit;
    end;

    hitDot := VectorDotProduct(hitNormal, rayVector);
    if hitDot<0 then
      inc(HitCount)
    else if hitDot>0 then
      dec(HitCount);

    // ditDot = 0 is a tricky special case where the ray is just grazing the
    // side of a face - this case means that it doesn't necessarily actually
    // enter the mesh - but it _could_ enter the mesh. If this situation occurs,
    // we should restart the run using a new rayVector - but this implementation
    // currently doesn't.

    // Restart the ray slightly beyond the point it hit the previous face. Note
    // that this step introduces a possible issue with faces that are very close
    rayStart := VectorAdd(hitPoint, VectorScale(rayVector, BRad/cPointRadiusStep));
  end;
end;

// OctreeSphereIntersect
//
function TGLFreeForm.OctreeSphereSweepIntersect(const rayStart, rayVector : TVector;
                                         const velocity, radius: Single;
                                         intersectPoint : PVector = nil;
                                         intersectNormal : PVector = nil) : Boolean;
var
   locRayStart, locRayVector : TVector;
begin
   Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');
   SetVector(locRayStart,  AbsoluteToLocal(rayStart));
   SetVector(locRayVector, AbsoluteToLocal(rayVector));
   Result:=Octree.SphereSweepIntersect(locRayStart, locRayVector,
                                      velocity, radius,
                                      intersectPoint, intersectNormal);
   if Result then begin
      if intersectPoint<>nil then
         SetVector(intersectPoint^,  LocalToAbsolute(intersectPoint^));
      if intersectNormal<>nil then begin
         SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
         if NormalsOrientation=mnoInvert then
            NegateVector(intersectNormal^);
      end;
   end;
end;

// OctreeTriangleIntersect
//
function TGLFreeForm.OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
var
  t1, t2, t3: TAffineVector;
begin
   Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');
   SetVector(t1, AbsoluteToLocal(v1));
   SetVector(t2, AbsoluteToLocal(v2));
   SetVector(t3, AbsoluteToLocal(v3));

   Result:= Octree.TriangleIntersect(t1, t2, t3);
end;


// OctreeAABBIntersect
//
function TGLFreeForm.OctreeAABBIntersect(const AABB: TAABB;
objMatrix, invObjMatrix: TMatrix; triangles:TAffineVectorList = nil): boolean;
var
  m1to2, m2to1: TMatrix;
begin
     Assert(Assigned(FOctree), 'Octree must have been prepared and setup before use.');

     //get matrixes needed
     //object to self
     MatrixMultiply(objMatrix, InvAbsoluteMatrix, m1to2);
     //self to object
     MatrixMultiply( AbsoluteMatrix,invObjMatrix, m2to1);

     result:=octree.AABBIntersect(aabb,m1to2,m2to1,triangles);
end;

// ------------------
// ------------------ TActorAnimation ------------------
// ------------------

// Create
//
constructor TActorAnimation.Create(Collection : TCollection);
begin
	inherited Create(Collection);
end;

// Destroy
//
destructor TActorAnimation.Destroy;
begin
   with (Collection as TActorAnimations).FOwner do
      if FTargetSmoothAnimation=Self then
         FTargetSmoothAnimation:=nil;
	inherited Destroy;
end;

// Assign
//
procedure TActorAnimation.Assign(Source: TPersistent);
begin
	if Source is TActorAnimation then begin
      FName:=TActorAnimation(Source).FName;
      FStartFrame:=TActorAnimation(Source).FStartFrame;
      FEndFrame:=TActorAnimation(Source).FEndFrame;
      FReference:=TActorAnimation(Source).FReference;
	end else inherited;
end;

// GetDisplayName
//
function TActorAnimation.GetDisplayName : String;
begin
	Result:=Format('%d - %s [%d - %d]', [Index, Name, StartFrame, EndFrame]);
end;

// FrameCount
//
function TActorAnimation.FrameCount : Integer;
begin
   case Reference of
      aarMorph :
         Result:=TActorAnimations(Collection).FOwner.MeshObjects.MorphTargetCount;
      aarSkeleton :
         Result:=TActorAnimations(Collection).FOwner.Skeleton.Frames.Count;
   else
      Result:=0;
      Assert(False);
   end;
end;

// SetStartFrame
//
procedure TActorAnimation.SetStartFrame(const val : Integer);
var
   m : Integer;
begin
   if val<0 then
      FStartFrame:=0
   else begin
      m:=FrameCount;
      if val>=m then
         FStartFrame:=m-1
      else FStartFrame:=val;
   end;
   if FStartFrame>FEndFrame then
      FEndFrame:=FStartFrame;
end;

// SetEndFrame
//
procedure TActorAnimation.SetEndFrame(const val : Integer);
var
   m : Integer;
begin
   if val<0 then
      FEndFrame:=0
   else begin
      m:=FrameCount;
      if val>=m then
        FEndFrame:=m-1
      else FEndFrame:=val;
   end;
   if FStartFrame>FEndFrame then
      FStartFrame:=FEndFrame;
end;

// SetReference
//
procedure TActorAnimation.SetReference(val : TActorAnimationReference);
begin
   if val<>FReference then begin
      FReference:=val;
      StartFrame:=StartFrame;
      EndFrame:=EndFrame;
   end;
end;

// SetAsString
//
procedure TActorAnimation.SetAsString(const val : String);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.CommaText:=val;
      Assert(sl.Count>=3);
      FName:=sl[0];
      FStartFrame:=StrToInt(sl[1]);
      FEndFrame:=StrToInt(sl[2]);
      if sl.Count=4 then begin
         if LowerCase(sl[3])='morph' then
            Reference:=aarMorph
         else if LowerCase(sl[3])='skeleton' then
            Reference:=aarSkeleton
         else Assert(False);
      end else Reference:=aarMorph;
   finally
      sl.Free;
   end;
end;

// GetAsString
//
function TActorAnimation.GetAsString : String;
const
   cAARToString : array [aarMorph..aarSkeleton] of String = ('morph', 'skeleton');
begin
   Result:=Format('"%s",%d,%d,%s',
                  [FName, FStartFrame, FEndFrame, cAARToString[Reference]]);
end;

// OwnerActor
//
function TActorAnimation.OwnerActor : TGLActor;
begin
   Result:=((Collection as TActorAnimations).GetOwner as TGLActor);
end;

// MakeSkeletalTranslationStatic
//
procedure TActorAnimation.MakeSkeletalTranslationStatic;
begin
   OwnerActor.Skeleton.MakeSkeletalTranslationStatic(StartFrame, EndFrame);
end;

// MakeSkeletalRotationDelta
//
procedure TActorAnimation.MakeSkeletalRotationDelta;
begin
   OwnerActor.Skeleton.MakeSkeletalRotationDelta(StartFrame, EndFrame);
end;

// ------------------
// ------------------ TActorAnimations ------------------
// ------------------

// Create
//
constructor TActorAnimations.Create(AOwner : TGLActor);
begin
	FOwner:=AOwner;
	inherited Create(TActorAnimation);
end;

// GetOwner
//
function TActorAnimations.GetOwner: TPersistent;
begin
	Result:=FOwner;
end;

// SetItems
//
procedure TActorAnimations.SetItems(index : Integer; const val : TActorAnimation);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TActorAnimations.GetItems(index : Integer) : TActorAnimation;
begin
	Result:=TActorAnimation(inherited Items[index]);
end;

// Last
//
function TActorAnimations.Last : TActorAnimation;
begin
   if Count>0 then
      Result:=TActorAnimation(inherited Items[Count-1])
   else Result:=nil;
end;

// Add
//
function TActorAnimations.Add: TActorAnimation;
begin
	Result:=(inherited Add) as TActorAnimation;
end;

// FindItemID
//
function TActorAnimations.FindItemID(ID: Integer): TActorAnimation;
begin
	Result:=(inherited FindItemID(ID)) as TActorAnimation;
end;

// FindName
//
function TActorAnimations.FindName(const aName : String) : TActorAnimation;
var
   i : Integer;
begin
	Result:=nil;
   for i:=0 to Count-1 do if CompareText(Items[i].Name, aName)=0 then begin
      Result:=Items[i];
      Break;
   end;
end;

// FindFrame
//
function TActorAnimations.FindFrame(aFrame : Integer;
                        aReference : TActorAnimationReference) : TActorAnimation;
var
   i : Integer;
begin
	Result:=nil;
   for i:=0 to Count-1 do with Items[i] do
      if (StartFrame<=aFrame) and (EndFrame>=aFrame)
            and (Reference=aReference) then begin
         Result:=Items[i];
         Break;
      end;
end;

// SetToStrings
//
procedure TActorAnimations.SetToStrings(aStrings : TStrings);

var
   i : Integer;
begin
   with aStrings do begin
      BeginUpdate;
      Clear;
      for i:=0 to Self.Count-1 do
         Add(Self.Items[i].Name);
      EndUpdate;
   end;
end;

// SaveToStream
//
procedure TActorAnimations.SaveToStream(aStream : TStream);
var
   i : Integer;
begin
   WriteCRLFString(aStream, cAAFHeader);
   WriteCRLFString(aStream, IntToStr(Count));
   for i:=0 to Count-1 do
      WriteCRLFString(aStream, Items[i].AsString);
end;

// LoadFromStream
//
procedure TActorAnimations.LoadFromStream(aStream : TStream);
var
   i, n : Integer;
begin
   Clear;
   if ReadCRLFString(aStream)<>cAAFHeader then Assert(False);
   n:=StrToInt(ReadCRLFString(aStream));
   for i:=0 to n-1 do
      Add.AsString:=ReadCRLFString(aStream);
end;

// SaveToFile
//
procedure TActorAnimations.SaveToFile(const fileName : String);
var
   fs : TStream;
begin
   fs:=CreateFileStream(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TActorAnimations.LoadFromFile(const fileName : String);
var
   fs : TStream;
begin
   fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyWrite);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// ------------------
// ------------------ TGLBaseAnimationControler ------------------
// ------------------

// Create
//
constructor TGLBaseAnimationControler.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FEnabled:=True;
end;

// Destroy
//
destructor TGLBaseAnimationControler.Destroy;
begin
   SetActor(nil);
	inherited Destroy;
end;

// Notification
//
procedure TGLBaseAnimationControler.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (AComponent=FActor) and (Operation=opRemove) then
      SetActor(nil);
   inherited;
end;

// DoChange
//
procedure TGLBaseAnimationControler.DoChange;
begin
   if Assigned(FActor) then
      FActor.NotifyChange(Self);
end;

// SetEnabled
//
procedure TGLBaseAnimationControler.SetEnabled(const val : Boolean);
begin
   if val<>FEnabled then begin
      FEnabled:=val;
      if Assigned(FActor) then DoChange;
   end;
end;

// SetActor
//
procedure TGLBaseAnimationControler.SetActor(const val : TGLActor);
begin
   if FActor<>val then begin
      if Assigned(FActor) then
         FActor.UnRegisterControler(Self);
      FActor:=val;
      if Assigned(FActor) then begin
         FActor.RegisterControler(Self);
         DoChange;
      end;
   end;
end;

// Apply
//
function TGLBaseAnimationControler.Apply(var lerpInfo : TBlendedLerpInfo) : Boolean;
begin
   // virtual
   Result:=False;
end;

// ------------------
// ------------------ TGLAnimationControler ------------------
// ------------------

// DoChange
//
procedure TGLAnimationControler.DoChange;
begin
   if AnimationName<>'' then
      inherited;
end;

// SetAnimationName
//
procedure TGLAnimationControler.SetAnimationName(const val : TActorAnimationName);
begin
   if FAnimationName<>val then begin
      FAnimationName:=val;
      DoChange;
   end;
end;

// SetRatio
//
procedure TGLAnimationControler.SetRatio(const val : Single);
begin
   if FRatio<>val then begin
      FRatio:=ClampValue(val, 0, 1);
      DoChange;
   end;
end;

// Apply
//
function TGLAnimationControler.Apply(var lerpInfo : TBlendedLerpInfo) : Boolean;
var
   anim : TActorAnimation;
   baseDelta : Integer;
begin
   if not Enabled then begin
      Result:=False;
      Exit;
   end;

   anim:=Actor.Animations.FindName(AnimationName);
   Result:=(anim<>nil);
   if not Result then Exit;

   with lerpInfo do begin
      if Ratio=0 then begin
         frameIndex1:=anim.StartFrame;
         frameIndex2:=frameIndex1;
         lerpFactor:=0;
      end else if Ratio=1 then begin
         frameIndex1:=anim.EndFrame;
         frameIndex2:=frameIndex1;
         lerpFactor:=0;
      end else begin
         baseDelta:=anim.EndFrame-anim.StartFrame;
         lerpFactor:=anim.StartFrame+baseDelta*Ratio;
         frameIndex1:=VectorGeometry.Trunc(lerpFactor);
         frameIndex2:=frameIndex1+1;
         lerpFactor:=VectorGeometry.Frac(lerpFactor);
      end;
      weight:=1;
      externalRotations:=nil;
      externalQuaternions:=nil;
   end;
end;

// ------------------
// ------------------ TGLActor ------------------
// ------------------

// Create
//
constructor TGLActor.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FFrameInterpolation:=afpLinear;
   FAnimationMode:=aamNone;
   FInterval:=100; // 10 animation frames per second
   FAnimations:=TActorAnimations.Create(Self);
   FControlers:=nil; // created on request
   FOptions:=cDefaultGLActorOptions;
end;

// Destroy
//
destructor TGLActor.Destroy;
begin
   inherited Destroy;
   FControlers.Free;
   FAnimations.Free;
end;

// Assign
//
procedure TGLActor.Assign(Source: TPersistent);
begin
   inherited Assign(Source);
   if Source is TGLActor then begin
      FAnimations.Assign(TGLActor(Source).FAnimations);
      FAnimationMode:=TGLActor(Source).FAnimationMode;
      Synchronize(TGLActor(Source));
   end;
end;

// RegisterControler
//
procedure TGLActor.RegisterControler(aControler : TGLBaseAnimationControler);
begin
   if not Assigned(FControlers) then
      FControlers:=TList.Create;
   FControlers.Add(aControler);
   FreeNotification(aControler);
end;

// UnRegisterControler
//
procedure TGLActor.UnRegisterControler(aControler : TGLBaseAnimationControler);
begin
   Assert(Assigned(FControlers));
   FControlers.Remove(aControler);
   RemoveFreeNotification(aControler);
   if FControlers.Count=0 then
      FreeAndNil(FControlers);
end;

// SetCurrentFrame
//
procedure TGLActor.SetCurrentFrame(val : Integer);
begin
   if val<>CurrentFrame then begin
      if val>FrameCount-1 then
         FCurrentFrame:=FrameCount-1
      else if val<0 then
         FCurrentFrame:=0
      else FCurrentFrame:=val;
      FCurrentFrameDelta:=0;
      case AnimationMode of
         aamPlayOnce :
            if CurrentFrame=EndFrame then FAnimationMode:=aamNone;
         aamBounceForward :
            if CurrentFrame=EndFrame then FAnimationMode:=aamBounceBackward;
         aamBounceBackward :
            if CurrentFrame=StartFrame then FAnimationMode:=aamBounceForward;
      end;
      StructureChanged;
      if Assigned(FOnFrameChanged) then FOnFrameChanged(Self);
   end;
end;

// SetStartFrame
//
procedure TGLActor.SetStartFrame(val : Integer);
begin
   if (val>=0) and (val<FrameCount) and (val<>StartFrame) then
      FStartFrame:=val;
   if EndFrame<StartFrame then
      FEndFrame:=FStartFrame;
   if CurrentFrame<StartFrame then
      CurrentFrame:=FStartFrame;
end;

// SetEndFrame
//
procedure TGLActor.SetEndFrame(val : Integer);
begin
   if (val>=0) and (val<FrameCount) and (val<>EndFrame) then
      FEndFrame:=val;
   if CurrentFrame>EndFrame then
      CurrentFrame:=FEndFrame;
end;

// SetReference
//
procedure TGLActor.SetReference(val : TActorAnimationReference);
begin
   if val<>Reference then begin
      FReference:=val;
      StartFrame:=StartFrame;
      EndFrame:=EndFrame;
      CurrentFrame:=CurrentFrame;
      StructureChanged;
   end;
end;

// SetAnimations
//
procedure TGLActor.SetAnimations(const val : TActorAnimations);
begin
   FAnimations.Assign(val);
end;

// StoreAnimations
//
function TGLActor.StoreAnimations : Boolean;
begin
   Result:=(FAnimations.Count>0);
end;

// SetOptions
//
procedure TGLActor.SetOptions(const val : TGLActorOptions);
begin
   if val<>FOptions then begin
      FOptions:=val;
      StructureChanged;
   end;
end;

// NextFrameIndex
//
function TGLActor.NextFrameIndex : Integer;
begin
   case AnimationMode of
      aamLoop, aamBounceForward : begin
         if FTargetSmoothAnimation<>nil then
            Result:=FTargetSmoothAnimation.StartFrame
         else begin
            Result:=CurrentFrame+1;
            if Result>EndFrame then begin
               Result:=StartFrame+(Result-EndFrame-1);
               if Result>EndFrame then
                  Result:=EndFrame;
            end;
         end;
      end;
      aamNone, aamPlayOnce : begin
         if FTargetSmoothAnimation<>nil then
            Result:=FTargetSmoothAnimation.StartFrame
         else begin
            Result:=CurrentFrame+1;
            if Result>EndFrame then
               Result:=EndFrame;
         end;
      end;
      aamBounceBackward,aamLoopBackward : begin
         if FTargetSmoothAnimation<>nil then
            Result:=FTargetSmoothAnimation.StartFrame
         else begin
            Result:=CurrentFrame-1;
            if Result<StartFrame then begin
               Result:=EndFrame-(StartFrame-Result-1);
               if Result<StartFrame then
                  Result:=StartFrame;
            end;
         end;
      end;
      aamExternal : Result:=CurrentFrame; // Do nothing
   else
      Result:=CurrentFrame;
      Assert(False);
   end;
end;

// NextFrame
//
procedure TGLActor.NextFrame(nbSteps : Integer = 1);
var
   n : Integer;
begin
   n:=nbSteps;
   while n>0 do begin
      CurrentFrame:=NextFrameIndex;
      Dec(n);
      if Assigned(FOnEndFrameReached) and (CurrentFrame=EndFrame) then
         FOnEndFrameReached(Self);
      if Assigned(FOnStartFrameReached) and (CurrentFrame=StartFrame) then
         FOnStartFrameReached(Self);
   end;
end;

// PrevFrame
//
procedure TGLActor.PrevFrame(nbSteps : Integer = 1);
var
   value : Integer;
begin
   value:=FCurrentFrame-nbSteps;
   if value<FStartFrame then begin
      value:=FEndFrame-(FStartFrame-value);
      if value<FStartFrame then
         value:=FStartFrame;
   end;
   CurrentFrame:=Value;
end;

// BuildList
//
procedure TGLActor.BuildList(var rci : TRenderContextInfo);
var
   i, k : Integer;
   nextFrameIdx : Integer;
   lerpInfos : array of TBlendedLerpInfo;
begin
   nextFrameIdx:=NextFrameIndex;
   case Reference of
      aarMorph : if nextFrameIdx>=0 then begin
         case FrameInterpolation of
            afpLinear :
               MeshObjects.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta)
         else
            MeshObjects.MorphTo(CurrentFrame);
         end;
      end;
      aarSkeleton : if Skeleton.Frames.Count>0 then begin
         if Assigned(FControlers) and (AnimationMode<>aamExternal) then begin
            // Blended Skeletal Lerping
            SetLength(lerpInfos, FControlers.Count+1);
            if nextFrameIdx>=0 then begin
               case FrameInterpolation of
                  afpLinear : with lerpInfos[0] do begin
                     frameIndex1:=CurrentFrame;
                     frameIndex2:=nextFrameIdx;
                     lerpFactor:=CurrentFrameDelta;
                     weight:=1;
                  end;
               else
                  with lerpInfos[0] do begin
                     frameIndex1:=CurrentFrame;
                     frameIndex2:=CurrentFrame;
                     lerpFactor:=0;
                     weight:=1;
                  end;
               end;
            end else begin
               with lerpInfos[0] do begin
                  frameIndex1:=CurrentFrame;
                  frameIndex2:=CurrentFrame;
                  lerpFactor:=0;
                  weight:=1;
               end;
            end;
            k:=1;
            for i:=0 to FControlers.Count-1 do
               if TGLBaseAnimationControler(FControlers[i]).Apply(lerpInfos[k]) then
                  Inc(k);
            SetLength(lerpInfos, k);
            Skeleton.BlendedLerps(lerpInfos);
         end else if (nextFrameIdx>=0) and (AnimationMode<>aamExternal) then begin
            // Single Skeletal Lerp
            case FrameInterpolation of
               afpLinear :
                  Skeleton.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta);
            else
               Skeleton.SetCurrentFrame(Skeleton.Frames[CurrentFrame]);
            end;
         end;
         Skeleton.MorphMesh(aoSkeletonNormalizeNormals in Options);
      end;
      aarNone : ; // do nothing
   end;
   inherited;
   if OverlaySkeleton then begin
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      Skeleton.RootBones.BuildList(rci);
      glPopAttrib;
   end;
end;

// PrepareMesh
//
procedure TGLActor.PrepareMesh;
begin
   FStartFrame:=0;
   FEndFrame:=FrameCount-1;
   FCurrentFrame:=0;
   if Assigned(FOnFrameChanged) then FOnFrameChanged(Self);
   inherited;
end;

// PrepareBuildList
//
procedure TGLActor.PrepareBuildList(var mrci : TRenderContextInfo);
begin
   // no preparation needed for actors, they don't use buildlists
end;

// FrameCount
//
function TGLActor.FrameCount : Integer;
begin
   case Reference of
      aarMorph :
         Result:=MeshObjects.MorphTargetCount;
      aarSkeleton :
         Result:=Skeleton.Frames.Count;
      aarNone :
         Result:=0;
   else
      Result:=0;
      Assert(False);
   end;
end;

// DoProgress
//
procedure TGLActor.DoProgress(const progressTime : TProgressTimes);
var
   fDelta : Single;
begin
   inherited;
   if (AnimationMode<>aamNone) and (Interval>0) then begin
      if (StartFrame<>EndFrame) and (FrameCount>1)  then begin
         FCurrentFrameDelta:=FCurrentFrameDelta+(progressTime.deltaTime*1000)/FInterval;
         if FCurrentFrameDelta>1 then begin
            if Assigned(FTargetSmoothAnimation) then begin
               SwitchToAnimation(FTargetSmoothAnimation);
               FTargetSmoothAnimation:=nil;
            end;
            // we need to step on
            fDelta:=Frac(FCurrentFrameDelta);
            NextFrame(Trunc(FCurrentFrameDelta));
            FCurrentFrameDelta:=fDelta;
            StructureChanged;
         end else if FrameInterpolation<>afpNone then
            StructureChanged;
      end;
   end;
end;

// LoadFromStream
//
procedure TGLActor.LoadFromStream(const fileName : String; aStream : TStream);
begin
   if fileName<>'' then begin
      Animations.Clear;
      inherited LoadFromStream(fileName, aStream);
   end;
end;

// SwitchToAnimation
//
procedure TGLActor.SwitchToAnimation(const animationName : String; smooth : Boolean = False);
begin
   SwitchToAnimation(Animations.FindName(animationName), smooth);
end;

// SwitchToAnimation
//
procedure TGLActor.SwitchToAnimation(animationIndex : Integer; smooth : Boolean = False);
begin
   if (animationIndex>=0) and (animationIndex<Animations.Count) then
      SwitchToAnimation(Animations[animationIndex], smooth);
end;

// SwitchToAnimation
//
procedure TGLActor.SwitchToAnimation(anAnimation : TActorAnimation; smooth : Boolean = False);
begin
   if Assigned(anAnimation) then begin
      if smooth then begin
         FTargetSmoothAnimation:=anAnimation;
         FCurrentFrameDelta:=0;
      end else begin
         Reference:=anAnimation.Reference;
         StartFrame:=anAnimation.StartFrame;
         EndFrame:=anAnimation.EndFrame;
         CurrentFrame:=StartFrame;
      end;
   end;
end;

// CurrentAnimation
//
function TGLActor.CurrentAnimation : String;
var
   aa : TActorAnimation;
begin
   aa:=Animations.FindFrame(CurrentFrame, Reference);
   if Assigned(aa) then
      Result:=aa.Name
   else Result:='';
end;

// Synchronize
//
procedure TGLActor.Synchronize(referenceActor : TGLActor);
begin
   if Assigned(referenceActor) then begin
      if referenceActor.StartFrame<FrameCount then
         FStartFrame:=referenceActor.StartFrame;
      if referenceActor.EndFrame<FrameCount then
         FEndFrame:=referenceActor.EndFrame;
      FReference:=referenceActor.Reference;
      if referenceActor.CurrentFrame<FrameCount then
         FCurrentFrame:=referenceActor.CurrentFrame;
      FCurrentFrameDelta:=referenceActor.CurrentFrameDelta;
      FAnimationMode:=referenceActor.AnimationMode;
      FFrameInterpolation:=referenceActor.FrameInterpolation;
      if referenceActor.FTargetSmoothAnimation<>nil then
         FTargetSmoothAnimation:=Animations.FindName(referenceActor.FTargetSmoothAnimation.Name)
      else FTargetSmoothAnimation:=nil;
      if (Skeleton.Frames.Count>0) and (referenceActor.Skeleton.Frames.Count>0) then
         Skeleton.Synchronize(referenceActor.Skeleton);
   end;
end;

function TGLActor.isSwitchingAnimation: boolean;
begin
   result:= FTargetSmoothAnimation <> nil;
end;



// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('glsm', 'GLScene Mesh', TGLGLSMVectorFile);

   RegisterClasses([TGLFreeForm, TGLActor, TSkeleton, TSkeletonFrame, TSkeletonBone,
                    TSkeletonMeshObject, TMeshObject, TSkeletonFrameList, TMeshMorphTarget,
                    TMorphableMeshObject, TFaceGroup, TFGVertexIndexList,
                    TFGVertexNormalTexIndexList, TGLAnimationControler,
                    TFGIndexTexCoordList, TSkeletonCollider, TSkeletonColliderList]);

finalization

   vVectorFileFormats.Free;

end.


