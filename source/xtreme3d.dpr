library xtreme3d;

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Messages,
  Windows,
  Winapi.OpenGL,
  VCL.Forms,
  VCL.Graphics,
  VCL.Dialogs,
  VCL.Imaging.PNGImage,
  GLS.ApplicationFileIO,
  GLS.BaseClasses,
  GLS.Blur,
  GLS.BitmapFont,
  GLS.Cadencer,
  GLS.Collision,
  GLS.Color,
  GLS.Context,
  GLS.Coordinates,
  GLS.DCE,
  GLS.ExplosionFx,
  GLS.Extrusion,
  GLS.VectorFileObjects,
  GLS.File3DPDF,
  GLS.File3DS,
  GLS.File3DSSceneObjects,
  GLS.FileASE,
  GLS.FileB3D,
  GLS.FileBMP,
  GLS.FileDDS,
  GLS.FileDEL,
  GLS.FileDXF,
  GLS.FileGL2,
  GLS.FileGLB,
  GLS.FileGLTF,
  GLS.FileGRD,
  GLS.FileGTS,
  GLS.FileHDR,
  GLS.FileJPEG,
  GLS.FileLMTS,
  GLS.FileLWO,
  GLS.FileMD2,
  GLS.FileMD3,
  GLS.FileMD5,
  GLS.FileMDC,
  GLS.FileMP3,
  GLS.FileMS3D,
  GLS.FileNMF,
  GLS.FileNurbs,
  GLS.FileO3TC,
  GLS.FileO3TCImage,
  GLS.FileOBJ,
  GLS.FileOCT,
  GLS.FilePAK,
  GLS.FilePGM,
  GLS.FilePLY,
  GLS.FilePNG,
  GLS.FileQ3BSP,
  GLS.FileQ3MD3,
  GLS.FileSMD,
  GLS.FileSTL,
  GLS.FileTGA,
  GLS.FileTIN,
  GLS.FileVfsPAK,
  GLS.FileVOR,
  GLS.FileVRML,
  GLS.FileWAV,
  GLS.FileX,
  GLS.FileZLIB,
  GLS.FireFX,
  GLS.FPSMovement,
  GLS.GeometryBB,
  GLS.GeomObjects,
  GLS.Graph,
  GLS.HeightData,
  GLS.HUDObjects,
  GLS.LensFlare,
  GLS.Material,
  GLS.MaterialScript,
  GLS.MeshUtils,
  GLS.Mirror,
  GLS.Movement,
  GLS.MultiProxy,
  GLS.Navigator,
  GLS.Objects,
  GLS.OpenGLAdapter,
  GLS.SpacePartition,
  GLS.PersistentClasses,
  GLS.ProcTextures,
  GLS.ProxyObjects,
  GLS.RenderContextInfo,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.Selection,
  GLS.ShadowPlane,
  GLS.ShadowVolume,
  GLS.SkyDome,
  GLS.SpaceText,
  GLS.State,
  GLS.TerrainRenderer,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.ThorFX,
  GLS.TilePlane,
  GLS.Trail,
  GLS.Tree,
  GLS.Utils,
  GLS.VerletTypes,
  GLS.VerletClothify,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorTypes,
  GLS.WaterPlane,
  GLS.WindowsFont,
  GLSL.LineShaders,
  GLSL.MultiMaterialShader,
  GLSL.ShapeShaders,
  GLSL.TextureShaders,
  Physics.ODEImport,
  Physics.ODEManager,
  Physics.ODERagdoll,
  GLSLShader,
  Hashes;

type
    TEmpty = class(TComponent)
        private
    end;

var
    showLoadingErrors: Boolean;
    scene: TGLScene;
    matlib: TGLMaterialLibrary;
    memviewer: TGLMemoryViewer;
    cadencer: TGLCadencer;
    empty: TEmpty;

    collisionPoint: TGLVector;
    collisionNormal: TGLVector;

    ode: TGLODEManager;
    //odeRagdollWorld: TGLODERagdollWorld;
    jointList: TGLODEJointList;

    //kraftRaycastPoint: TKraftVector3;
    //kraftRaycastNormal: TKraftVector3;

    previousTicks: Longint;

{$R *.res}

function ObjToReal(obj: TObject): real;
begin
    result := real(longint(Pointer(obj)));
end;

function PtrToReal(p: Pointer): real;
begin
    result := real(longint(p));
end;

function RealToPtr(v: real): Pointer;
var
    i: NativeInt;
begin
    i := Trunc(v);
    result := Pointer(i);
end;

function StrConv(s: PAnsiChar): String;
begin
    result := System.UTF8ToUnicodeString(s);
end;

function IsKeyDown(vk: integer): Boolean;
begin
   result := (GetAsyncKeyState(vk)<>0);
end;

function LoadStringFromFile(const fileName: String): String;
var
    n: Cardinal;
	  fs: TFileStream;
begin
    if FileStreamExists(fileName) then begin
   	    fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
        try
            n := fs.Size;
   	        SetLength(Result, n);
            if n > 0 then
         	      fs.Read(Result[1], n);
        finally
   	        fs.Free;
        end;
    end
    else Result := '';
end;

function IsExtensionSupported(v: TGLSceneViewer; const Extension: string): Boolean;
var
    Buffer : String;
    ExtPos: Integer;
begin
    v.Buffer.RenderingContext.Activate;
    Buffer := String(glGetString(GL_EXTENSIONS));
    // First find the position of the extension string as substring in Buffer.
    ExtPos := Pos(Extension, Buffer);
    Result := ExtPos > 0;
    // Now check that it isn't only a substring of another extension.
    if Result then
        Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                  or (Buffer[ExtPos + Length(Extension)]=' ');
    v.Buffer.RenderingContext.Deactivate;
end;

function Raycast(
  obj, target: TGLBaseSceneObject;
  var isecPoint, isecNorm: TGLVector): Boolean;
var
    rstart, rdir, ipoint, inorm: TGLVector;
begin
    rstart := obj.AbsolutePosition;
    rdir := obj.AbsoluteDirection;
    if target.RayCastIntersect(rstart, rdir, @ipoint, @inorm) then begin
        isecPoint := ipoint;
        isecNorm := inorm;
        result := True;
    end
    else result := False;
end;

function RecursiveRaycast(
  obj, target: TGLBaseSceneObject;
  rstart, rdir: TGLVector;
  var isecPoint, isecNorm: TGLVector;
  var bestDistance: Single): TGLBaseSceneObject;
var
    ipoint, inorm: TGLVector;
    bestObject: TGLBaseSceneObject;
    resObj: TGLBaseSceneObject;
    d: Single;
    i: Integer;
begin
    bestObject := nil;

    if target.RayCastIntersect(rstart, rdir, @ipoint, @inorm) then begin
        d := VectorDistance(rstart, ipoint);
        if d < bestDistance then begin
            isecPoint := ipoint;
            isecNorm := inorm;
            bestDistance := d;
            bestObject := target;
        end;
    end;

    for i := 0 to target.Count-1 do
    begin
        if target.Children[i].Visible then begin
            resObj := RecursiveRaycast(obj, target.Children[i], rstart, rdir, ipoint, inorm, bestDistance);
            if resObj <> nil then begin
                isecPoint := ipoint;
                isecNorm := inorm;
                bestObject := resObj;
            end;
        end;
    end;

    result := bestObject;
end;

function normalizeSlashes(s: string): string;
var
   i: integer;
begin
   SetLength(Result, Length(s));
   for i := 1 to Length(s) do
      if s[i] = '/' then
         Result[i] := '\'
      else
         Result[i] := s[i];
end;

function InvertBitmap(Bitmap: TBitmap): TBitmap;
begin
   Bitmap.Canvas.CopyMode := cmDstInvert;
   Bitmap.Canvas.CopyRect(Bitmap.Canvas.ClipRect, Bitmap.Canvas, Bitmap.Canvas.ClipRect);
   Bitmap.Canvas.CopyMode := cmSrcCopy;
   Result := Bitmap;
end;

function VectorDivide(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result.V[0]:=v1.V[0]/delta;
   Result.V[1]:=v1.V[1]/delta;
   Result.V[2]:=v1.V[2]/delta;
end;

function VectorMultiply(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result.V[0]:=v1.V[0]*delta;
   Result.V[1]:=v1.V[1]*delta;
   Result.V[2]:=v1.V[2]*delta;
end;

procedure GenMeshTangents(mesh: TGLMeshObject);
var
   i: Integer;
   v,t: array[0..2] of TAffineVector;

   x1, x2, y1, y2, z1, z2, t1, t2, s1, s2: Single;
   sDir, tDir: TAffineVector;
   sTan, tTan: TGLAffineVectorList;
   tangents, bitangents: TGLVectorList;
   sv, tv: array[0..2] of TAffineVector;
   r, oneOverR: Single;
   n, ta: TAffineVector;
   tang: TAffineVector;

   //tangent,
   //binormal   : array[0..2] of TGLVector;
   //vt,tt      : TAffineVector;
   //interp,dot : Single;

begin
   mesh.Tangents.Clear;
   mesh.Binormals.Clear;
   mesh.Tangents.Count:=mesh.Vertices.Count;
   mesh.Binormals.Count:=mesh.Vertices.Count;

   tangents := TGLVectorList.Create;
   tangents.Count:=mesh.Vertices.Count;

   bitangents := TGLVectorList.Create;
   bitangents.Count:=mesh.Vertices.Count;

   sTan := TGLAffineVectorList.Create;
   tTan := TGLAffineVectorList.Create;
   sTan.Count := mesh.Vertices.Count;
   tTan.Count := mesh.Vertices.Count;

   for i:=0 to mesh.TriangleCount-1 do begin
      sv[0] := AffineVectorMake(0, 0, 0);
      tv[0] := AffineVectorMake(0, 0, 0);
      sv[1] := AffineVectorMake(0, 0, 0);
      tv[1] := AffineVectorMake(0, 0, 0);
      sv[2] := AffineVectorMake(0, 0, 0);
      tv[2] := AffineVectorMake(0, 0, 0);

      mesh.SetTriangleData(i,sTan,sv[0],sv[1],sv[2]);
      mesh.SetTriangleData(i,tTan,tv[0],tv[1],tv[2]);
   end;

   for i:=0 to mesh.TriangleCount-1 do begin
      mesh.GetTriangleData(i,mesh.Vertices,v[0],v[1],v[2]);
      mesh.GetTriangleData(i,mesh.TexCoords,t[0],t[1],t[2]);

      x1 := v[1].V[0] - v[0].V[0];
      x2 := v[2].V[0] - v[0].V[0];
      y1 := v[1].V[1] - v[0].V[1];
      y2 := v[2].V[1] - v[0].V[1];
      z1 := v[1].V[2] - v[0].V[2];
      z2 := v[2].V[2] - v[0].V[2];

      s1 := t[1].V[0] - t[0].V[0];
      s2 := t[2].V[0] - t[0].V[0];
      t1 := t[1].V[1] - t[0].V[1];
      t2 := t[2].V[1] - t[0].V[1];

      r := (s1 * t2) - (s2 * t1);

      if r = 0.0 then
        r := 1.0;

      oneOverR := 1.0 / r;

      sDir.V[0] := (t2 * x1 - t1 * x2) * oneOverR;
      sDir.V[1] := (t2 * y1 - t1 * y2) * oneOverR;
      sDir.V[2] := (t2 * z1 - t1 * z2) * oneOverR;

      tDir.V[0] := (s1 * x2 - s2 * x1) * oneOverR;
      tDir.V[1] := (s1 * y2 - s2 * y1) * oneOverR;
      tDir.V[2] := (s1 * z2 - s2 * z1) * oneOverR;

      mesh.GetTriangleData(i,sTan,sv[0],sv[1],sv[2]);
      mesh.GetTriangleData(i,tTan,tv[0],tv[1],tv[2]);

      sv[0] := VectorAdd(sv[0], sDir);
      tv[0] := VectorAdd(tv[0], tDir);
      sv[1] := VectorAdd(sv[1], sDir);
      tv[1] := VectorAdd(tv[1], tDir);
      sv[2] := VectorAdd(sv[2], sDir);
      tv[2] := VectorAdd(tv[2], tDir);

      mesh.SetTriangleData(i,sTan,sv[0],sv[1],sv[2]);
      mesh.SetTriangleData(i,tTan,tv[0],tv[1],tv[2]);
   end;

   for i:=0 to mesh.Vertices.Count-1 do begin
      n := mesh.Normals[i];
      ta := sTan[i];
      tang := VectorSubtract(ta, VectorMultiply(n, VectorDotProduct(n, ta)));
      tang := VectorNormalize(tang);

      tangents[i] := VectorMake(tang, 1);
      bitangents[i] := VectorMake(VectorCrossProduct(n, tang), 1);
   end;

   mesh.Tangents := tangents;
   mesh.Binormals := bitangents;
end;

function getODEBehaviour(obj: TGLBaseSceneObject): TGLODEBehaviour;
begin
  result := TGLODEBehaviour(obj.Behaviours.GetByClass(TGLODEBehaviour));
end;

function getJointAxisParams(j: TGLODEJointBase; axis: Integer): TGLODEJointParams;
var
  res: TGLODEJointParams;
begin
  if j is TGLODEJointHinge then
  begin
    if axis = 1 then
      res := TGLODEJointHinge(j).AxisParams;
  end
  else if j is TGLODEJointHinge2 then
  begin
    if axis = 1 then
      res := TGLODEJointHinge2(j).Axis1Params
    else if axis = 2 then
      res := TGLODEJointHinge2(j).Axis2Params;
  end
  else if j is TGLODEJointUniversal then
  begin
    if axis = 1 then
      res := TGLODEJointUniversal(j).Axis1Params
    else if axis = 2 then
      res := TGLODEJointUniversal(j).Axis2Params;
  end;
  result := res;
end;

{$I 'xtreme3d/engine'}
{$I 'xtreme3d/pak'}
{$I 'xtreme3d/viewer'}
{$I 'xtreme3d/dummycube'}
{$I 'xtreme3d/camera'}
{$I 'xtreme3d/light'}
//{$I 'xtreme3d/lightfx'}
{$I 'xtreme3d/fonttext'}
{$I 'xtreme3d/sprite'}
//{$I 'xtreme3d/hudshapes'}
{$I 'xtreme3d/primitives'}
{$I 'xtreme3d/actor'}
{$I 'xtreme3d/freeform'}
{$I 'xtreme3d/object'}
{$I 'xtreme3d/material'}
{$I 'xtreme3d/shaders'}
{$I 'xtreme3d/thorfx'}
{$I 'xtreme3d/firefx'}
{$I 'xtreme3d/lensflare'}
{$I 'xtreme3d/terrain'}
{$I 'xtreme3d/blur'}
{$I 'xtreme3d/skybox'}
{$I 'xtreme3d/trail'}
{$I 'xtreme3d/shadowplane'}
{$I 'xtreme3d/shadowvolume'}
{$I 'xtreme3d/skydome'}
{$I 'xtreme3d/water'}
{$I 'xtreme3d/lines'}
{$I 'xtreme3d/tree'}
{$I 'xtreme3d/navigator'}
{$I 'xtreme3d/movement'}
{$I 'xtreme3d/dce'}
{$I 'xtreme3d/fps'}
{$I 'xtreme3d/mirror'}
{$I 'xtreme3d/partition'}
{$I 'xtreme3d/memviewer'}
//{$I 'xtreme3d/fbo'}
{$I 'xtreme3d/proxy'}
{$I 'xtreme3d/objecthash'}
{$I 'xtreme3d/grid'}
//{$I 'xtreme3d/shadowmap'}
{$I 'xtreme3d/ode'}
//{$I 'xtreme3d/kraft'}
//{$I 'xtreme3d/clipplane'}
{$I 'xtreme3d/input'}
{$I 'xtreme3d/window'}
{$I 'xtreme3d/color'}
{$I 'xtreme3d/pipe'}
{$I 'xtreme3d/verlet'}
{$I 'xtreme3d/picklist'}

exports
    // Engine
    EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
    EngineUpdate, EngineSaveScene, EngineLoadScene, EngineRootObject,
    EngineShowLoadingErrors, EngineGetTimeStep,
    PointerToReal,

    //Pak
    SetPakArchive, PakGetFileCount, PakGetFileName,
    PakExtract, PakExtractFile,

    // Viewer
    ViewerCreate, ViewerSetCamera, ViewerEnableVSync, ViewerRender, ViewerRenderToFile,
    ViewerResize, ViewerSetVisible, ViewerGetPixelColor, ViewerGetPixelDepth,
    ViewerSetLighting, ViewerSetBackgroundColor, ViewerSetAmbientColor,
    ViewerEnableFog, ViewerSetFogColor, ViewerSetFogDistance,
    ViewerScreenToWorld, ViewerWorldToScreen, ViewerCopyToTexture,
    ViewerGetPickedObject, ViewerGetPickedObjectsList, ViewerScreenToVector,
    ViewerVectorToScreen, ViewerPixelToDistance, ViewerSetAntiAliasing,
    ViewerGetSize, ViewerGetPosition, ViewerIsOpenGLExtensionSupported,
    ViewerGetFramesPerSecond, ViewerResetPerformanceMonitor,
    ViewerPixelRayToWorld, ViewerShadeModel,

    // Dummycube
    DummycubeCreate, DummycubeAmalgamate, DummycubeSetCameraMode,
    DummycubeSetVisible, DummycubeSetEdgeColor, DummycubeSetCubeSize,

    // Camera
    CameraCreate, CameraSetStyle, CameraSetFocal, CameraSetSceneScale,
    CameraScaleScene, CameraSetViewDepth, CameraSetTargetObject,
    CameraMoveAroundTarget, CameraSetDistanceToTarget, CameraGetDistanceToTarget,
    CameraCopyToTexture, CameraGetNearPlane, CameraSetNearPlaneBias,
    CameraAbsoluteVectorToTarget, CameraAbsoluteRightVectorToTarget, CameraAbsoluteUpVectorToTarget,
    CameraZoomAll, CameraScreenDeltaToVector, CameraScreenDeltaToVectorXY, CameraScreenDeltaToVectorXZ,
    CameraScreenDeltaToVectorYZ, CameraAbsoluteEyeSpaceVector, CameraSetAutoLeveling,
    CameraMoveInEyeSpace, CameraMoveTargetInEyeSpace, CameraPointInFront, CameraGetFieldOfView,

    // Light
    LightCreate, LightSetAmbientColor, LightSetDiffuseColor, LightSetSpecularColor,
    LightSetAttenuation, LightSetShining, LightSetSpotCutoff, LightSetSpotExponent,
    LightSetSpotDirection, LightSetStyle,
    LightGetColor, LightGetAttenuation, LightGetShining,

    // Font & Text
    BmpfontCreate, BmpfontLoad, WindowsBitmapfontCreate, HUDTextCreate,
    HUDTextSetRotation, HUDTextSetFont, HUDTextSetColor, HUDTextSetText,
    FlatTextCreate, FlatTextSetFont, FlatTextSetColor, FlatTextSetText,
    SpaceTextCreate, SpaceTextSetExtrusion, SpaceTextSetFont, SpaceTextSetText,

    // Sprite
    HUDSpriteCreate, HUDSpriteGetMouseOver, HUDSpriteXTiles, HUDSpriteYTiles,
    SpriteCreate, SpriteSetSize, SpriteGetSize, SpriteScale, SpriteSetRotation,
    SpriteRotate, SpriteMirror,

    // Primitives
    CubeCreate, CubeSetNormalDirection, PlaneCreate, SphereCreate, SphereSetAngleLimits,
    CylinderCreate, ConeCreate, AnnulusCreate, TorusCreate, DiskCreate, FrustrumCreate,
    DodecahedronCreate, IcosahedronCreate, TeapotCreate,
    TilePlaneCreate, TilePlaneSetTile,
    CubeGetNormalDirection, PlaneSetOptions, PlaneGetOptions, SphereGetOptions,
    SphereGetAngleLimits, SphereSetOptions, CylinderSetOptions, CylinderGetOptions,
    ConeGetOptions, ConeSetOptions, AnnulusSetOptions, AnnulusGetOptions,
    TorusSetOptions, TorusGetOptions, DiskSetOptions, DiskGetOptions,
    FrustrumSetOptions, FrustrumGetOptions,

    // Actor
    ActorCreate, ActorCopy, ActorSetAnimationRange, ActorGetCurrentFrame, ActorSwitchToAnimation,
    ActorSwitchToAnimationName, ActorSynchronize, ActorSetInterval, ActorSetAnimationMode,
    ActorSetFrameInterpolation, ActorAddObject, ActorGetCurrentAnimation, ActorGetFrameCount,
    ActorGetBoneCount, ActorGetBoneByName, ActorGetBoneRotation, ActorGetBonePosition,
    ActorBoneExportMatrix, ActorMakeSkeletalTranslationStatic, ActorMakeSkeletalRotationDelta,
    ActorShowSkeleton,
    AnimationBlenderCreate, AnimationBlenderSetActor, AnimationBlenderSetAnimation,
    AnimationBlenderSetRatio,
    ActorLoadQ3TagList, ActorLoadQ3Animations, ActorQ3TagExportMatrix,
    ActorMeshObjectsCount, ActorFaceGroupsCount, ActorFaceGroupGetMaterialName,
    ActorFaceGroupSetMaterial, ActorMoveBone, ActorRotateBone, ActorMeshSetVisible,
    ActorGetAnimationName, ActorGetAnimationCount, ActorAnimationDestroy,
    ActorAnimationNextFrame, ActorAnimationPrevFrame, ActorSetFrame,
    ActorTriangleCount,

    //Freeform
    FreeformCreate,
    FreeformMeshObjectsCount, FreeformMeshSetVisible,
    FreeformMeshSetSecondCoords,
    FreeformMeshFaceGroupsCount,
    FreeformMeshSetMaterial, FreeformUseMeshMaterials,
    FreeformSphereSweepIntersect, FreeformPointInMesh,
    FreeformToFreeforms,
    FreeformMeshTranslate, FreeformMeshRotate, FreeformMeshScale,
    FreeformCreateExplosionFX, FreeformExplosionFXReset,
    FreeformExplosionFXEnable, FreeformExplosionFXSetSpeed,
    FreeformCreateEmpty,
    FreeformAddMesh, FreeformMeshAddFaceGroup,
    FreeformMeshAddVertex, FreeformMeshAddNormal,
    FreeformMeshAddTexCoord, FreeformMeshAddSecondTexCoord,
    FreeformMeshAddTangent, FreeformMeshAddBinormal,
    FreeformMeshGetVertex, FreeformMeshGetNormal,
    FreeformMeshGetTexCoord, FreeformMeshGetSecondTexCoord,
    FreeformMeshGetTangent, FreeformMeshGetBinormal,
    FreeformMeshFaceGroupGetIndex,
    FreeformMeshSetVertex, FreeformMeshSetNormal,
    FreeformMeshSetTexCoord, FreeformMeshSetSecondTexCoord,
    FreeformMeshSetTangent, FreeformMeshSetBinormal,
    FreeformMeshFaceGroupSetIndex,
    FreeformMeshFaceGroupAddTriangle,
    FreeformMeshFaceGroupGetMaterial, FreeformMeshFaceGroupSetMaterial,
    FreeformMeshGenNormals, FreeformMeshGenTangents,
    FreeformMeshVerticesCount,
    FreeformMeshTriangleCount,
    FreeformMeshFaceGroupTriangleCount,
    FreeformSave,
    FreeformGenTangents, FreeformBuildOctree,
    FreeformSetMaterialLibraries,
    FreeformMeshFaceGroupSetLightmapIndex,
    FreeformMeshFaceGroupGetLightmapIndex,
    FreeformMeshObjectGetName, FreeformMeshObjectSetName, FreeformMeshObjectDestroy,

    // Terrain
    BmpHDSCreate, BmpHDSSetInfiniteWarp, BmpHDSInvert,
    BmpHDSCreateEmpty, BmpHDSSetHeight, BmpHDSGetHeight, BmpHDSSave,
    TerrainCreate, TerrainSetHeightData, TerrainSetTileSize, TerrainSetTilesPerTexture,
    TerrainSetQualityDistance, TerrainSetQualityStyle, TerrainSetMaxCLodTriangles,
    TerrainSetCLodPrecision, TerrainSetOcclusionFrameSkip, TerrainSetOcclusionTesselate,
    TerrainGetHeightAtObjectPosition, TerrainGetLastTriCount,
    TerrainGetHDSPosition,

    // Object
    ObjectHide, ObjectShow, ObjectIsVisible,
    ObjectCopy, ObjectDestroy, ObjectDestroyChildren,
    ObjectSetPosition, ObjectGetPosition, ObjectGetAbsolutePosition,
    ObjectSetPositionOfObject, ObjectAlignWithObject,
    ObjectSetPositionX, ObjectSetPositionY, ObjectSetPositionZ,
    ObjectGetPositionX, ObjectGetPositionY, ObjectGetPositionZ,
    ObjectSetAbsolutePosition,
    ObjectSetDirection, ObjectGetDirection,
    ObjectSetAbsoluteDirection, ObjectGetAbsoluteDirection,
    ObjectGetPitch, ObjectGetTurn, ObjectGetRoll, ObjectSetRotation,
    ObjectMove, ObjectLift, ObjectStrafe, ObjectTranslate, ObjectRotate,
    ObjectScale, ObjectSetScale, ObjectGetScale,
    ObjectSetUpVector, ObjectPointToObject,
    ObjectShowAxes,
    ObjectGetGroundHeight, ObjectSceneRaycast, ObjectRaycast,
    ObjectGetCollisionPosition, ObjectGetCollisionNormal,
    ObjectSetMaterial, ObjectGetMaterial,
    ObjectGetGroundHeight, ObjectSceneRaycast, ObjectRaycast,
    ObjectGetDistance,
    ObjectCheckCubeVsCube, ObjectCheckSphereVsSphere, ObjectCheckSphereVsCube,
    ObjectCheckCubeVsFace, ObjectCheckFaceVsFace,
    ObjectIsPointInObject,
    ObjectSetCulling,
    ObjectSetName, ObjectGetName, ObjectGetClassName,
    ObjectSetTag, ObjectGetTag,
    ObjectGetParent, ObjectGetChildCount, ObjectGetChild, ObjectGetIndex, ObjectFindChild,
    ObjectGetBoundingSphereRadius,
    ObjectGetAbsoluteUp, ObjectSetAbsoluteUp, ObjectGetAbsoluteRight,
    ObjectGetAbsoluteXVector, ObjectGetAbsoluteYVector, ObjectGetAbsoluteZVector,
    ObjectGetRight,
    ObjectMoveChildUp, ObjectMoveChildDown,
    ObjectSetParent, ObjectRemoveChild,
    ObjectMoveObjectAround,
    ObjectPitch, ObjectTurn, ObjectRoll,
    ObjectGetUp,
    ObjectRotateAbsolute, ObjectRotateAbsoluteVector,
    ObjectSetMatrixColumn, ObjectExportMatrix, ObjectExportAbsoluteMatrix,
    ObjectIgnoreDepthBuffer, ObjectInFrustum, ObjectFindByName, ObjectIsPicked,

    // Material
    MaterialLibraryCreate, MaterialLibraryActivate, MaterialLibrarySetTexturePaths,
    MaterialLibraryClear, MaterialLibraryDeleteUnused,
    MaterialLibraryHasMaterial, MaterialLibraryLoadScript,
    MaterialCreate, MaterialDestroy,
    MaterialAddCubeMap, MaterialCubeMapLoadImage, MaterialCubeMapGenerate, MaterialCubeMapFromScene,
    MaterialSetName, MaterialSetShininess,
    MaterialSetDiffuseColor, MaterialSetAmbientColor, MaterialSetSpecularColor, MaterialSetEmissionColor,
    MaterialGetColor, MaterialGetAlpha, MaterialSetBlendingMode, MaterialSetTextureMode,
    MaterialSetTextureMappingMode, MaterialSetPolygonMode, MaterialSetTextureImageAlpha,
    MaterialSetTextureScale, MaterialSetTextureOffset, MaterialSetTextureFilter,
    MaterialEnableTexture, MaterialGetCount, MaterialGetName,
    MaterialSetFaceCulling, MaterialSetSecondTexture, MaterialSetTextureFormat,
    MaterialSetTextureCompression, MaterialTextureRequiredMemory,
    MaterialSetFilteringQuality,
    MaterialAddTextureEx, MaterialTextureExClear, MaterialTextureExDelete,
    MaterialSetShader, MaterialSaveTexture, MaterialSetOptions,
    MaterialSetTextureWrap, MaterialGenTexture, MaterialSetTexture,
    MaterialGetTextureWidth, MaterialGetTextureHeight, MaterialLoadTexture,
    MaterialNoiseCreate, MaterialNoiseSetDimensions, MaterialNoiseAnimate,
    MaterialNoiseSetMinCut, MaterialNoiseSetSharpness, MaterialNoiseSetSeamless,
    MaterialNoiseRandomSeed, MaterialSetDepthWrite, MaterialSetDepthTest,
    MaterialGetNameFromLibrary,

    // Shaders
    ShaderEnable,
    CelShaderCreate, CelShaderSetLineColor, CelShaderSetLineWidth, CelShaderSetOptions,
    MultiMaterialShaderCreate,
    HiddenLineShaderCreate, HiddenLineShaderSetLineSmooth, HiddenLineShaderSetSolid,
    HiddenLineShaderSetSurfaceLit, HiddenLineShaderSetFrontLine, HiddenLineShaderSetBackLine,
    OutlineShaderCreate, OutlineShaderSetLineColor, OutlineShaderSetLineWidth,
    TexCombineShaderCreate, TexCombineShaderAddCombiner, TexCombineShaderMaterial3,
    TexCombineShaderMaterial4,
    GLSLShaderCreate, GLSLShaderCreateParameter,
    GLSLShaderSetParameter1i, GLSLShaderSetParameter1f, GLSLShaderSetParameter2f,
    GLSLShaderSetParameter3f, GLSLShaderSetParameter4f,
    GLSLShaderSetParameterTexture, GLSLShaderSetParameterSecondTexture,
    GLSLShaderSetParameterMatrix, GLSLShaderSetParameterInvMatrix,
    //GLSLShaderSetParameterShadowTexture, GLSLShaderSetParameterShadowMatrix,
    GLSLShaderSetParameterViewMatrix, GLSLShaderSetParameterInvViewMatrix,
    GLSLShaderSetParameterHasTextureEx,
    //GLSLShaderSetParameterFBOColorTexture, GLSLShaderSetParameterFBODepthTexture,
    //GLSLShaderSetParameterViewMatrix,

    // ThorFX
    ThorFXManagerCreate, ThorFXSetColor, ThorFXEnableCore, ThorFXEnableGlow,
    ThorFXSetMaxParticles, ThorFXSetGlowSize, ThorFXSetVibrate, ThorFXSetWildness,
    ThorFXSetTarget, ThorFXCreate,

    // FireFX
    FireFXManagerCreate, FireFXCreate,
    FireFXSetColor, FireFXSetMaxParticles, FireFXSetParticleSize,
    FireFXSetDensity, FireFXSetEvaporation, FireFXSetCrown,
    FireFXSetLife, FireFXSetBurst, FireFXSetRadius, FireFXExplosion,
    FireFXRingExplosion,

    // Lensflare
    LensflareCreate, LensflareSetSize, LensflareSetSeed, LensflareSetSqueeze,
    LensflareSetStreaks, LensflareSetStreakWidth, LensflareSetSecs,
    LensflareSetResolution, LensflareSetElements, LensflareSetGradients,

    // Skydome
    SkydomeCreate, SkydomeSetOptions, SkydomeSetDeepColor, SkydomeSetHazeColor,
    SkydomeSetNightColor, SkydomeSetSkyColor, SkydomeSetSunDawnColor, SkydomeSetSunZenithColor,
    SkydomeSetSunElevation, SkydomeSetTurbidity,
    SkydomeAddRandomStars, SkydomeAddStar, SkydomeClearStars, SkydomeTwinkleStars,

    // Water
    WaterCreate, WaterCreateRandomRipple,
    WaterCreateRippleAtGridPosition, WaterCreateRippleAtWorldPosition,
    WaterCreateRippleAtObjectPosition,
    WaterSetMask, WaterSetActive, WaterReset,
    WaterSetRainTimeInterval, WaterSetRainForce,
    WaterSetViscosity, WaterSetElastic, WaterSetResolution,
    //WaterSetLinearWaveHeight, WaterSetLinearWaveFrequency,

    // Blur
    BlurCreate, BlurSetPreset, BlurSetOptions, BlurSetResolution,
    BlurSetColor, BlurSetBlendingMode,

    // Skybox
    SkyboxCreate, SkyboxSetMaterial, SkyboxSetClouds, SkyboxSetStyle,

    // Lines
    LinesCreate, LinesAddNode, LinesDeleteNode, LinesSetColors, LinesSetSize,
    LinesSetSplineMode, LinesSetNodesAspect, LinesSetDivision, LinesSetNode,

    // Tree
    TreeCreate, TreeSetMaterials, TreeSetBranchFacets, TreeBuildMesh,
    TreeSetBranchNoise, TreeSetBranchAngle, TreeSetBranchSize, TreeSetBranchRadius,
    TreeSetBranchTwist, TreeSetDepth, TreeSetLeafSize, TreeSetLeafThreshold, TreeSetSeed,

    // Trail
    TrailCreate, TrailSetObject, TrailSetAlpha, TrailSetLimits, TrailSetMinDistance,
    TrailSetUVScale, TrailSetMarkStyle, TrailSetMarkWidth, TrailSetEnabled, TrailClearMarks,

    // Shadowplane
    ShadowplaneCreate, ShadowplaneSetLight, ShadowplaneSetObject, ShadowplaneSetOptions,

    // Shadowvolume
    ShadowvolumeCreate, ShadowvolumeSetActive,
    ShadowvolumeAddLight, ShadowvolumeRemoveLight,
    ShadowvolumeAddOccluder, ShadowvolumeRemoveOccluder,
    ShadowvolumeSetDarkeningColor, ShadowvolumeSetMode, ShadowvolumeSetOptions,

    // Navigator
    NavigatorCreate, NavigatorSetObject, NavigatorSetUseVirtualUp, NavigatorSetVirtualUp,
    NavigatorTurnHorizontal, NavigatorTurnVertical, NavigatorMoveForward,
    NavigatorStrafeHorizontal, NavigatorStrafeVertical, NavigatorStraighten,
    NavigatorFlyForward, NavigatorMoveUpWhenMovingForward, NavigatorInvertHorizontalWhenUpsideDown,
    NavigatorSetAngleLock, NavigatorSetAngles,

    // DCE
    DceManagerCreate, DceManagerStep, DceManagerSetGravity, DceManagerSetWorldDirection,
    DceManagerSetWorldScale, DceManagerSetMovementScale,
    DceManagerSetLayers, DceManagerSetManualStep,
    DceDynamicSetManager, DceDynamicSetActive, DceDynamicIsActive,
    DceDynamicSetUseGravity, DceDynamicSetLayer, DceDynamicGetLayer,
    DceDynamicSetSolid, DceDynamicSetFriction, DceDynamicSetBounce,
    DceDynamicSetSize, DceDynamicSetSlideOrBounce,
    DceDynamicApplyAcceleration, DceDynamicApplyAbsAcceleration,
    DceDynamicStopAcceleration, DceDynamicStopAbsAcceleration,
    DceDynamicJump, DceDynamicMove, DceDynamicMoveTo,
    DceDynamicSetVelocity, DceDynamicGetVelocity,
    //DceDynamicSetAbsVelocity, DceDynamicGetAbsVelocity,
    DceDynamicApplyImpulse,
    //DceDynamicApplyAbsImpulse,
    DceDynamicInGround, DceDynamicSetMaxRecursionDepth,
    DceStaticSetManager, DceStaticSetActive, DceStaticSetShape, DceStaticSetLayer,
    DceStaticSetSize, DceStaticSetSolid, DceStaticSetFriction, DceStaticSetBounceFactor,

    // ODE
    OdeManagerCreate, OdeManagerDestroy, OdeManagerStep, OdeManagerGetNumContactJoints,
    OdeManagerSetGravity, OdeManagerSetSolver, OdeManagerSetIterations,
    OdeManagerSetMaxContacts, OdeManagerSetVisible,
    //OdeManagerSetGeomColor,
    OdeWorldSetAutoDisableFlag, OdeWorldSetAutoDisableLinearThreshold,
    OdeWorldSetAutoDisableAngularThreshold, OdeWorldSetAutoDisableSteps, OdeWorldSetAutoDisableTime,
    OdeStaticCreate, OdeDynamicCreate, OdeTerrainCreate,
    OdeDynamicCalculateMass, OdeDynamicCalibrateCenterOfMass,
    OdeDynamicAlignObject, OdeDynamicEnable, OdeDynamicSetAutoDisableFlag,
    OdeDynamicSetAutoDisableLinearThreshold, OdeDynamicSetAutoDisableAngularThreshold,
    OdeDynamicSetAutoDisableSteps, OdeDynamicSetAutoDisableTime,
    OdeDynamicAddForce, OdeDynamicAddForceAtPos, OdeDynamicAddForceAtRelPos,
    OdeDynamicAddRelForce, OdeDynamicAddRelForceAtPos, OdeDynamicAddRelForceAtRelPos,
    OdeDynamicAddTorque, OdeDynamicAddRelTorque,
    //OdeDynamicGetContactCount, OdeStaticGetContactCount,
    OdeAddBox, OdeAddSphere, OdeAddPlane, OdeAddCylinder,
    //OdeAddCone,
    OdeAddCapsule, OdeAddTriMesh,
    OdeElementSetDensity,
    OdeSurfaceEnableRollingFrictionCoeff, OdeSurfaceSetRollingFrictionCoeff,
    OdeSurfaceSetMode, OdeSurfaceSetMu, OdeSurfaceSetMu2,
    OdeSurfaceSetBounce, OdeSurfaceSetBounceVel, OdeSurfaceSetSoftERP, OdeSurfaceSetSoftCFM,
    OdeSurfaceSetMotion1, OdeSurfaceSetMotion2, OdeSurfaceSetSlip1, OdeSurfaceSetSlip2,
    OdeAddJointBall, OdeAddJointFixed, OdeAddJointHinge, OdeAddJointHinge2,
    OdeAddJointSlider, OdeAddJointUniversal,
    OdeJointSetObjects, OdeJointEnable, OdeJointInitialize,
    OdeJointSetAnchor, OdeJointSetAnchorAtObject, OdeJointSetAxis1, OdeJointSetAxis2,
    OdeJointSetBounce, OdeJointSetCFM, OdeJointSetFMax, OdeJointSetFudgeFactor,
    OdeJointSetHiStop, OdeJointSetLoStop, OdeJointSetStopCFM, OdeJointSetStopERP, OdeJointSetVel,
    //OdeRagdollCreate, OdeRagdollHingeJointCreate, OdeRagdollUniversalJointCreate,
    //OdeRagdollDummyJointCreate, OdeRagdollBoneCreate,
    //OdeRagdollBuild, OdeRagdollEnable, OdeRagdollUpdate,
    //OdeDynamicSetVelocity, OdeDynamicSetAngularVelocity,
    //OdeDynamicGetVelocity, OdeDynamicGetAngularVelocity,
    //OdeDynamicSetPosition, OdeDynamicSetRotationQuaternion,

    // Fps
    FpsManagerCreate, FpsManagerSetNavigator, FpsManagerSetMovementScale,
    FpsManagerAddMap, FpsManagerRemoveMap, FpsManagerMapSetCollisionGroup,
    FpsSetManager, FpsSetCollisionGroup, FpsSetSphereRadius, FpsSetGravity,
    FpsMove, FpsStrafe, FpsLift, FpsGetVelocity,

    // Mirror
    MirrorCreate, MirrorSetObject, MirrorSetOptions,
    MirrorSetShape, MirrorSetDiskOptions,

    // Partition
    OctreeCreate, QuadtreeCreate, PartitionDestroy, PartitionAddLeaf,
    PartitionLeafChanged, PartitionQueryFrustum, PartitionQueryLeaf,
    PartitionQueryAABB, PartitionQueryBSphere, PartitionGetNodeTests,
    PartitionGetNodeCount, PartitionGetResult, PartitionGetResultCount,
    PartitionResultShow, PartitionResultHide,

    // Proxy & MultiProxy
    ProxyObjectCreate, ProxyObjectSetOptions, ProxyObjectSetTarget,
    MultiProxyObjectCreate, MultiProxyObjectAddTarget,
    ActorProxyObjectCreate,
    //ActorProxyObjectSwitchToAnimation,
    //ActorProxyObjectSetAnimationRange, ActorProxyObjectSetInterval,

    // Grid
    GridCreate, GridSetLineStyle, GridSetLineSmoothing, GridSetParts,
    GridSetColor, GridSetSize, GridSetPattern,
    GridSetTile, GridSetStep,

    // MemoryViewer
    MemoryViewerCreate, MemoryViewerSetCamera, MemoryViewerRender,
    MemoryViewerSetViewport, MemoryViewerCopyToTexture,

    // Movement
    MovementCreate, MovementStart, MovementStop, MovementAutoStartNextPath,
    MovementAddPath, MovementSetActivePath, MovementPathSetSplineMode,
    MovementPathAddNode,
    MovementPathNodeSetPosition, MovementPathNodeSetRotation,
    MovementPathNodeSetSpeed,
    MovementPathShow, MovementPathSetLoop, MovementPathDeleteNode,

    // ObjectHash
    ObjectHashCreate, ObjectHashSetItem, ObjectHashGetItem,
    ObjectHashDeleteItem, ObjectHashGetItemCount,
    ObjectHashClear, ObjectHashDestroy,

    // Input
    MouseGetPositionX, MouseGetPositionY, MouseSetPosition,
    MouseShowCursor, KeyIsPressed, MouseIsPressed,

    // Window
    WindowCreate, WindowGetHandle, WindowSetTitle, WindowDestroy,
    WindowCenter, WindowResize, WindowGetPosition, WindowGetSize,
    WindowSetIcon, WindowIsShowing,
    WindowDispatch,

    // Color
    MakeColorRGB, MakeColorRGBFloat,

    // Pipe
    PipeCreate, PipeAddNode, PipeSetDivision, PipeSetSplineMode, PipeDeleteNode,
    PipeSetRadius, PipeSetNode, PipeSetSlices,

    // Verlet
    VerletWorldCreate, VerletWorldCreateOctree,
    VerletWorldGravityCreate, VerletWorldGravitySetDirection,
    VerletWorldUpdate, EdgeDetectorCreate, EdgeDetectorSetWeldDistance,
    VerletConstraintFloorCreate, VerletConstraintFloorSetNormal,
    VerletConstraintFloorSetObjectLocations,
    VerletConstraintSetPosition, VerletConstraintSetFrictionRatio,
    VerletConstraintSetEnabled, VerletConstraintSphereCreate,
    VerletConstraintCylinderCreate, VerletConstraintCubeCreate,
    VerletConstraintCubeCreateSetCube, VerletConstraintCubeSetDirection,
    VerletConstraintCapsuleCreate, VerletConstraintCylinderSetAxis,
    VerletConstraintCapsuleSetAxis,
    VerletGetNodeCount, VerletNodeNailedDown,VerletNodeSetPosition, VerletNodeSetRadius,
    VerletNodeSetFriction, VerletNodeSetWeight, VerletNodeApplyFriction,
    VerletAirResistanceCreate, VerletAirResistanceSetWindDirection,
    VerletAirResistanceSetWindMagnitude, VerletAirResistanceSetWindChaos,
    VerletConstraintGetCount, VerletConstraintSetSlack,
    VerletWorldSetSimTime, VerletWorldSetMaxDeltaTime,

    // PickList
    PickListCreate, PickListClear, PickListGetCount, PickListGetHit;
begin
end.
