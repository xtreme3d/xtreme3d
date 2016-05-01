library xtreme3d;
uses
  Windows, Messages, Classes, Controls, StdCtrls, ExtCtrls, Dialogs, SysUtils,
  GLScene, GLObjects, GLWin32FullScreenViewer, GLMisc, GLGraph,
  GLCollision, GLTexture, OpenGL1x, VectorGeometry, Graphics,
  GLVectorFileObjects, GLWin32Viewer, GLSpaceText, GLGeomObjects, GLCadencer,
  JPeg, Tga, GLProcTextures, Spin, GLVfsPAK, GLCanvas, GLGraphics, GLPortal,
  GLHUDObjects, GLBitmapFont, GLWindowsFont, GLImposter, VectorTypes, GLUtils,
  GLPolyhedron, GLTeapot, GLzBuffer, GLFile3DS, GLFileGTS, GLFileLWO, GLFileMD2,
  GLFileMD3, Q3MD3, GLFileMS3D, GLFileMD5, GLFileNMF, GLFileNurbs, GLFileObj, GLFileOCT,
  GLFilePLY, GLFileQ3BSP, GLFileSMD, GLFileSTL, GLFileTIN, GLFileB3D,
  GLFileLOD, GLPhongShader, VectorLists, GLThorFX, GLFireFX, GLTexCombineShader,
  GLBumpShader, GLCelShader, GLContext, GLTerrainRenderer, GLHeightData,
  GLBlur, GLSLShader, GLMultiMaterialShader, GLOutlineShader, GLHiddenLineShader,
  ApplicationFileIO, GLMaterialScript, GLWaterPlane, GeometryBB, GLExplosionFx,
  GLSkyBox, GLShadowPlane, GLShadowVolume, GLSkydome, GLLensFlare, GLDCE,
  GLNavigator, GLFPSMovement, GLMirror, SpatialPartitioning, GLSpatialPartitioning;

type
   TEmpty = class(TComponent)
    private
   end;

var
  scene: TGLScene;
  matlib: TGLMaterialLibrary;
  memviewer: TGLMemoryViewer;
  cadencer: TGLCadencer;
  empty: TEmpty;

  collisionPoint: TVector;
  collisionNormal: TVector;

{$R *.res}

function LoadStringFromFile2(const fileName : String) : String;
var
   n : Cardinal;
	fs : TStream;
begin
   if FileStreamExists(fileName) then begin
   	fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
      try
         n:=fs.Size;
   	   SetLength(Result, n);
         if n>0 then
         	fs.Read(Result[1], n);
      finally
   	   fs.Free;
      end;
   end else Result:='';
end;

{$I 'engine'}
{$I 'viewer'}
{$I 'dummycube'}
{$I 'camera'}
{$I 'light'}
{$I 'fonttext'}
{$I 'sprite'}
{$I 'primitives'}
{$I 'memviewer'}
{$I 'zshadows'}
{$I 'actor'}
{$I 'freeform'}
{$I 'object'}
{$I 'polygon'}
{$I 'material'}
{$I 'shaders'}
{$I 'thorfx'}
{$I 'firefx'}
{$I 'lensflare'}
{$I 'terrain'}
{$I 'blur'}
{$I 'skybox'}
{$I 'shadowplane'}
{$I 'shadowvolume'}
{$I 'skydome'}
{$I 'water'}
{$I 'navigator'}
{$I 'dce'}
{$I 'fps'}
{$I 'mirror'}
{$I 'text'}

function OctreeCreate(maxdepth, leafthreshold, growgravy, culling: real): real; stdcall;
var
  octree: TOctreeSpacePartition;
begin
  octree := TOctreeSpacePartition.Create();
  octree.MaxTreeDepth := trunc64(maxdepth);
  octree.LeafThreshold := trunc64(leafthreshold);
  octree.GrowGravy := growgravy;
  if culling = 0 then octree.CullingMode := cmFineCulling;
  if culling = 1 then octree.CullingMode := cmGrossCulling;
  result := Integer(octree);
end;

function QuadtreeCreate(maxdepth, leafthreshold, growgravy, culling: real): real; stdcall;
var
  quadtree: TQuadtreeSpacePartition;
begin
  quadtree := TQuadtreeSpacePartition.Create();
  quadtree.MaxTreeDepth := trunc64(maxdepth);
  quadtree.LeafThreshold := trunc64(leafthreshold);
  quadtree.GrowGravy := growgravy;
  if culling = 0 then quadtree.CullingMode := cmFineCulling;
  if culling = 1 then quadtree.CullingMode := cmGrossCulling;
  result := Integer(quadtree);
end;

function PartitionDestroy(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  t.Free;
  result := 1.0;
end;

function PartitionAddLeaf(tree, obj: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  tobj: TSceneObj;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  tobj := TSceneObj.CreateObj(t, TGLBaseSceneObject(trunc64(obj)));
  result := Integer(tobj);
end;

function PartitionLeafChanged(leaf: real): real; stdcall;
var
  pleaf: TSpacePartitionLeaf;
begin
  pleaf := TSpacePartitionLeaf(trunc64(leaf));
  pleaf.Changed;
  result := 1.0;
end;

function PartitionQueryFrustum(tree, viewer: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  v: TGLSceneViewer;
  mvp: TMatrix;
  frustum : TFrustum;
begin
  v := TGLSceneViewer(trunc64(viewer));
  t := TSectoredSpacePartition(trunc64(tree));
  mvp := MatrixMultiply(v.Buffer.ModelViewMatrix, v.Buffer.ProjectionMatrix);
  frustum := ExtractFrustumFromModelViewProjection(mvp);
  result := t.QueryFrustum(frustum);
end;

function PartitionQueryLeaf(tree, leaf: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  pleaf: TSpacePartitionLeaf;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  pleaf := TSpacePartitionLeaf(trunc64(leaf));
  result := t.QueryLeaf(pleaf);
end;

function PartitionQueryAABB(tree, obj: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  o: TGLBaseSceneObject;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  o := TGLBaseSceneObject(trunc64(obj));
  result := t.QueryAABB(o.AxisAlignedBoundingBox);
end;

function PartitionQueryBSphere(tree, obj: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  o: TGLBaseSceneObject;
  bs: TBSphere;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  o := TGLBaseSceneObject(trunc64(obj));
  bs.Center := AffineVectorMake(o.AbsolutePosition);
  bs.Radius := o.BoundingSphereRadius;
  result := t.QueryBSphere(bs);
end;

function PartitionGetNodeTests(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  result := t.QueryNodeTests;
end;

function PartitionGetNodeCount(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  result := t.GetNodeCount;
end;

function PartitionGetResult(tree, ind: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  sobj: TSceneObj;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  sobj := t.QueryResult[trunc64(ind)] as TSceneObj;
  result := Integer(sobj.Obj);
end;

function PartitionGetResultCount(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  result := t.QueryResult.Count;
end;

function PartitionResultShow(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  sobj: TSceneObj;
  i: Integer;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  for i := 0 to t.QueryResult.Count do
  begin
    sobj := t.QueryResult[i] as TSceneObj;
    sobj.Obj.Visible := true;
  end;
end;

function PartitionResultHide(tree: real): real; stdcall;
var
  t: TSectoredSpacePartition;
  sobj: TSceneObj;
  i: Integer;
begin
  t := TSectoredSpacePartition(trunc64(tree));
  for i := 0 to t.QueryResult.Count do
  begin
    sobj := t.QueryResult[i] as TSceneObj;
    sobj.Obj.Visible := false;
  end;
end;

exports

//Engine
EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
SetPakArchive,
Update, TrisRendered,
//Viewer
ViewerCreate, ViewerSetCamera, ViewerEnableVSync, ViewerRenderToFile,
ViewerRender,
ViewerResize, ViewerSetVisible, ViewerGetPixelColor, ViewerGetPixelDepth,
ViewerSetLighting, ViewerSetBackgroundColor, ViewerSetAmbientColor, ViewerEnableFog,
ViewerSetFogColor, ViewerSetFogDistance, ViewerScreenToWorld, ViewerWorldToScreen,
ViewerCopyToTexture, ViewerGetFramesPerSecond, ViewerGetPickedObject,
ViewerScreenToVector, ViewerVectorToScreen, ViewerPixelToDistance, ViewerGetPickedObjectsList,
ViewerSetAntiAliasing,
ViewerGetVBOSupported, ViewerGetGLSLSupported,
//Dummycube
DummycubeCreate, DummycubeAmalgamate, DummycubeSetCameraMode, DummycubeSetVisible,
DummycubeSetEdgeColor, DummycubeSetCubeSize,
//Camera
CameraCreate, CameraSetStyle, CameraSetFocal, CameraSetSceneScale,
CameraScaleScene, CameraSetViewDepth, CameraSetTargetObject,
CameraMoveAroundTarget, CameraSetDistanceToTarget, CameraGetDistanceToTarget,
CameraCopyToTexture, CameraGetNearPlane, CameraSetNearPlaneBias,
CameraAbsoluteVectorToTarget, CameraAbsoluteRightVectorToTarget, CameraAbsoluteUpVectorToTarget,
CameraZoomAll, CameraScreenDeltaToVector, CameraScreenDeltaToVectorXY, CameraScreenDeltaToVectorXZ,
CameraScreenDeltaToVectorYZ, CameraAbsoluteEyeSpaceVector, CameraSetAutoLeveling,
CameraMoveInEyeSpace, CameraMoveTargetInEyeSpace, CameraPointInFront, CameraGetFieldOfView,
//Light
LightCreate, LightSetAmbientColor, LightSetDiffuseColor, LightSetSpecularColor,
LightSetAttenuation, LightSetShining, LightSetSpotCutoff, LightSetSpotExponent,
LightSetSpotDirection, LightSetStyle,
//Font & Text
BmpFontCreate, BmpFontLoad, WindowsBitmapfontCreate, HUDTextCreate, FlatTextCreate,
HUDTextSetRotation, SpaceTextCreate, SpaceTextSetExtrusion, HUDTextSetFont,
FlatTextSetFont, SpaceTextSetFont, HUDTextSetColor, FlatTextSetColor, HUDTextSetText,
FlatTextSetText, SpaceTextSetText,
//Sprite
HUDSpriteCreate, SpriteCreate, SpriteSetSize, SpriteScale, SpriteSetRotation,
SpriteRotate, SpriteMirror, SpriteNoZWrite,    
//Primitives
CubeCreate, CubeSetNormalDirection, PlaneCreate, SphereCreate, SphereSetAngleLimits,
CylinderCreate, ConeCreate, AnnulusCreate, TorusCreate, DiskCreate, FrustrumCreate,
DodecahedronCreate, IcosahedronCreate, TeapotCreate,
//Memory Viewer
MemoryViewerCreate, MemoryViewerSetCamera, MemoryViewerRender,
//ZShadows
ZShadowsCreate,
ZShadowsSetFrustShadow, ZShadowsSetSkyShadow, ZShadowsSetColor, ZShadowsCast,
ZShadowsSetSoft, ZShadowsSetTolerance, ZShadowsSetDepthFade,
//Actor
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
ActorFaceGroupSetMaterial,
//Freeform
FreeformCreate, FreeformMeshObjectsCount, FreeformMeshSetVisible,
FreeformMeshSetSecondCoords, FreeformMeshTriangleCount,  
FreeformFaceGroupsCount, FreeformFaceGroupTriangleCount,
FreeformSetLightmapsFromFreeform,
FreeformCreateExplosionFX, FreeformExplosionFXReset,
FreeformExplosionFXEnable, FreeformExplosionFXSetSpeed,
//Terrain
BmpHDSCreate, BmpHDSSetInfiniteWarp, BmpHDSInvert,
TerrainCreate, TerrainSetHeightData, TerrainSetTileSize, TerrainSetTilesPerTexture,
TerrainSetQualityDistance, TerrainSetQualityStyle, TerrainSetMaxCLodTriangles,
TerrainSetCLodPrecision, TerrainSetOcclusionFrameSkip, TerrainSetOcclusionTesselate,
TerrainGetHeightAtObjectPosition, TerrainGetLastTriCount,
//Object
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
ObjectScale, ObjectSetScale,
ObjectSetUpVector, ObjectPointToObject, 
ObjectShowAxes,
ObjectGetGroundHeight, ObjectSceneRaycast, ObjectRaycast,
ObjectGetCollisionPosition, ObjectGetCollisionNormal, 
ObjectSetMaterial,
ObjectGetDistance,
ObjectCheckCubeVsCube, ObjectCheckSphereVsSphere, ObjectCheckSphereVsCube,
ObjectIsPointInObject,
ObjectSetCulling,
ObjectSetName, ObjectGetName, ObjectGetClassName,
ObjectSetTag, ObjectGetTag,
ObjectGetParent, ObjectGetChildCound, ObjectGetChild, ObjectGetIndex, ObjectFindChild,
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
ObjectSetMatrixColumn,
ObjectExportMatrix, ObjectExportAbsoluteMatrix,
//Polygon
PolygonCreate, PolygonAddVertex, PolygonSetVertexPosition, PolygonDeleteVertex,
//Material
MaterialLibraryCreate, MaterialLibraryActivate, MaterialLibrarySetTexturePaths,
MaterialLibraryClear, MaterialLibraryDeleteUnused,
MaterialLibraryHasMaterial, MaterialLibraryLoadScript, 
MaterialCreate,
MaterialAddCubeMap, MaterialCubeMapLoadImage, MaterialCubeMapGenerate, MaterialCubeMapFromScene,
MaterialSaveTexture, MaterialSetBlendingMode, MaterialSetOptions,
MaterialSetTextureMappingMode, MaterialSetTextureMode,
MaterialSetShader, MaterialSetSecondTexture,
MaterialSetDiffuseColor, MaterialSetAmbientColor, MaterialSetSpecularColor, MaterialSetEmissionColor,
MaterialSetShininess,
MaterialSetPolygonMode, MaterialSetTextureImageAlpha,
MaterialSetTextureScale, MaterialSetTextureOffset,
MaterialSetTextureFilter, MaterialEnableTexture,
MaterialGetCount, MaterialGetName,
MaterialSetFaceCulling, MaterialSetSecondTexture,
MaterialSetTextureFormat, MaterialSetTextureCompression,
MaterialTextureRequiredMemory, MaterialSetFilteringQuality,
MaterialAddTextureEx, MaterialTextureExClear, MaterialTextureExDelete,
MaterialNoiseCreate, MaterialNoiseAnimate, MaterialNoiseSetDimensions,
MaterialNoiseSetMinCut, MaterialNoiseSetSharpness, MaterialNoiseSetSeamless,
MaterialNoiseRandomSeed,
//Shaders
ShaderEnable, 
BumpShaderCreate, BumpShaderSetMethod, BumpShaderSetSpecularMode,
BumpShaderSetSpace, BumpShaderSetOptions, BumpShaderSetParallaxOffset,
CelShaderCreate, CelShaderSetLineColor, CelShaderSetLineWidth, CelShaderSetOptions,
MultiMaterialShaderCreate,
HiddenLineShaderCreate, HiddenLineShaderSetLineSmooth, HiddenLineShaderSetSolid,
HiddenLineShaderSetSurfaceLit, HiddenLineShaderSetFrontLine, HiddenLineShaderSetBackLine,
OutlineShaderCreate, OutlineShaderSetLineColor, OutlineShaderSetLineWidth,
TexCombineShaderCreate, TexCombineShaderAddCombiner,
TexCombineShaderMaterial3, TexCombineShaderMaterial4,
PhongShaderCreate,
GLSLShaderCreate, GLSLShaderCreateParameter,
GLSLShaderSetParameter1i, GLSLShaderSetParameter1f, GLSLShaderSetParameter2f,
GLSLShaderSetParameter3f, GLSLShaderSetParameter4f, GLSLShaderSetParameterTexture,
GLSLShaderSetParameterMatrix, GLSLShaderSetParameterInvMatrix,
//ThorFX
ThorFXManagerCreate, ThorFXSetColor, ThorFXEnableCore, ThorFXEnableGlow,
ThorFXSetMaxParticles, ThorFXSetGlowSize, ThorFXSetVibrate, ThorFXSetWildness,
ThorFXSetTarget, ThorFXCreate,
// FireFX
FireFXManagerCreate, FireFXCreate,
FireFXSetColor, FireFXSetMaxParticles, FireFXSetParticleSize,
FireFXSetDensity, FireFXSetEvaporation, FireFXSetCrown,
FireFXSetLife, FireFXSetBurst, FireFXSetRadius, FireFXExplosion,
//Lensflare
LensflareCreate, LensflareSetSize, LensflareSetSeed, LensflareSetSqueeze,
LensflareSetStreaks, LensflareSetStreakWidth, LensflareSetSecs,
LensflareSetResolution, LensflareSetElements, LensflareSetGradients,
//Skydome
SkydomeCreate, SkydomeSetOptions, SkydomeSetDeepColor, SkydomeSetHazeColor,
SkydomeSetNightColor, SkydomeSetSkyColor, SkydomeSetSunDawnColor, SkydomeSetSunZenithColor,
SkydomeSetSunElevation, SkydomeSetTurbidity,
SkydomeAddRandomStars, SkydomeAddStar, SkydomeClearStars, SkydomeTwinkleStars, 
//Water
WaterCreate, WaterCreateRandomRipple,
WaterCreateRippleAtGridPosition, WaterCreateRippleAtWorldPosition,
WaterCreateRippleAtObjectPosition,
WaterSetMask, WaterSetActive, WaterReset,
WaterSetRainTimeInterval, WaterSetRainForce,
WaterSetViscosity, WaterSetElastic, WaterSetResolution,
WaterSetLinearWaveHeight, WaterSetLinearWaveFrequency,
//Blur
BlurCreate, BlurSetPreset, BlurSetOptions, BlurSetResolution,
BlurSetColor, BlurSetBlendingMode,
//Skybox
SkyboxCreate, SkyboxSetMaterial, SkyboxSetClouds, SkyboxSetStyle,
//Shadowplane
ShadowplaneCreate, ShadowplaneSetLight, ShadowplaneSetObject, ShadowplaneSetOptions,
//Shadowvolume
ShadowvolumeCreate, ShadowvolumeSetActive,
ShadowvolumeAddLight, ShadowvolumeRemoveLight,
ShadowvolumeAddOccluder, ShadowvolumeRemoveOccluder,
ShadowvolumeSetDarkeningColor, ShadowvolumeSetMode, ShadowvolumeSetOptions,
//Navigator
NavigatorCreate, NavigatorSetObject, NavigatorSetUseVirtualUp, NavigatorSetVirtualUp,  
NavigatorTurnHorizontal, NavigatorTurnVertical, NavigatorMoveForward,
NavigatorStrafeHorizontal, NavigatorStrafeVertical, NavigatorStraighten,
NavigatorFlyForward, NavigatorMoveUpWhenMovingForward, NavigatorInvertHorizontalWhenUpsideDown,
NavigatorSetAngleLock, NavigatorSetAngles,
//DCE
DceManagerCreate, DceManagerStep, DceManagerSetGravity, DceManagerSetWorldDirection,
DceManagerSetWorldScale, DceManagerSetMovementScale,
DceManagerSetLayers, DceManagerSetManualStep,
DceDynamicSetManager, DceDynamicSetActive, DceDynamicIsActive,
DceDynamicSetUseGravity, DceDynamicSetLayer, DceDynamicGetLayer,
DceDynamicSetSolid, DceDynamicSetFriction, DceDynamicSetBounce,
DceDynamicSetSize, DceDynamicSetSlideOrBounce,
DceDynamicApplyAcceleration, DceDynamicApplyAbsAcceleration,
DceDynamicStopAcceleration, DceDynamicStopAbsAcceleration,
DceDynamicJump, DceDynamicMove, DceDynamicMoveTo, DceDynamicSetSpeed,
DceDynamicInGround, DceDynamicSetMaxRecursionDepth,
DceStaticSetManager, DceStaticSetActive, DceStaticSetShape, DceStaticSetLayer,
DceStaticSetSize, DceStaticSetSolid, DceStaticSetFriction, DceStaticSetBounceFactor,
DceDynamicGetVelocity,
//FPSManager
FpsManagerCreate, FpsManagerSetNavigator, FpsManagerSetMovementScale,
FpsManagerAddMap, FpsManagerRemoveMap, FpsManagerMapSetCollisionGroup,
FpsSetManager, FpsSetCollisionGroup, FpsSetSphereRadius, FpsSetGravity,
FpsMove, FpsStrafe, FpsLift, FpsGetVelocity,
//Mirror
MirrorCreate, MirrorSetObject, MirrorSetOptions,
MirrorSetShape, MirrorSetDiskOptions,
//Text
TextRead;

begin
end.
