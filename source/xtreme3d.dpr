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
  GLSkyBox, GLShadowPlane, GLShadowVolume, GLSkydome, GLLensFlare, GLDCE;

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
{$I 'text'}

// DCE functions wrapper by Hacker

function DceManagerCreate: real; stdcall;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager.Create(scene);
  Result := Integer(DCEManager);
end;

function DceManagerStep(man, dt: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).Step(dt);
  Result := 1;
end;

function DceManagerSetGravity(man, grav: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).Gravity := grav;
  Result := 1;
end;

function DceManagerSetWorldDirection(man, x, y, z: real): real; stdcall;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager(trunc64(man));
  DCEManager.WorldDirection.X := x;
  DCEManager.WorldDirection.Y := y;
  DCEManager.WorldDirection.Z := z;
  Result := 1;
end;

function DceManagerSetWorldScale(man, scale: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).WorldScale := scale;
  Result := 1;
end;

function DceManagerSetMovementScale(man, scale: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).MovimentScale := scale;
  Result := 1;
end;

function DceManagerSetLayers(man, mode: real): real; stdcall;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager(trunc64(man));
  case trunc64(mode) of
    0: DCEManager.StandardiseLayers := ccsDCEStandard;
    1: DCEManager.StandardiseLayers := ccsCollisionStandard;
    2: DCEManager.StandardiseLayers := ccsHybrid;
  else
    DCEManager.StandardiseLayers := ccsDCEStandard;
  end;
  Result := 1;
end;

function DceManagerSetManualStep(man, mode: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).ManualStep := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicSetManager(obj, man: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  GetOrCreateDCEDynamic(ob).Manager := TGLDCEManager(trunc64(man));
  Result := 1;
end;

function DceDynamicSetActive(obj, mode: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  GetOrCreateDCEDynamic(ob).Active := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicIsActive(obj: real): real; stdcall;
begin
  Result := Integer(GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Active);
end;

function DceDynamicSetUseGravity(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).UseGravity := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicSetLayer(obj, layer: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Layer := trunc64(layer);
  Result := 1;
end;

function DceDynamicGetLayer(obj: real): real; stdcall;
begin
  Result := GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Layer;
end;

function DceDynamicSetSolid(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Solid := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicSetFriction(obj, friction: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Friction := friction;
  Result := 1;
end;

function DceDynamicSetBounce(obj, bounce: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).BounceFactor := bounce;
  Result := 1;
end;

function DceDynamicSetSize(obj, x, y, z: real): real; stdcall;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  dyn.Size.X := x;
  dyn.Size.Y := y;
  dyn.Size.Z := z;
  Result := 1;
end;

function DceDynamicSetSlideOrBounce(obj, mode: real): real; stdcall;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  case trunc64(mode) of
    0: dyn.SlideOrBounce := csbSlide;
    1: dyn.SlideOrBounce := csbBounce;
  else
    dyn.SlideOrBounce := csbSlide;
  end;
  Result := 1;
end;

function DceDynamicApplyAcceleration(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).ApplyAccel(x, y, z);
  Result := 1;
end;

function DceDynamicApplyAbsAcceleration(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).ApplyAbsAccel(x, y, z);
  Result := 1;
end;

function DceDynamicStopAcceleration(obj: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).StopAccel;
  Result := 1;
end;

function DceDynamicStopAbsAcceleration(obj: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).StopAbsAccel;
  Result := 1;
end;

function DceDynamicJump(obj, height, speed: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Jump(height, speed);
  Result := 1;
end;

function DceDynamicMove(obj, x, y, z, delta: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Move(AffineVectorMake(x, y, z), delta);
  Result := 1;
end;

function DceDynamicMoveTo(obj, x, y, z, amount: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).MoveTo(AffineVectorMake(x, y, z), amount);
  Result := 1;
end;

function DceDynamicSetSpeed(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Speed := AffineVectorMake(x, y, z);
  Result := 1;
end;

function DceDynamicInGround(obj: real): real; stdcall;
begin
  Result := Integer(GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).InGround);
end;

function DceDynamicSetMaxRecursionDepth(obj, depth: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).MaxRecursionDepth := trunc64(depth);
  Result := 1;
end;

function DceStaticSetManager(obj, man: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Manager :=
    TGLDCEManager(trunc64(man));
  Result := 1;
end;

function DceStaticSetActive(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Active :=
    Boolean(trunc64(mode));
  Result := 1;
end;

function DceStaticSetShape(obj, mode: real): real; stdcall;
var
  stat: TGLDCEStatic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  stat := GetOrCreateDCEStatic(ob);
  case Trunc(mode) of
    0: stat.Shape := csEllipsoid;
    1: stat.Shape := csBox;
    2: stat.Shape := csFreeform;
    3: stat.Shape := csTerrain;
  else
    Result := 0;
    Exit;
  end;
  Result := 1;
end;

function DceStaticSetLayer(obj, layer: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Layer := trunc64(layer);
  Result := 1;
end;

function DceStaticSetSize(obj, x, y, z: real): real; stdcall;
var
  stat: TGLDCEStatic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  stat := GetOrCreateDCEStatic(ob);
  stat.Size.X := x;
  stat.Size.Y := y;
  stat.Size.Z := z;
  Result := 1;
end;

function DceStaticSetSolid(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Solid := Boolean(trunc64(mode));
  Result := 1;
end;

function DceStaticSetFriction(obj, friction: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Friction := friction;
  Result := 1;
end;

function DceStaticSetBounceFactor(obj, bfactor: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).BounceFactor := bfactor;
  Result := 1;
end;

// TODO:
// DceDynamicGetVelocity
// DceDynamicGetGravity
// DceDynamicVelocityCollided
// DceDynamicGravityCollided
// DceCountCollisions
// DceGetCollidedObject
// DceGetCollisionPosition
// DceGetCollisionNormal

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
//Text
TextRead;

begin
end.
