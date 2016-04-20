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
  GLSkyBox, GLShadowPlane, GLSkydome, GLLensFlare;

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
{$I 'terrain'}
{$I 'blur'}
{$I 'skybox'}
{$I 'shadowplane'}
{$I 'skydome'}
{$I 'water'}
{$I 'text'}

function LensflareCreate(parent: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  if not (parent=0) then
    lf := TGLLensFlare.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    lf := TGLLensFlare.CreateAsChild(scene.Objects);
  result := Integer(lf);
end;

function LensflareSetSize(lensflare, size: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.Size := trunc64(size);
  result := 1.0;
end;

function LensflareSetSeed(lensflare, seed: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.Seed := trunc64(seed);
  result := 1.0;
end;

function LensflareSetSqueeze(lensflare, squeeze: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.Squeeze := squeeze;
  result := 1.0;
end;

function LensflareSetStreaks(lensflare, streaks: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.NumStreaks := trunc64(streaks);
  result := 1.0;
end;

function LensflareSetStreakWidth(lensflare, width: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.StreakWidth := width;
  result := 1.0;
end;

function LensflareSetSecs(lensflare, secs: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.NumSecs := trunc64(secs);
  result := 1.0;
end;

function LensflareSetResolution(lensflare, res: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.Resolution := trunc64(res);
  result := 1.0;
end;

function LensflareSetElements(lensflare, glow, ring, streaks, rays, secs: real): real; stdcall;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  lf.Elements := [];
  if glow = 1    then lf.Elements := lf.Elements + [feGlow];
  if ring = 1    then lf.Elements := lf.Elements + [feRing];
  if streaks = 1 then lf.Elements := lf.Elements + [feStreaks];
  if rays = 1    then lf.Elements := lf.Elements + [feRays];
  if secs = 1    then lf.Elements := lf.Elements + [feSecondaries];
  result := 1.0;
end;

function LensflareSetGradients(lensflare, ind, color1, alpha1, color2, alpha2 : real): real; stdcall;
var
  lf: TGLLensFlare;
  gradient: TGLFlareGradient;
begin
  lf := TGLLensFlare(trunc64(lensflare));
  if ind = 0 then gradient := lf.GlowGradient;
  if ind = 1 then gradient := lf.RingGradient;
  if ind = 2 then gradient := lf.StreaksGradient;
  if ind = 3 then gradient := lf.RaysGradient;
  if ind = 4 then gradient := lf.SecondariesGradient;
  gradient.FromColor.AsWinColor := TColor(trunc64(color1));
  gradient.FromColor.Alpha := alpha1;
  gradient.ToColor.AsWinColor := TColor(trunc64(color2));
  gradient.ToColor.Alpha := alpha2;
  result := 1.0;
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
//Text
TextRead;

begin
end.
