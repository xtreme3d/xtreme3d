/* Xtreme3D header */

#ifndef XTREME3D_H
#define XTREME3D_H

#include <xtreme3d_constants.h>
#include <xtreme3d_color_codes.h>
#include <xtreme3d_input_codes.h>

#ifdef __cplusplus
extern "C" {
#endif

/* actor.pas */
__declspec(dllimport) double ActorCreate(char*, double, double);
__declspec(dllimport) double ActorCopy(double, double);
__declspec(dllimport) double ActorSetAnimationRange(double, double, double);
__declspec(dllimport) double ActorGetCurrentFrame(double);
__declspec(dllimport) double ActorSwitchToAnimation(double, double, double);
__declspec(dllimport) double ActorSwitchToAnimationName(double, char*, double);
__declspec(dllimport) double ActorSynchronize(double, double);
__declspec(dllimport) double ActorSetInterval(double, double);
__declspec(dllimport) double ActorSetAnimationMode(double, double);
__declspec(dllimport) double ActorSetFrameInterpolation(double, double);
__declspec(dllimport) double ActorAddObject(double, char*);
__declspec(dllimport) char* ActorGetCurrentAnimation(double);
__declspec(dllimport) double ActorGetFrameCount(double);
__declspec(dllimport) double ActorGetBoneCount(double);
__declspec(dllimport) double ActorGetBoneByName(double, char*);
__declspec(dllimport) double ActorGetBoneRotation(double, double, double);
__declspec(dllimport) double ActorGetBonePosition(double, double, double);
__declspec(dllimport) double ActorBoneExportMatrix(double, double, double);
__declspec(dllimport) double ActorMakeSkeletalTranslationStatic(double, double);
__declspec(dllimport) double ActorMakeSkeletalRotationDelta(double, double);
__declspec(dllimport) double ActorShowSkeleton(double, double);
__declspec(dllimport) double AnimationBlenderCreate(void);
__declspec(dllimport) double AnimationBlenderSetActor(double, double);
__declspec(dllimport) double AnimationBlenderSetAnimation(double, char*);
__declspec(dllimport) double AnimationBlenderSetRatio(double, double);
__declspec(dllimport) double ActorLoadQ3TagList(char*);
__declspec(dllimport) double ActorQ3TagExportMatrix(double, double, char*, double);
__declspec(dllimport) double ActorLoadQ3Animations(double, char*, char*);
__declspec(dllimport) double ActorMeshObjectsCount(double);
__declspec(dllimport) double ActorFaceGroupsCount(double, double);
__declspec(dllimport) char* ActorFaceGroupGetMaterialName(double, double, double);
__declspec(dllimport) double ActorFaceGroupSetMaterial(double, double, double, char*);
__declspec(dllimport) double ActorMoveBone(double, double, double, double, double);
__declspec(dllimport) double ActorRotateBone(double, double, double, double, double);
__declspec(dllimport) double ActorMeshSetVisible(double, double, double);
__declspec(dllimport) char* ActorGetAnimationName(double, double);
__declspec(dllimport) double ActorGetAnimationCount(double);
__declspec(dllimport) double ActorAnimationDestroy(double, double);
__declspec(dllimport) double ActorAnimationNextFrame(double);
__declspec(dllimport) double ActorAnimationPrevFrame(double);
__declspec(dllimport) double ActorSetFrame(double, double);
__declspec(dllimport) double ActorTriangleCount(double);
__declspec(dllimport) double ActorSetReference(double, double);

/* audio.pas */
__declspec(dllimport) double AudioInit(void);
__declspec(dllimport) double AudioClose(void);
__declspec(dllimport) double AudioChannelIsPlaying(double);
__declspec(dllimport) double AudioMusicIsPlaying(void);
__declspec(dllimport) double AudioSetChannelVolume(double, double);
__declspec(dllimport) double AudioSetMusicVolume(double);
__declspec(dllimport) double AudioSetChannelPannning(double, double);
__declspec(dllimport) double AudioSetChannelPosition(double, double, double);
__declspec(dllimport) double AudioSetChannelDistance(double, double);
__declspec(dllimport) double AudioStopChannel(double, double);
__declspec(dllimport) double AudioStopChannelDelayed(double, double);
__declspec(dllimport) double AudioStopMusic(double);
__declspec(dllimport) double AudioPauseMusic(void);
__declspec(dllimport) double AudioResumeMusic(void);
__declspec(dllimport) double AudioRewindMusic(void);
__declspec(dllimport) double AudioSetMusicPosition(double);
__declspec(dllimport) double SoundLoad(char*);
__declspec(dllimport) double SoundPlay(double, double, double);
__declspec(dllimport) double MusicLoad(char*);
__declspec(dllimport) double MusicPlay(double, double);

/* blur.pas */
__declspec(dllimport) double BlurCreate(double, double);
__declspec(dllimport) double BlurSetPreset(double, double);
__declspec(dllimport) double BlurSetOptions(double, double, double, double, double, double);
__declspec(dllimport) double BlurSetResolution(double, double);
__declspec(dllimport) double BlurSetColor(double, double);
__declspec(dllimport) double BlurSetBlendingMode(double, double);

/* camera.pas */
__declspec(dllimport) double CameraCreate(double);
__declspec(dllimport) double CameraSetStyle(double, double);
__declspec(dllimport) double CameraSetFocal(double, double);
__declspec(dllimport) double CameraSetSceneScale(double, double);
__declspec(dllimport) double CameraScaleScene(double, double);
__declspec(dllimport) double CameraSetViewDepth(double, double);
__declspec(dllimport) double CameraSetTargetObject(double, double);
__declspec(dllimport) double CameraMoveAroundTarget(double, double, double);
__declspec(dllimport) double CameraSetDistanceToTarget(double, double);
__declspec(dllimport) double CameraGetDistanceToTarget(double);
__declspec(dllimport) double CameraCopyToTexture(double, char*, double, double);
__declspec(dllimport) double CameraGetNearPlane(double);
__declspec(dllimport) double CameraSetNearPlaneBias(double, double);
__declspec(dllimport) double CameraAbsoluteVectorToTarget(double, double);
__declspec(dllimport) double CameraAbsoluteRightVectorToTarget(double, double);
__declspec(dllimport) double CameraAbsoluteUpVectorToTarget(double, double);
__declspec(dllimport) double CameraZoomAll(double, double);
__declspec(dllimport) double CameraScreenDeltaToVector(double, double, double, double, double, double, double, double);
__declspec(dllimport) double CameraScreenDeltaToVectorXY(double, double, double, double, double);
__declspec(dllimport) double CameraScreenDeltaToVectorXZ(double, double, double, double, double);
__declspec(dllimport) double CameraScreenDeltaToVectorYZ(double, double, double, double, double);
__declspec(dllimport) double CameraAbsoluteEyeSpaceVector(double, double, double, double, double);
__declspec(dllimport) double CameraSetAutoLeveling(double, double);
__declspec(dllimport) double CameraMoveInEyeSpace(double, double, double, double);
__declspec(dllimport) double CameraMoveTargetInEyeSpace(double, double, double, double);
__declspec(dllimport) double CameraPointInFront(double, double, double, double);
__declspec(dllimport) double CameraGetFieldOfView(double, double);

/* clipplane.pas */
__declspec(dllimport) double ClipPlaneCreate(double);
__declspec(dllimport) double ClipPlaneEnable(double, double);
__declspec(dllimport) double ClipPlaneSetPlane(double, double, double, double, double, double, double);

/* color.pas */
__declspec(dllimport) double MakeColorRGB(double, double, double);
__declspec(dllimport) double MakeColorRGBFloat(double, double, double);

/* dce.pas */
__declspec(dllimport) double DceManagerCreate(void);
__declspec(dllimport) double DceManagerStep(double, double);
__declspec(dllimport) double DceManagerSetGravity(double, double);
__declspec(dllimport) double DceManagerSetWorldDirection(double, double, double, double);
__declspec(dllimport) double DceManagerSetWorldScale(double, double);
__declspec(dllimport) double DceManagerSetMovementScale(double, double);
__declspec(dllimport) double DceManagerSetLayers(double, double);
__declspec(dllimport) double DceManagerSetManualStep(double, double);
__declspec(dllimport) double DceDynamicSetManager(double, double);
__declspec(dllimport) double DceDynamicSetActive(double, double);
__declspec(dllimport) double DceDynamicIsActive(double);
__declspec(dllimport) double DceDynamicSetUseGravity(double, double);
__declspec(dllimport) double DceDynamicSetLayer(double, double);
__declspec(dllimport) double DceDynamicGetLayer(double);
__declspec(dllimport) double DceDynamicSetSolid(double, double);
__declspec(dllimport) double DceDynamicSetFriction(double, double);
__declspec(dllimport) double DceDynamicSetBounce(double, double);
__declspec(dllimport) double DceDynamicSetSize(double, double, double, double);
__declspec(dllimport) double DceDynamicSetSlideOrBounce(double, double);
__declspec(dllimport) double DceDynamicApplyAcceleration(double, double, double, double);
__declspec(dllimport) double DceDynamicApplyAbsAcceleration(double, double, double, double);
__declspec(dllimport) double DceDynamicStopAcceleration(double);
__declspec(dllimport) double DceDynamicStopAbsAcceleration(double);
__declspec(dllimport) double DceDynamicJump(double, double, double);
__declspec(dllimport) double DceDynamicMove(double, double, double, double, double);
__declspec(dllimport) double DceDynamicMoveTo(double, double, double, double, double);
__declspec(dllimport) double DceDynamicSetVelocity(double, double, double, double);
__declspec(dllimport) double DceDynamicInGround(double);
__declspec(dllimport) double DceDynamicSetMaxRecursionDepth(double, double);
__declspec(dllimport) double DceStaticSetManager(double, double);
__declspec(dllimport) double DceStaticSetActive(double, double);
__declspec(dllimport) double DceStaticSetShape(double, double);
__declspec(dllimport) double DceStaticSetLayer(double, double);
__declspec(dllimport) double DceStaticSetSize(double, double, double, double);
__declspec(dllimport) double DceStaticSetSolid(double, double);
__declspec(dllimport) double DceStaticSetFriction(double, double);
__declspec(dllimport) double DceStaticSetBounceFactor(double, double);
__declspec(dllimport) double DceDynamicGetVelocity(double, double);
__declspec(dllimport) double DceDynamicSetAbsVelocity(double, double, double, double);
__declspec(dllimport) double DceDynamicGetAbsVelocity(double, double);
__declspec(dllimport) double DceDynamicApplyImpulse(double, double, double, double);
__declspec(dllimport) double DceDynamicApplyAbsImpulse(double, double, double, double);

/* dummycube.pas */
__declspec(dllimport) double DummycubeCreate(double);
__declspec(dllimport) double DummycubeAmalgamate(double, double);
__declspec(dllimport) double DummycubeSetCameraMode(double, double);
__declspec(dllimport) double DummycubeSetVisible(double, double);
__declspec(dllimport) double DummycubeSetEdgeColor(double, double);
__declspec(dllimport) double DummycubeSetCubeSize(double, double);

/* engine.pas */
__declspec(dllimport) double EngineCreate(void);
__declspec(dllimport) double EngineDestroy(void);
__declspec(dllimport) double EngineSetObjectsSorting(double);
__declspec(dllimport) double EngineSetCulling(double);
__declspec(dllimport) double EngineUpdate(double);
__declspec(dllimport) double EngineSaveScene(char*);
__declspec(dllimport) double EngineLoadScene(char*);
__declspec(dllimport) double EngineRootObject(void);
__declspec(dllimport) double EngineShowLoadingErrors(double);
__declspec(dllimport) double EngineSetMaxLights(double);
__declspec(dllimport) double EngineGetTimeStep(void);
__declspec(dllimport) double EngineGetLastRaycastPosition(double);
__declspec(dllimport) double EngineGetLastRaycastNormal(double);
__declspec(dllimport) double PointerToReal(char*);

/* fbo.pas */
__declspec(dllimport) double FBOCreate(double, double, double);
__declspec(dllimport) double FBOSetActive(double, double);
__declspec(dllimport) double FBOSetAspect(double, double);
__declspec(dllimport) double FBOSetPickableTarget(double, double);
__declspec(dllimport) double FBOSetSize(double, double, double);
__declspec(dllimport) double FBOSetCamera(double, double);
__declspec(dllimport) double FBOSetRootObject(double, double);
__declspec(dllimport) double FBOSetBackgroundColor(double, double);
__declspec(dllimport) double FBOSetEnabledRenderBuffers(double, double, double);
__declspec(dllimport) double FBOSetSceneScaleFactor(double, double);
__declspec(dllimport) double FBOSetTargetVisibility(double, double);
__declspec(dllimport) double FBOSetMaterialLibrary(double, double);
__declspec(dllimport) double FBOSetColorTextureName(double, char*);
__declspec(dllimport) double FBOSetDepthTextureName(double, char*);
__declspec(dllimport) double FBOSetClearOptions(double, double, double, double, double);
__declspec(dllimport) double FBOSetStencilPrecision(double, double);
__declspec(dllimport) double FBOSetShadowMapMode(double, double);

/* firefx.pas */
__declspec(dllimport) double FireFXManagerCreate(void);
__declspec(dllimport) double FireFXCreate(double, double);
__declspec(dllimport) double FireFXSetColor(double, double, double, double, double);
__declspec(dllimport) double FireFXSetMaxParticles(double, double);
__declspec(dllimport) double FireFXSetParticleSize(double, double);
__declspec(dllimport) double FireFXSetDensity(double, double);
__declspec(dllimport) double FireFXSetEvaporation(double, double);
__declspec(dllimport) double FireFXSetCrown(double, double);
__declspec(dllimport) double FireFXSetLife(double, double);
__declspec(dllimport) double FireFXSetBurst(double, double);
__declspec(dllimport) double FireFXSetRadius(double, double);
__declspec(dllimport) double FireFXExplosion(double, double, double, double);
__declspec(dllimport) double FireFXRingExplosion(double, double, double, double, double, double, double, double, double, double);

/* fonttext.pas */
__declspec(dllimport) double BmpfontCreate(double, double, double, double, double, double, double, double);
__declspec(dllimport) double BmpfontLoad(double, char*);
__declspec(dllimport) double WindowsBitmapfontCreate(char*, double, double, double);
__declspec(dllimport) double HUDTextCreate(double, char*, double);
__declspec(dllimport) double HUDTextSetRotation(double, double);
__declspec(dllimport) double HUDTextSetFont(double, double);
__declspec(dllimport) double HUDTextSetColor(double, double, double);
__declspec(dllimport) double HUDTextSetText(double, char*);
__declspec(dllimport) double FlatTextCreate(double, char*, double);
__declspec(dllimport) double FlatTextSetFont(double, double);
__declspec(dllimport) double FlatTextSetColor(double, double, double);
__declspec(dllimport) double FlatTextSetText(double, char*);
__declspec(dllimport) double SpaceTextCreate(double, char*, double, double);
__declspec(dllimport) double SpaceTextSetExtrusion(double, double);
__declspec(dllimport) double SpaceTextSetFont(double, double);
__declspec(dllimport) double SpaceTextSetText(double, char*);
__declspec(dllimport) double TTFontCreate(char*, double);
__declspec(dllimport) double TTFontSetLineGap(double, double);

/* fps.pas */
__declspec(dllimport) double FpsManagerCreate(void);
__declspec(dllimport) double FpsManagerSetNavigator(double, double);
__declspec(dllimport) double FpsManagerSetMovementScale(double, double);
__declspec(dllimport) double FpsManagerAddMap(double, double);
__declspec(dllimport) double FpsManagerRemoveMap(double, double);
__declspec(dllimport) double FpsManagerMapSetCollisionGroup(double, double, double);
__declspec(dllimport) double FpsSetManager(double, double);
__declspec(dllimport) double FpsSetCollisionGroup(double, double);
__declspec(dllimport) double FpsSetSphereRadius(double, double);
__declspec(dllimport) double FpsSetGravity(double, double);
__declspec(dllimport) double FpsMove(double, double);
__declspec(dllimport) double FpsStrafe(double, double);
__declspec(dllimport) double FpsLift(double, double);
__declspec(dllimport) double FpsGetVelocity(double, double);

/* freeform.pas */
__declspec(dllimport) double FreeformCreate(char*, double, double, double);
__declspec(dllimport) double FreeformGenTangents(double);
__declspec(dllimport) double FreeformMeshObjectsCount(double);
__declspec(dllimport) double FreeformMeshSetVisible(double, double, double);
__declspec(dllimport) double FreeformMeshSetSecondCoords(double, double, double, double);
__declspec(dllimport) double FreeformMeshTriangleCount(double, double);
__declspec(dllimport) char* FreeformMeshObjectGetName(double, double);
__declspec(dllimport) double FreeformMeshObjectSetName(double, double, char*);
__declspec(dllimport) double FreeformMeshObjectDestroy(double, double);
__declspec(dllimport) double FreeformMeshFaceGroupsCount(double, double);
__declspec(dllimport) double FreeformMeshFaceGroupTriangleCount(double, double, double);
__declspec(dllimport) double FreeformCreateExplosionFX(double, double);
__declspec(dllimport) double FreeformExplosionFXReset(double);
__declspec(dllimport) double FreeformExplosionFXEnable(double, double);
__declspec(dllimport) double FreeformExplosionFXSetSpeed(double, double);
__declspec(dllimport) double FreeformSphereSweepIntersect(double, double, double, double);
__declspec(dllimport) double FreeformPointInMesh(double, double, double, double);
__declspec(dllimport) double FreeformMeshSetMaterial(double, double, char*);
__declspec(dllimport) double FreeformUseMeshMaterials(double, double);
__declspec(dllimport) double FreeformToFreeforms(double, double);
__declspec(dllimport) double FreeformMeshFaceGroupSetMaterial(double, double, double, char*);
__declspec(dllimport) char* FreeformMeshFaceGroupGetMaterial(double, double, double);
__declspec(dllimport) double FreeformCreateEmpty(double, double, double);
__declspec(dllimport) double FreeformAddMesh(double);
__declspec(dllimport) double FreeformMeshAddFaceGroup(double, double);
__declspec(dllimport) double FreeformMeshAddVertex(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshAddNormal(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshAddTexCoord(double, double, double, double);
__declspec(dllimport) double FreeformMeshAddSecondTexCoord(double, double, double, double);
__declspec(dllimport) double FreeformMeshAddTangent(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshAddBinormal(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshFaceGroupAddTriangle(double, double, double, double, double, double);
__declspec(dllimport) double FreeformMeshGenNormals(double, double);
__declspec(dllimport) double FreeformMeshGenTangents(double, double);
__declspec(dllimport) double FreeformMeshVerticesCount(double, double);
__declspec(dllimport) double FreeformMeshTranslate(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshRotate(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshScale(double, double, double, double, double);
__declspec(dllimport) double FreeformSave(double, char*);
__declspec(dllimport) double FreeformMeshGetVertex(double, double, double, double);
__declspec(dllimport) double FreeformMeshGetNormal(double, double, double, double);
__declspec(dllimport) double FreeformMeshGetTexCoord(double, double, double, double);
__declspec(dllimport) double FreeformMeshGetSecondTexCoord(double, double, double, double);
__declspec(dllimport) double FreeformMeshGetTangent(double, double, double, double);
__declspec(dllimport) double FreeformMeshGetBinormal(double, double, double, double);
__declspec(dllimport) double FreeformMeshFaceGroupGetIndex(double, double, double, double);
__declspec(dllimport) double FreeformMeshSetVertex(double, double, double, double, double, double);
__declspec(dllimport) double FreeformMeshSetNormal(double, double, double, double, double, double);
__declspec(dllimport) double FreeformMeshSetTexCoord(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshSetSecondTexCoord(double, double, double, double, double);
__declspec(dllimport) double FreeformMeshSetTangent(double, double, double, double, double, double);
__declspec(dllimport) double FreeformMeshSetBinormal(double, double, double, double, double, double);
__declspec(dllimport) double FreeformMeshFaceGroupSetIndex(double, double, double, double, double);
__declspec(dllimport) double FreeformBuildOctree(double);
__declspec(dllimport) double FreeformMeshFaceGroupGetLightmapIndex(double, double, double);
__declspec(dllimport) double FreeformMeshFaceGroupSetLightmapIndex(double, double, double, double);
__declspec(dllimport) double FreeformSetMaterialLibraries(double, double, double);
__declspec(dllimport) double BaseMeshBuildSilhouetteConnectivityData(double);

/* grid.pas */
__declspec(dllimport) double GridCreate(double, double, double, double, double);
__declspec(dllimport) double GridSetLineStyle(double, double);
__declspec(dllimport) double GridSetLineSmoothing(double, double);
__declspec(dllimport) double GridSetParts(double, double);
__declspec(dllimport) double GridSetColor(double, double, double);
__declspec(dllimport) double GridSetSize(double, double);
__declspec(dllimport) double GridSetPattern(double, double);
__declspec(dllimport) double GridSetTile(double, double, double, double);
__declspec(dllimport) double GridSetStep(double, double);

/* hudshapes.pas */
__declspec(dllimport) double HUDShapeRectangleCreate(double, double, double);
__declspec(dllimport) double HUDShapeCircleCreate(double, double, double, double, double);
__declspec(dllimport) double HUDShapeLineCreate(double, double, double, double, double);
__declspec(dllimport) double HUDShapeMeshCreate(double);
__declspec(dllimport) double HUDShapeSetRotation(double, double);
__declspec(dllimport) double HUDShapeRotate(double, double);
__declspec(dllimport) double HUDShapeSetColor(double, double, double);
__declspec(dllimport) double HUDShapeSetOrigin(double, double, double);
__declspec(dllimport) double HUDShapeSetSize(double, double, double);
__declspec(dllimport) double HUDShapeScale(double, double, double);
__declspec(dllimport) double HUDShapeCircleSetRadius(double, double);
__declspec(dllimport) double HUDShapeCircleSetSlices(double, double);
__declspec(dllimport) double HUDShapeCircleSetAngles(double, double, double);
__declspec(dllimport) double HUDShapeLineSetPoints(double, double, double, double, double);
__declspec(dllimport) double HUDShapeLineSetWidth(double, double);
__declspec(dllimport) double HUDShapeMeshAddVertex(double, double, double, double, double);
__declspec(dllimport) double HUDShapeMeshAddTriangle(double, double, double, double);
__declspec(dllimport) double HUDShapeMeshSetVertex(double, double, double, double);
__declspec(dllimport) double HUDShapeMeshSetTexCoord(double, double, double, double);

/* ini.pas */
__declspec(dllimport) double IniCreate(char*);
__declspec(dllimport) double IniClose(double);
__declspec(dllimport) double IniWriteString(double, char*, char*, char*);
__declspec(dllimport) double IniWriteNumber(double, char*, char*, double);
__declspec(dllimport) double IniWriteBool(double, char*, char*, double);
__declspec(dllimport) char* IniReadString(double, char*, char*, char*);
__declspec(dllimport) double IniReadNumber(double, char*, char*, double);
__declspec(dllimport) double IniReadBool(double, char*, char*, double);
__declspec(dllimport) double IniUpdateFile(double);

/* input.pas */
__declspec(dllimport) double MouseGetPositionX(void);
__declspec(dllimport) double MouseGetPositionY(void);
__declspec(dllimport) double MouseSetPosition(double, double);
__declspec(dllimport) double MouseShowCursor(double);
__declspec(dllimport) double KeyIsPressed(double);
__declspec(dllimport) double MouseIsPressed(double);

/* kraft.pas */
__declspec(dllimport) double KraftCreate(void);
__declspec(dllimport) double KraftStep(double, double);
__declspec(dllimport) double KraftGetRayHitPosition(double);
__declspec(dllimport) double KraftGetRayHitNormal(double);
__declspec(dllimport) double KraftCreateRigidBody(double, double);
__declspec(dllimport) double KraftRigidBodyFinish(double);
__declspec(dllimport) double KraftRigidBodySetGravity(double, double, double, double, double);
__declspec(dllimport) double KraftRigidBodySetPosition(double, double, double, double);
__declspec(dllimport) double KraftRigidBodyGetPosition(double, double);
__declspec(dllimport) double KraftRigidBodySetLinearVelocity(double, double, double, double);
__declspec(dllimport) double KraftRigidBodyGetLinearVelocity(double, double);
__declspec(dllimport) double KraftRigidBodySetRotation(double, double, double, double);
__declspec(dllimport) double KraftRigidBodyGetDirection(double, double);
__declspec(dllimport) double KraftRigidBodyGetUp(double, double);
__declspec(dllimport) double KraftRigidBodyGetRight(double, double);
__declspec(dllimport) double KraftRigidBodySetAngularVelocity(double, double, double, double);
__declspec(dllimport) double KraftRigidBodyGetAngularVelocity(double, double);
__declspec(dllimport) double KraftRigidBodyAddForce(double, double, double, double);
__declspec(dllimport) double KraftRigidBodyAddForceAtPos(double, double, double, double, double, double, double);
__declspec(dllimport) double KraftRigidBodyAddRelForce(double, double, double, double);
__declspec(dllimport) double KraftRayCast(double, double, double, double, double, double, double, double);
__declspec(dllimport) double KraftObjectSetRigidBody(double, double);
__declspec(dllimport) double KraftCreateShapeSphere(double, double);
__declspec(dllimport) double KraftCreateShapeBox(double, double, double, double);
__declspec(dllimport) double KraftCreateShapePlane(double, double, double, double, double);
__declspec(dllimport) double KraftCreateShapeCapsule(double, double, double);
__declspec(dllimport) double KraftCreateShapeMesh(double, double);
__declspec(dllimport) double KraftShapeSetDensity(double, double);
__declspec(dllimport) double KraftShapeSetFriction(double, double);
__declspec(dllimport) double KraftShapeSetRestitution(double, double);
__declspec(dllimport) double KraftShapeSetPosition(double, double, double, double);
__declspec(dllimport) double KraftShapeGetPosition(double, double);
__declspec(dllimport) double KraftShapeSetRayCastable(double, double);
__declspec(dllimport) double KraftCreateJointDistance(double, double);
__declspec(dllimport) double KraftCreateJointRope(double, double, double);
__declspec(dllimport) double KraftCreateJointBallSocket(double, double);
__declspec(dllimport) double KraftCreateJointFixed(double, double);
__declspec(dllimport) double KraftCreateJointHinge(double, double);
__declspec(dllimport) double KraftJointSetAnchor1(double, double, double, double);
__declspec(dllimport) double KraftJointSetAnchor2(double, double, double, double);
__declspec(dllimport) double KraftJointSetHingeAxis1(double, double, double, double);
__declspec(dllimport) double KraftJointSetHingeAxis2(double, double, double, double);

/* lensflare.pas */
__declspec(dllimport) double LensflareCreate(double);
__declspec(dllimport) double LensflareSetSize(double, double);
__declspec(dllimport) double LensflareSetSeed(double, double);
__declspec(dllimport) double LensflareSetSqueeze(double, double);
__declspec(dllimport) double LensflareSetStreaks(double, double);
__declspec(dllimport) double LensflareSetStreakWidth(double, double);
__declspec(dllimport) double LensflareSetSecs(double, double);
__declspec(dllimport) double LensflareSetResolution(double, double);
__declspec(dllimport) double LensflareSetElements(double, double, double, double, double, double);
__declspec(dllimport) double LensflareSetGradients(double, double, double, double, double, double);

/* light.pas */
__declspec(dllimport) double LightCreate(double, double);
__declspec(dllimport) double LightSetAmbientColor(double, double);
__declspec(dllimport) double LightSetDiffuseColor(double, double);
__declspec(dllimport) double LightSetSpecularColor(double, double);
__declspec(dllimport) double LightSetAttenuation(double, double, double, double);
__declspec(dllimport) double LightSetShining(double, double);
__declspec(dllimport) double LightSetSpotCutoff(double, double);
__declspec(dllimport) double LightSetSpotExponent(double, double);
__declspec(dllimport) double LightSetSpotDirection(double, double, double, double);
__declspec(dllimport) double LightSetStyle(double, double);
__declspec(dllimport) double LightGetColor(double, double);
__declspec(dllimport) double LightGetAttenuation(double, double);
__declspec(dllimport) double LightGetShining(double);

/* lightfx.pas */
__declspec(dllimport) double LightFXCreate(double);

/* lines.pas */
__declspec(dllimport) double LinesCreate(double);
__declspec(dllimport) double LinesAddNode(double, double, double, double);
__declspec(dllimport) double LinesDeleteNode(double, double);
__declspec(dllimport) double LinesSetNode(double, double, double, double, double);
__declspec(dllimport) double LinesSetSize(double, double, double);
__declspec(dllimport) double LinesSetSplineMode(double, double);
__declspec(dllimport) double LinesSetNodesAspect(double, double);
__declspec(dllimport) double LinesSetDivision(double, double);

/* logger.pas */
__declspec(dllimport) double LoggerCreate(char*, double);
__declspec(dllimport) double LoggerEnable(double, double);
__declspec(dllimport) double LoggerLog(double, double, char*);

/* material.pas */
__declspec(dllimport) double MaterialLibraryCreate(void);
__declspec(dllimport) double MaterialLibraryActivate(double);
__declspec(dllimport) double MaterialLibrarySetTexturePaths(double, char*);
__declspec(dllimport) double MaterialLibraryClear(double);
__declspec(dllimport) double MaterialLibraryDeleteUnused(double);
__declspec(dllimport) double MaterialLibraryHasMaterial(double, char*);
__declspec(dllimport) double MaterialLibraryLoadScript(double, char*);
__declspec(dllimport) double MaterialLibraryGetTextureByName(double, char*);
__declspec(dllimport) double MaterialCreate(char*, char*);
__declspec(dllimport) double MaterialDestroy(char*);
__declspec(dllimport) double MaterialAddCubeMap(char*);
__declspec(dllimport) double MaterialCubeMapLoadImage(char*, char*, double);
__declspec(dllimport) double MaterialCubeMapGenerate(char*, double);
__declspec(dllimport) double MaterialCubeMapFromScene(char*, double, double, double);
__declspec(dllimport) double MaterialSetName(char*, char*);
__declspec(dllimport) double MaterialSetShininess(char*, double);
__declspec(dllimport) double MaterialSetAmbientColor(char*, double, double);
__declspec(dllimport) double MaterialSetDiffuseColor(char*, double, double);
__declspec(dllimport) double MaterialSetSpecularColor(char*, double, double);
__declspec(dllimport) double MaterialSetEmissionColor(char*, double, double);
__declspec(dllimport) double MaterialGetColor(char*, double);
__declspec(dllimport) double MaterialGetAlpha(char*, double);
__declspec(dllimport) double MaterialSetBlendingMode(char*, double);
__declspec(dllimport) double MaterialSetTextureMode(char*, double);
__declspec(dllimport) double MaterialSetTextureMappingMode(char*, double);
__declspec(dllimport) double MaterialSetPolygonMode(char*, double);
__declspec(dllimport) double MaterialSetTextureImageAlpha(char*, double);
__declspec(dllimport) double MaterialSetTextureScale(char*, double, double);
__declspec(dllimport) double MaterialSetTextureOffset(char*, double, double);
__declspec(dllimport) double MaterialSetTextureFilter(char*, double, double);
__declspec(dllimport) double MaterialEnableTexture(char*, double);
__declspec(dllimport) double MaterialGetCount(void);
__declspec(dllimport) char* MaterialGetName(double);
__declspec(dllimport) double MaterialSetFaceCulling(char*, double);
__declspec(dllimport) double MaterialSetSecondTexture(char*, char*);
__declspec(dllimport) double MaterialSetTextureFormat(char*, double);
__declspec(dllimport) double MaterialSetTextureFormatEx(char*, double);
__declspec(dllimport) double MaterialSetTextureCompression(char*, double);
__declspec(dllimport) double MaterialTextureRequiredMemory(char*);
__declspec(dllimport) double MaterialSetFilteringQuality(char*, double);
__declspec(dllimport) double MaterialSetShader(char*, double);
__declspec(dllimport) double MaterialSaveTexture(char*, char*);
__declspec(dllimport) double MaterialSetOptions(char*, double, double);
__declspec(dllimport) double MaterialSetTextureWrap(char*, double);
__declspec(dllimport) double MaterialSetTextureWrapS(char*, double);
__declspec(dllimport) double MaterialSetTextureWrapT(char*, double);
__declspec(dllimport) double MaterialSetTextureWrapR(char*, double);
__declspec(dllimport) double MaterialSetTextureBorderColor(char*, double);
__declspec(dllimport) double MaterialGenTexture(char*, double, double);
__declspec(dllimport) double MaterialSetTexture(char*, char*);
__declspec(dllimport) double MaterialGetTextureWidth(char*);
__declspec(dllimport) double MaterialGetTextureHeight(char*);
__declspec(dllimport) double MaterialLoadTexture(char*, char*);
__declspec(dllimport) double MaterialAddTextureEx(char*, double);
__declspec(dllimport) double MaterialGetTextureEx(char*, double);
__declspec(dllimport) double MaterialTextureExClear(char*);
__declspec(dllimport) double MaterialHasTextureEx(char*, double);
__declspec(dllimport) double MaterialNoiseCreate(char*);
__declspec(dllimport) double MaterialNoiseSetDimensions(char*, double, double);
__declspec(dllimport) double MaterialNoiseAnimate(char*, double);
__declspec(dllimport) double MaterialNoiseSetMinCut(char*, double);
__declspec(dllimport) double MaterialNoiseSetSharpness(char*, double);
__declspec(dllimport) double MaterialNoiseSetSeamless(char*, double);
__declspec(dllimport) double MaterialNoiseRandomSeed(char*, double);
__declspec(dllimport) double MaterialSetDepthWrite(char*, double);
__declspec(dllimport) double MaterialSetDepthTest(char*, double);
__declspec(dllimport) char* MaterialGetNameFromLibrary(double, double);
__declspec(dllimport) double MaterialSetTextureCompareMode(char*, double);
__declspec(dllimport) double MaterialSetTextureDepthCompareFunc(char*, double);

/* memviewer.pas */
__declspec(dllimport) double MemoryViewerCreate(double, double);
__declspec(dllimport) double MemoryViewerSetCamera(double, double);
__declspec(dllimport) double MemoryViewerRender(double);
__declspec(dllimport) double MemoryViewerSetViewport(double, double, double, double, double);
__declspec(dllimport) double MemoryViewerCopyToTexture(double, char*);

/* mirror.pas */
__declspec(dllimport) double MirrorCreate(double, double);
__declspec(dllimport) double MirrorSetObject(double, double);
__declspec(dllimport) double MirrorSetOptions(double, double, double, double, double);
__declspec(dllimport) double MirrorSetShape(double, double);
__declspec(dllimport) double MirrorSetDiskOptions(double, double, double);

/* movement.pas */
__declspec(dllimport) double MovementCreate(double);
__declspec(dllimport) double MovementStart(double);
__declspec(dllimport) double MovementStop(double);
__declspec(dllimport) double MovementAutoStartNextPath(double, double);
__declspec(dllimport) double MovementAddPath(double);
__declspec(dllimport) double MovementSetActivePath(double, double);
__declspec(dllimport) double MovementPathSetSplineMode(double, double);
__declspec(dllimport) double MovementPathAddNode(double);
__declspec(dllimport) double MovementPathNodeSetPosition(double, double, double, double);
__declspec(dllimport) double MovementPathNodeSetRotation(double, double, double, double);
__declspec(dllimport) double MovementPathNodeSetSpeed(double, double);
__declspec(dllimport) double MovementPathShow(double, double);
__declspec(dllimport) double MovementPathSetLoop(double, double);
__declspec(dllimport) double MovementPathDeleteNode(double, double);

/* navigator.pas */
__declspec(dllimport) double NavigatorCreate(void);
__declspec(dllimport) double NavigatorSetObject(double, double);
__declspec(dllimport) double NavigatorSetUseVirtualUp(double, double);
__declspec(dllimport) double NavigatorSetVirtualUp(double, double, double, double);
__declspec(dllimport) double NavigatorTurnHorizontal(double, double);
__declspec(dllimport) double NavigatorTurnVertical(double, double);
__declspec(dllimport) double NavigatorMoveForward(double, double);
__declspec(dllimport) double NavigatorStrafeHorizontal(double, double);
__declspec(dllimport) double NavigatorStrafeVertical(double, double);
__declspec(dllimport) double NavigatorStraighten(double);
__declspec(dllimport) double NavigatorFlyForward(double, double);
__declspec(dllimport) double NavigatorMoveUpWhenMovingForward(double, double);
__declspec(dllimport) double NavigatorInvertHorizontalWhenUpsideDown(double, double);
__declspec(dllimport) double NavigatorSetAngleLock(double, double);
__declspec(dllimport) double NavigatorSetAngles(double, double, double);

/* object.pas */
__declspec(dllimport) double ObjectHide(double);
__declspec(dllimport) double ObjectShow(double);
__declspec(dllimport) double ObjectIsVisible(double);
__declspec(dllimport) double ObjectCopy(double, double);
__declspec(dllimport) double ObjectDestroy(double);
__declspec(dllimport) double ObjectDestroyChildren(double);
__declspec(dllimport) double ObjectSetPosition(double, double, double, double);
__declspec(dllimport) double ObjectGetPosition(double, double);
__declspec(dllimport) double ObjectGetAbsolutePosition(double, double);
__declspec(dllimport) double ObjectSetPositionOfObject(double, double);
__declspec(dllimport) double ObjectAlignWithObject(double, double);
__declspec(dllimport) double ObjectSetPositionX(double, double);
__declspec(dllimport) double ObjectSetPositionY(double, double);
__declspec(dllimport) double ObjectSetPositionZ(double, double);
__declspec(dllimport) double ObjectGetPositionX(double);
__declspec(dllimport) double ObjectGetPositionY(double);
__declspec(dllimport) double ObjectGetPositionZ(double);
__declspec(dllimport) double ObjectSetAbsolutePosition(double, double, double, double);
__declspec(dllimport) double ObjectSetDirection(double, double, double, double);
__declspec(dllimport) double ObjectGetDirection(double, double);
__declspec(dllimport) double ObjectSetAbsoluteDirection(double, double, double, double);
__declspec(dllimport) double ObjectGetAbsoluteDirection(double, double);
__declspec(dllimport) double ObjectGetPitch(double);
__declspec(dllimport) double ObjectGetTurn(double);
__declspec(dllimport) double ObjectGetRoll(double);
__declspec(dllimport) double ObjectSetRotation(double, double, double, double);
__declspec(dllimport) double ObjectMove(double, double);
__declspec(dllimport) double ObjectLift(double, double);
__declspec(dllimport) double ObjectStrafe(double, double);
__declspec(dllimport) double ObjectTranslate(double, double, double, double);
__declspec(dllimport) double ObjectRotate(double, double, double, double);
__declspec(dllimport) double ObjectScale(double, double, double, double);
__declspec(dllimport) double ObjectSetScale(double, double, double, double);
__declspec(dllimport) double ObjectGetScale(double, double);
__declspec(dllimport) double ObjectSetUpVector(double, double, double, double);
__declspec(dllimport) double ObjectPointToObject(double, double);
__declspec(dllimport) double ObjectShowAxes(double, double);
__declspec(dllimport) double ObjectGetGroundHeight(double, double);
__declspec(dllimport) double ObjectSceneRaycast(double, double);
__declspec(dllimport) double ObjectRaycast(double, double);
__declspec(dllimport) double ObjectSetMaterial(double, char*);
__declspec(dllimport) char* ObjectGetMaterial(double);
__declspec(dllimport) double ObjectGetDistance(double, double);
__declspec(dllimport) double ObjectCheckCubeVsCube(double, double);
__declspec(dllimport) double ObjectCheckSphereVsSphere(double, double);
__declspec(dllimport) double ObjectCheckSphereVsCube(double, double);
__declspec(dllimport) double ObjectCheckCubeVsFace(double, double);
__declspec(dllimport) double ObjectCheckFaceVsFace(double, double);
__declspec(dllimport) double ObjectIsPointInObject(double, double, double, double);
__declspec(dllimport) double ObjectSetCulling(double, double);
__declspec(dllimport) double ObjectSetName(double, char*);
__declspec(dllimport) char* ObjectGetName(double);
__declspec(dllimport) char* ObjectGetClassName(double);
__declspec(dllimport) double ObjectSetTag(double, double);
__declspec(dllimport) double ObjectGetTag(double);
__declspec(dllimport) double ObjectGetParent(double);
__declspec(dllimport) double ObjectGetChildCount(double);
__declspec(dllimport) double ObjectGetChild(double, double);
__declspec(dllimport) double ObjectGetIndex(double);
__declspec(dllimport) double ObjectFindChild(double, char*);
__declspec(dllimport) double ObjectGetBoundingSphereRadius(double);
__declspec(dllimport) double ObjectGetAbsoluteUp(double, double);
__declspec(dllimport) double ObjectSetAbsoluteUp(double, double, double, double);
__declspec(dllimport) double ObjectGetAbsoluteRight(double, double);
__declspec(dllimport) double ObjectGetAbsoluteXVector(double, double);
__declspec(dllimport) double ObjectGetAbsoluteYVector(double, double);
__declspec(dllimport) double ObjectGetAbsoluteZVector(double, double);
__declspec(dllimport) double ObjectGetRight(double, double);
__declspec(dllimport) double ObjectMoveChildUp(double, double);
__declspec(dllimport) double ObjectMoveChildDown(double, double);
__declspec(dllimport) double ObjectSetParent(double, double);
__declspec(dllimport) double ObjectRemoveChild(double, double, double);
__declspec(dllimport) double ObjectMoveObjectAround(double, double, double, double);
__declspec(dllimport) double ObjectPitch(double, double);
__declspec(dllimport) double ObjectTurn(double, double);
__declspec(dllimport) double ObjectRoll(double, double);
__declspec(dllimport) double ObjectGetUp(double, double);
__declspec(dllimport) double ObjectRotateAbsolute(double, double, double, double);
__declspec(dllimport) double ObjectRotateAbsoluteVector(double, double, double, double, double);
__declspec(dllimport) double ObjectSetMatrixColumn(double, double, double, double, double, double);
__declspec(dllimport) double ObjectExportMatrix(double, double);
__declspec(dllimport) double ObjectExportAbsoluteMatrix(double, double);
__declspec(dllimport) double ObjectInFrustum(double, double);
__declspec(dllimport) double ObjectFindByName(char*);
__declspec(dllimport) double ObjectIgnoreDepthBuffer(double, double);
__declspec(dllimport) double ObjectIsPicked(double, double, double, double);
__declspec(dllimport) double ObjectStructureChanged(double);
__declspec(dllimport) double ObjectClearStructureChanged(double);
__declspec(dllimport) double ObjectNotifyChange(double);

/* objecthash.pas */
__declspec(dllimport) double ObjectHashCreate(void);
__declspec(dllimport) double ObjectHashSetItem(double, double, double, double);
__declspec(dllimport) double ObjectHashGetItem(double);
__declspec(dllimport) double ObjectHashDeleteItem(double);
__declspec(dllimport) double ObjectHashGetItemCount(double);
__declspec(dllimport) double ObjectHashClear(double);
__declspec(dllimport) double ObjectHashDestroy(double);

/* objectlist.pas */
__declspec(dllimport) double ObjectListCreate(void);
__declspec(dllimport) double ObjectListAdd(double, double);
__declspec(dllimport) double ObjectListGetCount(double);

/* ode.pas */
__declspec(dllimport) double OdeManagerCreate(void);
__declspec(dllimport) double OdeManagerDestroy(void);
__declspec(dllimport) double OdeManagerStep(double);
__declspec(dllimport) double OdeManagerGetNumContactJoints(void);
__declspec(dllimport) double OdeManagerSetGravity(double, double, double);
__declspec(dllimport) double OdeManagerSetSolver(double);
__declspec(dllimport) double OdeManagerSetIterations(double);
__declspec(dllimport) double OdeManagerSetMaxContacts(double);
__declspec(dllimport) double OdeManagerSetVisible(double);
__declspec(dllimport) double OdeManagerSetGeomColor(double, double, double);
__declspec(dllimport) double OdeWorldSetAutoDisableFlag(double);
__declspec(dllimport) double OdeWorldSetAutoDisableLinearThreshold(double);
__declspec(dllimport) double OdeWorldSetAutoDisableAngularThreshold(double);
__declspec(dllimport) double OdeWorldSetAutoDisableSteps(double);
__declspec(dllimport) double OdeWorldSetAutoDisableTime(double);
__declspec(dllimport) double OdeStaticCreate(double);
__declspec(dllimport) double OdeDynamicCreate(double);
__declspec(dllimport) double OdeTerrainCreate(double);
__declspec(dllimport) double OdeDynamicCalculateMass(double);
__declspec(dllimport) double OdeDynamicCalibrateCenterOfMass(double);
__declspec(dllimport) double OdeDynamicAlignObject(double);
__declspec(dllimport) double OdeDynamicEnable(double, double);
__declspec(dllimport) double OdeDynamicSetAutoDisableFlag(double, double);
__declspec(dllimport) double OdeDynamicSetAutoDisableLinearThreshold(double, double);
__declspec(dllimport) double OdeDynamicSetAutoDisableAngularThreshold(double, double);
__declspec(dllimport) double OdeDynamicSetAutoDisableSteps(double, double);
__declspec(dllimport) double OdeDynamicSetAutoDisableTime(double, double);
__declspec(dllimport) double OdeDynamicAddForce(double, double, double, double);
__declspec(dllimport) double OdeDynamicAddForceAtPos(double, double, double, double, double, double, double);
__declspec(dllimport) double OdeDynamicAddForceAtRelPos(double, double, double, double, double, double, double);
__declspec(dllimport) double OdeDynamicAddRelForce(double, double, double, double);
__declspec(dllimport) double OdeDynamicAddRelForceAtPos(double, double, double, double, double, double, double);
__declspec(dllimport) double OdeDynamicAddRelForceAtRelPos(double, double, double, double, double, double, double);
__declspec(dllimport) double OdeDynamicAddTorque(double, double, double, double);
__declspec(dllimport) double OdeDynamicAddRelTorque(double, double, double, double);
__declspec(dllimport) double OdeAddBox(double, double, double, double, double, double, double);
__declspec(dllimport) double OdeAddSphere(double, double, double, double, double);
__declspec(dllimport) double OdeAddPlane(double);
__declspec(dllimport) double OdeAddCylinder(double, double, double, double, double, double);
__declspec(dllimport) double OdeAddCapsule(double, double, double, double, double, double);
__declspec(dllimport) double OdeAddTriMesh(double, double);
__declspec(dllimport) double OdeElementSetDensity(double, double);
__declspec(dllimport) double OdeSurfaceEnableRollingFrictionCoeff(double, double);
__declspec(dllimport) double OdeSurfaceSetRollingFrictionCoeff(double, double);
__declspec(dllimport) double OdeSurfaceSetMode(double, double, double, double, double, double, double, double, double, double);
__declspec(dllimport) double OdeSurfaceSetMu(double, double);
__declspec(dllimport) double OdeSurfaceSetMu2(double, double);
__declspec(dllimport) double OdeSurfaceSetBounce(double, double);
__declspec(dllimport) double OdeSurfaceSetBounceVel(double, double);
__declspec(dllimport) double OdeSurfaceSetSoftERP(double, double);
__declspec(dllimport) double OdeSurfaceSetSoftCFM(double, double);
__declspec(dllimport) double OdeSurfaceSetMotion1(double, double);
__declspec(dllimport) double OdeSurfaceSetMotion2(double, double);
__declspec(dllimport) double OdeSurfaceSetSlip1(double, double);
__declspec(dllimport) double OdeSurfaceSetSlip2(double, double);
__declspec(dllimport) double OdeAddJointBall(void);
__declspec(dllimport) double OdeAddJointFixed(void);
__declspec(dllimport) double OdeAddJointHinge(void);
__declspec(dllimport) double OdeAddJointHinge2(void);
__declspec(dllimport) double OdeAddJointSlider(void);
__declspec(dllimport) double OdeAddJointUniversal(void);
__declspec(dllimport) double OdeJointSetObjects(double, double, double);
__declspec(dllimport) double OdeJointEnable(double, double);
__declspec(dllimport) double OdeJointInitialize(double);
__declspec(dllimport) double OdeJointSetAnchor(double, double, double, double);
__declspec(dllimport) double OdeJointSetAnchorAtObject(double, double);
__declspec(dllimport) double OdeJointSetAxis1(double, double, double, double);
__declspec(dllimport) double OdeJointSetAxis2(double, double, double, double);
__declspec(dllimport) double OdeJointSetBounce(double, double, double);
__declspec(dllimport) double OdeJointSetCFM(double, double, double);
__declspec(dllimport) double OdeJointSetFMax(double, double, double);
__declspec(dllimport) double OdeJointSetFudgeFactor(double, double, double);
__declspec(dllimport) double OdeJointSetHiStop(double, double, double);
__declspec(dllimport) double OdeJointSetLoStop(double, double, double);
__declspec(dllimport) double OdeJointSetStopCFM(double, double, double);
__declspec(dllimport) double OdeJointSetStopERP(double, double, double);
__declspec(dllimport) double OdeJointSetVel(double, double, double);
__declspec(dllimport) double OdeRagdollCreate(double);
__declspec(dllimport) double OdeRagdollHingeJointCreate(double, double, double, double, double);
__declspec(dllimport) double OdeRagdollUniversalJointCreate(double, double, double, double, double, double, double, double, double, double);
__declspec(dllimport) double OdeRagdollDummyJointCreate(void);
__declspec(dllimport) double OdeRagdollBoneCreate(double, double, double, double);
__declspec(dllimport) double OdeRagdollBuild(double);
__declspec(dllimport) double OdeRagdollEnable(double, double);
__declspec(dllimport) double OdeRagdollUpdate(double);
__declspec(dllimport) double OdeDynamicSetVelocity(double, double, double, double);
__declspec(dllimport) double OdeDynamicSetAngularVelocity(double, double, double, double);
__declspec(dllimport) double OdeDynamicGetVelocity(double, double);
__declspec(dllimport) double OdeDynamicGetAngularVelocity(double, double);
__declspec(dllimport) double OdeDynamicSetPosition(double, double, double, double);
__declspec(dllimport) double OdeDynamicSetRotationQuaternion(double, double, double, double, double);

/* pak.pas */
__declspec(dllimport) double SetPakArchive(char*);
__declspec(dllimport) double PakGetFileCount(double);
__declspec(dllimport) char* PakGetFileName(double, double);
__declspec(dllimport) double PakExtract(double, char*);
__declspec(dllimport) double PakExtractFile(double, double, char*);

/* partition.pas */
__declspec(dllimport) double OctreeCreate(double, double, double, double);
__declspec(dllimport) double QuadtreeCreate(double, double, double, double);
__declspec(dllimport) double PartitionDestroy(double);
__declspec(dllimport) double PartitionAddLeaf(double, double);
__declspec(dllimport) double PartitionLeafChanged(double);
__declspec(dllimport) double PartitionQueryFrustum(double, double);
__declspec(dllimport) double PartitionQueryLeaf(double, double);
__declspec(dllimport) double PartitionQueryAABB(double, double);
__declspec(dllimport) double PartitionQueryBSphere(double, double);
__declspec(dllimport) double PartitionGetNodeTests(double);
__declspec(dllimport) double PartitionGetNodeCount(double);
__declspec(dllimport) double PartitionGetResult(double, double);
__declspec(dllimport) double PartitionGetResultCount(double);
__declspec(dllimport) double PartitionResultShow(double);
__declspec(dllimport) double PartitionResultHide(double);

/* picklist.pas */
__declspec(dllimport) double PickListCreate(double);
__declspec(dllimport) double PickListClear(double);
__declspec(dllimport) double PickListGetCount(double);
__declspec(dllimport) double PickListGetHit(double, double);

/* pipe.pas */
__declspec(dllimport) double PipeCreate(double, double, double);
__declspec(dllimport) double PipeAddNode(double, double, double, double);
__declspec(dllimport) double PipeSetDivision(double, double);
__declspec(dllimport) double PipeSetSplineMode(double, double);
__declspec(dllimport) double PipeDeleteNode(double, double);
__declspec(dllimport) double PipeSetRadius(double, double);
__declspec(dllimport) double PipeSetNode(double, double, double, double, double);
__declspec(dllimport) double PipeSetSlices(double, double);

/* primitives.pas */
__declspec(dllimport) double CubeCreate(double, double, double, double);
__declspec(dllimport) double CubeSetNormalDirection(double, double);
__declspec(dllimport) double CubeGetNormalDirection(double);
__declspec(dllimport) double PlaneCreate(double, double, double, double, double, double);
__declspec(dllimport) double PlaneSetOptions(double, double, double, double);
__declspec(dllimport) double PlaneGetOptions(double, double);
__declspec(dllimport) double TilePlaneCreate(double);
__declspec(dllimport) double TilePlaneSetTile(double, double, double, char*);
__declspec(dllimport) double SphereCreate(double, double, double, double);
__declspec(dllimport) double SphereSetAngleLimits(double, double, double, double, double);
__declspec(dllimport) double SphereGetAngleLimits(double, double);
__declspec(dllimport) double SphereSetOptions(double, double, double, double);
__declspec(dllimport) double SphereGetOptions(double, double);
__declspec(dllimport) double CylinderCreate(double, double, double, double, double, double, double);
__declspec(dllimport) double CylinderSetOptions(double, double, double, double, double, double, double);
__declspec(dllimport) double CylinderGetOptions(double, double);
__declspec(dllimport) double ConeCreate(double, double, double, double, double, double);
__declspec(dllimport) double ConeGetOptions(double, double);
__declspec(dllimport) double ConeSetOptions(double, double, double, double, double, double);
__declspec(dllimport) double AnnulusCreate(double, double, double, double, double, double, double);
__declspec(dllimport) double AnnulusSetOptions(double, double, double, double, double, double, double);
__declspec(dllimport) double AnnulusGetOptions(double, double);
__declspec(dllimport) double TorusCreate(double, double, double, double, double);
__declspec(dllimport) double TorusSetOptions(double, double, double, double, double);
__declspec(dllimport) double TorusGetOptions(double, double);
__declspec(dllimport) double DiskCreate(double, double, double, double, double, double, double);
__declspec(dllimport) double DiskSetOptions(double, double, double, double, double, double, double);
__declspec(dllimport) double DiskGetOptions(double, double);
__declspec(dllimport) double FrustrumCreate(double, double, double, double, double);
__declspec(dllimport) double FrustrumSetOptions(double, double, double, double, double);
__declspec(dllimport) double FrustrumGetOptions(double, double);
__declspec(dllimport) double DodecahedronCreate(double);
__declspec(dllimport) double IcosahedronCreate(double);
__declspec(dllimport) double TeapotCreate(double);

/* proxy.pas */
__declspec(dllimport) double ProxyObjectCreate(double, double);
__declspec(dllimport) double ProxyObjectSetTarget(double, double);
__declspec(dllimport) double MultiProxyObjectCreate(double);
__declspec(dllimport) double MultiProxyObjectAddTarget(double, double, double, double);
__declspec(dllimport) double ActorProxyObjectCreate(double, double);
__declspec(dllimport) double ActorProxyObjectSwitchToAnimation(double, double);
__declspec(dllimport) double ActorProxyObjectSetAnimationRange(double, double, double);
__declspec(dllimport) double ActorProxyObjectSetInterval(double, double);

/* sdl.pas */
__declspec(dllimport) double SDLLogError(double);

/* shaders.pas */
__declspec(dllimport) double ShaderEnable(double, double);
__declspec(dllimport) double CelShaderCreate(void);
__declspec(dllimport) double CelShaderSetLineColor(double, double);
__declspec(dllimport) double CelShaderSetLineWidth(double, double);
__declspec(dllimport) double CelShaderSetOptions(double, double, double);
__declspec(dllimport) double MultiMaterialShaderCreate(double);
__declspec(dllimport) double HiddenLineShaderCreate(void);
__declspec(dllimport) double HiddenLineShaderSetLineSmooth(double, double);
__declspec(dllimport) double HiddenLineShaderSetSolid(double, double);
__declspec(dllimport) double HiddenLineShaderSetSurfaceLit(double, double);
__declspec(dllimport) double HiddenLineShaderSetFrontLine(double, double, double, double, double);
__declspec(dllimport) double HiddenLineShaderSetBackLine(double, double, double, double, double);
__declspec(dllimport) double OutlineShaderCreate(double);
__declspec(dllimport) double OutlineShaderSetLineColor(double, double);
__declspec(dllimport) double OutlineShaderSetLineWidth(double, double);
__declspec(dllimport) double TexCombineShaderCreate(double);
__declspec(dllimport) double TexCombineShaderAddCombiner(double, char*);
__declspec(dllimport) double TexCombineShaderMaterial3(double, char*);
__declspec(dllimport) double TexCombineShaderMaterial4(double, char*);
__declspec(dllimport) double GLSLShaderCreate(char*, char*);
__declspec(dllimport) double GLSLShaderSetLogger(double, double);
__declspec(dllimport) double GLSLShaderCreateParameter(double, char*);
__declspec(dllimport) double GLSLShaderSetParameter1i(double, double);
__declspec(dllimport) double GLSLShaderSetParameter1f(double, double);
__declspec(dllimport) double GLSLShaderSetParameter2f(double, double, double);
__declspec(dllimport) double GLSLShaderSetParameter3f(double, double, double, double);
__declspec(dllimport) double GLSLShaderSetParameter4f(double, double, double, double, double);
__declspec(dllimport) double GLSLShaderSetParameterTexture(double, char*, double);
__declspec(dllimport) double GLSLShaderSetParameterSecondTexture(double, char*, double);
__declspec(dllimport) double GLSLShaderSetParameterShadowTexture(double, double, double);
__declspec(dllimport) double GLSLShaderSetParameterShadowMatrix(double, double);
__declspec(dllimport) double GLSLShaderSetParameterMatrix(double, double);
__declspec(dllimport) double GLSLShaderSetParameterInvMatrix(double, double);
__declspec(dllimport) double GLSLShaderSetParameterFBOColorTexture(double, double, double);
__declspec(dllimport) double GLSLShaderSetParameterFBODepthTexture(double, double, double);
__declspec(dllimport) double GLSLShaderSetParameterViewMatrix(double);
__declspec(dllimport) double GLSLShaderSetParameterInvViewMatrix(double);
__declspec(dllimport) double GLSLShaderSetParameterHasTextureEx(double, double);
__declspec(dllimport) double GLSLShaderForceDisableStencilTest(double, double);
__declspec(dllimport) double GLSLShaderSetOptions(double, double, double);
__declspec(dllimport) double PhongShaderCreate(void);
__declspec(dllimport) double PhongShaderUseTexture(double, double);
__declspec(dllimport) double PhongShaderSetMaxLights(double, double);
__declspec(dllimport) double BumpShaderCreate(void);
__declspec(dllimport) double BumpShaderSetDiffuseTexture(double, char*);
__declspec(dllimport) double BumpShaderSetNormalTexture(double, char*);
__declspec(dllimport) double BumpShaderSetHeightTexture(double, char*);
__declspec(dllimport) double BumpShaderSetMaxLights(double, double);
__declspec(dllimport) double BumpShaderUseParallax(double, double);
__declspec(dllimport) double BumpShaderSetParallaxOffset(double, double);
__declspec(dllimport) double BumpShaderSetShadowMap(double, double);
__declspec(dllimport) double BumpShaderSetShadowBlurRadius(double, double);
__declspec(dllimport) double BumpShaderUseAutoTangentSpace(double, double);

/* shadowmap.pas */
__declspec(dllimport) double ShadowMapCreate(double, double, double);
__declspec(dllimport) double ShadowMapUpdate(double);
__declspec(dllimport) double ShadowMapSetCamera(double, double);
__declspec(dllimport) double ShadowMapSetViewer(double, double);
__declspec(dllimport) double ShadowMapSetFBO(double, double);
__declspec(dllimport) double ShadowCameraCreate(double);
__declspec(dllimport) double ShadowCameraSetProjectionSize(double, double);
__declspec(dllimport) double ShadowCameraSetZClippingPlanes(double, double, double);

/* shadowplane.pas */
__declspec(dllimport) double ShadowplaneCreate(double, double, double, double, double, double, double, double, double);
__declspec(dllimport) double ShadowplaneSetLight(double, double);
__declspec(dllimport) double ShadowplaneSetObject(double, double);
__declspec(dllimport) double ShadowplaneSetOptions(double, double, double, double, double);

/* shadowvolume.pas */
__declspec(dllimport) double ShadowvolumeCreate(double);
__declspec(dllimport) double ShadowvolumeSetActive(double, double);
__declspec(dllimport) double ShadowvolumeAddLight(double, double);
__declspec(dllimport) double ShadowvolumeRemoveLight(double, double);
__declspec(dllimport) double ShadowvolumeAddOccluder(double, double);
__declspec(dllimport) double ShadowvolumeRemoveOccluder(double, double);
__declspec(dllimport) double ShadowvolumeSetDarkeningColor(double, double, double);
__declspec(dllimport) double ShadowvolumeSetMode(double, double);

/* skybox.pas */
__declspec(dllimport) double SkyboxCreate(double);
__declspec(dllimport) double SkyboxSetMaterial(double, double, char*);
__declspec(dllimport) double SkyboxSetClouds(double, double, double);
__declspec(dllimport) double SkyboxSetStyle(double, double);

/* skydome.pas */
__declspec(dllimport) double SkydomeCreate(double, double, double);
__declspec(dllimport) double SkydomeSetOptions(double, double, double);
__declspec(dllimport) double SkydomeSetDeepColor(double, double);
__declspec(dllimport) double SkydomeSetHazeColor(double, double);
__declspec(dllimport) double SkydomeSetNightColor(double, double);
__declspec(dllimport) double SkydomeSetSkyColor(double, double);
__declspec(dllimport) double SkydomeSetSunDawnColor(double, double);
__declspec(dllimport) double SkydomeSetSunZenithColor(double, double);
__declspec(dllimport) double SkydomeSetSunElevation(double, double);
__declspec(dllimport) double SkydomeSetTurbidity(double, double);
__declspec(dllimport) double SkydomeAddRandomStars(double, double, double);
__declspec(dllimport) double SkydomeAddStar(double, double, double, double, double);
__declspec(dllimport) double SkydomeClearStars(double);
__declspec(dllimport) double SkydomeTwinkleStars(double, double);

/* sprite.pas */
__declspec(dllimport) double HUDSpriteCreate(char*, double, double, double);
__declspec(dllimport) double HUDSpriteGetMouseOver(double, double);
__declspec(dllimport) double HUDSpriteXTiles(double, double);
__declspec(dllimport) double HUDSpriteYTiles(double, double);
__declspec(dllimport) double SpriteCreate(char*, double, double, double);
__declspec(dllimport) double SpriteSetSize(double, double, double);
__declspec(dllimport) double SpriteGetSize(double, double);
__declspec(dllimport) double SpriteScale(double, double, double);
__declspec(dllimport) double SpriteSetRotation(double, double);
__declspec(dllimport) double SpriteRotate(double, double);
__declspec(dllimport) double SpriteMirror(double, double, double);
__declspec(dllimport) double SpriteCreateEx(double, double, double, double, double, double, double);
__declspec(dllimport) double HUDSpriteCreateEx(double, double, double, double, double, double, double);
__declspec(dllimport) double SpriteSetBounds(double, double, double, double, double);
__declspec(dllimport) double SpriteSetBoundsUV(double, double, double, double, double);
__declspec(dllimport) double SpriteSetOrigin(double, double, double);

/* terrain.pas */
__declspec(dllimport) double BmpHDSCreate(char*);
__declspec(dllimport) double BmpHDSSetInfiniteWarp(double, double);
__declspec(dllimport) double BmpHDSInvert(double);
__declspec(dllimport) double BmpHDSCreateEmpty(double, double, double);
__declspec(dllimport) double BmpHDSSetHeight(double, double, double, double);
__declspec(dllimport) double BmpHDSGetHeight(double, double, double);
__declspec(dllimport) double BmpHDSSave(double, char*);
__declspec(dllimport) double TerrainCreate(double);
__declspec(dllimport) double TerrainSetHeightData(double, double);
__declspec(dllimport) double TerrainSetTileSize(double, double);
__declspec(dllimport) double TerrainSetTilesPerTexture(double, double);
__declspec(dllimport) double TerrainSetQualityDistance(double, double);
__declspec(dllimport) double TerrainSetQualityStyle(double, double);
__declspec(dllimport) double TerrainSetMaxCLodTriangles(double, double);
__declspec(dllimport) double TerrainSetCLodPrecision(double, double);
__declspec(dllimport) double TerrainSetOcclusionFrameSkip(double, double);
__declspec(dllimport) double TerrainSetOcclusionTesselate(double, double);
__declspec(dllimport) double TerrainGetHeightAtObjectPosition(double, double);
__declspec(dllimport) double TerrainGetLastTriCount(double);
__declspec(dllimport) double TerrainGetHDSPosition(double, double, double, double, double);

/* texture.pas */
__declspec(dllimport) double TextureExLoad(double, char*);
__declspec(dllimport) double TextureExSetFromMaterial(double, char*);
__declspec(dllimport) double TextureExGenerate(double, double, double);
__declspec(dllimport) double TextureExDelete(double);
__declspec(dllimport) double TextureExSetTextureScale(double, double, double);
__declspec(dllimport) double TextureExSetTextureOffset(double, double, double);
__declspec(dllimport) double TextureExEnable(double, double);

/* thorfx.pas */
__declspec(dllimport) double ThorFXManagerCreate(void);
__declspec(dllimport) double ThorFXSetColor(double, double, double, double, double, double, double);
__declspec(dllimport) double ThorFXEnableCore(double, double);
__declspec(dllimport) double ThorFXEnableGlow(double, double);
__declspec(dllimport) double ThorFXSetMaxParticles(double, double);
__declspec(dllimport) double ThorFXSetGlowSize(double, double);
__declspec(dllimport) double ThorFXSetVibrate(double, double);
__declspec(dllimport) double ThorFXSetWildness(double, double);
__declspec(dllimport) double ThorFXSetTarget(double, double, double, double);
__declspec(dllimport) double ThorFXCreate(double, double);

/* trail.pas */
__declspec(dllimport) double TrailCreate(double, double);
__declspec(dllimport) double TrailSetObject(double, double);
__declspec(dllimport) double TrailSetAlpha(double, double, double);
__declspec(dllimport) double TrailSetLimits(double, double, double);
__declspec(dllimport) double TrailSetMinDistance(double, double);
__declspec(dllimport) double TrailSetUVScale(double, double);
__declspec(dllimport) double TrailSetMarkStyle(double, double);
__declspec(dllimport) double TrailSetMarkWidth(double, double);
__declspec(dllimport) double TrailSetEnabled(double, double);
__declspec(dllimport) double TrailClearMarks(double);

/* tree.pas */
__declspec(dllimport) double TreeCreate(double);
__declspec(dllimport) double TreeSetMaterials(double);
__declspec(dllimport) double TreeSetBranchFacets(double, double);
__declspec(dllimport) double TreeBuildMesh(double, double);
__declspec(dllimport) double TreeSetBranchNoise(double, double);
__declspec(dllimport) double TreeSetBranchAngle(double, double);
__declspec(dllimport) double TreeSetBranchSize(double, double);
__declspec(dllimport) double TreeSetBranchRadius(double, double);
__declspec(dllimport) double TreeSetBranchTwist(double, double);
__declspec(dllimport) double TreeSetDepth(double, double);
__declspec(dllimport) double TreeSetLeafSize(double, double);
__declspec(dllimport) double TreeSetLeafThreshold(double, double);
__declspec(dllimport) double TreeSetSeed(double, double);

/* verlet.pas */
__declspec(dllimport) double VerletWorldCreate(double, double, double);
__declspec(dllimport) double VerletWorldCreateOctree(double, double, double, double, double, double, double, double, double);
__declspec(dllimport) double VerletGetNodeCount(double);
__declspec(dllimport) double VerletWorldGravityCreate(double, double, double, double);
__declspec(dllimport) double VerletWorldGravitySetDirection(double, double, double, double);
__declspec(dllimport) double VerletWorldUpdate(double, double);
__declspec(dllimport) double EdgeDetectorCreate(double, double);
__declspec(dllimport) double EdgeDetectorSetWeldDistance(double, double);
__declspec(dllimport) double VerletConstraintFloorCreate(double, double, double);
__declspec(dllimport) double VerletConstraintFloorSetNormal(double, double, double, double);
__declspec(dllimport) double VerletConstraintFloorSetObjectLocations(double, double);
__declspec(dllimport) double VerletConstraintSphereCreate(double, double);
__declspec(dllimport) double VerletConstraintCylinderCreate(double, double);
__declspec(dllimport) double VerletConstraintCylinderSetAxis(double, double, double, double);
__declspec(dllimport) double VerletConstraintCubeCreate(double, double, double, double);
__declspec(dllimport) double VerletConstraintCubeCreateSetCube(double, double);
__declspec(dllimport) double VerletConstraintCubeSetDirection(double, double, double, double);
__declspec(dllimport) double VerletConstraintCapsuleCreate(double, double, double);
__declspec(dllimport) double VerletConstraintCapsuleSetAxis(double, double, double, double);
__declspec(dllimport) double VerletConstraintSetPosition(double, double, double, double);
__declspec(dllimport) double VerletConstraintSetFrictionRatio(double, double);
__declspec(dllimport) double VerletConstraintSetEnabled(double, double);
__declspec(dllimport) double VerletNodeNailedDown(double, double, double);
__declspec(dllimport) double VerletNodeSetPosition(double, double, double, double, double);
__declspec(dllimport) double VerletNodeSetRadius(double, double, double);
__declspec(dllimport) double VerletNodeSetFriction(double, double, double);
__declspec(dllimport) double VerletNodeSetWeight(double, double, double);
__declspec(dllimport) double VerletNodeApplyFriction(double, double, double, double, double, double, double);
__declspec(dllimport) double VerletAirResistanceCreate(double, double, double);
__declspec(dllimport) double VerletAirResistanceSetWindDirection(double, double, double, double);
__declspec(dllimport) double VerletAirResistanceSetWindMagnitude(double, double);
__declspec(dllimport) double VerletAirResistanceSetWindChaos(double, double);
__declspec(dllimport) double VerletConstraintGetCount(double);
__declspec(dllimport) double VerletConstraintSetSlack(double, double, double);
__declspec(dllimport) double VerletWorldSetSimTime(double, double);
__declspec(dllimport) double VerletWorldSetMaxDeltaTime(double, double);

/* video.pas */
__declspec(dllimport) double VideoCreate(double);
__declspec(dllimport) double VideoIsPlaying(double);
__declspec(dllimport) double VideoPlay(double, char*);
__declspec(dllimport) double VideoClose(double);

/* viewer.pas */
__declspec(dllimport) double ViewerCreate(double, double, double, double, double);
__declspec(dllimport) double ViewerSetCamera(double, double);
__declspec(dllimport) double ViewerEnableVSync(double, double);
__declspec(dllimport) double ViewerRender(double);
__declspec(dllimport) double ViewerBeginRender(double);
__declspec(dllimport) double ViewerClear(double, double, double, double);
__declspec(dllimport) double ViewerRenderObject(double, double);
__declspec(dllimport) double ViewerEndRender(double);
__declspec(dllimport) double ViewerRenderToFile(double, char*);
__declspec(dllimport) double ViewerResize(double, double, double, double, double);
__declspec(dllimport) double ViewerSetVisible(double, double);
__declspec(dllimport) double ViewerGetPixelColor(double, double, double);
__declspec(dllimport) double ViewerGetPixelDepth(double, double, double);
__declspec(dllimport) double ViewerSetLighting(double, double);
__declspec(dllimport) double ViewerSetBackgroundColor(double, double);
__declspec(dllimport) double ViewerSetAmbientColor(double, double);
__declspec(dllimport) double ViewerEnableFog(double, double);
__declspec(dllimport) double ViewerSetFogColor(double, double);
__declspec(dllimport) double ViewerSetFogDistance(double, double, double);
__declspec(dllimport) double ViewerScreenToWorld(double, double, double, double);
__declspec(dllimport) double ViewerWorldToScreen(double, double, double, double, double);
__declspec(dllimport) double ViewerCopyToTexture(double, char*);
__declspec(dllimport) double ViewerGetPickedObject(double, double, double);
__declspec(dllimport) double ViewerGetPickedObjectsList(double, double, double, double, double, double, double);
__declspec(dllimport) double ViewerScreenToVector(double, double, double, double);
__declspec(dllimport) double ViewerVectorToScreen(double, double, double, double, double);
__declspec(dllimport) double ViewerPixelToDistance(double, double, double);
__declspec(dllimport) double ViewerSetAntiAliasing(double, double);
__declspec(dllimport) double ViewerGetGLSLSupported(double);
__declspec(dllimport) double ViewerGetFBOSupported(double);
__declspec(dllimport) double ViewerGetVBOSupported(double);
__declspec(dllimport) double ViewerGetSize(double, double);
__declspec(dllimport) double ViewerGetPosition(double, double);
__declspec(dllimport) double ViewerIsOpenGLExtensionSupported(double, char*);
__declspec(dllimport) double ViewerGetFramesPerSecond(double);
__declspec(dllimport) double ViewerResetPerformanceMonitor(double);
__declspec(dllimport) double ViewerPixelRayToWorld(double, double, double, double);
__declspec(dllimport) double ViewerShadeModel(double, double);
__declspec(dllimport) double ViewerSetAutoRender(double, double);

/* water.pas */
__declspec(dllimport) double WaterCreate(double);
__declspec(dllimport) double WaterCreateRandomRipple(double);
__declspec(dllimport) double WaterCreateRippleAtGridPosition(double, double, double);
__declspec(dllimport) double WaterCreateRippleAtWorldPosition(double, double, double, double);
__declspec(dllimport) double WaterCreateRippleAtObjectPosition(double, double);
__declspec(dllimport) double WaterSetMask(double, char*);
__declspec(dllimport) double WaterSetActive(double, double);
__declspec(dllimport) double WaterReset(double);
__declspec(dllimport) double WaterSetRainTimeInterval(double, double);
__declspec(dllimport) double WaterSetRainForce(double, double);
__declspec(dllimport) double WaterSetViscosity(double, double);
__declspec(dllimport) double WaterSetElastic(double, double);
__declspec(dllimport) double WaterSetResolution(double, double);
__declspec(dllimport) double WaterSetLinearWaveHeight(double, double);
__declspec(dllimport) double WaterSetLinearWaveFrequency(double, double);

/* window.pas */
__declspec(dllimport) double WindowCreate(double, double, double, double, double);
__declspec(dllimport) double WindowSetBackgroundColor(double, double);
__declspec(dllimport) double WindowCenter(double);
__declspec(dllimport) double WindowResize(double, double, double, double, double);
__declspec(dllimport) double WindowGetPosition(double, double);
__declspec(dllimport) double WindowGetSize(double, double);
__declspec(dllimport) double WindowGetHandle(double);
__declspec(dllimport) double WindowSetTitle(double, char*);
__declspec(dllimport) double WindowDestroy(double);
__declspec(dllimport) double WindowIsShowing(double);
__declspec(dllimport) double WindowSetIcon(double, char*);
__declspec(dllimport) double WindowDispatch(void);
__declspec(dllimport) double WindowIsActive(double);

/* windowcontrol.pas */
__declspec(dllimport) double WindowControlCreate(double, double, double, double, double);
__declspec(dllimport) double WindowControlSetBackgroundColor(double, double);
__declspec(dllimport) double WindowControlFree(double);

#ifdef __cplusplus
}
#endif

#endif /* XTREME3D_H */
