/* Xtreme3D header */

#ifndef XTREME3D_H
#define XTREME3D_H

#ifdef __cplusplus
extern "C" {
#endif

/* actor.pas */
double ActorCreate(char*, double, double);
double ActorCopy(double, double);
double ActorSetAnimationRange(double, double, double);
double ActorGetCurrentFrame(double);
double ActorSwitchToAnimation(double, double, double);
double ActorSwitchToAnimationName(double, char*, double);
double ActorSynchronize(double, double);
double ActorSetInterval(double, double);
double ActorSetAnimationMode(double, double);
double ActorSetFrameInterpolation(double, double);
double ActorAddObject(double, char*);
char* ActorGetCurrentAnimation(double);
double ActorGetFrameCount(double);
double ActorGetBoneCount(double);
double ActorGetBoneByName(double, char*);
double ActorGetBoneRotation(double, double, double);
double ActorGetBonePosition(double, double, double);
double ActorBoneExportMatrix(double, double, double);
double ActorMakeSkeletalTranslationStatic(double, double);
double ActorMakeSkeletalRotationDelta(double, double);
double ActorShowSkeleton(double, double);
double AnimationBlenderCreate(void);
double AnimationBlenderSetActor(double, double);
double AnimationBlenderSetAnimation(double, char*);
double AnimationBlenderSetRatio(double, double);
double ActorLoadQ3TagList(char*);
double ActorQ3TagExportMatrix(double, double, char*, double);
double ActorLoadQ3Animations(double, char*, char*);
double ActorMeshObjectsCount(double);
double ActorFaceGroupsCount(double, double);
char* ActorFaceGroupGetMaterialName(double, double, double);
double ActorFaceGroupSetMaterial(double, double, double, char*);
double ActorMoveBone(double, double, double, double, double);
double ActorRotateBone(double, double, double, double, double);
double ActorMeshSetVisible(double, double, double);
char* ActorGetAnimationName(double, double);
double ActorGetAnimationCount(double);
double ActorAnimationDestroy(double, double);
double ActorAnimationNextFrame(double);
double ActorAnimationPrevFrame(double);
double ActorSetFrame(double, double);
double ActorTriangleCount(double);
double ActorSetReference(double, double);

/* blur.pas */
double BlurCreate(double, double);
double BlurSetPreset(double, double);
double BlurSetOptions(double, double, double, double, double, double);
double BlurSetResolution(double, double);
double BlurSetColor(double, double);
double BlurSetBlendingMode(double, double);

/* camera.pas */
double CameraCreate(double);
double CameraSetStyle(double, double);
double CameraSetFocal(double, double);
double CameraSetSceneScale(double, double);
double CameraScaleScene(double, double);
double CameraSetViewDepth(double, double);
double CameraSetTargetObject(double, double);
double CameraMoveAroundTarget(double, double, double);
double CameraSetDistanceToTarget(double, double);
double CameraGetDistanceToTarget(double);
double CameraCopyToTexture(double, char*, double, double);
double CameraGetNearPlane(double);
double CameraSetNearPlaneBias(double, double);
double CameraAbsoluteVectorToTarget(double, double);
double CameraAbsoluteRightVectorToTarget(double, double);
double CameraAbsoluteUpVectorToTarget(double, double);
double CameraZoomAll(double, double);
double CameraScreenDeltaToVector(double, double, double, double, double, double, double, double);
double CameraScreenDeltaToVectorXY(double, double, double, double, double);
double CameraScreenDeltaToVectorXZ(double, double, double, double, double);
double CameraScreenDeltaToVectorYZ(double, double, double, double, double);
double CameraAbsoluteEyeSpaceVector(double, double, double, double, double);
double CameraSetAutoLeveling(double, double);
double CameraMoveInEyeSpace(double, double, double, double);
double CameraMoveTargetInEyeSpace(double, double, double, double);
double CameraPointInFront(double, double, double, double);
double CameraGetFieldOfView(double, double);

/* clipplane.pas */
double ClipPlaneCreate(double);
double ClipPlaneEnable(double, double);
double ClipPlaneSetPlane(double, double, double, double, double, double, double);

/* color.pas */
double MakeColorRGB(double, double, double);
double MakeColorRGBFloat(double, double, double);

/* dce.pas */
double DceManagerCreate(void);
double DceManagerStep(double, double);
double DceManagerSetGravity(double, double);
double DceManagerSetWorldDirection(double, double, double, double);
double DceManagerSetWorldScale(double, double);
double DceManagerSetMovementScale(double, double);
double DceManagerSetLayers(double, double);
double DceManagerSetManualStep(double, double);
double DceDynamicSetManager(double, double);
double DceDynamicSetActive(double, double);
double DceDynamicIsActive(double);
double DceDynamicSetUseGravity(double, double);
double DceDynamicSetLayer(double, double);
double DceDynamicGetLayer(double);
double DceDynamicSetSolid(double, double);
double DceDynamicSetFriction(double, double);
double DceDynamicSetBounce(double, double);
double DceDynamicSetSize(double, double, double, double);
double DceDynamicSetSlideOrBounce(double, double);
double DceDynamicApplyAcceleration(double, double, double, double);
double DceDynamicApplyAbsAcceleration(double, double, double, double);
double DceDynamicStopAcceleration(double);
double DceDynamicStopAbsAcceleration(double);
double DceDynamicJump(double, double, double);
double DceDynamicMove(double, double, double, double, double);
double DceDynamicMoveTo(double, double, double, double, double);
double DceDynamicSetVelocity(double, double, double, double);
double DceDynamicInGround(double);
double DceDynamicSetMaxRecursionDepth(double, double);
double DceStaticSetManager(double, double);
double DceStaticSetActive(double, double);
double DceStaticSetShape(double, double);
double DceStaticSetLayer(double, double);
double DceStaticSetSize(double, double, double, double);
double DceStaticSetSolid(double, double);
double DceStaticSetFriction(double, double);
double DceStaticSetBounceFactor(double, double);
double DceDynamicGetVelocity(double, double);
double DceDynamicSetAbsVelocity(double, double, double, double);
double DceDynamicGetAbsVelocity(double, double);
double DceDynamicApplyImpulse(double, double, double, double);
double DceDynamicApplyAbsImpulse(double, double, double, double);

/* dummycube.pas */
double DummycubeCreate(double);
double DummycubeAmalgamate(double, double);
double DummycubeSetCameraMode(double, double);
double DummycubeSetVisible(double, double);
double DummycubeSetEdgeColor(double, double);
double DummycubeSetCubeSize(double, double);

/* engine.pas */
double EngineCreate(void);
double EngineDestroy(void);
double EngineSetObjectsSorting(double);
double EngineSetCulling(double);
double EngineUpdate(double);
double EngineSaveScene(char*);
double EngineLoadScene(char*);
double EngineRootObject(void);
double EngineShowLoadingErrors(double);
double EngineSetMaxLights(double);
double EngineGetTimeStep(void);
double EngineGetLastRaycastPosition(double);
double EngineGetLastRaycastNormal(double);
double PointerToReal(char*);

/* fbo.pas */
double FBOCreate(double, double, double);
double FBOSetActive(double, double);
double FBOSetAspect(double, double);
double FBOSetPickableTarget(double, double);
double FBOSetSize(double, double, double);
double FBOSetCamera(double, double);
double FBOSetRootObject(double, double);
double FBOSetBackgroundColor(double, double);
double FBOSetEnabledRenderBuffers(double, double, double);
double FBOSetSceneScaleFactor(double, double);
double FBOSetTargetVisibility(double, double);
double FBOSetMaterialLibrary(double, double);
double FBOSetColorTextureName(double, char*);
double FBOSetDepthTextureName(double, char*);
double FBOSetClearOptions(double, double, double, double, double);
double FBOSetStencilPrecision(double, double);
double FBOSetShadowMapMode(double, double);

/* firefx.pas */
double FireFXManagerCreate(void);
double FireFXCreate(double, double);
double FireFXSetColor(double, double, double, double, double);
double FireFXSetMaxParticles(double, double);
double FireFXSetParticleSize(double, double);
double FireFXSetDensity(double, double);
double FireFXSetEvaporation(double, double);
double FireFXSetCrown(double, double);
double FireFXSetLife(double, double);
double FireFXSetBurst(double, double);
double FireFXSetRadius(double, double);
double FireFXExplosion(double, double, double, double);
double FireFXRingExplosion(double, double, double, double, double, double, double, double, double, double);

/* fonttext.pas */
double BmpfontCreate(double, double, double, double, double, double, double, double);
double BmpfontLoad(double, char*);
double WindowsBitmapfontCreate(char*, double, double, double);
double HUDTextCreate(double, char*, double);
double HUDTextSetRotation(double, double);
double HUDTextSetFont(double, double);
double HUDTextSetColor(double, double, double);
double HUDTextSetText(double, char*);
double FlatTextCreate(double, char*, double);
double FlatTextSetFont(double, double);
double FlatTextSetColor(double, double, double);
double FlatTextSetText(double, char*);
double SpaceTextCreate(double, char*, double, double);
double SpaceTextSetExtrusion(double, double);
double SpaceTextSetFont(double, double);
double SpaceTextSetText(double, char*);
double TTFontCreate(char*, double);
double TTFontSetLineGap(double, double);

/* fps.pas */
double FpsManagerCreate(void);
double FpsManagerSetNavigator(double, double);
double FpsManagerSetMovementScale(double, double);
double FpsManagerAddMap(double, double);
double FpsManagerRemoveMap(double, double);
double FpsManagerMapSetCollisionGroup(double, double, double);
double FpsSetManager(double, double);
double FpsSetCollisionGroup(double, double);
double FpsSetSphereRadius(double, double);
double FpsSetGravity(double, double);
double FpsMove(double, double);
double FpsStrafe(double, double);
double FpsLift(double, double);
double FpsGetVelocity(double, double);

/* freeform.pas */
double FreeformCreate(char*, double, double, double);
double FreeformGenTangents(double);
double FreeformMeshObjectsCount(double);
double FreeformMeshSetVisible(double, double, double);
double FreeformMeshSetSecondCoords(double, double, double, double);
double FreeformMeshTriangleCount(double, double);
char* FreeformMeshObjectGetName(double, double);
double FreeformMeshObjectSetName(double, double, char*);
double FreeformMeshObjectDestroy(double, double);
double FreeformMeshFaceGroupsCount(double, double);
double FreeformMeshFaceGroupTriangleCount(double, double, double);
double FreeformCreateExplosionFX(double, double);
double FreeformExplosionFXReset(double);
double FreeformExplosionFXEnable(double, double);
double FreeformExplosionFXSetSpeed(double, double);
double FreeformSphereSweepIntersect(double, double, double, double);
double FreeformPointInMesh(double, double, double, double);
double FreeformMeshSetMaterial(double, double, char*);
double FreeformUseMeshMaterials(double, double);
double FreeformToFreeforms(double, double);
double FreeformMeshFaceGroupSetMaterial(double, double, double, char*);
char* FreeformMeshFaceGroupGetMaterial(double, double, double);
double FreeformCreateEmpty(double, double, double);
double FreeformAddMesh(double);
double FreeformMeshAddFaceGroup(double, double);
double FreeformMeshAddVertex(double, double, double, double, double);
double FreeformMeshAddNormal(double, double, double, double, double);
double FreeformMeshAddTexCoord(double, double, double, double);
double FreeformMeshAddSecondTexCoord(double, double, double, double);
double FreeformMeshAddTangent(double, double, double, double, double);
double FreeformMeshAddBinormal(double, double, double, double, double);
double FreeformMeshFaceGroupAddTriangle(double, double, double, double, double, double);
double FreeformMeshGenNormals(double, double);
double FreeformMeshGenTangents(double, double);
double FreeformMeshVerticesCount(double, double);
double FreeformMeshTranslate(double, double, double, double, double);
double FreeformMeshRotate(double, double, double, double, double);
double FreeformMeshScale(double, double, double, double, double);
double FreeformSave(double, char*);
double FreeformMeshGetVertex(double, double, double, double);
double FreeformMeshGetNormal(double, double, double, double);
double FreeformMeshGetTexCoord(double, double, double, double);
double FreeformMeshGetSecondTexCoord(double, double, double, double);
double FreeformMeshGetTangent(double, double, double, double);
double FreeformMeshGetBinormal(double, double, double, double);
double FreeformMeshFaceGroupGetIndex(double, double, double, double);
double FreeformMeshSetVertex(double, double, double, double, double, double);
double FreeformMeshSetNormal(double, double, double, double, double, double);
double FreeformMeshSetTexCoord(double, double, double, double, double);
double FreeformMeshSetSecondTexCoord(double, double, double, double, double);
double FreeformMeshSetTangent(double, double, double, double, double, double);
double FreeformMeshSetBinormal(double, double, double, double, double, double);
double FreeformMeshFaceGroupSetIndex(double, double, double, double, double);
double FreeformBuildOctree(double);
double FreeformMeshFaceGroupGetLightmapIndex(double, double, double);
double FreeformMeshFaceGroupSetLightmapIndex(double, double, double, double);
double FreeformSetMaterialLibraries(double, double, double);
double BaseMeshBuildSilhouetteConnectivityData(double);

/* grid.pas */
double GridCreate(double, double, double, double, double);
double GridSetLineStyle(double, double);
double GridSetLineSmoothing(double, double);
double GridSetParts(double, double);
double GridSetColor(double, double, double);
double GridSetSize(double, double);
double GridSetPattern(double, double);
double GridSetTile(double, double, double, double);
double GridSetStep(double, double);

/* hudshapes.pas */
double HUDShapeRectangleCreate(double, double, double);
double HUDShapeCircleCreate(double, double, double, double, double);
double HUDShapeLineCreate(double, double, double, double, double);
double HUDShapeMeshCreate(double);
double HUDShapeSetRotation(double, double);
double HUDShapeRotate(double, double);
double HUDShapeSetColor(double, double, double);
double HUDShapeSetOrigin(double, double, double);
double HUDShapeSetSize(double, double, double);
double HUDShapeScale(double, double, double);
double HUDShapeCircleSetRadius(double, double);
double HUDShapeCircleSetSlices(double, double);
double HUDShapeCircleSetAngles(double, double, double);
double HUDShapeLineSetPoints(double, double, double, double, double);
double HUDShapeLineSetWidth(double, double);
double HUDShapeMeshAddVertex(double, double, double, double, double);
double HUDShapeMeshAddTriangle(double, double, double, double);
double HUDShapeMeshSetVertex(double, double, double, double);
double HUDShapeMeshSetTexCoord(double, double, double, double);

/* input.pas */
double MouseGetPositionX(void);
double MouseGetPositionY(void);
double MouseSetPosition(double, double);
double MouseShowCursor(double);
double KeyIsPressed(double);
double MouseIsPressed(double);

/* kraft.pas */
double KraftCreate(void);
double KraftStep(double, double);
double KraftGetRayHitPosition(double);
double KraftGetRayHitNormal(double);
double KraftCreateRigidBody(double, double);
double KraftRigidBodyFinish(double);
double KraftRigidBodySetGravity(double, double, double, double, double);
double KraftRigidBodySetPosition(double, double, double, double);
double KraftRigidBodyGetPosition(double, double);
double KraftRigidBodySetLinearVelocity(double, double, double, double);
double KraftRigidBodyGetLinearVelocity(double, double);
double KraftRigidBodySetRotation(double, double, double, double);
double KraftRigidBodyGetDirection(double, double);
double KraftRigidBodyGetUp(double, double);
double KraftRigidBodyGetRight(double, double);
double KraftRigidBodySetAngularVelocity(double, double, double, double);
double KraftRigidBodyGetAngularVelocity(double, double);
double KraftRigidBodyAddForce(double, double, double, double);
double KraftRigidBodyAddForceAtPos(double, double, double, double, double, double, double);
double KraftRigidBodyAddRelForce(double, double, double, double);
double KraftRayCast(double, double, double, double, double, double, double, double);
double KraftObjectSetRigidBody(double, double);
double KraftCreateShapeSphere(double, double);
double KraftCreateShapeBox(double, double, double, double);
double KraftCreateShapePlane(double, double, double, double, double);
double KraftCreateShapeCapsule(double, double, double);
double KraftCreateShapeMesh(double, double);
double KraftShapeSetDensity(double, double);
double KraftShapeSetFriction(double, double);
double KraftShapeSetRestitution(double, double);
double KraftShapeSetPosition(double, double, double, double);
double KraftShapeGetPosition(double, double);
double KraftShapeSetRayCastable(double, double);
double KraftCreateJointDistance(double, double);
double KraftCreateJointRope(double, double, double);
double KraftCreateJointBallSocket(double, double);
double KraftCreateJointFixed(double, double);
double KraftCreateJointHinge(double, double);
double KraftJointSetAnchor1(double, double, double, double);
double KraftJointSetAnchor2(double, double, double, double);
double KraftJointSetHingeAxis1(double, double, double, double);
double KraftJointSetHingeAxis2(double, double, double, double);

/* lensflare.pas */
double LensflareCreate(double);
double LensflareSetSize(double, double);
double LensflareSetSeed(double, double);
double LensflareSetSqueeze(double, double);
double LensflareSetStreaks(double, double);
double LensflareSetStreakWidth(double, double);
double LensflareSetSecs(double, double);
double LensflareSetResolution(double, double);
double LensflareSetElements(double, double, double, double, double, double);
double LensflareSetGradients(double, double, double, double, double, double);

/* light.pas */
double LightCreate(double, double);
double LightSetAmbientColor(double, double);
double LightSetDiffuseColor(double, double);
double LightSetSpecularColor(double, double);
double LightSetAttenuation(double, double, double, double);
double LightSetShining(double, double);
double LightSetSpotCutoff(double, double);
double LightSetSpotExponent(double, double);
double LightSetSpotDirection(double, double, double, double);
double LightSetStyle(double, double);
double LightGetColor(double, double);
double LightGetAttenuation(double, double);
double LightGetShining(double);

/* lightfx.pas */
double LightFXCreate(double);

/* lines.pas */
double LinesCreate(double);
double LinesAddNode(double, double, double, double);
double LinesDeleteNode(double, double);
double LinesSetNode(double, double, double, double, double);
double LinesSetSize(double, double, double);
double LinesSetSplineMode(double, double);
double LinesSetNodesAspect(double, double);
double LinesSetDivision(double, double);

/* logger.pas */
double LoggerCreate(char*, double);
double LoggerEnable(double, double);
double LoggerLog(double, double, char*);

/* material.pas */
double MaterialLibraryCreate(void);
double MaterialLibraryActivate(double);
double MaterialLibrarySetTexturePaths(double, char*);
double MaterialLibraryClear(double);
double MaterialLibraryDeleteUnused(double);
double MaterialLibraryHasMaterial(double, char*);
double MaterialLibraryLoadScript(double, char*);
double MaterialLibraryGetTextureByName(double, char*);
double MaterialCreate(char*, char*);
double MaterialDestroy(char*);
double MaterialAddCubeMap(char*);
double MaterialCubeMapLoadImage(char*, char*, double);
double MaterialCubeMapGenerate(char*, double);
double MaterialCubeMapFromScene(char*, double, double, double);
double MaterialSetName(char*, char*);
double MaterialSetShininess(char*, double);
double MaterialSetAmbientColor(char*, double, double);
double MaterialSetDiffuseColor(char*, double, double);
double MaterialSetSpecularColor(char*, double, double);
double MaterialSetEmissionColor(char*, double, double);
double MaterialGetColor(char*, double);
double MaterialGetAlpha(char*, double);
double MaterialSetBlendingMode(char*, double);
double MaterialSetTextureMode(char*, double);
double MaterialSetTextureMappingMode(char*, double);
double MaterialSetPolygonMode(char*, double);
double MaterialSetTextureImageAlpha(char*, double);
double MaterialSetTextureScale(char*, double, double);
double MaterialSetTextureOffset(char*, double, double);
double MaterialSetTextureFilter(char*, double, double);
double MaterialEnableTexture(char*, double);
double MaterialGetCount(void);
char* MaterialGetName(double);
double MaterialSetFaceCulling(char*, double);
double MaterialSetSecondTexture(char*, char*);
double MaterialSetTextureFormat(char*, double);
double MaterialSetTextureFormatEx(char*, double);
double MaterialSetTextureCompression(char*, double);
double MaterialTextureRequiredMemory(char*);
double MaterialSetFilteringQuality(char*, double);
double MaterialSetShader(char*, double);
double MaterialSaveTexture(char*, char*);
double MaterialSetOptions(char*, double, double);
double MaterialSetTextureWrap(char*, double);
double MaterialSetTextureWrapS(char*, double);
double MaterialSetTextureWrapT(char*, double);
double MaterialSetTextureWrapR(char*, double);
double MaterialSetTextureBorderColor(char*, double);
double MaterialGenTexture(char*, double, double);
double MaterialSetTexture(char*, char*);
double MaterialGetTextureWidth(char*);
double MaterialGetTextureHeight(char*);
double MaterialLoadTexture(char*, char*);
double MaterialAddTextureEx(char*, double);
double MaterialGetTextureEx(char*, double);
double MaterialTextureExClear(char*);
double MaterialHasTextureEx(char*, double);
double MaterialNoiseCreate(char*);
double MaterialNoiseSetDimensions(char*, double, double);
double MaterialNoiseAnimate(char*, double);
double MaterialNoiseSetMinCut(char*, double);
double MaterialNoiseSetSharpness(char*, double);
double MaterialNoiseSetSeamless(char*, double);
double MaterialNoiseRandomSeed(char*, double);
double MaterialSetDepthWrite(char*, double);
double MaterialSetDepthTest(char*, double);
char* MaterialGetNameFromLibrary(double, double);
double MaterialSetTextureCompareMode(char*, double);
double MaterialSetTextureDepthCompareFunc(char*, double);

/* memviewer.pas */
double MemoryViewerCreate(double, double);
double MemoryViewerSetCamera(double, double);
double MemoryViewerRender(double);
double MemoryViewerSetViewport(double, double, double, double, double);
double MemoryViewerCopyToTexture(double, char*);

/* mirror.pas */
double MirrorCreate(double, double);
double MirrorSetObject(double, double);
double MirrorSetOptions(double, double, double, double, double);
double MirrorSetShape(double, double);
double MirrorSetDiskOptions(double, double, double);

/* movement.pas */
double MovementCreate(double);
double MovementStart(double);
double MovementStop(double);
double MovementAutoStartNextPath(double, double);
double MovementAddPath(double);
double MovementSetActivePath(double, double);
double MovementPathSetSplineMode(double, double);
double MovementPathAddNode(double);
double MovementPathNodeSetPosition(double, double, double, double);
double MovementPathNodeSetRotation(double, double, double, double);
double MovementPathNodeSetSpeed(double, double);
double MovementPathShow(double, double);
double MovementPathSetLoop(double, double);
double MovementPathDeleteNode(double, double);

/* navigator.pas */
double NavigatorCreate(void);
double NavigatorSetObject(double, double);
double NavigatorSetUseVirtualUp(double, double);
double NavigatorSetVirtualUp(double, double, double, double);
double NavigatorTurnHorizontal(double, double);
double NavigatorTurnVertical(double, double);
double NavigatorMoveForward(double, double);
double NavigatorStrafeHorizontal(double, double);
double NavigatorStrafeVertical(double, double);
double NavigatorStraighten(double);
double NavigatorFlyForward(double, double);
double NavigatorMoveUpWhenMovingForward(double, double);
double NavigatorInvertHorizontalWhenUpsideDown(double, double);
double NavigatorSetAngleLock(double, double);
double NavigatorSetAngles(double, double, double);

/* object.pas */
double ObjectHide(double);
double ObjectShow(double);
double ObjectIsVisible(double);
double ObjectCopy(double, double);
double ObjectDestroy(double);
double ObjectDestroyChildren(double);
double ObjectSetPosition(double, double, double, double);
double ObjectGetPosition(double, double);
double ObjectGetAbsolutePosition(double, double);
double ObjectSetPositionOfObject(double, double);
double ObjectAlignWithObject(double, double);
double ObjectSetPositionX(double, double);
double ObjectSetPositionY(double, double);
double ObjectSetPositionZ(double, double);
double ObjectGetPositionX(double);
double ObjectGetPositionY(double);
double ObjectGetPositionZ(double);
double ObjectSetAbsolutePosition(double, double, double, double);
double ObjectSetDirection(double, double, double, double);
double ObjectGetDirection(double, double);
double ObjectSetAbsoluteDirection(double, double, double, double);
double ObjectGetAbsoluteDirection(double, double);
double ObjectGetPitch(double);
double ObjectGetTurn(double);
double ObjectGetRoll(double);
double ObjectSetRotation(double, double, double, double);
double ObjectMove(double, double);
double ObjectLift(double, double);
double ObjectStrafe(double, double);
double ObjectTranslate(double, double, double, double);
double ObjectRotate(double, double, double, double);
double ObjectScale(double, double, double, double);
double ObjectSetScale(double, double, double, double);
double ObjectGetScale(double, double);
double ObjectSetUpVector(double, double, double, double);
double ObjectPointToObject(double, double);
double ObjectShowAxes(double, double);
double ObjectGetGroundHeight(double, double);
double ObjectSceneRaycast(double, double);
double ObjectRaycast(double, double);
double ObjectSetMaterial(double, char*);
char* ObjectGetMaterial(double);
double ObjectGetDistance(double, double);
double ObjectCheckCubeVsCube(double, double);
double ObjectCheckSphereVsSphere(double, double);
double ObjectCheckSphereVsCube(double, double);
double ObjectCheckCubeVsFace(double, double);
double ObjectCheckFaceVsFace(double, double);
double ObjectIsPointInObject(double, double, double, double);
double ObjectSetCulling(double, double);
double ObjectSetName(double, char*);
char* ObjectGetName(double);
char* ObjectGetClassName(double);
double ObjectSetTag(double, double);
double ObjectGetTag(double);
double ObjectGetParent(double);
double ObjectGetChildCount(double);
double ObjectGetChild(double, double);
double ObjectGetIndex(double);
double ObjectFindChild(double, char*);
double ObjectGetBoundingSphereRadius(double);
double ObjectGetAbsoluteUp(double, double);
double ObjectSetAbsoluteUp(double, double, double, double);
double ObjectGetAbsoluteRight(double, double);
double ObjectGetAbsoluteXVector(double, double);
double ObjectGetAbsoluteYVector(double, double);
double ObjectGetAbsoluteZVector(double, double);
double ObjectGetRight(double, double);
double ObjectMoveChildUp(double, double);
double ObjectMoveChildDown(double, double);
double ObjectSetParent(double, double);
double ObjectRemoveChild(double, double, double);
double ObjectMoveObjectAround(double, double, double, double);
double ObjectPitch(double, double);
double ObjectTurn(double, double);
double ObjectRoll(double, double);
double ObjectGetUp(double, double);
double ObjectRotateAbsolute(double, double, double, double);
double ObjectRotateAbsoluteVector(double, double, double, double, double);
double ObjectSetMatrixColumn(double, double, double, double, double, double);
double ObjectExportMatrix(double, double);
double ObjectExportAbsoluteMatrix(double, double);
double ObjectInFrustum(double, double);
double ObjectFindByName(char*);
double ObjectIgnoreDepthBuffer(double, double);
double ObjectIsPicked(double, double, double, double);
double ObjectStructureChanged(double);
double ObjectClearStructureChanged(double);
double ObjectNotifyChange(double);

/* objecthash.pas */
double ObjectHashCreate(void);
double ObjectHashSetItem(double, double, double, double);
double ObjectHashGetItem(double);
double ObjectHashDeleteItem(double);
double ObjectHashGetItemCount(double);
double ObjectHashClear(double);
double ObjectHashDestroy(double);

/* objectlist.pas */
double ObjectListCreate(void);
double ObjectListAdd(double, double);
double ObjectListGetCount(double);

/* ode.pas */
double OdeManagerCreate(void);
double OdeManagerDestroy(void);
double OdeManagerStep(double);
double OdeManagerGetNumContactJoints(void);
double OdeManagerSetGravity(double, double, double);
double OdeManagerSetSolver(double);
double OdeManagerSetIterations(double);
double OdeManagerSetMaxContacts(double);
double OdeManagerSetVisible(double);
double OdeManagerSetGeomColor(double, double, double);
double OdeWorldSetAutoDisableFlag(double);
double OdeWorldSetAutoDisableLinearThreshold(double);
double OdeWorldSetAutoDisableAngularThreshold(double);
double OdeWorldSetAutoDisableSteps(double);
double OdeWorldSetAutoDisableTime(double);
double OdeStaticCreate(double);
double OdeDynamicCreate(double);
double OdeTerrainCreate(double);
double OdeDynamicCalculateMass(double);
double OdeDynamicCalibrateCenterOfMass(double);
double OdeDynamicAlignObject(double);
double OdeDynamicEnable(double, double);
double OdeDynamicSetAutoDisableFlag(double, double);
double OdeDynamicSetAutoDisableLinearThreshold(double, double);
double OdeDynamicSetAutoDisableAngularThreshold(double, double);
double OdeDynamicSetAutoDisableSteps(double, double);
double OdeDynamicSetAutoDisableTime(double, double);
double OdeDynamicAddForce(double, double, double, double);
double OdeDynamicAddForceAtPos(double, double, double, double, double, double, double);
double OdeDynamicAddForceAtRelPos(double, double, double, double, double, double, double);
double OdeDynamicAddRelForce(double, double, double, double);
double OdeDynamicAddRelForceAtPos(double, double, double, double, double, double, double);
double OdeDynamicAddRelForceAtRelPos(double, double, double, double, double, double, double);
double OdeDynamicAddTorque(double, double, double, double);
double OdeDynamicAddRelTorque(double, double, double, double);
double OdeAddBox(double, double, double, double, double, double, double);
double OdeAddSphere(double, double, double, double, double);
double OdeAddPlane(double);
double OdeAddCylinder(double, double, double, double, double, double);
double OdeAddCapsule(double, double, double, double, double, double);
double OdeAddTriMesh(double, double);
double OdeElementSetDensity(double, double);
double OdeSurfaceEnableRollingFrictionCoeff(double, double);
double OdeSurfaceSetRollingFrictionCoeff(double, double);
double OdeSurfaceSetMode(double, double, double, double, double, double, double, double, double, double);
double OdeSurfaceSetMu(double, double);
double OdeSurfaceSetMu2(double, double);
double OdeSurfaceSetBounce(double, double);
double OdeSurfaceSetBounceVel(double, double);
double OdeSurfaceSetSoftERP(double, double);
double OdeSurfaceSetSoftCFM(double, double);
double OdeSurfaceSetMotion1(double, double);
double OdeSurfaceSetMotion2(double, double);
double OdeSurfaceSetSlip1(double, double);
double OdeSurfaceSetSlip2(double, double);
double OdeAddJointBall(void);
double OdeAddJointFixed(void);
double OdeAddJointHinge(void);
double OdeAddJointHinge2(void);
double OdeAddJointSlider(void);
double OdeAddJointUniversal(void);
double OdeJointSetObjects(double, double, double);
double OdeJointEnable(double, double);
double OdeJointInitialize(double);
double OdeJointSetAnchor(double, double, double, double);
double OdeJointSetAnchorAtObject(double, double);
double OdeJointSetAxis1(double, double, double, double);
double OdeJointSetAxis2(double, double, double, double);
double OdeJointSetBounce(double, double, double);
double OdeJointSetCFM(double, double, double);
double OdeJointSetFMax(double, double, double);
double OdeJointSetFudgeFactor(double, double, double);
double OdeJointSetHiStop(double, double, double);
double OdeJointSetLoStop(double, double, double);
double OdeJointSetStopCFM(double, double, double);
double OdeJointSetStopERP(double, double, double);
double OdeJointSetVel(double, double, double);
double OdeRagdollCreate(double);
double OdeRagdollHingeJointCreate(double, double, double, double, double);
double OdeRagdollUniversalJointCreate(double, double, double, double, double, double, double, double, double, double);
double OdeRagdollDummyJointCreate(void);
double OdeRagdollBoneCreate(double, double, double, double);
double OdeRagdollBuild(double);
double OdeRagdollEnable(double, double);
double OdeRagdollUpdate(double);
double OdeDynamicSetVelocity(double, double, double, double);
double OdeDynamicSetAngularVelocity(double, double, double, double);
double OdeDynamicGetVelocity(double, double);
double OdeDynamicGetAngularVelocity(double, double);
double OdeDynamicSetPosition(double, double, double, double);
double OdeDynamicSetRotationQuaternion(double, double, double, double, double);

/* pak.pas */
double SetPakArchive(char*);
double PakGetFileCount(double);
char* PakGetFileName(double, double);
double PakExtract(double, char*);
double PakExtractFile(double, double, char*);

/* partition.pas */
double OctreeCreate(double, double, double, double);
double QuadtreeCreate(double, double, double, double);
double PartitionDestroy(double);
double PartitionAddLeaf(double, double);
double PartitionLeafChanged(double);
double PartitionQueryFrustum(double, double);
double PartitionQueryLeaf(double, double);
double PartitionQueryAABB(double, double);
double PartitionQueryBSphere(double, double);
double PartitionGetNodeTests(double);
double PartitionGetNodeCount(double);
double PartitionGetResult(double, double);
double PartitionGetResultCount(double);
double PartitionResultShow(double);
double PartitionResultHide(double);

/* picklist.pas */
double PickListCreate(double);
double PickListClear(double);
double PickListGetCount(double);
double PickListGetHit(double, double);

/* pipe.pas */
double PipeCreate(double, double, double);
double PipeAddNode(double, double, double, double);
double PipeSetDivision(double, double);
double PipeSetSplineMode(double, double);
double PipeDeleteNode(double, double);
double PipeSetRadius(double, double);
double PipeSetNode(double, double, double, double, double);
double PipeSetSlices(double, double);

/* primitives.pas */
double CubeCreate(double, double, double, double);
double CubeSetNormalDirection(double, double);
double CubeGetNormalDirection(double);
double PlaneCreate(double, double, double, double, double, double);
double PlaneSetOptions(double, double, double, double);
double PlaneGetOptions(double, double);
double TilePlaneCreate(double);
double TilePlaneSetTile(double, double, double, char*);
double SphereCreate(double, double, double, double);
double SphereSetAngleLimits(double, double, double, double, double);
double SphereGetAngleLimits(double, double);
double SphereSetOptions(double, double, double, double);
double SphereGetOptions(double, double);
double CylinderCreate(double, double, double, double, double, double, double);
double CylinderSetOptions(double, double, double, double, double, double, double);
double CylinderGetOptions(double, double);
double ConeCreate(double, double, double, double, double, double);
double ConeGetOptions(double, double);
double ConeSetOptions(double, double, double, double, double, double);
double AnnulusCreate(double, double, double, double, double, double, double);
double AnnulusSetOptions(double, double, double, double, double, double, double);
double AnnulusGetOptions(double, double);
double TorusCreate(double, double, double, double, double);
double TorusSetOptions(double, double, double, double, double);
double TorusGetOptions(double, double);
double DiskCreate(double, double, double, double, double, double, double);
double DiskSetOptions(double, double, double, double, double, double, double);
double DiskGetOptions(double, double);
double FrustrumCreate(double, double, double, double, double);
double FrustrumSetOptions(double, double, double, double, double);
double FrustrumGetOptions(double, double);
double DodecahedronCreate(double);
double IcosahedronCreate(double);
double TeapotCreate(double);

/* proxy.pas */
double ProxyObjectCreate(double, double);
double ProxyObjectSetTarget(double, double);
double MultiProxyObjectCreate(double);
double MultiProxyObjectAddTarget(double, double, double, double);
double ActorProxyObjectCreate(double, double);
double ActorProxyObjectSwitchToAnimation(double, double);
double ActorProxyObjectSetAnimationRange(double, double, double);
double ActorProxyObjectSetInterval(double, double);

/* shaders.pas */
double ShaderEnable(double, double);
double CelShaderCreate(void);
double CelShaderSetLineColor(double, double);
double CelShaderSetLineWidth(double, double);
double CelShaderSetOptions(double, double, double);
double MultiMaterialShaderCreate(double);
double HiddenLineShaderCreate(void);
double HiddenLineShaderSetLineSmooth(double, double);
double HiddenLineShaderSetSolid(double, double);
double HiddenLineShaderSetSurfaceLit(double, double);
double HiddenLineShaderSetFrontLine(double, double, double, double, double);
double HiddenLineShaderSetBackLine(double, double, double, double, double);
double OutlineShaderCreate(double);
double OutlineShaderSetLineColor(double, double);
double OutlineShaderSetLineWidth(double, double);
double TexCombineShaderCreate(double);
double TexCombineShaderAddCombiner(double, char*);
double TexCombineShaderMaterial3(double, char*);
double TexCombineShaderMaterial4(double, char*);
double GLSLShaderCreate(char*, char*);
double GLSLShaderSetLogger(double, double);
double GLSLShaderCreateParameter(double, char*);
double GLSLShaderSetParameter1i(double, double);
double GLSLShaderSetParameter1f(double, double);
double GLSLShaderSetParameter2f(double, double, double);
double GLSLShaderSetParameter3f(double, double, double, double);
double GLSLShaderSetParameter4f(double, double, double, double, double);
double GLSLShaderSetParameterTexture(double, char*, double);
double GLSLShaderSetParameterSecondTexture(double, char*, double);
double GLSLShaderSetParameterShadowTexture(double, double, double);
double GLSLShaderSetParameterShadowMatrix(double, double);
double GLSLShaderSetParameterMatrix(double, double);
double GLSLShaderSetParameterInvMatrix(double, double);
double GLSLShaderSetParameterFBOColorTexture(double, double, double);
double GLSLShaderSetParameterFBODepthTexture(double, double, double);
double GLSLShaderSetParameterViewMatrix(double);
double GLSLShaderSetParameterInvViewMatrix(double);
double GLSLShaderSetParameterHasTextureEx(double, double);
double GLSLShaderForceDisableStencilTest(double, double);
double GLSLShaderSetOptions(double, double, double);
double PhongShaderCreate(void);
double PhongShaderUseTexture(double, double);
double PhongShaderSetMaxLights(double, double);
double BumpShaderCreate(void);
double BumpShaderSetDiffuseTexture(double, char*);
double BumpShaderSetNormalTexture(double, char*);
double BumpShaderSetHeightTexture(double, char*);
double BumpShaderSetMaxLights(double, double);
double BumpShaderUseParallax(double, double);
double BumpShaderSetParallaxOffset(double, double);
double BumpShaderSetShadowMap(double, double);
double BumpShaderSetShadowBlurRadius(double, double);
double BumpShaderUseAutoTangentSpace(double, double);

/* shadowmap.pas */
double ShadowMapCreate(double, double, double);
double ShadowMapUpdate(double);
double ShadowMapSetCamera(double, double);
double ShadowMapSetViewer(double, double);
double ShadowMapSetFBO(double, double);
double ShadowCameraCreate(double);
double ShadowCameraSetProjectionSize(double, double);
double ShadowCameraSetZClippingPlanes(double, double, double);

/* shadowplane.pas */
double ShadowplaneCreate(double, double, double, double, double, double, double, double, double);
double ShadowplaneSetLight(double, double);
double ShadowplaneSetObject(double, double);
double ShadowplaneSetOptions(double, double, double, double, double);

/* shadowvolume.pas */
double ShadowvolumeCreate(double);
double ShadowvolumeSetActive(double, double);
double ShadowvolumeAddLight(double, double);
double ShadowvolumeRemoveLight(double, double);
double ShadowvolumeAddOccluder(double, double);
double ShadowvolumeRemoveOccluder(double, double);
double ShadowvolumeSetDarkeningColor(double, double, double);
double ShadowvolumeSetMode(double, double);

/* skybox.pas */
double SkyboxCreate(double);
double SkyboxSetMaterial(double, double, char*);
double SkyboxSetClouds(double, double, double);
double SkyboxSetStyle(double, double);

/* skydome.pas */
double SkydomeCreate(double, double, double);
double SkydomeSetOptions(double, double, double);
double SkydomeSetDeepColor(double, double);
double SkydomeSetHazeColor(double, double);
double SkydomeSetNightColor(double, double);
double SkydomeSetSkyColor(double, double);
double SkydomeSetSunDawnColor(double, double);
double SkydomeSetSunZenithColor(double, double);
double SkydomeSetSunElevation(double, double);
double SkydomeSetTurbidity(double, double);
double SkydomeAddRandomStars(double, double, double);
double SkydomeAddStar(double, double, double, double, double);
double SkydomeClearStars(double);
double SkydomeTwinkleStars(double, double);

/* sprite.pas */
double HUDSpriteCreate(char*, double, double, double);
double HUDSpriteGetMouseOver(double, double);
double HUDSpriteXTiles(double, double);
double HUDSpriteYTiles(double, double);
double SpriteCreate(char*, double, double, double);
double SpriteSetSize(double, double, double);
double SpriteGetSize(double, double);
double SpriteScale(double, double, double);
double SpriteSetRotation(double, double);
double SpriteRotate(double, double);
double SpriteMirror(double, double, double);
double SpriteCreateEx(double, double, double, double, double, double, double);
double HUDSpriteCreateEx(double, double, double, double, double, double, double);
double SpriteSetBounds(double, double, double, double, double);
double SpriteSetBoundsUV(double, double, double, double, double);
double SpriteSetOrigin(double, double, double);

/* terrain.pas */
double BmpHDSCreate(char*);
double BmpHDSSetInfiniteWarp(double, double);
double BmpHDSInvert(double);
double BmpHDSCreateEmpty(double, double, double);
double BmpHDSSetHeight(double, double, double, double);
double BmpHDSGetHeight(double, double, double);
double BmpHDSSave(double, char*);
double TerrainCreate(double);
double TerrainSetHeightData(double, double);
double TerrainSetTileSize(double, double);
double TerrainSetTilesPerTexture(double, double);
double TerrainSetQualityDistance(double, double);
double TerrainSetQualityStyle(double, double);
double TerrainSetMaxCLodTriangles(double, double);
double TerrainSetCLodPrecision(double, double);
double TerrainSetOcclusionFrameSkip(double, double);
double TerrainSetOcclusionTesselate(double, double);
double TerrainGetHeightAtObjectPosition(double, double);
double TerrainGetLastTriCount(double);
double TerrainGetHDSPosition(double, double, double, double, double);

/* text.pas */

/* texture.pas */
double TextureExLoad(double, char*);
double TextureExSetFromMaterial(double, char*);
double TextureExGenerate(double, double, double);
double TextureExDelete(double);
double TextureExSetTextureScale(double, double, double);
double TextureExSetTextureOffset(double, double, double);
double TextureExEnable(double, double);

/* thorfx.pas */
double ThorFXManagerCreate(void);
double ThorFXSetColor(double, double, double, double, double, double, double);
double ThorFXEnableCore(double, double);
double ThorFXEnableGlow(double, double);
double ThorFXSetMaxParticles(double, double);
double ThorFXSetGlowSize(double, double);
double ThorFXSetVibrate(double, double);
double ThorFXSetWildness(double, double);
double ThorFXSetTarget(double, double, double, double);
double ThorFXCreate(double, double);

/* trail.pas */
double TrailCreate(double, double);
double TrailSetObject(double, double);
double TrailSetAlpha(double, double, double);
double TrailSetLimits(double, double, double);
double TrailSetMinDistance(double, double);
double TrailSetUVScale(double, double);
double TrailSetMarkStyle(double, double);
double TrailSetMarkWidth(double, double);
double TrailSetEnabled(double, double);
double TrailClearMarks(double);

/* tree.pas */
double TreeCreate(double);
double TreeSetMaterials(double);
double TreeSetBranchFacets(double, double);
double TreeBuildMesh(double, double);
double TreeSetBranchNoise(double, double);
double TreeSetBranchAngle(double, double);
double TreeSetBranchSize(double, double);
double TreeSetBranchRadius(double, double);
double TreeSetBranchTwist(double, double);
double TreeSetDepth(double, double);
double TreeSetLeafSize(double, double);
double TreeSetLeafThreshold(double, double);
double TreeSetSeed(double, double);

/* verlet.pas */
double VerletWorldCreate(double, double, double);
double VerletWorldCreateOctree(double, double, double, double, double, double, double, double, double);
double VerletGetNodeCount(double);
double VerletWorldGravityCreate(double, double, double, double);
double VerletWorldGravitySetDirection(double, double, double, double);
double VerletWorldUpdate(double, double);
double EdgeDetectorCreate(double, double);
double EdgeDetectorSetWeldDistance(double, double);
double VerletConstraintFloorCreate(double, double, double);
double VerletConstraintFloorSetNormal(double, double, double, double);
double VerletConstraintFloorSetObjectLocations(double, double);
double VerletConstraintSphereCreate(double, double);
double VerletConstraintCylinderCreate(double, double);
double VerletConstraintCylinderSetAxis(double, double, double, double);
double VerletConstraintCubeCreate(double, double, double, double);
double VerletConstraintCubeCreateSetCube(double, double);
double VerletConstraintCubeSetDirection(double, double, double, double);
double VerletConstraintCapsuleCreate(double, double, double);
double VerletConstraintCapsuleSetAxis(double, double, double, double);
double VerletConstraintSetPosition(double, double, double, double);
double VerletConstraintSetFrictionRatio(double, double);
double VerletConstraintSetEnabled(double, double);
double VerletNodeNailedDown(double, double, double);
double VerletNodeSetPosition(double, double, double, double, double);
double VerletNodeSetRadius(double, double, double);
double VerletNodeSetFriction(double, double, double);
double VerletNodeSetWeight(double, double, double);
double VerletNodeApplyFriction(double, double, double, double, double, double, double);
double VerletAirResistanceCreate(double, double, double);
double VerletAirResistanceSetWindDirection(double, double, double, double);
double VerletAirResistanceSetWindMagnitude(double, double);
double VerletAirResistanceSetWindChaos(double, double);
double VerletConstraintGetCount(double);
double VerletConstraintSetSlack(double, double, double);
double VerletWorldSetSimTime(double, double);
double VerletWorldSetMaxDeltaTime(double, double);

/* viewer.pas */
double ViewerCreate(double, double, double, double, double);
double ViewerSetCamera(double, double);
double ViewerEnableVSync(double, double);
double ViewerRender(double);
double ViewerBeginRender(double);
double ViewerClear(double, double, double, double);
double ViewerRenderObject(double, double);
double ViewerEndRender(double);
double ViewerRenderToFile(double, char*);
double ViewerResize(double, double, double, double, double);
double ViewerSetVisible(double, double);
double ViewerGetPixelColor(double, double, double);
double ViewerGetPixelDepth(double, double, double);
double ViewerSetLighting(double, double);
double ViewerSetBackgroundColor(double, double);
double ViewerSetAmbientColor(double, double);
double ViewerEnableFog(double, double);
double ViewerSetFogColor(double, double);
double ViewerSetFogDistance(double, double, double);
double ViewerScreenToWorld(double, double, double, double);
double ViewerWorldToScreen(double, double, double, double, double);
double ViewerCopyToTexture(double, char*);
double ViewerGetPickedObject(double, double, double);
double ViewerGetPickedObjectsList(double, double, double, double, double, double, double);
double ViewerScreenToVector(double, double, double, double);
double ViewerVectorToScreen(double, double, double, double, double);
double ViewerPixelToDistance(double, double, double);
double ViewerSetAntiAliasing(double, double);
double ViewerGetGLSLSupported(double);
double ViewerGetFBOSupported(double);
double ViewerGetVBOSupported(double);
double ViewerGetSize(double, double);
double ViewerGetPosition(double, double);
double ViewerIsOpenGLExtensionSupported(double, char*);
double ViewerGetFramesPerSecond(double);
double ViewerResetPerformanceMonitor(double);
double ViewerPixelRayToWorld(double, double, double, double);
double ViewerShadeModel(double, double);
double ViewerSetAutoRender(double, double);

/* water.pas */
double WaterCreate(double);
double WaterCreateRandomRipple(double);
double WaterCreateRippleAtGridPosition(double, double, double);
double WaterCreateRippleAtWorldPosition(double, double, double, double);
double WaterCreateRippleAtObjectPosition(double, double);
double WaterSetMask(double, char*);
double WaterSetActive(double, double);
double WaterReset(double);
double WaterSetRainTimeInterval(double, double);
double WaterSetRainForce(double, double);
double WaterSetViscosity(double, double);
double WaterSetElastic(double, double);
double WaterSetResolution(double, double);
double WaterSetLinearWaveHeight(double, double);
double WaterSetLinearWaveFrequency(double, double);

/* window.pas */
double WindowCreate(double, double, double, double, double);
double WindowCenter(double);
double WindowResize(double, double, double, double, double);
double WindowGetPosition(double, double);
double WindowGetSize(double, double);
double WindowGetHandle(double);
double WindowSetTitle(double, char*);
double WindowDestroy(double);
double WindowIsShowing(double);
double WindowSetIcon(double, char*);
double WindowDispatch(void);
double WindowIsActive(double);

#ifdef __cplusplus
}
#endif

#endif /* XTREME3D_H */
