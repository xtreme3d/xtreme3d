function dll_init(dll) {
	// engine.pas
	global._EngineCreate = external_define(dll, "EngineCreate", dll_cdecl, ty_real, 0);
	global._EngineDestroy = external_define(dll, "EngineDestroy", dll_cdecl, ty_real, 0);
	global._EngineSetObjectsSorting = external_define(dll, "EngineSetObjectsSorting", dll_cdecl, ty_real, 1, ty_real);
	global._EngineSetCulling = external_define(dll, "EngineSetCulling", dll_cdecl, ty_real, 1, ty_real);
	global._EngineUpdate = external_define(dll, "EngineUpdate", dll_cdecl, ty_real, 1, ty_real);
	global._EngineSaveScene = external_define(dll, "EngineSaveScene", dll_cdecl, ty_real, 1, ty_string);
	global._EngineLoadScene = external_define(dll, "EngineLoadScene", dll_cdecl, ty_real, 1, ty_string);
	global._EngineRootObject = external_define(dll, "EngineRootObject", dll_cdecl, ty_real, 0);
	global._EngineShowLoadingErrors = external_define(dll, "EngineShowLoadingErrors", dll_cdecl, ty_real, 1, ty_real);
	global._EngineGetTimeStep = external_define(dll, "EngineGetTimeStep", dll_cdecl, ty_real, 0);
	
	// viewer.pas
	global._ViewerCreate = external_define(dll, "ViewerCreate", dll_cdecl, ty_real, 5, ty_real, ty_real, ty_real, ty_real, ty_real);
	global._ViewerSetCamera = external_define(dll, "ViewerSetCamera", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerEnableVSync = external_define(dll, "ViewerEnableVSync", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerRender = external_define(dll, "ViewerRender", dll_cdecl, ty_real, 1, ty_real);
	global._ViewerRenderToFile = external_define(dll, "ViewerRenderToFile", dll_cdecl, ty_real, 2, ty_real, ty_string);
	global._ViewerResize = external_define(dll, "ViewerResize", dll_cdecl, ty_real, 5, ty_real, ty_real, ty_real, ty_real, ty_real);
	global._ViewerSetVisible = external_define(dll, "ViewerSetVisible", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerGetPixelColor = external_define(dll, "ViewerGetPixelColor", dll_cdecl, ty_real, 3, ty_real, ty_real, ty_real);
	global._ViewerGetPixelDepth = external_define(dll, "ViewerGetPixelDepth", dll_cdecl, ty_real, 3, ty_real, ty_real, ty_real);
	global._ViewerSetLighting = external_define(dll, "ViewerSetLighting", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerSetBackgroundColor = external_define(dll, "ViewerSetBackgroundColor", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerSetAmbientColor = external_define(dll, "ViewerSetAmbientColor", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerEnableFog = external_define(dll, "ViewerEnableFog", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerSetFogColor = external_define(dll, "ViewerSetFogColor", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerSetFogDistance = external_define(dll, "ViewerSetFogDistance", dll_cdecl, ty_real, 3, ty_real, ty_real, ty_real);
	global._ViewerScreenToWorld = external_define(dll, "ViewerScreenToWorld", dll_cdecl, ty_real, 4, ty_real, ty_real, ty_real, ty_real);
	global._ViewerWorldToScreen = external_define(dll, "ViewerWorldToScreen", dll_cdecl, ty_real, 5, ty_real, ty_real, ty_real, ty_real, ty_real);
	global._ViewerCopyToTexture = external_define(dll, "ViewerCopyToTexture", dll_cdecl, ty_real, 2, ty_real, ty_string);
	global._ViewerGetPickedObject = external_define(dll, "ViewerGetPickedObject", dll_cdecl, ty_real, 3, ty_real, ty_real, ty_real);
	global._ViewerGetPickedObjectsList = external_define(dll, "ViewerGetPickedObjectsList", dll_cdecl, ty_real, 7, ty_real, ty_real, ty_real, ty_real, ty_real, ty_real, ty_real);
	global._ViewerScreenToVector = external_define(dll, "ViewerScreenToVector", dll_cdecl, ty_real, 4, ty_real, ty_real, ty_real, ty_real);
	global._ViewerVectorToScreen = external_define(dll, "ViewerVectorToScreen", dll_cdecl, ty_real, 5, ty_real, ty_real, ty_real, ty_real, ty_real);
	global._ViewerPixelToDistance = external_define(dll, "ViewerPixelToDistance", dll_cdecl, ty_real, 3, ty_real, ty_real, ty_real);
	global._ViewerSetAntiAliasing = external_define(dll, "ViewerSetAntiAliasing", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerGetSize = external_define(dll, "ViewerGetSize", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerGetPosition = external_define(dll, "ViewerGetPosition", dll_cdecl, ty_real, 2, ty_real, ty_real);
	global._ViewerIsOpenGLExtensionSupported = external_define(dll, "ViewerIsOpenGLExtensionSupported", dll_cdecl, ty_real, 2, ty_real, ty_string);
	global._ViewerGetFramesPerSecond = external_define(dll, "ViewerGetFramesPerSecond", dll_cdecl, ty_real, 1, ty_real);
	global._ViewerResetPerformanceMonitor = external_define(dll, "ViewerResetPerformanceMonitor", dll_cdecl, ty_real, 1, ty_real);
	global._ViewerPixelRayToWorld = external_define(dll, "ViewerPixelRayToWorld", dll_cdecl, ty_real, 4, ty_real, ty_real, ty_real, ty_real);
	global._ViewerShadeModel = external_define(dll, "ViewerShadeModel", dll_cdecl, ty_real, 2, ty_real, ty_real);
}

function ActorCreate(aFname, aMatl, aParent) {
	return external_call(global._ActorCreate, aFname, aMatl, aParent);
}

function ActorCopy(aActor, aParent) {
	return external_call(global._ActorCopy, aActor, aParent);
}

function ActorSetAnimationRange(aActor, aAstart, aAend) {
	return external_call(global._ActorSetAnimationRange, aActor, aAstart, aAend);
}

function ActorGetCurrentFrame(aActor) {
	return external_call(global._ActorGetCurrentFrame, aActor);
}

function ActorSwitchToAnimation(aActor, aAnim, aSmooth) {
	return external_call(global._ActorSwitchToAnimation, aActor, aAnim, aSmooth);
}

function ActorSwitchToAnimationName(aActor, aAnim, aSmooth) {
	return external_call(global._ActorSwitchToAnimationName, aActor, aAnim, aSmooth);
}

function ActorSynchronize(aActor1, aActor2) {
	return external_call(global._ActorSynchronize, aActor1, aActor2);
}

function ActorSetInterval(aActor, aInterv) {
	return external_call(global._ActorSetInterval, aActor, aInterv);
}

function ActorSetAnimationMode(aActor, aAam) {
	return external_call(global._ActorSetAnimationMode, aActor, aAam);
}

function ActorSetFrameInterpolation(aActor, aAfp) {
	return external_call(global._ActorSetFrameInterpolation, aActor, aAfp);
}

function ActorAddObject(aActor, aFname) {
	return external_call(global._ActorAddObject, aActor, aFname);
}

function ActorGetCurrentAnimation(aActor) {
	return external_call(global._ActorGetCurrentAnimation, aActor);
}

function ActorGetFrameCount(aActor) {
	return external_call(global._ActorGetFrameCount, aActor);
}

function ActorGetBoneCount(aActor) {
	return external_call(global._ActorGetBoneCount, aActor);
}

function ActorGetBoneByName(aActor, aName) {
	return external_call(global._ActorGetBoneByName, aActor, aName);
}

function ActorGetBoneRotation(aActor, aBone, aInd) {
	return external_call(global._ActorGetBoneRotation, aActor, aBone, aInd);
}

function ActorGetBonePosition(aActor, aBone, aInd) {
	return external_call(global._ActorGetBonePosition, aActor, aBone, aInd);
}

function ActorBoneExportMatrix(aActor, aBone, aObj) {
	return external_call(global._ActorBoneExportMatrix, aActor, aBone, aObj);
}

function ActorMakeSkeletalTranslationStatic(aActor, aAnim) {
	return external_call(global._ActorMakeSkeletalTranslationStatic, aActor, aAnim);
}

function ActorMakeSkeletalRotationDelta(aActor, aAnim) {
	return external_call(global._ActorMakeSkeletalRotationDelta, aActor, aAnim);
}

function ActorShowSkeleton(aActor, aMode) {
	return external_call(global._ActorShowSkeleton, aActor, aMode);
}

function AnimationBlenderCreate() {
	return external_call(global._AnimationBlenderCreate);
}

function AnimationBlenderSetActor(aAnim, aActor) {
	return external_call(global._AnimationBlenderSetActor, aAnim, aActor);
}

function AnimationBlenderSetAnimation(aAnim, aName) {
	return external_call(global._AnimationBlenderSetAnimation, aAnim, aName);
}

function AnimationBlenderSetRatio(aAnim, aRat) {
	return external_call(global._AnimationBlenderSetRatio, aAnim, aRat);
}

function ActorLoadQ3TagList(aFname) {
	return external_call(global._ActorLoadQ3TagList, aFname);
}

function ActorQ3TagExportMatrix(aActor, aTaglist, aTagname, aObj) {
	return external_call(global._ActorQ3TagExportMatrix, aActor, aTaglist, aTagname, aObj);
}

function ActorLoadQ3Animations(aActor, aFname, aClas) {
	return external_call(global._ActorLoadQ3Animations, aActor, aFname, aClas);
}

function ActorMeshObjectsCount(aActor) {
	return external_call(global._ActorMeshObjectsCount, aActor);
}

function ActorFaceGroupsCount(aActor, aMeshobject) {
	return external_call(global._ActorFaceGroupsCount, aActor, aMeshobject);
}

function ActorFaceGroupGetMaterialName(aActor, aMeshobject, aFacegroup) {
	return external_call(global._ActorFaceGroupGetMaterialName, aActor, aMeshobject, aFacegroup);
}

function ActorFaceGroupSetMaterial(aActor, aMeshobject, aFacegroup, aMtrl) {
	return external_call(global._ActorFaceGroupSetMaterial, aActor, aMeshobject, aFacegroup, aMtrl);
}

function ActorMoveBone(aActor, aBoneindex, aX, aY, aZ) {
	return external_call(global._ActorMoveBone, aActor, aBoneindex, aX, aY, aZ);
}

function ActorRotateBone(aActor, aBoneindex, aX, aY, aZ) {
	return external_call(global._ActorRotateBone, aActor, aBoneindex, aX, aY, aZ);
}

function ActorMeshSetVisible(aActor, aMesh, aMode) {
	return external_call(global._ActorMeshSetVisible, aActor, aMesh, aMode);
}

function ActorGetAnimationName(aActor, aInd) {
	return external_call(global._ActorGetAnimationName, aActor, aInd);
}

function ActorGetAnimationCount(aActor) {
	return external_call(global._ActorGetAnimationCount, aActor);
}

function ActorAnimationDestroy(aActor, aIndex) {
	return external_call(global._ActorAnimationDestroy, aActor, aIndex);
}

function ActorAnimationNextFrame(aActor) {
	return external_call(global._ActorAnimationNextFrame, aActor);
}

function ActorAnimationPrevFrame(aActor) {
	return external_call(global._ActorAnimationPrevFrame, aActor);
}

function ActorSetFrame(aActor, aFrame) {
	return external_call(global._ActorSetFrame, aActor, aFrame);
}

function ActorTriangleCount(aActor) {
	return external_call(global._ActorTriangleCount, aActor);
}

function BlurCreate(aTargetobj, aParent) {
	return external_call(global._BlurCreate, aTargetobj, aParent);
}

function BlurSetPreset(aBlur, aP) {
	return external_call(global._BlurSetPreset, aBlur, aP);
}

function BlurSetOptions(aBlur, aDelta, aLeft, aTop, aRight, aBottom) {
	return external_call(global._BlurSetOptions, aBlur, aDelta, aLeft, aTop, aRight, aBottom);
}

function BlurSetResolution(aBlur, aRes) {
	return external_call(global._BlurSetResolution, aBlur, aRes);
}

function BlurSetColor(aBlur, aCol) {
	return external_call(global._BlurSetColor, aBlur, aCol);
}

function BlurSetBlendingMode(aBlur, aBm) {
	return external_call(global._BlurSetBlendingMode, aBlur, aBm);
}

function CameraCreate(aParent) {
	return external_call(global._CameraCreate, aParent);
}

function CameraSetStyle(aCamera, aCs) {
	return external_call(global._CameraSetStyle, aCamera, aCs);
}

function CameraSetFocal(aCamera, aFov) {
	return external_call(global._CameraSetFocal, aCamera, aFov);
}

function CameraSetSceneScale(aCamera, aScale) {
	return external_call(global._CameraSetSceneScale, aCamera, aScale);
}

function CameraScaleScene(aCamera, aScale) {
	return external_call(global._CameraScaleScene, aCamera, aScale);
}

function CameraSetViewDepth(aCamera, aDepth) {
	return external_call(global._CameraSetViewDepth, aCamera, aDepth);
}

function CameraSetTargetObject(aCamera, aObj) {
	return external_call(global._CameraSetTargetObject, aCamera, aObj);
}

function CameraMoveAroundTarget(aCamera, aPitch, aTurn) {
	return external_call(global._CameraMoveAroundTarget, aCamera, aPitch, aTurn);
}

function CameraSetDistanceToTarget(aCamera, aDistance) {
	return external_call(global._CameraSetDistanceToTarget, aCamera, aDistance);
}

function CameraGetDistanceToTarget(aCamera) {
	return external_call(global._CameraGetDistanceToTarget, aCamera);
}

function CameraCopyToTexture(aCamera, aMtrl, aWidth, aHeight) {
	return external_call(global._CameraCopyToTexture, aCamera, aMtrl, aWidth, aHeight);
}

function CameraGetNearPlane(aCamera) {
	return external_call(global._CameraGetNearPlane, aCamera);
}

function CameraSetNearPlaneBias(aCamera, aBias) {
	return external_call(global._CameraSetNearPlaneBias, aCamera, aBias);
}

function CameraAbsoluteVectorToTarget(aCamera, aInd) {
	return external_call(global._CameraAbsoluteVectorToTarget, aCamera, aInd);
}

function CameraAbsoluteRightVectorToTarget(aCamera, aInd) {
	return external_call(global._CameraAbsoluteRightVectorToTarget, aCamera, aInd);
}

function CameraAbsoluteUpVectorToTarget(aCamera, aInd) {
	return external_call(global._CameraAbsoluteUpVectorToTarget, aCamera, aInd);
}

function CameraZoomAll(aCamera) {
	return external_call(global._CameraZoomAll, aCamera);
}

function CameraScreenDeltaToVector(aCamera, aDx, aDy, aRatio, aNx, aNy, aNz, aInd) {
	return external_call(global._CameraScreenDeltaToVector, aCamera, aDx, aDy, aRatio, aNx, aNy, aNz, aInd);
}

function CameraScreenDeltaToVectorXY(aCamera, aDx, aDy, aRatio, aInd) {
	return external_call(global._CameraScreenDeltaToVectorXY, aCamera, aDx, aDy, aRatio, aInd);
}

function CameraScreenDeltaToVectorXZ(aCamera, aDx, aDy, aRatio, aInd) {
	return external_call(global._CameraScreenDeltaToVectorXZ, aCamera, aDx, aDy, aRatio, aInd);
}

function CameraScreenDeltaToVectorYZ(aCamera, aDx, aDy, aRatio, aInd) {
	return external_call(global._CameraScreenDeltaToVectorYZ, aCamera, aDx, aDy, aRatio, aInd);
}

function CameraAbsoluteEyeSpaceVector(aCamera, aFordist, aRightdist, aUpdist, aInd) {
	return external_call(global._CameraAbsoluteEyeSpaceVector, aCamera, aFordist, aRightdist, aUpdist, aInd);
}

function CameraSetAutoLeveling(aCamera, aFactor) {
	return external_call(global._CameraSetAutoLeveling, aCamera, aFactor);
}

function CameraMoveInEyeSpace(aCamera, aFordist, aRightdist, aUpdist) {
	return external_call(global._CameraMoveInEyeSpace, aCamera, aFordist, aRightdist, aUpdist);
}

function CameraMoveTargetInEyeSpace(aCamera, aFordist, aRightdist, aUpdist) {
	return external_call(global._CameraMoveTargetInEyeSpace, aCamera, aFordist, aRightdist, aUpdist);
}

function CameraPointInFront(aCamera, aX, aY, aZ) {
	return external_call(global._CameraPointInFront, aCamera, aX, aY, aZ);
}

function CameraGetFieldOfView(aCamera, aVpdim) {
	return external_call(global._CameraGetFieldOfView, aCamera, aVpdim);
}

function ClipPlaneCreate(aParent) {
	return external_call(global._ClipPlaneCreate, aParent);
}

function ClipPlaneEnable(aCplane, aMode) {
	return external_call(global._ClipPlaneEnable, aCplane, aMode);
}

function ClipPlaneSetPlane(aCplane, aPx, aPy, aPz, aNx, aNy, aNz) {
	return external_call(global._ClipPlaneSetPlane, aCplane, aPx, aPy, aPz, aNx, aNy, aNz);
}

function MakeColorRGB(aR, aG, aB) {
	return external_call(global._MakeColorRGB, aR, aG, aB);
}

function MakeColorRGBFloat(aR, aG, aB) {
	return external_call(global._MakeColorRGBFloat, aR, aG, aB);
}

function DceManagerCreate() {
	return external_call(global._DceManagerCreate);
}

function DceManagerStep(aMan, aDt) {
	return external_call(global._DceManagerStep, aMan, aDt);
}

function DceManagerSetGravity(aMan, aGrav) {
	return external_call(global._DceManagerSetGravity, aMan, aGrav);
}

function DceManagerSetWorldDirection(aMan, aX, aY, aZ) {
	return external_call(global._DceManagerSetWorldDirection, aMan, aX, aY, aZ);
}

function DceManagerSetWorldScale(aMan, aScale) {
	return external_call(global._DceManagerSetWorldScale, aMan, aScale);
}

function DceManagerSetMovementScale(aMan, aScale) {
	return external_call(global._DceManagerSetMovementScale, aMan, aScale);
}

function DceManagerSetLayers(aMan, aMode) {
	return external_call(global._DceManagerSetLayers, aMan, aMode);
}

function DceManagerSetManualStep(aMan, aMode) {
	return external_call(global._DceManagerSetManualStep, aMan, aMode);
}

function DceDynamicSetManager(aObj, aMan) {
	return external_call(global._DceDynamicSetManager, aObj, aMan);
}

function DceDynamicSetActive(aObj, aMode) {
	return external_call(global._DceDynamicSetActive, aObj, aMode);
}

function DceDynamicIsActive(aObj) {
	return external_call(global._DceDynamicIsActive, aObj);
}

function DceDynamicSetUseGravity(aObj, aMode) {
	return external_call(global._DceDynamicSetUseGravity, aObj, aMode);
}

function DceDynamicSetLayer(aObj, aLayer) {
	return external_call(global._DceDynamicSetLayer, aObj, aLayer);
}

function DceDynamicGetLayer(aObj) {
	return external_call(global._DceDynamicGetLayer, aObj);
}

function DceDynamicSetSolid(aObj, aMode) {
	return external_call(global._DceDynamicSetSolid, aObj, aMode);
}

function DceDynamicSetFriction(aObj, aFriction) {
	return external_call(global._DceDynamicSetFriction, aObj, aFriction);
}

function DceDynamicSetBounce(aObj, aBounce) {
	return external_call(global._DceDynamicSetBounce, aObj, aBounce);
}

function DceDynamicSetSize(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicSetSize, aObj, aX, aY, aZ);
}

function DceDynamicSetSlideOrBounce(aObj, aMode) {
	return external_call(global._DceDynamicSetSlideOrBounce, aObj, aMode);
}

function DceDynamicApplyAcceleration(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicApplyAcceleration, aObj, aX, aY, aZ);
}

function DceDynamicApplyAbsAcceleration(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicApplyAbsAcceleration, aObj, aX, aY, aZ);
}

function DceDynamicStopAcceleration(aObj) {
	return external_call(global._DceDynamicStopAcceleration, aObj);
}

function DceDynamicStopAbsAcceleration(aObj) {
	return external_call(global._DceDynamicStopAbsAcceleration, aObj);
}

function DceDynamicJump(aObj, aHeight, aSpeed) {
	return external_call(global._DceDynamicJump, aObj, aHeight, aSpeed);
}

function DceDynamicMove(aObj, aX, aY, aZ, aDelta) {
	return external_call(global._DceDynamicMove, aObj, aX, aY, aZ, aDelta);
}

function DceDynamicMoveTo(aObj, aX, aY, aZ, aAmount) {
	return external_call(global._DceDynamicMoveTo, aObj, aX, aY, aZ, aAmount);
}

function DceDynamicSetVelocity(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicSetVelocity, aObj, aX, aY, aZ);
}

function DceDynamicInGround(aObj) {
	return external_call(global._DceDynamicInGround, aObj);
}

function DceDynamicSetMaxRecursionDepth(aObj, aDepth) {
	return external_call(global._DceDynamicSetMaxRecursionDepth, aObj, aDepth);
}

function DceStaticSetManager(aObj, aMan) {
	return external_call(global._DceStaticSetManager, aObj, aMan);
}

function DceStaticSetActive(aObj, aMode) {
	return external_call(global._DceStaticSetActive, aObj, aMode);
}

function DceStaticSetShape(aObj, aMode) {
	return external_call(global._DceStaticSetShape, aObj, aMode);
}

function DceStaticSetLayer(aObj, aLayer) {
	return external_call(global._DceStaticSetLayer, aObj, aLayer);
}

function DceStaticSetSize(aObj, aX, aY, aZ) {
	return external_call(global._DceStaticSetSize, aObj, aX, aY, aZ);
}

function DceStaticSetSolid(aObj, aMode) {
	return external_call(global._DceStaticSetSolid, aObj, aMode);
}

function DceStaticSetFriction(aObj, aFriction) {
	return external_call(global._DceStaticSetFriction, aObj, aFriction);
}

function DceStaticSetBounceFactor(aObj, aBfactor) {
	return external_call(global._DceStaticSetBounceFactor, aObj, aBfactor);
}

function DceDynamicGetVelocity(aObj, aInd) {
	return external_call(global._DceDynamicGetVelocity, aObj, aInd);
}

function DceDynamicSetAbsVelocity(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicSetAbsVelocity, aObj, aX, aY, aZ);
}

function DceDynamicGetAbsVelocity(aObj, aInd) {
	return external_call(global._DceDynamicGetAbsVelocity, aObj, aInd);
}

function DceDynamicApplyImpulse(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicApplyImpulse, aObj, aX, aY, aZ);
}

function DceDynamicApplyAbsImpulse(aObj, aX, aY, aZ) {
	return external_call(global._DceDynamicApplyAbsImpulse, aObj, aX, aY, aZ);
}

function DummycubeCreate(aParent) {
	return external_call(global._DummycubeCreate, aParent);
}

function DummycubeAmalgamate(aObj, aMode) {
	return external_call(global._DummycubeAmalgamate, aObj, aMode);
}

function DummycubeSetCameraMode(aObj, aCim) {
	return external_call(global._DummycubeSetCameraMode, aObj, aCim);
}

function DummycubeSetVisible(aObj, aMode) {
	return external_call(global._DummycubeSetVisible, aObj, aMode);
}

function DummycubeSetEdgeColor(aObj, aColor) {
	return external_call(global._DummycubeSetEdgeColor, aObj, aColor);
}

function DummycubeSetCubeSize(aObj, aSize) {
	return external_call(global._DummycubeSetCubeSize, aObj, aSize);
}

function EngineCreate() {
	return external_call(global._EngineCreate);
}

function EngineDestroy() {
	return external_call(global._EngineDestroy);
}

function EngineSetObjectsSorting(aOs) {
	return external_call(global._EngineSetObjectsSorting, aOs);
}

function EngineSetCulling(aVc) {
	return external_call(global._EngineSetCulling, aVc);
}

function Update(aDelta) {
	return external_call(global._Update, aDelta);
}

function TrisRendered() {
	return external_call(global._TrisRendered);
}

function EngineSaveScene(aFilename) {
	return external_call(global._EngineSaveScene, aFilename);
}

function EngineLoadScene(aFilename) {
	return external_call(global._EngineLoadScene, aFilename);
}

function EngineRootObject() {
	return external_call(global._EngineRootObject);
}

function EngineShowLoadingErrors(aMode) {
	return external_call(global._EngineShowLoadingErrors, aMode);
}

function EngineSetMaxLights(aLights) {
	return external_call(global._EngineSetMaxLights, aLights);
}

function EngineGetTimeStep() {
	return external_call(global._EngineGetTimeStep);
}

function FBOCreate(aW, aH, aViewer) {
	return external_call(global._FBOCreate, aW, aH, aViewer);
}

function FBOSetCamera(aFbo, aCam) {
	return external_call(global._FBOSetCamera, aFbo, aCam);
}

function FBORenderObject(aFbo, aObj) {
	return external_call(global._FBORenderObject, aFbo, aObj);
}

function FBORenderObjectEx(aFbo, aObj, aClearcolor, aCleardepth, aCopycolor, aCopydepth) {
	return external_call(global._FBORenderObjectEx, aFbo, aObj, aClearcolor, aCleardepth, aCopycolor, aCopydepth);
}

function FBOSetViewer(aFbo, aViewer) {
	return external_call(global._FBOSetViewer, aFbo, aViewer);
}

function FBOSetOverrideMaterial(aFbo, aMlb, aMtrl) {
	return external_call(global._FBOSetOverrideMaterial, aFbo, aMlb, aMtrl);
}

function FBOSetColorTextureFormat(aFbo, aTf) {
	return external_call(global._FBOSetColorTextureFormat, aFbo, aTf);
}

function FireFXManagerCreate() {
	return external_call(global._FireFXManagerCreate);
}

function FireFXCreate(aMngr, aObj) {
	return external_call(global._FireFXCreate, aMngr, aObj);
}

function FireFXSetColor(aMngr, aIncolor, aInalpha, aOutcolor, aOutalpha) {
	return external_call(global._FireFXSetColor, aMngr, aIncolor, aInalpha, aOutcolor, aOutalpha);
}

function FireFXSetMaxParticles(aMngr, aParticles) {
	return external_call(global._FireFXSetMaxParticles, aMngr, aParticles);
}

function FireFXSetParticleSize(aMngr, aSize) {
	return external_call(global._FireFXSetParticleSize, aMngr, aSize);
}

function FireFXSetDensity(aMngr, aDensity) {
	return external_call(global._FireFXSetDensity, aMngr, aDensity);
}

function FireFXSetEvaporation(aMngr, aEvaporation) {
	return external_call(global._FireFXSetEvaporation, aMngr, aEvaporation);
}

function FireFXSetCrown(aMngr, aCrown) {
	return external_call(global._FireFXSetCrown, aMngr, aCrown);
}

function FireFXSetLife(aMngr, aLife) {
	return external_call(global._FireFXSetLife, aMngr, aLife);
}

function FireFXSetBurst(aMngr, aBurst) {
	return external_call(global._FireFXSetBurst, aMngr, aBurst);
}

function FireFXSetRadius(aMngr, aRadius) {
	return external_call(global._FireFXSetRadius, aMngr, aRadius);
}

function FireFXExplosion(aMngr, aIsp, aMaxsp, aLbf) {
	return external_call(global._FireFXExplosion, aMngr, aIsp, aMaxsp, aLbf);
}

function FireFXRingExplosion(aMngr, aIsp, aMaxsp, aLbf, aRx, aRy, aRz, aSx, aSy, aSz) {
	return external_call(global._FireFXRingExplosion, aMngr, aIsp, aMaxsp, aLbf, aRx, aRy, aRz, aSx, aSy, aSz);
}

function BmpfontCreate(aW, aH, aHspace, aVspace, aIntx, aInty, aChstart, aChend) {
	return external_call(global._BmpfontCreate, aW, aH, aHspace, aVspace, aIntx, aInty, aChstart, aChend);
}

function BmpfontLoad(aFont, aMtrl) {
	return external_call(global._BmpfontLoad, aFont, aMtrl);
}

function WindowsBitmapfontCreate(aNm, aSize, aChstart, aChend) {
	return external_call(global._WindowsBitmapfontCreate, aNm, aSize, aChstart, aChend);
}

function HUDTextCreate(aFont, aTxt, aParent) {
	return external_call(global._HUDTextCreate, aFont, aTxt, aParent);
}

function HUDTextSetRotation(aText, aAngle) {
	return external_call(global._HUDTextSetRotation, aText, aAngle);
}

function HUDTextSetFont(aText, aFont) {
	return external_call(global._HUDTextSetFont, aText, aFont);
}

function HUDTextSetColor(aText, aColor, aAlph) {
	return external_call(global._HUDTextSetColor, aText, aColor, aAlph);
}

function HUDTextSetText(aText, aTxt) {
	return external_call(global._HUDTextSetText, aText, aTxt);
}

function FlatTextCreate(aFont, aTxt, aParent) {
	return external_call(global._FlatTextCreate, aFont, aTxt, aParent);
}

function FlatTextSetFont(aText, aFont) {
	return external_call(global._FlatTextSetFont, aText, aFont);
}

function FlatTextSetColor(aText, aColor, aAlph) {
	return external_call(global._FlatTextSetColor, aText, aColor, aAlph);
}

function FlatTextSetText(aText, aTxt) {
	return external_call(global._FlatTextSetText, aText, aTxt);
}

function SpaceTextCreate(aFont, aTxt, aExtr, aParent) {
	return external_call(global._SpaceTextCreate, aFont, aTxt, aExtr, aParent);
}

function SpaceTextSetExtrusion(aText, aExtr) {
	return external_call(global._SpaceTextSetExtrusion, aText, aExtr);
}

function SpaceTextSetFont(aText, aFont) {
	return external_call(global._SpaceTextSetFont, aText, aFont);
}

function SpaceTextSetText(aText, aTxt) {
	return external_call(global._SpaceTextSetText, aText, aTxt);
}

function TTFontCreate(aFilename, aHeight) {
	return external_call(global._TTFontCreate, aFilename, aHeight);
}

function TTFontSetLineGap(aFont, aGap) {
	return external_call(global._TTFontSetLineGap, aFont, aGap);
}

function TTFontSetEncoding(aFont, aTe) {
	return external_call(global._TTFontSetEncoding, aFont, aTe);
}

function TTFontLoadCodePage(aFont, aFilename) {
	return external_call(global._TTFontLoadCodePage, aFont, aFilename);
}

function FpsManagerCreate() {
	return external_call(global._FpsManagerCreate);
}

function FpsManagerSetNavigator(aMan, aNav) {
	return external_call(global._FpsManagerSetNavigator, aMan, aNav);
}

function FpsManagerSetMovementScale(aMan, aScale) {
	return external_call(global._FpsManagerSetMovementScale, aMan, aScale);
}

function FpsManagerAddMap(aMan, aFfm) {
	return external_call(global._FpsManagerAddMap, aMan, aFfm);
}

function FpsManagerRemoveMap(aMan, aFfm) {
	return external_call(global._FpsManagerRemoveMap, aMan, aFfm);
}

function FpsManagerMapSetCollisionGroup(aMan, aFfm, aGroup) {
	return external_call(global._FpsManagerMapSetCollisionGroup, aMan, aFfm, aGroup);
}

function FpsSetManager(aObj, aMan) {
	return external_call(global._FpsSetManager, aObj, aMan);
}

function FpsSetCollisionGroup(aObj, aGroup) {
	return external_call(global._FpsSetCollisionGroup, aObj, aGroup);
}

function FpsSetSphereRadius(aObj, aRadius) {
	return external_call(global._FpsSetSphereRadius, aObj, aRadius);
}

function FpsSetGravity(aObj, aMode) {
	return external_call(global._FpsSetGravity, aObj, aMode);
}

function FpsMove(aObj, aSpd) {
	return external_call(global._FpsMove, aObj, aSpd);
}

function FpsStrafe(aObj, aSpd) {
	return external_call(global._FpsStrafe, aObj, aSpd);
}

function FpsLift(aObj, aSpd) {
	return external_call(global._FpsLift, aObj, aSpd);
}

function FpsGetVelocity(aObj, aInd) {
	return external_call(global._FpsGetVelocity, aObj, aInd);
}

function FreeformCreate(aFname, aMatl1, aMatl2, aParent) {
	return external_call(global._FreeformCreate, aFname, aMatl1, aMatl2, aParent);
}

function FreeformGenTangents(aFf) {
	return external_call(global._FreeformGenTangents, aFf);
}

function FreeformMeshObjectsCount(aFf) {
	return external_call(global._FreeformMeshObjectsCount, aFf);
}

function FreeformMeshSetVisible(aFf, aMesh, aMode) {
	return external_call(global._FreeformMeshSetVisible, aFf, aMesh, aMode);
}

function FreeformMeshSetSecondCoords(aFf1, aMesh1, aFf2, aMesh2) {
	return external_call(global._FreeformMeshSetSecondCoords, aFf1, aMesh1, aFf2, aMesh2);
}

function FreeformMeshTriangleCount(aFf, aMesh) {
	return external_call(global._FreeformMeshTriangleCount, aFf, aMesh);
}

function FreeformMeshObjectGetName(aFf, aMesh) {
	return external_call(global._FreeformMeshObjectGetName, aFf, aMesh);
}

function FreeformMeshObjectSetName(aFf, aMesh, aName) {
	return external_call(global._FreeformMeshObjectSetName, aFf, aMesh, aName);
}

function FreeformMeshObjectDestroy(aFf, aMesh) {
	return external_call(global._FreeformMeshObjectDestroy, aFf, aMesh);
}

function FreeformMeshFaceGroupsCount(aFf, aMesh) {
	return external_call(global._FreeformMeshFaceGroupsCount, aFf, aMesh);
}

function FreeformMeshFaceGroupTriangleCount(aFf, aMesh, aFgr) {
	return external_call(global._FreeformMeshFaceGroupTriangleCount, aFf, aMesh, aFgr);
}

function FreeformCreateExplosionFX(aFf1, aEnable) {
	return external_call(global._FreeformCreateExplosionFX, aFf1, aEnable);
}

function FreeformExplosionFXReset(aFf1) {
	return external_call(global._FreeformExplosionFXReset, aFf1);
}

function FreeformExplosionFXEnable(aFf1, aMode) {
	return external_call(global._FreeformExplosionFXEnable, aFf1, aMode);
}

function FreeformExplosionFXSetSpeed(aFf1, aSpeed) {
	return external_call(global._FreeformExplosionFXSetSpeed, aFf1, aSpeed);
}

function FreeformSphereSweepIntersect(aFreeform, aObj, aRadius, aVel) {
	return external_call(global._FreeformSphereSweepIntersect, aFreeform, aObj, aRadius, aVel);
}

function FreeformPointInMesh(aFreeform, aX, aY, aZ) {
	return external_call(global._FreeformPointInMesh, aFreeform, aX, aY, aZ);
}

function FreeformMeshSetMaterial(aFf, aMesh, aMaterial) {
	return external_call(global._FreeformMeshSetMaterial, aFf, aMesh, aMaterial);
}

function FreeformUseMeshMaterials(aFf, aMode) {
	return external_call(global._FreeformUseMeshMaterials, aFf, aMode);
}

function FreeformToFreeforms(aFreeform, aParent) {
	return external_call(global._FreeformToFreeforms, aFreeform, aParent);
}

function FreeformMeshFaceGroupSetMaterial(aFf, aMesh, aFg, aMatname) {
	return external_call(global._FreeformMeshFaceGroupSetMaterial, aFf, aMesh, aFg, aMatname);
}

function FreeformMeshFaceGroupGetMaterial(aFf, aMesh, aFgroup) {
	return external_call(global._FreeformMeshFaceGroupGetMaterial, aFf, aMesh, aFgroup);
}

function FreeformCreateEmpty(aMatlib1, aMatlib2, aParent) {
	return external_call(global._FreeformCreateEmpty, aMatlib1, aMatlib2, aParent);
}

function FreeformAddMesh(aFf) {
	return external_call(global._FreeformAddMesh, aFf);
}

function FreeformMeshAddFaceGroup(aFf, aMesh) {
	return external_call(global._FreeformMeshAddFaceGroup, aFf, aMesh);
}

function FreeformMeshAddVertex(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshAddVertex, aFf, aMesh, aX, aY, aZ);
}

function FreeformMeshAddNormal(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshAddNormal, aFf, aMesh, aX, aY, aZ);
}

function FreeformMeshAddTexCoord(aFf, aMesh, aU, aV) {
	return external_call(global._FreeformMeshAddTexCoord, aFf, aMesh, aU, aV);
}

function FreeformMeshAddSecondTexCoord(aFf, aMesh, aU, aV) {
	return external_call(global._FreeformMeshAddSecondTexCoord, aFf, aMesh, aU, aV);
}

function FreeformMeshAddTangent(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshAddTangent, aFf, aMesh, aX, aY, aZ);
}

function FreeformMeshAddBinormal(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshAddBinormal, aFf, aMesh, aX, aY, aZ);
}

function FreeformMeshFaceGroupAddTriangle(aFf, aMesh, aFg, aI1, aI2, aI3) {
	return external_call(global._FreeformMeshFaceGroupAddTriangle, aFf, aMesh, aFg, aI1, aI2, aI3);
}

function FreeformMeshGenNormals(aFf, aMesh) {
	return external_call(global._FreeformMeshGenNormals, aFf, aMesh);
}

function FreeformMeshGenTangents(aFf, aMesh) {
	return external_call(global._FreeformMeshGenTangents, aFf, aMesh);
}

function FreeformMeshVerticesCount(aFf, aMesh) {
	return external_call(global._FreeformMeshVerticesCount, aFf, aMesh);
}

function FreeformMeshTranslate(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshTranslate, aFf, aMesh, aX, aY, aZ);
}

function FreeformMeshRotate(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshRotate, aFf, aMesh, aX, aY, aZ);
}

function FreeformMeshScale(aFf, aMesh, aX, aY, aZ) {
	return external_call(global._FreeformMeshScale, aFf, aMesh, aX, aY, aZ);
}

function FreeformSave(aFf, aFilename) {
	return external_call(global._FreeformSave, aFf, aFilename);
}

function FreeformMeshGetVertex(aFf, aMesh, aV, aIndex) {
	return external_call(global._FreeformMeshGetVertex, aFf, aMesh, aV, aIndex);
}

function FreeformMeshGetNormal(aFf, aMesh, aN, aIndex) {
	return external_call(global._FreeformMeshGetNormal, aFf, aMesh, aN, aIndex);
}

function FreeformMeshGetTexCoord(aFf, aMesh, aT, aIndex) {
	return external_call(global._FreeformMeshGetTexCoord, aFf, aMesh, aT, aIndex);
}

function FreeformMeshGetSecondTexCoord(aFf, aMesh, aT, aIndex) {
	return external_call(global._FreeformMeshGetSecondTexCoord, aFf, aMesh, aT, aIndex);
}

function FreeformMeshGetTangent(aFf, aMesh, aT, aIndex) {
	return external_call(global._FreeformMeshGetTangent, aFf, aMesh, aT, aIndex);
}

function FreeformMeshGetBinormal(aFf, aMesh, aB, aIndex) {
	return external_call(global._FreeformMeshGetBinormal, aFf, aMesh, aB, aIndex);
}

function FreeformMeshFaceGroupGetIndex(aFf, aMesh, aFg, aIndex) {
	return external_call(global._FreeformMeshFaceGroupGetIndex, aFf, aMesh, aFg, aIndex);
}

function FreeformMeshSetVertex(aFf, aMesh, aV, aX, aY, aZ) {
	return external_call(global._FreeformMeshSetVertex, aFf, aMesh, aV, aX, aY, aZ);
}

function FreeformMeshSetNormal(aFf, aMesh, aN, aX, aY, aZ) {
	return external_call(global._FreeformMeshSetNormal, aFf, aMesh, aN, aX, aY, aZ);
}

function FreeformMeshSetTexCoord(aFf, aMesh, aT, aU, aV) {
	return external_call(global._FreeformMeshSetTexCoord, aFf, aMesh, aT, aU, aV);
}

function FreeformMeshSetSecondTexCoord(aFf, aMesh, aT, aU, aV) {
	return external_call(global._FreeformMeshSetSecondTexCoord, aFf, aMesh, aT, aU, aV);
}

function FreeformMeshSetTangent(aFf, aMesh, aT, aX, aY, aZ) {
	return external_call(global._FreeformMeshSetTangent, aFf, aMesh, aT, aX, aY, aZ);
}

function FreeformMeshSetBinormal(aFf, aMesh, aB, aX, aY, aZ) {
	return external_call(global._FreeformMeshSetBinormal, aFf, aMesh, aB, aX, aY, aZ);
}

function FreeformMeshFaceGroupSetIndex(aFf, aMesh, aFg, aIndex, aI) {
	return external_call(global._FreeformMeshFaceGroupSetIndex, aFf, aMesh, aFg, aIndex, aI);
}

function FreeformBuildOctree(aFf) {
	return external_call(global._FreeformBuildOctree, aFf);
}

function FreeformMeshFaceGroupGetLightmapIndex(aFf, aMesh, aFg) {
	return external_call(global._FreeformMeshFaceGroupGetLightmapIndex, aFf, aMesh, aFg);
}

function FreeformMeshFaceGroupSetLightmapIndex(aFf, aMesh, aFg, aIndex) {
	return external_call(global._FreeformMeshFaceGroupSetLightmapIndex, aFf, aMesh, aFg, aIndex);
}

function FreeformSetMaterialLibraries(aFf, aMatlib, aLmmatlib) {
	return external_call(global._FreeformSetMaterialLibraries, aFf, aMatlib, aLmmatlib);
}

function GridCreate(aX, aY, aZ, aStep, aParent) {
	return external_call(global._GridCreate, aX, aY, aZ, aStep, aParent);
}

function GridSetLineStyle(aGrid, aMode) {
	return external_call(global._GridSetLineStyle, aGrid, aMode);
}

function GridSetLineSmoothing(aGrid, aMode) {
	return external_call(global._GridSetLineSmoothing, aGrid, aMode);
}

function GridSetParts(aGrid, aMode) {
	return external_call(global._GridSetParts, aGrid, aMode);
}

function GridSetColor(aGrid, aColor, aAlpha) {
	return external_call(global._GridSetColor, aGrid, aColor, aAlpha);
}

function GridSetSize(aGrid, aSize) {
	return external_call(global._GridSetSize, aGrid, aSize);
}

function GridSetPattern(aGrid, aPattern) {
	return external_call(global._GridSetPattern, aGrid, aPattern);
}

function GridSetTile(aGrid, aX, aY, aZ) {
	return external_call(global._GridSetTile, aGrid, aX, aY, aZ);
}

function GridSetStep(aGrid, aStep) {
	return external_call(global._GridSetStep, aGrid, aStep);
}

function HUDShapeRectangleCreate(aW, aH, aParent) {
	return external_call(global._HUDShapeRectangleCreate, aW, aH, aParent);
}

function HUDShapeCircleCreate(aRadius, aSlices, aStartang, aEndang, aParent) {
	return external_call(global._HUDShapeCircleCreate, aRadius, aSlices, aStartang, aEndang, aParent);
}

function HUDShapeLineCreate(aX1, aY1, aX2, aY2, aParent) {
	return external_call(global._HUDShapeLineCreate, aX1, aY1, aX2, aY2, aParent);
}

function HUDShapeMeshCreate(aParent) {
	return external_call(global._HUDShapeMeshCreate, aParent);
}

function HUDShapeSetRotation(aShape, aAngle) {
	return external_call(global._HUDShapeSetRotation, aShape, aAngle);
}

function HUDShapeRotate(aShape, aAngle) {
	return external_call(global._HUDShapeRotate, aShape, aAngle);
}

function HUDShapeSetColor(aShape, aCol, aAlpha) {
	return external_call(global._HUDShapeSetColor, aShape, aCol, aAlpha);
}

function HUDShapeSetOrigin(aShape, aX, aY) {
	return external_call(global._HUDShapeSetOrigin, aShape, aX, aY);
}

function HUDShapeSetSize(aShape, aW, aH) {
	return external_call(global._HUDShapeSetSize, aShape, aW, aH);
}

function HUDShapeScale(aShape, aU, aV) {
	return external_call(global._HUDShapeScale, aShape, aU, aV);
}

function HUDShapeCircleSetRadius(aShape, aRadius) {
	return external_call(global._HUDShapeCircleSetRadius, aShape, aRadius);
}

function HUDShapeCircleSetSlices(aShape, aSlices) {
	return external_call(global._HUDShapeCircleSetSlices, aShape, aSlices);
}

function HUDShapeCircleSetAngles(aShape, aStartang, aEndang) {
	return external_call(global._HUDShapeCircleSetAngles, aShape, aStartang, aEndang);
}

function HUDShapeLineSetPoints(aShape, aX1, aY1, aX2, aY2) {
	return external_call(global._HUDShapeLineSetPoints, aShape, aX1, aY1, aX2, aY2);
}

function HUDShapeLineSetWidth(aShape, aW) {
	return external_call(global._HUDShapeLineSetWidth, aShape, aW);
}

function HUDShapeMeshAddVertex(aShape, aX, aY, aU, aV) {
	return external_call(global._HUDShapeMeshAddVertex, aShape, aX, aY, aU, aV);
}

function HUDShapeMeshAddTriangle(aShape, aV1, aV2, aV3) {
	return external_call(global._HUDShapeMeshAddTriangle, aShape, aV1, aV2, aV3);
}

function HUDShapeMeshSetVertex(aShape, aIndex, aX, aY) {
	return external_call(global._HUDShapeMeshSetVertex, aShape, aIndex, aX, aY);
}

function HUDShapeMeshSetTexCoord(aShape, aIndex, aU, aV) {
	return external_call(global._HUDShapeMeshSetTexCoord, aShape, aIndex, aU, aV);
}

function MouseSetPosition(aMx, aMy) {
	return external_call(global._MouseSetPosition, aMx, aMy);
}

function MouseGetPositionX() {
	return external_call(global._MouseGetPositionX);
}

function MouseGetPositionY() {
	return external_call(global._MouseGetPositionY);
}

function MouseShowCursor(aMode) {
	return external_call(global._MouseShowCursor, aMode);
}

function KeyIsPressed(aKey) {
	return external_call(global._KeyIsPressed, aKey);
}

function MouseIsPressed(aBtn) {
	return external_call(global._MouseIsPressed, aBtn);
}

function KraftCreate() {
	return external_call(global._KraftCreate);
}

function KraftStep(aKr, aDt) {
	return external_call(global._KraftStep, aKr, aDt);
}

function KraftGetRayHitPosition(aIndex) {
	return external_call(global._KraftGetRayHitPosition, aIndex);
}

function KraftGetRayHitNormal(aIndex) {
	return external_call(global._KraftGetRayHitNormal, aIndex);
}

function KraftCreateRigidBody(aKr, aTyp) {
	return external_call(global._KraftCreateRigidBody, aKr, aTyp);
}

function KraftRigidBodyFinish(aKrb) {
	return external_call(global._KraftRigidBodyFinish, aKrb);
}

function KraftRigidBodySetGravity(aKrb, aX, aY, aZ, aScale) {
	return external_call(global._KraftRigidBodySetGravity, aKrb, aX, aY, aZ, aScale);
}

function KraftRigidBodySetPosition(aKrb, aX, aY, aZ) {
	return external_call(global._KraftRigidBodySetPosition, aKrb, aX, aY, aZ);
}

function KraftRigidBodyGetPosition(aKrb, aIndex) {
	return external_call(global._KraftRigidBodyGetPosition, aKrb, aIndex);
}

function KraftRigidBodySetLinearVelocity(aKrb, aX, aY, aZ) {
	return external_call(global._KraftRigidBodySetLinearVelocity, aKrb, aX, aY, aZ);
}

function KraftRigidBodyGetLinearVelocity(aKrb, aIndex) {
	return external_call(global._KraftRigidBodyGetLinearVelocity, aKrb, aIndex);
}

function KraftRigidBodySetRotation(aKrb, aX, aY, aZ) {
	return external_call(global._KraftRigidBodySetRotation, aKrb, aX, aY, aZ);
}

function KraftRigidBodyGetDirection(aKrb, aIndex) {
	return external_call(global._KraftRigidBodyGetDirection, aKrb, aIndex);
}

function KraftRigidBodyGetUp(aKrb, aIndex) {
	return external_call(global._KraftRigidBodyGetUp, aKrb, aIndex);
}

function KraftRigidBodyGetRight(aKrb, aIndex) {
	return external_call(global._KraftRigidBodyGetRight, aKrb, aIndex);
}

function KraftRigidBodySetAngularVelocity(aKrb, aX, aY, aZ) {
	return external_call(global._KraftRigidBodySetAngularVelocity, aKrb, aX, aY, aZ);
}

function KraftRigidBodyGetAngularVelocity(aKrb, aIndex) {
	return external_call(global._KraftRigidBodyGetAngularVelocity, aKrb, aIndex);
}

function KraftRigidBodyAddForce(aKrb, aX, aY, aZ) {
	return external_call(global._KraftRigidBodyAddForce, aKrb, aX, aY, aZ);
}

function KraftRigidBodyAddForceAtPos(aKrb, aX, aY, aZ, aPx, aPy, aPz) {
	return external_call(global._KraftRigidBodyAddForceAtPos, aKrb, aX, aY, aZ, aPx, aPy, aPz);
}

function KraftRigidBodyAddRelForce(aKrb, aX, aY, aZ) {
	return external_call(global._KraftRigidBodyAddRelForce, aKrb, aX, aY, aZ);
}

function KraftRayCast(aKr, aX, aY, aZ, aDx, aDy, aDz, aMaxtime) {
	return external_call(global._KraftRayCast, aKr, aX, aY, aZ, aDx, aDy, aDz, aMaxtime);
}

function KraftObjectSetRigidBody(aObj, aKrb) {
	return external_call(global._KraftObjectSetRigidBody, aObj, aKrb);
}

function KraftCreateShapeSphere(aRbody, aRadius) {
	return external_call(global._KraftCreateShapeSphere, aRbody, aRadius);
}

function KraftCreateShapeBox(aRbody, aX, aY, aZ) {
	return external_call(global._KraftCreateShapeBox, aRbody, aX, aY, aZ);
}

function KraftCreateShapePlane(aRbody, aX, aY, aZ, aD) {
	return external_call(global._KraftCreateShapePlane, aRbody, aX, aY, aZ, aD);
}

function KraftCreateShapeCapsule(aRbody, aRadius, aHeight) {
	return external_call(global._KraftCreateShapeCapsule, aRbody, aRadius, aHeight);
}

function KraftCreateShapeMesh(aRbody, aFf) {
	return external_call(global._KraftCreateShapeMesh, aRbody, aFf);
}

function KraftShapeSetDensity(aShape, aDensity) {
	return external_call(global._KraftShapeSetDensity, aShape, aDensity);
}

function KraftShapeSetFriction(aShape, aFriction) {
	return external_call(global._KraftShapeSetFriction, aShape, aFriction);
}

function KraftShapeSetRestitution(aShape, aRest) {
	return external_call(global._KraftShapeSetRestitution, aShape, aRest);
}

function KraftShapeSetPosition(aShape, aX, aY, aZ) {
	return external_call(global._KraftShapeSetPosition, aShape, aX, aY, aZ);
}

function KraftShapeGetPosition(aShape, aIndex) {
	return external_call(global._KraftShapeGetPosition, aShape, aIndex);
}

function KraftShapeSetRayCastable(aShape, aMode) {
	return external_call(global._KraftShapeSetRayCastable, aShape, aMode);
}

function KraftCreateJointDistance(aRbody1, aRbody2) {
	return external_call(global._KraftCreateJointDistance, aRbody1, aRbody2);
}

function KraftCreateJointRope(aRbody1, aRbody2, aMaxlength) {
	return external_call(global._KraftCreateJointRope, aRbody1, aRbody2, aMaxlength);
}

function KraftCreateJointBallSocket(aRbody1, aRbody2) {
	return external_call(global._KraftCreateJointBallSocket, aRbody1, aRbody2);
}

function KraftCreateJointFixed(aRbody1, aRbody2) {
	return external_call(global._KraftCreateJointFixed, aRbody1, aRbody2);
}

function KraftCreateJointHinge(aRbody1, aRbody2) {
	return external_call(global._KraftCreateJointHinge, aRbody1, aRbody2);
}

function KraftJointSetAnchor1(aJoint, aX, aY, aZ) {
	return external_call(global._KraftJointSetAnchor1, aJoint, aX, aY, aZ);
}

function KraftJointSetAnchor2(aJoint, aX, aY, aZ) {
	return external_call(global._KraftJointSetAnchor2, aJoint, aX, aY, aZ);
}

function KraftJointSetHingeAxis1(aJoint, aX, aY, aZ) {
	return external_call(global._KraftJointSetHingeAxis1, aJoint, aX, aY, aZ);
}

function KraftJointSetHingeAxis2(aJoint, aX, aY, aZ) {
	return external_call(global._KraftJointSetHingeAxis2, aJoint, aX, aY, aZ);
}

function LensflareCreate(aParent) {
	return external_call(global._LensflareCreate, aParent);
}

function LensflareSetSize(aLensflare, aSize) {
	return external_call(global._LensflareSetSize, aLensflare, aSize);
}

function LensflareSetSeed(aLensflare, aSeed) {
	return external_call(global._LensflareSetSeed, aLensflare, aSeed);
}

function LensflareSetSqueeze(aLensflare, aSqueeze) {
	return external_call(global._LensflareSetSqueeze, aLensflare, aSqueeze);
}

function LensflareSetStreaks(aLensflare, aStreaks) {
	return external_call(global._LensflareSetStreaks, aLensflare, aStreaks);
}

function LensflareSetStreakWidth(aLensflare, aWidth) {
	return external_call(global._LensflareSetStreakWidth, aLensflare, aWidth);
}

function LensflareSetSecs(aLensflare, aSecs) {
	return external_call(global._LensflareSetSecs, aLensflare, aSecs);
}

function LensflareSetResolution(aLensflare, aRes) {
	return external_call(global._LensflareSetResolution, aLensflare, aRes);
}

function LensflareSetElements(aLensflare, aGlow, aRing, aStreaks, aRays, aSecs) {
	return external_call(global._LensflareSetElements, aLensflare, aGlow, aRing, aStreaks, aRays, aSecs);
}

function LensflareSetGradients(aLensflare, aInd, aColor1, aAlpha1, aColor2, aAlpha2) {
	return external_call(global._LensflareSetGradients, aLensflare, aInd, aColor1, aAlpha1, aColor2, aAlpha2);
}

function LightCreate(aLs, aParent) {
	return external_call(global._LightCreate, aLs, aParent);
}

function LightSetAmbientColor(aLight, aColor) {
	return external_call(global._LightSetAmbientColor, aLight, aColor);
}

function LightSetDiffuseColor(aLight, aColor) {
	return external_call(global._LightSetDiffuseColor, aLight, aColor);
}

function LightSetSpecularColor(aLight, aColor) {
	return external_call(global._LightSetSpecularColor, aLight, aColor);
}

function LightSetAttenuation(aLight, aAconst, aAlinear, aAquadratic) {
	return external_call(global._LightSetAttenuation, aLight, aAconst, aAlinear, aAquadratic);
}

function LightSetShining(aLight, aMode) {
	return external_call(global._LightSetShining, aLight, aMode);
}

function LightSetSpotCutoff(aLight, aCutoff) {
	return external_call(global._LightSetSpotCutoff, aLight, aCutoff);
}

function LightSetSpotExponent(aLight, aExp) {
	return external_call(global._LightSetSpotExponent, aLight, aExp);
}

function LightSetSpotDirection(aLight, aX, aY, aZ) {
	return external_call(global._LightSetSpotDirection, aLight, aX, aY, aZ);
}

function LightSetStyle(aLight, aLs) {
	return external_call(global._LightSetStyle, aLight, aLs);
}

function LightGetColor(aLight, aIndex) {
	return external_call(global._LightGetColor, aLight, aIndex);
}

function LightGetAttenuation(aLight, aIndex) {
	return external_call(global._LightGetAttenuation, aLight, aIndex);
}

function LightGetShining(aLight) {
	return external_call(global._LightGetShining, aLight);
}

function LightFXCreate(aObj) {
	return external_call(global._LightFXCreate, aObj);
}

function LinesCreate(aParent) {
	return external_call(global._LinesCreate, aParent);
}

function LinesAddNode(aLines, aX, aY, aZ) {
	return external_call(global._LinesAddNode, aLines, aX, aY, aZ);
}

function LinesDeleteNode(aLines, aInd) {
	return external_call(global._LinesDeleteNode, aLines, aInd);
}

function LinesSetNode(aLines, aInd, aX, aY, aZ) {
	return external_call(global._LinesSetNode, aLines, aInd, aX, aY, aZ);
}

function LinesSetSize(aLines, aLinewidth, aNodesize) {
	return external_call(global._LinesSetSize, aLines, aLinewidth, aNodesize);
}

function LinesSetSplineMode(aLines, aLsm) {
	return external_call(global._LinesSetSplineMode, aLines, aLsm);
}

function LinesSetNodesAspect(aLines, aLna) {
	return external_call(global._LinesSetNodesAspect, aLines, aLna);
}

function LinesSetDivision(aLines, aDivision) {
	return external_call(global._LinesSetDivision, aLines, aDivision);
}

function MaterialLibraryCreate() {
	return external_call(global._MaterialLibraryCreate);
}

function MaterialLibraryActivate(aMlib) {
	return external_call(global._MaterialLibraryActivate, aMlib);
}

function MaterialLibrarySetTexturePaths(aMlb, aPath) {
	return external_call(global._MaterialLibrarySetTexturePaths, aMlb, aPath);
}

function MaterialLibraryClear(aMlb) {
	return external_call(global._MaterialLibraryClear, aMlb);
}

function MaterialLibraryDeleteUnused(aMlb) {
	return external_call(global._MaterialLibraryDeleteUnused, aMlb);
}

function MaterialLibraryHasMaterial(aMatlib, aName) {
	return external_call(global._MaterialLibraryHasMaterial, aMatlib, aName);
}

function MaterialLibraryLoadScript(aMatlib, aFilename) {
	return external_call(global._MaterialLibraryLoadScript, aMatlib, aFilename);
}

function MaterialCreate(aMtrl, aFname) {
	return external_call(global._MaterialCreate, aMtrl, aFname);
}

function MaterialDestroy(aMtrl) {
	return external_call(global._MaterialDestroy, aMtrl);
}

function MaterialAddCubeMap(aMtrl) {
	return external_call(global._MaterialAddCubeMap, aMtrl);
}

function MaterialCubeMapLoadImage(aMtrl, aTexture, aInd) {
	return external_call(global._MaterialCubeMapLoadImage, aMtrl, aTexture, aInd);
}

function MaterialCubeMapGenerate(aMtrl, aRes) {
	return external_call(global._MaterialCubeMapGenerate, aMtrl, aRes);
}

function MaterialCubeMapFromScene(aMtrl, aViewer, aCamera, aRes) {
	return external_call(global._MaterialCubeMapFromScene, aMtrl, aViewer, aCamera, aRes);
}

function MaterialSetName(aMtrl, aName) {
	return external_call(global._MaterialSetName, aMtrl, aName);
}

function MaterialSetShininess(aMtrl, aShin) {
	return external_call(global._MaterialSetShininess, aMtrl, aShin);
}

function MaterialSetAmbientColor(aMtrl, aCol, aAlpha) {
	return external_call(global._MaterialSetAmbientColor, aMtrl, aCol, aAlpha);
}

function MaterialSetDiffuseColor(aMtrl, aCol, aAlpha) {
	return external_call(global._MaterialSetDiffuseColor, aMtrl, aCol, aAlpha);
}

function MaterialSetSpecularColor(aMtrl, aCol, aAlpha) {
	return external_call(global._MaterialSetSpecularColor, aMtrl, aCol, aAlpha);
}

function MaterialSetEmissionColor(aMtrl, aCol, aAlpha) {
	return external_call(global._MaterialSetEmissionColor, aMtrl, aCol, aAlpha);
}

function MaterialGetColor(aMtrl, aIndex) {
	return external_call(global._MaterialGetColor, aMtrl, aIndex);
}

function MaterialGetAlpha(aMtrl, aIndex) {
	return external_call(global._MaterialGetAlpha, aMtrl, aIndex);
}

function MaterialSetBlendingMode(aMtrl, aBm) {
	return external_call(global._MaterialSetBlendingMode, aMtrl, aBm);
}

function MaterialSetTextureMode(aMtrl, aTm) {
	return external_call(global._MaterialSetTextureMode, aMtrl, aTm);
}

function MaterialSetTextureMappingMode(aMtrl, aTmm) {
	return external_call(global._MaterialSetTextureMappingMode, aMtrl, aTmm);
}

function MaterialSetPolygonMode(aMtrl, aPm) {
	return external_call(global._MaterialSetPolygonMode, aMtrl, aPm);
}

function MaterialSetTextureImageAlpha(aMtrl, aTia) {
	return external_call(global._MaterialSetTextureImageAlpha, aMtrl, aTia);
}

function MaterialSetTextureScale(aMtrl, aU, aV) {
	return external_call(global._MaterialSetTextureScale, aMtrl, aU, aV);
}

function MaterialSetTextureOffset(aMtrl, aU, aV) {
	return external_call(global._MaterialSetTextureOffset, aMtrl, aU, aV);
}

function MaterialSetTextureFilter(aMtrl, aMi, aMa) {
	return external_call(global._MaterialSetTextureFilter, aMtrl, aMi, aMa);
}

function MaterialEnableTexture(aMtrl, aMode) {
	return external_call(global._MaterialEnableTexture, aMtrl, aMode);
}

function MaterialGetCount() {
	return external_call(global._MaterialGetCount);
}

function MaterialGetName(aInd) {
	return external_call(global._MaterialGetName, aInd);
}

function MaterialSetFaceCulling(aMtrl, aFc) {
	return external_call(global._MaterialSetFaceCulling, aMtrl, aFc);
}

function MaterialSetSecondTexture(aMtrl, aMtrl2) {
	return external_call(global._MaterialSetSecondTexture, aMtrl, aMtrl2);
}

function MaterialSetTextureFormat(aMtrl, aTf) {
	return external_call(global._MaterialSetTextureFormat, aMtrl, aTf);
}

function MaterialSetTextureCompression(aMtrl, aTc) {
	return external_call(global._MaterialSetTextureCompression, aMtrl, aTc);
}

function MaterialTextureRequiredMemory(aMtrl) {
	return external_call(global._MaterialTextureRequiredMemory, aMtrl);
}

function MaterialSetFilteringQuality(aMtrl, aTf) {
	return external_call(global._MaterialSetFilteringQuality, aMtrl, aTf);
}

function MaterialAddTextureEx(aMtrl, aTex) {
	return external_call(global._MaterialAddTextureEx, aMtrl, aTex);
}

function MaterialTextureExClear(aMtrl) {
	return external_call(global._MaterialTextureExClear, aMtrl);
}

function MaterialTextureExDelete(aMtrl, aInd) {
	return external_call(global._MaterialTextureExDelete, aMtrl, aInd);
}

function MaterialSetShader(aMtrl, aShd) {
	return external_call(global._MaterialSetShader, aMtrl, aShd);
}

function MaterialSaveTexture(aMtrl, aFname) {
	return external_call(global._MaterialSaveTexture, aMtrl, aFname);
}

function MaterialSetOptions(aMtrl, aOp1, aOp2) {
	return external_call(global._MaterialSetOptions, aMtrl, aOp1, aOp2);
}

function MaterialSetTextureWrap(aMtrl, aWrap) {
	return external_call(global._MaterialSetTextureWrap, aMtrl, aWrap);
}

function MaterialGenTexture(aMtrl, aW, aH) {
	return external_call(global._MaterialGenTexture, aMtrl, aW, aH);
}

function MaterialSetTexture(aMtrl, aMtrl2) {
	return external_call(global._MaterialSetTexture, aMtrl, aMtrl2);
}

function MaterialGetTextureWidth(aMtrl) {
	return external_call(global._MaterialGetTextureWidth, aMtrl);
}

function MaterialGetTextureHeight(aMtrl) {
	return external_call(global._MaterialGetTextureHeight, aMtrl);
}

function MaterialLoadTexture(aMtrl, aFilename) {
	return external_call(global._MaterialLoadTexture, aMtrl, aFilename);
}

function MaterialLoadTextureEx(aMtrl, aFilename, aIndex) {
	return external_call(global._MaterialLoadTextureEx, aMtrl, aFilename, aIndex);
}

function MaterialSetTextureEx(aMtrl, aMtrl2, aIndex) {
	return external_call(global._MaterialSetTextureEx, aMtrl, aMtrl2, aIndex);
}

function MaterialGenTextureEx(aMtrl, aIndex, aW, aH) {
	return external_call(global._MaterialGenTextureEx, aMtrl, aIndex, aW, aH);
}

function MaterialEnableTextureEx(aMtrl, aIndex, aMode) {
	return external_call(global._MaterialEnableTextureEx, aMtrl, aIndex, aMode);
}

function MaterialHasTextureEx(aMtrl, aIndex) {
	return external_call(global._MaterialHasTextureEx, aMtrl, aIndex);
}

function MaterialNoiseCreate(aMtrl) {
	return external_call(global._MaterialNoiseCreate, aMtrl);
}

function MaterialNoiseSetDimensions(aMtrl, aW, aH) {
	return external_call(global._MaterialNoiseSetDimensions, aMtrl, aW, aH);
}

function MaterialNoiseAnimate(aMtrl, aSpeed) {
	return external_call(global._MaterialNoiseAnimate, aMtrl, aSpeed);
}

function MaterialNoiseSetMinCut(aMtrl, aM) {
	return external_call(global._MaterialNoiseSetMinCut, aMtrl, aM);
}

function MaterialNoiseSetSharpness(aMtrl, aS) {
	return external_call(global._MaterialNoiseSetSharpness, aMtrl, aS);
}

function MaterialNoiseSetSeamless(aMtrl, aMode) {
	return external_call(global._MaterialNoiseSetSeamless, aMtrl, aMode);
}

function MaterialNoiseRandomSeed(aMtrl, aS) {
	return external_call(global._MaterialNoiseRandomSeed, aMtrl, aS);
}

function MaterialCullFrontFaces(aMtrl, aCulff) {
	return external_call(global._MaterialCullFrontFaces, aMtrl, aCulff);
}

function MaterialSetZWrite(aMtrl, aZwrite) {
	return external_call(global._MaterialSetZWrite, aMtrl, aZwrite);
}

function MaterialSetTextureExFromLibrary(aMaterial1, aMatlib2, aMaterial2, aIndex) {
	return external_call(global._MaterialSetTextureExFromLibrary, aMaterial1, aMatlib2, aMaterial2, aIndex);
}

function MaterialGetNameFromLibrary(aMatlib, aIndex) {
	return external_call(global._MaterialGetNameFromLibrary, aMatlib, aIndex);
}

function MemoryViewerCreate(aW, aH) {
	return external_call(global._MemoryViewerCreate, aW, aH);
}

function MemoryViewerSetCamera(aMview, aCam) {
	return external_call(global._MemoryViewerSetCamera, aMview, aCam);
}

function MemoryViewerRender(aMview) {
	return external_call(global._MemoryViewerRender, aMview);
}

function MemoryViewerSetViewport(aMview, aX, aY, aW, aH) {
	return external_call(global._MemoryViewerSetViewport, aMview, aX, aY, aW, aH);
}

function MemoryViewerCopyToTexture(aMview, aMatname) {
	return external_call(global._MemoryViewerCopyToTexture, aMview, aMatname);
}

function MirrorCreate(aTarget, aParent) {
	return external_call(global._MirrorCreate, aTarget, aParent);
}

function MirrorSetObject(aMirror, aTarget) {
	return external_call(global._MirrorSetObject, aMirror, aTarget);
}

function MirrorSetOptions(aMirror, aStencil, aOpaque, aPlaneclipping, aClearzbuffer) {
	return external_call(global._MirrorSetOptions, aMirror, aStencil, aOpaque, aPlaneclipping, aClearzbuffer);
}

function MirrorSetShape(aMirror, aMs) {
	return external_call(global._MirrorSetShape, aMirror, aMs);
}

function MirrorSetDiskOptions(aMirror, aRadius, aSlices) {
	return external_call(global._MirrorSetDiskOptions, aMirror, aRadius, aSlices);
}

function MovementCreate(aObj) {
	return external_call(global._MovementCreate, aObj);
}

function MovementStart(aMovement) {
	return external_call(global._MovementStart, aMovement);
}

function MovementStop(aMovement) {
	return external_call(global._MovementStop, aMovement);
}

function MovementAutoStartNextPath(aMovement, aMode) {
	return external_call(global._MovementAutoStartNextPath, aMovement, aMode);
}

function MovementAddPath(aMovement) {
	return external_call(global._MovementAddPath, aMovement);
}

function MovementSetActivePath(aMovement, aInd) {
	return external_call(global._MovementSetActivePath, aMovement, aInd);
}

function MovementPathSetSplineMode(aPath, aLsm) {
	return external_call(global._MovementPathSetSplineMode, aPath, aLsm);
}

function MovementPathAddNode(aPath) {
	return external_call(global._MovementPathAddNode, aPath);
}

function MovementPathNodeSetPosition(aNode, aX, aY, aZ) {
	return external_call(global._MovementPathNodeSetPosition, aNode, aX, aY, aZ);
}

function MovementPathNodeSetRotation(aNode, aX, aY, aZ) {
	return external_call(global._MovementPathNodeSetRotation, aNode, aX, aY, aZ);
}

function MovementPathNodeSetSpeed(aNode, aSpeed) {
	return external_call(global._MovementPathNodeSetSpeed, aNode, aSpeed);
}

function MovementPathShow(aPat, aVis) {
	return external_call(global._MovementPathShow, aPat, aVis);
}

function MovementPathSetLoop(aPat, aLoopn) {
	return external_call(global._MovementPathSetLoop, aPat, aLoopn);
}

function MovementPathDeleteNode(aPat, aNode) {
	return external_call(global._MovementPathDeleteNode, aPat, aNode);
}

function NavigatorCreate() {
	return external_call(global._NavigatorCreate);
}

function NavigatorSetObject(aNavigator, aObj) {
	return external_call(global._NavigatorSetObject, aNavigator, aObj);
}

function NavigatorSetUseVirtualUp(aNavigator, aMode) {
	return external_call(global._NavigatorSetUseVirtualUp, aNavigator, aMode);
}

function NavigatorSetVirtualUp(aNavigator, aX, aY, aZ) {
	return external_call(global._NavigatorSetVirtualUp, aNavigator, aX, aY, aZ);
}

function NavigatorTurnHorizontal(aNavigator, aAngle) {
	return external_call(global._NavigatorTurnHorizontal, aNavigator, aAngle);
}

function NavigatorTurnVertical(aNavigator, aAngle) {
	return external_call(global._NavigatorTurnVertical, aNavigator, aAngle);
}

function NavigatorMoveForward(aNavigator, aSpd) {
	return external_call(global._NavigatorMoveForward, aNavigator, aSpd);
}

function NavigatorStrafeHorizontal(aNavigator, aSpd) {
	return external_call(global._NavigatorStrafeHorizontal, aNavigator, aSpd);
}

function NavigatorStrafeVertical(aNavigator, aSpd) {
	return external_call(global._NavigatorStrafeVertical, aNavigator, aSpd);
}

function NavigatorStraighten(aNavigator) {
	return external_call(global._NavigatorStraighten, aNavigator);
}

function NavigatorFlyForward(aNavigator, aSpd) {
	return external_call(global._NavigatorFlyForward, aNavigator, aSpd);
}

function NavigatorMoveUpWhenMovingForward(aNavigator, aMode) {
	return external_call(global._NavigatorMoveUpWhenMovingForward, aNavigator, aMode);
}

function NavigatorInvertHorizontalWhenUpsideDown(aNavigator, aMode) {
	return external_call(global._NavigatorInvertHorizontalWhenUpsideDown, aNavigator, aMode);
}

function NavigatorSetAngleLock(aNavigator, aMode) {
	return external_call(global._NavigatorSetAngleLock, aNavigator, aMode);
}

function NavigatorSetAngles(aNavigator, aMinangle, aMaxangle) {
	return external_call(global._NavigatorSetAngles, aNavigator, aMinangle, aMaxangle);
}

function ObjectHide(aObj) {
	return external_call(global._ObjectHide, aObj);
}

function ObjectShow(aObj) {
	return external_call(global._ObjectShow, aObj);
}

function ObjectIsVisible(aObj) {
	return external_call(global._ObjectIsVisible, aObj);
}

function ObjectDestroy(aObj) {
	return external_call(global._ObjectDestroy, aObj);
}

function ObjectDestroyChildren(aObj) {
	return external_call(global._ObjectDestroyChildren, aObj);
}

function ObjectSetPosition(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetPosition, aObj, aX, aY, aZ);
}

function ObjectGetPosition(aObj, aInd) {
	return external_call(global._ObjectGetPosition, aObj, aInd);
}

function ObjectGetAbsolutePosition(aObj, aInd) {
	return external_call(global._ObjectGetAbsolutePosition, aObj, aInd);
}

function ObjectSetPositionOfObject(aObj1, aObj2) {
	return external_call(global._ObjectSetPositionOfObject, aObj1, aObj2);
}

function ObjectAlignWithObject(aObj1, aObj2) {
	return external_call(global._ObjectAlignWithObject, aObj1, aObj2);
}

function ObjectSetPositionX(aObj, aPosx) {
	return external_call(global._ObjectSetPositionX, aObj, aPosx);
}

function ObjectSetPositionY(aObj, aPosy) {
	return external_call(global._ObjectSetPositionY, aObj, aPosy);
}

function ObjectSetPositionZ(aObj, aPosz) {
	return external_call(global._ObjectSetPositionZ, aObj, aPosz);
}

function ObjectGetPositionX(aObj) {
	return external_call(global._ObjectGetPositionX, aObj);
}

function ObjectGetPositionY(aObj) {
	return external_call(global._ObjectGetPositionY, aObj);
}

function ObjectGetPositionZ(aObj) {
	return external_call(global._ObjectGetPositionZ, aObj);
}

function ObjectSetAbsolutePosition(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetAbsolutePosition, aObj, aX, aY, aZ);
}

function ObjectSetDirection(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetDirection, aObj, aX, aY, aZ);
}

function ObjectGetDirection(aObj, aInd) {
	return external_call(global._ObjectGetDirection, aObj, aInd);
}

function ObjectSetAbsoluteDirection(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetAbsoluteDirection, aObj, aX, aY, aZ);
}

function ObjectGetAbsoluteDirection(aObj, aInd) {
	return external_call(global._ObjectGetAbsoluteDirection, aObj, aInd);
}

function ObjectGetPitch(aObj) {
	return external_call(global._ObjectGetPitch, aObj);
}

function ObjectGetTurn(aObj) {
	return external_call(global._ObjectGetTurn, aObj);
}

function ObjectGetRoll(aObj) {
	return external_call(global._ObjectGetRoll, aObj);
}

function ObjectSetRotation(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetRotation, aObj, aX, aY, aZ);
}

function ObjectMove(aObj, aSpd) {
	return external_call(global._ObjectMove, aObj, aSpd);
}

function ObjectLift(aObj, aSpd) {
	return external_call(global._ObjectLift, aObj, aSpd);
}

function ObjectStrafe(aObj, aSpd) {
	return external_call(global._ObjectStrafe, aObj, aSpd);
}

function ObjectTranslate(aObj, aX, aY, aZ) {
	return external_call(global._ObjectTranslate, aObj, aX, aY, aZ);
}

function ObjectRotate(aObj, aP, aT, aR) {
	return external_call(global._ObjectRotate, aObj, aP, aT, aR);
}

function ObjectScale(aObj, aX, aY, aZ) {
	return external_call(global._ObjectScale, aObj, aX, aY, aZ);
}

function ObjectSetScale(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetScale, aObj, aX, aY, aZ);
}

function ObjectGetScale(aObj, aInd) {
	return external_call(global._ObjectGetScale, aObj, aInd);
}

function ObjectSetUpVector(aObj, aX, aY, aZ) {
	return external_call(global._ObjectSetUpVector, aObj, aX, aY, aZ);
}

function ObjectPointToObject(aObj1, aObj2) {
	return external_call(global._ObjectPointToObject, aObj1, aObj2);
}

function ObjectShowAxes(aObj, aMode) {
	return external_call(global._ObjectShowAxes, aObj, aMode);
}

function ObjectGetGroundHeight(aObj, aTarget) {
	return external_call(global._ObjectGetGroundHeight, aObj, aTarget);
}

function ObjectSceneRaycast(aObj, aTarget) {
	return external_call(global._ObjectSceneRaycast, aObj, aTarget);
}

function ObjectRaycast(aObj, aTarget) {
	return external_call(global._ObjectRaycast, aObj, aTarget);
}

function ObjectGetCollisionPosition(aInd) {
	return external_call(global._ObjectGetCollisionPosition, aInd);
}

function ObjectGetCollisionNormal(aInd) {
	return external_call(global._ObjectGetCollisionNormal, aInd);
}

function ObjectSetMaterial(aObj, aMat) {
	return external_call(global._ObjectSetMaterial, aObj, aMat);
}

function ObjectGetMaterial(aObj) {
	return external_call(global._ObjectGetMaterial, aObj);
}

function ObjectGetDistance(aObj, aTarget) {
	return external_call(global._ObjectGetDistance, aObj, aTarget);
}

function ObjectCheckCubeVsCube(aObj1, aObj2) {
	return external_call(global._ObjectCheckCubeVsCube, aObj1, aObj2);
}

function ObjectCheckSphereVsSphere(aObj1, aObj2) {
	return external_call(global._ObjectCheckSphereVsSphere, aObj1, aObj2);
}

function ObjectCheckSphereVsCube(aObj1, aObj2) {
	return external_call(global._ObjectCheckSphereVsCube, aObj1, aObj2);
}

function ObjectCheckCubeVsFace(aObj1, aObj2) {
	return external_call(global._ObjectCheckCubeVsFace, aObj1, aObj2);
}

function ObjectCheckFaceVsFace(aObj1, aObj2) {
	return external_call(global._ObjectCheckFaceVsFace, aObj1, aObj2);
}

function ObjectIsPointInObject(aObj1, aX, aY, aZ) {
	return external_call(global._ObjectIsPointInObject, aObj1, aX, aY, aZ);
}

function ObjectSetCulling(aObj1, aVc) {
	return external_call(global._ObjectSetCulling, aObj1, aVc);
}

function ObjectSetName(aObj1, aName) {
	return external_call(global._ObjectSetName, aObj1, aName);
}

function ObjectGetName(aObj1) {
	return external_call(global._ObjectGetName, aObj1);
}

function ObjectGetClassName(aObj1) {
	return external_call(global._ObjectGetClassName, aObj1);
}

function ObjectSetTag(aObj1, aTag) {
	return external_call(global._ObjectSetTag, aObj1, aTag);
}

function ObjectGetTag(aObj1) {
	return external_call(global._ObjectGetTag, aObj1);
}

function ObjectGetParent(aObj1) {
	return external_call(global._ObjectGetParent, aObj1);
}

function ObjectGetChildCount(aObj1) {
	return external_call(global._ObjectGetChildCount, aObj1);
}

function ObjectGetChild(aObj1, aInd) {
	return external_call(global._ObjectGetChild, aObj1, aInd);
}

function ObjectGetIndex(aObj1) {
	return external_call(global._ObjectGetIndex, aObj1);
}

function ObjectFindChild(aObj1, aName) {
	return external_call(global._ObjectFindChild, aObj1, aName);
}

function ObjectGetBoundingSphereRadius(aObj1) {
	return external_call(global._ObjectGetBoundingSphereRadius, aObj1);
}

function ObjectGetAbsoluteUp(aObj1, aInd) {
	return external_call(global._ObjectGetAbsoluteUp, aObj1, aInd);
}

function ObjectSetAbsoluteUp(aObj1, aX, aY, aZ) {
	return external_call(global._ObjectSetAbsoluteUp, aObj1, aX, aY, aZ);
}

function ObjectGetAbsoluteRight(aObj1, aInd) {
	return external_call(global._ObjectGetAbsoluteRight, aObj1, aInd);
}

function ObjectGetAbsoluteXVector(aObj1, aInd) {
	return external_call(global._ObjectGetAbsoluteXVector, aObj1, aInd);
}

function ObjectGetAbsoluteYVector(aObj1, aInd) {
	return external_call(global._ObjectGetAbsoluteYVector, aObj1, aInd);
}

function ObjectGetAbsoluteZVector(aObj1, aInd) {
	return external_call(global._ObjectGetAbsoluteZVector, aObj1, aInd);
}

function ObjectGetRight(aObj1, aInd) {
	return external_call(global._ObjectGetRight, aObj1, aInd);
}

function ObjectMoveChildUp(aObj1, aInd) {
	return external_call(global._ObjectMoveChildUp, aObj1, aInd);
}

function ObjectMoveChildDown(aObj1, aInd) {
	return external_call(global._ObjectMoveChildDown, aObj1, aInd);
}

function ObjectSetParent(aObj1, aObj2) {
	return external_call(global._ObjectSetParent, aObj1, aObj2);
}

function ObjectRemoveChild(aObj1, aObj2, aKeep) {
	return external_call(global._ObjectRemoveChild, aObj1, aObj2, aKeep);
}

function ObjectMoveObjectAround(aObj1, aObj2, aP, aT) {
	return external_call(global._ObjectMoveObjectAround, aObj1, aObj2, aP, aT);
}

function ObjectPitch(aObj1, aAngle) {
	return external_call(global._ObjectPitch, aObj1, aAngle);
}

function ObjectTurn(aObj1, aAngle) {
	return external_call(global._ObjectTurn, aObj1, aAngle);
}

function ObjectRoll(aObj1, aAngle) {
	return external_call(global._ObjectRoll, aObj1, aAngle);
}

function ObjectGetUp(aObj1, aInd) {
	return external_call(global._ObjectGetUp, aObj1, aInd);
}

function ObjectRotateAbsolute(aObj1, aX, aY, aZ) {
	return external_call(global._ObjectRotateAbsolute, aObj1, aX, aY, aZ);
}

function ObjectRotateAbsoluteVector(aObj1, aX, aY, aZ, aAngle) {
	return external_call(global._ObjectRotateAbsoluteVector, aObj1, aX, aY, aZ, aAngle);
}

function ObjectSetMatrixColumn(aObj1, aInd, aX, aY, aZ, aW) {
	return external_call(global._ObjectSetMatrixColumn, aObj1, aInd, aX, aY, aZ, aW);
}

function ObjectExportMatrix(aObj1, aObj2) {
	return external_call(global._ObjectExportMatrix, aObj1, aObj2);
}

function ObjectExportAbsoluteMatrix(aObj1, aObj2) {
	return external_call(global._ObjectExportAbsoluteMatrix, aObj1, aObj2);
}

function ObjectCopy(aObj, aParent) {
	return external_call(global._ObjectCopy, aObj, aParent);
}

function ObjectInFrustum(aObj, aViewer) {
	return external_call(global._ObjectInFrustum, aObj, aViewer);
}

function ObjectFindByName(aName) {
	return external_call(global._ObjectFindByName, aName);
}

function ObjectIgnoreDepthBuffer(aObj, aMode) {
	return external_call(global._ObjectIgnoreDepthBuffer, aObj, aMode);
}

function ObjectIsPicked(aObj, aViewer, aX, aY) {
	return external_call(global._ObjectIsPicked, aObj, aViewer, aX, aY);
}

function ObjectHashCreate() {
	return external_call(global._ObjectHashCreate);
}

function ObjectHashSetItem(aHash, aKey, aObj) {
	return external_call(global._ObjectHashSetItem, aHash, aKey, aObj);
}

function ObjectHashGetItem(aHash, aKey) {
	return external_call(global._ObjectHashGetItem, aHash, aKey);
}

function ObjectHashDeleteItem(aHash, aKey) {
	return external_call(global._ObjectHashDeleteItem, aHash, aKey);
}

function ObjectHashGetItemCount(aHash) {
	return external_call(global._ObjectHashGetItemCount, aHash);
}

function ObjectHashClear(aHash) {
	return external_call(global._ObjectHashClear, aHash);
}

function ObjectHashDestroy(aHash) {
	return external_call(global._ObjectHashDestroy, aHash);
}

function OdeManagerCreate() {
	return external_call(global._OdeManagerCreate);
}

function OdeManagerDestroy() {
	return external_call(global._OdeManagerDestroy);
}

function OdeManagerStep(aDt) {
	return external_call(global._OdeManagerStep, aDt);
}

function OdeManagerGetNumContactJoints() {
	return external_call(global._OdeManagerGetNumContactJoints);
}

function OdeManagerSetGravity(aX, aY, aZ) {
	return external_call(global._OdeManagerSetGravity, aX, aY, aZ);
}

function OdeManagerSetSolver(aOsm) {
	return external_call(global._OdeManagerSetSolver, aOsm);
}

function OdeManagerSetIterations(aIterations) {
	return external_call(global._OdeManagerSetIterations, aIterations);
}

function OdeManagerSetMaxContacts(aMaxcontacts) {
	return external_call(global._OdeManagerSetMaxContacts, aMaxcontacts);
}

function OdeManagerSetVisible(aMode) {
	return external_call(global._OdeManagerSetVisible, aMode);
}

function OdeManagerSetGeomColor(aColor) {
	return external_call(global._OdeManagerSetGeomColor, aColor);
}

function OdeWorldSetAutoDisableFlag(aFlag) {
	return external_call(global._OdeWorldSetAutoDisableFlag, aFlag);
}

function OdeWorldSetAutoDisableLinearThreshold(aVelocity) {
	return external_call(global._OdeWorldSetAutoDisableLinearThreshold, aVelocity);
}

function OdeWorldSetAutoDisableAngularThreshold(aVelocity) {
	return external_call(global._OdeWorldSetAutoDisableAngularThreshold, aVelocity);
}

function OdeWorldSetAutoDisableSteps(aSteps) {
	return external_call(global._OdeWorldSetAutoDisableSteps, aSteps);
}

function OdeWorldSetAutoDisableTime(aTime) {
	return external_call(global._OdeWorldSetAutoDisableTime, aTime);
}

function OdeStaticCreate(aObj) {
	return external_call(global._OdeStaticCreate, aObj);
}

function OdeDynamicCreate(aObj) {
	return external_call(global._OdeDynamicCreate, aObj);
}

function OdeTerrainCreate(aTerr) {
	return external_call(global._OdeTerrainCreate, aTerr);
}

function OdeDynamicCalculateMass(aObj) {
	return external_call(global._OdeDynamicCalculateMass, aObj);
}

function OdeDynamicCalibrateCenterOfMass(aObj) {
	return external_call(global._OdeDynamicCalibrateCenterOfMass, aObj);
}

function OdeDynamicAlignObject(aObj) {
	return external_call(global._OdeDynamicAlignObject, aObj);
}

function OdeDynamicEnable(aObj, aMode) {
	return external_call(global._OdeDynamicEnable, aObj, aMode);
}

function OdeDynamicSetAutoDisableFlag(aObj, aMode) {
	return external_call(global._OdeDynamicSetAutoDisableFlag, aObj, aMode);
}

function OdeDynamicSetAutoDisableLinearThreshold(aObj, aVelocity) {
	return external_call(global._OdeDynamicSetAutoDisableLinearThreshold, aObj, aVelocity);
}

function OdeDynamicSetAutoDisableAngularThreshold(aObj, aVelocity) {
	return external_call(global._OdeDynamicSetAutoDisableAngularThreshold, aObj, aVelocity);
}

function OdeDynamicSetAutoDisableSteps(aObj, aSteps) {
	return external_call(global._OdeDynamicSetAutoDisableSteps, aObj, aSteps);
}

function OdeDynamicSetAutoDisableTime(aObj, aTime) {
	return external_call(global._OdeDynamicSetAutoDisableTime, aObj, aTime);
}

function OdeDynamicAddForce(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicAddForce, aObj, aX, aY, aZ);
}

function OdeDynamicAddForceAtPos(aObj, aX, aY, aZ, aPx, aPy, aPz) {
	return external_call(global._OdeDynamicAddForceAtPos, aObj, aX, aY, aZ, aPx, aPy, aPz);
}

function OdeDynamicAddForceAtRelPos(aObj, aX, aY, aZ, aPx, aPy, aPz) {
	return external_call(global._OdeDynamicAddForceAtRelPos, aObj, aX, aY, aZ, aPx, aPy, aPz);
}

function OdeDynamicAddRelForce(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicAddRelForce, aObj, aX, aY, aZ);
}

function OdeDynamicAddRelForceAtPos(aObj, aX, aY, aZ, aPx, aPy, aPz) {
	return external_call(global._OdeDynamicAddRelForceAtPos, aObj, aX, aY, aZ, aPx, aPy, aPz);
}

function OdeDynamicAddRelForceAtRelPos(aObj, aX, aY, aZ, aPx, aPy, aPz) {
	return external_call(global._OdeDynamicAddRelForceAtRelPos, aObj, aX, aY, aZ, aPx, aPy, aPz);
}

function OdeDynamicAddTorque(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicAddTorque, aObj, aX, aY, aZ);
}

function OdeDynamicAddRelTorque(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicAddRelTorque, aObj, aX, aY, aZ);
}

function OdeDynamicGetContactCount(aObj) {
	return external_call(global._OdeDynamicGetContactCount, aObj);
}

function OdeStaticGetContactCount(aObj) {
	return external_call(global._OdeStaticGetContactCount, aObj);
}

function OdeAddBox(aObj, aX, aY, aZ, aW, aH, aD) {
	return external_call(global._OdeAddBox, aObj, aX, aY, aZ, aW, aH, aD);
}

function OdeAddSphere(aObj, aX, aY, aZ, aR) {
	return external_call(global._OdeAddSphere, aObj, aX, aY, aZ, aR);
}

function OdeAddPlane(aObj) {
	return external_call(global._OdeAddPlane, aObj);
}

function OdeAddCylinder(aObj, aX, aY, aZ, aLen, aR) {
	return external_call(global._OdeAddCylinder, aObj, aX, aY, aZ, aLen, aR);
}

function OdeAddCone(aObj, aX, aY, aZ, aLen, aR) {
	return external_call(global._OdeAddCone, aObj, aX, aY, aZ, aLen, aR);
}

function OdeAddCapsule(aObj, aX, aY, aZ, aLen, aR) {
	return external_call(global._OdeAddCapsule, aObj, aX, aY, aZ, aLen, aR);
}

function OdeAddTriMesh(aObj, aMesh) {
	return external_call(global._OdeAddTriMesh, aObj, aMesh);
}

function OdeElementSetDensity(aElement, aDensity) {
	return external_call(global._OdeElementSetDensity, aElement, aDensity);
}

function OdeSurfaceEnableRollingFrictionCoeff(aObj, aMode) {
	return external_call(global._OdeSurfaceEnableRollingFrictionCoeff, aObj, aMode);
}

function OdeSurfaceSetRollingFrictionCoeff(aObj, aRfc) {
	return external_call(global._OdeSurfaceSetRollingFrictionCoeff, aObj, aRfc);
}

function OdeSurfaceSetMode(aObj, aMu2, aFdir1, aBounce, aSofterp, aSoftcfm, aMotion1, aMotion2, aSlip1, aSlip2) {
	return external_call(global._OdeSurfaceSetMode, aObj, aMu2, aFdir1, aBounce, aSofterp, aSoftcfm, aMotion1, aMotion2, aSlip1, aSlip2);
}

function OdeSurfaceSetMu(aObj, aMu) {
	return external_call(global._OdeSurfaceSetMu, aObj, aMu);
}

function OdeSurfaceSetMu2(aObj, aMu2) {
	return external_call(global._OdeSurfaceSetMu2, aObj, aMu2);
}

function OdeSurfaceSetBounce(aObj, aBounce) {
	return external_call(global._OdeSurfaceSetBounce, aObj, aBounce);
}

function OdeSurfaceSetBounceVel(aObj, aVel) {
	return external_call(global._OdeSurfaceSetBounceVel, aObj, aVel);
}

function OdeSurfaceSetSoftERP(aObj, aErp) {
	return external_call(global._OdeSurfaceSetSoftERP, aObj, aErp);
}

function OdeSurfaceSetSoftCFM(aObj, aCfm) {
	return external_call(global._OdeSurfaceSetSoftCFM, aObj, aCfm);
}

function OdeSurfaceSetMotion1(aObj, aMotion1) {
	return external_call(global._OdeSurfaceSetMotion1, aObj, aMotion1);
}

function OdeSurfaceSetMotion2(aObj, aMotion2) {
	return external_call(global._OdeSurfaceSetMotion2, aObj, aMotion2);
}

function OdeSurfaceSetSlip1(aObj, aSlip1) {
	return external_call(global._OdeSurfaceSetSlip1, aObj, aSlip1);
}

function OdeSurfaceSetSlip2(aObj, aSlip2) {
	return external_call(global._OdeSurfaceSetSlip2, aObj, aSlip2);
}

function OdeAddJointBall() {
	return external_call(global._OdeAddJointBall);
}

function OdeAddJointFixed() {
	return external_call(global._OdeAddJointFixed);
}

function OdeAddJointHinge() {
	return external_call(global._OdeAddJointHinge);
}

function OdeAddJointHinge2() {
	return external_call(global._OdeAddJointHinge2);
}

function OdeAddJointSlider() {
	return external_call(global._OdeAddJointSlider);
}

function OdeAddJointUniversal() {
	return external_call(global._OdeAddJointUniversal);
}

function OdeJointSetObjects(aJoint, aObj1, aObj2) {
	return external_call(global._OdeJointSetObjects, aJoint, aObj1, aObj2);
}

function OdeJointEnable(aJoint, aMode) {
	return external_call(global._OdeJointEnable, aJoint, aMode);
}

function OdeJointInitialize(aJoint) {
	return external_call(global._OdeJointInitialize, aJoint);
}

function OdeJointSetAnchor(aJoint, aX, aY, aZ) {
	return external_call(global._OdeJointSetAnchor, aJoint, aX, aY, aZ);
}

function OdeJointSetAnchorAtObject(aJoint, aObj) {
	return external_call(global._OdeJointSetAnchorAtObject, aJoint, aObj);
}

function OdeJointSetAxis1(aJoint, aX, aY, aZ) {
	return external_call(global._OdeJointSetAxis1, aJoint, aX, aY, aZ);
}

function OdeJointSetAxis2(aJoint, aX, aY, aZ) {
	return external_call(global._OdeJointSetAxis2, aJoint, aX, aY, aZ);
}

function OdeJointSetBounce(aJoint, aAxis, aBounce) {
	return external_call(global._OdeJointSetBounce, aJoint, aAxis, aBounce);
}

function OdeJointSetCFM(aJoint, aAxis, aCfm) {
	return external_call(global._OdeJointSetCFM, aJoint, aAxis, aCfm);
}

function OdeJointSetFMax(aJoint, aAxis, aFmax) {
	return external_call(global._OdeJointSetFMax, aJoint, aAxis, aFmax);
}

function OdeJointSetFudgeFactor(aJoint, aAxis, aFfactor) {
	return external_call(global._OdeJointSetFudgeFactor, aJoint, aAxis, aFfactor);
}

function OdeJointSetHiStop(aJoint, aAxis, aHistop) {
	return external_call(global._OdeJointSetHiStop, aJoint, aAxis, aHistop);
}

function OdeJointSetLoStop(aJoint, aAxis, aLostop) {
	return external_call(global._OdeJointSetLoStop, aJoint, aAxis, aLostop);
}

function OdeJointSetStopCFM(aJoint, aAxis, aCfm) {
	return external_call(global._OdeJointSetStopCFM, aJoint, aAxis, aCfm);
}

function OdeJointSetStopERP(aJoint, aAxis, aErp) {
	return external_call(global._OdeJointSetStopERP, aJoint, aAxis, aErp);
}

function OdeJointSetVel(aJoint, aAxis, aVelocity) {
	return external_call(global._OdeJointSetVel, aJoint, aAxis, aVelocity);
}

function OdeRagdollCreate(aActor) {
	return external_call(global._OdeRagdollCreate, aActor);
}

function OdeRagdollHingeJointCreate(aX, aY, aZ, aLostop, aHistop) {
	return external_call(global._OdeRagdollHingeJointCreate, aX, aY, aZ, aLostop, aHistop);
}

function OdeRagdollUniversalJointCreate(aX1, aY1, aZ1, aLostop1, aHistop1, aX2, aY2, aZ2, aLostop2, aHistop2) {
	return external_call(global._OdeRagdollUniversalJointCreate, aX1, aY1, aZ1, aLostop1, aHistop1, aX2, aY2, aZ2, aLostop2, aHistop2);
}

function OdeRagdollDummyJointCreate() {
	return external_call(global._OdeRagdollDummyJointCreate);
}

function OdeRagdollBoneCreate(aRag, aRagjoint, aBoneid, aParentbone) {
	return external_call(global._OdeRagdollBoneCreate, aRag, aRagjoint, aBoneid, aParentbone);
}

function OdeRagdollBuild(aRag) {
	return external_call(global._OdeRagdollBuild, aRag);
}

function OdeRagdollEnable(aRag, aMode) {
	return external_call(global._OdeRagdollEnable, aRag, aMode);
}

function OdeRagdollUpdate(aRag) {
	return external_call(global._OdeRagdollUpdate, aRag);
}

function OdeDynamicSetVelocity(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicSetVelocity, aObj, aX, aY, aZ);
}

function OdeDynamicSetAngularVelocity(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicSetAngularVelocity, aObj, aX, aY, aZ);
}

function OdeDynamicGetVelocity(aObj, aInd) {
	return external_call(global._OdeDynamicGetVelocity, aObj, aInd);
}

function OdeDynamicGetAngularVelocity(aObj, aInd) {
	return external_call(global._OdeDynamicGetAngularVelocity, aObj, aInd);
}

function OdeDynamicSetPosition(aObj, aX, aY, aZ) {
	return external_call(global._OdeDynamicSetPosition, aObj, aX, aY, aZ);
}

function OdeDynamicSetRotationQuaternion(aObj, aX, aY, aZ, aW) {
	return external_call(global._OdeDynamicSetRotationQuaternion, aObj, aX, aY, aZ, aW);
}

function SetPakArchive(aFname) {
	return external_call(global._SetPakArchive, aFname);
}

function PakGetFileCount(aP) {
	return external_call(global._PakGetFileCount, aP);
}

function PakGetFileName(aP, aIndex) {
	return external_call(global._PakGetFileName, aP, aIndex);
}

function PakExtract(aP, aDir) {
	return external_call(global._PakExtract, aP, aDir);
}

function PakExtractFile(aP, aIndex, aNewname) {
	return external_call(global._PakExtractFile, aP, aIndex, aNewname);
}

function OctreeCreate(aMaxdepth, aLeafthreshold, aGrowgravy, aCulling) {
	return external_call(global._OctreeCreate, aMaxdepth, aLeafthreshold, aGrowgravy, aCulling);
}

function QuadtreeCreate(aMaxdepth, aLeafthreshold, aGrowgravy, aCulling) {
	return external_call(global._QuadtreeCreate, aMaxdepth, aLeafthreshold, aGrowgravy, aCulling);
}

function PartitionDestroy(aTree) {
	return external_call(global._PartitionDestroy, aTree);
}

function PartitionAddLeaf(aTree, aObj) {
	return external_call(global._PartitionAddLeaf, aTree, aObj);
}

function PartitionLeafChanged(aLeaf) {
	return external_call(global._PartitionLeafChanged, aLeaf);
}

function PartitionQueryFrustum(aTree, aViewer) {
	return external_call(global._PartitionQueryFrustum, aTree, aViewer);
}

function PartitionQueryLeaf(aTree, aLeaf) {
	return external_call(global._PartitionQueryLeaf, aTree, aLeaf);
}

function PartitionQueryAABB(aTree, aObj) {
	return external_call(global._PartitionQueryAABB, aTree, aObj);
}

function PartitionQueryBSphere(aTree, aObj) {
	return external_call(global._PartitionQueryBSphere, aTree, aObj);
}

function PartitionGetNodeTests(aTree) {
	return external_call(global._PartitionGetNodeTests, aTree);
}

function PartitionGetNodeCount(aTree) {
	return external_call(global._PartitionGetNodeCount, aTree);
}

function PartitionGetResult(aTree, aInd) {
	return external_call(global._PartitionGetResult, aTree, aInd);
}

function PartitionGetResultCount(aTree) {
	return external_call(global._PartitionGetResultCount, aTree);
}

function PartitionResultShow(aTree) {
	return external_call(global._PartitionResultShow, aTree);
}

function PartitionResultHide(aTree) {
	return external_call(global._PartitionResultHide, aTree);
}

function PipeCreate(aDivs, aSlic, aParent) {
	return external_call(global._PipeCreate, aDivs, aSlic, aParent);
}

function PipeAddNode(aPipe, aX, aY, aZ) {
	return external_call(global._PipeAddNode, aPipe, aX, aY, aZ);
}

function PipeSetDivision(aPipe, aDivs) {
	return external_call(global._PipeSetDivision, aPipe, aDivs);
}

function PipeSetSplineMode(aPipe, aMode) {
	return external_call(global._PipeSetSplineMode, aPipe, aMode);
}

function PipeDeleteNode(aPipe, aInd) {
	return external_call(global._PipeDeleteNode, aPipe, aInd);
}

function PipeSetRadius(aPipe, aRad) {
	return external_call(global._PipeSetRadius, aPipe, aRad);
}

function PipeSetNode(aPipe, aInd, aX, aY, aZ) {
	return external_call(global._PipeSetNode, aPipe, aInd, aX, aY, aZ);
}

function PipeSetSlices(aPipe, aSlic) {
	return external_call(global._PipeSetSlices, aPipe, aSlic);
}

function PolygonCreate(aParent) {
	return external_call(global._PolygonCreate, aParent);
}

function PolygonAddVertex(aPolygon, aX, aY, aZ) {
	return external_call(global._PolygonAddVertex, aPolygon, aX, aY, aZ);
}

function PolygonSetVertexPosition(aPolygon, aVertex, aX, aY, aZ) {
	return external_call(global._PolygonSetVertexPosition, aPolygon, aVertex, aX, aY, aZ);
}

function PolygonDeleteVertex(aPolygon, aVertex) {
	return external_call(global._PolygonDeleteVertex, aPolygon, aVertex);
}

function CubeCreate(aW, aH, aD, aParent) {
	return external_call(global._CubeCreate, aW, aH, aD, aParent);
}

function CubeSetNormalDirection(aCube, aNd) {
	return external_call(global._CubeSetNormalDirection, aCube, aNd);
}

function CubeGetNormalDirection(aCube) {
	return external_call(global._CubeGetNormalDirection, aCube);
}

function PlaneCreate(aSquad, aW, aH, aXt, aYt, aParent) {
	return external_call(global._PlaneCreate, aSquad, aW, aH, aXt, aYt, aParent);
}

function PlaneSetOptions(aPlane, aSquad, aXt, aYt) {
	return external_call(global._PlaneSetOptions, aPlane, aSquad, aXt, aYt);
}

function PlaneGetOptions(aPlane, aIndex) {
	return external_call(global._PlaneGetOptions, aPlane, aIndex);
}

function TilePlaneCreate(aParent) {
	return external_call(global._TilePlaneCreate, aParent);
}

function TilePlaneSetTile(aTplane, aX, aY, aMat) {
	return external_call(global._TilePlaneSetTile, aTplane, aX, aY, aMat);
}

function SphereCreate(aRad, aSlic, aStaks, aParent) {
	return external_call(global._SphereCreate, aRad, aSlic, aStaks, aParent);
}

function SphereSetAngleLimits(aSphere, aStarta, aStopa, aTopa, aBottoma) {
	return external_call(global._SphereSetAngleLimits, aSphere, aStarta, aStopa, aTopa, aBottoma);
}

function SphereGetAngleLimits(aSphere, aIndex) {
	return external_call(global._SphereGetAngleLimits, aSphere, aIndex);
}

function SphereSetOptions(aSphere, aRad, aSlic, aStaks) {
	return external_call(global._SphereSetOptions, aSphere, aRad, aSlic, aStaks);
}

function SphereGetOptions(aSph, aInd) {
	return external_call(global._SphereGetOptions, aSph, aInd);
}

function CylinderCreate(aTopr, aBotr, aH, aSlic, aStaks, aLoop, aParent) {
	return external_call(global._CylinderCreate, aTopr, aBotr, aH, aSlic, aStaks, aLoop, aParent);
}

function CylinderSetOptions(aCyl, aTopr, aBotr, aH, aSlic, aStaks, aLoop) {
	return external_call(global._CylinderSetOptions, aCyl, aTopr, aBotr, aH, aSlic, aStaks, aLoop);
}

function CylinderGetOptions(aCyl, aInd) {
	return external_call(global._CylinderGetOptions, aCyl, aInd);
}

function ConeCreate(aBotr, aH, aSlic, aStaks, aLoop, aParent) {
	return external_call(global._ConeCreate, aBotr, aH, aSlic, aStaks, aLoop, aParent);
}

function ConeGetOptions(aCone, aInd) {
	return external_call(global._ConeGetOptions, aCone, aInd);
}

function ConeSetOptions(aCone, aBotr, aH, aSlic, aStaks, aLoop) {
	return external_call(global._ConeSetOptions, aCone, aBotr, aH, aSlic, aStaks, aLoop);
}

function AnnulusCreate(aInr, aOutr, aH, aSlic, aStaks, aLoop, aParent) {
	return external_call(global._AnnulusCreate, aInr, aOutr, aH, aSlic, aStaks, aLoop, aParent);
}

function AnnulusSetOptions(aAn, aInr, aOutr, aH, aSlic, aStaks, aLoop) {
	return external_call(global._AnnulusSetOptions, aAn, aInr, aOutr, aH, aSlic, aStaks, aLoop);
}

function AnnulusGetOptions(aAn, aInd) {
	return external_call(global._AnnulusGetOptions, aAn, aInd);
}

function TorusCreate(aInr, aOutr, aRing, aSide, aParent) {
	return external_call(global._TorusCreate, aInr, aOutr, aRing, aSide, aParent);
}

function TorusSetOptions(aTor, aInr, aOutr, aRing, aSide) {
	return external_call(global._TorusSetOptions, aTor, aInr, aOutr, aRing, aSide);
}

function TorusGetOptions(aTor, aInd) {
	return external_call(global._TorusGetOptions, aTor, aInd);
}

function DiskCreate(aInr, aOutr, aStarta, aSweepa, aLoop, aSlic, aParent) {
	return external_call(global._DiskCreate, aInr, aOutr, aStarta, aSweepa, aLoop, aSlic, aParent);
}

function DiskSetOptions(aDisk, aInr, aOutr, aStarta, aSweepa, aLoop, aSlic) {
	return external_call(global._DiskSetOptions, aDisk, aInr, aOutr, aStarta, aSweepa, aLoop, aSlic);
}

function DiskGetOptions(aDisk, aInd) {
	return external_call(global._DiskGetOptions, aDisk, aInd);
}

function FrustrumCreate(aBasew, aBased, aApexh, aCuth, aParent) {
	return external_call(global._FrustrumCreate, aBasew, aBased, aApexh, aCuth, aParent);
}

function FrustrumSetOptions(aFr, aBasew, aBased, aApexh, aCuth) {
	return external_call(global._FrustrumSetOptions, aFr, aBasew, aBased, aApexh, aCuth);
}

function FrustrumGetOptions(aFr, aInd) {
	return external_call(global._FrustrumGetOptions, aFr, aInd);
}

function DodecahedronCreate(aParent) {
	return external_call(global._DodecahedronCreate, aParent);
}

function IcosahedronCreate(aParent) {
	return external_call(global._IcosahedronCreate, aParent);
}

function TeapotCreate(aParent) {
	return external_call(global._TeapotCreate, aParent);
}

function ProxyObjectCreate(aTarget, aParent) {
	return external_call(global._ProxyObjectCreate, aTarget, aParent);
}

function ProxyObjectSetTarget(aProxy, aTarget) {
	return external_call(global._ProxyObjectSetTarget, aProxy, aTarget);
}

function MultiProxyObjectCreate(aParent) {
	return external_call(global._MultiProxyObjectCreate, aParent);
}

function MultiProxyObjectAddTarget(aMproxy, aTarget, aMindist, aMaxdist) {
	return external_call(global._MultiProxyObjectAddTarget, aMproxy, aTarget, aMindist, aMaxdist);
}

function ActorProxyObjectCreate(aActor, aParent) {
	return external_call(global._ActorProxyObjectCreate, aActor, aParent);
}

function ActorProxyObjectSwitchToAnimation(aProxy, aAnim) {
	return external_call(global._ActorProxyObjectSwitchToAnimation, aProxy, aAnim);
}

function ActorProxyObjectSetAnimationRange(aProxy, aStartf, aEndf) {
	return external_call(global._ActorProxyObjectSetAnimationRange, aProxy, aStartf, aEndf);
}

function ActorProxyObjectSetInterval(aProxy, aInterval) {
	return external_call(global._ActorProxyObjectSetInterval, aProxy, aInterval);
}

function ShaderEnable(aShader, aMode) {
	return external_call(global._ShaderEnable, aShader, aMode);
}

function PhongShaderCreate() {
	return external_call(global._PhongShaderCreate);
}

function PhongShaderUseTexture(aShader, aMode) {
	return external_call(global._PhongShaderUseTexture, aShader, aMode);
}

function PhongShaderSetMaxLights(aShader, aMaxlights) {
	return external_call(global._PhongShaderSetMaxLights, aShader, aMaxlights);
}

function BumpShaderCreate() {
	return external_call(global._BumpShaderCreate);
}

function BumpShaderSetDiffuseTexture(aShader, aMtrl) {
	return external_call(global._BumpShaderSetDiffuseTexture, aShader, aMtrl);
}

function BumpShaderSetNormalTexture(aShader, aMtrl) {
	return external_call(global._BumpShaderSetNormalTexture, aShader, aMtrl);
}

function BumpShaderSetHeightTexture(aShader, aMtrl) {
	return external_call(global._BumpShaderSetHeightTexture, aShader, aMtrl);
}

function BumpShaderSetMaxLights(aShader, aMaxlights) {
	return external_call(global._BumpShaderSetMaxLights, aShader, aMaxlights);
}

function BumpShaderUseParallax(aShader, aMode) {
	return external_call(global._BumpShaderUseParallax, aShader, aMode);
}

function BumpShaderSetParallaxOffset(aShader, aHeight) {
	return external_call(global._BumpShaderSetParallaxOffset, aShader, aHeight);
}

function BumpShaderSetShadowMap(aShader, aShadowmap) {
	return external_call(global._BumpShaderSetShadowMap, aShader, aShadowmap);
}

function BumpShaderSetShadowBlurRadius(aShader, aRadius) {
	return external_call(global._BumpShaderSetShadowBlurRadius, aShader, aRadius);
}

function BumpShaderUseAutoTangentSpace(aShader, aMode) {
	return external_call(global._BumpShaderUseAutoTangentSpace, aShader, aMode);
}

function CelShaderCreate() {
	return external_call(global._CelShaderCreate);
}

function CelShaderSetLineColor(aShader, aCol) {
	return external_call(global._CelShaderSetLineColor, aShader, aCol);
}

function CelShaderSetLineWidth(aShader, aWidth) {
	return external_call(global._CelShaderSetLineWidth, aShader, aWidth);
}

function CelShaderSetOptions(aShader, aOutlines, aTextured) {
	return external_call(global._CelShaderSetOptions, aShader, aOutlines, aTextured);
}

function MultiMaterialShaderCreate(aMatlib) {
	return external_call(global._MultiMaterialShaderCreate, aMatlib);
}

function HiddenLineShaderCreate() {
	return external_call(global._HiddenLineShaderCreate);
}

function HiddenLineShaderSetLineSmooth(aShader, aMode) {
	return external_call(global._HiddenLineShaderSetLineSmooth, aShader, aMode);
}

function HiddenLineShaderSetSolid(aShader, aMode) {
	return external_call(global._HiddenLineShaderSetSolid, aShader, aMode);
}

function HiddenLineShaderSetSurfaceLit(aShader, aMode) {
	return external_call(global._HiddenLineShaderSetSurfaceLit, aShader, aMode);
}

function HiddenLineShaderSetFrontLine(aShader, aWidth, aCol, aPattern, aForcemat) {
	return external_call(global._HiddenLineShaderSetFrontLine, aShader, aWidth, aCol, aPattern, aForcemat);
}

function HiddenLineShaderSetBackLine(aShader, aWidth, aCol, aPattern, aForcemat) {
	return external_call(global._HiddenLineShaderSetBackLine, aShader, aWidth, aCol, aPattern, aForcemat);
}

function OutlineShaderCreate(aSmooth) {
	return external_call(global._OutlineShaderCreate, aSmooth);
}

function OutlineShaderSetLineColor(aShader, aCol) {
	return external_call(global._OutlineShaderSetLineColor, aShader, aCol);
}

function OutlineShaderSetLineWidth(aShader, aWidth) {
	return external_call(global._OutlineShaderSetLineWidth, aShader, aWidth);
}

function TexCombineShaderCreate(aMatlib) {
	return external_call(global._TexCombineShaderCreate, aMatlib);
}

function TexCombineShaderAddCombiner(aTcs, aStr) {
	return external_call(global._TexCombineShaderAddCombiner, aTcs, aStr);
}

function TexCombineShaderMaterial3(aTcs, aM3) {
	return external_call(global._TexCombineShaderMaterial3, aTcs, aM3);
}

function TexCombineShaderMaterial4(aTcs, aM4) {
	return external_call(global._TexCombineShaderMaterial4, aTcs, aM4);
}

function GLSLShaderCreate(aVp, aFp) {
	return external_call(global._GLSLShaderCreate, aVp, aFp);
}

function GLSLShaderCreateParameter(aGlsl, aName) {
	return external_call(global._GLSLShaderCreateParameter, aGlsl, aName);
}

function GLSLShaderSetParameter1i(aPar, aVal) {
	return external_call(global._GLSLShaderSetParameter1i, aPar, aVal);
}

function GLSLShaderSetParameter1f(aPar, aX) {
	return external_call(global._GLSLShaderSetParameter1f, aPar, aX);
}

function GLSLShaderSetParameter2f(aPar, aX, aY) {
	return external_call(global._GLSLShaderSetParameter2f, aPar, aX, aY);
}

function GLSLShaderSetParameter3f(aPar, aX, aY, aZ) {
	return external_call(global._GLSLShaderSetParameter3f, aPar, aX, aY, aZ);
}

function GLSLShaderSetParameter4f(aPar, aX, aY, aZ, aW) {
	return external_call(global._GLSLShaderSetParameter4f, aPar, aX, aY, aZ, aW);
}

function GLSLShaderSetParameterTexture(aPar, aMtrl, aTexunit) {
	return external_call(global._GLSLShaderSetParameterTexture, aPar, aMtrl, aTexunit);
}

function GLSLShaderSetParameterSecondTexture(aPar, aMtrl, aTexunit) {
	return external_call(global._GLSLShaderSetParameterSecondTexture, aPar, aMtrl, aTexunit);
}

function GLSLShaderSetParameterShadowTexture(aPar, aShadowmap, aTexunit) {
	return external_call(global._GLSLShaderSetParameterShadowTexture, aPar, aShadowmap, aTexunit);
}

function GLSLShaderSetParameterShadowMatrix(aPar, aShadowmap) {
	return external_call(global._GLSLShaderSetParameterShadowMatrix, aPar, aShadowmap);
}

function GLSLShaderSetParameterMatrix(aPar, aObj) {
	return external_call(global._GLSLShaderSetParameterMatrix, aPar, aObj);
}

function GLSLShaderSetParameterInvMatrix(aPar, aObj) {
	return external_call(global._GLSLShaderSetParameterInvMatrix, aPar, aObj);
}

function GLSLShaderSetParameterFBOColorTexture(aPar, aFbo, aTexunit) {
	return external_call(global._GLSLShaderSetParameterFBOColorTexture, aPar, aFbo, aTexunit);
}

function GLSLShaderSetParameterFBODepthTexture(aPar, aFbo, aTexunit) {
	return external_call(global._GLSLShaderSetParameterFBODepthTexture, aPar, aFbo, aTexunit);
}

function GLSLShaderSetParameterViewMatrix(aPar) {
	return external_call(global._GLSLShaderSetParameterViewMatrix, aPar);
}

function GLSLShaderSetParameterInvViewMatrix(aPar) {
	return external_call(global._GLSLShaderSetParameterInvViewMatrix, aPar);
}

function GLSLShaderSetParameterHasTextureEx(aPar, aSlot) {
	return external_call(global._GLSLShaderSetParameterHasTextureEx, aPar, aSlot);
}

function ShadowMapCreate(aSize, aViewer, aCaster) {
	return external_call(global._ShadowMapCreate, aSize, aViewer, aCaster);
}

function ShadowMapSetCamera(aShadowmap, aCam) {
	return external_call(global._ShadowMapSetCamera, aShadowmap, aCam);
}

function ShadowMapSetCaster(aShadowmap, aCaster) {
	return external_call(global._ShadowMapSetCaster, aShadowmap, aCaster);
}

function ShadowMapSetProjectionSize(aShadowmap, aSize) {
	return external_call(global._ShadowMapSetProjectionSize, aShadowmap, aSize);
}

function ShadowMapSetZScale(aShadowmap, aScale) {
	return external_call(global._ShadowMapSetZScale, aShadowmap, aScale);
}

function ShadowMapSetZClippingPlanes(aShadowmap, aZnear, aZfar) {
	return external_call(global._ShadowMapSetZClippingPlanes, aShadowmap, aZnear, aZfar);
}

function ShadowMapRender(aShadowmap) {
	return external_call(global._ShadowMapRender, aShadowmap);
}

function ShadowMapSetFBO(aShadowmap, aFbo) {
	return external_call(global._ShadowMapSetFBO, aShadowmap, aFbo);
}

function ShadowplaneCreate(aWidth, aHeight, aXtiles, aYtiles, aTarget, aLight, aColor, aAlpha, aParent) {
	return external_call(global._ShadowplaneCreate, aWidth, aHeight, aXtiles, aYtiles, aTarget, aLight, aColor, aAlpha, aParent);
}

function ShadowplaneSetLight(aShadowplane, aLight) {
	return external_call(global._ShadowplaneSetLight, aShadowplane, aLight);
}

function ShadowplaneSetObject(aShadowplane, aTarget) {
	return external_call(global._ShadowplaneSetObject, aShadowplane, aTarget);
}

function ShadowplaneSetOptions(aShadowplane, aStencil, aScissor, aTransparent, aIgnorez) {
	return external_call(global._ShadowplaneSetOptions, aShadowplane, aStencil, aScissor, aTransparent, aIgnorez);
}

function ShadowvolumeCreate(aParent) {
	return external_call(global._ShadowvolumeCreate, aParent);
}

function ShadowvolumeSetActive(aShadowvolume, aActive) {
	return external_call(global._ShadowvolumeSetActive, aShadowvolume, aActive);
}

function ShadowvolumeAddLight(aShadowvolume, aLight) {
	return external_call(global._ShadowvolumeAddLight, aShadowvolume, aLight);
}

function ShadowvolumeRemoveLight(aShadowvolume, aLight) {
	return external_call(global._ShadowvolumeRemoveLight, aShadowvolume, aLight);
}

function ShadowvolumeAddOccluder(aShadowvolume, aObj) {
	return external_call(global._ShadowvolumeAddOccluder, aShadowvolume, aObj);
}

function ShadowvolumeRemoveOccluder(aShadowvolume, aObj) {
	return external_call(global._ShadowvolumeRemoveOccluder, aShadowvolume, aObj);
}

function ShadowvolumeSetDarkeningColor(aShadowvolume, aColor, aAlpha) {
	return external_call(global._ShadowvolumeSetDarkeningColor, aShadowvolume, aColor, aAlpha);
}

function ShadowvolumeSetMode(aShadowvolume, aSvm) {
	return external_call(global._ShadowvolumeSetMode, aShadowvolume, aSvm);
}

function SkyboxCreate(aParent) {
	return external_call(global._SkyboxCreate, aParent);
}

function SkyboxSetMaterial(aSkybox, aSbm, aMatname) {
	return external_call(global._SkyboxSetMaterial, aSkybox, aSbm, aMatname);
}

function SkyboxSetClouds(aSkybox, aOffset, aSize) {
	return external_call(global._SkyboxSetClouds, aSkybox, aOffset, aSize);
}

function SkyboxSetStyle(aSkybox, aSbs) {
	return external_call(global._SkyboxSetStyle, aSkybox, aSbs);
}

function SkydomeCreate(aSlices, aStacks, aParent) {
	return external_call(global._SkydomeCreate, aSlices, aStacks, aParent);
}

function SkydomeSetOptions(aSkydome, aFade, aRotate) {
	return external_call(global._SkydomeSetOptions, aSkydome, aFade, aRotate);
}

function SkydomeSetDeepColor(aSkydome, aColor) {
	return external_call(global._SkydomeSetDeepColor, aSkydome, aColor);
}

function SkydomeSetHazeColor(aSkydome, aColor) {
	return external_call(global._SkydomeSetHazeColor, aSkydome, aColor);
}

function SkydomeSetNightColor(aSkydome, aColor) {
	return external_call(global._SkydomeSetNightColor, aSkydome, aColor);
}

function SkydomeSetSkyColor(aSkydome, aColor) {
	return external_call(global._SkydomeSetSkyColor, aSkydome, aColor);
}

function SkydomeSetSunDawnColor(aSkydome, aColor) {
	return external_call(global._SkydomeSetSunDawnColor, aSkydome, aColor);
}

function SkydomeSetSunZenithColor(aSkydome, aColor) {
	return external_call(global._SkydomeSetSunZenithColor, aSkydome, aColor);
}

function SkydomeSetSunElevation(aSkydome, aAngle) {
	return external_call(global._SkydomeSetSunElevation, aSkydome, aAngle);
}

function SkydomeSetTurbidity(aSkydome, aTurbidity) {
	return external_call(global._SkydomeSetTurbidity, aSkydome, aTurbidity);
}

function SkydomeAddRandomStars(aSkydome, aStars, aColor) {
	return external_call(global._SkydomeAddRandomStars, aSkydome, aStars, aColor);
}

function SkydomeAddStar(aSkydome, aRightascension, aDeclination, aMagnitude, aColor) {
	return external_call(global._SkydomeAddStar, aSkydome, aRightascension, aDeclination, aMagnitude, aColor);
}

function SkydomeClearStars(aSkydome) {
	return external_call(global._SkydomeClearStars, aSkydome);
}

function SkydomeTwinkleStars(aSkydome, aMode) {
	return external_call(global._SkydomeTwinkleStars, aSkydome, aMode);
}

function HUDSpriteCreate(aMtrl, aW, aH, aParent) {
	return external_call(global._HUDSpriteCreate, aMtrl, aW, aH, aParent);
}

function HUDSpriteGetMouseOver(aSprite, aViewer) {
	return external_call(global._HUDSpriteGetMouseOver, aSprite, aViewer);
}

function HUDSpriteXTiles(aSprite, aXtls) {
	return external_call(global._HUDSpriteXTiles, aSprite, aXtls);
}

function HUDSpriteYTiles(aSprite, aYtls) {
	return external_call(global._HUDSpriteYTiles, aSprite, aYtls);
}

function SpriteCreate(aMtrl, aW, aH, aParent) {
	return external_call(global._SpriteCreate, aMtrl, aW, aH, aParent);
}

function SpriteSetSize(aSprite, aW, aH) {
	return external_call(global._SpriteSetSize, aSprite, aW, aH);
}

function SpriteGetSize(aSprite, aType_Val) {
	return external_call(global._SpriteGetSize, aSprite, aType_Val);
}

function SpriteScale(aSprite, aU, aV) {
	return external_call(global._SpriteScale, aSprite, aU, aV);
}

function SpriteSetRotation(aSprite, aAngle) {
	return external_call(global._SpriteSetRotation, aSprite, aAngle);
}

function SpriteRotate(aSprite, aAngle) {
	return external_call(global._SpriteRotate, aSprite, aAngle);
}

function SpriteMirror(aSprite, aU, aV) {
	return external_call(global._SpriteMirror, aSprite, aU, aV);
}

function SpriteNoZWrite(aSprite, aMode) {
	return external_call(global._SpriteNoZWrite, aSprite, aMode);
}

function SpriteCreateEx(aW, aH, aLeft, aTop, aRight, aBottom, aParent) {
	return external_call(global._SpriteCreateEx, aW, aH, aLeft, aTop, aRight, aBottom, aParent);
}

function HUDSpriteCreateEx(aW, aH, aLeft, aTop, aRight, aBottom, aParent) {
	return external_call(global._HUDSpriteCreateEx, aW, aH, aLeft, aTop, aRight, aBottom, aParent);
}

function SpriteSetBounds(aSprite, aLeft, aTop, aRight, aBottom) {
	return external_call(global._SpriteSetBounds, aSprite, aLeft, aTop, aRight, aBottom);
}

function SpriteSetBoundsUV(aSprite, aLeft, aTop, aRight, aBottom) {
	return external_call(global._SpriteSetBoundsUV, aSprite, aLeft, aTop, aRight, aBottom);
}

function SpriteSetOrigin(aSprite, aX, aY) {
	return external_call(global._SpriteSetOrigin, aSprite, aX, aY);
}

function BmpHDSCreate(aImg) {
	return external_call(global._BmpHDSCreate, aImg);
}

function BmpHDSSetInfiniteWarp(aHds, aIwarp) {
	return external_call(global._BmpHDSSetInfiniteWarp, aHds, aIwarp);
}

function BmpHDSInvert(aHds) {
	return external_call(global._BmpHDSInvert, aHds);
}

function BmpHDSCreateEmpty(aW, aH, aFill) {
	return external_call(global._BmpHDSCreateEmpty, aW, aH, aFill);
}

function BmpHDSSetHeight(aHds, aX, aY, aH) {
	return external_call(global._BmpHDSSetHeight, aHds, aX, aY, aH);
}

function BmpHDSGetHeight(aHds, aX, aY) {
	return external_call(global._BmpHDSGetHeight, aHds, aX, aY);
}

function BmpHDSSave(aHds, aFilename) {
	return external_call(global._BmpHDSSave, aHds, aFilename);
}

function TerrainCreate(aParent) {
	return external_call(global._TerrainCreate, aParent);
}

function TerrainSetHeightData(aTerrain, aHds) {
	return external_call(global._TerrainSetHeightData, aTerrain, aHds);
}

function TerrainSetTileSize(aTerrain, aTsize) {
	return external_call(global._TerrainSetTileSize, aTerrain, aTsize);
}

function TerrainSetTilesPerTexture(aTerrain, aTpt) {
	return external_call(global._TerrainSetTilesPerTexture, aTerrain, aTpt);
}

function TerrainSetQualityDistance(aTerrain, aQd) {
	return external_call(global._TerrainSetQualityDistance, aTerrain, aQd);
}

function TerrainSetQualityStyle(aTerrain, aHrs) {
	return external_call(global._TerrainSetQualityStyle, aTerrain, aHrs);
}

function TerrainSetMaxCLodTriangles(aTerrain, aTri) {
	return external_call(global._TerrainSetMaxCLodTriangles, aTerrain, aTri);
}

function TerrainSetCLodPrecision(aTerrain, aPrec) {
	return external_call(global._TerrainSetCLodPrecision, aTerrain, aPrec);
}

function TerrainSetOcclusionFrameSkip(aTerrain, aOfs) {
	return external_call(global._TerrainSetOcclusionFrameSkip, aTerrain, aOfs);
}

function TerrainSetOcclusionTesselate(aTerrain, aTot) {
	return external_call(global._TerrainSetOcclusionTesselate, aTerrain, aTot);
}

function TerrainGetHeightAtObjectPosition(aTerrain, aObj) {
	return external_call(global._TerrainGetHeightAtObjectPosition, aTerrain, aObj);
}

function TerrainGetLastTriCount(aTerrain) {
	return external_call(global._TerrainGetLastTriCount, aTerrain);
}

function TerrainGetHDSPosition(aTerrain, aX, aY, aZ, aIndex) {
	return external_call(global._TerrainGetHDSPosition, aTerrain, aX, aY, aZ, aIndex);
}

function TextRead(aFilename) {
	return external_call(global._TextRead, aFilename);
}

function TextConvertANSIToUTF8(aStr) {
	return external_call(global._TextConvertANSIToUTF8, aStr);
}

function ThorFXManagerCreate() {
	return external_call(global._ThorFXManagerCreate);
}

function ThorFXSetColor(aFx, aIncolor, aInalpha, aOutcolor, aOutalpha, aCcolor, aCalpha) {
	return external_call(global._ThorFXSetColor, aFx, aIncolor, aInalpha, aOutcolor, aOutalpha, aCcolor, aCalpha);
}

function ThorFXEnableCore(aFx, aMode) {
	return external_call(global._ThorFXEnableCore, aFx, aMode);
}

function ThorFXEnableGlow(aFx, aMode) {
	return external_call(global._ThorFXEnableGlow, aFx, aMode);
}

function ThorFXSetMaxParticles(aFx, aMaxp) {
	return external_call(global._ThorFXSetMaxParticles, aFx, aMaxp);
}

function ThorFXSetGlowSize(aFx, aSize) {
	return external_call(global._ThorFXSetGlowSize, aFx, aSize);
}

function ThorFXSetVibrate(aFx, aVibr) {
	return external_call(global._ThorFXSetVibrate, aFx, aVibr);
}

function ThorFXSetWildness(aFx, aWild) {
	return external_call(global._ThorFXSetWildness, aFx, aWild);
}

function ThorFXSetTarget(aFx, aX, aY, aZ) {
	return external_call(global._ThorFXSetTarget, aFx, aX, aY, aZ);
}

function ThorFXCreate(aFx, aObj) {
	return external_call(global._ThorFXCreate, aFx, aObj);
}

function TrailCreate(aObj, aParent) {
	return external_call(global._TrailCreate, aObj, aParent);
}

function TrailSetObject(aTrail, aObj) {
	return external_call(global._TrailSetObject, aTrail, aObj);
}

function TrailSetAlpha(aTrail, aAlpha, aFade) {
	return external_call(global._TrailSetAlpha, aTrail, aAlpha, aFade);
}

function TrailSetLimits(aTrail, aVl, aTl) {
	return external_call(global._TrailSetLimits, aTrail, aVl, aTl);
}

function TrailSetMinDistance(aTrail, aDistance) {
	return external_call(global._TrailSetMinDistance, aTrail, aDistance);
}

function TrailSetUVScale(aTrail, aScale) {
	return external_call(global._TrailSetUVScale, aTrail, aScale);
}

function TrailSetMarkStyle(aTrail, aMs) {
	return external_call(global._TrailSetMarkStyle, aTrail, aMs);
}

function TrailSetMarkWidth(aTrail, aWidth) {
	return external_call(global._TrailSetMarkWidth, aTrail, aWidth);
}

function TrailSetEnabled(aTrail, aMode) {
	return external_call(global._TrailSetEnabled, aTrail, aMode);
}

function TrailClearMarks(aTrail) {
	return external_call(global._TrailClearMarks, aTrail);
}

function TreeCreate(aParent) {
	return external_call(global._TreeCreate, aParent);
}

function TreeSetMaterials(aTree, aMfront, aMback, aMbranch) {
	return external_call(global._TreeSetMaterials, aTree, aMfront, aMback, aMbranch);
}

function TreeSetBranchFacets(aTree, aFacets) {
	return external_call(global._TreeSetBranchFacets, aTree, aFacets);
}

function TreeBuildMesh(aTree, aParent) {
	return external_call(global._TreeBuildMesh, aTree, aParent);
}

function TreeSetBranchNoise(aTree, aNoise) {
	return external_call(global._TreeSetBranchNoise, aTree, aNoise);
}

function TreeSetBranchAngle(aTree, aAngle) {
	return external_call(global._TreeSetBranchAngle, aTree, aAngle);
}

function TreeSetBranchSize(aTree, aSize) {
	return external_call(global._TreeSetBranchSize, aTree, aSize);
}

function TreeSetBranchRadius(aTree, aRadius) {
	return external_call(global._TreeSetBranchRadius, aTree, aRadius);
}

function TreeSetBranchTwist(aTree, aTwist) {
	return external_call(global._TreeSetBranchTwist, aTree, aTwist);
}

function TreeSetDepth(aTree, aDepth) {
	return external_call(global._TreeSetDepth, aTree, aDepth);
}

function TreeSetLeafSize(aTree, aLeafsize) {
	return external_call(global._TreeSetLeafSize, aTree, aLeafsize);
}

function TreeSetLeafThreshold(aTree, aThreshold) {
	return external_call(global._TreeSetLeafThreshold, aTree, aThreshold);
}

function TreeSetSeed(aTree, aSeed) {
	return external_call(global._TreeSetSeed, aTree, aSeed);
}

function VerletWorldCreate(aIter, aUpdatespacepartion, aDrag) {
	return external_call(global._VerletWorldCreate, aIter, aUpdatespacepartion, aDrag);
}

function VerletWorldCreateOctree(aWorld, aXmin, aYmin, aZmin, aXmax, aYmax, aZmax, aLeaf, aDepth) {
	return external_call(global._VerletWorldCreateOctree, aWorld, aXmin, aYmin, aZmin, aXmax, aYmax, aZmax, aLeaf, aDepth);
}

function VerletGetNodeCount(aWorld) {
	return external_call(global._VerletGetNodeCount, aWorld);
}

function VerletWorldGravityCreate(aWorld, aX, aY, aZ) {
	return external_call(global._VerletWorldGravityCreate, aWorld, aX, aY, aZ);
}

function VerletWorldGravitySetDirection(aGrv, aX, aY, aZ) {
	return external_call(global._VerletWorldGravitySetDirection, aGrv, aX, aY, aZ);
}

function VerletWorldUpdate(aWorld, aNewtime) {
	return external_call(global._VerletWorldUpdate, aWorld, aNewtime);
}

function EdgeDetectorCreate(aWorld, aObj) {
	return external_call(global._EdgeDetectorCreate, aWorld, aObj);
}

function EdgeDetectorSetWeldDistance(aEdge, aDis) {
	return external_call(global._EdgeDetectorSetWeldDistance, aEdge, aDis);
}

function VerletConstraintFloorCreate(aWorld, aBou, aLevel) {
	return external_call(global._VerletConstraintFloorCreate, aWorld, aBou, aLevel);
}

function VerletConstraintFloorSetNormal(aFlr, aX, aY, aZ) {
	return external_call(global._VerletConstraintFloorSetNormal, aFlr, aX, aY, aZ);
}

function VerletConstraintFloorSetObjectLocations(aFlr, aObj) {
	return external_call(global._VerletConstraintFloorSetObjectLocations, aFlr, aObj);
}

function VerletConstraintSphereCreate(aWorld, aRad) {
	return external_call(global._VerletConstraintSphereCreate, aWorld, aRad);
}

function VerletConstraintCylinderCreate(aWorld, aRad) {
	return external_call(global._VerletConstraintCylinderCreate, aWorld, aRad);
}

function VerletConstraintCylinderSetAxis(aCyl, aX, aY, aZ) {
	return external_call(global._VerletConstraintCylinderSetAxis, aCyl, aX, aY, aZ);
}

function VerletConstraintCubeCreate(aWorld, aX, aY, aZ) {
	return external_call(global._VerletConstraintCubeCreate, aWorld, aX, aY, aZ);
}

function VerletConstraintCubeCreateSetCube(aWorld, aCube1) {
	return external_call(global._VerletConstraintCubeCreateSetCube, aWorld, aCube1);
}

function VerletConstraintCubeSetDirection(aCb, aX, aY, aZ) {
	return external_call(global._VerletConstraintCubeSetDirection, aCb, aX, aY, aZ);
}

function VerletConstraintCapsuleCreate(aWorld, aRad, aLen) {
	return external_call(global._VerletConstraintCapsuleCreate, aWorld, aRad, aLen);
}

function VerletConstraintCapsuleSetAxis(aCp, aX, aY, aZ) {
	return external_call(global._VerletConstraintCapsuleSetAxis, aCp, aX, aY, aZ);
}

function VerletConstraintSetPosition(aObj, aX, aY, aZ) {
	return external_call(global._VerletConstraintSetPosition, aObj, aX, aY, aZ);
}

function VerletConstraintSetFrictionRatio(aObj, aFr) {
	return external_call(global._VerletConstraintSetFrictionRatio, aObj, aFr);
}

function VerletConstraintSetEnabled(aObj, aEn) {
	return external_call(global._VerletConstraintSetEnabled, aObj, aEn);
}

function VerletNodeNailedDown(aWorld, aInd, aBol) {
	return external_call(global._VerletNodeNailedDown, aWorld, aInd, aBol);
}

function VerletNodeSetPosition(aWorld, aInd, aX, aY, aZ) {
	return external_call(global._VerletNodeSetPosition, aWorld, aInd, aX, aY, aZ);
}

function VerletNodeSetRadius(aWorld, aInd, aRad) {
	return external_call(global._VerletNodeSetRadius, aWorld, aInd, aRad);
}

function VerletNodeSetFriction(aWorld, aInd, aFr) {
	return external_call(global._VerletNodeSetFriction, aWorld, aInd, aFr);
}

function VerletNodeSetWeight(aWorld, aInd, aWeight) {
	return external_call(global._VerletNodeSetWeight, aWorld, aInd, aWeight);
}

function VerletNodeApplyFriction(aWorld, aInd, aFr, aDepth, aX, aY, aZ) {
	return external_call(global._VerletNodeApplyFriction, aWorld, aInd, aFr, aDepth, aX, aY, aZ);
}

function VerletAirResistanceCreate(aWorld, aMagnitude, aChaos) {
	return external_call(global._VerletAirResistanceCreate, aWorld, aMagnitude, aChaos);
}

function VerletAirResistanceSetWindDirection(aAir, aX, aY, aZ) {
	return external_call(global._VerletAirResistanceSetWindDirection, aAir, aX, aY, aZ);
}

function VerletAirResistanceSetWindMagnitude(aAir, aMag) {
	return external_call(global._VerletAirResistanceSetWindMagnitude, aAir, aMag);
}

function VerletAirResistanceSetWindChaos(aAir, aCh) {
	return external_call(global._VerletAirResistanceSetWindChaos, aAir, aCh);
}

function VerletConstraintGetCount(aWr) {
	return external_call(global._VerletConstraintGetCount, aWr);
}

function VerletConstraintSetSlack(aWr, aCon, aSla) {
	return external_call(global._VerletConstraintSetSlack, aWr, aCon, aSla);
}

function VerletWorldSetSimTime(aWr, aTm) {
	return external_call(global._VerletWorldSetSimTime, aWr, aTm);
}

function VerletWorldSetMaxDeltaTime(aWr, aTm) {
	return external_call(global._VerletWorldSetMaxDeltaTime, aWr, aTm);
}

function ViewerCreate(aTop, aLeft, aW, aH, aPw) {
	return external_call(global._ViewerCreate, aTop, aLeft, aW, aH, aPw);
}

function ViewerSetCamera(aViewer, aCamera) {
	return external_call(global._ViewerSetCamera, aViewer, aCamera);
}

function ViewerEnableVSync(aViewer, aVsm) {
	return external_call(global._ViewerEnableVSync, aViewer, aVsm);
}

function ViewerRender(aViewer) {
	return external_call(global._ViewerRender, aViewer);
}

function ViewerRenderToFile(aViewer, aFname) {
	return external_call(global._ViewerRenderToFile, aViewer, aFname);
}

function ViewerRenderToFilePNG(aViewer, aFname) {
	return external_call(global._ViewerRenderToFilePNG, aViewer, aFname);
}

function ViewerRenderEx(aViewer, aObj, aClear, aSwap, aUpdatefps) {
	return external_call(global._ViewerRenderEx, aViewer, aObj, aClear, aSwap, aUpdatefps);
}

function ViewerResize(aViewer, aLeft, aTop, aW, aH) {
	return external_call(global._ViewerResize, aViewer, aLeft, aTop, aW, aH);
}

function ViewerSetVisible(aViewer, aMode) {
	return external_call(global._ViewerSetVisible, aViewer, aMode);
}

function ViewerGetPixelColor(aViewer, aX, aY) {
	return external_call(global._ViewerGetPixelColor, aViewer, aX, aY);
}

function ViewerGetPixelDepth(aViewer, aX, aY) {
	return external_call(global._ViewerGetPixelDepth, aViewer, aX, aY);
}

function ViewerSetLighting(aViewer, aMode) {
	return external_call(global._ViewerSetLighting, aViewer, aMode);
}

function ViewerSetBackgroundColor(aViewer, aColor) {
	return external_call(global._ViewerSetBackgroundColor, aViewer, aColor);
}

function ViewerSetAmbientColor(aViewer, aColor) {
	return external_call(global._ViewerSetAmbientColor, aViewer, aColor);
}

function ViewerEnableFog(aViewer, aMode) {
	return external_call(global._ViewerEnableFog, aViewer, aMode);
}

function ViewerSetFogColor(aViewer, aColor) {
	return external_call(global._ViewerSetFogColor, aViewer, aColor);
}

function ViewerSetFogDistance(aViewer, aFstart, aFend) {
	return external_call(global._ViewerSetFogDistance, aViewer, aFstart, aFend);
}

function ViewerScreenToWorld(aViewer, aX, aY, aInd) {
	return external_call(global._ViewerScreenToWorld, aViewer, aX, aY, aInd);
}

function ViewerWorldToScreen(aViewer, aX, aY, aZ, aInd) {
	return external_call(global._ViewerWorldToScreen, aViewer, aX, aY, aZ, aInd);
}

function ViewerCopyToTexture(aViewer, aMtrl) {
	return external_call(global._ViewerCopyToTexture, aViewer, aMtrl);
}

function ViewerGetPickedObject(aViewer, aX, aY) {
	return external_call(global._ViewerGetPickedObject, aViewer, aX, aY);
}

function ViewerGetPickedObjectsList(aViewer, aX, aY, aW, aH, aNum, aInd) {
	return external_call(global._ViewerGetPickedObjectsList, aViewer, aX, aY, aW, aH, aNum, aInd);
}

function ViewerScreenToVector(aViewer, aX, aY, aInd) {
	return external_call(global._ViewerScreenToVector, aViewer, aX, aY, aInd);
}

function ViewerVectorToScreen(aViewer, aX, aY, aZ, aInd) {
	return external_call(global._ViewerVectorToScreen, aViewer, aX, aY, aZ, aInd);
}

function ViewerPixelToDistance(aViewer, aX, aY) {
	return external_call(global._ViewerPixelToDistance, aViewer, aX, aY);
}

function ViewerSetAntiAliasing(aViewer, aAa) {
	return external_call(global._ViewerSetAntiAliasing, aViewer, aAa);
}

function ViewerGetGLSLSupported(aViewer) {
	return external_call(global._ViewerGetGLSLSupported, aViewer);
}

function ViewerGetFBOSupported(aViewer) {
	return external_call(global._ViewerGetFBOSupported, aViewer);
}

function ViewerGetVBOSupported(aViewer) {
	return external_call(global._ViewerGetVBOSupported, aViewer);
}

function ViewerSetAutoRender(aViewer, aMode) {
	return external_call(global._ViewerSetAutoRender, aViewer, aMode);
}

function ViewerSetOverrideMaterial(aViewer, aMlb, aMtrl) {
	return external_call(global._ViewerSetOverrideMaterial, aViewer, aMlb, aMtrl);
}

function ViewerGetSize(aViewer, aIndex) {
	return external_call(global._ViewerGetSize, aViewer, aIndex);
}

function ViewerGetPosition(aViewer, aIndex) {
	return external_call(global._ViewerGetPosition, aViewer, aIndex);
}

function ViewerIsOpenGLExtensionSupported(aViewer, aExt) {
	return external_call(global._ViewerIsOpenGLExtensionSupported, aViewer, aExt);
}

function ViewerGetFramesPerSecond(aViewer) {
	return external_call(global._ViewerGetFramesPerSecond, aViewer);
}

function ViewerResetPerformanceMonitor(aViewer) {
	return external_call(global._ViewerResetPerformanceMonitor, aViewer);
}

function ViewerPixelRayToWorld(aViewer, aX, aY, aInd) {
	return external_call(global._ViewerPixelRayToWorld, aViewer, aX, aY, aInd);
}

function ViewerShadeModel(aViewer, aInd) {
	return external_call(global._ViewerShadeModel, aViewer, aInd);
}

function WaterCreate(aParent) {
	return external_call(global._WaterCreate, aParent);
}

function WaterCreateRandomRipple(aWater) {
	return external_call(global._WaterCreateRandomRipple, aWater);
}

function WaterCreateRippleAtGridPosition(aWater, aX, aY) {
	return external_call(global._WaterCreateRippleAtGridPosition, aWater, aX, aY);
}

function WaterCreateRippleAtWorldPosition(aWater, aX, aY, aZ) {
	return external_call(global._WaterCreateRippleAtWorldPosition, aWater, aX, aY, aZ);
}

function WaterCreateRippleAtObjectPosition(aWater, aObj) {
	return external_call(global._WaterCreateRippleAtObjectPosition, aWater, aObj);
}

function WaterSetMask(aWater, aMaterial) {
	return external_call(global._WaterSetMask, aWater, aMaterial);
}

function WaterSetActive(aWater, aMode) {
	return external_call(global._WaterSetActive, aWater, aMode);
}

function WaterReset(aWater) {
	return external_call(global._WaterReset, aWater);
}

function WaterSetRainTimeInterval(aWater, aInterval) {
	return external_call(global._WaterSetRainTimeInterval, aWater, aInterval);
}

function WaterSetRainForce(aWater, aForce) {
	return external_call(global._WaterSetRainForce, aWater, aForce);
}

function WaterSetViscosity(aWater, aViscosity) {
	return external_call(global._WaterSetViscosity, aWater, aViscosity);
}

function WaterSetElastic(aWater, aElastic) {
	return external_call(global._WaterSetElastic, aWater, aElastic);
}

function WaterSetResolution(aWater, aRes) {
	return external_call(global._WaterSetResolution, aWater, aRes);
}

function WaterSetLinearWaveHeight(aWater, aHeight) {
	return external_call(global._WaterSetLinearWaveHeight, aWater, aHeight);
}

function WaterSetLinearWaveFrequency(aWater, aFreq) {
	return external_call(global._WaterSetLinearWaveFrequency, aWater, aFreq);
}

function WindowCreate(aX, aY, aW, aH, aResizeable) {
	return external_call(global._WindowCreate, aX, aY, aW, aH, aResizeable);
}

function WindowCenter(aW) {
	return external_call(global._WindowCenter, aW);
}

function WindowResize(aW, aX, aY, aWidth, aHeight) {
	return external_call(global._WindowResize, aW, aX, aY, aWidth, aHeight);
}

function WindowGetPosition(aW, aIndex) {
	return external_call(global._WindowGetPosition, aW, aIndex);
}

function WindowGetSize(aW, aIndex) {
	return external_call(global._WindowGetSize, aW, aIndex);
}

function WindowGetHandle(aW) {
	return external_call(global._WindowGetHandle, aW);
}

function WindowSetTitle(aW, aTitle) {
	return external_call(global._WindowSetTitle, aW, aTitle);
}

function WindowDestroy(aW) {
	return external_call(global._WindowDestroy, aW);
}

function WindowIsShowing(aW) {
	return external_call(global._WindowIsShowing, aW);
}

function WindowSetIcon(aW, aFilename) {
	return external_call(global._WindowSetIcon, aW, aFilename);
}

function WindowDispatch() {
	return external_call(global._WindowDispatch);
}

function PtrToReal(p) {
	return external_call(global._PtrToReal, p);
}

#macro osInherited 0
#macro osNone 1
#macro osRenderFarthestFirst 2
#macro osRenderBlendedLast 3
#macro osRenderNearestFirst 4
#macro vcNone 0
#macro vcInherited 1
#macro vcObjectBased 2
#macro vcHierarchical 3
#macro vsmSync 0
#macro vsmNoSync 1
#macro cimNone 0
#macro cimPosition 1
#macro cimOrientation 2
#macro csPerspective 0
#macro csOrthogonal 1
#macro csOrtho2D 2
#macro csInfinitePerspective 4
#macro lsSpot 0
#macro lsOmni 1
#macro lsParallel 2
#macro aamNone 0
#macro aamPlayOnce 1
#macro aamLoop 2
#macro aamBounceForward 3
#macro aamBounceBackward 4
#macro aamLoopBackward 5
#macro scNoOverlap 0
#macro scContainsFully 1
#macro scContainsPartially 2
#macro pmFill 0
#macro pmLines 1
#macro pmPoints 2
#macro tmmUser 0
#macro tmmObjectLinear 1
#macro tmmEyeLinear 2
#macro tmmSphere 3
#macro tmmCubeMapReflection 4
#macro tmmCubeMapNormal 5
#macro tmmCubeMapLight0 6
#macro tmmCubeMapCamera 7
#macro tiaDefault 0
#macro tiaAlphaFromIntensity 1
#macro tiaSuperBlackTransparent 2
#macro tiaLuminance 3
#macro tiaLuminanceSqrt 4
#macro tiaOpaque 5
#macro tiaTopLeftPointColorTransparent 6
#macro tiaInverseLuminance 7
#macro tiaInverseLuminanceSqrt 8
#macro tmDecal 0
#macro tmModulate 1
#macro tmBlend 2
#macro tmReplace 3
#macro bmOpaque 0
#macro bmTransparency 1
#macro bmAdditive 2
#macro bmAlphaTest50 3
#macro bmAlphaTest100 4
#macro bmModulate 5
#macro miNearest 0
#macro miLinear 1
#macro miNearestMipmapNearest 2
#macro miLinearMipmapNearest 3
#macro miNearestMipmapLinear 4
#macro miLinearMipmapLinear 5
#macro maNearest 0
#macro maLinear 1
#macro fcBufferDefault 0
#macro fcCull 1
#macro fcNoCull 2
#macro tfDefault 0
#macro tfRGB 1
#macro tfRGBA 2
#macro tfRGB16 3
#macro tfRGBA16 4
#macro tfAlpha 5
#macro tfLuminance 6
#macro tfLuminanceAlpha 7
#macro tfIntensity 8
#macro tfNormalMap 9
#macro tfRGBAFloat16 10
#macro tfRGBAFloat32 11
#macro tcDefault 0
#macro tcNone 1
#macro tcStandard 2
#macro tcHighQuality 3
#macro tcHighSpeed 4
#macro tfIsotropic 0
#macro tfAnisotropic 1
#macro pNone 0
#macro pGlossy 1
#macro pBeastView 2
#macro pOceanDepth 3
#macro pDream 4
#macro pOverBlur 5
#macro sbmTop 0
#macro sbmBottom 1
#macro sbmLeft 2
#macro sbmRight 3
#macro sbmFront 4
#macro sbmBack 5
#macro sbmClouds 6
#macro sbsFull 0
#macro sbsTopHalf 1
#macro sbsBottomHalf 2
#macro sbTopTwoThirds 3
#macro sbsTopHalfClamped 4
#macro lsmLines 0
#macro lsmCubicSpline 1
#macro lsmBezierSpline 2
#macro lsmNURBSCurve 3
#macro lsmSegments 4
#macro lnaInvisible 0
#macro lnaAxes 1
#macro lnaCube 2
#macro lnaDodecahedron 3
#macro msUp 0
#macro msDirection 1
#macro msFaceCamera 2
#macro svmAccurate 0
#macro svmDarkening 1
#macro svmOff 2
#macro osmStep 0
#macro osmStepFast 1
#macro osmQuickStep 2
#macro hrsFullGeometry 0
#macro hrsTesselated 1
#macro totTesselateAlways 0
#macro totTesselateIfVisible 1
#macro aaDefault 0
#macro aaNone 1
#macro aa2x 2
#macro aa2xHQ 3
#macro aa4x 4
#macro aa4xHQ 5
#macro afpNone 0
#macro afpLinear 1
#macro ccsDCEStandard 0
#macro ccsCollisionStandard 1
#macro ccsHybrid 2
#macro csEllipsoid 0
#macro csBox 1
#macro csFreeform 2
#macro csTerrain 3
#macro cmFineCulling 0
#macro cmGrossCulling 1
#macro glsSegments 0
#macro glsLine 1
#macro gpXY 0
#macro gpYZ 1
#macro gpXZ 2
#macro gpXYZ 3
#macro teUTF8 0
#macro teWindows 1
#macro krbtUnknown 0
#macro krbtStatic 1
#macro krbtDynamic 2
#macro krbtKinematic 3
#macro uspEveryIteration 0
#macro uspEveryFrame 1
#macro uspNever 2

