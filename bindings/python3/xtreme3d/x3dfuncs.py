import ctypes

x3d = ctypes.CDLL('./xtreme3d.dll')

# actor.pas
x3d.ActorCreate.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.ActorCreate.restype = ctypes.c_double
def ActorCreate(aFname, aMatl, aParent):
    return x3d.ActorCreate(aFname, aMatl, aParent)

x3d.ActorCopy.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorCopy.restype = ctypes.c_double
def ActorCopy(aActor, aParent):
    return x3d.ActorCopy(aActor, aParent)

x3d.ActorSetAnimationRange.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorSetAnimationRange.restype = ctypes.c_double
def ActorSetAnimationRange(aActor, aFramestart, aFrameend):
    return x3d.ActorSetAnimationRange(aActor, aFramestart, aFrameend)

x3d.ActorGetCurrentFrame.argtypes = [ctypes.c_double]
x3d.ActorGetCurrentFrame.restype = ctypes.c_double
def ActorGetCurrentFrame(aActor):
    return x3d.ActorGetCurrentFrame(aActor)

x3d.ActorSwitchToAnimation.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorSwitchToAnimation.restype = ctypes.c_double
def ActorSwitchToAnimation(aActor, aAnim, aSmooth):
    return x3d.ActorSwitchToAnimation(aActor, aAnim, aSmooth)

x3d.ActorSwitchToAnimationName.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double]
x3d.ActorSwitchToAnimationName.restype = ctypes.c_double
def ActorSwitchToAnimationName(aActor, aAnim, aSmooth):
    return x3d.ActorSwitchToAnimationName(aActor, aAnim, aSmooth)

x3d.ActorSynchronize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorSynchronize.restype = ctypes.c_double
def ActorSynchronize(aActor1, aActor2):
    return x3d.ActorSynchronize(aActor1, aActor2)

x3d.ActorSetInterval.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorSetInterval.restype = ctypes.c_double
def ActorSetInterval(aActor, aInterv):
    return x3d.ActorSetInterval(aActor, aInterv)

x3d.ActorSetAnimationMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorSetAnimationMode.restype = ctypes.c_double
def ActorSetAnimationMode(aActor, aAam):
    return x3d.ActorSetAnimationMode(aActor, aAam)

x3d.ActorSetFrameInterpolation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorSetFrameInterpolation.restype = ctypes.c_double
def ActorSetFrameInterpolation(aActor, aAfp):
    return x3d.ActorSetFrameInterpolation(aActor, aAfp)

x3d.ActorAddObject.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ActorAddObject.restype = ctypes.c_double
def ActorAddObject(aActor, aFname):
    return x3d.ActorAddObject(aActor, aFname)

x3d.ActorGetCurrentAnimation.argtypes = [ctypes.c_double]
x3d.ActorGetCurrentAnimation.restype = ctypes.c_char_p
def ActorGetCurrentAnimation(aActor):
    return x3d.ActorGetCurrentAnimation(aActor)

x3d.ActorGetFrameCount.argtypes = [ctypes.c_double]
x3d.ActorGetFrameCount.restype = ctypes.c_double
def ActorGetFrameCount(aActor):
    return x3d.ActorGetFrameCount(aActor)

x3d.ActorGetBoneCount.argtypes = [ctypes.c_double]
x3d.ActorGetBoneCount.restype = ctypes.c_double
def ActorGetBoneCount(aActor):
    return x3d.ActorGetBoneCount(aActor)

x3d.ActorGetBoneByName.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ActorGetBoneByName.restype = ctypes.c_double
def ActorGetBoneByName(aActor, aName):
    return x3d.ActorGetBoneByName(aActor, aName)

x3d.ActorGetBoneRotation.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorGetBoneRotation.restype = ctypes.c_double
def ActorGetBoneRotation(aActor, aBone, aInd):
    return x3d.ActorGetBoneRotation(aActor, aBone, aInd)

x3d.ActorGetBonePosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorGetBonePosition.restype = ctypes.c_double
def ActorGetBonePosition(aActor, aBone, aInd):
    return x3d.ActorGetBonePosition(aActor, aBone, aInd)

x3d.ActorBoneExportMatrix.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorBoneExportMatrix.restype = ctypes.c_double
def ActorBoneExportMatrix(aActor, aBone, aObj):
    return x3d.ActorBoneExportMatrix(aActor, aBone, aObj)

x3d.ActorMakeSkeletalTranslationStatic.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorMakeSkeletalTranslationStatic.restype = ctypes.c_double
def ActorMakeSkeletalTranslationStatic(aActor, aAnim):
    return x3d.ActorMakeSkeletalTranslationStatic(aActor, aAnim)

x3d.ActorMakeSkeletalRotationDelta.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorMakeSkeletalRotationDelta.restype = ctypes.c_double
def ActorMakeSkeletalRotationDelta(aActor, aAnim):
    return x3d.ActorMakeSkeletalRotationDelta(aActor, aAnim)

x3d.ActorShowSkeleton.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorShowSkeleton.restype = ctypes.c_double
def ActorShowSkeleton(aActor, aMode):
    return x3d.ActorShowSkeleton(aActor, aMode)

x3d.AnimationBlenderCreate.argtypes = []
x3d.AnimationBlenderCreate.restype = ctypes.c_double
def AnimationBlenderCreate():
    return x3d.AnimationBlenderCreate()

x3d.AnimationBlenderSetActor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AnimationBlenderSetActor.restype = ctypes.c_double
def AnimationBlenderSetActor(aAnim, aActor):
    return x3d.AnimationBlenderSetActor(aAnim, aActor)

x3d.AnimationBlenderSetAnimation.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.AnimationBlenderSetAnimation.restype = ctypes.c_double
def AnimationBlenderSetAnimation(aAnim, aName):
    return x3d.AnimationBlenderSetAnimation(aAnim, aName)

x3d.AnimationBlenderSetRatio.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AnimationBlenderSetRatio.restype = ctypes.c_double
def AnimationBlenderSetRatio(aAnim, aRat):
    return x3d.AnimationBlenderSetRatio(aAnim, aRat)

x3d.ActorLoadQ3TagList.argtypes = [ctypes.c_char_p]
x3d.ActorLoadQ3TagList.restype = ctypes.c_double
def ActorLoadQ3TagList(aFname):
    return x3d.ActorLoadQ3TagList(aFname)

x3d.ActorQ3TagExportMatrix.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_char_p, ctypes.c_double]
x3d.ActorQ3TagExportMatrix.restype = ctypes.c_double
def ActorQ3TagExportMatrix(aActor, aTaglist, aTagname, aObj):
    return x3d.ActorQ3TagExportMatrix(aActor, aTaglist, aTagname, aObj)

x3d.ActorLoadQ3Animations.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_char_p]
x3d.ActorLoadQ3Animations.restype = ctypes.c_double
def ActorLoadQ3Animations(aActor, aFname, aClas):
    return x3d.ActorLoadQ3Animations(aActor, aFname, aClas)

x3d.ActorMeshObjectsCount.argtypes = [ctypes.c_double]
x3d.ActorMeshObjectsCount.restype = ctypes.c_double
def ActorMeshObjectsCount(aActor):
    return x3d.ActorMeshObjectsCount(aActor)

x3d.ActorFaceGroupsCount.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorFaceGroupsCount.restype = ctypes.c_double
def ActorFaceGroupsCount(aActor, aMeshobject):
    return x3d.ActorFaceGroupsCount(aActor, aMeshobject)

x3d.ActorFaceGroupGetMaterialName.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorFaceGroupGetMaterialName.restype = ctypes.c_char_p
def ActorFaceGroupGetMaterialName(aActor, aMeshobject, aFacegroup):
    return x3d.ActorFaceGroupGetMaterialName(aActor, aMeshobject, aFacegroup)

x3d.ActorFaceGroupSetMaterial.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.ActorFaceGroupSetMaterial.restype = ctypes.c_double
def ActorFaceGroupSetMaterial(aActor, aMeshobject, aFacegroup, aMtrl):
    return x3d.ActorFaceGroupSetMaterial(aActor, aMeshobject, aFacegroup, aMtrl)

x3d.ActorMoveBone.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorMoveBone.restype = ctypes.c_double
def ActorMoveBone(aActor, aBoneindex, aX, aY, aZ):
    return x3d.ActorMoveBone(aActor, aBoneindex, aX, aY, aZ)

x3d.ActorRotateBone.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorRotateBone.restype = ctypes.c_double
def ActorRotateBone(aActor, aBoneindex, aX, aY, aZ):
    return x3d.ActorRotateBone(aActor, aBoneindex, aX, aY, aZ)

x3d.ActorMeshSetVisible.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorMeshSetVisible.restype = ctypes.c_double
def ActorMeshSetVisible(aActor, aMesh, aMode):
    return x3d.ActorMeshSetVisible(aActor, aMesh, aMode)

x3d.ActorGetAnimationName.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorGetAnimationName.restype = ctypes.c_char_p
def ActorGetAnimationName(aActor, aInd):
    return x3d.ActorGetAnimationName(aActor, aInd)

x3d.ActorGetAnimationCount.argtypes = [ctypes.c_double]
x3d.ActorGetAnimationCount.restype = ctypes.c_double
def ActorGetAnimationCount(aActor):
    return x3d.ActorGetAnimationCount(aActor)

x3d.ActorAnimationDestroy.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorAnimationDestroy.restype = ctypes.c_double
def ActorAnimationDestroy(aActor, aIndex):
    return x3d.ActorAnimationDestroy(aActor, aIndex)

x3d.ActorAnimationNextFrame.argtypes = [ctypes.c_double]
x3d.ActorAnimationNextFrame.restype = ctypes.c_double
def ActorAnimationNextFrame(aActor):
    return x3d.ActorAnimationNextFrame(aActor)

x3d.ActorAnimationPrevFrame.argtypes = [ctypes.c_double]
x3d.ActorAnimationPrevFrame.restype = ctypes.c_double
def ActorAnimationPrevFrame(aActor):
    return x3d.ActorAnimationPrevFrame(aActor)

x3d.ActorSetFrame.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorSetFrame.restype = ctypes.c_double
def ActorSetFrame(aActor, aFrame):
    return x3d.ActorSetFrame(aActor, aFrame)

x3d.ActorTriangleCount.argtypes = [ctypes.c_double]
x3d.ActorTriangleCount.restype = ctypes.c_double
def ActorTriangleCount(aActor):
    return x3d.ActorTriangleCount(aActor)

x3d.ActorSetReference.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorSetReference.restype = ctypes.c_double
def ActorSetReference(aActor, aAar):
    return x3d.ActorSetReference(aActor, aAar)


# blur.pas
x3d.BlurCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BlurCreate.restype = ctypes.c_double
def BlurCreate(aTargetobj, aParent):
    return x3d.BlurCreate(aTargetobj, aParent)

x3d.BlurSetPreset.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BlurSetPreset.restype = ctypes.c_double
def BlurSetPreset(aBlur, aP):
    return x3d.BlurSetPreset(aBlur, aP)

x3d.BlurSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.BlurSetOptions.restype = ctypes.c_double
def BlurSetOptions(aBlur, aDelta, aLeft, aTop, aRight, aBottom):
    return x3d.BlurSetOptions(aBlur, aDelta, aLeft, aTop, aRight, aBottom)

x3d.BlurSetResolution.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BlurSetResolution.restype = ctypes.c_double
def BlurSetResolution(aBlur, aRes):
    return x3d.BlurSetResolution(aBlur, aRes)

x3d.BlurSetColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BlurSetColor.restype = ctypes.c_double
def BlurSetColor(aBlur, aCol):
    return x3d.BlurSetColor(aBlur, aCol)

x3d.BlurSetBlendingMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BlurSetBlendingMode.restype = ctypes.c_double
def BlurSetBlendingMode(aBlur, aBm):
    return x3d.BlurSetBlendingMode(aBlur, aBm)


# camera.pas
x3d.CameraCreate.argtypes = [ctypes.c_double]
x3d.CameraCreate.restype = ctypes.c_double
def CameraCreate(aParent):
    return x3d.CameraCreate(aParent)

x3d.CameraSetStyle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetStyle.restype = ctypes.c_double
def CameraSetStyle(aCamera, aCs):
    return x3d.CameraSetStyle(aCamera, aCs)

x3d.CameraSetFocal.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetFocal.restype = ctypes.c_double
def CameraSetFocal(aCamera, aFov):
    return x3d.CameraSetFocal(aCamera, aFov)

x3d.CameraSetSceneScale.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetSceneScale.restype = ctypes.c_double
def CameraSetSceneScale(aCamera, aScale):
    return x3d.CameraSetSceneScale(aCamera, aScale)

x3d.CameraScaleScene.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraScaleScene.restype = ctypes.c_double
def CameraScaleScene(aCamera, aScale):
    return x3d.CameraScaleScene(aCamera, aScale)

x3d.CameraSetViewDepth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetViewDepth.restype = ctypes.c_double
def CameraSetViewDepth(aCamera, aDepth):
    return x3d.CameraSetViewDepth(aCamera, aDepth)

x3d.CameraSetTargetObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetTargetObject.restype = ctypes.c_double
def CameraSetTargetObject(aCamera, aObj):
    return x3d.CameraSetTargetObject(aCamera, aObj)

x3d.CameraMoveAroundTarget.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraMoveAroundTarget.restype = ctypes.c_double
def CameraMoveAroundTarget(aCamera, aPitch, aTurn):
    return x3d.CameraMoveAroundTarget(aCamera, aPitch, aTurn)

x3d.CameraSetDistanceToTarget.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetDistanceToTarget.restype = ctypes.c_double
def CameraSetDistanceToTarget(aCamera, aDistance):
    return x3d.CameraSetDistanceToTarget(aCamera, aDistance)

x3d.CameraGetDistanceToTarget.argtypes = [ctypes.c_double]
x3d.CameraGetDistanceToTarget.restype = ctypes.c_double
def CameraGetDistanceToTarget(aCamera):
    return x3d.CameraGetDistanceToTarget(aCamera)

x3d.CameraCopyToTexture.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.CameraCopyToTexture.restype = ctypes.c_double
def CameraCopyToTexture(aCamera, aMtrl, aWidth, aHeight):
    return x3d.CameraCopyToTexture(aCamera, aMtrl, aWidth, aHeight)

x3d.CameraGetNearPlane.argtypes = [ctypes.c_double]
x3d.CameraGetNearPlane.restype = ctypes.c_double
def CameraGetNearPlane(aCamera):
    return x3d.CameraGetNearPlane(aCamera)

x3d.CameraSetNearPlaneBias.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetNearPlaneBias.restype = ctypes.c_double
def CameraSetNearPlaneBias(aCamera, aBias):
    return x3d.CameraSetNearPlaneBias(aCamera, aBias)

x3d.CameraAbsoluteVectorToTarget.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraAbsoluteVectorToTarget.restype = ctypes.c_double
def CameraAbsoluteVectorToTarget(aCamera, aInd):
    return x3d.CameraAbsoluteVectorToTarget(aCamera, aInd)

x3d.CameraAbsoluteRightVectorToTarget.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraAbsoluteRightVectorToTarget.restype = ctypes.c_double
def CameraAbsoluteRightVectorToTarget(aCamera, aInd):
    return x3d.CameraAbsoluteRightVectorToTarget(aCamera, aInd)

x3d.CameraAbsoluteUpVectorToTarget.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraAbsoluteUpVectorToTarget.restype = ctypes.c_double
def CameraAbsoluteUpVectorToTarget(aCamera, aInd):
    return x3d.CameraAbsoluteUpVectorToTarget(aCamera, aInd)

x3d.CameraZoomAll.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraZoomAll.restype = ctypes.c_double
def CameraZoomAll(aCamera, aViewer):
    return x3d.CameraZoomAll(aCamera, aViewer)

x3d.CameraScreenDeltaToVector.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraScreenDeltaToVector.restype = ctypes.c_double
def CameraScreenDeltaToVector(aCamera, aDx, aDy, aRatio, aNx, aNy, aNz, aInd):
    return x3d.CameraScreenDeltaToVector(aCamera, aDx, aDy, aRatio, aNx, aNy, aNz, aInd)

x3d.CameraScreenDeltaToVectorXY.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraScreenDeltaToVectorXY.restype = ctypes.c_double
def CameraScreenDeltaToVectorXY(aCamera, aDx, aDy, aRatio, aInd):
    return x3d.CameraScreenDeltaToVectorXY(aCamera, aDx, aDy, aRatio, aInd)

x3d.CameraScreenDeltaToVectorXZ.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraScreenDeltaToVectorXZ.restype = ctypes.c_double
def CameraScreenDeltaToVectorXZ(aCamera, aDx, aDy, aRatio, aInd):
    return x3d.CameraScreenDeltaToVectorXZ(aCamera, aDx, aDy, aRatio, aInd)

x3d.CameraScreenDeltaToVectorYZ.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraScreenDeltaToVectorYZ.restype = ctypes.c_double
def CameraScreenDeltaToVectorYZ(aCamera, aDx, aDy, aRatio, aInd):
    return x3d.CameraScreenDeltaToVectorYZ(aCamera, aDx, aDy, aRatio, aInd)

x3d.CameraAbsoluteEyeSpaceVector.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraAbsoluteEyeSpaceVector.restype = ctypes.c_double
def CameraAbsoluteEyeSpaceVector(aCamera, aFordist, aRightdist, aUpdist, aInd):
    return x3d.CameraAbsoluteEyeSpaceVector(aCamera, aFordist, aRightdist, aUpdist, aInd)

x3d.CameraSetAutoLeveling.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraSetAutoLeveling.restype = ctypes.c_double
def CameraSetAutoLeveling(aCamera, aFactor):
    return x3d.CameraSetAutoLeveling(aCamera, aFactor)

x3d.CameraMoveInEyeSpace.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraMoveInEyeSpace.restype = ctypes.c_double
def CameraMoveInEyeSpace(aCamera, aFordist, aRightdist, aUpdist):
    return x3d.CameraMoveInEyeSpace(aCamera, aFordist, aRightdist, aUpdist)

x3d.CameraMoveTargetInEyeSpace.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraMoveTargetInEyeSpace.restype = ctypes.c_double
def CameraMoveTargetInEyeSpace(aCamera, aFordist, aRightdist, aUpdist):
    return x3d.CameraMoveTargetInEyeSpace(aCamera, aFordist, aRightdist, aUpdist)

x3d.CameraPointInFront.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CameraPointInFront.restype = ctypes.c_double
def CameraPointInFront(aCamera, aX, aY, aZ):
    return x3d.CameraPointInFront(aCamera, aX, aY, aZ)

x3d.CameraGetFieldOfView.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CameraGetFieldOfView.restype = ctypes.c_double
def CameraGetFieldOfView(aCamera, aVpdim):
    return x3d.CameraGetFieldOfView(aCamera, aVpdim)


# clipplane.pas
x3d.ClipPlaneCreate.argtypes = [ctypes.c_double]
x3d.ClipPlaneCreate.restype = ctypes.c_double
def ClipPlaneCreate(aParent):
    return x3d.ClipPlaneCreate(aParent)

x3d.ClipPlaneEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ClipPlaneEnable.restype = ctypes.c_double
def ClipPlaneEnable(aCplane, aMode):
    return x3d.ClipPlaneEnable(aCplane, aMode)

x3d.ClipPlaneSetPlane.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ClipPlaneSetPlane.restype = ctypes.c_double
def ClipPlaneSetPlane(aCplane, aPx, aPy, aPz, aNx, aNy, aNz):
    return x3d.ClipPlaneSetPlane(aCplane, aPx, aPy, aPz, aNx, aNy, aNz)


# color.pas
x3d.MakeColorRGB.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MakeColorRGB.restype = ctypes.c_double
def MakeColorRGB(aR, aG, aB):
    return x3d.MakeColorRGB(aR, aG, aB)

x3d.MakeColorRGBFloat.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MakeColorRGBFloat.restype = ctypes.c_double
def MakeColorRGBFloat(aR, aG, aB):
    return x3d.MakeColorRGBFloat(aR, aG, aB)


# dce.pas
x3d.DceManagerCreate.argtypes = []
x3d.DceManagerCreate.restype = ctypes.c_double
def DceManagerCreate():
    return x3d.DceManagerCreate()

x3d.DceManagerStep.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceManagerStep.restype = ctypes.c_double
def DceManagerStep(aMan, aDt):
    return x3d.DceManagerStep(aMan, aDt)

x3d.DceManagerSetGravity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceManagerSetGravity.restype = ctypes.c_double
def DceManagerSetGravity(aMan, aGrav):
    return x3d.DceManagerSetGravity(aMan, aGrav)

x3d.DceManagerSetWorldDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceManagerSetWorldDirection.restype = ctypes.c_double
def DceManagerSetWorldDirection(aMan, aX, aY, aZ):
    return x3d.DceManagerSetWorldDirection(aMan, aX, aY, aZ)

x3d.DceManagerSetWorldScale.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceManagerSetWorldScale.restype = ctypes.c_double
def DceManagerSetWorldScale(aMan, aScale):
    return x3d.DceManagerSetWorldScale(aMan, aScale)

x3d.DceManagerSetMovementScale.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceManagerSetMovementScale.restype = ctypes.c_double
def DceManagerSetMovementScale(aMan, aScale):
    return x3d.DceManagerSetMovementScale(aMan, aScale)

x3d.DceManagerSetLayers.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceManagerSetLayers.restype = ctypes.c_double
def DceManagerSetLayers(aMan, aMode):
    return x3d.DceManagerSetLayers(aMan, aMode)

x3d.DceManagerSetManualStep.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceManagerSetManualStep.restype = ctypes.c_double
def DceManagerSetManualStep(aMan, aMode):
    return x3d.DceManagerSetManualStep(aMan, aMode)

x3d.DceDynamicSetManager.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetManager.restype = ctypes.c_double
def DceDynamicSetManager(aObj, aMan):
    return x3d.DceDynamicSetManager(aObj, aMan)

x3d.DceDynamicSetActive.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetActive.restype = ctypes.c_double
def DceDynamicSetActive(aObj, aMode):
    return x3d.DceDynamicSetActive(aObj, aMode)

x3d.DceDynamicIsActive.argtypes = [ctypes.c_double]
x3d.DceDynamicIsActive.restype = ctypes.c_double
def DceDynamicIsActive(aObj):
    return x3d.DceDynamicIsActive(aObj)

x3d.DceDynamicSetUseGravity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetUseGravity.restype = ctypes.c_double
def DceDynamicSetUseGravity(aObj, aMode):
    return x3d.DceDynamicSetUseGravity(aObj, aMode)

x3d.DceDynamicSetLayer.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetLayer.restype = ctypes.c_double
def DceDynamicSetLayer(aObj, aLayer):
    return x3d.DceDynamicSetLayer(aObj, aLayer)

x3d.DceDynamicGetLayer.argtypes = [ctypes.c_double]
x3d.DceDynamicGetLayer.restype = ctypes.c_double
def DceDynamicGetLayer(aObj):
    return x3d.DceDynamicGetLayer(aObj)

x3d.DceDynamicSetSolid.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetSolid.restype = ctypes.c_double
def DceDynamicSetSolid(aObj, aMode):
    return x3d.DceDynamicSetSolid(aObj, aMode)

x3d.DceDynamicSetFriction.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetFriction.restype = ctypes.c_double
def DceDynamicSetFriction(aObj, aFriction):
    return x3d.DceDynamicSetFriction(aObj, aFriction)

x3d.DceDynamicSetBounce.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetBounce.restype = ctypes.c_double
def DceDynamicSetBounce(aObj, aBounce):
    return x3d.DceDynamicSetBounce(aObj, aBounce)

x3d.DceDynamicSetSize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetSize.restype = ctypes.c_double
def DceDynamicSetSize(aObj, aX, aY, aZ):
    return x3d.DceDynamicSetSize(aObj, aX, aY, aZ)

x3d.DceDynamicSetSlideOrBounce.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetSlideOrBounce.restype = ctypes.c_double
def DceDynamicSetSlideOrBounce(aObj, aMode):
    return x3d.DceDynamicSetSlideOrBounce(aObj, aMode)

x3d.DceDynamicApplyAcceleration.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicApplyAcceleration.restype = ctypes.c_double
def DceDynamicApplyAcceleration(aObj, aX, aY, aZ):
    return x3d.DceDynamicApplyAcceleration(aObj, aX, aY, aZ)

x3d.DceDynamicApplyAbsAcceleration.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicApplyAbsAcceleration.restype = ctypes.c_double
def DceDynamicApplyAbsAcceleration(aObj, aX, aY, aZ):
    return x3d.DceDynamicApplyAbsAcceleration(aObj, aX, aY, aZ)

x3d.DceDynamicStopAcceleration.argtypes = [ctypes.c_double]
x3d.DceDynamicStopAcceleration.restype = ctypes.c_double
def DceDynamicStopAcceleration(aObj):
    return x3d.DceDynamicStopAcceleration(aObj)

x3d.DceDynamicStopAbsAcceleration.argtypes = [ctypes.c_double]
x3d.DceDynamicStopAbsAcceleration.restype = ctypes.c_double
def DceDynamicStopAbsAcceleration(aObj):
    return x3d.DceDynamicStopAbsAcceleration(aObj)

x3d.DceDynamicJump.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicJump.restype = ctypes.c_double
def DceDynamicJump(aObj, aHeight, aSpeed):
    return x3d.DceDynamicJump(aObj, aHeight, aSpeed)

x3d.DceDynamicMove.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicMove.restype = ctypes.c_double
def DceDynamicMove(aObj, aX, aY, aZ, aDelta):
    return x3d.DceDynamicMove(aObj, aX, aY, aZ, aDelta)

x3d.DceDynamicMoveTo.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicMoveTo.restype = ctypes.c_double
def DceDynamicMoveTo(aObj, aX, aY, aZ, aAmount):
    return x3d.DceDynamicMoveTo(aObj, aX, aY, aZ, aAmount)

x3d.DceDynamicSetVelocity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetVelocity.restype = ctypes.c_double
def DceDynamicSetVelocity(aObj, aX, aY, aZ):
    return x3d.DceDynamicSetVelocity(aObj, aX, aY, aZ)

x3d.DceDynamicInGround.argtypes = [ctypes.c_double]
x3d.DceDynamicInGround.restype = ctypes.c_double
def DceDynamicInGround(aObj):
    return x3d.DceDynamicInGround(aObj)

x3d.DceDynamicSetMaxRecursionDepth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetMaxRecursionDepth.restype = ctypes.c_double
def DceDynamicSetMaxRecursionDepth(aObj, aDepth):
    return x3d.DceDynamicSetMaxRecursionDepth(aObj, aDepth)

x3d.DceStaticSetManager.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetManager.restype = ctypes.c_double
def DceStaticSetManager(aObj, aMan):
    return x3d.DceStaticSetManager(aObj, aMan)

x3d.DceStaticSetActive.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetActive.restype = ctypes.c_double
def DceStaticSetActive(aObj, aMode):
    return x3d.DceStaticSetActive(aObj, aMode)

x3d.DceStaticSetShape.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetShape.restype = ctypes.c_double
def DceStaticSetShape(aObj, aMode):
    return x3d.DceStaticSetShape(aObj, aMode)

x3d.DceStaticSetLayer.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetLayer.restype = ctypes.c_double
def DceStaticSetLayer(aObj, aLayer):
    return x3d.DceStaticSetLayer(aObj, aLayer)

x3d.DceStaticSetSize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetSize.restype = ctypes.c_double
def DceStaticSetSize(aObj, aX, aY, aZ):
    return x3d.DceStaticSetSize(aObj, aX, aY, aZ)

x3d.DceStaticSetSolid.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetSolid.restype = ctypes.c_double
def DceStaticSetSolid(aObj, aMode):
    return x3d.DceStaticSetSolid(aObj, aMode)

x3d.DceStaticSetFriction.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetFriction.restype = ctypes.c_double
def DceStaticSetFriction(aObj, aFriction):
    return x3d.DceStaticSetFriction(aObj, aFriction)

x3d.DceStaticSetBounceFactor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceStaticSetBounceFactor.restype = ctypes.c_double
def DceStaticSetBounceFactor(aObj, aBfactor):
    return x3d.DceStaticSetBounceFactor(aObj, aBfactor)

x3d.DceDynamicGetVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicGetVelocity.restype = ctypes.c_double
def DceDynamicGetVelocity(aObj, aInd):
    return x3d.DceDynamicGetVelocity(aObj, aInd)

x3d.DceDynamicSetAbsVelocity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicSetAbsVelocity.restype = ctypes.c_double
def DceDynamicSetAbsVelocity(aObj, aX, aY, aZ):
    return x3d.DceDynamicSetAbsVelocity(aObj, aX, aY, aZ)

x3d.DceDynamicGetAbsVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DceDynamicGetAbsVelocity.restype = ctypes.c_double
def DceDynamicGetAbsVelocity(aObj, aInd):
    return x3d.DceDynamicGetAbsVelocity(aObj, aInd)

x3d.DceDynamicApplyImpulse.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicApplyImpulse.restype = ctypes.c_double
def DceDynamicApplyImpulse(aObj, aX, aY, aZ):
    return x3d.DceDynamicApplyImpulse(aObj, aX, aY, aZ)

x3d.DceDynamicApplyAbsImpulse.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DceDynamicApplyAbsImpulse.restype = ctypes.c_double
def DceDynamicApplyAbsImpulse(aObj, aX, aY, aZ):
    return x3d.DceDynamicApplyAbsImpulse(aObj, aX, aY, aZ)


# dummycube.pas
x3d.DummycubeCreate.argtypes = [ctypes.c_double]
x3d.DummycubeCreate.restype = ctypes.c_double
def DummycubeCreate(aParent):
    return x3d.DummycubeCreate(aParent)

x3d.DummycubeAmalgamate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DummycubeAmalgamate.restype = ctypes.c_double
def DummycubeAmalgamate(aObj, aMode):
    return x3d.DummycubeAmalgamate(aObj, aMode)

x3d.DummycubeSetCameraMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DummycubeSetCameraMode.restype = ctypes.c_double
def DummycubeSetCameraMode(aObj, aCim):
    return x3d.DummycubeSetCameraMode(aObj, aCim)

x3d.DummycubeSetVisible.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DummycubeSetVisible.restype = ctypes.c_double
def DummycubeSetVisible(aObj, aMode):
    return x3d.DummycubeSetVisible(aObj, aMode)

x3d.DummycubeSetEdgeColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DummycubeSetEdgeColor.restype = ctypes.c_double
def DummycubeSetEdgeColor(aObj, aColor):
    return x3d.DummycubeSetEdgeColor(aObj, aColor)

x3d.DummycubeSetCubeSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DummycubeSetCubeSize.restype = ctypes.c_double
def DummycubeSetCubeSize(aObj, aSize):
    return x3d.DummycubeSetCubeSize(aObj, aSize)


# engine.pas
x3d.EngineCreate.argtypes = []
x3d.EngineCreate.restype = ctypes.c_double
def EngineCreate():
    return x3d.EngineCreate()

x3d.EngineDestroy.argtypes = []
x3d.EngineDestroy.restype = ctypes.c_double
def EngineDestroy():
    return x3d.EngineDestroy()

x3d.EngineSetObjectsSorting.argtypes = [ctypes.c_double]
x3d.EngineSetObjectsSorting.restype = ctypes.c_double
def EngineSetObjectsSorting(aOs):
    return x3d.EngineSetObjectsSorting(aOs)

x3d.EngineSetCulling.argtypes = [ctypes.c_double]
x3d.EngineSetCulling.restype = ctypes.c_double
def EngineSetCulling(aVc):
    return x3d.EngineSetCulling(aVc)

x3d.EngineUpdate.argtypes = [ctypes.c_double]
x3d.EngineUpdate.restype = ctypes.c_double
def EngineUpdate(aDelta):
    return x3d.EngineUpdate(aDelta)

x3d.EngineSaveScene.argtypes = [ctypes.c_char_p]
x3d.EngineSaveScene.restype = ctypes.c_double
def EngineSaveScene(aFilename):
    return x3d.EngineSaveScene(aFilename)

x3d.EngineLoadScene.argtypes = [ctypes.c_char_p]
x3d.EngineLoadScene.restype = ctypes.c_double
def EngineLoadScene(aFilename):
    return x3d.EngineLoadScene(aFilename)

x3d.EngineRootObject.argtypes = []
x3d.EngineRootObject.restype = ctypes.c_double
def EngineRootObject():
    return x3d.EngineRootObject()

x3d.EngineShowLoadingErrors.argtypes = [ctypes.c_double]
x3d.EngineShowLoadingErrors.restype = ctypes.c_double
def EngineShowLoadingErrors(aMode):
    return x3d.EngineShowLoadingErrors(aMode)

x3d.EngineSetMaxLights.argtypes = [ctypes.c_double]
x3d.EngineSetMaxLights.restype = ctypes.c_double
def EngineSetMaxLights(aLights):
    return x3d.EngineSetMaxLights(aLights)

x3d.EngineGetTimeStep.argtypes = []
x3d.EngineGetTimeStep.restype = ctypes.c_double
def EngineGetTimeStep():
    return x3d.EngineGetTimeStep()

x3d.EngineGetLastRaycastPosition.argtypes = [ctypes.c_double]
x3d.EngineGetLastRaycastPosition.restype = ctypes.c_double
def EngineGetLastRaycastPosition(aInd):
    return x3d.EngineGetLastRaycastPosition(aInd)

x3d.EngineGetLastRaycastNormal.argtypes = [ctypes.c_double]
x3d.EngineGetLastRaycastNormal.restype = ctypes.c_double
def EngineGetLastRaycastNormal(aInd):
    return x3d.EngineGetLastRaycastNormal(aInd)

x3d.PointerToReal.argtypes = [ctypes.c_char_p]
x3d.PointerToReal.restype = ctypes.c_double
def PointerToReal(aP):
    return x3d.PointerToReal(aP)


# fbo.pas
x3d.FBOCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FBOCreate.restype = ctypes.c_double
def FBOCreate(aW, aH, aParent):
    return x3d.FBOCreate(aW, aH, aParent)

x3d.FBOSetActive.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetActive.restype = ctypes.c_double
def FBOSetActive(aFbo, aMode):
    return x3d.FBOSetActive(aFbo, aMode)

x3d.FBOSetAspect.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetAspect.restype = ctypes.c_double
def FBOSetAspect(aFbo, aAspect):
    return x3d.FBOSetAspect(aFbo, aAspect)

x3d.FBOSetPickableTarget.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetPickableTarget.restype = ctypes.c_double
def FBOSetPickableTarget(aFbo, aMode):
    return x3d.FBOSetPickableTarget(aFbo, aMode)

x3d.FBOSetSize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FBOSetSize.restype = ctypes.c_double
def FBOSetSize(aFbo, aWidth, aHeight):
    return x3d.FBOSetSize(aFbo, aWidth, aHeight)

x3d.FBOSetCamera.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetCamera.restype = ctypes.c_double
def FBOSetCamera(aFbo, aCam):
    return x3d.FBOSetCamera(aFbo, aCam)

x3d.FBOSetRootObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetRootObject.restype = ctypes.c_double
def FBOSetRootObject(aFbo, aObj):
    return x3d.FBOSetRootObject(aFbo, aObj)

x3d.FBOSetBackgroundColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetBackgroundColor.restype = ctypes.c_double
def FBOSetBackgroundColor(aFbo, aColor):
    return x3d.FBOSetBackgroundColor(aFbo, aColor)

x3d.FBOSetEnabledRenderBuffers.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FBOSetEnabledRenderBuffers.restype = ctypes.c_double
def FBOSetEnabledRenderBuffers(aFbo, aDepth, aStencil):
    return x3d.FBOSetEnabledRenderBuffers(aFbo, aDepth, aStencil)

x3d.FBOSetSceneScaleFactor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetSceneScaleFactor.restype = ctypes.c_double
def FBOSetSceneScaleFactor(aFbo, aScalefactor):
    return x3d.FBOSetSceneScaleFactor(aFbo, aScalefactor)

x3d.FBOSetTargetVisibility.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetTargetVisibility.restype = ctypes.c_double
def FBOSetTargetVisibility(aFbo, aTv):
    return x3d.FBOSetTargetVisibility(aFbo, aTv)

x3d.FBOSetMaterialLibrary.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetMaterialLibrary.restype = ctypes.c_double
def FBOSetMaterialLibrary(aFbo, aMatlib):
    return x3d.FBOSetMaterialLibrary(aFbo, aMatlib)

x3d.FBOSetColorTextureName.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.FBOSetColorTextureName.restype = ctypes.c_double
def FBOSetColorTextureName(aFbo, aName):
    return x3d.FBOSetColorTextureName(aFbo, aName)

x3d.FBOSetDepthTextureName.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.FBOSetDepthTextureName.restype = ctypes.c_double
def FBOSetDepthTextureName(aFbo, aName):
    return x3d.FBOSetDepthTextureName(aFbo, aName)

x3d.FBOSetClearOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FBOSetClearOptions.restype = ctypes.c_double
def FBOSetClearOptions(aFbo, aClearcolor, aCleardepth, aClearstencil, aUsebufferbackground):
    return x3d.FBOSetClearOptions(aFbo, aClearcolor, aCleardepth, aClearstencil, aUsebufferbackground)

x3d.FBOSetStencilPrecision.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetStencilPrecision.restype = ctypes.c_double
def FBOSetStencilPrecision(aFbo, aSp):
    return x3d.FBOSetStencilPrecision(aFbo, aSp)

x3d.FBOSetShadowMapMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FBOSetShadowMapMode.restype = ctypes.c_double
def FBOSetShadowMapMode(aFbo, aMode):
    return x3d.FBOSetShadowMapMode(aFbo, aMode)


# firefx.pas
x3d.FireFXManagerCreate.argtypes = []
x3d.FireFXManagerCreate.restype = ctypes.c_double
def FireFXManagerCreate():
    return x3d.FireFXManagerCreate()

x3d.FireFXCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXCreate.restype = ctypes.c_double
def FireFXCreate(aMngr, aObj):
    return x3d.FireFXCreate(aMngr, aObj)

x3d.FireFXSetColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FireFXSetColor.restype = ctypes.c_double
def FireFXSetColor(aMngr, aIncolor, aInalpha, aOutcolor, aOutalpha):
    return x3d.FireFXSetColor(aMngr, aIncolor, aInalpha, aOutcolor, aOutalpha)

x3d.FireFXSetMaxParticles.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetMaxParticles.restype = ctypes.c_double
def FireFXSetMaxParticles(aMngr, aParticles):
    return x3d.FireFXSetMaxParticles(aMngr, aParticles)

x3d.FireFXSetParticleSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetParticleSize.restype = ctypes.c_double
def FireFXSetParticleSize(aMngr, aSize):
    return x3d.FireFXSetParticleSize(aMngr, aSize)

x3d.FireFXSetDensity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetDensity.restype = ctypes.c_double
def FireFXSetDensity(aMngr, aDensity):
    return x3d.FireFXSetDensity(aMngr, aDensity)

x3d.FireFXSetEvaporation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetEvaporation.restype = ctypes.c_double
def FireFXSetEvaporation(aMngr, aEvaporation):
    return x3d.FireFXSetEvaporation(aMngr, aEvaporation)

x3d.FireFXSetCrown.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetCrown.restype = ctypes.c_double
def FireFXSetCrown(aMngr, aCrown):
    return x3d.FireFXSetCrown(aMngr, aCrown)

x3d.FireFXSetLife.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetLife.restype = ctypes.c_double
def FireFXSetLife(aMngr, aLife):
    return x3d.FireFXSetLife(aMngr, aLife)

x3d.FireFXSetBurst.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetBurst.restype = ctypes.c_double
def FireFXSetBurst(aMngr, aBurst):
    return x3d.FireFXSetBurst(aMngr, aBurst)

x3d.FireFXSetRadius.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FireFXSetRadius.restype = ctypes.c_double
def FireFXSetRadius(aMngr, aRadius):
    return x3d.FireFXSetRadius(aMngr, aRadius)

x3d.FireFXExplosion.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FireFXExplosion.restype = ctypes.c_double
def FireFXExplosion(aMngr, aIsp, aMaxsp, aLbf):
    return x3d.FireFXExplosion(aMngr, aIsp, aMaxsp, aLbf)

x3d.FireFXRingExplosion.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FireFXRingExplosion.restype = ctypes.c_double
def FireFXRingExplosion(aMngr, aIsp, aMaxsp, aLbf, aRx, aRy, aRz, aSx, aSy, aSz):
    return x3d.FireFXRingExplosion(aMngr, aIsp, aMaxsp, aLbf, aRx, aRy, aRz, aSx, aSy, aSz)


# fonttext.pas
x3d.BmpfontCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.BmpfontCreate.restype = ctypes.c_double
def BmpfontCreate(aW, aH, aHspace, aVspace, aIntx, aInty, aChstart, aChend):
    return x3d.BmpfontCreate(aW, aH, aHspace, aVspace, aIntx, aInty, aChstart, aChend)

x3d.BmpfontLoad.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.BmpfontLoad.restype = ctypes.c_double
def BmpfontLoad(aFont, aMtrl):
    return x3d.BmpfontLoad(aFont, aMtrl)

x3d.WindowsBitmapfontCreate.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.WindowsBitmapfontCreate.restype = ctypes.c_double
def WindowsBitmapfontCreate(aNm, aSize, aChstart, aChend):
    return x3d.WindowsBitmapfontCreate(aNm, aSize, aChstart, aChend)

x3d.HUDTextCreate.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double]
x3d.HUDTextCreate.restype = ctypes.c_double
def HUDTextCreate(aFont, aTxt, aParent):
    return x3d.HUDTextCreate(aFont, aTxt, aParent)

x3d.HUDTextSetRotation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDTextSetRotation.restype = ctypes.c_double
def HUDTextSetRotation(aText, aAngle):
    return x3d.HUDTextSetRotation(aText, aAngle)

x3d.HUDTextSetFont.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDTextSetFont.restype = ctypes.c_double
def HUDTextSetFont(aText, aFont):
    return x3d.HUDTextSetFont(aText, aFont)

x3d.HUDTextSetColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDTextSetColor.restype = ctypes.c_double
def HUDTextSetColor(aText, aColor, aAlph):
    return x3d.HUDTextSetColor(aText, aColor, aAlph)

x3d.HUDTextSetText.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.HUDTextSetText.restype = ctypes.c_double
def HUDTextSetText(aText, aTxt):
    return x3d.HUDTextSetText(aText, aTxt)

x3d.FlatTextCreate.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double]
x3d.FlatTextCreate.restype = ctypes.c_double
def FlatTextCreate(aFont, aTxt, aParent):
    return x3d.FlatTextCreate(aFont, aTxt, aParent)

x3d.FlatTextSetFont.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FlatTextSetFont.restype = ctypes.c_double
def FlatTextSetFont(aText, aFont):
    return x3d.FlatTextSetFont(aText, aFont)

x3d.FlatTextSetColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FlatTextSetColor.restype = ctypes.c_double
def FlatTextSetColor(aText, aColor, aAlph):
    return x3d.FlatTextSetColor(aText, aColor, aAlph)

x3d.FlatTextSetText.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.FlatTextSetText.restype = ctypes.c_double
def FlatTextSetText(aText, aTxt):
    return x3d.FlatTextSetText(aText, aTxt)

x3d.SpaceTextCreate.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.SpaceTextCreate.restype = ctypes.c_double
def SpaceTextCreate(aFont, aTxt, aExtr, aParent):
    return x3d.SpaceTextCreate(aFont, aTxt, aExtr, aParent)

x3d.SpaceTextSetExtrusion.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SpaceTextSetExtrusion.restype = ctypes.c_double
def SpaceTextSetExtrusion(aText, aExtr):
    return x3d.SpaceTextSetExtrusion(aText, aExtr)

x3d.SpaceTextSetFont.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SpaceTextSetFont.restype = ctypes.c_double
def SpaceTextSetFont(aText, aFont):
    return x3d.SpaceTextSetFont(aText, aFont)

x3d.SpaceTextSetText.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.SpaceTextSetText.restype = ctypes.c_double
def SpaceTextSetText(aText, aTxt):
    return x3d.SpaceTextSetText(aText, aTxt)

x3d.TTFontCreate.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.TTFontCreate.restype = ctypes.c_double
def TTFontCreate(aFilename, aHeight):
    return x3d.TTFontCreate(aFilename, aHeight)

x3d.TTFontSetLineGap.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TTFontSetLineGap.restype = ctypes.c_double
def TTFontSetLineGap(aFont, aGap):
    return x3d.TTFontSetLineGap(aFont, aGap)


# fps.pas
x3d.FpsManagerCreate.argtypes = []
x3d.FpsManagerCreate.restype = ctypes.c_double
def FpsManagerCreate():
    return x3d.FpsManagerCreate()

x3d.FpsManagerSetNavigator.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsManagerSetNavigator.restype = ctypes.c_double
def FpsManagerSetNavigator(aMan, aNav):
    return x3d.FpsManagerSetNavigator(aMan, aNav)

x3d.FpsManagerSetMovementScale.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsManagerSetMovementScale.restype = ctypes.c_double
def FpsManagerSetMovementScale(aMan, aScale):
    return x3d.FpsManagerSetMovementScale(aMan, aScale)

x3d.FpsManagerAddMap.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsManagerAddMap.restype = ctypes.c_double
def FpsManagerAddMap(aMan, aFfm):
    return x3d.FpsManagerAddMap(aMan, aFfm)

x3d.FpsManagerRemoveMap.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsManagerRemoveMap.restype = ctypes.c_double
def FpsManagerRemoveMap(aMan, aFfm):
    return x3d.FpsManagerRemoveMap(aMan, aFfm)

x3d.FpsManagerMapSetCollisionGroup.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FpsManagerMapSetCollisionGroup.restype = ctypes.c_double
def FpsManagerMapSetCollisionGroup(aMan, aFfm, aGroup):
    return x3d.FpsManagerMapSetCollisionGroup(aMan, aFfm, aGroup)

x3d.FpsSetManager.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsSetManager.restype = ctypes.c_double
def FpsSetManager(aObj, aMan):
    return x3d.FpsSetManager(aObj, aMan)

x3d.FpsSetCollisionGroup.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsSetCollisionGroup.restype = ctypes.c_double
def FpsSetCollisionGroup(aObj, aGroup):
    return x3d.FpsSetCollisionGroup(aObj, aGroup)

x3d.FpsSetSphereRadius.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsSetSphereRadius.restype = ctypes.c_double
def FpsSetSphereRadius(aObj, aRadius):
    return x3d.FpsSetSphereRadius(aObj, aRadius)

x3d.FpsSetGravity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsSetGravity.restype = ctypes.c_double
def FpsSetGravity(aObj, aMode):
    return x3d.FpsSetGravity(aObj, aMode)

x3d.FpsMove.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsMove.restype = ctypes.c_double
def FpsMove(aObj, aSpd):
    return x3d.FpsMove(aObj, aSpd)

x3d.FpsStrafe.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsStrafe.restype = ctypes.c_double
def FpsStrafe(aObj, aSpd):
    return x3d.FpsStrafe(aObj, aSpd)

x3d.FpsLift.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsLift.restype = ctypes.c_double
def FpsLift(aObj, aSpd):
    return x3d.FpsLift(aObj, aSpd)

x3d.FpsGetVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FpsGetVelocity.restype = ctypes.c_double
def FpsGetVelocity(aObj, aInd):
    return x3d.FpsGetVelocity(aObj, aInd)


# freeform.pas
x3d.FreeformCreate.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformCreate.restype = ctypes.c_double
def FreeformCreate(aFname, aMatl1, aMatl2, aParent):
    return x3d.FreeformCreate(aFname, aMatl1, aMatl2, aParent)

x3d.FreeformGenTangents.argtypes = [ctypes.c_double]
x3d.FreeformGenTangents.restype = ctypes.c_double
def FreeformGenTangents(aFf):
    return x3d.FreeformGenTangents(aFf)

x3d.FreeformMeshObjectsCount.argtypes = [ctypes.c_double]
x3d.FreeformMeshObjectsCount.restype = ctypes.c_double
def FreeformMeshObjectsCount(aFf):
    return x3d.FreeformMeshObjectsCount(aFf)

x3d.FreeformMeshSetVisible.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetVisible.restype = ctypes.c_double
def FreeformMeshSetVisible(aFf, aMesh, aMode):
    return x3d.FreeformMeshSetVisible(aFf, aMesh, aMode)

x3d.FreeformMeshSetSecondCoords.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetSecondCoords.restype = ctypes.c_double
def FreeformMeshSetSecondCoords(aFf1, aMesh1, aFf2, aMesh2):
    return x3d.FreeformMeshSetSecondCoords(aFf1, aMesh1, aFf2, aMesh2)

x3d.FreeformMeshTriangleCount.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshTriangleCount.restype = ctypes.c_double
def FreeformMeshTriangleCount(aFf, aMesh):
    return x3d.FreeformMeshTriangleCount(aFf, aMesh)

x3d.FreeformMeshObjectGetName.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshObjectGetName.restype = ctypes.c_char_p
def FreeformMeshObjectGetName(aFf, aMesh):
    return x3d.FreeformMeshObjectGetName(aFf, aMesh)

x3d.FreeformMeshObjectSetName.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.FreeformMeshObjectSetName.restype = ctypes.c_double
def FreeformMeshObjectSetName(aFf, aMesh, aName):
    return x3d.FreeformMeshObjectSetName(aFf, aMesh, aName)

x3d.FreeformMeshObjectDestroy.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshObjectDestroy.restype = ctypes.c_double
def FreeformMeshObjectDestroy(aFf, aMesh):
    return x3d.FreeformMeshObjectDestroy(aFf, aMesh)

x3d.FreeformMeshFaceGroupsCount.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupsCount.restype = ctypes.c_double
def FreeformMeshFaceGroupsCount(aFf, aMesh):
    return x3d.FreeformMeshFaceGroupsCount(aFf, aMesh)

x3d.FreeformMeshFaceGroupTriangleCount.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupTriangleCount.restype = ctypes.c_double
def FreeformMeshFaceGroupTriangleCount(aFf, aMesh, aFgr):
    return x3d.FreeformMeshFaceGroupTriangleCount(aFf, aMesh, aFgr)

x3d.FreeformCreateExplosionFX.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformCreateExplosionFX.restype = ctypes.c_double
def FreeformCreateExplosionFX(aFf1, aEnable):
    return x3d.FreeformCreateExplosionFX(aFf1, aEnable)

x3d.FreeformExplosionFXReset.argtypes = [ctypes.c_double]
x3d.FreeformExplosionFXReset.restype = ctypes.c_double
def FreeformExplosionFXReset(aFf1):
    return x3d.FreeformExplosionFXReset(aFf1)

x3d.FreeformExplosionFXEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformExplosionFXEnable.restype = ctypes.c_double
def FreeformExplosionFXEnable(aFf1, aMode):
    return x3d.FreeformExplosionFXEnable(aFf1, aMode)

x3d.FreeformExplosionFXSetSpeed.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformExplosionFXSetSpeed.restype = ctypes.c_double
def FreeformExplosionFXSetSpeed(aFf1, aSpeed):
    return x3d.FreeformExplosionFXSetSpeed(aFf1, aSpeed)

x3d.FreeformSphereSweepIntersect.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformSphereSweepIntersect.restype = ctypes.c_double
def FreeformSphereSweepIntersect(aFreeform, aObj, aRadius, aVel):
    return x3d.FreeformSphereSweepIntersect(aFreeform, aObj, aRadius, aVel)

x3d.FreeformPointInMesh.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformPointInMesh.restype = ctypes.c_double
def FreeformPointInMesh(aFreeform, aX, aY, aZ):
    return x3d.FreeformPointInMesh(aFreeform, aX, aY, aZ)

x3d.FreeformMeshSetMaterial.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.FreeformMeshSetMaterial.restype = ctypes.c_double
def FreeformMeshSetMaterial(aFf, aMesh, aMaterial):
    return x3d.FreeformMeshSetMaterial(aFf, aMesh, aMaterial)

x3d.FreeformUseMeshMaterials.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformUseMeshMaterials.restype = ctypes.c_double
def FreeformUseMeshMaterials(aFf, aMode):
    return x3d.FreeformUseMeshMaterials(aFf, aMode)

x3d.FreeformToFreeforms.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformToFreeforms.restype = ctypes.c_double
def FreeformToFreeforms(aFreeform, aParent):
    return x3d.FreeformToFreeforms(aFreeform, aParent)

x3d.FreeformMeshFaceGroupSetMaterial.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.FreeformMeshFaceGroupSetMaterial.restype = ctypes.c_double
def FreeformMeshFaceGroupSetMaterial(aFf, aMesh, aFg, aMatname):
    return x3d.FreeformMeshFaceGroupSetMaterial(aFf, aMesh, aFg, aMatname)

x3d.FreeformMeshFaceGroupGetMaterial.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupGetMaterial.restype = ctypes.c_char_p
def FreeformMeshFaceGroupGetMaterial(aFf, aMesh, aFgroup):
    return x3d.FreeformMeshFaceGroupGetMaterial(aFf, aMesh, aFgroup)

x3d.FreeformCreateEmpty.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformCreateEmpty.restype = ctypes.c_double
def FreeformCreateEmpty(aMatlib1, aMatlib2, aParent):
    return x3d.FreeformCreateEmpty(aMatlib1, aMatlib2, aParent)

x3d.FreeformAddMesh.argtypes = [ctypes.c_double]
x3d.FreeformAddMesh.restype = ctypes.c_double
def FreeformAddMesh(aFf):
    return x3d.FreeformAddMesh(aFf)

x3d.FreeformMeshAddFaceGroup.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddFaceGroup.restype = ctypes.c_double
def FreeformMeshAddFaceGroup(aFf, aMesh):
    return x3d.FreeformMeshAddFaceGroup(aFf, aMesh)

x3d.FreeformMeshAddVertex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddVertex.restype = ctypes.c_double
def FreeformMeshAddVertex(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshAddVertex(aFf, aMesh, aX, aY, aZ)

x3d.FreeformMeshAddNormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddNormal.restype = ctypes.c_double
def FreeformMeshAddNormal(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshAddNormal(aFf, aMesh, aX, aY, aZ)

x3d.FreeformMeshAddTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddTexCoord.restype = ctypes.c_double
def FreeformMeshAddTexCoord(aFf, aMesh, aU, aV):
    return x3d.FreeformMeshAddTexCoord(aFf, aMesh, aU, aV)

x3d.FreeformMeshAddSecondTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddSecondTexCoord.restype = ctypes.c_double
def FreeformMeshAddSecondTexCoord(aFf, aMesh, aU, aV):
    return x3d.FreeformMeshAddSecondTexCoord(aFf, aMesh, aU, aV)

x3d.FreeformMeshAddTangent.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddTangent.restype = ctypes.c_double
def FreeformMeshAddTangent(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshAddTangent(aFf, aMesh, aX, aY, aZ)

x3d.FreeformMeshAddBinormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshAddBinormal.restype = ctypes.c_double
def FreeformMeshAddBinormal(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshAddBinormal(aFf, aMesh, aX, aY, aZ)

x3d.FreeformMeshFaceGroupAddTriangle.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupAddTriangle.restype = ctypes.c_double
def FreeformMeshFaceGroupAddTriangle(aFf, aMesh, aFg, aI1, aI2, aI3):
    return x3d.FreeformMeshFaceGroupAddTriangle(aFf, aMesh, aFg, aI1, aI2, aI3)

x3d.FreeformMeshGenNormals.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGenNormals.restype = ctypes.c_double
def FreeformMeshGenNormals(aFf, aMesh):
    return x3d.FreeformMeshGenNormals(aFf, aMesh)

x3d.FreeformMeshGenTangents.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGenTangents.restype = ctypes.c_double
def FreeformMeshGenTangents(aFf, aMesh):
    return x3d.FreeformMeshGenTangents(aFf, aMesh)

x3d.FreeformMeshVerticesCount.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshVerticesCount.restype = ctypes.c_double
def FreeformMeshVerticesCount(aFf, aMesh):
    return x3d.FreeformMeshVerticesCount(aFf, aMesh)

x3d.FreeformMeshTranslate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshTranslate.restype = ctypes.c_double
def FreeformMeshTranslate(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshTranslate(aFf, aMesh, aX, aY, aZ)

x3d.FreeformMeshRotate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshRotate.restype = ctypes.c_double
def FreeformMeshRotate(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshRotate(aFf, aMesh, aX, aY, aZ)

x3d.FreeformMeshScale.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshScale.restype = ctypes.c_double
def FreeformMeshScale(aFf, aMesh, aX, aY, aZ):
    return x3d.FreeformMeshScale(aFf, aMesh, aX, aY, aZ)

x3d.FreeformSave.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.FreeformSave.restype = ctypes.c_double
def FreeformSave(aFf, aFilename):
    return x3d.FreeformSave(aFf, aFilename)

x3d.FreeformMeshGetVertex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGetVertex.restype = ctypes.c_double
def FreeformMeshGetVertex(aFf, aMesh, aV, aIndex):
    return x3d.FreeformMeshGetVertex(aFf, aMesh, aV, aIndex)

x3d.FreeformMeshGetNormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGetNormal.restype = ctypes.c_double
def FreeformMeshGetNormal(aFf, aMesh, aN, aIndex):
    return x3d.FreeformMeshGetNormal(aFf, aMesh, aN, aIndex)

x3d.FreeformMeshGetTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGetTexCoord.restype = ctypes.c_double
def FreeformMeshGetTexCoord(aFf, aMesh, aT, aIndex):
    return x3d.FreeformMeshGetTexCoord(aFf, aMesh, aT, aIndex)

x3d.FreeformMeshGetSecondTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGetSecondTexCoord.restype = ctypes.c_double
def FreeformMeshGetSecondTexCoord(aFf, aMesh, aT, aIndex):
    return x3d.FreeformMeshGetSecondTexCoord(aFf, aMesh, aT, aIndex)

x3d.FreeformMeshGetTangent.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGetTangent.restype = ctypes.c_double
def FreeformMeshGetTangent(aFf, aMesh, aT, aIndex):
    return x3d.FreeformMeshGetTangent(aFf, aMesh, aT, aIndex)

x3d.FreeformMeshGetBinormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshGetBinormal.restype = ctypes.c_double
def FreeformMeshGetBinormal(aFf, aMesh, aB, aIndex):
    return x3d.FreeformMeshGetBinormal(aFf, aMesh, aB, aIndex)

x3d.FreeformMeshFaceGroupGetIndex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupGetIndex.restype = ctypes.c_double
def FreeformMeshFaceGroupGetIndex(aFf, aMesh, aFg, aIndex):
    return x3d.FreeformMeshFaceGroupGetIndex(aFf, aMesh, aFg, aIndex)

x3d.FreeformMeshSetVertex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetVertex.restype = ctypes.c_double
def FreeformMeshSetVertex(aFf, aMesh, aV, aX, aY, aZ):
    return x3d.FreeformMeshSetVertex(aFf, aMesh, aV, aX, aY, aZ)

x3d.FreeformMeshSetNormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetNormal.restype = ctypes.c_double
def FreeformMeshSetNormal(aFf, aMesh, aN, aX, aY, aZ):
    return x3d.FreeformMeshSetNormal(aFf, aMesh, aN, aX, aY, aZ)

x3d.FreeformMeshSetTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetTexCoord.restype = ctypes.c_double
def FreeformMeshSetTexCoord(aFf, aMesh, aT, aU, aV):
    return x3d.FreeformMeshSetTexCoord(aFf, aMesh, aT, aU, aV)

x3d.FreeformMeshSetSecondTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetSecondTexCoord.restype = ctypes.c_double
def FreeformMeshSetSecondTexCoord(aFf, aMesh, aT, aU, aV):
    return x3d.FreeformMeshSetSecondTexCoord(aFf, aMesh, aT, aU, aV)

x3d.FreeformMeshSetTangent.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetTangent.restype = ctypes.c_double
def FreeformMeshSetTangent(aFf, aMesh, aT, aX, aY, aZ):
    return x3d.FreeformMeshSetTangent(aFf, aMesh, aT, aX, aY, aZ)

x3d.FreeformMeshSetBinormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshSetBinormal.restype = ctypes.c_double
def FreeformMeshSetBinormal(aFf, aMesh, aB, aX, aY, aZ):
    return x3d.FreeformMeshSetBinormal(aFf, aMesh, aB, aX, aY, aZ)

x3d.FreeformMeshFaceGroupSetIndex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupSetIndex.restype = ctypes.c_double
def FreeformMeshFaceGroupSetIndex(aFf, aMesh, aFg, aIndex, aI):
    return x3d.FreeformMeshFaceGroupSetIndex(aFf, aMesh, aFg, aIndex, aI)

x3d.FreeformBuildOctree.argtypes = [ctypes.c_double]
x3d.FreeformBuildOctree.restype = ctypes.c_double
def FreeformBuildOctree(aFf):
    return x3d.FreeformBuildOctree(aFf)

x3d.FreeformMeshFaceGroupGetLightmapIndex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupGetLightmapIndex.restype = ctypes.c_double
def FreeformMeshFaceGroupGetLightmapIndex(aFf, aMesh, aFg):
    return x3d.FreeformMeshFaceGroupGetLightmapIndex(aFf, aMesh, aFg)

x3d.FreeformMeshFaceGroupSetLightmapIndex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformMeshFaceGroupSetLightmapIndex.restype = ctypes.c_double
def FreeformMeshFaceGroupSetLightmapIndex(aFf, aMesh, aFg, aIndex):
    return x3d.FreeformMeshFaceGroupSetLightmapIndex(aFf, aMesh, aFg, aIndex)

x3d.FreeformSetMaterialLibraries.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FreeformSetMaterialLibraries.restype = ctypes.c_double
def FreeformSetMaterialLibraries(aFf, aMatlib, aLmmatlib):
    return x3d.FreeformSetMaterialLibraries(aFf, aMatlib, aLmmatlib)

x3d.BaseMeshBuildSilhouetteConnectivityData.argtypes = [ctypes.c_double]
x3d.BaseMeshBuildSilhouetteConnectivityData.restype = ctypes.c_double
def BaseMeshBuildSilhouetteConnectivityData(aObj):
    return x3d.BaseMeshBuildSilhouetteConnectivityData(aObj)


# grid.pas
x3d.GridCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GridCreate.restype = ctypes.c_double
def GridCreate(aX, aY, aZ, aStep, aParent):
    return x3d.GridCreate(aX, aY, aZ, aStep, aParent)

x3d.GridSetLineStyle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GridSetLineStyle.restype = ctypes.c_double
def GridSetLineStyle(aGrid, aMode):
    return x3d.GridSetLineStyle(aGrid, aMode)

x3d.GridSetLineSmoothing.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GridSetLineSmoothing.restype = ctypes.c_double
def GridSetLineSmoothing(aGrid, aMode):
    return x3d.GridSetLineSmoothing(aGrid, aMode)

x3d.GridSetParts.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GridSetParts.restype = ctypes.c_double
def GridSetParts(aGrid, aMode):
    return x3d.GridSetParts(aGrid, aMode)

x3d.GridSetColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GridSetColor.restype = ctypes.c_double
def GridSetColor(aGrid, aColor, aAlpha):
    return x3d.GridSetColor(aGrid, aColor, aAlpha)

x3d.GridSetSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GridSetSize.restype = ctypes.c_double
def GridSetSize(aGrid, aSize):
    return x3d.GridSetSize(aGrid, aSize)

x3d.GridSetPattern.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GridSetPattern.restype = ctypes.c_double
def GridSetPattern(aGrid, aPattern):
    return x3d.GridSetPattern(aGrid, aPattern)

x3d.GridSetTile.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GridSetTile.restype = ctypes.c_double
def GridSetTile(aGrid, aX, aY, aZ):
    return x3d.GridSetTile(aGrid, aX, aY, aZ)

x3d.GridSetStep.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GridSetStep.restype = ctypes.c_double
def GridSetStep(aGrid, aStep):
    return x3d.GridSetStep(aGrid, aStep)


# hudshapes.pas
x3d.HUDShapeRectangleCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeRectangleCreate.restype = ctypes.c_double
def HUDShapeRectangleCreate(aW, aH, aParent):
    return x3d.HUDShapeRectangleCreate(aW, aH, aParent)

x3d.HUDShapeCircleCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeCircleCreate.restype = ctypes.c_double
def HUDShapeCircleCreate(aRadius, aSlices, aStartang, aEndang, aParent):
    return x3d.HUDShapeCircleCreate(aRadius, aSlices, aStartang, aEndang, aParent)

x3d.HUDShapeLineCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeLineCreate.restype = ctypes.c_double
def HUDShapeLineCreate(aX1, aY1, aX2, aY2, aParent):
    return x3d.HUDShapeLineCreate(aX1, aY1, aX2, aY2, aParent)

x3d.HUDShapeMeshCreate.argtypes = [ctypes.c_double]
x3d.HUDShapeMeshCreate.restype = ctypes.c_double
def HUDShapeMeshCreate(aParent):
    return x3d.HUDShapeMeshCreate(aParent)

x3d.HUDShapeSetRotation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDShapeSetRotation.restype = ctypes.c_double
def HUDShapeSetRotation(aShape, aAngle):
    return x3d.HUDShapeSetRotation(aShape, aAngle)

x3d.HUDShapeRotate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDShapeRotate.restype = ctypes.c_double
def HUDShapeRotate(aShape, aAngle):
    return x3d.HUDShapeRotate(aShape, aAngle)

x3d.HUDShapeSetColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeSetColor.restype = ctypes.c_double
def HUDShapeSetColor(aShape, aCol, aAlpha):
    return x3d.HUDShapeSetColor(aShape, aCol, aAlpha)

x3d.HUDShapeSetOrigin.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeSetOrigin.restype = ctypes.c_double
def HUDShapeSetOrigin(aShape, aX, aY):
    return x3d.HUDShapeSetOrigin(aShape, aX, aY)

x3d.HUDShapeSetSize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeSetSize.restype = ctypes.c_double
def HUDShapeSetSize(aShape, aW, aH):
    return x3d.HUDShapeSetSize(aShape, aW, aH)

x3d.HUDShapeScale.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeScale.restype = ctypes.c_double
def HUDShapeScale(aShape, aU, aV):
    return x3d.HUDShapeScale(aShape, aU, aV)

x3d.HUDShapeCircleSetRadius.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDShapeCircleSetRadius.restype = ctypes.c_double
def HUDShapeCircleSetRadius(aShape, aRadius):
    return x3d.HUDShapeCircleSetRadius(aShape, aRadius)

x3d.HUDShapeCircleSetSlices.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDShapeCircleSetSlices.restype = ctypes.c_double
def HUDShapeCircleSetSlices(aShape, aSlices):
    return x3d.HUDShapeCircleSetSlices(aShape, aSlices)

x3d.HUDShapeCircleSetAngles.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeCircleSetAngles.restype = ctypes.c_double
def HUDShapeCircleSetAngles(aShape, aStartang, aEndang):
    return x3d.HUDShapeCircleSetAngles(aShape, aStartang, aEndang)

x3d.HUDShapeLineSetPoints.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeLineSetPoints.restype = ctypes.c_double
def HUDShapeLineSetPoints(aShape, aX1, aY1, aX2, aY2):
    return x3d.HUDShapeLineSetPoints(aShape, aX1, aY1, aX2, aY2)

x3d.HUDShapeLineSetWidth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDShapeLineSetWidth.restype = ctypes.c_double
def HUDShapeLineSetWidth(aShape, aW):
    return x3d.HUDShapeLineSetWidth(aShape, aW)

x3d.HUDShapeMeshAddVertex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeMeshAddVertex.restype = ctypes.c_double
def HUDShapeMeshAddVertex(aShape, aX, aY, aU, aV):
    return x3d.HUDShapeMeshAddVertex(aShape, aX, aY, aU, aV)

x3d.HUDShapeMeshAddTriangle.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeMeshAddTriangle.restype = ctypes.c_double
def HUDShapeMeshAddTriangle(aShape, aV1, aV2, aV3):
    return x3d.HUDShapeMeshAddTriangle(aShape, aV1, aV2, aV3)

x3d.HUDShapeMeshSetVertex.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeMeshSetVertex.restype = ctypes.c_double
def HUDShapeMeshSetVertex(aShape, aIndex, aX, aY):
    return x3d.HUDShapeMeshSetVertex(aShape, aIndex, aX, aY)

x3d.HUDShapeMeshSetTexCoord.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDShapeMeshSetTexCoord.restype = ctypes.c_double
def HUDShapeMeshSetTexCoord(aShape, aIndex, aU, aV):
    return x3d.HUDShapeMeshSetTexCoord(aShape, aIndex, aU, aV)


# input.pas
x3d.MouseGetPositionX.argtypes = []
x3d.MouseGetPositionX.restype = ctypes.c_double
def MouseGetPositionX():
    return x3d.MouseGetPositionX()

x3d.MouseGetPositionY.argtypes = []
x3d.MouseGetPositionY.restype = ctypes.c_double
def MouseGetPositionY():
    return x3d.MouseGetPositionY()

x3d.MouseSetPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MouseSetPosition.restype = ctypes.c_double
def MouseSetPosition(aMx, aMy):
    return x3d.MouseSetPosition(aMx, aMy)

x3d.MouseShowCursor.argtypes = [ctypes.c_double]
x3d.MouseShowCursor.restype = ctypes.c_double
def MouseShowCursor(aMode):
    return x3d.MouseShowCursor(aMode)

x3d.KeyIsPressed.argtypes = [ctypes.c_double]
x3d.KeyIsPressed.restype = ctypes.c_double
def KeyIsPressed(aKey):
    return x3d.KeyIsPressed(aKey)

x3d.MouseIsPressed.argtypes = [ctypes.c_double]
x3d.MouseIsPressed.restype = ctypes.c_double
def MouseIsPressed(aBtn):
    return x3d.MouseIsPressed(aBtn)


# kraft.pas
x3d.KraftCreate.argtypes = []
x3d.KraftCreate.restype = ctypes.c_double
def KraftCreate():
    return x3d.KraftCreate()

x3d.KraftStep.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftStep.restype = ctypes.c_double
def KraftStep(aKr, aDt):
    return x3d.KraftStep(aKr, aDt)

x3d.KraftGetRayHitPosition.argtypes = [ctypes.c_double]
x3d.KraftGetRayHitPosition.restype = ctypes.c_double
def KraftGetRayHitPosition(aIndex):
    return x3d.KraftGetRayHitPosition(aIndex)

x3d.KraftGetRayHitNormal.argtypes = [ctypes.c_double]
x3d.KraftGetRayHitNormal.restype = ctypes.c_double
def KraftGetRayHitNormal(aIndex):
    return x3d.KraftGetRayHitNormal(aIndex)

x3d.KraftCreateRigidBody.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateRigidBody.restype = ctypes.c_double
def KraftCreateRigidBody(aKr, aTyp):
    return x3d.KraftCreateRigidBody(aKr, aTyp)

x3d.KraftRigidBodyFinish.argtypes = [ctypes.c_double]
x3d.KraftRigidBodyFinish.restype = ctypes.c_double
def KraftRigidBodyFinish(aKrb):
    return x3d.KraftRigidBodyFinish(aKrb)

x3d.KraftRigidBodySetGravity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodySetGravity.restype = ctypes.c_double
def KraftRigidBodySetGravity(aKrb, aX, aY, aZ, aScale):
    return x3d.KraftRigidBodySetGravity(aKrb, aX, aY, aZ, aScale)

x3d.KraftRigidBodySetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodySetPosition.restype = ctypes.c_double
def KraftRigidBodySetPosition(aKrb, aX, aY, aZ):
    return x3d.KraftRigidBodySetPosition(aKrb, aX, aY, aZ)

x3d.KraftRigidBodyGetPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyGetPosition.restype = ctypes.c_double
def KraftRigidBodyGetPosition(aKrb, aIndex):
    return x3d.KraftRigidBodyGetPosition(aKrb, aIndex)

x3d.KraftRigidBodySetLinearVelocity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodySetLinearVelocity.restype = ctypes.c_double
def KraftRigidBodySetLinearVelocity(aKrb, aX, aY, aZ):
    return x3d.KraftRigidBodySetLinearVelocity(aKrb, aX, aY, aZ)

x3d.KraftRigidBodyGetLinearVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyGetLinearVelocity.restype = ctypes.c_double
def KraftRigidBodyGetLinearVelocity(aKrb, aIndex):
    return x3d.KraftRigidBodyGetLinearVelocity(aKrb, aIndex)

x3d.KraftRigidBodySetRotation.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodySetRotation.restype = ctypes.c_double
def KraftRigidBodySetRotation(aKrb, aX, aY, aZ):
    return x3d.KraftRigidBodySetRotation(aKrb, aX, aY, aZ)

x3d.KraftRigidBodyGetDirection.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyGetDirection.restype = ctypes.c_double
def KraftRigidBodyGetDirection(aKrb, aIndex):
    return x3d.KraftRigidBodyGetDirection(aKrb, aIndex)

x3d.KraftRigidBodyGetUp.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyGetUp.restype = ctypes.c_double
def KraftRigidBodyGetUp(aKrb, aIndex):
    return x3d.KraftRigidBodyGetUp(aKrb, aIndex)

x3d.KraftRigidBodyGetRight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyGetRight.restype = ctypes.c_double
def KraftRigidBodyGetRight(aKrb, aIndex):
    return x3d.KraftRigidBodyGetRight(aKrb, aIndex)

x3d.KraftRigidBodySetAngularVelocity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodySetAngularVelocity.restype = ctypes.c_double
def KraftRigidBodySetAngularVelocity(aKrb, aX, aY, aZ):
    return x3d.KraftRigidBodySetAngularVelocity(aKrb, aX, aY, aZ)

x3d.KraftRigidBodyGetAngularVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyGetAngularVelocity.restype = ctypes.c_double
def KraftRigidBodyGetAngularVelocity(aKrb, aIndex):
    return x3d.KraftRigidBodyGetAngularVelocity(aKrb, aIndex)

x3d.KraftRigidBodyAddForce.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyAddForce.restype = ctypes.c_double
def KraftRigidBodyAddForce(aKrb, aX, aY, aZ):
    return x3d.KraftRigidBodyAddForce(aKrb, aX, aY, aZ)

x3d.KraftRigidBodyAddForceAtPos.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyAddForceAtPos.restype = ctypes.c_double
def KraftRigidBodyAddForceAtPos(aKrb, aX, aY, aZ, aPx, aPy, aPz):
    return x3d.KraftRigidBodyAddForceAtPos(aKrb, aX, aY, aZ, aPx, aPy, aPz)

x3d.KraftRigidBodyAddRelForce.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRigidBodyAddRelForce.restype = ctypes.c_double
def KraftRigidBodyAddRelForce(aKrb, aX, aY, aZ):
    return x3d.KraftRigidBodyAddRelForce(aKrb, aX, aY, aZ)

x3d.KraftRayCast.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftRayCast.restype = ctypes.c_double
def KraftRayCast(aKr, aX, aY, aZ, aDx, aDy, aDz, aMaxtime):
    return x3d.KraftRayCast(aKr, aX, aY, aZ, aDx, aDy, aDz, aMaxtime)

x3d.KraftObjectSetRigidBody.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftObjectSetRigidBody.restype = ctypes.c_double
def KraftObjectSetRigidBody(aObj, aKrb):
    return x3d.KraftObjectSetRigidBody(aObj, aKrb)

x3d.KraftCreateShapeSphere.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateShapeSphere.restype = ctypes.c_double
def KraftCreateShapeSphere(aRbody, aRadius):
    return x3d.KraftCreateShapeSphere(aRbody, aRadius)

x3d.KraftCreateShapeBox.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftCreateShapeBox.restype = ctypes.c_double
def KraftCreateShapeBox(aRbody, aX, aY, aZ):
    return x3d.KraftCreateShapeBox(aRbody, aX, aY, aZ)

x3d.KraftCreateShapePlane.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftCreateShapePlane.restype = ctypes.c_double
def KraftCreateShapePlane(aRbody, aX, aY, aZ, aD):
    return x3d.KraftCreateShapePlane(aRbody, aX, aY, aZ, aD)

x3d.KraftCreateShapeCapsule.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftCreateShapeCapsule.restype = ctypes.c_double
def KraftCreateShapeCapsule(aRbody, aRadius, aHeight):
    return x3d.KraftCreateShapeCapsule(aRbody, aRadius, aHeight)

x3d.KraftCreateShapeMesh.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateShapeMesh.restype = ctypes.c_double
def KraftCreateShapeMesh(aRbody, aFf):
    return x3d.KraftCreateShapeMesh(aRbody, aFf)

x3d.KraftShapeSetDensity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftShapeSetDensity.restype = ctypes.c_double
def KraftShapeSetDensity(aShape, aDensity):
    return x3d.KraftShapeSetDensity(aShape, aDensity)

x3d.KraftShapeSetFriction.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftShapeSetFriction.restype = ctypes.c_double
def KraftShapeSetFriction(aShape, aFriction):
    return x3d.KraftShapeSetFriction(aShape, aFriction)

x3d.KraftShapeSetRestitution.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftShapeSetRestitution.restype = ctypes.c_double
def KraftShapeSetRestitution(aShape, aRest):
    return x3d.KraftShapeSetRestitution(aShape, aRest)

x3d.KraftShapeSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftShapeSetPosition.restype = ctypes.c_double
def KraftShapeSetPosition(aShape, aX, aY, aZ):
    return x3d.KraftShapeSetPosition(aShape, aX, aY, aZ)

x3d.KraftShapeGetPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftShapeGetPosition.restype = ctypes.c_double
def KraftShapeGetPosition(aShape, aIndex):
    return x3d.KraftShapeGetPosition(aShape, aIndex)

x3d.KraftShapeSetRayCastable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftShapeSetRayCastable.restype = ctypes.c_double
def KraftShapeSetRayCastable(aShape, aMode):
    return x3d.KraftShapeSetRayCastable(aShape, aMode)

x3d.KraftCreateJointDistance.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateJointDistance.restype = ctypes.c_double
def KraftCreateJointDistance(aRbody1, aRbody2):
    return x3d.KraftCreateJointDistance(aRbody1, aRbody2)

x3d.KraftCreateJointRope.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftCreateJointRope.restype = ctypes.c_double
def KraftCreateJointRope(aRbody1, aRbody2, aMaxlength):
    return x3d.KraftCreateJointRope(aRbody1, aRbody2, aMaxlength)

x3d.KraftCreateJointBallSocket.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateJointBallSocket.restype = ctypes.c_double
def KraftCreateJointBallSocket(aRbody1, aRbody2):
    return x3d.KraftCreateJointBallSocket(aRbody1, aRbody2)

x3d.KraftCreateJointFixed.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateJointFixed.restype = ctypes.c_double
def KraftCreateJointFixed(aRbody1, aRbody2):
    return x3d.KraftCreateJointFixed(aRbody1, aRbody2)

x3d.KraftCreateJointHinge.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.KraftCreateJointHinge.restype = ctypes.c_double
def KraftCreateJointHinge(aRbody1, aRbody2):
    return x3d.KraftCreateJointHinge(aRbody1, aRbody2)

x3d.KraftJointSetAnchor1.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftJointSetAnchor1.restype = ctypes.c_double
def KraftJointSetAnchor1(aJoint, aX, aY, aZ):
    return x3d.KraftJointSetAnchor1(aJoint, aX, aY, aZ)

x3d.KraftJointSetAnchor2.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftJointSetAnchor2.restype = ctypes.c_double
def KraftJointSetAnchor2(aJoint, aX, aY, aZ):
    return x3d.KraftJointSetAnchor2(aJoint, aX, aY, aZ)

x3d.KraftJointSetHingeAxis1.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftJointSetHingeAxis1.restype = ctypes.c_double
def KraftJointSetHingeAxis1(aJoint, aX, aY, aZ):
    return x3d.KraftJointSetHingeAxis1(aJoint, aX, aY, aZ)

x3d.KraftJointSetHingeAxis2.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.KraftJointSetHingeAxis2.restype = ctypes.c_double
def KraftJointSetHingeAxis2(aJoint, aX, aY, aZ):
    return x3d.KraftJointSetHingeAxis2(aJoint, aX, aY, aZ)


# lensflare.pas
x3d.LensflareCreate.argtypes = [ctypes.c_double]
x3d.LensflareCreate.restype = ctypes.c_double
def LensflareCreate(aParent):
    return x3d.LensflareCreate(aParent)

x3d.LensflareSetSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetSize.restype = ctypes.c_double
def LensflareSetSize(aLensflare, aSize):
    return x3d.LensflareSetSize(aLensflare, aSize)

x3d.LensflareSetSeed.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetSeed.restype = ctypes.c_double
def LensflareSetSeed(aLensflare, aSeed):
    return x3d.LensflareSetSeed(aLensflare, aSeed)

x3d.LensflareSetSqueeze.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetSqueeze.restype = ctypes.c_double
def LensflareSetSqueeze(aLensflare, aSqueeze):
    return x3d.LensflareSetSqueeze(aLensflare, aSqueeze)

x3d.LensflareSetStreaks.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetStreaks.restype = ctypes.c_double
def LensflareSetStreaks(aLensflare, aStreaks):
    return x3d.LensflareSetStreaks(aLensflare, aStreaks)

x3d.LensflareSetStreakWidth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetStreakWidth.restype = ctypes.c_double
def LensflareSetStreakWidth(aLensflare, aWidth):
    return x3d.LensflareSetStreakWidth(aLensflare, aWidth)

x3d.LensflareSetSecs.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetSecs.restype = ctypes.c_double
def LensflareSetSecs(aLensflare, aSecs):
    return x3d.LensflareSetSecs(aLensflare, aSecs)

x3d.LensflareSetResolution.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LensflareSetResolution.restype = ctypes.c_double
def LensflareSetResolution(aLensflare, aRes):
    return x3d.LensflareSetResolution(aLensflare, aRes)

x3d.LensflareSetElements.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LensflareSetElements.restype = ctypes.c_double
def LensflareSetElements(aLensflare, aGlow, aRing, aStreaks, aRays, aSecs):
    return x3d.LensflareSetElements(aLensflare, aGlow, aRing, aStreaks, aRays, aSecs)

x3d.LensflareSetGradients.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LensflareSetGradients.restype = ctypes.c_double
def LensflareSetGradients(aLensflare, aInd, aColor1, aAlpha1, aColor2, aAlpha2):
    return x3d.LensflareSetGradients(aLensflare, aInd, aColor1, aAlpha1, aColor2, aAlpha2)


# light.pas
x3d.LightCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightCreate.restype = ctypes.c_double
def LightCreate(aLs, aParent):
    return x3d.LightCreate(aLs, aParent)

x3d.LightSetAmbientColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetAmbientColor.restype = ctypes.c_double
def LightSetAmbientColor(aLight, aColor):
    return x3d.LightSetAmbientColor(aLight, aColor)

x3d.LightSetDiffuseColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetDiffuseColor.restype = ctypes.c_double
def LightSetDiffuseColor(aLight, aColor):
    return x3d.LightSetDiffuseColor(aLight, aColor)

x3d.LightSetSpecularColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetSpecularColor.restype = ctypes.c_double
def LightSetSpecularColor(aLight, aColor):
    return x3d.LightSetSpecularColor(aLight, aColor)

x3d.LightSetAttenuation.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LightSetAttenuation.restype = ctypes.c_double
def LightSetAttenuation(aLight, aAconst, aAlinear, aAquadratic):
    return x3d.LightSetAttenuation(aLight, aAconst, aAlinear, aAquadratic)

x3d.LightSetShining.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetShining.restype = ctypes.c_double
def LightSetShining(aLight, aMode):
    return x3d.LightSetShining(aLight, aMode)

x3d.LightSetSpotCutoff.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetSpotCutoff.restype = ctypes.c_double
def LightSetSpotCutoff(aLight, aCutoff):
    return x3d.LightSetSpotCutoff(aLight, aCutoff)

x3d.LightSetSpotExponent.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetSpotExponent.restype = ctypes.c_double
def LightSetSpotExponent(aLight, aExp):
    return x3d.LightSetSpotExponent(aLight, aExp)

x3d.LightSetSpotDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LightSetSpotDirection.restype = ctypes.c_double
def LightSetSpotDirection(aLight, aX, aY, aZ):
    return x3d.LightSetSpotDirection(aLight, aX, aY, aZ)

x3d.LightSetStyle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightSetStyle.restype = ctypes.c_double
def LightSetStyle(aLight, aLs):
    return x3d.LightSetStyle(aLight, aLs)

x3d.LightGetColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightGetColor.restype = ctypes.c_double
def LightGetColor(aLight, aIndex):
    return x3d.LightGetColor(aLight, aIndex)

x3d.LightGetAttenuation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LightGetAttenuation.restype = ctypes.c_double
def LightGetAttenuation(aLight, aIndex):
    return x3d.LightGetAttenuation(aLight, aIndex)

x3d.LightGetShining.argtypes = [ctypes.c_double]
x3d.LightGetShining.restype = ctypes.c_double
def LightGetShining(aLight):
    return x3d.LightGetShining(aLight)


# lightfx.pas
x3d.LightFXCreate.argtypes = [ctypes.c_double]
x3d.LightFXCreate.restype = ctypes.c_double
def LightFXCreate(aObj):
    return x3d.LightFXCreate(aObj)


# lines.pas
x3d.LinesCreate.argtypes = [ctypes.c_double]
x3d.LinesCreate.restype = ctypes.c_double
def LinesCreate(aParent):
    return x3d.LinesCreate(aParent)

x3d.LinesAddNode.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LinesAddNode.restype = ctypes.c_double
def LinesAddNode(aLines, aX, aY, aZ):
    return x3d.LinesAddNode(aLines, aX, aY, aZ)

x3d.LinesDeleteNode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LinesDeleteNode.restype = ctypes.c_double
def LinesDeleteNode(aLines, aInd):
    return x3d.LinesDeleteNode(aLines, aInd)

x3d.LinesSetNode.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LinesSetNode.restype = ctypes.c_double
def LinesSetNode(aLines, aInd, aX, aY, aZ):
    return x3d.LinesSetNode(aLines, aInd, aX, aY, aZ)

x3d.LinesSetSize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.LinesSetSize.restype = ctypes.c_double
def LinesSetSize(aLines, aLinewidth, aNodesize):
    return x3d.LinesSetSize(aLines, aLinewidth, aNodesize)

x3d.LinesSetSplineMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LinesSetSplineMode.restype = ctypes.c_double
def LinesSetSplineMode(aLines, aLsm):
    return x3d.LinesSetSplineMode(aLines, aLsm)

x3d.LinesSetNodesAspect.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LinesSetNodesAspect.restype = ctypes.c_double
def LinesSetNodesAspect(aLines, aLna):
    return x3d.LinesSetNodesAspect(aLines, aLna)

x3d.LinesSetDivision.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LinesSetDivision.restype = ctypes.c_double
def LinesSetDivision(aLines, aDivision):
    return x3d.LinesSetDivision(aLines, aDivision)


# logger.pas
x3d.LoggerCreate.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.LoggerCreate.restype = ctypes.c_double
def LoggerCreate(aFname, aLoglevel):
    return x3d.LoggerCreate(aFname, aLoglevel)

x3d.LoggerEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.LoggerEnable.restype = ctypes.c_double
def LoggerEnable(aLogger, aMode):
    return x3d.LoggerEnable(aLogger, aMode)

x3d.LoggerLog.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.LoggerLog.restype = ctypes.c_double
def LoggerLog(aLogger, aLoglevel, aMsg):
    return x3d.LoggerLog(aLogger, aLoglevel, aMsg)


# material.pas
x3d.MaterialLibraryCreate.argtypes = []
x3d.MaterialLibraryCreate.restype = ctypes.c_double
def MaterialLibraryCreate():
    return x3d.MaterialLibraryCreate()

x3d.MaterialLibraryActivate.argtypes = [ctypes.c_double]
x3d.MaterialLibraryActivate.restype = ctypes.c_double
def MaterialLibraryActivate(aMlib):
    return x3d.MaterialLibraryActivate(aMlib)

x3d.MaterialLibrarySetTexturePaths.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.MaterialLibrarySetTexturePaths.restype = ctypes.c_double
def MaterialLibrarySetTexturePaths(aMlb, aPath):
    return x3d.MaterialLibrarySetTexturePaths(aMlb, aPath)

x3d.MaterialLibraryClear.argtypes = [ctypes.c_double]
x3d.MaterialLibraryClear.restype = ctypes.c_double
def MaterialLibraryClear(aMlb):
    return x3d.MaterialLibraryClear(aMlb)

x3d.MaterialLibraryDeleteUnused.argtypes = [ctypes.c_double]
x3d.MaterialLibraryDeleteUnused.restype = ctypes.c_double
def MaterialLibraryDeleteUnused(aMlb):
    return x3d.MaterialLibraryDeleteUnused(aMlb)

x3d.MaterialLibraryHasMaterial.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.MaterialLibraryHasMaterial.restype = ctypes.c_double
def MaterialLibraryHasMaterial(aMatlib, aName):
    return x3d.MaterialLibraryHasMaterial(aMatlib, aName)

x3d.MaterialLibraryLoadScript.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.MaterialLibraryLoadScript.restype = ctypes.c_double
def MaterialLibraryLoadScript(aMatlib, aFilename):
    return x3d.MaterialLibraryLoadScript(aMatlib, aFilename)

x3d.MaterialLibraryGetTextureByName.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.MaterialLibraryGetTextureByName.restype = ctypes.c_double
def MaterialLibraryGetTextureByName(aMatlib, aName):
    return x3d.MaterialLibraryGetTextureByName(aMatlib, aName)

x3d.MaterialCreate.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialCreate.restype = ctypes.c_double
def MaterialCreate(aMtrl, aFname):
    return x3d.MaterialCreate(aMtrl, aFname)

x3d.MaterialDestroy.argtypes = [ctypes.c_char_p]
x3d.MaterialDestroy.restype = ctypes.c_double
def MaterialDestroy(aMtrl):
    return x3d.MaterialDestroy(aMtrl)

x3d.MaterialAddCubeMap.argtypes = [ctypes.c_char_p]
x3d.MaterialAddCubeMap.restype = ctypes.c_double
def MaterialAddCubeMap(aMtrl):
    return x3d.MaterialAddCubeMap(aMtrl)

x3d.MaterialCubeMapLoadImage.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_double]
x3d.MaterialCubeMapLoadImage.restype = ctypes.c_double
def MaterialCubeMapLoadImage(aMtrl, aFilename, aInd):
    return x3d.MaterialCubeMapLoadImage(aMtrl, aFilename, aInd)

x3d.MaterialCubeMapGenerate.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialCubeMapGenerate.restype = ctypes.c_double
def MaterialCubeMapGenerate(aMtrl, aRes):
    return x3d.MaterialCubeMapGenerate(aMtrl, aRes)

x3d.MaterialCubeMapFromScene.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MaterialCubeMapFromScene.restype = ctypes.c_double
def MaterialCubeMapFromScene(aMtrl, aViewer, aCamera, aRes):
    return x3d.MaterialCubeMapFromScene(aMtrl, aViewer, aCamera, aRes)

x3d.MaterialSetName.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialSetName.restype = ctypes.c_double
def MaterialSetName(aMtrl, aName):
    return x3d.MaterialSetName(aMtrl, aName)

x3d.MaterialSetShininess.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetShininess.restype = ctypes.c_double
def MaterialSetShininess(aMtrl, aShin):
    return x3d.MaterialSetShininess(aMtrl, aShin)

x3d.MaterialSetAmbientColor.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetAmbientColor.restype = ctypes.c_double
def MaterialSetAmbientColor(aMtrl, aCol, aAlpha):
    return x3d.MaterialSetAmbientColor(aMtrl, aCol, aAlpha)

x3d.MaterialSetDiffuseColor.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetDiffuseColor.restype = ctypes.c_double
def MaterialSetDiffuseColor(aMtrl, aCol, aAlpha):
    return x3d.MaterialSetDiffuseColor(aMtrl, aCol, aAlpha)

x3d.MaterialSetSpecularColor.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetSpecularColor.restype = ctypes.c_double
def MaterialSetSpecularColor(aMtrl, aCol, aAlpha):
    return x3d.MaterialSetSpecularColor(aMtrl, aCol, aAlpha)

x3d.MaterialSetEmissionColor.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetEmissionColor.restype = ctypes.c_double
def MaterialSetEmissionColor(aMtrl, aCol, aAlpha):
    return x3d.MaterialSetEmissionColor(aMtrl, aCol, aAlpha)

x3d.MaterialGetColor.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialGetColor.restype = ctypes.c_double
def MaterialGetColor(aMtrl, aIndex):
    return x3d.MaterialGetColor(aMtrl, aIndex)

x3d.MaterialGetAlpha.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialGetAlpha.restype = ctypes.c_double
def MaterialGetAlpha(aMtrl, aIndex):
    return x3d.MaterialGetAlpha(aMtrl, aIndex)

x3d.MaterialSetBlendingMode.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetBlendingMode.restype = ctypes.c_double
def MaterialSetBlendingMode(aMtrl, aBm):
    return x3d.MaterialSetBlendingMode(aMtrl, aBm)

x3d.MaterialSetTextureMode.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureMode.restype = ctypes.c_double
def MaterialSetTextureMode(aMtrl, aTm):
    return x3d.MaterialSetTextureMode(aMtrl, aTm)

x3d.MaterialSetTextureMappingMode.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureMappingMode.restype = ctypes.c_double
def MaterialSetTextureMappingMode(aMtrl, aTmm):
    return x3d.MaterialSetTextureMappingMode(aMtrl, aTmm)

x3d.MaterialSetPolygonMode.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetPolygonMode.restype = ctypes.c_double
def MaterialSetPolygonMode(aMtrl, aPm):
    return x3d.MaterialSetPolygonMode(aMtrl, aPm)

x3d.MaterialSetTextureImageAlpha.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureImageAlpha.restype = ctypes.c_double
def MaterialSetTextureImageAlpha(aMtrl, aTia):
    return x3d.MaterialSetTextureImageAlpha(aMtrl, aTia)

x3d.MaterialSetTextureScale.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetTextureScale.restype = ctypes.c_double
def MaterialSetTextureScale(aMtrl, aU, aV):
    return x3d.MaterialSetTextureScale(aMtrl, aU, aV)

x3d.MaterialSetTextureOffset.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetTextureOffset.restype = ctypes.c_double
def MaterialSetTextureOffset(aMtrl, aU, aV):
    return x3d.MaterialSetTextureOffset(aMtrl, aU, aV)

x3d.MaterialSetTextureFilter.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetTextureFilter.restype = ctypes.c_double
def MaterialSetTextureFilter(aMtrl, aMi, aMa):
    return x3d.MaterialSetTextureFilter(aMtrl, aMi, aMa)

x3d.MaterialEnableTexture.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialEnableTexture.restype = ctypes.c_double
def MaterialEnableTexture(aMtrl, aMode):
    return x3d.MaterialEnableTexture(aMtrl, aMode)

x3d.MaterialGetCount.argtypes = []
x3d.MaterialGetCount.restype = ctypes.c_double
def MaterialGetCount():
    return x3d.MaterialGetCount()

x3d.MaterialGetName.argtypes = [ctypes.c_double]
x3d.MaterialGetName.restype = ctypes.c_char_p
def MaterialGetName(aInd):
    return x3d.MaterialGetName(aInd)

x3d.MaterialSetFaceCulling.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetFaceCulling.restype = ctypes.c_double
def MaterialSetFaceCulling(aMtrl, aFc):
    return x3d.MaterialSetFaceCulling(aMtrl, aFc)

x3d.MaterialSetSecondTexture.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialSetSecondTexture.restype = ctypes.c_double
def MaterialSetSecondTexture(aMtrl, aMtrl2):
    return x3d.MaterialSetSecondTexture(aMtrl, aMtrl2)

x3d.MaterialSetTextureFormat.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureFormat.restype = ctypes.c_double
def MaterialSetTextureFormat(aMtrl, aTf):
    return x3d.MaterialSetTextureFormat(aMtrl, aTf)

x3d.MaterialSetTextureFormatEx.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureFormatEx.restype = ctypes.c_double
def MaterialSetTextureFormatEx(aMtrl, aTfex):
    return x3d.MaterialSetTextureFormatEx(aMtrl, aTfex)

x3d.MaterialSetTextureCompression.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureCompression.restype = ctypes.c_double
def MaterialSetTextureCompression(aMtrl, aTc):
    return x3d.MaterialSetTextureCompression(aMtrl, aTc)

x3d.MaterialTextureRequiredMemory.argtypes = [ctypes.c_char_p]
x3d.MaterialTextureRequiredMemory.restype = ctypes.c_double
def MaterialTextureRequiredMemory(aMtrl):
    return x3d.MaterialTextureRequiredMemory(aMtrl)

x3d.MaterialSetFilteringQuality.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetFilteringQuality.restype = ctypes.c_double
def MaterialSetFilteringQuality(aMtrl, aTf):
    return x3d.MaterialSetFilteringQuality(aMtrl, aTf)

x3d.MaterialSetShader.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetShader.restype = ctypes.c_double
def MaterialSetShader(aMtrl, aShd):
    return x3d.MaterialSetShader(aMtrl, aShd)

x3d.MaterialSaveTexture.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialSaveTexture.restype = ctypes.c_double
def MaterialSaveTexture(aMtrl, aFname):
    return x3d.MaterialSaveTexture(aMtrl, aFname)

x3d.MaterialSetOptions.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialSetOptions.restype = ctypes.c_double
def MaterialSetOptions(aMtrl, aOp1, aOp2):
    return x3d.MaterialSetOptions(aMtrl, aOp1, aOp2)

x3d.MaterialSetTextureWrap.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureWrap.restype = ctypes.c_double
def MaterialSetTextureWrap(aMtrl, aWrap):
    return x3d.MaterialSetTextureWrap(aMtrl, aWrap)

x3d.MaterialSetTextureWrapS.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureWrapS.restype = ctypes.c_double
def MaterialSetTextureWrapS(aMtrl, aWrap):
    return x3d.MaterialSetTextureWrapS(aMtrl, aWrap)

x3d.MaterialSetTextureWrapT.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureWrapT.restype = ctypes.c_double
def MaterialSetTextureWrapT(aMtrl, aWrap):
    return x3d.MaterialSetTextureWrapT(aMtrl, aWrap)

x3d.MaterialSetTextureWrapR.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureWrapR.restype = ctypes.c_double
def MaterialSetTextureWrapR(aMtrl, aWrap):
    return x3d.MaterialSetTextureWrapR(aMtrl, aWrap)

x3d.MaterialSetTextureBorderColor.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureBorderColor.restype = ctypes.c_double
def MaterialSetTextureBorderColor(aMtrl, aCol):
    return x3d.MaterialSetTextureBorderColor(aMtrl, aCol)

x3d.MaterialGenTexture.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialGenTexture.restype = ctypes.c_double
def MaterialGenTexture(aMtrl, aW, aH):
    return x3d.MaterialGenTexture(aMtrl, aW, aH)

x3d.MaterialSetTexture.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialSetTexture.restype = ctypes.c_double
def MaterialSetTexture(aMtrl, aMtrl2):
    return x3d.MaterialSetTexture(aMtrl, aMtrl2)

x3d.MaterialGetTextureWidth.argtypes = [ctypes.c_char_p]
x3d.MaterialGetTextureWidth.restype = ctypes.c_double
def MaterialGetTextureWidth(aMtrl):
    return x3d.MaterialGetTextureWidth(aMtrl)

x3d.MaterialGetTextureHeight.argtypes = [ctypes.c_char_p]
x3d.MaterialGetTextureHeight.restype = ctypes.c_double
def MaterialGetTextureHeight(aMtrl):
    return x3d.MaterialGetTextureHeight(aMtrl)

x3d.MaterialLoadTexture.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.MaterialLoadTexture.restype = ctypes.c_double
def MaterialLoadTexture(aMtrl, aFilename):
    return x3d.MaterialLoadTexture(aMtrl, aFilename)

x3d.MaterialAddTextureEx.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialAddTextureEx.restype = ctypes.c_double
def MaterialAddTextureEx(aMtrl, aIndex):
    return x3d.MaterialAddTextureEx(aMtrl, aIndex)

x3d.MaterialGetTextureEx.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialGetTextureEx.restype = ctypes.c_double
def MaterialGetTextureEx(aMtrl, aIndex):
    return x3d.MaterialGetTextureEx(aMtrl, aIndex)

x3d.MaterialTextureExClear.argtypes = [ctypes.c_char_p]
x3d.MaterialTextureExClear.restype = ctypes.c_double
def MaterialTextureExClear(aMtrl):
    return x3d.MaterialTextureExClear(aMtrl)

x3d.MaterialHasTextureEx.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialHasTextureEx.restype = ctypes.c_double
def MaterialHasTextureEx(aMtrl, aIndex):
    return x3d.MaterialHasTextureEx(aMtrl, aIndex)

x3d.MaterialNoiseCreate.argtypes = [ctypes.c_char_p]
x3d.MaterialNoiseCreate.restype = ctypes.c_double
def MaterialNoiseCreate(aMtrl):
    return x3d.MaterialNoiseCreate(aMtrl)

x3d.MaterialNoiseSetDimensions.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double]
x3d.MaterialNoiseSetDimensions.restype = ctypes.c_double
def MaterialNoiseSetDimensions(aMtrl, aW, aH):
    return x3d.MaterialNoiseSetDimensions(aMtrl, aW, aH)

x3d.MaterialNoiseAnimate.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialNoiseAnimate.restype = ctypes.c_double
def MaterialNoiseAnimate(aMtrl, aSpeed):
    return x3d.MaterialNoiseAnimate(aMtrl, aSpeed)

x3d.MaterialNoiseSetMinCut.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialNoiseSetMinCut.restype = ctypes.c_double
def MaterialNoiseSetMinCut(aMtrl, aM):
    return x3d.MaterialNoiseSetMinCut(aMtrl, aM)

x3d.MaterialNoiseSetSharpness.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialNoiseSetSharpness.restype = ctypes.c_double
def MaterialNoiseSetSharpness(aMtrl, aS):
    return x3d.MaterialNoiseSetSharpness(aMtrl, aS)

x3d.MaterialNoiseSetSeamless.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialNoiseSetSeamless.restype = ctypes.c_double
def MaterialNoiseSetSeamless(aMtrl, aMode):
    return x3d.MaterialNoiseSetSeamless(aMtrl, aMode)

x3d.MaterialNoiseRandomSeed.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialNoiseRandomSeed.restype = ctypes.c_double
def MaterialNoiseRandomSeed(aMtrl, aS):
    return x3d.MaterialNoiseRandomSeed(aMtrl, aS)

x3d.MaterialSetDepthWrite.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetDepthWrite.restype = ctypes.c_double
def MaterialSetDepthWrite(aMtrl, aMode):
    return x3d.MaterialSetDepthWrite(aMtrl, aMode)

x3d.MaterialSetDepthTest.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetDepthTest.restype = ctypes.c_double
def MaterialSetDepthTest(aMtrl, aMode):
    return x3d.MaterialSetDepthTest(aMtrl, aMode)

x3d.MaterialGetNameFromLibrary.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MaterialGetNameFromLibrary.restype = ctypes.c_char_p
def MaterialGetNameFromLibrary(aMatlib, aIndex):
    return x3d.MaterialGetNameFromLibrary(aMatlib, aIndex)

x3d.MaterialSetTextureCompareMode.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureCompareMode.restype = ctypes.c_double
def MaterialSetTextureCompareMode(aMtrl, aTcm):
    return x3d.MaterialSetTextureCompareMode(aMtrl, aTcm)

x3d.MaterialSetTextureDepthCompareFunc.argtypes = [ctypes.c_char_p, ctypes.c_double]
x3d.MaterialSetTextureDepthCompareFunc.restype = ctypes.c_double
def MaterialSetTextureDepthCompareFunc(aMtrl, aTcf):
    return x3d.MaterialSetTextureDepthCompareFunc(aMtrl, aTcf)


# memviewer.pas
x3d.MemoryViewerCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MemoryViewerCreate.restype = ctypes.c_double
def MemoryViewerCreate(aW, aH):
    return x3d.MemoryViewerCreate(aW, aH)

x3d.MemoryViewerSetCamera.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MemoryViewerSetCamera.restype = ctypes.c_double
def MemoryViewerSetCamera(aMview, aCam):
    return x3d.MemoryViewerSetCamera(aMview, aCam)

x3d.MemoryViewerRender.argtypes = [ctypes.c_double]
x3d.MemoryViewerRender.restype = ctypes.c_double
def MemoryViewerRender(aMview):
    return x3d.MemoryViewerRender(aMview)

x3d.MemoryViewerSetViewport.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MemoryViewerSetViewport.restype = ctypes.c_double
def MemoryViewerSetViewport(aMview, aX, aY, aW, aH):
    return x3d.MemoryViewerSetViewport(aMview, aX, aY, aW, aH)

x3d.MemoryViewerCopyToTexture.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.MemoryViewerCopyToTexture.restype = ctypes.c_double
def MemoryViewerCopyToTexture(aMview, aMatname):
    return x3d.MemoryViewerCopyToTexture(aMview, aMatname)


# mirror.pas
x3d.MirrorCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MirrorCreate.restype = ctypes.c_double
def MirrorCreate(aTarget, aParent):
    return x3d.MirrorCreate(aTarget, aParent)

x3d.MirrorSetObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MirrorSetObject.restype = ctypes.c_double
def MirrorSetObject(aMirror, aTarget):
    return x3d.MirrorSetObject(aMirror, aTarget)

x3d.MirrorSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MirrorSetOptions.restype = ctypes.c_double
def MirrorSetOptions(aMirror, aStencil, aOpaque, aPlaneclipping, aClearzbuffer):
    return x3d.MirrorSetOptions(aMirror, aStencil, aOpaque, aPlaneclipping, aClearzbuffer)

x3d.MirrorSetShape.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MirrorSetShape.restype = ctypes.c_double
def MirrorSetShape(aMirror, aMs):
    return x3d.MirrorSetShape(aMirror, aMs)

x3d.MirrorSetDiskOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MirrorSetDiskOptions.restype = ctypes.c_double
def MirrorSetDiskOptions(aMirror, aRadius, aSlices):
    return x3d.MirrorSetDiskOptions(aMirror, aRadius, aSlices)


# movement.pas
x3d.MovementCreate.argtypes = [ctypes.c_double]
x3d.MovementCreate.restype = ctypes.c_double
def MovementCreate(aObj):
    return x3d.MovementCreate(aObj)

x3d.MovementStart.argtypes = [ctypes.c_double]
x3d.MovementStart.restype = ctypes.c_double
def MovementStart(aMovement):
    return x3d.MovementStart(aMovement)

x3d.MovementStop.argtypes = [ctypes.c_double]
x3d.MovementStop.restype = ctypes.c_double
def MovementStop(aMovement):
    return x3d.MovementStop(aMovement)

x3d.MovementAutoStartNextPath.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementAutoStartNextPath.restype = ctypes.c_double
def MovementAutoStartNextPath(aMovement, aMode):
    return x3d.MovementAutoStartNextPath(aMovement, aMode)

x3d.MovementAddPath.argtypes = [ctypes.c_double]
x3d.MovementAddPath.restype = ctypes.c_double
def MovementAddPath(aMovement):
    return x3d.MovementAddPath(aMovement)

x3d.MovementSetActivePath.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementSetActivePath.restype = ctypes.c_double
def MovementSetActivePath(aMovement, aInd):
    return x3d.MovementSetActivePath(aMovement, aInd)

x3d.MovementPathSetSplineMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementPathSetSplineMode.restype = ctypes.c_double
def MovementPathSetSplineMode(aPath, aLsm):
    return x3d.MovementPathSetSplineMode(aPath, aLsm)

x3d.MovementPathAddNode.argtypes = [ctypes.c_double]
x3d.MovementPathAddNode.restype = ctypes.c_double
def MovementPathAddNode(aPath):
    return x3d.MovementPathAddNode(aPath)

x3d.MovementPathNodeSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MovementPathNodeSetPosition.restype = ctypes.c_double
def MovementPathNodeSetPosition(aNode, aX, aY, aZ):
    return x3d.MovementPathNodeSetPosition(aNode, aX, aY, aZ)

x3d.MovementPathNodeSetRotation.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MovementPathNodeSetRotation.restype = ctypes.c_double
def MovementPathNodeSetRotation(aNode, aX, aY, aZ):
    return x3d.MovementPathNodeSetRotation(aNode, aX, aY, aZ)

x3d.MovementPathNodeSetSpeed.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementPathNodeSetSpeed.restype = ctypes.c_double
def MovementPathNodeSetSpeed(aNode, aSpeed):
    return x3d.MovementPathNodeSetSpeed(aNode, aSpeed)

x3d.MovementPathShow.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementPathShow.restype = ctypes.c_double
def MovementPathShow(aPat, aVis):
    return x3d.MovementPathShow(aPat, aVis)

x3d.MovementPathSetLoop.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementPathSetLoop.restype = ctypes.c_double
def MovementPathSetLoop(aPat, aLoopn):
    return x3d.MovementPathSetLoop(aPat, aLoopn)

x3d.MovementPathDeleteNode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MovementPathDeleteNode.restype = ctypes.c_double
def MovementPathDeleteNode(aPat, aNode):
    return x3d.MovementPathDeleteNode(aPat, aNode)


# navigator.pas
x3d.NavigatorCreate.argtypes = []
x3d.NavigatorCreate.restype = ctypes.c_double
def NavigatorCreate():
    return x3d.NavigatorCreate()

x3d.NavigatorSetObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorSetObject.restype = ctypes.c_double
def NavigatorSetObject(aNavigator, aObj):
    return x3d.NavigatorSetObject(aNavigator, aObj)

x3d.NavigatorSetUseVirtualUp.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorSetUseVirtualUp.restype = ctypes.c_double
def NavigatorSetUseVirtualUp(aNavigator, aMode):
    return x3d.NavigatorSetUseVirtualUp(aNavigator, aMode)

x3d.NavigatorSetVirtualUp.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.NavigatorSetVirtualUp.restype = ctypes.c_double
def NavigatorSetVirtualUp(aNavigator, aX, aY, aZ):
    return x3d.NavigatorSetVirtualUp(aNavigator, aX, aY, aZ)

x3d.NavigatorTurnHorizontal.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorTurnHorizontal.restype = ctypes.c_double
def NavigatorTurnHorizontal(aNavigator, aAngle):
    return x3d.NavigatorTurnHorizontal(aNavigator, aAngle)

x3d.NavigatorTurnVertical.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorTurnVertical.restype = ctypes.c_double
def NavigatorTurnVertical(aNavigator, aAngle):
    return x3d.NavigatorTurnVertical(aNavigator, aAngle)

x3d.NavigatorMoveForward.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorMoveForward.restype = ctypes.c_double
def NavigatorMoveForward(aNavigator, aSpd):
    return x3d.NavigatorMoveForward(aNavigator, aSpd)

x3d.NavigatorStrafeHorizontal.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorStrafeHorizontal.restype = ctypes.c_double
def NavigatorStrafeHorizontal(aNavigator, aSpd):
    return x3d.NavigatorStrafeHorizontal(aNavigator, aSpd)

x3d.NavigatorStrafeVertical.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorStrafeVertical.restype = ctypes.c_double
def NavigatorStrafeVertical(aNavigator, aSpd):
    return x3d.NavigatorStrafeVertical(aNavigator, aSpd)

x3d.NavigatorStraighten.argtypes = [ctypes.c_double]
x3d.NavigatorStraighten.restype = ctypes.c_double
def NavigatorStraighten(aNavigator):
    return x3d.NavigatorStraighten(aNavigator)

x3d.NavigatorFlyForward.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorFlyForward.restype = ctypes.c_double
def NavigatorFlyForward(aNavigator, aSpd):
    return x3d.NavigatorFlyForward(aNavigator, aSpd)

x3d.NavigatorMoveUpWhenMovingForward.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorMoveUpWhenMovingForward.restype = ctypes.c_double
def NavigatorMoveUpWhenMovingForward(aNavigator, aMode):
    return x3d.NavigatorMoveUpWhenMovingForward(aNavigator, aMode)

x3d.NavigatorInvertHorizontalWhenUpsideDown.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorInvertHorizontalWhenUpsideDown.restype = ctypes.c_double
def NavigatorInvertHorizontalWhenUpsideDown(aNavigator, aMode):
    return x3d.NavigatorInvertHorizontalWhenUpsideDown(aNavigator, aMode)

x3d.NavigatorSetAngleLock.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.NavigatorSetAngleLock.restype = ctypes.c_double
def NavigatorSetAngleLock(aNavigator, aMode):
    return x3d.NavigatorSetAngleLock(aNavigator, aMode)

x3d.NavigatorSetAngles.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.NavigatorSetAngles.restype = ctypes.c_double
def NavigatorSetAngles(aNavigator, aMinangle, aMaxangle):
    return x3d.NavigatorSetAngles(aNavigator, aMinangle, aMaxangle)


# object.pas
x3d.ObjectHide.argtypes = [ctypes.c_double]
x3d.ObjectHide.restype = ctypes.c_double
def ObjectHide(aObj):
    return x3d.ObjectHide(aObj)

x3d.ObjectShow.argtypes = [ctypes.c_double]
x3d.ObjectShow.restype = ctypes.c_double
def ObjectShow(aObj):
    return x3d.ObjectShow(aObj)

x3d.ObjectIsVisible.argtypes = [ctypes.c_double]
x3d.ObjectIsVisible.restype = ctypes.c_double
def ObjectIsVisible(aObj):
    return x3d.ObjectIsVisible(aObj)

x3d.ObjectCopy.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectCopy.restype = ctypes.c_double
def ObjectCopy(aObj, aParent):
    return x3d.ObjectCopy(aObj, aParent)

x3d.ObjectDestroy.argtypes = [ctypes.c_double]
x3d.ObjectDestroy.restype = ctypes.c_double
def ObjectDestroy(aObj):
    return x3d.ObjectDestroy(aObj)

x3d.ObjectDestroyChildren.argtypes = [ctypes.c_double]
x3d.ObjectDestroyChildren.restype = ctypes.c_double
def ObjectDestroyChildren(aObj):
    return x3d.ObjectDestroyChildren(aObj)

x3d.ObjectSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetPosition.restype = ctypes.c_double
def ObjectSetPosition(aObj, aX, aY, aZ):
    return x3d.ObjectSetPosition(aObj, aX, aY, aZ)

x3d.ObjectGetPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetPosition.restype = ctypes.c_double
def ObjectGetPosition(aObj, aInd):
    return x3d.ObjectGetPosition(aObj, aInd)

x3d.ObjectGetAbsolutePosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsolutePosition.restype = ctypes.c_double
def ObjectGetAbsolutePosition(aObj, aInd):
    return x3d.ObjectGetAbsolutePosition(aObj, aInd)

x3d.ObjectSetPositionOfObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetPositionOfObject.restype = ctypes.c_double
def ObjectSetPositionOfObject(aObj1, aObj2):
    return x3d.ObjectSetPositionOfObject(aObj1, aObj2)

x3d.ObjectAlignWithObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectAlignWithObject.restype = ctypes.c_double
def ObjectAlignWithObject(aObj1, aObj2):
    return x3d.ObjectAlignWithObject(aObj1, aObj2)

x3d.ObjectSetPositionX.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetPositionX.restype = ctypes.c_double
def ObjectSetPositionX(aObj, aPosx):
    return x3d.ObjectSetPositionX(aObj, aPosx)

x3d.ObjectSetPositionY.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetPositionY.restype = ctypes.c_double
def ObjectSetPositionY(aObj, aPosy):
    return x3d.ObjectSetPositionY(aObj, aPosy)

x3d.ObjectSetPositionZ.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetPositionZ.restype = ctypes.c_double
def ObjectSetPositionZ(aObj, aPosz):
    return x3d.ObjectSetPositionZ(aObj, aPosz)

x3d.ObjectGetPositionX.argtypes = [ctypes.c_double]
x3d.ObjectGetPositionX.restype = ctypes.c_double
def ObjectGetPositionX(aObj):
    return x3d.ObjectGetPositionX(aObj)

x3d.ObjectGetPositionY.argtypes = [ctypes.c_double]
x3d.ObjectGetPositionY.restype = ctypes.c_double
def ObjectGetPositionY(aObj):
    return x3d.ObjectGetPositionY(aObj)

x3d.ObjectGetPositionZ.argtypes = [ctypes.c_double]
x3d.ObjectGetPositionZ.restype = ctypes.c_double
def ObjectGetPositionZ(aObj):
    return x3d.ObjectGetPositionZ(aObj)

x3d.ObjectSetAbsolutePosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetAbsolutePosition.restype = ctypes.c_double
def ObjectSetAbsolutePosition(aObj, aX, aY, aZ):
    return x3d.ObjectSetAbsolutePosition(aObj, aX, aY, aZ)

x3d.ObjectSetDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetDirection.restype = ctypes.c_double
def ObjectSetDirection(aObj, aX, aY, aZ):
    return x3d.ObjectSetDirection(aObj, aX, aY, aZ)

x3d.ObjectGetDirection.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetDirection.restype = ctypes.c_double
def ObjectGetDirection(aObj, aInd):
    return x3d.ObjectGetDirection(aObj, aInd)

x3d.ObjectSetAbsoluteDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetAbsoluteDirection.restype = ctypes.c_double
def ObjectSetAbsoluteDirection(aObj, aX, aY, aZ):
    return x3d.ObjectSetAbsoluteDirection(aObj, aX, aY, aZ)

x3d.ObjectGetAbsoluteDirection.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsoluteDirection.restype = ctypes.c_double
def ObjectGetAbsoluteDirection(aObj, aInd):
    return x3d.ObjectGetAbsoluteDirection(aObj, aInd)

x3d.ObjectGetPitch.argtypes = [ctypes.c_double]
x3d.ObjectGetPitch.restype = ctypes.c_double
def ObjectGetPitch(aObj):
    return x3d.ObjectGetPitch(aObj)

x3d.ObjectGetTurn.argtypes = [ctypes.c_double]
x3d.ObjectGetTurn.restype = ctypes.c_double
def ObjectGetTurn(aObj):
    return x3d.ObjectGetTurn(aObj)

x3d.ObjectGetRoll.argtypes = [ctypes.c_double]
x3d.ObjectGetRoll.restype = ctypes.c_double
def ObjectGetRoll(aObj):
    return x3d.ObjectGetRoll(aObj)

x3d.ObjectSetRotation.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetRotation.restype = ctypes.c_double
def ObjectSetRotation(aObj, aX, aY, aZ):
    return x3d.ObjectSetRotation(aObj, aX, aY, aZ)

x3d.ObjectMove.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectMove.restype = ctypes.c_double
def ObjectMove(aObj, aSpd):
    return x3d.ObjectMove(aObj, aSpd)

x3d.ObjectLift.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectLift.restype = ctypes.c_double
def ObjectLift(aObj, aSpd):
    return x3d.ObjectLift(aObj, aSpd)

x3d.ObjectStrafe.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectStrafe.restype = ctypes.c_double
def ObjectStrafe(aObj, aSpd):
    return x3d.ObjectStrafe(aObj, aSpd)

x3d.ObjectTranslate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectTranslate.restype = ctypes.c_double
def ObjectTranslate(aObj, aX, aY, aZ):
    return x3d.ObjectTranslate(aObj, aX, aY, aZ)

x3d.ObjectRotate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectRotate.restype = ctypes.c_double
def ObjectRotate(aObj, aP, aT, aR):
    return x3d.ObjectRotate(aObj, aP, aT, aR)

x3d.ObjectScale.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectScale.restype = ctypes.c_double
def ObjectScale(aObj, aX, aY, aZ):
    return x3d.ObjectScale(aObj, aX, aY, aZ)

x3d.ObjectSetScale.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetScale.restype = ctypes.c_double
def ObjectSetScale(aObj, aX, aY, aZ):
    return x3d.ObjectSetScale(aObj, aX, aY, aZ)

x3d.ObjectGetScale.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetScale.restype = ctypes.c_double
def ObjectGetScale(aObj, aInd):
    return x3d.ObjectGetScale(aObj, aInd)

x3d.ObjectSetUpVector.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetUpVector.restype = ctypes.c_double
def ObjectSetUpVector(aObj, aX, aY, aZ):
    return x3d.ObjectSetUpVector(aObj, aX, aY, aZ)

x3d.ObjectPointToObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectPointToObject.restype = ctypes.c_double
def ObjectPointToObject(aObj1, aObj2):
    return x3d.ObjectPointToObject(aObj1, aObj2)

x3d.ObjectShowAxes.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectShowAxes.restype = ctypes.c_double
def ObjectShowAxes(aObj, aMode):
    return x3d.ObjectShowAxes(aObj, aMode)

x3d.ObjectGetGroundHeight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetGroundHeight.restype = ctypes.c_double
def ObjectGetGroundHeight(aObj, aTarget):
    return x3d.ObjectGetGroundHeight(aObj, aTarget)

x3d.ObjectSceneRaycast.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSceneRaycast.restype = ctypes.c_double
def ObjectSceneRaycast(aObj, aTarget):
    return x3d.ObjectSceneRaycast(aObj, aTarget)

x3d.ObjectRaycast.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectRaycast.restype = ctypes.c_double
def ObjectRaycast(aObj, aTarget):
    return x3d.ObjectRaycast(aObj, aTarget)

x3d.ObjectSetMaterial.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ObjectSetMaterial.restype = ctypes.c_double
def ObjectSetMaterial(aObj, aMat):
    return x3d.ObjectSetMaterial(aObj, aMat)

x3d.ObjectGetMaterial.argtypes = [ctypes.c_double]
x3d.ObjectGetMaterial.restype = ctypes.c_char_p
def ObjectGetMaterial(aObj):
    return x3d.ObjectGetMaterial(aObj)

x3d.ObjectGetDistance.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetDistance.restype = ctypes.c_double
def ObjectGetDistance(aObj, aTarget):
    return x3d.ObjectGetDistance(aObj, aTarget)

x3d.ObjectCheckCubeVsCube.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectCheckCubeVsCube.restype = ctypes.c_double
def ObjectCheckCubeVsCube(aObj1, aObj2):
    return x3d.ObjectCheckCubeVsCube(aObj1, aObj2)

x3d.ObjectCheckSphereVsSphere.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectCheckSphereVsSphere.restype = ctypes.c_double
def ObjectCheckSphereVsSphere(aObj1, aObj2):
    return x3d.ObjectCheckSphereVsSphere(aObj1, aObj2)

x3d.ObjectCheckSphereVsCube.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectCheckSphereVsCube.restype = ctypes.c_double
def ObjectCheckSphereVsCube(aObj1, aObj2):
    return x3d.ObjectCheckSphereVsCube(aObj1, aObj2)

x3d.ObjectCheckCubeVsFace.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectCheckCubeVsFace.restype = ctypes.c_double
def ObjectCheckCubeVsFace(aObj1, aObj2):
    return x3d.ObjectCheckCubeVsFace(aObj1, aObj2)

x3d.ObjectCheckFaceVsFace.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectCheckFaceVsFace.restype = ctypes.c_double
def ObjectCheckFaceVsFace(aObj1, aObj2):
    return x3d.ObjectCheckFaceVsFace(aObj1, aObj2)

x3d.ObjectIsPointInObject.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectIsPointInObject.restype = ctypes.c_double
def ObjectIsPointInObject(aObj1, aX, aY, aZ):
    return x3d.ObjectIsPointInObject(aObj1, aX, aY, aZ)

x3d.ObjectSetCulling.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetCulling.restype = ctypes.c_double
def ObjectSetCulling(aObj1, aVc):
    return x3d.ObjectSetCulling(aObj1, aVc)

x3d.ObjectSetName.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ObjectSetName.restype = ctypes.c_double
def ObjectSetName(aObj1, aName):
    return x3d.ObjectSetName(aObj1, aName)

x3d.ObjectGetName.argtypes = [ctypes.c_double]
x3d.ObjectGetName.restype = ctypes.c_char_p
def ObjectGetName(aObj1):
    return x3d.ObjectGetName(aObj1)

x3d.ObjectGetClassName.argtypes = [ctypes.c_double]
x3d.ObjectGetClassName.restype = ctypes.c_char_p
def ObjectGetClassName(aObj1):
    return x3d.ObjectGetClassName(aObj1)

x3d.ObjectSetTag.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetTag.restype = ctypes.c_double
def ObjectSetTag(aObj1, aTag):
    return x3d.ObjectSetTag(aObj1, aTag)

x3d.ObjectGetTag.argtypes = [ctypes.c_double]
x3d.ObjectGetTag.restype = ctypes.c_double
def ObjectGetTag(aObj1):
    return x3d.ObjectGetTag(aObj1)

x3d.ObjectGetParent.argtypes = [ctypes.c_double]
x3d.ObjectGetParent.restype = ctypes.c_double
def ObjectGetParent(aObj1):
    return x3d.ObjectGetParent(aObj1)

x3d.ObjectGetChildCount.argtypes = [ctypes.c_double]
x3d.ObjectGetChildCount.restype = ctypes.c_double
def ObjectGetChildCount(aObj1):
    return x3d.ObjectGetChildCount(aObj1)

x3d.ObjectGetChild.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetChild.restype = ctypes.c_double
def ObjectGetChild(aObj1, aInd):
    return x3d.ObjectGetChild(aObj1, aInd)

x3d.ObjectGetIndex.argtypes = [ctypes.c_double]
x3d.ObjectGetIndex.restype = ctypes.c_double
def ObjectGetIndex(aObj1):
    return x3d.ObjectGetIndex(aObj1)

x3d.ObjectFindChild.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ObjectFindChild.restype = ctypes.c_double
def ObjectFindChild(aObj1, aName):
    return x3d.ObjectFindChild(aObj1, aName)

x3d.ObjectGetBoundingSphereRadius.argtypes = [ctypes.c_double]
x3d.ObjectGetBoundingSphereRadius.restype = ctypes.c_double
def ObjectGetBoundingSphereRadius(aObj1):
    return x3d.ObjectGetBoundingSphereRadius(aObj1)

x3d.ObjectGetAbsoluteUp.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsoluteUp.restype = ctypes.c_double
def ObjectGetAbsoluteUp(aObj1, aInd):
    return x3d.ObjectGetAbsoluteUp(aObj1, aInd)

x3d.ObjectSetAbsoluteUp.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetAbsoluteUp.restype = ctypes.c_double
def ObjectSetAbsoluteUp(aObj1, aX, aY, aZ):
    return x3d.ObjectSetAbsoluteUp(aObj1, aX, aY, aZ)

x3d.ObjectGetAbsoluteRight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsoluteRight.restype = ctypes.c_double
def ObjectGetAbsoluteRight(aObj1, aInd):
    return x3d.ObjectGetAbsoluteRight(aObj1, aInd)

x3d.ObjectGetAbsoluteXVector.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsoluteXVector.restype = ctypes.c_double
def ObjectGetAbsoluteXVector(aObj1, aInd):
    return x3d.ObjectGetAbsoluteXVector(aObj1, aInd)

x3d.ObjectGetAbsoluteYVector.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsoluteYVector.restype = ctypes.c_double
def ObjectGetAbsoluteYVector(aObj1, aInd):
    return x3d.ObjectGetAbsoluteYVector(aObj1, aInd)

x3d.ObjectGetAbsoluteZVector.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetAbsoluteZVector.restype = ctypes.c_double
def ObjectGetAbsoluteZVector(aObj1, aInd):
    return x3d.ObjectGetAbsoluteZVector(aObj1, aInd)

x3d.ObjectGetRight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetRight.restype = ctypes.c_double
def ObjectGetRight(aObj1, aInd):
    return x3d.ObjectGetRight(aObj1, aInd)

x3d.ObjectMoveChildUp.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectMoveChildUp.restype = ctypes.c_double
def ObjectMoveChildUp(aObj1, aInd):
    return x3d.ObjectMoveChildUp(aObj1, aInd)

x3d.ObjectMoveChildDown.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectMoveChildDown.restype = ctypes.c_double
def ObjectMoveChildDown(aObj1, aInd):
    return x3d.ObjectMoveChildDown(aObj1, aInd)

x3d.ObjectSetParent.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectSetParent.restype = ctypes.c_double
def ObjectSetParent(aObj1, aObj2):
    return x3d.ObjectSetParent(aObj1, aObj2)

x3d.ObjectRemoveChild.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectRemoveChild.restype = ctypes.c_double
def ObjectRemoveChild(aObj1, aObj2, aKeep):
    return x3d.ObjectRemoveChild(aObj1, aObj2, aKeep)

x3d.ObjectMoveObjectAround.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectMoveObjectAround.restype = ctypes.c_double
def ObjectMoveObjectAround(aObj1, aObj2, aP, aT):
    return x3d.ObjectMoveObjectAround(aObj1, aObj2, aP, aT)

x3d.ObjectPitch.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectPitch.restype = ctypes.c_double
def ObjectPitch(aObj1, aAngle):
    return x3d.ObjectPitch(aObj1, aAngle)

x3d.ObjectTurn.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectTurn.restype = ctypes.c_double
def ObjectTurn(aObj1, aAngle):
    return x3d.ObjectTurn(aObj1, aAngle)

x3d.ObjectRoll.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectRoll.restype = ctypes.c_double
def ObjectRoll(aObj1, aAngle):
    return x3d.ObjectRoll(aObj1, aAngle)

x3d.ObjectGetUp.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectGetUp.restype = ctypes.c_double
def ObjectGetUp(aObj1, aInd):
    return x3d.ObjectGetUp(aObj1, aInd)

x3d.ObjectRotateAbsolute.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectRotateAbsolute.restype = ctypes.c_double
def ObjectRotateAbsolute(aObj1, aX, aY, aZ):
    return x3d.ObjectRotateAbsolute(aObj1, aX, aY, aZ)

x3d.ObjectRotateAbsoluteVector.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectRotateAbsoluteVector.restype = ctypes.c_double
def ObjectRotateAbsoluteVector(aObj1, aX, aY, aZ, aAngle):
    return x3d.ObjectRotateAbsoluteVector(aObj1, aX, aY, aZ, aAngle)

x3d.ObjectSetMatrixColumn.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectSetMatrixColumn.restype = ctypes.c_double
def ObjectSetMatrixColumn(aObj1, aInd, aX, aY, aZ, aW):
    return x3d.ObjectSetMatrixColumn(aObj1, aInd, aX, aY, aZ, aW)

x3d.ObjectExportMatrix.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectExportMatrix.restype = ctypes.c_double
def ObjectExportMatrix(aObj1, aObj2):
    return x3d.ObjectExportMatrix(aObj1, aObj2)

x3d.ObjectExportAbsoluteMatrix.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectExportAbsoluteMatrix.restype = ctypes.c_double
def ObjectExportAbsoluteMatrix(aObj1, aObj2):
    return x3d.ObjectExportAbsoluteMatrix(aObj1, aObj2)

x3d.ObjectInFrustum.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectInFrustum.restype = ctypes.c_double
def ObjectInFrustum(aObj, aViewer):
    return x3d.ObjectInFrustum(aObj, aViewer)

x3d.ObjectFindByName.argtypes = [ctypes.c_char_p]
x3d.ObjectFindByName.restype = ctypes.c_double
def ObjectFindByName(aName):
    return x3d.ObjectFindByName(aName)

x3d.ObjectIgnoreDepthBuffer.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectIgnoreDepthBuffer.restype = ctypes.c_double
def ObjectIgnoreDepthBuffer(aObj, aMode):
    return x3d.ObjectIgnoreDepthBuffer(aObj, aMode)

x3d.ObjectIsPicked.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectIsPicked.restype = ctypes.c_double
def ObjectIsPicked(aObj, aViewer, aX, aY):
    return x3d.ObjectIsPicked(aObj, aViewer, aX, aY)

x3d.ObjectStructureChanged.argtypes = [ctypes.c_double]
x3d.ObjectStructureChanged.restype = ctypes.c_double
def ObjectStructureChanged(aObj):
    return x3d.ObjectStructureChanged(aObj)

x3d.ObjectClearStructureChanged.argtypes = [ctypes.c_double]
x3d.ObjectClearStructureChanged.restype = ctypes.c_double
def ObjectClearStructureChanged(aObj):
    return x3d.ObjectClearStructureChanged(aObj)

x3d.ObjectNotifyChange.argtypes = [ctypes.c_double]
x3d.ObjectNotifyChange.restype = ctypes.c_double
def ObjectNotifyChange(aObj):
    return x3d.ObjectNotifyChange(aObj)


# objecthash.pas
x3d.ObjectHashCreate.argtypes = []
x3d.ObjectHashCreate.restype = ctypes.c_double
def ObjectHashCreate():
    return x3d.ObjectHashCreate()

x3d.ObjectHashSetItem.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ObjectHashSetItem.restype = ctypes.c_double
def ObjectHashSetItem(aHash, aKey, aPchar, aObj):
    return x3d.ObjectHashSetItem(aHash, aKey, aPchar, aObj)

x3d.ObjectHashGetItem.argtypes = [ctypes.c_double]
x3d.ObjectHashGetItem.restype = ctypes.c_double
def ObjectHashGetItem(aHash, aKey, aPchar):
    return x3d.ObjectHashGetItem(aHash, aKey, aPchar)

x3d.ObjectHashDeleteItem.argtypes = [ctypes.c_double]
x3d.ObjectHashDeleteItem.restype = ctypes.c_double
def ObjectHashDeleteItem(aHash, aKey, aPchar):
    return x3d.ObjectHashDeleteItem(aHash, aKey, aPchar)

x3d.ObjectHashGetItemCount.argtypes = [ctypes.c_double]
x3d.ObjectHashGetItemCount.restype = ctypes.c_double
def ObjectHashGetItemCount(aHash):
    return x3d.ObjectHashGetItemCount(aHash)

x3d.ObjectHashClear.argtypes = [ctypes.c_double]
x3d.ObjectHashClear.restype = ctypes.c_double
def ObjectHashClear(aHash):
    return x3d.ObjectHashClear(aHash)

x3d.ObjectHashDestroy.argtypes = [ctypes.c_double]
x3d.ObjectHashDestroy.restype = ctypes.c_double
def ObjectHashDestroy(aHash):
    return x3d.ObjectHashDestroy(aHash)


# objectlist.pas
x3d.ObjectListCreate.argtypes = []
x3d.ObjectListCreate.restype = ctypes.c_double
def ObjectListCreate():
    return x3d.ObjectListCreate()

x3d.ObjectListAdd.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ObjectListAdd.restype = ctypes.c_double
def ObjectListAdd(aList, aObj):
    return x3d.ObjectListAdd(aList, aObj)

x3d.ObjectListGetCount.argtypes = [ctypes.c_double]
x3d.ObjectListGetCount.restype = ctypes.c_double
def ObjectListGetCount(aList):
    return x3d.ObjectListGetCount(aList)


# ode.pas
x3d.OdeManagerCreate.argtypes = []
x3d.OdeManagerCreate.restype = ctypes.c_double
def OdeManagerCreate():
    return x3d.OdeManagerCreate()

x3d.OdeManagerDestroy.argtypes = []
x3d.OdeManagerDestroy.restype = ctypes.c_double
def OdeManagerDestroy():
    return x3d.OdeManagerDestroy()

x3d.OdeManagerStep.argtypes = [ctypes.c_double]
x3d.OdeManagerStep.restype = ctypes.c_double
def OdeManagerStep(aDt):
    return x3d.OdeManagerStep(aDt)

x3d.OdeManagerGetNumContactJoints.argtypes = []
x3d.OdeManagerGetNumContactJoints.restype = ctypes.c_double
def OdeManagerGetNumContactJoints():
    return x3d.OdeManagerGetNumContactJoints()

x3d.OdeManagerSetGravity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeManagerSetGravity.restype = ctypes.c_double
def OdeManagerSetGravity(aX, aY, aZ):
    return x3d.OdeManagerSetGravity(aX, aY, aZ)

x3d.OdeManagerSetSolver.argtypes = [ctypes.c_double]
x3d.OdeManagerSetSolver.restype = ctypes.c_double
def OdeManagerSetSolver(aOsm):
    return x3d.OdeManagerSetSolver(aOsm)

x3d.OdeManagerSetIterations.argtypes = [ctypes.c_double]
x3d.OdeManagerSetIterations.restype = ctypes.c_double
def OdeManagerSetIterations(aIterations):
    return x3d.OdeManagerSetIterations(aIterations)

x3d.OdeManagerSetMaxContacts.argtypes = [ctypes.c_double]
x3d.OdeManagerSetMaxContacts.restype = ctypes.c_double
def OdeManagerSetMaxContacts(aMaxcontacts):
    return x3d.OdeManagerSetMaxContacts(aMaxcontacts)

x3d.OdeManagerSetVisible.argtypes = [ctypes.c_double]
x3d.OdeManagerSetVisible.restype = ctypes.c_double
def OdeManagerSetVisible(aMode):
    return x3d.OdeManagerSetVisible(aMode)

x3d.OdeManagerSetGeomColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeManagerSetGeomColor.restype = ctypes.c_double
def OdeManagerSetGeomColor(aColordynamicdisabled, aColordynamicenabled, aColorstatic):
    return x3d.OdeManagerSetGeomColor(aColordynamicdisabled, aColordynamicenabled, aColorstatic)

x3d.OdeWorldSetAutoDisableFlag.argtypes = [ctypes.c_double]
x3d.OdeWorldSetAutoDisableFlag.restype = ctypes.c_double
def OdeWorldSetAutoDisableFlag(aFlag):
    return x3d.OdeWorldSetAutoDisableFlag(aFlag)

x3d.OdeWorldSetAutoDisableLinearThreshold.argtypes = [ctypes.c_double]
x3d.OdeWorldSetAutoDisableLinearThreshold.restype = ctypes.c_double
def OdeWorldSetAutoDisableLinearThreshold(aVelocity):
    return x3d.OdeWorldSetAutoDisableLinearThreshold(aVelocity)

x3d.OdeWorldSetAutoDisableAngularThreshold.argtypes = [ctypes.c_double]
x3d.OdeWorldSetAutoDisableAngularThreshold.restype = ctypes.c_double
def OdeWorldSetAutoDisableAngularThreshold(aVelocity):
    return x3d.OdeWorldSetAutoDisableAngularThreshold(aVelocity)

x3d.OdeWorldSetAutoDisableSteps.argtypes = [ctypes.c_double]
x3d.OdeWorldSetAutoDisableSteps.restype = ctypes.c_double
def OdeWorldSetAutoDisableSteps(aSteps):
    return x3d.OdeWorldSetAutoDisableSteps(aSteps)

x3d.OdeWorldSetAutoDisableTime.argtypes = [ctypes.c_double]
x3d.OdeWorldSetAutoDisableTime.restype = ctypes.c_double
def OdeWorldSetAutoDisableTime(aTime):
    return x3d.OdeWorldSetAutoDisableTime(aTime)

x3d.OdeStaticCreate.argtypes = [ctypes.c_double]
x3d.OdeStaticCreate.restype = ctypes.c_double
def OdeStaticCreate(aObj):
    return x3d.OdeStaticCreate(aObj)

x3d.OdeDynamicCreate.argtypes = [ctypes.c_double]
x3d.OdeDynamicCreate.restype = ctypes.c_double
def OdeDynamicCreate(aObj):
    return x3d.OdeDynamicCreate(aObj)

x3d.OdeTerrainCreate.argtypes = [ctypes.c_double]
x3d.OdeTerrainCreate.restype = ctypes.c_double
def OdeTerrainCreate(aTerr):
    return x3d.OdeTerrainCreate(aTerr)

x3d.OdeDynamicCalculateMass.argtypes = [ctypes.c_double]
x3d.OdeDynamicCalculateMass.restype = ctypes.c_double
def OdeDynamicCalculateMass(aObj):
    return x3d.OdeDynamicCalculateMass(aObj)

x3d.OdeDynamicCalibrateCenterOfMass.argtypes = [ctypes.c_double]
x3d.OdeDynamicCalibrateCenterOfMass.restype = ctypes.c_double
def OdeDynamicCalibrateCenterOfMass(aObj):
    return x3d.OdeDynamicCalibrateCenterOfMass(aObj)

x3d.OdeDynamicAlignObject.argtypes = [ctypes.c_double]
x3d.OdeDynamicAlignObject.restype = ctypes.c_double
def OdeDynamicAlignObject(aObj):
    return x3d.OdeDynamicAlignObject(aObj)

x3d.OdeDynamicEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicEnable.restype = ctypes.c_double
def OdeDynamicEnable(aObj, aMode):
    return x3d.OdeDynamicEnable(aObj, aMode)

x3d.OdeDynamicSetAutoDisableFlag.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetAutoDisableFlag.restype = ctypes.c_double
def OdeDynamicSetAutoDisableFlag(aObj, aMode):
    return x3d.OdeDynamicSetAutoDisableFlag(aObj, aMode)

x3d.OdeDynamicSetAutoDisableLinearThreshold.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetAutoDisableLinearThreshold.restype = ctypes.c_double
def OdeDynamicSetAutoDisableLinearThreshold(aObj, aVelocity):
    return x3d.OdeDynamicSetAutoDisableLinearThreshold(aObj, aVelocity)

x3d.OdeDynamicSetAutoDisableAngularThreshold.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetAutoDisableAngularThreshold.restype = ctypes.c_double
def OdeDynamicSetAutoDisableAngularThreshold(aObj, aVelocity):
    return x3d.OdeDynamicSetAutoDisableAngularThreshold(aObj, aVelocity)

x3d.OdeDynamicSetAutoDisableSteps.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetAutoDisableSteps.restype = ctypes.c_double
def OdeDynamicSetAutoDisableSteps(aObj, aSteps):
    return x3d.OdeDynamicSetAutoDisableSteps(aObj, aSteps)

x3d.OdeDynamicSetAutoDisableTime.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetAutoDisableTime.restype = ctypes.c_double
def OdeDynamicSetAutoDisableTime(aObj, aTime):
    return x3d.OdeDynamicSetAutoDisableTime(aObj, aTime)

x3d.OdeDynamicAddForce.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddForce.restype = ctypes.c_double
def OdeDynamicAddForce(aObj, aX, aY, aZ):
    return x3d.OdeDynamicAddForce(aObj, aX, aY, aZ)

x3d.OdeDynamicAddForceAtPos.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddForceAtPos.restype = ctypes.c_double
def OdeDynamicAddForceAtPos(aObj, aX, aY, aZ, aPx, aPy, aPz):
    return x3d.OdeDynamicAddForceAtPos(aObj, aX, aY, aZ, aPx, aPy, aPz)

x3d.OdeDynamicAddForceAtRelPos.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddForceAtRelPos.restype = ctypes.c_double
def OdeDynamicAddForceAtRelPos(aObj, aX, aY, aZ, aPx, aPy, aPz):
    return x3d.OdeDynamicAddForceAtRelPos(aObj, aX, aY, aZ, aPx, aPy, aPz)

x3d.OdeDynamicAddRelForce.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddRelForce.restype = ctypes.c_double
def OdeDynamicAddRelForce(aObj, aX, aY, aZ):
    return x3d.OdeDynamicAddRelForce(aObj, aX, aY, aZ)

x3d.OdeDynamicAddRelForceAtPos.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddRelForceAtPos.restype = ctypes.c_double
def OdeDynamicAddRelForceAtPos(aObj, aX, aY, aZ, aPx, aPy, aPz):
    return x3d.OdeDynamicAddRelForceAtPos(aObj, aX, aY, aZ, aPx, aPy, aPz)

x3d.OdeDynamicAddRelForceAtRelPos.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddRelForceAtRelPos.restype = ctypes.c_double
def OdeDynamicAddRelForceAtRelPos(aObj, aX, aY, aZ, aPx, aPy, aPz):
    return x3d.OdeDynamicAddRelForceAtRelPos(aObj, aX, aY, aZ, aPx, aPy, aPz)

x3d.OdeDynamicAddTorque.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddTorque.restype = ctypes.c_double
def OdeDynamicAddTorque(aObj, aX, aY, aZ):
    return x3d.OdeDynamicAddTorque(aObj, aX, aY, aZ)

x3d.OdeDynamicAddRelTorque.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicAddRelTorque.restype = ctypes.c_double
def OdeDynamicAddRelTorque(aObj, aX, aY, aZ):
    return x3d.OdeDynamicAddRelTorque(aObj, aX, aY, aZ)

x3d.OdeAddBox.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeAddBox.restype = ctypes.c_double
def OdeAddBox(aObj, aX, aY, aZ, aW, aH, aD):
    return x3d.OdeAddBox(aObj, aX, aY, aZ, aW, aH, aD)

x3d.OdeAddSphere.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeAddSphere.restype = ctypes.c_double
def OdeAddSphere(aObj, aX, aY, aZ, aR):
    return x3d.OdeAddSphere(aObj, aX, aY, aZ, aR)

x3d.OdeAddPlane.argtypes = [ctypes.c_double]
x3d.OdeAddPlane.restype = ctypes.c_double
def OdeAddPlane(aObj):
    return x3d.OdeAddPlane(aObj)

x3d.OdeAddCylinder.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeAddCylinder.restype = ctypes.c_double
def OdeAddCylinder(aObj, aX, aY, aZ, aLen, aR):
    return x3d.OdeAddCylinder(aObj, aX, aY, aZ, aLen, aR)

x3d.OdeAddCapsule.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeAddCapsule.restype = ctypes.c_double
def OdeAddCapsule(aObj, aX, aY, aZ, aLen, aR):
    return x3d.OdeAddCapsule(aObj, aX, aY, aZ, aLen, aR)

x3d.OdeAddTriMesh.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeAddTriMesh.restype = ctypes.c_double
def OdeAddTriMesh(aObj, aMesh):
    return x3d.OdeAddTriMesh(aObj, aMesh)

x3d.OdeElementSetDensity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeElementSetDensity.restype = ctypes.c_double
def OdeElementSetDensity(aElement, aDensity):
    return x3d.OdeElementSetDensity(aElement, aDensity)

x3d.OdeSurfaceEnableRollingFrictionCoeff.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceEnableRollingFrictionCoeff.restype = ctypes.c_double
def OdeSurfaceEnableRollingFrictionCoeff(aObj, aMode):
    return x3d.OdeSurfaceEnableRollingFrictionCoeff(aObj, aMode)

x3d.OdeSurfaceSetRollingFrictionCoeff.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetRollingFrictionCoeff.restype = ctypes.c_double
def OdeSurfaceSetRollingFrictionCoeff(aObj, aRfc):
    return x3d.OdeSurfaceSetRollingFrictionCoeff(aObj, aRfc)

x3d.OdeSurfaceSetMode.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetMode.restype = ctypes.c_double
def OdeSurfaceSetMode(aObj, aMu2, aFdir1, aBounce, aSofterp, aSoftcfm, aMotion1, aMotion2, aSlip1, aSlip2):
    return x3d.OdeSurfaceSetMode(aObj, aMu2, aFdir1, aBounce, aSofterp, aSoftcfm, aMotion1, aMotion2, aSlip1, aSlip2)

x3d.OdeSurfaceSetMu.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetMu.restype = ctypes.c_double
def OdeSurfaceSetMu(aObj, aMu):
    return x3d.OdeSurfaceSetMu(aObj, aMu)

x3d.OdeSurfaceSetMu2.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetMu2.restype = ctypes.c_double
def OdeSurfaceSetMu2(aObj, aMu2):
    return x3d.OdeSurfaceSetMu2(aObj, aMu2)

x3d.OdeSurfaceSetBounce.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetBounce.restype = ctypes.c_double
def OdeSurfaceSetBounce(aObj, aBounce):
    return x3d.OdeSurfaceSetBounce(aObj, aBounce)

x3d.OdeSurfaceSetBounceVel.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetBounceVel.restype = ctypes.c_double
def OdeSurfaceSetBounceVel(aObj, aVel):
    return x3d.OdeSurfaceSetBounceVel(aObj, aVel)

x3d.OdeSurfaceSetSoftERP.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetSoftERP.restype = ctypes.c_double
def OdeSurfaceSetSoftERP(aObj, aErp):
    return x3d.OdeSurfaceSetSoftERP(aObj, aErp)

x3d.OdeSurfaceSetSoftCFM.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetSoftCFM.restype = ctypes.c_double
def OdeSurfaceSetSoftCFM(aObj, aCfm):
    return x3d.OdeSurfaceSetSoftCFM(aObj, aCfm)

x3d.OdeSurfaceSetMotion1.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetMotion1.restype = ctypes.c_double
def OdeSurfaceSetMotion1(aObj, aMotion1):
    return x3d.OdeSurfaceSetMotion1(aObj, aMotion1)

x3d.OdeSurfaceSetMotion2.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetMotion2.restype = ctypes.c_double
def OdeSurfaceSetMotion2(aObj, aMotion2):
    return x3d.OdeSurfaceSetMotion2(aObj, aMotion2)

x3d.OdeSurfaceSetSlip1.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetSlip1.restype = ctypes.c_double
def OdeSurfaceSetSlip1(aObj, aSlip1):
    return x3d.OdeSurfaceSetSlip1(aObj, aSlip1)

x3d.OdeSurfaceSetSlip2.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeSurfaceSetSlip2.restype = ctypes.c_double
def OdeSurfaceSetSlip2(aObj, aSlip2):
    return x3d.OdeSurfaceSetSlip2(aObj, aSlip2)

x3d.OdeAddJointBall.argtypes = []
x3d.OdeAddJointBall.restype = ctypes.c_double
def OdeAddJointBall():
    return x3d.OdeAddJointBall()

x3d.OdeAddJointFixed.argtypes = []
x3d.OdeAddJointFixed.restype = ctypes.c_double
def OdeAddJointFixed():
    return x3d.OdeAddJointFixed()

x3d.OdeAddJointHinge.argtypes = []
x3d.OdeAddJointHinge.restype = ctypes.c_double
def OdeAddJointHinge():
    return x3d.OdeAddJointHinge()

x3d.OdeAddJointHinge2.argtypes = []
x3d.OdeAddJointHinge2.restype = ctypes.c_double
def OdeAddJointHinge2():
    return x3d.OdeAddJointHinge2()

x3d.OdeAddJointSlider.argtypes = []
x3d.OdeAddJointSlider.restype = ctypes.c_double
def OdeAddJointSlider():
    return x3d.OdeAddJointSlider()

x3d.OdeAddJointUniversal.argtypes = []
x3d.OdeAddJointUniversal.restype = ctypes.c_double
def OdeAddJointUniversal():
    return x3d.OdeAddJointUniversal()

x3d.OdeJointSetObjects.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetObjects.restype = ctypes.c_double
def OdeJointSetObjects(aJoint, aObj1, aObj2):
    return x3d.OdeJointSetObjects(aJoint, aObj1, aObj2)

x3d.OdeJointEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeJointEnable.restype = ctypes.c_double
def OdeJointEnable(aJoint, aMode):
    return x3d.OdeJointEnable(aJoint, aMode)

x3d.OdeJointInitialize.argtypes = [ctypes.c_double]
x3d.OdeJointInitialize.restype = ctypes.c_double
def OdeJointInitialize(aJoint):
    return x3d.OdeJointInitialize(aJoint)

x3d.OdeJointSetAnchor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetAnchor.restype = ctypes.c_double
def OdeJointSetAnchor(aJoint, aX, aY, aZ):
    return x3d.OdeJointSetAnchor(aJoint, aX, aY, aZ)

x3d.OdeJointSetAnchorAtObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetAnchorAtObject.restype = ctypes.c_double
def OdeJointSetAnchorAtObject(aJoint, aObj):
    return x3d.OdeJointSetAnchorAtObject(aJoint, aObj)

x3d.OdeJointSetAxis1.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetAxis1.restype = ctypes.c_double
def OdeJointSetAxis1(aJoint, aX, aY, aZ):
    return x3d.OdeJointSetAxis1(aJoint, aX, aY, aZ)

x3d.OdeJointSetAxis2.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetAxis2.restype = ctypes.c_double
def OdeJointSetAxis2(aJoint, aX, aY, aZ):
    return x3d.OdeJointSetAxis2(aJoint, aX, aY, aZ)

x3d.OdeJointSetBounce.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetBounce.restype = ctypes.c_double
def OdeJointSetBounce(aJoint, aAxis, aBounce):
    return x3d.OdeJointSetBounce(aJoint, aAxis, aBounce)

x3d.OdeJointSetCFM.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetCFM.restype = ctypes.c_double
def OdeJointSetCFM(aJoint, aAxis, aCfm):
    return x3d.OdeJointSetCFM(aJoint, aAxis, aCfm)

x3d.OdeJointSetFMax.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetFMax.restype = ctypes.c_double
def OdeJointSetFMax(aJoint, aAxis, aFmax):
    return x3d.OdeJointSetFMax(aJoint, aAxis, aFmax)

x3d.OdeJointSetFudgeFactor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetFudgeFactor.restype = ctypes.c_double
def OdeJointSetFudgeFactor(aJoint, aAxis, aFfactor):
    return x3d.OdeJointSetFudgeFactor(aJoint, aAxis, aFfactor)

x3d.OdeJointSetHiStop.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetHiStop.restype = ctypes.c_double
def OdeJointSetHiStop(aJoint, aAxis, aHistop):
    return x3d.OdeJointSetHiStop(aJoint, aAxis, aHistop)

x3d.OdeJointSetLoStop.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetLoStop.restype = ctypes.c_double
def OdeJointSetLoStop(aJoint, aAxis, aLostop):
    return x3d.OdeJointSetLoStop(aJoint, aAxis, aLostop)

x3d.OdeJointSetStopCFM.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetStopCFM.restype = ctypes.c_double
def OdeJointSetStopCFM(aJoint, aAxis, aCfm):
    return x3d.OdeJointSetStopCFM(aJoint, aAxis, aCfm)

x3d.OdeJointSetStopERP.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetStopERP.restype = ctypes.c_double
def OdeJointSetStopERP(aJoint, aAxis, aErp):
    return x3d.OdeJointSetStopERP(aJoint, aAxis, aErp)

x3d.OdeJointSetVel.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeJointSetVel.restype = ctypes.c_double
def OdeJointSetVel(aJoint, aAxis, aVelocity):
    return x3d.OdeJointSetVel(aJoint, aAxis, aVelocity)

x3d.OdeRagdollCreate.argtypes = [ctypes.c_double]
x3d.OdeRagdollCreate.restype = ctypes.c_double
def OdeRagdollCreate(aActor):
    return x3d.OdeRagdollCreate(aActor)

x3d.OdeRagdollHingeJointCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeRagdollHingeJointCreate.restype = ctypes.c_double
def OdeRagdollHingeJointCreate(aX, aY, aZ, aLostop, aHistop):
    return x3d.OdeRagdollHingeJointCreate(aX, aY, aZ, aLostop, aHistop)

x3d.OdeRagdollUniversalJointCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeRagdollUniversalJointCreate.restype = ctypes.c_double
def OdeRagdollUniversalJointCreate(aX1, aY1, aZ1, aLostop1, aHistop1, aX2, aY2, aZ2, aLostop2, aHistop2):
    return x3d.OdeRagdollUniversalJointCreate(aX1, aY1, aZ1, aLostop1, aHistop1, aX2, aY2, aZ2, aLostop2, aHistop2)

x3d.OdeRagdollDummyJointCreate.argtypes = []
x3d.OdeRagdollDummyJointCreate.restype = ctypes.c_double
def OdeRagdollDummyJointCreate():
    return x3d.OdeRagdollDummyJointCreate()

x3d.OdeRagdollBoneCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeRagdollBoneCreate.restype = ctypes.c_double
def OdeRagdollBoneCreate(aRag, aRagjoint, aBoneid, aParentbone):
    return x3d.OdeRagdollBoneCreate(aRag, aRagjoint, aBoneid, aParentbone)

x3d.OdeRagdollBuild.argtypes = [ctypes.c_double]
x3d.OdeRagdollBuild.restype = ctypes.c_double
def OdeRagdollBuild(aRag):
    return x3d.OdeRagdollBuild(aRag)

x3d.OdeRagdollEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeRagdollEnable.restype = ctypes.c_double
def OdeRagdollEnable(aRag, aMode):
    return x3d.OdeRagdollEnable(aRag, aMode)

x3d.OdeRagdollUpdate.argtypes = [ctypes.c_double]
x3d.OdeRagdollUpdate.restype = ctypes.c_double
def OdeRagdollUpdate(aRag):
    return x3d.OdeRagdollUpdate(aRag)

x3d.OdeDynamicSetVelocity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetVelocity.restype = ctypes.c_double
def OdeDynamicSetVelocity(aObj, aX, aY, aZ):
    return x3d.OdeDynamicSetVelocity(aObj, aX, aY, aZ)

x3d.OdeDynamicSetAngularVelocity.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetAngularVelocity.restype = ctypes.c_double
def OdeDynamicSetAngularVelocity(aObj, aX, aY, aZ):
    return x3d.OdeDynamicSetAngularVelocity(aObj, aX, aY, aZ)

x3d.OdeDynamicGetVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicGetVelocity.restype = ctypes.c_double
def OdeDynamicGetVelocity(aObj, aInd):
    return x3d.OdeDynamicGetVelocity(aObj, aInd)

x3d.OdeDynamicGetAngularVelocity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicGetAngularVelocity.restype = ctypes.c_double
def OdeDynamicGetAngularVelocity(aObj, aInd):
    return x3d.OdeDynamicGetAngularVelocity(aObj, aInd)

x3d.OdeDynamicSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetPosition.restype = ctypes.c_double
def OdeDynamicSetPosition(aObj, aX, aY, aZ):
    return x3d.OdeDynamicSetPosition(aObj, aX, aY, aZ)

x3d.OdeDynamicSetRotationQuaternion.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OdeDynamicSetRotationQuaternion.restype = ctypes.c_double
def OdeDynamicSetRotationQuaternion(aObj, aX, aY, aZ, aW):
    return x3d.OdeDynamicSetRotationQuaternion(aObj, aX, aY, aZ, aW)


# pak.pas
x3d.SetPakArchive.argtypes = [ctypes.c_char_p]
x3d.SetPakArchive.restype = ctypes.c_double
def SetPakArchive(aFname):
    return x3d.SetPakArchive(aFname)

x3d.PakGetFileCount.argtypes = [ctypes.c_double]
x3d.PakGetFileCount.restype = ctypes.c_double
def PakGetFileCount(aP):
    return x3d.PakGetFileCount(aP)

x3d.PakGetFileName.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PakGetFileName.restype = ctypes.c_char_p
def PakGetFileName(aP, aIndex):
    return x3d.PakGetFileName(aP, aIndex)

x3d.PakExtract.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.PakExtract.restype = ctypes.c_double
def PakExtract(aP, aDir):
    return x3d.PakExtract(aP, aDir)

x3d.PakExtractFile.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.PakExtractFile.restype = ctypes.c_double
def PakExtractFile(aP, aIndex, aNewname):
    return x3d.PakExtractFile(aP, aIndex, aNewname)


# partition.pas
x3d.OctreeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.OctreeCreate.restype = ctypes.c_double
def OctreeCreate(aMaxdepth, aLeafthreshold, aGrowgravy, aCulling):
    return x3d.OctreeCreate(aMaxdepth, aLeafthreshold, aGrowgravy, aCulling)

x3d.QuadtreeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.QuadtreeCreate.restype = ctypes.c_double
def QuadtreeCreate(aMaxdepth, aLeafthreshold, aGrowgravy, aCulling):
    return x3d.QuadtreeCreate(aMaxdepth, aLeafthreshold, aGrowgravy, aCulling)

x3d.PartitionDestroy.argtypes = [ctypes.c_double]
x3d.PartitionDestroy.restype = ctypes.c_double
def PartitionDestroy(aTree):
    return x3d.PartitionDestroy(aTree)

x3d.PartitionAddLeaf.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PartitionAddLeaf.restype = ctypes.c_double
def PartitionAddLeaf(aTree, aObj):
    return x3d.PartitionAddLeaf(aTree, aObj)

x3d.PartitionLeafChanged.argtypes = [ctypes.c_double]
x3d.PartitionLeafChanged.restype = ctypes.c_double
def PartitionLeafChanged(aLeaf):
    return x3d.PartitionLeafChanged(aLeaf)

x3d.PartitionQueryFrustum.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PartitionQueryFrustum.restype = ctypes.c_double
def PartitionQueryFrustum(aTree, aViewer):
    return x3d.PartitionQueryFrustum(aTree, aViewer)

x3d.PartitionQueryLeaf.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PartitionQueryLeaf.restype = ctypes.c_double
def PartitionQueryLeaf(aTree, aLeaf):
    return x3d.PartitionQueryLeaf(aTree, aLeaf)

x3d.PartitionQueryAABB.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PartitionQueryAABB.restype = ctypes.c_double
def PartitionQueryAABB(aTree, aObj):
    return x3d.PartitionQueryAABB(aTree, aObj)

x3d.PartitionQueryBSphere.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PartitionQueryBSphere.restype = ctypes.c_double
def PartitionQueryBSphere(aTree, aObj):
    return x3d.PartitionQueryBSphere(aTree, aObj)

x3d.PartitionGetNodeTests.argtypes = [ctypes.c_double]
x3d.PartitionGetNodeTests.restype = ctypes.c_double
def PartitionGetNodeTests(aTree):
    return x3d.PartitionGetNodeTests(aTree)

x3d.PartitionGetNodeCount.argtypes = [ctypes.c_double]
x3d.PartitionGetNodeCount.restype = ctypes.c_double
def PartitionGetNodeCount(aTree):
    return x3d.PartitionGetNodeCount(aTree)

x3d.PartitionGetResult.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PartitionGetResult.restype = ctypes.c_double
def PartitionGetResult(aTree, aInd):
    return x3d.PartitionGetResult(aTree, aInd)

x3d.PartitionGetResultCount.argtypes = [ctypes.c_double]
x3d.PartitionGetResultCount.restype = ctypes.c_double
def PartitionGetResultCount(aTree):
    return x3d.PartitionGetResultCount(aTree)

x3d.PartitionResultShow.argtypes = [ctypes.c_double]
x3d.PartitionResultShow.restype = ctypes.c_double
def PartitionResultShow(aTree):
    return x3d.PartitionResultShow(aTree)

x3d.PartitionResultHide.argtypes = [ctypes.c_double]
x3d.PartitionResultHide.restype = ctypes.c_double
def PartitionResultHide(aTree):
    return x3d.PartitionResultHide(aTree)


# picklist.pas
x3d.PickListCreate.argtypes = [ctypes.c_double]
x3d.PickListCreate.restype = ctypes.c_double
def PickListCreate(aPs):
    return x3d.PickListCreate(aPs)

x3d.PickListClear.argtypes = [ctypes.c_double]
x3d.PickListClear.restype = ctypes.c_double
def PickListClear(aList):
    return x3d.PickListClear(aList)

x3d.PickListGetCount.argtypes = [ctypes.c_double]
x3d.PickListGetCount.restype = ctypes.c_double
def PickListGetCount(aList):
    return x3d.PickListGetCount(aList)

x3d.PickListGetHit.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PickListGetHit.restype = ctypes.c_double
def PickListGetHit(aList, aIndex):
    return x3d.PickListGetHit(aList, aIndex)


# pipe.pas
x3d.PipeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.PipeCreate.restype = ctypes.c_double
def PipeCreate(aDivs, aSlic, aParent):
    return x3d.PipeCreate(aDivs, aSlic, aParent)

x3d.PipeAddNode.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.PipeAddNode.restype = ctypes.c_double
def PipeAddNode(aPipe, aX, aY, aZ):
    return x3d.PipeAddNode(aPipe, aX, aY, aZ)

x3d.PipeSetDivision.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PipeSetDivision.restype = ctypes.c_double
def PipeSetDivision(aPipe, aDivs):
    return x3d.PipeSetDivision(aPipe, aDivs)

x3d.PipeSetSplineMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PipeSetSplineMode.restype = ctypes.c_double
def PipeSetSplineMode(aPipe, aMode):
    return x3d.PipeSetSplineMode(aPipe, aMode)

x3d.PipeDeleteNode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PipeDeleteNode.restype = ctypes.c_double
def PipeDeleteNode(aPipe, aInd):
    return x3d.PipeDeleteNode(aPipe, aInd)

x3d.PipeSetRadius.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PipeSetRadius.restype = ctypes.c_double
def PipeSetRadius(aPipe, aRad):
    return x3d.PipeSetRadius(aPipe, aRad)

x3d.PipeSetNode.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.PipeSetNode.restype = ctypes.c_double
def PipeSetNode(aPipe, aInd, aX, aY, aZ):
    return x3d.PipeSetNode(aPipe, aInd, aX, aY, aZ)

x3d.PipeSetSlices.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PipeSetSlices.restype = ctypes.c_double
def PipeSetSlices(aPipe, aSlic):
    return x3d.PipeSetSlices(aPipe, aSlic)


# primitives.pas
x3d.CubeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CubeCreate.restype = ctypes.c_double
def CubeCreate(aW, aH, aD, aParent):
    return x3d.CubeCreate(aW, aH, aD, aParent)

x3d.CubeSetNormalDirection.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CubeSetNormalDirection.restype = ctypes.c_double
def CubeSetNormalDirection(aCube, aNd):
    return x3d.CubeSetNormalDirection(aCube, aNd)

x3d.CubeGetNormalDirection.argtypes = [ctypes.c_double]
x3d.CubeGetNormalDirection.restype = ctypes.c_double
def CubeGetNormalDirection(aCube):
    return x3d.CubeGetNormalDirection(aCube)

x3d.PlaneCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.PlaneCreate.restype = ctypes.c_double
def PlaneCreate(aSquad, aW, aH, aXt, aYt, aParent):
    return x3d.PlaneCreate(aSquad, aW, aH, aXt, aYt, aParent)

x3d.PlaneSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.PlaneSetOptions.restype = ctypes.c_double
def PlaneSetOptions(aPlane, aSquad, aXt, aYt):
    return x3d.PlaneSetOptions(aPlane, aSquad, aXt, aYt)

x3d.PlaneGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PlaneGetOptions.restype = ctypes.c_double
def PlaneGetOptions(aPlane, aIndex):
    return x3d.PlaneGetOptions(aPlane, aIndex)

x3d.TilePlaneCreate.argtypes = [ctypes.c_double]
x3d.TilePlaneCreate.restype = ctypes.c_double
def TilePlaneCreate(aParent):
    return x3d.TilePlaneCreate(aParent)

x3d.TilePlaneSetTile.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.TilePlaneSetTile.restype = ctypes.c_double
def TilePlaneSetTile(aTplane, aX, aY, aMat):
    return x3d.TilePlaneSetTile(aTplane, aX, aY, aMat)

x3d.SphereCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SphereCreate.restype = ctypes.c_double
def SphereCreate(aRad, aSlic, aStaks, aParent):
    return x3d.SphereCreate(aRad, aSlic, aStaks, aParent)

x3d.SphereSetAngleLimits.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SphereSetAngleLimits.restype = ctypes.c_double
def SphereSetAngleLimits(aSphere, aStarta, aStopa, aTopa, aBottoma):
    return x3d.SphereSetAngleLimits(aSphere, aStarta, aStopa, aTopa, aBottoma)

x3d.SphereGetAngleLimits.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SphereGetAngleLimits.restype = ctypes.c_double
def SphereGetAngleLimits(aSphere, aIndex):
    return x3d.SphereGetAngleLimits(aSphere, aIndex)

x3d.SphereSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SphereSetOptions.restype = ctypes.c_double
def SphereSetOptions(aSphere, aRad, aSlic, aStaks):
    return x3d.SphereSetOptions(aSphere, aRad, aSlic, aStaks)

x3d.SphereGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SphereGetOptions.restype = ctypes.c_double
def SphereGetOptions(aSph, aInd):
    return x3d.SphereGetOptions(aSph, aInd)

x3d.CylinderCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CylinderCreate.restype = ctypes.c_double
def CylinderCreate(aTopr, aBotr, aH, aSlic, aStaks, aLoop, aParent):
    return x3d.CylinderCreate(aTopr, aBotr, aH, aSlic, aStaks, aLoop, aParent)

x3d.CylinderSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CylinderSetOptions.restype = ctypes.c_double
def CylinderSetOptions(aCyl, aTopr, aBotr, aH, aSlic, aStaks, aLoop):
    return x3d.CylinderSetOptions(aCyl, aTopr, aBotr, aH, aSlic, aStaks, aLoop)

x3d.CylinderGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CylinderGetOptions.restype = ctypes.c_double
def CylinderGetOptions(aCyl, aInd):
    return x3d.CylinderGetOptions(aCyl, aInd)

x3d.ConeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ConeCreate.restype = ctypes.c_double
def ConeCreate(aBotr, aH, aSlic, aStaks, aLoop, aParent):
    return x3d.ConeCreate(aBotr, aH, aSlic, aStaks, aLoop, aParent)

x3d.ConeGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ConeGetOptions.restype = ctypes.c_double
def ConeGetOptions(aCone, aInd):
    return x3d.ConeGetOptions(aCone, aInd)

x3d.ConeSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ConeSetOptions.restype = ctypes.c_double
def ConeSetOptions(aCone, aBotr, aH, aSlic, aStaks, aLoop):
    return x3d.ConeSetOptions(aCone, aBotr, aH, aSlic, aStaks, aLoop)

x3d.AnnulusCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.AnnulusCreate.restype = ctypes.c_double
def AnnulusCreate(aInr, aOutr, aH, aSlic, aStaks, aLoop, aParent):
    return x3d.AnnulusCreate(aInr, aOutr, aH, aSlic, aStaks, aLoop, aParent)

x3d.AnnulusSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.AnnulusSetOptions.restype = ctypes.c_double
def AnnulusSetOptions(aAn, aInr, aOutr, aH, aSlic, aStaks, aLoop):
    return x3d.AnnulusSetOptions(aAn, aInr, aOutr, aH, aSlic, aStaks, aLoop)

x3d.AnnulusGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AnnulusGetOptions.restype = ctypes.c_double
def AnnulusGetOptions(aAn, aInd):
    return x3d.AnnulusGetOptions(aAn, aInd)

x3d.TorusCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TorusCreate.restype = ctypes.c_double
def TorusCreate(aInr, aOutr, aRing, aSide, aParent):
    return x3d.TorusCreate(aInr, aOutr, aRing, aSide, aParent)

x3d.TorusSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TorusSetOptions.restype = ctypes.c_double
def TorusSetOptions(aTor, aInr, aOutr, aRing, aSide):
    return x3d.TorusSetOptions(aTor, aInr, aOutr, aRing, aSide)

x3d.TorusGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TorusGetOptions.restype = ctypes.c_double
def TorusGetOptions(aTor, aInd):
    return x3d.TorusGetOptions(aTor, aInd)

x3d.DiskCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DiskCreate.restype = ctypes.c_double
def DiskCreate(aInr, aOutr, aStarta, aSweepa, aLoop, aSlic, aParent):
    return x3d.DiskCreate(aInr, aOutr, aStarta, aSweepa, aLoop, aSlic, aParent)

x3d.DiskSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.DiskSetOptions.restype = ctypes.c_double
def DiskSetOptions(aDisk, aInr, aOutr, aStarta, aSweepa, aLoop, aSlic):
    return x3d.DiskSetOptions(aDisk, aInr, aOutr, aStarta, aSweepa, aLoop, aSlic)

x3d.DiskGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.DiskGetOptions.restype = ctypes.c_double
def DiskGetOptions(aDisk, aInd):
    return x3d.DiskGetOptions(aDisk, aInd)

x3d.FrustrumCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FrustrumCreate.restype = ctypes.c_double
def FrustrumCreate(aBasew, aBased, aApexh, aCuth, aParent):
    return x3d.FrustrumCreate(aBasew, aBased, aApexh, aCuth, aParent)

x3d.FrustrumSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.FrustrumSetOptions.restype = ctypes.c_double
def FrustrumSetOptions(aFr, aBasew, aBased, aApexh, aCuth):
    return x3d.FrustrumSetOptions(aFr, aBasew, aBased, aApexh, aCuth)

x3d.FrustrumGetOptions.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.FrustrumGetOptions.restype = ctypes.c_double
def FrustrumGetOptions(aFr, aInd):
    return x3d.FrustrumGetOptions(aFr, aInd)

x3d.DodecahedronCreate.argtypes = [ctypes.c_double]
x3d.DodecahedronCreate.restype = ctypes.c_double
def DodecahedronCreate(aParent):
    return x3d.DodecahedronCreate(aParent)

x3d.IcosahedronCreate.argtypes = [ctypes.c_double]
x3d.IcosahedronCreate.restype = ctypes.c_double
def IcosahedronCreate(aParent):
    return x3d.IcosahedronCreate(aParent)

x3d.TeapotCreate.argtypes = [ctypes.c_double]
x3d.TeapotCreate.restype = ctypes.c_double
def TeapotCreate(aParent):
    return x3d.TeapotCreate(aParent)


# proxy.pas
x3d.ProxyObjectCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ProxyObjectCreate.restype = ctypes.c_double
def ProxyObjectCreate(aTarget, aParent):
    return x3d.ProxyObjectCreate(aTarget, aParent)

x3d.ProxyObjectSetTarget.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ProxyObjectSetTarget.restype = ctypes.c_double
def ProxyObjectSetTarget(aProxy, aTarget):
    return x3d.ProxyObjectSetTarget(aProxy, aTarget)

x3d.MultiProxyObjectCreate.argtypes = [ctypes.c_double]
x3d.MultiProxyObjectCreate.restype = ctypes.c_double
def MultiProxyObjectCreate(aParent):
    return x3d.MultiProxyObjectCreate(aParent)

x3d.MultiProxyObjectAddTarget.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.MultiProxyObjectAddTarget.restype = ctypes.c_double
def MultiProxyObjectAddTarget(aMproxy, aTarget, aMindist, aMaxdist):
    return x3d.MultiProxyObjectAddTarget(aMproxy, aTarget, aMindist, aMaxdist)

x3d.ActorProxyObjectCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorProxyObjectCreate.restype = ctypes.c_double
def ActorProxyObjectCreate(aActor, aParent):
    return x3d.ActorProxyObjectCreate(aActor, aParent)

x3d.ActorProxyObjectSwitchToAnimation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorProxyObjectSwitchToAnimation.restype = ctypes.c_double
def ActorProxyObjectSwitchToAnimation(aProxy, aAnim):
    return x3d.ActorProxyObjectSwitchToAnimation(aProxy, aAnim)

x3d.ActorProxyObjectSetAnimationRange.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ActorProxyObjectSetAnimationRange.restype = ctypes.c_double
def ActorProxyObjectSetAnimationRange(aProxy, aStartf, aEndf):
    return x3d.ActorProxyObjectSetAnimationRange(aProxy, aStartf, aEndf)

x3d.ActorProxyObjectSetInterval.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ActorProxyObjectSetInterval.restype = ctypes.c_double
def ActorProxyObjectSetInterval(aProxy, aInterval):
    return x3d.ActorProxyObjectSetInterval(aProxy, aInterval)


# shaders.pas
x3d.ShaderEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShaderEnable.restype = ctypes.c_double
def ShaderEnable(aShader, aMode):
    return x3d.ShaderEnable(aShader, aMode)

x3d.CelShaderCreate.argtypes = []
x3d.CelShaderCreate.restype = ctypes.c_double
def CelShaderCreate():
    return x3d.CelShaderCreate()

x3d.CelShaderSetLineColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CelShaderSetLineColor.restype = ctypes.c_double
def CelShaderSetLineColor(aShader, aCol):
    return x3d.CelShaderSetLineColor(aShader, aCol)

x3d.CelShaderSetLineWidth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.CelShaderSetLineWidth.restype = ctypes.c_double
def CelShaderSetLineWidth(aShader, aWidth):
    return x3d.CelShaderSetLineWidth(aShader, aWidth)

x3d.CelShaderSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.CelShaderSetOptions.restype = ctypes.c_double
def CelShaderSetOptions(aShader, aOutlines, aTextured):
    return x3d.CelShaderSetOptions(aShader, aOutlines, aTextured)

x3d.MultiMaterialShaderCreate.argtypes = [ctypes.c_double]
x3d.MultiMaterialShaderCreate.restype = ctypes.c_double
def MultiMaterialShaderCreate(aMatlib):
    return x3d.MultiMaterialShaderCreate(aMatlib)

x3d.HiddenLineShaderCreate.argtypes = []
x3d.HiddenLineShaderCreate.restype = ctypes.c_double
def HiddenLineShaderCreate():
    return x3d.HiddenLineShaderCreate()

x3d.HiddenLineShaderSetLineSmooth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HiddenLineShaderSetLineSmooth.restype = ctypes.c_double
def HiddenLineShaderSetLineSmooth(aShader, aMode):
    return x3d.HiddenLineShaderSetLineSmooth(aShader, aMode)

x3d.HiddenLineShaderSetSolid.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HiddenLineShaderSetSolid.restype = ctypes.c_double
def HiddenLineShaderSetSolid(aShader, aMode):
    return x3d.HiddenLineShaderSetSolid(aShader, aMode)

x3d.HiddenLineShaderSetSurfaceLit.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HiddenLineShaderSetSurfaceLit.restype = ctypes.c_double
def HiddenLineShaderSetSurfaceLit(aShader, aMode):
    return x3d.HiddenLineShaderSetSurfaceLit(aShader, aMode)

x3d.HiddenLineShaderSetFrontLine.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HiddenLineShaderSetFrontLine.restype = ctypes.c_double
def HiddenLineShaderSetFrontLine(aShader, aWidth, aCol, aPattern, aForcemat):
    return x3d.HiddenLineShaderSetFrontLine(aShader, aWidth, aCol, aPattern, aForcemat)

x3d.HiddenLineShaderSetBackLine.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HiddenLineShaderSetBackLine.restype = ctypes.c_double
def HiddenLineShaderSetBackLine(aShader, aWidth, aCol, aPattern, aForcemat):
    return x3d.HiddenLineShaderSetBackLine(aShader, aWidth, aCol, aPattern, aForcemat)

x3d.OutlineShaderCreate.argtypes = [ctypes.c_double]
x3d.OutlineShaderCreate.restype = ctypes.c_double
def OutlineShaderCreate(aSmooth):
    return x3d.OutlineShaderCreate(aSmooth)

x3d.OutlineShaderSetLineColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OutlineShaderSetLineColor.restype = ctypes.c_double
def OutlineShaderSetLineColor(aShader, aCol):
    return x3d.OutlineShaderSetLineColor(aShader, aCol)

x3d.OutlineShaderSetLineWidth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.OutlineShaderSetLineWidth.restype = ctypes.c_double
def OutlineShaderSetLineWidth(aShader, aWidth):
    return x3d.OutlineShaderSetLineWidth(aShader, aWidth)

x3d.TexCombineShaderCreate.argtypes = [ctypes.c_double]
x3d.TexCombineShaderCreate.restype = ctypes.c_double
def TexCombineShaderCreate(aMatlib):
    return x3d.TexCombineShaderCreate(aMatlib)

x3d.TexCombineShaderAddCombiner.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.TexCombineShaderAddCombiner.restype = ctypes.c_double
def TexCombineShaderAddCombiner(aTcs, aStr):
    return x3d.TexCombineShaderAddCombiner(aTcs, aStr)

x3d.TexCombineShaderMaterial3.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.TexCombineShaderMaterial3.restype = ctypes.c_double
def TexCombineShaderMaterial3(aTcs, aM3):
    return x3d.TexCombineShaderMaterial3(aTcs, aM3)

x3d.TexCombineShaderMaterial4.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.TexCombineShaderMaterial4.restype = ctypes.c_double
def TexCombineShaderMaterial4(aTcs, aM4):
    return x3d.TexCombineShaderMaterial4(aTcs, aM4)

x3d.GLSLShaderCreate.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
x3d.GLSLShaderCreate.restype = ctypes.c_double
def GLSLShaderCreate(aVp, aFp):
    return x3d.GLSLShaderCreate(aVp, aFp)

x3d.GLSLShaderSetLogger.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetLogger.restype = ctypes.c_double
def GLSLShaderSetLogger(aShader, aLogger):
    return x3d.GLSLShaderSetLogger(aShader, aLogger)

x3d.GLSLShaderCreateParameter.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.GLSLShaderCreateParameter.restype = ctypes.c_double
def GLSLShaderCreateParameter(aGlsl, aName):
    return x3d.GLSLShaderCreateParameter(aGlsl, aName)

x3d.GLSLShaderSetParameter1i.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameter1i.restype = ctypes.c_double
def GLSLShaderSetParameter1i(aPar, aVal):
    return x3d.GLSLShaderSetParameter1i(aPar, aVal)

x3d.GLSLShaderSetParameter1f.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameter1f.restype = ctypes.c_double
def GLSLShaderSetParameter1f(aPar, aX):
    return x3d.GLSLShaderSetParameter1f(aPar, aX)

x3d.GLSLShaderSetParameter2f.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameter2f.restype = ctypes.c_double
def GLSLShaderSetParameter2f(aPar, aX, aY):
    return x3d.GLSLShaderSetParameter2f(aPar, aX, aY)

x3d.GLSLShaderSetParameter3f.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameter3f.restype = ctypes.c_double
def GLSLShaderSetParameter3f(aPar, aX, aY, aZ):
    return x3d.GLSLShaderSetParameter3f(aPar, aX, aY, aZ)

x3d.GLSLShaderSetParameter4f.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameter4f.restype = ctypes.c_double
def GLSLShaderSetParameter4f(aPar, aX, aY, aZ, aW):
    return x3d.GLSLShaderSetParameter4f(aPar, aX, aY, aZ, aW)

x3d.GLSLShaderSetParameterTexture.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double]
x3d.GLSLShaderSetParameterTexture.restype = ctypes.c_double
def GLSLShaderSetParameterTexture(aPar, aMtrl, aTexunit):
    return x3d.GLSLShaderSetParameterTexture(aPar, aMtrl, aTexunit)

x3d.GLSLShaderSetParameterSecondTexture.argtypes = [ctypes.c_double, ctypes.c_char_p, ctypes.c_double]
x3d.GLSLShaderSetParameterSecondTexture.restype = ctypes.c_double
def GLSLShaderSetParameterSecondTexture(aPar, aMtrl, aTexunit):
    return x3d.GLSLShaderSetParameterSecondTexture(aPar, aMtrl, aTexunit)

x3d.GLSLShaderSetParameterShadowTexture.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterShadowTexture.restype = ctypes.c_double
def GLSLShaderSetParameterShadowTexture(aPar, aShadowmap, aTexunit):
    return x3d.GLSLShaderSetParameterShadowTexture(aPar, aShadowmap, aTexunit)

x3d.GLSLShaderSetParameterShadowMatrix.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterShadowMatrix.restype = ctypes.c_double
def GLSLShaderSetParameterShadowMatrix(aPar, aShadowmap):
    return x3d.GLSLShaderSetParameterShadowMatrix(aPar, aShadowmap)

x3d.GLSLShaderSetParameterMatrix.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterMatrix.restype = ctypes.c_double
def GLSLShaderSetParameterMatrix(aPar, aObj):
    return x3d.GLSLShaderSetParameterMatrix(aPar, aObj)

x3d.GLSLShaderSetParameterInvMatrix.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterInvMatrix.restype = ctypes.c_double
def GLSLShaderSetParameterInvMatrix(aPar, aObj):
    return x3d.GLSLShaderSetParameterInvMatrix(aPar, aObj)

x3d.GLSLShaderSetParameterFBOColorTexture.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterFBOColorTexture.restype = ctypes.c_double
def GLSLShaderSetParameterFBOColorTexture(aPar, aFbo, aTexunit):
    return x3d.GLSLShaderSetParameterFBOColorTexture(aPar, aFbo, aTexunit)

x3d.GLSLShaderSetParameterFBODepthTexture.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterFBODepthTexture.restype = ctypes.c_double
def GLSLShaderSetParameterFBODepthTexture(aPar, aFbo, aTexunit):
    return x3d.GLSLShaderSetParameterFBODepthTexture(aPar, aFbo, aTexunit)

x3d.GLSLShaderSetParameterViewMatrix.argtypes = [ctypes.c_double]
x3d.GLSLShaderSetParameterViewMatrix.restype = ctypes.c_double
def GLSLShaderSetParameterViewMatrix(aPar):
    return x3d.GLSLShaderSetParameterViewMatrix(aPar)

x3d.GLSLShaderSetParameterInvViewMatrix.argtypes = [ctypes.c_double]
x3d.GLSLShaderSetParameterInvViewMatrix.restype = ctypes.c_double
def GLSLShaderSetParameterInvViewMatrix(aPar):
    return x3d.GLSLShaderSetParameterInvViewMatrix(aPar)

x3d.GLSLShaderSetParameterHasTextureEx.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetParameterHasTextureEx.restype = ctypes.c_double
def GLSLShaderSetParameterHasTextureEx(aPar, aSlot):
    return x3d.GLSLShaderSetParameterHasTextureEx(aPar, aSlot)

x3d.GLSLShaderForceDisableStencilTest.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderForceDisableStencilTest.restype = ctypes.c_double
def GLSLShaderForceDisableStencilTest(aShader, aMode):
    return x3d.GLSLShaderForceDisableStencilTest(aShader, aMode)

x3d.GLSLShaderSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.GLSLShaderSetOptions.restype = ctypes.c_double
def GLSLShaderSetOptions(aShader, aLightingenabled, aFogenabled):
    return x3d.GLSLShaderSetOptions(aShader, aLightingenabled, aFogenabled)

x3d.PhongShaderCreate.argtypes = []
x3d.PhongShaderCreate.restype = ctypes.c_double
def PhongShaderCreate():
    return x3d.PhongShaderCreate()

x3d.PhongShaderUseTexture.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PhongShaderUseTexture.restype = ctypes.c_double
def PhongShaderUseTexture(aShader, aMode):
    return x3d.PhongShaderUseTexture(aShader, aMode)

x3d.PhongShaderSetMaxLights.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.PhongShaderSetMaxLights.restype = ctypes.c_double
def PhongShaderSetMaxLights(aShader, aMaxlights):
    return x3d.PhongShaderSetMaxLights(aShader, aMaxlights)

x3d.BumpShaderCreate.argtypes = []
x3d.BumpShaderCreate.restype = ctypes.c_double
def BumpShaderCreate():
    return x3d.BumpShaderCreate()

x3d.BumpShaderSetDiffuseTexture.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.BumpShaderSetDiffuseTexture.restype = ctypes.c_double
def BumpShaderSetDiffuseTexture(aShader, aMtrl):
    return x3d.BumpShaderSetDiffuseTexture(aShader, aMtrl)

x3d.BumpShaderSetNormalTexture.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.BumpShaderSetNormalTexture.restype = ctypes.c_double
def BumpShaderSetNormalTexture(aShader, aMtrl):
    return x3d.BumpShaderSetNormalTexture(aShader, aMtrl)

x3d.BumpShaderSetHeightTexture.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.BumpShaderSetHeightTexture.restype = ctypes.c_double
def BumpShaderSetHeightTexture(aShader, aMtrl):
    return x3d.BumpShaderSetHeightTexture(aShader, aMtrl)

x3d.BumpShaderSetMaxLights.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BumpShaderSetMaxLights.restype = ctypes.c_double
def BumpShaderSetMaxLights(aShader, aMaxlights):
    return x3d.BumpShaderSetMaxLights(aShader, aMaxlights)

x3d.BumpShaderUseParallax.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BumpShaderUseParallax.restype = ctypes.c_double
def BumpShaderUseParallax(aShader, aMode):
    return x3d.BumpShaderUseParallax(aShader, aMode)

x3d.BumpShaderSetParallaxOffset.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BumpShaderSetParallaxOffset.restype = ctypes.c_double
def BumpShaderSetParallaxOffset(aShader, aHeight):
    return x3d.BumpShaderSetParallaxOffset(aShader, aHeight)

x3d.BumpShaderSetShadowMap.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BumpShaderSetShadowMap.restype = ctypes.c_double
def BumpShaderSetShadowMap(aShader, aShadowmap):
    return x3d.BumpShaderSetShadowMap(aShader, aShadowmap)

x3d.BumpShaderSetShadowBlurRadius.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BumpShaderSetShadowBlurRadius.restype = ctypes.c_double
def BumpShaderSetShadowBlurRadius(aShader, aRadius):
    return x3d.BumpShaderSetShadowBlurRadius(aShader, aRadius)

x3d.BumpShaderUseAutoTangentSpace.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BumpShaderUseAutoTangentSpace.restype = ctypes.c_double
def BumpShaderUseAutoTangentSpace(aShader, aMode):
    return x3d.BumpShaderUseAutoTangentSpace(aShader, aMode)


# shadowmap.pas
x3d.ShadowMapCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ShadowMapCreate.restype = ctypes.c_double
def ShadowMapCreate(aFbo, aViewer, aShadowcamera):
    return x3d.ShadowMapCreate(aFbo, aViewer, aShadowcamera)

x3d.ShadowMapUpdate.argtypes = [ctypes.c_double]
x3d.ShadowMapUpdate.restype = ctypes.c_double
def ShadowMapUpdate(aShadowmap):
    return x3d.ShadowMapUpdate(aShadowmap)

x3d.ShadowMapSetCamera.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowMapSetCamera.restype = ctypes.c_double
def ShadowMapSetCamera(aSm, aSc):
    return x3d.ShadowMapSetCamera(aSm, aSc)

x3d.ShadowMapSetViewer.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowMapSetViewer.restype = ctypes.c_double
def ShadowMapSetViewer(aSm, aV):
    return x3d.ShadowMapSetViewer(aSm, aV)

x3d.ShadowMapSetFBO.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowMapSetFBO.restype = ctypes.c_double
def ShadowMapSetFBO(aSm, aFbo):
    return x3d.ShadowMapSetFBO(aSm, aFbo)

x3d.ShadowCameraCreate.argtypes = [ctypes.c_double]
x3d.ShadowCameraCreate.restype = ctypes.c_double
def ShadowCameraCreate(aParent):
    return x3d.ShadowCameraCreate(aParent)

x3d.ShadowCameraSetProjectionSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowCameraSetProjectionSize.restype = ctypes.c_double
def ShadowCameraSetProjectionSize(aSc, aSize):
    return x3d.ShadowCameraSetProjectionSize(aSc, aSize)

x3d.ShadowCameraSetZClippingPlanes.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ShadowCameraSetZClippingPlanes.restype = ctypes.c_double
def ShadowCameraSetZClippingPlanes(aSc, aZnear, aZfar):
    return x3d.ShadowCameraSetZClippingPlanes(aSc, aZnear, aZfar)


# shadowplane.pas
x3d.ShadowplaneCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ShadowplaneCreate.restype = ctypes.c_double
def ShadowplaneCreate(aWidth, aHeight, aXtiles, aYtiles, aTarget, aLight, aColor, aAlpha, aParent):
    return x3d.ShadowplaneCreate(aWidth, aHeight, aXtiles, aYtiles, aTarget, aLight, aColor, aAlpha, aParent)

x3d.ShadowplaneSetLight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowplaneSetLight.restype = ctypes.c_double
def ShadowplaneSetLight(aShadowplane, aLight):
    return x3d.ShadowplaneSetLight(aShadowplane, aLight)

x3d.ShadowplaneSetObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowplaneSetObject.restype = ctypes.c_double
def ShadowplaneSetObject(aShadowplane, aTarget):
    return x3d.ShadowplaneSetObject(aShadowplane, aTarget)

x3d.ShadowplaneSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ShadowplaneSetOptions.restype = ctypes.c_double
def ShadowplaneSetOptions(aShadowplane, aStencil, aScissor, aTransparent, aIgnorez):
    return x3d.ShadowplaneSetOptions(aShadowplane, aStencil, aScissor, aTransparent, aIgnorez)


# shadowvolume.pas
x3d.ShadowvolumeCreate.argtypes = [ctypes.c_double]
x3d.ShadowvolumeCreate.restype = ctypes.c_double
def ShadowvolumeCreate(aParent):
    return x3d.ShadowvolumeCreate(aParent)

x3d.ShadowvolumeSetActive.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeSetActive.restype = ctypes.c_double
def ShadowvolumeSetActive(aShadowvolume, aActive):
    return x3d.ShadowvolumeSetActive(aShadowvolume, aActive)

x3d.ShadowvolumeAddLight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeAddLight.restype = ctypes.c_double
def ShadowvolumeAddLight(aShadowvolume, aLight):
    return x3d.ShadowvolumeAddLight(aShadowvolume, aLight)

x3d.ShadowvolumeRemoveLight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeRemoveLight.restype = ctypes.c_double
def ShadowvolumeRemoveLight(aShadowvolume, aLight):
    return x3d.ShadowvolumeRemoveLight(aShadowvolume, aLight)

x3d.ShadowvolumeAddOccluder.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeAddOccluder.restype = ctypes.c_double
def ShadowvolumeAddOccluder(aShadowvolume, aObj):
    return x3d.ShadowvolumeAddOccluder(aShadowvolume, aObj)

x3d.ShadowvolumeRemoveOccluder.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeRemoveOccluder.restype = ctypes.c_double
def ShadowvolumeRemoveOccluder(aShadowvolume, aObj):
    return x3d.ShadowvolumeRemoveOccluder(aShadowvolume, aObj)

x3d.ShadowvolumeSetDarkeningColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeSetDarkeningColor.restype = ctypes.c_double
def ShadowvolumeSetDarkeningColor(aShadowvolume, aColor, aAlpha):
    return x3d.ShadowvolumeSetDarkeningColor(aShadowvolume, aColor, aAlpha)

x3d.ShadowvolumeSetMode.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ShadowvolumeSetMode.restype = ctypes.c_double
def ShadowvolumeSetMode(aShadowvolume, aSvm):
    return x3d.ShadowvolumeSetMode(aShadowvolume, aSvm)


# skybox.pas
x3d.SkyboxCreate.argtypes = [ctypes.c_double]
x3d.SkyboxCreate.restype = ctypes.c_double
def SkyboxCreate(aParent):
    return x3d.SkyboxCreate(aParent)

x3d.SkyboxSetMaterial.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_char_p]
x3d.SkyboxSetMaterial.restype = ctypes.c_double
def SkyboxSetMaterial(aSkybox, aSbm, aMatname):
    return x3d.SkyboxSetMaterial(aSkybox, aSbm, aMatname)

x3d.SkyboxSetClouds.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SkyboxSetClouds.restype = ctypes.c_double
def SkyboxSetClouds(aSkybox, aOffset, aSize):
    return x3d.SkyboxSetClouds(aSkybox, aOffset, aSize)

x3d.SkyboxSetStyle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkyboxSetStyle.restype = ctypes.c_double
def SkyboxSetStyle(aSkybox, aSbs):
    return x3d.SkyboxSetStyle(aSkybox, aSbs)


# skydome.pas
x3d.SkydomeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SkydomeCreate.restype = ctypes.c_double
def SkydomeCreate(aSlices, aStacks, aParent):
    return x3d.SkydomeCreate(aSlices, aStacks, aParent)

x3d.SkydomeSetOptions.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetOptions.restype = ctypes.c_double
def SkydomeSetOptions(aSkydome, aFade, aRotate):
    return x3d.SkydomeSetOptions(aSkydome, aFade, aRotate)

x3d.SkydomeSetDeepColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetDeepColor.restype = ctypes.c_double
def SkydomeSetDeepColor(aSkydome, aColor):
    return x3d.SkydomeSetDeepColor(aSkydome, aColor)

x3d.SkydomeSetHazeColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetHazeColor.restype = ctypes.c_double
def SkydomeSetHazeColor(aSkydome, aColor):
    return x3d.SkydomeSetHazeColor(aSkydome, aColor)

x3d.SkydomeSetNightColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetNightColor.restype = ctypes.c_double
def SkydomeSetNightColor(aSkydome, aColor):
    return x3d.SkydomeSetNightColor(aSkydome, aColor)

x3d.SkydomeSetSkyColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetSkyColor.restype = ctypes.c_double
def SkydomeSetSkyColor(aSkydome, aColor):
    return x3d.SkydomeSetSkyColor(aSkydome, aColor)

x3d.SkydomeSetSunDawnColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetSunDawnColor.restype = ctypes.c_double
def SkydomeSetSunDawnColor(aSkydome, aColor):
    return x3d.SkydomeSetSunDawnColor(aSkydome, aColor)

x3d.SkydomeSetSunZenithColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetSunZenithColor.restype = ctypes.c_double
def SkydomeSetSunZenithColor(aSkydome, aColor):
    return x3d.SkydomeSetSunZenithColor(aSkydome, aColor)

x3d.SkydomeSetSunElevation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetSunElevation.restype = ctypes.c_double
def SkydomeSetSunElevation(aSkydome, aAngle):
    return x3d.SkydomeSetSunElevation(aSkydome, aAngle)

x3d.SkydomeSetTurbidity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeSetTurbidity.restype = ctypes.c_double
def SkydomeSetTurbidity(aSkydome, aTurbidity):
    return x3d.SkydomeSetTurbidity(aSkydome, aTurbidity)

x3d.SkydomeAddRandomStars.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SkydomeAddRandomStars.restype = ctypes.c_double
def SkydomeAddRandomStars(aSkydome, aStars, aColor):
    return x3d.SkydomeAddRandomStars(aSkydome, aStars, aColor)

x3d.SkydomeAddStar.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SkydomeAddStar.restype = ctypes.c_double
def SkydomeAddStar(aSkydome, aRightascension, aDeclination, aMagnitude, aColor):
    return x3d.SkydomeAddStar(aSkydome, aRightascension, aDeclination, aMagnitude, aColor)

x3d.SkydomeClearStars.argtypes = [ctypes.c_double]
x3d.SkydomeClearStars.restype = ctypes.c_double
def SkydomeClearStars(aSkydome):
    return x3d.SkydomeClearStars(aSkydome)

x3d.SkydomeTwinkleStars.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SkydomeTwinkleStars.restype = ctypes.c_double
def SkydomeTwinkleStars(aSkydome, aMode):
    return x3d.SkydomeTwinkleStars(aSkydome, aMode)


# sound.pas
x3d.AudioInit.argtypes = []
x3d.AudioInit.restype = ctypes.c_double
def AudioInit():
    return x3d.AudioInit()

x3d.AudioClose.argtypes = []
x3d.AudioClose.restype = ctypes.c_double
def AudioClose():
    return x3d.AudioClose()

x3d.AudioChannelIsPlaying.argtypes = [ctypes.c_double]
x3d.AudioChannelIsPlaying.restype = ctypes.c_double
def AudioChannelIsPlaying(aChannel):
    return x3d.AudioChannelIsPlaying(aChannel)

x3d.AudioMusicIsPlaying.argtypes = []
x3d.AudioMusicIsPlaying.restype = ctypes.c_double
def AudioMusicIsPlaying():
    return x3d.AudioMusicIsPlaying()

x3d.AudioSetChannelVolume.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AudioSetChannelVolume.restype = ctypes.c_double
def AudioSetChannelVolume(aChannel, aVol):
    return x3d.AudioSetChannelVolume(aChannel, aVol)

x3d.AudioSetMusicVolume.argtypes = [ctypes.c_double]
x3d.AudioSetMusicVolume.restype = ctypes.c_double
def AudioSetMusicVolume(aVol):
    return x3d.AudioSetMusicVolume(aVol)

x3d.AudioSetChannelPannning.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AudioSetChannelPannning.restype = ctypes.c_double
def AudioSetChannelPannning(aChannel, aPanning):
    return x3d.AudioSetChannelPannning(aChannel, aPanning)

x3d.AudioSetChannelPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.AudioSetChannelPosition.restype = ctypes.c_double
def AudioSetChannelPosition(aChannel, aAngle, aDistance):
    return x3d.AudioSetChannelPosition(aChannel, aAngle, aDistance)

x3d.AudioSetChannelDistance.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AudioSetChannelDistance.restype = ctypes.c_double
def AudioSetChannelDistance(aChannel, aDistance):
    return x3d.AudioSetChannelDistance(aChannel, aDistance)

x3d.AudioStopChannel.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AudioStopChannel.restype = ctypes.c_double
def AudioStopChannel(aChannel, aFade):
    return x3d.AudioStopChannel(aChannel, aFade)

x3d.AudioStopChannelDelayed.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.AudioStopChannelDelayed.restype = ctypes.c_double
def AudioStopChannelDelayed(aChannel, aDelay):
    return x3d.AudioStopChannelDelayed(aChannel, aDelay)

x3d.AudioStopMusic.argtypes = [ctypes.c_double]
x3d.AudioStopMusic.restype = ctypes.c_double
def AudioStopMusic(aFade):
    return x3d.AudioStopMusic(aFade)

x3d.AudioPauseMusic.argtypes = []
x3d.AudioPauseMusic.restype = ctypes.c_double
def AudioPauseMusic():
    return x3d.AudioPauseMusic()

x3d.AudioResumeMusic.argtypes = []
x3d.AudioResumeMusic.restype = ctypes.c_double
def AudioResumeMusic():
    return x3d.AudioResumeMusic()

x3d.AudioRewindMusic.argtypes = []
x3d.AudioRewindMusic.restype = ctypes.c_double
def AudioRewindMusic():
    return x3d.AudioRewindMusic()

x3d.AudioSetMusicPosition.argtypes = [ctypes.c_double]
x3d.AudioSetMusicPosition.restype = ctypes.c_double
def AudioSetMusicPosition(aPos):
    return x3d.AudioSetMusicPosition(aPos)

x3d.SoundLoad.argtypes = [ctypes.c_char_p]
x3d.SoundLoad.restype = ctypes.c_double
def SoundLoad(aFilename):
    return x3d.SoundLoad(aFilename)

x3d.SoundPlay.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SoundPlay.restype = ctypes.c_double
def SoundPlay(aS, aChannel, aLoops):
    return x3d.SoundPlay(aS, aChannel, aLoops)

x3d.MusicLoad.argtypes = [ctypes.c_char_p]
x3d.MusicLoad.restype = ctypes.c_double
def MusicLoad(aFilename):
    return x3d.MusicLoad(aFilename)

x3d.MusicPlay.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.MusicPlay.restype = ctypes.c_double
def MusicPlay(aM, aLoops):
    return x3d.MusicPlay(aM, aLoops)


# sprite.pas
x3d.HUDSpriteCreate.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDSpriteCreate.restype = ctypes.c_double
def HUDSpriteCreate(aMtrl, aW, aH, aParent):
    return x3d.HUDSpriteCreate(aMtrl, aW, aH, aParent)

x3d.HUDSpriteGetMouseOver.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDSpriteGetMouseOver.restype = ctypes.c_double
def HUDSpriteGetMouseOver(aSprite, aViewer):
    return x3d.HUDSpriteGetMouseOver(aSprite, aViewer)

x3d.HUDSpriteXTiles.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDSpriteXTiles.restype = ctypes.c_double
def HUDSpriteXTiles(aSprite, aXtls):
    return x3d.HUDSpriteXTiles(aSprite, aXtls)

x3d.HUDSpriteYTiles.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.HUDSpriteYTiles.restype = ctypes.c_double
def HUDSpriteYTiles(aSprite, aYtls):
    return x3d.HUDSpriteYTiles(aSprite, aYtls)

x3d.SpriteCreate.argtypes = [ctypes.c_char_p, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteCreate.restype = ctypes.c_double
def SpriteCreate(aMtrl, aW, aH, aParent):
    return x3d.SpriteCreate(aMtrl, aW, aH, aParent)

x3d.SpriteSetSize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteSetSize.restype = ctypes.c_double
def SpriteSetSize(aSprite, aW, aH):
    return x3d.SpriteSetSize(aSprite, aW, aH)

x3d.SpriteGetSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SpriteGetSize.restype = ctypes.c_double
def SpriteGetSize(aSprite, aType_Val):
    return x3d.SpriteGetSize(aSprite, aType_Val)

x3d.SpriteScale.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteScale.restype = ctypes.c_double
def SpriteScale(aSprite, aU, aV):
    return x3d.SpriteScale(aSprite, aU, aV)

x3d.SpriteSetRotation.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SpriteSetRotation.restype = ctypes.c_double
def SpriteSetRotation(aSprite, aAngle):
    return x3d.SpriteSetRotation(aSprite, aAngle)

x3d.SpriteRotate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.SpriteRotate.restype = ctypes.c_double
def SpriteRotate(aSprite, aAngle):
    return x3d.SpriteRotate(aSprite, aAngle)

x3d.SpriteMirror.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteMirror.restype = ctypes.c_double
def SpriteMirror(aSprite, aU, aV):
    return x3d.SpriteMirror(aSprite, aU, aV)

x3d.SpriteCreateEx.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteCreateEx.restype = ctypes.c_double
def SpriteCreateEx(aW, aH, aLeft, aTop, aRight, aBottom, aParent):
    return x3d.SpriteCreateEx(aW, aH, aLeft, aTop, aRight, aBottom, aParent)

x3d.HUDSpriteCreateEx.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.HUDSpriteCreateEx.restype = ctypes.c_double
def HUDSpriteCreateEx(aW, aH, aLeft, aTop, aRight, aBottom, aParent):
    return x3d.HUDSpriteCreateEx(aW, aH, aLeft, aTop, aRight, aBottom, aParent)

x3d.SpriteSetBounds.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteSetBounds.restype = ctypes.c_double
def SpriteSetBounds(aSprite, aLeft, aTop, aRight, aBottom):
    return x3d.SpriteSetBounds(aSprite, aLeft, aTop, aRight, aBottom)

x3d.SpriteSetBoundsUV.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteSetBoundsUV.restype = ctypes.c_double
def SpriteSetBoundsUV(aSprite, aLeft, aTop, aRight, aBottom):
    return x3d.SpriteSetBoundsUV(aSprite, aLeft, aTop, aRight, aBottom)

x3d.SpriteSetOrigin.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.SpriteSetOrigin.restype = ctypes.c_double
def SpriteSetOrigin(aSprite, aX, aY):
    return x3d.SpriteSetOrigin(aSprite, aX, aY)


# terrain.pas
x3d.BmpHDSCreate.argtypes = [ctypes.c_char_p]
x3d.BmpHDSCreate.restype = ctypes.c_double
def BmpHDSCreate(aImg):
    return x3d.BmpHDSCreate(aImg)

x3d.BmpHDSSetInfiniteWarp.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.BmpHDSSetInfiniteWarp.restype = ctypes.c_double
def BmpHDSSetInfiniteWarp(aHds, aIwarp):
    return x3d.BmpHDSSetInfiniteWarp(aHds, aIwarp)

x3d.BmpHDSInvert.argtypes = [ctypes.c_double]
x3d.BmpHDSInvert.restype = ctypes.c_double
def BmpHDSInvert(aHds):
    return x3d.BmpHDSInvert(aHds)

x3d.BmpHDSCreateEmpty.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.BmpHDSCreateEmpty.restype = ctypes.c_double
def BmpHDSCreateEmpty(aW, aH, aFill):
    return x3d.BmpHDSCreateEmpty(aW, aH, aFill)

x3d.BmpHDSSetHeight.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.BmpHDSSetHeight.restype = ctypes.c_double
def BmpHDSSetHeight(aHds, aX, aY, aH):
    return x3d.BmpHDSSetHeight(aHds, aX, aY, aH)

x3d.BmpHDSGetHeight.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.BmpHDSGetHeight.restype = ctypes.c_double
def BmpHDSGetHeight(aHds, aX, aY):
    return x3d.BmpHDSGetHeight(aHds, aX, aY)

x3d.BmpHDSSave.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.BmpHDSSave.restype = ctypes.c_double
def BmpHDSSave(aHds, aFilename):
    return x3d.BmpHDSSave(aHds, aFilename)

x3d.TerrainCreate.argtypes = [ctypes.c_double]
x3d.TerrainCreate.restype = ctypes.c_double
def TerrainCreate(aParent):
    return x3d.TerrainCreate(aParent)

x3d.TerrainSetHeightData.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetHeightData.restype = ctypes.c_double
def TerrainSetHeightData(aTerrain, aHds):
    return x3d.TerrainSetHeightData(aTerrain, aHds)

x3d.TerrainSetTileSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetTileSize.restype = ctypes.c_double
def TerrainSetTileSize(aTerrain, aTsize):
    return x3d.TerrainSetTileSize(aTerrain, aTsize)

x3d.TerrainSetTilesPerTexture.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetTilesPerTexture.restype = ctypes.c_double
def TerrainSetTilesPerTexture(aTerrain, aTpt):
    return x3d.TerrainSetTilesPerTexture(aTerrain, aTpt)

x3d.TerrainSetQualityDistance.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetQualityDistance.restype = ctypes.c_double
def TerrainSetQualityDistance(aTerrain, aQd):
    return x3d.TerrainSetQualityDistance(aTerrain, aQd)

x3d.TerrainSetQualityStyle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetQualityStyle.restype = ctypes.c_double
def TerrainSetQualityStyle(aTerrain, aHrs):
    return x3d.TerrainSetQualityStyle(aTerrain, aHrs)

x3d.TerrainSetMaxCLodTriangles.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetMaxCLodTriangles.restype = ctypes.c_double
def TerrainSetMaxCLodTriangles(aTerrain, aTri):
    return x3d.TerrainSetMaxCLodTriangles(aTerrain, aTri)

x3d.TerrainSetCLodPrecision.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetCLodPrecision.restype = ctypes.c_double
def TerrainSetCLodPrecision(aTerrain, aPrec):
    return x3d.TerrainSetCLodPrecision(aTerrain, aPrec)

x3d.TerrainSetOcclusionFrameSkip.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetOcclusionFrameSkip.restype = ctypes.c_double
def TerrainSetOcclusionFrameSkip(aTerrain, aOfs):
    return x3d.TerrainSetOcclusionFrameSkip(aTerrain, aOfs)

x3d.TerrainSetOcclusionTesselate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainSetOcclusionTesselate.restype = ctypes.c_double
def TerrainSetOcclusionTesselate(aTerrain, aTot):
    return x3d.TerrainSetOcclusionTesselate(aTerrain, aTot)

x3d.TerrainGetHeightAtObjectPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TerrainGetHeightAtObjectPosition.restype = ctypes.c_double
def TerrainGetHeightAtObjectPosition(aTerrain, aObj):
    return x3d.TerrainGetHeightAtObjectPosition(aTerrain, aObj)

x3d.TerrainGetLastTriCount.argtypes = [ctypes.c_double]
x3d.TerrainGetLastTriCount.restype = ctypes.c_double
def TerrainGetLastTriCount(aTerrain):
    return x3d.TerrainGetLastTriCount(aTerrain)

x3d.TerrainGetHDSPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TerrainGetHDSPosition.restype = ctypes.c_double
def TerrainGetHDSPosition(aTerrain, aX, aY, aZ, aIndex):
    return x3d.TerrainGetHDSPosition(aTerrain, aX, aY, aZ, aIndex)


# texture.pas
x3d.TextureExLoad.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.TextureExLoad.restype = ctypes.c_double
def TextureExLoad(aTextureitem, aFilename):
    return x3d.TextureExLoad(aTextureitem, aFilename)

x3d.TextureExSetFromMaterial.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.TextureExSetFromMaterial.restype = ctypes.c_double
def TextureExSetFromMaterial(aTextureitem, aMtrl):
    return x3d.TextureExSetFromMaterial(aTextureitem, aMtrl)

x3d.TextureExGenerate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TextureExGenerate.restype = ctypes.c_double
def TextureExGenerate(aTextureitem, aW, aH):
    return x3d.TextureExGenerate(aTextureitem, aW, aH)

x3d.TextureExDelete.argtypes = [ctypes.c_double]
x3d.TextureExDelete.restype = ctypes.c_double
def TextureExDelete(aTextureitem):
    return x3d.TextureExDelete(aTextureitem)

x3d.TextureExSetTextureScale.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TextureExSetTextureScale.restype = ctypes.c_double
def TextureExSetTextureScale(aTextureitem, aX, aY):
    return x3d.TextureExSetTextureScale(aTextureitem, aX, aY)

x3d.TextureExSetTextureOffset.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TextureExSetTextureOffset.restype = ctypes.c_double
def TextureExSetTextureOffset(aTextureitem, aX, aY):
    return x3d.TextureExSetTextureOffset(aTextureitem, aX, aY)

x3d.TextureExEnable.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TextureExEnable.restype = ctypes.c_double
def TextureExEnable(aTextureitem, aMode):
    return x3d.TextureExEnable(aTextureitem, aMode)


# thorfx.pas
x3d.ThorFXManagerCreate.argtypes = []
x3d.ThorFXManagerCreate.restype = ctypes.c_double
def ThorFXManagerCreate():
    return x3d.ThorFXManagerCreate()

x3d.ThorFXSetColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ThorFXSetColor.restype = ctypes.c_double
def ThorFXSetColor(aFx, aIncolor, aInalpha, aOutcolor, aOutalpha, aCcolor, aCalpha):
    return x3d.ThorFXSetColor(aFx, aIncolor, aInalpha, aOutcolor, aOutalpha, aCcolor, aCalpha)

x3d.ThorFXEnableCore.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXEnableCore.restype = ctypes.c_double
def ThorFXEnableCore(aFx, aMode):
    return x3d.ThorFXEnableCore(aFx, aMode)

x3d.ThorFXEnableGlow.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXEnableGlow.restype = ctypes.c_double
def ThorFXEnableGlow(aFx, aMode):
    return x3d.ThorFXEnableGlow(aFx, aMode)

x3d.ThorFXSetMaxParticles.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXSetMaxParticles.restype = ctypes.c_double
def ThorFXSetMaxParticles(aFx, aMaxp):
    return x3d.ThorFXSetMaxParticles(aFx, aMaxp)

x3d.ThorFXSetGlowSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXSetGlowSize.restype = ctypes.c_double
def ThorFXSetGlowSize(aFx, aSize):
    return x3d.ThorFXSetGlowSize(aFx, aSize)

x3d.ThorFXSetVibrate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXSetVibrate.restype = ctypes.c_double
def ThorFXSetVibrate(aFx, aVibr):
    return x3d.ThorFXSetVibrate(aFx, aVibr)

x3d.ThorFXSetWildness.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXSetWildness.restype = ctypes.c_double
def ThorFXSetWildness(aFx, aWild):
    return x3d.ThorFXSetWildness(aFx, aWild)

x3d.ThorFXSetTarget.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ThorFXSetTarget.restype = ctypes.c_double
def ThorFXSetTarget(aFx, aX, aY, aZ):
    return x3d.ThorFXSetTarget(aFx, aX, aY, aZ)

x3d.ThorFXCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ThorFXCreate.restype = ctypes.c_double
def ThorFXCreate(aFx, aObj):
    return x3d.ThorFXCreate(aFx, aObj)


# trail.pas
x3d.TrailCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailCreate.restype = ctypes.c_double
def TrailCreate(aObj, aParent):
    return x3d.TrailCreate(aObj, aParent)

x3d.TrailSetObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailSetObject.restype = ctypes.c_double
def TrailSetObject(aTrail, aObj):
    return x3d.TrailSetObject(aTrail, aObj)

x3d.TrailSetAlpha.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TrailSetAlpha.restype = ctypes.c_double
def TrailSetAlpha(aTrail, aAlpha, aFade):
    return x3d.TrailSetAlpha(aTrail, aAlpha, aFade)

x3d.TrailSetLimits.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.TrailSetLimits.restype = ctypes.c_double
def TrailSetLimits(aTrail, aVl, aTl):
    return x3d.TrailSetLimits(aTrail, aVl, aTl)

x3d.TrailSetMinDistance.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailSetMinDistance.restype = ctypes.c_double
def TrailSetMinDistance(aTrail, aDistance):
    return x3d.TrailSetMinDistance(aTrail, aDistance)

x3d.TrailSetUVScale.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailSetUVScale.restype = ctypes.c_double
def TrailSetUVScale(aTrail, aScale):
    return x3d.TrailSetUVScale(aTrail, aScale)

x3d.TrailSetMarkStyle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailSetMarkStyle.restype = ctypes.c_double
def TrailSetMarkStyle(aTrail, aMs):
    return x3d.TrailSetMarkStyle(aTrail, aMs)

x3d.TrailSetMarkWidth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailSetMarkWidth.restype = ctypes.c_double
def TrailSetMarkWidth(aTrail, aWidth):
    return x3d.TrailSetMarkWidth(aTrail, aWidth)

x3d.TrailSetEnabled.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TrailSetEnabled.restype = ctypes.c_double
def TrailSetEnabled(aTrail, aMode):
    return x3d.TrailSetEnabled(aTrail, aMode)

x3d.TrailClearMarks.argtypes = [ctypes.c_double]
x3d.TrailClearMarks.restype = ctypes.c_double
def TrailClearMarks(aTrail):
    return x3d.TrailClearMarks(aTrail)


# tree.pas
x3d.TreeCreate.argtypes = [ctypes.c_double]
x3d.TreeCreate.restype = ctypes.c_double
def TreeCreate(aParent):
    return x3d.TreeCreate(aParent)

x3d.TreeSetMaterials.argtypes = [ctypes.c_double]
x3d.TreeSetMaterials.restype = ctypes.c_double
def TreeSetMaterials(aTree, aMfront, aMback, aMbranch, aPchar):
    return x3d.TreeSetMaterials(aTree, aMfront, aMback, aMbranch, aPchar)

x3d.TreeSetBranchFacets.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetBranchFacets.restype = ctypes.c_double
def TreeSetBranchFacets(aTree, aFacets):
    return x3d.TreeSetBranchFacets(aTree, aFacets)

x3d.TreeBuildMesh.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeBuildMesh.restype = ctypes.c_double
def TreeBuildMesh(aTree, aParent):
    return x3d.TreeBuildMesh(aTree, aParent)

x3d.TreeSetBranchNoise.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetBranchNoise.restype = ctypes.c_double
def TreeSetBranchNoise(aTree, aNoise):
    return x3d.TreeSetBranchNoise(aTree, aNoise)

x3d.TreeSetBranchAngle.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetBranchAngle.restype = ctypes.c_double
def TreeSetBranchAngle(aTree, aAngle):
    return x3d.TreeSetBranchAngle(aTree, aAngle)

x3d.TreeSetBranchSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetBranchSize.restype = ctypes.c_double
def TreeSetBranchSize(aTree, aSize):
    return x3d.TreeSetBranchSize(aTree, aSize)

x3d.TreeSetBranchRadius.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetBranchRadius.restype = ctypes.c_double
def TreeSetBranchRadius(aTree, aRadius):
    return x3d.TreeSetBranchRadius(aTree, aRadius)

x3d.TreeSetBranchTwist.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetBranchTwist.restype = ctypes.c_double
def TreeSetBranchTwist(aTree, aTwist):
    return x3d.TreeSetBranchTwist(aTree, aTwist)

x3d.TreeSetDepth.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetDepth.restype = ctypes.c_double
def TreeSetDepth(aTree, aDepth):
    return x3d.TreeSetDepth(aTree, aDepth)

x3d.TreeSetLeafSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetLeafSize.restype = ctypes.c_double
def TreeSetLeafSize(aTree, aLeafsize):
    return x3d.TreeSetLeafSize(aTree, aLeafsize)

x3d.TreeSetLeafThreshold.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetLeafThreshold.restype = ctypes.c_double
def TreeSetLeafThreshold(aTree, aThreshold):
    return x3d.TreeSetLeafThreshold(aTree, aThreshold)

x3d.TreeSetSeed.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.TreeSetSeed.restype = ctypes.c_double
def TreeSetSeed(aTree, aSeed):
    return x3d.TreeSetSeed(aTree, aSeed)


# verlet.pas
x3d.VerletWorldCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletWorldCreate.restype = ctypes.c_double
def VerletWorldCreate(aIter, aUpdatespacepartion, aDrag):
    return x3d.VerletWorldCreate(aIter, aUpdatespacepartion, aDrag)

x3d.VerletWorldCreateOctree.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletWorldCreateOctree.restype = ctypes.c_double
def VerletWorldCreateOctree(aWorld, aXmin, aYmin, aZmin, aXmax, aYmax, aZmax, aLeaf, aDepth):
    return x3d.VerletWorldCreateOctree(aWorld, aXmin, aYmin, aZmin, aXmax, aYmax, aZmax, aLeaf, aDepth)

x3d.VerletGetNodeCount.argtypes = [ctypes.c_double]
x3d.VerletGetNodeCount.restype = ctypes.c_double
def VerletGetNodeCount(aWorld):
    return x3d.VerletGetNodeCount(aWorld)

x3d.VerletWorldGravityCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletWorldGravityCreate.restype = ctypes.c_double
def VerletWorldGravityCreate(aWorld, aX, aY, aZ):
    return x3d.VerletWorldGravityCreate(aWorld, aX, aY, aZ)

x3d.VerletWorldGravitySetDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletWorldGravitySetDirection.restype = ctypes.c_double
def VerletWorldGravitySetDirection(aGrv, aX, aY, aZ):
    return x3d.VerletWorldGravitySetDirection(aGrv, aX, aY, aZ)

x3d.VerletWorldUpdate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletWorldUpdate.restype = ctypes.c_double
def VerletWorldUpdate(aWorld, aNewtime):
    return x3d.VerletWorldUpdate(aWorld, aNewtime)

x3d.EdgeDetectorCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.EdgeDetectorCreate.restype = ctypes.c_double
def EdgeDetectorCreate(aWorld, aObj):
    return x3d.EdgeDetectorCreate(aWorld, aObj)

x3d.EdgeDetectorSetWeldDistance.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.EdgeDetectorSetWeldDistance.restype = ctypes.c_double
def EdgeDetectorSetWeldDistance(aEdge, aDis):
    return x3d.EdgeDetectorSetWeldDistance(aEdge, aDis)

x3d.VerletConstraintFloorCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintFloorCreate.restype = ctypes.c_double
def VerletConstraintFloorCreate(aWorld, aBou, aLevel):
    return x3d.VerletConstraintFloorCreate(aWorld, aBou, aLevel)

x3d.VerletConstraintFloorSetNormal.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintFloorSetNormal.restype = ctypes.c_double
def VerletConstraintFloorSetNormal(aFlr, aX, aY, aZ):
    return x3d.VerletConstraintFloorSetNormal(aFlr, aX, aY, aZ)

x3d.VerletConstraintFloorSetObjectLocations.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintFloorSetObjectLocations.restype = ctypes.c_double
def VerletConstraintFloorSetObjectLocations(aFlr, aObj):
    return x3d.VerletConstraintFloorSetObjectLocations(aFlr, aObj)

x3d.VerletConstraintSphereCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintSphereCreate.restype = ctypes.c_double
def VerletConstraintSphereCreate(aWorld, aRad):
    return x3d.VerletConstraintSphereCreate(aWorld, aRad)

x3d.VerletConstraintCylinderCreate.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCylinderCreate.restype = ctypes.c_double
def VerletConstraintCylinderCreate(aWorld, aRad):
    return x3d.VerletConstraintCylinderCreate(aWorld, aRad)

x3d.VerletConstraintCylinderSetAxis.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCylinderSetAxis.restype = ctypes.c_double
def VerletConstraintCylinderSetAxis(aCyl, aX, aY, aZ):
    return x3d.VerletConstraintCylinderSetAxis(aCyl, aX, aY, aZ)

x3d.VerletConstraintCubeCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCubeCreate.restype = ctypes.c_double
def VerletConstraintCubeCreate(aWorld, aX, aY, aZ):
    return x3d.VerletConstraintCubeCreate(aWorld, aX, aY, aZ)

x3d.VerletConstraintCubeCreateSetCube.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCubeCreateSetCube.restype = ctypes.c_double
def VerletConstraintCubeCreateSetCube(aWorld, aCube1):
    return x3d.VerletConstraintCubeCreateSetCube(aWorld, aCube1)

x3d.VerletConstraintCubeSetDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCubeSetDirection.restype = ctypes.c_double
def VerletConstraintCubeSetDirection(aCb, aX, aY, aZ):
    return x3d.VerletConstraintCubeSetDirection(aCb, aX, aY, aZ)

x3d.VerletConstraintCapsuleCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCapsuleCreate.restype = ctypes.c_double
def VerletConstraintCapsuleCreate(aWorld, aRad, aLen):
    return x3d.VerletConstraintCapsuleCreate(aWorld, aRad, aLen)

x3d.VerletConstraintCapsuleSetAxis.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintCapsuleSetAxis.restype = ctypes.c_double
def VerletConstraintCapsuleSetAxis(aCp, aX, aY, aZ):
    return x3d.VerletConstraintCapsuleSetAxis(aCp, aX, aY, aZ)

x3d.VerletConstraintSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintSetPosition.restype = ctypes.c_double
def VerletConstraintSetPosition(aObj, aX, aY, aZ):
    return x3d.VerletConstraintSetPosition(aObj, aX, aY, aZ)

x3d.VerletConstraintSetFrictionRatio.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintSetFrictionRatio.restype = ctypes.c_double
def VerletConstraintSetFrictionRatio(aObj, aFr):
    return x3d.VerletConstraintSetFrictionRatio(aObj, aFr)

x3d.VerletConstraintSetEnabled.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintSetEnabled.restype = ctypes.c_double
def VerletConstraintSetEnabled(aObj, aEn):
    return x3d.VerletConstraintSetEnabled(aObj, aEn)

x3d.VerletNodeNailedDown.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletNodeNailedDown.restype = ctypes.c_double
def VerletNodeNailedDown(aWorld, aInd, aBol):
    return x3d.VerletNodeNailedDown(aWorld, aInd, aBol)

x3d.VerletNodeSetPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletNodeSetPosition.restype = ctypes.c_double
def VerletNodeSetPosition(aWorld, aInd, aX, aY, aZ):
    return x3d.VerletNodeSetPosition(aWorld, aInd, aX, aY, aZ)

x3d.VerletNodeSetRadius.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletNodeSetRadius.restype = ctypes.c_double
def VerletNodeSetRadius(aWorld, aInd, aRad):
    return x3d.VerletNodeSetRadius(aWorld, aInd, aRad)

x3d.VerletNodeSetFriction.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletNodeSetFriction.restype = ctypes.c_double
def VerletNodeSetFriction(aWorld, aInd, aFr):
    return x3d.VerletNodeSetFriction(aWorld, aInd, aFr)

x3d.VerletNodeSetWeight.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletNodeSetWeight.restype = ctypes.c_double
def VerletNodeSetWeight(aWorld, aInd, aWeight):
    return x3d.VerletNodeSetWeight(aWorld, aInd, aWeight)

x3d.VerletNodeApplyFriction.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletNodeApplyFriction.restype = ctypes.c_double
def VerletNodeApplyFriction(aWorld, aInd, aFr, aDepth, aX, aY, aZ):
    return x3d.VerletNodeApplyFriction(aWorld, aInd, aFr, aDepth, aX, aY, aZ)

x3d.VerletAirResistanceCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletAirResistanceCreate.restype = ctypes.c_double
def VerletAirResistanceCreate(aWorld, aMagnitude, aChaos):
    return x3d.VerletAirResistanceCreate(aWorld, aMagnitude, aChaos)

x3d.VerletAirResistanceSetWindDirection.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletAirResistanceSetWindDirection.restype = ctypes.c_double
def VerletAirResistanceSetWindDirection(aAir, aX, aY, aZ):
    return x3d.VerletAirResistanceSetWindDirection(aAir, aX, aY, aZ)

x3d.VerletAirResistanceSetWindMagnitude.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletAirResistanceSetWindMagnitude.restype = ctypes.c_double
def VerletAirResistanceSetWindMagnitude(aAir, aMag):
    return x3d.VerletAirResistanceSetWindMagnitude(aAir, aMag)

x3d.VerletAirResistanceSetWindChaos.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletAirResistanceSetWindChaos.restype = ctypes.c_double
def VerletAirResistanceSetWindChaos(aAir, aCh):
    return x3d.VerletAirResistanceSetWindChaos(aAir, aCh)

x3d.VerletConstraintGetCount.argtypes = [ctypes.c_double]
x3d.VerletConstraintGetCount.restype = ctypes.c_double
def VerletConstraintGetCount(aWr):
    return x3d.VerletConstraintGetCount(aWr)

x3d.VerletConstraintSetSlack.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.VerletConstraintSetSlack.restype = ctypes.c_double
def VerletConstraintSetSlack(aWr, aCon, aSla):
    return x3d.VerletConstraintSetSlack(aWr, aCon, aSla)

x3d.VerletWorldSetSimTime.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletWorldSetSimTime.restype = ctypes.c_double
def VerletWorldSetSimTime(aWr, aTm):
    return x3d.VerletWorldSetSimTime(aWr, aTm)

x3d.VerletWorldSetMaxDeltaTime.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.VerletWorldSetMaxDeltaTime.restype = ctypes.c_double
def VerletWorldSetMaxDeltaTime(aWr, aTm):
    return x3d.VerletWorldSetMaxDeltaTime(aWr, aTm)


# viewer.pas
x3d.ViewerCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerCreate.restype = ctypes.c_double
def ViewerCreate(aTop, aLeft, aWidth, aHeight, aWinhandle):
    return x3d.ViewerCreate(aTop, aLeft, aWidth, aHeight, aWinhandle)

x3d.ViewerSetCamera.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetCamera.restype = ctypes.c_double
def ViewerSetCamera(aViewer, aCamera):
    return x3d.ViewerSetCamera(aViewer, aCamera)

x3d.ViewerEnableVSync.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerEnableVSync.restype = ctypes.c_double
def ViewerEnableVSync(aViewer, aVsm):
    return x3d.ViewerEnableVSync(aViewer, aVsm)

x3d.ViewerRender.argtypes = [ctypes.c_double]
x3d.ViewerRender.restype = ctypes.c_double
def ViewerRender(aViewer):
    return x3d.ViewerRender(aViewer)

x3d.ViewerBeginRender.argtypes = [ctypes.c_double]
x3d.ViewerBeginRender.restype = ctypes.c_double
def ViewerBeginRender(aViewer):
    return x3d.ViewerBeginRender(aViewer)

x3d.ViewerClear.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerClear.restype = ctypes.c_double
def ViewerClear(aViewer, aClearcolor, aCleardepth, aClearstencil):
    return x3d.ViewerClear(aViewer, aClearcolor, aCleardepth, aClearstencil)

x3d.ViewerRenderObject.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerRenderObject.restype = ctypes.c_double
def ViewerRenderObject(aViewer, aObj):
    return x3d.ViewerRenderObject(aViewer, aObj)

x3d.ViewerEndRender.argtypes = [ctypes.c_double]
x3d.ViewerEndRender.restype = ctypes.c_double
def ViewerEndRender(aViewer):
    return x3d.ViewerEndRender(aViewer)

x3d.ViewerRenderToFile.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ViewerRenderToFile.restype = ctypes.c_double
def ViewerRenderToFile(aV, aFname):
    return x3d.ViewerRenderToFile(aV, aFname)

x3d.ViewerResize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerResize.restype = ctypes.c_double
def ViewerResize(aV, aLeft, aTop, aWidth, aHeight):
    return x3d.ViewerResize(aV, aLeft, aTop, aWidth, aHeight)

x3d.ViewerSetVisible.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetVisible.restype = ctypes.c_double
def ViewerSetVisible(aViewer, aMode):
    return x3d.ViewerSetVisible(aViewer, aMode)

x3d.ViewerGetPixelColor.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerGetPixelColor.restype = ctypes.c_double
def ViewerGetPixelColor(aViewer, aX, aY):
    return x3d.ViewerGetPixelColor(aViewer, aX, aY)

x3d.ViewerGetPixelDepth.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerGetPixelDepth.restype = ctypes.c_double
def ViewerGetPixelDepth(aViewer, aX, aY):
    return x3d.ViewerGetPixelDepth(aViewer, aX, aY)

x3d.ViewerSetLighting.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetLighting.restype = ctypes.c_double
def ViewerSetLighting(aViewer, aMode):
    return x3d.ViewerSetLighting(aViewer, aMode)

x3d.ViewerSetBackgroundColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetBackgroundColor.restype = ctypes.c_double
def ViewerSetBackgroundColor(aViewer, aColor):
    return x3d.ViewerSetBackgroundColor(aViewer, aColor)

x3d.ViewerSetAmbientColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetAmbientColor.restype = ctypes.c_double
def ViewerSetAmbientColor(aViewer, aColor):
    return x3d.ViewerSetAmbientColor(aViewer, aColor)

x3d.ViewerEnableFog.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerEnableFog.restype = ctypes.c_double
def ViewerEnableFog(aViewer, aMode):
    return x3d.ViewerEnableFog(aViewer, aMode)

x3d.ViewerSetFogColor.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetFogColor.restype = ctypes.c_double
def ViewerSetFogColor(aViewer, aColor):
    return x3d.ViewerSetFogColor(aViewer, aColor)

x3d.ViewerSetFogDistance.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerSetFogDistance.restype = ctypes.c_double
def ViewerSetFogDistance(aV, aFstart, aFend):
    return x3d.ViewerSetFogDistance(aV, aFstart, aFend)

x3d.ViewerScreenToWorld.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerScreenToWorld.restype = ctypes.c_double
def ViewerScreenToWorld(aViewer, aX, aY, aInd):
    return x3d.ViewerScreenToWorld(aViewer, aX, aY, aInd)

x3d.ViewerWorldToScreen.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerWorldToScreen.restype = ctypes.c_double
def ViewerWorldToScreen(aViewer, aX, aY, aZ, aInd):
    return x3d.ViewerWorldToScreen(aViewer, aX, aY, aZ, aInd)

x3d.ViewerCopyToTexture.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ViewerCopyToTexture.restype = ctypes.c_double
def ViewerCopyToTexture(aViewer, aMtrl):
    return x3d.ViewerCopyToTexture(aViewer, aMtrl)

x3d.ViewerGetPickedObject.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerGetPickedObject.restype = ctypes.c_double
def ViewerGetPickedObject(aViewer, aX, aY):
    return x3d.ViewerGetPickedObject(aViewer, aX, aY)

x3d.ViewerGetPickedObjectsList.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerGetPickedObjectsList.restype = ctypes.c_double
def ViewerGetPickedObjectsList(aViewer, aList, aX, aY, aW, aH, aNum):
    return x3d.ViewerGetPickedObjectsList(aViewer, aList, aX, aY, aW, aH, aNum)

x3d.ViewerScreenToVector.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerScreenToVector.restype = ctypes.c_double
def ViewerScreenToVector(aViewer, aX, aY, aInd):
    return x3d.ViewerScreenToVector(aViewer, aX, aY, aInd)

x3d.ViewerVectorToScreen.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerVectorToScreen.restype = ctypes.c_double
def ViewerVectorToScreen(aViewer, aX, aY, aZ, aInd):
    return x3d.ViewerVectorToScreen(aViewer, aX, aY, aZ, aInd)

x3d.ViewerPixelToDistance.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerPixelToDistance.restype = ctypes.c_double
def ViewerPixelToDistance(aViewer, aX, aY):
    return x3d.ViewerPixelToDistance(aViewer, aX, aY)

x3d.ViewerSetAntiAliasing.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetAntiAliasing.restype = ctypes.c_double
def ViewerSetAntiAliasing(aV, aAa):
    return x3d.ViewerSetAntiAliasing(aV, aAa)

x3d.ViewerGetGLSLSupported.argtypes = [ctypes.c_double]
x3d.ViewerGetGLSLSupported.restype = ctypes.c_double
def ViewerGetGLSLSupported(aViewer):
    return x3d.ViewerGetGLSLSupported(aViewer)

x3d.ViewerGetFBOSupported.argtypes = [ctypes.c_double]
x3d.ViewerGetFBOSupported.restype = ctypes.c_double
def ViewerGetFBOSupported(aViewer):
    return x3d.ViewerGetFBOSupported(aViewer)

x3d.ViewerGetVBOSupported.argtypes = [ctypes.c_double]
x3d.ViewerGetVBOSupported.restype = ctypes.c_double
def ViewerGetVBOSupported(aViewer):
    return x3d.ViewerGetVBOSupported(aViewer)

x3d.ViewerGetSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerGetSize.restype = ctypes.c_double
def ViewerGetSize(aViewer, aIndex):
    return x3d.ViewerGetSize(aViewer, aIndex)

x3d.ViewerGetPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerGetPosition.restype = ctypes.c_double
def ViewerGetPosition(aViewer, aIndex):
    return x3d.ViewerGetPosition(aViewer, aIndex)

x3d.ViewerIsOpenGLExtensionSupported.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.ViewerIsOpenGLExtensionSupported.restype = ctypes.c_double
def ViewerIsOpenGLExtensionSupported(aViewer, aExt):
    return x3d.ViewerIsOpenGLExtensionSupported(aViewer, aExt)

x3d.ViewerGetFramesPerSecond.argtypes = [ctypes.c_double]
x3d.ViewerGetFramesPerSecond.restype = ctypes.c_double
def ViewerGetFramesPerSecond(aViewer):
    return x3d.ViewerGetFramesPerSecond(aViewer)

x3d.ViewerResetPerformanceMonitor.argtypes = [ctypes.c_double]
x3d.ViewerResetPerformanceMonitor.restype = ctypes.c_double
def ViewerResetPerformanceMonitor(aViewer):
    return x3d.ViewerResetPerformanceMonitor(aViewer)

x3d.ViewerPixelRayToWorld.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.ViewerPixelRayToWorld.restype = ctypes.c_double
def ViewerPixelRayToWorld(aViewer, aX, aY, aInd):
    return x3d.ViewerPixelRayToWorld(aViewer, aX, aY, aInd)

x3d.ViewerShadeModel.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerShadeModel.restype = ctypes.c_double
def ViewerShadeModel(aV, aInd):
    return x3d.ViewerShadeModel(aV, aInd)

x3d.ViewerSetAutoRender.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.ViewerSetAutoRender.restype = ctypes.c_double
def ViewerSetAutoRender(aV, aMode):
    return x3d.ViewerSetAutoRender(aV, aMode)


# water.pas
x3d.WaterCreate.argtypes = [ctypes.c_double]
x3d.WaterCreate.restype = ctypes.c_double
def WaterCreate(aParent):
    return x3d.WaterCreate(aParent)

x3d.WaterCreateRandomRipple.argtypes = [ctypes.c_double]
x3d.WaterCreateRandomRipple.restype = ctypes.c_double
def WaterCreateRandomRipple(aWater):
    return x3d.WaterCreateRandomRipple(aWater)

x3d.WaterCreateRippleAtGridPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.WaterCreateRippleAtGridPosition.restype = ctypes.c_double
def WaterCreateRippleAtGridPosition(aWater, aX, aY):
    return x3d.WaterCreateRippleAtGridPosition(aWater, aX, aY)

x3d.WaterCreateRippleAtWorldPosition.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.WaterCreateRippleAtWorldPosition.restype = ctypes.c_double
def WaterCreateRippleAtWorldPosition(aWater, aX, aY, aZ):
    return x3d.WaterCreateRippleAtWorldPosition(aWater, aX, aY, aZ)

x3d.WaterCreateRippleAtObjectPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterCreateRippleAtObjectPosition.restype = ctypes.c_double
def WaterCreateRippleAtObjectPosition(aWater, aObj):
    return x3d.WaterCreateRippleAtObjectPosition(aWater, aObj)

x3d.WaterSetMask.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.WaterSetMask.restype = ctypes.c_double
def WaterSetMask(aWater, aMaterial):
    return x3d.WaterSetMask(aWater, aMaterial)

x3d.WaterSetActive.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetActive.restype = ctypes.c_double
def WaterSetActive(aWater, aMode):
    return x3d.WaterSetActive(aWater, aMode)

x3d.WaterReset.argtypes = [ctypes.c_double]
x3d.WaterReset.restype = ctypes.c_double
def WaterReset(aWater):
    return x3d.WaterReset(aWater)

x3d.WaterSetRainTimeInterval.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetRainTimeInterval.restype = ctypes.c_double
def WaterSetRainTimeInterval(aWater, aInterval):
    return x3d.WaterSetRainTimeInterval(aWater, aInterval)

x3d.WaterSetRainForce.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetRainForce.restype = ctypes.c_double
def WaterSetRainForce(aWater, aForce):
    return x3d.WaterSetRainForce(aWater, aForce)

x3d.WaterSetViscosity.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetViscosity.restype = ctypes.c_double
def WaterSetViscosity(aWater, aViscosity):
    return x3d.WaterSetViscosity(aWater, aViscosity)

x3d.WaterSetElastic.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetElastic.restype = ctypes.c_double
def WaterSetElastic(aWater, aElastic):
    return x3d.WaterSetElastic(aWater, aElastic)

x3d.WaterSetResolution.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetResolution.restype = ctypes.c_double
def WaterSetResolution(aWater, aRes):
    return x3d.WaterSetResolution(aWater, aRes)

x3d.WaterSetLinearWaveHeight.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetLinearWaveHeight.restype = ctypes.c_double
def WaterSetLinearWaveHeight(aWater, aHeight):
    return x3d.WaterSetLinearWaveHeight(aWater, aHeight)

x3d.WaterSetLinearWaveFrequency.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WaterSetLinearWaveFrequency.restype = ctypes.c_double
def WaterSetLinearWaveFrequency(aWater, aFreq):
    return x3d.WaterSetLinearWaveFrequency(aWater, aFreq)


# window.pas
x3d.WindowCreate.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.WindowCreate.restype = ctypes.c_double
def WindowCreate(aX, aY, aW, aH, aResizeable):
    return x3d.WindowCreate(aX, aY, aW, aH, aResizeable)

x3d.WindowCenter.argtypes = [ctypes.c_double]
x3d.WindowCenter.restype = ctypes.c_double
def WindowCenter(aW):
    return x3d.WindowCenter(aW)

x3d.WindowResize.argtypes = [ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double, ctypes.c_double]
x3d.WindowResize.restype = ctypes.c_double
def WindowResize(aW, aX, aY, aWidth, aHeight):
    return x3d.WindowResize(aW, aX, aY, aWidth, aHeight)

x3d.WindowGetPosition.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WindowGetPosition.restype = ctypes.c_double
def WindowGetPosition(aW, aIndex):
    return x3d.WindowGetPosition(aW, aIndex)

x3d.WindowGetSize.argtypes = [ctypes.c_double, ctypes.c_double]
x3d.WindowGetSize.restype = ctypes.c_double
def WindowGetSize(aW, aIndex):
    return x3d.WindowGetSize(aW, aIndex)

x3d.WindowGetHandle.argtypes = [ctypes.c_double]
x3d.WindowGetHandle.restype = ctypes.c_double
def WindowGetHandle(aW):
    return x3d.WindowGetHandle(aW)

x3d.WindowSetTitle.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.WindowSetTitle.restype = ctypes.c_double
def WindowSetTitle(aW, aTitle):
    return x3d.WindowSetTitle(aW, aTitle)

x3d.WindowDestroy.argtypes = [ctypes.c_double]
x3d.WindowDestroy.restype = ctypes.c_double
def WindowDestroy(aW):
    return x3d.WindowDestroy(aW)

x3d.WindowIsShowing.argtypes = [ctypes.c_double]
x3d.WindowIsShowing.restype = ctypes.c_double
def WindowIsShowing(aW):
    return x3d.WindowIsShowing(aW)

x3d.WindowSetIcon.argtypes = [ctypes.c_double, ctypes.c_char_p]
x3d.WindowSetIcon.restype = ctypes.c_double
def WindowSetIcon(aW, aFilename):
    return x3d.WindowSetIcon(aW, aFilename)

x3d.WindowDispatch.argtypes = []
x3d.WindowDispatch.restype = ctypes.c_double
def WindowDispatch():
    return x3d.WindowDispatch()

x3d.WindowIsActive.argtypes = [ctypes.c_double]
x3d.WindowIsActive.restype = ctypes.c_double
def WindowIsActive(aW):
    return x3d.WindowIsActive(aW)


