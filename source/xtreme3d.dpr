library xtreme3d;
uses
  Windows, Messages, Classes, Controls, StdCtrls, ExtCtrls, Dialogs, SysUtils, TypInfo,
  GLScene, GLObjects, GLWin32FullScreenViewer, GLMisc, GLGraph,
  GLCollision, GLTexture, OpenGL1x, VectorGeometry, Graphics,
  GLVectorFileObjects, GLWin32Viewer, GLSpaceText, GLGeomObjects, GLCadencer,
  Jpeg, Tga, DDS, PNG, GLProcTextures, Spin, GLVfsPAK, GLCanvas, GLGraphics, GLPortal,
  GLHUDObjects, GLBitmapFont, GLWindowsFont, GLImposter, VectorTypes, GLUtils,
  GLPolyhedron, GLTeapot, GLzBuffer, GLFile3DS, GLFileGTS, GLFileLWO, GLFileMD2,
  GLFileMD3, Q3MD3, GLFileMS3D, GLFileMD5, GLFileNMF, GLFileNurbs, GLFileObj, GLFileOCT,
  GLFilePLY, GLFileQ3BSP, GLFileSMD, GLFileSTL, GLFileTIN, GLFileB3D,
  GLFileMDC, GLFileVRML, GLFileLOD, GLFileX, GLFileCSM, GLFileLMTS, GLFileASE, GLFileDXS,
  GLPhongShader, VectorLists, GLThorFX, GLFireFX,
  GLTexCombineShader, GLBumpShader, GLCelShader, GLContext, GLTerrainRenderer, GLHeightData,
  GLBlur, GLSLShader, GLMultiMaterialShader, GLOutlineShader, GLHiddenLineShader,
  ApplicationFileIO, GLMaterialScript, GLWaterPlane, GeometryBB, GLExplosionFx,
  GLSkyBox, GLShadowPlane, GLShadowVolume, GLSkydome, GLLensFlare, GLDCE,
  GLNavigator, GLFPSMovement, GLMirror, SpatialPartitioning, GLSpatialPartitioning,
  GLTrail, GLTree, GLMultiProxy, GLODEManager, dynode, GLODECustomColliders,
  GLShadowMap, MeshUtils, pngimage, GLRagdoll, GLODERagdoll, GLMovement, GLHUDShapes, GLActorProxy,
  GLFBO, Hashes, Freetype, GLFreetypeFont, GLClippingPlane, GLLightFx,
  Keyboard, Forms, Kraft, GLKraft, GLFileFBX, GLCrossPlatform;

type
   TEmpty = class(TComponent)
    private
   end;

const
   {$I 'bumpshader'}
   {$I 'phongshader'}

var
  showLoadingErrors: Boolean;
  scene: TGLScene;
  matlib: TGLMaterialLibrary;
  memviewer: TGLMemoryViewer;
  cadencer: TGLCadencer;
  empty: TEmpty;

  collisionPoint: TVector;
  collisionNormal: TVector;

  ode: TGLODEManager;
  odeRagdollWorld: TODERagdollWorld;
  jointList: TGLODEJointList;

  kraftRaycastPoint: TKraftVector3;
  kraftRaycastNormal: TKraftVector3;

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

function VectorDivide(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result[0]:=v1[0]/delta;
   Result[1]:=v1[1]/delta;
   Result[2]:=v1[2]/delta;
end;

function VectorMultiply(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result[0]:=v1[0]*delta;
   Result[1]:=v1[1]*delta;
   Result[2]:=v1[2]*delta;
end;

function IsExtensionSupported(v: TGLSceneViewer; const Extension: string): Boolean;
var
   Buffer : String;
   ExtPos: Integer;
begin
   v.Buffer.RenderingContext.Activate;
   Buffer := StrPas(glGetString(GL_EXTENSIONS));
   // First find the position of the extension string as substring in Buffer.
   ExtPos := Pos(Extension, Buffer);
   Result := ExtPos > 0;
   // Now check that it isn't only a substring of another extension.
   if Result then
     Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
               or (Buffer[ExtPos + Length(Extension)]=' ');
   v.Buffer.RenderingContext.Deactivate;
end;

procedure GenMeshTangents(mesh: TMeshObject);
var
   i,j: Integer;
   v,t: array[0..2] of TAffineVector;

   x1, x2, y1, y2, z1, z2, t1, t2, s1, s2: Single;
   sDir, tDir: TAffineVector;
   sTan, tTan: TAffineVectorList;
   tangents, bitangents: TVectorList;
   sv, tv: array[0..2] of TAffineVector;
   r, oneOverR: Single;
   n, ta: TAffineVector;
   tang: TAffineVector;

   tangent,
   binormal   : array[0..2] of TVector;
   vt,tt      : TAffineVector;
   interp,dot : Single;

begin
   mesh.Tangents.Clear;
   mesh.Binormals.Clear;
   mesh.Tangents.Count:=mesh.Vertices.Count;
   mesh.Binormals.Count:=mesh.Vertices.Count;

   tangents := TVectorList.Create;
   tangents.Count:=mesh.Vertices.Count;

   bitangents := TVectorList.Create;
   bitangents.Count:=mesh.Vertices.Count; 

   sTan := TAffineVectorList.Create;
   tTan := TAffineVectorList.Create;
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

      x1 := v[1][0] - v[0][0];
      x2 := v[2][0] - v[0][0];
      y1 := v[1][1] - v[0][1];
      y2 := v[2][1] - v[0][1];
      z1 := v[1][2] - v[0][2];
      z2 := v[2][2] - v[0][2];

      s1 := t[1][0] - t[0][0];
      s2 := t[2][0] - t[0][0];
      t1 := t[1][1] - t[0][1];
      t2 := t[2][1] - t[0][1];

      r := (s1 * t2) - (s2 * t1);

      if r = 0.0 then
        r := 1.0;

      oneOverR := 1.0 / r;

      sDir[0] := (t2 * x1 - t1 * x2) * oneOverR;
      sDir[1] := (t2 * y1 - t1 * y2) * oneOverR;
      sDir[2] := (t2 * z1 - t1 * z2) * oneOverR;

      tDir[0] := (s1 * x2 - s2 * x1) * oneOverR;
      tDir[1] := (s1 * y2 - s2 * y1) * oneOverR;
      tDir[2] := (s1 * z2 - s2 * z1) * oneOverR;

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

function getJointAxisParams(j: TODEJointBase; axis: Integer): TODEJointParams;
var
  res: TODEJointParams;
begin
  if j is TODEJointHinge then
  begin
    if axis = 1 then
      res := TODEJointHinge(j).AxisParams;
  end
  else if j is TODEJointHinge2 then
  begin
    if axis = 1 then
      res := TODEJointHinge2(j).Axis1Params
    else if axis = 2 then
      res := TODEJointHinge2(j).Axis2Params;
  end
  else if j is TODEJointUniversal then
  begin
    if axis = 1 then
      res := TODEJointUniversal(j).Axis1Params
    else if axis = 2 then
      res := TODEJointUniversal(j).Axis2Params;
  end;
  result := res;
end;

procedure onContactStay(const ContactPair: PKraftContactPair; const WithShape: TKraftShape);
begin
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

function InvertBitmap(Bitmap:TBitmap): TBitmap;
begin
  Bitmap.Canvas.CopyMode := cmDstInvert;
  Bitmap.Canvas.CopyRect(Bitmap.Canvas.ClipRect, Bitmap.Canvas, Bitmap.Canvas.ClipRect);
  Bitmap.Canvas.CopyMode := cmSrcCopy;
  Result := Bitmap;
end;

{$I 'xtreme3d/engine'}
{$I 'xtreme3d/pak'}
{$I 'xtreme3d/viewer'}
{$I 'xtreme3d/dummycube'}
{$I 'xtreme3d/camera'}
{$I 'xtreme3d/light'}
{$I 'xtreme3d/lightfx'}
{$I 'xtreme3d/fonttext'}
{$I 'xtreme3d/sprite'}
{$I 'xtreme3d/hudshapes'}
{$I 'xtreme3d/primitives'}
{$I 'xtreme3d/actor'}
{$I 'xtreme3d/freeform'}
{$I 'xtreme3d/object'}
{$I 'xtreme3d/polygon'}
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
{$I 'xtreme3d/fbo'}
{$I 'xtreme3d/proxy'}
{$I 'xtreme3d/text'}
{$I 'xtreme3d/objecthash'}
{$I 'xtreme3d/grid'}
{$I 'xtreme3d/shadowmap'}
{$I 'xtreme3d/ode'}
{$I 'xtreme3d/kraft'}
{$I 'xtreme3d/clipplane'}
{$I 'xtreme3d/input'}
{$I 'xtreme3d/window'}
{$I 'xtreme3d/color'}

// New functions for Xtreme3D 3.7.1:

function HUDSpriteSetSize(sprite, w,h: real): real; stdcall;
var
  spr: TGLHUDSprite;
begin
  spr := TGLHUDSprite(trunc64(sprite));
  spr.SetSize(w, h);
  result := 1;
end;

function HUDSpriteGetSize(sprite, type_val: real): real; stdcall;
var
  spr: TGLHUDSprite;
begin
  spr := TGLHUDSprite(trunc64(sprite));
  if (type_val = 0) then
    result := spr.Width;
  if (type_val = 1) then
    result := spr.Height;
end;

exports

//Engine
EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
Update, TrisRendered,
EngineSaveScene, EngineLoadScene, EngineRootObject,
EngineShowLoadingErrors, EngineSetMaxLights,
//Pak
SetPakArchive, PakGetFileCount, PakGetFileName, PakExtract, PakExtractFile,
//Viewer
ViewerCreate, ViewerSetCamera, ViewerEnableVSync, ViewerRenderToFile,
ViewerRender, ViewerSetAutoRender, ViewerRenderEx,
ViewerResize, ViewerSetVisible, ViewerGetPixelColor, ViewerGetPixelDepth,
ViewerSetLighting, ViewerSetBackgroundColor, ViewerSetAmbientColor, ViewerEnableFog,
ViewerSetFogColor, ViewerSetFogDistance, ViewerScreenToWorld, ViewerWorldToScreen,
ViewerCopyToTexture, ViewerGetFramesPerSecond, ViewerGetPickedObject,
ViewerScreenToVector, ViewerVectorToScreen, ViewerPixelToDistance, ViewerGetPickedObjectsList,
ViewerSetAntiAliasing,
ViewerSetOverrideMaterial,
ViewerIsOpenGLExtensionSupported,
ViewerGetGLSLSupported, ViewerGetFBOSupported, ViewerGetVBOSupported,
ViewerGetSize, ViewerGetPosition,
ViewerResetPerformanceMonitor,
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
// LightFx
LightFXCreate, 
//Font & Text
BmpFontCreate, BmpFontLoad,
TTFontCreate, TTFontSetLineGap, TTFontSetEncoding, TTFontLoadCodePage,
WindowsBitmapfontCreate,
HUDTextCreate, FlatTextCreate,
HUDTextSetRotation, SpaceTextCreate, SpaceTextSetExtrusion, HUDTextSetFont,
FlatTextSetFont, SpaceTextSetFont, HUDTextSetColor, FlatTextSetColor, HUDTextSetText,
FlatTextSetText, SpaceTextSetText,
HUDSpriteGetMouseOver,
//Sprite
HUDSpriteCreate, HUDSpriteSetSize, HUDSpriteGetSize,
SpriteCreate, SpriteSetSize, SpriteScale, SpriteSetRotation,
SpriteRotate, SpriteMirror, SpriteNoZWrite,
SpriteCreateEx, HUDSpriteCreateEx, SpriteSetBounds, SpriteSetBoundsUV,
SpriteSetOrigin,
//HUDShapes
HUDShapeRectangleCreate, HUDShapeCircleCreate, HUDShapeLineCreate, HUDShapeMeshCreate,
HUDShapeSetRotation, HUDShapeSetColor,
HUDShapeRotate, HUDShapeSetOrigin, HUDShapeSetSize, HUDShapeScale,
HUDShapeCircleSetRadius, HUDShapeCircleSetSlices, HUDShapeCircleSetAngles,
HUDShapeLineSetPoints, HUDShapeLineSetWidth,
HUDShapeMeshAddVertex, HUDShapeMeshAddTriangle,
HUDShapeMeshSetVertex, HUDShapeMeshSetTexCoord,
//Primitives
CubeCreate, CubeSetNormalDirection, PlaneCreate, SphereCreate, SphereSetAngleLimits,
CylinderCreate, ConeCreate, AnnulusCreate, TorusCreate, DiskCreate, FrustrumCreate,
DodecahedronCreate, IcosahedronCreate, TeapotCreate,
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
ActorMeshObjectsCount, ActorMeshSetVisible, ActorFaceGroupsCount, ActorFaceGroupGetMaterialName,
ActorFaceGroupSetMaterial,
ActorMoveBone, ActorRotateBone,
//Freeform
FreeformCreate, FreeformCreateEmpty,
FreeformSetMaterialLibraries,
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
FreeformMeshFaceGroupGetLightmapIndex, FreeformMeshFaceGroupSetLightmapIndex,
FreeformMeshGenNormals, FreeformMeshGenTangents,
FreeformMeshVerticesCount, FreeformMeshTriangleCount, 
FreeformMeshObjectsCount, FreeformMeshSetVisible,
FreeformMeshSetSecondCoords,
FreeformMeshFaceGroupsCount, FreeformMeshFaceGroupTriangleCount,
FreeformMeshSetMaterial, FreeformUseMeshMaterials,
FreeformSphereSweepIntersect, FreeformPointInMesh,
FreeformToFreeforms,
FreeformMeshTranslate, FreeformMeshRotate, FreeformMeshScale,
FreeformSave,

FreeformGenTangents, FreeformBuildOctree,

FreeformCreateExplosionFX, FreeformExplosionFXReset,
FreeformExplosionFXEnable, FreeformExplosionFXSetSpeed,
//Terrain
BmpHDSCreate, BmpHDSCreateEmpty, BmpHDSSetInfiniteWarp, BmpHDSInvert,
BmpHDSSetHeight, BmpHDSGetHeight, BmpHDSSave,
TerrainCreate, TerrainSetHeightData, TerrainSetTileSize, TerrainSetTilesPerTexture,
TerrainSetQualityDistance, TerrainSetQualityStyle, TerrainSetMaxCLodTriangles,
TerrainSetCLodPrecision, TerrainSetOcclusionFrameSkip, TerrainSetOcclusionTesselate,
TerrainGetHeightAtObjectPosition, TerrainGetLastTriCount,
TerrainGetHDSPosition,
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
ObjectScale, ObjectSetScale, ObjectGetScale,
ObjectSetUpVector, ObjectPointToObject, 
ObjectShowAxes,
ObjectGetGroundHeight, ObjectSceneRaycast, ObjectRaycast,
ObjectGetCollisionPosition, ObjectGetCollisionNormal, 
ObjectSetMaterial,
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
ObjectSetMatrixColumn,
ObjectExportMatrix, ObjectExportAbsoluteMatrix,
ObjectInFrustum,
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
MaterialGetCount, MaterialGetName, MaterialGetNameFromLibrary,
MaterialSetFaceCulling,
MaterialSetTexture, MaterialSetSecondTexture,
MaterialSetTextureFormat, MaterialSetTextureCompression,
MaterialTextureRequiredMemory, MaterialSetFilteringQuality,
MaterialAddTextureEx, MaterialTextureExClear, MaterialTextureExDelete,
MaterialNoiseCreate, MaterialNoiseAnimate, MaterialNoiseSetDimensions,
MaterialNoiseSetMinCut, MaterialNoiseSetSharpness, MaterialNoiseSetSeamless,
MaterialNoiseRandomSeed,
MaterialGenTexture, MaterialSetTextureWrap,
MaterialGetTextureWidth, MaterialGetTextureHeight,
MaterialLoadTexture,
MaterialLoadTextureEx, MaterialSetTextureEx, MaterialGenTextureEx,
MaterialEnableTextureEx, MaterialHasTextureEx,
MaterialSetTextureExFromLibrary,
MaterialCullFrontFaces, MaterialSetZWrite,
//Shaders
ShaderEnable, 
BumpShaderCreate,
BumpShaderSetDiffuseTexture, BumpShaderSetNormalTexture, BumpShaderSetHeightTexture,
BumpShaderSetMaxLights, BumpShaderUseParallax, BumpShaderSetParallaxOffset,
BumpShaderSetShadowMap, BumpShaderSetShadowBlurRadius, BumpShaderUseAutoTangentSpace,
CelShaderCreate, CelShaderSetLineColor, CelShaderSetLineWidth, CelShaderSetOptions,
MultiMaterialShaderCreate,
HiddenLineShaderCreate, HiddenLineShaderSetLineSmooth, HiddenLineShaderSetSolid,
HiddenLineShaderSetSurfaceLit, HiddenLineShaderSetFrontLine, HiddenLineShaderSetBackLine,
OutlineShaderCreate, OutlineShaderSetLineColor, OutlineShaderSetLineWidth,
TexCombineShaderCreate, TexCombineShaderAddCombiner,
TexCombineShaderMaterial3, TexCombineShaderMaterial4,
PhongShaderCreate, PhongShaderUseTexture, PhongShaderSetMaxLights,
GLSLShaderCreate, GLSLShaderCreateParameter,
GLSLShaderSetParameter1i, GLSLShaderSetParameter1f, GLSLShaderSetParameter2f,
GLSLShaderSetParameter3f, GLSLShaderSetParameter4f,
GLSLShaderSetParameterTexture, GLSLShaderSetParameterSecondTexture,
GLSLShaderSetParameterMatrix, GLSLShaderSetParameterInvMatrix,
GLSLShaderSetParameterShadowTexture, GLSLShaderSetParameterShadowMatrix,
GLSLShaderSetParameterFBOColorTexture, GLSLShaderSetParameterFBODepthTexture,
GLSLShaderSetParameterViewMatrix, GLSLShaderSetParameterInvViewMatrix,
GLSLShaderSetParameterHasTextureEx,
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
//Lines
LinesCreate, LinesAddNode, LinesDeleteNode, LinesSetColors, LinesSetSize,
LinesSetSplineMode, LinesSetNodesAspect, LinesSetDivision,
//Tree
TreeCreate, TreeSetMaterials, TreeSetBranchFacets, TreeBuildMesh,
TreeSetBranchNoise, TreeSetBranchAngle, TreeSetBranchSize, TreeSetBranchRadius,
TreeSetBranchTwist, TreeSetDepth, TreeSetLeafSize, TreeSetLeafThreshold, TreeSetSeed,
//Trail
TrailCreate, TrailSetObject, TrailSetAlpha, TrailSetLimits, TrailSetMinDistance,
TrailSetUVScale, TrailSetMarkStyle, TrailSetMarkWidth, TrailSetEnabled, TrailClearMarks,
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
//Movement
MovementCreate, MovementStart, MovementStop, MovementAutoStartNextPath, 
MovementAddPath, MovementSetActivePath, MovementPathSetSplineMode,
MovementPathAddNode,
MovementPathNodeSetPosition, MovementPathNodeSetRotation,
MovementPathNodeSetSpeed,
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
DceDynamicJump, DceDynamicMove, DceDynamicMoveTo,
DceDynamicSetVelocity, DceDynamicGetVelocity,
DceDynamicSetAbsVelocity, DceDynamicGetAbsVelocity,
DceDynamicApplyImpulse, DceDynamicApplyAbsImpulse,
DceDynamicInGround, DceDynamicSetMaxRecursionDepth,
DceStaticSetManager, DceStaticSetActive, DceStaticSetShape, DceStaticSetLayer,
DceStaticSetSize, DceStaticSetSolid, DceStaticSetFriction, DceStaticSetBounceFactor,
//FPSManager
FpsManagerCreate, FpsManagerSetNavigator, FpsManagerSetMovementScale,
FpsManagerAddMap, FpsManagerRemoveMap, FpsManagerMapSetCollisionGroup,
FpsSetManager, FpsSetCollisionGroup, FpsSetSphereRadius, FpsSetGravity,
FpsMove, FpsStrafe, FpsLift, FpsGetVelocity,
//Mirror
MirrorCreate, MirrorSetObject, MirrorSetOptions,
MirrorSetShape, MirrorSetDiskOptions,
//Partition
OctreeCreate, QuadtreeCreate, PartitionDestroy, PartitionAddLeaf,
PartitionLeafChanged, PartitionQueryFrustum, PartitionQueryLeaf,
PartitionQueryAABB, PartitionQueryBSphere, PartitionGetNodeTests,
PartitionGetNodeCount, PartitionGetResult, PartitionGetResultCount,
PartitionResultShow, PartitionResultHide,
//Proxy
ProxyObjectCreate, ProxyObjectSetOptions, ProxyObjectSetTarget,
MultiProxyObjectCreate, MultiProxyObjectAddTarget,
ActorProxyObjectCreate, ActorProxyObjectSwitchToAnimation,
ActorProxyObjectSetAnimationRange, ActorProxyObjectSetInterval,
//Text
TextRead, TextConvertANSIToUTF8,
//ObjectHash
ObjectHashCreate, ObjectHashSetItem, ObjectHashGetItem,
ObjectHashDeleteItem, ObjectHashGetItemCount,
ObjectHashClear, ObjectHashDestroy,
//Grid
GridCreate, GridSetLineStyle, GridSetLineSmoothing, GridSetParts,
GridSetColor, GridSetSize, GridSetPattern,
//ClipPlane
ClipPlaneCreate, ClipPlaneEnable, ClipPlaneSetPlane,
//MemoryViewer
MemoryViewerCreate, MemoryViewerSetCamera, MemoryViewerRender,
MemoryViewerSetViewport, MemoryViewerCopyToTexture,
//FBO
FBOCreate, FBOSetCamera, FBOSetViewer,
FBORenderObject, FBORenderObjectEx,
FBOSetOverrideMaterial,
FBOSetColorTextureFormat,
//ShadowMap
ShadowMapCreate, ShadowMapSetCamera, ShadowMapSetCaster,
ShadowMapSetProjectionSize, ShadowMapSetZScale, ShadowMapSetZClippingPlanes,
ShadowMapSetFBO,
ShadowMapRender,
//ODE
OdeManagerCreate, OdeManagerDestroy, OdeManagerStep, OdeManagerGetNumContactJoints,
OdeManagerSetGravity, OdeManagerSetSolver, OdeManagerSetIterations,
OdeManagerSetMaxContacts, OdeManagerSetVisible, OdeManagerSetGeomColor,
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
OdeDynamicGetContactCount, OdeStaticGetContactCount,
OdeAddBox, OdeAddSphere, OdeAddPlane, OdeAddCylinder, OdeAddCone, OdeAddCapsule, OdeAddTriMesh,
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
OdeRagdollCreate, OdeRagdollHingeJointCreate, OdeRagdollUniversalJointCreate,
OdeRagdollDummyJointCreate, OdeRagdollBoneCreate,
OdeRagdollBuild, OdeRagdollEnable, OdeRagdollUpdate,

OdeDynamicSetVelocity, OdeDynamicSetAngularVelocity,
OdeDynamicGetVelocity, OdeDynamicGetAngularVelocity,
OdeDynamicSetPosition, OdeDynamicSetRotationQuaternion, 

// Input
MouseSetPosition, MouseGetPositionX, MouseGetPositionY, MouseShowCursor, 
KeyIsPressed,
// Color
MakeColorRGB, MakeColorRGBFloat,
// Window
WindowCreate, WindowGetHandle, WindowSetTitle, WindowDestroy,
WindowCenter, WindowResize, WindowGetPosition, WindowGetSize,
WindowIsShowing, WindowSetIcon,

// Kraft
KraftCreate, KraftStep,
KraftRayCast, KraftGetRayHitPosition, KraftGetRayHitNormal, 
KraftCreateRigidBody, KraftRigidBodyFinish,
KraftRigidBodySetGravity,
KraftRigidBodySetPosition, KraftRigidBodyGetPosition,
KraftRigidBodySetLinearVelocity, KraftRigidBodyGetLinearVelocity,
KraftRigidBodySetAngularVelocity, KraftRigidBodyGetAngularVelocity,
KraftRigidBodySetRotation,
KraftRigidBodyGetDirection, KraftRigidBodyGetUp, KraftRigidBodyGetRight,
KraftRigidBodyAddForce, KraftRigidBodyAddForceAtPos, KraftRigidBodyAddRelForce,
KraftObjectSetRigidBody,
KraftCreateShapeSphere, KraftCreateShapeBox, KraftCreateShapePlane, KraftCreateShapeCapsule,
KraftCreateShapeMesh,
KraftShapeSetDensity, KraftShapeSetFriction, KraftShapeSetRestitution, 
KraftShapeSetPosition, KraftShapeGetPosition,
KraftShapeSetRayCastable,

KraftCreateJointDistance, KraftCreateJointRope, KraftCreateJointBallSocket, KraftCreateJointFixed,
KraftCreateJointHinge,
KraftJointSetAnchor1, KraftJointSetAnchor2,
KraftJointSetHingeAxis1, KraftJointSetHingeAxis2;

begin
end.
