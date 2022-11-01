library xtreme3d;

uses
  System.SysUtils,
  System.Classes,
  Windows,
  Winapi.OpenGL,
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.Imaging.PNGImage,
  GLS.ApplicationFileIO,
  GLS.BitmapFont,
  GLS.Cadencer,
  GLS.Collision,
  GLS.Color,
  GLS.Context,
  GLS.Coordinates,
  GLS.FileVfsPAK,
  GLS.FileQ3MD3,
  GLS.GeomObjects,
  GLS.HUDObjects,
  GLS.Material,
  GLS.MaterialScript,
  GLS.Objects,
  GLS.OpenGLAdapter,
  GLS.PersistentClasses,
  GLS.ProcTextures,
  GLS.RenderContextInfo,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.Selection,
  GLS.SpaceText,
  GLS.State,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.TilePlane,
  GLS.VectorFileObjects,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorTypes,
  GLS.WindowsFont;
  //Physics.ODEManager,
  //Physics.ODERagdoll;

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

    //ode: TGLODEManager;
    //odeRagdollWorld: TGLODERagdollWorld;
    //jointList: TGLODEJointList;

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
//{$I 'xtreme3d/freeform'}
{$I 'xtreme3d/object'}
//{$I 'xtreme3d/polygon'}
{$I 'xtreme3d/material'}
//{$I 'xtreme3d/shaders'}
//{$I 'xtreme3d/thorfx'}
//{$I 'xtreme3d/firefx'}
//{$I 'xtreme3d/lensflare'}
//{$I 'xtreme3d/terrain'}
//{$I 'xtreme3d/blur'}
//{$I 'xtreme3d/skybox'}
//{$I 'xtreme3d/trail'}
//{$I 'xtreme3d/shadowplane'}
//{$I 'xtreme3d/shadowvolume'}
//{$I 'xtreme3d/skydome'}
//{$I 'xtreme3d/water'}
//{$I 'xtreme3d/lines'}
//{$I 'xtreme3d/tree'}
//{$I 'xtreme3d/navigator'}
//{$I 'xtreme3d/movement'}
//{$I 'xtreme3d/dce'}
//{$I 'xtreme3d/fps'}
//{$I 'xtreme3d/mirror'}
//{$I 'xtreme3d/partition'}
//{$I 'xtreme3d/memviewer'}
//{$I 'xtreme3d/fbo'}
//{$I 'xtreme3d/proxy'}
//{$I 'xtreme3d/text'}
//{$I 'xtreme3d/objecthash'}
//{$I 'xtreme3d/grid'}
//{$I 'xtreme3d/shadowmap'}
//{$I 'xtreme3d/ode'}
//{$I 'xtreme3d/kraft'}
//{$I 'xtreme3d/clipplane'}
{$I 'xtreme3d/input'}
//{$I 'xtreme3d/window'}
//{$I 'xtreme3d/color'}
//{$I 'xtreme3d/pipe'}
//{$I 'xtreme3d/verlet'}
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
    ViewerCreate, ViewerSetCamera, ViewerRender, ViewerRenderToFile,
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

    // Freeform
    // Terrain

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

    // Polygon

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
    // ThorFX
    // FireFX
    // Lensflare
    // Skydome
    // Water
    // Blur
    // Skybox
    // Lines
    // Tree
    // Trail
    // Shadowplane
    // Shadowvolume
    // Navigator
    // DCE
    // ODE
    // FPSManager
    // Mirror
    // Partition
    // Proxy & MultiProxy
    // Grid
    // MemoryViewer
    // ShadowMap
    // Movement
    // FBO
    // ObjectHash
    // Text
    // ClipPlane

    // Input
    MouseGetPositionX, MouseGetPositionY, MouseSetPosition,
    MouseShowCursor, KeyIsPressed, MouseIsPressed,

    // Window
    // Color
    // Kraft
    // Grid
    // Verlet

    // PickList
    PickListCreate, PickListClear, PickListGetCount, PickListGetHit;
begin
end.
