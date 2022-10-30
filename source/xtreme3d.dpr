library xtreme3d;

uses
  System.SysUtils,
  System.Classes,
  Windows,
  Winapi.OpenGL,
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.Imaging.PNGImage,
  GLS.Cadencer,
  GLS.Context,
  GLS.Material,
  GLS.Objects,
  GLS.OpenGLAdapter,
  GLS.RenderContextInfo,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.SceneViewer,
  GLS.Selection;
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

function IsKeyDown(vk: integer): Boolean;
begin
   result := (GetAsyncKeyState(vk)<>0);
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

{$I 'xtreme3d/engine'}
{$I 'xtreme3d/viewer'}
{$I 'xtreme3d/dummycube'}
{$I 'xtreme3d/camera'}
{$I 'xtreme3d/primitives'}
{$I 'xtreme3d/object'}
{$I 'xtreme3d/input'}
{$I 'xtreme3d/picklist'}

exports
    // Engine
    EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
    EngineUpdate, EngineSaveScene, EngineLoadScene, EngineRootObject,
    EngineShowLoadingErrors, EngineGetTimeStep,
    PointerToReal,

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
    DummycubeCreate,

    // Primitives
    CubeCreate,

    // Camera
    CameraCreate,

    // Object
    ObjectSetPosition, ObjectRotate, ObjectMove, ObjectLift, ObjectStrafe,

    // Input
    MouseGetPositionX, MouseGetPositionY, MouseSetPosition,
    MouseShowCursor, KeyIsPressed, MouseIsPressed,

    // PickList
    PickListCreate, PickListClear, PickListGetCount, PickListGetHit;
begin
end.
