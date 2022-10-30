library xtreme3d;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

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
{$I 'xtreme3d/picklist'}

exports
    // Engine
    EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
    EngineUpdate, EngineSaveScene, EngineLoadScene, EngineRootObject,
    EngineShowLoadingErrors, EngineGetTimeStep,
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

    PickListCreate, PickListClear, PickListGetCount, PickListGetHit;

begin
end.
