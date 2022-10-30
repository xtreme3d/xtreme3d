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
  Vcl.Graphics,
  Vcl.Dialogs,
  GLS.Cadencer,
  GLS.Material,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.SceneViewer;
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

    // TODO: user-defined cadencer
    cadencer: TGLCadencer;

    empty: TEmpty;

    //collisionPoint: TVector;
    //collisionNormal: TVector;

    //ode: TGLODEManager;
    //odeRagdollWorld: TGLODERagdollWorld;
    //jointList: TGLODEJointList;

    //kraftRaycastPoint: TKraftVector3;
    //kraftRaycastNormal: TKraftVector3;

    previousTicks: Longint;

{$R *.res}

{$I 'xtreme3d/engine'}
{$I 'xtreme3d/viewer'}

function TestStr(param: PAnsiChar): real; cdecl;
begin
    //ShowMessage(String(AnsiString(param)));
    result := Length(AnsiString(param));
end;

exports
    TestStr,

    EngineCreate, EngineDestroy,
    EngineUpdate,
    ViewerCreate, ViewerRender,
    ViewerSetBackgroundColor;

begin
end.
