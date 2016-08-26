unit GLViewer;

interface

{$I GLScene.inc}

uses
  {$IFDEF LINUX}GLLinuxViewer; {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}GLWin32Viewer; {$ENDIF MSWINDOWS}

type
{$IFDEF LINUX}
  TGLSceneViewer = class(TGLLinuxSceneViewer);
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
  TGLSceneViewer = class(TGLWin32SceneViewer);
{$ENDIF MSWINDOWS}

implementation

end.
 
