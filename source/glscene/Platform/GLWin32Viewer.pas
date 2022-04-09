// GLWin32Viewer
{: Win32 specific.<p>

	<b>History : </b><font size=-1><ul>
      <li>04/12/04 - MF - Added FieldOfView, formula by Ivan Sivak Jr.
      <li>24/07/03 - EG - FullScreen Viewer moved to GLWin32FullScreenViewer
      <li>11/06/03 - EG - Now uses ViewerBeforeChange to adjust VSync
      <li>29/10/02 - EG - Added MouseEnter/Leave/InControl
      <li>27/09/02 - EG - Added Ability to set display frequency
      <li>22/08/02 - EG - Added TGLSceneViewer.RecreateWnd
      <li>19/08/02 - EG - Added GetHandle
      <li>14/03/02 - EG - No longer invalidates while rendering
      <li>11/02/02 - EG - Fixed BeforeRender
      <li>29/01/02 - EG - New StayOnTop/Maximize logic (Richard Smuts)
      <li>22/01/02 - EG - Added TGLFullScreenViewer
      <li>28/12/01 - EG - Event persistence change (GliGli / Dephi bug)
	   <li>12/12/01 - EG - Creation (split from GLScene.pas)
	</ul></font>
}
unit GLWin32Viewer;

interface

{$i GLScene.inc}

uses Windows, Graphics, Forms, Messages, Classes, GLScene, Controls, Menus,
   GLContext;

type

{$ifdef FPC}
   TWMPaint = packed record
      Msg: Cardinal;
      DC: HDC;
      Unused: Longint;
      Result: Longint;
   end;
  TWMDestroy = TWMNoParams;
{$endif}

   // TVSyncMode
   //
   TVSyncMode = (vsmSync, vsmNoSync);

   // TGLSceneViewer
   //
   {: Component where the GLScene objects get rendered.<p>
      This component delimits the area where OpenGL renders the scene,
      it represents the 3D scene viewed from a camera (specified in the
      camera property). This component can also render to a file or to a bitmap.<p>
      It is primarily a windowed component, but it can handle full-screen
      operations : simply make this component fit the whole screen (use a
      borderless form).<p>
      This viewer also allows to define rendering options such a fog, face culling,
      depth testing, etc. and can take care of framerate calculation.<p> }
   TGLSceneViewer = class (TWinControl)
      private
         { Private Declarations }
         FBuffer : TGLSceneBuffer;
         FVSync : TVSyncMode;
         FOwnDC : Cardinal;
         FOnMouseEnter, FOnMouseLeave : TNotifyEvent;
         FMouseInControl : Boolean;
         FIsOpenGLAvailable : Boolean;
         FLastScreenPos : TPoint;
         FAutoRender: Boolean;

         procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
         procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
         procedure WMSize(var Message: TWMSize); Message WM_SIZE;
         procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;

          procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
          procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
        function GetFieldOfView: single;
        procedure SetFieldOfView(const Value: single);

      protected
         { Protected Declarations }
         procedure SetBeforeRender(const val : TNotifyEvent);
         function GetBeforeRender : TNotifyEvent;
         procedure SetPostRender(const val : TNotifyEvent);
         function GetPostRender : TNotifyEvent;
         procedure SetAfterRender(const val : TNotifyEvent);
         function GetAfterRender : TNotifyEvent;
         procedure SetCamera(const val : TGLCamera);
         function GetCamera : TGLCamera;
         procedure SetBuffer(const val : TGLSceneBuffer);

         procedure CreateParams(var Params: TCreateParams); override;
         procedure CreateWnd; override;
         procedure DestroyWnd; override;
         procedure Loaded; override;
         procedure DoBeforeRender(Sender : TObject); dynamic;
         procedure DoBufferChange(Sender : TObject); virtual;
         procedure DoBufferStructuralChange(Sender : TObject); dynamic;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         {: Makes TWinControl's RecreateWnd public.<p>
            This procedure allows to work around limitations in some OpenGL
            drivers (like MS Software OpenGL) that are not able to share lists
            between RCs that already have display lists. }
         procedure RecreateWnd;

         property IsOpenGLAvailable : Boolean read FIsOpenGLAvailable;

         function LastFrameTime : Single;
         function FramesPerSecond : Single;
         function FramesPerSecondText(decimals : Integer = 1) : String;
         procedure ResetPerformanceMonitor;

         function CreateSnapShotBitmap : TBitmap;

         property RenderDC : Cardinal read FOwnDC;
         property MouseInControl : Boolean read FMouseInControl;

      published
         { Published Declarations }
         {: Camera from which the scene is rendered. }
         property Camera : TGLCamera read GetCamera write SetCamera;

         {: Specifies if the refresh should be synchronized with the VSync signal.<p>
            If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
            extension, this property is ignored.  }
         property VSync : TVSyncMode read FVSync write FVSync default vsmNoSync;

         {: Triggered before the scene's objects get rendered.<p>
            You may use this event to execute your own OpenGL rendering. }
         property BeforeRender : TNotifyEvent read GetBeforeRender write SetBeforeRender;
         {: Triggered just after all the scene's objects have been rendered.<p>
            The OpenGL context is still active in this event, and you may use it
            to execute your own OpenGL rendering.<p> }
         property PostRender : TNotifyEvent read GetPostRender write SetPostRender;
         {: Called after rendering.<p>
            You cannot issue OpenGL calls in this event, if you want to do your own
            OpenGL stuff, use the PostRender event. }
         property AfterRender : TNotifyEvent read GetAfterRender write SetAfterRender;

         {: Access to buffer properties. }
         property Buffer : TGLSceneBuffer read FBuffer write SetBuffer;

         {: Returns or sets the field of view for the viewer, in degrees.<p>
         This value depends on the camera and the width and height of the scene.
         The value isn't persisted, if the width/height or camera.focallength is
         changed, FieldOfView is changed also. }
         property FieldOfView : single read GetFieldOfView write SetFieldOfView;

         property AutoRender: Boolean read FAutoRender write FAutoRender;

               property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
               property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
         
         property Align;
         property Anchors;
         property DragCursor;
         property DragMode;
         property Enabled;
         property HelpContext;
         property Hint;
         property PopupMenu;
         property Visible;

         property OnClick;
         property OnDblClick;
         property OnDragDrop;
         property OnDragOver;
         property OnStartDrag;
         property OnEndDrag;
         property OnMouseDown;
         property OnMouseMove;
         property OnMouseUp;
{$ifdef GLS_COMPILER_5_UP}
         property OnContextPopup;
{$endif}
   end;

procedure SetupVSync(vsync : TVSyncMode);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL1x, SysUtils, GLWin32Context, GLCrossPlatform;

// SetupVSync
//
procedure SetupVSync(vsync : TVSyncMode);
var
   i : Integer;
begin
   if WGL_EXT_swap_control then begin
      i:=wglGetSwapIntervalEXT;
      case VSync of
         vsmSync    : if i<>1 then wglSwapIntervalEXT(1);
         vsmNoSync  : if i<>0 then wglSwapIntervalEXT(0);
      else
         Assert(False);
      end;
   end;
end;

// ------------------
// ------------------ TGLSceneViewer ------------------
// ------------------

// Create
//
constructor TGLSceneViewer.Create(AOwner: TComponent);
begin
   FIsOpenGLAvailable:=InitOpenGL;
   inherited Create(AOwner);
   ControlStyle:=[csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
   if csDesigning in ComponentState then
      ControlStyle:=ControlStyle+[csFramed];
   Width:=100;
   Height:=100;
   FVSync:=vsmNoSync;
   FBuffer:=TGLSceneBuffer.Create(Self);
   FBuffer.ViewerBeforeRender:=DoBeforeRender;
   FBuffer.OnChange:=DoBufferChange;
   FBuffer.OnStructuralChange:=DoBufferStructuralChange;
   FAutoRender := true;
end;

// Destroy
//
destructor TGLSceneViewer.Destroy;
begin
   FBuffer.Free;
   inherited Destroy;
end;

// Notification
//
procedure TGLSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (FBuffer<>nil) and (AComponent=Camera) then
      Camera:=nil;
   inherited;
end;

// RecreateWnd
//
procedure TGLSceneViewer.RecreateWnd;
begin
   inherited;
end;

// SetBeforeRender
//
procedure TGLSceneViewer.SetBeforeRender(const val : TNotifyEvent);
begin
   FBuffer.BeforeRender:=val;
end;

// GetBeforeRender
//
function TGLSceneViewer.GetBeforeRender : TNotifyEvent;
begin
   Result:=FBuffer.BeforeRender;
end;

// SetPostRender
//
procedure TGLSceneViewer.SetPostRender(const val : TNotifyEvent);
begin
   FBuffer.PostRender:=val;
end;

// GetPostRender
//
function TGLSceneViewer.GetPostRender : TNotifyEvent;
begin
   Result:=FBuffer.PostRender;
end;

// SetAfterRender
//
procedure TGLSceneViewer.SetAfterRender(const val : TNotifyEvent);
begin
   FBuffer.AfterRender:=val;
end;

// GetAfterRender
//
function TGLSceneViewer.GetAfterRender : TNotifyEvent;
begin
   Result:=FBuffer.AfterRender;
end;

// SetCamera
//
procedure TGLSceneViewer.SetCamera(const val : TGLCamera);
begin
   FBuffer.Camera:=val;
end;

// GetCamera
//
function TGLSceneViewer.GetCamera : TGLCamera;
begin
   Result:=FBuffer.Camera;
end;

// SetBuffer
//
procedure TGLSceneViewer.SetBuffer(const val : TGLSceneBuffer);
begin
   FBuffer.Assign(val);
end;

// CreateParams
//
procedure TGLSceneViewer.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   with Params do begin
      Style:=Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
      WindowClass.Style:=WindowClass.Style or CS_OWNDC;
   end;
end;

// CreateWnd
//
procedure TGLSceneViewer.CreateWnd;
begin
   inherited CreateWnd;
   if IsOpenGLAvailable then begin
      // initialize and activate the OpenGL rendering context
      // need to do this only once per window creation as we have a private DC
      FBuffer.Resize(Self.Width, Self.Height);
      FOwnDC:=GetDC(Handle);
      FBuffer.CreateRC(FOwnDC, False);
   end;
end;

// DestroyWnd
//
procedure TGLSceneViewer.DestroyWnd;
begin
   FBuffer.DestroyRC;
   if FOwnDC<>0 then begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC:=0;
   end;
   inherited;
end;

// WMEraseBkgnd
//
procedure TGLSceneViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   if IsOpenGLAvailable then
      Message.Result:=1
   else inherited; 
end;

// WMSize
//
procedure TGLSceneViewer.WMSize(var Message: TWMSize);
begin
   inherited;
   FBuffer.Resize(Message.Width, Message.Height);
end;

// WMPaint
//
procedure TGLSceneViewer.WMPaint(var Message: TWMPaint);
var
   PS : TPaintStruct;
   p : TPoint;
begin
   p:=ClientToScreen(Point(0, 0));
   if (FLastScreenPos.X<>p.X) or (FLastScreenPos.Y<>p.Y) then begin
      // Workaround for MS OpenGL "black borders" bug
      if FBuffer.RCInstantiated then
         PostMessage(Handle, WM_SIZE, SIZE_RESTORED,
                     Width+(Height shl 16));
      FLastScreenPos:=p;
   end;
   BeginPaint(Handle, PS);
   try
      if IsOpenGLAvailable and (Width>0) and (Height>0) then begin
         if FAutoRender then
             FBuffer.Render;
      end
   finally
      EndPaint(Handle, PS);
      Message.Result:=0;
   end;
end;

// WMDestroy
//
procedure TGLSceneViewer.WMDestroy(var Message: TWMDestroy);
begin
   FBuffer.DestroyRC;
   if FOwnDC<>0 then begin
      ReleaseDC(Handle, FOwnDC);
      FOwnDC:=0;
   end;
   inherited;
end;

// CMMouseEnter
//
procedure TGLSceneViewer.CMMouseEnter(var msg: TMessage);
begin
   inherited;
   FMouseInControl:=True;
   if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

// CMMouseLeave
//
procedure TGLSceneViewer.CMMouseLeave(var msg: TMessage);
begin
   inherited;
   FMouseInControl:=False;
   if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

// Loaded
//
procedure TGLSceneViewer.Loaded;
begin
   inherited Loaded;
   // initiate window creation
   HandleNeeded;
end;

// DoBeforeRender
//
procedure TGLSceneViewer.DoBeforeRender(Sender : TObject);
begin
   SetupVSync(VSync);
end;

// DoBufferChange
//
procedure TGLSceneViewer.DoBufferChange(Sender : TObject);
begin
   if (not Buffer.Rendering) and (not Buffer.Freezed) then
      Invalidate;
end;

// DoBufferStructuralChange
//
procedure TGLSceneViewer.DoBufferStructuralChange(Sender : TObject);
begin
   RecreateWnd;
end;

// LastFrameTime
//
function TGLSceneViewer.LastFrameTime : Single;
begin
   Result:=FBuffer.LastFrameTime;
end;

// FramesPerSecond
//
function TGLSceneViewer.FramesPerSecond : Single;
begin
   Result:=FBuffer.FramesPerSecond;
end;

// FramesPerSecondText
//
function TGLSceneViewer.FramesPerSecondText(decimals : Integer = 1) : String;
begin
   Result:=Format('%.*f FPS', [decimals, FBuffer.FramesPerSecond]);
end;

// ResetPerformanceMonitor
//
procedure TGLSceneViewer.ResetPerformanceMonitor;
begin
   FBuffer.ResetPerformanceMonitor;
end;

// CreateSnapShotBitmap
//
function TGLSceneViewer.CreateSnapShotBitmap : TBitmap;
begin
   Result:=TBitmap.Create;
   Result.PixelFormat:=pf24bit;
   Result.Width:=Width;
   Result.Height:=Height;

   BitBlt(Result.Canvas.Handle, 0, 0, Width, Height,
          RenderDC, 0, 0, SRCCOPY);
end;

// GetFieldOfView
//
function TGLSceneViewer.GetFieldOfView: single;
begin
  if not Assigned(Camera) then
    result := 0

  else if Width<Height then
    result := Camera.GetFieldOfView(Width)

  else
    result := Camera.GetFieldOfView(Height);
end;

procedure TGLSceneViewer.SetFieldOfView(const Value: single);
begin
  if Assigned(Camera) then
  begin
    if Width<Height then
      Camera.SetFieldOfView(Value, Width)

    else
      Camera.SetFieldOfView(Value, Height);
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLSceneViewer);

end.

