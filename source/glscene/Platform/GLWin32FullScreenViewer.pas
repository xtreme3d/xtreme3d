// GLWin32FullScreenViewer
{: Win32 specific full-screen viewer.<p>

   Currently TForm+TGLSceneViewer based, may be made into a standalone
   Win32 control someday, so don't assume there is a TForm in your code.<p>

	<b>History : </b><font size=-1><ul>
      <li>24/07/03 - EG - Creation from GLWin32Viewer split 
	</ul></font>
}
unit GLWin32FullScreenViewer;

interface

{$i GLScene.inc}

uses Windows, Forms, Messages, Classes, GLScene, Controls, Menus, GLWin32Viewer;

type

   // TGLScreenDepth
   //
   TGLScreenDepth = (sd8bits, sd16bits, sd24bits, sd32bits);

   // TGLFullScreenViewer
   //
   {: A FullScreen viewer.<p>
      This non visual viewer will, when activated, use the full screen as rendering
      surface. It will also switch/restore videomode depending on the required
      width/height.<br>
      This is performed by creating an underlying TForm and using its surface
      for rendering OpenGL, "decent" ICDs will automatically use PageFlipping
      instead of BlockTransfer (slower buffer flipping mode used for windowed
      OpenGL).<br>
      Note: if you terminate the application either via a kill or in the IDE,
      the original resolution isn't restored. }
   TGLFullScreenViewer = class (TGLNonVisualViewer)
      private
         { Private Declarations }
         FForm : TForm;
         FScreenDepth : TGLScreenDepth;
         FActive : Boolean;
         FSwitchedResolution : Boolean;
         FUpdateCount : Integer;
         FOnMouseDown : TMouseEvent;
         FOnMouseUp : TMouseEvent;
         FOnMouseMove : TMouseMoveEvent;
         FOnMouseWheel : TMouseWheelEvent;
         FOnClick, FOnDblClick : TNotifyEvent;
         FOnKeyDown : TKeyEvent;
         FOnKeyUp : TKeyEvent;
         FOnKeyPress : TKeyPressEvent;
         FOnClose : TCloseEvent;
         FOnCloseQuery : TCloseQueryEvent;
         FOldWndProc : TWndMethod;
         FStayOnTop : Boolean;
         FVSync : TVSyncMode;
         FRefreshRate : Integer;
         FCursor : TCursor;
         FPopupMenu : TPopupMenu;

      protected
         { Protected Declarations }
         procedure SetScreenDepth(const val : TGLScreenDepth);
         procedure SetActive(const val : Boolean);
         procedure SetOnMouseDown(const val : TMouseEvent);
         procedure SetOnMouseUp(const val : TMouseEvent);
         procedure SetOnMouseMove(const val : TMouseMoveEvent);
         procedure SetOnMouseWheel(const val : TMouseWheelEvent);
         procedure SetOnClick(const val : TNotifyEvent);
         procedure SetOnDblClick(const val : TNotifyEvent);
         procedure SetOnCloseQuery(const val : TCloseQueryEvent);
         procedure SetOnClose(const val : TCloseEvent);
         procedure SetOnKeyUp(const val : TKeyEvent);
         procedure SetOnKeyDown(const val : TKeyEvent);
         procedure SetOnKeyPress(const val : TKeyPressEvent);
         procedure SetStayOnTop(const val : Boolean);
         procedure SetCursor(const val : TCursor);
         procedure SetPopupMenu(const val : TPopupMenu);
         function  GetHandle : HWND;

         procedure DoBeforeRender(Sender : TObject);
         procedure DoBufferChange(Sender : TObject); override;
         procedure DoBufferStructuralChange(Sender : TObject); override;
         procedure PrepareGLContext; override;

         procedure Startup;
         procedure Shutdown;
         procedure BindFormEvents;
         procedure DoCloseQuery(Sender: TObject; var CanClose: Boolean);
         procedure DoPaint(Sender : TObject);
         procedure WndProc(var Message: TMessage);
         procedure DoActivate(Sender : TObject);
         procedure DoDeactivate(Sender : TObject);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Render(baseObject : TGLBaseSceneObject = nil); override;

         {: Adjusts property so that current resolution will be used.<p>
            Call this method if you want to make sure video mode isn't switched. }
         procedure UseCurrentResolution;

	      procedure BeginUpdate;
	      procedure EndUpdate;

         {: Activates/deactivates full screen mode.<p> }
         property Active : Boolean read FActive write SetActive;
         {: Read access to the underlying form handle.<p>
            Returns 0 (zero) if the viewer is not active or has not yet
            instantiated its form. }
         property Handle : HWND read GetHandle;

      published
         { Public Declarations }

         {: Requested ScreenDepth. }
         property ScreenDepth : TGLScreenDepth read FScreenDepth write SetScreenDepth default sd32bits;

         {: Specifies if the underlying form is "fsStayOnTop".<p>
            The benefit of StayOnTop is that it hides the windows bar and
            other background windows. The "fsStayOnTop" is automatically
            switched off/on when the underlying form loses/gains focus.<p>
            It is recommended not to use StayOnTop while running in the IDE
            or during the debugging phase.<p> }
         property StayOnTop : Boolean read FStayOnTop write SetStayOnTop default False;

         {: Specifies if the refresh should be synchronized with the VSync signal.<p>
            If the underlying OpenGL ICD does not support the WGL_EXT_swap_control
            extension, this property is ignored.  }
         property VSync : TVSyncMode read FVSync write FVSync default vsmSync;
         {: Screen refresh rate.<p>
            Use zero for system default. This property allows you to work around
            the WinXP bug that limits uses a refresh rate of 60Hz when changeing
            resolution. It is however suggested to give the user the opportunity
            to adjust it instead of having a fixed value (expecially beyond
            75Hz or for resolutions beyond 1024x768).<p>
            The value will be automatically clamped to the highest value
            *reported* compatible with the monitor. }
         property RefreshRate : Integer read FRefreshRate write FRefreshRate;

         property Cursor : TCursor read FCursor write SetCursor default crDefault;
         property PopupMenu : TPopupMenu read FPopupMenu write SetPopupMenu;

         property OnClose : TCloseEvent read FOnClose write SetOnClose;
         property OnKeyUp : TKeyEvent read FOnKeyUp write SetOnKeyUp;
         property OnKeyDown : TKeyEvent read FOnKeyDown write SetOnKeyDown;
         property OnKeyPress : TKeyPressEvent read FOnKeyPress write SetOnKeyPress;
         property OnCloseQuery : TCloseQueryEvent read FOnCloseQuery write SetOnCloseQuery;
         property OnClick : TNotifyEvent read FOnClick write SetOnClick;
         property OnDblClick : TNotifyEvent read FOnDblClick write SetOnDblClick;
         property OnMouseDown : TMouseEvent read FOnMouseDown write SetOnMouseDown;
         property OnMouseUp : TMouseEvent read FOnMouseUp write SetOnMouseUp;
         property OnMouseMove : TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
         property OnMouseWheel : TMouseWheelEvent read FOnMouseWheel write SetOnMouseWheel;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL1x, SysUtils, GLWin32Context, GLCrossPlatform, GLScreen;

const
   cScreenDepthToBPP : array [sd8bits..sd32bits] of Integer = (8, 16, 24, 32);

// ------------------
// ------------------ TGLFullScreenViewer ------------------
// ------------------

// Create
//
constructor TGLFullScreenViewer.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Width:=800;
   Height:=600;
   FScreenDepth:=sd32bits;
   FVSync:=vsmSync;
   FCursor:=crDefault;
   Buffer.ViewerBeforeRender:=DoBeforeRender;
end;

// Destroy
//
destructor TGLFullScreenViewer.Destroy;
begin
   Active:=False;
   inherited Destroy;
end;

// DoBeforeRender
//
procedure TGLFullScreenViewer.DoBeforeRender(Sender : TObject);
begin
   SetupVSync(VSync);
end;

// DoBufferChange
//
procedure TGLFullScreenViewer.DoBufferChange(Sender : TObject);
begin
   if Assigned(FForm) and (not Buffer.Rendering) then
//      Render;
      FForm.Invalidate;
end;

// DoBufferStructuralChange
//
procedure TGLFullScreenViewer.DoBufferStructuralChange(Sender : TObject);
begin
   if Active and (FUpdateCount=0) then begin
      Shutdown;
      Startup;
   end;
end;

// PrepareGLContext
//
procedure TGLFullScreenViewer.PrepareGLContext;
begin
   // nothing yet
end;

// Render
//
procedure TGLFullScreenViewer.Render(baseObject : TGLBaseSceneObject = nil);
begin
   LoadOpenGL;
   if Buffer.RenderingContext=nil then begin
      Buffer.CreateRC(0, False);
   end;
   Buffer.Render(baseObject);
end;

// BeginUpdate
//
procedure TGLFullScreenViewer.BeginUpdate;
begin
   Inc(FUpdateCount);
end;

// EndUpdate
//
procedure TGLFullScreenViewer.EndUpdate;
begin
   Dec(FUpdateCount);
   if FUpdateCount=0 then begin
      if Active then DoBufferStructuralChange(Self)
   end else if FUpdateCount<0 then begin
      FUpdateCount:=0;
      Assert(False, 'Unbalanced Begin/EndUpdate');
   end;
end;

// UseCurrentResolution
//
procedure TGLFullScreenViewer.UseCurrentResolution;
begin
   BeginUpdate;
   try
      Width:=Screen.Width;
      Height:=Screen.Height;
      case GetCurrentColorDepth of
         24 : ScreenDepth:=sd24bits;
         16 : ScreenDepth:=sd16bits;
         8 : ScreenDepth:=sd8bits;
      else
         // highest depth possible otherwise
         ScreenDepth:=sd32bits;
      end;
   finally
      EndUpdate;
   end;
end;

// SetActive
//
procedure TGLFullScreenViewer.SetActive(const val : Boolean);
begin
   if val<>FActive then begin
      if FActive then
         ShutDown
      else Startup;
   end;
end;

// Startup
//
procedure TGLFullScreenViewer.Startup;
var
   res : TResolution;
   dc : HDC;
begin
   Assert(FForm=nil);

   res:=GetIndexFromResolution(Width, Height, cScreenDepthToBPP[ScreenDepth]);
   if res=0 then
      raise Exception.Create('Unsupported video mode');

   FForm:=TForm.Create(nil);
   with FForm do begin
      if StayOnTop then
         FormStyle:=fsStayOnTop
      else FormStyle:=fsNormal;

      // Following lines doesn't seem to work on ATI hardware,
      // so we do it via API calls
      //  BorderStyle:=bsNone;
      BorderStyle:=bsSizeable;
      SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);

      Cursor:=Self.Cursor;
      PopupMenu:=Self.PopupMenu;

      ClientWidth:=Self.Width;
      ClientHeight:=Self.Height;
      WindowState:=wsMaximized;

      BindFormEvents;
      FOldWndProc:=WindowProc;
      WindowProc:=WndProc;
   end;

   // Hides Taskbar
   ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);

   // Switch video mode
   if (Screen.Width<>Width) or (Screen.Height<>Height)
         or (GetCurrentColorDepth<>cScreenDepthToBPP[ScreenDepth]) then begin
      SetFullscreenMode(res, FRefreshRate);
      FSwitchedResolution:=True;
   end else FSwitchedResolution:=False;

   FForm.Show;

   Buffer.Resize(Width, Height);
   dc:=GetDC(FForm.Handle);
   Buffer.CreateRC(dc, False);

   // todo
   FActive:=True;
end;

// Shutdown
//
procedure TGLFullScreenViewer.Shutdown;
var
   f : TForm;
begin
   try
      Buffer.DestroyRC;
      f:=FForm;
      FForm:=nil;
      f.WindowProc:=FOldWndProc;
      f.Release;
   finally
      // attempt that, at the very least...
      if FSwitchedResolution then
         RestoreDefaultMode;
   end;

   // Restore Taskbar
   ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_SHOWNA);
   
   FActive:=False;
end;

// BindFormEvents
//
procedure TGLFullScreenViewer.BindFormEvents;
begin
   if Assigned(FForm) then with FForm do begin
      OnMouseDown:=FOnMouseDown;
      OnMouseUp:=FOnMouseUp;
      OnMouseMove:=FOnMouseMove;
      OnMouseWheel:=FOnMouseWheel;
      OnClick:=FOnClick;
      OnDblClick:=FOnDblClick;
      OnPaint:=DoPaint;
      OnCloseQuery:=DoCloseQuery;
      OnClose:=FOnClose;
      OnActivate:=DoActivate;
      OnDeactivate:=DoDeactivate;
      OnKeyUp:=FOnKeyUp;
      OnKeyDown:=FOnKeyDown;
      OnKeyPress:=FOnKeyPress;
   end;
end;

// DoCloseQuery
//
procedure TGLFullScreenViewer.DoCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   if Assigned(FOnCloseQuery) then
      FOnCloseQuery(Sender, CanClose);
   if CanClose then Shutdown;
end;

// DoPaint
//
procedure TGLFullScreenViewer.DoPaint(Sender : TObject);
begin
   if Assigned(FForm) then
      Render;
end;

// WndProc
//
procedure TGLFullScreenViewer.WndProc(var Message: TMessage);
begin
   case Message.Msg of
      WM_ERASEBKGND : begin
         Message.Result:=1;   // do nothing!
      end;
   else
      FOldWndProc(Message);
   end;
end;

// DoActivate
//
procedure TGLFullScreenViewer.DoActivate(Sender : TObject);
begin
   if Assigned(FForm) and StayOnTop then
      FForm.FormStyle:=fsStayOnTop;
end;

// DoDeactivate
//
procedure TGLFullScreenViewer.DoDeactivate(Sender : TObject);
begin
   if Assigned(FForm) and StayOnTop then
      FForm.FormStyle:=fsNormal;
end;

// SetScreenDepth
//
procedure TGLFullScreenViewer.SetScreenDepth(const val : TGLScreenDepth);
begin
   if FScreenDepth<>val then begin
      FScreenDepth:=val;
      DoBufferStructuralChange(Self);
   end;
end;

// SetStayOnTop
//
procedure TGLFullScreenViewer.SetStayOnTop(const val : Boolean);
begin
   if val<>FStayOnTop then begin
      FStayOnTop:=val;
      DoBufferStructuralChange(Self);
   end;
end;

// SetOnCloseQuery
//
procedure TGLFullScreenViewer.SetOnCloseQuery(const val : TCloseQueryEvent);
begin
   FOnCloseQuery:=val; // this one uses a special binding
end;

// SetOnClose
//
procedure TGLFullScreenViewer.SetOnClose(const val : TCloseEvent);
begin
   FOnClose:=val;
   BindFormEvents;
end;

// SetOnKeyPress
//
procedure TGLFullScreenViewer.SetOnKeyPress(const val : TKeyPressEvent);
begin
   FOnKeyPress:=val;
   BindFormEvents;
end;

// SetOnKeyUp
//
procedure TGLFullScreenViewer.SetOnKeyUp(const val : TKeyEvent);
begin
   FOnKeyUp:=val;
   BindFormEvents;
end;

// SetOnKeyDown
//
procedure TGLFullScreenViewer.SetOnKeyDown(const val : TKeyEvent);
begin
   FOnKeyDown:=val;
   BindFormEvents;
end;

// SetOnMouseWheel
//
procedure TGLFullScreenViewer.SetOnMouseWheel(const val : TMouseWheelEvent);
begin
   FOnMouseWheel:=val;
   BindFormEvents;
end;

// SetOnClick
//
procedure TGLFullScreenViewer.SetOnClick(const val : TNotifyEvent);
begin
   FOnClick:=val;
   BindFormEvents;
end;

// SetOnDblClick
//
procedure TGLFullScreenViewer.SetOnDblClick(const val : TNotifyEvent);
begin
   FOnDblClick:=val;
   BindFormEvents;
end;

// SetOnMouseMove
//
procedure TGLFullScreenViewer.SetOnMouseMove(const val : TMouseMoveEvent);
begin
   FOnMouseMove:=val;
   BindFormEvents;
end;

// SetOnMouseDown
//
procedure TGLFullScreenViewer.SetOnMouseDown(const val : TMouseEvent);
begin
   FOnMouseDown:=val;
   BindFormEvents;
end;

// SetOnMouseUp
//
procedure TGLFullScreenViewer.SetOnMouseUp(const val : TMouseEvent);
begin
   FOnMouseUp:=val;
   BindFormEvents;
end;

// SetCursor
//
procedure TGLFullScreenViewer.SetCursor(const val : TCursor);
begin
   if val<>FCursor then begin
      FCursor:=val;
      if Assigned(FForm) then
         FForm.Cursor:=val;
   end;
end;

// SetPopupMenu
//
procedure TGLFullScreenViewer.SetPopupMenu(const val : TPopupMenu);
begin
   if val<>FPopupMenu then begin
      FPopupMenu:=val;
      if Assigned(FForm) then
         FForm.PopupMenu:=val;
   end;
end;

// GetHandle
//
function TGLFullScreenViewer.GetHandle : HWND;
begin
   if Assigned(FForm) then
      Result:=FForm.Handle
   else Result:=0;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClasses([TGLSceneViewer, TGLFullScreenViewer]);

end.

