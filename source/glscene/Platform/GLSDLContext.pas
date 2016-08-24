{: GLSDLContext<p>

   SDL specific Context and Viewer.<p>

   NOTA: SDL notifies use of context destruction *after* it happened, this prevents
         clean release of allocated stuff and requires a temporary switch to
         "ignore OpenGL errors" mode during destruction, thus potentially
         leaking memory (depending on hardware drivers willingness to perform
         automatic releases)<p>

   <b>History : </b><font size=-1><ul>
      <li>12/12/01 - EG - Creation
   </ul></font>
}
unit GLSDLContext;

interface

uses Classes, SysUtils, GLContext, SDLWindow, GLScene, SDL;

type

   // TGLSDLViewer
   //
   {: A viewer using SDL.<p>
      Beware: only one at a time, no other viewers allowed!<br>
      Will also close the application when the window is closed! }
   TGLSDLViewer = class (TGLNonVisualViewer)
      private
         { Private Declarations }
         FCaption : String;
         FOnSDLEvent : TSDLEvent;
         FOnEventPollDone : TNotifyEvent;
         FOnResize : TNotifyEvent;

      protected
         { Protected Declarations }
         procedure SetCaption(const val : String);

         procedure DoOnOpen(sender : TObject);
         procedure DoOnClose(sender : TObject);
         procedure DoOnResize(sender : TObject);
         procedure DoOnSDLEvent(sender : TObject; const event : TSDL_Event);
         procedure DoOnEventPollDone(sender : TObject);

         procedure DoBufferStructuralChange(Sender : TObject); override;
         procedure PrepareGLContext; override;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Render(baseObject : TGLBaseSceneObject = nil); override;

         function Active : Boolean;

      published
         { Public Declarations }
         property Caption : String read FCaption write SetCaption;

         property OnResize : TNotifyEvent read FOnResize write FOnResize;

         {: Fired whenever an SDL Event is polled.<p>
            SDL_QUITEV and SDL_VIDEORESIZE are not passed to this event handler,
            they are passed via OnClose and OnResize respectively. }
         property OnSDLEvent : TSDLEvent read FOnSDLEvent write FOnSDLEvent;
         {: Fired whenever an event polling completes with no events left to poll. }
         property OnEventPollDone : TNotifyEvent read FOnEventPollDone write FOnEventPollDone;
   end;

   // TGLSDLContext
   //
   {: A context driver for OpenGL via SDL (libsdl.org).<p>
      Due to limitations of SDL:<ul>
      <li>you may have only one SDL window opened at any time (you cannot
         have memory viewers)
      <li>closing the SDL window will terminate the application
      </ul> }
   TGLSDLContext = class (TGLScreenControlingContext)
      private
         { Private Declarations }
         FSDLWin : TSDLWindow;
         FSimulatedValidity : Boolean; // Hack around SDL's post-notified destruction of context

      protected
         { Protected Declarations }
         procedure DoCreateContext(outputDevice : Integer); override;
         procedure DoCreateMemoryContext(outputDevice, width, height : Integer); override;
         procedure DoShareLists(aContext : TGLContext); override;
         procedure DoDestroyContext; override;
         procedure DoActivate; override;
         procedure DoDeactivate; override;

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;

         function IsValid : Boolean; override;
         procedure SwapBuffers; override;

         function RenderOutputDevice : Integer; override;

         property SDLWindow : TSDLWindow read FSDLWin;
   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses OpenGL1x, GLCrossPlatform, XOpenGL;

procedure Register;
begin
	RegisterComponents('GLScene', [TGLSDLViewer]);
end;

// ------------------
// ------------------ TGLSDLViewer ------------------
// ------------------

// Create
//
constructor TGLSDLViewer.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Width:=640;
   Height:=480;
end;

// Destroy
//
destructor TGLSDLViewer.Destroy;
begin
   inherited Destroy;
end;

// DoBufferStructuralChange
//
procedure TGLSDLViewer.DoBufferStructuralChange(Sender : TObject);
begin
   // ignore that, supporting it with SDL is not very praticable as of now...
end;

// PrepareGLContext
//
procedure TGLSDLViewer.PrepareGLContext;
begin
   with Buffer.RenderingContext as TGLSDLContext do begin
      Width:=Self.Width;
      Height:=Self.Height;
      with FSDLWin do begin
         Caption:=Self.Caption;
         OnOpen:=DoOnOpen;
         OnClose:=DoOnClose;
         OnResize:=DoOnResize;
         OnSDLEvent:=DoOnSDLEvent;
         OnEventPollDone:=DoOnEventPollDone;
      end;
   end;
end;

// Render
//
procedure TGLSDLViewer.Render(baseObject : TGLBaseSceneObject = nil);
begin
   LoadOpenGL;
   if Buffer.RenderingContext=nil then begin
      Buffer.CreateRC(0, False);
   end;
   Buffer.Render(baseObject);
end;

// Active
//
function TGLSDLViewer.Active : Boolean;
begin
   Result:=Assigned(Buffer.RenderingContext) and Buffer.RenderingContext.IsValid;
end;

// SetCaption
//
procedure TGLSDLViewer.SetCaption(const val : String);
begin
   if val<>FCaption then begin
      FCaption:=val;
      if Buffer.RenderingContext<>nil then
         with Buffer.RenderingContext as TGLSDLContext do
            if Assigned(FSDLWin) then
               FSDLWin.Caption:=FCaption;
   end;
end;

// DoOnOpen
//
procedure TGLSDLViewer.DoOnOpen(sender : TObject);
begin
   // nothing yet
end;

// DoOnClose
//
procedure TGLSDLViewer.DoOnClose(sender : TObject);
begin
   // nothing yet
end;

// DoOnResize
//
procedure TGLSDLViewer.DoOnResize(sender : TObject);
begin
   with Buffer.RenderingContext as TGLSDLContext do begin
      Self.Width:=FSDLWin.Width;
      Self.Height:=FSDLWin.Height;
      Buffer.Resize(FSDLWin.Width, FSDLWin.Height);
   end;
   if Assigned(FOnResize) then
      FOnResize(Self);
end;

// DoOnSDLEvent
//
procedure TGLSDLViewer.DoOnSDLEvent(sender : TObject; const event : TSDL_Event);
begin
   if Assigned(FOnSDLEvent) then
      FOnSDLEvent(sender, event);
end;

// DoOnEventPollDone
//
procedure TGLSDLViewer.DoOnEventPollDone(sender : TObject);
begin
   if Assigned(FOnEventPollDone) then
      FOnEventPollDone(sender);
end;

// ------------------
// ------------------ TGLSDLContext ------------------
// ------------------

// Create
//
constructor TGLSDLContext.Create;
begin
   inherited Create;
   FSDLWin:=TSDLWindow.Create(nil);
end;

// Destroy
//
destructor TGLSDLContext.Destroy;
var
   oldIgnore : Boolean;
begin
   oldIgnore:=vIgnoreOpenGLErrors;
   FSimulatedValidity:=True;
   vIgnoreOpenGLErrors:=True;
   try
      inherited Destroy;
   finally
      vIgnoreOpenGLErrors:=oldIgnore;
      FSimulatedValidity:=False;
   end;
   FreeAndNil(FSDLWin);
end;

// DoCreateContext
//
procedure TGLSDLContext.DoCreateContext(outputDevice : Integer);
var
   sdlOpt : TSDLWindowOptions;
begin
   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;

   FSDLWin.Width:=Width;
   FSDLWin.Height:=Height;
   if ColorBits>16 then
      FSDLWin.PixelDepth:=vpd24bits
   else FSDLWin.PixelDepth:=vpd16bits;

   sdlOpt:=[voOpenGL, voHardwareAccel];
   if FullScreen then
      sdlOpt:=sdlOpt+[voFullScreen]
   else sdlOpt:=sdlOpt+[voResizable];
   if rcoDoubleBuffered in Options then
      sdlOpt:=sdlOpt+[voDoubleBuffer];
   if StencilBits>0 then
      sdlOpt:=sdlOpt+[voStencilBuffer];

   FSDLWin.Open;
   if not FSDLWin.Active then
      raise Exception.Create('SDLWindow open failed.');

   xglMapTexCoordToNull;
   ReadExtensions;
   ReadImplementationProperties;
   xglMapTexCoordToMain;
end;

// DoCreateMemoryContext
//
procedure TGLSDLContext.DoCreateMemoryContext(outputDevice, width, height : Integer);
begin
   raise Exception.Create(ClassName+': Memory contexts not supported');
end;

// DoShareLists
//
procedure TGLSDLContext.DoShareLists(aContext : TGLContext);
begin
   // nothing (only one context at all times... no need to share)
end;

// DoDestroyContext
//
procedure TGLSDLContext.DoDestroyContext;
begin
   // Beware, SDL will also terminate the application
   FSDLWin.Close;
end;

// DoActivate
//
procedure TGLSDLContext.DoActivate;
begin
   // nothing particular (only one context, always active)
end;

// Deactivate
//
procedure TGLSDLContext.DoDeactivate;
begin
   // nothing particular (only one context, always active)
end;

// IsValid
//
function TGLSDLContext.IsValid : Boolean;
begin
   Result:=(Assigned(FSDLWin) and (FSDLWin.Active)) or FSimulatedValidity;
end;

// SwapBuffers
//
procedure TGLSDLContext.SwapBuffers;
begin
   FSDLWin.SwapBuffers;
end;

// RenderOutputDevice
//
function TGLSDLContext.RenderOutputDevice : Integer;
begin
   // unsupported
   Result:=0;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLSDLViewer);
   RegisterGLContextClass(TGLSDLContext);

end.
