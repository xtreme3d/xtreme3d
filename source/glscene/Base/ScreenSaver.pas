//
// This unit is part of the GLScene Project, http://glscene.org
//
{: ScreenSaver<p>

	Component for making screen-savers an easy task<p>

	<b>History : </b><font size=-1><ul>
      <li>09/07/01 - Egg - Fix in PreviewSaver (from Marco Dissel)
      <li>12/04/00 - Egg - Added ssoEnhancedMouseMoveDetection
		<li>11/04/00 - Egg - Creation
	</ul></font>

	Parts of this code are based on DeskSpin sample by	Tom Nuydens
		(http://www.gamedeveloper.org/delphi3d/).<p>

	NB : The password stuff does NOT work under NT, dll references I found in
			Tom's sample simply did not exist under my NT4...
}
unit ScreenSaver;

interface

{$i GLScene.inc}
{$IFDEF LINUX}{$Message Error 'Unit not supported'}{$ENDIF LINUX}

uses Windows, Classes, Controls, Forms, Extctrls;

type

   // TScreenSaverOptions
   //
   {: Options d'automatisation du screen-saver.<p>
      <ul>
      <li>ssoAutoAdjustFormProperties : all relevant properties of main form
			will be auto-adjusted (form style, border style, form size and for
			preview, ParentWindow).
      <li>ssoAutoHookKeyboardEvents : hooks to main form's OnKeyPress and closes
         screen saver when a key is pressed (form's KeyPreview is also set to True)
      <li>ssoAutoHookMouseEvents : hooks to main form's OnMouseMove and closes
         screen saver when mouse is moved (you mays have to handle other mouse
         move events manually if you have placed components on the form)
      <li>ssoEnhancedMouseMoveDetection : gets the mouse position every half-second
         and closes the saver if position changed (uses GetCursorPos and a TTimer)
      </ul> }
   TScreenSaverOption = (ssoAutoAdjustFormProperties, ssoAutoHookKeyboardEvents,
                         ssoAutoHookMouseEvents, ssoEnhancedMouseMoveDetection);
	TScreenSaverOptions = set of TScreenSaverOption;

const
   cDefaultScreenSaverOptions = [ssoAutoAdjustFormProperties, ssoAutoHookKeyboardEvents,
                                 ssoEnhancedMouseMoveDetection];

type

   // TScreenSaverPreviewEvent
   //
   {: This event is fired when screen saver should start in preview mode.<p>
      The passed hwnd is that of the small preview window in Windows Display
      Properties (or any other screen-saver previewing utility, so don't
      assume width/height is constant/universal or whatever). }
   TScreenSaverPreviewEvent = procedure (Sender: TObject; previewHwnd : HWND) of object;

	// TScreenSaver
	//
	{: Drop this component on your main form to make it a screensaver.<p>
		You'll also need to change the extension from ".exe" to ".scr" (in the
		project options / application tab).<p>
		How this component works :<ul>
		<li>At design-time, the only event you may want to hook is
			OnPropertiesRequested (to diplay your screen-saver's config dialog,
			if you don't have one, at least fill in the AboutString property
			and it will be used in a ShowMessage)
		<li>At run-time, once its props are loaded, this component will parse the
			command line and trigger relevant events
		<li>Basicly, you only need to care about drawing in your main form's
			client area (in a resolution/size independant way if possible)
		</ul><br>
		There is no real difference between execution and preview modes, except
		for the events fired... and the size of the form :). }
	TScreenSaver = class (TComponent)
		private
			{ Private Declarations }
			mouseEventsToIgnore : Integer;
			FHonourWindowsPassword : Boolean;
         FOptions : TScreenSaverOptions;
			FOnPropertiesRequested : TNotifyEvent;
			FOnExecute : TNotifyEvent;
			FOnPreview : TScreenSaverPreviewEvent;
			FOnCloseQuery : TCloseQueryEvent;
			FAboutString : String;
			FInPreviewMode : Boolean;
         mouseTimer : TTimer; // alocated only if necessary
         lastMousePosition : TPoint;
      FMutex: THandle;
		protected
			{ Protected Declarations }
			procedure Loaded; override;

			procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
			procedure FormKeyPress(Sender: TObject; var Key: Char);
         procedure OnMouseTimer(Sender: TObject);

			procedure ConfigureSaver;
			procedure PreviewSaver;
			procedure ExecuteSaver;

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;

			{: Invokes the standard Windows dialog to set the password.<p>
				May be invoked from your Properties/Configuration dialog. }
			procedure SetPassword;
			{: Properly handles request to close the main window.<p>
            Returns True if the Close request wasn't canceled (by event or
            password fail) and will actually happen.
				Use this if you implemented specific screen-saver exiting in your
				main form.<br>
				It first triggers the OnCloseQuery, where the close request can be
				canceled, if this passed, the password is checked if there is any,
				then sends a WM_CLOSE to the saver windows. }
			function CloseSaver : Boolean;

			{: True if the screen-save is in preview mode.<p>
				Valid only when the TScreenSaver has completed loading. }
			property InPreviewMode : Boolean read FInPreviewMode;

		published
			{ Published Declarations }
			property Options : TScreenSaverOptions read FOptions write FOptions default cDefaultScreenSaverOptions;
			{: If True, windows screen-saver password is checked before closing.<p>
				You may be wanting to set this prop to false if you're using your
				own password scheme or do not want any password to be set. }
			property HonourWindowsPassword : Boolean read FHonourWindowsPassword write FHonourWindowsPassword default True;
			{: This string is displayed if OnPropertiesRequested is not used.<p>
				You may use it as a quick "AboutBox".  }
			property AboutString : String read FAboutString write FAboutString;

			{: Display the properties dialog when this event is triggered.<p>
				This event may be called before Delphi's form auto-creation is complete,
				and should not rely on auto-created dialogs/forms but create what
				needs be. }
			property OnPropertiesRequested : TNotifyEvent read FOnPropertiesRequested write FOnPropertiesRequested;
			{: Fired when the saver should start executing, after form props are adjusted. }
			property OnExecute : TNotifyEvent read FOnExecute write FOnExecute;
			{: Fired when preview is requested, after form props are adjusted. }
			property OnPreview : TScreenSaverPreviewEvent read FOnPreview write FOnPreview;
			{: Fired when screen-saver execution should close.<p>
				It is invoked before querying for password (if there is a password). }
			property OnCloseQuery : TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;

	end;

procedure Register;

{: Invokes the standard Windows dialog to set the password.<p>
	May be invoked from your Properties/Configuration dialog. }
procedure SetScreenSaverPassword;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils, Registry, Dialogs, Messages;

procedure Register;
begin
	RegisterComponents('GLScene Utils', [TScreenSaver]);
end;

// GetSystemDirectory
//
{: Returns system path and makes sure there is a trailing '\'.<p> }
function GetSystemDirectory : String;
var
	newLength : Integer;
begin
	SetLength(Result, MAX_PATH);
	newLength:=Windows.GetSystemDirectory(PChar(Result), MAX_PATH);
	SetLength(Result, newLength);
	if Copy(Result, newLength, 1)<>'\' then
		Result:=Result+'\';
end;

// SetScreenSaverPassword
//
procedure SetScreenSaverPassword;
type
	TSetPwdFunc = function(a: PChar; ParentHandle: THandle; b, c: Integer): Integer; stdcall;
var
	mprDll : THandle;
	p : TSetPwdFunc;
begin
	mprDll:=LoadLibrary(PChar(GetSystemDirectory+'mpr.dll'));
	if mprDll <> 0 then begin
		p:=GetProcAddress(mprDll, 'PwdChangePasswordA');
		if Assigned(p) then
			p('SCRSAVE', StrToIntDef(ParamStr(2), 0), 0, 0);
		FreeLibrary(mprDll);
	end;
end;


// ------------------
// ------------------ TScreenSaver ------------------
// ------------------

// Create
//
constructor TScreenSaver.Create(AOwner : TComponent);
begin
	inherited;
	mouseEventsToIgnore:=5;
	FOptions:=cDefaultScreenSaverOptions;
	FHonourWindowsPassword:=True;
  FMutex := 0;
end;

// Destroy
//
destructor TScreenSaver.Destroy;
begin
   // mouseTimer is owned, it'll be automatically destroyed if created

  CloseHandle(FMutex);
	inherited;
end;

// Loaded
//
procedure TScreenSaver.Loaded;
var
	param : String;
begin
	inherited;
	if not (csDesigning in ComponentState) then begin
		// Read the command line parameters to determine the saver mode
		if ParamCount>0 then begin
			// Ignore the parameter's leading '-' or '/'
			param:=UpperCase(Copy(ParamStr(1), 2, 1));
			if param='C' then
				ConfigureSaver
			else if ParamCount>1 then
				if param='A' then begin
					SetPassword;
					Application.Terminate;
				end else if param='P' then
					PreviewSaver
				else ExecuteSaver
			else ExecuteSaver;
		end else ConfigureSaver;
	end;
end;

// ConfigureSaver
//
procedure TScreenSaver.ConfigureSaver;
begin
	if Assigned(FOnPropertiesRequested) then begin
		OnPropertiesRequested(Self);
		Application.Terminate;
	end else if FAboutString<>'' then
		ShowMessage(FAboutString);
end;

// SetPassword
//
procedure TScreenSaver.SetPassword;
begin
	SetScreenSaverPassword;
end;

// PreviewSaver
//
procedure TScreenSaver.PreviewSaver;
var
	frm : TForm;
	previewHwnd : HWND;
	previewRect : TRect;
begin
	FInPreviewMode:=True;
   previewHwnd:=StrToIntDef(ParamStr(2), 0);
  	if ssoAutoAdjustFormProperties in FOptions then begin
		frm:=(Owner as TForm);
		if Assigned(frm) then begin
			GetWindowRect(previewHwnd, previewRect);
			with previewRect do
				frm.SetBounds(0, 0, Right-Left, Bottom-Top);
			frm.BorderStyle:=bsNone;
			frm.ParentWindow:=previewHwnd;
			frm.Cursor:=crNone;
         frm.Visible:=False;
		end;
	end;
	if Assigned(FOnPreview) then
		FOnPreview(Self, previewHwnd);
end;

// ExecuteSaver
//
procedure TScreenSaver.ExecuteSaver;
var
	frm : TForm;
begin
  FMutex := CreateMutex(nil, True, 'GLScene::ScreenSaver');
  if (FMutex <> 0) and (GetLastError = 0) then begin
	frm:=(Owner as TForm);
	if Assigned(frm) then begin
   	if ssoAutoAdjustFormProperties in FOptions then begin
			frm.FormStyle:=fsStayOnTop;
			frm.WindowState:=wsMaximized;
			frm.BorderStyle:=bsNone;
		end;
		if ssoAutoHookKeyboardEvents in FOptions then begin
			frm.OnKeyPress:=FormKeyPress;
			frm.KeyPreview:=True;
		end;
		if ssoAutoHookMouseEvents in FOptions then
			frm.OnMouseMove:=FormMouseMove;
		if ssoEnhancedMouseMoveDetection in FOptions then begin
			mouseTimer:=TTimer.Create(Self);
			mouseTimer.Interval:=500;
			mouseTimer.OnTimer:=OnMouseTimer;
			mouseTimer.Enabled:=True;
			OnMouseTimer(Self);
		end;
	end;
	if Assigned(FOnExecute) then
		FOnExecute(Self);
	ShowCursor(False);
  end
  else
    Application.Terminate;
end;

// CloseSaver
//
function TScreenSaver.CloseSaver : Boolean;
type
	TPwdProc = function(Parent : THandle): Boolean; stdcall;
const
	cScreenSaveUsePassword = 'ScreenSaveUsePassword';
var
	reg : TRegistry;
	p : TPwdProc;
	pwdCpl : THandle;
begin
	Result:=True;
	if Assigned(FOnCloseQuery) then begin
		FOnCloseQuery(Self, Result);
		if not Result then Exit;
	end;
	// Try to close the saver, but check for a password first!
	// Check the registry to see if we should ask for a password.
	reg:=TRegistry.Create;
	try
		reg.RootKey:=HKEY_CURRENT_USER;
		if reg.OpenKey('Control Panel\Desktop', FALSE) then begin
			if reg.ValueExists(cScreenSaveUsePassword) then
				if reg.ReadInteger(cScreenSaveUsePassword) <> 0 then begin
					// We need to ask for the password!
					// The Passwords control panel exports a routine that we can use: VerifyScreenSavePwd()
					pwdCpl:=LoadLibrary(PChar(GetSystemDirectory+'password.cpl'));
					if pwdCpl<>0 then begin
						p:=GetProcAddress(pwdCpl, 'VerifyScreenSavePwd');
						Result:=p((Owner as TForm).Handle);
						FreeLibrary(pwdCpl);
					end;
				end;
		end;
	finally
		reg.Free;
	end;
	if Result then begin
		ShowCursor(True);
		SendMessage((Owner as TForm).Handle, WM_CLOSE, 0, 0);
	end;
end;

// FormMouseMove
//
procedure TScreenSaver.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
	if mouseEventsToIgnore<=0 then
		CloseSaver
	else Dec(mouseEventsToIgnore);
end;

// FormKeyPress
//
procedure TScreenSaver.FormKeyPress(Sender: TObject; var Key: Char);
begin
	CloseSaver;
end;

// OnMouseTimer
//
procedure TScreenSaver.OnMouseTimer(Sender: TObject);
var
   mousePos : TPoint;
begin
   GetCursorPos(mousePos);
   if Sender<>Self then
      if (mousePos.x<>lastMousePosition.x) or (mousePos.y<>lastMousePosition.y) then
         if CloseSaver then
            mouseTimer.Enabled:=False;
   lastMousePosition:=mousePos;
end;

end.
