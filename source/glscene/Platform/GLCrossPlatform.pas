{: GLCrossPlatform<p>

	Cross platform support functions and types for GLScene.<p>

   Ultimately, *no* cross-platform or cross-version defines should be present
   in the core GLScene units, and have all moved here instead.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>08/07/04 - LR - Added clBlack  
      <li>03/07/04 - LR - Added constant for Keyboard (glKey_TAB, ...)
                          Added function GLOKMessageBox to avoid the uses of Forms
                          Added other abstraction calls
                          Added procedure ShowHTMLUrl for unit Info.pas
                          Added GLShowCursor, GLSetCursorPos, GLGetCursorPos,
                          GLGetScreenWidth, GLGetScreenHeight for GLNavigation
                          Added GLGetTickCount for GLFPSMovement
      <li>28/06/04 - LR - Added TGLTextLayout, GLLoadBitmapFromInstance
                          Added GetDeviceCapabilities to replace the old function
      <li>30/05/03 - EG - Added RDTSC and RDTSC-based precision timing for non-WIN32
      <li>22/01/02 - EG - Added OpenPictureDialog, ApplicationTerminated
      <li>07/01/02 - EG - Added QuestionDialog and SavePictureDialog,
                          Added PrecisionTimer funcs 
      <li>06/12/01 - EG - Added several abstraction calls
      <li>31/08/01 - EG - Creation
	</ul></font>
}
unit GLCrossPlatform;

interface

{$include GLScene.inc}

{$IFDEF MSWINDOWS}
uses
  Windows, Classes, SysUtils, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtDlgs, Consts;
{$ENDIF}
{$IFDEF LINUX}
uses
  libc, Classes, SysUtils, Qt, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, Types, QConsts;
{$ENDIF}

type

   // Several aliases to shield us from the need of ifdef'ing between
   // the "almost cross-platform" units like Graphics/QGraphics etc.
   // Gives a little "alien" look to names, but that's the only way around :(

   TGLPoint = TPoint;
   PGLPoint = ^TGLPoint;
   TGLRect = TRect;
   PGLRect = ^TGLRect;
   TDelphiColor = TColor;

   TGLPicture = TPicture;
   TGLGraphic = TGraphic;
   TGLBitmap = TBitmap;
   TGraphicClass = class of TGraphic;

   TGLTextLayout = (tlTop, tlCenter, tlBottom); // idem TTextLayout;

   TGLMouseButton = (mbLeft, mbRight, mbMiddle); // idem TMouseButton;
   TGLMouseEvent = procedure(Sender: TObject; Button: TGLMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
   TGLMouseMoveEvent = TMouseMoveEvent;
   TGLKeyEvent = TKeyEvent;
   TGLKeyPressEvent = TKeyPressEvent;

{$ifdef GLS_DELPHI_5}
   EGLOSError = EWin32Error;
{$else}
   {$ifdef FPC}
      EGLOSError = EWin32Error;
   {$else}
      EGLOSError = EOSError;
   {$endif}
{$endif}

const
{$ifdef WIN32}
   glpf8Bit = pf8bit;
   glpf24bit = pf24bit;
   glpf32Bit = pf32bit;
   glpfDevice = pfDevice;
{$endif}
{$ifdef LINUX}
   glpf8Bit = pf8bit;
   glpf24bit = pf32bit;
   glpf32Bit = pf32bit;
   glpfDevice = pf32bit;
{$endif}

   // standard colors
{$ifdef WIN32}
   clBtnFace = Graphics.clBtnFace;
   clRed = Graphics.clRed;
   clGreen = Graphics.clGreen;
   clBlue = Graphics.clBlue;
   clSilver = Graphics.clSilver;
   clBlack = Graphics.clBlack;
{$endif}
{$ifdef LINUX}
   clBtnFace = QGraphics.clBtnFace;
   clRed = QGraphics.clRed;
   clGreen = QGraphics.clGreen;
   clBlue = QGraphics.clBlue;
   clSilver = QGraphics.clSilver;
   clBlack = QGraphics.clBlack;
{$endif}

// standard keyboard
{$ifdef WIN32}
  glKey_TAB = VK_TAB;
  glKey_SPACE = VK_SPACE;
  glKey_RETURN = VK_RETURN;
  glKey_DELETE = VK_DELETE;
  glKey_LEFT = VK_LEFT;
  glKey_RIGHT = VK_RIGHT;
  glKey_HOME = VK_HOME;
  glKey_END = VK_END;
  glKey_CANCEL = VK_CANCEL;
  glKey_UP = VK_UP;
  glKey_DOWN = VK_DOWN;
{$endif}
{$ifdef LINUX}
  glKey_TAB = Key_Tab;
  glKey_SPACE = Key_Space;
  glKey_RETURN = Key_Return;
  glKey_DELETE = Key_Delete;
  glKey_LEFT = Key_Left;
  glKey_RIGHT = Key_Right;
  glKey_HOME = Key_Home;
  glKey_END = Key_End;
  glKey_CANCEL = Key_Escape;   // ?
  glKey_UP = Key_Up;
  glKey_DOWN = Key_DOWN;
{$endif}

// Several define from unit Consts
const
  glsAllFilter: string = sAllFilter;


function GLPoint(const x, y : Integer) : TGLPoint;

{: Builds a TColor from Red Green Blue components. }
function RGB(const r, g, b : Byte) : TColor;
{: Converts 'magic' colors to their RGB values. }
function ColorToRGB(color : TColor) : TColor;

function GetRValue(rgb: DWORD): Byte;
function GetGValue(rgb: DWORD): Byte;
function GetBValue(rgb: DWORD): Byte;
procedure InitWinColors;

function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
{: Increases or decreases the width and height of the specified rectangle.<p>
   Adds dx units to the left and right ends of the rectangle and dy units to
   the top and bottom. }
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);

{: Pops up a simple dialog with msg and an Ok button. }
procedure InformationDlg(const msg : String);
{: Pops up a simple question dialog with msg and yes/no buttons.<p>
   Returns True if answer was "yes". }
function QuestionDlg(const msg : String) : Boolean;
{: Posp a simple dialog with a string input. }
function InputDlg(const aCaption, aPrompt, aDefault : String) : String;

{: Pops up a simple save picture dialog. }
function SavePictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
{: Pops up a simple open picture dialog. }
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;

{: Returns True if the application has been terminated. }
function ApplicationTerminated : Boolean;

procedure RaiseLastOSError;

{$IFNDEF GLS_DELPHI_5_UP}
procedure FreeAndNil(var anObject);
{$ENDIF GLS_DELPHI_5_UP}

{: Number of pixels per logical inch along the screen width for the device.<p>
   Under Win32 awaits a HDC and returns its LOGPIXELSX. }
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
{: Number of bits per pixel for the current desktop resolution. }
function GetCurrentColorDepth : Integer;
{: Returns the number of color bits associated to the given pixel format. }
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;

{: Returns the bitmap's scanline for the specified row. }
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;

{: Suspends thread execution for length milliseconds.<p>
   If length is zero, only the remaining time in the current thread's time
   slice is relinquished. }
procedure Sleep(length : Cardinal);

{: Returns the current value of the highest-resolution counter.<p>
   If the platform has none, should return a value derived from the highest
   precision time reference available, avoiding, if possible, timers that
   allocate specific system resources. }
procedure QueryPerformanceCounter(var val : Int64);
{: Returns the frequency of the counter used by QueryPerformanceCounter.<p>
   Return value is in ticks per second (Hz), returns False if no precision
   counter is available. }
function QueryPerformanceFrequency(var val : Int64) : Boolean;

{: Starts a precision timer.<p>
   Returned value should just be considered as 'handle', even if it ain't so.
   Default platform implementation is to use QueryPerformanceCounter and
   QueryPerformanceFrequency, if higher precision references are available,
   they should be used. The timer will and must be stopped/terminated/released
   with StopPrecisionTimer. }
function StartPrecisionTimer : Int64;
{: Computes time elapsed since timer start.<p>
   Return time lap in seconds. }
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
{: Computes time elapsed since timer start and stop timer.<p>
   Return time lap in seconds. }
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
{: Returns the number of CPU cycles since startup.<p>
   Use the similarly named CPU instruction. }
function RDTSC : Int64;

procedure GLLoadBitmapFromInstance(ABitmap: TBitmap; AName: string);
function GLOKMessageBox(const Text, Caption: string): Integer;
procedure ShowHTMLUrl(Url: String);
procedure GLShowCursor(AShow: boolean);
procedure GLSetCursorPos(AScreenX, AScreenY: integer);
procedure GLGetCursorPos(var point: TGLPoint);
function GLGetScreenWidth:integer;
function GLGetScreenHeight:integer;
function GLGetTickCount:int64;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
uses
  ShellApi, GLTexture;
{$ENDIF}
{$IFDEF LINUX}

{$ENDIF}

var
   vInvPerformanceCounterFrequency : Double;
   vInvPerformanceCounterFrequencyReady : Boolean = False;

procedure GLLoadBitmapFromInstance(ABitmap: TBitmap; AName: string);
begin
{$IFDEF MSWINDOWS}
  ABitmap.Handle := LoadBitmap(HInstance, PChar(AName));
{$ENDIF}
{$IFDEF LINUX}
  ABitmap.LoadFromResourceName(HInstance, PChar(AName));
{$ENDIF}
end;

function GLOKMessageBox(const Text, Caption: string): Integer;
begin
{$IFDEF MSWINDOWS}
  result := Application.MessageBox(PChar(Text),PChar(Caption),MB_OK);
{$ENDIF}
{$IFDEF LINUX}
  result := integer(Application.MessageBox(Text,Caption));
{$ENDIF}
end;

procedure GLShowCursor(AShow: boolean);
begin
{$IFDEF MSWINDOWS}
  ShowCursor(AShow);
{$ENDIF}
{$IFDEF LINUX}
  {$MESSAGE Warn 'ShowCursor: Needs to be implemented'}
{$ENDIF}
end;

procedure GLSetCursorPos(AScreenX, AScreenY: integer);
begin
{$IFDEF MSWINDOWS}
  SetCursorPos(AScreenX, AScreenY);
{$ENDIF}
{$IFDEF LINUX}
  {$MESSAGE Warn 'SetCursorPos: Needs to be implemented'}
{$ENDIF}
end;

procedure GLGetCursorPos(var point: TGLPoint);
begin
{$IFDEF MSWINDOWS}
  GetCursorPos(point);
{$ENDIF}
{$IFDEF LINUX}
  {$MESSAGE Warn 'GetCursorPos: Needs to be implemented'}
{$ENDIF}
end;

function GLGetScreenWidth:integer;
begin
  result := Screen.Width;
end;

function GLGetScreenHeight:integer;
begin
  result := Screen.Height;
end;

function GLGetTickCount:int64;
begin
{$IFDEF MSWINDOWS}
  result := GetTickCount;
{$ENDIF}
{$IFDEF LINUX}
  QueryPerformanceCounter(result);
{$ENDIF}
end;

{$IFDEF LINUX}
function QueryCombo(const ACaption, APrompt: string; Alist:TStringList;
                          var Index: integer; var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Combo: TComboBox;
  Dialogfrms: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Scaled := false;
      Canvas.Font := Font;
      Dialogfrms := Point(Canvas.TextWidth('L'),Canvas.TextHeight('R'));
      BorderStyle := fbsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, Dialogfrms.X, 4);
      ClientHeight := MulDiv(63, Dialogfrms.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, Dialogfrms.X, 4);
        Top := MulDiv(8, Dialogfrms.Y, 8);
        Caption := APrompt;
      end;
      Combo := TComboBox.Create(Form);
      with Combo do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, Dialogfrms.Y, 8);
        Width := MulDiv(164, Dialogfrms.X, 4);
        DropDownCount := 3;
        Items.AddStrings(AList);
        Combo.ItemIndex := index;
      end;
      ButtonTop := MulDiv(41, Dialogfrms.Y, 8);
      ButtonWidth := MulDiv(50, Dialogfrms.X, 4);
      ButtonHeight := MulDiv(14, Dialogfrms.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, Dialogfrms.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
        TabOrder := 0;
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, Dialogfrms.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Combo.Text;
        index := Combo.ItemIndex;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

resourcestring
  sFileName = '/tmp/delete-me.txt';

// Code inspired from unit Misc.pas of TPlot component of Mat Ballard
function CheckForRPM(AnRPM: String): String;
var
  TmpFile: TStringList;
begin
  Result := '';
  TmpFile := TStringList.Create;
  Libc.system(PChar('rpm -ql ' + AnRPM + ' > ' + sFileName));
  TmpFile.LoadFromFile(sFileName);
  if (Length(TmpFile.Strings[0]) > 0) then
    if (Pos('not installed', TmpFile.Strings[0]) = 0) then
      Result := TmpFile.Strings[0];
  DeleteFile(sFileName);
  TmpFile.Free;
end;

function GetBrowser: String;
var
  Index: Integer;
  AProgram,
  ExeName: String;
  BrowserList: TStringList;
begin
{Get the $BROWSER environment variable:}
  ExeName := getenv('BROWSER');

  if (Length(ExeName) = 0) then
  begin
{Get the various possible browsers:}
    BrowserList := TStringList.Create;

    try
      if (FileExists('/usr/bin/konqueror')) then
        BrowserList.Add('/usr/bin/konqueror');

      AProgram := CheckForRPM('mozilla');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('netscape-common');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('opera');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('lynx');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);
      AProgram := CheckForRPM('links');
      if (Length(AProgram) > 0) then
        BrowserList.Add(AProgram);

      Index := 0;
      if QueryCombo('Browser Selection', 'Which Web Browser Program To Use ?',
        BrowserList, Index, AProgram) then
      begin
        ExeName := AProgram;
        Libc.putenv(PChar('BROWSER=' + ExeName));
      end;

    finally
      BrowserList.Free;
    end;
  end;

  Result := ExeName;
end;
{$ENDIF}

procedure ShowHTMLUrl(Url: String);
{$IFDEF LINUX}
var
  TheBrowser: String;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(Url), Nil, Nil, SW_SHOW);
{$ENDIF}
{$IFDEF LINUX}
  TheBrowser := GetBrowser;
{the ' &' means immediately continue:}
  if (Length(TheBrowser) > 0) then
    Libc.system(PChar(TheBrowser + ' ' + Url + ' &'));
{$ENDIF}
end;

// GLPoint
//
function GLPoint(const x, y : Integer) : TGLPoint;
begin
   Result.X:=x;
   Result.Y:=y;
end;

// RGB
//
function RGB(const r, g, b : Byte) : TColor;
begin
   Result:=(b shl 16) or (g shl 8) or r;
end;

// ColorToRGB
//
function ColorToRGB(color : TColor) : TColor;
begin
   {$ifdef MSWINDOWS}
   if color<0 then
      Result:=GetSysColor(color and $FF)
   else Result:=color;
   {$else}
   Result:=QGraphics.ColorToRGB(color);
   {$endif}
end;

// GetRValue
//
function GetRValue(rgb: DWORD): Byte;
begin
   Result:=Byte(rgb);
end;

// GetGValue
//
function GetGValue(rgb: DWORD): Byte;
begin
   Result:=Byte(rgb shr 8);
end;

// GetBValue
//
function GetBValue(rgb: DWORD): Byte;
begin
   Result:=Byte(rgb shr 16);
end;

// InitWinColors
//
procedure InitWinColors;
begin
   {$ifdef MSWINDOWS}
   clrScrollBar:=ConvertWinColor(clScrollBar);
   clrBackground:=ConvertWinColor(clBackground);
   clrActiveCaption:=ConvertWinColor(clActiveCaption);
   clrInactiveCaption:=ConvertWinColor(clInactiveCaption);
   clrMenu:=ConvertWinColor(clMenu);
   clrWindow:=ConvertWinColor(clWindow);
   clrWindowFrame:=ConvertWinColor(clWindowFrame);
   clrMenuText:=ConvertWinColor(clMenuText);
   clrWindowText:=ConvertWinColor(clWindowText);
   clrCaptionText:=ConvertWinColor(clCaptionText);
   clrActiveBorder:=ConvertWinColor(clActiveBorder);
   clrInactiveBorder:=ConvertWinColor(clInactiveBorder);
   clrAppWorkSpace:=ConvertWinColor(clAppWorkSpace);
   clrHighlight:=ConvertWinColor(clHighlight);
   clrHighlightText:=ConvertWinColor(clHighlightText);
   clrBtnFace:=ConvertWinColor(clBtnFace);
   clrBtnShadow:=ConvertWinColor(clBtnShadow);
   clrGrayText:=ConvertWinColor(clGrayText);
   clrBtnText:=ConvertWinColor(clBtnText);
   clrInactiveCaptionText:=ConvertWinColor(clInactiveCaptionText);
   clrBtnHighlight:=ConvertWinColor(clBtnHighlight);
   clr3DDkShadow:=ConvertWinColor(cl3DDkShadow);
   clr3DLight:=ConvertWinColor(cl3DLight);
   clrInfoText:=ConvertWinColor(clInfoText);
   clrInfoBk:=ConvertWinColor(clInfoBk);
   {$endif}
end;

// GLRect
//
function GLRect(const aLeft, aTop, aRight, aBottom : Integer) : TGLRect;
begin
   Result.Left:=aLeft;
   Result.Top:=aTop;
   Result.Right:=aRight;
   Result.Bottom:=aBottom;
end;

// InflateRect
//
procedure InflateGLRect(var aRect : TGLRect; dx, dy : Integer);
begin
   aRect.Left:=aRect.Left-dx;
   aRect.Right:=aRect.Right+dx;
   if aRect.Right<aRect.Left then
      aRect.Right:=aRect.Left;
   aRect.Top:=aRect.Top-dy;
   aRect.Bottom:=aRect.Bottom+dy;
   if aRect.Bottom<aRect.Top then
      aRect.Bottom:=aRect.Top;
end;

// IntersectGLRect
//
procedure IntersectGLRect(var aRect : TGLRect; const rect2 : TGLRect);
var
   a : Integer;
begin
   if (aRect.Left>rect2.Right) or (aRect.Right<rect2.Left)
      or (aRect.Top>rect2.Bottom) or (aRect.Bottom<rect2.Top) then begin
      // no intersection
      a:=0;
      aRect.Left:=a;
      aRect.Right:=a;
      aRect.Top:=a;
      aRect.Bottom:=a;
   end else begin
      if aRect.Left<rect2.Left then
         aRect.Left:=rect2.Left;
      if aRect.Right>rect2.Right then
         aRect.Right:=rect2.Right;
      if aRect.Top<rect2.Top then
         aRect.Top:=rect2.Top;
      if aRect.Bottom>rect2.Bottom then
         aRect.Bottom:=rect2.Bottom;
   end;
end;

// InformationDlg
//
procedure InformationDlg(const msg : String);
begin
   ShowMessage(msg);
end;

// QuestionDlg
//
function QuestionDlg(const msg : String) : Boolean;
begin
   Result:=(MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0)=mrYes);
end;

// InputDlg
//
function InputDlg(const aCaption, aPrompt, aDefault : String) : String;
begin
   Result:=InputBox(aCaption, aPrompt, aDefault);
end;

// SavePictureDialog
//
function SavePictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
{$ifdef WIN32}
var
   saveDialog : TSavePictureDialog;
begin
   saveDialog:=TSavePictureDialog.Create(Application);
   try
      with saveDialog do begin
         Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
         if aTitle<>'' then
            Title:=aTitle;
         FileName:=aFileName;
         Result:=Execute;
         if Result then
            aFileName:=FileName;
      end;
   finally
      saveDialog.Free;
   end;
{$else}
begin
   InformationDlg('SavePictureDialog not supported on this platform.');
   Result:=False;
{$endif}
end;

// OpenPictureDialog
//
function OpenPictureDialog(var aFileName : String; const aTitle : String = '') : Boolean;
{$ifdef WIN32}
var
   openDialog : TOpenPictureDialog;
begin
   openDialog:=TOpenPictureDialog.Create(Application);
   try
      with openDialog do begin
         Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
         if aTitle<>'' then
            Title:=aTitle;
         FileName:=aFileName;
         Result:=Execute;
         if Result then
            aFileName:=FileName;
      end;
   finally
      openDialog.Free;
   end;
{$else}
begin
   InformationDlg('OpenPictureDialog not supported on this platform.');
   Result:=False;
{$endif}
end;

// ApplicationTerminated
//
function ApplicationTerminated : Boolean;
begin
   Result:=Application.Terminated;
end;

// RaiseLastOSError
//
procedure RaiseLastOSError;
var
   e : EGLOSError;
begin
   e:=EGLOSError.Create('OS Error : '+SysErrorMessage(GetLastError));
   raise e;
end;

{$IFNDEF GLS_DELPHI_5_UP}
// FreeAndNil
//
procedure FreeAndNil(var anObject);
var
  buf : TObject;
begin
  buf:=TObject(anObject);
  TObject(anObject):=nil;  // clear the reference before destroying the object
  buf.Free;
end;
{$ENDIF GLS_DELPHI_5_UP}

type
  TDeviceCapabilities = record
    Xdpi, Ydpi: integer;        // Number of pixels per logical inch.
    Depth: integer;             // The bit depth.
    NumColors: integer;         // Number of entries in the device's color table.
  end;

function GetDeviceCapabilities: TDeviceCapabilities;
{$IFDEF MSWINDOWS}
var
  Device: HDC;
begin
  Device := GetDC(0);
  try
    result.Xdpi := GetDeviceCaps(Device,LOGPIXELSX);
    result.Ydpi := GetDeviceCaps(Device,LOGPIXELSY);
    result.Depth := GetDeviceCaps(Device,BITSPIXEL);
    result.NumColors := GetDeviceCaps(Device,NUMCOLORS);
  finally
    ReleaseDC(0, Device);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  metrics: QPAintDeviceMetricsH;
  paintDevice: QPaintDeviceH;

begin
  paintDevice := QWidget_to_QPaintDevice(QApplication_desktop);
  if paintDevice <> nil then
  begin
    metrics := QPaintDeviceMetrics_create(paintDevice);
    try
      result.Xdpi := QPaintDeviceMetrics_logicalDpiX(metrics);
      result.Ydpi := QPaintDeviceMetrics_logicalDpiY(metrics);
      result.Depth := QPaintDeviceMetrics_depth(metrics);
      result.NumColors := QPaintDeviceMetrics_numColors(metrics);
    finally
      QPaintDeviceMetrics_destroy(metrics);
    end;
  end;
end;
{$ENDIF}

// GetDeviceLogicalPixelsX
//
function GetDeviceLogicalPixelsX(device : Cardinal) : Integer;
begin
  result := GetDeviceCapabilities().Xdpi;
(*   {$ifdef WIN32}
   Result:=GetDeviceCaps(device, LOGPIXELSX);
   {$else}
   Result:=96; // dunno how to do it properly, so I fake it
   {$endif}   *)
end;

// GetCurrentColorDepth
//

function GetCurrentColorDepth : Integer;
(*
{$ifdef WIN32}
var
   topDC : HDC;
begin
   topDC:=GetDC(0);
   try
      Result:=GetDeviceCaps(topDC, BITSPIXEL)*GetDeviceCaps(topDC, PLANES);
   finally
      ReleaseDC(0, topDC);
   end;
{$else}
begin
   Result:=32; // dunno how to do it properly, so I fake it
{$endif}     *)
begin
  result := GetDeviceCapabilities().Depth;
end;

// PixelFormatToColorBits
//
function PixelFormatToColorBits(aPixelFormat : TPixelFormat) : Integer;
begin
   case aPixelFormat of
      pfCustom {$ifdef WIN32}, pfDevice{$ENDIF} :  // use current color depth
         Result:=GetCurrentColorDepth;
      pf1bit  : Result:=1;
{$ifdef WIN32}
      pf4bit  : Result:=4;
      pf15bit : Result:=15;
{$endif}
      pf8bit  : Result:=8;
      pf16bit : Result:=16;
      pf32bit : Result:=32;
   else
      Result:=24;
   end;
end;

// BitmapScanLine
//
function BitmapScanLine(aBitmap : TGLBitmap; aRow : Integer) : Pointer;
begin
{$ifdef FPC}
   Assert(False, 'BitmapScanLine unsupported');
   Result:=nil;
{$else}
   Result:=aBitmap.ScanLine[aRow];
{$endif}
end;


// Sleep
//
procedure Sleep(length : Cardinal);
begin
{$ifdef WIN32}
   Windows.Sleep(length);
{$else}
   usleep(length*1000);
{$endif}
end;

// QueryPerformanceCounter
//
procedure QueryPerformanceCounter(var val : Int64);
begin
{$ifdef WIN32}
   Windows.QueryPerformanceCounter(val);
{$else}
   val:=RDTSC;
{$endif}
end;

// QueryPerformanceFrequency
//
function QueryPerformanceFrequency(var val : Int64) : Boolean;
{$ifndef WIN32}
var
   startCycles, endCycles : Int64;
   aTime, refTime : TDateTime;
{$endif}
begin
{$ifdef WIN32}
   Result:=Boolean(Windows.QueryPerformanceFrequency(val));
{$else}
   aTime:=Now;
   while aTime=Now do ;
   startCycles:=RDTSC;
   refTime:=Now;
   while refTime=Now do ;
   endCycles:=RDTSC;
   aTime:=Now;
   val:=Round((endCycles-startCycles)/((aTime-refTime)*(3600*24)));
   Result:=True;
{$endif}
end;

// StartPrecisionTimer
//
function StartPrecisionTimer : Int64;
begin
   QueryPerformanceCounter(Result);
end;

// PrecisionTimeLap
//
function PrecisionTimerLap(const precisionTimer : Int64) : Double;
begin
   // we can do this, because we don't really stop anything
   Result:=StopPrecisionTimer(precisionTimer);
end;

// StopPrecisionTimer
//
function StopPrecisionTimer(const precisionTimer : Int64) : Double;
var
   cur, freq : Int64;
begin
   QueryPerformanceCounter(cur);
   if not vInvPerformanceCounterFrequencyReady then begin
      QueryPerformanceFrequency(freq);
      vInvPerformanceCounterFrequency:=1.0/freq;
      vInvPerformanceCounterFrequencyReady:=True;
   end;
   Result:=(cur-precisionTimer)*vInvPerformanceCounterFrequency;
end;

// RDTSC
//
function RDTSC : Int64;
asm
   db $0f, $31
end;

end.
