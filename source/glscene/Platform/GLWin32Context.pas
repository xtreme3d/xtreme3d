{: GLWin32Context<p>

   Win32 specific Context.<p>

   <b>History : </b><font size=-1><ul>
      <li>03/10/04 - NC - Added float texture support
      <li>03/07/02 - EG - ChooseWGLFormat Kyro fix (Patrick Chevalley)
      <li>13/03/02 - EG - aaDefault now prefers non-AA when possible
      <li>03/03/02 - EG - Fixed aaNone mode (AA specifically off)
      <li>01/03/02 - EG - Fixed CurrentPixelFormatIsHardwareAccelerated
      <li>22/02/02 - EG - Unified ChooseWGLFormat for visual & non-visual
      <li>21/02/02 - EG - AntiAliasing support *experimental* (Chris N. Strahm)
      <li>05/02/02 - EG - Fixed UnTrackWindow
      <li>03/02/02 - EG - Added experimental Hook-based window tracking
      <li>29/01/02 - EG - Improved recovery for ICDs without pbuffer  support
      <li>21/01/02 - EG - More graceful recovery for ICDs without pbuffer support
      <li>07/01/02 - EG - DoCreateMemoryContext now retrieved topDC when needed
      <li>15/12/01 - EG - Added support for AlphaBits
      <li>30/11/01 - EG - Hardware acceleration support now detected
      <li>20/11/01 - EG - New temp HWnd code for memory contexts (improved compat.)
      <li>04/09/01 - EG - Added ChangeIAttrib, support for 16bits depth buffer
      <li>25/08/01 - EG - Added pbuffer support and CreateMemoryContext interface
      <li>24/08/01 - EG - Fixed PropagateSharedContext
      <li>12/08/01 - EG - Handles management completed
      <li>22/07/01 - EG - Creation (glcontext.omm)
   </ul></font>
}
unit GLWin32Context;

interface

{$i GLScene.inc}

uses Windows, Classes, SysUtils, GLContext;

type

   // TGLWin32Context
   //
   {: A context driver for standard Windows OpenGL (via MS OpenGL). }
   TGLWin32Context = class (TGLContext)
      private
         { Private Declarations }
         FRC, FDC : Cardinal;
         FHPBUFFER : Integer;
         FiAttribs : packed array of Integer;
         FfAttribs : packed array of Single;
         FLegacyContextsOnly : Boolean;

         procedure SpawnLegacyContext(aDC : HDC); // used for WGL_pixel_format soup

      protected
         { Protected Declarations }
         procedure ClearIAttribs;
         procedure AddIAttrib(attrib, value : Integer);
         procedure ChangeIAttrib(attrib, newValue : Integer);
         procedure DropIAttrib(attrib : Integer);
         procedure ClearFAttribs;
         procedure AddFAttrib(attrib, value : Single);

         procedure DestructionEarlyWarning(sender : TObject);

         procedure ChooseWGLFormat(DC: HDC; nMaxFormats: Cardinal; piFormats: PInteger;
                                   var nNumFormats: Integer);
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

         property DC : Cardinal read FDC;
         property RC : Cardinal read FRC;
   end;


function CreateTempWnd : HWND;

var
   { This boolean controls a hook-based tracking of top-level forms destruction,
     with the purpose of being able to properly release OpenGL contexts before
     they are (improperly) released by some drivers upon top-level form
     destruction. }
   vUseWindowTrackingHook : Boolean = True;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Forms, OpenGL1x, GLCrossPlatform, Messages;

resourcestring
   cIncompatibleContexts =       'Incompatible contexts';
   cDeleteContextFailed =        'Delete context failed';
   cContextActivationFailed =    'Context activation failed: %X, %s';
   cContextDeactivationFailed =  'Context deactivation failed';
   cUnableToCreateLegacyContext= 'Unable to create legacy context';

var
   vTrackingCount : Integer;
   vTrackedHwnd : array of HWND;
   vTrackedEvents : array of TNotifyEvent;
   vTrackingHook : HHOOK;

// TrackHookProc
//
function TrackHookProc(nCode : Integer; wParam : wParam; lParam : LPARAM) : Integer; stdcall;
var
   i : Integer;
   p : PCWPStruct;
begin
   if nCode=HC_ACTION then begin
      p:=PCWPStruct(lParam);
 //   if (p.message=WM_DESTROY) or (p.message=WM_CLOSE) then begin // destroy & close variant
      if p.message=WM_DESTROY then begin
         // special care must be taken by this loop, items may go away unexpectedly
         i:=vTrackingCount-1;
         while i>=0 do begin
            if IsChild(p.hwnd, vTrackedHwnd[i]) then begin
               // got one, send notification
               vTrackedEvents[i](nil);
            end;
            Dec(i);
            while i>=vTrackingCount do Dec(i);
         end;
      end;
      CallNextHookEx(vTrackingHook, nCode, wParam, lParam);
      Result:=0;
   end else Result:=CallNextHookEx(vTrackingHook, nCode, wParam, lParam);
end;

// TrackWindow
//
procedure TrackWindow(h : HWND; notifyEvent : TNotifyEvent);
begin
   if not IsWindow(h) then Exit;
   if vTrackingCount=0 then
      vTrackingHook:=SetWindowsHookEx(WH_CALLWNDPROC, @TrackHookProc, 0, GetCurrentThreadID);
   Inc(vTrackingCount);
   SetLength(vTrackedHwnd, vTrackingCount);
   vTrackedHwnd[vTrackingCount-1]:=h;
   SetLength(vTrackedEvents, vTrackingCount);
   vTrackedEvents[vTrackingCount-1]:=notifyEvent;
end;

// UnTrackWindows
//
procedure UnTrackWindow(h : HWND);
var
   i, k : Integer;
begin
   if not IsWindow(h) then Exit;
   if vTrackingCount=0 then Exit;
   k:=0;
   for i:=0 to vTrackingCount-1 do if vTrackedHwnd[i]<>h then begin
      vTrackedHwnd[k]:=vTrackedHwnd[i];
      vTrackedEvents[k]:=vTrackedEvents[i];
      Inc(k);
   end;
   Dec(vTrackingCount);
   SetLength(vTrackedHwnd, vTrackingCount);
   SetLength(vTrackedEvents, vTrackingCount);
   if vTrackingCount=0 then
      UnhookWindowsHookEx(vTrackingHook);
end;

var
   vUtilWindowClass: TWndClass = (
      style: 0;
      lpfnWndProc: @DefWindowProc;
      cbClsExtra: 0;
      cbWndExtra: 0;
      hInstance: 0;
      hIcon: 0;
      hCursor: 0;
      hbrBackground: 0;
      lpszMenuName: nil;
      lpszClassName: 'GLSUtilWindow');

// CreateTempWnd
//
function CreateTempWnd : HWND;
var
   classRegistered: Boolean;
   tempClass: TWndClass;
begin
   vUtilWindowClass.hInstance:=HInstance;
   classRegistered:=GetClassInfo(HInstance, vUtilWindowClass.lpszClassName,
                                 tempClass);
   if not classRegistered then
      Windows.RegisterClass(vUtilWindowClass);
   Result:=CreateWindowEx(WS_EX_TOOLWINDOW, vUtilWindowClass.lpszClassName,
                          '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

// ------------------
// ------------------ TGLWin32Context ------------------
// ------------------

var
   vLastPixelFormat : Integer;
   vLastVendor : String;

// Create
//
constructor TGLWin32Context.Create;
begin
   inherited Create;
   ClearIAttribs;
   ClearFAttribs;
end;

// Destroy
//
destructor TGLWin32Context.Destroy;
begin
   inherited Destroy;
end;

// SetupPalette
//
function SetupPalette(DC: HDC; PFD: TPixelFormatDescriptor): HPalette;
var
   nColors, I : Integer;
   LogPalette : TMaxLogPalette;
   RedMask, GreenMask, BlueMask : Byte;
begin
   nColors := 1 shl Pfd.cColorBits;
   LogPalette.palVersion := $300;
   LogPalette.palNumEntries := nColors;
   RedMask := (1 shl Pfd.cRedBits  ) - 1;
   GreenMask := (1 shl Pfd.cGreenBits) - 1;
   BlueMask := (1 shl Pfd.cBlueBits ) - 1;
   with LogPalette, PFD do for I := 0 to nColors - 1 do begin
      palPalEntry[I].peRed := (((I shr cRedShift  ) and RedMask  ) * 255) div RedMask;
      palPalEntry[I].peGreen := (((I shr cGreenShift) and GreenMask) * 255) div GreenMask;
      palPalEntry[I].peBlue := (((I shr cBlueShift ) and BlueMask ) * 255) div BlueMask;
      palPalEntry[I].peFlags := 0;
   end;

   Result := CreatePalette(PLogPalette(@LogPalette)^);
   if Result <> 0 then begin
      SelectPalette(DC, Result, False);
      RealizePalette(DC);
   end else RaiseLastOSError;
end;

// ClearIAttribs
//
procedure TGLWin32Context.ClearIAttribs;
begin
   SetLength(FiAttribs, 1);
   FiAttribs[0]:=0;
end;

// AddIAttrib
//
procedure TGLWin32Context.AddIAttrib(attrib, value : Integer);
var
   n : Integer;
begin
   n:=Length(FiAttribs);
   SetLength(FiAttribs, n+2);
   FiAttribs[n-1]:=attrib;       FiAttribs[n]:=value;
   FiAttribs[n+1]:=0;
end;

// ChangeIAttrib
//
procedure TGLWin32Context.ChangeIAttrib(attrib, newValue : Integer);
var
   i : Integer;
begin
   i:=0;
   while i<Length(FiAttribs) do begin
      if FiAttribs[i]=attrib then begin
         FiAttribs[i+1]:=newValue;
         Exit;
      end;
      Inc(i, 2);
   end;
   AddIAttrib(attrib, newValue);
end;

// DropIAttrib
//
procedure TGLWin32Context.DropIAttrib(attrib : Integer);
var
   i : Integer;
begin
   i:=0;
   while i<Length(FiAttribs) do begin
      if FiAttribs[i]=attrib then begin
         Inc(i, 2);
         while i<Length(FiAttribs) do begin
            FiAttribs[i-2]:=FiAttribs[i];
            Inc(i);
         end;
         SetLength(FiAttribs, Length(FiAttribs)-2);
         Exit;
      end;
      Inc(i, 2);
   end;
end;

// ClearFAttribs
//
procedure TGLWin32Context.ClearFAttribs;
begin
   SetLength(FfAttribs, 1);
   FfAttribs[0]:=0;
end;

// AddFAttrib
//
procedure TGLWin32Context.AddFAttrib(attrib, value : Single);
var
   n : Integer;
begin
   n:=Length(FfAttribs);
   SetLength(FfAttribs, n+2);
   FfAttribs[n-1]:=attrib;       FfAttribs[n]:=value;
   FfAttribs[n+1]:=0;
end;

// DestructionEarlyWarning
//
procedure TGLWin32Context.DestructionEarlyWarning(sender : TObject);
begin
   DestroyContext;
end;

// ChooseWGLFormat
//
procedure TGLWin32Context.ChooseWGLFormat(DC: HDC; nMaxFormats: Cardinal; piFormats: PInteger;
                                          var nNumFormats: Integer);
const
   cAAToSamples : array [aaNone..aa4xHQ] of Integer = (1, 2, 2, 4, 4);

   procedure ChoosePixelFormat;
   begin
      if not wglChoosePixelFormatARB(DC, @FiAttribs[0], @FfAttribs[0],
                                     32, PGLint(piFormats), @nNumFormats) then
         nNumFormats:=0;
   end;

var
   float : boolean;

begin
   float:= (ColorBits = 64) or (ColorBits = 128);   // float_type

   // request hardware acceleration
   AddIAttrib(WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB);

   if float then begin    // float_type
     if GL_ATI_texture_float then begin // NV40 uses ATI_float, with linear filtering
       AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_FLOAT_ATI);
       end
     else begin
       AddIAttrib(WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB);
       AddIAttrib(WGL_FLOAT_COMPONENTS_NV, GL_TRUE);
     end;
   end;

//   if Multi_buffer>0 then AddIAttrib(WGL_AUX_BUFFERS_ARB, Multi_buffer);

   AddIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
   if AlphaBits>0 then
     AddIAttrib(WGL_ALPHA_BITS_ARB, AlphaBits);
   AddIAttrib(WGL_DEPTH_BITS_ARB, DepthBits);
   if StencilBits>0 then
      AddIAttrib(WGL_STENCIL_BITS_ARB, StencilBits);
   if AccumBits>0 then
      AddIAttrib(WGL_ACCUM_BITS_ARB, AccumBits);
   if AuxBuffers>0 then
      AddIAttrib(WGL_AUX_BUFFERS_ARB, AuxBuffers);
   if (AntiAliasing<>aaDefault) and WGL_ARB_multisample and GL_ARB_multisample then begin
      if AntiAliasing=aaNone then
         AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, GL_FALSE)
      else begin
         AddIAttrib(WGL_SAMPLE_BUFFERS_ARB, GL_TRUE);
         AddIAttrib(WGL_SAMPLES_ARB, cAAToSamples[AntiAliasing]);
      end;
   end;

   ClearFAttribs;
   ChoosePixelFormat;
   if (nNumFormats=0) and (AntiAliasing<>aaDefault) then begin
      // couldn't find AA buffer, try without
      DropIAttrib(WGL_SAMPLE_BUFFERS_ARB);
      DropIAttrib(WGL_SAMPLES_ARB);
   end;
   if (nNumFormats=0) and (DepthBits>=32) then begin
      // couldn't find 32+ bits depth buffer, 24 bits one available?
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, 24);
      ChoosePixelFormat;
   end;
   if (nNumFormats=0) and (DepthBits>=24) then begin
      // couldn't find 24+ bits depth buffer, 16 bits one available?
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, 16);
      ChoosePixelFormat;
   end;
   if (nNumFormats=0) and (ColorBits>=24) then begin
      // couldn't find 24+ bits color buffer, 16 bits one available?
      ChangeIAttrib(WGL_COLOR_BITS_ARB, 16);
      ChoosePixelFormat;
   end;
   if nNumFormats=0 then begin
      // ok, last attempt: no AA, restored depth and color,
      // relaxed hardware-acceleration request
      ChangeIAttrib(WGL_COLOR_BITS_ARB, ColorBits);
      ChangeIAttrib(WGL_DEPTH_BITS_ARB, DepthBits);
      DropIAttrib(WGL_ACCELERATION_ARB);
      ChoosePixelFormat;
   end;
end;

// DoCreateContext
//
procedure TGLWin32Context.DoCreateContext(outputDevice : Integer);
const
   cMemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
   cBoolToInt : array [False..True] of Integer = (GL_FALSE, GL_TRUE);
var
   pfDescriptor : TPixelFormatDescriptor;
   pixelFormat, nbFormats, softwarePixelFormat : Integer;
   aType : DWORD;
   iFormats : array [0..31] of Integer;
   tempWnd : HWND;
   tempDC, outputDC : HDC;
   localDC, localRC : Integer;

   function CurrentPixelFormatIsHardwareAccelerated : Boolean;
   var
      localPFD : TPixelFormatDescriptor;
   begin
      Result:=False;
      if pixelFormat=0 then Exit;
      with localPFD do begin
         nSize:=SizeOf(localPFD);
         nVersion:=1;
      end;
      DescribePixelFormat(outputDC, pixelFormat, SizeOf(localPFD), localPFD);
      Result:=((localPFD.dwFlags and PFD_GENERIC_FORMAT)=0);
   end;

var
   i, iAttrib, iValue : Integer;
begin
   outputDC:=HDC(outputDevice);
   if vUseWindowTrackingHook then
      TrackWindow(WindowFromDC(LongWord(outputDC)), DestructionEarlyWarning);

   // Just in case it didn't happen already.
   if not InitOpenGL then RaiseLastOSError;

   // Prepare PFD
   FillChar(pfDescriptor, SizeOf(pfDescriptor), 0);
   with PFDescriptor do begin
      nSize:=SizeOf(PFDescriptor);
      nVersion:=1;
      dwFlags:=PFD_SUPPORT_OPENGL;
      aType:=GetObjectType(outputDC);
      if aType=0 then
         RaiseLastOSError;
      if aType in cMemoryDCs then
         dwFlags:=dwFlags or PFD_DRAW_TO_BITMAP
      else dwFlags:=dwFlags or PFD_DRAW_TO_WINDOW;
      if rcoDoubleBuffered in Options then
         dwFlags:=dwFlags or PFD_DOUBLEBUFFER;
      if rcoStereo in Options then
         dwFlags:=dwFlags or PFD_STEREO;
      iPixelType:=PFD_TYPE_RGBA;
      cColorBits:=ColorBits;
      cDepthBits:=DepthBits;
      cStencilBits:=StencilBits;
      cAccumBits:=AccumBits;
      cAlphaBits:=AlphaBits;
      cAuxBuffers:=AuxBuffers;
      iLayerType:=PFD_MAIN_PLANE;
   end;
   pixelFormat:=0;

   // WGL_ARB_pixel_format is used if available
   //
   if not (IsMesaGL or FLegacyContextsOnly or (aType in cMemoryDCs)) then begin
      // the WGL mechanism is a little awkward: we first create a dummy context
      // on the TOP-level DC (ie. screen), to retrieve our pixelformat, create
      // our stuff, etc.
      tempWnd:=CreateTempWnd;
      tempDC:=GetDC(tempWnd);
      localDC:=0;
      localRC:=0;
      try
         SpawnLegacyContext(tempDC);
         try
            DoActivate;
            try
               ClearGLError;
               if WGL_ARB_pixel_format then begin
                  // New pixel format selection via wglChoosePixelFormatARB
                  ClearIAttribs;
                  AddIAttrib(WGL_DRAW_TO_WINDOW_ARB, GL_TRUE);
                  AddIAttrib(WGL_STEREO_ARB, cBoolToInt[rcoStereo in Options]);
                  AddIAttrib(WGL_DOUBLE_BUFFER_ARB, cBoolToInt[rcoDoubleBuffered in Options]);
                  ChooseWGLFormat(outputDC, 32, @iFormats, nbFormats);
                  if nbFormats>0 then begin
                     if WGL_ARB_multisample and (AntiAliasing in [aaNone, aaDefault]) then begin
                        // Pick first non AntiAliased for aaDefault and aaNone modes
                        iAttrib:=WGL_SAMPLE_BUFFERS_ARB;
                        for i:=0 to nbFormats-1 do begin
                           pixelFormat:=iFormats[i];
                           iValue:=GL_FALSE;
                           wglGetPixelFormatAttribivARB(outputDC, pixelFormat, 0, 1,
                                                        @iAttrib, @iValue);
                           if iValue=GL_FALSE then Break;
                        end;
                     end else pixelFormat:=iFormats[0];
                     if GetPixelFormat(outputDC)<>pixelFormat then begin
                        if not SetPixelFormat(outputDC, pixelFormat, @PFDescriptor) then
                           RaiseLastOSError;
                     end;
                  end;
               end;
            finally
               DoDeactivate;
            end;
         finally
            DoDestroyContext;
         end;
      finally
         ReleaseDC(0, tempDC);
         DestroyWindow(tempWnd);
         FDC:=localDC;
         FRC:=localRC;
      end;
   end;
   if pixelFormat=0 then begin
      // Legacy pixel format selection
      pixelFormat:=ChoosePixelFormat(outputDC, @PFDescriptor);
      if (not (aType in cMemoryDCs)) and (not CurrentPixelFormatIsHardwareAccelerated) then begin
         softwarePixelFormat:=pixelFormat;
         pixelFormat:=0;
      end else softwarePixelFormat:=0;
      if pixelFormat=0 then begin
         // Failed on default params, try with 16 bits depth buffer
         PFDescriptor.cDepthBits:=16;
         pixelFormat:=ChoosePixelFormat(outputDC, @PFDescriptor);
         if not CurrentPixelFormatIsHardwareAccelerated then
            pixelFormat:=0;
         if pixelFormat=0 then begin
            // Failed, try with 16 bits color buffer
            PFDescriptor.cColorBits:=16;
            pixelFormat:=ChoosePixelFormat(outputDC, @PFDescriptor);
         end;
         if not CurrentPixelFormatIsHardwareAccelerated then begin
            // Fallback to original, should be supported by software
            pixelFormat:=softwarePixelFormat;
         end;
         if pixelFormat=0 then RaiseLastOSError;
      end;
      ClearGLError;
   end;

   if GetPixelFormat(outputDC)<>pixelFormat then begin
      if not SetPixelFormat(outputDC, pixelFormat, @PFDescriptor) then
         RaiseLastOSError;
   end;

   // Check the properties we just set.
   DescribePixelFormat(outputDC, pixelFormat, SizeOf(PFDescriptor), PFDescriptor);
   with pfDescriptor do
      if (dwFlags and PFD_NEED_PALETTE) <> 0 then
         SetupPalette(outputDC, PFDescriptor);

   if (pfDescriptor.dwFlags and PFD_GENERIC_FORMAT)>0 then
      FAcceleration:=chaSoftware
   else FAcceleration:=chaHardware;

   FRC:=wglCreateContext(outputDC);
   if FRC=0 then
      RaiseLastOSError
   else vLastPixelFormat:=0;
   FDC:=outputDC;
end;

// SpawnLegacyContext
//
procedure TGLWin32Context.SpawnLegacyContext(aDC : HDC);
begin
   try
      FLegacyContextsOnly:=True;
      try
         DoCreateContext(Integer(aDC));
      finally
         FLegacyContextsOnly:=False;
      end;
   except
      on E: Exception do begin
         raise Exception.Create(cUnableToCreateLegacyContext+#13#10
                                +E.ClassName+': '+E.Message);
      end;
   end;
end;

// DoCreateMemoryContext
//
procedure TGLWin32Context.DoCreateMemoryContext(outputDevice, width, height : Integer);
var
   nbFormats : Integer;
   iFormats : array [0..31] of Integer;
   iPBufferAttribs : array [0..0] of Integer;
   localHPBuffer : Integer;
   localDC, localRC, tempDC : Cardinal;
   tempWnd : HWND;
begin
   localHPBuffer:=0;
   localDC:=0;
   localRC:=0;
   // the WGL mechanism is a little awkward: we first create a dummy context
   // on the TOP-level DC (ie. screen), to retrieve our pixelformat, create
   // our stuff, etc.
   tempWnd:=CreateTempWnd;
   tempDC:=GetDC(tempWnd);
   try
      SpawnLegacyContext(tempDC);
      try
         DoActivate;
         try
            ClearGLError;
            if WGL_ARB_pixel_format and WGL_ARB_pbuffer then begin
               ClearIAttribs;
               AddIAttrib(WGL_DRAW_TO_PBUFFER_ARB, 1);
               ChooseWGLFormat(Cardinal(tempDC), 32, @iFormats, nbFormats);
               if nbFormats=0 then
                  raise Exception.Create('Format not supported for pbuffer operation.');
               iPBufferAttribs[0]:=0;

               localHPBuffer:=wglCreatePbufferARB(tempDC, iFormats[0], width, height,
                                                  @iPBufferAttribs[0]);
               if localHPBuffer=0 then
                  raise Exception.Create('Unabled to create pbuffer.');
               try
                  localDC:=wglGetPbufferDCARB(localHPBuffer);
                  if localDC=0 then
                     raise Exception.Create('Unabled to create pbuffer''s DC.');
                  try
                     localRC:=wglCreateContext(localDC);
                     if localRC=0 then
                        raise Exception.Create('Unabled to create pbuffer''s RC.');
                  except
                     wglReleasePBufferDCARB(localHPBuffer, localDC);
                     raise;
                  end;
               except
                  wglDestroyPBufferARB(localHPBuffer);
                  raise;
               end;
            end else raise Exception.Create('WGL_ARB_pbuffer support required.');
            CheckOpenGLError;
         finally
            DoDeactivate;
         end;
      finally
         DoDestroyContext;
      end;
   finally
      ReleaseDC(0, tempDC);
      DestroyWindow(tempWnd);
      FHPBUFFER:=localHPBuffer;
      FDC:=localDC;
      FRC:=localRC;
   end;
   FAcceleration:=chaHardware;
end;

// DoShareLists
//
procedure TGLWin32Context.DoShareLists(aContext : TGLContext);
var
   otherRC : Cardinal;
begin
   if aContext is TGLWin32Context then begin
      otherRC:=TGLWin32Context(aContext).FRC;
      // some drivers fail (access violation) when requesting to share
      // a context with itself
      if FRC<>otherRC then
         wglShareLists(FRC, otherRC);
   end else raise Exception.Create(cIncompatibleContexts);
end;

// DoDestroyContext
//
procedure TGLWin32Context.DoDestroyContext;
begin
   if vUseWindowTrackingHook then
      UnTrackWindow(WindowFromDC(FDC));

   if FRC<>0 then
      if not wglDeleteContext(FRC) then
         raise EGLContext.Create(cDeleteContextFailed);
   if FHPBUFFER<>0 then begin
      wglReleasePbufferDCARB(FHPBuffer, FDC);
      wglDestroyPbufferARB(FHPBUFFER);
      FHPBUFFER:=0;
   end;
   FRC:=0;
   FDC:=0;
end;

// DoActivate
//
procedure TGLWin32Context.DoActivate;
var
   pixelFormat : Integer;
begin
   if not wglMakeCurrent(Cardinal(FDC), Cardinal(FRC)) then
      raise EGLContext.Create(Format(cContextActivationFailed,
                                     [GetLastError, SysErrorMessage(GetLastError)]));

   // The extension function addresses are unique for each pixel format. All rendering
   // contexts of a given pixel format share the same extension function addresses.
   pixelFormat:=GetPixelFormat(Cardinal(FDC));
   if PixelFormat<>vLastPixelFormat then begin
      if glGetString(GL_VENDOR)<>vLastVendor then begin
         ReadExtensions;
         ReadImplementationProperties;
         vLastVendor:=glGetString(GL_VENDOR);
      end else begin
         ReadWGLExtensions;
         ReadWGLImplementationProperties;
      end;
      vLastPixelFormat:=pixelFormat;
   end;

   // If we are using AntiAliasing, adjust filtering hints
   if (AntiAliasing in [aa2xHQ, aa4xHQ]) and GL_ARB_multisample then begin
      // Hint for nVidia HQ modes (Quincunx etc.)
      if GL_NV_multisample_filter_hint then
         glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
   end;
end;

// Deactivate
//
procedure TGLWin32Context.DoDeactivate;
begin
   if not wglMakeCurrent(0, 0) then
      raise Exception.Create(cContextDeactivationFailed);
end;

// IsValid
//
function TGLWin32Context.IsValid : Boolean;
begin
   Result:=(FRC<>0);
end;

// SwapBuffers
//
procedure TGLWin32Context.SwapBuffers;
begin
   if (FHPBUFFER=0) and (FDC<>0) and (rcoDoubleBuffered in Options) then
      Windows.SwapBuffers(Cardinal(FDC));
end;

// RenderOutputDevice
//
function TGLWin32Context.RenderOutputDevice : Integer;
begin
   Result:=FDC;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLContextClass(TGLWin32Context);
   
end.
