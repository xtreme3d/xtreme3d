{: GLCadencer<p>

	Cadencing composant for GLScene (ease Progress processing)<p>

	<b>History : </b><font size=-1><ul>
      <li>28/06/04 - LR - Added some ifdef Win32 for Linux
      <li>20/10/03 - EG - Fixed issues about cadencer destruction
      <li>29/08/03 - EG - Added MinDeltaTime and FixedDeltaTime
      <li>21/08/03 - EG - Fixed Application.OnIdle reset bug (Solerman Kaplon)
      <li>04/07/03 - EG - Improved TimeMultiplier transitions (supports zero)
      <li>06/06/03 - EG - Added cmApplicationIdle Mode
      <li>19/05/03 - EG - Added Reset (Roberto Bussola)
      <li>04/03/02 - EG - Added SetTimeMultiplier
      <li>01/07/02 - EG - Added TGLCadencedComponent
      <li>05/12/01 - EG - Fix in subscription mechanism (D6 IDE freezes gone?)
      <li>30/11/01 - EG - Added IsBusy (thx Chris S)
      <li>08/09/01 - EG - Added MaxDeltaTime limiter
      <li>23/08/01 - EG - No more "deprecated" warning for Delphi6
      <li>12/08/01 - EG - Protection against "timer flood"
      <li>19/07/01 - EG - Fixed Memory Leak in RegisterASAPCadencer,
                          Added speed limiter TASAPHandler.WndProc
      <li>01/02/01 - EG - Fixed "Freezing" when Enabled set to False
      <li>08/10/00 - EG - Added TASAPHandler to support multiple ASAP cadencers
      <li>19/06/00 - EG - Fixed TGLCadencer.Notification
      <li>14/04/00 - EG - Minor fixes
      <li>13/04/00 - EG - Creation
	</ul></font>
}
unit GLCadencer;

interface

{$i GLScene.inc}

uses GLScene, Classes, GLMisc, GLCrossPlatform,
   {$ifdef WIN32}
   Windows, Controls, Messages, StdCtrls, Forms
   {$endif}
   {$ifdef LINUX}
   QForms
   {$endif}
   ;

type

	// TGLCadencerMode
	//
	{: Determines how the TGLCadencer operates.<p>
		- cmManual : you must trigger progress manually (in your code)<br>
		- cmASAP : progress is triggered As Soon As Possible after a previous
			progress (uses windows messages).
      - cmApplicationIdle : will hook Application.OnIdle, this will overwrite
         any previous event handle, and only one cadencer may be in this mode. }
	TGLCadencerMode = (cmManual, cmASAP, cmApplicationIdle);

	// TGLCadencerTimeReference
	//
	{: Determines which time reference the TGLCadencer should use.<p>
		- cmRTC : the Real Time Clock is used (precise over long periods, but
			not accurate to the millisecond, may limit your effective framerate
         to less than 50 FPS on some systems)<br>
		- cmPerformanceCounter : the windows performance counter is used (nice
			precision, may derive over long periods, this is the default option
			as it allows the smoothest animation on fast systems)<br>
		- cmExternal : the CurrentTime property is used }
	TGLCadencerTimeReference = (cmRTC, cmPerformanceCounter, cmExternal);

	// TGLCadencer
	//
	{: This component allows auto-progression of animation.<p>
		Basicly dropping this component and linking it to your TGLScene will send
		it real-time progression events (time will be measured in seconds) while
		keeping the CPU 100% busy if possible (ie. if things change in your scene).<p>
		The progression time (the one you'll see in you progression events)
		is calculated using  (CurrentTime-OriginTime)*TimeMultiplier,
		CurrentTime being either manually or automatically updated using
		TimeReference (setting CurrentTime does NOT trigger progression). }
	TGLCadencer = class (TComponent)
		private
			{ Private Declarations }
         FSubscribedCadenceableComponents : TList;
			FScene : TGLScene;
			FTimeMultiplier : Double;
			lastTime, downTime, lastMultiplier : Double;
			FEnabled : Boolean;
			FSleepLength : Integer;
			FMode : TGLCadencerMode;
			FTimeReference : TGLCadencerTimeReference;
			FCurrentTime : Double;
			FOriginTime : Double;
         FMaxDeltaTime, FMinDeltaTime, FFixedDeltaTime : Double;
			FOnProgress : TGLProgressEvent;
			FProgressing : Integer;

		protected
			{ Protected Declarations }
			procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			function StoreTimeMultiplier : Boolean;
			procedure SetEnabled(const val : Boolean);
			procedure SetScene(const val : TGLScene);
			procedure SetMode(const val : TGLCadencerMode);
			procedure SetTimeReference(const val : TGLCadencerTimeReference);
         procedure SetTimeMultiplier(const val : Double);
			{: Returns raw ref time (no multiplier, no offset) }
			function GetRawReferenceTime : Double;
         procedure RestartASAP;
         procedure Loaded; override;

         procedure OnIdleEvent(Sender: TObject; var Done: Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;

         procedure Subscribe(aComponent : TGLCadenceAbleComponent);
         procedure UnSubscribe(aComponent : TGLCadenceAbleComponent);

			{: Allows to manually trigger a progression.<p>
				Time stuff is handled automatically.<br>
				If cadencer is disabled, this functions does nothing. }
			procedure Progress;
			{: Adjusts CurrentTime if necessary, then returns its value. }
			function GetCurrentTime : Double;
         {: Returns True if a "Progress" is underway.<p>
            Be aware that as long as IsBusy is True, the Cadencer may be
            sending messages and progression calls to cadenceable components
            and scenes. }
         function IsBusy : Boolean;

         {: Reset the time parameters and returns to zero.<p>}
         procedure Reset;

			{: Value soustracted to current time to obtain progression time. }
			property OriginTime : Double read FOriginTime write FOriginTime;
			{: Current time (manually or automatically set, see TimeReference). }
			property CurrentTime : Double read FCurrentTime write FCurrentTime;

		published
			{ Published Declarations }
			{: The TGLScene that will be cadenced (progressed). }
			property Scene : TGLScene read FScene write SetScene;
			{: Enables/Disables cadencing.<p>
				Disabling won't cause a jump when restarting, it is working like
				a play/pause (ie. may modify OriginTime to keep things smooth). }
			property Enabled : Boolean read FEnabled write SetEnabled default True;
			{: Defines how CurrentTime is updated.<p>
				See TGLCadencerTimeReference.<br>
				Dynamically changeing the TimeReference may cause a "jump".  }
			property TimeReference : TGLCadencerTimeReference read FTimeReference write SetTimeReference default cmPerformanceCounter;
			{: Multiplier applied to the time reference.<p>
            Zero isn't an allowed value, and be aware that if negative values
            are accepted, they may not be supported by other GLScene objects.<br>
				Changing the TimeMultiplier will alter OriginTime. }
			property TimeMultiplier : Double read FTimeMultiplier write SetTimeMultiplier stored StoreTimeMultiplier;
         {: Maximum value for deltaTime in progression events.<p>
            If null or negative, no max deltaTime is defined, otherwise, whenever
            an event whose actual deltaTime would be superior to MaxDeltaTime
            occurs, deltaTime is clamped to this max, and the extra time is hidden
            by the cadencer (it isn't visible in CurrentTime either).<br>
            This option allows to limit progression rate in simulations where
            high values would result in errors/random behaviour. }
         property MaxDeltaTime : Double read FMaxDeltaTime write FMaxDeltaTime;
         {: Minimum value for deltaTime in progression events.<p>
            If superior to zero, this value specifies the minimum time step
            between two progression events.<br>
            This option allows to limit progression rate in simulations where
            low values would result in errors/random behaviour. }
         property MinDeltaTime : Double read FMinDeltaTime write FMinDeltaTime;
         {: Fixed time-step value for progression events.<p>
            If superior to zero, progression steps will happen with that fixed
            delta time. The progression remains time based, so zero to N events
            may be fired depending on the actual deltaTime (if deltaTime is
            inferior to FixedDeltaTime, no event will be fired, if it is superior
            to two times FixedDeltaTime, two events will be fired, etc.).<br>
            This option allows to use fixed time steps in simulations (while the
            animation and rendering itself may happen at a lower or higher
            framerate). }
         property FixedDeltaTime : Double read FFixedDeltaTime write FFixedDeltaTime;
			{: Adjusts how progression events are triggered.<p>
				See TGLCadencerMode. }
			property Mode : TGLCadencerMode read FMode write SetMode default cmASAP;
			{: Allows relinquishing time to other threads/processes.<p>
				A "sleep" is issued BEFORE each progress if SleepLength>=0 (see
				help for the "sleep" procedure in delphi for details). }
			property SleepLength : Integer read FSleepLength write FSleepLength default -1;
			{: Happens AFTER scene was progressed. }
			property OnProgress : TGLProgressEvent read FOnProgress write FOnProgress;
	end;

	// TGLCustomCadencedComponent
	//
	{: Adds a property to connect/subscribe to a cadencer.<p> }
	TGLCustomCadencedComponent = class (TGLUpdateAbleComponent)
      private
         { Private Declarations }
         FCadencer : TGLCadencer;

      protected
         { Protected Declarations }
         procedure SetCadencer(const val : TGLCadencer);

         property Cadencer : TGLCadencer read FCadencer write SetCadencer;

		public
	      { Public Declarations }
         destructor Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
	end;

	// TGLCadencedComponent
	//
	TGLCadencedComponent = class (TGLCustomCadencedComponent)
		published
			{ Published Declarations }
         property Cadencer;
   end;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils;

var
	vCounterFrequency : Int64;

{$ifdef WIN32}
type
   TASAPHandler = class
   	FWindowHandle : HWND;
      FTooFastCounter : Integer;
      FTimer : Cardinal;

   	procedure WndProc(var Msg: TMessage);

      constructor Create;
      destructor Destroy; override;
   end;

var
	vWMTickCadencer : Cardinal;
   vASAPCadencerList : TList;
   vHandler : TASAPHandler;

const
	cTickGLCadencer = 'TickGLCadencer';

// RegisterASAPCadencer
//
procedure RegisterASAPCadencer(aCadencer : TGLCadencer);
begin
   if aCadencer.Mode=cmASAP then begin
      if not Assigned(vASAPCadencerList) then
         vASAPCadencerList:=TList.Create;
      if vASAPCadencerList.IndexOf(aCadencer)<0 then begin
         vASAPCadencerList.Add(aCadencer);
         if not Assigned(vHandler) then
            vHandler:=TASAPHandler.Create;
      end;
   end else if aCadencer.Mode=cmApplicationIdle then
      Application.OnIdle:=aCadencer.OnIdleEvent;
end;

// UnRegisterASAPCadencer
//
procedure UnRegisterASAPCadencer(aCadencer : TGLCadencer);
var
   i : Integer;
begin
   if aCadencer.Mode=cmASAP then begin
      if Assigned(vASAPCadencerList) then begin
         i:=vASAPCadencerList.IndexOf(aCadencer);
         if i>=0 then
            vASAPCadencerList[i]:=nil;
      end;
   end else if aCadencer.Mode=cmApplicationIdle then
      Application.OnIdle:=nil;
end;

// ------------------
// ------------------ TASAPHandler ------------------
// ------------------

// Create
//
constructor TASAPHandler.Create;
begin
	inherited Create;
{$ifdef GLS_DELPHI_6_UP}
   FWindowHandle:=Classes.AllocateHWnd(WndProc);
{$else}
   FWindowHandle:=AllocateHWnd(WndProc);
{$endif}
	PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
end;

// Destroy
//
destructor TASAPHandler.Destroy;
begin
   if FTimer<>0 then
      KillTimer(FWindowHandle, FTimer);
{$ifdef GLS_DELPHI_6_UP}
 	Classes.DeallocateHWnd(FWindowHandle);
{$else}
 	DeallocateHWnd(FWindowHandle);
{$endif}
   inherited Destroy;
end;

// WndProc
//
var
   vWndProcInLoop : Boolean;
procedure TASAPHandler.WndProc(var Msg: TMessage);
var
   i : Integer;
   cad : TGLCadencer;
begin
//   Windows.Beep(440, 10);
   with Msg do begin
      if Msg=WM_TIMER then begin
         KillTimer(FWindowHandle, FTimer);
         FTimer:=0;
      end;
      if (Msg<>WM_TIMER) and (Cardinal(GetMessageTime)=GetTickCount) then begin
         // if we're going too fast, "sleep" for 1 msec
         Inc(FTooFastCounter);
         if FTooFastCounter>5000 then begin
            if FTimer=0 then
               FTimer:=SetTimer(FWindowHandle, 1, 1, nil);
            FTooFastCounter:=0;
         end;
      end else FTooFastCounter:=0;
      if FTimer<>0 then begin
         Result:=0;
         Exit;
      end;
      if not vWndProcInLoop then begin
         vWndProcInLoop:=True;
         try
         	if (Msg=vWMTickCadencer) or (Msg=WM_TIMER) then begin
               // Progress
               for i:=vASAPCadencerList.Count-1 downto 0 do begin
                  cad:=TGLCadencer(vASAPCadencerList[i]);
                  if     Assigned(cad) and (cad.Mode=cmASAP)
                     and cad.Enabled and (cad.FProgressing=0) then begin
                     if Application.Terminated then begin
                        // force stop
                        cad.Enabled:=False
                     end else begin
                        try
                           // do stuff
                           cad.Progress;
                        except
                           Application.HandleException(Self);
                           // it faulted, stop it
                           cad.Enabled:=False
                        end
                     end;
                  end;
               end;
               // care for nils
               vASAPCadencerList.Pack;
               if vASAPCadencerList.Count=0 then begin
                  vASAPCadencerList.Free;
                  vASAPCadencerList:=nil;
                  vHandler.Free;
                  vHandler:=nil;
               end else begin
                  // Prepare the return of the infernal loop...
                  PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
               end;
            end;
         finally
            vWndProcInLoop:=False;
         end;
      end;
		Result:=0;
	end;
end;
{$endif}

// ------------------
// ------------------ TGLCadencer ------------------
// ------------------

// Create
//
constructor TGLCadencer.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FTimeReference:=cmPerformanceCounter;
	downTime:=GetRawReferenceTime;
	FOriginTime:=downTime;
	FTimeMultiplier:=1;
	FSleepLength:=-1;
	Mode:=cmASAP;
   Enabled:=True;
end;

// Destroy
//
destructor TGLCadencer.Destroy;
begin
   Assert(FProgressing=0);
   {$ifdef WIN32}
   UnRegisterASAPCadencer(Self);
   {$endif}
   FSubscribedCadenceableComponents.Free;
   FSubscribedCadenceableComponents:=nil;
	inherited Destroy;
end;

// Subscribe
//
procedure TGLCadencer.Subscribe(aComponent : TGLCadenceAbleComponent);
begin
   if not Assigned(FSubscribedCadenceableComponents) then
      FSubscribedCadenceableComponents:=TList.Create;
   if FSubscribedCadenceableComponents.IndexOf(aComponent)<0 then begin
      FSubscribedCadenceableComponents.Add(aComponent);
      aComponent.FreeNotification(Self);
   end;
end;

// UnSubscribe
//
procedure TGLCadencer.UnSubscribe(aComponent : TGLCadenceAbleComponent);
var
   i : Integer;
begin
   if Assigned(FSubscribedCadenceableComponents) then begin
      i:=FSubscribedCadenceableComponents.IndexOf(aComponent);
      if i>=0 then begin
         FSubscribedCadenceableComponents.Delete(i);
         aComponent.RemoveFreeNotification(Self);
      end;
   end;
end;

// Notification
//
procedure TGLCadencer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
   	if AComponent=FScene then FScene:=nil;
      if Assigned(FSubscribedCadenceableComponents) then
         FSubscribedCadenceableComponents.Remove(AComponent);
   end;
   inherited;
end;

// Loaded
//
procedure TGLCadencer.Loaded;
begin
   inherited Loaded;
   RestartASAP;
end;

// OnIdleEvent
//
procedure TGLCadencer.OnIdleEvent(Sender: TObject; var Done: Boolean);
begin
   Progress;
   Done:=False;
end;

// RestartASAP
//
procedure TGLCadencer.RestartASAP;
begin
   if not (csLoading in ComponentState) then begin
      {$ifdef WIN32}
      if (Mode in [cmASAP, cmApplicationIdle]) and (not (csDesigning in ComponentState))
         and Assigned(FScene) and Enabled then
            RegisterASAPCadencer(Self)
      else UnRegisterASAPCadencer(Self);
      {$endif}
   end;
end;

// SetEnabled
//
procedure TGLCadencer.SetEnabled(const val : Boolean);
begin
	if FEnabled<>val then begin
		FEnabled:=val;
      if not (csDesigning in ComponentState) then begin
   		if Enabled then
	   		FOriginTime:=FOriginTime+GetRawReferenceTime-downTime
   		else downTime:=GetRawReferenceTime;
         RestartASAP;
      end;
	end;
end;

// SetScene
//
procedure TGLCadencer.SetScene(const val : TGLScene);
begin
	if FScene<>val then begin
      if Assigned(FScene) then
         FScene.RemoveFreeNotification(Self);
		FScene:=val;
      if Assigned(FScene) then
         FScene.FreeNotification(Self);
      RestartASAP;
	end;
end;

// SetTimeMultiplier
//
procedure TGLCadencer.SetTimeMultiplier(const val : Double);
var
   rawRef : Double;
begin
   if val<>FTimeMultiplier then begin
      if val=0 then begin
         lastMultiplier:=FTimeMultiplier;
         Enabled:=False;
      end else begin
         rawRef:=GetRawReferenceTime;
         if FTimeMultiplier=0 then begin
            Enabled:=True;
            // continuity of time:
            // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*lastMultiplier
            FOriginTime:=rawRef-(rawRef-FOriginTime)*lastMultiplier/val;
         end else begin
            // continuity of time:
            // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*FTimeMultiplier
            FOriginTime:=rawRef-(rawRef-FOriginTime)*FTimeMultiplier/val;
         end;
      end;
      FTimeMultiplier:=val;
   end;
end;

// StoreTimeMultiplier
//
function TGLCadencer.StoreTimeMultiplier : Boolean;
begin
	Result:=(FTimeMultiplier<>1);
end;

// SetMode
//
procedure TGLCadencer.SetMode(const val : TGLCadencerMode);
begin
	if FMode<>val then begin
        {$ifdef WIN32}
      if FMode<>cmManual then
         UnRegisterASAPCadencer(Self);
        {$endif}
		FMode:=val;
      RestartASAP;
	end;
end;

// SetTimeReference
//
procedure TGLCadencer.SetTimeReference(const val : TGLCadencerTimeReference);
begin
	// nothing more, yet
	FTimeReference:=val;
end;

// Progress
//
procedure TGLCadencer.Progress;
var
	deltaTime, newTime, totalDelta : Double;
   i : Integer;
   pt : TProgressTimes;
begin
	// basic protection against infinite loops,
   // shall never happen, unless there is a bug in user code
   if FProgressing<0 then Exit;
   if Enabled then begin
   	// avoid stalling everything else...
		if SleepLength>=0 then
			Sleep(SleepLength);
      // in manual mode, the user is supposed to make sure messages are handled
      // in Idle mode, this processing is implicit
      if Mode=cmASAP then begin
         Application.ProcessMessages;
         {$ifdef WIN32}
         if    (not Assigned(vASAPCadencerList))
            or (vASAPCadencerList.IndexOf(Self)<0) then Exit;
         {$endif}
      end;
   end;
	Inc(FProgressing);
	try
		if Enabled then begin
         // One of the processed messages might have disabled us
         if Enabled then begin
            // ...and progress !
            newTime:=GetCurrentTime;
            deltaTime:=newTime-lastTime;
            if (deltaTime>=MinDeltaTime) and (deltaTime>=FixedDeltaTime) then begin
               if FMaxDeltaTime>0 then begin
                  if deltaTime>FMaxDeltaTime then begin
                     FOriginTime:=FOriginTime+(deltaTime-FMaxDeltaTime)/FTimeMultiplier;
                     deltaTime:=FMaxDeltaTime;
                     newTime:=lastTime+deltaTime;
                  end;
               end;
               totalDelta:=deltaTime;
               if FixedDeltaTime>0 then
                  deltaTime:=FixedDeltaTime;
               while totalDelta>=deltaTime do begin
                  lastTime:=lastTime+deltaTime;
                  if Assigned(FScene) and (deltaTime<>0) then begin
                     FProgressing:=-FProgressing;
                     try
                        FScene.Progress(deltaTime, lastTime);
                     finally
                        FProgressing:=-FProgressing;
                     end;
                  end;
                  pt.deltaTime:=deltaTime;
                  pt.newTime:=lastTime;
                  if Assigned(FSubscribedCadenceableComponents) then
                     for i:=0 to FSubscribedCadenceableComponents.Count-1 do
                        with TGLCadenceAbleComponent(FSubscribedCadenceableComponents[i]) do
                           DoProgress(pt);
                  if Assigned(FOnProgress) and (not (csDesigning in ComponentState)) then
                     FOnProgress(Self, deltaTime, newTime);
                  if deltaTime<=0 then Break;
                  totalDelta:=totalDelta-deltaTime;
               end;
            end;
         end;
		end;
	finally
      Dec(FProgressing);
	end;
end;

// GetRawReferenceTime
//
function TGLCadencer.GetRawReferenceTime : Double;
var
	counter : Int64;
begin
	case FTimeReference of
		cmRTC : // Real Time Clock
			Result:=Now*(3600*24);
		cmPerformanceCounter : begin // HiRes Performance Counter
			QueryPerformanceCounter(counter);
			Result:=counter/vCounterFrequency;
		end;
		cmExternal : // User defined value
			Result:=FCurrentTime;
	else
		Result:=0;
		Assert(False);
	end;
end;

// GetCurrentTime
//
function TGLCadencer.GetCurrentTime : Double;
begin
	Result:=(GetRawReferenceTime-FOriginTime)*FTimeMultiplier;
	FCurrentTime:=Result;
end;

// IsBusy
//
function TGLCadencer.IsBusy : Boolean;
begin
   Result:=(FProgressing<>0);   
end;

//  Reset
//
procedure TGLCadencer.Reset;
begin
   lasttime:=0;
   downTime:=GetRawReferenceTime;
   FOriginTime:=downTime;
end;

// ------------------
// ------------------ TGLCustomCadencedComponent ------------------
// ------------------

// Destroy
//
destructor TGLCustomCadencedComponent.Destroy;
begin
   Cadencer:=nil;
   inherited Destroy;
end;

// Notification
//
procedure TGLCustomCadencedComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FCadencer) then
      Cadencer:=nil;
   inherited;
end;

// SetCadencer
//
procedure TGLCustomCadencedComponent.SetCadencer(const val : TGLCadencer);
begin
   if FCadencer<>val then begin
      if Assigned(FCadencer) then
         FCadencer.UnSubscribe(Self);
      FCadencer:=val;
      if Assigned(FCadencer) then
         FCadencer.Subscribe(Self);
   end;
end;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
initialization
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

	// Get our Windows message ID
   {$ifdef WIN32}
	vWMTickCadencer:=RegisterWindowMessage(cTickGLCadencer);
   {$endif}

	// Preparation for high resolution timer
	if not QueryPerformanceFrequency(vCounterFrequency) then
		vCounterFrequency := 0;

finalization
{$ifdef WIN32}
   FreeAndNil(vHandler);
   FreeAndNil(vASAPCadencerList);
{$endif}
end.
