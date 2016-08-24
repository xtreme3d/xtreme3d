{: GLSMBASS<p>

	BASS based sound-manager (http://www.un4seen.com/music/, free for freeware).<p>

   Unsupported feature(s) :<ul>
      <li>sound source velocity
      <li>looping (sounds are played either once or forever)
      <li>source priorities (not relevant, channels are not limited)
   </ul><p>

	<b>History : </b><font size=-1><ul>
      <li>09/05/04 - GAK - Updated to BASS Version 2.0, and swapped to Dynamic DLL loading
      <li>24/09/02 - EG - BASS activation errors no longer result in Asserts (ignored)
      <li>27/02/02 - EG - Added 3D Factors and Environment support
      <li>05/02/02 - EG - BASS 1.4 compatibility
      <li>05/02/01 - EG - Fixed TGLSMBASS.CPUUsagePercent
	   <li>13/01/01 - EG - Creation (compat BASS 0.8)
	</ul></font>
}
unit GLSMBASS;

interface

uses Classes, GLSound, GLScene;

type

   // TBASS3DAlgorithm
   //
   TBASS3DAlgorithm = (algDefault, algOff, algFull, algLight);

	// TGLSMBASS
	//
	TGLSMBASS = class (TGLSoundManager)
	   private
	      { Private Declarations }
         FActivated : Boolean;
         FAlgorithm3D : TBASS3DAlgorithm;

	   protected
	      { Protected Declarations }
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;
         procedure NotifyMasterVolumeChange; override;
         procedure Notify3DFactorsChanged; override;
         procedure NotifyEnvironmentChanged; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;
         procedure MuteSource(aSource : TGLBaseSoundSource; muted : Boolean); override;
         procedure PauseSource(aSource : TGLBaseSoundSource; paused : Boolean); override;

         function GetDefaultFrequency(aSource : TGLBaseSoundSource) : Integer;
         
      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure UpdateSources; override;

         function CPUUsagePercent : Single; override;
         function EAXSupported : Boolean; override;

	   published
	      { Published Declarations }
         property Algorithm3D : TBASS3DAlgorithm read FAlgorithm3D write FAlgorithm3D default algDefault;
	end;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses Forms, SysUtils, Bass, VectorGeometry, Dialogs;

type
   TBASSInfo =  record
      channel : HCHANNEL;
      sample : HSAMPLE;
   end;
   PBASSInfo = ^TBASSInfo;

procedure Register;
begin
  RegisterComponents('GLScene', [TGLSMBASS]);
end;

// VectorToBASSVector
//
procedure VectorToBASSVector(const aVector : TVector; var aBASSVector : BASS_3DVECTOR);
begin
   with aBASSVector do begin
      x:=aVector[0];
      y:=aVector[1];
      z:=-aVector[2];
   end;
end;

// ------------------
// ------------------ TGLSMBASS ------------------
// ------------------

// Create
//
constructor TGLSMBASS.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
  BASS_Load(BASS_DLL);
   MaxChannels:=32;
end;

// Destroy
//
destructor TGLSMBASS.Destroy;
begin
	inherited Destroy;
  BASS_UnLoad;
end;

// DoActivate
//
function TGLSMBASS.DoActivate : Boolean;
const
   c3DAlgo : array [algDefault..algLight] of Integer =
      (BASS_3DALG_DEFAULT, BASS_3DALG_OFF, BASS_3DALG_FULL, BASS_3DALG_LIGHT);
begin
   assert(bass_isloaded,'BASS DLL is not present');

   if not BASS_Init(1, OutputFrequency, BASS_DEVICE_3D, Application.Handle,nil) then begin
      Result:=False;
      Exit;
   end;
   if not BASS_Start then begin
      Result:=False;
      Exit;
   end;
   FActivated:=True;
   BASS_SetConfig(BASS_CONFIG_3DALGORITHM, c3DAlgo[FAlgorithm3D]);
   NotifyMasterVolumeChange;
   Notify3DFactorsChanged;
   if Environment<>seDefault then
      NotifyEnvironmentChanged;
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMBASS.DoDeActivate;
begin
   FActivated:=False;
   BASS_Stop;
   BASS_Free;
end;

// NotifyMasterVolumeChange
//
procedure TGLSMBASS.NotifyMasterVolumeChange;
begin
   if FActivated then
      BASS_SetVolume(Round(MasterVolume*100));
end;

// Notify3DFactorsChanged
//
procedure TGLSMBASS.Notify3DFactorsChanged;
begin
   if FActivated then
      BASS_Set3DFactors(DistanceFactor, RollOffFactor, DopplerFactor);
end;

// NotifyEnvironmentChanged
//
procedure TGLSMBASS.NotifyEnvironmentChanged;
const
   cEnvironmentToBASSConstant : array [seDefault..sePsychotic] of Integer = (
      EAX_ENVIRONMENT_GENERIC, EAX_ENVIRONMENT_PADDEDCELL, EAX_ENVIRONMENT_ROOM,
      EAX_ENVIRONMENT_BATHROOM, EAX_ENVIRONMENT_LIVINGROOM, EAX_ENVIRONMENT_STONEROOM,
      EAX_ENVIRONMENT_AUDITORIUM, EAX_ENVIRONMENT_CONCERTHALL, EAX_ENVIRONMENT_CAVE,
      EAX_ENVIRONMENT_ARENA, EAX_ENVIRONMENT_HANGAR, EAX_ENVIRONMENT_CARPETEDHALLWAY,
      EAX_ENVIRONMENT_HALLWAY, EAX_ENVIRONMENT_STONECORRIDOR, EAX_ENVIRONMENT_ALLEY,
      EAX_ENVIRONMENT_FOREST, EAX_ENVIRONMENT_CITY, EAX_ENVIRONMENT_MOUNTAINS,
      EAX_ENVIRONMENT_QUARRY, EAX_ENVIRONMENT_PLAIN, EAX_ENVIRONMENT_PARKINGLOT,
      EAX_ENVIRONMENT_SEWERPIPE, EAX_ENVIRONMENT_UNDERWATER, EAX_ENVIRONMENT_DRUGGED,
      EAX_ENVIRONMENT_DIZZY, EAX_ENVIRONMENT_PSYCHOTIC);
begin
   if FActivated and EAXSupported then
      BASS_SetEAXParameters(cEnvironmentToBASSConstant[Environment],-1,-1,-1);
end;

// KillSource
//
procedure TGLSMBASS.KillSource(aSource : TGLBaseSoundSource);
var
   p : PBASSInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if p.channel<>0 then
         if not BASS_ChannelStop(p.channel) then Assert(False);
      BASS_SampleFree(p.sample);
      FreeMem(p);
      aSource.ManagerTag:=0;
   end;
end;

// UpdateSource
//
procedure TGLSMBASS.UpdateSource(aSource : TGLBaseSoundSource);
var
   i : Integer;
   p : PBASSInfo;
   objPos, objOri, objVel : TVector;
   position, orientation, velocity : BASS_3DVECTOR;
begin
   if (aSource.Sample=nil) or (aSource.Sample.Data.WAVDataSize=0) then Exit;
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if BASS_ChannelIsActive(p.channel)=0 then begin
         p.channel:=0;
         aSource.Free;
         Exit;
      end;
   end else begin
      p:=AllocMem(SizeOf(TBASSInfo));
      p.channel:=0;
      i:=BASS_SAMPLE_VAM+BASS_SAMPLE_3D+BASS_SAMPLE_OVER_DIST;
      if aSource.NbLoops>1 then
         i:=i+BASS_SAMPLE_LOOP;
      p.sample:=BASS_SampleLoad(True, aSource.Sample.Data.WAVData, 0,
                                aSource.Sample.Data.WAVDataSize,
                                MaxChannels, i);
      Assert(p.sample<>0, 'BASS Error '+IntToStr(Integer(BASS_ErrorGetCode)));
      aSource.ManagerTag:=Integer(p);
      if aSource.Frequency<=0 then
         aSource.Frequency:=-1;
   end;
   if aSource.Origin<>nil then begin
      objPos:=aSource.Origin.AbsolutePosition;
      objOri:=aSource.Origin.AbsoluteZVector;
      objVel:=NullHmgVector;
   end else begin
      objPos:=NullHmgPoint;
      objOri:=ZHmgVector;
      objVel:=NullHmgVector;
   end;
   VectorToBASSVector(objPos, position);
   VectorToBASSVector(objVel, velocity);
   VectorToBASSVector(objOri, orientation);
   if p.channel=0 then begin
      p.channel:=BASS_SamplePlay3D(p.sample, position, orientation, velocity);
      Assert(p.channel<>0);
      BASS_ChannelSet3DAttributes(p.channel, BASS_3DMODE_NORMAL,
                                  aSource.MinDistance, aSource.MaxDistance,
                                  Round(aSource.InsideConeAngle),
                                  Round(aSource.OutsideConeAngle),
                                  Round(aSource.ConeOutsideVolume*100));
   end else BASS_ChannelSet3DPosition(p.channel, position, orientation, velocity);
   if p.channel<>0 then begin
      if not BASS_ChannelSetAttributes(p.channel, aSource.Frequency, Round(aSource.Volume*100), -101) then
         Assert(False);
   end else aSource.Free;
end;

// MuteSource
//
procedure TGLSMBASS.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
var
   p : PBASSInfo;
   res : Boolean;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if muted then
         res:=BASS_ChannelSetAttributes(p.channel, -1, 0, -101)
      else res:=BASS_ChannelSetAttributes(p.channel, -1, Round(aSource.Volume*100), -101);
      Assert(res);
   end;
end;

// PauseSource
//
procedure TGLSMBASS.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
var
   p : PBASSInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PBASSInfo(aSource.ManagerTag);
      if paused then
         BASS_ChannelPause(p.channel)
      else BASS_ChannelResume(p.channel);
   end;
end;

// UpdateSources
//
procedure TGLSMBASS.UpdateSources;
var
   objPos, objVel, objDir, objUp : TVector;
   position, velocity, fwd, top : BASS_3DVECTOR;
begin
   // update listener
   ListenerCoordinates(objPos, objVel, objDir, objUp);
   VectorToBASSVector(objPos, position);
   VectorToBASSVector(objVel, velocity);
   VectorToBASSVector(objDir, fwd);
   VectorToBASSVector(objUp, top);
   if not BASS_Set3DPosition(position, velocity, fwd, top) then Assert(False);
   // update sources
   inherited;
   {if not }BASS_Apply3D;{ then Assert(False);}
end;

// CPUUsagePercent
//
function TGLSMBASS.CPUUsagePercent : Single;
begin
   Result:=BASS_GetCPU*100;
end;

// EAXSupported
//
function TGLSMBASS.EAXSupported : Boolean;
var
   c : Cardinal;
   s : Single;
begin
   Result:=BASS_GetEAXParameters(c, s, s, s);
end;

// GetDefaultFrequency
//
function TGLSMBASS.GetDefaultFrequency(aSource : TGLBaseSoundSource): integer;
var
   p : PBASSInfo;
   sampleInfo : BASS_Sample;
begin
   try
      p:=PBASSInfo(aSource.ManagerTag);
      BASS_SampleGetInfo(p.sample, sampleInfo);
      Result:=sampleInfo.freq;
   except
      Result:=-1;
   end;
end;

end.

