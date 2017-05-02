unit Squall;

interface

uses SysUtils, Windows;

const
 SQUALL_ERROR_NO_SOUND: Integer =               -1;
 SQUALL_ERROR_MEMORY: Integer =                 -2;
 SQUALL_ERROR_UNINITIALIZED: Integer =          -3;
 SQUALL_ERROR_INVALID_PARAM: Integer =          -4;
 SQUALL_ERROR_CREATE_WINDOW: Integer =          -5; 
 SQUALL_ERROR_CREATE_DIRECT_SOUND: Integer =    -6;
 SQUALL_ERROR_CREATE_THREAD: Integer =          -7;
 SQUALL_ERROR_SET_LISTENER_PARAM: Integer =     -8;
 SQUALL_ERROR_GET_LISTENER_PARAM: Integer =     -9;
 SQUALL_ERROR_NO_FREE_CHANNEL: Integer =        -10;
 SQUALL_ERROR_CREATE_CHANNEL: Integer =         -11;
 SQUALL_ERROR_CHANNEL_NOT_FOUND: Integer =      -12;
 SQUALL_ERROR_SET_CHANNEL_PARAM: Integer =      -13;
 SQUALL_ERROR_GET_CHANNEL_PARAM: Integer =      -14; 
 SQUALL_ERROR_METHOD: Integer =                 -15;
 SQUALL_ERROR_ALGORITHM: Integer =              -16;
 SQUALL_ERROR_NO_EAX: Integer =                 -17;
 SQUALL_ERROR_EAX_VERSION: Integer =            -18;
 SQUALL_ERROR_SET_EAX_PARAM: Integer =          -19;
 SQUALL_ERROR_GET_EAX_PARAM: Integer =          -20;
 SQUALL_ERROR_NO_ZOOMFX: Integer =              -21;
 SQUALL_ERROR_SET_ZOOMFX_PARAM: Integer =       -22;
 SQUALL_ERROR_GET_ZOOMFX_PARAM: Integer =       -23;
 SQUALL_ERROR_UNKNOWN: Integer =                -24;
 SQUALL_ERROR_SAMPLE_INIT: Integer =            -25;
 SQUALL_ERROR_SAMPLE_BAD: Integer =             -26;
 SQUALL_ERROR_SET_MIXER_PARAM: Integer =        -27;
 SQUALL_ERROR_GET_MIXER_PARAM: Integer =        -28;

 SQUALL_LISTENER_MODE_IMMEDIATE: Integer = 0;
 SQUALL_LISTENER_MODE_DEFERRED: Integer =  1;

 SQUALL_ALG_3D_DEFAULT: Integer =          0;
 SQUALL_ALG_3D_OFF: Integer =              1;
 SQUALL_ALG_3D_FULL: Integer =             2;
 SQUALL_ALG_3D_LIGTH: Integer =            3;

 SQUALL_DEVICE_CAPS_HARDWARE: Integer =    $00000001;
 SQUALL_DEVICE_CAPS_HARDWARE_3D: Integer = $00000002;
 SQUALL_DEVICE_CAPS_EAX10: Integer =       $00000004;
 SQUALL_DEVICE_CAPS_EAX20: Integer =       $00000008;
 SQUALL_DEVICE_CAPS_EAX30: Integer =       $00000010;
 SQUALL_DEVICE_CAPS_ZOOMFX: Integer =      $00000100;

 SQUALL_SPEAKER_DEFAULT: Integer =         $00000000;
 SQUALL_SPEAKER_HEADPHONE: Integer =       $00000001;
 SQUALL_SPEAKER_MONO: Integer =            $00000002;
 SQUALL_SPEAKER_STEREO: Integer =          $00000003;
 SQUALL_SPEAKER_QUAD: Integer =            $00000004;
 SQUALL_SPEAKER_SURROUND: Integer =        $00000005;
 SQUALL_SPEAKER_5POINT1: Integer =         $00000006;

 SQUALL_CHANNEL_STATUS_NONE: Integer =     0;
 SQUALL_CHANNEL_STATUS_PLAY: Integer =     1;
 SQUALL_CHANNEL_STATUS_PAUSE: Integer =    2;
 SQUALL_CHANNEL_STATUS_PREPARED: Integer = 3;

 SQUALL_EAX_LISTENER_FLAGS_DECAYTIMESCALE: Integer =          $00000001;
 SQUALL_EAX_LISTENER_FLAGS_REFLECTIONSSCALE: Integer =        $00000002;
 SQUALL_EAX_LISTENER_FLAGS_REFLECTIONSDELAYSCALE: Integer =   $00000004;
 SQUALL_EAX_LISTENER_FLAGS_REVERBSCALE: Integer =             $00000008;
 SQUALL_EAX_LISTENER_FLAGS_REVERBDELAYSCALE: Integer =        $00000010;
 SQUALL_EAX_LISTENER_FLAGS_DECAYHFLIMIT: Integer =            $00000020;

 SQUALL_EAX_LISTENER_FLAGS_ECHOTIMESCALE: Integer =           $00000040;
 SQUALL_EAX_LISTENER_FLAGS_MODULATIONTIMESCALE: Integer =     $00000080;

 SQUALL_EAX_LISTENER_FLAGS_DEFAULT: Integer =                 $0000003f;

 SQUALL_EAX_OFF: Integer =              -1;
 SQUALL_EAX_GENERIC: Integer =          0;
 SQUALL_EAX_PADDEDCELL: Integer =       1;
 SQUALL_EAX_ROOM: Integer =             2;
 SQUALL_EAX_BATHROOM: Integer =         3;
 SQUALL_EAX_LIVINGROOM: Integer =       4;
 SQUALL_EAX_STONEROOM: Integer =        5;
 SQUALL_EAX_AUDITORIUM: Integer =       6;
 SQUALL_EAX_CONCERTHALL: Integer =      7;
 SQUALL_EAX_CAVE: Integer =             8;
 SQUALL_EAX_ARENA: Integer =            9;
 SQUALL_EAX_HANGAR: Integer =           10;
 SQUALL_EAX_CARPETEDHALLWAY: Integer =  11;
 SQUALL_EAX_HALLWAY: Integer =          12;
 SQUALL_EAX_STONECORRIDOR: Integer =    13;
 SQUALL_EAX_ALLEY: Integer =            14;
 SQUALL_EAX_FOREST: Integer =           15;
 SQUALL_EAX_CITY: Integer =             16;
 SQUALL_EAX_MOUNTAINS: Integer =        17;
 SQUALL_EAX_QUARRY: Integer =           18;
 SQUALL_EAX_PLAIN: Integer =            19;
 SQUALL_EAX_PARKINGLOT: Integer =       20;
 SQUALL_EAX_SEWERPIPE: Integer =        21;
 SQUALL_EAX_UNDERWATER: Integer =       22;
 SQUALL_EAX_DRUGGED: Integer =          23;
 SQUALL_EAX_DIZZY: Integer =            24;
 SQUALL_EAX_PSYCHOTIC: Integer =        25;

 SQUALL_EAX_CHANNEL_FLAGS_DIRECTHFAUTO: Integer =             $00000001;
 SQUALL_EAX_CHANNEL_FLAGS_ROOMAUTO: Integer =                 $00000002;
 SQUALL_EAX_CHANNEL_FLAGS_ROOMHFAUTO: Integer =               $00000004;
 SQUALL_EAX_CHANNEL_FLAGS_DEFAULT: Integer =                  $00000007;

type

 squall_parameters_t = record
  Window: PHandle;
  Device: Integer;
  SampleRate: Integer;
  BitPerSample: Integer;
  Channels: Integer;
  UseHW2D: Integer;
  UseHW3D: Integer;
  UseAlg: Integer;
  BufferSize: Integer;
  ListenerMode: Integer;
  DistanceFactor: Single;
  RolloffFactor: Single;
  DopplerFactor: Single;
 end;

 squall_sample_default_t = record
  SampleGroupID: Integer;
  Priority: Integer;
  Frequency: Integer;
  Volume: Integer;
  Pan: Integer;
  MinDist: Single;
  MaxDist: Single;
 end;

 squall_channels_t = record
  Play: Integer;
  Pause: Integer;
  Prepare: Integer;
  Play3D: Integer;
  Pause3D: Integer;
  Prepare3D: Integer;
 end;

 squall_device_caps_t = record
  Flags: Integer;
  HardwareChannels: Integer;
  Hardware3DChannels: Integer;
 end;

 squall_eax_listener_t = record
  case Integer of
  0:
   (
    eax1: record
    Environment: Cardinal;
    Volume: Single;
    DecayTime_sec: Single;
    Damping: Single;
    end;
   );
  1:
   (
    eax2: record
     Room: Integer;
     RoomHF: Integer;
     RoomRolloffFactor: Single;
     DecayTime: Single;
     DecayHFRatio: Single;
     Reflections: Integer;
     ReflectionsDelay: Single;
     Reverb: Integer;
     ReverbDelay: Single;
     Environment: Cardinal;
     EnvironmentSize: Single;
     EnvironmentDiffusion: Single;
     AirAbsorptionHF: Single;
     Flags: Cardinal;
    end;
   );
  2:
   (
    eax3: record
     Environment: Cardinal;
     EnvironmentSize: Single;
     EnvironmentDiffusion: Single;
     Room: Integer;
     RoomHF: Integer;
     RoomLF: Integer;
     DecayTime: Single;
     DecayHFRatio: Single;
     DecayLFRatio: Single;
     Reflections: Integer;
     ReflectionsDelay: Single;
     ReflectionsPan: array [0..2] of Single;
     Reverb: Integer;
     ReverbDelay: Single;
     ReverbPan: array [0..2] of Single;
     EchoTime: Single;
     EchoDepth: Single;
     ModulationTime: Single;
     ModulationDepth: Single;
     AirAbsorptionHF: Single;
     HFReference: Single;
     LFReference: Single;
     RoomRolloffFactor: Single;
     Flags: Cardinal;
    end;
   );
 end;

 squall_eax_channel_t = record
  case Integer of
  0:
   (
    eax1: record
     Mix: Single;
    end;
   );
  1:
   (
    eax2: record
     Direct: Integer;
     DirectHF: Integer;
     Room: Integer;
     RoomHF: Integer;
     RoomRolloffFactor: Single;
     Obstruction: Integer;
     ObstructionLFRatio: Single;
     Occlusion: Integer;
     OcclusionLFRatio: Single;
     OcclusionRoomRatio: Single;
     OutsideVolumeHF: Integer;
     AirAbsorptionFactor: Single;
     Flags: Cardinal
    end;
   );
  2:
   (
    eax3: record
     Direct: Integer;
     DirectHF: Integer;
     Room: Integer;
     RoomHF: Integer;
     Obstruction: Integer;
     ObstructionLFRatio: Single;
     Occlusion: Integer;
     OcclusionLFRatio: Single;
     OcclusionRoomRatio: Single;
     OcclusionDirectRatio: Single;
     Exclusion: Integer;
     ExclusionLFRatio: Single;
     OutsideVolumeHF: Integer;
     DopplerFactor: Single;
     RolloffFactor: Single;
     RoomRolloffFactor: Single;
     AirAbsorptionFactor: Single;
     Flags: Cardinal;
    end;
   );
 end;

 squall_zoomfx_channel_t = record
  Min: array [0..2] of Single;
  Max: array [0..2] of Single;
  Front: array [0..2] of Single;
  Top: array [0..2] of Single;
  MacroFX: Integer;
 end;

 psquall_parameters_t = ^squall_parameters_t;
 psquall_sample_default_t = ^squall_sample_default_t;
 psquall_eax_listener_t = ^squall_eax_listener_t;
 psquall_eax_channel_t = ^squall_eax_channel_t;
 psquall_zoomfx_channel_t = ^squall_zoomfx_channel_t;

var

SQUALL_Init: function(SystemParam: psquall_parameters_t): Integer; cdecl;
SQUALL_Free: procedure();cdecl;

//TODO:
SQUALL_Pause: function(Pause: Integer): Integer; cdecl;
SQUALL_Stop: function(): Integer; cdecl;
SQUALL_SetDevice: function(Num: Integer): Integer; cdecl;
SQUALL_GetDevice: function(): Integer; cdecl;
SQUALL_SetHardwareAcceleration: function(UseHW2D,UseHW3D: Integer): Integer; cdecl;
SQUALL_GetHardwareAcceleration: function(var UseHW2D, UseHW3D: Integer): Integer; cdecl;
SQUALL_SetSpeakerMode: function(Mode: Integer): Integer; cdecl;
SQUALL_GetSpeakerMode: function(): Integer; cdecl;
SQUALL_Set3DAlgorithm: function(Algorithm: Integer): Integer; cdecl;
SQUALL_Get3DAlgorithm: function(): Integer; cdecl;
SQUALL_SetBufferSize: function(BufferSize: Integer): Integer; cdecl;
SQUALL_GetBufferSize: function(): Integer; cdecl;
SQUALL_SetMemoryCallbacks: function(UserAlloc,UserFree: pointer): Integer; cdecl;
SQUALL_SetFileCallbacks: function(UserOpen,UserSeek,UserRead,UserClose: Pointer): Integer; cdecl;
SQUALL_GetNumDevice: function(): Integer; cdecl;
SQUALL_GetDeviceName: function(Num: Integer; Buffer: PChar; Size: Integer): Integer; cdecl;
SQUALL_GetDeviceCaps: function(Num: Integer; var Caps:squall_device_caps_t): Integer; cdecl;
SQUALL_GetEAXVersion: function(): Integer; cdecl;
SQUALL_GetChannelsInfo: function(var info: squall_channels_t): Integer; cdecl;
SQUALL_Listener_SetParameters: function(Position,Front,Top,Velocity: PSingle): Integer; cdecl;
SQUALL_Listener_GetParameters: function(var Position,Front,Top,Velocity: PSingle): Integer; cdecl;
SQUALL_Listener_SetVelocity: function(Velocity: PSingle): Integer; cdecl;
SQUALL_Listener_GetVelocity: function(var Velocity: PSingle): Integer; cdecl;
SQUALL_Listener_SetPosition: function(Position: PSingle): Integer; cdecl;
SQUALL_Listener_GetPosition: function(var Position: PSingle): Integer; cdecl;
SQUALL_Listener_SetDistanceFactor: function(DistanceFactor: Single): Integer; cdecl;
SQUALL_Listener_GetDistanceFactor: function(var DistanceFactor: Single): Integer; cdecl;

{
function SQUALL_Listener_SetRolloffFactor(RolloffFactor: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_GetRolloffFactor(var RolloffFactor: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_SetDopplerFactor(DopplerFactor: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_GetDopplerFactor(var DopplerFactor: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_Update(): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_EAX_SetPreset(Preset: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_EAX_SetProperties(Version: Integer;Properties: psquall_eax_listener_t): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_EAX_GetProperties(Version: Integer;var Properties: squall_eax_listener_t): Integer; cdecl; external 'squall.dll';

function SQUALL_Listener_SetWorker(Worker,Param: Pointer;UpdateTime: Cardinal): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_Start(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_Pause(ChannelID,Pause: Integer): Integer; cdecl; external 'squall.dll';
}

SQUALL_Channel_Stop: function(ChannelID: Integer): Integer; cdecl;

{
function SQUALL_Channel_Status(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetVolume(ChannelID,Volume: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetVolume(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetFrequency(ChannelID,Frequency: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetFrequency(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetPlayPosition(ChannelID,Position: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetPlayPosition(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetPlayPositionMs(ChannelID,Position: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetPlayPositionMs(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetWorker(ChannelID: Integer;Worker,Param: Pointer;UpdateTime: Cardinal): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetFragment(ChannelID,Start,Endp: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetFragment(ChannelID: Integer; var Start,Endp: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetFragmentMs(ChannelID,Start,Endp: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetFragmentMs(ChannelID: Integer; var Start,Endp: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetLength(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetLengthMs(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetPriority(ChannelID,Priority: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetPriority(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetLoop(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetLoop(ChannelID,Loop: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetPan(ChannelID,Pan: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetPan(ChannelID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_Set3DPosition(ChannelID: Integer;Position: PSingle): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_Get3DPosition(ChannelID: Integer;Position: PSingle): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetVelocity(ChannelID: Integer;Velocity: PSingle): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetVelocity(ChannelID: Integer;Velocity: PSingle): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetMinMaxDistance(ChannelID: Integer;MinDist,MaxDist: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetMinMaxDistance(ChannelID: Integer;var MinDist,MaxDist: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_SetConeParameters(ChannelID: Integer;Orientation: Single;InsideConeAngle, OutsideConeAngle, OutsideVolume: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_GetConeParameters(ChannelID: Integer; var Orientation: Single; var InsideConeAngle, OutsideConeAngle, OutsideVolume: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_EAX_SetProperties(ChannelID, Version: Integer; EAXProperty: psquall_eax_channel_t): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_EAX_GetProperties(ChannelID, Version: Integer; var EAXProperty: squall_eax_channel_t): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_ZOOMFX_SetProperties(ChannelID: Integer; ZoomFXProperty: psquall_zoomfx_channel_t): Integer; cdecl; external 'squall.dll';

function SQUALL_Channel_ZOOMFX_GetProperties(ChannelID: Integer; var ZoomFXProperty: squall_zoomfx_channel_t): Integer; cdecl; external 'squall.dll';

function SQUALL_ChannelGroup_Pause(ChannelGroupID, Pause: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_ChannelGroup_Stop(ChannelGroupID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_ChannelGroup_SetVolume(ChannelGroupID, Volume: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_ChannelGroup_SetFrequency(ChannelGroupID, Frequency: Integer): Integer; cdecl; external 'squall.dll';
}

SQUALL_Sample_LoadFile: function (FileName: PChar;MemFlag: Integer; Default: psquall_sample_default_t): Integer; cdecl;

{
function SQUALL_Sample_LoadFromMemory(MemoryPtr: Pointer; MemorySize: Cardinal; NewMemory: Integer; Default: psquall_sample_default_t): Integer; cdecl; external 'squall.dll';

procedure SQUALL_Sample_UnloadAll() cdecl; external 'squall.dll';

procedure SQUALL_Sample_Unload(SampleID: Integer) cdecl; external 'squall.dll';

function SQUALL_Sample_GetFileLength(SampleID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_GetFileLengthMs(SampleID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_GetFileFrequency(SampleID: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_SetDefault(SampleID: Integer; Default: psquall_sample_default_t): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_GetDefault(SampleID: Integer; var Default: squall_sample_default_t): Integer; cdecl; external 'squall.dll';
}

SQUALL_Sample_Play: function (SampleID,Loop,Group,Start: Integer): Integer; cdecl;

{
function SQUALL_Sample_PlayEx(SampleID,Loop,Group,Start,Priority,Volume,Frequency,Pan: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_Play3D(SampleID,Loop,Group,Start: Integer; Position,Velocity: PSingle): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_Play3DEx(SampleID,Loop,Group,Start: Integer; Position,Velocity: PSingle;Priority,Volume,Frequency: Integer;MinDist,MaxDist: Single): Integer; cdecl; external 'squall.dll';

function SQUALL_Sample_Pause(SampleID,Pause: Integer): Integer; cdecl; external 'squall.dll';
}

SQUALL_Sample_Stop: function(SampleID: Integer): Integer; cdecl;

{
function SQUALL_SampleGroup_Play(SoundGroupID,Loop,Group,Start: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_SampleGroup_PlayEx(SoundGroupID,Loop,Group,Start,Priority,Volume,Frequency,Pan: Integer): Integer; cdecl; external 'squall.dll';

function SQUALL_SampleGroup_Play3D(SoundGroupID,Loop,Group,Start: Integer; Position,Velocity: PSingle): Integer; cdecl; external 'squall.dll';

function SQUALL_SampleGroup_Play3DEx(SoundGroupID,Loop,Group,Start: Integer; Position,Velocity: PSingle; Priority,Volume,Frequency: Integer;MinDist,MaxDist: Single): Integer; cdecl; external 'squall.dll';
}

function InitSquall(dllName: pchar): boolean;
procedure CloseSquall;

implementation

uses moduleloader;

var
  IsSquallInitialized: boolean = False;
  vSquallHandle: TModuleHandle;

function InitSquall(dllName: pchar): boolean;
begin
  result := IsSquallInitialized;
  if not IsSquallInitialized then
  begin
    IsSquallInitialized := LoadModule(vSquallHandle, dllName);
    result := IsSquallInitialized;

    if IsSquallInitialized then
    begin
      SQUALL_Init := GetModuleSymbol(vSquallHandle, 'SQUALL_Init');
      SQUALL_Free := GetModuleSymbol(vSquallHandle, 'SQUALL_Free');
      SQUALL_Channel_Stop := GetModuleSymbol(vSquallHandle, 'SQUALL_Channel_Stop');
      SQUALL_Sample_LoadFile := GetModuleSymbol(vSquallHandle, 'SQUALL_Sample_LoadFile');
      SQUALL_Sample_Play := GetModuleSymbol(vSquallHandle, 'SQUALL_Sample_Play');
    end;
  end;
end;

procedure CloseSquall;
begin
  IsSquallInitialized := false;
  UnLoadModule(vSquallHandle);
end;

end.
