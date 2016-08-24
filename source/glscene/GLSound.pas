{: GLSound<p>

	Base classes and interface for GLScene Sound System<p>

	<b>History : </b><font size=-1><ul>
      <li>22/07/02 - EG - SetMute/SetPause fix (Sternas Stefanos)
      <li>02/07/02 - EG - Persistence fix (MP3 / Sternas Stefanos)
      <li>05/03/02 - EG - TGLBSoundEmitter.Loaded
      <li>27/02/02 - EG - Added 3D Factors, special listener-is-camera support
      <li>13/01/01 - EG - Added CPUUsagePercent
      <li>09/06/00 - EG - Various enhancements
	   <li>04/06/00 - EG - Creation
	</ul></font>
}
unit GLSound;

interface

uses Classes, GLSoundFileObjects, GLScene, XCollection, VectorGeometry, GLCadencer,
     GLMisc;

{$i GLScene.inc}

type

	// TGLSoundSample
	//
   {: Stores a single PCM coded sound sample. }
	TGLSoundSample = class (TCollectionItem)
	   private
	      { Private Declarations }
         FName : String;
         FData : TGLSoundFile;

	   protected
	      { Protected Declarations }
			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream); virtual;
			procedure WriteData(Stream: TStream); virtual;
         function GetDisplayName : String; override;
         procedure SetData(const val : TGLSoundFile);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         procedure LoadFromFile(const fileName : String);

         procedure PlayOnWaveOut;

         function Sampling : TGLSoundSampling;
	      function LengthInBytes : Integer;
	      function LengthInSamples : Integer;
	      function LengthInSec : Single;

	   published
	      { Published Declarations }
         property Name : String read FName write FName;
         property Data : TGLSoundFile read FData write SetData stored False;
	end;

	// TGLSoundSamples
	//
	TGLSoundSamples = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TGLSoundSample);
	      function GetItems(index : Integer) : TGLSoundSample;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);
         function Add: TGLSoundSample;
	      function FindItemID(ID : Integer) : TGLSoundSample;
	      property Items[index : Integer] : TGLSoundSample read GetItems write SetItems; default;
         function GetByName(const aName : String) : TGLSoundSample;

         function AddFile(const fileName : String; const sampleName : String = '') : TGLSoundSample;
   end;

	// TGLSoundLibrary
	//
	TGLSoundLibrary = class (TComponent)
	   private
	      { Private Declarations }
         FSamples : TGLSoundSamples;

	   protected
	      { Protected Declarations }
         procedure SetSamples(const val : TGLSoundSamples);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

	   published
	      { Published Declarations }
         property Samples : TGLSoundSamples read FSamples write SetSamples;
	end;

   // TGLSoundSource
   //
   TGLSoundSourceChange = (sscTransformation, sscSample, sscStatus);
   TGLSoundSourceChanges = set of TGLSoundSourceChange;

   TGLBSoundEmitter = class;

	// TGLBaseSoundSource
	//
   {: Base class for origin of sound playback. }
	TGLBaseSoundSource = class (TCollectionItem)
	   private
	      { Private Declarations }
         FBehaviourToNotify : TGLBSoundEmitter; // private only, NOT persistent, not assigned
         FPriority : Integer;
         FOrigin : TGLBaseSceneObject;   // NOT persistent
         FVolume : Single;
         FMinDistance, FMaxDistance : Single;
         FInsideConeAngle, FOutsideConeAngle : Single;
         FConeOutsideVolume : Single;
         FSoundLibraryName : String;   // used for persistence
         FSoundLibrary : TGLSoundLibrary;  // persistence via name
         FSoundName : String;
         FMute : Boolean;
         FPause : Boolean;
         FChanges : TGLSoundSourceChanges; // NOT persistent, not assigned
         FNbLoops : Integer;
         FTag : Integer;                   // NOT persistent, not assigned
         FFrequency : Integer;

	   protected
	      { Protected Declarations }
			procedure WriteToFiler(writer : TWriter);
         procedure ReadFromFiler(reader : TReader);

         function GetDisplayName : String; override;
         procedure SetPriority(const val : Integer);
         procedure SetOrigin(const val : TGLBaseSceneObject);
         procedure SetVolume(const val : Single);
         procedure SetMinDistance(const val : Single);
         procedure SetMaxDistance(const val : Single);
         procedure SetInsideConeAngle(const val : Single);
         procedure SetOutsideConeAngle(const val : Single);
         procedure SetConeOutsideVolume(const val : Single);
         function  GetSoundLibrary : TGLSoundLibrary;
         procedure SetSoundLibrary(const val : TGLSoundLibrary);
         procedure SetSoundName(const val : String);
         procedure SetMute(const val : Boolean);
         procedure SetPause(const val : Boolean);
         procedure SetNbLoops(const val : Integer);
         procedure SetFrequency(const val : Integer);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         property Changes : TGLSoundSourceChanges read FChanges;

         function Sample : TGLSoundSample;

         //: This Tag is reserved for sound manager use only
         property ManagerTag : Integer read FTag write FTag;

         {: Origin object for the sound sources.<p>
            Absolute object position/orientation are taken into account, the
            object's TGLBInertia is considered if any.<p>
            If origin is nil, the source is assumed to be static at the origin.<p>
            <b>Note :</b> since TCollectionItem do not support the "Notification"
            scheme, it is up to the Origin object to take care of updating this
            property prior to release/destruction. }
         property Origin : TGLBaseSceneObject read FOrigin write SetOrigin;

	   published
	      { Published Declarations }
         property SoundLibrary : TGLSoundLibrary read GetSoundLibrary write SetSoundLibrary;
         property SoundName : String read FSoundName write SetSoundName;

         {: Volume of the source, [0.0; 1.0] range }
         property Volume : Single read FVolume write SetVolume;
         {: Nb of playing loops. }
         property NbLoops : Integer read FNbLoops write SetNbLoops default 1;

         property Mute : Boolean read FMute write SetMute default False;
         property Pause : Boolean read FPause write SetPause default False;

         {: Sound source priority, the higher the better.<p>
            When maximum number of sound sources is reached, only the sources
            with the highest priority will continue to play, however, even
            non-playing sources should be tracked by the manager, thus allowing
            an "unlimited" amount of sources from the application point of view. }
         property Priority : Integer read FPriority write SetPriority default 0;

         {: Min distance before spatial attenuation occurs.<p>
            1.0 by default }
         property MinDistance : Single read FMinDistance write SetMinDistance;
         {: Max distance, if source is further away, it will not be heard.<p>
            100.0 by default }
         property MaxDistance : Single read FMaxDistance write SetMaxDistance;

         {: Inside cone angle, [0°; 360°].<p>
            Sound volume is maximal within this cone.<p>
            See DirectX SDK for details. }
         property InsideConeAngle : Single read FInsideConeAngle write SetInsideConeAngle;
         {: Outside cone angle, [0°; 360°].<p>
            Between inside and outside cone, sound volume decreases between max
            and cone outside volume.<p>
            See DirectX SDK for details. }
         property OutsideConeAngle : Single read FOutsideConeAngle write SetOutsideConeAngle;
         {: Cone outside volume, [0.0; 1.0] range.<p>
            See DirectX SDK for details. }
         property ConeOutsideVolume : Single read FConeOutsideVolume write SetConeOutsideVolume;
         {: Sample custom playback frequency.<p>
            Values null or negative are interpreted as 'default frequency'. }
         property Frequency : Integer read FFrequency write SetFrequency default -1;
	end;

	// TGLSoundSource
	//
   {: Origin of sound playback.<p>
      Just publishes the 'Origin' property.<p>
      Note that the "orientation" is the the source's Direction, ie. the "Z"
      vector. }
	TGLSoundSource = class (TGLBaseSoundSource)
      public
	      { Public Declarations }
	      destructor Destroy; override;

	   published
	      { Published Declarations }
         property Origin;
   end;

	// TGLSoundSources
	//
	TGLSoundSources = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TGLSoundSource);
	      function GetItems(index : Integer) : TGLSoundSource;

         function Add: TGLSoundSource;
	      function FindItemID(ID: Integer): TGLSoundSource;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);

	      property Items[index : Integer] : TGLSoundSource read GetItems write SetItems; default;
   end;

   // TGLSoundEnvironment
   //
   {: EAX standard sound environments. }
   TGLSoundEnvironment = (seDefault, sePaddedCell, seRoom, seBathroom,
                          seLivingRoom, seStoneroom, seAuditorium,
                          seConcertHall, seCave, seArena, seHangar,
                          seCarpetedHallway, seHallway, seStoneCorridor,
                          seAlley, seForest, seCity, seMountains, seQuarry,
                          sePlain, seParkingLot, seSewerPipe, seUnderWater,
                          seDrugged, seDizzy, sePsychotic);

	// TGLSoundManager
	//
   {: Base class for sound manager components.<p>
      The sound manager component is the interface to a low-level audio API
      (like DirectSound), there can only be one active manager at any time
      (this class takes care of this).<p>
      Subclass should override the DoActivate and DoDeActivate protected methods
      to "initialize/unitialize" their sound layer, actual data releases should
      occur in destructor however. }
	TGLSoundManager = class (TGLCadenceAbleComponent)
	   private
	      { Private Declarations }
         FActive : Boolean;
         FMute : Boolean;
         FPause : Boolean;
         FMasterVolume : Single;
         FListener : TGLBaseSceneObject;
         FLastListenerPosition : TVector;
         FSources : TGLSoundSources;
         FMaxChannels : Integer;
         FOutputFrequency : Integer;
         FUpdateFrequency : Single;
         FDistanceFactor : Single;
         FRollOffFactor : Single;
         FDopplerFactor : Single;
         FSoundEnvironment : TGLSoundEnvironment;
         FLastUpdateTime, FLastDeltaTime : Single; // last time UpdateSources was fired, not persistent
         FCadencer : TGLCadencer;
         procedure SetActive(const val : Boolean);
         procedure SetMute(const val : Boolean);
         procedure SetPause(const val : Boolean);
         procedure WriteDoppler(writer : TWriter);
         procedure ReadDoppler(reader : TReader);

	   protected
	      { Protected Declarations }
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetSources(const val : TGLSoundSources);
         procedure SetMasterVolume(const val : Single);
         procedure SetListener(const val : TGLBaseSceneObject);
         procedure SetMaxChannels(const val : Integer);
         procedure SetOutputFrequency(const val : Integer);
         procedure SetUpdateFrequency(const val : Single);
         function  StoreUpdateFrequency : Boolean;
         procedure SetCadencer(const val : TGLCadencer);
         procedure SetDistanceFactor(const val : Single);
         function  StoreDistanceFactor : Boolean;
         procedure SetRollOffFactor(const val : Single);
         function  StoreRollOffFactor : Boolean;
         procedure SetDopplerFactor(const val : Single);
         procedure SetSoundEnvironment(const val : TGLSoundEnvironment);

         procedure Loaded; override;
         procedure DefineProperties(Filer: TFiler); override;
         
         procedure ListenerCoordinates(var position, velocity, direction, up : TVector);

	      function DoActivate : Boolean; dynamic;
         //: Invoked AFTER all sources have been stopped
	      procedure DoDeActivate; dynamic;
         {: Effect mute of all sounds.<p>
            Default implementation call MuteSource for all non-muted sources
            with "True" as parameter. }
	      function DoMute : Boolean; dynamic;
         {: Effect un-mute of all sounds.<p>
            Default implementation call MuteSource for all non-muted sources
            with "False" as parameter. }
	      procedure DoUnMute; dynamic;
         {: Effect pause of all sounds.<p>
            Default implementation call PauseSource for all non-paused sources
            with "True" as parameter. }
	      function DoPause : Boolean; dynamic;
         {: Effect un-pause of all sounds.<p>
            Default implementation call PauseSource for all non-paused sources
            with "True" as parameter. }
	      procedure DoUnPause; dynamic;

         procedure NotifyMasterVolumeChange; dynamic;
         procedure Notify3DFactorsChanged; dynamic;
         procedure NotifyEnvironmentChanged; dynamic;

         //: Called when a source will be freed
         procedure KillSource(aSource : TGLBaseSoundSource); virtual;
         {: Request to update source's data in low-level sound API.<p>
            Default implementation just clears the "Changes" flags. }
         procedure UpdateSource(aSource : TGLBaseSoundSource); virtual;
         procedure MuteSource(aSource : TGLBaseSoundSource; muted : Boolean); virtual;
         procedure PauseSource(aSource : TGLBaseSoundSource; paused : Boolean); virtual;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         {: Manual request to update all sources to reflect changes.<p>
            Default implementation invokes UpdateSource for all known sources. }
         procedure UpdateSources; virtual;
         {: Stop and free all sources. }
         procedure StopAllSources;

         {: Progress notification for time synchronization.<p>
            This method will call UpdateSources depending on the last time
            it was performed and the value of the UpdateFrequency property. }
			procedure DoProgress(const progressTime : TProgressTimes); override;

         {: Sound manager API reported CPU Usage.<p>
            Returns -1 when unsupported. }
         function CPUUsagePercent : Single; virtual;
         {: True if EAX is supported. }
         function EAXSupported : Boolean; dynamic;

	   published
	      { Published Declarations }
         {: Activation/deactivation of the low-level sound API }
         property Active : Boolean read FActive write SetActive default False;

         {: Maximum number of sound output channels.<p>
            While some drivers will just ignore this value, others cannot
            dynamically adjust the maximum number of channels (you need to
            de-activate and re-activate the manager for this property to be
            taken into account). }
         property MaxChannels : Integer read FMaxChannels write SetMaxChannels default 8;
         {: Sound output mixing frequency.<p>
            Commonly used values ar 11025, 22050 and 44100.<p>
            Note that most driver cannot dynamically adjust the output frequency
            (you need to de-ativate and re-activate the manager for this property
            to be taken into account). }
         property OutputFrequency : Integer read FOutputFrequency write SetOutputFrequency default 44100;

         {: Request to mute all sounds.<p>
            All sound requests should be handled as if sound is unmuted though,
            however drivers should try to take a CPU advantage of mute over
            MasterVolume=0 }
         property Mute : Boolean read FMute write SetMute default False;
         {: Request to pause all sound, sound output should be muted too.<p>
            When unpausing, all sound should resume at the point they were paused. }
         property Pause : Boolean read FPause write SetPause default False;
         {: Master Volume adjustement in the [0.0; 1.0] range.<p>
            Driver should take care of properly clamping the master volume. }
         property MasterVolume : Single read FMasterVolume write SetMasterVolume;

         {: Scene object that materializes the listener.<p>
            The sceneobject's AbsolutePosition and orientation are used to define
            the listener coordinates, velocity is automatically calculated
            (if you're using DoProgress or connected the manager to a cadencer).<p>
            If this property is nil, the listener is assumed to be static at
            the NullPoint coordinate, facing Z axis, with up being Y (ie. the
            default GLScene orientation). }
         property Listener : TGLBaseSceneObject read FListener write SetListener;
         {: Currently active and playing sound sources. }
         property Sources : TGLSoundSources read FSources write SetSources;

         {: Update frequency for time-based control (DoProgress).<p>
            Default value is 10 Hz (frequency is clamped in the 1Hz-60Hz range). }
         property UpdateFrequency : Single read FUpdateFrequency write SetUpdateFrequency stored StoreUpdateFrequency;
         {: Cadencer for time-based control.<p> }
         property Cadencer : TGLCadencer read FCadencer write SetCadencer;
         {: Engine relative distance factor, compared to 1.0 meters.<p>
            Equates to 'how many units per meter' your engine has. }
         property DistanceFactor : Single read FDistanceFactor write SetDistanceFactor stored StoreDistanceFactor;
         {: Sets the global attenuation rolloff factor.<p>
            Normally volume for a sample will scale at 1 / distance.
            This gives a logarithmic attenuation of volume as the source gets
            further away (or closer).<br>
            Setting this value makes the sound drop off faster or slower.
            The higher the value, the faster volume will fall off. }
         property RollOffFactor : Single read FRollOffFactor write SetRollOffFactor stored StoreRollOffFactor;
         {: Engine relative Doppler factor, compared to 1.0 meters.<p>
            Equates to 'how many units per meter' your engine has. }
         property DopplerFactor : Single read FDopplerFactor write SetDopplerFactor stored False;
         {: Sound environment (requires EAX compatible soundboard). }
         property Environment : TGLSoundEnvironment read FSoundEnvironment write SetSoundEnvironment default seDefault;
	end;

	// TGLBSoundEmitter
	//
	{: A sound emitter behaviour, plug it on any object to make it noisy.<p>
      This behaviour is just an interface to a TGLSoundSource, for editing
      convenience. }
	TGLBSoundEmitter = class (TGLBehaviour)
		private
			{ Private Declarations }
         FPlaying : Boolean; // used at design-time ONLY
         FSource : TGLBaseSoundSource;
         FPlayingSource : TGLSoundSource;

		protected
			{ Protected Declarations }
			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;
         procedure Loaded; override;

         procedure SetSource(const val : TGLBaseSoundSource);
         procedure SetPlaying(const val : Boolean);
         function GetPlaying : Boolean;

         procedure NotifySourceDestruction(aSource : TGLSoundSource);

		public
			{ Public Declarations }
			constructor Create(aOwner : TXCollection); override;
			destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;
			class function UniqueItem : Boolean; override;

			procedure DoProgress(const progressTime : TProgressTimes); override;

         property PlayingSource : TGLSoundSource read FPlayingSource;

		published
			{ Published Declarations }
         property Source : TGLBaseSoundSource read FSource write SetSource;
         property Playing : Boolean read GetPlaying write SetPlaying default False;

	end;

	// TGLSMWaveOut
	//
   {: Basic sound manager based on WinMM <i>waveOut</i> function.<p>
      This manager has NO 3D miximing capacity, this is merely a default manager
      that should work on any system, and help showcasing/testing basic GLSS
      core functionality.<p>
      Apart from 3D, mute, pause, priority and volume are ignored too, and only
      sampling conversions supported by the windows ACM driver are supported
      (ie. no 4bits samples playback etc.). }
	TGLSMWaveOut = class (TGLSoundManager)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

      published
	      { Published Declarations }
         property MaxChannels default 4;
	end;

procedure Register;

function ActiveSoundManager : TGLSoundManager;
function GetSoundLibraryByName(const aName : String) : TGLSoundLibrary;

function GetOrCreateSoundEmitter(behaviours : TGLBehaviours) : TGLBSoundEmitter; overload;
function GetOrCreateSoundEmitter(obj : TGLBaseSceneObject) : TGLBSoundEmitter; overload;

var
   // If this variable is true, errors in GLSM may be displayed to the user
   vVerboseGLSMErrors : Boolean = True;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, MMSystem, GLCrossPlatform;

var
   vActiveSoundManager : TGLSoundManager;
   vSoundLibraries : TList;

// Register
//
procedure Register;
begin
  RegisterComponents('GLScene', [TGLSoundLibrary, TGLSMWaveOut]);
end;

// ActiveSoundManager
//
function ActiveSoundManager : TGLSoundManager;
begin
   Result:=vActiveSoundManager;
end;

// GetSoundLibraryByName
//
function GetSoundLibraryByName(const aName : String) : TGLSoundLibrary;
var
   i : Integer;
begin
   Result:=nil;
   if Assigned(vSoundLibraries) then for i:=0 to vSoundLibraries.Count-1 do
      if TGLSoundLibrary(vSoundLibraries[i]).Name=aName then begin
         Result:=TGLSoundLibrary(vSoundLibraries[i]);
         Break;
      end;
end;

// GetOrCreateSoundEmitter (TGLBehaviours)
//
function GetOrCreateSoundEmitter(behaviours : TGLBehaviours) : TGLBSoundEmitter;
var
	i : Integer;
begin
	i:=behaviours.IndexOfClass(TGLBSoundEmitter);
	if i>=0 then
		Result:=TGLBSoundEmitter(behaviours[i])
	else Result:=TGLBSoundEmitter.Create(behaviours);
end;

// GetOrCreateSoundEmitter (TGLBaseSceneObject)
//
function GetOrCreateSoundEmitter(obj : TGLBaseSceneObject) : TGLBSoundEmitter;
begin
	Result:=GetOrCreateSoundEmitter(obj.Behaviours);
end;

// ------------------
// ------------------ TGLSoundSample ------------------
// ------------------

// Create
//
constructor TGLSoundSample.Create(Collection : TCollection);
begin
	inherited Create(Collection);
end;

// Destroy
//
destructor TGLSoundSample.Destroy;
begin
   FData.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLSoundSample.Assign(Source: TPersistent);
begin
	if Source is TGLSoundSample then begin
      FName:=TGLSoundSample(Source).Name;
      FData.Free;
      FData:=TGLSoundFile(TGLSoundSample(Source).Data.CreateCopy(Self));
	end;
	inherited Destroy;
end;

// DefineProperties
//
procedure TGLSoundSample.DefineProperties(Filer: TFiler);
begin
   Filer.DefineBinaryProperty('BinData', ReadData, WriteData, Assigned(FData));
end;

// ReadData
//
procedure TGLSoundSample.ReadData(Stream: TStream);
var
   n : Integer;
   clName : String;
begin
   with Stream do begin
      Read(n, SizeOf(Integer));
      SetLength(clName, n);
      if n>0 then
         Read(clName[1], n);
      FData:=TGLSoundFileClass(FindClass(clName)).Create(Self);
      FData.LoadFromStream(Stream);
   end;
end;

// WriteData
//
procedure TGLSoundSample.WriteData(Stream: TStream);
var
   n : Integer;
   buf : String;
begin
   with Stream do begin
      n:=Length(FData.ClassName);
      Write(n, SizeOf(Integer));
      buf:=FData.ClassName;
      if n>0 then
         Write(buf[1], n);
      FData.SaveToStream(Stream);
   end;
end;

// GetDisplayName
//
function TGLSoundSample.GetDisplayName : String;
var
   s : String;
begin
   if Assigned(FData) then begin
      if Data.Sampling.NbChannels>1 then s:='s' else s:='';
   	Result:=Format('%s (%d Hz, %d bits, %d channel%s, %.2f sec)',
                     [Name, Data.Sampling.Frequency,
                      Data.Sampling.BitsPerSample,
                      Data.Sampling.NbChannels, s, LengthInSec])
   end else Result:=Format('%s (empty)', [Name]);
end;

// LoadFromFile
//
procedure TGLSoundSample.LoadFromFile(const fileName : String);
var
   sfc : TGLSoundFileClass;
begin
   FData.Free;
   sfc:=GetGLSoundFileFormats.FindExt(ExtractFileExt(fileName));
   if Assigned(sfc) then begin
      FData:=sfc.Create(Self);
      FData.LoadFromFile(fileName);
   end else FData:=nil;
   Name:=ExtractFileName(fileName);
end;

// PlayOnWaveOut
//
procedure TGLSoundSample.PlayOnWaveOut;
begin
   if Assigned(FData) then
      FData.PlayOnWaveOut;
end;

// TGLSoundSample
//
function TGLSoundSample.Sampling : TGLSoundSampling;
begin
   if Assigned(FData) then
      Result:=FData.Sampling
   else Result:=nil;
end;

// LengthInBytes
//
function TGLSoundSample.LengthInBytes : Integer;
begin
   if Assigned(FData) then
      Result:=FData.LengthInBytes
   else Result:=0;
end;

// LengthInSamples
//
function TGLSoundSample.LengthInSamples : Integer;
begin
   if Assigned(FData) then
      Result:=FData.LengthInSamples
   else Result:=0;
end;

// LengthInSec
//
function TGLSoundSample.LengthInSec : Single;
begin
   if Assigned(FData) then
      Result:=FData.LengthInSec
   else Result:=0;
end;

// SetData
//
procedure TGLSoundSample.SetData(const val : TGLSoundFile);
begin
   FData.Free;
   if Assigned(val) then
      FData:=TGLSoundFile(val.CreateCopy(Self))
   else FData:=nil;
end;

// ------------------
// ------------------ TGLSoundSamples ------------------
// ------------------

constructor TGLSoundSamples.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TGLSoundSample);
end;

function TGLSoundSamples.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TGLSoundSamples.SetItems(index : Integer; const val : TGLSoundSample);
begin
	inherited Items[index]:=val;
end;

function TGLSoundSamples.GetItems(index : Integer) : TGLSoundSample;
begin
	Result:=TGLSoundSample(inherited Items[index]);
end;

function TGLSoundSamples.Add: TGLSoundSample;
begin
	Result:=(inherited Add) as TGLSoundSample;
end;

function TGLSoundSamples.FindItemID(ID: Integer): TGLSoundSample;
begin
	Result:=(inherited FindItemID(ID)) as TGLSoundSample;
end;

// GetByName
//
function TGLSoundSamples.GetByName(const aName : String) : TGLSoundSample;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to Count-1 do if CompareText(Items[i].Name, aName)=0 then begin
      Result:=Items[i];
      Break;
   end;
end;

// AddFile
//
function TGLSoundSamples.AddFile(const fileName : String; const sampleName : String = '') : TGLSoundSample;
begin
   Result:=Add;
   Result.LoadFromFile(fileName);
   if sampleName<>'' then
      Result.Name:=sampleName;
end;

// ------------------
// ------------------ TGLSoundLibrary ------------------
// ------------------

constructor TGLSoundLibrary.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   FSamples:=TGLSoundSamples.Create(Self);
   vSoundLibraries.Add(Self);
end;

destructor TGLSoundLibrary.Destroy;
begin
   vSoundLibraries.Remove(Self);
   FSamples.Free;
	inherited Destroy;
end;

// Notification
//
procedure TGLSoundLibrary.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
end;

// SetSamples
//
procedure TGLSoundLibrary.SetSamples(const val : TGLSoundSamples);
begin
   FSamples.Assign(val);
end;

// ------------------
// ------------------ TGLBaseSoundSource ------------------
// ------------------

// Create
//
constructor TGLBaseSoundSource.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FChanges:=[sscTransformation, sscSample, sscStatus];
   FVolume:=1.0;
   FMinDistance:=1.0; FMaxDistance:=100.0;
   FInsideConeAngle:=360;
   FOutsideConeAngle:=360;
   FConeOutsideVolume:=0.0;
   FNbLoops:=1;
   FFrequency:=-1;
end;

// Destroy
//
destructor TGLBaseSoundSource.Destroy;
begin
	inherited Destroy;
end;

// GetDisplayName
//
function TGLBaseSoundSource.GetDisplayName : String;
begin
	Result:=Format('%s', [FSoundName]);
end;

// Assign
//
procedure TGLBaseSoundSource.Assign(Source: TPersistent);
begin
   if Source is TGLBaseSoundSource then begin
      FPriority:=TGLBaseSoundSource(Source).FPriority;
      FOrigin:=TGLBaseSoundSource(Source).FOrigin;
      FVolume:=TGLBaseSoundSource(Source).FVolume;
      FMinDistance:=TGLBaseSoundSource(Source).FMinDistance;
      FMaxDistance:=TGLBaseSoundSource(Source).FMaxDistance;
      FInsideConeAngle:=TGLBaseSoundSource(Source).FInsideConeAngle;
      FOutsideConeAngle:=TGLBaseSoundSource(Source).FOutsideConeAngle;
      FConeOutsideVolume:=TGLBaseSoundSource(Source).FConeOutsideVolume;
      FSoundLibraryName:=TGLBaseSoundSource(Source).FSoundLibraryName;
      FSoundLibrary:=TGLBaseSoundSource(Source).FSoundLibrary;
      FSoundName:=TGLBaseSoundSource(Source).FSoundName;
      FMute:=TGLBaseSoundSource(Source).FMute;
      FPause:=TGLBaseSoundSource(Source).FPause;
      FChanges:=[sscTransformation, sscSample, sscStatus];
      FNbLoops:=TGLBaseSoundSource(Source).FNbLoops;
      FFrequency:=TGLBaseSoundSource(Source).FFrequency;
   end else inherited Assign(Source);
end;

// WriteToFiler
//
procedure TGLBaseSoundSource.WriteToFiler(writer : TWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteInteger(FPriority);
      WriteFloat(FVolume);
      WriteFloat(FMinDistance);
      WriteFloat(FMaxDistance);
      WriteFloat(FInsideConeAngle);
      WriteFloat(FOutsideConeAngle);
      WriteFloat(FConeOutsideVolume);
      if Assigned(FSoundLibrary) then
         WriteString(FSoundLibrary.Name)
      else WriteString(FSoundLibraryName);
      WriteString(FSoundName);
      WriteBoolean(FMute);
      WriteBoolean(FPause);
      WriteInteger(FNbLoops);
//      WriteInteger(FFrequency);
   end;
end;

// ReadFromFiler
//
procedure TGLBaseSoundSource.ReadFromFiler(reader : TReader);
begin
   inherited;
   with reader do begin
      ReadInteger; // ignore archiveVersion
      FPriority:=ReadInteger;
      FVolume:=ReadFloat;
      FMinDistance:=ReadFloat;
      FMaxDistance:=ReadFloat;
      FInsideConeAngle:=ReadFloat;
      FOutsideConeAngle:=ReadFloat;
      FConeOutsideVolume:=ReadFloat;
      FSoundLibraryName:=ReadString;
      FSoundLibrary:=nil;
      FSoundName:=ReadString;
      FMute:=ReadBoolean;
      FPause:=ReadBoolean;
      FChanges:=[sscTransformation, sscSample, sscStatus];
      FNbLoops:=ReadInteger;
//      FFrequency:=ReadInteger;
   end;
end;

// Sample
//
function TGLBaseSoundSource.Sample : TGLSoundSample;
begin
   if SoundLibrary<>nil then
      Result:=FSoundLibrary.Samples.GetByName(FSoundName)
   else Result:=nil;
end;

// SetPriority
//
procedure TGLBaseSoundSource.SetPriority(const val : Integer);
begin
   if val<>FPriority then begin
      FPriority:=val;
      Include(FChanges, sscStatus);
   end;
end;

// SetOrigin
//
procedure TGLBaseSoundSource.SetOrigin(const val : TGLBaseSceneObject);
begin
   if val<>FOrigin then begin
      FOrigin:=val;
      Include(FChanges, sscTransformation);
   end;
end;

// SetVolume
//
procedure TGLBaseSoundSource.SetVolume(const val : Single);
begin
   if val<>FVolume then begin
      FVolume:=ClampValue(val, 0, 1);
      Include(FChanges, sscStatus);
   end;
end;

// SetMinDistance
//
procedure TGLBaseSoundSource.SetMinDistance(const val : Single);
begin
   if val<>FMinDistance then begin
      FMinDistance:=ClampValue(val, 0);
      Include(FChanges, sscStatus);
   end;
end;

// SetMaxDistance
//
procedure TGLBaseSoundSource.SetMaxDistance(const val : Single);
begin
   if val<>FMaxDistance then begin
      FMaxDistance:=ClampValue(val, 0);
      Include(FChanges, sscStatus);
   end;
end;

// SetInsideConeAngle
//
procedure TGLBaseSoundSource.SetInsideConeAngle(const val : Single);
begin
   if val<>FInsideConeAngle then begin
      FInsideConeAngle:=ClampValue(val, 0, 360);
      Include(FChanges, sscStatus);
   end;
end;

// SetOutsideConeAngle
//
procedure TGLBaseSoundSource.SetOutsideConeAngle(const val : Single);
begin
   if val<>FOutsideConeAngle then begin
      FOutsideConeAngle:=ClampValue(val, 0, 360);
      Include(FChanges, sscStatus);
   end;
end;

// SetConeOutsideVolume
//
procedure TGLBaseSoundSource.SetConeOutsideVolume(const val : Single);
begin
   if val<>FConeOutsideVolume then begin
      FConeOutsideVolume:=ClampValue(val, 0, 1);
      Include(FChanges, sscStatus);
   end;
end;

// GetSoundLibrary
//
function TGLBaseSoundSource.GetSoundLibrary : TGLSoundLibrary;
begin
   if (FSoundLibrary=nil) and (FSoundLibraryName<>'') then
      FSoundLibrary:=GetSoundLibraryByName(FSoundLibraryName);
   Result:=FSoundLibrary;
end;

// SetSoundLibrary
//
procedure TGLBaseSoundSource.SetSoundLibrary(const val : TGLSoundLibrary);
begin
   if val<>FSoundLibrary then begin
      FSoundLibrary:=val;
      if Assigned(FSoundLibrary) then
         FSoundLibraryName:=FSoundLibrary.Name
      else FSoundLibraryName:='';
      Include(FChanges, sscSample);
   end;
end;

// SetSoundName
//
procedure TGLBaseSoundSource.SetSoundName(const val : String);
begin
   if val<>FSoundName then begin
      FSoundName:=val;
      Include(FChanges, sscSample);
   end;
end;

// SetPause
//
procedure TGLBaseSoundSource.SetPause(const val : Boolean);
begin
   if val<>FPause then begin
      FPause:=val;
      if Collection<>nil then
         TGLSoundManager(TGLSoundSources(Collection).owner).PauseSource(Self, FPause);
   end;
end;

// SetNbLoops
//
procedure TGLBaseSoundSource.SetNbLoops(const val : Integer);
begin
   if val<>FNbLoops then begin
      FNbLoops:=val;
      Include(FChanges, sscSample);
   end;
end;

// SetFrequency
//
procedure TGLBaseSoundSource.SetFrequency(const val : integer);
begin
   if val<>FFrequency then begin
      FFrequency:=val;
      Include(FChanges, sscStatus);
   end;
end;

// SetMute
//
procedure TGLBaseSoundSource.SetMute(const val : Boolean);
begin
   if val<>FMute then begin
      FMute:=val;
      if Collection<>nil then
         TGLSoundManager(TGLSoundSources(Collection).owner).MuteSource(Self, FMute);
   end;
end;

// ------------------
// ------------------ TGLSoundSource ------------------
// ------------------

// Destroy
//
destructor TGLSoundSource.Destroy;
begin
   if Assigned(FBehaviourToNotify) then
      FBehaviourToNotify.NotifySourceDestruction(Self);
   if Collection<>nil then
      ((Collection as TGLSoundSources).Owner as TGLSoundManager).KillSource(Self);
   inherited;   
end;

// ------------------
// ------------------ TGLSoundSources ------------------
// ------------------

constructor TGLSoundSources.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TGLSoundSource);
end;

function TGLSoundSources.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TGLSoundSources.SetItems(index : Integer; const val : TGLSoundSource);
begin
	inherited Items[index]:=val;
end;

function TGLSoundSources.GetItems(index : Integer) : TGLSoundSource;
begin
	Result:=TGLSoundSource(inherited Items[index]);
end;

function TGLSoundSources.Add: TGLSoundSource;
begin
	Result:=(inherited Add) as TGLSoundSource;
end;

function TGLSoundSources.FindItemID(ID: Integer): TGLSoundSource;
begin
	Result:=(inherited FindItemID(ID)) as TGLSoundSource;
end;

// ------------------
// ------------------ TGLSoundManager ------------------
// ------------------

// Create
//
constructor TGLSoundManager.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   FSources:=TGLSoundSources.Create(Self);
   FMasterVolume:=1.0;
   FOutputFrequency:=44100;
   FMaxChannels:=8;
   FUpdateFrequency:=10;
   FLastUpdateTime:=-1e30;
   FDistanceFactor:=1.0;
   FRollOffFactor:=1.0;
   FDopplerFactor:=1.0;
end;

// Destroy
//
destructor TGLSoundManager.Destroy;
begin
   Active:=False;
   Listener:=nil;
   FSources.Free;
	inherited Destroy;
end;

// Notification
//
procedure TGLSoundManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FListener then Listener:=nil;
      if AComponent=FCadencer then Cadencer:=nil;
   end;
   inherited;
end;

// SetActive
//
procedure TGLSoundManager.SetActive(const val : Boolean);
begin
   if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      FActive:=val
   else if val<>FActive then begin
      if val then begin
         if Assigned(vActiveSoundManager) then
            vActiveSoundManager.Active:=False;
         if DoActivate then begin
            FActive:=True;
            vActiveSoundManager:=Self;
         end;
      end else begin
         try
            StopAllSources;
            DoDeActivate;
         finally
            FActive:=val;
            vActiveSoundManager:=nil;
         end;
      end;
   end;
end;

// Activate
//
function TGLSoundManager.DoActivate : Boolean;
begin
	Result:=True;
end;

// DeActivate
//
procedure TGLSoundManager.DoDeActivate;
begin
   StopAllSources;
end;

// SetMute
//
procedure TGLSoundManager.SetMute(const val : Boolean);
begin
   if val<>FMute then begin
      if val then begin
         if DoMute then FMute:=True
      end else begin
         DoUnMute;
         FMute:=False;
      end;
   end;
end;

// DoMute
//
function TGLSoundManager.DoMute : Boolean;
var
   i : Integer;
begin
   for i:=0 to Sources.Count-1 do if not Sources[i].Mute then
      MuteSource(Sources[i], True);
	Result:=True;
end;

// DoUnMute
//
procedure TGLSoundManager.DoUnMute;
var
   i : Integer;
begin
   for i:=0 to Sources.Count-1 do if not Sources[i].Mute then
      MuteSource(Sources[i], False);
end;

// SetPause
//
procedure TGLSoundManager.SetPause(const val : Boolean);
begin
   if val<>FPause then begin
      if val then begin
         if DoPause then FPause:=True
      end else begin
         DoUnPause;
         FPause:=False;
      end;
   end;
end;

// Loaded
//
procedure TGLSoundManager.Loaded;
begin
   inherited;
   if Active and (not (csDesigning in ComponentState)) then begin
      FActive:=False;
      Active:=True;
   end;
end;

// DefineProperties
//
procedure TGLSoundManager.DefineProperties(Filer: TFiler);
begin
   inherited;
   Filer.DefineProperty('Doppler', ReadDoppler, WriteDoppler, (DopplerFactor<>1));
end;

// WriteDoppler
//
procedure TGLSoundManager.WriteDoppler(writer : TWriter);
begin
   writer.WriteFloat(DopplerFactor);
end;

// ReadDoppler
//
procedure TGLSoundManager.ReadDoppler(reader : TReader);
begin
   FDopplerFactor:=reader.ReadFloat;
end;

// DoPause
//
function TGLSoundManager.DoPause : Boolean;
var
   i : Integer;
begin
   for i:=0 to Sources.Count-1 do if not Sources[i].Pause then
      PauseSource(Sources[i], True);
	Result:=True;
end;

// DoUnPause
//
procedure TGLSoundManager.DoUnPause;
var
   i : Integer;
begin
   for i:=0 to Sources.Count-1 do if not Sources[i].Pause then
      PauseSource(Sources[i], False);
end;

// SetMasterVolume
//
procedure TGLSoundManager.SetMasterVolume(const val : Single);
begin
   if val<0 then
      FMasterVolume:=0
   else if val>1 then
      FMasterVolume:=1
   else FMasterVolume:=val;
   NotifyMasterVolumeChange;
end;

// SetMaxChannels
//
procedure TGLSoundManager.SetMaxChannels(const val : Integer);
begin
   if val<>FMaxChannels then begin
      if val<1 then
         FMaxChannels:=1
      else FMaxChannels:=val;
   end;
end;

// SetOutputFrequency
//
procedure TGLSoundManager.SetOutputFrequency(const val : Integer);
begin
   if val<>FOutputFrequency then begin
      if val<11025 then
         FOutputFrequency:=11025
      else FOutputFrequency:=val;
   end;
end;

// SetUpdateFrequency
//
procedure TGLSoundManager.SetUpdateFrequency(const val : Single);
begin
   FUpdateFrequency:=ClampValue(val, 1, 60);
end;

// StoreUpdateFrequency
//
function TGLSoundManager.StoreUpdateFrequency : Boolean;
begin
   Result:=(FUpdateFrequency<>10);
end;

// SetCadencer
//
procedure TGLSoundManager.SetCadencer(const val : TGLCadencer);
begin
   if val<>FCadencer then begin
      if Assigned(FCadencer) then
         FCadencer.UnSubscribe(Self);
      FCadencer:=val;
      if Assigned(FCadencer) then
         FCadencer.Subscribe(Self);
   end;
end;

// SetDistanceFactor
//
procedure TGLSoundManager.SetDistanceFactor(const val : Single);
begin
   if val<=0 then
      FDistanceFactor:=1
   else FDistanceFactor:=val;
   Notify3DFactorsChanged;
end;

// StoreDistanceFactor
//
function TGLSoundManager.StoreDistanceFactor : Boolean;
begin
   Result:=(FDistanceFactor<>1);
end;

// SetRollOffFactor
//
procedure TGLSoundManager.SetRollOffFactor(const val : Single);
begin
   if val<=0 then
      FRollOffFactor:=1
   else FRollOffFactor:=val;
   Notify3DFactorsChanged;
end;

// StoreRollOffFactor
//
function TGLSoundManager.StoreRollOffFactor : Boolean;
begin
   Result:=(FRollOffFactor<>1);
end;

// SetDopplerFactor
//
procedure TGLSoundManager.SetDopplerFactor(const val : Single);
begin
   if val<0 then
      FDopplerFactor:=0
   else if val>10 then
      FDopplerFactor:=10
   else FDopplerFactor:=val;
   Notify3DFactorsChanged;
end;

// SetSoundEnvironment
//
procedure TGLSoundManager.SetSoundEnvironment(const val : TGLSoundEnvironment);
begin
   if val<>FSoundEnvironment then begin
      FSoundEnvironment:=val;
      NotifyEnvironmentChanged;
   end;
end;

// ListenerCoordinates
//
procedure TGLSoundManager.ListenerCoordinates(var position, velocity, direction, up : TVector);
var
   right : TVector;
begin
   if Listener<>nil then begin
      position:=Listener.AbsolutePosition;
      if FLastDeltaTime<>0 then begin
         velocity:=VectorSubtract(position, FLastListenerPosition);
         ScaleVector(velocity, 1/FLastDeltaTime);
      end;
      FLastListenerPosition:=position;
      if (Listener is TGLCamera) and (TGLCamera(Listener).TargetObject<>nil) then begin
         // special case of the camera targeting something
         direction:=TGLCamera(Listener).AbsoluteVectorToTarget;
         NormalizeVector(direction);
         up:=Listener.AbsoluteYVector;
         right:=VectorCrossProduct(direction, up);
         up:=VectorCrossProduct(right, direction);
      end else begin
         direction:=Listener.AbsoluteZVector;
         up:=Listener.AbsoluteYVector;
      end;
   end else begin
      position:=NullHmgPoint;
      velocity:=NullHmgVector;
      direction:=ZHmgVector;
      up:=YHmgVector;
   end;
end;

// NotifyMasterVolumeChange
//
procedure TGLSoundManager.NotifyMasterVolumeChange;
begin
   // nothing
end;

// Notify3DFactorsChanged
//
procedure TGLSoundManager.Notify3DFactorsChanged;
begin
   // nothing
end;

// NotifyEnvironmentChanged
//
procedure TGLSoundManager.NotifyEnvironmentChanged;
begin
   // nothing
end;

// SetListener
//
procedure TGLSoundManager.SetListener(const val : TGLBaseSceneObject);
begin
   if Assigned(FListener) then
		FListener.RemoveFreeNotification(Self);
   FListener:=val;
   if Assigned(FListener) then
      FListener.FreeNotification(Self);
end;

// SetSources
//
procedure TGLSoundManager.SetSources(const val : TGLSoundSources);
begin
   FSources.Assign(val);
end;

// KillSource
//
procedure TGLSoundManager.KillSource(aSource : TGLBaseSoundSource);
begin
   // nothing
end;

// UpdateSource
//
procedure TGLSoundManager.UpdateSource(aSource : TGLBaseSoundSource);
begin
   aSource.FChanges:=[];
end;

// MuteSource
//
procedure TGLSoundManager.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
begin
   // nothing
end;

// PauseSource
//
procedure TGLSoundManager.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
begin
   // nothing
end;

// UpdateSources
//
procedure TGLSoundManager.UpdateSources;
var
   i : Integer;
begin
   for i:=Sources.Count-1 downto 0 do
      UpdateSource(Sources[i]);
end;

// StopAllSources
//
procedure TGLSoundManager.StopAllSources;
var
   i : Integer;
begin
{$ifdef GLS_DELPHI_5_UP}
	for i:=Sources.Count-1 downto 0 do Sources.Delete(i);
{$else}
	for i:=Sources.Count-1 downto 0 do Sources[i].Free;
{$endif}
end;

// DoProgress
//
procedure TGLSoundManager.DoProgress(const progressTime : TProgressTimes);
begin
   if not Active then Exit; 
   with progressTime do if newTime-FLastUpdateTime>1/FUpdateFrequency then begin
      FLastDeltaTime:=newTime-FLastUpdateTime;
      FLastUpdateTime:=newTime;
      UpdateSources;
   end;
end;

// CPUUsagePercent
//
function TGLSoundManager.CPUUsagePercent : Single;
begin
   Result:=-1;
end;

// EAXSupported
//
function TGLSoundManager.EAXSupported : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TGLBSoundEmitter ------------------
// ------------------

// Create
//
constructor TGLBSoundEmitter.Create(aOwner : TXCollection);
begin
	inherited Create(aOwner);
   FSource:=TGLSoundSource.Create(nil);
end;

// Destroy
//
destructor TGLBSoundEmitter.Destroy;
begin
   if Assigned(FPlayingSource) then FPlayingSource.Free;
   FSource.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLBSoundEmitter.Assign(Source: TPersistent);
begin
   if Source is TGLBSoundEmitter then begin
      FSource.Assign(TGLBSoundEmitter(Source).FSource);
   end;
   inherited Assign(Source);
end;

// WriteToFiler
//
procedure TGLBSoundEmitter.WriteToFiler(writer : TWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0); // Archive Version 0
      FSource.WriteToFiler(writer);
      WriteBoolean(FPlaying);
   end;
end;

// ReadFromFiler
//
procedure TGLBSoundEmitter.ReadFromFiler(reader : TReader);
begin
   inherited;
   with reader do begin
      ReadInteger; // ignore archiveVersion
      FSource.ReadFromFiler(reader);
      FPlaying:=ReadBoolean;
   end;
end;

// Loaded
//
procedure TGLBSoundEmitter.Loaded;
begin
   inherited;
   if not (csDesigning in OwnerBaseSceneObject.ComponentState) then
      SetPlaying(FPlaying);
end;

// FriendlyName
//
class function TGLBSoundEmitter.FriendlyName : String;
begin
	Result:='Sound Emitter';
end;

// FriendlyDescription
//
class function TGLBSoundEmitter.FriendlyDescription : String;
begin
	Result:='A simple sound emitter behaviour';
end;

// UniqueBehaviour
//
class function TGLBSoundEmitter.UniqueItem : Boolean;
begin
	Result:=False;
end;

// DoProgress
//
procedure TGLBSoundEmitter.DoProgress(const progressTime : TProgressTimes);
begin
   // nothing, yet
end;

// SetSource
//
procedure TGLBSoundEmitter.SetSource(const val : TGLBaseSoundSource);
begin
   FSource.Assign(val);
end;

// SetPlaying
//
procedure TGLBSoundEmitter.SetPlaying(const val : Boolean);
begin
   if csDesigning in OwnerBaseSceneObject.ComponentState then
      FPlaying:=val
   else if ActiveSoundManager<>nil then begin
      if val<>Playing then begin
         if val then begin
            FPlayingSource:=ActiveSoundManager.Sources.Add;
            FPlayingSource.FBehaviourToNotify:=Self;
            FPlayingSource.Assign(FSource);
            FPlayingSource.Origin:=OwnerBaseSceneObject;
         end else FPlayingSource.Free;
      end;
   end else if vVerboseGLSMErrors then
      InformationDlg('No Active Sound Manager.'#13#10'Make sure manager is created before emitter');
end;

// GetPlaying
//
function TGLBSoundEmitter.GetPlaying : Boolean;
begin
   if csDesigning in OwnerBaseSceneObject.ComponentState then
      Result:=FPlaying
   else Result:=Assigned(FPlayingSource);
end;

// NotifySourceDestruction
//
procedure TGLBSoundEmitter.NotifySourceDestruction(aSource : TGLSoundSource);
begin
   Assert(FPlayingSource=aSource);
   FPlayingSource:=nil;
end;

// ------------------
// ------------------ TGLSMWaveOut ------------------
// ------------------

// Create
//
constructor TGLSMWaveOut.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   MaxChannels:=4;
end;

// Destroy
//
destructor TGLSMWaveOut.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMWaveOut.DoActivate : Boolean;
begin
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMWaveOut.DoDeActivate;
var
   i : Integer;
begin
   for i:=Sources.Count-1 downto 0 do
      KillSource(Sources[i]);
end;

// KillSource
//
procedure TGLSMWaveOut.KillSource(aSource : TGLBaseSoundSource);
begin
   if aSource.ManagerTag<>0 then begin
      waveOutClose(aSource.ManagerTag);
      aSource.ManagerTag:=0;
   end;
end;

procedure _waveOutCallBack(hwo : HWAVEOUT; uMsg : Cardinal;
                           dwInstance, dwParam1, dwParam2 : Integer); stdcall;
begin
   if uMsg=WOM_DONE then begin
      waveOutClose(hwo);
      TGLSoundSource(dwInstance).ManagerTag:=-1;
   end;
end;

// UpdateSource
//
procedure TGLSMWaveOut.UpdateSource(aSource : TGLBaseSoundSource);
var
   i, n : Integer;
   wfx : TWaveFormatEx;
   smp : TGLSoundSample;
   wh : wavehdr;
   mmres : MMRESULT;
   hwo : hwaveout;
begin
   // count nb of playing sources and delete done ones
   n:=0;
   for i:=Sources.Count-1 downto 0 do
      if Sources[i].ManagerTag>0 then
         Inc(n)
      else if Sources[i].ManagerTag=-1 then
{$ifdef GLS_DELPHI_5_UP}
			Sources.Delete(i);
{$else}
			Sources[i].Free;
{$endif}
	// start sources if some capacity remains, and forget the others
   for i:=Sources.Count-1 downto 0 do if Sources[i].ManagerTag=0 then begin
      if n<FMaxChannels then begin
         smp:=Sources[i].Sample;
         if Assigned(smp) and (smp.Data<>nil) then begin
            wfx:=smp.Data.Sampling.WaveFormat;
            mmres:=waveOutOpen(@hwo, WAVE_MAPPER, @wfx,
                               Cardinal(@_waveOutCallBack), Integer(Sources[i]),
                               CALLBACK_FUNCTION);
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            wh.dwBufferLength:=smp.LengthInBytes;
            wh.lpData:=smp.Data.PCMData;
            wh.dwLoops:=Sources[i].NbLoops;
            if wh.dwLoops>1 then
               wh.dwFlags:=WHDR_BEGINLOOP+WHDR_ENDLOOP
            else wh.dwFlags:=0;
            wh.lpNext:=nil;
            mmres:=waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            mmres:=waveOutWrite(hwo, @wh, SizeOf(wavehdr));
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            Sources[i].ManagerTag:=hwo;
            Inc(n);
			end else
{$ifdef GLS_DELPHI_5_UP}
				Sources.Delete(i);
{$else}
				Sources[i].Free;
{$endif}
		end else
{$ifdef GLS_DELPHI_5_UP}
			Sources.Delete(i);
{$else}
			Sources[i].Free;
{$endif}
	end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
	RegisterXCollectionItemClass(TGLBSoundEmitter);
   vSoundLibraries:=TList.Create;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   if Assigned(vActiveSoundManager) then
      vActiveSoundManager.Active:=False;

   vSoundLibraries.Free; vSoundLibraries:=nil;

end.

