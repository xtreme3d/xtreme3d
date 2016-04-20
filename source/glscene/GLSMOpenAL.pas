{: GLSMOpenAL<p>

	OpenAL based sound-manager (http://www.openal.org).<p>
   OpenAL drivers can be download from the OpenAL site or you soundcard
   manufacturer's website.<p>

   OpenAL headers: you can use the JEDI headers (http://www.delphi-jedi.org/),
   but until dynamic binding headers are created, use of GLSMOpenAL is *not*
   recommended as a component in the IDE (ie. don't add it to your DPKs or DFMs,
   create it manually at runtime instead).<p>

   Unsupported feature(s) :<ul>
      <li>Accepts only simple *uncompressed* WAV files (8/16 bits, mono/stereo)
      <li>Dynamic loading/unloading
      <li>Global 3D parameters
      <li>Master Volume
      <li>Environments
      <li>CPUUsagePercent
      <li>???
   </ul><p>

	<b>History : </b><font size=-1><ul>
      <li>??/??/03 - Mrqzz - Creation
	</ul></font>
}
unit GLSMOpenAL;

interface

uses
   Classes, GLSound, GLScene, SysUtils, GLSoundFileObjects;

type

	// TGLSMOpenAL
	//
	TGLSMOpenAL = class (TGLSoundManager)
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

         function GetALFormat(sampling : TGLSoundSampling) : Integer;
         
      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure UpdateSources; override;

         function EAXSupported : Boolean; override;
	end;

   EOpenALError = Exception;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses Forms, VectorGeometry, Dialogs, al, alut, alTypes;

procedure Register;
begin
// No registering by default (headers aren't dynamic)
//   RegisterComponents('GLScene', [TGLSMOpenAL]);
end;

//checks for an error and raises an exception if necessary
procedure CheckOpenALError;
var
   error : integer;
begin
   error:=alGetError;
   if error<>AL_NO_ERROR then
      raise EOpenALError.Create('OpenAL Error #' + intToStr(error));
end;

//clears the error-states
procedure ClearOpenALError;
begin
   alGetError;
end;

// ------------------
// ------------------ TGLSMOpenAL ------------------
// ------------------

// Create
//
constructor TGLSMOpenAL.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
end;

// Destroy
//
destructor TGLSMOpenAL.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMOpenAL.DoActivate : Boolean;
var
dummy: array of PChar;
begin
     dummy:= nil;
     alutInit(nil, dummy);
     CheckOpenALError;
     alDistanceModel(AL_INVERSE_DISTANCE);
     CheckOpenALError;
     Result:=True;
end;

// DoDeActivate
//
procedure TGLSMOpenAL.DoDeActivate;
begin
     alutExit;
end;

// NotifyMasterVolumeChange
//
procedure TGLSMOpenAL.NotifyMasterVolumeChange;
begin
end;

// Notify3DFactorsChanged
//
procedure TGLSMOpenAL.Notify3DFactorsChanged;
begin
end;

// NotifyEnvironmentChanged
//
procedure TGLSMOpenAL.NotifyEnvironmentChanged;
begin
end;

// KillSource
//
procedure TGLSMOpenAL.KillSource(aSource : TGLBaseSoundSource);
begin
end;

// UpdateSource
//
procedure TGLSMOpenAL.UpdateSource(aSource : TGLBaseSoundSource);
begin
     ClearOpenALError;
     if aSource.ManagerTag = 0 then begin
          alGenSources(1, PALuint(@aSource.managerTag));
          CheckOpenALError;
     end;

     //if sscTransformation in aSource.Changes then begin
          alSourcefv(aSource.ManagerTag, AL_POSITION, PALFloat(aSource.Origin.Position.asAddress));
          CheckOpenALError;
          alSourcefv(aSource.ManagerTag, AL_DIRECTION, PALFloat(aSource.Origin.Direction.asAddress));
          CheckOpenALError;
     //end;

     if aSource.SoundName <> '' then begin
          if aSource.Sample.ManagerTag = 0 then begin
               alGenBuffers(1, PALuint(@aSource.sample.ManagerTag));
               CheckOpenALError;
          end;

          if (sscSample in aSource.Changes) and assigned(aSource.Sample.Data) then begin
               alBufferData(aSource.sample.ManagerTag,
                            GetALFormat(aSource.sample.Sampling),
                            aSource.sample.Data.PCMData,
                            aSource.sample.data.LengthInBytes,
                            aSource.Sample.Data.Sampling.Frequency);
               CheckOpenALError;
               alSourcei(aSource.ManagerTag, AL_BUFFER, aSource.sample.ManagerTag);
               CheckOpenALError;
          end;
     end;

     if sscStatus in aSource.changes then begin
          alSourcef(aSource.ManagerTag,AL_PITCH,1.0);
          CheckOpenALError;
          alSourcei(aSource.managerTag,AL_LOOPING, AL_TRUE);
          CheckOpenALError;
          //alSourcef(aSource.managerTag, AL_MAX_DISTANCE, aSource.MaxDistance);
          //CheckOpenALError;
          alSourcef(aSource.managerTag, AL_ROLLOFF_FACTOR, 1.0);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_REFERENCE_DISTANCE, aSource.MinDistance);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_CONE_INNER_ANGLE, aSource.InsideConeAngle);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_CONE_OUTER_ANGLE, aSource.OutsideConeAngle);
          CheckOpenALError;
          alSourcef(aSource.ManagerTag, AL_CONE_OUTER_GAIN, aSource.ConeOutsideVolume);
     end;

     inherited UpdateSource(aSource);
end;

// MuteSource
//
procedure TGLSMOpenAL.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
begin
     if muted then alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 0.0)
     else alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 1.0);
end;

// PauseSource
//
procedure TGLSMOpenAL.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
begin
     if not paused then begin
          alSourceRewind(aSource.managerTag);
          alSourcePlay(aSource.ManagerTag);
     end else
          alSourcePause(aSource.ManagerTag);
end;

// UpdateSources
//
procedure TGLSMOpenAL.UpdateSources;
var
     pos, dir, up, vel: TVector;
     DirUp: array[0..5] of TALfloat; //orientation
begin
     ListenerCoordinates(pos, vel, dir, up);
     alListenerfv(AL_POSITION, PALfloat(@pos));
     alListenerfv(AL_VELOCITY, PALfloat(@vel));

     dirUp[0]:= dir[0];
     dirUp[1]:= dir[1];
     dirUp[2]:= dir[2];
     dirUp[3]:= up[0];
     dirUp[4]:= up[1];
     dirUp[5]:= up[2];
     alListenerfv(AL_ORIENTATION, PALfloat(@dirUp));

     inherited;
end;

// EAXSupported
//
function TGLSMOpenAL.EAXSupported : Boolean;
begin
     result:= alIsExtensionPresent(PChar('EAX2.0'));
end;

// GetDefaultFrequency
//
function TGLSMOpenAL.GetDefaultFrequency(aSource : TGLBaseSoundSource): integer;
begin
      Result:=-1;
end;

// GetALFormat
//
function TGLSMOpenAL.GetALFormat(sampling: TGLSoundSampling): integer;
begin
     result:= 0;
     
     //mono
     if sampling.NbChannels = 1 then case sampling.BitsPerSample of
          8: result:= AL_FORMAT_MONO8;
          16: result:= AL_FORMAT_MONO16;
     end else case sampling.BitsPerSample of //stereo
          8: result:= AL_FORMAT_STEREO8;
          16: result:= AL_FORMAT_STEREO16;
     end;
end;

end.

