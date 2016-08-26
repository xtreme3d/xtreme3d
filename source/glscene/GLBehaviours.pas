{: GLBehaviours<p>

	Standard TGLBehaviour subclasses for GLScene<p>

	<b>History : </b><font size=-1><ul>
      <li>24/09/02 - Egg - Support for negative rotation speeds (Marco Chong)
      <li>02/10/00 - Egg - Fixed TGLBInertia.DoProgress (DamplingEnabled bug) 
      <li>09/10/00 - Egg - Fixed ApplyTranslationAcceleration & ApplyForce
      <li>11/08/00 - Egg - Fixed translation bug with root level objects & Inertia
      <li>10/04/00 - Egg - Improved persistence logic
		<li>06/04/00 - Egg - Added Damping stuff to inertia
		<li>05/04/00 - Egg - Creation
	</ul></font>
}
unit GLBehaviours;

interface

uses Classes, GLScene, VectorGeometry, GLMisc, XCollection;

type

	// TGLDamping
	//
	{: Holds parameters for TGLScene basic damping model.<p>
		Damping is modeled by calculating a force from the speed, this force
		can then be transformed to an acceleration is you know the object's mass.<br>
		Formulas :<ul>
		<li>damping = constant + linear * Speed + quadratic * Speed^2
		<li>accel = damping / Mass
		</ul> That's just basic physics :). A note on the components :<ul>
		<li>constant : use it for solid friction (will stop abruptly an object after
			decreasing its speed.
		<li>linear : linear friction damping.
		<li>quadratic : expresses viscosity.
		</ul>	}
	TGLDamping = class (TGLUpdateAbleObject)
	   private
	      { Private Declarations }
         FConstant : Single;
         FLinear : Single;
         FQuadratic : Single;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create(aOwner: TPersistent); override;
         destructor Destroy; override;

         procedure WriteToFiler(writer : TWriter);
			procedure ReadFromFiler(reader : TReader);

			procedure Assign(source : TPersistent); override;
			{: Calculates attenuated speed over deltaTime.<p>
            Integration step is 0.01 sec, and the following formula is applied
            at each step: constant+linear*speed+quadratic*speed^2 }
			function Calculate(speed, deltaTime : Double) : Double;
			//: Returns a "[constant; linear; quadractic]" string
			function AsString(const damping : TGLDamping) : String;
			{: Sets all damping parameters in a single call. }
			procedure SetDamping(const constant : Single = 0;
										const linear : Single = 0;
										const quadratic : Single = 0);

		published
	      { Published Declarations }
         property Constant : Single read FConstant write FConstant;
         property Linear : Single read FLinear write FLinear;
         property Quadratic : Single read FQuadratic write FQuadratic;
	end;

	// TGLBInertia
	//
	{: Simple translation and rotation Inertia behaviour.<p>
		Stores translation and rotation speeds, to which you can apply
		accelerations.<p>
		Note that the rotation model is not physical, so feel free to contribute
		a "realworld" inertia class with realistic, axis-free, rotation inertia
		if this approximation does not suits your needs :). }
	TGLBInertia = class (TGLBehaviour)
		private
			{ Private Declarations }
			FMass : Single;
         FTranslationSpeed : TGLCoordinates;
			FTurnSpeed,	FRollSpeed,	FPitchSpeed : Single;
			FTranslationDamping, FRotationDamping : TGLDamping;
			FDampingEnabled : Boolean;

		protected
			{ Protected Declarations }
         procedure SetTranslationSpeed(const val : TGLCoordinates);
         procedure SetTranslationDamping(const val : TGLDamping);
			procedure SetRotationDamping(const val : TGLDamping);

			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;

		public
			{ Public Declarations }
			constructor Create(aOwner : TXCollection); override;
			destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;
			class function UniqueItem : Boolean; override;

			procedure DoProgress(const progressTime : TProgressTimes); override;

			{: Adds time-proportionned acceleration to the speed. }
			procedure ApplyTranslationAcceleration(const deltaTime : Double; const accel : TVector);
			{: Applies a timed force to the inertia.<p>
				If Mass is null, nothing is done. }
			procedure ApplyForce(const deltaTime : Double; const force : TVector);
			{: Applies a timed torque to the inertia (yuck!).<p>
				This gets a "yuck!" because it is as false as the rest of the
				rotation	model. }
			procedure ApplyTorque(const deltaTime : Double; const turnTorque, rollTorque, pitchTorque : Single);
			{: Inverts the translation vector.<p> }
			procedure MirrorTranslation;
         {: Bounce speed as if hitting a surface.<p>
            restitution is the coefficient of restituted energy (1=no energy loss,
            0=no bounce). The normal is NOT assumed to be normalized. }
         procedure SurfaceBounce(const surfaceNormal : TVector; restitution : Single);

		published
			{ Published Declarations }
			property Mass : Single read FMass write FMass;
			property TranslationSpeed : TGLCoordinates read FTranslationSpeed write SetTranslationSpeed;
			property TurnSpeed : Single read FTurnSpeed write FTurnSpeed;
			property RollSpeed : Single read FRollSpeed write FRollSpeed;
			property PitchSpeed : Single read FPitchSpeed write FPitchSpeed;

			{: Enable/Disable damping (damping has a high cpu-cycle cost).<p>
				Damping is enabled by default. }
			property DampingEnabled : Boolean read FDampingEnabled write FDampingEnabled;
			{: Damping applied to translation speed.<br>
				Note that it is not "exactly" applied, ie. if damping would stop
				your object after 0.5 time unit, and your progression steps are
				of 1 time unit, there will be an integration error of 0.5 time unit. }
			property TranslationDamping : TGLDamping read FTranslationDamping write SetTranslationDamping;
			{: Damping applied to rotation speed (yuck!).<br>
				Well, this one is not "exact", like TranslationDamping, and neither
				it is "physical" since I'm reusing the mass and... and... well don't
				show this to your science teacher 8).<br>
				Anyway that's easier to use than the realworld formulas, calculated
				faster, and properly used can give a good illusion of reality. }
			property RotationDamping : TGLDamping read FRotationDamping write SetRotationDamping;
	end;

	// TGLBAcceleration
	//
	{: Applies a constant acceleration to a TGLBInertia.<p> }
	TGLBAcceleration = class (TGLBehaviour)
		private
			{ Private Declarations }
         FAcceleration : TGLCoordinates;

		protected
			{ Protected Declarations }
         procedure SetAcceleration(const val : TGLCoordinates);

			procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;

		public
			{ Public Declarations }
			constructor Create(aOwner : TXCollection); override;
			destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;
			class function UniqueItem : Boolean; override;

			procedure DoProgress(const progressTime : TProgressTimes); override;

		published
			{ Published Declarations }
			property Acceleration : TGLCoordinates read FAcceleration write FAcceleration;
	end;

{: Returns or creates the TGLBInertia within the given behaviours.<p>
	This helper function is convenient way to access a TGLBInertia. }
function GetOrCreateInertia(behaviours : TGLBehaviours) : TGLBInertia; overload;
function GetOrCreateInertia(obj : TGLBaseSceneObject) : TGLBInertia; overload;

{: Returns or creates the TGLBAcceleration within the given behaviours.<p>
	This helper function is convenient way to access a TGLBAcceleration. }
function GetOrCreateAcceleration(behaviours : TGLBehaviours) : TGLBAcceleration; overload;
function GetOrCreateAcceleration(obj : TGLBaseSceneObject) : TGLBAcceleration; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x;

// GetOrCreateInertia (TGLBehaviours)
//
function GetOrCreateInertia(behaviours : TGLBehaviours) : TGLBInertia;
var
	i : Integer;
begin
	i:=behaviours.IndexOfClass(TGLBInertia);
	if i>=0 then
		Result:=TGLBInertia(behaviours[i])
	else Result:=TGLBInertia.Create(behaviours);
end;

// GetOrCreateInertia (TGLBaseSceneObject)
//
function GetOrCreateInertia(obj : TGLBaseSceneObject) : TGLBInertia;
begin
	Result:=GetOrCreateInertia(obj.Behaviours);
end;

// GetOrCreateAcceleration (TGLBehaviours)
//
function GetOrCreateAcceleration(behaviours : TGLBehaviours) : TGLBAcceleration;
var
	i : Integer;
begin
	i:=behaviours.IndexOfClass(TGLBAcceleration);
	if i>=0 then
		Result:=TGLBAcceleration(behaviours[i])
	else Result:=TGLBAcceleration.Create(behaviours);
end;

// GetOrCreateAcceleration (TGLBaseSceneObject)
//
function GetOrCreateAcceleration(obj : TGLBaseSceneObject) : TGLBAcceleration;
begin
	Result:=GetOrCreateAcceleration(obj.Behaviours);
end;

// ------------------
// ------------------ TGLDamping ------------------
// ------------------

// Create
//
constructor TGLDamping.Create(aOwner : TPersistent);
begin
	inherited Create(AOwner);
end;

destructor TGLDamping.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TGLDamping.Assign(Source: TPersistent);
begin
   if Source is TGLDamping then begin
      FConstant:=TGLDamping(Source).Constant;
      FLinear:=TGLDamping(Source).Linear;
      FQuadratic:=TGLDamping(Source).Quadratic;
   end else inherited Assign(Source);
end;

// WriteToFiler
//
procedure TGLDamping.WriteToFiler(writer : TWriter);
var
   writeStuff : Boolean;
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      writeStuff:=(FConstant<>0) or (FLinear<>0) or (FQuadratic<>0);
      WriteBoolean(writeStuff);
      if writeStuff then begin
         WriteFloat(FConstant);
         WriteFloat(FLinear);
         WriteFloat(FQuadratic);
      end;
   end;
end;

// ReadFromFiler
//
procedure TGLDamping.ReadFromFiler(reader : TReader);
begin
   with reader do begin
      ReadInteger; // ignore Archive Version
      if ReadBoolean then begin
         FConstant:=ReadFloat;
         FLinear:=ReadFloat;
         FQuadratic:=ReadFloat;
      end else begin
         FConstant:=0;
         FLinear:=0;
         FQuadratic:=0;
      end;
   end;
end;

// Calculate
//
function TGLDamping.Calculate(speed, deltaTime : Double) : Double;
var
   dt : Double;
begin
   while deltaTime>0 do begin
      if deltaTime>0.01 then begin
         dt:=0.01;
         deltaTime:=deltaTime-0.01;
      end else begin
         dt:=deltaTime;
         deltaTime:=0;
      end;
      speed:=speed-dt*((FQuadratic*speed+FLinear)*speed+FConstant);
   end;
   Result:=speed;
end;

// DampingAsString
//
function TGLDamping.AsString(const damping : TGLDamping) : String;
begin
	Result:=Format('[%f; %f; %f]', [Constant, Linear, Quadratic]);
end;

// SetDamping
//
procedure TGLDamping.SetDamping(const constant : Single = 0;
							const linear : Single = 0;	const quadratic : Single = 0);
begin
	FConstant:=constant;
	FLinear:=linear;
	FQuadratic:=quadratic;
end;

// ------------------
// ------------------ TGLBInertia ------------------
// ------------------

// Create
//
constructor TGLBInertia.Create(aOwner : TXCollection);
begin
	inherited Create(aOwner);
	FTranslationSpeed:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
	FMass:=1;
	FDampingEnabled:=True;
	FTranslationDamping:=TGLDamping.Create(Self);
   FRotationDamping:=TGLDamping.Create(Self);
end;

// Destroy
//
destructor TGLBInertia.Destroy;
begin
   FRotationDamping.Free;
   FTranslationDamping.Free;
   FTranslationSpeed.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLBInertia.Assign(Source: TPersistent);
begin
   if Source.ClassType=Self.ClassType then begin
		FMass:=TGLBInertia(Source).Mass;
		FTranslationSpeed.Assign(TGLBInertia(Source).FTranslationSpeed);
		FTurnSpeed:=TGLBInertia(Source).TurnSpeed;
		FRollSpeed:=TGLBInertia(Source).RollSpeed;
		FPitchSpeed:=TGLBInertia(Source).PitchSpeed;
		FDampingEnabled:=TGLBInertia(Source).DampingEnabled;
		FTranslationDamping.Assign(TGLBInertia(Source).TranslationDamping);
		FRotationDamping.Assign(TGLBInertia(Source).RotationDamping);
   end;
   inherited Assign(Source);
end;

// WriteToFiler
//
procedure TGLBInertia.WriteToFiler(writer : TWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteFloat(FMass);
      FTranslationSpeed.WriteToFiler(writer);
      WriteFloat(FTurnSpeed);
      WriteFloat(FRollSpeed);
      WriteFloat(FPitchSpeed);
      WriteBoolean(FDampingEnabled);
      FTranslationDamping.WriteToFiler(writer);
      FRotationDamping.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TGLBInertia.ReadFromFiler(reader : TReader);
begin
   inherited;
   with reader do begin
      ReadInteger; // ignore archiveVersion
      FMass:=ReadFloat;
      FTranslationSpeed.ReadFromFiler(reader);
      FTurnSpeed:=ReadFloat;
      FRollSpeed:=ReadFloat;
      FPitchSpeed:=ReadFloat;
      FDampingEnabled:=ReadBoolean;
      FTranslationDamping.ReadFromFiler(reader);
      FRotationDamping.ReadFromFiler(reader);
   end;
end;

// SetTranslationSpeed
//
procedure TGLBInertia.SetTranslationSpeed(const val : TGLCoordinates);
begin
   FTranslationSpeed.Assign(val);
end;

// SetTranslationDamping
//
procedure TGLBInertia.SetTranslationDamping(const val : TGLDamping);
begin
   FTranslationDamping.Assign(val);
end;

// SetRotationDamping
//
procedure TGLBInertia.SetRotationDamping(const val : TGLDamping);
begin
   FRotationDamping.Assign(val);
end;

// FriendlyName
//
class function TGLBInertia.FriendlyName : String;
begin
	Result:='Simple Inertia';
end;

// FriendlyDescription
//
class function TGLBInertia.FriendlyDescription : String;
begin
	Result:='A simple translation and rotation inertia'; 
end;

// UniqueBehaviour
//
class function TGLBInertia.UniqueItem : Boolean;
begin
	Result:=True;
end;

// DoProgress
//
procedure TGLBInertia.DoProgress(const progressTime : TProgressTimes);
var
	trnVector : TVector;
	speed, newSpeed : Double;

	procedure ApplyRotationDamping(var rotationSpeed : Single);
	begin
      if rotationSpeed>0 then begin
   		rotationSpeed:=RotationDamping.Calculate(rotationSpeed, progressTime.deltaTime);
	   	if rotationSpeed<=0 then
		   	rotationSpeed:=0;
      end else begin
   		rotationSpeed:=-RotationDamping.Calculate(-rotationSpeed, progressTime.deltaTime);
	   	if rotationSpeed>=0 then
		   	rotationSpeed:=0;
      end;
	end;

begin
	// Apply damping to speed
	if DampingEnabled then begin
		// Translation damping
		speed:=TranslationSpeed.VectorLength;
      if speed>0 then begin
   		newSpeed:=TranslationDamping.Calculate(speed, progressTime.deltaTime);
	   	if newSpeed<=0 then begin
            trnVector:=NullHmgVector;
			   TranslationSpeed.AsVector:=trnVector;
   		end else begin
            TranslationSpeed.Scale(newSpeed/Speed);
            SetVector(trnVector, TranslationSpeed.AsVector);
         end;
      end else SetVector(trnVector, NullHmgVector);
		// Rotation damping (yuck!)
		ApplyRotationDamping(FTurnSpeed);
		ApplyRotationDamping(FRollSpeed);
		ApplyRotationDamping(FPitchSpeed);
	end else SetVector(trnVector, TranslationSpeed.AsVector);
	// Apply speed to object
	with OwnerBaseSceneObject do with progressTime do begin
		Position.AddScaledVector(deltaTime, trnVector);
		TurnAngle:=TurnAngle+TurnSpeed*deltaTime;
		RollAngle:=RollAngle+RollSpeed*deltaTime;
		PitchAngle:=PitchAngle+PitchSpeed*deltaTime;
	end;
end;

// ApplyTranslationAcceleration
//
procedure TGLBInertia.ApplyTranslationAcceleration(const deltaTime : Double; const accel : TVector);
begin
	FTranslationSpeed.AsVector:=VectorCombine(FTranslationSpeed.AsVector, accel, 1, deltaTime);
end;

// ApplyForce
//
procedure TGLBInertia.ApplyForce(const deltaTime : Double; const force : TVector);
begin
	if Mass<>0 then
		FTranslationSpeed.AsVector:=VectorCombine(FTranslationSpeed.AsVector, force, 1, deltaTime/Mass);
end;

// ApplyTorque
//
procedure TGLBInertia.ApplyTorque(const deltaTime : Double;
											 const turnTorque, rollTorque, pitchTorque : Single);
var
	factor : Double;
begin
	if Mass<>0 then begin
		factor:=deltaTime/Mass;
		FTurnSpeed:=FTurnSpeed+turnTorque*factor;
		FRollSpeed:=FRollSpeed+rollTorque*factor;
		FPitchSpeed:=FPitchSpeed+pitchTorque*factor;
	end;
end;

// MirrorTranslation
//
procedure TGLBInertia.MirrorTranslation;
begin
	FTranslationSpeed.Invert;
end;

// SurfaceBounce
//
procedure TGLBInertia.SurfaceBounce(const surfaceNormal : TVector; restitution : Single);
var
   f : Single;
begin
   // does the current speed vector comply?
   f:=VectorDotProduct(FTranslationSpeed.AsVector, surfaceNormal);
   if f<0 then begin
      // remove the non-complying part of the speed vector
      FTranslationSpeed.AddScaledVector(-f/VectorNorm(surfaceNormal)*(1+restitution), surfaceNormal);
   end;
end;

// ------------------
// ------------------ TGLBAcceleration ------------------
// ------------------

// Create
//
constructor TGLBAcceleration.Create(aOwner : TXCollection);
begin
   inherited;
   FAcceleration:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

// Destroy
//
destructor TGLBAcceleration.Destroy;
begin
   inherited;
   FAcceleration.Free;
end;

// Assign
//
procedure TGLBAcceleration.Assign(Source: TPersistent);
begin
   if Source.ClassType=Self.ClassType then begin
		FAcceleration.Assign(TGLBAcceleration(Source).FAcceleration);
   end;
   inherited Assign(Source);
end;

// WriteToFiler
//
procedure TGLBAcceleration.WriteToFiler(writer : TWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0); // Archive Version 0
      FAcceleration.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TGLBAcceleration.ReadFromFiler(reader : TReader);
begin
   inherited;
   with reader do begin
      ReadInteger; // ignore archiveVersion
      FAcceleration.ReadFromFiler(reader);
   end;
end;

// SetAcceleration
//
procedure TGLBAcceleration.SetAcceleration(const val : TGLCoordinates);
begin
   FAcceleration.Assign(val);
end;

// FriendlyName
//
class function TGLBAcceleration.FriendlyName : String;
begin
	Result:='Simple Acceleration';
end;

// FriendlyDescription
//
class function TGLBAcceleration.FriendlyDescription : String;
begin
	Result:='A simple and constant acceleration'; 
end;

// UniqueBehaviour
//
class function TGLBAcceleration.UniqueItem : Boolean;
begin
	Result:=False;
end;

// DoProgress
//
procedure TGLBAcceleration.DoProgress(const progressTime : TProgressTimes);
begin
   GetOrCreateInertia(OwnerBaseSceneObject).ApplyTranslationAcceleration(progressTime.deltaTime, FAcceleration.DirectVector);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
	RegisterXCollectionItemClass(TGLBInertia);
	RegisterXCollectionItemClass(TGLBAcceleration);

end.

