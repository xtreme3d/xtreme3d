//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLParticleFX<p>

   Base classes for scene-wide blended particles FX.<p>

   These provide a mechanism to render heterogenous particles systems with per
   particle depth-sorting (allowing correct rendering of interwoven separate
   fire and smoke particle systems for instance).<p>

   <b>History : </b><font size=-1><ul>
      <li>08/10/05 - Mathx - Fixed access violation when a PFXManager was removed from
                             form but a particleFX still had a reference to it (added
                             the FUsers property). Butracker ID=783625. 
      <li>17/02/05 - EG - Restored correct PFXSource.Burst relative/absolute behaviour,
                          EffectsScale support not added back (no clue what it does... Mrqz?)
      <li>23/11/04 - SG - Fixed memory leak in TGLLifeColoredPFXManager (kenguru)
      <li>03/10/04 - Mrqzzz - added property TGLParticleFXEffect.DisabledIfOwnerInvisible. Fixed PositionDispersionRange to honour VelocityMode=svmRelative
      <li>25/09/04 - Graham Kennedy - Fixed restore of currentTexturingMode
      <li>09/09/04 - Mrqzzz - added property TGLParticleFXEffect.EffectScale allowing different scaling of effect with same manager. TGLParticleFXEffect.ArchiveVersion updated to 4
      <li>29/08/04 - Mrqzzz - fixed particles initial position when VelocityMode=svmRelative
      <li>28/08/04 - Mrqzzz - fixed particles direction when VelocityMode=svmRelative
      <li>09/07/04 - Mrqzzz - small fixup (TGLSourcePFXEffect.WriteToFiler Archive V.4)
      <li>08/07/04 - Eugene Kryukov - Added rotation for particles, RotateAngle in
                                      LifeColor. And added AbsoluteRotation for TGLDynamicPFXManager
      <li>25/04/04 - EG - Added friction, Life sizes, multiple sprites per texture
                          and sprites sharing
      <li>24/04/04 - Mrqzzz - Added property "enabled" to TGLSourcePFXEffect
      <li>15/04/04 - EG - AspectRatio and Rotation added to sprite PFX,
                          improved texturing mode switches
      <li>26/05/03 - EG - Improved TGLParticleFXRenderer.BuildList
      <li>05/11/02 - EG - Enable per-manager blending mode control
      <li>27/01/02 - EG - Added TGLLifeColoredPFXManager, TGLBaseSpritePFXManager
                          and TGLPointLightPFXManager.
      <li>23/01/02 - EG - Added ZWrite and BlendingMode to the PFX renderer,
                          minor sort and render optims
      <li>22/01/02 - EG - Another RenderParticle color lerp fix (GliGli)
      <li>20/01/02 - EG - Several optimization (35% faster on Volcano bench)
      <li>18/01/02 - EG - RenderParticle color lerp fix (GliGli)
      <li>08/09/01 - EG - Creation (GLParticleFX.omm)
   </ul></font>
}
unit GLParticleFX;

interface

uses Classes, PersistentClasses, GLScene, VectorGeometry, XCollection, GLTexture,
     GLCadencer, GLMisc, VectorLists, GLGraphics, GLContext;

const
   cPFXNbRegions = 128;     // number of distance regions
   cPFXGranularity = 128;   // granularity of particles per region

type

   TGLParticleList = class;
   TGLParticleFXManager = class;
   TGLParticleFXEffect = class;

   // TGLParticle
   //
   {: Base class for particles.<p>
      The class implements properties for position, velocity and time, whatever
      you need in excess of that will have to be placed in subclasses (this
      class should remain as compact as possible). }
   TGLParticle = class (TPersistentObject)
      private
         { Private Declarations }
         FID, FTag : Integer;
         FManager : TGLParticleFXManager; // NOT persistent
         FPosition : TAffineVector;
         FVelocity : TAffineVector;
         FRotation : Single;
         FCreationTime : Double;
         FEffectScale : Single;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         property Manager : TGLParticleFXManager read FManager write FManager;
         
         {: Particle's ID, given at birth.<p>
            ID is a value unique per manager. }
         property ID : Integer read FID;
         {: Particle's absolute position.<p>
            Note that this property is read-accessed directly at rendering time
            in the innards of the depth-sorting code. }
         property Position : TAffineVector read FPosition write FPosition;
         {: Particle's velocity.<p>
            This velocity is indicative and is NOT automatically applied
            to the position during progression events by this class (subclasses
            may implement that). }
         property Velocity : TAffineVector read FVelocity write FVelocity;
         {: Time at which particle was created }
         property CreationTime : Double read FCreationTime write FCreationTime;

         property PosX : Single read FPosition[0] write FPosition[0];
         property PosY : Single read FPosition[1] write FPosition[1];
         property PosZ : Single read FPosition[2] write FPosition[2];
         property VelX : Single read FVelocity[0] write FVelocity[0];
         property VelY : Single read FVelocity[1] write FVelocity[1];
         property VelZ : Single read FVelocity[2] write FVelocity[2];
         property Tag : Integer read FTag write FTag;
   end;

   TGLParticleClass = class of TGLParticle;
   TGLParticleArray = array [0..MaxInt shr 4] of TGLParticle;
   PGLParticleArray = ^TGLParticleArray;

   // TGLParticleList
   //
   {: List of particles.<p>
      This list is managed with particles and performance in mind, make sure to
      check methods doc. }
   TGLParticleList = class (TPersistentObject)
      private
         { Private Declarations }
         FOwner : TGLParticleFXManager;  // NOT persistent
         FItemList : TPersistentObjectList;
         FDirectList : PGLParticleArray; // NOT persistent

      protected
         { Protected Declarations }
         function GetItems(index : Integer) : TGLParticle;
         procedure SetItems(index : Integer; val : TGLParticle);
         procedure AfterItemCreated(Sender : TObject);

      public
         { Public Declarations }
         constructor Create; override;
         destructor Destroy; override;
         procedure WriteToFiler(writer : TVirtualWriter); override;
         procedure ReadFromFiler(reader : TVirtualReader); override;

         {: Refers owner manager }
         property Owner : TGLParticleFXManager read FOwner write FOwner;
         property Items[index : Integer] : TGLParticle read GetItems write SetItems; default;

         function ItemCount : Integer;
         {: Adds a particle to the list.<p>
            Particle owneship is defined blindly, if the particle was previously
            in another list, it won't be automatically removed from that list. }
         function AddItem(aItem : TGLParticle) : Integer;
         {: Removes and frees a particular item for the list.<p>
            If the item is not part of the list, nothing is done.<br>
            If found in the list, the item's "slot" is set to nil and item is
            freed (after setting its ownership to nil). The nils can be removed
            with a call to Pack. }
         procedure RemoveAndFreeItem(aItem : TGLParticle);
         function IndexOfItem(aItem : TGLParticle) : Integer;
         {: Packs the list by removing all "nil" items.<p>
            Note: this functions is orders of magnitude faster than the TList
            version. }
         procedure Pack;

         property List : PGLParticleArray read FDirectList;
   end;

   TGLParticleFXRenderer = class;
   TPFXCreateParticleEvent = procedure (Sender : TObject; aParticle : TGLParticle) of object;

   // TGLParticleFXManager
   //
   {: Base class for particle FX managers.<p>
      Managers take care of life and death of particles for a particular
      particles FX system. You can have multiple scene-wide particle
      FX managers in a scene, handled by the same ParticleFxRenderer.<p>
      Before subclassing, make sure you understood how the Initialize/Finalize
      Rendering, Begin/End Particles and RenderParticles methods (and also
      understood that rendering of manager's particles may be interwoven). }
   TGLParticleFXManager = class (TGLCadencedComponent)
      private
         { Private Declarations }
			FBlendingMode : TBlendingMode;
         FRenderer : TGLParticleFXRenderer;
         FParticles : TGLParticleList;
         FNextID : Integer;
         FOnCreateParticle : TPFXCreateParticleEvent;
         FAutoFreeWhenEmpty : Boolean;

         FUsers: TList; //list of objects that use this manager

      protected
         { Protected Declarations }
         procedure SetRenderer(const val : TGLParticleFXRenderer);
         procedure SetParticles(const aParticles : TGLParticleList);

         {: Texturing mode for the particles.<p>
            Subclasses should return GL_TEXTURE_1D, 2D or 3D depending on their
            needs, and zero if they don't use texturing. This method is used
            to reduce the number of texturing activations/deactivations. }
         function TexturingMode : Cardinal; virtual; abstract;

         {: Invoked when the particles of the manager will be rendered.<p>
            This method is fired with the "base" OpenGL states and matrices
            that will be used throughout the whole rendering, per-frame
            initialization should take place here.<br>
            OpenGL states/matrices should not be altered in any way here. }
         procedure InitializeRendering; dynamic; abstract;
         {: Triggered just before rendering a set of particles.<p>
            The current OpenGL state should be assumed to be the "base" one as
            was found during InitializeRendering. Manager-specific states should
            be established here.<br>
            Multiple BeginParticles can occur during a render (but all will be
            between InitializeRendering and Finalizerendering, and at least one
            particle will be rendered before EndParticles is invoked). }
         procedure BeginParticles; virtual; abstract;
         {: Request to render a particular particle.<p>
            Due to the nature of the rendering, no particular order should be
            assumed. If possible, no OpenGL state changes should be made in this
            method, but should be placed in Begin/EndParticles. }
         procedure RenderParticle(aParticle : TGLParticle); virtual; abstract;
         {: Triggered after a set of particles as been rendered.<p>
            If OpenGL state were altered directly (ie. not through the states
            caches of GLMisc), it should be restored back to the "base" state. }
         procedure EndParticles; virtual; abstract;
         {: Invoked when rendering of particles for this manager is done. }
         procedure FinalizeRendering; dynamic; abstract;

         {: ID for the next created particle. }
         property NextID : Integer read FNextID write FNextID;

         {: Blending mode for the particles.<p>
            Protected and unused in the base class. }
			property BlendingMode : TBlendingMode read FBlendingMode write FBlendingMode;
         {: Apply BlendingMode relatively to the renderer's blending mode. }
         procedure ApplyBlendingMode;
         {: Unapply BlendingMode relatively by restoring the renderer's blending mode. }
         procedure UnapplyBlendingMode;

         procedure registerUser(obj: TGLParticleFXEffect);
         procedure unregisterUser(obj: TGLParticleFXEffect);

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

			procedure NotifyChange(Sender : TObject); override;
         procedure DoProgress(const progressTime : TProgressTimes); override;

         //: Class of particles created by this manager. }
         class function ParticlesClass : TGLParticleClass; virtual;
         {: Creates a new particle controled by the manager. }
         function CreateParticle : TGLParticle; virtual;
         {: Create several particles at once. }
         procedure CreateParticles(nbParticles : Integer);

         {: A TGLParticleList property. }
         property Particles : TGLParticleList read FParticles write SetParticles;
         {: Return the number of particles.<p>
            Note that subclasses may decide to return a particle count inferior
            to Particles.ItemCount, and the value returned by this method will
            be the one honoured at render time. }
         function ParticleCount : Integer; virtual;

         {: If True the manager will free itself when its particle count reaches zero.<p>
            Check happens in the progression event, use with caution and only
            if you know what you're doing! }
         property AutoFreeWhenEmpty : Boolean read FAutoFreeWhenEmpty write FAutoFreeWhenEmpty;


		published
			{ Published Declarations }
         {: References the renderer.<p>
            The renderer takes care of ordering the particles of the manager
            (and other managers linked to it) and rendering them all depth-sorted. }
         property Renderer : TGLParticleFXRenderer read FRenderer write SetRenderer;
         {: Event triggered after standard particle creation and initialization. }
         property OnCreateParticle : TPFXCreateParticleEvent read FOnCreateParticle write FOnCreateParticle;

         property Cadencer;
   end;

   // TGLParticleFXEffect
   //
   {: Base class for linking scene objects to a particle FX manager.<p> }
   TGLParticleFXEffect = class (TGLObjectPostEffect)
      private
         { Private Declarations }
         FManager : TGLParticleFXManager;
         FManagerName : String;
         FEffectScale: single;
    procedure SetEffectScale(const Value: single); // NOT persistent, temporarily used for persistence

      protected
         { Protected Declarations }
         procedure SetManager(val : TGLParticleFXManager);

         procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;

         procedure Loaded; override;

         procedure managerNotification(aManager: TGLParticleFXManager; Operation: TOperation);

      public
         { Public Declarations }
         constructor Create(aOwner : TXCollection); override;
         destructor Destroy; override;

		published
			{ Published Declarations }
         {: Reference to the Particle FX manager }
         property Manager : TGLParticleFXManager read FManager write SetManager;
         property EffectScale : single read FEffectScale write SetEffectScale;

   end;

   // PFX region rendering structures

   TParticleReference = packed record
      particle : TGLParticle;
      distance : Integer;  // stores an IEEE single!
   end;
   PParticleReference = ^TParticleReference;
   TParticleReferenceArray = packed array [0..MaxInt shr 4] of TParticleReference;
   PParticleReferenceArray = ^TParticleReferenceArray;
   TPFXRegion = record
      count, capacity : Integer;
      particleRef : PParticleReferenceArray;
      particleOrder : PPointerList;
   end;
   PPFXRegion = ^TPFXRegion;

   // TPFXSortAccuracy
   //
   TPFXSortAccuracy = (saLow, saOneTenth, saOneThird, saOneHalf, saHigh);

   // TGLParticleFXRenderer
   //
   {: Rendering interface for scene-wide particle FX.<p>
      A renderer can take care of rendering any number of particle systems,
      its main task being to depth-sort the particles so that they are blended
      appropriately.<br>
      This object will usually be placed at the end of the scene hierarchy,
      just before the HUD overlays, its position, rotation etc. is of no
      importance and has no effect on the rendering of the particles. }
   TGLParticleFXRenderer = class (TGLBaseSceneObject)
      private
         { Private Declarations }
         FManagerList : TList;
         FLastSortTime : Double;
         FLastParticleCount : Integer;
         FZWrite, FZTest, FZCull : Boolean;
         FZSortAccuracy : TPFXSortAccuracy;
         FZMaxDistance : Single;
			FBlendingMode : TBlendingMode;
         FCurrentRCI : PRenderContextInfo;
         FRegions : array [0..cPFXNbRegions-1] of TPFXRegion;

      protected
         { Protected Declarations }
         function StoreZMaxDistance : Boolean;

         {: Register a manager }
         procedure RegisterManager(aManager : TGLParticleFXManager);
         {: UnRegister a manager }
         procedure UnRegisterManager(aManager : TGLParticleFXManager);

         procedure UnRegisterAll;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;

         {: Pointer to the current rci, valid only during rendering. }
         property CurrentRCI : PRenderContextInfo read FCurrentRCI;
         {: Time (in msec) spent sorting the particles for last render. }
         property LastSortTime : Double read FLastSortTime;
         {: Amount of particles during the last render. }
         property LastParticleCount : Integer read FLastParticleCount;

		published
   		{ Published Declarations }
         {: Specifies if particles should write to ZBuffer.<p>
            If the PFXRenderer is the last object to be rendered in the scene,
            it is not necessary to write to the ZBuffer since the particles
            are depth-sorted. Writing to the ZBuffer has a performance penalty. }
         property ZWrite : Boolean read FZWrite write FZWrite default False;
         {: Specifies if particles should write to test ZBuffer.<p> }
         property ZTest : Boolean read FZTest write FZTest default True;
         {: If true the renderer will cull particles that are behind the camera. }
         property ZCull : Boolean read FZCull write FZCull default True;
         {: If true particles will be accurately sorted back to front.<p>
            When false, only a rough ordering is used, which can result in
            visual glitches but may be faster. } 
         property ZSortAccuracy : TPFXSortAccuracy read FZSortAccuracy write FZSortAccuracy default saHigh;
         {: Maximum distance for rendering PFX particles.<p>
            If zero, camera's DepthOfView is used. }
         property ZMaxDistance : Single read FZMaxDistance write FZMaxDistance stored StoreZMaxDistance;
         {: Default blending mode for particles.<p>
            "Additive" blending is the usual mode (increases brightness and
            saturates), "transparency" may be used for smoke or systems that
            opacify view, "opaque" is more rarely used.<p>
            Note: specific PFX managers may override/ignore this setting. } 
			property BlendingMode : TBlendingMode read FBlendingMode write FBlendingMode default bmAdditive;

         property Visible;
   end;

   // TGLSourcePFXVelocityMode
   //
   TGLSourcePFXVelocityMode = (svmAbsolute, svmRelative);

   // TGLSourcePFXPositionMode
   //
   TGLSourcePFXPositionMode = (spmAbsoluteOffset, spmRelative);

   // TGLSourcePFXDispersionMode
   //
   TGLSourcePFXDispersionMode = (sdmFast, sdmIsotropic);

   // TGLSourcePFXEffect
   //
   {: Simple Particles Source.<p> }
   TGLSourcePFXEffect = class (TGLParticleFXEffect)
      private
         { Private Declarations }
         FInitialVelocity : TGLCoordinates;
         FInitialPosition : TGLCoordinates;
         FPositionDispersionRange : TGLCoordinates;
         FVelocityDispersion : Single;
         FPositionDispersion : Single;
         FParticleInterval : Single;
         FVelocityMode : TGLSourcePFXVelocityMode;
         FPositionMode : TGLSourcePFXPositionMode;
         FDispersionMode : TGLSourcePFXDispersionMode;
         FEnabled : Boolean;
         FDisabledIfOwnerInvisible: Boolean;
         FTimeRemainder : Double;
         FRotationDispersion: Single;

      protected
         { Protected Declarations }
         procedure SetInitialVelocity(const val : TGLCoordinates);
         procedure SetInitialPosition(const val : TGLCoordinates);
         procedure SetPositionDispersionRange(const val : TGLCoordinates);
         procedure SetParticleInterval(const val : Single);
         procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;

         function ParticleAbsoluteInitialPos : TAffineVector;

      public
         { Public Declarations }
         constructor Create(aOwner : TXCollection); override;
         destructor Destroy; override;

			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

         procedure DoProgress(const progressTime : TProgressTimes); override;

         //: Instantaneously creates nb particles
         procedure Burst(time : Double; nb : Integer);
         procedure RingExplosion(time : Double;
                                 minInitialSpeed, maxInitialSpeed : Single;
                                 nbParticles : Integer);

		published
			{ Published Declarations }
         property InitialVelocity : TGLCoordinates read FInitialVelocity write SetInitialVelocity;
         property VelocityDispersion : Single read FVelocityDispersion write FVelocityDispersion;
         property InitialPosition : TGLCoordinates read FInitialPosition write SetInitialPosition;
         property PositionDispersion : Single read FPositionDispersion write FPositionDispersion;
         property PositionDispersionRange : TGLCoordinates read FPositionDispersionRange write SetPositionDispersionRange;
         property ParticleInterval : Single read FParticleInterval write SetParticleInterval;
         property VelocityMode : TGLSourcePFXVelocityMode read FVelocityMode write FVelocityMode default svmAbsolute;
         property PositionMode : TGLSourcePFXPositionMode read FPositionMode write FPositionMode default spmAbsoluteOffset;
         property DispersionMode : TGLSourcePFXDispersionMode read FDispersionMode write FDispersionMode default sdmFast;
         property RotationDispersion : Single read FRotationDispersion write FRotationDispersion;
         property Enabled : boolean read FEnabled write FEnabled;
         property DisabledIfOwnerInvisible : boolean read FDisabledIfOwnerInvisible write FDisabledIfOwnerInvisible;
   end;

   // TGLDynamicPFXManager
   //
   {: An abstract PFX manager for simple dynamic particles.<p>
      Adds properties and progress implementation for handling moving particles
      (simple velocity and const acceleration integration). }
   TGLDynamicPFXManager = class (TGLParticleFXManager)
      private
         { Private Declarations }
         FAcceleration : TGLCoordinates;
         FFriction : Single;
         FCurrentTime : Double;
         
         FRotationCenter: TAffineVector;
         

      protected
         { Protected Declarations }
         procedure SetAcceleration(const val : TGLCoordinates);

         {: Returns the maximum age for a particle.<p>
            Particles older than that will be killed by DoProgress. }
         function MaxParticleAge : Single; dynamic; abstract;

         property CurrentTime : Double read FCurrentTime;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoProgress(const progressTime : TProgressTimes); override;

	   published
	      { Published Declarations }
         {: Oriented acceleration applied to the particles. }
         property Acceleration : TGLCoordinates read FAcceleration write SetAcceleration;
         {: Friction applied to the particles.<p>
            Friction is applied as a speed scaling factor over 1 second, ie.
            a friction of 0.5 will half speed over 1 second, a friction of 3
            will triple speed over 1 second, and a friction of 1 (default
            value) will have no effect. }
         property Friction : Single read FFriction write FFriction;
   end;

	// TPFXLifeColor
	//
	TPFXLifeColor = class (TCollectionItem)
	   private
	      { Private Declarations }
         FColorInner : TGLColor;
         FColorOuter : TGLColor;
         FLifeTime, FInvLifeTime : Single;
         FIntervalRatio : Single;
         FSizeScale : Single;
         FDoScale : Boolean;
         FDoRotate: boolean;
         
         FRotateAngle: Single;
         
	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetColorInner(const val : TGLColor);
         procedure SetColorOuter(const val : TGLColor);
         procedure SetLifeTime(const val : Single);
         procedure SetSizeScale(const val : Single);
         procedure SetRotateAngle(const Value: Single);  // indirectly persistent

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;

         {: Stores 1/LifeTime }
         property InvLifeTime : Single read FInvLifeTime;
         {: Stores 1/(LifeTime[Next]-LifeTime[Self]) }
         property InvIntervalRatio : Single read FIntervalRatio;

	   published
	      { Published Declarations }
         property ColorInner : TGLColor read FColorInner write SetColorInner;
         property ColorOuter : TGLColor read FColorOuter write SetColorOuter;
         property LifeTime : Single read FLifeTime write SetLifeTime;
         property SizeScale : Single read FSizeScale write SetSizeScale;
         
         property RotateAngle : Single read FRotateAngle write SetRotateAngle;

	end;

	// TPFXLifeColors
	//
	TPFXLifeColors = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TPFXLifeColor);
	      function GetItems(index : Integer) : TPFXLifeColor;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);

         function Add: TPFXLifeColor;
	      function FindItemID(ID: Integer): TPFXLifeColor;
	      property Items[index : Integer] : TPFXLifeColor read GetItems write SetItems; default;

         function MaxLifeTime : Double;
         function RotationsDefined : Boolean;
         procedure PrepareIntervalRatios;
   end;

   // TGLLifeColoredPFXManager
   //
   {: Base PFX manager for particles with life colors.<p>
      Particles have a core and edge color, for subclassing. }
   TGLLifeColoredPFXManager = class (TGLDynamicPFXManager)
      private
         { Private Declarations }
         FLifeColors : TPFXLifeColors;
         FLifeColorsLookup : TList;
         FLifeRotations : Boolean;
         FColorInner : TGLColor;
         FColorOuter : TGLColor;
         FParticleSize : Single;

      protected
         { Protected Declarations }
         procedure SetLifeColors(const val : TPFXLifeColors);
         procedure SetColorInner(const val : TGLColor);
         procedure SetColorOuter(const val : TGLColor);

         procedure InitializeRendering; override;
         procedure FinalizeRendering; override;

         function MaxParticleAge : Single; override;

         procedure ComputeColors(var lifeTime : Single; var inner, outer : TColorVector);
         procedure ComputeInnerColor(var lifeTime : Single; var inner : TColorVector);
         procedure ComputeOuterColor(var lifeTime : Single; var outer : TColorVector);
         function  ComputeSizeScale(var lifeTime : Single; var sizeScale : Single) : Boolean;
         function  ComputeRotateAngle(var lifeTime, rotateAngle: Single): Boolean;
         
         procedure RotateVertexBuf(buf : TAffineVectorList; lifeTime : Single;
                                   const pos : TAffineVector);

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         property ParticleSize : Single read FParticleSize write FParticleSize;
         property ColorInner : TGLColor read FColorInner write SetColorInner;
         property ColorOuter : TGLColor read FColorOuter write SetColorOuter;
         property LifeColors : TPFXLifeColors read FLifeColors write SetLifeColors;

	   published
	      { Published Declarations }
         property BlendingMode default bmAdditive;
   end;

   TPFXDirectRenderEvent = procedure (Sender : TObject; aParticle : TGLParticle;
                                      var rci : TRenderContextInfo) of object;
   TPFXProgressEvent = procedure (Sender : TObject; const progressTime : TProgressTimes;
                                  var defaultProgress : Boolean) of object;
   TPFXParticleProgress = procedure (Sender : TObject; const progressTime : TProgressTimes;
                                     aParticle : TGLParticle; var killParticle : Boolean) of object;
   TPFXGetParticleCountEvent = function (Sender : TObject) : Integer of object;

   // TGLCustomPFXManager
   //
   {: A particles FX manager offering events for customization/experimentation.<p>
      This manager essentially surfaces the PFX methods as events, and is best
      suited when you have specific particles that don't fall into any existing
      category, or when you want to experiment with particles and later plan to
      wrap things up in a full-blown manager.<br>
      If the events aren't handled, nothing will be rendered. }
   TGLCustomPFXManager = class (TGLLifeColoredPFXManager)
      private
         { Private Declarations }
         FOnInitializeRendering : TDirectRenderEvent;
         FOnBeginParticles : TDirectRenderEvent;
         FOnRenderParticle : TPFXDirectRenderEvent;
         FOnEndParticles : TDirectRenderEvent;
         FOnFinalizeRendering : TDirectRenderEvent;
         FOnProgress : TPFXProgressEvent;
         FOnParticleProgress : TPFXParticleProgress;
         FOnGetParticleCountEvent : TPFXGetParticleCountEvent;

      protected
         { Protected Declarations }
         function  TexturingMode : Cardinal; override;
         procedure InitializeRendering; override;
         procedure BeginParticles; override;
         procedure RenderParticle(aParticle : TGLParticle); override;
         procedure EndParticles; override;
         procedure FinalizeRendering; override;

      public
         { Public Declarations }
         procedure DoProgress(const progressTime : TProgressTimes); override;
         function ParticleCount : Integer; override;

	   published
	      { Published Declarations }
         property OnInitializeRendering : TDirectRenderEvent read FOnInitializeRendering write FOnInitializeRendering;
         property OnBeginParticles : TDirectRenderEvent read FOnBeginParticles write FOnBeginParticles;
         property OnRenderParticle : TPFXDirectRenderEvent read FOnRenderParticle write FOnRenderParticle;
         property OnEndParticles : TDirectRenderEvent read FOnEndParticles write FOnEndParticles;
         property OnFinalizeRendering : TDirectRenderEvent read FOnFinalizeRendering write FOnFinalizeRendering;
         property OnProgress : TPFXProgressEvent read FOnProgress write FOnProgress;
         property OnParticleProgress : TPFXParticleProgress read FOnParticleProgress write FOnParticleProgress;
         property OnGetParticleCountEvent : TPFXGetParticleCountEvent read FOnGetParticleCountEvent write FOnGetParticleCountEvent; 

         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

   // TGLPolygonPFXManager
   //
   {: Polygonal particles FX manager.<p>
      The particles of this manager are made of N-face regular polygon with
      a core and edge color. No texturing is available.<br>
      If you render large particles and don't have T&L acceleration, consider
      using TGLPointLightPFXManager. }
   TGLPolygonPFXManager = class (TGLLifeColoredPFXManager)
      private
         { Private Declarations }
         FNbSides : Integer;
         Fvx, Fvy : TAffineVector;        // NOT persistent
         FVertices : TAffineVectorList;   // NOT persistent
         FVertBuf : TAffineVectorList;    // NOT persistent

      protected
         { Protected Declarations }
         procedure SetNbSides(const val : Integer);

         function TexturingMode : Cardinal; override;
         procedure InitializeRendering; override;
         procedure BeginParticles; override;
         procedure RenderParticle(aParticle : TGLParticle); override;
         procedure EndParticles; override;
         procedure FinalizeRendering; override;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }
         property NbSides : Integer read FNbSides write SetNbSides default 6;

         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

   // TSpriteColorMode
   //
   {: Sprite color modes.<p>
      <ul>
      <li>scmFade: vertex coloring is used to fade inner-outer
      <li>scmInner: vertex coloring uses inner color only
      <li>scmOuter: vertex coloring uses outer color only
      <li>scmNone: vertex coloring is NOT used (colors are ignored).
      </ul> }
   TSpriteColorMode = (scmFade, scmInner, scmOuter, scmNone);

   // TSpritesPerTexture
   //
   {: Sprites per sprite texture for the SpritePFX. }
   TSpritesPerTexture = (sptOne, sptFour);

   // TGLBaseSpritePFXManager
   //
   {: Base class for sprite-based particles FX managers.<p>
      The particles are made of optionally centered single-textured quads. }
   TGLBaseSpritePFXManager = class (TGLLifeColoredPFXManager)
      private
         { Private Declarations }
         FTexHandle : TGLTextureHandle;
         Fvx, Fvy, Fvz : TAffineVector;   // NOT persistent
         FVertices : TAffineVectorList;   // NOT persistent
         FVertBuf : TAffineVectorList;    // NOT persistent
         FAspectRatio : Single;
         FRotation : Single;
         FShareSprites : TGLBaseSpritePFXManager;

         FSpritesPerTexture : TSpritesPerTexture;
         FColorMode : TSpriteColorMode;

      protected
         { Protected Declarations }
         {: Subclasses should draw their stuff in this bmp32. }
         procedure PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer); virtual; abstract;

         procedure BindTexture;
         procedure SetSpritesPerTexture(const val : TSpritesPerTexture); virtual;
         procedure SetColorMode(const val : TSpriteColorMode);
         procedure SetAspectRatio(const val : Single);
         function StoreAspectRatio : Boolean;
         procedure SetRotation(const val : Single);
         procedure SetShareSprites(const val : TGLBaseSpritePFXManager);

         function TexturingMode : Cardinal; override;
         procedure InitializeRendering; override;
         procedure BeginParticles; override;
         procedure RenderParticle(aParticle : TGLParticle); override;
         procedure EndParticles; override;
         procedure FinalizeRendering; override;

         property SpritesPerTexture : TSpritesPerTexture read FSpritesPerTexture write SetSpritesPerTexture;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         property ColorMode : TSpriteColorMode read FColorMode write SetColorMode;

	   published
	      { Published Declarations }
         {: Ratio between width and height.<p>
            An AspectRatio of 1 (default) will result in square sprite particles,
            values higher than one will result in horizontally stretched sprites,
            values below one will stretch vertically (assuming no rotation is applied). }
         property AspectRatio : Single read FAspectRatio write SetAspectRatio stored StoreAspectRatio;
         {: Particle sprites rotation (in degrees).<p>
            All particles of the PFX manager share this rotation. }
         property Rotation : Single read FRotation write SetRotation;
         {: If specified the manager will reuse the other manager's sprites.<p>
            Sharing sprites between PFX managers can help at the rendering stage
            if particles of the managers are mixed by helping reduce the number
            of texture switches. Note that only the texture is shared, not the
            colors, sizes or other dynamic parameters.<br> }
         property ShareSprites : TGLBaseSpritePFXManager read FShareSprites write FShareSprites;
   end;

   // TPFXPrepareTextureImageEvent
   //
   TPFXPrepareTextureImageEvent = procedure (Sender : TObject; destBmp32 : TGLBitmap32; var texFormat : Integer) of object;

   // TGLPointLightPFXManager
   //
   {: A sprite-based particles FX managers using user-specified code to prepare the texture.<p> }
   TGLCustomSpritePFXManager = class (TGLBaseSpritePFXManager)
      private
         { Private Declarations }
         FOnPrepareTextureImage : TPFXPrepareTextureImageEvent;

      protected
         { Protected Declarations }
         procedure PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer); override;

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }
         {: Place your texture rendering code in this event.<p> }
         property OnPrepareTextureImage : TPFXPrepareTextureImageEvent read FOnPrepareTextureImage write FOnPrepareTextureImage;

         property ColorMode default scmInner;
         property SpritesPerTexture default sptOne;
         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

   // TGLPointLightPFXManager
   //
   {: A sprite-based particles FX managers using point light maps.<p>
      The texture map is a round, distance-based transparency map (center "opaque"),
      you can adjust the quality (size) of the underlying texture map with the
      TexMapSize property.<p>
      This PFX manager renders particles similar to what you can get with
      TGLPolygonPFXManager but stresses fillrate more than T&L rate (and will
      usually be slower than the PolygonPFX when nbSides is low or T&L acceleration
      available). Consider this implementation as a sample for your own PFX managers
      that may use particles with more complex textures. }
   TGLPointLightPFXManager = class (TGLBaseSpritePFXManager)
      private
         { Private Declarations }
         FTexMapSize : Integer;

      protected
         { Protected Declarations }
         procedure PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer); override;

         procedure SetTexMapSize(const val : Integer);

      public
         { Public Declarations }
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }
         {: Underlying texture map size, as a power of two.<p>
            Min value is 3 (size=8), max value is 9 (size=512). }
         property TexMapSize : Integer read FTexMapSize write SetTexMapSize default 5;

         property ColorMode default scmInner;
         property ParticleSize;
         property ColorInner;
         property ColorOuter;
         property LifeColors;
   end;

{: Returns or creates the TGLBInertia within the given object's behaviours.<p> }
function GetOrCreateSourcePFX(obj : TGLBaseSceneObject; const name : String = '') : TGLSourcePFXEffect;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLCrossPlatform, GLState, GLUtils, PerlinNoise;

// GetOrCreateSourcePFX
//
function GetOrCreateSourcePFX(obj : TGLBaseSceneObject; const name : String = '') : TGLSourcePFXEffect;
var
	i : Integer;
begin
   with obj.Effects do begin
      if name='' then begin
      	i:=IndexOfClass(TGLSourcePFXEffect);
      	if i>=0 then
	      	Result:=TGLSourcePFXEffect(Items[i])
      	else Result:=TGLSourcePFXEffect.Create(obj.Effects);
      end else begin
         i:=IndexOfName(name);
         if i>=0 then
            Result:=(Items[i] as TGLSourcePFXEffect)
         else begin
            Result:=TGLSourcePFXEffect.Create(obj.Effects);
            Result.Name:=name;
         end;
      end;
   end;
end;

// RndVector
//
procedure RndVector(const dispersion : TGLSourcePFXDispersionMode;
                    var v : TAffineVector; var f : Single;
                    dispersionRange : TGLCoordinates);
var
   f2, fsq : Single;
   p : TVector;
begin
   f2:=2*f;
   if Assigned(dispersionRange) then
      p:=VectorScale(dispersionRange.DirectVector, f2)
   else p:=VectorScale(XYZHmgVector, f2);
   case dispersion of
      sdmFast : begin
         v[0]:=(Random-0.5)*p[0];
         v[1]:=(Random-0.5)*p[1];
         v[2]:=(Random-0.5)*p[2];
      end;
   else
      fsq:=Sqr(0.5);
      repeat
         v[0]:=(Random-0.5);
         v[1]:=(Random-0.5);
         v[2]:=(Random-0.5);
      until VectorNorm(v)<=fsq;
      v[0]:=v[0]*p[0];
      v[1]:=v[1]*p[1];
      v[2]:=v[2]*p[2];
   end;
end;

// ------------------
// ------------------ TGLParticle ------------------
// ------------------

// Create
//
constructor TGLParticle.Create;
begin
   FEffectScale:=1;
   inherited Create;
end;

// Destroy
//
destructor TGLParticle.Destroy;
begin
   inherited Destroy;
end;

// WriteToFiler
//
procedure TGLParticle.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteInteger(FID);
      Write(FPosition, SizeOf(FPosition));
      Write(FVelocity, SizeOf(FVelocity));
      WriteFloat(FCreationTime);
   end;
end;

// ReadFromFiler
//
procedure TGLParticle.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FID:=ReadInteger;
      Read(FPosition, SizeOf(FPosition));
      Read(FVelocity, SizeOf(FVelocity));
      FCreationTime:=ReadFloat;
   end else RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TGLParticleList ------------------
// ------------------

// Create
//
constructor TGLParticleList.Create;
begin
   inherited Create;
   FItemList:=TPersistentObjectList.Create;
   FitemList.GrowthDelta:=64;
   FDirectList:=nil;
end;

// Destroy
//
destructor TGLParticleList.Destroy;
begin
   FItemList.CleanFree;
   inherited Destroy;
end;

// WriteToFiler
//
procedure TGLParticleList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited WriteToFiler(writer);
   with writer do begin
      WriteInteger(0); // Archive Version 0
      FItemList.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TGLParticleList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : integer;
begin
   inherited ReadFromFiler(reader);
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FItemList.ReadFromFilerWithEvent(reader, AfterItemCreated);
      FDirectList:=PGLParticleArray(FItemList.List);
   end else RaiseFilerException(archiveVersion);
end;

// GetItems
//
function TGLParticleList.GetItems(index : Integer) : TGLParticle;
begin
   Result:=TGLParticle(FItemList[index]);
end;

// SetItems
//
procedure TGLParticleList.SetItems(index : Integer; val : TGLParticle);
begin
   FItemList[index]:=val;
end;

// AfterItemCreated
//
procedure TGLParticleList.AfterItemCreated(Sender : TObject);
begin
   (Sender as TGLParticle).Manager:=Self.Owner;
end;

// ItemCount
//
function TGLParticleList.ItemCount : Integer;
begin
   Result:=FItemList.Count;
end;

// AddItem
//
function  TGLParticleList.AddItem(aItem : TGLParticle) : Integer;
begin
   aItem.Manager:=Self.Owner;
   Result:=FItemList.Add(aItem);
   FDirectList:=PGLParticleArray(FItemList.List);
end;

// RemoveAndFreeItem
//
procedure TGLParticleList.RemoveAndFreeItem(aItem : TGLParticle);
var
   i : Integer;
begin
   i:=FItemList.IndexOf(aItem);
   if i>=0 then begin
      if aItem.Manager=Self.Owner then
         aItem.Manager:=nil;
      aItem.Free;
      FItemList.List[i]:=nil;
   end;
end;

// IndexOfItem
//
function TGLParticleList.IndexOfItem(aItem : TGLParticle) : Integer;
begin
   Result:=FItemList.IndexOf(aItem);
end;

// Pack
//
procedure TGLParticleList.Pack;
begin
   FItemList.Pack;
   FDirectList:=PGLParticleArray(FItemList.List);
end;

// ------------------
// ------------------ TGLParticleFXManager ------------------
// ------------------

// Create
//
constructor TGLParticleFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FUsers:= TList.create;
   FParticles:=TGLParticleList.Create;
   FParticles.Owner:=Self;
   FBlendingMode:=bmAdditive;
   RegisterManager(Self);
end;

// Destroy
//
destructor TGLParticleFXManager.Destroy;
var
i: integer;
begin
   inherited Destroy;
   for i:= FUsers.Count -1 downto 0 do
      TGLParticleFXEffect(FUsers[i]).managerNotification(self, opRemove);
   DeRegisterManager(Self);
   Renderer:=nil;
   FParticles.Free;
end;

// NotifyChange
//
procedure TGLParticleFXManager.NotifyChange(Sender : TObject);
begin
   if Assigned(FRenderer) then
      Renderer.StructureChanged;
end;

// DoProgress
//
procedure TGLParticleFXManager.DoProgress(const progressTime : TProgressTimes);
begin
   inherited;
   if FAutoFreeWhenEmpty and (FParticles.ItemCount=0) then Free;
end;

// ParticlesClass
//
class function TGLParticleFXManager.ParticlesClass : TGLParticleClass;
begin
   Result:=TGLParticle;
end;

// CreateParticle
//
function TGLParticleFXManager.CreateParticle : TGLParticle;
begin
   Result:=ParticlesClass.Create;
   Result.FID:=FNextID;
   if Assigned(cadencer) then
      Result.FCreationTime:=Cadencer.CurrentTime;
   Inc(FNextID);
   FParticles.AddItem(Result);
   if Assigned(FOnCreateParticle) then
      FOnCreateParticle(Self, Result);
end;

// CreateParticles
//
procedure TGLParticleFXManager.CreateParticles(nbParticles : Integer);
var
   i : Integer;
begin
   FParticles.FItemList.RequiredCapacity(FParticles.ItemCount+nbParticles);
   for i:=1 to nbParticles do
      CreateParticle;
end;

// SetRenderer
//
procedure TGLParticleFXManager.SetRenderer(const val : TGLParticleFXRenderer);
begin
   if FRenderer<>val then begin
      if Assigned(FRenderer) then
         FRenderer.UnRegisterManager(Self);
      FRenderer:=val;
      if Assigned(FRenderer) then
         FRenderer.RegisterManager(Self);
   end;
end;

// SetParticles
//
procedure TGLParticleFXManager.SetParticles(const aParticles : TGLParticleList);
begin
   FParticles.Assign(aParticles);
end;

// ParticleCount
//
function TGLParticleFXManager.ParticleCount : Integer;
begin
   Result:=FParticles.FItemList.Count;
end;

// ApplyBlendingMode
//
procedure TGLParticleFXManager.ApplyBlendingMode;
begin
   if Renderer.BlendingMode<>BlendingMode then begin
      // case disjunction to minimize OpenGL State changes
      if Renderer.BlendingMode in [bmAdditive, bmTransparency] then begin
         case BlendingMode of
            bmAdditive :
               glBlendFunc(GL_SRC_ALPHA, GL_ONE);
            bmTransparency :
               glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         else // bmOpaque
            glDisable(GL_BLEND);
         end;
      end else begin
         case BlendingMode of
            bmAdditive : begin
               glBlendFunc(GL_SRC_ALPHA, GL_ONE);
               glEnable(GL_BLEND);
            end;
            bmTransparency : begin
               glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
               glEnable(GL_BLEND);
            end;
         else
            // bmOpaque, do nothing
         end;
      end;
   end;
end;

// ApplyBlendingMode
//
procedure TGLParticleFXManager.UnapplyBlendingMode;
begin
   if Renderer.BlendingMode<>BlendingMode then begin
      // case disjunction to minimize OpenGL State changes
      if BlendingMode in [bmAdditive, bmTransparency] then begin
         case Renderer.BlendingMode of
            bmAdditive :
               glBlendFunc(GL_SRC_ALPHA, GL_ONE);
            bmTransparency :
               glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         else // bmOpaque
            glDisable(GL_BLEND);
         end;
      end else begin
         case Renderer.BlendingMode of
            bmAdditive : begin
               glBlendFunc(GL_SRC_ALPHA, GL_ONE);
               glEnable(GL_BLEND);
            end;
            bmTransparency : begin
               glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
               glEnable(GL_BLEND);
            end;
         else
            // bmOpaque, do nothing
         end;
      end;
   end;
end;

// registerUser
//
procedure TGLParticleFXManager.registerUser(obj: TGLParticleFXEffect);
begin
     if FUsers.IndexOf(obj) = -1 then
          FUsers.Add(obj);
end;

// unregisterUser
//
procedure TGLParticleFXManager.unregisterUser(obj: TGLParticleFXEffect);
begin
     FUsers.Remove(obj);
end;

// ------------------
// ------------------ TGLParticleFXEffect ------------------
// ------------------

// Create
//
constructor TGLParticleFXEffect.Create(aOwner : TXCollection);
begin
     FEffectScale := 1;
   inherited;
end;

// Destroy
//
destructor TGLParticleFXEffect.Destroy;
begin
   Manager:=nil;
   inherited Destroy;
end;


// WriteToFiler
//
procedure TGLParticleFXEffect.WriteToFiler(writer : TWriter);
var
   st: string;
begin
   with writer do begin
      WriteInteger(1);  // ArchiveVersion 1
      if Manager<>nil then
         st := Manager.GetNamePath
      else
          st :='';
      WriteString(st);
      WriteFloat(FEffectScale);
   end;
end;


// ReadFromFiler
//
procedure TGLParticleFXEffect.ReadFromFiler(reader : TReader);
var
   archiveVersion : integer;
begin
   with reader do begin
      archiveVersion:=ReadInteger;
      Assert(archiveVersion in [0..1]);
      if archiveVersion>=0 then
      begin
           FManagerName:=ReadString;
           Manager:=nil;
      end;
      if archiveVersion>=1 then
      begin
           FEffectScale:=ReadFloat;
      end;
   end;
end;



// Loaded
//
procedure TGLParticleFXEffect.Loaded;
var
   mng : TComponent;
begin
   inherited;
   if FManagerName<>'' then begin
      mng:=FindManager(TGLParticleFXManager, FManagerName);
      if Assigned(mng) then
         Manager:=TGLParticleFXManager(mng);
      FManagerName:='';
   end;
end;

// SetManager
//
procedure TGLParticleFXEffect.SetManager(val : TGLParticleFXManager);
begin
   if assigned(FManager) then
      FManager.unregisterUser(self);
   FManager:=val;
   if assigned(FManager) then
      FManager.registerUser(self);
end;

procedure TGLParticleFXEffect.SetEffectScale(const Value: single);
begin
  FEffectScale := Value;
end;

// managerNotification
//
procedure TGLParticleFXEffect.managerNotification(
  aManager: TGLParticleFXManager; Operation: TOperation);
begin
   if (Operation = opRemove) and (aManager = manager) then
      manager:= nil;
end;


// ------------------
// ------------------ TGLParticleFXRenderer ------------------
// ------------------

// Create
//
constructor TGLParticleFXRenderer.Create(aOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osNoVisibilityCulling, osDirectDraw];
   FZTest:=True;
   FZCull:=True;
   FZSortAccuracy:=saHigh;
   FManagerList:=TList.Create;
   FBlendingMode:=bmAdditive;
end;

// Destroy
//
destructor TGLParticleFXRenderer.Destroy;
var
   i : Integer;
begin
   for i:=0 to cPFXNbRegions-1 do begin
      FreeMem(FRegions[i].particleRef);
      FreeMem(FRegions[i].particleOrder);
   end;

   UnRegisterAll;
   FManagerList.Free;
   inherited Destroy;
end;

// RegisterManager
//
procedure TGLParticleFXRenderer.RegisterManager(aManager : TGLParticleFXManager);
begin
   FManagerList.Add(aManager);
end;

// UnRegisterManager
//
procedure TGLParticleFXRenderer.UnRegisterManager(aManager : TGLParticleFXManager);
begin
   FManagerList.Remove(aManager);
end;

// UnRegisterAll
//
procedure TGLParticleFXRenderer.UnRegisterAll;
begin
   while FManagerList.Count>0 do
      TGLParticleFXManager(FManagerList[FManagerList.Count-1]).Renderer:=nil;
end;

// BuildList
// (beware, large and complex stuff below... this is the heart of the ParticleFX)
procedure TGLParticleFXRenderer.BuildList(var rci : TRenderContextInfo);
{
   Quick Explanation of what is below:

   The purpose is to depth-sort a large number (thousandths) of particles and
   render them back to front. The rendering part is not particularly complex,
   it just invokes the various PFX managers involved and request particle
   renderings.
   The sort uses a first-pass region partition (the depth range is split into
   regions, and particles are assigned directly to the region they belong to),
   then each region is sorted with a QuickSort.
   The QuickSort itself is the regular classic variant, but the comparison is
   made on singles as if they were integers, this is allowed by the IEEE format
   in a very efficient manner if all values are superior to 1, which is ensured
   by the distance calculation and a fixed offset of 1.
}
type
   PInteger = ^Integer;
   PSingle = ^Single;
var
   dist, distDelta, invRegionSize : Single;
   managerIdx, particleIdx, regionIdx : Integer;

   procedure QuickSortRegion(startIndex, endIndex : Integer; region : PPFXRegion);
   var
      I, J : Integer;
      P : Integer;
      poptr : PPointerArray;
      buf : Pointer;
   begin
      if endIndex-startIndex>1 then begin
         poptr:=@region.particleOrder[0];
         repeat
            I:=startIndex;
            J:=endIndex;
            P:=PParticleReference(poptr[(I + J) shr 1]).distance;
            repeat
               while PParticleReference(poptr[I]).distance<P do Inc(I);
               while PParticleReference(poptr[J]).distance>P do Dec(J);
               if I<=J then begin
                  buf:=poptr[I];
                  poptr[I]:=poptr[J];
                  poptr[J]:=buf;
                  Inc(I); Dec(J);
               end;
            until I>J;
            if startIndex<J then
               QuickSortRegion(startIndex, J, region);
            startIndex:=I;
         until I >= endIndex;
      end else if endIndex-startIndex>0 then begin
         poptr:=@region.particleOrder[0];
         if PParticleReference(poptr[endIndex]).distance<PParticleReference(poptr[startIndex]).distance then begin
            buf:=poptr[startIndex];
            poptr[startIndex]:=poptr[endIndex];
            poptr[endIndex]:=buf;
         end;
      end;
   end;

   // !! WARNING !! This may cause incorrect behaviour if optimization is turned
   // off for the project.
   procedure DistToRegionIdx; register;
   asm
//   begin
      // fast version of
//      regionIdx := Trunc((dist - distDelta) * invRegionSize);
      FLD     dist
      FSUB    distDelta
      FMUL    invRegionSize
      FISTP   regionIdx
   end;

var
   minDist, maxDist, sortMaxRegion : Integer;
   curManager : TGLParticleFXManager;
   curList : PGLParticleArray;
   curParticle : TGLParticle;
   curRegion : PPFXRegion;
   curParticleOrder : PPointerArray;
   cameraPos, cameraVector : TAffineVector;
   timer : Int64;
   oldDepthMask : TGLboolean;
   currentTexturingMode : Cardinal;
begin
   if csDesigning in ComponentState then Exit;
   FCurrentRCI:=@rci;
   timer:=StartPrecisionTimer;
   // precalcs
   with Scene.CurrentGLCamera do begin
      PSingle(@minDist)^:=NearPlane+1;
      if ZMaxDistance<=0 then begin
         PSingle(@maxDist)^:=NearPlane+DepthOfView+1;
         invRegionSize:=(cPFXNbRegions-2)/DepthOfView;
      end else begin
         PSingle(@maxDist)^:=NearPlane+ZMaxDistance+1;
         invRegionSize:=(cPFXNbRegions-2)/ZMaxDistance;
      end;
      distDelta:=NearPlane+1+0.49999/invRegionSize
   end;
   SetVector(cameraPos, rci.cameraPosition);
   SetVector(cameraVector, rci.cameraDirection);
   try
      // Collect particles
      // only depth-clipping performed as of now.
      FLastParticleCount:=0;
      for managerIdx:=0 to FManagerList.Count-1 do begin
         curManager:=TGLParticleFXManager(FManagerList[managerIdx]);
         curList:=curManager.FParticles.List;
         Inc(FLastParticleCount, curManager.ParticleCount);
         for particleIdx:=0 to curManager.ParticleCount-1 do begin
            curParticle:=curList[particleIdx];
            dist:=PointProject(curParticle.FPosition, cameraPos, cameraVector)+1;
            if not FZCull then begin
               if PInteger(@dist)^<minDist then
                  PInteger(@dist)^:=minDist;
            end;
            if (PInteger(@dist)^>=minDist) and (PInteger(@dist)^<=maxDist) then begin
               DistToRegionIdx;
               curRegion:=@FRegions[regionIdx];
               // add particle to region
               if curRegion.count=curRegion.capacity then begin
                  Inc(curRegion.capacity, cPFXGranularity);
                  ReallocMem(curRegion.particleRef, curRegion.capacity*SizeOf(TParticleReference));
                  ReallocMem(curRegion.particleOrder, curRegion.capacity*SizeOf(Pointer));
               end;
               with curRegion.particleRef[curRegion.count] do begin
                  particle:=curParticle;
                  distance:=PInteger(@dist)^;
               end;
               Inc(curRegion.count);
            end;
         end;
      end;
      // Sort regions
      case ZSortAccuracy of
         saLow : sortMaxRegion:=0;
         saOneTenth : sortMaxRegion:=cPFXNbRegions div 10;
         saOneThird : sortMaxRegion:=cPFXNbRegions div 3;
         saOneHalf : sortMaxRegion:=cPFXNbRegions div 2;
      else
         sortMaxRegion:=cPFXNbRegions;
      end;
      for regionIdx:=0 to cPFXNbRegions-1 do begin
         curRegion:=@FRegions[regionIdx];
         if curRegion.count>1 then begin
            // Prepare order table
            with curRegion^ do for particleIdx:=0 to count-1 do
               particleOrder[particleIdx]:=@particleRef[particleIdx];
            // QuickSort
            if (regionIdx<sortMaxRegion) and (FBlendingMode<>bmAdditive) then
               QuickSortRegion(0, curRegion.count-1, curRegion);
         end else if curRegion.Count=1 then begin
            // Prepare order table
            curRegion.particleOrder[0]:=@curRegion.particleRef[0];
         end;
      end;
      FLastSortTime:=StopPrecisionTimer(timer)*1000;

      glPushMatrix;
      glLoadMatrixf(@Scene.CurrentBuffer.ModelViewMatrix);

      glPushAttrib(GL_ALL_ATTRIB_BITS);
      
      glDisable(GL_CULL_FACE);
      glDisable(GL_TEXTURE_2D);
      currentTexturingMode:=0;
      glDisable(GL_LIGHTING);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

      case FBlendingMode of
         bmAdditive : begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE);
            glEnable(GL_BLEND);
         end;
         bmTransparency : begin
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            glEnable(GL_BLEND);
         end;
      else
         // bmOpaque, do nothing
      end;
      glDepthFunc(GL_LEQUAL);
      if not FZWrite then begin
         glGetBooleanv(GL_DEPTH_WRITEMASK, @oldDepthMask);
         glDepthMask(False);
      end;
      if not FZTest then
         glDisable(GL_DEPTH_TEST);

      try
         // Initialize managers
         for managerIdx:=0 to FManagerList.Count-1 do
            TGLParticleFXManager(FManagerList.List[managerIdx]).InitializeRendering;
         // Start Rendering... at last ;)
         try
            curManager:=nil;
            for regionIdx:=cPFXNbRegions-1 downto 0 do begin
               curRegion:=@FRegions[regionIdx];
               if curRegion.count>0 then begin
                  curParticleOrder:=@curRegion.particleOrder[0];
                  for particleIdx:=curRegion.count-1 downto 0 do begin
                     curParticle:=PParticleReference(curParticleOrder[particleIdx]).particle;
                     if curParticle.Manager<>curManager then begin
                        if Assigned(curManager) then
                           curManager.EndParticles;
                        curManager:=curParticle.Manager;
                        if curManager.TexturingMode<>currentTexturingMode then begin
                           if currentTexturingMode<>0 then
                              glDisable(currentTexturingMode);
                           currentTexturingMode:=curManager.TexturingMode;
                           if currentTexturingMode<>0 then
                              glEnable(currentTexturingMode);
                        end;
                        curManager.BeginParticles;
                     end;
                     curManager.RenderParticle(curParticle);
                  end;
               end;
            end;
            if Assigned(curManager) then
               curManager.EndParticles;
         finally
            // Finalize managers
            for managerIdx:=0 to FManagerList.Count-1 do
               TGLParticleFXManager(FManagerList.List[managerIdx]).FinalizeRendering;
         end;
      finally
         if FZWrite then
            glDepthMask(oldDepthMask);
         glPopMatrix;
         glPopAttrib;
      end;
   finally
      // cleanup
      for regionIdx:=cPFXNbRegions-1 downto 0 do
         FRegions[regionIdx].count:=0;
   end;
end;

// StoreZMaxDistance
//
function TGLParticleFXRenderer.StoreZMaxDistance : Boolean;
begin
   Result:=(FZMaxDistance<>0);
end;

// ------------------
// ------------------ TGLSourcePFXEffect ------------------
// ------------------

// Create
//
constructor TGLSourcePFXEffect.Create(aOwner : TXCollection);
begin
   inherited;
   FInitialVelocity:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
   FInitialPosition:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csPoint);
   FPositionDispersionRange:=TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csPoint);
   FVelocityDispersion:=0;
   FPositionDispersion:=0;
   FParticleInterval:=0.1;
   FVelocityMode:=svmAbsolute;
   FPositionMode:=spmAbsoluteOffset;
   FDispersionMode:=sdmFast;
   FEnabled := true;
   FDisabledIfOwnerInvisible := False;
end;

// Destroy
//
destructor TGLSourcePFXEffect.Destroy;
begin
   FPositionDispersionRange.Free;
   FInitialVelocity.Free;
   FInitialPosition.Free;
   inherited Destroy;
end;

// FriendlyName
//
class function TGLSourcePFXEffect.FriendlyName : String;
begin
   Result:='PFX Source';
end;

// FriendlyDescription
//
class function TGLSourcePFXEffect.FriendlyDescription : String;
begin
   Result:='Simple Particles FX Source';
end;

// WriteToFiler
//
procedure TGLSourcePFXEffect.WriteToFiler(writer : TWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(6);  // ArchiveVersion 6, added FPositionMode
                        // ArchiveVersion 5, added FDisabledIfOwnerInvisible:
                        // ArchiveVersion 4, added FRotationDispersion
                        // ArchiveVersion 3, added FEnabled
                        // ArchiveVersion 2, added FPositionDispersionRange
                        // ArchiveVersion 1, added FDispersionMode
      FInitialVelocity.WriteToFiler(writer);
      FInitialPosition.WriteToFiler(writer);
      FPositionDispersionRange.WriteToFiler(writer);
      WriteFloat(FVelocityDispersion);
      WriteFloat(FPositionDispersion);
      WriteFloat(FParticleInterval);
      WriteInteger(Integer(FVelocityMode));
      WriteInteger(Integer(FDispersionMode));
      WriteBoolean(FEnabled);
      WriteFloat(FRotationDispersion);
      WriteBoolean(FDisabledIfOwnerInvisible);
      WriteInteger(Integer(FPositionMode));
   end;
end;

// ReadFromFiler
//
procedure TGLSourcePFXEffect.ReadFromFiler(reader : TReader);
var
   archiveVersion : Integer;
begin
   inherited;
   with reader do begin
      archiveVersion:=ReadInteger;
      Assert(archiveVersion in [0..6]);
      FInitialVelocity.ReadFromFiler(reader);
      FInitialPosition.ReadFromFiler(reader);
      if archiveVersion>=2 then
         FPositionDispersionRange.ReadFromFiler(reader);
      FVelocityDispersion:=ReadFloat;
      FPositionDispersion:=ReadFloat;
      FParticleInterval:=ReadFloat;
      FVelocityMode:=TGLSourcePFXVelocityMode(ReadInteger);
      if archiveVersion>=1 then
         FDispersionMode:=TGLSourcePFXDispersionMode(ReadInteger);
      if archiveVersion>=3 then
         FEnabled:=ReadBoolean;
      if archiveVersion>=4 then
         FRotationDispersion := ReadFloat;
      if archiveVersion>=5 then
         FDisabledIfOwnerInvisible:=ReadBoolean;
      if archiveVersion>=6 then
         FPositionMode:=TGLSourcePFXPositionMode(ReadInteger);
   end;
end;

// SetInitialVelocity
//
procedure TGLSourcePFXEffect.SetInitialVelocity(const val : TGLCoordinates);
begin
   FInitialVelocity.Assign(val);
end;

// SetInitialPosition
//
procedure TGLSourcePFXEffect.SetInitialPosition(const val : TGLCoordinates);
begin
   FInitialPosition.Assign(val);
end;

// SetPositionDispersionRange
//
procedure TGLSourcePFXEffect.SetPositionDispersionRange(const val : TGLCoordinates);
begin
   FPositionDispersionRange.Assign(val);
end;

// SetParticleInterval
//
procedure TGLSourcePFXEffect.SetParticleInterval(const val : Single);
begin
   if FParticleInterval<>val then begin
      FParticleInterval:=val;
      if FParticleInterval<0 then FParticleInterval:=0;
      if FTimeRemainder>FParticleInterval then
         FTimeRemainder:=FParticleInterval;
   end;
end;

// DoProgress
//
procedure TGLSourcePFXEffect.DoProgress(const progressTime : TProgressTimes);
var
   n : Integer;
begin
   if Enabled and Assigned(Manager) and (ParticleInterval>0) then
   begin
      if OwnerBaseSceneObject.Visible or (not DisabledIfOwnerInvisible) then begin
         FTimeRemainder:=FTimeRemainder+progressTime.deltaTime;
         if FTimeRemainder>FParticleInterval then begin
            n:=Trunc((FTimeRemainder-FParticleInterval)/FParticleInterval);
            Burst(progressTime.newTime, n);
            FTimeRemainder:=FTimeRemainder-n*FParticleInterval;
         end;
      end;
   end;
end;

// ParticleAbsoluteInitialPos
//
function TGLSourcePFXEffect.ParticleAbsoluteInitialPos : TAffineVector;
begin
   if PositionMode=spmRelative then begin
      Result:=OwnerBaseSceneObject.LocalToAbsolute(InitialPosition.AsAffineVector);
   end else begin
      SetVector(Result, OwnerBaseSceneObject.AbsolutePosition);
      AddVector(Result, InitialPosition.AsAffineVector);
   end;
end;

// Burst
//
procedure TGLSourcePFXEffect.Burst(time : Double; nb : Integer);
var
   particle : TGLParticle;
   av, pos : TAffineVector;
begin
   if Manager=nil then Exit;
   pos:=ParticleAbsoluteInitialPos;
   while nb>0 do begin
      particle:=Manager.CreateParticle;
      particle.FEffectScale:=EffectScale;
      RndVector(DispersionMode, av, FPositionDispersion, FPositionDispersionRange);
      VectorAdd(pos, av, @particle.Position);
      RndVector(DispersionMode, av, FVelocityDispersion, nil);
      if VelocityMode=svmRelative then
         SetVector(particle.FVelocity, OwnerBaseSceneObject.LocalToAbsolute(InitialVelocity.AsVector))
      else SetVector(particle.FVelocity, InitialVelocity.AsVector);
      AddVector(particle.FVelocity, av);
      particle.CreationTime:=time;
      Dec(nb);
   end;
end;

// RingExplosion
//
procedure TGLSourcePFXEffect.RingExplosion(time : Double;
                                           minInitialSpeed, maxInitialSpeed : Single;
                                           nbParticles : Integer);
var
   particle : TGLParticle;
   av, pos, tmp : TAffineVector;
   ringVectorX, ringVectorY : TAffineVector;
   fx, fy, d : Single;
begin
   if (Manager=nil) or (nbParticles<=0) then Exit;
   pos:=ParticleAbsoluteInitialPos;
   SetVector(ringVectorY, OwnerBaseSceneObject.AbsoluteUp);
   SetVector(ringVectorX, OwnerBaseSceneObject.AbsoluteDirection);
   ringVectorY:=VectorCrossProduct(ringVectorX, ringVectorY);
   while (nbParticles>0) do begin
      // okay, ain't exactly an "isotropic" ring...
      fx:=Random-0.5;
      fy:=Random-0.5;
      d:=RLength(fx, fy);
      tmp:=VectorCombine(ringVectorX, ringVectorY, fx*d, fy*d);
      ScaleVector(tmp, minInitialSpeed+Random*(maxInitialSpeed-minInitialSpeed));
      AddVector(tmp, InitialVelocity.AsVector);
      particle:=Manager.CreateParticle;
      with particle do begin
         RndVector(DispersionMode, av, FPositionDispersion, FPositionDispersionRange);
         VectorAdd(pos, av, @Position);
         RndVector(DispersionMode, av, FVelocityDispersion, nil);
         VectorAdd(tmp, av, @Velocity);
         if VelocityMode=svmRelative then
            Velocity:=OwnerBaseSceneObject.LocalToAbsolute(Velocity);
         particle.CreationTime:=time;
      end;
      Dec(nbParticles);
   end;
end;

// ------------------
// ------------------ TPFXLifeColor ------------------
// ------------------

// Create
//
constructor TPFXLifeColor.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FColorInner:=TGLColor.CreateInitialized(Self, NullHmgVector);
   FColorOuter:=TGLColor.CreateInitialized(Self, NullHmgVector);
   FLifeTime:=1;
   FInvLifeTime:=1;
   FSizeScale:=1;
   FRotateAngle:=0;
end;

// Destroy
//
destructor TPFXLifeColor.Destroy;
begin
   FColorOuter.Free;
   FColorInner.Free;
	inherited Destroy;
end;

// Assign
//
procedure TPFXLifeColor.Assign(Source: TPersistent);
begin
	if Source is TPFXLifeColor then
  begin
      FColorInner.Assign(TPFXLifeColor(Source).ColorInner);
      FColorOuter.Assign(TPFXLifeColor(Source).ColorOuter);
      FLifeTime := TPFXLifeColor(Source).LifeTime;
      FRotateAngle := TPFXLifeColor(Source).RotateAngle;
	end
  else
    inherited;
end;

// GetDisplayName
//
function TPFXLifeColor.GetDisplayName : String;
begin
	Result:=Format('LifeTime %f - Inner [%.2f, %.2f, %.2f, %.2f] - Outer [%.2f, %.2f, %.2f, %.2f]',
                  [LifeTime,
                   ColorInner.Red, ColorInner.Green, ColorInner.Blue, ColorInner.Alpha,
                   ColorOuter.Red, ColorOuter.Green, ColorOuter.Blue, ColorOuter.Alpha]);
end;

// SetColorInner
//
procedure TPFXLifeColor.SetColorInner(const val : TGLColor);
begin
   FColorInner.Assign(val);
end;

// SetColorOuter
//
procedure TPFXLifeColor.SetColorOuter(const val : TGLColor);
begin
   FColorOuter.Assign(val);
end;

// SetLifeTime
//
procedure TPFXLifeColor.SetLifeTime(const val : Single);
begin
   if FLifeTime<>val then begin
      FLifeTime:=val;
      if FLifeTime<=0 then FLifeTime:=1e-6;
      FInvLifeTime:=1/FLifeTime;
   end;
end;

// SetSizeScale
//
procedure TPFXLifeColor.SetSizeScale(const val : Single);
begin
   if FSizeScale<>val then begin
      FSizeScale:=val;
      FDoScale:=(FSizeScale<>1);
   end;
end;

procedure TPFXLifeColor.SetRotateAngle(const Value: Single);
begin
  if FRotateAngle <> Value then
  begin
    FRotateAngle := Value;
    FDoRotate := (FRotateAngle <> 0);
  end;
end;

// ------------------
// ------------------ TPFXLifeColors ------------------
// ------------------

constructor TPFXLifeColors.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TPFXLifeColor);
end;

function TPFXLifeColors.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TPFXLifeColors.SetItems(index : Integer; const val : TPFXLifeColor);
begin
	inherited Items[index]:=val;
end;

function TPFXLifeColors.GetItems(index : Integer) : TPFXLifeColor;
begin
	Result:=TPFXLifeColor(inherited Items[index]);
end;

function TPFXLifeColors.Add: TPFXLifeColor;
begin
	Result:=(inherited Add) as TPFXLifeColor;
end;

// FindItemID
//
function TPFXLifeColors.FindItemID(ID: Integer): TPFXLifeColor;
begin
	Result:=(inherited FindItemID(ID)) as TPFXLifeColor;
end;

// MaxLifeTime
//
function TPFXLifeColors.MaxLifeTime : Double;
begin
   if Count>0 then
      Result:=Items[Count-1].LifeTime
   else Result:=1e30;
end;

// RotationsDefined
//
function TPFXLifeColors.RotationsDefined : Boolean;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      if Items[i].RotateAngle<>0 then begin
         Result:=True;
         Exit;
      end;
   end;
   Result:=False;
end;

// PrepareIntervalRatios
//
procedure TPFXLifeColors.PrepareIntervalRatios;
var
   i : Integer;
begin
   for i:=0 to Count-2 do
      Items[i].FIntervalRatio:=1/(Items[i+1].LifeTime-Items[i].LifeTime);
end;

// ------------------
// ------------------ TGLDynamicPFXManager ------------------
// ------------------

// Create
//
constructor TGLDynamicPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FAcceleration:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
   FFriction:=1;
end;

// Destroy
//
destructor TGLDynamicPFXManager.Destroy;
begin
   FAcceleration.Free;
   inherited Destroy;
end;

// DoProgress
//
procedure TGLDynamicPFXManager.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
   curParticle : TGLParticle;
   maxAge : Double;
   pos, pos1, axis, accelVector : TAffineVector;
   ff, dt : Single;
   list : PGLParticleArray;
   doFriction, doPack : Boolean;
   frictionScale : Single;
   pos4: TVector;
begin
   maxAge:=MaxParticleAge;
   accelVector:=Acceleration.AsAffineVector;
   dt:=progressTime.deltaTime;
   doFriction:=(FFriction<>1);
   if doFriction then begin
      frictionScale:=Power(FFriction, dt)
   end else frictionScale:=1;
   FCurrentTime:=progressTime.newTime;

   doPack:=False;
   list:=Particles.List;
   for i:=0 to Particles.ItemCount-1 do begin
      curParticle:=list[i];
      if (progressTime.newTime-curParticle.CreationTime)<maxAge then begin
         // particle alive, just update velocity and position
         with curParticle do begin
            CombineVector(FPosition, FVelocity, dt);

            if (FRotation <> 0) and (Renderer <> nil) then begin
              pos := FPosition;
              pos1 := FPosition;
              ff := 1;
              CombineVector(pos1, FVelocity, ff);

              SetVector(axis, Renderer.Scene.CurrentGLCamera.AbsolutePosition);
              SetVector(axis, VectorSubtract(axis, FRotationCenter));
              NormalizeVector(axis);
              MakeVector(pos4, pos1);
              pos4[0] := pos4[0] - FRotationCenter[0];
              pos4[1] := pos4[1] - FRotationCenter[1];
              pos4[2] := pos4[2] - FRotationCenter[2];
              RotateVector(pos4, axis, FRotation * dt);
              pos4[0] := pos4[0] + FRotationCenter[0];
              pos4[1] := pos4[1] + FRotationCenter[1];
              pos4[2] := pos4[2] + FRotationCenter[2];
              MakeVector(pos1, pos4[0], pos4[1], pos4[2]);

              FVelocity := VectorSubtract(pos1, pos);
              CombineVector(FPosition, FVelocity, dt);
            end;
            
            CombineVector(FVelocity, accelVector, dt);
            if doFriction then
               ScaleVector(FVelocity, frictionScale);
         end;
      end else begin
         // kill particle
         curParticle.Free;
         list[i]:=nil;
         doPack:=True;
      end;
   end;
   if doPack then
      Particles.Pack;
end;

// SetAcceleration
//
procedure TGLDynamicPFXManager.SetAcceleration(const val : TGLCoordinates);
begin
   FAcceleration.Assign(val);
end;

// ------------------
// ------------------ TGLLifeColoredPFXManager ------------------
// ------------------

// Create
//
constructor TGLLifeColoredPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FLifeColors:=TPFXLifeColors.Create(Self);
   FColorInner:=TGLColor.CreateInitialized(Self, clrYellow);
   FColorOuter:=TGLColor.CreateInitialized(Self, NullHmgVector);
   with FLifeColors.Add do begin
      LifeTime:=3;
   end;
   FParticleSize:=1;
end;

// Destroy
//
destructor TGLLifeColoredPFXManager.Destroy;
begin
   FLifeColors.Free;
   FColorInner.Free;
   FColorOuter.Free;
   inherited Destroy;
end;

// SetColorInner
//
procedure TGLLifeColoredPFXManager.SetColorInner(const val : TGLColor);
begin
   FColorInner.Assign(val);
end;

// SetColorOuter
//
procedure TGLLifeColoredPFXManager.SetColorOuter(const val : TGLColor);
begin
   FColorOuter.Assign(val);
end;

// SetLifeColors
//
procedure TGLLifeColoredPFXManager.SetLifeColors(const val : TPFXLifeColors);
begin
   FLifeColors.Assign(Self);
end;

// InitializeRendering
//
procedure TGLLifeColoredPFXManager.InitializeRendering;
var
   i, n : Integer;
begin
   n:=LifeColors.Count;
   FLifeColorsLookup:=TList.Create;
   FLifeColorsLookup.Capacity:=n;
   for i:=0 to n-1 do
      FLifeColorsLookup.Add(LifeColors[i]);
   FLifeRotations:=LifeColors.RotationsDefined;
   LifeColors.PrepareIntervalRatios;
end;

// FinalizeRendering
//
procedure TGLLifeColoredPFXManager.FinalizeRendering;
begin
   FLifeColorsLookup.Free;
end;

// MaxParticleAge
//
function TGLLifeColoredPFXManager.MaxParticleAge : Single;
begin
   Result:=LifeColors.MaxLifeTime;
end;

// ComputeColors
//
procedure TGLLifeColoredPFXManager.ComputeColors(var lifeTime : Single; var inner, outer : TColorVector);
var
   i, k, n : Integer;
   f : Single;
   lck, lck1 : TPFXLifeColor;
begin
   with LifeColors do begin
      n:=Count-1;
      if n<0 then begin
         inner:=ColorInner.Color;
         outer:=ColorOuter.Color;
      end else begin
         if n>0 then begin
            k:=-1;
            for i:=0 to n do
               if TPFXLifeColor(FLifeColorsLookup.List[i]).LifeTime<lifeTime then k:=i;
            if k<n then Inc(k);
         end else k:=0;
         case k of
            0 : begin
               lck:=TPFXLifeColor(FLifeColorsLookup.List[k]);
               f:=lifeTime*lck.InvLifeTime;
               VectorLerp(ColorInner.Color, lck.ColorInner.Color, f, inner);
               VectorLerp(ColorOuter.Color, lck.ColorOuter.Color, f, outer);
            end;
         else
            lck:=TPFXLifeColor(FLifeColorsLookup.List[k]);
            lck1:=TPFXLifeColor(FLifeColorsLookup.List[k-1]);
            f:=(lifeTime-lck1.LifeTime)*lck1.InvIntervalRatio;
            VectorLerp(lck1.ColorInner.Color, lck.ColorInner.Color, f, inner);
            VectorLerp(lck1.ColorOuter.Color, lck.ColorOuter.Color, f, outer);
         end;
      end;
   end;
end;

// ComputeInnerColor
//
procedure TGLLifeColoredPFXManager.ComputeInnerColor(var lifeTime : Single; var inner : TColorVector);
var
   i, k, n : Integer;
   f : Single;
   lck, lck1 : TPFXLifeColor;
   lifeColorsLookupList : PPointerList;
begin
   with LifeColors do begin
      n:=Count-1;
      if n<0 then
         inner:=ColorInner.Color
      else begin
         lifeColorsLookupList:=FLifeColorsLookup.List;
         if n>0 then begin
            k:=-1;
            for i:=0 to n do
               if TPFXLifeColor(lifeColorsLookupList[i]).LifeTime<lifeTime then k:=i;
            if k<n then Inc(k);
         end else k:=0;
         if k=0 then begin
            lck:=TPFXLifeColor(lifeColorsLookupList[k]);
            f:=lifeTime*lck.InvLifeTime;
            VectorLerp(ColorInner.Color, lck.ColorInner.Color, f, inner);
         end else begin
            lck:=TPFXLifeColor(lifeColorsLookupList[k]);
            lck1:=TPFXLifeColor(lifeColorsLookupList[k-1]);
            f:=(lifeTime-lck1.LifeTime)*lck1.InvIntervalRatio;
            VectorLerp(lck1.ColorInner.Color, lck.ColorInner.Color, f, inner);
         end;
      end;
   end;
end;

// ComputeOuterColor
//
procedure TGLLifeColoredPFXManager.ComputeOuterColor(var lifeTime : Single; var outer : TColorVector);
var
   i, k, n : Integer;
   f : Single;
   lck, lck1 : TPFXLifeColor;
begin
   with LifeColors do begin
      n:=Count-1;
      if n<0 then
         outer:=ColorOuter.Color
      else begin
         if n>0 then begin
            k:=-1;
            for i:=0 to n do
               if TPFXLifeColor(FLifeColorsLookup.List[i]).LifeTime<lifeTime then k:=i;
            if k<n then Inc(k);
         end else k:=0;
         case k of
            0 : begin
               lck:=TPFXLifeColor(FLifeColorsLookup.List[k]);
               f:=lifeTime*lck.InvLifeTime;
               VectorLerp(ColorOuter.Color, lck.ColorOuter.Color, f, outer);
            end;
         else
            lck:=TPFXLifeColor(FLifeColorsLookup.List[k]);
            lck1:=TPFXLifeColor(FLifeColorsLookup.List[k-1]);
            f:=(lifeTime-lck1.LifeTime)*lck1.InvIntervalRatio;
            VectorLerp(lck1.ColorOuter.Color, lck.ColorOuter.Color, f, outer);
         end;
      end;
   end;
end;

// ComputeSizeScale
//
function TGLLifeColoredPFXManager.ComputeSizeScale(var lifeTime : Single; var sizeScale : Single) : Boolean;
var
   i, k, n : Integer;
   f : Single;
   lck, lck1 : TPFXLifeColor;
begin
   with LifeColors do begin
      n:=Count-1;
      if n<0 then
         Result:=False
      else begin
         if n>0 then begin
            k:=-1;
            for i:=0 to n do
               if TPFXLifeColor(FLifeColorsLookup.List[i]).LifeTime<lifeTime then k:=i;
            if k<n then Inc(k);
         end else k:=0;
         case k of
            0 : begin
               lck:=TPFXLifeColor(FLifeColorsLookup.List[k]);
               Result:=lck.FDoScale;
               if Result then begin
                  f:=lifeTime*lck.InvLifeTime;
                  sizeScale:=Lerp(1, lck.SizeScale, f);
               end;
            end;
         else
            lck:=TPFXLifeColor(FLifeColorsLookup.List[k]);
            lck1:=TPFXLifeColor(FLifeColorsLookup.List[k-1]);
            Result:=lck.FDoScale or lck1.FDoScale;
            if Result then begin
               f:=(lifeTime-lck1.LifeTime)*lck1.InvIntervalRatio;
               sizeScale:=Lerp(lck1.SizeScale, lck.SizeScale, f);
            end;
         end;
      end;
   end;
end;

// ComputeRotateAngle
//
function TGLLifeColoredPFXManager.ComputeRotateAngle(var lifeTime : Single; var rotateAngle : Single) : Boolean;
var
   i, k, n : Integer;
   f : Single;
   lck, lck1 : TPFXLifeColor;
begin
   with LifeColors do begin
      n:=Count-1;
      if n<0 then
         Result:=False
      else begin
         if n>0 then begin
            k:=-1;
            for i:=0 to n do
               if Items[i].LifeTime<lifeTime then k:=i;
            if k<n then Inc(k);
         end else k:=0;
         case k of
            0 : begin
               lck:=LifeColors[k];
               Result:=lck.FDoRotate;
               if Result then
               begin
                  f := lifeTime*lck.InvLifeTime;
                  rotateAngle := Lerp(1, lck.rotateAngle, f);
               end;
            end;
         else
            lck:=LifeColors[k];
            lck1:=LifeColors[k-1];
            Result:=lck.FDoScale or lck1.FDoRotate;
            if Result then
            begin
               f:=(lifeTime-lck1.LifeTime)*lck1.InvIntervalRatio;
               rotateAngle := Lerp(lck1.rotateAngle, lck.rotateAngle, f);
            end;
         end;
      end;
   end;
end;

// RotateVertexBuf
//
procedure TGLLifeColoredPFXManager.RotateVertexBuf(buf : TAffineVectorList;
               lifeTime : Single; const pos : TAffineVector);
var
   rotateAngle : Single;
   axis, p : TAffineVector;
   rotMatrix : TMatrix;
begin
   if ComputeRotateAngle(lifeTime, rotateAngle) then begin
      MakeVector(axis, 0, 0, 1);
      axis := VectorTransform(axis, Renderer.Scene.CurrentBuffer.ModelViewMatrix);
      NormalizeVector(axis);

      // code below probably does it in the slowest fashion possible
      rotMatrix := CreateRotationMatrix(axis, rotateAngle*c180divPI);
      p[0] := -pos[0];
      p[1] := -pos[1];
      p[2] := -pos[2];
      buf.Translate(p);
      buf.TransformAsVectors(rotMatrix);
      buf.Translate(pos);
   end;
end;

// ------------------
// ------------------ TGLCustomPFXManager ------------------
// ------------------

// DoProgress
//
procedure TGLCustomPFXManager.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
   list : PGLParticleArray;
   curParticle : TGLParticle;
   defaultProgress, killParticle, doPack : Boolean;
begin
   if Assigned(FOnProgress) then begin
      defaultProgress:=False;
      FOnProgress(Self, progressTime, defaultProgress);
      if defaultProgress then
         inherited;
   end else inherited;
   if Assigned(FOnParticleProgress) then begin
      doPack:=False;
      list:=Particles.List;
      for i:=0 to Particles.ItemCount-1 do begin
         killParticle:=True;
         curParticle:=list[i];
         FOnParticleProgress(Self, progressTime, curParticle, killParticle);
         if killParticle then begin
            curParticle.Free;
            list[i]:=nil;
            doPack:=True;
         end;
      end;
      if doPack then
         Particles.Pack;
   end;
end;

// TexturingMode
//
function TGLCustomPFXManager.TexturingMode : Cardinal;
begin
   Result:=0;
end;

// InitializeRendering
//
procedure TGLCustomPFXManager.InitializeRendering;
begin
   inherited;
   if Assigned(FOnInitializeRendering) then
      FOnInitializeRendering(Self, Renderer.CurrentRCI^);
end;

// BeginParticles
//
procedure TGLCustomPFXManager.BeginParticles;
begin
   if Assigned(FOnBeginParticles) then
      FOnBeginParticles(Self, Renderer.CurrentRCI^);
end;

// RenderParticle
//
procedure TGLCustomPFXManager.RenderParticle(aParticle : TGLParticle);
begin
   if Assigned(FOnRenderParticle) then
      FOnRenderParticle(Self, aParticle, Renderer.CurrentRCI^);
end;

// EndParticles
//
procedure TGLCustomPFXManager.EndParticles;
begin
   if Assigned(FOnEndParticles) then
      FOnEndParticles(Self, Renderer.CurrentRCI^);
end;

// FinalizeRendering
//
procedure TGLCustomPFXManager.FinalizeRendering;
begin
   if Assigned(FOnFinalizeRendering) then
      FOnFinalizeRendering(Self, Renderer.CurrentRCI^);
   inherited;
end;

// ParticleCount
//
function TGLCustomPFXManager.ParticleCount : Integer;
begin
   if Assigned(FOnGetParticleCountEvent) then
      Result:=FOnGetParticleCountEvent(Self)
   else Result:=FParticles.FItemList.Count;
end;

// ------------------
// ------------------ TGLPolygonPFXManager ------------------
// ------------------

// Create
//
constructor TGLPolygonPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FNbSides:=6;
end;

// Destroy
//
destructor TGLPolygonPFXManager.Destroy;
begin
   inherited Destroy;
end;

// SetNbSides
//
procedure TGLPolygonPFXManager.SetNbSides(const val : Integer);
begin
   if val<>FNbSides then begin
      FNbSides:=val;
      if FNbSides<3 then FNbSides:=3;
      NotifyChange(Self);
   end;
end;

// TexturingMode
//
function TGLPolygonPFXManager.TexturingMode : Cardinal;
begin
   Result:=0;
end;

// InitializeRendering
//
procedure TGLPolygonPFXManager.InitializeRendering;
var
   i : Integer;
   matrix : TMatrix;
   s, c : Single;
begin
   inherited;
   glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);
   for i:=0 to 2 do begin
      Fvx[i]:=matrix[i][0]*FParticleSize;
      Fvy[i]:=matrix[i][1]*FParticleSize;
   end;
   FVertices:=TAffineVectorList.Create;
   FVertices.Capacity:=FNbSides;
   for i:=0 to FNbSides-1 do begin
      SinCos(i*c2PI/FNbSides, s, c);
      FVertices.Add(VectorCombine(FVx, Fvy, c, s));
   end;
   FVertBuf:=TAffineVectorList.Create;
   FVertBuf.Count:=FVertices.Count;
end;

// BeginParticles
//
procedure TGLPolygonPFXManager.BeginParticles;
begin
   ApplyBlendingMode;
end;

// RenderParticle
//
procedure TGLPolygonPFXManager.RenderParticle(aParticle : TGLParticle);
var
   i : Integer;
   lifeTime, sizeScale : Single;
   inner, outer : TColorVector;
   pos : TAffineVector;
   vertexList : PAffineVectorArray;
begin
   lifeTime:=FCurrentTime-aParticle.CreationTime;
   ComputeColors(lifeTime, inner, outer);

   pos:=aParticle.Position;

   vertexList:=FVertBuf.List;

   if aParticle.FEffectScale<>1 then begin
      for i:=0 to FVertBuf.Count-1 do
         VectorAdd(VectorScale(FVertices.List[i], aParticle.FEffectScale), pos, vertexList[i])
   end else VectorArrayAdd(FVertices.List, pos, FVertBuf.Count, vertexList);

   if FLifeRotations then
      RotateVertexBuf(FVertBuf, lifeTime, pos);

   if ComputeSizeScale(lifeTime, sizeScale) then
      FVertBuf.Scale(sizeScale);

   glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@inner);
      glVertex3fv(@pos);
      glColor4fv(@outer);
      for i:=0 to FVertBuf.Count-1 do
         glVertex3fv(@vertexList[i]);

      glVertex3fv(@vertexList[0]);
   glEnd;                 
end;

// EndParticles
//
procedure TGLPolygonPFXManager.EndParticles;
begin
   UnapplyBlendingMode;
end;

// FinalizeRendering
//
procedure TGLPolygonPFXManager.FinalizeRendering;
begin
   FVertBuf.Free;
   FVertices.Free;
   inherited;
end;

// ------------------
// ------------------ TGLBaseSpritePFXManager ------------------
// ------------------

// Create
//
constructor TGLBaseSpritePFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FTexHandle:=TGLTextureHandle.Create;
   FSpritesPerTexture:=sptOne;
   FAspectRatio:=1;
end;

// Destroy
//
destructor TGLBaseSpritePFXManager.Destroy;
begin
   FTexHandle.Free;
   FShareSprites:=nil;
   inherited Destroy;
end;

// SetSpritesPerTexture
//
procedure TGLBaseSpritePFXManager.SetSpritesPerTexture(const val : TSpritesPerTexture);
begin
   if val<>FSpritesPerTexture then begin
      FSpritesPerTexture:=val;
      FTexHandle.DestroyHandle;
      NotifyChange(Self);
   end;
end;

// SetColorMode
//
procedure TGLBaseSpritePFXManager.SetColorMode(const val : TSpriteColorMode);
begin
   if val<>FColorMode then begin
      FColorMode:=val;
      NotifyChange(Self);
   end;
end;

// SetAspectRatio
//
procedure TGLBaseSpritePFXManager.SetAspectRatio(const val : Single);
begin
   if FAspectRatio<>val then begin
      FAspectRatio:=ClampValue(val, 1e-3, 1e3);
      NotifyChange(Self);
   end;
end;

// StoreAspectRatio
//
function TGLBaseSpritePFXManager.StoreAspectRatio : Boolean;
begin
   Result:=(FAspectRatio<>1);
end;

// SetRotation
//
procedure TGLBaseSpritePFXManager.SetRotation(const val : Single);
begin
   if FRotation<>val then begin
      FRotation:=val;
      NotifyChange(Self);
   end;
end;

// SetShareSprites
//
procedure TGLBaseSpritePFXManager.SetShareSprites(const val : TGLBaseSpritePFXManager);
begin
   if FShareSprites<>val then begin
      if Assigned(FShareSprites) then
         FShareSprites.RemoveFreeNotification(Self);
      FShareSprites:=val;
      if Assigned(FShareSprites) then
         FShareSprites.FreeNotification(Self);
   end;
end;

// BindTexture
//
procedure TGLBaseSpritePFXManager.BindTexture;
var
   bmp32 : TGLBitmap32;
   tw, th, tf : Integer;
begin
   if Assigned(FShareSprites) then
      FShareSprites.BindTexture
   else begin
      if FTexHandle.Handle=0 then begin
         FTexHandle.AllocateHandle;
         glBindTexture(GL_TEXTURE_2D, FTexHandle.Handle);

         glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
         glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
         glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
         glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
         glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

         glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
         glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

         bmp32:=TGLBitmap32.Create;
         try
            tf:=GL_RGBA;
            PrepareImage(bmp32, tf);
            tw:=bmp32.Width;
            th:=bmp32.Height;
            bmp32.RegisterAsOpenGLTexture(GL_TEXTURE_2D, miLinearMipmapLinear,
                                          tf, tw, th);
         finally
            bmp32.Free;
         end;
      end else begin
         Renderer.CurrentRCI.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, FTexHandle.Handle);
      end;
   end;
end;

// TexturingMode
//
function TGLBaseSpritePFXManager.TexturingMode : Cardinal;
begin
   Result:=GL_TEXTURE_2D;
end;

// InitializeRendering
//
procedure TGLBaseSpritePFXManager.InitializeRendering;
var
   i : Integer;
   matrix : TMatrix;
   s, c, w, h : Single;
begin
   inherited;
   glGetFloatv(GL_MODELVIEW_MATRIX, @matrix);

   w:=FParticleSize*Sqrt(FAspectRatio);
   h:=Sqr(FParticleSize)/w;

   for i:=0 to 2 do begin
      Fvx[i]:=matrix[i][0]*w;
      Fvy[i]:=matrix[i][1]*h;
      Fvz[i]:=matrix[i][2];
   end;

   FVertices:=TAffineVectorList.Create;
   for i:=0 to 3 do begin
      SinCos(i*cPIdiv2+cPIdiv4, s, c);
      FVertices.Add(VectorCombine(Fvx, Fvy, c, s));
   end;
   if FRotation<>0 then begin
      matrix:=CreateRotationMatrix(Fvz, -FRotation);
      FVertices.TransformAsPoints(matrix);
   end;

   FVertBuf:=TAffineVectorList.Create;
   FVertBuf.Count:=FVertices.Count;
end;

// BeginParticles
//
procedure TGLBaseSpritePFXManager.BeginParticles;
begin
   BindTexture;
   if ColorMode=scmNone then
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)
   else glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   ApplyBlendingMode;
   //if ColorMode<>scmFade then
   //   glBegin(GL_QUADS);
end;

// RenderParticle
//
procedure TGLBaseSpritePFXManager.RenderParticle(aParticle : TGLParticle);
type
   TTexCoordsSet = array [0..3] of TTexPoint;
   PTexCoordsSet = ^TTexCoordsSet;
const
   cBaseTexCoordsSet : TTexCoordsSet = ((S: 1; T: 1), (S: 0; T: 1), (S: 0; T: 0), (S: 1; T: 0));
   cTexCoordsSets : array [0..3] of TTexCoordsSet =
                     ( ((S: 1.0; T: 1.0), (S: 0.5; T: 1.0), (S: 0.5; T: 0.5), (S: 1.0; T: 0.5)),
                       ((S: 0.5; T: 1.0), (S: 0.0; T: 1.0), (S: 0.0; T: 0.5), (S: 0.5; T: 0.5)),
                       ((S: 1.0; T: 0.5), (S: 0.5; T: 0.5), (S: 0.5; T: 0.0), (S: 1.0; T: 0.0)),
                       ((S: 0.5; T: 0.5), (S: 0.0; T: 0.5), (S: 0.0; T: 0.0), (S: 0.5; T: 0.0)));
var
   lifeTime, sizeScale : Single;
   inner, outer : TColorVector;
   vertexList : PAffineVectorArray;
   i : Integer;
   tcs : PTexCoordsSet;
   spt : TSpritesPerTexture;
   v: TAffineVector;

   procedure IssueVertices(tcs : PTexCoordsSet; vertexList : PAffineVectorArray);
   begin
      glTexCoord2fv(@tcs[0]);
      glVertex3fv(@vertexList[0]);
      glTexCoord2fv(@tcs[1]);
      glVertex3fv(@vertexList[1]);
      glTexCoord2fv(@tcs[2]);
      glVertex3fv(@vertexList[2]);
      glTexCoord2fv(@tcs[3]);
      glVertex3fv(@vertexList[3]);
   end;

begin
   lifeTime:=FCurrentTime-aParticle.CreationTime;

   if Assigned(ShareSprites) then
      spt:=ShareSprites.SpritesPerTexture
   else spt:=SpritesPerTexture;
   case spt of
      sptFour : tcs:=@cTexCoordsSets[(aParticle.ID and 3)];
   else
      tcs:=@cBaseTexCoordsSet;
   end;

   vertexList:=FVertBuf.List;
   if aParticle.FEffectScale<>1 then begin
      if ComputeSizeScale(lifeTime, sizeScale) then
         sizeScale:=sizeScale*aParticle.FEffectScale
      else sizeScale:=aParticle.FEffectScale;
      for i:=0 to FVertBuf.Count-1 do
         vertexList[i]:=VectorCombine(FVertices.List[i], aParticle.Position, sizeScale, 1);
   end else begin
      if ComputeSizeScale(lifeTime, sizeScale) then begin
         for i:=0 to FVertBuf.Count-1 do
            vertexList[i]:=VectorCombine(FVertices.List[i], aParticle.Position, sizeScale, 1);
      end else VectorArrayAdd(FVertices.List, aParticle.Position, FVertBuf.Count, vertexList);
   end;

   if FLifeRotations then
      RotateVertexBuf(FVertBuf, lifeTime, aParticle.Position);
   
   glPointSize(5.0);
   glBegin(GL_POINTS);
   glVertex3fv(@aParticle.Position);
   glEnd();
   glPointSize(1.0);
   
   {
   case ColorMode of
      scmFade : begin
         ComputeColors(lifeTime, inner, outer);
         glBegin(GL_TRIANGLE_FAN);
            glColor4fv(@inner);
            glTexCoord2f((tcs[0].S+tcs[2].S)*0.5, (tcs[0].T+tcs[2].T)*0.5);
            glVertex3fv(@aParticle.Position);
            
            glColor4fv(@outer);
            IssueVertices(tcs, vertexList);

            glTexCoord2fv(@tcs[0]);
            glVertex3fv(@vertexList[0]);
         glEnd;
      end;
      scmInner : begin
         ComputeInnerColor(lifeTime, inner);
         glColor4fv(@inner);
         IssueVertices(tcs, vertexList);
      end;
      scmOuter : begin
         ComputeOuterColor(lifeTime, outer);
         glColor4fv(@outer);
         IssueVertices(tcs, vertexList);
      end;
      scmNone : begin
         IssueVertices(tcs, vertexList);
      end;
   else
      Assert(False);
   end;
    }            
end;

// EndParticles
//
procedure TGLBaseSpritePFXManager.EndParticles;
begin
   //if ColorMode<>scmFade then
   //   glEnd;
   UnApplyBlendingMode;
end;

// FinalizeRendering
//
procedure TGLBaseSpritePFXManager.FinalizeRendering;
begin
   FVertBuf.Free;
   FVertices.Free;
   inherited;
end;

// ------------------
// ------------------ TGLCustomSpritePFXManager ------------------
// ------------------

// Create
//
constructor TGLCustomSpritePFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FColorMode:=scmInner;
   FSpritesPerTexture:=sptOne;
end;

// Destroy
//
destructor TGLCustomSpritePFXManager.Destroy;
begin
   inherited Destroy;
end;

// BindTexture
//
procedure TGLCustomSpritePFXManager.PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer);
begin
   if Assigned(FOnPrepareTextureImage) then
      FOnPrepareTextureImage(Self, bmp32, texFormat);
end;

// ------------------
// ------------------ TGLPointLightPFXManager ------------------
// ------------------

// Create
//
constructor TGLPointLightPFXManager.Create(aOwner : TComponent);
begin
   inherited;
   FTexMapSize:=5;
   FColorMode:=scmInner;
end;

// Destroy
//
destructor TGLPointLightPFXManager.Destroy;
begin
   inherited Destroy;
end;

// SetTexMapSize
//
procedure TGLPointLightPFXManager.SetTexMapSize(const val : Integer);
begin
   if val<>FTexMapSize then begin
      FTexMapSize:=val;
      if FTexMapSize<3 then FTexMapSize:=3;
      if FTexMapSize>9 then FTexMapSize:=9;
      NotifyChange(Self);
   end;
end;

// BindTexture
//
procedure TGLPointLightPFXManager.PrepareImage(bmp32 : TGLBitmap32; var texFormat : Integer);
var
   s : Integer;
   x, y, d, h2 : Integer;
   ih2, f, fy : Single;
   scanLine1, scanLine2 : PGLPixel32Array;
begin
   s:=(1 shl TexMapSize);
   bmp32.Width:=s;
   bmp32.Height:=s;
   texFormat:=GL_LUMINANCE_ALPHA;

   h2:=s div 2;
   ih2:=1/h2;
   for y:=0 to h2-1 do begin
      fy:=Sqr((y+0.5-h2)*ih2);
      scanLine1:=bmp32.ScanLine[y];
      scanLine2:=bmp32.ScanLine[s-1-y];
      for x:=0 to h2-1 do begin
         f:=Sqr((x+0.5-h2)*ih2)+fy;
         if f<1 then begin
            d:=Trunc((1-Sqrt(f))*256);
            d:=d+(d shl 8)+(d shl 16)+(d shl 24);
         end else d:=0;
         PInteger(@scanLine1[x])^:=d;
         PInteger(@scanLine2[x])^:=d;
         PInteger(@scanLine1[s-1-x])^:=d;
         PInteger(@scanLine2[s-1-x])^:=d;
      end;
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
   RegisterClasses([TGLParticle, TGLParticleList,
                    TGLParticleFXEffect, TGLParticleFXRenderer,
                    TGLCustomPFXManager,
                    TGLPolygonPFXManager,
                    TGLCustomSpritePFXManager,
                    TGLPointLightPFXManager]);
   RegisterXCollectionItemClass(TGLSourcePFXEffect);

end.
