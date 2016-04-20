{: GLContext<p>

   Prototypes and base implementation of TGLContext.<p>
   Currently NOT thread-safe.<p>

   <b>History : </b><font size=-1><ul>
      <li>25/04/04 - EG - Added TGLOcclusionQueryHandle.Active 
      <li>25/09/03 - EG - Added TGLVBOHandle
      <li>20/09/03 - EG - Added TGLOcclusionQueryHandle
      <li>30/01/02 - EG - Added TGLVirtualHandle
      <li>29/01/02 - EG - Improved recovery for context creation failures
      <li>28/01/02 - EG - Activation failures always ignored
      <li>21/01/02 - EG - Activation failures now ignored if application is
                          terminating (workaround for some weird ICDs)
      <li>15/12/01 - EG - Added support for AlphaBits
      <li>30/11/01 - EG - Added TGLContextAcceleration
      <li>06/09/01 - EG - Win32Context moved to new GLWin32Context unit
      <li>04/09/01 - EG - Added ChangeIAttrib, support for 16bits depth buffer
      <li>25/08/01 - EG - Added pbuffer support and CreateMemoryContext interface
      <li>24/08/01 - EG - Fixed PropagateSharedContext
      <li>12/08/01 - EG - Handles management completed
      <li>22/07/01 - EG - Creation (glcontext.omm)
   </ul></font>
}
unit GLContext;

interface

uses Classes, SysUtils, OpenGL1x, VectorGeometry;

{$i GLScene.inc}

type

   // TGLRCOptions
   //
   TGLRCOption = ( rcoDoubleBuffered, rcoStereo );
   TGLRCOptions = set of TGLRCOption;

   TGLContextManager = class;

   // TGLContextAcceleration
   //
   TGLContextAcceleration = (chaUnknown, chaHardware, chaSoftware);

   // TGLAntiAliasing
   //
   TGLAntiAliasing = (aaDefault, aaNone, aa2x, aa2xHQ, aa4x, aa4xHQ);

   // TGLContext
   //
   {: Wrapper around an OpenGL rendering context.<p>
      The aim of this class is to offer platform-independant
      initialization, activation and management of OpenGL
      rendering context. The class also offers notifications
      event and error/problems detection.<br>
      This is a virtual abstract a class, and platform-specific
      subclasses must be used.<br>
      All rendering context share the same lists. }
   TGLContext = class
      private
         { Private Declarations }
         FColorBits, FAlphaBits : Integer;
         FDepthBits : Integer;
         FStencilBits : Integer;
         FAccumBits : Integer;
         FAuxBuffers : Integer;
         FAntiAliasing : TGLAntiAliasing;
         FOptions : TGLRCOptions;
         FOnDestroyContext : TNotifyEvent;
         FManager : TGLContextManager;
         FActivationCount : Integer;
         FSharedContexts : TList;
         FOwnedHandles : TList;

      protected
         { Protected Declarations }
         FAcceleration : TGLContextAcceleration;

         procedure SetColorBits(const aColorBits : Integer);
         procedure SetAlphaBits(const aAlphaBits : Integer);
         procedure SetDepthBits(const val : Integer);
         procedure SetStencilBits(const aStencilBits : Integer);
         procedure SetAccumBits(const aAccumBits : Integer);
         procedure SetAuxBuffers(const aAuxBuffers : Integer);
         procedure SetOptions(const aOptions : TGLRCOptions);
         procedure SetAntiAliasing(const val : TGLAntiAliasing);
         function  GetActive : Boolean;
         procedure SetActive(const aActive : Boolean);
         procedure PropagateSharedContext;

         procedure DoCreateContext(outputDevice : Integer); dynamic; abstract;
         procedure DoCreateMemoryContext(outputDevice, width, height : Integer); dynamic; abstract;
         procedure DoShareLists(aContext : TGLContext); dynamic; abstract;
         procedure DoDestroyContext; dynamic; abstract;
         procedure DoActivate; virtual; abstract;
         procedure DoDeactivate; virtual; abstract;

      public
         { Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         //: Context manager reference
         property Manager : TGLContextManager read FManager;

         {: Color bits for the rendering context }
         property ColorBits : Integer read FColorBits write SetColorBits;
         {: Alpha bits for the rendering context }
         property AlphaBits : Integer read FAlphaBits write SetAlphaBits;
         {: Depth bits for the rendering context }
         property DepthBits : Integer read FDepthBits write SetDepthBits;
         {: Stencil bits for the rendering context }
         property StencilBits : Integer read FStencilBits write SetStencilBits;
         {: Accumulation buffer bits for the rendering context }
         property AccumBits : Integer read FAccumBits write SetAccumBits;
         {: Auxiliary buffers bits for the rendering context }
         property AuxBuffers : Integer read FAuxBuffers write SetAuxBuffers;
         {: AntiAliasing option.<p>
            Ignored if not hardware supported, currently based on ARB_multisample. }
         property AntiAliasing : TGLAntiAliasing read FAntiAliasing write SetAntiAliasing;
         {: Rendering context options. }
         property Options : TGLRCOptions read FOptions write SetOptions;
         {: Allows reading and defining the activity for the context.<p>
            The methods of this property are just wrappers around calls
            to Activate and Deactivate. }
         property Active : Boolean read GetActive write SetActive;
         {: Indicates if the context is hardware-accelerated. }
         property Acceleration : TGLContextAcceleration read FAcceleration;
         {: Triggered whenever the context is destroyed.<p>
            This events happens *before* the context has been
            actually destroyed, OpenGL resource cleanup can
            still occur here. }
         property OnDestroyContext : TNotifyEvent read FOnDestroyContext write FOnDestroyContext;

         {: Creates the context.<p>
            This method must be invoked before the context can be used. }
         procedure CreateContext(outputDevice : Integer);
         {: Creates an in-memory context.<p>
            The function should fail if no hardware-accelerated memory context
            can be created (the CreateContext method can handle software OpenGL
            contexts). }
         procedure CreateMemoryContext(outputDevice, width, height : Integer);
         {: Setup display list sharing between two rendering contexts.<p>
            Both contexts must have the same pixel format. }
         procedure ShareLists(aContext : TGLContext);
         {: Destroy the context.<p>
            Will fail if no context has been created.<br>
            The method will first invoke the OnDestroyContext
            event, then attempts to deactivate the context
            (if it is active) before destroying it. }
         procedure DestroyContext;
         {: Activates the context.<p>
            A context can be activated multiple times (and must be
            deactivated the same number of times), but this function
            will fail if another context is already active. }
         procedure Activate;
         {: Deactivates the context.<p>
            Will fail if the context is not active or another
            context has been activated. }
         procedure Deactivate;
         {: Returns true if the context is valid.<p>
            A context is valid from the time it has been successfully
            created to the time of its destruction. }
         function IsValid : Boolean; virtual; abstract;
         {: Request to swap front and back buffers if they were defined. }
         procedure SwapBuffers; virtual; abstract;

         {: Returns the first compatible context that isn't self in the shares. }
         function FindCompatibleContext : TGLContext;
         procedure DestroyAllHandles;

         function RenderOutputDevice : Integer; virtual; abstract;
   end;

   TGLContextClass = class of TGLContext;

   // TGLScreenControlingContext
   //
   {: A TGLContext with screen control property and methods.<p>
      This variety of contexts is for drivers that access windows and OpenGL
      through an intermediate opaque cross-platform API.<p>
      TGLSceneViewer won't use them, TGLMemoryViewer may be able to use them,
      but most of the time they will be accessed through a specific viewer
      class/subclass. } 
   TGLScreenControlingContext = class (TGLContext)
      private
         { Private Declarations }
         FWidth, FHeight : Integer;
         FFullScreen : Boolean;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         property Width : Integer read FWidth write FWidth;
         property Height : Integer read FHeight write FHeight;
         property FullScreen : Boolean read FFullScreen write FFullScreen;
   end;

   // TGLContextHandle
   //
   {: Wrapper around an OpenGL context handle.<p>
      This wrapper also takes care of context registrations and data releases
      related to context releases an cleanups. This is an abstract class,
      use the TGLListHandle and TGLTextureHandle subclasses. }
   TGLContextHandle = class
      private
         { Private Declarations }
         FRenderingContext : TGLContext;
         FHandle : Integer;

      protected
         { Protected Declarations }
         //: Invoked by when there is no compatible context left for relocation
         procedure ContextDestroying;

         //: Specifies if the handle can be transfered across shared contexts
         class function Transferable : Boolean; virtual;

         function DoAllocateHandle : Integer; virtual; abstract;
         procedure DoDestroyHandle; virtual; abstract;

      public
         { Public Declarations }
         constructor Create; virtual;
         constructor CreateAndAllocate(failIfAllocationFailed : Boolean = True);
         destructor Destroy; override;

         property Handle : Integer read FHandle;
         property RenderingContext : TGLContext read FRenderingContext;

         procedure AllocateHandle;
         procedure DestroyHandle;
   end;

   TGLVirtualHandle = class;
   TGLVirtualHandleEvent = procedure (sender : TGLVirtualHandle; var handle : Integer) of object;

   // TGLVirtualHandle
   //
   {: A context handle with event-based handle allocation and destruction. }
   TGLVirtualHandle = class (TGLContextHandle)
      private
         { Private Declarations }
         FOnAllocate, FOnDestroy : TGLVirtualHandleEvent;
         FTag : Integer;

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         property OnAllocate : TGLVirtualHandleEvent read FOnAllocate write FOnAllocate;
         property OnDestroy : TGLVirtualHandleEvent read FOnDestroy write FOnDestroy;

         property Tag : Integer read FTag write FTag;
   end;

   // TGLListHandle
   //
   {: Manages a handle to a display list. } 
   TGLListHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         procedure NewList(mode : Cardinal);
         procedure EndList;
         procedure CallList;
   end;

   // TGLTextureHandle
   //
   {: Manages a handle to a texture. } 
   TGLTextureHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
   end;

   // TGLOcclusionQueryHandle
   //
   {: Manages a handle to an NV_occlusion_query.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user. }
   TGLOcclusionQueryHandle = class (TGLContextHandle)
      private
         { Private Declarations }
         FActive : Boolean;

      protected
         { Protected Declarations }
         class function Transferable : Boolean; override;
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         procedure BeginOcclusionQuery;
         procedure EndOcclusionQuery;

         {: True if within a Begin/EndOcclusionQuery. }
         property Active : Boolean read FActive;
         function PixelCount : Integer;
   end;

   // TGLVBOHandle
   //
   {: Manages a handle to an Vertex Buffer Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br>
      Do not use this class directly, use one of its subclasses instead. }
   TGLVBOHandle = class (TGLContextHandle)
      private
         { Private Declarations }
         FVBOTarget : TGLuint;

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;
         procedure DoDestroyHandle; override;

      public
         { Public Declarations }
         {: Creates the VBO buffer and initializes it. }
         constructor CreateFromData(p : Pointer; size : Integer; bufferUsage : TGLuint);

         procedure Bind;
         {: Note that it is not necessary to UnBind before Binding another buffer. }
         procedure UnBind;
         {: Specifies buffer content.<p>
            Common bufferUsage values are GL_STATIC_DRAW_ARB for data that will
            change rarely, but be used often, GL_STREAM_DRAW_ARB for data specified
            once but used only a few times, and GL_DYNAMIC_DRAW_ARB for data
            that is re-specified very often.<p>
            Valid only if the buffer has been bound. }
         procedure BufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
         //: Invokes Bind then BufferData
         procedure BindBufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
         {: Updates part of an already existing buffer.<p>
            offset and size indicate which part of the data in the buffer is
            to bo modified and p where the data should be taken from. }
         procedure BufferSubData(offset, size : Integer; p : Pointer);
         {: Map buffer content to memory.<p>
            Values for access are GL_READ_ONLY_ARB, GL_WRITE_ONLY_ARB and
            GL_READ_WRITE_ARB.<p>
            Valid only if the buffer has been bound, must be followed by
            an UnmapBuffer, only one buffer may be mapped at a time. }
         function MapBuffer(access : TGLuint) : Pointer;
         {: Unmap buffer content from memory.<p>
            Must follow a MapBuffer, and happen before the buffer is unbound. }
         function UnmapBuffer : Boolean;

         property VBOTarget : TGLuint read FVBOTarget;
   end;

   // TGLVBOArrayBufferHandle
   //
   {: Manages a handle to VBO Array Buffer.<p>
      Typically used to store vertices, normals, texcoords, etc. }
   TGLVBOArrayBufferHandle = class (TGLVBOHandle)
      public
         { Public Declarations }
         constructor Create; override;
   end;

   // TGLVBOElementArrayHandle
   //
   {: Manages a handle to VBO Element Array Buffer.<p>
      Typically used to store vertex indices. }
   TGLVBOElementArrayHandle = class (TGLVBOHandle)
      public
         { Public Declarations }
         constructor Create; override;
   end;

   // TGLSLHandle
   //
   {: Base class for GLSL handles (programs and shaders).<p>
      Do not use this class directly, use one of its subclasses instead. }
   TGLSLHandle = class (TGLContextHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         procedure DoDestroyHandle; override;
         
      public
         { Public Declarations }
         function InfoLog : String;
   end;

   // TGLShaderHandle
   //
   {: Manages a handle to a Shader Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br>
      Do not use this class directly, use one of its subclasses instead. }
   TGLShaderHandle = class (TGLSLHandle)
      private
         { Private Declarations }
         FShaderType : Cardinal;

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;

      public
         { Public Declarations }
         procedure ShaderSource(const source : String); overload;
         //: Returns True if compilation sucessful
         function CompileShader : Boolean;

         property ShaderType : Cardinal read FShaderType;
   end;

   TGLShaderHandleClass = class of TGLShaderHandle;

   // TGLVertexShaderHandle
   //
   {: Manages a handle to a Vertex Shader Object. }
   TGLVertexShaderHandle = class (TGLShaderHandle)
      public
         { Public Declarations }
         constructor Create; override;
   end;

   // TGLFragmentShaderHandle
   //
   {: Manages a handle to a Fragment Shader Object. }
   TGLFragmentShaderHandle = class (TGLShaderHandle)
      public
         { Public Declarations }
         constructor Create; override;
   end;

   // TGLProgramHandle
   //
   {: Manages a GLSL Program Object.<br>
      Does *NOT* check for extension availability, this is assumed to have been
      checked by the user.<br> }
   TGLProgramHandle = class (TGLSLHandle)
      private
         { Private Declarations }

      protected
         { Protected Declarations }
         function DoAllocateHandle : Integer; override;

         function GetUniform1i(const index : String) : Integer;
         procedure SetUniform1i(const index : String; val : Integer);
         function GetUniform1f(const index : String) : Single;
         procedure SetUniform1f(const index : String; val : Single);
         function GetUniform3f(const index : String) : TAffineVector;
         procedure SetUniform3f(const index : String; const val : TAffineVector);
         function GetUniform4f(const index : String) : TVector;
         procedure SetUniform4f(const index : String; const val : TVector);
         function GetUniformMatrix4fv(const index : String) : TMatrix;
         procedure SetUniformMatrix4fv(const index : String; const val : TMatrix);

      public
         { Public Declarations }
         {: Compile and attach a new shader.<p>
            Raises an EGLShader exception in case of failure. }
         procedure AddShader(shaderType : TGLShaderHandleClass; const shaderSource : String;
                             treatWarningsAsErrors : Boolean = False);

         procedure AttachObject(shader : TGLShaderHandle);
         procedure BindAttribLocation(index : Integer; const name : String);
         function LinkProgram : Boolean;
         function ValidateProgram : Boolean;
         function GetAttribLocation(const name : String) : Integer;
         function GetUniformLocation(const name : String) : Integer;
         procedure UseProgramObject;
         procedure EndUseProgramObject;

         property Uniform1i[const index : String] : Integer read GetUniform1i write SetUniform1i;
         property Uniform1f[const index : String] : Single read GetUniform1f write SetUniform1f;
         property Uniform3f[const index : String] : TAffineVector read GetUniform3f write SetUniform3f;
         property Uniform4f[const index : String] : TVector read GetUniform4f write SetUniform4f;
         property UniformMatrix4fv[const index : String] : TMatrix read GetUniformMatrix4fv write SetUniformMatrix4fv;
   end;

   // TGLContextNotification
   //
   TGLContextNotification = record
      obj : TObject;
      event : TNotifyEvent;
   end;

   // TGLContextManager
   //
   {: Stores and manages all the TGLContext objects.<p> }
   TGLContextManager = class
      private
         { Private Declarations }
         FList : TThreadList;
         FTerminated : Boolean;
         FNotifications : array of TGLContextNotification;
         FCreatedRCCount : Integer;

      protected
         { Protected Declarations }
         procedure Lock;
         procedure UnLock;

         procedure RegisterContext(aContext : TGLContext);
         procedure UnRegisterContext(aContext : TGLContext);

         procedure ContextCreatedBy(aContext : TGLContext);
         procedure DestroyingContextBy(aContext : TGLContext);

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         {: Returns an appropriate, ready-to use context.<p>
            The returned context should be freed by caller. }
         function CreateContext : TGLContext;

         {: Returns the number of TGLContext object.<p>
            This is *not* the number of OpenGL rendering contexts! }
         function ContextCount : Integer;
         {: Registers a new object to notify when the last context is destroyed.<p>
            When the last rendering context is destroyed, the 'anEvent' will
            be invoked with 'anObject' as parameter.<br>
            Note that the registration is kept until the notification is triggered
            or a RemoveNotification on 'anObject' is issued. }
         procedure LastContextDestroyNotification(anObject : TObject; anEvent : TNotifyEvent);
         {: Unregisters an object from the notification lists.<p> }
         procedure RemoveNotification(anObject : TObject);

         //: Marks the context manager for termination
         procedure Terminate;

         {: Request all contexts to destroy all their handles. }
         procedure DestroyAllHandles;
   end;

   EGLContext = class(Exception);

   EGLShader = class(EGLContext);

   EOpenGLError = class(Exception);

{: Drivers should register themselves via this function. }
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);
{: The TGLContext that is the currently active context, if any.<p>
   Returns nil if no context is active. }
function CurrentGLContext : TGLContext;

{: Gets the oldest error from OpenGL engine and tries to clear the error queue.<p> }
procedure CheckOpenGLError;
{: Clears all pending OpenGL errors. }
procedure ClearGLError;
{: Raises an EOpenGLError with 'msg' error string. }
procedure RaiseOpenGLError(const msg : String);

var
   GLContextManager : TGLContextManager;
   vIgnoreOpenGLErrors : Boolean = False;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

resourcestring
   cCannotAlterAnActiveContext = 'Cannot alter an active context';
   cInvalidContextRegistration = 'Invalid context registration';
   cInvalidNotificationRemoval = 'Invalid notification removal';
   cContextAlreadyCreated =      'Context already created';
   cContextNotCreated =          'Context not created';
   cUnbalancedContexActivations= 'Unbalanced context activations';

var
   vContextClasses : TList = nil;
   vIgnoreContextActivationFailures : Boolean = False;

var
   vCurrentGLContext : TGLContext;

// CurrentGLContext
//
function CurrentGLContext : TGLContext;
begin
   Result:=vCurrentGLContext;
end;

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError : LongWord;
	Count : Word;
begin
	GLError:=glGetError;
	if GLError <> GL_NO_ERROR then begin
		Count:=0;
      // Because under some circumstances reading the error code creates a new error
      // and thus hanging up the thread, we limit the loop to 6 reads.
      try
         while (glGetError <> GL_NO_ERROR) and (Count < 6) do Inc(Count);
      except
         // Egg : ignore exceptions here, will perhaps avoid problem expressed before
		end;
      if not vIgnoreOpenGLErrors then
   		raise EOpenGLError.Create(gluErrorString(GLError));
	end;
end;

// ClearGLError
//
procedure ClearGLError;
var
   n : Integer;
begin
   n:=0;
   while (glGetError<>GL_NO_ERROR) and (n<6) do Inc(n);
end;

// RaiseOpenGLError
//
procedure RaiseOpenGLError(const msg : String);
begin
   raise EOpenGLError.Create(msg);
end;

// RegisterGLContextClass
//
procedure RegisterGLContextClass(aGLContextClass : TGLContextClass);
begin
   if not Assigned(vContextClasses) then
      vContextClasses:=TList.Create;
   vContextClasses.Add(aGLContextClass);
end;

// ------------------
// ------------------ TGLContext ------------------
// ------------------

// Create
//
constructor TGLContext.Create;
begin
   inherited Create;
   FColorBits:=32;
   FStencilBits:=0;
   FAccumBits:=0;
   FAuxBuffers:=0;
   FOptions:=[];
   FSharedContexts:=TList.Create;
   FOwnedHandles:=TList.Create;
   FAcceleration:=chaUnknown;
   GLContextManager.RegisterContext(Self);
end;

// Destroy
//
destructor TGLContext.Destroy;
begin
   if IsValid then
      DestroyContext;
   GLContextManager.UnRegisterContext(Self);
   FOwnedHandles.Free;
   FSharedContexts.Free;
   inherited Destroy;
end;

// SetColorBits
//
procedure TGLContext.SetColorBits(const aColorBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FColorBits:=aColorBits;
end;

// SetAlphaBits
//
procedure TGLContext.SetAlphaBits(const aAlphaBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAlphaBits:=aAlphaBits;
end;

// SetDepthBits
//
procedure TGLContext.SetDepthBits(const val : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FDepthBits:=val;
end;

// SetStencilBits
//
procedure TGLContext.SetStencilBits(const aStencilBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FStencilBits:=aStencilBits;
end;

// SetAccumBits
//
procedure TGLContext.SetAccumBits(const aAccumBits : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAccumBits:=aAccumBits;
end;

// SetAuxBuffers
//
procedure TGLContext.SetAuxBuffers(const aAuxBuffers : Integer);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAuxBuffers:=aAuxBuffers;
end;

// SetOptions
//
procedure TGLContext.SetOptions(const aOptions : TGLRCOptions);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FOptions:=aOptions;
end;

// SetAntiAliasing
//
procedure TGLContext.SetAntiAliasing(const val : TGLAntiAliasing);
begin
   if Active then
      raise EGLContext.Create(cCannotAlterAnActiveContext)
   else FAntiAliasing:=val;
end;

// GetActive
//
function TGLContext.GetActive : Boolean;
begin
   Result:=(FActivationCount>0);
end;

// SetActive
//
procedure TGLContext.SetActive(const aActive : Boolean);
begin
   // activation/deactivation can be nested...
   while aActive<>Active do begin
      if aActive then
         Activate
      else Deactivate;
   end;
end;

// CreateContext
//
procedure TGLContext.CreateContext(outputDevice : Integer);
begin
   if IsValid then
      raise EGLContext.Create(cContextAlreadyCreated);
   FAcceleration:=chaUnknown;
   DoCreateContext(outputDevice);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// CreateMemoryContext
//
procedure TGLContext.CreateMemoryContext(outputDevice, width, height : Integer);
begin
   if IsValid then
      raise EGLContext.Create(cContextAlreadyCreated);
   FAcceleration:=chaUnknown;
   DoCreateMemoryContext(outputDevice, width, height);
   FSharedContexts.Add(Self);
   Manager.ContextCreatedBy(Self);
end;

// PropagateSharedContext
//
procedure TGLContext.PropagateSharedContext;
var
   i, j : Integer;
   otherContext : TGLContext;
begin
   for i:=0 to FSharedContexts.Count-1 do begin
      if TGLContext(FSharedContexts[i])<>Self then begin
         otherContext:=TGLContext(FSharedContexts[i]);
         otherContext.FSharedContexts.Clear;
         for j:=0 to FSharedContexts.Count-1 do
            otherContext.FSharedContexts.Add(FSharedContexts[j]);
      end;
   end;
end;

// ShareLists
//
procedure TGLContext.ShareLists(aContext : TGLContext);
begin
   if IsValid then begin
      if FSharedContexts.IndexOf(aContext)<0 then begin
         DoShareLists(aContext);
         FSharedContexts.Add(aContext);
         PropagateSharedContext;
      end;
   end else raise EGLContext.Create(cContextNotCreated);
end;

// DestroyAllHandles
//
procedure TGLContext.DestroyAllHandles;
var
   i : Integer;
begin
   Activate;
   try
      for i:=FOwnedHandles.Count-1 downto 0 do
         TGLContextHandle(FOwnedHandles[i]).DestroyHandle;
   finally
      Deactivate;
   end;
end;

// DestroyContext
//
procedure TGLContext.DestroyContext;
var
   i : Integer;
   oldContext, compatContext : TGLContext;
   contextHandle : TGLContextHandle;
begin
   if vCurrentGLContext<>Self then begin
      oldContext:=vCurrentGLContext;
      if Assigned(oldContext) then
         oldContext.Deactivate;
   end else oldContext:=nil;
   Activate;
   try
      compatContext:=FindCompatibleContext;
      if Assigned(compatContext) then begin
         // transfer handle ownerships to the compat context
         for i:=FOwnedHandles.Count-1 downto 0 do begin
            contextHandle:=TGLContextHandle(FOwnedHandles[i]);
            if contextHandle.Transferable then begin
               compatContext.FOwnedHandles.Add(contextHandle);
               contextHandle.FRenderingContext:=compatContext;
            end else contextHandle.ContextDestroying;
         end;
      end else begin
         // no compat context, release handles
         for i:=FOwnedHandles.Count-1 downto 0 do begin
            contextHandle:=TGLContextHandle(FOwnedHandles[i]);
            contextHandle.ContextDestroying;
         end;
      end;
      FOwnedHandles.Clear;
      Manager.DestroyingContextBy(Self);
      FSharedContexts.Remove(Self);
      PropagateSharedContext;
      FSharedContexts.Clear;
      Active:=False;
      DoDestroyContext;
   finally
      if Assigned(oldContext) then
         oldContext.Activate;
   end;
   vIgnoreContextActivationFailures:=False;
   FAcceleration:=chaUnknown;
end;

// Activate
//
procedure TGLContext.Activate;
begin
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      try
         DoActivate;
      except
//         if True then // ApplicationTerminated?
//            vIgnoreContextActivationFailures:=True
//         else
         raise;
      end;
      vCurrentGLContext:=Self;
   end else Assert(vCurrentGLContext=Self);
   Inc(FActivationCount);
end;

// Deactivate
//
procedure TGLContext.Deactivate;
begin
   Assert(vCurrentGLContext=Self);
   Dec(FActivationCount);
   if FActivationCount=0 then begin
      if not IsValid then
         raise EGLContext.Create(cContextNotCreated);
      if not vIgnoreContextActivationFailures then
         DoDeactivate;
      vCurrentGLContext:=nil;
   end else if FActivationCount<0 then
      raise EGLContext.Create(cUnbalancedContexActivations);
end;

// FindCompatibleContext
//
function TGLContext.FindCompatibleContext : TGLContext;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to FSharedContexts.Count-1 do
      if FSharedContexts[i]<>Self then begin
         Result:=TGLContext(FSharedContexts[i]);
         Break;
      end;
end;

// ------------------
// ------------------ TGLContextHandle ------------------
// ------------------

// Create
//
constructor TGLContextHandle.Create;
begin
   inherited Create;
end;

// CreateAndAllocate
//
constructor TGLContextHandle.CreateAndAllocate(failIfAllocationFailed : Boolean = True);
begin
   Create;
   AllocateHandle;
   if failIfAllocationFailed and (Handle=0) then
      raise EGLContext.Create('Auto-allocation failed');
end;

// Destroy
//
destructor TGLContextHandle.Destroy;
begin
   DestroyHandle;
   inherited Destroy;
end;

// AllocateHandle
//
procedure TGLContextHandle.AllocateHandle;
begin
   Assert(FHandle=0);
   Assert(vCurrentGLContext<>nil);
   FHandle:=DoAllocateHandle;
   if FHandle<>0 then begin
      FRenderingContext:=vCurrentGLContext;
      vCurrentGLContext.FOwnedHandles.Add(Self);
   end;
end;

// DestroyHandle
//
procedure TGLContextHandle.DestroyHandle;
var
   oldContext, handleContext : TGLContext;
begin
   if FHandle<>0 then begin
      FRenderingContext.FOwnedHandles.Remove(Self);
      if (vCurrentGLContext=FRenderingContext)
            or ((vCurrentGLContext<>nil)
                and (vCurrentGLContext.FSharedContexts.IndexOf(FRenderingContext)>=0)) then begin
         // current context is ours or compatible one
         DoDestroyHandle;
      end else begin
         // some other context (or none)
         oldContext:=vCurrentGLContext;
         if Assigned(oldContext) then
            oldContext.Deactivate;
         FRenderingContext.Activate;
         handleContext:=FRenderingContext;
         try
            DoDestroyHandle;
         finally
            handleContext.Deactivate;
            if Assigned(oldContext) then
               oldContext.Activate;
         end;
      end;
      FHandle:=0;
      FRenderingContext:=nil;
   end;
end;

// ContextDestroying
//
procedure TGLContextHandle.ContextDestroying;
begin
   if FHandle<>0 then begin
      // we are always in the original context or a compatible context
      DoDestroyHandle;
      FHandle:=0;
      FRenderingContext:=nil;
   end;
end;

// Transferable
//
class function TGLContextHandle.Transferable : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TGLVirtualHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLVirtualHandle.DoAllocateHandle : Integer;
begin
   Result:=0;
   if Assigned(FOnAllocate) then
      FOnAllocate(Self, Result);
end;

// DoDestroyHandle
//
procedure TGLVirtualHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      ClearGLError;
      // delete
      if Assigned(FOnDestroy) then
         FOnDestroy(Self, FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLListHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLListHandle.DoAllocateHandle : Integer;
begin
   Result:=glGenLists(1);
end;

// DoDestroyHandle
//
procedure TGLListHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      ClearGLError;
      // delete
      glDeleteLists(FHandle, 1);
      // check for error
      CheckOpenGLError;
   end;
end;

// NewList
//
procedure TGLListHandle.NewList(mode : Cardinal);
begin
   glNewList(FHandle, mode);
end;

// EndList
//
procedure TGLListHandle.EndList;
begin
   glEndList;
end;

// CallList
//
procedure TGLListHandle.CallList;
begin
   glCallList(FHandle);
end;

// ------------------
// ------------------ TGLTextureHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLTextureHandle.DoAllocateHandle : Integer;
begin
   glGenTextures(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLTextureHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      glGetError;
      // delete
      if glIsTexture(FHandle) then
    	   glDeleteTextures(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// ------------------
// ------------------ TGLOcclusionQueryHandle ------------------
// ------------------

// Transferable
//
class function TGLOcclusionQueryHandle.Transferable : Boolean;
begin
   Result:=False;
end;

// DoAllocateHandle
//
function TGLOcclusionQueryHandle.DoAllocateHandle : Integer;
begin
   glGenOcclusionQueriesNV(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLOcclusionQueryHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      glGetError;
      // delete
 	   glDeleteOcclusionQueriesNV(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// BeginOcclusionQuery
//
procedure TGLOcclusionQueryHandle.BeginOcclusionQuery;
begin
   Assert(Handle<>0);
   glBeginOcclusionQueryNV(Handle);
   Factive:=True;
end;

// EndOcclusionQuery
//
procedure TGLOcclusionQueryHandle.EndOcclusionQuery;
begin
   Factive:=False;
   Assert(Handle<>0);
   glEndOcclusionQueryNV;
end;

// PixelCount
//
function TGLOcclusionQueryHandle.PixelCount : Integer;
begin
   Assert(FHandle<>0);
   Result:=0;
   glGetOcclusionQueryuivNV(Handle, GL_PIXEL_COUNT_NV, @Result);
end;

// ------------------
// ------------------ TGLVBOHandle ------------------
// ------------------

// CreateFromData
//
constructor TGLVBOHandle.CreateFromData(p : Pointer; size : Integer; bufferUsage : TGLuint);
begin
   Create;
   AllocateHandle;
   Bind;
   BufferData(p, size, bufferUsage);
   UnBind;
end;

// DoAllocateHandle
//
function TGLVBOHandle.DoAllocateHandle : Integer;
begin
   Assert((FVBOTarget=GL_ARRAY_BUFFER_ARB) or (FVBOTarget=GL_ELEMENT_ARRAY_BUFFER_ARB));
   glGenBuffersARB(1, @Result);
end;

// DoDestroyHandle
//
procedure TGLVBOHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      glGetError;
      // delete
 	   glDeleteBuffersARB(1, @FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// BindAsArrayBuffer
//
procedure TGLVBOHandle.Bind;
begin
   glBindBufferARB(FVBOTarget, Handle);
end;

// UnBind
//
procedure TGLVBOHandle.UnBind;
begin
   glBindBufferARB(FVBOTarget, 0);
end;

// BufferData
//
procedure TGLVBOHandle.BufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
begin
   glBufferDataARB(FVBOTarget, size, p, bufferUsage);
end;

// BindBufferData
//
procedure TGLVBOHandle.BindBufferData(p : Pointer; size : Integer; bufferUsage : TGLuint);
begin
   glBindBufferARB(FVBOTarget, Handle);
   glBufferDataARB(FVBOTarget, size, p, bufferUsage);
end;

// BufferSubData
//
procedure TGLVBOHandle.BufferSubData(offset, size : Integer; p : Pointer);
begin
   glBufferSubDataARB(FVBOTarget, offset, size, p);
end;

// MapBuffer
//
function TGLVBOHandle.MapBuffer(access : TGLuint) : Pointer;
begin
   Result:=glMapBufferARB(FVBOTarget, access);
end;

// UnmapBuffer
//
function TGLVBOHandle.UnmapBuffer : Boolean;
begin
   Result:=glUnmapBufferARB(FVBOTarget);
end;

// ------------------
// ------------------ TGLVBOArrayBufferHandle ------------------
// ------------------

// Create
//
constructor TGLVBOArrayBufferHandle.Create;
begin
   FVBOTarget:=GL_ARRAY_BUFFER_ARB;
   inherited;
end;

// ------------------
// ------------------ TGLVBOElementArrayHandle ------------------
// ------------------

// Create
//
constructor TGLVBOElementArrayHandle.Create;
begin
   FVBOTarget:=GL_ELEMENT_ARRAY_BUFFER_ARB;
   inherited;
end;

// ------------------
// ------------------ TGLSLHandle ------------------
// ------------------

// DoDestroyHandle
//
procedure TGLSLHandle.DoDestroyHandle;
begin
   if not vIgnoreContextActivationFailures then begin
      // reset error status
      ClearGLError;
      // delete
      glDeleteObjectARB(FHandle);
      // check for error
      CheckOpenGLError;
   end;
end;

// InfoLog
//
function TGLSLHandle.InfoLog : String;
var
   maxLength : Integer;
begin
   maxLength:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_INFO_LOG_LENGTH_ARB, @maxLength);
   SetLength(Result, maxLength);
   if maxLength>0 then begin
      glGetInfoLogARB(FHandle, maxLength, @maxLength, @Result[1]);
      SetLength(Result, maxLength);
   end;
end;

// ------------------
// ------------------ TGLShaderHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLShaderHandle.DoAllocateHandle : Integer;
begin
   Result:=glCreateShaderObjectARB(FShaderType);
end;

// ShaderSource
//
procedure TGLShaderHandle.ShaderSource(const source : String);
var
   p : PChar;
begin
   p:=PChar(source);
   glShaderSourceARB(FHandle, 1, @p, nil);
end;

// CompileShader
//
function TGLShaderHandle.CompileShader : Boolean;
var
   compiled : Integer;
begin
   glCompileShaderARB(FHandle);
   compiled:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_COMPILE_STATUS_ARB, @compiled);
   Result:=(compiled<>0);
end;

// ------------------
// ------------------ TGLVertexShaderHandle ------------------
// ------------------

// Create
//
constructor TGLVertexShaderHandle.Create;
begin
   FShaderType:=GL_VERTEX_SHADER_ARB;
   inherited;
end;

// ------------------
// ------------------ TGLFragmentShaderHandle ------------------
// ------------------

// Create
//
constructor TGLFragmentShaderHandle.Create;
begin
   FShaderType:=GL_FRAGMENT_SHADER_ARB;
   inherited;
end;

// ------------------
// ------------------ TGLProgramHandle ------------------
// ------------------

// DoAllocateHandle
//
function TGLProgramHandle.DoAllocateHandle : Integer;
begin
   Result:=glCreateProgramObjectARB();
end;

// AddShader
//
procedure TGLProgramHandle.AddShader(shaderType : TGLShaderHandleClass; const shaderSource : String;
                                     treatWarningsAsErrors : Boolean = False);
var
   shader : TGLShaderHandle;
begin
   shader:=shaderType.CreateAndAllocate;
   try
      if shader.Handle=0 then
         raise EGLShader.Create('Couldn''t allocate '+shaderType.ClassName);
      shader.ShaderSource(shaderSource);
      if    (not shader.CompileShader)
         or (treatWarningsAsErrors and (Pos('warning', LowerCase(shader.InfoLog))>0)) then
         raise EGLShader.Create(shader.ClassName+': '+shader.InfoLog);
      AttachObject(shader);
   finally
      shader.Free;
   end;
   CheckOpenGLError;
end;

// AttachObject
//
procedure TGLProgramHandle.AttachObject(shader : TGLShaderHandle);
begin
   glAttachObjectARB(FHandle, shader.Handle);
end;

// BindAttribLocation
//
procedure TGLProgramHandle.BindAttribLocation(index : Integer; const name : String);
begin
   glBindAttribLocationARB(FHandle, index, PChar(name));
end;

// LinkProgram
//
function TGLProgramHandle.LinkProgram : Boolean;
var
   linked : Integer;
begin
   glLinkProgramARB(FHandle);
   linked:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_LINK_STATUS_ARB, @linked);
   Result:=(linked<>0);
end;

// ValidateProgram
//
function TGLProgramHandle.ValidateProgram : Boolean;
var
   validated : Integer;
begin
   glValidateProgramARB(FHandle);
   validated:=0;
   glGetObjectParameterivARB(FHandle, GL_OBJECT_VALIDATE_STATUS_ARB, @validated);
   Result:=(validated<>0);
end;

// GetAttribLocation
//
function TGLProgramHandle.GetAttribLocation(const name : String) : Integer;
begin
   Result:=glGetAttribLocationARB(Handle, PChar(name));
   Assert(Result>=0, 'Unknown attrib "'+name+'" or program not in use');
end;

// GetUniformLocation
//
function TGLProgramHandle.GetUniformLocation(const name : String) : Integer;
begin
   Result:=glGetUniformLocationARB(Handle, PChar(name));
   Assert(Result>=0, 'Unknown uniform "'+name+'" or program not in use');
end;

// GetAttribLocation
//
procedure TGLProgramHandle.UseProgramObject;
begin
   glUseProgramObjectARB(FHandle);
end;

// GetAttribLocation
//
procedure TGLProgramHandle.EndUseProgramObject;
begin
   glUseProgramObjectARB(0);
end;

// GetUniform1i
//
function TGLProgramHandle.GetUniform1i(const index : String) : Integer;
begin
   glGetUniformivARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1f
//
procedure TGLProgramHandle.SetUniform1f(const index : String; val : Single);
begin
   glUniform1fARB(GetUniformLocation(index), val);
end;

// GetUniform1f
//
function TGLProgramHandle.GetUniform1f(const index : String) : Single;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform1i
//
procedure TGLProgramHandle.SetUniform1i(const index : String; val : Integer);
begin
   glUniform1iARB(GetUniformLocation(index), val);
end;

// GetUniform3f
//
function TGLProgramHandle.GetUniform3f(const index : String) : TAffineVector;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform3f
//
procedure TGLProgramHandle.SetUniform3f(const index : String; const val : TAffineVector);
begin
   glUniform3fARB(GetUniformLocation(index), val[0], val[1], val[2]);
end;

// GetUniform4f
//
function TGLProgramHandle.GetUniform4f(const index : String) : TVector;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniform4f
//
procedure TGLProgramHandle.SetUniform4f(const index : String; const val : TVector);
begin
   glUniform4fARB(GetUniformLocation(index), val[0], val[1], val[2], val[3]);
end;

// GetUniformMatrix4fv
//
function TGLProgramHandle.GetUniformMatrix4fv(const index : String) : TMatrix;
begin
   glGetUniformfvARB(FHandle, GetUniformLocation(index), @Result);
end;

// SetUniformMatrix4fv
//
procedure TGLProgramHandle.SetUniformMatrix4fv(const index : String; const val : TMatrix);
begin
   glUniformMatrix4fvARB(GetUniformLocation(index), 1, False, @val);
end;

// ------------------
// ------------------ TGLContextManager ------------------
// ------------------

// Create
//
constructor TGLContextManager.Create;
begin
   inherited Create;
   FList:=TThreadList.Create;
end;

// Destroy
//
destructor TGLContextManager.Destroy;
begin
   FList.Free;
   inherited Destroy;
end;

// CreateContext
//
function TGLContextManager.CreateContext : TGLContext;
begin
   if Assigned(vContextClasses) and (vContextClasses.Count>0) then begin
      Result:=TGLContextClass(vContextClasses[0]).Create;
      Result.FManager:=Self;
   end else Result:=nil;
end;

// Lock
//
procedure TGLContextManager.Lock;
begin
   FList.LockList;
end;

// UnLock
//
procedure TGLContextManager.UnLock;
begin
   FList.UnlockList;
end;

// ContextCount
//
function TGLContextManager.ContextCount : Integer;
begin
   // try..finally just a waste of CPU here, if Count fails, the list is amok,
   // and so is the lock...
   Result:=FList.LockList.Count;
   FList.UnLockList;
end;

// RegisterContext
//
procedure TGLContextManager.RegisterContext(aContext : TGLContext);
begin
   with FList.LockList do try
      if IndexOf(aContext)>=0 then
         raise EGLContext.Create(cInvalidContextRegistration)
      else Add(aContext);
   finally
      FList.UnlockList;
   end;
end;

// UnRegisterContext
//
procedure TGLContextManager.UnRegisterContext(aContext : TGLContext);
begin
   with FList.LockList do try
      if IndexOf(aContext)<0 then
         raise EGLContext.Create(cInvalidContextRegistration)
      else Remove(aContext);
   finally
      FList.UnlockList;
   end;
end;

// ContextCreatedBy
//
procedure TGLContextManager.ContextCreatedBy(aContext : TGLContext);
begin
   Lock;
   try
      Inc(FCreatedRCCount);
   finally
      UnLock;
   end;
end;

// DestroyingContextBy
//
procedure TGLContextManager.DestroyingContextBy(aContext : TGLContext);
var
   cn : TGLContextNotification;
begin
   Lock;
   try
      Dec(FCreatedRCCount);
      if FCreatedRCCount=0 then begin
         // yes, slow and bulky, but allows for the triggered event to
         // cascade-remove notifications safely
         while Length(FNotifications)>0 do begin
            cn:=FNotifications[High(FNotifications)];
            SetLength(FNotifications, Length(FNotifications)-1);
            cn.event(cn.obj);
         end;
      end;
   finally
      UnLock;
   end;
end;

// LastContextDestroyNotification
//
procedure TGLContextManager.LastContextDestroyNotification(
                                    anObject : TObject; anEvent : TNotifyEvent);
begin
   Lock;
   try
      SetLength(FNotifications, Length(FNotifications)+1);
      with FNotifications[High(FNotifications)] do begin
         obj:=anObject;
         event:=anEvent;
      end;
   finally
      UnLock;
   end;
end;

// RemoveNotification
//
procedure TGLContextManager.RemoveNotification(anObject : TObject);
var
   i : Integer;
   found : Boolean;
begin
   Lock;
   try
      found:=False;
      i:=Low(FNotifications);
      while i<=High(FNotifications) do begin
         if FNotifications[i].obj=anObject then begin
            found:=True;
            while i<=High(FNotifications) do begin
               FNotifications[i]:=FNotifications[i+1];
               Inc(i);
            end;
            SetLength(FNotifications, Length(FNotifications)-1);
            Break;
         end;
         Inc(i);
      end;
      if not found then
         raise EGLContext.Create(cInvalidNotificationRemoval);
   finally
      UnLock;
   end;
end;

// Terminate
//
procedure TGLContextManager.Terminate;
begin
   FTerminated:=True;
   if ContextCount=0 then begin
      GLContextManager:=nil;
      Free;
   end;
end;

// DestroyAllHandles
//
procedure TGLContextManager.DestroyAllHandles;
var
   i : Integer;
begin
   with FList.LockList do try
      for i:=Count-1 downto 0 do
         TGLContext(Items[i]).DestroyAllHandles;
   finally
      FList.UnLockList;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   GLContextManager:=TGLContextManager.Create;

finalization

   GLContextManager.Terminate;
   vContextClasses.Free;
   vContextClasses:=nil;

end.
