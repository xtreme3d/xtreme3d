// GLImposter
{: Imposter building and rendering implementation for GLScene.<p>

   <b>History : </b><font size=-1><ul>
      <li>07/05/04 - EG - Perspective distortion properly applied
      <li>06/05/04 - EG - Fixes, improvements, clean ups
      <li>04/05/04 - EG - Reworked architecture
      <li>14/04/04 - SG - Fixed texture clamping for old cards and 
                          switched to GL_NEAREST texture sampling.
      <li>24/03/04 - SG - Initial.
   </ul></font><p>
}
unit GLImposter;

interface

uses
  Classes, GLScene, GLContext, GLTexture, VectorTypes, VectorGeometry,
  GeometryBB, GLMisc, PersistentClasses, GLCrossPlatform, GLGraphics;

type
   // TImposterOptions
   //
   {: Imposter rendering options.<p>
      Following options are supported:<ul>
      <li>impoBlended : the imposters are transparently blended during renders,
      this will smooth their edges but requires them to be rendered sorted
      from back to front
      <li>impoAlphaTest : alpha test is used to eliminate transparent pixels,
      the alpha treshold is adjusted by the AlphaTreshold property
      <li>impoNearestFiltering : use nearest texture filtering (the alternative
      is linear filtering)
      <li>impoPerspectiveCorrection : activates a special imposter rendering
      projection suitable for distorting the sprites when seen from a level
      angle of view with a wide focal camera (think trees/grass when walking
      in a forest), if not active, the imposter sprites are camera-facing
      </ul>
   }
   TImposterOption = (impoBlended, impoAlphaTest, impoNearestFiltering,
                      impoPerspectiveCorrection);
   TImposterOptions = set of TImposterOption;

const
   cDefaultImposterOptions = [impoBlended, impoAlphaTest];

type
   TGLImposterBuilder = class;

   // TImposter
   //
   {: Base class for imposters manipulation and handling.<br>
      Rendering imposters is performed by three methods, BeginRender must
      be invoked first, then Render for each of the impostr
      This class assumes a single impostor per texture. 
      
      Note: Remeber to enable Destination Alpha on your viewer.}
   TImposter = class (TObject)
      private
	      { Private Declarations }
         FRequestCount : Integer;
         FBuilder : TGLImposterBuilder;
         FTexture : TGLTextureHandle;
         FImpostoredObject : TGLBaseSceneObject;
         FAspectRatio : Single;
         FModulated : Boolean;

		protected
			{ Protected Declarations }
         FVx, FVy : TVector;
         FStaticOffset : TVector;
         FQuad : array [0..3] of TVector;
         FStaticScale : Single;

         procedure PrepareTexture; dynamic;
         procedure RenderQuad(const texExtents, objPos : TVector; size : Single);

      public
	      { Public Declarations }
         constructor Create(aBuilder : TGLImposterBuilder); virtual;
         destructor Destroy; override;

         procedure BeginRender(var rci : TRenderContextInfo); virtual;
         procedure Render(var rci : TRenderContextInfo;
                          const objPos, localCameraPos : TVector;
                          size : Single); virtual;
         procedure EndRender(var rci : TRenderContextInfo); virtual;

         procedure RenderOnce(var rci : TRenderContextInfo;
                              const objPos, localCameraPos : TVector;
                              size : Single);

         property AspectRatio : Single read FAspectRatio write FAspectRatio;
         property Builder : TGLImposterBuilder read FBuilder;
         property Texture : TGLTextureHandle read FTexture;
         property ImpostoredObject : TGLBaseSceneObject read FImpostoredObject write FImpostoredObject;
         property Modulated : Boolean read FModulated write FModulated;
   end;

   // Imposter loading events
   //
   TLoadingImposterEvent = function (Sender : TObject; impostoredObject : TGLBaseSceneObject;
                                     destImposter : TImposter) : TGLBitmap32 of object;
   TImposterLoadedEvent = procedure (Sender : TObject; impostoredObject : TGLBaseSceneObject;
                                     destImposter : TImposter) of object;

   // TImposterReference
   //
   TImposterReference = (irCenter, irTop, irBottom);  

   // TGLImposterBuilder
   //
   {: Abstract ImposterBuilder class. }
   TGLImposterBuilder = class (TGLUpdateAbleComponent)
      private
	      { Private Declarations }
         FBackColor : TGLColor;
         FBuildOffset : TGLCoordinates;
         FImposterRegister : TPersistentObjectList;
         FRenderPoint : TGLRenderPoint;
         FImposterOptions : TImposterOptions;
         FAlphaTreshold : Single;
         FImposterReference : TImposterReference;
         FOnLoadingImposter : TLoadingImposterEvent;
         FOnImposterLoaded : TImposterLoadedEvent;

      protected
			{ Protected Declarations }
         procedure SetRenderPoint(val : TGLRenderPoint);
         procedure RenderPointFreed(Sender : TObject);
         procedure SetBackColor(val : TGLColor);
         procedure SetBuildOffset(val : TGLCoordinates);
         procedure SetImposterReference(val : TImposterReference);

         procedure InitializeImpostorTexture(const textureSize : TGLPoint);

         property ImposterRegister : TPersistentObjectList read FImposterRegister;
         procedure UnregisterImposter(imposter : TImposter);

         function CreateNewImposter : TImposter; virtual;
         procedure PrepareImposters(Sender : TObject; var rci : TRenderContextInfo); virtual;
         procedure DoPrepareImposter(var rci : TRenderContextInfo;
                                     impostoredObject : TGLBaseSceneObject;
                                     destImposter : TImposter); virtual; abstract;
         procedure DoUserSpecifiedImposter(destImposter : TImposter;
                                           bmp32 : TGLBitmap32); virtual;

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure NotifyChange(Sender : TObject); override;

         {: Returns a valid imposter for the specified object.<p>
            Imposter must have been requested first, and the builder given
            an opportunity to prepare it before it can be available. } 
         function ImposterFor(impostoredObject : TGLBaseSceneObject) : TImposter;
         {: Request an imposter to be prepared for the specified object. }
         procedure RequestImposterFor(impostoredObject : TGLBaseSceneObject);
         {: Tells the imposter for the specified object is no longer needed. }
         procedure UnRequestImposterFor(impostoredObject : TGLBaseSceneObject);

      published
	      { Published Declarations }
         {: Specifies the render point at which the impostor texture(s) can be prepared.<p>
            For best result, the render point should happen in viewer that has
            a destination alpha (otherwise, impostors will be opaque). }
         property RenderPoint : TGLRenderPoint read FRenderPoint write SetRenderPoint;
         {: Background color for impostor rendering.<p>
            Typically, you'll want to leave the alpha channel to zero, and pick
            as RGB as color that matches the impostor'ed objects edge colors most.}
         property BackColor : TGLColor read FBackColor write SetBackColor;
         {: Offset applied to the impostor'ed object during imposter construction.<p>
            Can be used to manually tune the centering of objects. }
         property BuildOffset : TGLCoordinates read FBuildOffset write SetBuildOffset;
         {: Imposter rendering options. }
         property ImposterOptions : TImposterOptions read FImposterOptions write FImposterOptions default cDefaultImposterOptions;
         {: Determines how the imposter are handled.<p>
            This is the reference point for imposters, impostor'ed objects that
            are centered should use irCenter, those whose bottom is the origin
            should use irBottom, etc. }
         property ImposterReference : TImposterReference read FImposterReference write SetImposterReference default irCenter;
         {: Alpha testing teshold.<p> }
         property AlphaTreshold : Single read FAlphaTreshold write FAlphaTreshold;

         {: Event fired before preparing/loading an imposter.<p>
            If an already prepared version of the importer is available, place
            it in the TGLBitmap32 the event shall return (the bitmap will be
            freed by the imposter builder). If a bitmap is specified, it will
            be used in place of what automatic generation could have generated. }
         property OnLoadingImposter : TLoadingImposterEvent read FOnLoadingImposter write FOnLoadingImposter;
         {: Event fired after preparing/loading an imposter.<p>
            This events gives an opportunity to save the imposter after it has
            been loaded or prepared. }
         property OnImposterLoaded : TImposterLoadedEvent read FOnImposterLoaded write FOnImposterLoaded;
   end;

	// TGLStaticImposterBuilderCorona
	//
   {: Describes a set of orientation in a corona fashion. }
	TGLStaticImposterBuilderCorona = class (TCollectionItem)
	   private
	      { Private Declarations }
         FSamples : Integer;
         FElevation : Single;
         FSampleBaseIndex : Integer;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetSamples(val : Integer);
         procedure SetElevation(val : Single);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

	   published
	      { Published Declarations }
         property Samples : Integer read FSamples write SetSamples default 8;
         property Elevation : Single read FElevation write SetElevation;
	end;

   TCoronaTangentLookup = record
      minTan, maxTan : Single;
      corona : TGLStaticImposterBuilderCorona;
   end;

	// TGLStaticImposterBuilderCoronas
	//
	TGLStaticImposterBuilderCoronas = class (TOwnedCollection)
	   private
	      { Private Declarations }
         FCoronaTangentLookup : array of TCoronaTangentLookup;

	   protected
	      { Protected Declarations }
         procedure SetItems(index : Integer; const val : TGLStaticImposterBuilderCorona);
	      function GetItems(index : Integer) : TGLStaticImposterBuilderCorona;
         procedure Update(Item: TCollectionItem); override;

         procedure PrepareSampleBaseIndices;
         procedure PrepareCoronaTangentLookup;
         function CoronaForElevationTangent(aTangent : Single) : TGLStaticImposterBuilderCorona;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TPersistent);

         function Add : TGLStaticImposterBuilderCorona; overload;
         function Add(const elevation : Single; samples : Integer) : TGLStaticImposterBuilderCorona; overload;
	      property Items[index : Integer] : TGLStaticImposterBuilderCorona read GetItems write SetItems; default;
         function SampleCount : Integer;

         procedure NotifyChange; virtual;
         procedure EndUpdate; override;
   end;

   // TStaticImposter
   //
   {: Imposter class whose texture contains several views from different angles. }
   TStaticImposter = class (TImposter)
      private
	      { Private Declarations }

		protected
			{ Protected Declarations }

      public
	      { Public Declarations }
         procedure Render(var rci : TRenderContextInfo;
                          const objPos, localCameraPos : TVector;
                          size : Single); override;
   end;

   // TSIBLigthing
   //
   TSIBLigthing = (siblNoLighting, siblStaticLighting, siblLocalLighting);

   // TGLStaticImposterBuilder
   //
   {: Builds imposters whose texture is a catalog of prerendered views. }
   TGLStaticImposterBuilder = class (TGLImposterBuilder)
      private
	      { Private Declarations }
         FCoronas : TGLStaticImposterBuilderCoronas;
         FSampleSize : Integer;
         FTextureSize : TGLPoint;
         FSamplesPerAxis : TGLPoint;
         FInvSamplesPerAxis : TVector2f;
         FSamplingRatioBias, FInvSamplingRatioBias : Single;
         FLighting : TSIBLigthing;
         FSamplesAlphaScale : Single;

      protected
			{ Protected Declarations }
         procedure SetCoronas(val : TGLStaticImposterBuilderCoronas);
         procedure SetSampleSize(val : Integer);
         procedure SetSamplingRatioBias(val : Single);
         function  StoreSamplingRatioBias : Boolean;
         procedure SetLighting(val : TSIBLigthing);
         procedure SetSamplesAlphaScale(val : Single);
         function  StoreSamplesAlphaScale : Boolean;

         function GetTextureSizeInfo : String;
         procedure SetTextureSizeInfo(const texSize : String);

         {: Computes the optimal texture size that would be able to hold all samples. }
         function ComputeOptimalTextureSize : TGLPoint;

         function CreateNewImposter : TImposter; override;
         procedure DoPrepareImposter(var rci : TRenderContextInfo;
                                     impostoredObject : TGLBaseSceneObject;
                                     destImposter : TImposter); override;
         procedure DoUserSpecifiedImposter(destImposter : TImposter;
                                           bmp32 : TGLBitmap32); override;
         procedure ComputeStaticParams(destImposter : TImposter);

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         {: Render imposter texture.<p>
            Buffer and object must be compatible, RC must have been activated. }
         procedure Render(var rci : TRenderContextInfo;
                          impostoredObject : TGLBaseSceneObject;
                          destImposter : TImposter);
         {: Ratio (0..1) of the texture that will be used by samples.<p>
            If this value is below 1, you're wasting texture space and may
            as well increase the number of samples. }
         function TextureFillRatio : Single;

         {: Meaningful only after imposter texture has been prepared. }
         property TextureSize : TGLPoint read FTextureSize;
         property SamplesPerAxis : TGLPoint read FSamplesPerAxis;

      published
	      { Published Declarations }
         {: Description of the samples looking orientations. }
         property Coronas : TGLStaticImposterBuilderCoronas read FCoronas write SetCoronas;
         {: Size of the imposter samples (square). }
         property SampleSize : Integer read FSampleSize write SetSampleSize default 32;
         {: Size ratio applied to the impostor'ed objects during sampling.<p>
            Values greater than one can be used to "fill" the samples more
            by scaling up the object. This is especially useful when the impostor'ed
            object doesn't fill its bounding sphere, and/or if the outer details
            are not relevant for impostoring. }
         property SamplingRatioBias : Single read FSamplingRatioBias write SetSamplingRatioBias stored StoreSamplingRatioBias;
         {: Scale factor apply to the sample alpha channel.<p>
            Main use is to saturate the samples alpha channel, and make fully
            opaque what would have been partially transparent, while leaving
            fully transparent what was fully transparent. }
         property SamplesAlphaScale : Single read FSamplesAlphaScale write SetSamplesAlphaScale stored StoreSamplesAlphaScale;
         {: Lighting mode to apply during samples construction. }
         property Lighting : TSIBLigthing read FLighting write FLighting default siblStaticLighting;
         {: Dummy property that returns the size of the imposter texture.<p>
            This property is essentially here as a helper at design time,
            to give you the requirements your coronas and samplesize parameters
            imply. } 
         property TextureSizeInfo : String read GetTextureSizeInfo write SetTextureSizeInfo stored False;
   end;

   // TGLDynamicImposterBuilder
   //
   TGLDynamicImposterBuilder = class (TGLImposterBuilder)
      private
	      { Private Declarations }
         FMinTexSize, FMaxTexSize : Integer;
         FMinDistance, FTolerance : Single;
         FUseMatrixError : Boolean;

      protected
			{ Protected Declarations }
         procedure SetMinDistance(const Value : Single);

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
{         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override; }

      published
	      { Published Declarations }
         property MinTexSize : Integer read FMinTexSize write FMinTexSize;
         property MaxTexSize : Integer read FMaxTexSize write FMaxTexSize;
         property MinDistance : Single read FMinDistance write SetMinDistance;
         property Tolerance : Single read FTolerance write FTolerance;
         property UseMatrixError : Boolean read FUseMatrixError write FUseMatrixError;

   end;

   // TGLImposter
   //
   TGLImposter = class(TGLImmaterialSceneObject)
      private
	      { Private Declarations }
         FBuilder : TGLImposterBuilder;
         FImpostoredObject : TGLBaseSceneObject;

      protected
			{ Protected Declarations }
         procedure SetBuilder(const val : TGLImposterBuilder);
         procedure SetImpostoredObject(const val : TGLBaseSceneObject);

      public
	      { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

      published
	      { Published Declarations }
         property Builder : TGLImposterBuilder read FBuilder write SetBuilder;
         property ImpostoredObject : TGLBaseSceneObject read FImpostoredObject write SetImpostoredObject;
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, OpenGL1x, GLUtils;

const
   cReferenceToPos : array [Low(TImposterReference)..High(TImposterReference)] of Single =
                           ( 0, -1, 1 );

// ----------
// ---------- TImposter ----------
// ----------

// Create
//
constructor TImposter.Create(aBuilder : TGLImposterBuilder);
begin
   inherited Create;
   FBuilder:=aBuilder;
   FTexture:=TGLTextureHandle.Create;
   aBuilder.FImposterRegister.Add(Self);
   FAspectRatio:=1;
end;

// Destroy
//
destructor TImposter.Destroy;
begin
   if Assigned(FBuilder) then
      FBuilder.UnregisterImposter(Self);
   FTexture.Free;
   inherited;
end;

// PrepareTexture
//
procedure TImposter.PrepareTexture;
var
   i : Integer;
begin
   if FTexture.Handle<>0 then Exit;

   FTexture.AllocateHandle;
   glBindTexture(GL_TEXTURE_2D, FTexture.Handle);
   if GL_EXT_texture_edge_clamp then
      i:=GL_CLAMP_TO_EDGE
   else i:=GL_CLAMP;
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, i);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, i);
 	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

// BeginRender
//
procedure TImposter.BeginRender(var rci : TRenderContextInfo);
var
   mat : TMatrix;
   filter : TGLEnum;
   fx, fy, yOffset, cosAlpha, dynScale : Single;
begin
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);
   glDisable(GL_CULL_FACE);
   glEnable(GL_TEXTURE_2D);

   if impoAlphaTest in Builder.ImposterOptions then begin
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GEQUAL, Builder.AlphaTreshold);
   end else glDisable(GL_ALPHA_TEST);

   if impoBlended in Builder.ImposterOptions then begin
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   end else glDisable(GL_BLEND);

   rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, Texture.Handle);

   if impoNearestFiltering in Builder.ImposterOptions then
      filter:=GL_NEAREST
   else filter:=GL_LINEAR;
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter);
   if FModulated then begin
      glColor4fv(@XYZWHmgVector);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   end else glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

   glGetFloatv(GL_MODELVIEW_MATRIX, @mat[0][0]);
   FVx[0]:=mat[0][0];
   FVx[1]:=mat[1][0];
   FVx[2]:=mat[2][0];
   NormalizeVector(FVx);

   FVy[0]:=mat[0][1];
   FVy[1]:=mat[1][1];
   FVy[2]:=mat[2][1];
   NormalizeVector(FVy);
   if impoPerspectiveCorrection in Builder.ImposterOptions then begin
      cosAlpha:=VectorDotProduct(FVy, YHmgVector);
      FVy:=VectorLerp(FVy, YHmgVector, Abs(cosAlpha));
      NormalizeVector(FVy);
      dynScale:=ClampValue(1/cosAlpha, 1, 1.414)*FStaticScale;
   end else dynScale:=FStaticScale;

   fx:=Sqrt(FAspectRatio);
   fy:=1/fx;
   yOffset:=cReferenceToPos[Builder.ImposterReference]*dynScale*fy;
   fx:=fx*dynScale;
   fy:=fy*dynScale;

   FQuad[0]:=VectorSubtract(VectorCombine(FVx, FVy,  fx,  fy+yOffset), FStaticOffset);
   FQuad[1]:=VectorSubtract(VectorCombine(FVx, FVy, -fx,  fy+yOffset), FStaticOffset);
   FQuad[2]:=VectorSubtract(VectorCombine(FVx, FVy, -fx, -fy+yOffset), FStaticOffset);
   FQuad[3]:=VectorSubtract(VectorCombine(FVx, FVy,  fx, -fy+yOffset), FStaticOffset);

   glBegin(GL_QUADS);
end;

// Render
//
procedure TImposter.Render(var rci : TRenderContextInfo;
                           const objPos, localCameraPos : TVector;
                           size : Single);
const
   cQuadTexExtents : TVector = (0, 0, 1, 1);
begin
   RenderQuad(cQuadTexExtents, objPos, size);
end;

// RenderQuad
//
procedure TImposter.RenderQuad(const texExtents, objPos : TVector; size : Single);
var
   pos : TVector;
begin
   VectorCombine(objPos, FQuad[0], size, pos);
   glTexCoord2f(texExtents[2], texExtents[3]);  glVertex3fv(@pos);
   VectorCombine(objPos, FQuad[1], size, pos);
   glTexCoord2f(texExtents[0], texExtents[3]);  glVertex3fv(@pos);
   VectorCombine(objPos, FQuad[2], size, pos);
   glTexCoord2f(texExtents[0], texExtents[1]);  glVertex3fv(@pos);
   VectorCombine(objPos, FQuad[3], size, pos);
   glTexCoord2f(texExtents[2], texExtents[1]);  glVertex3fv(@pos);
end;

// EndRender
//
procedure TImposter.EndRender(var rci : TRenderContextInfo);
begin
   glEnd;
   glPopAttrib;
end;

// RenderOnce
//
procedure TImposter.RenderOnce(var rci : TRenderContextInfo;
                               const objPos, localCameraPos : TVector;
                               size : Single);
begin
   BeginRender(rci);
   Render(rci, objPos, localCameraPos, size);
   EndRender(rci);
end;

// ----------
// ---------- TGLImposterBuilder ----------
// ----------

// Create
//
constructor TGLImposterBuilder.Create(AOwner : TComponent);
begin
   inherited;
   FImposterRegister:=TPersistentObjectList.Create;
   FBackColor:=TGLColor.CreateInitialized(Self, clrTransparent);
   FBuildOffset:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint);
   FImposterOptions:=cDefaultImposterOptions;
   FAlphaTreshold:=0.5;
end;

// Destroy
//
destructor TGLImposterBuilder.Destroy;
var
   i : Integer;
begin
   FBuildOffset.Free;
   FBackColor.Free;
   for i:=0 to FImposterRegister.Count-1 do
      TImposter(FImposterRegister[i]).FBuilder:=nil;
   FImposterRegister.CleanFree;
   inherited;
end;

// Notification
//
procedure TGLImposterBuilder.Notification(AComponent: TComponent; Operation: TOperation);
var
   i : Integer;
   imposter : TImposter;
begin
   if Operation=opRemove then begin
      if AComponent=FRenderPoint then
         FRenderPoint:=nil;
      for i:=FImposterRegister.Count-1 downto 0 do begin
         imposter:=TImposter(FImposterRegister[i]);
         if imposter.ImpostoredObject=AComponent then begin
            imposter.Free;
            Break;
         end;
      end;
   end;
   inherited;
end;

// CreateNewImposter
//
function TGLImposterBuilder.CreateNewImposter : TImposter;
begin
   Result:=TImposter.Create(Self);
end;

// PrepareImposters
//
procedure TGLImposterBuilder.PrepareImposters(Sender : TObject; var rci : TRenderContextInfo);
var
   i : Integer;
   imp : TImposter;
   bmp32 : TGLBitmap32;
begin
   for i:=0 to ImposterRegister.Count-1 do begin
      imp:=TImposter(ImposterRegister[i]);
      if (imp.ImpostoredObject<>nil) and (imp.Texture.Handle=0) then begin
         if Assigned(FOnLoadingImposter) then
            bmp32:=FOnLoadingImposter(Self, imp.ImpostoredObject, imp)
         else bmp32:=nil;
         if not Assigned(bmp32) then
            DoPrepareImposter(rci, imp.ImpostoredObject, imp)
         else begin
            DoUserSpecifiedImposter(imp, bmp32);
            bmp32.Free;
         end;
         if Assigned(FOnImposterLoaded) then
            FOnImposterLoaded(Self, imp.ImpostoredObject, imp);
      end;
   end;
end;

// DoUserSpecifiedImposter
//
procedure TGLImposterBuilder.DoUserSpecifiedImposter(destImposter : TImposter;
                                                     bmp32 : TGLBitmap32);
begin
   destImposter.PrepareTexture;
   bmp32.RegisterAsOpenGLTexture(GL_TEXTURE_2D, miLinear, GL_RGBA8);
end;

// NotifyChange
//
procedure TGLImposterBuilder.NotifyChange(Sender : TObject);
var
   i : Integer;
begin
   for i:=0 to FImposterRegister.Count-1 do
      TImposter(FImposterRegister[i]).Texture.DestroyHandle;
   inherited;
end;

// ImposterFor
//
function TGLImposterBuilder.ImposterFor(impostoredObject : TGLBaseSceneObject) : TImposter;
var
   i : Integer;
begin
   for i:=0 to FImposterRegister.Count-1 do begin
      Result:=TImposter(FImposterRegister[i]);
      if Result.ImpostoredObject=impostoredObject then
         Exit;
   end;
   Result:=nil;
end;

// RequestImposterFor
//
procedure TGLImposterBuilder.RequestImposterFor(impostoredObject : TGLBaseSceneObject);
var
   imposter : TImposter;
begin
   if impostoredObject=nil then Exit;
   imposter:=ImposterFor(impostoredObject);
   if imposter=nil then begin
      imposter:=CreateNewImposter;
      imposter.ImpostoredObject:=impostoredObject;
   end;
   Inc(imposter.FRequestCount);
end;

procedure TGLImposterBuilder.UnRequestImposterFor(impostoredObject : TGLBaseSceneObject);
var
   imposter : TImposter;
begin
   if impostoredObject=nil then Exit;
   imposter:=ImposterFor(impostoredObject);
   if imposter<>nil then begin
      Dec(imposter.FRequestCount);
      if imposter.FRequestCount=0 then
         imposter.Free;
   end;
end;

// SetRenderPoint
//
procedure TGLImposterBuilder.SetRenderPoint(val : TGLRenderPoint);
begin
   if val<>FRenderPoint then begin
      if Assigned(FRenderPoint) then begin
         FRenderPoint.RemoveFreeNotification(Self);
         FRenderPoint.UnRegisterCallBack(PrepareImposters);
      end;
      FRenderPoint:=val;
      if Assigned(FRenderPoint) then begin
         FRenderPoint.FreeNotification(Self);
         FRenderPoint.RegisterCallBack(PrepareImposters, RenderPointFreed);
      end;
   end;
end;

// RenderPointFreed
//
procedure TGLImposterBuilder.RenderPointFreed(Sender : TObject);
begin
   FRenderPoint:=nil;
end;

// SetBackColor
//
procedure TGLImposterBuilder.SetBackColor(val : TGLColor);
begin
   FBackColor.Assign(val);
end;

// SetBuildOffset
//
procedure TGLImposterBuilder.SetBuildOffset(val : TGLCoordinates);
begin
   FBuildOffset.Assign(val);
end;

// SetImposterReference
//
procedure TGLImposterBuilder.SetImposterReference(val : TImposterReference);
begin
   if FImposterReference<>val then begin
      FImposterReference:=val;
      NotifyChange(Self);
   end;
end;

// InitializeImpostorTexture
//
procedure TGLImposterBuilder.InitializeImpostorTexture(const textureSize : TGLPoint);
var
   memBuffer : Pointer;
begin
   memBuffer:=GetMemory(textureSize.X*textureSize.Y*4);
   try
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, textureSize.X, textureSize.Y, 0,
                   GL_RGBA, GL_UNSIGNED_BYTE, memBuffer);
   finally
      FreeMemory(memBuffer);
   end;
end;

// UnregisterImposter
//
procedure TGLImposterBuilder.UnregisterImposter(imposter : TImposter);
begin
   if imposter.Builder=Self then begin
      FImposterRegister.Remove(imposter);
      imposter.FBuilder:=nil;
   end;
end;

// ----------
// ---------- TGLStaticImposterBuilderCorona ----------
// ----------

// Create
//
constructor TGLStaticImposterBuilderCorona.Create(Collection : TCollection);
begin
   inherited;
   FSamples:=8;
end;

// Destroy
//
destructor TGLStaticImposterBuilderCorona.Destroy;
begin
   inherited;
end;

// Assign
//
procedure TGLStaticImposterBuilderCorona.Assign(Source: TPersistent);
begin
   if Source is TGLStaticImposterBuilderCorona then begin
      FSamples:=TGLStaticImposterBuilderCorona(Source).FSamples;
      FElevation:=TGLStaticImposterBuilderCorona(Source).FElevation;
   end;
   inherited;
end;

// GetDisplayName
//
function TGLStaticImposterBuilderCorona.GetDisplayName : String;
begin
   Result:=Format('%.1f° / %d samples', [Elevation, Samples]);
end;

// SetSamples
//
procedure TGLStaticImposterBuilderCorona.SetSamples(val : Integer);
begin
   if val<>FSamples then begin
      FSamples:=val;
      if FSamples<1 then FSamples:=1;
      (Collection as TGLStaticImposterBuilderCoronas).NotifyChange;
   end;
end;

// SetElevation
//
procedure TGLStaticImposterBuilderCorona.SetElevation(val : Single);
begin
   if val<>FElevation then begin
      FElevation:=ClampValue(val, -89, 89);
      (Collection as TGLStaticImposterBuilderCoronas).NotifyChange;
   end;
end;

// ----------
// ---------- TGLStaticImposterBuilderCoronas ----------
// ----------

// Create
//
constructor TGLStaticImposterBuilderCoronas.Create(AOwner : TPersistent);
begin
   inherited Create(AOwner, TGLStaticImposterBuilderCorona);
end;

// Add
//
function TGLStaticImposterBuilderCoronas.Add : TGLStaticImposterBuilderCorona;
begin
	Result:=(inherited Add) as TGLStaticImposterBuilderCorona;
end;

// Add (elevation, samples)
//
function TGLStaticImposterBuilderCoronas.Add(const elevation : Single;
                              samples : Integer) : TGLStaticImposterBuilderCorona;
begin
	Result:=(inherited Add) as TGLStaticImposterBuilderCorona;
   Result.Elevation:=elevation;
   Result.Samples:=samples;
end;

// SetItems
//
procedure TGLStaticImposterBuilderCoronas.SetItems(index : Integer; const val : TGLStaticImposterBuilderCorona);
begin
   inherited Items[index]:=val;
end;

// GetItems
//
function TGLStaticImposterBuilderCoronas.GetItems(index : Integer) : TGLStaticImposterBuilderCorona;
begin
   Result:=TGLStaticImposterBuilderCorona(inherited Items[index]);
end;

// Update
//
procedure TGLStaticImposterBuilderCoronas.Update(Item: TCollectionItem);
begin
   inherited;
   NotifyChange;
end;

// NotifyChange
//
procedure TGLStaticImposterBuilderCoronas.NotifyChange;
begin
   if (UpdateCount=0) and (GetOwner<>nil) and (GetOwner is TGLUpdateAbleComponent) then
      TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLStaticImposterBuilderCoronas.EndUpdate;
begin
   inherited;
   NotifyChange;
end;

// SampleCount
//
function TGLStaticImposterBuilderCoronas.SampleCount : Integer;
var
   i : Integer;
begin
   Result:=0;
   for i:=0 to Count-1 do
      Result:=Result+Items[i].Samples;
end;

// PrepareSampleBaseIndices
//
procedure TGLStaticImposterBuilderCoronas.PrepareSampleBaseIndices;
var
   p, i : Integer;
begin
   p:=0;
   for i:=0 to Count-1 do begin
      Items[i].FSampleBaseIndex:=p;
      Inc(p, Items[i].Samples);
   end;
end;

// PrepareCoronaTangentLookup
//
procedure TGLStaticImposterBuilderCoronas.PrepareCoronaTangentLookup;
var
   i, j : Integer;
   corona : TGLStaticImposterBuilderCorona;
   boundary : Single;
begin
   SetLength(FCoronaTangentLookup, Count);
   // place them in the array and sort by ascending elevation
   for i:=0 to Count-1 do
      FCoronaTangentLookup[i].corona:=Items[i];
   for i:=0 to Count-2 do for j:=i+1 to Count-1 do
      if FCoronaTangentLookup[j].corona.Elevation<FCoronaTangentLookup[i].corona.Elevation then begin
         corona:=FCoronaTangentLookup[j].corona;
         FCoronaTangentLookup[j].corona:=FCoronaTangentLookup[i].corona;
         FCoronaTangentLookup[i].corona:=corona;
      end;
   // adjust min max then intermediate boundaries
   FCoronaTangentLookup[0].minTan:=-1e30;
   FCoronaTangentLookup[Count-1].minTan:=1e30;
   for i:=0 to Count-2 do begin
      boundary:=Tan((0.5*cPIdiv180)*( FCoronaTangentLookup[i].corona.Elevation
                                     +FCoronaTangentLookup[i+1].corona.Elevation));
      FCoronaTangentLookup[i].maxTan:=boundary;
      FCoronaTangentLookup[i+1].minTan:=boundary;
   end;
end;

// CoronaForElevationTangent
//
function TGLStaticImposterBuilderCoronas.CoronaForElevationTangent(aTangent : Single) : TGLStaticImposterBuilderCorona;
var
   i, n : Integer;
begin
   n:=High(FCoronaTangentLookup);
   if (n=0) or (aTangent<=FCoronaTangentLookup[0].maxTan) then
      Result:=FCoronaTangentLookup[0].corona
   else if aTangent>FCoronaTangentLookup[n].minTan then
      Result:=FCoronaTangentLookup[n].corona
   else begin
      Result:=FCoronaTangentLookup[1].corona;
      for i:=2 to n-2 do begin
         if aTangent<=FCoronaTangentLookup[i].minTan then Break;
         Result:=FCoronaTangentLookup[i].corona;
      end;
   end;
end;

// ----------
// ---------- TStaticImposter ----------
// ----------

// Render
//
procedure TStaticImposter.Render(var rci : TRenderContextInfo;
                                 const objPos, localCameraPos : TVector;
                                 size : Single);
var
   azimuthAngle : Single;
   i : Integer;
   x, y : Word;
   bestCorona : TGLStaticImposterBuilderCorona;
   texExtents : TVector;
   tdx, tdy : Single;
   siBuilder : TGLStaticImposterBuilder;
begin                  // inherited; exit;
   siBuilder:=TGLStaticImposterBuilder(Builder);

   // determine closest corona
   bestCorona:=siBuilder.Coronas.CoronaForElevationTangent(
                     localCameraPos[1]/VectorLength(localCameraPos[0], localCameraPos[2]));

   // determine closest sample in corona
   azimuthAngle:=FastArcTan2(localCameraPos[2], localCameraPos[0])+cPI;
   i:=Round(azimuthAngle*bestCorona.Samples*cInv2PI);
   if i<0 then
      i:=0
   else if i>=bestCorona.Samples then
      i:=bestCorona.Samples-1;
   i:=bestCorona.FSampleBaseIndex+i;

   tdx:=siBuilder.FInvSamplesPerAxis[0];
   tdy:=siBuilder.FInvSamplesPerAxis[1];
   DivMod(i, siBuilder.SamplesPerAxis.X, y, x);
   texExtents[0]:=tdx*x;
   texExtents[1]:=tdy*y;
   texExtents[2]:=texExtents[0]+tdx;
   texExtents[3]:=texExtents[1]+tdy;

   // then render it
   RenderQuad(texExtents, objPos, Size);
end;

// ----------
// ---------- TGLStaticImposterBuilder ----------
// ----------

// Create
//
constructor TGLStaticImposterBuilder.Create(AOwner : TComponent);
begin
   inherited;
   FCoronas:=TGLStaticImposterBuilderCoronas.Create(Self);
   FCoronas.Add;
   FSampleSize:=16;
   FSamplingRatioBias:=1;
   FInvSamplingRatioBias:=1;
   FLighting:=siblStaticLighting;
   FSamplesAlphaScale:=1;
end;

// Destroy
//
destructor TGLStaticImposterBuilder.Destroy;
begin
   FCoronas.Free;
   inherited;
end;

// CreateNewImposter
//
function TGLStaticImposterBuilder.CreateNewImposter : TImposter;
begin
   Result:=TStaticImposter.Create(Self);
end;

// SetCoronas
//
procedure TGLStaticImposterBuilder.SetCoronas(val : TGLStaticImposterBuilderCoronas);
begin
   FCoronas.Assign(val);
   NotifyChange(Self);
end;

// SetSampleSize
//
procedure TGLStaticImposterBuilder.SetSampleSize(val : Integer);
begin
   val:=RoundUpToPowerOf2(val);
   if val<8 then val:=8;
   if val>1024 then val:=1024;
   if val<>FSampleSize then begin
      FSampleSize:=val;
      NotifyChange(Self);
   end;
end;

// SetSamplingRatioBias
//
procedure TGLStaticImposterBuilder.SetSamplingRatioBias(val : Single);
begin
   val:=ClampValue(val, 0.1, 10);
   if val<>FSamplingRatioBias then begin
      FSamplingRatioBias:=val;
      FInvSamplingRatioBias:=1/val;
      NotifyChange(Self);
   end;
end;

// StoreSamplingRatioBias
//
function TGLStaticImposterBuilder.StoreSamplingRatioBias : Boolean;
begin
   Result:=(FSamplingRatioBias<>1);
end;

// SetLighting
//
procedure TGLStaticImposterBuilder.SetLighting(val : TSIBLigthing);
begin
   if val<>FLighting then begin
      FLighting:=val;
      NotifyChange(Self);
   end;
end;

// SetSamplesAlphaScale
//
procedure TGLStaticImposterBuilder.SetSamplesAlphaScale(val : Single);
begin
   if FSamplesAlphaScale<>val then begin
      FSamplesAlphaScale:=val;
      NotifyChange(Self);
   end;
end;

// StoreSamplesAlphaScale
//
function TGLStaticImposterBuilder.StoreSamplesAlphaScale : Boolean;
begin
   Result:=(FSamplesAlphaScale<>1);
end;

// GetTextureSizeInfo
//
function TGLStaticImposterBuilder.GetTextureSizeInfo : String;
var
   t : TGLPoint;
   fill : Integer;
begin
   t:=ComputeOptimalTextureSize;
   Result:=Format('%d x %d', [t.X, t.Y]);
   fill:=Coronas.SampleCount*SampleSize*SampleSize;
   if fill<t.X*t.Y then
      Result:=Result+Format(' (%.1f%%)', [(100*fill)/(t.X*t.Y)]);
end;

// SetTextureSizeInfo
//
procedure TGLStaticImposterBuilder.SetTextureSizeInfo(const texSize : String);
begin
   // do nothing, this is a dummy property!
end;

// DoPrepareImposter
//
procedure TGLStaticImposterBuilder.DoPrepareImposter(var rci : TRenderContextInfo;
                  impostoredObject : TGLBaseSceneObject; destImposter : TImposter);
begin
   Render(rci, impostoredObject, destImposter);
end;

// DoUserSpecifiedImposter
//
procedure TGLStaticImposterBuilder.DoUserSpecifiedImposter(destImposter : TImposter;
                                                           bmp32 : TGLBitmap32);
begin
   inherited;
   FTextureSize.X:=bmp32.Width;
   FTextureSize.Y:=bmp32.Height;
   ComputeStaticParams(destImposter);
end;

// ComputeStaticParams
//
procedure TGLStaticImposterBuilder.ComputeStaticParams(destImposter : TImposter);
var
   radius : Single;
begin
   Coronas.PrepareCoronaTangentLookup;
   Coronas.PrepareSampleBaseIndices;

   FSamplesPerAxis.X:=FTextureSize.X div SampleSize;
   FSamplesPerAxis.Y:=FTextureSize.Y div SampleSize;
   FInvSamplesPerAxis[0]:=1/FSamplesPerAxis.X;
   FInvSamplesPerAxis[1]:=1/FSamplesPerAxis.Y;
   Assert(FSamplesPerAxis.X*FSamplesPerAxis.Y>=Coronas.SampleCount,
          'User specified bitmap and imposter parameters don''t match');

   radius:=destImposter.ImpostoredObject.BoundingSphereRadius/SamplingRatioBias;
   
   if ImposterReference=irCenter then
      destImposter.FStaticScale:=radius
   else destImposter.FStaticScale:=radius*0.5;
   destImposter.FStaticOffset:=FBuildOffset.DirectVector;
end;

// Render
//
procedure TGLStaticImposterBuilder.Render(var rci : TRenderContextInfo;
            impostoredObject : TGLBaseSceneObject; destImposter : TImposter);
var
   i, coronaIdx, curSample, maxLight : Integer;
   radius : Single;
   cameraDirection, cameraOffset : TVector;
   xDest, xSrc, yDest, ySrc : Integer;
   corona : TGLStaticImposterBuilderCorona;
   fx, fy, yOffset : Single;
   viewPort : TVector4i;
begin
   FTextureSize:=ComputeOptimalTextureSize;
   if (FTextureSize.X<=0) and (FTextureSize.Y<=0) then begin
      SampleSize:=SampleSize shr 1;
      Assert(False, 'Too many samples, can''t fit in a texture! Reduce SampleSize.');
   end;

   ComputeStaticParams(destImposter);

   radius:=impostoredObject.BoundingSphereRadius/SamplingRatioBias;
   if ImposterReference<>irCenter then
      radius:=radius*0.5;

   glGetIntegerv(GL_MAX_LIGHTS, @maxLight);
   glGetIntegerv(GL_VIEWPORT, @viewPort);
   Assert((viewPort[2]>=SampleSize) and (viewPort[3]>=SampleSize),
          'ViewPort too small to render imposter samples!');

   // Setup the buffer in a suitable fashion for our needs
   glPushAttrib(GL_ENABLE_BIT+GL_COLOR_BUFFER_BIT);
   with FBackColor do
      glClearColor(Red, Green, Blue, Alpha);
   if Lighting=siblNoLighting then
      glDisable(GL_LIGHTING);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   fx:=radius*viewPort[2]/SampleSize;
   fy:=radius*viewPort[3]/SampleSize;
   yOffset:=cReferenceToPos[ImposterReference]*radius;
   glOrtho(-fx, fx, yOffset-fy, yOffset+fy, radius*0.5, radius*5);
   xSrc:=(viewPort[2]-SampleSize) div 2;
   ySrc:=(viewPort[3]-SampleSize) div 2;

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;

   // setup imposter texture
   if destImposter.Texture.Handle=0 then begin
      destImposter.PrepareTexture;
      InitializeImpostorTexture(FTextureSize);
      rci.GLStates.ResetGLCurrentTexture;
   end;

   glPixelTransferf(GL_ALPHA_SCALE, FSamplesAlphaScale);

   // Now render each sample
   curSample:=0;
   for coronaIdx:=0 to Coronas.Count-1 do begin
      corona:=Coronas[coronaIdx];
      cameraDirection:=XHmgVector;
      RotateVector(cameraDirection, ZHmgPoint, corona.Elevation*cPIdiv180);
      for i:=0 to corona.Samples-1 do begin
         cameraOffset:=cameraDirection;
         RotateVector(cameraOffset, YHmgVector, (c2PI*i)/corona.Samples);
         ScaleVector(cameraOffset, -radius*2);

         glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT);
         glLoadIdentity;
         gluLookAt(cameraOffset[0], cameraOffset[1], cameraOffset[2],
                   0, 0, 0, 0, 1, 0);
         if Lighting=siblStaticLighting then
            (rci.scene as TGLScene).SetupLights(maxLight);
         glTranslatef(FBuildOffset.X, FBuildOffset.Y, FBuildOffset.Z);
         impostoredObject.Render(rci);
         rci.GLStates.ResetAll;
         CheckOpenGLError;

         xDest:=(curSample mod FSamplesPerAxis.X)*SampleSize;
         yDest:=(curSample div FSamplesPerAxis.X)*SampleSize;

         glBindTexture(GL_TEXTURE_2D, destImposter.Texture.Handle);
         glCopyTexSubImage2D(GL_TEXTURE_2D, 0, xDest, yDest, xSrc, ySrc,
                             SampleSize, SampleSize);

         Inc(curSample);
      end;
   end;

   // Restore buffer stuff
   glPixelTransferf(GL_ALPHA_SCALE, 1);
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_PROJECTION);
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   rci.GLStates.ResetGLCurrentTexture;

   glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT);
   if Lighting=siblStaticLighting then
      (rci.scene as TGLScene).SetupLights(maxLight);
end;

// ComputeOptimalTextureSize
//
function TGLStaticImposterBuilder.ComputeOptimalTextureSize : TGLPoint;
var
   nbSamples, maxSamples, maxTexSize, baseSize : Integer;
   texDim, bestTexDim : TGLPoint;
   requiredSurface, currentSurface, bestSurface : Integer;
begin
   nbSamples:=Coronas.SampleCount;
   if CurrentGLContext=nil then
      maxTexSize:=16*1024
   else glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxTexSize);
   maxSamples:=Sqr(maxTexSize div SampleSize);
   if nbSamples<maxSamples then begin
      Result.X:=-1;
      Result.Y:=-1;
   end;
   requiredSurface:=nbSamples*SampleSize*SampleSize;
   baseSize:=RoundUpToPowerOf2(SampleSize);

   // determine the texture size with the best fill ratio
   bestSurface:=MaxInt;
   texDim.X:=baseSize;
   while texDim.X<=maxTexSize do begin
      texDim.Y:=baseSize;
      while texDim.Y<=maxTexSize do begin
         currentSurface:=texDim.X*texDim.Y;
         if currentSurface>=requiredSurface then begin
            if currentSurface<bestSurface then begin
               bestTexDim:=texDim;
               bestSurface:=currentSurface;
            end else if (currentSurface=bestSurface)
                  and (MaxInteger(texDim.X, texDim.Y)<MaxInteger(bestTexDim.X, bestTexDim.Y)) then begin
               bestTexDim:=texDim;
               bestSurface:=currentSurface;
            end else Break;
         end;
         texDim.Y:=texDim.Y*2;
      end;
      texDim.X:=texDim.X*2;
   end;
   Assert(bestSurface<>MaxInt);

   Result:=bestTexDim;
end;

// TextureFillRatio
//
function TGLStaticImposterBuilder.TextureFillRatio : Single;
var
   texDim : TGLPoint;
begin
   texDim:=ComputeOptimalTextureSize;
   Result:=(Coronas.SampleCount*SampleSize*SampleSize)/(texDim.X*texDim.Y);
end;

// ----------
// ---------- TGLDynamicImposterBuilder ----------
// ----------

// Create
//
constructor TGLDynamicImposterBuilder.Create(AOwner : TComponent);
begin
  inherited;
  FTolerance:=0.1;
  FUseMatrixError:=True;
  FMinTexSize:=16;
  FMaxTexSize:=64;
end;

// Destroy
//
destructor TGLDynamicImposterBuilder.Destroy;
begin
  inherited;
end;
{
// DoRender
//
procedure TGLDynamicImposterBuilder.DoRender(var rci : TRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
  i, size, Left, Top, Width, Height : Integer;
  imposter : TGLImposter;
  mat, projection, modelview : TMatrix;
  BackColor, pos, temp : TVector;
  rad : Single;
  AABB : TAABB;
begin
  if (csDesigning in ComponentState) or not FEnabled then exit;

  // Store the current clear color
  glGetFloatv(GL_COLOR_CLEAR_VALUE, @BackColor[0]);

  // Get the projection matrix
  if UseMatrixError then
    glGetFloatv(GL_PROJECTION_MATRIX, @projection);

  // Render and save each imposter as required
  for i:=0 to FImposterRegister.Count-1 do begin
    imposter:=TGLImposter(FImposterRegister[i]);
    if (imposter.Count = 0) or not imposter.Visible then Continue;
    imposter.FDrawImposter:=True;

    if VectorDistance(imposter.AbsolutePosition, rci.cameraPosition)<FMinDistance then begin
      imposter.FDrawImposter:=False;
      Continue;
    end;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glMultMatrixf(@imposter.AbsoluteMatrixAsAddress[0]);
    glGetFloatv(GL_MODELVIEW_MATRIX, @modelview);

    // Get imposters dimensions
    AABB:=imposter.AxisAlignedBoundingBox;
    rad:=MaxFloat(AABB.max[0],AABB.max[1],AABB.max[2]);
    pos:=imposter.AbsolutePosition;
    temp:=Scene.CurrentBuffer.Camera.AbsoluteEyeSpaceVector(0,1,0);
    temp:=VectorAdd(pos, VectorScale(temp,rad));
    pos:=Scene.CurrentBuffer.WorldToScreen(pos);
    temp:=Scene.CurrentBuffer.WorldToScreen(temp);
    size:=RoundUpToPowerOf2(Round(2*VectorDistance(pos,temp)));
    if size<FMinTexSize then size:=FMinTexSize;
    if size>FMaxTexSize then begin
      imposter.FDrawImposter:=False;
      glPopMatrix;
      Continue;
    end;
    temp:=pos;
    temp[0]:=temp[0]+size;
    temp:=Scene.CurrentBuffer.ScreenToWorld(temp);
    Imposter.FSize:=VectorDistance(imposter.AbsolutePosition,temp);
    imposter.FTexSize:=size;
    pos[0]:=pos[0]-size/2;
    pos[1]:=pos[1]-size/2;

    // Calculate error
    if UseMatrixError then begin
      mat:=MatrixMultiply(modelview, projection);
      if (imposter.CalcError(mat)>FTolerance) or (imposter.FInvalidated) then
        imposter.FOldMatrix:=mat
      else begin
        glPopMatrix;
        Continue;
      end;
    end;

    // Clear to transparent black
    glClearColor(0,0,0,0);

    // Determine size by color (for debug purposes)
    (*case size of
      16 : glClearColor(0,0,1,0.1);
      32 : glClearColor(0,1,0,0.1);
      64 : glClearColor(1,0,0,0.1);
      128 : glClearColor(1,1,0,0.1);
      256 : glClearColor(1,0,1,0.1);
    end;// *)

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

    // Render the imposter's children
    imposter.RenderChildren(0, imposter.Count-1, rci);
    glPopMatrix;

    // Select the imposters texture (will create the handle if null)
    glBindTexture(GL_TEXTURE_2D,imposter.TextureHandle);

    // Check for resize or invalidation
    if (imposter.FTexSize <> imposter.FLastTexSize)
    or (imposter.FInvalidated) then begin
      glTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      imposter.FLastTexSize:=imposter.FTexSize;
      imposter.FInvalidated:=False;
      imposter.NotifyChange(self);
    end;

    // Get the region to be copied from the frame buffer
    Left:=Floor(pos[0]); Top:=Floor(pos[1]);
    Width:=Size; Height:=Size;
    // ... Perhaps some region clamping here?

    // Copy the frame buffer pixels to the imposter texture
    glCopyTexSubImage2d(GL_TEXTURE_2D, 0, 0, 0,
                        Left, Top, Width, Height);
  end;

  // Reset the clear color and clear color, depth and stencil buffers
  glClearColor(BackColor[0],BackColor[1],BackColor[2],BackColor[3]);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;
}
// SetMinDistance
//
procedure TGLDynamicImposterBuilder.SetMinDistance(const Value : Single);
begin
  if Value<>FMinDistance then begin
    FMinDistance:=Value;
    NotifyChange(Self);
  end;
end;

// ----------
// ---------- TGLImposter ----------
// ----------

// Create
//
constructor TGLImposter.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// Destroy
//
destructor TGLImposter.Destroy;
begin
   Builder:=nil;
   ImpostoredObject:=nil;
   inherited;
end;

// Notification
//
procedure TGLImposter.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=Builder then Builder:=nil;
      if AComponent=ImpostoredObject then ImpostoredObject:=nil;
   end;
   inherited;
end;

// DoRender
//
procedure TGLImposter.DoRender(var rci : TRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
   camPos : TVector;
   imposter : TImposter;
begin
   if renderSelf and Assigned(Builder) and Assigned(ImpostoredObject) then begin
      imposter:=Builder.ImposterFor(ImpostoredObject);
      if Assigned(imposter) and (imposter.Texture.Handle<>0) then begin
         camPos:=AbsoluteToLocal(rci.cameraPosition);
         imposter.BeginRender(rci);
         imposter.Render(rci, NullHmgPoint, camPos, Scale.MaxXYZ);
         imposter.EndRender(rci);
      end;
   end;
   if renderChildren then
       Self.RenderChildren(0, Count-1,rci);
end;

// SetBuilder
//
procedure TGLImposter.SetBuilder(const val : TGLImposterBuilder);
begin
   if val<>FBuilder then begin
      if Assigned(FBuilder) then begin
         FBuilder.RemoveFreeNotification(Self);
         FBuilder.UnRequestImposterFor(ImpostoredObject);
      end;
      FBuilder:=val;
      if Assigned(FBuilder) then begin
         FBuilder.FreeNotification(Self);
         FBuilder.RequestImposterFor(ImpostoredObject);
      end;
   end;
end;

// SetImpostoredObject
//
procedure TGLImposter.SetImpostoredObject(const val : TGLBaseSceneObject);
begin
   if val<>FImpostoredObject then begin
      if Assigned(Builder) then
         FBuilder.UnRequestImposterFor(ImpostoredObject);
      FImpostoredObject:=val;
      if Assigned(Builder) then
         FBuilder.RequestImposterFor(ImpostoredObject);
   end;
end;

{
// AxisAlignedDimensionsUnscaled
//
function TGLImposter.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result:=NullHMGVector;
end;

// CalcDifference
//
function TGLImposter.CalcError(NewMatrix : TMatrix) : Single;
var
   i : Integer;
   mat : TMatrix;
   err : Single;
begin
   err:=0;
   mat:=NewMatrix;
   InvertMatrix(mat);
   mat:=MatrixMultiply(FOldMatrix, mat);
   for i:=0 to 3 do mat[i][i]:=mat[i][i]-1;
   for i:=0 to 15 do err:=err+Abs(mat[i div 4][i mod 4]);
   Result:=err;
end;

// GetTextureHandle
//
function TGLImposter.GetTextureHandle: Cardinal;
begin
  if FTextureHandle = 0 then
    glGenTextures(1, @FTextureHandle);
  Result:=FTextureHandle;
end;

// Invalidate
//
procedure TGLImposter.Invalidate;
begin
  FInvalidated:=True;
end;
}
initialization

//  RegisterClasses([TGLDynamicImposterBuilder, TGLImposter]);
    RegisterClasses([TGLImposter]);

end.
