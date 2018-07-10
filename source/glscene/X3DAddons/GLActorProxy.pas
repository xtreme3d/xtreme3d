unit GLActorProxy;

interface

uses Classes, GLScene, VectorGeometry, GLMisc, GLTexture, GLSilhouette,
   GLVectorFileObjects;

  // VCL
  //Classes, SysUtils,

  // GLScene
  //GLScene, GLVectorGeometry, GLTexture, GLSilhouette, GLVectorFileObjects,
  //GLStrings, GLRenderContextInfo, GLBaseClasses, GLMaterial;

type
  // TBoneMatrixObj
  //
  {: An object containing the bone matrix for TGLActorProxy.<p> }
   TBoneMatrixObj = class
     Matrix:TMatrix;
     BoneName:string;
     BoneIndex:integer;
   end;

  // pamLoop mode was too difficalt to implement, so it was discarded ...for now.
  // pamPlayOnce only works if Actor.AnimationMode <> aamNone.
  TGLActorProxyAnimationMode = (pamInherited, pamNone, pamPlayOnce);

  // TGLActorProxy
  //
  {: A proxy object specialized for Actors.<p> }
  TGLActorProxy = class(TGLProxyObject)
  private
    { Private Declarations }
    FCurrentFrame: Integer;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FLastFrame: Integer;
    FCurrentFrameDelta: Single;
    FCurrentTime: TProgressTimes;
    FAnimation: TActorAnimationName;

    FTempLibMaterialName: string;
    FMasterLibMaterial: TGLLibMaterial;
    FMaterialLibrary: TGLMaterialLibrary;

    FBonesMatrices:TStringList;
    FStoreBonesMatrix: boolean;
    FStoredBoneNames: TStrings;
    FOnBeforeRender: TGLProgressEvent;
    FAnimationMode: TGLActorProxyAnimationMode;

    FInterval: Integer;

    procedure SetAnimation(const Value: TActorAnimationName);
    procedure SetMasterActorObject(const Value: TGLActor);
    function GetMasterActorObject: TGLActor;
    //function GetLibMaterialName: TGLLibMaterialName;
    procedure SetLibMaterialName(const Value: TGLLibMaterialName);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLMaterialLibrary;
    procedure SetStoreBonesMatrix(const Value: boolean);
    procedure SetStoredBoneNames(const Value: TStrings);
    procedure SetOnBeforeRender(const Value: TGLProgressEvent);
  protected
    { Protected Declarations }
    procedure DoStoreBonesMatrices; // stores matrices of bones of the current frame rendered
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoRender(var ARci : TRenderContextInfo;
                        ARenderSelf, ARenderChildren : Boolean); override;
    procedure DoProgress(const progressTime : TProgressTimes); override;
    property CurrentFrame: Integer read FCurrentFrame;
    property StartFrame: Integer read FStartFrame;
    property EndFrame: Integer read FEndFrame;
    property CurrentFrameDelta: Single read FCurrentFrameDelta;
    property CurrentTime: TProgressTimes read FCurrentTime;
    {: Gets the Bones Matrix in the current animation frame.
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    function BoneMatrix(BoneIndex:integer):TMatrix; overload;
    function BoneMatrix(BoneName:string):TMatrix; overload;
    procedure BoneMatricesClear;

    procedure SwitchToAnimation(animationIndex : Integer);
    procedure SetAnimationRange(startIndex, endIndex : Integer);

    {: A standard version of the RayCastIntersect function. }
    function RayCastIntersect(const rayStart, rayVector : TVector;
                              intersectPoint : PVector = nil;
                              intersectNormal : PVector = nil) : Boolean; override;


    {: Raycasts on self, but actually on the "RefActor" Actor.
       Note that the "RefActor" parameter does not necessarily have to be
       the same Actor refernced by the MasterObject property:
       This allows to pass a low-low-low-poly Actor to raycast in the "RefActor" parameter,
       while using a high-poly Actor in the "MasterObject" property,
       of course we assume that the two Masterobject Actors have same animations.
      }
    function RayCastIntersectEx( RefActor:TGLActor; const rayStart, rayVector : TVector;
                               intersectPoint : PVector = nil;
                               intersectNormal : PVector = nil) : Boolean; overload;

  published
    { Published Declarations }
    property AnimationMode: TGLActorProxyAnimationMode read FAnimationMode write FAnimationMode default pamInherited;
    property Animation: TActorAnimationName read FAnimation write SetAnimation;
    // Redeclare as TGLActor.
    property MasterObject: TGLActor read GetMasterActorObject write SetMasterActorObject;
    // Redeclare without pooTransformation
    // (Don't know why it causes the object to be oriented incorrecly.)
    property ProxyOptions default [pooEffects, pooObjects];
    {: Specifies the MaterialLibrary, that current proxy will use. }
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    {: Specifies the Material, that current proxy will use. }
    //property LibMaterialName: TGLLibMaterialName read GetLibMaterialName write SetLibMaterialName;
    {: Specifies if it will store the Bones Matrices, accessible via the BoneMatrix function
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    property StoreBonesMatrix:boolean read FStoreBonesMatrix write SetStoreBonesMatrix;
    {: Specifies the names of the bones we want the matrices to be stored. If empty, all bones will be stored
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) }
    property StoredBoneNames:TStrings read FStoredBoneNames write SetStoredBoneNames;
    {: Event allowing to apply extra transformations (f.ex: bone rotations) to the referenced
       Actor on order to have the proxy render these changes.  }
    property OnBeforeRender : TGLProgressEvent read FOnBeforeRender write SetOnBeforeRender;

    property Interval : Integer read FInterval write FInterval;
  end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses OpenGL1x;

// ------------------
// ------------------ TGLActorProxy ------------------
// ------------------

// Create
//
function TGLActorProxy.BoneMatrix(BoneIndex: integer): TMatrix;
begin
     if BoneIndex<FBonesMatrices.count then
        result := TBoneMatrixObj(FBonesMatrices.Objects[BoneIndex]).Matrix;
end;

function TGLActorProxy.BoneMatrix(BoneName: string): TMatrix;
var
  i: Integer;
begin
     i := FBonesMatrices.IndexOf(BoneName);
     if i>-1 then
        result := TBoneMatrixObj(FBonesMatrices.Objects[i]).Matrix;
end;

procedure TGLActorProxy.BoneMatricesClear;
var
  i: Integer;
begin
     for i:=0 to FBonesMatrices.Count-1 do
     begin
          TBoneMatrixObj(FBonesMatrices.Objects[i]).free;
     end;
     FBonesMatrices.Clear;
end;


constructor TGLActorProxy.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationMode := pamInherited;
  ProxyOptions := ProxyOptions - [pooTransformation];
  FBonesMatrices:=TStringList.create;
  FStoredBoneNames:=TStringList.create;
  FStoreBonesMatrix:=false; // default is false to speed up a little if we don't need bones info
  FInterval:=100;
end;

// DoProgress
//
destructor TGLActorProxy.Destroy;
begin
  BoneMatricesClear;
  FBonesMatrices.free;
  FStoredBoneNames.free;
  inherited;
end;

procedure TGLActorProxy.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  FCurrentTime := progressTime;  
  //FCurrentFrameDelta := FCurrentFrameDelta + (progressTime.deltaTime * 1000) / FInterval;
end;

// DoRender
//
procedure TGLActorProxy.DoRender(var ARci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  // TGLActorProxy specific
  cf, sf, ef: Integer;
  cfd: Single;
  ival: Integer;
  // General proxy stuff.
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
  MasterActor: TGLActor;
begin
  try
    MasterActor := GetMasterActorObject;
    gotMaster := MasterActor <> nil;
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions) and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          glMultMatrixf(PGLFloat(MasterActor.MatrixAsAddress));

        // At last TGLActorProxy specific stuff!
          cfd := MasterActor.CurrentFrameDelta;
          cf := MasterActor.CurrentFrame;
          sf := MasterActor.startframe;
          ef := MasterActor.endframe;
          ival := MasterActor.Interval;

          case FAnimationMode of
            pamInherited: MasterActor.CurrentFrameDelta := FCurrentFrameDelta;
            pamPlayOnce:
              begin
                if (FLastFrame <> FEndFrame - 1) then
                  MasterActor.CurrentFrameDelta := FCurrentFrameDelta
                else
                begin
                  FCurrentFrameDelta := 0;
                  FAnimationMode := pamNone;
                end;
              end;
            pamNone: MasterActor.CurrentFrameDelta := 0;
          else
            Assert(False, 'Unsupported animation mode');
          end;

          MasterActor.CurrentFrameDelta := FCurrentFrameDelta;

          MasterActor.Interval := FInterval;

          MasterActor.SetCurrentFrameDirect(FCurrentFrame);
          FLastFrame := FCurrentFrame;
          MasterActor.StartFrame := FStartFrame;
          MasterActor.EndFrame := FEndFrame;

          //if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
          //  MasterActor.Material.QuickAssignMaterial(
          //                             FMaterialLibrary, FMasterLibMaterial);

          MasterActor.DoProgress(FCurrentTime);

          if Assigned(FOnBeforeRender) then
             FOnBeforeRender(self,FCurrentTime.deltaTime,FCurrentTime.newTime);

          MasterActor.DoRender(ARci,ARenderSelf,MasterActor.Count>0);

          // Stores Bones matrices of the current frame
          if (FStoreBonesMatrix) and (MasterActor.Skeleton<>nil) then
             DoStoreBonesMatrices;

          FCurrentFrameDelta := MasterActor.CurrentFrameDelta;
          FCurrentFrame := MasterActor.CurrentFrame;

          MasterActor.CurrentFrameDelta := cfd;
          MasterActor.SetCurrentFrameDirect(cf);
          MasterActor.CurrentFrame := cf;
          MasterActor.startframe := sf;
          MasterActor.endframe := ef;
          MasterActor.Interval := ival;

        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterActor.Effects.RenderPostEffects(Scene.CurrentBuffer, ARci);
  finally
    ClearStructureChanged;
  end;
end;

procedure TGLActorProxy.DoStoreBonesMatrices;
var
   i,n:integer;
   Bmo:TBoneMatrixObj;
   Bone:TSkeletonBone;
begin
     if FStoredBoneNames.count>0 then
     begin
        // If we specified some bone names, only those bones matrices will be stored (save some cpu)
        if FBonesMatrices.Count<FStoredBoneNames.Count then
        begin
             n := FBonesMatrices.Count;
             for i := n to FStoredBoneNames.Count-1 do
             begin
                  Bone := MasterObject.Skeleton.BoneByName(FStoredBoneNames[i]);
                  if Bone <>nil then
                  begin
                       Bmo := TBoneMatrixObj.Create;
                       Bmo.BoneName:=Bone.Name;
                       Bmo.BoneIndex:=Bone.BoneID;
                       FBonesMatrices.AddObject(Bone.Name,Bmo);
                  end;

             end;
        end;
     end
     else
     begin
        // Add (missing) TBoneMatrixObjects (actually ony 1st time) from all bones in skeleton
        if FBonesMatrices.Count<MasterObject.Skeleton.BoneCount-1 then // note : BoneCount actually returns 1 count more.
        begin
             n := FBonesMatrices.Count;
             for i := n to MasterObject.Skeleton.BoneCount-2 do  // note : BoneCount actually returns 1 count more.
             begin
                  Bone := MasterObject.Skeleton.BoneByID(i);
                  if Bone<>nil then
                  begin
                       Bmo := TBoneMatrixObj.Create;
                       Bmo.BoneName:=Bone.Name;
                       Bmo.BoneIndex:=Bone.BoneID;
                       FBonesMatrices.AddObject(Bone.Name,Bmo);
                  end;

             end;
        end;
     end;


     // fill FBonesMatrices list
     for i:=0 to FBonesMatrices.count-1 do
     begin
          Bmo := TBoneMatrixObj(FBonesMatrices.Objects[i]);
          Bmo.Matrix := MasterObject.Skeleton.BoneByID(Bmo.BoneIndex).GlobalMatrix;
     end;
end;

// GetMasterObject
//
function TGLActorProxy.GetMasterActorObject: TGLActor;
begin
  Result := TGLActor(inherited MasterObject);
end;
{
function TGLActorProxy.GetLibMaterialName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;
}

function TGLActorProxy.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLActorProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

function TGLActorProxy.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint, intersectNormal: PVector): Boolean;
begin
  if MasterObject <> nil then
    Result := RayCastIntersectEx(GetMasterActorObject, rayStart, rayVector,
                                               intersectPoint, intersectNormal)
  else
    Result := inherited RayCastIntersect(rayStart, rayVector, intersectPoint,
                                                               intersectNormal);
end;

// Gain access to TGLDummyActor.DoAnimate().
type TGLDummyActor = class(TGLActor);

function TGLActorProxy.RayCastIntersectEx(RefActor: TGLActor; const rayStart,
  rayVector: TVector; intersectPoint, intersectNormal: PVector): Boolean;
var
   localRayStart, localRayVector : TVector;
   cf, sf, ef: Integer;
   cfd: Single;
   HaspooTransformation:boolean;
begin
   // Set RefObject frame as current ActorProxy frame
   //with RefActor do
   //begin
     // VARS FOR ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
     cfd := RefActor.CurrentFrameDelta;
     cf := RefActor.CurrentFrame;
     sf := RefActor.startframe;
     ef := RefActor.endframe;
     RefActor.CurrentFrameDelta := self.CurrentFrameDelta;
     RefActor.SetCurrentFrameDirect(self.CurrentFrame);
     RefActor.StartFrame := self.StartFrame;
     RefActor.EndFrame := self.EndFrame;
     RefActor.CurrentFrame := self.CurrentFrame;

     // FORCE ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
     TGLDummyActor(RefActor).DoAnimate();

     HaspooTransformation:=pooTransformation in self.ProxyOptions;

     // transform RAYSTART
     SetVector(localRayStart, self.AbsoluteToLocal(rayStart));
     if not HaspooTransformation then
        SetVector(localRayStart, RefActor.LocalToAbsolute(localRayStart));

     // transform RAYVECTOR
     SetVector(localRayVector, self.AbsoluteToLocal(rayVector));
     if not HaspooTransformation then
        SetVector(localRayVector, RefActor.LocalToAbsolute(localRayVector));


     NormalizeVector(localRayVector);

     Result:=RefActor.RayCastIntersect(localRayStart, localRayVector,
                                           intersectPoint, intersectNormal);
     if Result then begin
        if Assigned(intersectPoint) then
        begin
           if not HaspooTransformation then
              SetVector(intersectPoint^, RefActor.AbsoluteToLocal(intersectPoint^));
           SetVector(intersectPoint^, self.LocalToAbsolute(intersectPoint^));
        end;
        if Assigned(intersectNormal) then
        begin
           if not HaspooTransformation then
              SetVector(intersectNormal^, RefActor.AbsoluteToLocal(intersectNormal^));
           SetVector(intersectNormal^, self.LocalToAbsolute(intersectNormal^));
        end;
     end;


     // Return RefObject to it's old time
     RefActor.CurrentFrameDelta:=cfd;
     RefActor.SetCurrentFrameDirect(cf);
     
     RefActor.CurrentFrame:=cf;
     RefActor.startframe:=sf;
     RefActor.endframe:=ef;

     // REVERT ACTOR TO ASSUME ORIGINAL ANIMATION FRAME
     TGLDummyActor(RefActor).DoAnimate();
  // end;
end;

// SetAnimation
//
procedure TGLActorProxy.SetAnimation(const Value: TActorAnimationName);
var
  anAnimation : TActorAnimation;
begin
  // We first assign the value (for persistency support), then check it.
  FAnimation := Value;

  if Assigned(MasterObject) then
  begin
    anAnimation := GetMasterActorObject.Animations.FindName(Value);
    if Assigned(anAnimation) then
    begin
      FStartFrame := anAnimation.StartFrame;
      FEndFrame := anAnimation.EndFrame;
      FCurrentFrame := FStartFrame;
      FLastFrame := FCurrentFrame;
    end;
  end;
end;

procedure TGLActorProxy.SwitchToAnimation(animationIndex : Integer);
var
  anAnimation : TActorAnimation;
begin
  if Assigned(MasterObject) then
  begin
    anAnimation := GetMasterActorObject.Animations[animationIndex];
    if Assigned(anAnimation) then
    begin
      FStartFrame := anAnimation.StartFrame;
      FEndFrame := anAnimation.EndFrame;
      FCurrentFrame := FStartFrame;
      FLastFrame := FCurrentFrame;
    end;
  end;
end;

procedure TGLActorProxy.SetAnimationRange(startIndex, endIndex : Integer);
begin
  FStartFrame := startIndex;
  FEndFrame := endIndex;
  //FCurrentFrame := FStartFrame;
  //FLastFrame := FCurrentFrame;
end;

procedure TGLActorProxy.SetStoredBoneNames(const Value: TStrings);
begin
  if value<>nil then
     FStoredBoneNames.Assign(Value);
end;

// SetMasterObject
//
procedure TGLActorProxy.SetMasterActorObject(const Value: TGLActor);
begin
  inherited SetMasterObject(Value);
  BoneMatricesClear;
end;


procedure TGLActorProxy.SetLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    //if not (csLoading in ComponentState) then
    //  raise ETexture.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TGLActorProxy.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

procedure TGLActorProxy.SetOnBeforeRender(const Value: TGLProgressEvent);
begin
  FOnBeforeRender := Value;
end;

procedure TGLActorProxy.SetStoreBonesMatrix(const Value: boolean);
begin
  FStoreBonesMatrix := Value;
end;

{ TGLMaterialProxy }
{
constructor TGLMaterialProxy.Create(AOwner: TComponent);
begin
  inherited;
  // Nothing here.
end;

destructor TGLMaterialProxy.Destroy;
begin
  // Nothing here.
  inherited;
end;

procedure TGLMaterialProxy.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
   gotMaster, masterGotEffects, oldProxySubObject : Boolean;
begin
   if FRendering then Exit;
   FRendering:=True;
   try
      gotMaster:=Assigned(MasterObject);
      masterGotEffects:=gotMaster and (pooEffects in ProxyOptions)
                        and (MasterObject.Effects.Count>0);
      if gotMaster then begin
         if pooObjects in ProxyOptions then begin
            oldProxySubObject:=ARci.proxySubObject;
            ARci.proxySubObject:=True;
            if pooTransformation in ProxyOptions then
               glMultMatrixf(PGLFloat(MasterObject.MatrixAsAddress));

            if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
              GetMasterMaterialObject.Material.QuickAssignMaterial(
                                         FMaterialLibrary, FMasterLibMaterial);

            MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
            ARci.proxySubObject:=oldProxySubObject;
         end;
      end;
      // now render self stuff (our children, our effects, etc.)
      if ARenderChildren and (Count>0) then
         Self.RenderChildren(0, Count-1, ARci);
      if masterGotEffects then
         MasterObject.Effects.RenderPostEffects(ARci);
   finally
      FRendering:=False;
   end;
   ClearStructureChanged;
end;

function TGLMaterialProxy.GetMasterLibMaterialName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TGLMaterialProxy.GetMasterMaterialObject: TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(inherited MasterObject);
end;

function TGLMaterialProxy.GetMaterialLibrary: TGLMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLMaterialProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

procedure TGLMaterialProxy.SetMasterLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TGLMaterialProxy.SetMasterMaterialObject(
  const Value: TGLCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

procedure TGLMaterialProxy.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetMasterLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;
}

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLActorProxy]);

end.

