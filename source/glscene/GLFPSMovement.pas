//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFPSMovement<p>

   FPS-like movement behaviour and manager.<p>

	<b>History : </b><font size=-1><ul>
      <li>08/03/06 - ur - Fixed warnigs for Delphi 2006
      <li>02/12/04 - DB - Fixed memory leak, spotted by dikoe Kenguru
      <li>03/07/04 - LR - Corrections for Linux compatibility
                          Replace GetTickCount by GLGetTickCount
      <li>19/06/2004 -Mrqzzz - fixed SphereSweepAndSlide to work for scaled freeforms (SphereRadiusRel)
      <li>14/06/04 - Mathx - Preventing repeated maps when adding through maps.addMap
      <li>09/06/04 - Mathx - Creation
	</ul></font>
}
unit GLFPSMovement;

interface

uses
     OpenGL1x, VectorGeometry, GLMisc, GLScene, GLVectorFileObjects, GLTexture,
     VectorLists, XCollection, Classes, GLGeomObjects, SysUtils,
     GLNavigator;

type
     TContactPoint = record
       intPoint,intNormal:TVector;
     end;

     TCollisionState=class
       Position:TVector;
       Contact:TContactPoint;
       Time:Int64;
     end;

     TCollisionStates=class(TList)
     end;

     TGLBFPSMovement = class;

     TGLMapCollectionItem = class(TXCollectionItem)
     private
          FMap: TGLFreeForm;
          FMapName: string;
          FCollisionGroup: integer;

          procedure setMap(value: TGLFreeForm);
     protected
         procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;
         procedure Loaded; override;
     public
          constructor Create(aOwner : TXCollection); override;
          class function FriendlyName : String; override;
     published

          property Map: TGLFreeForm read FMap write SetMap;

          {: Indicates the collision group of this map. A Collision Group
             is a set of logical maps and movers that can collide between
             themselves (i.e. a Behaviour with group 1 can only collide with
             maps that are also on group 1).
          }
          property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;
     end;
     TGLMapCollectionItemClass = class of TGLMapCollectionItem;

     TGLMapCollection = class(TXCollection)
     public
          class function ItemsClass : TXCollectionItemClass; override;
          function addMap(map: TGLFreeForm; collisionGroup: integer = 0): TGLMapCollectionItem;
          function findMap(mapFreeForm: TGLFreeForm): TGLMapCollectionItem;
     end;

     TGLFPSMovementManager = class(TComponent)
     private
          FNavigator: TGLNavigator;
          FDisplayTime: integer;
          FMovementScale: single;
          FMaps: TGLMapCollection;
          FScene: TGLScene;

          procedure SetNavigator(value: TGLNavigator);
          procedure setScene(value: TGLScene);
          procedure DrawArrows(intPoint,intNormal,Ray:TVector;
                               Arrow1, Arrow2:TGLArrowLine);
     protected
          procedure Loaded; override;
          procedure DefineProperties(Filer: TFiler); override;
          procedure WriteMaps(stream : TStream);
          procedure ReadMaps(stream : TStream);
          procedure Notification(AComponent: TComponent;
                                 Operation: TOperation); override;
     public
          constructor Create(aOwner: TComponent); override;
          destructor Destroy; override;

          // Basic idea is to OctreeSphereSweepIntersect to plane, update position then change
          //  velocity to slide along the plane
          //  Camera can collide with multiple planes (e.g. floor + multiple walls + ceiling)
          // limit iterations to 4 or 5 for now, may need to be higher for more complex maps or fast motion
          function SphereSweepAndSlide(freeform:TGLFreeform;
                                        behaviour: TGLBFPSMovement;
                                        SphereStart:TVector;
                                        var Velocity, newPosition:TVector;
                                        sphereRadius: single): boolean; overload;

          procedure SphereSweepAndSlide(behaviour: TGLBFPSMovement;
                                        SphereStart:TVector;
                                        var Velocity, newPosition:TVector;
                                        sphereRadius: single); overload;

     published
          property Maps: TGLMapCollection read FMaps write FMaps;
          property Navigator: TGLNavigator read FNavigator write SetNavigator;
          property Scene: TGLScene read FScene write setScene;

          {: Display Time for the arrow lines. }
          property DisplayTime: integer read FDisplayTIme write FDisplayTime;
          property MovementScale: single read FMovementScale write FMovementScale;
     end;

     TGLBFPSMovement = class(TGLBehaviour)
     private
          FManager: TGLFPSMovementManager;
          CollisionStates: TCollisionStates;
          ArrowLine1, ArrowLine2, ArrowLine3,
            ArrowLine4, ArrowLine5, ArrowLine6: TGLArrowLine;
          dirGl: TGLDirectOpenGL;
          tickCount: int64;

          oldPosition: TVector;

          FGravityEnabled: boolean;

          FSphereRadius: single;
          FShowArrows: boolean;
          FCollisionGroup: integer;
          FManagerName: string;

          procedure setShowArrows(value: boolean);
          procedure RenderArrowLines(Sender: TObject;
                                     var rci: TRenderContextInfo);
     protected
         procedure WriteToFiler(writer : TWriter); override;
         procedure ReadFromFiler(reader : TReader); override;
         procedure Loaded; override;
     public
          velocity: TVector;

          constructor Create(aOwner : TXCollection); override;
          destructor Destroy; override;

          procedure DoProgress(const progressTime : TProgressTimes); override;

          class function FriendlyName: string; override;

          Procedure   TurnHorizontal(Angle : Single);
          Procedure   TurnVertical(Angle : Single);
          Procedure   MoveForward(Distance : Single);
          Procedure   StrafeHorizontal(Distance : Single);
          Procedure   StrafeVertical(Distance : Single);
          Procedure   Straighten;
     published
          property Manager: TGLFPSMovementManager read FManager write FManager;

          {:
             Radius to execute the testing with. A value < 0 indicates to use
             the boundingSphereRadius of the object.
          }
          property SphereRadius: single read FSphereRadius write FSphereRadius;

          {: Show Arrows and trailing for debuging. }
          property ShowArrows: boolean read FShowArrows write SetShowArrows;

          {: Indicates the collision group of this behaviour. A Collision Group
             is a set of logical maps and movers that can collide between
             themselves (i.e. a Behaviour with group 1 can only collide with
             maps that are also on group 1).
          }
          property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;

          property GravityEnabled: boolean read FGravityEnabled write FGravityEnabled;
     end;

function GetFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
function GetFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;
function GetOrCreateFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
function GetOrCreateFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;

procedure Register;

implementation

uses GLCrossPlatform;

procedure register;
begin
     RegisterComponents('GLScene Utils', [TGLFPSMovementManager]);
end;

function GetFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBFPSMovement);
  if i >= 0 then
    Result := TGLBFPSMovement(behaviours[i])
  else
    Result := nil;
end;

function GetFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;
begin
  Result := GetFpsMovement(obj.behaviours);
end;

function GetOrCreateFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBFPSMovement);
  if i >= 0 then
    Result := TGLBFPSMovement(behaviours[i])
  else
    Result := TGLBFPSMovement.Create(behaviours);
end;

function GetOrCreateFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;
begin
  Result := GetOrCreateFpsMovement(obj.behaviours);
end;

// ------------------
// ------------------ TGLMapCollectionItem ------------------
// ------------------
constructor TGLMapCollectionItem.Create(aOwner : TXCollection);
begin
     inherited create(aOwner);
                                   
     FCollisionGroup:= 0;
end;

procedure TGLMapCollectionItem.setMap(value: TGLFreeForm);
begin
     assert(owner.owner.InheritsFrom(TGLFPSMovementManager));
     if assigned(FMap) then FMap.RemoveFreeNotification(TComponent(owner.owner));
     FMap:= value;
     if assigned(FMap) then FMap.FreeNotification(TComponent(owner.Owner));
end;

procedure TGLMapCollectionItem.WriteToFiler(writer : TWriter);
begin
     inherited WriteToFiler(writer);
     with writer do begin
          writeInteger(0); //ArchiveVersion
          WriteInteger(FCollisionGroup);
          if assigned(FMap) then
               WriteString(FMap.Name)
          else
               writeString('');
     end;
end;

procedure TGLMapCollectionItem.ReadFromFiler(reader : TReader);
var
archiveVersion: integer;
begin
     inherited ReadFromFiler(reader);
     with reader do begin
          archiveVersion:= readInteger;
          assert(archiveVersion = 0, 'Wrong ArchiveVersion for TGLMapCollectionItem');
          FCollisionGroup:= ReadInteger;
          FMapName:= ReadString;
     end;
end;

procedure TGLMapCollectionItem.Loaded;
begin
     if FMapName <> '' then begin
          assert(Owner.Owner.InheritsFrom(TGLFPSMovementManager));
          Map:= TGLFreeForm(TGLFPSMovementManager(owner.owner).Scene.FindSceneObject(FMapName));
     end;
end;

class function TGLMapCollectionItem.FriendlyName : String;
begin
   Result:='FPSMovementMap';
end;

// ------------------
// ------------------ TGLMapCollection ------------------
// ------------------
class function TGLMapCollection.ItemsClass : TXCollectionItemClass;
begin
     result:= TGLMapCollectionItem;
end;

function TGLMapCollection.addMap(map: TGLFreeForm;
collisionGroup: integer = 0): TGLMapCollectionItem;
begin
     //no repeated maps (would only present delays...)
     result:= findMap(map);
     if assigned(result) then exit;

     result:= TGLMapCollectionItem.Create(self);
     result.Map:= map;
     result.CollisionGroup:= collisionGroup;
     add(result);
end;

function TGLMapCollection.findMap(mapFreeForm: TGLFreeForm): TGLMapCollectionItem;
var
i: integer;
aux: TGLMapCollectionItem;
begin
     result:= nil;
     for i:= 0 to count -1 do begin
          aux:= TGLMapCollectionItem(Items[i]);
          if aux.Map = mapFreeForm then begin
               result:= aux;
               break;
          end;
     end;
end;

// ------------------
// ------------------ TGLFPSMovementManager ------------------
// ------------------
constructor TGLFPSMovementManager.Create(aOwner: TComponent);
begin
     inherited Create(aOwner);

     Maps:= TGLMapCollection.Create(self);

     MovementScale:= 4.0;
     DisplayTime:= 2000;

     RegisterManager(self);
end;

destructor TGLFPSMovementManager.Destroy;
begin
     DeRegisterManager(self);
     maps.Free;
     inherited Destroy;
end;

procedure TGLFPSMovementManager.Loaded;
begin
     inherited Loaded;
     if assigned(FMaps) then Maps.Loaded;
end;

// DefineProperties
//
procedure TGLFPSMovementManager.DefineProperties(Filer: TFiler);
begin
   inherited;
   {FOriginalFiler := Filer;}

   Filer.DefineBinaryProperty('MapsData',
                              ReadMaps, WriteMaps,
                              (Assigned(FMaps) and (FMaps.Count>0)));
   {FOriginalFiler:=nil;}
end;

// WriteBehaviours
//
procedure TGLFPSMovementManager.WriteMaps(stream : TStream);
var
   writer : TWriter;
begin
   writer:=TWriter.Create(stream, 16384);
   try
      Maps.WriteToFiler(writer);
   finally
      writer.Free;
   end;
end;

// ReadBehaviours
//
procedure TGLFPSMovementManager.ReadMaps(stream : TStream);
var
   reader : TReader;
begin
   reader:=TReader.Create(stream, 16384);
    try
       Maps.ReadFromFiler(reader);
    finally
       reader.Free;
    end;
end;

procedure TGLFPSMovementManager.SetNavigator(value: TGLNavigator);
begin
     if assigned(FNavigator) then FNavigator.RemoveFreeNotification(self);
     FNavigator:= value;
     if assigned(value) then value.FreeNotification(self);
end;

procedure TGLFPSMovementManager.setScene(value: TGLScene);
begin
     if assigned(FScene) then FScene.RemoveFreeNotification(self);
     FScene:= value;
     if assigned(FScene) then FScene.FreeNotification(self);
end;

procedure TGLFPSMovementManager.Notification(AComponent: TComponent;
Operation: TOperation);
var
map: TGLMapCollectionItem;
begin
     inherited Notification(aComponent, operation);

     if Operation <> opRemove then exit;

     if (aComponent = FNavigator) then
          navigator:= nil;
     if (aComponent = FScene) then
          FScene:= nil;
     if aComponent.InheritsFrom(TGLFreeForm) then begin
          map:= Maps.findMap(TGLFreeForm(aComponent));
          if assigned(map) then map.Map:= nil;
     end;
end;

procedure TGLFPSMovementManager.DrawArrows(intPoint,intNormal,Ray:TVector;
Arrow1, Arrow2:TGLArrowLine);
begin
     Arrow1.Position.AsVector:=intPoint;
     Arrow1.Direction.AsVector:=intNormal;
     Arrow1.Scale.z:=VectorLength(intNormal);
     Arrow1.Move(Arrow1.Scale.z/2);
     Arrow1.Visible:=True;

     Arrow2.Position.AsVector:=intPoint;
     Arrow2.Direction.AsVector:=Ray;
     Arrow2.Visible:=True;
end;

procedure TGLFPSMovementManager.SphereSweepAndSlide(behaviour: TGLBFPSMovement;
SphereStart:TVector; var Velocity,newPosition:TVector; sphereRadius: single);
var
i: integer;
map: TGLMapCollectionItem;
begin
     for i:= 0 to Maps.Count -1 do begin
          map:= TGLMapCollectionItem(Maps.GetItems(i));
          if map.CollisionGroup = behaviour.CollisionGroup then
               SphereSweepAndSlide(map.Map, behaviour, sphereStart,
                                      velocity, newPosition, sphereRadius)
     end;
end;

function TGLFPSMovementManager.SphereSweepAndSlide(freeform:TGLFreeform;
behaviour: TGLBFPSMovement; SphereStart:TVector;
var Velocity,newPosition:TVector; sphereRadius: single): boolean;
var
  oldPosition, ray:TVector;
  vel,slidedistance:Single;
  intPoint,intNormal:TVector;
  newDirection, newRay,collisionPosition, pointOnSphere,point2OnSphere:TVector;
  i:integer;
  CollisionState:TCollisionState;
  SphereRadiusRel : single; //mrqzzz
begin
  SphereRadiusRel:=SphereRadius/freeform.Scale.x; // could be Scale.y, or Scale.z assuming they are the same

  oldPosition:=SphereStart;

  result:= true;

  //Direction sphere is moving in
  ray:=VectorSubtract(newPosition,oldPosition);
//  ray:=Velocity;
//  newPosition:=VectorAdd(newPosition,ray);
  //Speed of sphere
  vel:=VectorLength(ray);

  //if the Sphere is not moving, nothing is required
  // else do up to 7 loops

  if vel>0 then
  for i:=0 to 6 do
  begin
    //if an intersection occurs, will need to do further calculations
    if (freeform.OctreeSphereSweepIntersect(oldPosition,ray,vel,SphereRadiusRel,@intPoint,@intNormal)) then
    begin
      if VectorDistance2(oldPosition,intPoint)<=sqr(SphereRadius) then
      begin
        //sphere is intersecting triangle
        intNormal:=VectorScale(VectorSubtract(oldPosition,intPoint),1.0001);
      end
      else
      begin
        //sphere is not intersecting triangle
        //intNormal:=VectorSubtract(oldPosition,intPoint);  //not correct but works okay at small time steps
        //intNormal:=VectorScale(VectorNormalize(intNormal),SphereRadius+0.0001);
        if RayCastSphereInterSect(intPoint,VectorNormalize(VectorNegate(ray)),oldPosition,SphereRadius,PointOnSphere,Point2OnSphere)>0 then
          intNormal:=VectorScale(VectorSubtract(oldPosition,PointOnSphere),1.0001)
          //intNormal:=VectorScale(VectorNormalize(VectorSubtract(oldPosition,PointOnSphere)),SphereRadius+0.001)//VectorDistance(oldPosition,PointOnSphere));
        else
        begin
//          Assert(False);  //this shouldn't happen (this is here for debugging)
          intNormal:=VectorScale(VectorSubtract(oldPosition,intPoint),1.0001);
        end;

      end;

      //calculate position of centre of sphere when collision occurs
      collisionPosition:=VectorAdd(intPoint,intNormal);
      oldPosition:=collisionPosition;

      //calculate distance that wasn't travelled, due to obstacle
      newRay:=VectorSubtract(newPosition,collisionPosition);

      //calculate new direction when a wall is hit (could add bouncing to this)
      newDirection:=VectorCrossProduct(intNormal,VectorCrossProduct(newRay,intNormal));
      if VectorNorm(NewDirection)>0 then
        NormalizeVector(newDirection);

      //calculate distance that it should slide (depends on angle between plane & ray)
      SlideDistance:=vectorDotProduct(newRay,newDirection);
      //still need to implement friction properly
//      if abs(SlideDistance)<10*deltaTime then SlideDistance:=0;
      ScaleVector(newDirection,SlideDistance);

      //calculate new position sphere is heading towards
      newPosition:=VectorAdd(collisionPosition,newDirection);
      ray:=newDirection;
      vel:=VectorLength(ray);
      
      //display arrows for collision normals & slide direction
      if (i=0) and (behaviour.ShowArrows) then
        DrawArrows(intPoint,intNormal,Ray, behaviour.ArrowLine1, behaviour.ArrowLine4)
      else if (i=1) and (behaviour.ShowArrows) then
        DrawArrows(intPoint,intNormal,Ray, behaviour.ArrowLine2, behaviour.ArrowLine5)
      else if (i=2)  and (behaviour.ShowArrows) then
        DrawArrows(intPoint,intNormal,Ray, behaviour.ArrowLine3, behaviour.ArrowLine6)
      else if i=6 then
      begin
//        caption:=FloatToStr(vectordistance(newPosition,oldPosition));
        newPosition:=oldPosition;
        break;
      end;

      //check if very small motion (e.g. when stuck in a corner)
      if vel<1E-10 then//deltaTime then
      begin
        newPosition:=oldPosition;
        break;
      end;

      CollisionState:=TCollisionState.Create();
      CollisionState.Position:=oldPosition;
      CollisionState.Contact.intNormal:=intNormal;
      CollisionState.Contact.intPoint:=intPoint;
      CollisionState.Time:=GLGetTickCount();

      behaviour.CollisionStates.Add(CollisionState);

    end
    else //no collision occured, so quit loop
    begin
      if i = 0 then result:= false;
      Break;
    end;
  end; //end i loop
  Velocity:=Ray;
end;


// ------------------
// ------------------ TGLBFPSMovement ------------------
// ------------------

constructor TGLBFPSMovement.Create(aOwner : TXCollection);

     procedure setupArrow(arrow: TGLArrowLine; color: TDelphiColor);
     begin with arrow do begin
          slices:= 16; stacks:= 4; TopArrowHeadHeight:= 0.1;
          TopArrowHeadRadius:= 0.04; TopRadius:= 0.02;
          BottomArrowHeadHeight:= 0.05; BottomArrowHeadRadius:= 0.02;
          BottomRadius:= 0.02;
          Material.FrontProperties.Diffuse.AsWinColor:= color;
     end; end;

begin
     inherited create(aOwner);

     velocity:= NullHmgVector;
     SphereRadius:= -1;
     CollisionGroup:= 0;

     CollisionStates:= TCollisionStates.Create;

     //FIXME: Creating arrows here, but they should be only added when
     //a "showArrows" property changed
     ArrowLine1:= TGLArrowLine.Create(nil); setupArrow(ArrowLine1, clRed);
     ArrowLine2:= TGLArrowLine.Create(nil); setupArrow(ArrowLine2, clGreen);
     ArrowLine3:= TGLArrowLine.Create(nil); setupArrow(ArrowLine3, clBlue);
     ArrowLine4:= TGLArrowLine.Create(nil); setupArrow(ArrowLine4, clSilver);
     ArrowLine5:= TGLArrowLine.Create(nil); setupArrow(ArrowLine5, clSilver);
     ArrowLine6:= TGLArrowLine.Create(nil); setupArrow(ArrowLine6, clSilver);

     dirGl:= TGLDirectOpenGl.create(nil);
     dirGL.OnRender:= RenderArrowLines;

     oldPosition:= OwnerBaseSceneObject.Position.AsVector;
     FManagerName:= '';
end;

destructor TGLBFPSMovement.Destroy;
var
  i:integer;
begin
  // remove all states
  for i:= 0 to CollisionStates.Count-1 do
    TCollisionState(CollisionStates[i]).Free;
  FreeAndNil(collisionStates);
  // remove all objects used to display graphical results of collisions
  FreeAndNil(ArrowLine1);
  FreeAndNil(ArrowLine2);
  FreeAndNil(ArrowLine3);
  FreeAndNil(ArrowLine4);
  FreeAndNil(ArrowLine5);
  FreeAndNil(ArrowLine6);
  FreeAndNil(dirGl);
  inherited Destroy;
end;

class function TGLBFPSMovement.FriendlyName : String;
begin
   Result:='FPS Movement';
end;

procedure TGLBFPSMovement.WriteToFiler(writer : TWriter);
begin
     inherited WriteToFiler(writer);
     with writer do begin
          writeInteger(0); //ArchiveVersion 0 (initial)
          WriteInteger(FCollisionGroup);
          WriteSingle(FSphereRadius);
          WriteBoolean(FGravityEnabled);
          writeBoolean(FShowArrows);
          if assigned(FManager) then
               WriteString(FManager.GetNamePath)
          else
               WriteString('');
     end;
end;

procedure TGLBFPSMovement.ReadFromFiler(reader : TReader);
var
archiveVersion: integer;
begin
     inherited ReadFromFiler(reader);
     with reader do begin
          archiveVersion:= ReadInteger;
          assert(archiveVersion = 0, 'Wrong ArchiveVersion for TGLBFPSMovement');
          CollisionGroup:= ReadInteger;
          SphereRadius:= ReadSingle;
          GravityEnabled:= ReadBoolean;
          ShowArrows:= ReadBoolean;
          FManagerName:= ReadString;
     end;
end;

procedure TGLBFPSMovement.Loaded;
var
   mng : TComponent;
begin
     inherited Loaded;
     if FManagerName <> '' then begin
          mng:=FindManager(TGLFPSMovementManager, FManagerName);
          if Assigned(mng) then
               Manager:=TGLFPSMovementManager(mng);
          FManagerName:='';
     end;
end;

procedure TGLBFPSMovement.setShowArrows(value: boolean);
begin
     FShowArrows:= value;
     dirGL.visible:= value;
     if (OwnerBaseSceneObject <> nil) and not (csDesigning in ownerBaseSceneObject.ComponentState) then begin
          ArrowLine1.MoveTo(OwnerBaseSceneObject.Parent);
          ArrowLine2.MoveTo(OwnerBaseSceneObject.Parent);
          ArrowLine3.MoveTo(OwnerBaseSceneObject.Parent);
          ArrowLine4.MoveTo(OwnerBaseSceneObject.Parent);
          ArrowLine5.MoveTo(OwnerBaseSceneObject.Parent);
          ArrowLine6.MoveTo(OwnerBaseSceneObject.Parent);
          dirGl.MoveTo(OwnerBaseSceneObject.parent);
     end;
end;

procedure TGLBFPSMovement.MoveForward(distance: single);
var
prevObj: TGLBaseSceneObject;
begin
     assert(assigned(manager), 'Manager not assigned on TGLBFPSMovement behaviour!');
     prevObj:= manager.Navigator.MovingObject;
     manager.navigator.MovingObject:= OwnerBaseSceneObject;
     manager.Navigator.MoveForward(distance);
     manager.navigator.MovingObject:= prevObj;
end;

procedure TGLBFPSMovement.StrafeHorizontal(distance: single);
var
prevObj: TGLBaseSceneObject;
begin
     assert(assigned(manager), 'Manager not assigned on TGLBFPSMovement behaviour!');
     prevObj:= manager.Navigator.MovingObject;
     manager.navigator.MovingObject:= OwnerBaseSceneObject;
     manager.Navigator.StrafeHorizontal(distance);
     manager.navigator.MovingObject:= prevObj;
end;

procedure TGLBFPSMovement.StrafeVertical(distance: single);
var
prevObj: TGLBaseSceneObject;
begin
     assert(assigned(manager), 'Manager not assigned on TGLBFPSMovement behaviour!');
     prevObj:= manager.Navigator.MovingObject;
     manager.navigator.MovingObject:= OwnerBaseSceneObject;
     manager.Navigator.StrafeVertical(distance);
     manager.navigator.MovingObject:= prevObj;
end;

procedure TGLBFPSMovement.TurnHorizontal(angle: single);
var
prevObj: TGLBaseSceneObject;
begin
     assert(assigned(manager), 'Manager not assigned on TGLBFPSMovement behaviour!');
     prevObj:= manager.Navigator.MovingObject;
     manager.navigator.MovingObject:= OwnerBaseSceneObject;
     manager.Navigator.TurnHorizontal(angle);
     manager.navigator.MovingObject:= prevObj;
end;

procedure TGLBFPSMovement.TurnVertical(angle: single);
var
prevObj: TGLBaseSceneObject;
begin
     assert(assigned(manager), 'Manager not assigned on TGLBFPSMovement behaviour!');
     prevObj:= manager.Navigator.MovingObject;
     manager.navigator.MovingObject:= OwnerBaseSceneObject;
     manager.Navigator.TurnVertical(angle);
     manager.navigator.MovingObject:= prevObj;
end;

procedure TGLBFPSMovement.Straighten;
var
prevObj: TGLBaseSceneObject;
begin
     assert(assigned(manager), 'Manager not assigned on TGLBFPSMovement behaviour!');
     prevObj:= manager.Navigator.MovingObject;
     manager.navigator.MovingObject:= OwnerBaseSceneObject;
     manager.Navigator.Straighten;
     manager.navigator.MovingObject:= prevObj;
end;

procedure TGLBFPSMovement.DoProgress(const progressTime : TProgressTimes);
var
  newPosition:TVector;
  CollisionState:TCollisionState;
begin
     inherited doProgress(progressTime);

     assert(assigned(manager), 'FPS Manager not assigned to behaviour.');

     //make arrowlines invisible (they are made visible in SphereSweepAndSlide)
     ArrowLine1.Visible:=False;
     ArrowLine2.Visible:=False;
     ArrowLine3.Visible:=False;
     ArrowLine4.Visible:=False;
     ArrowLine5.Visible:=False;
     ArrowLine6.Visible:=False;

     CollisionState:=TCollisionState.Create();
     CollisionState.Position:=oldPosition;
     CollisionStates.Add(CollisionState);

     //this is the position we are trying to move to with controls
     newPosition:= OwnerBaseSceneObject.Position.AsVector;

     //Change in position = velocity * time taken
     if GravityEnabled then
          newPosition[1]:=  newPosition[1]-manager.MovementScale*0.5* progressTime.deltaTime;

     //do some magic!!!  and store new position in newPosition
     if SphereRadius < 0 then
          manager.SphereSweepAndSlide(self,oldPosition,Velocity,newPosition,
                                      OwnerBaseSceneObject.boundingSphereRadius)
     else
          manager.SphereSweepAndSlide(self,oldPosition,Velocity,newPosition,
                                      SphereRadius);

     OwnerBaseSceneObject.Position.AsVector:=newPosition;
     oldPosition:= newPosition;

     if CollisionStates.Count>0 then
     begin
       CollisionState:=TCollisionState(CollisionStates.First);
       TickCount:=GLGetTickCount();
       //remove all old states
       while (CollisionState<>nil)and(CollisionState.Time<TickCount-manager.DisplayTime) do
       begin
         CollisionStates.Remove(CollisionState);
         CollisionState.Free;
         if CollisionStates.Count=0 then Exit;
         CollisionState:=TCollisionState(CollisionStates.First);
       end;
     end;     
end;

procedure TGLBFPSMovement.RenderArrowLines(Sender: TObject;
  var rci: TRenderContextInfo);
var
  x,y,z,t:Single;
  i:integer;
  CollisionState:TCollisionState;
begin
//  caption:= IntToStr(CollisionStates.Count);
  glColor3f(1,1,1);
  glPushAttrib(GL_LIGHTING_BIT);
  glDisable(GL_LIGHTING);
  //draw position trail
  glBegin(GL_LINE_STRIP);
  for i:=0 to CollisionStates.Count-1 do
  begin
    CollisionState:=TCollisionState(CollisionStates.Items[i]);
    x:=CollisionState.Position[0];
    y:=CollisionState.Position[1];
    z:=CollisionState.Position[2];
    glVertex3f(x,y,z);
  end;
  glEnd();
  //draw normals trail
  glBegin(GL_LINES);
  for i:=0 to CollisionStates.Count-1 do
  begin
    CollisionState:=TCollisionState(CollisionStates.Items[i]);
    t:=(Manager.DisplayTime-(TickCount-CollisionState.Time))/manager.DisplayTime;
    glColor3f(t,t,t);
      glvertex3f(CollisionState.Contact.intPoint[0],CollisionState.Contact.intPoint[1],CollisionState.Contact.intPoint[2]);
      glvertex3f(CollisionState.Contact.intPoint[0]+CollisionState.Contact.intNormal[0],//GLSphere4.Radius,
      CollisionState.Contact.intPoint[1]+CollisionState.Contact.intNormal[1],//GLSphere4.Radius,
      CollisionState.Contact.intPoint[2]+CollisionState.Contact.intNormal[2]);//GLSphere4.Radius);
  end;
  glEnd();
  glPopAttrib;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TGLMapCollectionItem);
  RegisterXCollectionItemClass(TGLBFPSMovement);

end.
