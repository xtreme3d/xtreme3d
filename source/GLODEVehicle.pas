unit GLODEVehicle;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, GLUtils, GLMisc, GLODEManager,
  GLTerrainRenderer, GLObjects;

type

  TGLODEVehicle = class;

  TGLODEVehicleSuspension = class(TGLBaseSceneObject)
  protected
    FVehicle: TGLODEVehicle;
    FWheelRadius: Single;
    FMaxLength: Single;
    FStiffness: Single;
    FDamping: Single;
    FCompression: Single;
    FLength: Single;
    FLengthPrev: Single;
    FForcePosition: TAffineVector;
    FWheel: TGLDummyCube;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetVehicleFromParent;
    property Vehicle: TGLODEVehicle read FVehicle;
    property WheelRadius: Single read FWheelRadius write FWheelRadius;
    property MaxLength: Single read FMaxLength write FMaxLength;
    property Stiffness: Single read FStiffness write FStiffness;
    property Damping: Single read FDamping write FDamping;
    property Compression: Single read FCompression write FCompression;
    property Length: Single read FLength write FLength;
    property LengthPrev: Single read FLengthPrev write FLengthPrev;
    property ForcePosition: TAffineVector read FForcePosition write FForcePosition;
    property Wheel: TGLDummyCube read FWheel;
  end;

  TGLODEVehicle = class(TGLBaseSceneObject)
  protected
    FRaycastScene: TGLBaseSceneObject;
    //FSideForce: TAffineVector;
    FForwardForce: Single;
    procedure ApplySuspensionForce(susp: TGLODEVehicleSuspension; dt: double);
    procedure RaycastSuspension(susp: TGLODEVehicleSuspension);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
    function AddSuspension(position: TAffineVector): TGLODEVehicleSuspension;
    property RaycastScene: TGLBaseSceneObject read FRaycastScene write FRaycastScene;
    property ForwardForce: Single read FForwardForce write FForwardForce; 
  end;

implementation

constructor TGLODEVehicleSuspension.Create(AOwner: Tcomponent);
begin
  inherited;
  FWheelRadius := 1.0;
  FStiffness := 200.0;
  FDamping := 0.5;
  FCompression := 0.0;
  FLength := 0.0;
  FLengthPrev := 0.0;
  FMaxLength := 0.0;
  FForcePosition := AffineVectorMake(0.0, 0.0, 0.0);
  FWheel := TGLDummyCube.CreateAsChild(self);
end;

procedure TGLODEVehicleSuspension.SetVehicleFromParent;
begin
  FVehicle := TGLODEVehicle(Parent);
end;

constructor TGLODEVehicle.Create(AOwner: Tcomponent);
begin
  inherited;

  FForwardForce := 0.0;
end;

procedure TGLODEVehicle.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer; 
  susp: TGLODEVehicleSuspension; 
begin
  inherited;
  for i := 0 to Count-1 do
  begin
    if Children[i].ClassType = TGLODEVehicleSuspension then
    begin
      susp := TGLODEVehicleSuspension(Children[i]);
      RaycastSuspension(susp);
      ApplySuspensionForce(susp, progressTime.deltaTime);
    end;
  end;
end;

function TGLODEVehicle.AddSuspension(position: TAffineVector): TGLODEVehicleSuspension;
var
  susp: TGLODEVehicleSuspension;
begin
  susp := TGLODEVehicleSuspension.CreateAsChild(self);
  susp.SetVehicleFromParent;
  susp.Position.AsAffineVector := position;
  result := susp;
end;

procedure TGLODEVehicle.ApplySuspensionForce(susp: TGLODEVehicleSuspension; dt: double);
var
  dyna: TGLODEDynamic;
  bodyVelocity: TAffineVector;
  bodyAngVelocity: TAffineVector;
  radiusVector: TAffineVector;
  pointVelocity: TAffineVector;
  forwardSpeed: Single;
  wheelRollSpeed: Single;
  springForceVec: TAffineVector;
  dampingForceVec: TAffineVector;
  springForce: Single;
  dampingForce: Single;
  sideForce: TAffineVector;
  sideSpeed: Single;
  forwardDir: TAffineVector;
  maxFrictionForce: Single;
  groundForce: TAffineVector;
begin
  dyna := TGLODEDynamic(Behaviours.GetByClass(TGLODEDynamic));
  if dyna <> nil then
  begin
    springForce := susp.Compression * susp.Stiffness;
    dampingForce := ((susp.LengthPrev - susp.Length) * susp.Damping) / dt;
    springForceVec := AffineVectorMake(0, 1, 0);
    dampingForceVec := AffineVectorMake(0, 1, 0);
    ScaleVector(springForceVec, springForce);
    ScaleVector(dampingForceVec, dampingForce);
    dyna.AddForceAtPos(springForceVec, susp.ForcePosition);
    dyna.AddForceAtPos(dampingForceVec, susp.ForcePosition);

    bodyVelocity := AffineVectorMake(dyna.Body.lvel[0], dyna.Body.lvel[1], dyna.Body.lvel[2]);
    bodyAngVelocity := AffineVectorMake(dyna.Body.avel[0], dyna.Body.avel[1], dyna.Body.avel[2]);

    forwardSpeed := VectorDotProduct(bodyVelocity, AffineVectorMake(AbsoluteDirection));
    wheelRollSpeed := -forwardSpeed / susp.WheelRadius;
    susp.Wheel.Pitch(RadToDeg(wheelRollSpeed * dt));

    sideForce := AffineVectorMake(susp.AbsoluteRight);
    radiusVector := VectorSubtract(susp.ForcePosition, AffineVectorMake(AbsolutePosition));
    pointVelocity := VectorAdd(bodyVelocity, VectorCrossProduct(bodyAngVelocity, radiusVector));
    sideSpeed := VectorDotProduct(pointVelocity, sideForce);
    ScaleVector(sideForce, -sideSpeed);// * 0.05
    //dyna.AddForceAtPos(sideForce, susp.ForcePosition);

    if Abs(forwardSpeed) < 10.0 then
    begin
      forwardDir := AffineVectorMake(AbsoluteDirection);
      ScaleVector(forwardDir, FForwardForce);
      //dyna.AddForce(forwardDir);
    end
    else
      forwardDir := AffineVectorMake(0, 0, 0);

    maxFrictionForce := (springForce + dampingForce) * 0.5;
    groundForce := VectorAdd(forwardDir, sideForce);
    if VectorLength(groundForce) > maxFrictionForce then
    begin
      if VectorLength(groundForce) > EPSILON then
        groundForce := VectorNormalize(groundForce);
      ScaleVector(groundForce, maxFrictionForce);
    end;

    dyna.AddForce(groundForce);
  end;
end;

function ObjRaycast(obj, target: TGLBaseSceneObject; var point: TAffineVector; var distance: Single): Boolean;
var
  rstart, rdir, ipoint, inorm: TVector;
  tmpPoint: TAffineVector;
  i: Integer;
  maxh, d: Single;
  bestDistance: Single;
  res: Boolean;
begin
  rstart := obj.AbsolutePosition;
  rdir := VectorMake(0, 1, 0, 0); 
  ipoint := VectorMake(0, 0, 0, 0);
  inorm := VectorMake(0, 0, 0, 0);
  bestDistance := 100;
  res := False;
  if target.ClassType = TGLTerrainRenderer then
  begin
    bestDistance := rstart[1] - TGLTerrainRenderer(target).InterpolatedHeight(rstart);
    point := AffineVectorMake(rstart[0], bestDistance, rstart[2]);
    res := true;
  end
  else
  begin
    if target.RayCastIntersect(rstart, rdir, @ipoint, @inorm) then
    begin
      bestDistance := rstart[1] - ipoint[1];
      point := AffineVectorMake(ipoint[0], ipoint[1], ipoint[2]);
      res := true;
    end;
  end;
  for i := 0 to target.Count-1 do
  begin
    if ObjRaycast(obj, target.Children[i], tmpPoint, d) then
    begin
      if d < bestDistance then
      begin
        point := tmpPoint;
        bestDistance := d;
        res := true;
      end;
    end;
  end;
  distance := bestDistance;
  result := res;
end;

procedure TGLODEVehicle.RaycastSuspension(susp: TGLODEVehicleSuspension);
var
  rayDist, suspToGround: Single;
  point: TAffineVector;
begin
  if FRaycastScene = nil then
  begin
    susp.ForcePosition := AffineVectorMake(susp.AbsolutePosition[0], 0.0, susp.AbsolutePosition[2]);
    suspToGround := susp.AbsolutePosition[1];
  end
  else
  begin
    point := AffineVectorMake(0, 0, 0);
    suspToGround := 100;
    if ObjRaycast(susp, FRaycastScene, point, rayDist) then
    begin
      suspToGround := rayDist;
      susp.ForcePosition := point;
    end;
  end;
  
  if suspToGround > susp.MaxLength + susp.WheelRadius then
  begin
    susp.Compression := 0;
    susp.LengthPrev := 0;
    susp.Length := 0;
    susp.Wheel.Position.Y := -susp.MaxLength;
  end
  else
  begin
    susp.LengthPrev := susp.Length;
    susp.Length := suspToGround - susp.WheelRadius;
    susp.Compression := susp.MaxLength - susp.Length;
    susp.Wheel.Position.Y := -susp.Length;
  end;
end;

end.
