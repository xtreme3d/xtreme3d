unit GLODEVehicle;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, GLUtils, GLMisc, GLODEManager,
  GLTerrainRenderer, GLObjects;

type

  TGLODEVehicle = class;

  TGLODEVehicleWheel = class(TGLBaseSceneObject)
  protected
    FVehicle: TGLODEVehicle;
    FRadius: Single;
    FSuspensionMaxLength: Single;
    FStiffness: Single;
    FDamping: Single;
    FCompression: Single;
    FSuspensionLength: Single;
    FSuspensionLengthPrev: Single;
    FForcePosition: TAffineVector;
    FWheelPivot: TGLDummyCube;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetVehicleFromParent;
    property Vehicle: TGLODEVehicle read FVehicle;
    property Radius: Single read FRadius write FRadius;
    property SuspensionMaxLength: Single read FSuspensionMaxLength write FSuspensionMaxLength;
    property Stiffness: Single read FStiffness write FStiffness;
    property Damping: Single read FDamping write FDamping;
    property Compression: Single read FCompression write FCompression;
    property SuspensionLength: Single read FSuspensionLength write FSuspensionLength;
    property SuspensionLengthPrev: Single read FSuspensionLengthPrev write FSuspensionLengthPrev;
    property ForcePosition: TAffineVector read FForcePosition write FForcePosition;
    property WheelPivot: TGLDummyCube read FWheelPivot;
  end;

  TGLODEVehicle = class(TGLBaseSceneObject)
  protected
    FRaycastScene: TGLBaseSceneObject;
    procedure ApplySuspensionForce(wheel: TGLODEVehicleWheel; dt: double);
    procedure RaycastWheel(wheel: TGLODEVehicleWheel);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
    function AddWheel(position: TAffineVector): TGLODEVehicleWheel;
    property RaycastScene: TGLBaseSceneObject read FRaycastScene write FRaycastScene;
  end;

implementation

constructor TGLODEVehicleWheel.Create(AOwner: Tcomponent);
begin
  inherited;
  FRadius := 1.0;
  FStiffness := 200.0;
  FDamping := 0.5;
  FCompression := 0.0;
  FSuspensionLength := 0.0;
  FSuspensionLengthPrev := 0.0;
  FSuspensionMaxLength := 0.0;
  FForcePosition := AffineVectorMake(0.0, 0.0, 0.0);
  FWheelPivot := TGLDummyCube.CreateAsChild(self);
end;

procedure TGLODEVehicleWheel.SetVehicleFromParent;
begin
  FVehicle := TGLODEVehicle(Parent);
end;

constructor TGLODEVehicle.Create(AOwner: Tcomponent);
begin
  inherited;
end;

procedure TGLODEVehicle.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer; 
  wheel: TGLODEVehicleWheel; 
begin
  inherited;
  for i := 0 to Count-1 do
  begin
    if Children[i].ClassType = TGLODEVehicleWheel then
    begin
      wheel := TGLODEVehicleWheel(Children[i]);
      RaycastWheel(wheel);
      ApplySuspensionForce(wheel, progressTime.deltaTime);
    end;
  end;
end;

function TGLODEVehicle.AddWheel(position: TAffineVector): TGLODEVehicleWheel;
var
  wheel: TGLODEVehicleWheel;
begin
  wheel := TGLODEVehicleWheel.CreateAsChild(self);
  wheel.SetVehicleFromParent;
  wheel.Position.AsAffineVector := position;
  result := wheel;
end;

procedure TGLODEVehicle.ApplySuspensionForce(wheel: TGLODEVehicleWheel; dt: double);
var
  dyna: TGLODEDynamic;
  springForceVec: TAffineVector;
  dampingForceVec: TAffineVector;
  springForce: Single;
  dampingForce: Single;
begin
  dyna := TGLODEDynamic(Behaviours.GetByClass(TGLODEDynamic));
  if dyna <> nil then
  begin
    springForce := wheel.Compression * wheel.Stiffness;
    dampingForce := ((wheel.SuspensionLengthPrev - wheel.SuspensionLength) * wheel.Damping) / dt;
    springForceVec := AffineVectorMake(0, 1, 0);
    dampingForceVec := AffineVectorMake(0, 1, 0);
    ScaleVector(springForceVec, springForce);
    ScaleVector(dampingForceVec, dampingForce);
    dyna.AddForceAtPos(springForceVec, wheel.ForcePosition);
    dyna.AddForceAtPos(dampingForceVec, wheel.ForcePosition);
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

procedure TGLODEVehicle.RaycastWheel(wheel: TGLODEVehicleWheel);
var
  rayDist, suspToGround: Single;
  point: TAffineVector;
begin
  if FRaycastScene = nil then
    Exit;

  point := AffineVectorMake(0, 0, 0);
  suspToGround := 100;
  if ObjRaycast(wheel, FRaycastScene, point, rayDist) then
  begin
    suspToGround := rayDist;
    wheel.ForcePosition := point;
  end;
  if suspToGround > wheel.SuspensionMaxLength + wheel.Radius then
  begin
    wheel.Compression := 0;
    wheel.SuspensionLengthPrev := 0;
    wheel.SuspensionLength := 0;
    wheel.WheelPivot.Position.Y := -wheel.SuspensionMaxLength;
  end
  else
  begin
    wheel.SuspensionLengthPrev := wheel.SuspensionLength;
    wheel.SuspensionLength := suspToGround - wheel.Radius;
    wheel.Compression := wheel.SuspensionMaxLength - wheel.SuspensionLength;
    wheel.WheelPivot.Position.Y := -wheel.SuspensionLength;
  end;
end;

end.
