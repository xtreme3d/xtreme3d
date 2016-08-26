// DCE functions wrapper by Hacker

function DceManagerCreate: real; stdcall;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager.Create(scene);
  DCEManager.ManualStep := true;
  Result := Integer(DCEManager);
end;

function DceManagerStep(man, dt: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).Step(dt);
  Result := 1;
end;

function DceManagerSetGravity(man, grav: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).Gravity := grav;
  Result := 1;
end;

function DceManagerSetWorldDirection(man, x, y, z: real): real; stdcall;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager(trunc64(man));
  DCEManager.WorldDirection.X := x;
  DCEManager.WorldDirection.Y := y;
  DCEManager.WorldDirection.Z := z;
  Result := 1;
end;

function DceManagerSetWorldScale(man, scale: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).WorldScale := scale;
  Result := 1;
end;

function DceManagerSetMovementScale(man, scale: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).MovimentScale := scale;
  Result := 1;
end;

function DceManagerSetLayers(man, mode: real): real; stdcall;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager(trunc64(man));
  case trunc64(mode) of
    0: DCEManager.StandardiseLayers := ccsDCEStandard;
    1: DCEManager.StandardiseLayers := ccsCollisionStandard;
    2: DCEManager.StandardiseLayers := ccsHybrid;
  else
    DCEManager.StandardiseLayers := ccsDCEStandard;
  end;
  Result := 1;
end;

function DceManagerSetManualStep(man, mode: real): real; stdcall;
begin
  TGLDCEManager(trunc64(man)).ManualStep := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicSetManager(obj, man: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  dyn: TGLDCEDynamic;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  dyn.Manager := TGLDCEManager(trunc64(man));
  Result := 1;
end;

function DceDynamicSetActive(obj, mode: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  GetOrCreateDCEDynamic(ob).Active := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicIsActive(obj: real): real; stdcall;
begin
  Result := Integer(GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Active);
end;

function DceDynamicSetUseGravity(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).UseGravity := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicSetLayer(obj, layer: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Layer := trunc64(layer);
  Result := 1;
end;

function DceDynamicGetLayer(obj: real): real; stdcall;
begin
  Result := GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Layer;
end;

function DceDynamicSetSolid(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Solid := Boolean(trunc64(mode));
  Result := 1;
end;

function DceDynamicSetFriction(obj, friction: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Friction := friction;
  Result := 1;
end;

function DceDynamicSetBounce(obj, bounce: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).BounceFactor := bounce;
  Result := 1;
end;

function DceDynamicSetSize(obj, x, y, z: real): real; stdcall;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  dyn.Size.X := x;
  dyn.Size.Y := y;
  dyn.Size.Z := z;
  Result := 1;
end;

function DceDynamicSetSlideOrBounce(obj, mode: real): real; stdcall;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  case trunc64(mode) of
    0: dyn.SlideOrBounce := csbSlide;
    1: dyn.SlideOrBounce := csbBounce;
  else
    dyn.SlideOrBounce := csbSlide;
  end;
  Result := 1;
end;

function DceDynamicApplyAcceleration(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).ApplyAccel(x, y, z);
  Result := 1;
end;

function DceDynamicApplyAbsAcceleration(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).ApplyAbsAccel(x, y, z);
  Result := 1;
end;

function DceDynamicStopAcceleration(obj: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).StopAccel;
  Result := 1;
end;

function DceDynamicStopAbsAcceleration(obj: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).StopAbsAccel;
  Result := 1;
end;

function DceDynamicJump(obj, height, speed: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Jump(height, speed);
  Result := 1;
end;

function DceDynamicMove(obj, x, y, z, delta: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Move(AffineVectorMake(x, y, z), delta);
  Result := 1;
end;

function DceDynamicMoveTo(obj, x, y, z, amount: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).MoveTo(AffineVectorMake(x, y, z), amount);
  Result := 1;
end;

function DceDynamicSetVelocity(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Speed := AffineVectorMake(x, y, z);
  Result := 1;
end;

function DceDynamicInGround(obj: real): real; stdcall;
begin
  Result := Integer(GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).InGround);
end;

function DceDynamicSetMaxRecursionDepth(obj, depth: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).MaxRecursionDepth := trunc64(depth);
  Result := 1;
end;

function DceStaticSetManager(obj, man: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Manager :=
    TGLDCEManager(trunc64(man));
  Result := 1;
end;

function DceStaticSetActive(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Active :=
    Boolean(trunc64(mode));
  Result := 1;
end;

function DceStaticSetShape(obj, mode: real): real; stdcall;
var
  stat: TGLDCEStatic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  stat := GetOrCreateDCEStatic(ob);
  if mode = 0 then stat.Shape := csEllipsoid;
  if mode = 1 then stat.Shape := csBox;
  if mode = 2 then stat.Shape := csFreeform;
  if mode = 3 then stat.Shape := csTerrain;
  Result := 1;
end;

function DceStaticSetLayer(obj, layer: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Layer := trunc64(layer);
  Result := 1;
end;

function DceStaticSetSize(obj, x, y, z: real): real; stdcall;
var
  stat: TGLDCEStatic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  stat := GetOrCreateDCEStatic(ob);
  stat.Size.X := x;
  stat.Size.Y := y;
  stat.Size.Z := z;
  Result := 1;
end;

function DceStaticSetSolid(obj, mode: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Solid := Boolean(trunc64(mode));
  Result := 1;
end;

function DceStaticSetFriction(obj, friction: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).Friction := friction;
  Result := 1;
end;

function DceStaticSetBounceFactor(obj, bfactor: real): real; stdcall;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(trunc64(obj))).BounceFactor := bfactor;
  Result := 1;
end;

function DceDynamicGetVelocity(obj, ind: real): real; stdcall;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  Result := dyn.Speed[trunc64(ind)];
end;

function DceDynamicSetAbsVelocity(obj, x, y, z: real): real; stdcall;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).AbsSpeed := AffineVectorMake(x, y, z);
  Result := 1;
end;

function DceDynamicGetAbsVelocity(obj, ind: real): real; stdcall;
begin
  Result := GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).AbsSpeed[trunc64(ind)];
end;

function DceDynamicApplyImpulse(obj, x, y, z: real): real; stdcall;
var
  imp: TAffineVector;
begin
  imp := GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Speed;
  imp := VectorAdd(imp, AffineVectorMake(x, y, z));
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).Speed := imp;
  Result := 1;
end;

function DceDynamicApplyAbsImpulse(obj, x, y, z: real): real; stdcall;
var
  imp: TAffineVector;
begin
  imp := GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).AbsSpeed;
  imp := VectorAdd(imp, AffineVectorMake(x, y, z));
  GetOrCreateDCEDynamic(TGLBaseSceneObject(trunc64(obj))).AbsSpeed := imp;
  Result := 1;
end;

// DceDynamicGetGravity is no longer available

// TODO:
// DceDynamicVelocityCollided
// DceDynamicGravityCollided
// DceDynamicCountCollisions
// DceDynamicGetCollidedObject
// DceDynamicGetCollisionPosition
// DceDynamicGetCollisionNormal
