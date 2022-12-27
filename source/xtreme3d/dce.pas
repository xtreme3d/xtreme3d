// DCE functions wrapper by Hacker

function DceManagerCreate: real; cdecl;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager.Create(scene);
  DCEManager.ManualStep := true;
  Result := ObjToReal(DCEManager);
end;

function DceManagerStep(man, dt: real): real; cdecl;
begin
  TGLDCEManager(RealToPtr(man)).Step(dt);
  Result := 1;
end;

function DceManagerSetGravity(man, grav: real): real; cdecl;
begin
  TGLDCEManager(RealToPtr(man)).Gravity := grav;
  Result := 1;
end;

function DceManagerSetWorldDirection(man, x, y, z: real): real; cdecl;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager(RealToPtr(man));
  DCEManager.WorldDirection.X := x;
  DCEManager.WorldDirection.Y := y;
  DCEManager.WorldDirection.Z := z;
  Result := 1;
end;

function DceManagerSetWorldScale(man, scale: real): real; cdecl;
begin
  TGLDCEManager(RealToPtr(man)).WorldScale := scale;
  Result := 1;
end;

function DceManagerSetMovementScale(man, scale: real): real; cdecl;
begin
  TGLDCEManager(RealToPtr(man)).MovimentScale := scale;
  Result := 1;
end;

function DceManagerSetLayers(man, mode: real): real; cdecl;
var
  DCEManager: TGLDCEManager;
begin
  DCEManager := TGLDCEManager(RealToPtr(man));
  case trunc(mode) of
    0: DCEManager.StandardiseLayers := ccsDCEStandard;
    1: DCEManager.StandardiseLayers := ccsCollisionStandard;
    2: DCEManager.StandardiseLayers := ccsHybrid;
  else
    DCEManager.StandardiseLayers := ccsDCEStandard;
  end;
  Result := 1;
end;

function DceManagerSetManualStep(man, mode: real): real; cdecl;
begin
  TGLDCEManager(RealToPtr(man)).ManualStep := Boolean(trunc(mode));
  Result := 1;
end;

function DceDynamicSetManager(obj, man: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  dyn: TGLDCEDynamic;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  dyn.Manager := TGLDCEManager(RealToPtr(man));
  Result := 1;
end;

function DceDynamicSetActive(obj, mode: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  GetOrCreateDCEDynamic(ob).Active := Boolean(trunc(mode));
  Result := 1;
end;

function DceDynamicIsActive(obj: real): real; cdecl;
begin
  Result := Integer(GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Active);
end;

function DceDynamicSetUseGravity(obj, mode: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).UseGravity := Boolean(trunc(mode));
  Result := 1;
end;

function DceDynamicSetLayer(obj, layer: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Layer := trunc(layer);
  Result := 1;
end;

function DceDynamicGetLayer(obj: real): real; cdecl;
begin
  Result := GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Layer;
end;

function DceDynamicSetSolid(obj, mode: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Solid := Boolean(trunc(mode));
  Result := 1;
end;

function DceDynamicSetFriction(obj, friction: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Friction := friction;
  Result := 1;
end;

function DceDynamicSetBounce(obj, bounce: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).BounceFactor := bounce;
  Result := 1;
end;

function DceDynamicSetSize(obj, x, y, z: real): real; cdecl;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  dyn.Size.X := x;
  dyn.Size.Y := y;
  dyn.Size.Z := z;
  Result := 1;
end;

function DceDynamicSetSlideOrBounce(obj, mode: real): real; cdecl;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  case trunc(mode) of
    0: dyn.SlideOrBounce := csbSlide;
    1: dyn.SlideOrBounce := csbBounce;
  else
    dyn.SlideOrBounce := csbSlide;
  end;
  Result := 1;
end;

function DceDynamicApplyAcceleration(obj, x, y, z: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).ApplyAccel(x, y, z);
  Result := 1;
end;

function DceDynamicApplyAbsAcceleration(obj, x, y, z: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).ApplyAbsAccel(x, y, z);
  Result := 1;
end;

function DceDynamicStopAcceleration(obj: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).StopAccel;
  Result := 1;
end;

function DceDynamicStopAbsAcceleration(obj: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).StopAbsAccel;
  Result := 1;
end;

function DceDynamicJump(obj, height, speed: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Jump(height, speed);
  Result := 1;
end;

function DceDynamicMove(obj, x, y, z, delta: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Move(AffineVectorMake(x, y, z), delta);
  Result := 1;
end;

function DceDynamicMoveTo(obj, x, y, z, amount: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).MoveTo(AffineVectorMake(x, y, z), amount);
  Result := 1;
end;

function DceDynamicSetVelocity(obj, x, y, z: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Speed := AffineVectorMake(x, y, z);
  Result := 1;
end;

function DceDynamicInGround(obj: real): real; cdecl;
begin
  Result := Integer(GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).InGround);
end;

function DceDynamicSetMaxRecursionDepth(obj, depth: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).MaxRecursionDepth := trunc(depth);
  Result := 1;
end;

function DceStaticSetManager(obj, man: real): real; cdecl;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(RealToPtr(obj))).Manager := TGLDCEManager(RealToPtr(man));
  Result := 1;
end;

function DceStaticSetActive(obj, mode: real): real; cdecl;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(RealToPtr(obj))).Active := Boolean(trunc(mode));
  Result := 1;
end;

function DceStaticSetShape(obj, mode: real): real; cdecl;
var
  stat: TGLDCEStatic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOrCreateDCEStatic(ob);
  if mode = 0 then stat.Shape := csEllipsoid;
  if mode = 1 then stat.Shape := csBox;
  if mode = 2 then stat.Shape := csFreeform;
  if mode = 3 then stat.Shape := csTerrain;
  Result := 1;
end;

function DceStaticSetLayer(obj, layer: real): real; cdecl;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(RealToPtr(obj))).Layer := trunc(layer);
  Result := 1;
end;

function DceStaticSetSize(obj, x, y, z: real): real; cdecl;
var
  stat: TGLDCEStatic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOrCreateDCEStatic(ob);
  stat.Size.X := x;
  stat.Size.Y := y;
  stat.Size.Z := z;
  Result := 1;
end;

function DceStaticSetSolid(obj, mode: real): real; cdecl;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(RealToPtr(obj))).Solid := Boolean(trunc(mode));
  Result := 1;
end;

function DceStaticSetFriction(obj, friction: real): real; cdecl;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(RealToPtr(obj))).Friction := friction;
  Result := 1;
end;

function DceStaticSetBounceFactor(obj, bfactor: real): real; cdecl;
begin
  GetOrCreateDCEStatic(TGLBaseSceneObject(RealToPtr(obj))).BounceFactor := bfactor;
  Result := 1;
end;

function DceDynamicGetVelocity(obj, ind: real): real; cdecl;
var
  dyn: TGLDCEDynamic;
  ob: TGLBaseSceneObject;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  dyn := GetOrCreateDCEDynamic(ob);
  Result := dyn.Speed.v[trunc(ind)];
end;

{
function DceDynamicSetAbsVelocity(obj, x, y, z: real): real; cdecl;
begin
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).AbsSpeed := AffineVectorMake(x, y, z);
  Result := 1;
end;
}

{
function DceDynamicGetAbsVelocity(obj, ind: real): real; cdecl;
begin
  Result := GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).AbsSpeed[trunc64(ind)];
end;
}

function DceDynamicApplyImpulse(obj, x, y, z: real): real; cdecl;
var
  imp: TAffineVector;
begin
  imp := GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Speed;
  imp := VectorAdd(imp, AffineVectorMake(x, y, z));
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).Speed := imp;
  Result := 1;
end;

{
function DceDynamicApplyAbsImpulse(obj, x, y, z: real): real; cdecl;
var
  imp: TAffineVector;
begin
  imp := GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).AbsSpeed;
  imp := VectorAdd(imp, AffineVectorMake(x, y, z));
  GetOrCreateDCEDynamic(TGLBaseSceneObject(RealToPtr(obj))).AbsSpeed := imp;
  Result := 1;
end;
}

