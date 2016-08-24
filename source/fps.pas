function FpsManagerCreate: real; stdcall;
var
  FPSManager: TGLFPSMovementManager;
begin
  FPSManager := TGLFPSMovementManager.Create(scene);
  Result := Integer(FPSManager);
end;

function FpsManagerSetNavigator(man, nav: real): real; stdcall;
begin
  TGLFPSMovementManager(trunc64(man)).Navigator := TGLNavigator(trunc64(nav));
  Result := 1;
end;

function FpsManagerSetMovementScale(man, scale: real): real; stdcall;
begin
  TGLFPSMovementManager(trunc64(man)).MovementScale := scale;
  Result := 1;
end;

function FpsManagerAddMap(man, ffm: real): real; stdcall;
var
  freeform: TGLFreeform;
begin
  freeform := TGLFreeform(trunc64(ffm));
  TGLFPSMovementManager(trunc64(man)).Maps.addMap(freeform);
  Result := 1;
end;

function FpsManagerRemoveMap(man, ffm: real): real; stdcall;
begin
  TGLFPSMovementManager(trunc64(man)).Maps.findMap(TGLFreeform(trunc64(ffm))).Free;
  Result := 1;
end;

function FpsManagerMapSetCollisionGroup(man, ffm, group: real): real; stdcall;
begin
  TGLFPSMovementManager(trunc64(man)).Maps.findMap(TGLFreeform(trunc64(ffm))).CollisionGroup := trunc64(group);
  Result := 1;
end;

function FpsSetManager(obj, man: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.Manager := TGLFPSMovementManager(trunc64(man));
  Result := 1;
end;

function FpsSetCollisionGroup(obj, group: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.CollisionGroup := trunc64(group);
  Result := 1;
end;

function FpsSetSphereRadius(obj, radius: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.SphereRadius := radius;
  Result := 1;
end;

function FpsSetGravity(obj, mode: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.GravityEnabled := Boolean(trunc64(mode));
  Result := 1;
end;

function FpsMove(obj, spd: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.MoveForward(spd);
  Result := 1;
end;

function FpsStrafe(obj, spd: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.StrafeHorizontal(spd);
  Result := 1;
end;

function FpsLift(obj, spd: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.StrafeVertical(spd);
  Result := 1;
end;

function FpsGetVelocity(obj, ind: real): real; stdcall;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(trunc64(obj));
  fps := GetOrCreateFPSMovement(ob);
  Result := fps.velocity[trunc64(ind)];
end;

// TODO:
// FpsCountCollisions
// FpsClearCollisions
// FpsGetCollisionPosition
// FpsGetCollisionNormal
// FpsGetCollidedObject
