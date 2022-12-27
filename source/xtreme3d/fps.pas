function FpsManagerCreate: real; cdecl;
var
  FPSManager: TGLFPSMovementManager;
begin
  FPSManager := TGLFPSMovementManager.Create(scene);
  Result := ObjToReal(FPSManager);
end;

function FpsManagerSetNavigator(man, nav: real): real; cdecl;
begin
  TGLFPSMovementManager(RealToPtr(man)).Navigator := TGLNavigator(RealToPtr(nav));
  Result := 1;
end;

function FpsManagerSetMovementScale(man, scale: real): real; cdecl;
begin
  TGLFPSMovementManager(RealToPtr(man)).MovementScale := scale;
  Result := 1;
end;

function FpsManagerAddMap(man, ffm: real): real; cdecl;
var
  freeform: TGLFreeform;
begin
  freeform := TGLFreeform(RealToPtr(ffm));
  TGLFPSMovementManager(RealToPtr(man)).Maps.addMap(freeform);
  Result := 1;
end;

function FpsManagerRemoveMap(man, ffm: real): real; cdecl;
begin
  TGLFPSMovementManager(RealToPtr(man)).Maps.findMap(TGLFreeform(RealToPtr(ffm))).Free;
  Result := 1;
end;

function FpsManagerMapSetCollisionGroup(man, ffm, group: real): real; cdecl;
begin
  TGLFPSMovementManager(RealToPtr(man)).Maps.findMap(TGLFreeform(RealToPtr(ffm))).CollisionGroup := trunc(group);
  Result := 1;
end;

function FpsSetManager(obj, man: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.Manager := TGLFPSMovementManager(RealToPtr(man));
  Result := 1;
end;

function FpsSetCollisionGroup(obj, group: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.CollisionGroup := trunc(group);
  Result := 1;
end;

function FpsSetSphereRadius(obj, radius: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.SphereRadius := radius;
  Result := 1;
end;

function FpsSetGravity(obj, mode: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.GravityEnabled := Boolean(trunc(mode));
  Result := 1;
end;

function FpsMove(obj, spd: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.MoveForward(spd);
  Result := 1;
end;

function FpsStrafe(obj, spd: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.StrafeHorizontal(spd);
  Result := 1;
end;

function FpsLift(obj, spd: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  fps.StrafeVertical(spd);
  Result := 1;
end;

function FpsGetVelocity(obj, ind: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  fps: TGLBFPSMovement;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  fps := GetOrCreateFPSMovement(ob);
  Result := fps.velocity.v[trunc(ind)];
end;

