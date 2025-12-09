function OdeManagerCreate(): real; cdecl;
begin
  ode := TGLODEManager.Create(nil);
  dWorldSetAutoDisableFlag(ode.World, 0);
  ode.RenderPoint := TGLRenderPoint.CreateAsChild(scene.Objects);
  jointList := TGLODEJointList.Create(nil);
  odeRagdollWorld := TGLODERagdollWorld.CreateFrom(ode.world, ode.space, ode.contactgroup);
  result := 1.0;
end;

function OdeManagerDestroy(): real; cdecl;
begin
  ode.Destroy;
  result := 1.0;
end;

function OdeManagerStep(dt: real): real; cdecl;
begin
  ode.Step(dt);
  result := 1.0;
end;

function OdeManagerGetNumContactJoints(): real; cdecl;
begin
  result := ode.NumContactJoints;
end;

function OdeManagerSetGravity(x,y,z: real): real; cdecl;
begin
  ode.Gravity.SetVector(x, y, z);
  result := 1.0;
end;

function OdeManagerSetSolver(osm: real): real; cdecl;
begin
  if osm = 0 then ode.Solver := osmDefault;
  if osm = 1 then ode.Solver := osmStepFast;
  if osm = 2 then ode.Solver := osmQuickStep; 
  result := 1.0;
end;

function OdeManagerSetIterations(iterations: real): real; cdecl;
begin
  ode.Iterations := trunc(iterations);
  result := 1.0;
end;

function OdeManagerSetMaxContacts(maxcontacts: real): real; cdecl;
begin
  ode.MaxContacts := trunc(maxcontacts);
  result := 1.0;
end;

function OdeManagerSetVisible(mode: real): real; cdecl;
begin
  ode.Visible := Boolean(trunc(mode));
  ode.VisibleAtRunTime := Boolean(trunc(mode));
  result := 1.0;
end;

function OdeManagerSetGeomColor(colorDynamicDisabled, colorDynamicEnabled, colorStatic: real): real; cdecl;
begin
  ode.GeomColorDynD.AsWinColor := TColor(trunc(colorDynamicDisabled));
  ode.GeomColorDynD.Alpha := 1.0;
  ode.GeomColorDynE.AsWinColor := TColor(trunc(colorDynamicEnabled));
  ode.GeomColorDynE.Alpha := 1.0;
  ode.GeomColorStat.AsWinColor := TColor(trunc(colorStatic));
  ode.GeomColorStat.Alpha := 1.0;
  result := 1.0;
end;

function OdeWorldSetAutoDisableFlag(flag: real): real; cdecl;
begin
  dWorldSetAutoDisableFlag(ode.World, trunc(flag));
  result := 1.0;
end;

function OdeWorldSetAutoDisableLinearThreshold(velocity: real): real; cdecl;
begin
  dWorldSetAutoDisableLinearThreshold(ode.World, velocity);
  result := 1.0;
end;

function OdeWorldSetAutoDisableAngularThreshold(velocity: real): real; cdecl;
begin
  dWorldSetAutoDisableAngularThreshold(ode.World, velocity);
  result := 1.0;
end;

function OdeWorldSetAutoDisableSteps(steps: real): real; cdecl;
begin
  dWorldSetAutoDisableSteps(ode.World, trunc(steps));
  result := 1.0;
end;

function OdeWorldSetAutoDisableTime(time: real): real; cdecl;
begin
  dWorldSetAutoDisableTime(ode.World, time);
  result := 1.0;
end;

function OdeStaticCreate(obj: real): real; cdecl;
var
  stat: TGLODEStatic;
begin
  stat := GetOrCreateOdeStatic(TGLBaseSceneObject(RealToPtr(obj)));
  stat.Manager := ode;
  result := 1.0;
end;

function OdeDynamicCreate(obj: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOrCreateOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.Manager := ode;
  result := 1.0;
end;

// Note: Terrain/Trimesh collision is not supported
function OdeTerrainCreate(terr: real): real; cdecl;
var
  hf: TGLODEHeightField;
begin
  hf := GetOrCreateODEHeightField(TGLBaseSceneObject(RealToPtr(terr)));
  hf.Manager := ode;
  result := 1.0;
end;

function OdeDynamicCalculateMass(obj: real): real; cdecl;
var
  dyna: TGLODEDynamic;
  m: TdMass;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  m := dyna.CalculateMass;
  dBodySetMass(dyna.Body, @m);
  result := 1.0;
end;

function OdeDynamicCalibrateCenterOfMass(obj: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.CalibrateCenterOfMass;
  result := 1.0;
end;

function OdeDynamicAlignObject(obj: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AlignObject;
  result := 1.0;
end;

function OdeDynamicEnable(obj, mode: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.Enabled := Boolean(trunc(mode));
  result := 1.0;
end;

function OdeDynamicSetAutoDisableFlag(obj, mode: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dBodySetAutoDisableFlag(dyna.Body, trunc(mode));
  result := 1.0;
end;

function OdeDynamicSetAutoDisableLinearThreshold(obj, velocity: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dBodySetAutoDisableLinearThreshold(dyna.Body, velocity);
  result := 1.0;
end;

function OdeDynamicSetAutoDisableAngularThreshold(obj, velocity: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dBodySetAutoDisableAngularThreshold(dyna.Body, velocity);
  result := 1.0;
end;

function OdeDynamicSetAutoDisableSteps(obj, steps: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dBodySetAutoDisableSteps(dyna.Body, trunc(steps));
  result := 1.0;
end;

function OdeDynamicSetAutoDisableTime(obj, time: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dBodySetAutoDisableTime(dyna.Body, time);
  result := 1.0;
end;

function OdeDynamicAddForce(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddForce(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicAddForceAtPos(obj, x, y, z, px, py, pz: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddForceAtPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddForceAtRelPos(obj, x, y, z, px, py, pz: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddForceAtRelPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddRelForce(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddRelForce(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicAddRelForceAtPos(obj, x, y, z, px, py, pz: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddRelForceAtPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddRelForceAtRelPos(obj, x, y, z, px, py, pz: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddRelForceAtRelPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddTorque(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddTorque(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicAddRelTorque(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.AddRelTorque(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeAddBox(obj, x, y, z, w, h, d: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TGLODEElementBox;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  elem := nil;
  if stat <> nil then
    elem := TGLODEElementBox(stat.AddNewElement(TGLODEElementBox));
  if dyna <> nil then
    elem := TGLODEElementBox(dyna.AddNewElement(TGLODEElementBox));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      BoxWidth := w;
      BoxHeight := h;
      BoxDepth := d;
    end;
    result := ObjToReal(elem);
    Exit;
  end;
  result := 0.0;
end;

function OdeAddSphere(obj, x, y, z, r: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TGLODEElementSphere;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  elem := nil;
  if stat <> nil then
    elem := TGLODEElementSphere(stat.AddNewElement(TGLODEElementSphere));
  if dyna <> nil then
    elem := TGLODEElementSphere(dyna.AddNewElement(TGLODEElementSphere));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
    end;
    result := ObjToReal(elem);
    Exit;
  end;
  result := 0.0;
end;

// Note: Plane/Trimesh collision is not supported
function OdeAddPlane(obj: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TGLODEElementBase;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  elem := nil;
  if stat <> nil then
    elem := stat.AddNewElement(TGLODEElementBase);
  if dyna <> nil then
    elem := dyna.AddNewElement(TGLODEElementBase);
  if elem <> nil then
  begin  
    result := ObjToReal(elem);
    Exit;
  end;
  result := 0.0;
end;

// Note: Cylinder/Trimesh collision is not supported
function OdeAddCylinder(obj, x, y, z, len, r: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TGLODEElementCylinder;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  elem := nil;
  if stat <> nil then
    elem := TGLODEElementCylinder(stat.AddNewElement(TGLODEElementCylinder));
  if dyna <> nil then
    elem := TGLODEElementCylinder(dyna.AddNewElement(TGLODEElementCylinder));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
      Length := len;
    end;
    result := ObjToReal(elem);
    Exit;
  end;
  result := 0.0;
end;

function OdeAddCapsule(obj, x, y, z, len, r: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TGLODEElementCapsule;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  elem := nil;
  if stat <> nil then
    elem := TGLODEElementCapsule(stat.AddNewElement(TGLODEElementCapsule));
  if dyna <> nil then
    elem := TGLODEElementCapsule(dyna.AddNewElement(TGLODEElementCapsule));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
      Length := len;
    end;
    result := ObjToReal(elem);
    Exit;
  end;
  result := 0.0;
end;

// Note Trimes/Trimesh collision is not supported
function OdeAddTriMesh(obj, mesh: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TGLODEElementTriMesh;
  m: Integer;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  elem := nil;
  if stat <> nil then
    elem := TGLODEElementTriMesh(stat.AddNewElement(TGLODEElementTriMesh));
  if dyna <> nil then
    elem := TGLODEElementTriMesh(dyna.AddNewElement(TGLODEElementTriMesh));
  m := trunc(mesh);
  if elem <> nil then
  begin
    with elem do
    begin
      Vertices := TGLFreeform(o).MeshObjects.Items[m].ExtractTriangles;
      Indices := BuildVectorCountOptimizedIndices(Vertices);
    end;
    result := ObjToReal(elem);
    Exit;
  end;
  result := 0.0;
end;

function OdeElementSetDensity(element, density: real): real; cdecl;
var
  elem: TGLODEElementBase;
begin
  elem := TGLODEElementBase(RealToPtr(element));
  elem.Density := density;
  result := 1.0;
end;

function OdeSurfaceEnableRollingFrictionCoeff(obj, mode: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.RollingFrictionEnabled := Boolean(trunc(mode));
  result := 1.0;
end;

function OdeSurfaceSetRollingFrictionCoeff(obj, rfc: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.RollingFrictionCoeff := rfc;
  result := 1.0;
end;

function OdeSurfaceSetMode(obj, Mu2, FDir1, Bounce, SoftERP, SoftCFM, Motion1, Motion2, Slip1, Slip2 : real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
  surf: TGLODECollisionSurface;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
  begin
    surf := beh.Surface;
    surf.SurfaceMode := [];
    if Mu2 = 1     then surf.SurfaceMode := surf.SurfaceMode + [csmMu2];
    if FDir1 = 1   then surf.SurfaceMode := surf.SurfaceMode + [csmFDir1];
    if Bounce = 1  then surf.SurfaceMode := surf.SurfaceMode + [csmBounce];
    if SoftERP = 1 then surf.SurfaceMode := surf.SurfaceMode + [csmSoftERP];
    if SoftCFM = 1 then surf.SurfaceMode := surf.SurfaceMode + [csmSoftCFM];
    if Motion1 = 1 then surf.SurfaceMode := surf.SurfaceMode + [csmMotion1];
    if Motion2 = 1 then surf.SurfaceMode := surf.SurfaceMode + [csmMotion2];
    if Slip1 = 1   then surf.SurfaceMode := surf.SurfaceMode + [csmSlip1];
    if Slip2 = 1   then surf.SurfaceMode := surf.SurfaceMode + [csmSlip2];
  end;
  result := 1.0;
end;

function OdeSurfaceSetMu(obj, mu: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Mu := mu;
  result := 1.0;
end;

function OdeSurfaceSetMu2(obj, mu2: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Mu2 := mu2;
  result := 1.0;
end;

function OdeSurfaceSetBounce(obj, bounce: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Bounce := bounce;
  result := 1.0;
end;

function OdeSurfaceSetBounceVel(obj, vel: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Bounce_Vel := vel;
  result := 1.0;
end;

function OdeSurfaceSetSoftERP(obj, erp: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.SoftERP := erp;
  result := 1.0;
end;

function OdeSurfaceSetSoftCFM(obj, cfm: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.SoftCFM := cfm;
  result := 1.0;
end;

function OdeSurfaceSetMotion1(obj, motion1: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Motion1 := motion1;
  result := 1.0;
end;

function OdeSurfaceSetMotion2(obj, motion2: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Motion2 := motion2;
  result := 1.0;
end;

function OdeSurfaceSetSlip1(obj, slip1: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Slip1 := slip1;
  result := 1.0;
end;

function OdeSurfaceSetSlip2(obj, slip2: real): real; cdecl;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(RealToPtr(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Slip2 := slip2;
  result := 1.0;
end;

function OdeAddJointBall: real; cdecl;
var
  j: TGLODEJointBall;
begin
  j := TGLODEJointBall.Create(jointList.Joints);
  j.Manager := ode;
  result := ObjToReal(j);
end;

function OdeAddJointFixed: real; cdecl;
var
  j: TGLODEJointFixed;
begin
  j := TGLODEJointFixed.Create(jointList.Joints);
  j.Manager := ode;
  result := ObjToReal(j);
end;

function OdeAddJointHinge: real; cdecl;
var
  j: TGLODEJointHinge;
begin
  j := TGLODEJointHinge.Create(jointList.Joints);
  j.Manager := ode;
  result := ObjToReal(j);
end;

function OdeAddJointHinge2: real; cdecl;
var
  j: TGLODEJointHinge2;
begin
  j := TGLODEJointHinge2.Create(jointList.Joints);
  j.Manager := ode;
  result := ObjToReal(j);
end;

function OdeAddJointSlider: real; cdecl;
var
  j: TGLODEJointSlider;
begin
  j := TGLODEJointSlider.Create(jointList.Joints);
  j.Manager := ode;
  result := ObjToReal(j);
end;

function OdeAddJointUniversal: real; cdecl;
var
  j: TGLODEJointUniversal;
begin
  j := TGLODEJointUniversal.Create(jointList.Joints);
  j.Manager := ode;
  result := ObjToReal(j);
end;

function OdeJointSetObjects(joint, obj1, obj2: real): real; cdecl;
var
  j: TGLODEJointBase;
  o1: TGLBaseSceneObject;
  o2: TGLBaseSceneObject;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  o1 := TGLBaseSceneObject(RealToPtr(obj1));
  o2 := TGLBaseSceneObject(RealToPtr(obj2));
  j.Object1 := o1;
  j.Object2 := o2;
  result := 1.0;
end;

function OdeJointEnable(joint, mode: real): real; cdecl;
var
  j: TGLODEJointBase;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  j.Enabled := Boolean(trunc(mode));
  result := 1.0;
end;

function OdeJointInitialize(joint: real): real; cdecl;
var
  j: TGLODEJointBase;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  j.Initialize;
  result := 1.0;
end;

function OdeJointSetAnchor(joint, x, y, z: real): real; cdecl;
var
  j: TGLODEJointBase;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  if j is TGLODEJointHinge then
    TGLODEJointHinge(j).Anchor.SetPoint(x, y, z)
  else if j is TGLODEJointBall then
    TGLODEJointBall(j).Anchor.SetPoint(x, y, z)
  else if j is TGLODEJointHinge2 then
    TGLODEJointHinge2(j).Anchor.SetPoint(x, y, z)
  else if j is TGLODEJointUniversal then
    TGLODEJointUniversal(j).Anchor.SetPoint(x, y, z);
  result := 1.0;
end;

function OdeJointSetAnchorAtObject(joint, obj: real): real; cdecl;
var
  j: TGLODEJointBase;
  o: TGLBaseSceneObject;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  o := TGLBaseSceneObject(RealToPtr(obj));
  if j is TGLODEJointHinge then
    TGLODEJointHinge(j).Anchor.SetPoint(o.AbsolutePosition)
  else if j is TGLODEJointBall then
    TGLODEJointBall(j).Anchor.SetPoint(o.AbsolutePosition)
  else if j is TGLODEJointHinge2 then
    TGLODEJointHinge2(j).Anchor.SetPoint(o.AbsolutePosition)
  else if j is TGLODEJointUniversal then
    TGLODEJointUniversal(j).Anchor.SetPoint(o.AbsolutePosition);
  result := 1.0;
end;

function OdeJointSetAxis1(joint, x, y, z: real): real; cdecl;
var
  j: TGLODEJointBase;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  if j is TGLODEJointHinge then
    TGLODEJointHinge(j).Axis.SetVector(x, y, z)
  else if j is TGLODEJointHinge2 then
    TGLODEJointHinge2(j).Axis1.SetVector(x, y, z)
  else if j is TGLODEJointUniversal then
    TGLODEJointUniversal(j).Axis1.SetVector(x, y, z);
  result := 1.0;
end;

function OdeJointSetAxis2(joint, x, y, z: real): real; cdecl;
var
  j: TGLODEJointBase;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  if j is TGLODEJointHinge2 then
    TGLODEJointHinge2(j).Axis2.SetVector(x, y, z)
  else if j is TGLODEJointUniversal then
    TGLODEJointUniversal(j).Axis2.SetVector(x, y, z);
  result := 1.0;
end;

function OdeJointSetBounce(joint, axis, bounce: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.Bounce := bounce;
  result := 1.0;
end;

function OdeJointSetCFM(joint, axis, cfm: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.CFM := cfm;
  result := 1.0;
end;

function OdeJointSetFMax(joint, axis, fmax: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.FMax := fmax;
  result := 1.0;
end;

function OdeJointSetFudgeFactor(joint, axis, ffactor: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.FudgeFactor := ffactor;
  result := 1.0;
end;

function OdeJointSetHiStop(joint, axis, histop: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.HiStop := histop;
  result := 1.0;
end;

function OdeJointSetLoStop(joint, axis, lostop: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.LoStop := lostop;
  result := 1.0;
end;

function OdeJointSetStopCFM(joint, axis, cfm: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.StopCFM := cfm;
  result := 1.0;
end;

function OdeJointSetStopERP(joint, axis, erp: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.StopERP := erp;
  result := 1.0;
end;

function OdeJointSetVel(joint, axis, velocity: real): real; cdecl;
var
  j: TGLODEJointBase;
  p: TGLODEJointParams;
begin
  j := TGLODEJointBase(RealToPtr(joint));
  p := getJointAxisParams(j, trunc(axis));
  if p <> nil then
    p.Vel := velocity;
  result := 1.0;
end;

function OdeRagdollCreate(actor: real): real; cdecl;
var
  act: TGLActor;
  ragdoll: TGLODERagdoll;
begin
  act := TGLActor(RealToPtr(actor));
  ragdoll := TGLODERagdoll.Create(act);
  ragdoll.ODEWorld := odeRagdollWorld;
  ragdoll.GLSceneRoot := scene.Objects;
  ragdoll.ShowBoundingBoxes := False;
  result := ObjToReal(ragdoll);
end;

function OdeRagdollHingeJointCreate(x, y, z, lostop, histop: real): real; cdecl;
var
  hjoint: TGLODERagdollHingeJoint;
begin
  hjoint := TGLODERagdollHingeJoint.Create(AffineVectorMake(x, y, z), lostop, histop);
  result := ObjToReal(hjoint);
end;

function OdeRagdollUniversalJointCreate(x1, y1, z1, lostop1, histop1, x2, y2, z2, lostop2, histop2: real): real; cdecl;
var
  ujoint: TGLODERagdollUniversalJoint;
begin
  ujoint := TGLODERagdollUniversalJoint.Create(
    AffineVectorMake(x1, y1, z1), lostop1, histop1,
    AffineVectorMake(x2, y2, z2), lostop2, histop2);
  result := ObjToReal(ujoint);
end;

function OdeRagdollDummyJointCreate: real; cdecl;
var
  djoint: TGLODERagdollDummyJoint;
begin
  djoint := TGLODERagdollDummyJoint.Create;
  result := ObjToReal(djoint);
end;

function OdeRagdollBoneCreate(rag, ragjoint, boneid, parentbone: real): real; cdecl;
var
  ragdoll: TGLODERagdoll;
  bone: TGLODERagdollBone;
begin
  ragdoll := TGLODERagdoll(RealToPtr(rag));
  if not (parentbone = 0) then
    bone := TGLODERagdollBone.CreateOwned(TGLODERagdollBone(RealToPtr(parentbone)))
  else
  begin
    bone := TGLODERagdollBone.Create(ragdoll);
    ragdoll.SetRootBone(bone);
  end;
  bone.Joint := TGLRagdolJoint(RealToPtr(ragjoint));
  bone.BoneID := trunc(boneid);
  //bone.Name := IntToStr(bone.BoneID);
  result := ObjToReal(bone);
end;

function OdeRagdollBuild(rag: real): real; cdecl;
var
  ragdoll: TGLODERagdoll;
begin
  ragdoll := TGLODERagdoll(RealToPtr(rag));
  ragdoll.BuildRagdoll;
  result := 1.0;
end;

function OdeRagdollEnable(rag, mode: real): real; cdecl;
var
  ragdoll: TGLODERagdoll;
begin
  ragdoll := TGLODERagdoll(RealToPtr(rag));
  if (Boolean(trunc(mode))) then
    ragdoll.Start
  else
    ragdoll.Stop;
  result := 1.0;
end;

function OdeRagdollUpdate(rag: real): real; cdecl;
var
  ragdoll: TGLODERagdoll;
begin
  ragdoll := TGLODERagdoll(RealToPtr(rag));
  ragdoll.Update;
  result := 1.0;
end;

function OdeDynamicSetVelocity(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.SetVelocity(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicSetAngularVelocity(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.SetAngularVelocity(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicGetVelocity(obj, ind: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  result := dyna.GetVelocity.V[trunc(ind)];
end;

function OdeDynamicGetAngularVelocity(obj, ind: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  result := dyna.GetAngularVelocity.V[trunc(ind)];
end;

function OdeDynamicSetPosition(obj, x, y, z: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.SetPosition(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicSetRotationQuaternion(obj, x, y, z, w: real): real; cdecl;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(RealToPtr(obj)));
  dyna.SetRotation(x, y, z, w);
  result := 1.0;
end;

