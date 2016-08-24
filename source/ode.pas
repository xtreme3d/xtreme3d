function OdeManagerCreate(): real; stdcall;
begin
  ode := TGLODEManager.Create(nil);
  dWorldSetAutoDisableFlag(ode.World, 0);
  ode.RenderPoint := TGLRenderPoint.CreateAsChild(scene.Objects);
  jointList := TGLODEJointList.Create(nil);
  result := 1.0;
end;

function OdeManagerDestroy(): real; stdcall;
begin
  ode.Destroy;
  result := 1.0;
end;

function OdeManagerStep(dt: real): real; stdcall;
begin
  ode.Step(dt);
  result := 1.0;
end;

function OdeManagerGetNumContactJoints(): real; stdcall;
begin
  result := ode.NumContactJoints;
end;

function OdeManagerSetGravity(x,y,z: real): real; stdcall;
begin
  ode.Gravity.SetVector(x, y, z);
  result := 1.0;
end;

function OdeManagerSetSolver(osm: real): real; stdcall;
begin
  if osm = 0 then ode.Solver := osmDefault;
  if osm = 1 then ode.Solver := osmStepFast;
  if osm = 2 then ode.Solver := osmQuickStep; 
  result := 1.0;
end;

function OdeManagerSetIterations(iterations: real): real; stdcall;
begin
  ode.Iterations := trunc64(iterations);
  result := 1.0;
end;

function OdeManagerSetMaxContacts(maxcontacts: real): real; stdcall;
begin
  ode.MaxContacts := trunc64(maxcontacts);
  result := 1.0;
end;

function OdeManagerSetVisible(mode: real): real; stdcall;
begin
  ode.Visible := Boolean(trunc64(mode));
  ode.VisibleAtRunTime := Boolean(trunc64(mode));
  result := 1.0;
end;

function OdeManagerSetGeomColor(color: real): real; stdcall;
begin
  ode.GeomColor.AsWinColor := TColor(trunc64(color));
  ode.GeomColor.Alpha := 1.0;
  result := 1.0;
end;

function OdeWorldSetAutoDisableFlag(flag: real): real; stdcall;
begin
  dWorldSetAutoDisableFlag(ode.World, trunc64(flag));
  result := 1.0;
end;

function OdeWorldSetAutoDisableLinearThreshold(velocity: real): real; stdcall;
begin
  dWorldSetAutoDisableLinearThreshold(ode.World, velocity);
  result := 1.0;
end;

function OdeWorldSetAutoDisableAngularThreshold(velocity: real): real; stdcall;
begin
  dWorldSetAutoDisableAngularThreshold(ode.World, velocity);
  result := 1.0;
end;

function OdeWorldSetAutoDisableSteps(steps: real): real; stdcall;
begin
  dWorldSetAutoDisableSteps(ode.World, trunc64(steps));
  result := 1.0;
end;

function OdeWorldSetAutoDisableTime(time: real): real; stdcall;
begin
  dWorldSetAutoDisableTime(ode.World, time);
  result := 1.0;
end;

function OdeStaticCreate(obj: real): real; stdcall;
var
  stat: TGLODEStatic;
begin
  stat := GetOrCreateOdeStatic(TGLBaseSceneObject(trunc64(obj)));
  stat.Manager := ode;
  result := 1.0;
end;

function OdeDynamicCreate(obj: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOrCreateOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.Manager := ode;
  result := 1.0;
end;

// New function
// Note: Terrain/Trimesh collision is not supported
function OdeTerrainCreate(terr: real): real; stdcall;
var
  hf: TGLODEHeightField;
begin
  hf := GetOrCreateODEHeightField(TGLBaseSceneObject(trunc64(terr)));
  hf.Manager := ode;
  result := 1.0;
end;

// New function
function OdeDynamicCalculateMass(obj: real): real; stdcall;
var
  dyna: TGLODEDynamic;
  m: TdMass;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  m := dyna.CalculateMass;
  dBodySetMass(dyna.Body, @m);
end;

function OdeDynamicCalibrateCenterOfMass(obj: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.CalibrateCenterOfMass;
  result := 1.0;
end;

function OdeDynamicAlignObject(obj: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AlignObject;
  result := 1.0;
end;

function OdeDynamicEnable(obj, mode: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.Enabled := Boolean(trunc64(mode));
  result := 1.0;
end;

function OdeDynamicSetAutoDisableFlag(obj, mode: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dBodySetAutoDisableFlag(dyna.Body, trunc64(mode));
  result := 1.0;
end;

function OdeDynamicSetAutoDisableLinearThreshold(obj, velocity: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dBodySetAutoDisableLinearThreshold(dyna.Body, velocity);
  result := 1.0;
end;

function OdeDynamicSetAutoDisableAngularThreshold(obj, velocity: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dBodySetAutoDisableAngularThreshold(dyna.Body, velocity);
  result := 1.0;
end;

function OdeDynamicSetAutoDisableSteps(obj, steps: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dBodySetAutoDisableSteps(dyna.Body, trunc64(steps));
  result := 1.0;
end;

function OdeDynamicSetAutoDisableTime(obj, time: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dBodySetAutoDisableTime(dyna.Body, time);
  result := 1.0;
end;

function OdeDynamicAddForce(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddForce(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicAddForceAtPos(obj, x, y, z, px, py, pz: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddForceAtPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddForceAtRelPos(obj, x, y, z, px, py, pz: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddForceAtRelPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddRelForce(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddRelForce(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicAddRelForceAtPos(obj, x, y, z, px, py, pz: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddRelForceAtPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddRelForceAtRelPos(obj, x, y, z, px, py, pz: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddRelForceAtRelPos(AffineVectorMake(x, y, z), AffineVectorMake(px, py, pz));
  result := 1.0;
end;

function OdeDynamicAddTorque(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddTorque(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicAddRelTorque(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.AddRelTorque(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicGetContactCount(obj: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  result := dyna.NumContacts;
end;

function OdeStaticGetContactCount(obj: real): real; stdcall;
var
  stat: TGLODEStatic;
begin
  stat := TGLODEStatic(TGLBaseSceneObject(trunc64(obj)));
  result := stat.NumContacts;
end;

// TODO:
// OdeDynamicGetContact
// OdeStaticGetContact

// Change from Xtreme3D 2.0: position should be specified
function OdeAddBox(obj, x, y, z, w, h, d: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementBox;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := TODEElementBox(stat.AddNewElement(TODEElementBox));
  if dyna <> nil then
    elem := TODEElementBox(dyna.AddNewElement(TODEElementBox));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      BoxWidth := w;
      BoxHeight := h;
      BoxDepth := d;
    end;
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// Change from Xtreme3D 2.0: position should be specified
function OdeAddSphere(obj, x, y, z, r: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementSphere;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := TODEElementSphere(stat.AddNewElement(TODEElementSphere));
  if dyna <> nil then
    elem := TODEElementSphere(dyna.AddNewElement(TODEElementSphere));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
    end;
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// Note: Plane/Trimesh collision is not supported
function OdeAddPlane(obj: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementBase;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := stat.AddNewElement(TODEElementPlane);
  if dyna <> nil then
    elem := dyna.AddNewElement(TODEElementPlane);
  if elem <> nil then
  begin  
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// Change from Xtreme3D 2.0: position should be specified
// Note: Cylinder/Trimesh collision is not supported
function OdeAddCylinder(obj, x, y, z, len, r: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementCylinder;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := TODEElementCylinder(stat.AddNewElement(TODEElementCylinder));
  if dyna <> nil then
    elem := TODEElementCylinder(dyna.AddNewElement(TODEElementCylinder));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
      Length := len;
    end;
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// Change from Xtreme3D 2.0: position should be specified
function OdeAddCone(obj, x, y, z, len, r: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementCone;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := TODEElementCone(stat.AddNewElement(TODEElementCone));
  if dyna <> nil then
    elem := TODEElementCone(dyna.AddNewElement(TODEElementCone));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
      Length := len;
    end;
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// Change from Xtreme3D 2.0: position should be specified
function OdeAddCapsule(obj, x, y, z, len, r: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementCapsule;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := TODEElementCapsule(stat.AddNewElement(TODEElementCapsule));
  if dyna <> nil then
    elem := TODEElementCapsule(dyna.AddNewElement(TODEElementCapsule));
  if elem <> nil then
  begin
    with elem do
    begin
      Position.SetPoint(x, y, z);
      Radius := r;
      Length := len;
    end;
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// Change from Xtreme3D 2.0: mesh index should be specified
// Note Trimes/Trimesh collision is not supported
function OdeAddTriMesh(obj, mesh: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  stat: TGLODEStatic;
  dyna: TGLODEDynamic;
  elem: TODEElementTriMesh;
  m: Integer;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  stat := GetOdeStatic(o);
  dyna := GetOdeDynamic(o);
  if stat <> nil then
    elem := TODEElementTriMesh(stat.AddNewElement(TODEElementTriMesh));
  if dyna <> nil then
    elem := TODEElementTriMesh(dyna.AddNewElement(TODEElementTriMesh));
  m := trunc64(mesh);
  if elem <> nil then
  begin
    with elem do
    begin
      Vertices := TGLFreeform(o).MeshObjects.Items[m].ExtractTriangles;
      Indices := BuildVectorCountOptimizedIndices(Vertices);
    end;
    result := Integer(elem);
    Exit;
  end;
  result := 0.0;
end;

// New function
function OdeElementSetDensity(element, density: real): real; stdcall;
var
  elem: TODEElementBase;
begin
  elem := TODEElementBase(trunc64(element));
  elem.Density := density;
  result := 1.0;
end;

function OdeSurfaceEnableRollingFrictionCoeff(obj, mode: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.RollingFrictionEnabled := Boolean(trunc64(mode));
  result := 1.0;
end;

function OdeSurfaceSetRollingFrictionCoeff(obj, rfc: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.RollingFrictionCoeff := rfc;
  result := 1.0;
end;

function OdeSurfaceSetMode(
  obj, Mu2, FDir1,
  Bounce, SoftERP, SoftCFM,
  Motion1, Motion2,
  Slip1, Slip2 : real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
  surf: TODECollisionSurface;
begin
  o := TGLBaseSceneObject(trunc64(obj));
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

function OdeSurfaceSetMu(obj, mu: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Mu := mu;
  result := 1.0;
end;

function OdeSurfaceSetMu2(obj, mu2: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Mu2 := mu2;
  result := 1.0;
end;

function OdeSurfaceSetBounce(obj, bounce: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Bounce := bounce;
  result := 1.0;
end;

function OdeSurfaceSetBounceVel(obj, vel: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Bounce_Vel := vel;
  result := 1.0;
end;

function OdeSurfaceSetSoftERP(obj, erp: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.SoftERP := erp;
  result := 1.0;
end;

function OdeSurfaceSetSoftCFM(obj, cfm: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.SoftCFM := cfm;
  result := 1.0;
end;

function OdeSurfaceSetMotion1(obj, motion1: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Motion1 := motion1;
  result := 1.0;
end;

function OdeSurfaceSetMotion2(obj, motion2: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Motion2 := motion2;
  result := 1.0;
end;

function OdeSurfaceSetSlip1(obj, slip1: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Slip1 := slip1;
  result := 1.0;
end;

function OdeSurfaceSetSlip2(obj, slip2: real): real; stdcall;
var
  o: TGLBaseSceneObject;
  beh: TGLODEBehaviour;
begin
  o := TGLBaseSceneObject(trunc64(obj));
  beh := getODEBehaviour(o);
  if beh <> nil then
    beh.Surface.Slip2 := slip2;
  result := 1.0;
end;

function OdeAddJointBall: real; stdcall;
var
  j: TODEJointBall;
begin
  j := TODEJointBall.Create(jointList.Joints);
  j.Manager := ode;
  result := Integer(j);
end;

function OdeAddJointFixed: real; stdcall;
var
  j: TODEJointFixed;
begin
  j := TODEJointFixed.Create(jointList.Joints);
  j.Manager := ode;
  result := Integer(j);
end;

function OdeAddJointHinge: real; stdcall;
var
  j: TODEJointHinge;
begin
  j := TODEJointHinge.Create(jointList.Joints);
  j.Manager := ode;
  result := Integer(j);
end;

function OdeAddJointHinge2: real; stdcall;
var
  j: TODEJointHinge2;
begin
  j := TODEJointHinge2.Create(jointList.Joints);
  j.Manager := ode;
  result := Integer(j);
end;

function OdeAddJointSlider: real; stdcall;
var
  j: TODEJointSlider;
begin
  j := TODEJointSlider.Create(jointList.Joints);
  j.Manager := ode;
  result := Integer(j);
end;

function OdeAddJointUniversal: real; stdcall;
var
  j: TODEJointUniversal;
begin
  j := TODEJointUniversal.Create(jointList.Joints);
  j.Manager := ode;
  result := Integer(j);
end;

function OdeJointSetObjects(joint, obj1, obj2: real): real; stdcall;
var
  j: TODEJointBase;
  o1: TGLBaseSceneObject;
  o2: TGLBaseSceneObject;
begin
  j := TODEJointBase(trunc64(joint));
  o1 := TGLBaseSceneObject(trunc64(obj1));
  o2 := TGLBaseSceneObject(trunc64(obj2));
  j.Object1 := o1;
  j.Object2 := o2;
  result := 1.0;
end;

function OdeJointEnable(joint, mode: real): real; stdcall;
var
  j: TODEJointBase;
begin
  j := TODEJointBase(trunc64(joint));
  j.Enabled := Boolean(trunc64(mode));
  result := 1.0;
end;

function OdeJointInitialize(joint: real): real; stdcall;
var
  j: TODEJointBase;
begin
  j := TODEJointBase(trunc64(joint));
  j.Initialize;
  result := 1.0;
end;

function OdeJointSetAnchor(joint, x, y, z: real): real; stdcall;
var
  j: TODEJointBase;
begin
  j := TODEJointBase(trunc64(joint));
  if j is TODEJointHinge then
    TODEJointHinge(j).Anchor.SetPoint(x, y, z)
  else if j is TODEJointBall then
    TODEJointBall(j).Anchor.SetPoint(x, y, z)
  else if j is TODEJointHinge2 then
    TODEJointHinge2(j).Anchor.SetPoint(x, y, z)
  else if j is TODEJointUniversal then
    TODEJointUniversal(j).Anchor.SetPoint(x, y, z);
  result := 1.0;
end;

function OdeJointSetAnchorAtObject(joint, obj: real): real; stdcall;
var
  j: TODEJointBase;
  o: TGLBaseSceneObject;
begin
  j := TODEJointBase(trunc64(joint));
  o := TGLBaseSceneObject(trunc64(obj));
  if j is TODEJointHinge then
    TODEJointHinge(j).Anchor.SetPoint(o.AbsolutePosition)
  else if j is TODEJointBall then
    TODEJointBall(j).Anchor.SetPoint(o.AbsolutePosition)
  else if j is TODEJointHinge2 then
    TODEJointHinge2(j).Anchor.SetPoint(o.AbsolutePosition)
  else if j is TODEJointUniversal then
    TODEJointUniversal(j).Anchor.SetPoint(o.AbsolutePosition);
  result := 1.0;
end;

function OdeJointSetAxis1(joint, x, y, z: real): real; stdcall;
var
  j: TODEJointBase;
begin
  j := TODEJointBase(trunc64(joint));
  if j is TODEJointHinge then
    TODEJointHinge(j).Axis.SetVector(x, y, z)
  else if j is TODEJointHinge2 then
    TODEJointHinge2(j).Axis1.SetVector(x, y, z)
  else if j is TODEJointUniversal then
    TODEJointUniversal(j).Axis1.SetVector(x, y, z);
  result := 1.0;
end;

function OdeJointSetAxis2(joint, x, y, z: real): real; stdcall;
var
  j: TODEJointBase;
begin
  j := TODEJointBase(trunc64(joint));
  if j is TODEJointHinge2 then
    TODEJointHinge2(j).Axis2.SetVector(x, y, z)
  else if j is TODEJointUniversal then
    TODEJointUniversal(j).Axis2.SetVector(x, y, z);
  result := 1.0;
end;

function OdeJointSetBounce(joint, axis, bounce: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.Bounce := bounce;
  result := 1.0;
end;

function OdeJointSetCFM(joint, axis, cfm: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.CFM := cfm;
  result := 1.0;
end;

function OdeJointSetFMax(joint, axis, fmax: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.FMax := fmax;
  result := 1.0;
end;

function OdeJointSetFudgeFactor(joint, axis, ffactor: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.FudgeFactor := ffactor;
  result := 1.0;
end;

function OdeJointSetHiStop(joint, axis, histop: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.HiStop := histop;
  result := 1.0;
end;

function OdeJointSetLoStop(joint, axis, lostop: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.LoStop := lostop;
  result := 1.0;
end;

function OdeJointSetStopCFM(joint, axis, cfm: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.StopCFM := cfm;
  result := 1.0;
end;

function OdeJointSetStopERP(joint, axis, erp: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.StopERP := erp;
  result := 1.0;
end;

function OdeJointSetVel(joint, axis, velocity: real): real; stdcall;
var
  j: TODEJointBase;
  p: TODEJointParams;
begin
  j := TODEJointBase(trunc64(joint));
  p := getJointAxisParams(j, trunc64(axis));
  if p <> nil then
    p.Vel := velocity;
  result := 1.0;
end;
