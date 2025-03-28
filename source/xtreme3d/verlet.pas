// Verlet functions by FireRun

function VerletWorldCreate (iter,UpdateSpacePartion,drag: real): real; cdecl;
var
  world: TGLVerletWorld;
begin
  world := TGLVerletWorld.Create;
  world.UpdateSpacePartion := uspEveryFrame;
  world.Iterations := trunc(iter);
  if UpdateSpacePartion = 0 then world.UpdateSpacePartion := uspEveryIteration;
  if UpdateSpacePartion = 1 then world.UpdateSpacePartion := uspEveryFrame;
  if UpdateSpacePartion = 2 then world.UpdateSpacePartion := uspNever;
  world.Drag:=drag;
  result:=ObjToReal(world);
end;

function VerletWorldCreateOctree (world,xmin,ymin,zmin,xmax,ymax,zmax,leaf,depth: real): real; cdecl;
var
  vworld: TGLVerletWorld;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  vworld.CreateOctree(
    AffineVectorMake(xmin, ymin, zmin),
    AffineVectorMake(xmax, ymax, zmax), trunc(leaf), trunc(depth));
  result:=1;
end;

function VerletGetNodeCount (world: real): real; cdecl;
var
  ver: TGLVerletWorld;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  result := Integer(ver.Nodes.Count);
end;

function VerletWorldGravityCreate (world,x,y,z: real): real; cdecl;
var
  gr: TGLVerletGravity;
  worldd: TGLVerletWorld;
begin
  worldd := TGLVerletWorld(RealToPtr(world));
  gr := TGLVerletGravity.Create(worldd);
  gr.Gravity := AffineVectorMake (x, y, z);
  result := ObjToReal(gr);
end;

function VerletWorldGravitySetDirection (grv,x,y,z: real): real; cdecl;
var
  gr: TGLVerletGravity;
begin
  gr:= TGLVerletGravity(RealToPtr(grv));
  gr.Gravity := AffineVectorMake (x, y, z);
  result:=1;
end;

function VerletWorldUpdate (world,newTime: real): real; cdecl;
var
  ver: TGLVerletWorld;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  ver.Progress(ver.MaxDeltaTime, newTime);
  result:=1;
end;

function EdgeDetectorCreate (world,obj: real): real; cdecl;
var
  edg: TGLEdgeDetector;
  ver: TGLVerletWorld;
  mesh: TGLBaseMesh;
begin
  mesh:=TGLBaseMesh(RealToPtr(obj));
  ver:=TGLVerletWorld(RealToPtr(world));
  edg := TGLEdgeDetector.Create (mesh);
  edg.ProcessMesh;
  edg.AddEdgesAsSticks (ver, 0.15);
  edg.AddEdgesAsSolidEdges (ver);
  result:=ObjToReal(edg);
end;

function EdgeDetectorSetWeldDistance (edge,dis: real): real; cdecl;
var
  ed: TGLEdgeDetector;
begin
  ed:=TGLEdgeDetector(RealToPtr(edge));
  ed.WeldDistance:=dis;
  result:=1;
end;

function VerletConstraintFloorCreate (world,bou,level:real): real; cdecl;
var
  ver: TGLVerletWorld;
  floor: TGLVerletFloor;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  floor:= TGLVerletFloor.Create (ver);
  floor.BounceRatio:=bou;
  floor.FloorLevel:=level;
  result:=ObjToReal(floor);
end;

function VerletConstraintFloorSetNormal (flr,x,y,z:real): real; cdecl;
var
  floor: TGLVerletFloor;
begin
  floor:=TGLVerletFloor(RealToPtr(flr));
  floor.Normal:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintFloorSetObjectLocations (flr,obj:real): real; cdecl;
var
  floor: TGLVerletFloor;
  j: TGLSceneObject;
begin
  j:=TGLSceneObject(RealToPtr(obj));
  floor:=TGLVerletFloor(RealToPtr(flr));
  floor.Normal   := j.Direction.AsAffineVector;
  floor.Location := VectorAdd (j.Position.AsAffineVector, VectorScale (j.Direction.AsAffineVector, 1));
  result:=1;
end;

function VerletConstraintSphereCreate (world,rad:real): real; cdecl;
var
  ver: TGLVerletWorld;
  sphere: TGLVerletFrictionSphere;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  sphere:= TGLVerletFrictionSphere.Create (ver);
  sphere.Radius:= rad;
  result:=ObjToReal(sphere);
end;

function VerletConstraintCylinderCreate (world,rad:real): real; cdecl;
var
  ver: TGLVerletWorld;
  cylinder: TGLVerletFrictionCylinder;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  cylinder:= TGLVerletFrictionCylinder.Create(ver);
  cylinder.Radius:= rad;
  result:=ObjToReal(cylinder);
end;

function VerletConstraintCylinderSetAxis (cyl,x,y,z:real): real; cdecl;
var
  cylinder: TGLVerletFrictionCylinder;
begin
  cylinder:= TGLVerletFrictionCylinder(RealToPtr(cyl));
  cylinder.Axis:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintCubeCreate (world,x,y,z:real): real; cdecl;
var
  ver: TGLVerletWorld;
  cube: TGLVerletFrictionCube;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  cube:= TGLVerletFrictionCube.Create (ver);
  cube.Sides:=AffineVectorMake(x,y,z);
  result:=ObjToReal(cube);
end;

function VerletConstraintCubeCreateSetCube(world,cube1:real): real; cdecl;
var
  ver: TGLVerletWorld;
  pr: TGLCube;
  Cube: TGLVerletFrictionCube;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  Cube := TGLVerletFrictionCube.Create(ver);
	pr:=TGLCube(RealToPtr(cube1));
  Cube.Location := AffineVectorMake(pr.AbsolutePosition);
  Cube.FrictionRatio := 0.1;
  Cube.Sides:=  AffineVectorMake(pr.CubeWidth * 1.1,pr.CubeHeight * 1.1,pr.CubeDepth * 1.1);
  result:=ObjToReal(Cube);
end;

function VerletConstraintCubeSetDirection(cb,x,y,z:real): real; cdecl;
var
  cube: TGLVerletFrictionCube;
begin
  cube:= TGLVerletFrictionCube(RealToPtr(cb));
  cube.Direction:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintCapsuleCreate (world,rad,len:real): real; cdecl;
var
  ver: TGLVerletWorld;
  caps: TGLVerletFrictionCapsule;
begin
  ver:=TGLVerletWorld(RealToPtr(world));
  caps:= TGLVerletFrictionCapsule.Create (ver);
  caps.Radius:=rad;
  caps.Length:=len;
  result:=ObjToReal(caps);
end;

function VerletConstraintCapsuleSetAxis (cp,x,y,z:real): real; cdecl;
var
  caps: TGLVerletFrictionCapsule;
begin
  caps:= TGLVerletFrictionCapsule(RealToPtr(cp));
  caps.Axis:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintSetPosition(obj,x,y,z:real): real; cdecl;
var
  objj: TGLVerletGlobalConstraint;
begin
  objj:=TGLVerletGlobalConstraint(RealToPtr(obj));
  objj.Location:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintSetFrictionRatio (obj,fr:real): real; cdecl;
var
  objj: TGLVerletGlobalFrictionConstraint;
begin
  objj:=TGLVerletGlobalFrictionConstraint(RealToPtr(obj));
  objj.FrictionRatio:= fr;
  result:=1;
end;

function VerletConstraintSetEnabled (obj,en:real): real; cdecl;
var
  objj: TGLVerletConstraint;
begin
  objj:=TGLVerletConstraint(RealToPtr(obj));
  objj.Enabled := Boolean(trunc(en));
  result:=1;
end;

function VerletNodeNailedDown(world,ind,bol:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  nodeIndex: Integer;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  nodeIndex := trunc(ind);
  vworld.Nodes.Items[nodeIndex].NailedDown := Boolean(trunc(bol));
  result:=1;
end;

function VerletNodeSetPosition (world,ind,x,y,z:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  nodeIndex: Integer;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  nodeIndex := trunc(ind);
  vworld.Nodes.Items[nodeIndex].Location := AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletNodeSetRadius (world,ind,rad:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  nodeIndex: Integer;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  nodeIndex := trunc(ind);
  vworld.Nodes.Items[nodeIndex].Radius := rad;
  result:=1;
end;

function VerletNodeSetFriction (world,ind,fr:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  nodeIndex: Integer;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  nodeIndex := trunc(ind);
  vworld.Nodes.Items[nodeIndex].Friction := fr;
  result:=1;
end;

function VerletNodeSetWeight (world,ind,weight:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  nodeIndex: Integer;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  nodeIndex := trunc(ind);
  vworld.Nodes.Items[nodeIndex].Weight := weight;
  result:=1;
end;

function VerletNodeApplyFriction (world,ind,fr,depth,x,y,z:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  nodeIndex: Integer;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  nodeIndex := trunc(ind);
  vworld.Nodes.Items[nodeIndex].ApplyFriction(fr,depth,AffineVectorMake(x,y,z));
  result:=1;
end;

function VerletAirResistanceCreate (world,Magnitude,Chaos:real): real; cdecl;
var
  vworld: TGLVerletWorld;
  air: TGLVerletAirResistance;
begin
  vworld:=TGLVerletWorld(RealToPtr(world));
  air:= TGLVerletAirResistance.Create(vworld);
  air.WindDirection := AffineVectorMake(1,0,0);
  air.WindMagnitude:=Magnitude;
  air.WindChaos:=Chaos; 
  result:=ObjToReal(air);
end;

function VerletAirResistanceSetWindDirection (air,x,y,z:real): real; cdecl;
var
  airr: TGLVerletAirResistance;
begin
  airr:=TGLVerletAirResistance(RealToPtr(air));
  airr.WindDirection := AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletAirResistanceSetWindMagnitude (air,mag:real): real; cdecl;
var
  airr: TGLVerletAirResistance;
begin
  airr:=TGLVerletAirResistance(RealToPtr(air));
  airr.WindMagnitude := mag;
  result:=1;
end;

function VerletAirResistanceSetWindChaos (air,ch:real): real; cdecl;
var
  airr: TGLVerletAirResistance;
begin
  airr:=TGLVerletAirResistance(RealToPtr(air));
  airr.WindChaos := ch;
  result:=1;
end;

function VerletConstraintGetCount (wr:real): real; cdecl;
var
  vworld: TGLVerletWorld;
begin
  vworld:=TGLVerletWorld(RealToPtr(wr));
  result:=vworld.Constraints.Count-1;
end;

function VerletConstraintSetSlack (wr,con,sla:real): real; cdecl;
var
  vworld: TGLVerletWorld;
begin
  vworld:=TGLVerletWorld(RealToPtr(wr));
  TGLVerletStick(vworld.Constraints[trunc(con)]).Slack := sla;
  result:=1;
end;

function VerletWorldSetSimTime (wr,tm:real): real; cdecl;
var
  vworld: TGLVerletWorld;
begin
  vworld:=TGLVerletWorld(RealToPtr(wr));
  vworld.SimTime:=tm;
  result:=1;
end;

function VerletWorldSetMaxDeltaTime (wr,tm:real): real; cdecl;
var
  vworld: TGLVerletWorld;
begin
  vworld:=TGLVerletWorld(RealToPtr(wr));
  vworld.MaxDeltaTime:=tm;
  result:=1;
end;
