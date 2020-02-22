function VerletWorldCreate (iter,UpdateSpacePartion,drag: real): real; stdcall;
var
world: TVerletWorld;
begin
world := TVerletWorld.Create;
world.UpdateSpacePartion := uspEveryFrame;
  world.Iterations:= trunc64(iter);
  if UpdateSpacePartion = 0 then world.UpdateSpacePartion := uspEveryIteration;
  if UpdateSpacePartion = 1 then world.UpdateSpacePartion := uspEveryFrame;
  if UpdateSpacePartion = 2 then world.UpdateSpacePartion := uspNever;
  world.Drag:=drag;
  result:=Integer(world);
end;

function VerletWorldCreateOctree (world,xmin,ymin,zmin,xmax,ymax,zmax,leaf,depth: real): real; stdcall;
var
vworld: TVerletWorld;
begin
vworld:=TVerletWorld(trunc64(world));
vworld.CreateOctree(
      AffineVectorMake( xmin, ymin, zmin),
      AffineVectorMake(  xmax,  ymax,  zmax), trunc64(leaf), trunc64(depth));
  result:=1;
end;


function VerletGetNodeCount (world: real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  result:=Integer(ver.Nodes.Count);
end;


function VerletWorldGravityCreate (world,x,y,z: real): real; stdcall;
var
gr: TVFGravity;
worldd: TVerletWorld;
begin
worldd:=TVerletWorld(trunc64(world));
gr:= TVFGravity.Create(worldd);
gr.Gravity := AffineVectorMake (x, y, z);
  result:=Integer(gr);
end;

function VerletWorldGravitySetDirection (grv,x,y,z: real): real; stdcall;
var
gr: TVFGravity;
begin
gr:= TVFGravity(trunc64(grv));
gr.Gravity := AffineVectorMake (x, y, z);
  result:=1;
end;


function VerletWorldUpdate (world,newTime: real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
ver.Progress (ver.MaxDeltaTime, newTime);
  result:=1;
end;

function EdgeDetectorCreate (world,obj: real): real; stdcall;
var
edg: TEdgeDetector;
ver: TVerletWorld;
mesh: TGLBaseMesh;
begin
mesh:=TGLBaseMesh(trunc64(obj));
ver:=TVerletWorld(trunc64(world));
edg := TEdgeDetector.Create (mesh);
edg.ProcessMesh;
edg.AddEdgesAsSticks (ver, 0.15);
edg.AddEdgesAsSolidEdges (ver);
  result:=Integer(edg);
end;

function EdgeDetectorSetWeldDistance (edge,dis: real): real; stdcall;
var
ed: TEdgeDetector;
begin
ed:=TEdgeDetector(trunc64(edge));
ed.WeldDistance:=dis;
  result:=1;
end;

function VerletConstraintFloorCreate (world,bou,level:real): real; stdcall;
var
ver: TVerletWorld;
floor: TVCFloor;
begin
ver:=TVerletWorld(trunc64(world));
floor:= TVCFloor.Create (ver);
floor.BounceRatio:=bou;
floor.FloorLevel:=level;
  result:=Integer(floor);
end;

function VerletConstraintFloorSetNormal (flr,x,y,z:real): real; stdcall;
var
floor: TVCFloor;
begin
floor:=TVCFloor(trunc64(flr));
floor.Normal:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintFloorSetObjectLocations (flr,obj:real): real; stdcall;
var
floor: TVCFloor;
j: TGLSceneObject;
begin
j:=TGLSceneObject(trunc64(obj));
floor:=TVCFloor(trunc64(flr));
floor.Normal   := j.Direction.AsAffineVector;
    floor.Location := VectorAdd (j.Position.AsAffineVector,
      VectorScale (j.Direction.AsAffineVector, 1));
  result:=1;
end;

function VerletConstraintSphereCreate (world,rad:real): real; stdcall;
var
ver: TVerletWorld;
sphere: TVCSphere;
begin
ver:=TVerletWorld(trunc64(world));
sphere:= TVCSphere.Create (ver);
sphere.Radius:= rad;
  result:=Integer(sphere);
end;

function VerletConstraintCylinderCreate (world,rad:real): real; stdcall;
var
ver: TVerletWorld;
cylinder: TVCCylinder;
begin
ver:=TVerletWorld(trunc64(world));
cylinder:= TVCCylinder.Create (ver);
cylinder.Radius:= rad;
  result:=Integer(cylinder);
end;

function VerletConstraintCylinderSetAxis (cyl,x,y,z:real): real; stdcall;
var
cylinder: TVCCylinder;
begin
cylinder:= TVCCylinder(trunc64(cyl));
cylinder.Axis:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintCubeCreate (world,x,y,z:real): real; stdcall;
var
ver: TVerletWorld;
cube: TVCCube;
begin
ver:=TVerletWorld(trunc64(world));
cube:= TVCCube.Create (ver);
cube.Sides:=AffineVectorMake(x,y,z);
  result:=Integer(cube);
end;

function VerletConstraintCubeCreateSetCube (world,cube1:real): real; stdcall;
var
ver: TVerletWorld;
pr: TGLCube;
Cube: TVCCube;
begin
ver:=TVerletWorld(trunc64(world));
    Cube := TVCCube.Create(ver);
	pr:=TGLCube(trunc64(cube1));
    Cube.Location := AffineVectorMake(pr.AbsolutePosition);
    Cube.FrictionRatio := 0.1;
    Cube.Sides:=  AffineVectorMake(pr.CubeWidth * 1.1,pr.CubeHeight * 1.1,pr.CubeDepth * 1.1);
  result:=Integer(Cube);
end;

function VerletConstraintCubeSetDirection(cb,x,y,z:real): real; stdcall;
var
cube: TVCCube;
begin
cube:= TVCCube(trunc64(cb));
cube.Direction:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintCapsuleCreate (world,rad,len:real): real; stdcall;
var
ver: TVerletWorld;
caps: TVCCapsule;
begin
ver:=TVerletWorld(trunc64(world));
caps:= TVCCapsule.Create (ver);
caps.Radius:=rad;
caps.Length:=len;
  result:=Integer(caps);
end;

function VerletConstraintCapsuleSetAxis (cp,x,y,z:real): real; stdcall;
var
caps: TVCCapsule;
begin
caps:= TVCCapsule(trunc64(cp));
caps.Axis:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintSetPosition (obj,x,y,z:real): real; stdcall;
var
objj: TVerletGlobalConstraint;
begin
objj:=TVerletGlobalConstraint(trunc64(obj));
objj.Location:= AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletConstraintSetFrictionRatio (obj,fr:real): real; stdcall;
var
objj: TVerletGlobalFrictionConstraint;
begin
objj:=TVerletGlobalFrictionConstraint(trunc64(obj));
objj.FrictionRatio:= fr;
  result:=1;
end;

function VerletConstraintSetEnabled (obj,en:real): real; stdcall;
var
objj: TVerletConstraint;
begin
objj:=TVerletConstraint(trunc64(obj));
objj.Enabled:= Boolean(trunc64(en));
  result:=1;
end;

function VerletNodeNailedDown (world,ind,bol:real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  ver.Nodes.Items[trunc64(ind)].NailedDown := Boolean(trunc64(bol));
  result:=1;
end;

function VerletNodeSetPosition (world,ind,x,y,z:real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  ver.Nodes.Items[trunc64(ind)].Location := AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletNodeSetRadius (world,ind,rad:real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  ver.Nodes.Items[trunc64(ind)].Radius := rad;
  result:=1;
end;

function VerletNodeSetFriction (world,ind,fr:real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  ver.Nodes.Items[trunc64(ind)].Friction := fr;
  result:=1;
end;

function VerletNodeSetWeight (world,ind,weight:real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  ver.Nodes.Items[trunc64(ind)].Weight := weight;
  result:=1;
end;

function VerletNodeApplyFriction (world,ind,fr,depth,x,y,z:real): real; stdcall;
var
ver: TVerletWorld;
begin
ver:=TVerletWorld(trunc64(world));
  ver.Nodes.Items[trunc64(ind)].ApplyFriction(fr,depth,AffineVectorMake(x,y,z));
  result:=1;
end;


function VerletAirResistanceCreate (world,Magnitude,Chaos:real): real; stdcall;
var
ver: TVerletWorld;
air: TVFAirResistance;
begin
ver:=TVerletWorld(trunc64(world));
  air:= TVFAirResistance.Create(ver);
  air.WindDirection := AffineVectorMake(1,0,0);
  air.WindMagnitude:=Magnitude;
  air.WindChaos:=Chaos; 
  result:=Integer(air);
end;

function VerletAirResistanceSetWindDirection (air,x,y,z:real): real; stdcall;
var
airr: TVFAirResistance;
begin
airr:=TVFAirResistance(trunc64(air));
  airr.WindDirection := AffineVectorMake(x,y,z);
  result:=1;
end;

function VerletAirResistanceSetWindMagnitude (air,mag:real): real; stdcall;
var
airr: TVFAirResistance;
begin
airr:=TVFAirResistance(trunc64(air));
  airr.WindMagnitude := mag;
  result:=1;
end;

function VerletAirResistanceSetWindChaos (air,ch:real): real; stdcall;
var
airr: TVFAirResistance;
begin
airr:=TVFAirResistance(trunc64(air));
  airr.WindChaos := ch;
  result:=1;
end;

function VerletConstraintGetCount (wr:real): real; stdcall;
var
world: TVerletWorld;
begin
world:=TVerletWorld(trunc64(wr));
  result:=world.Constraints.Count-1;
end;

function VerletConstraintSetSlack (wr,con,sla:real): real; stdcall;
var
world: TVerletWorld;
begin
world:=TVerletWorld(trunc64(wr));
TVCStick(world.Constraints[trunc64(con)]).Slack := sla;
  result:=1;
end;

function VerletWorldSetSimTime (wr,tm:real): real; stdcall;
var
world: TVerletWorld;
begin
world:=TVerletWorld(trunc64(wr));
world.SimTime:=tm;
  result:=1;
end;

function VerletWorldSetMaxDeltaTime (wr,tm:real): real; stdcall;
var
world: TVerletWorld;
begin
world:=TVerletWorld(trunc64(wr));
world.MaxDeltaTime:=tm;
  result:=1;
end;

