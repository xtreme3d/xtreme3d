function CubeCreate(w,h,d,parent: real): real; stdcall;
var
  GLCube1: TGLCube;
begin
  if not (parent=0) then
    GLCube1:=TGLCube.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLCube1:=TGLCube.CreateAsChild(scene.Objects);
  GLCube1.CubeWidth:=w;
  GLCube1.CubeHeight:=h;
  GLCube1.CubeDepth:=d;
  result:=Integer(GLCube1);
end;

function CubeSetNormalDirection(cube,nd: real): real; stdcall;
var
  GLCube1: TGLCube;
begin
  GLCube1:=TGLCube(trunc64(cube));
  if nd=0 then GLCube1.NormalDirection:=ndOutside;
  if nd=1 then GLCube1.NormalDirection:=ndInside;
  result:=1;
end;

function PlaneCreate(squad,w,h,xt,yt,parent: real): real; stdcall;
var
  GLPlane1: TGLPlane;
begin
  if not (parent=0) then
    GLPlane1:=TGLPlane.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLPlane1:=TGLPlane.CreateAsChild(scene.Objects);
  if squad=1 then GLPlane1.Style:=[psSingleQuad,psTileTexture];
  if squad=0 then GLPlane1.Style:=[psTileTexture];
  GLPlane1.Width:=w;
  GLPlane1.Height:=h;
  GLPlane1.XTiles:=trunc64(xt);
  GLPlane1.YTiles:=trunc64(yt);
  result:=Integer(GLPlane1);
end;

function SphereCreate(rad,slic,staks,parent: real): real; stdcall;
var
  GLSphere1: TGLSphere;
begin
  if not (parent=0) then
    GLSphere1:=TGLSphere.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLSphere1:=TGLSphere.CreateAsChild(scene.Objects);
  GLSphere1.Radius:=rad;
  GLSphere1.Slices:=trunc64(slic);
  GLSphere1.Stacks:=trunc64(staks);
  result:=Integer(GLSphere1);
end;

function SphereSetAngleLimits(sphere,starta,stopa,topa,bottoma: real): real; stdcall;
var
  GLSphere1: TGLSphere;
begin
  GLSphere1:=TGLSphere(trunc64(sphere));
  GLSphere1.Start:=TAngleLimit2(trunc64(starta));
  GLSphere1.Stop:=TAngleLimit2(trunc64(stopa));
  GLSphere1.Top:=TAngleLimit1(trunc64(topa));
  GLSphere1.Bottom:=TAngleLimit1(trunc64(bottoma));
  result:=1;
end;

function CylinderCreate(topr,botr,h,slic,staks,loop,parent: real): real; stdcall;
var
  GLCylinder1: TGLCylinder;
begin
  if not (parent=0) then
    GLCylinder1:=TGLCylinder.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLCylinder1:=TGLCylinder.CreateAsChild(scene.Objects);
  GLCylinder1.TopRadius:=topr;
  GLCylinder1.BottomRadius:=botr;
  GLCylinder1.Height:=h;
  GLCylinder1.Slices:=trunc64(slic);
  GLCylinder1.Stacks:=trunc64(staks);
  GLCylinder1.Loops:=trunc64(loop);
  result:=integer(GLCylinder1);
end;

function ConeCreate(botr,h,slic,staks,loop,parent: real): real; stdcall;
var
  GLCone1: TGLCone;
begin
  if not (parent=0) then
    GLCone1:=TGLCone.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLCone1:=TGLCone.CreateAsChild(scene.Objects);
  GLCone1.BottomRadius:=botr;
  GLCone1.Height:=h;
  GLCone1.Slices:=trunc64(slic);
  GLCone1.Stacks:=trunc64(staks);
  GLCone1.Loops:=trunc64(loop);
  result:=integer(GLCone1);
end;

function AnnulusCreate(inr,outr,h,slic,staks,loop,parent: real): real; stdcall;
var
  GLAnnulus1: TGLAnnulus;
begin
  if not (parent=0) then
    GLAnnulus1:=TGLAnnulus.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLAnnulus1:=TGLAnnulus.CreateAsChild(scene.Objects);
  GLAnnulus1.BottomInnerRadius:=inr;
  GLAnnulus1.TopInnerRadius:=inr;
  GLAnnulus1.BottomRadius:=outr;
  GLAnnulus1.TopRadius:=outr;
  GLAnnulus1.Height:=h;
  GLAnnulus1.Slices:=trunc64(slic);
  GLAnnulus1.Stacks:=trunc64(staks);
  GLAnnulus1.Loops:=trunc64(loop);
  result:=integer(GLAnnulus1);
end;

function TorusCreate(inr,outr,ring,side,parent: real): real; stdcall;
var
  GLTorus1: TGLTorus;
begin
  if not (parent=0) then
    GLTorus1:=TGLTorus.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLTorus1:=TGLTorus.CreateAsChild(scene.Objects);
  GLTorus1.MinorRadius:=inr;
  GLTorus1.MajorRadius:=outr;
  GLTorus1.Rings:=trunc64(ring);
  GLTorus1.Sides:=trunc64(side);
  result:=integer(GLTorus1);
end;

function DiskCreate(inr,outr,starta,sweepa,loop,slic,parent: real): real; stdcall;
var
  GLDisk1: TGLDisk;
begin
  if not (parent=0) then
    GLDisk1:=TGLDisk.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLDisk1:=TGLDisk.CreateAsChild(scene.Objects);
  GLDisk1.InnerRadius:=inr;
  GLDisk1.OuterRadius:=outr;
  GLDisk1.StartAngle:=starta;
  GLDisk1.SweepAngle:=sweepa;
  GLDisk1.Loops:=trunc64(loop);
  GLDisk1.Slices:=trunc64(slic);
  result:=integer(GLDisk1);
end;

function FrustrumCreate(basew,based,apexh,cuth,parent: real): real; stdcall;
var
  GLFrustrum1: TGLFrustrum;
begin
  if not (parent=0) then
    GLFrustrum1:=TGLFrustrum.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLFrustrum1:=TGLFrustrum.CreateAsChild(scene.Objects);
  GLFrustrum1.BaseWidth:=basew;
  GLFrustrum1.BaseDepth:=based;
  GLFrustrum1.ApexHeight:=apexh;
  GLFrustrum1.Height:=cuth;
  result:=integer(GLFrustrum1);
end;

function DodecahedronCreate(parent: real): real; stdcall;
var
  GLDodecahedron1: TGLDodecahedron;
begin
  if not (parent=0) then
    GLDodecahedron1:=TGLDodecahedron.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLDodecahedron1:=TGLDodecahedron.CreateAsChild(scene.Objects);
  result:=integer(GLDodecahedron1);
end;

function IcosahedronCreate(parent: real): real; stdcall;
var
  GLIcosahedron1: TGLIcosahedron;
begin
  if not (parent=0) then
    GLIcosahedron1:=TGLIcosahedron.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLIcosahedron1:=TGLIcosahedron.CreateAsChild(scene.Objects);
  result:=integer(GLIcosahedron1);
end;

function TeapotCreate(parent: real): real; stdcall;
var
  GLTeapot1: TGLTeapot;
begin
  if not (parent=0) then
    GLTeapot1:=TGLTeapot.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLTeapot1:=TGLTeapot.CreateAsChild(scene.Objects);
  result:=integer(GLTeapot1);
end;