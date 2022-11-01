function CubeCreate(w, h, d, parent: real): real; cdecl;
var
    cube: TGLCube;
begin
    if not (parent=0) then
        cube:=TGLCube.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
        cube:=TGLCube.CreateAsChild(scene.Objects);
    cube.CubeWidth:=w;
    cube.CubeHeight:=h;
    cube.CubeDepth:=d;
    result:=ObjToReal(cube);
end;

function CubeSetNormalDirection(cube,nd: real): real; cdecl;
var
  GLCube1: TGLCube;
begin
  GLCube1:=TGLCube(RealToPtr(cube));
  if nd=0 then GLCube1.NormalDirection:=ndOutside;
  if nd=1 then GLCube1.NormalDirection:=ndInside;
  result:=1;
end;

function CubeGetNormalDirection(cube: real): real; cdecl;
var
  GLCube1: TGLCube;
begin
   GLCube1:=TGLCube(RealToPtr(cube));
   result:=integer(GLCube1.NormalDirection);
end;

function PlaneCreate(squad,w,h,xt,yt,parent: real): real; cdecl;
var
  GLPlane1: TGLPlane;
begin
  if not (parent=0) then
    GLPlane1:=TGLPlane.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLPlane1:=TGLPlane.CreateAsChild(scene.Objects);
  if squad=1 then GLPlane1.Style:=[psSingleQuad,psTileTexture];
  if squad=0 then GLPlane1.Style:=[psTileTexture];
  GLPlane1.Width:=w;
  GLPlane1.Height:=h;
  GLPlane1.XTiles:=Trunc(xt);
  GLPlane1.YTiles:=Trunc(yt);
  result:=ObjToReal(GLPlane1);
end;

function PlaneSetOptions(plane,squad,xt,yt: real): real; cdecl;
var
  GLPlane1: TGLPlane;
begin
  GLPlane1:=TGLPlane(RealToPtr(plane));
  if squad=1 then GLPlane1.Style:=[psSingleQuad,psTileTexture];
  if squad=0 then GLPlane1.Style:=[psTileTexture];
  GLPlane1.XTiles:=Trunc(xt);
  GLPlane1.YTiles:=Trunc(yt);
  result:=1;
end;

function PlaneGetOptions(plane,index: real): real; cdecl;
var
  GLPlane1: TGLPlane;
begin
  GLPlane1:=TGLPlane(RealToPtr(plane));
  result:=0;
  if index=0 then
    result:=GLPlane1.XTiles;
  if index=1 then
    result:=GLPlane1.YTiles;
end;

function TilePlaneCreate(parent: real): real; cdecl;
var
  tplane: TGLTilePlane;
begin
    if not (parent=0) then
    tplane:=TGLTilePlane.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    tplane:=TGLTilePlane.CreateAsChild(scene.Objects);
    tplane.SortByMaterials:=True;

  result:=ObjToReal(tplane);
end;

function TilePlaneSetTile(tplane,x,y:real; mat: PAnsiChar): real; cdecl;
var
  tileplane: TGLTilePlane;
begin

  tileplane:=TGLTilePlane(RealToPtr(tplane));
  tileplane.MaterialLibrary:=matlib;
  tileplane.Tiles[Trunc(x), Trunc(y)]:=Integer(matlib.LibMaterialByName(String(AnsiString(mat))).Index);
  tileplane.StructureChanged;  
  result:=1;
end;

function SphereCreate(rad,slic,staks,parent: real): real; cdecl;
var
  GLSphere1: TGLSphere;
begin
  if not (parent=0) then
    GLSphere1:=TGLSphere.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLSphere1:=TGLSphere.CreateAsChild(scene.Objects);
  GLSphere1.Radius:=rad;
  GLSphere1.Slices:=Trunc(slic);
  GLSphere1.Stacks:=Trunc(staks);
  result:=ObjToReal(GLSphere1);
end;

function SphereSetAngleLimits(sphere,starta,stopa,topa,bottoma: real): real; cdecl;
var
  GLSphere1: TGLSphere;
begin
  GLSphere1:=TGLSphere(RealToPtr(sphere));
  GLSphere1.Start:=TAngleLimit2(Trunc(starta));
  GLSphere1.Stop:=TAngleLimit2(Trunc(stopa));
  GLSphere1.Top:=TAngleLimit1(Trunc(topa));
  GLSphere1.Bottom:=TAngleLimit1(Trunc(bottoma));
  result:=1;
end;

function SphereGetAngleLimits(sphere,index: real): real; cdecl;
var
  GLSphere1: TGLSphere;
begin
 GLSphere1:=TGLSphere(RealToPtr(sphere));
 result:=0;
 if index=0 then
   result:=GLSphere1.Start;
 if index=1 then
   result:=GLSphere1.Stop;
 if index=2 then
   result:=GLSphere1.Top;
 if index=3 then
   result:=GLSphere1.Bottom;
end;

function SphereSetOptions(sphere,rad,slic,staks: real): real; cdecl;
var
  sphr: TGLSphere;
begin
sphr:=TGLSphere(RealToPtr(sphere));
  sphr.Radius:=rad;
  sphr.Slices:=Trunc(slic);
  sphr.Stacks:=Trunc(staks);
  result:=1;
end;

function SphereGetOptions(sph,ind: real): real; cdecl;
var
  sphr: TGLSphere;
begin
sphr:=TGLSphere(RealToPtr(sph));
result:=0;
if (ind=0) then
    result:=sphr.Radius;
if (ind=1) then
    result:=sphr.Slices;
if (ind=2) then
    result:=sphr.Stacks;
end;

function CylinderCreate(topr,botr,h,slic,staks,loop,parent: real): real; cdecl;
var
  GLCylinder1: TGLCylinder;
begin
  if not (parent=0) then
    GLCylinder1:=TGLCylinder.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLCylinder1:=TGLCylinder.CreateAsChild(scene.Objects);
  GLCylinder1.TopRadius:=topr;
  GLCylinder1.BottomRadius:=botr;
  GLCylinder1.Height:=h;
  GLCylinder1.Slices:=Trunc(slic);
  GLCylinder1.Stacks:=Trunc(staks);
  GLCylinder1.Loops:=Trunc(loop);
  result:=ObjToReal(GLCylinder1);
end;

function CylinderSetOptions(cyl,topr,botr,h,slic,staks,loop: real): real; cdecl;
var
  GLCylinder1: TGLCylinder;
begin
  GLCylinder1:=TGLCylinder(RealToPtr(cyl));
  GLCylinder1.TopRadius:=topr;
  GLCylinder1.BottomRadius:=botr;
  GLCylinder1.Height:=h;
  GLCylinder1.Slices:=Trunc(slic);
  GLCylinder1.Stacks:=Trunc(staks);
  GLCylinder1.Loops:=Trunc(loop);
  result:=1;
end;

function CylinderGetOptions(cyl,ind: real): real; cdecl;
var
  GLCylinder1: TGLCylinder;
begin
  GLCylinder1:=TGLCylinder(RealToPtr(cyl));
  result:=0;
  if (ind=0) then
      result:=GLCylinder1.TopRadius;
  if (ind=1) then
      result:=GLCylinder1.BottomRadius;
  if (ind=2) then
      result:=GLCylinder1.Height;
  if (ind=3) then
      result:=GLCylinder1.Slices;
  if (ind=4) then
      result:=GLCylinder1.Stacks;
  if (ind=5) then
      result:=GLCylinder1.Loops;
end;

function ConeCreate(botr,h,slic,staks,loop,parent: real): real; cdecl;
var
  GLCone1: TGLCone;
begin
  if not (parent=0) then
    GLCone1:=TGLCone.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLCone1:=TGLCone.CreateAsChild(scene.Objects);
  GLCone1.BottomRadius:=botr;
  GLCone1.Height:=h;
  GLCone1.Slices:=Trunc(slic);
  GLCone1.Stacks:=Trunc(staks);
  GLCone1.Loops:=Trunc(loop);
  result:=ObjToReal(GLCone1);
end;

function ConeGetOptions(cone,ind: real): real; cdecl;
var
  cone1: TGLCone;
begin
  cone1:=TGLCone(RealToPtr(cone));
  result:=0;
  if (ind=0) then
      result:=cone1.BottomRadius;
  if (ind=1) then
      result:=cone1.Height;
  if (ind=2) then
      result:=cone1.Slices;
  if (ind=3) then
      result:=cone1.Stacks;
  if (ind=4) then
      result:=cone1.Loops;
end;

function ConeSetOptions(cone,botr,h,slic,staks,loop: real): real; cdecl;
var
  GLCone1: TGLCone;
begin
  GLCone1:=TGLCone(RealToPtr(cone));
  GLCone1.BottomRadius:=botr;
  GLCone1.Height:=h;
  GLCone1.Slices:=Trunc(slic);
  GLCone1.Stacks:=Trunc(staks);
  GLCone1.Loops:=Trunc(loop);
  result:=1;
end;

function AnnulusCreate(inr,outr,h,slic,staks,loop,parent: real): real; cdecl;
var
  GLAnnulus1: TGLAnnulus;
begin
  if not (parent=0) then
    GLAnnulus1:=TGLAnnulus.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLAnnulus1:=TGLAnnulus.CreateAsChild(scene.Objects);
  GLAnnulus1.BottomInnerRadius:=inr;
  GLAnnulus1.TopInnerRadius:=inr;
  GLAnnulus1.BottomRadius:=outr;
  GLAnnulus1.TopRadius:=outr;
  GLAnnulus1.Height:=h;
  GLAnnulus1.Slices:=Trunc(slic);
  GLAnnulus1.Stacks:=Trunc(staks);
  GLAnnulus1.Loops:=Trunc(loop);
  result:=ObjToReal(GLAnnulus1);
end;

function AnnulusSetOptions(an,inr,outr,h,slic,staks,loop: real): real; cdecl;
var
  GLAnnulus1: TGLAnnulus;
begin
  GLAnnulus1:=TGLAnnulus(RealToPtr(an));
  GLAnnulus1.BottomInnerRadius:=inr;
  GLAnnulus1.TopInnerRadius:=inr;
  GLAnnulus1.BottomRadius:=outr;
  GLAnnulus1.TopRadius:=outr;
  GLAnnulus1.Height:=h;
  GLAnnulus1.Slices:=Trunc(slic);
  GLAnnulus1.Stacks:=Trunc(staks);
  GLAnnulus1.Loops:=Trunc(loop);
  result:=1;
end;

function AnnulusGetOptions(an,ind: real): real; cdecl;
var
  an1: TGLAnnulus;
begin
  an1:=TGLAnnulus(RealToPtr(an));
  result:=0;
  if (ind=0) then
      result:=an1.BottomInnerRadius;
  if (ind=1) then
      result:=an1.BottomRadius;
  if (ind=2) then
      result:=an1.Height;
  if (ind=3) then
      result:=an1.Slices;
  if (ind=4) then
      result:=an1.Stacks;
  if (ind=5) then
      result:=an1.Loops;
end;

function TorusCreate(inr,outr,ring,side,parent: real): real; cdecl;
var
  GLTorus1: TGLTorus;
begin
  if not (parent=0) then
    GLTorus1:=TGLTorus.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLTorus1:=TGLTorus.CreateAsChild(scene.Objects);
  GLTorus1.MinorRadius:=inr;
  GLTorus1.MajorRadius:=outr;
  GLTorus1.Rings:=Trunc(ring);
  GLTorus1.Sides:=Trunc(side);
  result:=ObjToReal(GLTorus1);
end;

function TorusSetOptions(tor,inr,outr,ring,side: real): real; cdecl;
var
  GLTorus1: TGLTorus;
begin
  GLTorus1:=TGLTorus(RealToPtr(tor));
  GLTorus1.MinorRadius:=inr;
  GLTorus1.MajorRadius:=outr;
  GLTorus1.Rings:=Trunc(ring);
  GLTorus1.Sides:=Trunc(side);
  result:=ObjToReal(GLTorus1);
end;

function TorusGetOptions(tor,ind: real): real; cdecl;
var
  tor1: TGLTorus;
begin
  tor1:=TGLTorus(RealToPtr(tor));
  result:=0;
  if (ind=0) then
      result:=tor1.MinorRadius;
  if (ind=1) then
      result:=tor1.MajorRadius;
  if (ind=2) then
      result:=tor1.Rings;
  if (ind=3) then
      result:=tor1.Sides;
end;

function DiskCreate(inr,outr,starta,sweepa,loop,slic,parent: real): real; cdecl;
var
  GLDisk1: TGLDisk;
begin
  if not (parent=0) then
    GLDisk1:=TGLDisk.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLDisk1:=TGLDisk.CreateAsChild(scene.Objects);
  GLDisk1.InnerRadius:=inr;
  GLDisk1.OuterRadius:=outr;
  GLDisk1.StartAngle:=starta;
  GLDisk1.SweepAngle:=sweepa;
  GLDisk1.Loops:=Trunc(loop);
  GLDisk1.Slices:=Trunc(slic);
  result:=ObjToReal(GLDisk1);
end;

function DiskSetOptions(disk,inr,outr,starta,sweepa,loop,slic: real): real; cdecl;
var
  GLDisk1: TGLDisk;
begin
  GLDisk1:=TGLDisk(RealToPtr(disk));
  GLDisk1.InnerRadius:=inr;
  GLDisk1.OuterRadius:=outr;
  GLDisk1.StartAngle:=starta;
  GLDisk1.SweepAngle:=sweepa;
  GLDisk1.Loops:=Trunc(loop);
  GLDisk1.Slices:=Trunc(slic);
  result:=1;
end;

function DiskGetOptions(disk,ind: real): real; cdecl;
var
  disk1: TGLDisk;
begin
  disk1:=TGLDisk(RealToPtr(disk));
  result:=0;
  if (ind=0) then
      result:=disk1.InnerRadius;
  if (ind=1) then
      result:=disk1.OuterRadius;
  if (ind=2) then
      result:=disk1.StartAngle;
  if (ind=3) then
      result:=disk1.SweepAngle;
  if (ind=4) then
      result:=disk1.Loops;
  if (ind=5) then
      result:=disk1.Slices;
end;

function FrustrumCreate(basew,based,apexh,cuth,parent: real): real; cdecl;
var
  GLFrustrum1: TGLFrustrum;
begin
  if not (parent=0) then
    GLFrustrum1:=TGLFrustrum.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLFrustrum1:=TGLFrustrum.CreateAsChild(scene.Objects);
  GLFrustrum1.BaseWidth:=basew;
  GLFrustrum1.BaseDepth:=based;
  GLFrustrum1.ApexHeight:=apexh;
  GLFrustrum1.Height:=cuth;
  result:=ObjToReal(GLFrustrum1);
end;

function FrustrumSetOptions(fr,basew,based,apexh,cuth: real): real; cdecl;
var
  GLFrustrum1: TGLFrustrum;
begin
  GLFrustrum1:=TGLFrustrum(RealToPtr(fr));
  GLFrustrum1.BaseWidth:=basew;
  GLFrustrum1.BaseDepth:=based;
  GLFrustrum1.ApexHeight:=apexh;
  GLFrustrum1.Height:=cuth;
  result:=1;
end;

function FrustrumGetOptions(fr,ind: real): real; cdecl;
var
  fr1: TGLFrustrum;
begin
  fr1:=TGLFrustrum(RealToPtr(fr));
  result:=0;
  if (ind=0) then
      result:=fr1.BaseWidth;
  if (ind=1) then
      result:=fr1.BaseDepth;
  if (ind=2) then
      result:=fr1.ApexHeight;
  if (ind=3) then
      result:=fr1.Height;
end;

function DodecahedronCreate(parent: real): real; cdecl;
var
  GLDodecahedron1: TGLDodecahedron;
begin
  if not (parent=0) then
    GLDodecahedron1:=TGLDodecahedron.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLDodecahedron1:=TGLDodecahedron.CreateAsChild(scene.Objects);
  result:=ObjToReal(GLDodecahedron1);
end;

function IcosahedronCreate(parent: real): real; cdecl;
var
  GLIcosahedron1: TGLIcosahedron;
begin
  if not (parent=0) then
    GLIcosahedron1:=TGLIcosahedron.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLIcosahedron1:=TGLIcosahedron.CreateAsChild(scene.Objects);
  result:=ObjToReal(GLIcosahedron1);
end;

function TeapotCreate(parent: real): real; cdecl;
var
  GLTeapot1: TGLTeapot;
begin
  if not (parent=0) then
    GLTeapot1:=TGLTeapot.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLTeapot1:=TGLTeapot.CreateAsChild(scene.Objects);
  result:=ObjToReal(GLTeapot1);
end;
