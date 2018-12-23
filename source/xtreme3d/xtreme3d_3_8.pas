// New functions for Xtreme3D 3.8:

function SpriteGetSize(sprite, type_val: real): real; stdcall;
var
  spr: TGLSprite;
begin
  spr := TGLSprite(trunc64(sprite));
  if (type_val = 0) then
    result := spr.Width;
  if (type_val = 1) then
    result := spr.Height;
end;

function MaterialDestroy(mtrl: pchar): real; stdcall;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
  mat.Free;
  result := 1;
end;

function MaterialSetName(mtrl, name: pchar): real; stdcall;
var
  mat: TGLLibMaterial;
begin
  mat := matlib.Materials.GetLibMaterialByName(String(mtrl));
  mat.Name := String(name);
  result := 1;
end;


function LightGetColor(light,index: real): real; stdcall
var 
lght: TGLLightSource;
begin
lght:= TGLLightSource(trunc64(light));
if (index=0) then
  result:=lght.Ambient.AsWinColor;
if (index=1) then
  result:=lght.Diffuse.AsWinColor;
if (index=2) then
  result:=lght.Specular.AsWinColor;
end;

function LightGetAttenuation(light,index: real): real; stdcall
begin
if (index=0) then
    result:=TGLLightSource(trunc64(light)).ConstAttenuation;
if (index=1) then
    result:=TGLLightSource(trunc64(light)).LinearAttenuation;
if (index=2) then
    result:=TGLLightSource(trunc64(light)).QuadraticAttenuation;	
end;

function LightGetShining(light: real): real; stdcall
begin
  result:=integer(TGLLightSource(trunc64(light)).Shining);
end;


function CubeGetNormalDirection(cube: real): real; stdcall;
var
  GLCube1: TGLCube;
begin
   GLCube1:=TGLCube(trunc64(cube));
   result:=integer(GLCube1.NormalDirection);
end;

function PlaneSetOptions(plane,squad,xt,yt: real): real; stdcall;
var
  GLPlane1: TGLPlane;
begin
  GLPlane1:=TGLPlane(trunc64(plane));
  if squad=1 then GLPlane1.Style:=[psSingleQuad,psTileTexture];
  if squad=0 then GLPlane1.Style:=[psTileTexture];
  GLPlane1.XTiles:=trunc64(xt);
  GLPlane1.YTiles:=trunc64(yt);
  result:=1;
end;

function PlaneGetOptions(plane,index: real): real; stdcall;
var
  GLPlane1: TGLPlane;
begin
  GLPlane1:=TGLPlane(trunc64(plane));
  if index=0 then
    result:=GLPlane1.XTiles;
  if index=1 then
    result:=GLPlane1.YTiles;	
end;

function SphereGetOptions(sph,ind: real): real; stdcall;
var
  sphr: TGLSphere;
begin
sphr:=TGLSphere(trunc64(sph));
if (ind=0) then
    result:=sphr.Radius;
if (ind=1) then
    result:=sphr.Slices;	
if (ind=2) then
    result:=sphr.Stacks;	
end;

function SphereGetAngleLimits(sphere,index: real): real; stdcall;
var
  GLSphere1: TGLSphere;
begin
 GLSphere1:=TGLSphere(trunc64(sphere));
 if index=0 then
   result:=GLSphere1.Start;
 if index=1 then
   result:=GLSphere1.Stop;
 if index=2 then
   result:=GLSphere1.Top;
 if index=3 then
   result:=GLSphere1.Bottom;   
end;

function SphereSetOptions(sphere,rad,slic,staks: real): real; stdcall;
var
  sphr: TGLSphere;
begin
sphr:=TGLSphere(trunc64(sphere));
  sphr.Radius:=rad;
  sphr.Slices:=trunc64(slic);
  sphr.Stacks:=trunc64(staks);
  result:=1;
end;

function CylinderSetOptions(cyl,topr,botr,h,slic,staks,loop: real): real; stdcall;
var
  GLCylinder1: TGLCylinder;
begin
  GLCylinder1:=TGLCylinder(trunc64(cyl));
  GLCylinder1.TopRadius:=topr;
  GLCylinder1.BottomRadius:=botr;
  GLCylinder1.Height:=h;
  GLCylinder1.Slices:=trunc64(slic);
  GLCylinder1.Stacks:=trunc64(staks);
  GLCylinder1.Loops:=trunc64(loop);
  result:=1;
end;

function CylinderGetOptions(cyl,ind: real): real; stdcall;
var
  GLCylinder1: TGLCylinder;
begin
  GLCylinder1:=TGLCylinder(trunc64(cyl));
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


function ConeGetOptions(cone,ind: real): real; stdcall;
var
  cone1: TGLCone;
begin
  cone1:=TGLCone(trunc64(cone));
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

function ConeSetOptions(cone,botr,h,slic,staks,loop: real): real; stdcall;
var
  GLCone1: TGLCone;
begin
  GLCone1:=TGLCone(trunc64(cone));
  GLCone1.BottomRadius:=botr;
  GLCone1.Height:=h;
  GLCone1.Slices:=trunc64(slic);
  GLCone1.Stacks:=trunc64(staks);
  GLCone1.Loops:=trunc64(loop);
  result:=1;
end;


function AnnulusSetOptions(an,inr,outr,h,slic,staks,loop: real): real; stdcall;
var
  GLAnnulus1: TGLAnnulus;
begin
  GLAnnulus1:=TGLAnnulus(trunc64(an));
  GLAnnulus1.BottomInnerRadius:=inr;
  GLAnnulus1.TopInnerRadius:=inr;
  GLAnnulus1.BottomRadius:=outr;
  GLAnnulus1.TopRadius:=outr;
  GLAnnulus1.Height:=h;
  GLAnnulus1.Slices:=trunc64(slic);
  GLAnnulus1.Stacks:=trunc64(staks);
  GLAnnulus1.Loops:=trunc64(loop);
  result:=1;
end;

function AnnulusGetOptions(an,ind: real): real; stdcall;
var
  an1: TGLAnnulus;
begin
  an1:=TGLAnnulus(trunc64(an));
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


function TorusSetOptions(tor,inr,outr,ring,side: real): real; stdcall;
var
  GLTorus1: TGLTorus;
begin
  GLTorus1:=TGLTorus(trunc64(tor));
  GLTorus1.MinorRadius:=inr;
  GLTorus1.MajorRadius:=outr;
  GLTorus1.Rings:=trunc64(ring);
  GLTorus1.Sides:=trunc64(side);
  result:=integer(GLTorus1);
end;

function TorusGetOptions(tor,ind: real): real; stdcall;
var
  tor1: TGLTorus;
begin
  tor1:=TGLTorus(trunc64(tor));
  if (ind=0) then
      result:=tor1.MinorRadius;
  if (ind=1) then
      result:=tor1.MajorRadius;
  if (ind=2) then
      result:=tor1.Rings;
  if (ind=3) then
      result:=tor1.Sides;
end;


function DiskSetOptions(disk,inr,outr,starta,sweepa,loop,slic: real): real; stdcall;
var
  GLDisk1: TGLDisk;
begin
  GLDisk1:=TGLDisk(trunc64(disk));
  GLDisk1.InnerRadius:=inr;
  GLDisk1.OuterRadius:=outr;
  GLDisk1.StartAngle:=starta;
  GLDisk1.SweepAngle:=sweepa;
  GLDisk1.Loops:=trunc64(loop);
  GLDisk1.Slices:=trunc64(slic);
  result:=1;
end;

function DiskGetOptions(disk,ind: real): real; stdcall;
var
  disk1: TGLDisk;
begin
  disk1:=TGLDisk(trunc64(disk));
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


function FrustrumSetOptions(fr,basew,based,apexh,cuth: real): real; stdcall;
var
  GLFrustrum1: TGLFrustrum;
begin
  GLFrustrum1:=TGLFrustrum(trunc64(fr));
  GLFrustrum1.BaseWidth:=basew;
  GLFrustrum1.BaseDepth:=based;
  GLFrustrum1.ApexHeight:=apexh;
  GLFrustrum1.Height:=cuth;
  result:=1;
end;

function FrustrumGetOptions(fr,ind: real): real; stdcall;
var
  fr1: TGLFrustrum;
begin
  fr1:=TGLFrustrum(trunc64(fr));
  if (ind=0) then
      result:=fr1.BaseWidth;
  if (ind=1) then
      result:=fr1.BaseDepth;
  if (ind=2) then
      result:=fr1.ApexHeight;
  if (ind=3) then
      result:=fr1.Height;
end;

function ObjectGetMaterial(obj:real): pchar; stdcall;
var
  object1:TGLSCeneObject;
begin
  object1:=TGLSceneObject(trunc64(obj));
  result:=pchar(object1.Material.LibMaterialName);
end;

function MaterialGetColor(mtrl: pchar; index: real): real; stdcall;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if (index=0) then
    result:=mat.Material.FrontProperties.Ambient.AsWinColor;
  if (index=1) then
    result:=mat.Material.FrontProperties.Diffuse.AsWinColor;
  if (index=2) then
    result:=mat.Material.FrontProperties.Specular.AsWinColor;
  if (index=3) then
    result:=mat.Material.FrontProperties.Emission.AsWinColor;	
end;

function MaterialGetAlpha(mtrl: pchar; index: real): real; stdcall;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  if (index=0) then
    result:=mat.Material.FrontProperties.Ambient.Alpha;
  if (index=1) then
    result:=mat.Material.FrontProperties.Diffuse.Alpha;
  if (index=2) then
    result:=mat.Material.FrontProperties.Specular.Alpha;
  if (index=3) then
    result:=mat.Material.FrontProperties.Emission.Alpha;	
end;

function GridSetTile(grid,x,y,z: real): real; stdcall;
var
  GLXyzGrid1: TGLXYZGrid;
begin
  GLXyzGrid1:=TGLXYZGrid(trunc64(grid));
  GLXyzGrid1.XSamplingScale.Min:= -x;
  GLXyzGrid1.XSamplingScale.max:=  x;
  GLXyzGrid1.YSamplingScale.Min:=  -y;
  GLXyzGrid1.YSamplingScale.max:=   y;
  GLXyzGrid1.ZSamplingScale.Min:= -z;
  GLXyzGrid1.ZSamplingScale.max:=  z;
  result:=1;
end;

function GridSetStep(grid,step: real): real; stdcall;
var
  GLXyzGrid1: TGLXYZGrid;
begin
  GLXyzGrid1:=TGLXYZGrid(trunc64(grid));
  GLXyzGrid1.XSamplingScale.step:= step;
  GLXyzGrid1.YSamplingScale.step:= step;
  GLXyzGrid1.ZSamplingScale.step:= step;
  result:=1;
end;

function LinesSetNode(lines, ind, x, y, z: real): real; stdcall;
var
  li: TGLLines;
begin
  li := TGLLines(trunc64(lines));
  li.Nodes[trunc64(ind)].X := x;
  li.Nodes[trunc64(ind)].Y := y;
  li.Nodes[trunc64(ind)].Y := z;
  result := 1.0;
end;
