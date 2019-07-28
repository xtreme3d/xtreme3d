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


//>>>>>>>>>>Xtreme3D 3.7.3<<<<<<<<<<


function ViewerRenderToFilePNG(viewer:real; fname:pchar): real; stdcall; //RenderToPNG
var
  bmp: TBitmap;
  bufw,bufb: TBitmap;
  png: TPNGObject;
  i,j: integer;
  pw,pb,pr,pa: PByteArray;
  f: single;
  oldColor: TColor;
begin  
	oldColor:=TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor;
    TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor := $ffffff;
    TGLSceneViewer(trunc64(viewer)).Buffer.Render;
    bufw := TGLSceneViewer(trunc64(viewer)).Buffer.CreateSnapShotBitmap;

    TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor := 0;
    TGLSceneViewer(trunc64(viewer)).Buffer.Render;
    bufb := TGLSceneViewer(trunc64(viewer)).Buffer.CreateSnapShotBitmap;

	TGLSceneViewer(trunc64(viewer)).Buffer.BackgroundColor := oldColor;
    TGLSceneViewer(trunc64(viewer)).Buffer.Render;
	bmp := TBitmap.Create;
    bmp.PixelFormat := pf32bit;
    bmp.Transparent := true;
    bmp.Width := TGLSceneViewer(trunc64(viewer)).Width;
    bmp.Height := TGLSceneViewer(trunc64(viewer)).Height;

    for j := 0 to bufw.Height - 1 do begin
      pw := bufw.ScanLine[ j ];
      pb := bufb.ScanLine[ j ];
      pr := bmp.ScanLine[ j ];
      for i := 0 to bufw.Width - 1 do begin
      // alpha
        pr[i * 4 + 3] := pb[i * 4 + 1] - pw[i * 4 + 1] + 255;
      // color
        f := 255 / pr[i * 4 + 3];
        pr[i * 4] := round( clampValue( pb[i * 4] * f, 0, 255 ));
        pr[i * 4 + 1] := round( clampValue( pb[i * 4 + 1] * f, 0, 255 ));
        pr[i * 4 + 2] := round( clampValue( pb[i * 4 + 2] * f, 0, 255 ));
      end;
    end;
    png := TPNGObject.Create;
    png.Assign( bmp );
    png.CreateAlpha;
	for j := 0 to png.Height - 1 do begin
      pr := bmp.ScanLine[ j ];
      pa := png.AlphaScanline[ j ];
      for i := 0 to png.Width - 1 do begin
        pa[i] := pr[i * 4 + 3];
	  end;	
	end;
    png.SaveToFile(String(fname));
    png.Free;
	bmp.Free;
  result:=1;
end;


function ViewerPixelRayToWorld(viewer,x, y,ind: real): real; stdcall;
var
vec: TAffineVector;
begin
  vec:=TGLSceneViewer(trunc64(viewer)).Buffer.PixelRayToWorld(trunc64(x),trunc64(y));
  result:=vec[trunc64(ind)];
end;


function ViewerShadeModel(viewer,ind: real): real; stdcall;
begin
if (ind=0) then
  TGLSceneViewer(trunc64(viewer)).Buffer.ShadeModel:=smFlat;
if (ind=1) then
   TGLSceneViewer(trunc64(viewer)).Buffer.ShadeModel:=smSmooth;
result:=1;
end;


function PipeCreate(divs,slic, parent: real): real; stdcall;
var
  pipe: TGLPipe;
begin
    if not (parent=0) then
    pipe:=TGLPipe.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    pipe:=TGLPipe.CreateAsChild(scene.Objects);
	
	pipe.Division:=trunc64(divs);
	pipe.Slices:=trunc64(slic);
	//pipe.NodesColorMode:=pncmAmbientAndDiffuse;
  result:=Integer(pipe);
end;

function PipeAddNode(pipe, x,y,z: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.AddNode(x,y,z);
  result := 1;
end;

function PipeSetDivision(pipe, divs: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Division:=trunc64(divs);
  result := 1;
end;

function PipeSetSplineMode(pipe, mode: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  if mode = 0 then pipeObj.SplineMode := lsmLines;
  if mode = 1 then pipeObj.SplineMode := lsmCubicSpline;
  if mode = 2 then pipeObj.SplineMode := lsmBezierSpline;
  if mode = 3 then pipeObj.SplineMode := lsmNURBSCurve;
  if mode = 4 then pipeObj.SplineMode := lsmSegments;
  result := 1;
end;

function PipeDeleteNode(pipe, ind: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Nodes.Delete(trunc64(ind));
  result := 1;
end;

function PipeSetRadius(pipe, rad: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Radius:=rad;
  result := 1;
end;

function PipeSetNode(pipe,ind, x,y,z: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Nodes[trunc64(ind)].X:=x;
  pipeObj.Nodes[trunc64(ind)].Y:=y;
  pipeObj.Nodes[trunc64(ind)].Z:=z;
  result := 1;
end;

function PipeSetSlices(pipe,slic: real): real; stdcall;
var
  pipeObj: TGLPipe;
begin
  pipeObj:=TGLPipe(trunc64(pipe));
  pipeObj.Slices:=trunc64(slic);
  result := 1;
end;


function ActorGetAnimationName (actor,ind:real): pchar; stdcall;
var
  act: TGLActor;
begin
  act:=TGLActor(trunc64(actor));
  result := pchar(act.Animations.Items[trunc64(ind)].Name);
end;

function ActorGetAnimationCount (actor:real): real; stdcall;
var
  act: TGLActor;
begin
  act:=TGLActor(trunc64(actor));
  result := act.Animations.Count;
end;

function ActorAnimationDestroy (actor,index:real): real; stdcall;
var
  act: TGLActor;
begin
  act:=TGLActor(trunc64(actor));
  //act.Animations.Items[trunc64(index)].Destroy;
  act.Animations.Delete(trunc64(index));
  result := 1;
end;

function ActorAnimationNextFrame (actor:real): real; stdcall;
var
  act: TGLActor;
begin
  act:=TGLActor(trunc64(actor));
  act.NextFrame;
  result := 1;
end;

function ActorAnimationPrevFrame (actor:real): real; stdcall;
var
  act: TGLActor;
begin
  act:=TGLActor(trunc64(actor));
  act.PrevFrame;
  result := 1;
end;

function MovementPathShow(pat,vis: real): real; stdcall;
var
 path: TGLMovementPath;
begin
  path := TGLMovementPath(trunc64(pat));
  path.ShowPath := Boolean(trunc64(vis));
  Result := 1;
end;

function MovementPathSetLoop(pat,loopn: real): real; stdcall;
var
 path: TGLMovementPath;
begin
  path := TGLMovementPath(trunc64(pat));
  path.Looped := Boolean(trunc64(loopn));
  Result := 1;
end;

function MovementPathDeleteNode(pat,node: real): real; stdcall;
var
 path: TGLMovementPath;
 nod: TGLPathNode;
begin
  path := TGLMovementPath(trunc64(pat));
  nod := TGLPathNode(trunc64(node));
  path.DeleteNode(nod);
  Result := 1;
end;

function HUDSpriteXTiles(sprite,xtls: real): real; stdcall;
var
 spr: TGLHUDSprite;
begin
  spr := TGLHUDSprite(trunc64(sprite));
  spr.XTiles:=trunc64(xtls);
  Result := 1;
end;

function HUDSpriteYTiles(sprite,ytls: real): real; stdcall;
var
 spr: TGLHUDSprite;
begin
  spr := TGLHUDSprite(trunc64(sprite));
  spr.YTiles:=trunc64(ytls);
  Result := 1;
end;


function TilePlaneCreate(parent: real): real; stdcall;
var
  tplane: TGLTilePlane;
begin
    if not (parent=0) then
    tplane:=TGLTilePlane.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    tplane:=TGLTilePlane.CreateAsChild(scene.Objects);
	tplane.SortByMaterials:=True;

  result:=Integer(tplane);
end;

function TilePlaneSetTile(tplane,x,y:real; mat: pchar): real; stdcall;
var
  tileplane: TGLTilePlane;
begin

  tileplane:=TGLTilePlane(trunc64(tplane));
  tileplane.MaterialLibrary:=matlib;
  tileplane.Tiles[trunc64(x), trunc64(y)]:=Integer(matlib.LibMaterialByName(mat).Index);
  tileplane.StructureChanged;  
  result:=1;
end;


//function TilePlaneSetTileByIndex(tplane,x,y,index:real): real; stdcall;
//var
//  tileplane: TGLTilePlane;
//begin

//  tileplane:=TGLTilePlane(trunc64(tplane));
//  tileplane.MaterialLibrary:=matlib;
//  tileplane.Tiles[trunc64(x), trunc64(y)]:=trunc64(index);
//  tileplane.StructureChanged;
//  result:=1;
//end;



function ActorTriangleCount(actor: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  result:=Integer(GLActor1.MeshObjects.TriangleCount);
end;

function FreeformMeshObjectGetName(ff,mesh: real): pchar; stdcall;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  result:=pchar(GLFreeForm1.MeshObjects.Items[trunc64(mesh)].Name);
end;

function FreeformMeshObjectSetName(ff,mesh: real; name: pchar): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  GLFreeForm1.MeshObjects.Items[trunc64(mesh)].Name:=name;
  result:=1;
end;

function FreeformMeshObjectDestroy(ff,mesh: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  GLFreeForm1.MeshObjects.Items[trunc64(mesh)].Destroy;
  result:=1;
end;

function ObjectFindByName (name: pchar): real; stdcall;
begin
  result:=Integer(scene.FindSceneObject(name));
end;


//----------VerletWorld----------

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








