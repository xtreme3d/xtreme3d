library xtreme3d;
uses
  Windows, Messages, Classes, Controls, StdCtrls, ExtCtrls, Dialogs, SysUtils, TypInfo,
  GLScene, GLObjects, GLWin32FullScreenViewer, GLMisc, GLGraph,
  GLCollision, GLTexture, OpenGL1x, VectorGeometry, Graphics,
  GLVectorFileObjects, GLWin32Viewer, GLSpaceText, GLGeomObjects, GLCadencer,
  Jpeg, Tga, DDS, PNG, GLProcTextures, Spin, GLVfsPAK, GLCanvas, GLGraphics, GLPortal,
  GLHUDObjects, GLBitmapFont, GLWindowsFont, GLImposter, VectorTypes, GLUtils,
  GLPolyhedron, GLTeapot, GLzBuffer, GLFile3DS, GLFileGTS, GLFileLWO, GLFileMD2,
  GLFileMD3, Q3MD3, GLFileMS3D, GLFileMD5, GLFileNMF, GLFileNurbs, GLFileObj, GLFileOCT,
  GLFilePLY, GLFileQ3BSP, GLFileSMD, GLFileSTL, GLFileTIN, GLFileB3D,
  GLFileMDC, GLFileVRML, GLFileLOD, GLFileX, GLFileCSM, GLFileLMTS, GLFileASE, GLFileDXS,
  GLPhongShader, VectorLists, GLThorFX, GLFireFX,
  GLTexCombineShader, GLBumpShader, GLCelShader, GLContext, GLTerrainRenderer, GLHeightData,
  GLBlur, GLSLShader, GLMultiMaterialShader, GLOutlineShader, GLHiddenLineShader,
  ApplicationFileIO, GLMaterialScript, GLWaterPlane, GeometryBB, GLExplosionFx,
  GLSkyBox, GLShadowPlane, GLShadowVolume, GLSkydome, GLLensFlare, GLDCE,
  GLNavigator, GLFPSMovement, GLMirror, SpatialPartitioning, GLSpatialPartitioning,
  GLTrail, GLTree, GLMultiProxy, GLODEManager, dynode, GLODECustomColliders,
  GLShadowMap, MeshUtils, pngimage, GLRagdoll, GLODERagdoll, GLMovement, GLHUDShapes, GLActorProxy,
  GLFBO, Hashes, Freetype, GLFreetypeFont, GLClippingPlane, GLLightFx,
  Keyboard, Forms, Kraft, GLKraft, GLFileFBX, GLCrossPlatform;

type
   TEmpty = class(TComponent)
    private
   end;

const
   {$I 'bumpshader'}
   {$I 'phongshader'}

var
  showLoadingErrors: Boolean;
  scene: TGLScene;
  matlib: TGLMaterialLibrary;
  memviewer: TGLMemoryViewer;
  cadencer: TGLCadencer;
  empty: TEmpty;

  collisionPoint: TVector;
  collisionNormal: TVector;

  ode: TGLODEManager;
  odeRagdollWorld: TODERagdollWorld;
  jointList: TGLODEJointList;

  kraftRaycastPoint: TKraftVector3;
  kraftRaycastNormal: TKraftVector3;

{$R *.res}

function LoadStringFromFile2(const fileName : String) : String;
var
   n : Cardinal;
	fs : TStream;
begin
   if FileStreamExists(fileName) then begin
   	fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
      try
         n:=fs.Size;
   	   SetLength(Result, n);
         if n>0 then
         	fs.Read(Result[1], n);
      finally
   	   fs.Free;
      end;
   end else Result:='';
end;

function VectorDivide(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result[0]:=v1[0]/delta;
   Result[1]:=v1[1]/delta;
   Result[2]:=v1[2]/delta;
end;

function VectorMultiply(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result[0]:=v1[0]*delta;
   Result[1]:=v1[1]*delta;
   Result[2]:=v1[2]*delta;
end;

function IsExtensionSupported(v: TGLSceneViewer; const Extension: string): Boolean;
var
   Buffer : String;
   ExtPos: Integer;
begin
   v.Buffer.RenderingContext.Activate;
   Buffer := StrPas(glGetString(GL_EXTENSIONS));
   // First find the position of the extension string as substring in Buffer.
   ExtPos := Pos(Extension, Buffer);
   Result := ExtPos > 0;
   // Now check that it isn't only a substring of another extension.
   if Result then
     Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
               or (Buffer[ExtPos + Length(Extension)]=' ');
   v.Buffer.RenderingContext.Deactivate;
end;

procedure GenMeshTangents(mesh: TMeshObject);
var
   i,j: Integer;
   v,t: array[0..2] of TAffineVector;

   x1, x2, y1, y2, z1, z2, t1, t2, s1, s2: Single;
   sDir, tDir: TAffineVector;
   sTan, tTan: TAffineVectorList;
   tangents, bitangents: TVectorList;
   sv, tv: array[0..2] of TAffineVector;
   r, oneOverR: Single;
   n, ta: TAffineVector;
   tang: TAffineVector;

   tangent,
   binormal   : array[0..2] of TVector;
   vt,tt      : TAffineVector;
   interp,dot : Single;

begin
   mesh.Tangents.Clear;
   mesh.Binormals.Clear;
   mesh.Tangents.Count:=mesh.Vertices.Count;
   mesh.Binormals.Count:=mesh.Vertices.Count;

   tangents := TVectorList.Create;
   tangents.Count:=mesh.Vertices.Count;

   bitangents := TVectorList.Create;
   bitangents.Count:=mesh.Vertices.Count; 

   sTan := TAffineVectorList.Create;
   tTan := TAffineVectorList.Create;
   sTan.Count := mesh.Vertices.Count;
   tTan.Count := mesh.Vertices.Count;

   for i:=0 to mesh.TriangleCount-1 do begin
      sv[0] := AffineVectorMake(0, 0, 0);
      tv[0] := AffineVectorMake(0, 0, 0);
      sv[1] := AffineVectorMake(0, 0, 0);
      tv[1] := AffineVectorMake(0, 0, 0);
      sv[2] := AffineVectorMake(0, 0, 0);
      tv[2] := AffineVectorMake(0, 0, 0);

      mesh.SetTriangleData(i,sTan,sv[0],sv[1],sv[2]);
      mesh.SetTriangleData(i,tTan,tv[0],tv[1],tv[2]);
   end;

   for i:=0 to mesh.TriangleCount-1 do begin
      mesh.GetTriangleData(i,mesh.Vertices,v[0],v[1],v[2]);
      mesh.GetTriangleData(i,mesh.TexCoords,t[0],t[1],t[2]);

      x1 := v[1][0] - v[0][0];
      x2 := v[2][0] - v[0][0];
      y1 := v[1][1] - v[0][1];
      y2 := v[2][1] - v[0][1];
      z1 := v[1][2] - v[0][2];
      z2 := v[2][2] - v[0][2];

      s1 := t[1][0] - t[0][0];
      s2 := t[2][0] - t[0][0];
      t1 := t[1][1] - t[0][1];
      t2 := t[2][1] - t[0][1];

      r := (s1 * t2) - (s2 * t1);

      if r = 0.0 then
        r := 1.0;

      oneOverR := 1.0 / r;

      sDir[0] := (t2 * x1 - t1 * x2) * oneOverR;
      sDir[1] := (t2 * y1 - t1 * y2) * oneOverR;
      sDir[2] := (t2 * z1 - t1 * z2) * oneOverR;

      tDir[0] := (s1 * x2 - s2 * x1) * oneOverR;
      tDir[1] := (s1 * y2 - s2 * y1) * oneOverR;
      tDir[2] := (s1 * z2 - s2 * z1) * oneOverR;

      mesh.GetTriangleData(i,sTan,sv[0],sv[1],sv[2]);
      mesh.GetTriangleData(i,tTan,tv[0],tv[1],tv[2]);

      sv[0] := VectorAdd(sv[0], sDir);
      tv[0] := VectorAdd(tv[0], tDir);
      sv[1] := VectorAdd(sv[1], sDir);
      tv[1] := VectorAdd(tv[1], tDir);
      sv[2] := VectorAdd(sv[2], sDir);
      tv[2] := VectorAdd(tv[2], tDir);

      mesh.SetTriangleData(i,sTan,sv[0],sv[1],sv[2]);
      mesh.SetTriangleData(i,tTan,tv[0],tv[1],tv[2]);
   end;

   for i:=0 to mesh.Vertices.Count-1 do begin
      n := mesh.Normals[i];
      ta := sTan[i];
      tang := VectorSubtract(ta, VectorMultiply(n, VectorDotProduct(n, ta)));
      tang := VectorNormalize(tang);

      tangents[i] := VectorMake(tang, 1);
      bitangents[i] := VectorMake(VectorCrossProduct(n, tang), 1);
   end;

   mesh.Tangents := tangents;
   mesh.Binormals := bitangents;
end;

function getODEBehaviour(obj: TGLBaseSceneObject): TGLODEBehaviour;
begin
  result := TGLODEBehaviour(obj.Behaviours.GetByClass(TGLODEBehaviour));
end;

function getJointAxisParams(j: TODEJointBase; axis: Integer): TODEJointParams;
var
  res: TODEJointParams;
begin
  if j is TODEJointHinge then
  begin
    if axis = 1 then
      res := TODEJointHinge(j).AxisParams;
  end
  else if j is TODEJointHinge2 then
  begin
    if axis = 1 then
      res := TODEJointHinge2(j).Axis1Params
    else if axis = 2 then
      res := TODEJointHinge2(j).Axis2Params;
  end
  else if j is TODEJointUniversal then
  begin
    if axis = 1 then
      res := TODEJointUniversal(j).Axis1Params
    else if axis = 2 then
      res := TODEJointUniversal(j).Axis2Params;
  end;
  result := res;
end;

procedure onContactStay(const ContactPair: PKraftContactPair; const WithShape: TKraftShape);
begin
end;

function normalizeSlashes(s: string): string;
var
   i: integer;
begin
   SetLength(Result, Length(s));
   for i := 1 to Length(s) do
      if s[i] = '/' then
         Result[i] := '\'
      else
         Result[i] := s[i];
end;

function InvertBitmap(Bitmap:TBitmap): TBitmap;
begin
  Bitmap.Canvas.CopyMode := cmDstInvert;
  Bitmap.Canvas.CopyRect(Bitmap.Canvas.ClipRect, Bitmap.Canvas, Bitmap.Canvas.ClipRect);
  Bitmap.Canvas.CopyMode := cmSrcCopy;
  Result := Bitmap;
end;

{$I 'xtreme3d/engine'}
{$I 'xtreme3d/pak'}
{$I 'xtreme3d/viewer'}
{$I 'xtreme3d/dummycube'}
{$I 'xtreme3d/camera'}
{$I 'xtreme3d/light'}
{$I 'xtreme3d/lightfx'}
{$I 'xtreme3d/fonttext'}
{$I 'xtreme3d/sprite'}
{$I 'xtreme3d/hudshapes'}
{$I 'xtreme3d/primitives'}
{$I 'xtreme3d/actor'}
{$I 'xtreme3d/freeform'}
{$I 'xtreme3d/object'}
{$I 'xtreme3d/polygon'}
{$I 'xtreme3d/material'}
{$I 'xtreme3d/shaders'}
{$I 'xtreme3d/thorfx'}
{$I 'xtreme3d/firefx'}
{$I 'xtreme3d/lensflare'}
{$I 'xtreme3d/terrain'}
{$I 'xtreme3d/blur'}
{$I 'xtreme3d/skybox'}
{$I 'xtreme3d/trail'}
{$I 'xtreme3d/shadowplane'}
{$I 'xtreme3d/shadowvolume'}
{$I 'xtreme3d/skydome'}
{$I 'xtreme3d/water'}
{$I 'xtreme3d/lines'}
{$I 'xtreme3d/tree'}
{$I 'xtreme3d/navigator'}
{$I 'xtreme3d/movement'}
{$I 'xtreme3d/dce'}
{$I 'xtreme3d/fps'}
{$I 'xtreme3d/mirror'}
{$I 'xtreme3d/partition'}
{$I 'xtreme3d/memviewer'}
{$I 'xtreme3d/fbo'}
{$I 'xtreme3d/proxy'}
{$I 'xtreme3d/text'}
{$I 'xtreme3d/objecthash'}
{$I 'xtreme3d/grid'}
{$I 'xtreme3d/shadowmap'}
{$I 'xtreme3d/ode'}
{$I 'xtreme3d/kraft'}
{$I 'xtreme3d/clipplane'}
{$I 'xtreme3d/input'}
{$I 'xtreme3d/window'}
{$I 'xtreme3d/color'}

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


exports

//Engine
EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
Update, TrisRendered,
EngineSaveScene, EngineLoadScene, EngineRootObject,
EngineShowLoadingErrors, EngineSetMaxLights,
//Pak
SetPakArchive, PakGetFileCount, PakGetFileName, PakExtract, PakExtractFile,
//Viewer
ViewerCreate, ViewerSetCamera, ViewerEnableVSync, ViewerRenderToFile,
ViewerRender, ViewerSetAutoRender, ViewerRenderEx,
ViewerResize, ViewerSetVisible, ViewerGetPixelColor, ViewerGetPixelDepth,
ViewerSetLighting, ViewerSetBackgroundColor, ViewerSetAmbientColor, ViewerEnableFog,
ViewerSetFogColor, ViewerSetFogDistance, ViewerScreenToWorld, ViewerWorldToScreen,
ViewerCopyToTexture, ViewerGetFramesPerSecond, ViewerGetPickedObject,
ViewerScreenToVector, ViewerVectorToScreen, ViewerPixelToDistance, ViewerGetPickedObjectsList,
ViewerSetAntiAliasing,
ViewerSetOverrideMaterial,
ViewerIsOpenGLExtensionSupported,
ViewerGetGLSLSupported, ViewerGetFBOSupported, ViewerGetVBOSupported,
ViewerGetSize, ViewerGetPosition,
ViewerResetPerformanceMonitor,
//Dummycube
DummycubeCreate, DummycubeAmalgamate, DummycubeSetCameraMode, DummycubeSetVisible,
DummycubeSetEdgeColor, DummycubeSetCubeSize,
//Camera
CameraCreate, CameraSetStyle, CameraSetFocal, CameraSetSceneScale,
CameraScaleScene, CameraSetViewDepth, CameraSetTargetObject,
CameraMoveAroundTarget, CameraSetDistanceToTarget, CameraGetDistanceToTarget,
CameraCopyToTexture, CameraGetNearPlane, CameraSetNearPlaneBias,
CameraAbsoluteVectorToTarget, CameraAbsoluteRightVectorToTarget, CameraAbsoluteUpVectorToTarget,
CameraZoomAll, CameraScreenDeltaToVector, CameraScreenDeltaToVectorXY, CameraScreenDeltaToVectorXZ,
CameraScreenDeltaToVectorYZ, CameraAbsoluteEyeSpaceVector, CameraSetAutoLeveling,
CameraMoveInEyeSpace, CameraMoveTargetInEyeSpace, CameraPointInFront, CameraGetFieldOfView,
//Light
LightCreate, LightSetAmbientColor, LightSetDiffuseColor, LightSetSpecularColor,
LightSetAttenuation, LightSetShining, LightSetSpotCutoff, LightSetSpotExponent,
LightSetSpotDirection, LightSetStyle,
// LightFx
LightFXCreate, 
//Font & Text
BmpFontCreate, BmpFontLoad,
TTFontCreate, TTFontSetLineGap, TTFontSetEncoding, TTFontLoadCodePage,
WindowsBitmapfontCreate,
HUDTextCreate, FlatTextCreate,
HUDTextSetRotation, SpaceTextCreate, SpaceTextSetExtrusion, HUDTextSetFont,
FlatTextSetFont, SpaceTextSetFont, HUDTextSetColor, FlatTextSetColor, HUDTextSetText,
FlatTextSetText, SpaceTextSetText,
HUDSpriteGetMouseOver,
//Sprite
HUDSpriteCreate, 
SpriteCreate, SpriteSetSize, SpriteScale, SpriteSetRotation,
SpriteRotate, SpriteMirror, SpriteNoZWrite,
SpriteCreateEx, HUDSpriteCreateEx, SpriteSetBounds, SpriteSetBoundsUV,
SpriteSetOrigin,
//HUDShapes
HUDShapeRectangleCreate, HUDShapeCircleCreate, HUDShapeLineCreate, HUDShapeMeshCreate,
HUDShapeSetRotation, HUDShapeSetColor,
HUDShapeRotate, HUDShapeSetOrigin, HUDShapeSetSize, HUDShapeScale,
HUDShapeCircleSetRadius, HUDShapeCircleSetSlices, HUDShapeCircleSetAngles,
HUDShapeLineSetPoints, HUDShapeLineSetWidth,
HUDShapeMeshAddVertex, HUDShapeMeshAddTriangle,
HUDShapeMeshSetVertex, HUDShapeMeshSetTexCoord,
//Primitives
CubeCreate, CubeSetNormalDirection, PlaneCreate, SphereCreate, SphereSetAngleLimits,
CylinderCreate, ConeCreate, AnnulusCreate, TorusCreate, DiskCreate, FrustrumCreate,
DodecahedronCreate, IcosahedronCreate, TeapotCreate,
//Actor
ActorCreate, ActorCopy, ActorSetAnimationRange, ActorGetCurrentFrame, ActorSwitchToAnimation,
ActorSwitchToAnimationName, ActorSynchronize, ActorSetInterval, ActorSetAnimationMode,
ActorSetFrameInterpolation, ActorAddObject, ActorGetCurrentAnimation, ActorGetFrameCount,
ActorGetBoneCount, ActorGetBoneByName, ActorGetBoneRotation, ActorGetBonePosition,
ActorBoneExportMatrix, ActorMakeSkeletalTranslationStatic, ActorMakeSkeletalRotationDelta, 
ActorShowSkeleton, 
AnimationBlenderCreate, AnimationBlenderSetActor, AnimationBlenderSetAnimation,
AnimationBlenderSetRatio,
ActorLoadQ3TagList, ActorLoadQ3Animations, ActorQ3TagExportMatrix,
ActorMeshObjectsCount, ActorMeshSetVisible, ActorFaceGroupsCount, ActorFaceGroupGetMaterialName,
ActorFaceGroupSetMaterial,
ActorMoveBone, ActorRotateBone,
//Freeform
FreeformCreate, FreeformCreateEmpty,
FreeformSetMaterialLibraries,
FreeformAddMesh, FreeformMeshAddFaceGroup, 
FreeformMeshAddVertex, FreeformMeshAddNormal,
FreeformMeshAddTexCoord, FreeformMeshAddSecondTexCoord,
FreeformMeshAddTangent, FreeformMeshAddBinormal,

FreeformMeshGetVertex, FreeformMeshGetNormal,
FreeformMeshGetTexCoord, FreeformMeshGetSecondTexCoord,
FreeformMeshGetTangent, FreeformMeshGetBinormal,
FreeformMeshFaceGroupGetIndex,

FreeformMeshSetVertex, FreeformMeshSetNormal,
FreeformMeshSetTexCoord, FreeformMeshSetSecondTexCoord,
FreeformMeshSetTangent, FreeformMeshSetBinormal,
FreeformMeshFaceGroupSetIndex,

FreeformMeshFaceGroupAddTriangle,
FreeformMeshFaceGroupGetMaterial, FreeformMeshFaceGroupSetMaterial,
FreeformMeshFaceGroupGetLightmapIndex, FreeformMeshFaceGroupSetLightmapIndex,
FreeformMeshGenNormals, FreeformMeshGenTangents,
FreeformMeshVerticesCount, FreeformMeshTriangleCount, 
FreeformMeshObjectsCount, FreeformMeshSetVisible,
FreeformMeshSetSecondCoords,
FreeformMeshFaceGroupsCount, FreeformMeshFaceGroupTriangleCount,
FreeformMeshSetMaterial, FreeformUseMeshMaterials,
FreeformSphereSweepIntersect, FreeformPointInMesh,
FreeformToFreeforms,
FreeformMeshTranslate, FreeformMeshRotate, FreeformMeshScale,
FreeformSave,

FreeformGenTangents, FreeformBuildOctree,

FreeformCreateExplosionFX, FreeformExplosionFXReset,
FreeformExplosionFXEnable, FreeformExplosionFXSetSpeed,
//Terrain
BmpHDSCreate, BmpHDSCreateEmpty, BmpHDSSetInfiniteWarp, BmpHDSInvert,
BmpHDSSetHeight, BmpHDSGetHeight, BmpHDSSave,
TerrainCreate, TerrainSetHeightData, TerrainSetTileSize, TerrainSetTilesPerTexture,
TerrainSetQualityDistance, TerrainSetQualityStyle, TerrainSetMaxCLodTriangles,
TerrainSetCLodPrecision, TerrainSetOcclusionFrameSkip, TerrainSetOcclusionTesselate,
TerrainGetHeightAtObjectPosition, TerrainGetLastTriCount,
TerrainGetHDSPosition,
//Object
ObjectHide, ObjectShow, ObjectIsVisible,
ObjectCopy, ObjectDestroy, ObjectDestroyChildren,
ObjectSetPosition, ObjectGetPosition, ObjectGetAbsolutePosition,
ObjectSetPositionOfObject, ObjectAlignWithObject,
ObjectSetPositionX, ObjectSetPositionY, ObjectSetPositionZ,
ObjectGetPositionX, ObjectGetPositionY, ObjectGetPositionZ,
ObjectSetAbsolutePosition,
ObjectSetDirection, ObjectGetDirection,
ObjectSetAbsoluteDirection, ObjectGetAbsoluteDirection,
ObjectGetPitch, ObjectGetTurn, ObjectGetRoll, ObjectSetRotation,
ObjectMove, ObjectLift, ObjectStrafe, ObjectTranslate, ObjectRotate,
ObjectScale, ObjectSetScale, ObjectGetScale,
ObjectSetUpVector, ObjectPointToObject, 
ObjectShowAxes,
ObjectGetGroundHeight, ObjectSceneRaycast, ObjectRaycast,
ObjectGetCollisionPosition, ObjectGetCollisionNormal, 
ObjectSetMaterial,
ObjectGetDistance,
ObjectCheckCubeVsCube, ObjectCheckSphereVsSphere, ObjectCheckSphereVsCube,
ObjectCheckCubeVsFace, ObjectCheckFaceVsFace,
ObjectIsPointInObject,
ObjectSetCulling,
ObjectSetName, ObjectGetName, ObjectGetClassName,
ObjectSetTag, ObjectGetTag,
ObjectGetParent, ObjectGetChildCount, ObjectGetChild, ObjectGetIndex, ObjectFindChild,
ObjectGetBoundingSphereRadius,
ObjectGetAbsoluteUp, ObjectSetAbsoluteUp, ObjectGetAbsoluteRight,
ObjectGetAbsoluteXVector, ObjectGetAbsoluteYVector, ObjectGetAbsoluteZVector,
ObjectGetRight,
ObjectMoveChildUp, ObjectMoveChildDown,
ObjectSetParent, ObjectRemoveChild,
ObjectMoveObjectAround,
ObjectPitch, ObjectTurn, ObjectRoll,
ObjectGetUp,
ObjectRotateAbsolute, ObjectRotateAbsoluteVector,
ObjectSetMatrixColumn,
ObjectExportMatrix, ObjectExportAbsoluteMatrix,
ObjectInFrustum,
//Polygon
PolygonCreate, PolygonAddVertex, PolygonSetVertexPosition, PolygonDeleteVertex,
//Material
MaterialLibraryCreate, MaterialLibraryActivate, MaterialLibrarySetTexturePaths,
MaterialLibraryClear, MaterialLibraryDeleteUnused,
MaterialLibraryHasMaterial, MaterialLibraryLoadScript, 
MaterialCreate,
MaterialAddCubeMap, MaterialCubeMapLoadImage, MaterialCubeMapGenerate, MaterialCubeMapFromScene,
MaterialSaveTexture, MaterialSetBlendingMode, MaterialSetOptions,
MaterialSetTextureMappingMode, MaterialSetTextureMode,
MaterialSetShader, MaterialSetSecondTexture,
MaterialSetDiffuseColor, MaterialSetAmbientColor, MaterialSetSpecularColor, MaterialSetEmissionColor,
MaterialSetShininess,
MaterialSetPolygonMode, MaterialSetTextureImageAlpha,
MaterialSetTextureScale, MaterialSetTextureOffset,
MaterialSetTextureFilter, MaterialEnableTexture,
MaterialGetCount, MaterialGetName, MaterialGetNameFromLibrary,
MaterialSetFaceCulling,
MaterialSetTexture, MaterialSetSecondTexture,
MaterialSetTextureFormat, MaterialSetTextureCompression,
MaterialTextureRequiredMemory, MaterialSetFilteringQuality,
MaterialAddTextureEx, MaterialTextureExClear, MaterialTextureExDelete,
MaterialNoiseCreate, MaterialNoiseAnimate, MaterialNoiseSetDimensions,
MaterialNoiseSetMinCut, MaterialNoiseSetSharpness, MaterialNoiseSetSeamless,
MaterialNoiseRandomSeed,
MaterialGenTexture, MaterialSetTextureWrap,
MaterialGetTextureWidth, MaterialGetTextureHeight,
MaterialLoadTexture,
MaterialLoadTextureEx, MaterialSetTextureEx, MaterialGenTextureEx,
MaterialEnableTextureEx, MaterialHasTextureEx,
MaterialSetTextureExFromLibrary,
MaterialCullFrontFaces, MaterialSetZWrite,
//Shaders
ShaderEnable, 
BumpShaderCreate,
BumpShaderSetDiffuseTexture, BumpShaderSetNormalTexture, BumpShaderSetHeightTexture,
BumpShaderSetMaxLights, BumpShaderUseParallax, BumpShaderSetParallaxOffset,
BumpShaderSetShadowMap, BumpShaderSetShadowBlurRadius, BumpShaderUseAutoTangentSpace,
CelShaderCreate, CelShaderSetLineColor, CelShaderSetLineWidth, CelShaderSetOptions,
MultiMaterialShaderCreate,
HiddenLineShaderCreate, HiddenLineShaderSetLineSmooth, HiddenLineShaderSetSolid,
HiddenLineShaderSetSurfaceLit, HiddenLineShaderSetFrontLine, HiddenLineShaderSetBackLine,
OutlineShaderCreate, OutlineShaderSetLineColor, OutlineShaderSetLineWidth,
TexCombineShaderCreate, TexCombineShaderAddCombiner,
TexCombineShaderMaterial3, TexCombineShaderMaterial4,
PhongShaderCreate, PhongShaderUseTexture, PhongShaderSetMaxLights,
GLSLShaderCreate, GLSLShaderCreateParameter,
GLSLShaderSetParameter1i, GLSLShaderSetParameter1f, GLSLShaderSetParameter2f,
GLSLShaderSetParameter3f, GLSLShaderSetParameter4f,
GLSLShaderSetParameterTexture, GLSLShaderSetParameterSecondTexture,
GLSLShaderSetParameterMatrix, GLSLShaderSetParameterInvMatrix,
GLSLShaderSetParameterShadowTexture, GLSLShaderSetParameterShadowMatrix,
GLSLShaderSetParameterFBOColorTexture, GLSLShaderSetParameterFBODepthTexture,
GLSLShaderSetParameterViewMatrix, GLSLShaderSetParameterInvViewMatrix,
GLSLShaderSetParameterHasTextureEx,
//ThorFX
ThorFXManagerCreate, ThorFXSetColor, ThorFXEnableCore, ThorFXEnableGlow,
ThorFXSetMaxParticles, ThorFXSetGlowSize, ThorFXSetVibrate, ThorFXSetWildness,
ThorFXSetTarget, ThorFXCreate,
// FireFX
FireFXManagerCreate, FireFXCreate,
FireFXSetColor, FireFXSetMaxParticles, FireFXSetParticleSize,
FireFXSetDensity, FireFXSetEvaporation, FireFXSetCrown,
FireFXSetLife, FireFXSetBurst, FireFXSetRadius, FireFXExplosion,
//Lensflare
LensflareCreate, LensflareSetSize, LensflareSetSeed, LensflareSetSqueeze,
LensflareSetStreaks, LensflareSetStreakWidth, LensflareSetSecs,
LensflareSetResolution, LensflareSetElements, LensflareSetGradients,
//Skydome
SkydomeCreate, SkydomeSetOptions, SkydomeSetDeepColor, SkydomeSetHazeColor,
SkydomeSetNightColor, SkydomeSetSkyColor, SkydomeSetSunDawnColor, SkydomeSetSunZenithColor,
SkydomeSetSunElevation, SkydomeSetTurbidity,
SkydomeAddRandomStars, SkydomeAddStar, SkydomeClearStars, SkydomeTwinkleStars, 
//Water
WaterCreate, WaterCreateRandomRipple,
WaterCreateRippleAtGridPosition, WaterCreateRippleAtWorldPosition,
WaterCreateRippleAtObjectPosition,
WaterSetMask, WaterSetActive, WaterReset,
WaterSetRainTimeInterval, WaterSetRainForce,
WaterSetViscosity, WaterSetElastic, WaterSetResolution,
WaterSetLinearWaveHeight, WaterSetLinearWaveFrequency,
//Blur
BlurCreate, BlurSetPreset, BlurSetOptions, BlurSetResolution,
BlurSetColor, BlurSetBlendingMode,
//Skybox
SkyboxCreate, SkyboxSetMaterial, SkyboxSetClouds, SkyboxSetStyle,
//Lines
LinesCreate, LinesAddNode, LinesDeleteNode, LinesSetColors, LinesSetSize,
LinesSetSplineMode, LinesSetNodesAspect, LinesSetDivision,
//Tree
TreeCreate, TreeSetMaterials, TreeSetBranchFacets, TreeBuildMesh,
TreeSetBranchNoise, TreeSetBranchAngle, TreeSetBranchSize, TreeSetBranchRadius,
TreeSetBranchTwist, TreeSetDepth, TreeSetLeafSize, TreeSetLeafThreshold, TreeSetSeed,
//Trail
TrailCreate, TrailSetObject, TrailSetAlpha, TrailSetLimits, TrailSetMinDistance,
TrailSetUVScale, TrailSetMarkStyle, TrailSetMarkWidth, TrailSetEnabled, TrailClearMarks,
//Shadowplane
ShadowplaneCreate, ShadowplaneSetLight, ShadowplaneSetObject, ShadowplaneSetOptions,
//Shadowvolume
ShadowvolumeCreate, ShadowvolumeSetActive,
ShadowvolumeAddLight, ShadowvolumeRemoveLight,
ShadowvolumeAddOccluder, ShadowvolumeRemoveOccluder,
ShadowvolumeSetDarkeningColor, ShadowvolumeSetMode, ShadowvolumeSetOptions,
//Navigator
NavigatorCreate, NavigatorSetObject, NavigatorSetUseVirtualUp, NavigatorSetVirtualUp,  
NavigatorTurnHorizontal, NavigatorTurnVertical, NavigatorMoveForward,
NavigatorStrafeHorizontal, NavigatorStrafeVertical, NavigatorStraighten,
NavigatorFlyForward, NavigatorMoveUpWhenMovingForward, NavigatorInvertHorizontalWhenUpsideDown,
NavigatorSetAngleLock, NavigatorSetAngles,
//Movement
MovementCreate, MovementStart, MovementStop, MovementAutoStartNextPath, 
MovementAddPath, MovementSetActivePath, MovementPathSetSplineMode,
MovementPathAddNode,
MovementPathNodeSetPosition, MovementPathNodeSetRotation,
MovementPathNodeSetSpeed,
//DCE
DceManagerCreate, DceManagerStep, DceManagerSetGravity, DceManagerSetWorldDirection,
DceManagerSetWorldScale, DceManagerSetMovementScale,
DceManagerSetLayers, DceManagerSetManualStep,
DceDynamicSetManager, DceDynamicSetActive, DceDynamicIsActive,
DceDynamicSetUseGravity, DceDynamicSetLayer, DceDynamicGetLayer,
DceDynamicSetSolid, DceDynamicSetFriction, DceDynamicSetBounce,
DceDynamicSetSize, DceDynamicSetSlideOrBounce,
DceDynamicApplyAcceleration, DceDynamicApplyAbsAcceleration,
DceDynamicStopAcceleration, DceDynamicStopAbsAcceleration,
DceDynamicJump, DceDynamicMove, DceDynamicMoveTo,
DceDynamicSetVelocity, DceDynamicGetVelocity,
DceDynamicSetAbsVelocity, DceDynamicGetAbsVelocity,
DceDynamicApplyImpulse, DceDynamicApplyAbsImpulse,
DceDynamicInGround, DceDynamicSetMaxRecursionDepth,
DceStaticSetManager, DceStaticSetActive, DceStaticSetShape, DceStaticSetLayer,
DceStaticSetSize, DceStaticSetSolid, DceStaticSetFriction, DceStaticSetBounceFactor,
//FPSManager
FpsManagerCreate, FpsManagerSetNavigator, FpsManagerSetMovementScale,
FpsManagerAddMap, FpsManagerRemoveMap, FpsManagerMapSetCollisionGroup,
FpsSetManager, FpsSetCollisionGroup, FpsSetSphereRadius, FpsSetGravity,
FpsMove, FpsStrafe, FpsLift, FpsGetVelocity,
//Mirror
MirrorCreate, MirrorSetObject, MirrorSetOptions,
MirrorSetShape, MirrorSetDiskOptions,
//Partition
OctreeCreate, QuadtreeCreate, PartitionDestroy, PartitionAddLeaf,
PartitionLeafChanged, PartitionQueryFrustum, PartitionQueryLeaf,
PartitionQueryAABB, PartitionQueryBSphere, PartitionGetNodeTests,
PartitionGetNodeCount, PartitionGetResult, PartitionGetResultCount,
PartitionResultShow, PartitionResultHide,
//Proxy
ProxyObjectCreate, ProxyObjectSetOptions, ProxyObjectSetTarget,
MultiProxyObjectCreate, MultiProxyObjectAddTarget,
ActorProxyObjectCreate, ActorProxyObjectSwitchToAnimation,
ActorProxyObjectSetAnimationRange, ActorProxyObjectSetInterval,
//Text
TextRead, TextConvertANSIToUTF8,
//ObjectHash
ObjectHashCreate, ObjectHashSetItem, ObjectHashGetItem,
ObjectHashDeleteItem, ObjectHashGetItemCount,
ObjectHashClear, ObjectHashDestroy,
//Grid
GridCreate, GridSetLineStyle, GridSetLineSmoothing, GridSetParts,
GridSetColor, GridSetSize, GridSetPattern,
//ClipPlane
ClipPlaneCreate, ClipPlaneEnable, ClipPlaneSetPlane,
//MemoryViewer
MemoryViewerCreate, MemoryViewerSetCamera, MemoryViewerRender,
MemoryViewerSetViewport, MemoryViewerCopyToTexture,
//FBO
FBOCreate, FBOSetCamera, FBOSetViewer,
FBORenderObject, FBORenderObjectEx,
FBOSetOverrideMaterial,
FBOSetColorTextureFormat,
//ShadowMap
ShadowMapCreate, ShadowMapSetCamera, ShadowMapSetCaster,
ShadowMapSetProjectionSize, ShadowMapSetZScale, ShadowMapSetZClippingPlanes,
ShadowMapSetFBO,
ShadowMapRender,
//ODE
OdeManagerCreate, OdeManagerDestroy, OdeManagerStep, OdeManagerGetNumContactJoints,
OdeManagerSetGravity, OdeManagerSetSolver, OdeManagerSetIterations,
OdeManagerSetMaxContacts, OdeManagerSetVisible, OdeManagerSetGeomColor,
OdeWorldSetAutoDisableFlag, OdeWorldSetAutoDisableLinearThreshold,
OdeWorldSetAutoDisableAngularThreshold, OdeWorldSetAutoDisableSteps, OdeWorldSetAutoDisableTime,
OdeStaticCreate, OdeDynamicCreate, OdeTerrainCreate,
OdeDynamicCalculateMass, OdeDynamicCalibrateCenterOfMass,
OdeDynamicAlignObject, OdeDynamicEnable, OdeDynamicSetAutoDisableFlag,
OdeDynamicSetAutoDisableLinearThreshold, OdeDynamicSetAutoDisableAngularThreshold,
OdeDynamicSetAutoDisableSteps, OdeDynamicSetAutoDisableTime,
OdeDynamicAddForce, OdeDynamicAddForceAtPos, OdeDynamicAddForceAtRelPos, 
OdeDynamicAddRelForce, OdeDynamicAddRelForceAtPos, OdeDynamicAddRelForceAtRelPos,
OdeDynamicAddTorque, OdeDynamicAddRelTorque,
OdeDynamicGetContactCount, OdeStaticGetContactCount,
OdeAddBox, OdeAddSphere, OdeAddPlane, OdeAddCylinder, OdeAddCone, OdeAddCapsule, OdeAddTriMesh,
OdeElementSetDensity,
OdeSurfaceEnableRollingFrictionCoeff, OdeSurfaceSetRollingFrictionCoeff,
OdeSurfaceSetMode, OdeSurfaceSetMu, OdeSurfaceSetMu2,
OdeSurfaceSetBounce, OdeSurfaceSetBounceVel, OdeSurfaceSetSoftERP, OdeSurfaceSetSoftCFM,
OdeSurfaceSetMotion1, OdeSurfaceSetMotion2, OdeSurfaceSetSlip1, OdeSurfaceSetSlip2,
OdeAddJointBall, OdeAddJointFixed, OdeAddJointHinge, OdeAddJointHinge2,
OdeAddJointSlider, OdeAddJointUniversal, 
OdeJointSetObjects, OdeJointEnable, OdeJointInitialize,
OdeJointSetAnchor, OdeJointSetAnchorAtObject, OdeJointSetAxis1, OdeJointSetAxis2,
OdeJointSetBounce, OdeJointSetCFM, OdeJointSetFMax, OdeJointSetFudgeFactor,
OdeJointSetHiStop, OdeJointSetLoStop, OdeJointSetStopCFM, OdeJointSetStopERP, OdeJointSetVel,
OdeRagdollCreate, OdeRagdollHingeJointCreate, OdeRagdollUniversalJointCreate,
OdeRagdollDummyJointCreate, OdeRagdollBoneCreate,
OdeRagdollBuild, OdeRagdollEnable, OdeRagdollUpdate,

OdeDynamicSetVelocity, OdeDynamicSetAngularVelocity,
OdeDynamicGetVelocity, OdeDynamicGetAngularVelocity,
OdeDynamicSetPosition, OdeDynamicSetRotationQuaternion, 

// Input
MouseSetPosition, MouseGetPositionX, MouseGetPositionY, MouseShowCursor, 
KeyIsPressed,
// Color
MakeColorRGB, MakeColorRGBFloat,
// Window
WindowCreate, WindowGetHandle, WindowSetTitle, WindowDestroy,
WindowCenter, WindowResize, WindowGetPosition, WindowGetSize,
WindowIsShowing, WindowSetIcon,

// Kraft
KraftCreate, KraftStep,
KraftRayCast, KraftGetRayHitPosition, KraftGetRayHitNormal, 
KraftCreateRigidBody, KraftRigidBodyFinish,
KraftRigidBodySetGravity,
KraftRigidBodySetPosition, KraftRigidBodyGetPosition,
KraftRigidBodySetLinearVelocity, KraftRigidBodyGetLinearVelocity,
KraftRigidBodySetAngularVelocity, KraftRigidBodyGetAngularVelocity,
KraftRigidBodySetRotation,
KraftRigidBodyGetDirection, KraftRigidBodyGetUp, KraftRigidBodyGetRight,
KraftRigidBodyAddForce, KraftRigidBodyAddForceAtPos, KraftRigidBodyAddRelForce,
KraftObjectSetRigidBody,
KraftCreateShapeSphere, KraftCreateShapeBox, KraftCreateShapePlane, KraftCreateShapeCapsule,
KraftCreateShapeMesh,
KraftShapeSetDensity, KraftShapeSetFriction, KraftShapeSetRestitution, 
KraftShapeSetPosition, KraftShapeGetPosition,
KraftShapeSetRayCastable,
KraftCreateJointDistance, KraftCreateJointRope, KraftCreateJointBallSocket, KraftCreateJointFixed,
KraftCreateJointHinge,
KraftJointSetAnchor1, KraftJointSetAnchor2,
KraftJointSetHingeAxis1, KraftJointSetHingeAxis2,

// New functions in 3.8
SpriteGetSize, MaterialDestroy, MaterialSetName,
LightGetColor, LightGetAttenuation, LightGetShining,
CubeGetNormalDirection, PlaneSetOptions, PlaneGetOptions,
SphereGetOptions, SphereGetAngleLimits,
SphereSetOptions, CylinderSetOptions, CylinderGetOptions,
ConeGetOptions, ConeSetOptions, AnnulusSetOptions,
AnnulusGetOptions, TorusSetOptions, TorusGetOptions,
DiskSetOptions, DiskGetOptions, FrustrumSetOptions,
FrustrumGetOptions, ObjectGetMaterial, MaterialGetColor,
MaterialGetAlpha, GridSetTile, GridSetStep;

begin
end.
