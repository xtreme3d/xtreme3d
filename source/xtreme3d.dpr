library xtreme3d;
uses
  Windows, Messages, Classes, Controls, StdCtrls, ExtCtrls, Dialogs, SysUtils,
  GLScene, GLObjects, GLWin32FullScreenViewer, GLMisc, GLGraph,
  GLCollision, GLTexture, OpenGL1x, VectorGeometry, Graphics,
  GLVectorFileObjects, GLWin32Viewer, GLSpaceText, GLGeomObjects, GLCadencer,
  JPeg, Tga, GLProcTextures, Spin, GLVfsPAK, GLCanvas, GLGraphics, GLPortal,
  GLHUDObjects, GLBitmapFont, GLWindowsFont, GLImposter, VectorTypes, GLUtils,
  GLPolyhedron, GLTeapot, GLzBuffer, GLFile3DS, GLFileGTS, GLFileLWO, GLFileMD2,
  GLFileMD3, Q3MD3, GLFileMS3D, GLFileMD5, GLFileNMF, GLFileNurbs, GLFileObj, GLFileOCT,
  GLFilePLY, GLFileQ3BSP, GLFileSMD, GLFileSTL, GLFileTIN, GLFileB3D,
  GLFileLOD, GLPhongShader, VectorLists, GLThorFX, GLFireFX, GLTexCombineShader,
  GLBumpShader, GLCelShader, GLContext, GLTerrainRenderer, GLHeightData,
  GLBlur, GLSLShader, GLMultiMaterialShader, GLOutlineShader, GLHiddenLineShader,
  ApplicationFileIO, GLMaterialScript, GLWaterPlane, GeometryBB, GLExplosionFx,
  GLSkyBox, GLShadowPlane, GLShadowVolume, GLSkydome, GLLensFlare, GLDCE,
  GLNavigator, GLFPSMovement, GLMirror, SpatialPartitioning, GLSpatialPartitioning,
  GLTrail, GLTree, GLMultiProxy, GLODEManager, dynode, GLODECustomColliders,
  GLShadowMap, GLParticleFX, MeshUtils;

type
   TEmpty = class(TComponent)
    private
   end;

var
  scene: TGLScene;
  matlib: TGLMaterialLibrary;
  memviewer: TGLMemoryViewer;
  cadencer: TGLCadencer;
  empty: TEmpty;

  collisionPoint: TVector;
  collisionNormal: TVector;

  ode: TGLODEManager;

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
//var
//  dyna: TGLODEDynamic;
//  stat: TGLODEStatic;
begin
{
  stat := GetOdeStatic(obj);
  dyna := GetOdeDynamic(obj);
  result := nil;
  if stat <> nil then
  begin
    result := stat;
  end;
  if dyna <> nil then
  begin
    result := dyna;
  end;
}
  result := TGLODEBehaviour(obj.Behaviours.GetByClass(TGLODEBehaviour));
end;

{$I 'engine'}
{$I 'viewer'}
{$I 'dummycube'}
{$I 'camera'}
{$I 'light'}
{$I 'fonttext'}
{$I 'sprite'}
{$I 'primitives'}
{$I 'memviewer'}
{$I 'actor'}
{$I 'freeform'}
{$I 'object'}
{$I 'polygon'}
{$I 'material'}
{$I 'shaders'}
{$I 'thorfx'}
{$I 'firefx'}
{$I 'lensflare'}
{$I 'terrain'}
{$I 'blur'}
{$I 'skybox'}
{$I 'trail'}
{$I 'shadowplane'}
{$I 'shadowvolume'}
{$I 'skydome'}
{$I 'water'}
{$I 'lines'}
{$I 'tree'}
{$I 'navigator'}
{$I 'dce'}
{$I 'fps'}
{$I 'mirror'}
{$I 'partition'}
{$I 'proxy'}
{$I 'text'}
{$I 'grid'}
{$I 'shadowmap'}

function OdeManagerCreate(): real; stdcall;
begin
  ode := TGLODEManager.Create(nil);
  dWorldSetAutoDisableFlag(ode.World, 0);
  ode.RenderPoint := TGLRenderPoint.CreateAsChild(scene.Objects);
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

// TODO:
// OdeSurfaceSetMode
// OdeSurfaceSetMu
// OdeSurfaceSetMu2
// OdeSurfaceSetBounceVel
// OdeSurfaceSetSoftERP
// OdeSurfaceSetSoftCFM
// OdeSurfaceSetMotion1
// OdeSurfaceSetMotion2
// OdeSurfaceSetSlip1
// OdeSurfaceSetSlip2

// TODO:
// OdeAddJointBall
// OdeAddJointFixed
// OdeAddJointHinge
// OdeAddJointHinge2
// OdeAddJointSlider
// OdeAddJointUniversal

// TODO:
// OdeJointSetObjects
// OdeJointEnable
// OdeJointInitialize
// OdeJointSetAnchor
// OdeJointSetAnchorAtObject
// OdeJointSetAxis1
// OdeJointSetAxis2
// OdeJointSetBounce
// OdeJointSetCFM
// OdeJointSetFMax
// OdeJointSetFudgeFactor
// OdeJointSetHiStop
// OdeJointSetLoStop
// OdeJointSetStopCFM
// OdeJointSetStopERP
// OdeJointSetVel

exports

//Engine
EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
SetPakArchive,
Update, TrisRendered,
//Viewer
ViewerCreate, ViewerSetCamera, ViewerEnableVSync, ViewerRenderToFile,
ViewerRender, ViewerSetAutoRender,
ViewerResize, ViewerSetVisible, ViewerGetPixelColor, ViewerGetPixelDepth,
ViewerSetLighting, ViewerSetBackgroundColor, ViewerSetAmbientColor, ViewerEnableFog,
ViewerSetFogColor, ViewerSetFogDistance, ViewerScreenToWorld, ViewerWorldToScreen,
ViewerCopyToTexture, ViewerGetFramesPerSecond, ViewerGetPickedObject,
ViewerScreenToVector, ViewerVectorToScreen, ViewerPixelToDistance, ViewerGetPickedObjectsList,
ViewerSetAntiAliasing,
ViewerGetVBOSupported, ViewerGetGLSLSupported,
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
//Font & Text
BmpFontCreate, BmpFontLoad, WindowsBitmapfontCreate, HUDTextCreate, FlatTextCreate,
HUDTextSetRotation, SpaceTextCreate, SpaceTextSetExtrusion, HUDTextSetFont,
FlatTextSetFont, SpaceTextSetFont, HUDTextSetColor, FlatTextSetColor, HUDTextSetText,
FlatTextSetText, SpaceTextSetText,
//Sprite
HUDSpriteCreate, SpriteCreate, SpriteSetSize, SpriteScale, SpriteSetRotation,
SpriteRotate, SpriteMirror, SpriteNoZWrite,    
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
ActorMeshObjectsCount, ActorFaceGroupsCount, ActorFaceGroupGetMaterialName,
ActorFaceGroupSetMaterial,
//Freeform
FreeformCreate, FreeformMeshObjectsCount, FreeformMeshSetVisible,
FreeformMeshSetSecondCoords, FreeformMeshTriangleCount,  
FreeformFaceGroupsCount, FreeformFaceGroupTriangleCount,
FreeformCreateExplosionFX, FreeformExplosionFXReset,
FreeformExplosionFXEnable, FreeformExplosionFXSetSpeed,
FreeformSphereSweepIntersect, FreeformPointInMesh,
FreeformToFreeforms,
FreeformMeshSetMaterial, FreeformUseMeshMaterials,
//Terrain
BmpHDSCreate, BmpHDSSetInfiniteWarp, BmpHDSInvert,
TerrainCreate, TerrainSetHeightData, TerrainSetTileSize, TerrainSetTilesPerTexture,
TerrainSetQualityDistance, TerrainSetQualityStyle, TerrainSetMaxCLodTriangles,
TerrainSetCLodPrecision, TerrainSetOcclusionFrameSkip, TerrainSetOcclusionTesselate,
TerrainGetHeightAtObjectPosition, TerrainGetLastTriCount,
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
ObjectScale, ObjectSetScale,
ObjectSetUpVector, ObjectPointToObject, 
ObjectShowAxes,
ObjectGetGroundHeight, ObjectSceneRaycast, ObjectRaycast,
ObjectGetCollisionPosition, ObjectGetCollisionNormal, 
ObjectSetMaterial,
ObjectGetDistance,
ObjectCheckCubeVsCube, ObjectCheckSphereVsSphere, ObjectCheckSphereVsCube,
ObjectIsPointInObject,
ObjectSetCulling,
ObjectSetName, ObjectGetName, ObjectGetClassName,
ObjectSetTag, ObjectGetTag,
ObjectGetParent, ObjectGetChildCound, ObjectGetChild, ObjectGetIndex, ObjectFindChild,
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
MaterialGetCount, MaterialGetName,
MaterialSetFaceCulling, MaterialSetSecondTexture,
MaterialSetTextureFormat, MaterialSetTextureCompression,
MaterialTextureRequiredMemory, MaterialSetFilteringQuality,
MaterialAddTextureEx, MaterialTextureExClear, MaterialTextureExDelete,
MaterialNoiseCreate, MaterialNoiseAnimate, MaterialNoiseSetDimensions,
MaterialNoiseSetMinCut, MaterialNoiseSetSharpness, MaterialNoiseSetSeamless,
MaterialNoiseRandomSeed,
MaterialGenTexture, MaterialSetTextureWrap,
//Shaders
ShaderEnable, 
BumpShaderCreate, BumpShaderSetMethod, BumpShaderSetSpecularMode,
BumpShaderSetSpace, BumpShaderSetOptions, BumpShaderSetParallaxOffset,
CelShaderCreate, CelShaderSetLineColor, CelShaderSetLineWidth, CelShaderSetOptions,
MultiMaterialShaderCreate,
HiddenLineShaderCreate, HiddenLineShaderSetLineSmooth, HiddenLineShaderSetSolid,
HiddenLineShaderSetSurfaceLit, HiddenLineShaderSetFrontLine, HiddenLineShaderSetBackLine,
OutlineShaderCreate, OutlineShaderSetLineColor, OutlineShaderSetLineWidth,
TexCombineShaderCreate, TexCombineShaderAddCombiner,
TexCombineShaderMaterial3, TexCombineShaderMaterial4,
PhongShaderCreate,
GLSLShaderCreate, GLSLShaderCreateParameter,
GLSLShaderSetParameter1i, GLSLShaderSetParameter1f, GLSLShaderSetParameter2f,
GLSLShaderSetParameter3f, GLSLShaderSetParameter4f, GLSLShaderSetParameterTexture,
GLSLShaderSetParameterMatrix, GLSLShaderSetParameterInvMatrix,
GLSLShaderSetParameterShadowTexture, GLSLShaderSetParameterShadowMatrix,
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
//Text
TextRead,
//Grid
GridCreate, GridSetLineStyle, GridSetLineSmoothing, GridSetParts,
GridSetColor, GridSetSize, GridSetPattern,
//Memory Viewer
MemoryViewerCreate, MemoryViewerSetCamera, MemoryViewerRender,
MemoryViewerSetViewport, MemoryViewerCopyToTexture,
//ShadowMap
ShadowMapCreate, ShadowMapSetCamera, ShadowMapSetCaster,
ShadowMapSetProjectionSize, ShadowMapSetZScale, ShadowMapSetZClippingPlanes,
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
OdeSurfaceSetBounce;

begin
end.
