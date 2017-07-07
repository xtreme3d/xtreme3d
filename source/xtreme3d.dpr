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
  GLFBO, Hashes, Freetype, GLFreetypeFont, GLClippingPlane, Keyboard, Forms, Squall, CrystalLUA;

type
   TEmpty = class(TComponent)
    private
   end;

const
   {$I 'bumpshader'}
   {$I 'phongshader'}

var
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

{$I 'xtreme3d/engine'}
{$I 'xtreme3d/viewer'}
{$I 'xtreme3d/dummycube'}
{$I 'xtreme3d/camera'}
{$I 'xtreme3d/light'}
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

function EngineDestroy: real; stdcall;
begin
  cadencer.Enabled := false;
  cadencer.Scene := nil;
  cadencer.Free;
  scene.Free;
  empty.Free;
  memviewer.Free;
  result:=1;
end;

function FreeformGenTangents(ff: real): real; stdcall;
var
  GLFreeForm1: TGLFreeForm;
  mesh1: TMeshObject;
  mi: Integer;
begin
  GLFreeForm1:=TGLFreeForm(trunc64(ff));
  for mi:=0 to GLFreeForm1.MeshObjects.Count-1 do begin
      mesh1 := GLFreeForm1.MeshObjects[mi];
      if (mesh1.Vertices.Count > 0) and (mesh1.TexCoords.Count > 0) then
        GenMeshTangents(mesh1);
  end;
  GLFreeForm1.StructureChanged;
  GLFreeForm1.NotifyChange(nil);
  result:=1.0;
end;

function OdeDynamicSetVelocity(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.SetVelocity(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicSetAngularVelocity(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.SetAngularVelocity(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicGetVelocity(obj, ind: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  result := dyna.GetVelocity[trunc64(ind)];
end;

function OdeDynamicGetAngularVelocity(obj, ind: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  result := dyna.GetAngularVelocity[trunc64(ind)];
end;

function OdeDynamicSetPosition(obj, x, y, z: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.SetPosition(AffineVectorMake(x, y, z));
  result := 1.0;
end;

function OdeDynamicSetRotationQuaternion(obj, x, y, z, w: real): real; stdcall;
var
  dyna: TGLODEDynamic;
begin
  dyna := GetOdeDynamic(TGLBaseSceneObject(trunc64(obj)));
  dyna.SetRotation(x, y, z, w);
  result := 1.0;
end;

function ClipPlaneCreate(parent: real): real; stdcall;
var
  cp: TGLClipPlane;
begin
  if not (parent = 0) then
    cp := TGLClipPlane.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    cp := TGLClipPlane.CreateAsChild(scene.Objects);
  result := Integer(cp);
end;

function ClipPlaneEnable(cplane, mode: real): real; stdcall;
var
  cp: TGLClipPlane;
begin
  cp := TGLClipPlane(trunc64(cplane));
  cp.ClipPlaneEnabled := Boolean(trunc64(mode));
  result := 1;
end;

function ClipPlaneSetPlane(cplane, px, py, pz, nx, ny, nz: real): real; stdcall;
var
  cp: TGLClipPlane;
begin
  cp := TGLClipPlane(trunc64(cplane));
  cp.SetClipPlane(AffineVectorMake(px, py, pz), AffineVectorMake(nx, ny, nz));
  result := 1;
end;

function MaterialCullFrontFaces(mtrl: pchar; culff: real): real; stdcall;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.CullFrontFaces := Boolean(trunc64(culff));
  result:=1;
end;

function MaterialSetZWrite(mtrl: pchar; zwrite: real): real; stdcall;
var
  mat:TGLLibMaterial;
begin
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.ZWrite := Boolean(trunc64(zwrite));
  result:=1;
end;

// New in Xtreme3D 3.6:

function EngineSaveScene(filename: pchar): real; stdcall;
begin
  scene.SaveToTextFile(String(filename));
  result := 1.0;
end;

function EngineLoadScene(filename: pchar): real; stdcall;
begin
  scene.LoadFromTextFile(String(filename));
  result := 1.0;
end;

function EngineRootObject: real; stdcall;
begin
  result := Integer(scene.Objects);
end;

function ActorMoveBone(actor, boneindex, x, y, z: real): real; stdcall;
var
  ac: TGLActor;
  pos: TAffineVector;
begin
  ac := TGLActor(trunc64(actor));
  pos := ac.Skeleton.Frames[ac.NextFrameIndex].Position[trunc64(boneindex)];
  ac.Skeleton.Frames[ac.NextFrameIndex].Position[trunc64(boneindex)] :=
    VectorAdd(pos, AffineVectorMake(x, y, z));
  result := 1;
end;

function ActorRotateBone(actor, boneindex, x, y, z: real): real; stdcall;
var
  ac: TGLActor;
  rot: TAffineVector;
begin
  ac := TGLActor(trunc64(actor));
  rot := ac.Skeleton.Frames[ac.NextFrameIndex].Rotation[trunc64(boneindex)];
  ac.Skeleton.Frames[ac.NextFrameIndex].Rotation[trunc64(boneindex)] :=
    VectorAdd(rot, AffineVectorMake(x, y, z));
  result := 1;
end; 

function MouseSetPosition(mx, my: real): real; stdcall;
begin
  SetCursorPos(trunc64(mx), trunc64(my));
  Result := 1;
end;

function MouseGetPositionX: real; stdcall;
var
  mouse : TPoint;
begin
  GetCursorPos(mouse);
  Result := Integer(mouse.X);
end;

function MouseGetPositionY: real; stdcall;
var
  mouse : TPoint;
begin
  GetCursorPos(mouse);
  Result := Integer(mouse.Y);
end;

function MouseShowCursor(mode: real): real; stdcall;
begin
  ShowCursor(LongBool(trunc64(mode)));
  Result := 1;
end;

function KeyIsPressed(key: real): real; stdcall;
begin
  Result := Integer(IsKeyDown(trunc64(key)));
end;

function WindowCreate(x, y, w, h, resizeable: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm.Create(nil);
  frm.Width := trunc64(w);
  frm.Height := trunc64(h);
  frm.Left := trunc64(x);
  frm.Top := trunc64(y);
  if trunc64(resizeable) = 0 then
    frm.BorderStyle := bsSingle;
  frm.Show;
  result := Integer(frm);
end;

function WindowGetHandle(w: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  result := Integer(frm.Handle);
end;

function WindowSetTitle(w: real; title: pchar): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  frm.Caption := String(title);
  result := 1;
end;

function WindowDestroy(w: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  frm.Free;
  result := 1;
end;

function ObjectCopy(obj, parent: real): real; stdcall;
var
  obj1, obj2, par: TGLBaseSceneObject;
begin
  obj1 := TGLBaseSceneObject(trunc64(obj));
  if not (parent=0) then
    par := TGLBaseSceneObject(trunc64(parent))
  else
    par := scene.Objects;
  obj2 := TGLBaseSceneObject(obj1.NewInstance).CreateAsChild(par);
  result:=Integer(obj2);
end;

function SquallInit: real; stdcall;
var
  r: boolean;
begin
  r := InitSquall('squall.dll');
  if r then
    result := SQUALL_Init(nil)
  else
    result := 0;
end;

function SquallAddSound(filename: pchar): real; stdcall;
var
  s: Integer;
begin
  s := SQUALL_Sample_LoadFile(filename, 1, nil);
  result := s;
end;

function SquallPlay(snd, loop: real): real; stdcall;
var
  ch: Integer;
begin
  ch := SQUALL_Sample_Play(trunc64(snd), trunc64(loop), 0, 1);
  result := ch;
end;

function SquallStop(chan: real): real; stdcall;
begin
  SQUALL_Channel_Stop(trunc64(chan));
  result := 1.0;
end;

{$I 'xtreme3d/lua/engine'}
{$I 'xtreme3d/lua/viewer'}
{$I 'xtreme3d/lua/dummycube'}
{$I 'xtreme3d/lua/camera'}
{$I 'xtreme3d/lua/light'}
{$I 'xtreme3d/lua/fonttext'}
{$I 'xtreme3d/lua/sprite'}
{$I 'xtreme3d/lua/hudshape'}
{$I 'xtreme3d/lua/primitives'}
{$I 'xtreme3d/lua/object'}
{$I 'xtreme3d/lua/input'}

procedure luaRegX3DFunctions(lua: TLua);
begin
  // Register Engine functions
  lua.RegProc('EngineCreate', @lua_EngineCreate, 0);
  lua.RegProc('EngineDestroy', @lua_EngineDestroy, 0);
  lua.RegProc('EngineSetObjectsSorting', @lua_EngineSetObjectsSorting, 1);
  lua.RegProc('EngineSetCulling', @lua_EngineSetCulling, 1);
  lua.RegProc('SetPakArchive', @lua_SetPakArchive, 1);
  lua.RegProc('Update', @lua_Update, 1);
  lua.RegProc('EngineSaveScene', @lua_EngineSaveScene, 1);
  lua.RegProc('EngineLoadScene', @lua_EngineLoadScene, 1);
  lua.RegProc('EngineRootObject', @lua_EngineRootObject, 0);

  // Register Viewer functions
  lua.RegProc('ViewerCreate', @lua_ViewerCreate, 5);
  lua.RegProc('ViewerSetCamera', @lua_ViewerSetCamera, 2);
  lua.RegProc('ViewerEnableVSync', @lua_ViewerEnableVSync, 2);
  lua.RegProc('ViewerRender', @lua_ViewerRender, 1);
  lua.RegProc('ViewerRenderToFile', @lua_ViewerRenderToFile, 2);
  lua.RegProc('ViewerRenderEx', @lua_ViewerRenderEx, 5);
  lua.RegProc('ViewerResize', @lua_ViewerResize, 5);
  lua.RegProc('ViewerSetVisible', @lua_ViewerSetVisible, 2);
  lua.RegProc('ViewerGetPixelColor', @lua_ViewerGetPixelColor, 3);
  lua.RegProc('ViewerGetPixelDepth', @lua_ViewerGetPixelDepth, 3);
  lua.RegProc('ViewerSetLighting', @lua_ViewerSetLighting, 2);
  lua.RegProc('ViewerSetBackgroundColor', @lua_ViewerSetBackgroundColor, 2);
  lua.RegProc('ViewerSetAmbientColor', @lua_ViewerSetAmbientColor, 2);
  lua.RegProc('ViewerEnableFog', @lua_ViewerEnableFog, 2);
  lua.RegProc('ViewerSetFogColor', @lua_ViewerSetFogColor, 2);
  lua.RegProc('ViewerSetFogDistance', @lua_ViewerSetFogDistance, 2);
  lua.RegProc('ViewerScreenToWorld', @lua_ViewerScreenToWorld, 4);
  lua.RegProc('ViewerWorldToScreen', @lua_ViewerWorldToScreen, 5);
  lua.RegProc('ViewerCopyToTexture', @lua_ViewerCopyToTexture, 2);
  lua.RegProc('ViewerGetFramesPerSecond', @lua_ViewerGetFramesPerSecond, 1);
  lua.RegProc('ViewerGetPickedObject', @lua_ViewerGetPickedObject, 3);
  lua.RegProc('ViewerGetPickedObjectsList', @lua_ViewerGetPickedObjectsList, 7);
  lua.RegProc('ViewerScreenToVector', @lua_ViewerScreenToVector, 4);
  lua.RegProc('ViewerVectorToScreen', @lua_ViewerVectorToScreen, 5);
  lua.RegProc('ViewerPixelToDistance', @lua_ViewerPixelToDistance, 3);
  lua.RegProc('ViewerSetAntiAliasing', @lua_ViewerSetAntiAliasing, 2);
  lua.RegProc('ViewerGetGLSLSupported', @lua_ViewerGetGLSLSupported, 1);
  lua.RegProc('ViewerGetFBOSupported', @lua_ViewerGetFBOSupported, 1);
  lua.RegProc('ViewerGetVBOSupported', @lua_ViewerGetVBOSupported, 1);
  lua.RegProc('ViewerSetAutoRender', @lua_ViewerSetAutoRender, 2);
  lua.RegProc('ViewerSetOverrideMaterial', @lua_ViewerSetOverrideMaterial, 3);
  lua.RegProc('ViewerGetSize', @lua_ViewerGetSize, 2);
  lua.RegProc('ViewerGetPosition', @lua_ViewerGetPosition, 2);
  lua.RegProc('ViewerIsOpenGLExtensionSupported', @lua_ViewerIsOpenGLExtensionSupported, 1);

  // Register Dummycube functions
  lua.RegProc('DummycubeCreate', @lua_DummycubeCreate, 1);
  lua.RegProc('DummycubeAmalgamate', @lua_DummycubeAmalgamate, 2);
  lua.RegProc('DummycubeSetCameraMode', @lua_DummycubeSetCameraMode, 2);
  lua.RegProc('DummycubeSetVisible', @lua_DummycubeSetVisible, 2);
  lua.RegProc('DummycubeSetEdgeColor', @lua_DummycubeSetEdgeColor, 2);
  lua.RegProc('DummycubeSetCubeSize', @lua_DummycubeSetCubeSize, 2);

  // Register Camera functions
  lua.RegProc('CameraCreate', @lua_CameraCreate, 1);
  lua.RegProc('CameraSetStyle', @lua_CameraSetStyle, 2);
  lua.RegProc('CameraSetFocal', @lua_CameraSetFocal, 2);
  lua.RegProc('CameraSetSceneScale', @lua_CameraSetSceneScale, 2);
  lua.RegProc('CameraScaleScene', @lua_CameraScaleScene, 2);
  lua.RegProc('CameraSetViewDepth', @lua_CameraSetViewDepth, 2);
  lua.RegProc('CameraSetTargetObject', @lua_CameraSetTargetObject, 2);
  lua.RegProc('CameraMoveAroundTarget', @lua_CameraMoveAroundTarget, 3);
  lua.RegProc('CameraSetDistanceToTarget', @lua_CameraSetDistanceToTarget, 2);
  lua.RegProc('CameraGetDistanceToTarget', @lua_CameraGetDistanceToTarget, 1);
  lua.RegProc('CameraCopyToTexture', @lua_CameraCopyToTexture, 4);
  lua.RegProc('CameraGetNearPlane', @lua_CameraGetNearPlane, 1);
  lua.RegProc('CameraSetNearPlaneBias', @lua_CameraSetNearPlaneBias, 2);
  lua.RegProc('CameraAbsoluteVectorToTarget', @lua_CameraAbsoluteVectorToTarget, 2);
  lua.RegProc('CameraAbsoluteRightVectorToTarget', @lua_CameraAbsoluteRightVectorToTarget, 2);
  lua.RegProc('CameraAbsoluteUpVectorToTarget', @lua_CameraAbsoluteUpVectorToTarget, 2);
  lua.RegProc('CameraZoomAll', @lua_CameraZoomAll, 1);
  lua.RegProc('CameraScreenDeltaToVector', @lua_CameraScreenDeltaToVector, 8);
  lua.RegProc('CameraScreenDeltaToVectorXY', @lua_CameraScreenDeltaToVectorXY, 5);
  lua.RegProc('CameraScreenDeltaToVectorXZ', @lua_CameraScreenDeltaToVectorXZ, 5);
  lua.RegProc('CameraScreenDeltaToVectorYZ', @lua_CameraScreenDeltaToVectorYZ, 5);
  lua.RegProc('CameraAbsoluteEyeSpaceVector', @lua_CameraAbsoluteEyeSpaceVector, 5);
  lua.RegProc('CameraSetAutoLeveling', @lua_CameraSetAutoLeveling, 2);
  lua.RegProc('CameraMoveInEyeSpace', @lua_CameraMoveInEyeSpace, 4);
  lua.RegProc('CameraMoveTargetInEyeSpace', @lua_CameraMoveTargetInEyeSpace, 4);
  lua.RegProc('CameraPointInFront', @lua_CameraPointInFront, 4);
  lua.RegProc('CameraGetFieldOfView', @lua_CameraGetFieldOfView, 2);

  // Register Light functions
  lua.RegProc('LightCreate', @lua_LightCreate, 2);
  lua.RegProc('LightSetAmbientColor', @lua_LightSetAmbientColor, 2);
  lua.RegProc('LightSetDiffuseColor', @lua_LightSetDiffuseColor, 2);
  lua.RegProc('LightSetSpecularColor', @lua_LightSetSpecularColor, 2);
  lua.RegProc('LightSetAttenuation', @lua_LightSetAttenuation, 4);
  lua.RegProc('LightSetShining', @lua_LightSetShining, 2);
  lua.RegProc('LightSetSpotCutoff', @lua_LightSetSpotCutoff, 2);
  lua.RegProc('LightSetSpotExponent', @lua_LightSetSpotExponent, 2);
  lua.RegProc('LightSetSpotDirection', @lua_LightSetSpotDirection, 4);
  lua.RegProc('LightSetStyle', @lua_LightSetStyle, 2);

  // Register Font & Text functions
  lua.RegProc('BmpfontCreate', @lua_BmpfontCreate, 8);
  lua.RegProc('BmpfontLoad', @lua_BmpfontLoad, 2);
  lua.RegProc('TTFontCreate', @lua_TTFontCreate, 2);
  lua.RegProc('TTFontSetLineGap', @lua_TTFontSetLineGap, 2);
  lua.RegProc('WindowsBitmapfontCreate', @lua_WindowsBitmapfontCreate, 4);
  lua.RegProc('FlatTextCreate', @lua_FlatTextCreate, 3);
  lua.RegProc('FlatTextSetFont', @lua_FlatTextSetFont, 2);
  lua.RegProc('FlatTextSetColor', @lua_FlatTextSetColor, 3);
  lua.RegProc('FlatTextSetText', @lua_FlatTextSetText, 2);
  lua.RegProc('HUDTextCreate', @lua_HUDTextCreate, 3);
  lua.RegProc('HUDTextSetRotation', @lua_HUDTextSetRotation, 2);
  lua.RegProc('HUDTextSetFont', @lua_HUDTextSetFont, 2);
  lua.RegProc('HUDTextSetColor', @lua_HUDTextSetColor, 3);
  lua.RegProc('HUDTextSetText', @lua_HUDTextSetText, 2);
  lua.RegProc('SpaceTextCreate', @lua_SpaceTextCreate, 4);
  lua.RegProc('SpaceTextSetExtrusion', @lua_SpaceTextSetExtrusion, 2);
  lua.RegProc('SpaceTextSetFont', @lua_SpaceTextSetFont, 2);
  lua.RegProc('SpaceTextSetText', @lua_SpaceTextSetText, 2);

  // Register Sprite functions
  lua.RegProc('HUDSpriteCreate', @lua_HUDSpriteCreate, 4);
  lua.RegProc('HUDSpriteCreateEx', @lua_HUDSpriteCreateEx, 7);
  lua.RegProc('SpriteCreate', @lua_SpriteCreate, 4);
  lua.RegProc('SpriteCreateEx', @lua_SpriteCreateEx, 7);
  lua.RegProc('SpriteSetSize', @lua_SpriteSetSize, 3);
  lua.RegProc('SpriteScale', @lua_SpriteScale, 3);
  lua.RegProc('SpriteSetRotation', @lua_SpriteSetRotation, 2);
  lua.RegProc('SpriteRotate', @lua_SpriteRotate, 2);
  lua.RegProc('SpriteMirror', @lua_SpriteMirror, 3);
  lua.RegProc('SpriteNoZWrite', @lua_SpriteNoZWrite, 2);
  lua.RegProc('SpriteSetBounds', @lua_SpriteSetBounds, 5);
  lua.RegProc('SpriteSetBoundsUV', @lua_SpriteSetBoundsUV, 5);
  lua.RegProc('SpriteSetOrigin', @lua_SpriteSetOrigin, 3);

  // Register HUDShape functions
  lua.RegProc('HUDShapeRectangleCreate', @lua_HUDShapeRectangleCreate, 3);
  lua.RegProc('HUDShapeCircleCreate', @lua_HUDShapeCircleCreate, 5);
  lua.RegProc('HUDShapeLineCreate', @lua_HUDShapeLineCreate, 5);
  lua.RegProc('HUDShapeMeshCreate', @lua_HUDShapeMeshCreate, 1);
  lua.RegProc('HUDShapeSetSize', @lua_HUDShapeSetSize, 3);
  lua.RegProc('HUDShapeScale', @lua_HUDShapeScale, 3);
  lua.RegProc('HUDShapeSetRotation', @lua_HUDShapeSetRotation, 2);
  lua.RegProc('HUDShapeRotate', @lua_HUDShapeRotate, 2);
  lua.RegProc('HUDShapeSetColor', @lua_HUDShapeSetColor, 3);
  lua.RegProc('HUDShapeSetOrigin', @lua_HUDShapeSetOrigin, 3);
  lua.RegProc('HUDShapeCircleSetRadius', @lua_HUDShapeCircleSetRadius, 2);
  lua.RegProc('HUDShapeCircleSetSlices', @lua_HUDShapeCircleSetSlices, 2);
  lua.RegProc('HUDShapeCircleSetAngles', @lua_HUDShapeCircleSetAngles, 3);
  lua.RegProc('HUDShapeLineSetPoints', @lua_HUDShapeLineSetPoints, 5);
  lua.RegProc('HUDShapeLineSetWidth', @lua_HUDShapeLineSetWidth, 2);
  lua.RegProc('HUDShapeMeshAddVertex', @lua_HUDShapeMeshAddVertex, 5);
  lua.RegProc('HUDShapeMeshAddTriangle', @lua_HUDShapeMeshAddTriangle, 4);
  lua.RegProc('HUDShapeMeshSetVertex', @lua_HUDShapeMeshSetVertex, 4);
  lua.RegProc('HUDShapeMeshSetTexCoord', @lua_HUDShapeMeshSetTexCoord, 4);

  // Register Primitives functions
  lua.RegProc('PlaneCreate', @lua_PlaneCreate, 6);
  lua.RegProc('CubeCreate', @lua_CubeCreate, 4);
  lua.RegProc('CubeSetNormalDirection', @lua_CubeSetNormalDirection, 2);
  lua.RegProc('SphereCreate', @lua_SphereCreate, 4);
  lua.RegProc('CylinderCreate', @lua_CylinderCreate, 7);
  lua.RegProc('ConeCreate', @lua_ConeCreate, 6);
  lua.RegProc('AnnulusCreate', @lua_AnnulusCreate, 7);
  lua.RegProc('TorusCreate', @lua_TorusCreate, 5);
  lua.RegProc('DiskCreate', @lua_DiskCreate, 7);
  lua.RegProc('FrustrumCreate', @lua_FrustrumCreate, 5);
  lua.RegProc('DodecahedronCreate', @lua_DodecahedronCreate, 1);
  lua.RegProc('IcosahedronCreate', @lua_IcosahedronCreate, 1);
  lua.RegProc('TeapotCreate', @lua_TeapotCreate, 1);

  // Register Object functions
  lua.RegProc('ObjectHide', @lua_ObjectHide, 1);
  lua.RegProc('ObjectShow', @lua_ObjectShow, 1);
  lua.RegProc('ObjectIsVisible', @lua_ObjectIsVisible, 1);
  lua.RegProc('ObjectCopy', @lua_ObjectCopy, 2);
  lua.RegProc('ObjectDestroy', @lua_ObjectDestroy, 1);
  lua.RegProc('ObjectDestroyChildren', @lua_ObjectDestroyChildren, 1);
  lua.RegProc('ObjectSetPosition', @lua_ObjectSetPosition, 4);
  lua.RegProc('ObjectGetPosition', @lua_ObjectGetPosition, 2);
  lua.RegProc('ObjectGetAbsolutePosition', @lua_ObjectGetAbsolutePosition, 2);
  lua.RegProc('ObjectSetPositionOfObject', @lua_ObjectSetPositionOfObject, 2);
  lua.RegProc('ObjectAlignWithObject', @lua_ObjectAlignWithObject, 2);
  lua.RegProc('ObjectSetPositionX', @lua_ObjectSetPositionX, 2);
  lua.RegProc('ObjectSetPositionY', @lua_ObjectSetPositionY, 2);
  lua.RegProc('ObjectSetPositionZ', @lua_ObjectSetPositionZ, 2);
  lua.RegProc('ObjectGetPositionX', @lua_ObjectGetPositionX, 1);
  lua.RegProc('ObjectGetPositionY', @lua_ObjectGetPositionY, 1);
  lua.RegProc('ObjectGetPositionZ', @lua_ObjectGetPositionZ, 1);
  lua.RegProc('ObjectSetAbsolutePosition', @lua_ObjectSetAbsolutePosition, 4);
  lua.RegProc('ObjectSetDirection', @lua_ObjectSetDirection, 4);
  lua.RegProc('ObjectGetDirection', @lua_ObjectGetDirection, 2);
  lua.RegProc('ObjectSetAbsoluteDirection', @lua_ObjectSetAbsoluteDirection, 4);
  lua.RegProc('ObjectGetPitch', @lua_ObjectGetPitch, 1);
  lua.RegProc('ObjectGetTurn', @lua_ObjectGetTurn, 1);
  lua.RegProc('ObjectGetRoll', @lua_ObjectGetRoll, 1);
  lua.RegProc('ObjectSetRotation', @lua_ObjectSetRotation, 4);
  lua.RegProc('ObjectMove', @lua_ObjectMove, 2);
  lua.RegProc('ObjectLift', @lua_ObjectLift, 2);
  lua.RegProc('ObjectStrafe', @lua_ObjectStrafe, 2);
  lua.RegProc('ObjectTranslate', @lua_ObjectTranslate, 4);
  lua.RegProc('ObjectRotate', @lua_ObjectRotate, 4);
  lua.RegProc('ObjectScale', @lua_ObjectScale, 4);
  lua.RegProc('ObjectSetScale', @lua_ObjectSetScale, 4);
  lua.RegProc('ObjectSetUpVector', @lua_ObjectSetUpVector, 4);
  lua.RegProc('ObjectPointToObject', @lua_ObjectPointToObject, 2);
  lua.RegProc('ObjectShowAxes', @lua_ObjectShowAxes, 2);
  lua.RegProc('ObjectGetGroundHeight', @lua_ObjectGetGroundHeight, 2);
  lua.RegProc('ObjectSceneRaycast', @lua_ObjectSceneRaycast, 2);
  lua.RegProc('ObjectRaycast', @lua_ObjectRaycast, 2);
  lua.RegProc('ObjectGetCollisionPosition', @lua_ObjectGetCollisionPosition, 1);
  lua.RegProc('ObjectGetCollisionNormal', @lua_ObjectGetCollisionNormal, 1);
  lua.RegProc('ObjectSetMaterial', @lua_ObjectSetMaterial, 2);
  lua.RegProc('ObjectGetDistance', @lua_ObjectGetDistance, 2);
  lua.RegProc('ObjectCheckCubeVsCube', @lua_ObjectCheckCubeVsCube, 2);
  lua.RegProc('ObjectCheckSphereVsSphere', @lua_ObjectCheckSphereVsSphere, 2);
  lua.RegProc('ObjectCheckSphereVsCube', @lua_ObjectCheckSphereVsCube, 2);
  lua.RegProc('ObjectCheckCubeVsFace', @lua_ObjectCheckCubeVsFace, 2);
  lua.RegProc('ObjectCheckFaceVsFace', @lua_ObjectCheckFaceVsFace, 2);
  lua.RegProc('ObjectIsPointInObject', @lua_ObjectIsPointInObject, 4);
  lua.RegProc('ObjectSetCulling', @lua_ObjectSetCulling, 2);
  lua.RegProc('ObjectSetName', @lua_ObjectSetName, 2);
  lua.RegProc('ObjectGetName', @lua_ObjectGetName, 1);
  lua.RegProc('ObjectGetClassName', @lua_ObjectGetClassName, 1);
  lua.RegProc('ObjectSetTag', @lua_ObjectSetTag, 2);
  lua.RegProc('ObjectGetTag', @lua_ObjectGetTag, 1);
  lua.RegProc('ObjectGetParent', @lua_ObjectGetParent, 1);
  lua.RegProc('ObjectGetChildCount', @lua_ObjectGetChildCount, 1);
  lua.RegProc('ObjectGetChild', @lua_ObjectGetChild, 2);
  lua.RegProc('ObjectGetIndex', @lua_ObjectGetIndex, 1);
  lua.RegProc('ObjectFindChild', @lua_ObjectFindChild, 2);
  lua.RegProc('ObjectGetBoundingSphereRadius', @lua_ObjectGetBoundingSphereRadius, 1);
  lua.RegProc('ObjectGetAbsoluteUp', @lua_ObjectGetAbsoluteUp, 2);
  lua.RegProc('ObjectSetAbsoluteUp', @lua_ObjectSetAbsoluteUp, 4);
  lua.RegProc('ObjectGetAbsoluteRight', @lua_ObjectGetAbsoluteRight, 2);
  lua.RegProc('ObjectGetAbsoluteXVector', @lua_ObjectGetAbsoluteXVector, 2);
  lua.RegProc('ObjectGetAbsoluteYVector', @lua_ObjectGetAbsoluteYVector, 2);
  lua.RegProc('ObjectGetAbsoluteZVector', @lua_ObjectGetAbsoluteZVector, 2);
  lua.RegProc('ObjectMoveChildUp', @lua_ObjectMoveChildUp, 2);
  lua.RegProc('ObjectMoveChildDown', @lua_ObjectMoveChildDown, 2);
  lua.RegProc('ObjectSetParent', @lua_ObjectSetParent, 2);
  lua.RegProc('ObjectRemoveChild', @lua_ObjectRemoveChild, 3);
  lua.RegProc('ObjectMoveObjectAround', @lua_ObjectMoveObjectAround, 4);
  lua.RegProc('ObjectPitch', @lua_ObjectPitch, 2);
  lua.RegProc('ObjectTurn', @lua_ObjectTurn, 2);
  lua.RegProc('ObjectRoll', @lua_ObjectRoll, 2);
  lua.RegProc('ObjectGetUp', @lua_ObjectGetUp, 2);
  lua.RegProc('ObjectRotateAbsolute', @lua_ObjectRotateAbsolute, 4);
  lua.RegProc('ObjectRotateAbsoluteVector', @lua_ObjectRotateAbsoluteVector, 5);
  lua.RegProc('ObjectSetMatrixColumn', @lua_ObjectSetMatrixColumn, 6);
  lua.RegProc('ObjectExportMatrix', @lua_ObjectExportMatrix, 2);
  lua.RegProc('ObjectExportAbsoluteMatrix', @lua_ObjectExportAbsoluteMatrix, 2);

  // Register Input functions
  lua.RegProc('KeyIsPressed', @lua_KeyIsPressed, 1);
end;

procedure luaRegGMConstants(lua: TLua);
begin
  // TODO: vk_nokey
  // TODO: vk_anykey
  lua.RegConst('vk_left', 37.0);
  lua.RegConst('vk_up', 38.0);
  lua.RegConst('vk_right', 39.0);
  lua.RegConst('vk_down', 40.0);
  lua.RegConst('vk_enter', 13.0);
  lua.RegConst('vk_escape', 27.0);
  lua.RegConst('vk_space', 32.0);
  lua.RegConst('vk_shift', 16.0);
  lua.RegConst('vk_control', 17.0);
  lua.RegConst('vk_alt', 18.0);
  lua.RegConst('vk_backspace', 8.0);
  lua.RegConst('vk_tab', 9.0);
  lua.RegConst('vk_home', 36.0);
  lua.RegConst('vk_end', 35.0);
  lua.RegConst('vk_delete', 46.0);
  lua.RegConst('vk_insert', 45.0);
  lua.RegConst('vk_pageup', 33.0);
  lua.RegConst('vk_pagedown', 34.0);
  lua.RegConst('vk_pause', 19.0);
  lua.RegConst('vk_printscreen', 44.0);
  lua.RegConst('vk_f1', 112.0);
  lua.RegConst('vk_f2', 113.0);
  lua.RegConst('vk_f3', 114.0);
  lua.RegConst('vk_f4', 115.0);
  lua.RegConst('vk_f5', 116.0);
  lua.RegConst('vk_f6', 117.0);
  lua.RegConst('vk_f7', 118.0);
  lua.RegConst('vk_f8', 119.0);
  lua.RegConst('vk_f9', 120.0);
  lua.RegConst('vk_f10', 121.0);
  lua.RegConst('vk_f11', 122.0);
  lua.RegConst('vk_f12', 123.0);
  lua.RegConst('vk_numpad0', 96.0);
  lua.RegConst('vk_numpad1', 97.0);
  lua.RegConst('vk_numpad2', 98.0);
  lua.RegConst('vk_numpad3', 99.0);
  lua.RegConst('vk_numpad4', 100.0);
  lua.RegConst('vk_numpad5', 101.0);
  lua.RegConst('vk_numpad6', 102.0);
  lua.RegConst('vk_numpad7', 103.0);
  lua.RegConst('vk_numpad8', 104.0);
  lua.RegConst('vk_numpad9', 105.0);
  lua.RegConst('vk_multiply', 106.0);
  lua.RegConst('vk_divide', 111.0);
  lua.RegConst('vk_add', 107.0);
  lua.RegConst('vk_subtract', 109.0);
  lua.RegConst('vk_decimal', 110.0);

  lua.RegConst('c_aqua', 16776960.0);
  lua.RegConst('c_black', 0.0);
  lua.RegConst('c_blue', 16711680.0);
  lua.RegConst('c_dkgray', 4210752.0);
  lua.RegConst('c_fuchsia', 16711935.0);
  lua.RegConst('c_gray', 8421504.0);
  lua.RegConst('c_green', 32768.0);
  lua.RegConst('c_lime', 65280.0);
  lua.RegConst('c_ltgray', 12632256.0);
  lua.RegConst('c_maroon', 128.0);
  lua.RegConst('c_navy', 8388608.0);
  lua.RegConst('c_olive', 32896.0);
  lua.RegConst('c_purple', 8388736.0);
  lua.RegConst('c_red', 255.0);
  lua.RegConst('c_silver', 12632256.0);
  lua.RegConst('c_teal', 8421376.0);
  lua.RegConst('c_white', 16777215.0);
  lua.RegConst('c_yellow', 65535.0);
  lua.RegConst('c_orange', 33023.0);
end;

procedure luaRegX3DConstants(lua: TLua);
begin
  lua.RegConst('osInherited', 0.0);
  lua.RegConst('osNone', 1.0);
  lua.RegConst('osRenderFarthestFirst', 2.0);
  lua.RegConst('osRenderBlendedLast', 3.0);
  lua.RegConst('osRenderNearestFirst', 4.0);

  lua.RegConst('vcNone', 0.0);
  lua.RegConst('vcInherited', 1.0);
  lua.RegConst('vcObjectBased', 2.0);
  lua.RegConst('vcHierarchical', 3.0);

  lua.RegConst('vsmSync', 0.0);
  lua.RegConst('vsmNoSync', 1.0);

  lua.RegConst('vsmNoSync', 1.0);

  lua.RegConst('aaDefault', 0.0);
  lua.RegConst('aaNone', 1.0);
  lua.RegConst('aa2x', 2.0);
  lua.RegConst('aa2xHQ', 3.0);
  lua.RegConst('aa4x', 4.0);
  lua.RegConst('aa4xHQ', 5.0);

  lua.RegConst('cimNone', 0.0);
  lua.RegConst('cimPosition', 1.0);
  lua.RegConst('cimOrientation', 2.0);

  lua.RegConst('csPerspective', 0.0);
  lua.RegConst('csOrthogonal', 1.0);
  lua.RegConst('csOrtho2D', 2.0);
  lua.RegConst('csInfinitePerspective', 3.0);

  lua.RegConst('lsSpot', 0.0);
  lua.RegConst('lsOmni', 1.0);
  lua.RegConst('lsParallel', 2.0);

  // TODO
end;

function LuaManagerCreate: real; stdcall;
var
  lua: TLua;
begin
  lua := TLua.Create();
  luaRegX3DFunctions(lua);
  luaRegGMConstants(lua);
  luaRegX3DConstants(lua);
  result := Integer(lua);
end;

function LuaManagerSetConstantReal(lu: real; name: pchar; val: real): real; stdcall;
var
  lua: TLua;
begin
  lua := TLua(trunc64(lu));
  lua.RegConst(string(name), val);
  result := 1;
end;

function LuaManagerSetConstantString(lu: real; name, val: pchar): real; stdcall;
var
  lua: TLua;
begin
  lua := TLua(trunc64(lu));
  lua.RegConst(string(name), string(val));
  result := 1;
end;

function LuaManagerRunScript(lu: real; script: pchar): real; stdcall;
var
  lua: TLua;
begin
  result := 1;
  lua := TLua(trunc64(lu));
  try
   lua.RunScript(script);
  except
    On E: Exception do
    begin
      ShowMessage(E.Message);
      result := 0;
    end;
  end;
end;

function LuaManagerCallFunction(lu: real; name: pchar): real; stdcall;
var
  lua: TLua;
begin
  lua := TLua(trunc64(lu));
  result := 0;
  if lua.ProcExists(string(name)) then
  begin
    result := 1;
    try
      lua.Call(string(name), LuaArgs(0));
    except
      On E: Exception do
      begin
        ShowMessage(E.Message);
        result := 0;
      end;
    end;
  end;
end;

function ActorProxyObjectCreate(actor, parent: real): real; stdcall;
var
  p: TGLActorProxy;
begin
  if not (parent = 0) then
    p := TGLActorProxy.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    p := TGLActorProxy.CreateAsChild(scene.Objects);
  p.MasterObject := TGLActor(trunc64(actor));
  result := Integer(p);
end;

function ActorProxyObjectSwitchToAnimation(proxy, anim: real): real; stdcall;
var
  p: TGLActorProxy;
begin
  p := TGLActorProxy(trunc64(proxy));
  p.SwitchToAnimation(trunc64(anim));
  result := 1.0;
end;

exports

//Engine
EngineCreate, EngineDestroy, EngineSetObjectsSorting, EngineSetCulling,
SetPakArchive,
Update, TrisRendered,
EngineSaveScene, EngineLoadScene, EngineRootObject,
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
BmpFontCreate, BmpFontLoad,
TTFontCreate, TTFontSetLineGap,
WindowsBitmapfontCreate,
HUDTextCreate, FlatTextCreate,
HUDTextSetRotation, SpaceTextCreate, SpaceTextSetExtrusion, HUDTextSetFont,
FlatTextSetFont, SpaceTextSetFont, HUDTextSetColor, FlatTextSetColor, HUDTextSetText,
FlatTextSetText, SpaceTextSetText,
//Sprite
HUDSpriteCreate, SpriteCreate, SpriteSetSize, SpriteScale, SpriteSetRotation,
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
ActorMeshObjectsCount, ActorFaceGroupsCount, ActorFaceGroupGetMaterialName,
ActorFaceGroupSetMaterial,
ActorMoveBone, ActorRotateBone,
//Freeform
FreeformCreate, FreeformCreateEmpty,
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
{
OdeVehicleCreate, OdeVehicleSetScene, OdeVehicleSetForwardForce,
OdeVehicleAddSuspension, OdeVehicleSuspensionGetWheel, OdeVehicleSuspensionSetSteeringAngle;
}
// Input
MouseSetPosition, MouseGetPositionX, MouseGetPositionY, MouseShowCursor, 
KeyIsPressed,
// Window
WindowCreate, WindowGetHandle, WindowSetTitle, WindowDestroy,
// Squall
SquallInit, SquallAddSound, SquallPlay, SquallStop,
// Lua
LuaManagerCreate, LuaManagerSetConstantReal, LuaManagerSetConstantString,
LuaManagerRunScript, LuaManagerCallFunction;

begin
end.
