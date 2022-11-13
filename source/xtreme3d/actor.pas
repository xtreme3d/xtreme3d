function ActorCreate(fname: PAnsiChar; matl,parent: real): real; cdecl;
var
  actor: TGLActor;
  ml: TGLMaterialLibrary;
begin
  ml:=TGLMaterialLibrary(RealToPtr(matl));
  if not (parent=0) then
    actor:=TGLActor.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    actor:=TGLActor.CreateAsChild(scene.Objects);
  actor.MaterialLibrary:=ml;
  actor.IgnoreMissingTextures := True;
  
  if not FileExists(StrConv(fname)) then begin
    result:=0;
    ShowMessage('ActorCreate:' + #13#10 + 'File does not exist');
    Exit;
  end;
  try
    actor.LoadFromFile(StrConv(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorCreate:' + #13#10 + E.Message);
    end;
  end;

  actor.AnimationMode:=aamLoop;
  result:=ObjToReal(actor);
end;

function ActorCopy(actor,parent: real): real; cdecl;
var
  GLActor1,GLActor2: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor2:=TGLActor.Create(scene);
  TGLScene(scene).Objects.AddChild(GLActor2);
  if not (parent=0) then GLActor2.Parent:=TGLBaseSceneObject(RealToPtr(parent));
  GLActor2.Assign(GLActor1);
  result:=Integer(GLActor2);
end;

function ActorSetAnimationRange(actor,astart,aend: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.StartFrame:=Trunc(astart);
  GLActor1.EndFrame:=Trunc(aend);
  result:=1;
end;

function ActorGetCurrentFrame(actor: real): real; cdecl;
var
  GLActor1: TGLActor;
  fr: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  fr:=GLActor1.CurrentFrame;
  result:=fr;
end;

function ActorSwitchToAnimation(actor,anim,smooth: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.SwitchToAnimation(Trunc(anim),boolean(Trunc(smooth)));
  result:=1;
end;

function ActorSwitchToAnimationName(actor: real; anim: PAnsiChar; smooth: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.SwitchToAnimation(String(AnsiString(anim)), boolean(Trunc(smooth)));
  result:=1;
end;

function ActorSynchronize(actor1,actor2: real): real; cdecl;
var
  GLActor1,GLActor2: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor1));
  GLActor2:=TGLActor(RealToPtr(actor2));
  GLActor1.Synchronize(GLActor2);
  result:=1;
end;

function ActorSetInterval(actor,interv: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.Interval:=Trunc(interv);
  result:=1;
end;

function ActorSetAnimationMode(actor,aam: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  if aam=0 then GLActor1.AnimationMode:=aamNone;
  if aam=1 then GLActor1.AnimationMode:=aamPlayOnce;
  if aam=2 then GLActor1.AnimationMode:=aamLoop;
  if aam=3 then GLActor1.AnimationMode:=aamBounceForward;
  if aam=4 then GLActor1.AnimationMode:=aamBounceBackward;
  if aam=5 then GLActor1.AnimationMode:=aamLoopBackward;
  result:=1;
end;

function ActorSetFrameInterpolation(actor,afp: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  if afp=0 then GLActor1.FrameInterpolation:=afpNone;
  if afp=1 then GLActor1.FrameInterpolation:=afpLinear;
  result:=1;
end;

function ActorAddObject(actor: real; fname: PAnsiChar): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));

  try
    GLActor1.AddDataFromFile(StrConv(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorAddObject:' + #13#10 + E.Message);
    end;
  end;

  result:=1;
end;

function ActorGetCurrentAnimation(actor: real): PAnsiChar; cdecl;
var
  GLActor1: TGLActor;
  anim: String;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  anim:=GLActor1.CurrentAnimation;
  result:=PAnsiChar(AnsiString(anim));
end;

function ActorGetFrameCount(actor: real): real; cdecl;
var
  GLActor1: TGLActor;
  fcount: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  fcount:=GLActor1.FrameCount;
  result:=fcount;
end;

function ActorGetBoneCount(actor: real): real; cdecl;
var
  GLActor1: TGLActor;
  bcount: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  bcount:=GLActor1.Skeleton.BoneCount;
  result:=bcount;
end;

function ActorGetBoneByName(actor: real; name: PAnsiChar): real; cdecl;
var
  GLActor1: TGLActor;
  bone: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  bone:=GLActor1.Skeleton.BoneByName(String(AnsiString(name))).BoneID;
  result:=bone;
end;

function ActorGetBoneRotation(actor,bone,ind: real): real; cdecl;
var
  GLActor1: TGLActor;
  m: TMatrix4f;
  index: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  m:=GLActor1.Skeleton.BoneByID(Trunc(bone)).GlobalMatrix;
  index:=Trunc(ind);
  result:=m.V[2].V[index];
end;

function ActorGetBonePosition(actor,bone,ind: real): real; cdecl;
var
  GLActor1: TGLActor;
  m: TMatrix4f;
  index: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  m:=GLActor1.Skeleton.BoneByID(Trunc(bone)).GlobalMatrix;
  index:=Trunc(ind);
  result:=m.V[3].V[index];
end;

function ActorBoneExportMatrix(actor,bone,obj: real): real; cdecl;
var
  GLActor1: TGLActor;
  scobj: TGLSceneObject;
  m: TMatrix4f;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  scobj:=TGLSceneObject(RealToPtr(obj));
  m:=GLActor1.Skeleton.BoneByID(Trunc(bone)).GlobalMatrix;
  scobj.SetMatrix(m);
  result:=1;
end;

function ActorMakeSkeletalTranslationStatic(actor,anim: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.Animations[Trunc(anim)].MakeSkeletalTranslationStatic;
  result:=1;
end;

function ActorMakeSkeletalRotationDelta(actor,anim: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.Animations[Trunc(anim)].MakeSkeletalRotationDelta;
  result:=1;
end;

function ActorShowSkeleton(actor,mode: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.OverlaySkeleton:=boolean(Trunc(mode));
  result:=1;
end;

function AnimationBlenderCreate: real; cdecl;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler.Create(scene);
  result:=ObjToReal(GLAnim1);
end;

function AnimationBlenderSetActor(anim,actor: real): real; cdecl;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler(RealToPtr(anim));
  if actor=0 then GLAnim1.Actor:=nil
  else GLAnim1.Actor:=TGLActor(RealToPtr(actor));
  result:=1;
end;

function AnimationBlenderSetAnimation(anim: real; name: PAnsiChar): real; cdecl;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler(RealToPtr(anim));
  GLAnim1.AnimationName:=String(AnsiString(name));
  result:=1;
end;

function AnimationBlenderSetRatio(anim,rat: real): real; cdecl;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler(RealToPtr(anim));
  GLAnim1.Ratio:=rat;
  result:=1;
end;

function ActorLoadQ3TagList(fname: PAnsiChar): real; cdecl;
var
  tlist: TMD3TagList;
begin
  tlist:=TMD3TagList.Create;
  
  try
    tlist.LoadFromFile(StrConv(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorLoadQ3TagList:' + #13#10 + E.Message);
    end;
  end;
  
  result:=ObjToReal(tlist);
end;

function ActorQ3TagExportMatrix(actor,taglist: real; tagname: PAnsiChar; obj: real): real; cdecl;
var
  GLActor1: TGLActor;
  GLObject: TGLSceneObject;
  tlist: TMD3TagList;
  m1,m2,m3: TGLMatrix;
  i,j : integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLObject:=TGLSceneObject(RealToPtr(obj));
  tlist:=TMD3TagList(RealToPtr(taglist));
  m1:=tlist.GetTransform(String(AnsiString(tagname)), GLActor1.CurrentFrame);
  m2:=tlist.GetTransform(String(AnsiString(tagname)), GLActor1.NextFrameIndex);
  for j:=0 to 3 do
    for i:=0 to 3 do
      m3.V[i].V[j] := m1.V[i].V[j] + (m2.V[i].V[j] - m1.V[i].V[j]) * GLActor1.CurrentFrameDelta;
  //GLObject.Matrix:=m3;
  GLObject.SetMatrix(m3);
  result:=1;
end;

function ActorLoadQ3Animations(actor: real; fname,clas: PAnsiChar): real; cdecl;
var
  GLActor1: TGLActor;
  list: TStringList;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  list := TStringList.Create;
  
  try
    list.Text := LoadStringFromFile(StrConv(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorLoadQ3Animations:' + #13#10 + E.Message);
    end;
  end;
  
  LoadQ3Anims(GLActor1.Animations, list, String(AnsiString(clas)));
  result:=1;
end;

function ActorMeshObjectsCount(actor: real): real; cdecl;
var
  GLActor1: TGLActor;
  mobj: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  mobj:=GLActor1.MeshObjects.Count;
  result:=mobj;
end;

function ActorFaceGroupsCount(actor,meshobject: real): real; cdecl;
var
  GLActor1: TGLActor;
  mfg: integer;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  mfg:=GLActor1.MeshObjects[Trunc(meshobject)].FaceGroups.Count;
  result:=mfg;
end;

function ActorFaceGroupGetMaterialName(actor,meshobject,facegroup: real): PAnsiChar; cdecl;
var
  GLActor1: TGLActor;
  fgm: String;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  fgm:=GLActor1.MeshObjects[Trunc(meshobject)].FaceGroups[Trunc(facegroup)].MaterialName;
  result:=PAnsiChar(AnsiString(fgm));
end;

function ActorFaceGroupSetMaterial(actor,meshobject,facegroup: real; mtrl: PAnsiChar): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  GLActor1.MeshObjects[Trunc(meshobject)].FaceGroups[Trunc(facegroup)].MaterialName:=String(AnsiString(mtrl));
  result:=1;
end;

function ActorMoveBone(actor, boneindex, x, y, z: real): real; cdecl;
var
  ac: TGLActor;
  pos: TAffineVector;
begin
  ac := TGLActor(RealToPtr(actor));
  pos := ac.Skeleton.Frames[ac.NextFrameIndex].Position[Trunc(boneindex)];
  ac.Skeleton.Frames[ac.NextFrameIndex].Position[Trunc(boneindex)] :=
    VectorAdd(pos, AffineVectorMake(x, y, z));
  result := 1;
end;

function ActorRotateBone(actor, boneindex, x, y, z: real): real; cdecl;
var
  ac: TGLActor;
  rot: TAffineVector;
begin
  ac := TGLActor(RealToPtr(actor));
  rot := ac.Skeleton.Frames[ac.NextFrameIndex].Rotation[Trunc(boneindex)];
  ac.Skeleton.Frames[ac.NextFrameIndex].Rotation[Trunc(boneindex)] :=
    VectorAdd(rot, AffineVectorMake(x, y, z));
  result := 1;
end;

function ActorMeshSetVisible(actor, mesh, mode: real): real; cdecl;
var
  act: TGLActor;
begin
  act := TGLActor(RealToPtr(actor));
  act.MeshObjects[Trunc(mesh)].Visible := Boolean(Trunc(mode));
  result := 1.0;
end;

function ActorGetAnimationName(actor,ind:real): PAnsiChar; cdecl;
var
  act: TGLActor;
begin
  act:=TGLActor(RealToPtr(actor));
  result := PAnsiChar(AnsiString(act.Animations.Items[Trunc(ind)].Name));
end;

function ActorGetAnimationCount(actor:real): real; cdecl;
var
  act: TGLActor;
begin
  act:=TGLActor(RealToPtr(actor));
  result := act.Animations.Count;
end;

function ActorAnimationDestroy(actor,index:real): real; cdecl;
var
  act: TGLActor;
begin
  act:=TGLActor(RealToPtr(actor));
  //act.Animations.Items[Trunc(index)].Destroy;
  act.Animations.Delete(Trunc(index));
  result := 1;
end;

function ActorAnimationNextFrame(actor:real): real; cdecl;
var
  act: TGLActor;
begin
  act:=TGLActor(RealToPtr(actor));
  act.NextFrame;
  result := 1;
end;

function ActorAnimationPrevFrame(actor:real): real; cdecl;
var
  act: TGLActor;
begin
  act:=TGLActor(RealToPtr(actor));
  act.PrevFrame;
  result := 1;
end;

function ActorSetFrame(actor, frame: real): real; cdecl;
var
  act: TGLActor;
begin
  act := TGLActor(RealToPtr(actor));
  act.CurrentFrame:=Trunc(frame);
  result := 1.0;
end;

function ActorTriangleCount(actor: real): real; cdecl;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(RealToPtr(actor));
  result:=Integer(GLActor1.MeshObjects.TriangleCount);
end;
