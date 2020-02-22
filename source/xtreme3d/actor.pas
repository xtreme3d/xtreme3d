function ActorCreate(fname: pchar; matl,parent: real): real; stdcall;
var
  GLActor1: TGLActor;
  ml: TGLMaterialLibrary;
begin
  ml:=TGLMaterialLibrary(trunc64(matl));
  if not (parent=0) then
    GLActor1:=TGLActor.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLActor1:=TGLActor.CreateAsChild(scene.Objects);
  GLActor1.MaterialLibrary:=ml;
  
  try
    GLActor1.LoadFromFile(String(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorCreate:' + #13#10 + 'Error loading ' + String(fname) + #13#10 + E.Message);
    end;
  end;
  
  GLActor1.AnimationMode:=aamLoop;
  result:=Integer(GLActor1);
end;

function ActorCopy(actor,parent: real): real; stdcall;
var
  GLActor1,GLActor2: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor2:=TGLActor.Create(scene);
  TGLScene(scene).Objects.AddChild(GLActor2);
  if not (parent=0) then GLActor2.Parent:=TGLBaseSceneObject(trunc64(parent));
  GLActor2.Assign(GLActor1);
  result:=Integer(GLActor2);
end;

function ActorSetAnimationRange(actor,astart,aend: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.StartFrame:=trunc64(astart);
  GLActor1.EndFrame:=trunc64(aend);
  result:=1;
end;

function ActorGetCurrentFrame(actor: real): real; stdcall;
var
  GLActor1: TGLActor;
  fr: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  fr:=GLActor1.CurrentFrame;
  result:=fr;
end;

function ActorSwitchToAnimation(actor,anim,smooth: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.SwitchToAnimation(trunc64(anim),boolean(trunc64(smooth)));
  result:=1;
end;

function ActorSwitchToAnimationName(actor: real; anim: pchar; smooth: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.SwitchToAnimation(anim,boolean(trunc64(smooth)));
  result:=1;
end;

function ActorSynchronize(actor1,actor2: real): real; stdcall;
var
  GLActor1,GLActor2: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor1));
  GLActor2:=TGLActor(trunc64(actor2));
  GLActor1.Synchronize(GLActor2);
  result:=1;
end;

function ActorSetInterval(actor,interv: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.Interval:=trunc64(interv);
  result:=1;
end;

function ActorSetAnimationMode(actor,aam: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  if aam=0 then GLActor1.AnimationMode:=aamNone;
  if aam=1 then GLActor1.AnimationMode:=aamPlayOnce;
  if aam=2 then GLActor1.AnimationMode:=aamLoop;
  if aam=3 then GLActor1.AnimationMode:=aamBounceForward;
  if aam=4 then GLActor1.AnimationMode:=aamBounceBackward;
  if aam=5 then GLActor1.AnimationMode:=aamLoopBackward;
  result:=1;
end;

function ActorSetFrameInterpolation(actor,afp: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  if afp=0 then GLActor1.FrameInterpolation:=afpNone;
  if afp=1 then GLActor1.FrameInterpolation:=afpLinear;
  result:=1;
end;

function ActorAddObject(actor: real; fname: pchar): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  
  try
    GLActor1.AddDataFromFile(String(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorAddObject:' + #13#10 + 'Error loading ' + String(fname) + #13#10 + E.Message);
    end;
  end;
  
  result:=1;
end;

function ActorGetCurrentAnimation(actor: real): pchar; stdcall;
var
  GLActor1: TGLActor;
  anim: string;
begin
  GLActor1:=TGLActor(trunc64(actor));
  anim:=GLActor1.CurrentAnimation;
  result:=pchar(anim);
end;

function ActorGetFrameCount(actor: real): real; stdcall;
var
  GLActor1: TGLActor;
  fcount: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  fcount:=GLActor1.FrameCount;
  result:=fcount;
end;

function ActorGetBoneCount(actor: real): real; stdcall;
var
  GLActor1: TGLActor;
  bcount: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  bcount:=GLActor1.Skeleton.BoneCount;
  result:=bcount;
end;

function ActorGetBoneByName(actor: real; name: pchar): real; stdcall;
var
  GLActor1: TGLActor;
  bone: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  bone:=GLActor1.Skeleton.BoneByName(name).BoneID;
  result:=bone;
end;

function ActorGetBoneRotation(actor,bone,ind: real): real; stdcall;
var
  GLActor1: TGLActor;
  m: TMatrix4f;
  index: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  m:=GLActor1.Skeleton.BoneByID(trunc64(bone)).GlobalMatrix;
  index:=trunc64(ind);
  result:=m[2][index];
end;

function ActorGetBonePosition(actor,bone,ind: real): real; stdcall;
var
  GLActor1: TGLActor;
  m: TMatrix4f;
  index: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  m:=GLActor1.Skeleton.BoneByID(trunc64(bone)).GlobalMatrix;
  index:=trunc64(ind);
  result:=m[3][index];
end;

function ActorBoneExportMatrix(actor,bone,obj: real): real; stdcall;
var
  GLActor1: TGLActor;
  scobj: TGLSceneObject;
  m: TMatrix4f;
begin
  GLActor1:=TGLActor(trunc64(actor));
  scobj:=TGLSceneObject(trunc64(obj));
  m:=GLActor1.Skeleton.BoneByID(trunc64(bone)).GlobalMatrix;
  scobj.Matrix:=m;
  result:=1;
end;

function ActorMakeSkeletalTranslationStatic(actor,anim: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.Animations[trunc64(anim)].MakeSkeletalTranslationStatic;
  result:=1;
end;

function ActorMakeSkeletalRotationDelta(actor,anim: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.Animations[trunc64(anim)].MakeSkeletalRotationDelta;
  result:=1;
end;

function ActorShowSkeleton(actor,mode: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.OverlaySkeleton:=boolean(trunc64(mode));
  result:=1;
end;

function AnimationBlenderCreate: real; stdcall;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler.Create(scene);
  result:=Integer(GLAnim1);
end;

function AnimationBlenderSetActor(anim,actor: real): real; stdcall;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler(trunc64(anim));
  if actor=0 then GLAnim1.Actor:=nil
  else GLAnim1.Actor:=TGLActor(trunc64(actor));
  result:=1;
end;

function AnimationBlenderSetAnimation(anim: real; name: pchar): real; stdcall;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler(trunc64(anim));
  GLAnim1.AnimationName:=name;
  result:=1;
end;

function AnimationBlenderSetRatio(anim,rat: real): real; stdcall;
var
  GLAnim1: TGLAnimationControler;
begin
  GLAnim1:=TGLAnimationControler(trunc64(anim));
  GLAnim1.Ratio:=rat;
  result:=1;
end;

function ActorLoadQ3TagList(fname: pchar): real; stdcall;
var
  tlist: TMD3TagList;
begin
  tlist:=TMD3TagList.Create;
  
  try
    tlist.LoadFromFile(String(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorLoadQ3TagList:' + #13#10 + 'Error loading ' + String(fname) + #13#10 + E.Message);
    end;
  end;
  
  result:=integer(tlist);
end;

function ActorQ3TagExportMatrix(actor,taglist: real; tagname: pchar; obj: real): real; stdcall;
var
  GLActor1: TGLActor;
  GLObject: TGLSceneObject;
  tlist: TMD3TagList;
  m1,m2,m3: TMatrix;
  i,j : integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLObject:=TGLSceneObject(trunc64(obj));
  tlist:=TMD3TagList(trunc64(taglist));
  m1:=tlist.GetTransform(tagname,GLActor1.CurrentFrame);
  m2:=tlist.GetTransform(tagname,GLActor1.NextFrameIndex);
  for j:=0 to 3 do
    for i:=0 to 3 do
      m3[i][j]:=m1[i][j]+(m2[i][j]-m1[i][j])*GLActor1.CurrentFrameDelta;
  GLObject.Matrix:=m3;
  result:=1;
end;

function ActorLoadQ3Animations(actor: real; fname,clas: pchar): real; stdcall;
var
  GLActor1: TGLActor;
  list: TStringList;
begin
  GLActor1:=TGLActor(trunc64(actor));
  list := TStringList.Create;
  
  try
    list.Text := LoadStringFromFile2(String(fname));
  except
    On E: Exception do
    begin
      if showLoadingErrors then
        ShowMessage('ActorLoadQ3Animations:' + #13#10 + 'Error loading ' + String(fname) + #13#10 + E.Message);
    end;
  end;
  
  LoadQ3Anims(GLActor1.Animations, list, clas);
  result:=1;
end;

function ActorMeshObjectsCount(actor: real): real; stdcall;
var
  GLActor1: TGLActor;
  mobj: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  mobj:=GLActor1.MeshObjects.Count;
  result:=mobj;
end;

function ActorFaceGroupsCount(actor,meshobject: real): real; stdcall;
var
  GLActor1: TGLActor;
  mfg: integer;
begin
  GLActor1:=TGLActor(trunc64(actor));
  mfg:=GLActor1.MeshObjects[trunc64(meshobject)].FaceGroups.Count;
  result:=mfg;
end;

function ActorFaceGroupGetMaterialName(actor,meshobject,facegroup: real): pchar; stdcall;
var
  GLActor1: TGLActor;
  fgm: string;
begin
  GLActor1:=TGLActor(trunc64(actor));
  fgm:=GLActor1.MeshObjects[trunc64(meshobject)].FaceGroups[trunc64(facegroup)].MaterialName;
  result:=pchar(fgm);
end;

function ActorFaceGroupSetMaterial(actor,meshobject,facegroup: real; mtrl: pchar): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  GLActor1.MeshObjects[trunc64(meshobject)].FaceGroups[trunc64(facegroup)].MaterialName:=mtrl;
  result:=1;
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

function ActorMeshSetVisible(actor, mesh, mode: real): real; stdcall;
var
  act: TGLActor;
begin
  act := TGLActor(trunc64(actor));
  act.MeshObjects[trunc64(mesh)].Visible := Boolean(trunc64(mode));
  result := 1.0;
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

function ActorSetFrame(actor, frame: real): real; stdcall;
var
  act: TGLActor;
begin
  act := TGLActor(trunc64(actor));
  act.CurrentFrame:=trunc64(frame);
  result := 1.0;
end;

function ActorTriangleCount(actor: real): real; stdcall;
var
  GLActor1: TGLActor;
begin
  GLActor1:=TGLActor(trunc64(actor));
  result:=Integer(GLActor1.MeshObjects.TriangleCount);
end;


