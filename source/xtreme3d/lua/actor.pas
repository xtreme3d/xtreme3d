// Actor
function lua_ActorCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorCreate(pchar(Args[0].AsString), Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorCopy(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorCopy(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSetAnimationRange(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSetAnimationRange(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorGetCurrentFrame(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorGetCurrentFrame(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSwitchToAnimation(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSwitchToAnimation(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSwitchToAnimationName(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSwitchToAnimationName(Args[0].AsDouble, pchar(Args[1].AsString), Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSynchronize(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSynchronize(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSetInterval(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSetInterval(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSetAnimationMode(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSetAnimationMode(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorSetFrameInterpolation(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorSetFrameInterpolation(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorAddObject(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorAddObject(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_ActorGetCurrentAnimation(const Args: TLuaArgs): TLuaArg;
var
  c: String;
begin
  c := String(ActorGetCurrentAnimation(Args[0].AsDouble));
  result := LuaArg(c);
end;

function lua_ActorGetFrameCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorGetFrameCount(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorGetBoneCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorGetBoneCount(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorGetBoneByName(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorGetBoneByName(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_ActorRotateBone(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorRotateBone(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorGetBoneRotation(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorGetBoneRotation(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorMoveBone(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorMoveBone(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorGetBonePosition(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorGetBonePosition(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorShowSkeleton(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorShowSkeleton(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorBoneExportMatrix(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorBoneExportMatrix(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorMakeSkeletalTranslationStatic(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorMakeSkeletalTranslationStatic(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorMakeSkeletalRotationDelta(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorMakeSkeletalRotationDelta(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_AnimationBlenderCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := AnimationBlenderCreate();
  result := LuaArg(c);
end;

function lua_AnimationBlenderSetActor(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := AnimationBlenderSetActor(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_AnimationBlenderSetAnimation(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := AnimationBlenderSetAnimation(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_AnimationBlenderSetRatio(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := AnimationBlenderSetRatio(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorLoadQ3TagList(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorLoadQ3TagList(pchar(Args[0].AsString));
  result := LuaArg(c);
end;

function lua_ActorQ3TagExportMatrix(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorQ3TagExportMatrix(Args[0].AsDouble, Args[1].AsDouble, pchar(Args[2].AsString), Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorLoadQ3Animations(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorLoadQ3Animations(Args[0].AsDouble, pchar(Args[1].AsString), pchar(Args[2].AsString));
  result := LuaArg(c);
end;

function lua_ActorMeshObjectsCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorMeshObjectsCount(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorFaceGroupsCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorFaceGroupsCount(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_ActorFaceGroupGetMaterialName(const Args: TLuaArgs): TLuaArg;
var
  c: String;
begin
  c := String(ActorFaceGroupGetMaterialName(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble));
  result := LuaArg(c);
end;

function lua_ActorFaceGroupSetMaterial(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := ActorFaceGroupSetMaterial(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, pchar(Args[3].AsString));
  result := LuaArg(c);
end;
