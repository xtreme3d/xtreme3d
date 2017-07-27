// Freeform
function lua_FreeformCreate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformCreate(pchar(Args[0].AsString), Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformCreateEmpty(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformCreateEmpty(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformAddMesh(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformAddMesh(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddVertex(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddVertex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddNormal(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddNormal(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddTexCoord(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddSecondTexCoord(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddSecondTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddTangent(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddTangent(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddBinormal(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddBinormal(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetVertex(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetVertex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetNormal(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetNormal(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetTexCoord(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetSecondTexCoord(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetSecondTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetTangent(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetTangent(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetBinormal(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetBinormal(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshFaceGroupSetIndex(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshFaceGroupSetIndex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGetVertex(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGetVertex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGetNormal(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGetNormal(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGetTexCoord(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGetTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGetSecondTexCoord(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGetSecondTexCoord(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGetTangent(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGetTangent(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGetBinormal(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGetBinormal(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshFaceGroupGetIndex(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshFaceGroupGetIndex(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshVerticesCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshVerticesCount(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshTriangleCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshTriangleCount(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshObjectsCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshObjectsCount(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshAddFaceGroup(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshAddFaceGroup(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshFaceGroupAddTriangle(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshFaceGroupAddTriangle(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble, Args[5].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshFaceGroupTriangleCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshFaceGroupTriangleCount(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshFaceGroupSetMaterial(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshFaceGroupSetMaterial(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, pchar(Args[3].AsString));
  result := LuaArg(c);
end;

function lua_FreeformMeshFaceGroupGetMaterial(const Args: TLuaArgs): TLuaArg;
var
  c: pchar;
begin
  c := FreeformMeshFaceGroupGetMaterial(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(string(c));
end;

function lua_FreeformMeshFaceGroupsCount(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshFaceGroupsCount(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGenNormals(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGenNormals(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshGenTangents(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshGenTangents(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetVisible(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetVisible(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetSecondCoords(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetSecondCoords(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshTranslate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshTranslate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshRotate(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshRotate(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshScale(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshScale(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble, Args[4].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformMeshSetMaterial(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformMeshSetMaterial(Args[0].AsDouble, Args[1].AsDouble, pchar(Args[2].AsString));
  result := LuaArg(c);
end;

function lua_FreeformUseMeshMaterials(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformUseMeshMaterials(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformPointInMesh(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformPointInMesh(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformSphereSweepIntersect(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformSphereSweepIntersect(Args[0].AsDouble, Args[1].AsDouble, Args[2].AsDouble, Args[3].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformToFreeforms(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformToFreeforms(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformSave(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformSave(Args[0].AsDouble, pchar(Args[1].AsString));
  result := LuaArg(c);
end;

function lua_FreeformGenTangents(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformGenTangents(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformBuildOctree(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformBuildOctree(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformCreateExplosionFX(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformCreateExplosionFX(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformExplosionFXReset(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformExplosionFXReset(Args[0].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformExplosionFXEnable(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformExplosionFXEnable(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;

function lua_FreeformExplosionFXSetSpeed(const Args: TLuaArgs): TLuaArg;
var
  c: double;
begin
  c := FreeformExplosionFXSetSpeed(Args[0].AsDouble, Args[1].AsDouble);
  result := LuaArg(c);
end;
