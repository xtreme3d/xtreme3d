function KraftCreate: real; cdecl;
var
  kraft: TKraft;
begin
  kraft := TKraft.Create(-1);
  kraft.SetFrequency(60.0);
  kraft.VelocityIterations := 8;
  kraft.PositionIterations := 3;
  kraft.SpeculativeIterations := 8;
  kraft.TimeOfImpactIterations := 20;
  kraft.Gravity.y := -9.81;
  Result := ObjToReal(kraft);
end;

function KraftStep(kr, dt: real): real; cdecl;
var
  kraft: TKraft;
begin
  kraft := TKraft(RealToPtr(kr));
  kraft.Step(dt);
  Result := 1.0;
end;

function KraftGetRayHitPosition(index: real): real; cdecl;
begin
  Result := kraftRaycastPoint.xyz[trunc(index)];
end;

function KraftGetRayHitNormal(index: real): real; cdecl;
begin
  Result := kraftRaycastNormal.xyz[trunc(index)];
end;

function KraftCreateRigidBody(kr, typ: real): real; cdecl;
var
  rb: TKraftRigidBody;
  rbt: TKraftRigidBodyType;
begin
  rb := TKraftRigidBody.Create(TKraft(RealToPtr(kr)));
  rbt := krbtUnknown;
       if typ = 0 then rbt := krbtUnknown
  else if typ = 1 then rbt := krbtStatic
  else if typ = 2 then rbt := krbtDynamic
  else if typ = 3 then rbt := krbtKinematic;
  rb.SetRigidBodyType(rbt);
  rb.SetToAwake;
  Result := ObjToReal(rb);
end;

function KraftRigidBodyFinish(krb: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.Finish;
  Result := 1.0;
end;

function KraftRigidBodySetGravity(krb, x, y, z, scale: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.Gravity.x := x;
  rb.Gravity.y := y;
  rb.Gravity.z := z;
  rb.GravityScale := scale;
  Result := 1.0;
end;

function KraftRigidBodySetPosition(krb, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.SetWorldPosition(Vector3(x, y, z));
  rb.SetToAwake;
  Result := 1.0;
end;

function KraftRigidBodyGetPosition(krb, index: real): real; cdecl;
var
  rb: TKraftRigidBody;
  v: TKraftVector4;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  v := Matrix4x4GetColumn(rb.WorldTransform, 3);
  Result := v.xyzw[trunc(index)];
end;

function KraftRigidBodySetLinearVelocity(krb, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.LinearVelocity := Vector3(x, y, z);
  rb.SetToAwake;
  Result := 1.0;
end;

function KraftRigidBodyGetLinearVelocity(krb, index: real): real; cdecl;
var
  rb: TKraftRigidBody;
  v: TKraftVector3;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  v := rb.LinearVelocity;
  Result := v.xyz[trunc(index)];
end;

function KraftRigidBodySetRotation(krb, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.SetOrientation(x, y, z);
  rb.SetToAwake;
  Result := 1.0;
end;

function KraftRigidBodyGetDirection(krb, index: real): real; cdecl;
var
  rb: TKraftRigidBody;
  v: TKraftVector4;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  v := Matrix4x4GetColumn(rb.WorldTransform, 2);
  Result := v.xyz[trunc(index)];
end;

function KraftRigidBodyGetUp(krb, index: real): real; cdecl;
var
  rb: TKraftRigidBody;
  v: TKraftVector4;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  v := Matrix4x4GetColumn(rb.WorldTransform, 1);
  Result := v.xyz[trunc(index)];
end;

function KraftRigidBodyGetRight(krb, index: real): real; cdecl;
var
  rb: TKraftRigidBody;
  v: TKraftVector4;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  v := Matrix4x4GetColumn(rb.WorldTransform, 0);
  Result := v.xyz[trunc(index)];
end;

function KraftRigidBodySetAngularVelocity(krb, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.AngularVelocity := Vector3(x, y, z);
  rb.SetToAwake;
  Result := 1.0;
end;

function KraftRigidBodyGetAngularVelocity(krb, index: real): real; cdecl;
var
  rb: TKraftRigidBody;
  v: TKraftVector3;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  v := rb.AngularVelocity;
  Result := v.xyz[trunc(index)];
end;

function KraftRigidBodyAddForce(krb, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.AddWorldForce(Vector3(x, y, z));
  Result := 1.0;
end;

function KraftRigidBodyAddForceAtPos(krb, x, y, z, px, py, pz: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.AddForceAtPosition(Vector3(x, y, z), Vector3(px, py, pz));
  Result := 1.0;
end;

function KraftRigidBodyAddRelForce(krb, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
begin
  rb := TKraftRigidBody(RealToPtr(krb));
  rb.AddBodyForce(Vector3(x, y, z));
  Result := 1.0;
end;

function KraftRayCast(kr, x, y, z, dx, dy, dz, maxTime: real): real; cdecl;
var
  kraft: TKraft;
  s: TKraftShape;
  t: TKraftScalar;
  r: Boolean;
begin
  kraft := TKraft(RealToPtr(kr));
  r := kraft.RayCast(Vector3(x, y, z), Vector3(dx, dy, dz), maxTime, s, t, kraftRaycastPoint, kraftRaycastNormal);
  Result := Integer(r);
end;

function KraftObjectSetRigidBody(obj, krb: real): real; cdecl;
var
  ob: TGLBaseSceneObject;
  rb: TKraftRigidBody;
  glkrb: TGLKraftRigidBody;
begin
  ob := TGLBaseSceneObject(RealToPtr(obj));
  rb := TKraftRigidBody(RealToPtr(krb));
  glkrb := GetOrCreateKraftRigidBody(ob);
  glkrb.RigidBody := rb;
  Result := ObjToReal(glkrb);
end; 

function KraftCreateShapeSphere(rbody, radius: real): real; cdecl;
var
  rb: TKraftRigidBody;
  ss: TKraftShapeSphere;
begin
  rb := TKraftRigidBody(RealToPtr(rbody));
  ss := TKraftShapeSphere.Create(rb.Physics, rb, radius);
  Result := ObjToReal(ss);
end;

function KraftCreateShapeBox(rbody, x, y, z: real): real; cdecl;
var
  rb: TKraftRigidBody;
  sb: TKraftShapeBox;
begin
  rb := TKraftRigidBody(RealToPtr(rbody));
  sb := TKraftShapeBox.Create(rb.Physics, rb, Vector3(x, y, z));
  Result := ObjToReal(sb);
end;

function KraftCreateShapePlane(rbody, x, y, z, d: real): real; cdecl;
var
  rb: TKraftRigidBody;
  sp: TKraftShapePlane;
begin
  rb := TKraftRigidBody(RealToPtr(rbody));
  sp := TKraftShapePlane.Create(rb.Physics, rb, Plane(Vector3(x, y, z), d));
  Result := ObjToReal(sp);
end;

function KraftCreateShapeCapsule(rbody, radius, height: real): real; cdecl;
var
  rb: TKraftRigidBody;
  sc: TKraftShapeCapsule;
begin
  rb := TKraftRigidBody(RealToPtr(rbody));
  sc := TKraftShapeCapsule.Create(rb.Physics, rb, radius, height);
  Result := ObjToReal(sc);
end;

function KraftCreateShapeMesh(rbody, ff: real): real; cdecl;
var
  rb: TKraftRigidBody;
  mesh: TKraftMesh;
  sm: TKraftShapeMesh;
  freeform: TGLFreeForm;
  glsmesh: TGLMeshObject;
  mi, fgi, ii: Integer;
  vi1, vi2, vi3: Integer;
  vec1, vec2, vec3: TAffineVector;
  nor1, nor2, nor3: TAffineVector; 
  fg: TFGVertexIndexList;
begin
  rb := TKraftRigidBody(RealToPtr(rbody));
  freeform := TGLFreeForm(RealToPtr(ff));
  mesh := TKraftMesh.Create(rb.Physics);
  for mi:=0 to freeform.MeshObjects.Count-1 do begin
    glsmesh := freeform.MeshObjects[mi];

    for fgi:=0 to glsmesh.FaceGroups.Count-1 do begin
      fg := TFGVertexIndexList(glsmesh.FaceGroups[fgi]);

      for ii:=0 to fg.TriangleCount - 1 do begin
        vi1 := fg.VertexIndices[ii * 3 + 0];
        vi2 := fg.VertexIndices[ii * 3 + 1];
        vi3 := fg.VertexIndices[ii * 3 + 2];

        vec1 := glsmesh.Vertices[vi1];
        vec2 := glsmesh.Vertices[vi2];
        vec3 := glsmesh.Vertices[vi3];

        nor1 := glsmesh.Normals[vi1];
        nor2 := glsmesh.Normals[vi2];
        nor3 := glsmesh.Normals[vi3];

        mesh.AddTriangle(
          mesh.AddVertex(Vector3(vec1.v[0], vec1.v[1], vec1.v[2])),
          mesh.AddVertex(Vector3(vec2.v[0], vec2.v[1], vec2.v[2])),
          mesh.AddVertex(Vector3(vec3.v[0], vec3.v[1], vec3.v[2])),
          mesh.AddNormal(Vector3(nor1.v[0], nor1.v[1], nor1.v[2])),
          mesh.AddNormal(Vector3(nor2.v[0], nor2.v[1], nor2.v[2])),
          mesh.AddNormal(Vector3(nor3.v[0], nor3.v[1], nor3.v[2])));
      end;
    end;
  end;

  mesh.Scale(Vector3(freeform.Scale.x, freeform.Scale.y, freeform.Scale.z));
  mesh.DoubleSided := True;
  mesh.Finish;
  sm := TKraftShapeMesh.Create(rb.Physics, rb, mesh);
  sm.Finish;
  Result := ObjToReal(sm);
end;

function KraftShapeSetDensity(shape, density: real): real; cdecl;
var
  s: TKraftShape;
begin
  s := TKraftShape(RealToPtr(shape));
  s.Density := density;
  Result := 1.0;
end;

function KraftShapeSetFriction(shape, friction: real): real; cdecl;
var
  s: TKraftShape;
begin
  s := TKraftShape(RealToPtr(shape));
  s.Friction := friction;
  Result := 1.0;
end;

function KraftShapeSetRestitution(shape, rest: real): real; cdecl;
var
  s: TKraftShape;
begin
  s := TKraftShape(RealToPtr(shape));
  s.Restitution := rest;
  Result := 1.0;
end;

function KraftShapeSetPosition(shape, x, y, z: real): real; cdecl;
var
  s: TKraftShape;
  m: TKraftMatrix4x4;
  v: TKraftVector4;
begin
  s := TKraftShape(RealToPtr(shape));
  m := s.LocalTransform;
  v := Matrix4x4GetColumn(m, 3);
  v.x := x;
  v.y := y;
  v.z := z;
  Matrix4x4SetColumn(m, 3, v);
  s.LocalTransform := m;
  Result := 1.0;
end;

function KraftShapeGetPosition(shape, index: real): real; cdecl;
var
  s: TKraftShape;
  m: TKraftMatrix4x4;
  v: TKraftVector4;
begin
  s := TKraftShape(RealToPtr(shape));
  m := s.LocalTransform;
  v := Matrix4x4GetColumn(m, 3);
  Result := v.xyzw[trunc(index)];
end;

function KraftShapeSetRayCastable(shape, mode: real): real; cdecl;
var
  s: TKraftShape;
begin
  s := TKraftShape(RealToPtr(shape));
  if not Boolean(trunc(mode)) then
    s.Flags := s.Flags - [ksfRayCastable]
  else
    s.Flags := s.Flags + [ksfRayCastable];
  Result := 1.0;
end;

function KraftCreateJointDistance(rbody1, rbody2: real): real; cdecl;
var
  rba, rbb: TKraftRigidBody;
  j: TKraftConstraintJointDistance;
begin
  rba := TKraftRigidBody(RealToPtr(rbody1));
  rbb := TKraftRigidBody(RealToPtr(rbody2));
  j := TKraftConstraintJointDistance.Create(rba.Physics, rba, rbb, Vector3(0, 0, 0), Vector3(0, 0, 0), 0, 0, true);
  Result := ObjToReal(j);
end;

function KraftCreateJointRope(rbody1, rbody2, maxlength: real): real; cdecl;
var
  rba, rbb: TKraftRigidBody;
  j: TKraftConstraintJointRope;
begin
  rba := TKraftRigidBody(RealToPtr(rbody1));
  rbb := TKraftRigidBody(RealToPtr(rbody2));
  j := TKraftConstraintJointRope.Create(rba.Physics, rba, rbb, Vector3(0, 0, 0), Vector3(0, 0, 0), maxlength, true);
  Result := ObjToReal(j);
end;

function KraftCreateJointBallSocket(rbody1, rbody2: real): real; cdecl;
var
  rba, rbb: TKraftRigidBody;
  j: TKraftConstraintJointBallSocket;
begin
  rba := TKraftRigidBody(RealToPtr(rbody1));
  rbb := TKraftRigidBody(RealToPtr(rbody2));
  j := TKraftConstraintJointBallSocket.Create(rba.Physics, rba, rbb, Vector3(0, 0, 0), Vector3(0, 0, 0), true);
  Result := ObjToReal(j);
end;

function KraftCreateJointFixed(rbody1, rbody2: real): real; cdecl;
var
  rba, rbb: TKraftRigidBody;
  j: TKraftConstraintJointFixed;
begin
  rba := TKraftRigidBody(RealToPtr(rbody1));
  rbb := TKraftRigidBody(RealToPtr(rbody2));
  j := TKraftConstraintJointFixed.Create(rba.Physics, rba, rbb, Vector3(0, 0, 0));
  Result := ObjToReal(j);
end;

function KraftCreateJointHinge(rbody1, rbody2: real): real; cdecl;
var
  rba, rbb: TKraftRigidBody;
  j: TKraftConstraintJointHinge;
begin
  rba := TKraftRigidBody(RealToPtr(rbody1));
  rbb := TKraftRigidBody(RealToPtr(rbody2));
  j := TKraftConstraintJointHinge.Create(rba.Physics, rba, rbb, Vector3(0, 0, 0), Vector3(0, 0, 0), false, false, -1, 1, 0, 0, true);
  Result := ObjToReal(j);
end;

function KraftJointSetAnchor1(joint, x, y, z: real): real; cdecl;
var
  j: TKraftConstraintJoint;
begin
  j := TKraftConstraintJoint(RealToPtr(joint));
  j.SetLocalAnchorA(Vector3(x, y, z));
  Result := 1.0;
end;

function KraftJointSetAnchor2(joint, x, y, z: real): real; cdecl;
var
  j: TKraftConstraintJoint;
begin
  j := TKraftConstraintJoint(RealToPtr(joint));
  j.SetLocalAnchorB(Vector3(x, y, z));
  Result := 1.0;
end;

function KraftJointSetHingeAxis1(joint, x, y, z: real): real; cdecl;
var
  j: TKraftConstraintJointHinge;
begin
  j := TKraftConstraintJointHinge(RealToPtr(joint));
  j.SetLocalAxisA(Vector3(x, y, z));
  Result := 1.0;
end;

function KraftJointSetHingeAxis2(joint, x, y, z: real): real; cdecl;
var
  j: TKraftConstraintJointHinge;
begin
  j := TKraftConstraintJointHinge(RealToPtr(joint));
  j.SetLocalAxisB(Vector3(x, y, z));
  Result := 1.0;
end;
