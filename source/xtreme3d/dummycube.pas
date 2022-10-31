function DummycubeCreate(parent: real): real; cdecl;
var
    dummycube: TGLDummyCube;
begin
    if not (parent = 0) then
        dummycube:=TGLDummyCube.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
        dummycube:=TGLDummyCube.CreateAsChild(scene.Objects);
    result := ObjToReal(dummycube);
end;

function DummycubeAmalgamate(obj,mode: real): real; cdecl;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(RealToPtr(obj)));
  GLDummyCube1.Amalgamate:=boolean(Trunc(mode));
  result:=1;
end;

function DummycubeSetCameraMode(obj,cim: real): real; cdecl;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(RealToPtr(obj)));
  if cim=0 then GLDummyCube1.CamInvarianceMode:=cimNone;
  if cim=1 then GLDummyCube1.CamInvarianceMode:=cimPosition;
  if cim=2 then GLDummyCube1.CamInvarianceMode:=cimOrientation;
  result:=1;
end;

function DummycubeSetVisible(obj,mode: real): real; cdecl;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(RealToPtr(obj)));
  GLDummyCube1.VisibleAtRunTime:=boolean(Trunc(mode));
  result:=1;
end;

function DummycubeSetEdgeColor(obj,color: real): real; cdecl;
var
  GLDummyCube1: TGLDummyCube;
  GLColor: TGLColor;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(RealToPtr(obj)));
  GLColor:=TGLColor.Create(scene);
  GLColor.AsWinColor:=Trunc(color);
  GLDummyCube1.EdgeColor:=GLColor;
  result:=1;
end;

function DummycubeSetCubeSize(obj,size: real): real; cdecl;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(RealToPtr(obj)));
  GLDummyCube1.CubeSize:=size;
  result:=1;
end;

