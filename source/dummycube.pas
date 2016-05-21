function DummycubeCreate(parent: real): real; stdcall;
var
  GLDummyCube1: TGLDummyCube;
begin
  if not (parent=0) then
    GLDummyCube1:=TGLDummyCube.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLDummyCube1:=TGLDummyCube.CreateAsChild(scene.Objects);
  result:=Integer(GLDummyCube1);
end;

function DummycubeAmalgamate(obj,mode: real): real; stdcall;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(trunc64(obj)));
  GLDummyCube1.Amalgamate:=boolean(trunc64(mode));
  result:=1;
end;

function DummycubeSetCameraMode(obj,cim: real): real; stdcall;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(trunc64(obj)));
  if cim=0 then GLDummyCube1.CamInvarianceMode:=cimNone;
  if cim=1 then GLDummyCube1.CamInvarianceMode:=cimPosition;
  if cim=2 then GLDummyCube1.CamInvarianceMode:=cimOrientation;
  result:=1;
end;

function DummycubeSetVisible(obj,mode: real): real; stdcall;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(trunc64(obj)));
  GLDummyCube1.VisibleAtRunTime:=boolean(trunc64(mode));
  result:=1;
end;

function DummycubeSetEdgeColor(obj,color: real): real; stdcall;
var
  GLDummyCube1: TGLDummyCube;
  GLColor: TGLColor;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(trunc64(obj)));
  GLColor:=TGLColor.Create(scene);
  GLColor.AsWinColor:=trunc64(color);
  GLDummyCube1.EdgeColor:=GLColor;
  result:=1;
end;

function DummycubeSetCubeSize(obj,size: real): real; stdcall;
var
  GLDummyCube1: TGLDummyCube;
begin
  GLDummyCube1:=TGLDummyCube(TGLSceneObject(trunc64(obj)));
  GLDummyCube1.CubeSize:=size;
  result:=1;
end;
