function CameraCreate(parent: real): real; stdcall;
var
  GLCamera1: TGLCamera;
begin
  if not (parent=0) then
    GLCamera1:=TGLCamera.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLCamera1:=TGLCamera.CreateAsChild(scene.Objects);
  result:=Integer(GLCamera1);
end;

function CameraSetStyle(camera,cs: real): real; stdcall;
begin
  if trunc64(cs)=0 then TGLCamera(trunc64(camera)).CameraStyle:=csPerspective;
  if trunc64(cs)=1 then TGLCamera(trunc64(camera)).CameraStyle:=csOrthogonal;
  if trunc64(cs)=2 then TGLCamera(trunc64(camera)).CameraStyle:=csOrtho2D;
  if trunc64(cs)=3 then TGLCamera(trunc64(camera)).CameraStyle:=csInfinitePerspective;
  result:=1;
end;

function CameraSetFocal(camera,fov: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).FocalLength:=fov;
  result:=1;
end;

function CameraSetSceneScale(camera,scale: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).SceneScale:=scale;
  result:=1;
end;

function CameraScaleScene(camera,scale: real): real; stdcall;
var
  current: real;
begin
  current:=TGLCamera(trunc64(camera)).SceneScale;
  TGLCamera(trunc64(camera)).SceneScale:=current+scale;
  result:=1;
end;

function CameraSetViewDepth(camera,depth: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).DepthOfView:=depth;
  result:=1;
end;

function CameraSetTargetObject(camera,obj: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).TargetObject:=TGLBaseSceneObject(trunc64(obj));
  result:=1;
end;

function CameraMoveAroundTarget(camera,pitch,turn: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).MoveAroundTarget(pitch,turn);
  result:=1;
end;

function CameraSetDistanceToTarget(camera,distance: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).AdjustDistanceToTarget(distance);
  result:=1;
end;

function CameraGetDistanceToTarget(camera: real): real; stdcall;
var
  dist:real;
begin
  dist:=TGLCamera(trunc64(camera)).DistanceToTarget;
  result:=dist;
end;

function CameraCopyToTexture(camera: real; mtrl: pchar; width,height: real): real; stdcall;
var
  mat: TGLLibMaterial;
begin
  memviewer.Width:=trunc64(width);
  memviewer.Height:=trunc64(height);
  memviewer.Camera:=TGLCamera(trunc64(camera));
  mat:=matlib.Materials.GetLibMaterialByName(mtrl);
  mat.Material.Texture.Disabled:=false;
  memviewer.Render;
  memviewer.CopyToTexture(mat.Material.Texture);
  result:=1;
end;

function CameraGetNearPlane(camera: real): real; stdcall;
var
  npl: real;
begin
  npl:=TGLCamera(trunc64(camera)).NearPlane;
  result:=npl;
end;

function CameraSetNearPlaneBias(camera,bias: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).NearPlaneBias:=bias;
  result:=1;
end;

function CameraAbsoluteVectorToTarget(camera,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).AbsoluteVectorToTarget;
  result:=vec[trunc64(ind)];
end;

function CameraAbsoluteRightVectorToTarget(camera,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).AbsoluteRightVectorToTarget;
  result:=vec[trunc64(ind)];
end;

function CameraAbsoluteUpVectorToTarget(camera,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).AbsoluteUpVectorToTarget;
  result:=vec[trunc64(ind)];
end;

function CameraZoomAll(camera: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).ZoomAll;
  result:=1;
end;

function CameraScreenDeltaToVector(camera,dx,dy,ratio,nx,ny,nz,ind: real): real; stdcall;
var
  vec,pnorm: TVector;
begin
  pnorm := VectorMake(nx, ny, nz);
  vec:=TGLCamera(trunc64(camera)).ScreenDeltaToVector(trunc64(dx),trunc64(dy),ratio,pnorm);
  result:=vec[trunc64(ind)];
end;

function CameraScreenDeltaToVectorXY(camera,dx,dy,ratio,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).ScreenDeltaToVectorXY(trunc64(dx),trunc64(dy),ratio);
  result:=vec[trunc64(ind)];
end;

function CameraScreenDeltaToVectorXZ(camera,dx,dy,ratio,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).ScreenDeltaToVectorXZ(trunc64(dx),trunc64(dy),ratio);
  result:=vec[trunc64(ind)];
end;

function CameraScreenDeltaToVectorYZ(camera,dx,dy,ratio,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).ScreenDeltaToVectorYZ(trunc64(dx),trunc64(dy),ratio);
  result:=vec[trunc64(ind)];
end;

function CameraAbsoluteEyeSpaceVector(camera,fordist,rightdist,updist,ind: real): real; stdcall;
var
  vec: TVector;
begin
  vec:=TGLCamera(trunc64(camera)).AbsoluteEyeSpaceVector(fordist,rightdist,updist);
  result:=vec[trunc64(ind)];
end;

function CameraSetAutoLeveling(camera,factor: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).AutoLeveling(factor);
  result:=1;
end;

function CameraMoveInEyeSpace(camera,fordist,rightdist,updist: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).MoveInEyeSpace(fordist,rightdist,updist);
  result:=1;
end;

function CameraMoveTargetInEyeSpace(camera,fordist,rightdist,updist: real): real; stdcall;
begin
  TGLCamera(trunc64(camera)).MoveTargetInEyeSpace(fordist,rightdist,updist);
  result:=1;
end;

function CameraPointInFront(camera,x,y,z: real): real; stdcall;
var
  vec: TVector;
  pif: Boolean;
begin
  SetVector(vec,x,y,z);
  pif:=TGLCamera(trunc64(camera)).PointInFront(vec);
  result:=Integer(pif);
end;

function CameraGetFieldOfView(camera,vpdim: real): real; stdcall;
var
  fov: real;
begin
  fov:=TGLCamera(trunc64(camera)).GetFieldOfView(vpdim);
  result:=fov;
end;

