function CameraCreate(parent: real): real; cdecl;
var
    camera: TGLCamera;
begin
    if not (parent = 0) then
        camera := TGLCamera.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
        camera := TGLCamera.CreateAsChild(scene.Objects);
    result := ObjToReal(camera);
end;

function CameraSetStyle(camera,cs: real): real; cdecl;
begin
  if Trunc(cs)=0 then TGLCamera(RealToPtr(camera)).CameraStyle:=csPerspective;
  if Trunc(cs)=1 then TGLCamera(RealToPtr(camera)).CameraStyle:=csOrthogonal;
  if Trunc(cs)=2 then TGLCamera(RealToPtr(camera)).CameraStyle:=csOrtho2D;
  if Trunc(cs)=3 then TGLCamera(RealToPtr(camera)).CameraStyle:=csInfinitePerspective;
  result:=1;
end;

function CameraSetFocal(camera,fov: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).FocalLength:=fov;
  result:=1;
end;

function CameraSetSceneScale(camera,scale: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).SceneScale:=scale;
  result:=1;
end;

function CameraScaleScene(camera,scale: real): real; cdecl;
var
  current: real;
begin
  current:=TGLCamera(RealToPtr(camera)).SceneScale;
  TGLCamera(RealToPtr(camera)).SceneScale:=current+scale;
  result:=1;
end;

function CameraSetViewDepth(camera,depth: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).DepthOfView:=depth;
  result:=1;
end;

function CameraSetTargetObject(camera,obj: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).TargetObject:=TGLBaseSceneObject(RealToPtr(obj));
  result:=1;
end;

function CameraMoveAroundTarget(camera,pitch,turn: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).MoveAroundTarget(pitch,turn);
  result:=1;
end;

function CameraSetDistanceToTarget(camera,distance: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).AdjustDistanceToTarget(distance);
  result:=1;
end;

function CameraGetDistanceToTarget(camera: real): real; cdecl;
var
  dist:real;
begin
  dist:=TGLCamera(RealToPtr(camera)).DistanceToTarget;
  result:=dist;
end;

function CameraCopyToTexture(camera: real; mtrl: PAnsiChar; width,height: real): real; cdecl;
var
  mat: TGLLibMaterial;
begin
  memviewer.Width:=Trunc(width);
  memviewer.Height:=Trunc(height);
  memviewer.Camera:=TGLCamera(RealToPtr(camera));
  mat:=matlib.Materials.GetLibMaterialByName(String(AnsiString(mtrl)));
  mat.Material.Texture.Disabled:=false;
  memviewer.Render;
  memviewer.CopyToTexture(mat.Material.Texture);
  result:=1;
end;

function CameraGetNearPlane(camera: real): real; cdecl;
var
  npl: real;
begin
  npl:=TGLCamera(RealToPtr(camera)).NearPlane;
  result:=npl;
end;

function CameraSetNearPlaneBias(camera,bias: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).NearPlaneBias:=bias;
  result:=1;
end;

function CameraAbsoluteVectorToTarget(camera,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).AbsoluteVectorToTarget;
  result:=vec.V[Trunc(ind)];
end;

function CameraAbsoluteRightVectorToTarget(camera,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).AbsoluteRightVectorToTarget;
  result:=vec.V[Trunc(ind)];
end;

function CameraAbsoluteUpVectorToTarget(camera,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).AbsoluteUpVectorToTarget;
  result:=vec.V[Trunc(ind)];
end;

function CameraZoomAll(camera, viewer: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).ZoomAll(TGLSceneViewer(RealToPtr(viewer)).Buffer);
  result:=1;
end;

function CameraScreenDeltaToVector(camera,dx,dy,ratio,nx,ny,nz,ind: real): real; cdecl;
var
  vec,pnorm: TGLVector;
begin
  pnorm := VectorMake(nx, ny, nz);
  vec:=TGLCamera(RealToPtr(camera)).ScreenDeltaToVector(Trunc(dx),Trunc(dy),ratio,pnorm);
  result:=vec.V[Trunc(ind)];
end;

function CameraScreenDeltaToVectorXY(camera,dx,dy,ratio,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).ScreenDeltaToVectorXY(Trunc(dx),Trunc(dy),ratio);
  result:=vec.V[Trunc(ind)];
end;

function CameraScreenDeltaToVectorXZ(camera,dx,dy,ratio,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).ScreenDeltaToVectorXZ(Trunc(dx),Trunc(dy),ratio);
  result:=vec.V[Trunc(ind)];
end;

function CameraScreenDeltaToVectorYZ(camera,dx,dy,ratio,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).ScreenDeltaToVectorYZ(Trunc(dx),Trunc(dy),ratio);
  result:=vec.V[Trunc(ind)];
end;

function CameraAbsoluteEyeSpaceVector(camera,fordist,rightdist,updist,ind: real): real; cdecl;
var
  vec: TGLVector;
begin
  vec:=TGLCamera(RealToPtr(camera)).AbsoluteEyeSpaceVector(fordist,rightdist,updist);
  result:=vec.V[Trunc(ind)];
end;

function CameraSetAutoLeveling(camera,factor: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).AutoLeveling(factor);
  result:=1;
end;

function CameraMoveInEyeSpace(camera,fordist,rightdist,updist: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).MoveInEyeSpace(fordist,rightdist,updist);
  result:=1;
end;

function CameraMoveTargetInEyeSpace(camera,fordist,rightdist,updist: real): real; cdecl;
begin
  TGLCamera(RealToPtr(camera)).MoveTargetInEyeSpace(fordist,rightdist,updist);
  result:=1;
end;

function CameraPointInFront(camera,x,y,z: real): real; cdecl;
var
  vec: TGLVector;
  pif: Boolean;
begin
  SetVector(vec,x,y,z);
  pif:=TGLCamera(RealToPtr(camera)).PointInFront(vec);
  result:=Integer(pif);
end;

function CameraGetFieldOfView(camera,vpdim: real): real; cdecl;
var
  fov: real;
begin
  fov:=TGLCamera(RealToPtr(camera)).GetFieldOfView(vpdim);
  result:=fov;
end;
