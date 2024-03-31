function ShadowMapCreate(fbo, viewer, shadowCamera: real): real; cdecl;
var
  sm: TGLShadowMap;
begin
  sm := TGLShadowMap.Create(scene, TGLFBORendererEx(RealToPtr(fbo)), TGLSceneViewer(RealToPtr(viewer)), TGLShadowCamera(RealToPtr(shadowCamera)));
  result := PtrToReal(sm);
end;

function ShadowMapUpdate(shadowMap: real): real; cdecl;
var
  sm: TGLShadowMap;
begin
  sm := TGLShadowMap(RealToPtr(shadowMap));
  sm.Update();
  result := 1;
end;

function ShadowCameraCreate(parent: real): real; cdecl;
var
    shadowCamera: TGLShadowCamera;
begin
    if not (parent = 0) then
        shadowCamera := TGLShadowCamera.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
        shadowCamera := TGLShadowCamera.CreateAsChild(scene.Objects);
    result := ObjToReal(shadowCamera);
end;