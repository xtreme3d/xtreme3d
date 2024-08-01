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

function ShadowMapSetCamera(sm, sc: real): real; cdecl;
var
    shadowMap: TGLShadowMap;
begin
    shadowMap := TGLShadowMap(RealToPtr(sm));
    shadowMap.ShadowCamera := TGLShadowCamera(RealToPtr(sc));
    result := 1;
end;

function ShadowMapSetViewer(sm, v: real): real; cdecl;
var
    shadowMap: TGLShadowMap;
begin
    shadowMap := TGLShadowMap(RealToPtr(sm));
    shadowMap.Viewer := TGLSceneViewer(RealToPtr(v));
    result := 1;
end;

function ShadowMapSetFBO(sm, fbo: real): real; cdecl;
var
    shadowMap: TGLShadowMap;
begin
    shadowMap := TGLShadowMap(RealToPtr(sm));
    shadowMap.FBO := TGLFBORendererEx(RealToPtr(fbo));
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

function ShadowCameraSetProjectionSize(sc, size: real): real; cdecl;
var
    shadowCamera: TGLShadowCamera;
begin
    shadowCamera := TGLShadowCamera(RealToPtr(sc));
    shadowCamera.ProjectionSize := size;
    result := 1.0;
end;

function ShadowCameraSetZClippingPlanes(sc, znear, zfar: real): real; cdecl;
var
    shadowCamera: TGLShadowCamera;
begin
    shadowCamera := TGLShadowCamera(RealToPtr(sc));
    shadowCamera.ZNear := znear;
    shadowCamera.ZFar := zfar;
    result := 1.0;
end;


