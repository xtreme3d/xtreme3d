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
