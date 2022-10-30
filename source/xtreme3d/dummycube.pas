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
