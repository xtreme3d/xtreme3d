function CubeCreate(w, h, d, parent: real): real; cdecl;
var
    cube: TGLCube;
begin
    if not (parent=0) then
        cube:=TGLCube.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
    else
        cube:=TGLCube.CreateAsChild(scene.Objects);
    cube.CubeWidth:=w;
    cube.CubeHeight:=h;
    cube.CubeDepth:=d;
    result:=ObjToReal(cube);
end;
