function LightFXCreate(obj: real): real; stdcall;
var
  ffx: TGLLightFX;
  objct: TGLBaseSceneObject;
begin
  objct := TGLBaseSceneObject(trunc64(obj));
  ffx := GetOrCreateLightFX(objct);
  result := Integer(ffx);
end;
