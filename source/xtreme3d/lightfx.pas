function LightFXCreate(obj: real): real; cdecl;
var
  fx: TGLLightFX;
  objct: TGLBaseSceneObject;
begin
  objct := TGLBaseSceneObject(RealToPtr(obj));
  fx := GetOrCreateLightFX(objct);
  result := PtrToReal(fx);
end;

