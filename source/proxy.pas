function ProxyObjectCreate(target, parent: real): real; stdcall;
var
  pr: TGLProxyObject;
begin
  if not (parent=0) then
    pr := TGLProxyObject.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    pr := TGLProxyObject.CreateAsChild(scene.Objects);
  pr.MasterObject := TGLBaseSceneObject(trunc64(target));
  pr.ProxyOptions := [pooEffects, pooObjects];
  result := Integer(pr);
end;

function ProxyObjectSetOptions(proxy,
  copyeffects, copyobjects, copytransform: real): real; stdcall;
var
  pr: TGLProxyObject;
begin
  pr := TGLProxyObject(trunc64(proxy));
  pr.ProxyOptions := [];
  if copyeffects = 1 then pr.ProxyOptions := pr.ProxyOptions + [pooEffects];
  if copyobjects = 1 then pr.ProxyOptions := pr.ProxyOptions + [pooObjects];
  if copytransform = 1 then pr.ProxyOptions := pr.ProxyOptions + [pooTransformation];
  result := 1.0;
end;

function ProxyObjectSetTarget(proxy, target: real): real; stdcall;
var
  pr: TGLProxyObject;
begin
  pr := TGLProxyObject(trunc64(proxy));
  pr.MasterObject := TGLBaseSceneObject(trunc64(target));
  result := 1.0;
end;
