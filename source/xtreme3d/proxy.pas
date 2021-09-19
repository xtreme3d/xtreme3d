function ProxyObjectCreate(target, parent: real): real; cdecl;
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
  copyeffects, copyobjects, copytransform: real): real; cdecl;
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

function ProxyObjectSetTarget(proxy, target: real): real; cdecl;
var
  pr: TGLProxyObject;
begin
  pr := TGLProxyObject(trunc64(proxy));
  pr.MasterObject := TGLBaseSceneObject(trunc64(target));
  result := 1.0;
end;

function MultiProxyObjectCreate(parent: real): real; cdecl;
var
  mp: TGLMultiProxy;
begin
  if not (parent=0) then
    mp := TGLMultiProxy.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    mp := TGLMultiProxy.CreateAsChild(scene.Objects);
  result := Integer(mp);
end;

function MultiProxyObjectAddTarget(mproxy, target, mindist, maxdist: real): real; cdecl;
var
  mp: TGLMultiProxy;
  ob: TGLBaseSceneObject;
begin
  mp := TGLMultiProxy(trunc64(mproxy));
  ob := TGLBaseSceneObject(trunc64(target));
  mp.MasterObjects.Add(ob, mindist, maxdist);
  result := 1.0;
end;

function ActorProxyObjectCreate(actor, parent: real): real; cdecl;
var
  p: TGLActorProxy;
begin
  if not (parent = 0) then
    p := TGLActorProxy.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    p := TGLActorProxy.CreateAsChild(scene.Objects);
  p.MasterObject := TGLActor(trunc64(actor));
  result := Integer(p);
end;

function ActorProxyObjectSwitchToAnimation(proxy, anim: real): real; cdecl;
var
  p: TGLActorProxy;
begin
  p := TGLActorProxy(trunc64(proxy));
  p.SwitchToAnimation(trunc64(anim));
  result := 1.0;
end;

function ActorProxyObjectSetAnimationRange(proxy, startf, endf: real): real; cdecl;
var
  p: TGLActorProxy;
begin
  p := TGLActorProxy(trunc64(proxy));
  p.SetAnimationRange(trunc64(startf), trunc64(endf));
  result := 1.0;
end;

function ActorProxyObjectSetInterval(proxy, interval: real): real; cdecl;
var
  p: TGLActorProxy;
begin
  p := TGLActorProxy(trunc64(proxy));
  p.Interval := trunc64(interval);
  result := 1.0;
end;
