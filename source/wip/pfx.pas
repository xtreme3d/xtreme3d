function PFXRendererCreate(parent: real): real; stdcall;
var
  pfxr: TGLParticleFXRenderer;
begin
  if not (parent=0) then
    pfxr := TGLParticleFXRenderer.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    pfxr := TGLParticleFXRenderer.CreateAsChild(scene.Objects);
  pfxr.Visible := true;
  result := Integer(pfxr);
end;

function PFXManagerCreate(renderer: real): real; stdcall;
var
  pfxm: TGLPointLightPFXManager;
  pfxc: TPFXLifeColor;
begin
  pfxm := TGLPointLightPFXManager.Create(scene);
  pfxm.Renderer := TGLParticleFXRenderer(trunc64(renderer));
  pfxm.Cadencer := cadencer;
  pfxm.ParticleSize := 1.0;
  pfxc := pfxm.LifeColors.Add;
  pfxc.ColorInner.SetColor(1,1,1,1);
  pfxc.ColorOuter.SetColor(1,1,1,1);
  pfxc.LifeTime := 10;
  result := Integer(pfxm);
end;

function PFXSourceCreate(manager, obj: real): real; stdcall;
var
  pfxm: TGLPointLightPFXManager;
  objct: TGLSceneObject;
  spsx: TGLSourcePFXEffect;
begin
  pfxm := TGLPointLightPFXManager(trunc64(manager));
  objct := TGLSceneObject(trunc64(obj));
  spsx := GetOrCreateSourcePFX(objct);
  spsx.Enabled := true;
  spsx.EffectScale := 1.0;
  spsx.DisabledIfOwnerInvisible := false;
  spsx.ParticleInterval := 1;
  result := Integer(spsx);
end;

function PFXSourceBurst(src: real): real; stdcall;
var
  spsx: TGLSourcePFXEffect;
begin
  spsx := TGLSourcePFXEffect(trunc64(src));
  spsx.Burst(cadencer.CurrentTime, 10);
  result := 1.0;
end;

