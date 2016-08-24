function FireFXManagerCreate: real; stdcall;
var
  m: TGLFireFXManager;
begin
  m := TGLFireFXManager.Create(scene);
  m.Disabled:=false;
  m.Cadencer:=cadencer;
  m.MaxParticles := 96;
  m.ParticleSize := 0.69;
  m.FireDensity := 0.5;
  m.FireEvaporation := 1.0;
  m.FireBurst := 1;
  m.FireRadius := 0.5;
  m.InnerColor.Alpha := 1.0;
  result:=Integer(m);
end;

function FireFXCreate(mngr, obj: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
  ffx: TGLBFireFX;
  objct: TGLSceneObject;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  objct := TGLSceneObject(trunc64(obj));
  ffx := GetOrCreateFireFX(objct.Effects);
  ffx.Manager := ffxMngr;
  ffxMngr.Disabled:=false;
  result := 1.0;
end;

function FireFXSetColor(mngr, incolor, inalpha, outcolor, outalpha: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.InnerColor.AsWinColor := TColor(trunc64(incolor));
  ffxMngr.InnerColor.Alpha := inalpha;
  ffxMngr.OuterColor.AsWinColor := TColor(trunc64(outcolor));
  ffxMngr.OuterColor.Alpha := outalpha;
  result := 1.0;
end;

function FireFXSetMaxParticles(mngr, particles: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.MaxParticles := trunc64(particles);
  result := 1.0;
end;

function FireFXSetParticleSize(mngr, size: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.ParticleSize := size;
  result := 1.0;
end;

function FireFXSetDensity(mngr, density: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.FireDensity := density;
  result := 1.0;
end;

function FireFXSetEvaporation(mngr, evaporation: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.FireEvaporation := evaporation;
  result := 1.0;
end;

function FireFXSetCrown(mngr, crown: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.FireCrown := crown;
  result := 1.0;
end;

function FireFXSetLife(mngr, life: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.ParticleLife := trunc64(life);
  result := 1.0;
end;

function FireFXSetBurst(mngr, burst: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.FireBurst := burst;
  result := 1.0;
end;

function FireFXSetRadius(mngr, radius: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.FireRadius := radius;
  result := 1.0;
end;

function FireFXExplosion(mngr, isp, maxsp, lbf: real): real; stdcall;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(trunc64(mngr));
  ffxMngr.IsotropicExplosion(isp, maxsp, lbf);
  result := 1.0;
end;
