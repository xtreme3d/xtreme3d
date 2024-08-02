function FireFXManagerCreate: real; cdecl;
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
  result:=ObjToReal(m);
end;

function FireFXCreate(mngr, obj: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
  ffx: TGLBFireFX;
  objct: TGLSceneObject;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  objct := TGLSceneObject(RealToPtr(obj));
  ffx := GetOrCreateFireFX(objct.Effects);
  ffx.Manager := ffxMngr;
  ffxMngr.Disabled:=false;
  result := 1.0;
end;

function FireFXSetColor(mngr, incolor, inalpha, outcolor, outalpha: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.InnerColor.AsWinColor := TColor(trunc(incolor));
  ffxMngr.InnerColor.Alpha := inalpha;
  ffxMngr.OuterColor.AsWinColor := TColor(trunc(outcolor));
  ffxMngr.OuterColor.Alpha := outalpha;
  result := 1.0;
end;

function FireFXSetMaxParticles(mngr, particles: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.MaxParticles := trunc(particles);
  result := 1.0;
end;

function FireFXSetParticleSize(mngr, size: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.ParticleSize := size;
  result := 1.0;
end;

function FireFXSetDensity(mngr, density: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.FireDensity := density;
  result := 1.0;
end;

function FireFXSetEvaporation(mngr, evaporation: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.FireEvaporation := evaporation;
  result := 1.0;
end;

function FireFXSetCrown(mngr, crown: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.FireCrown := crown;
  result := 1.0;
end;

function FireFXSetLife(mngr, life: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.ParticleLife := trunc(life);
  result := 1.0;
end;

function FireFXSetBurst(mngr, burst: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.FireBurst := burst;
  result := 1.0;
end;

function FireFXSetRadius(mngr, radius: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.FireRadius := radius;
  result := 1.0;
end;

function FireFXExplosion(mngr, isp, maxsp, lbf: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.IsotropicExplosion(isp, maxsp, lbf);
  result := 1.0;
end;

function FireFXRingExplosion(mngr, isp, maxsp, lbf,rx,ry,rz,sx,sy,sz: real): real; cdecl;
var
  ffxMngr: TGLFireFXManager;
begin
  ffxMngr := TGLFireFXManager(RealToPtr(mngr));
  ffxMngr.RingExplosion(isp, maxsp, lbf,AffineVectorMake(rx,ry,rz),AffineVectorMake(sx,sy,sz));
  result := 1.0;
end;

