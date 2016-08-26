function ThorFXManagerCreate: real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager.Create(scene);
  GLTFX.Disabled:=false;
  GLTFX.Cadencer:=cadencer;
  result:=Integer(GLTFX);
end;

function ThorFXSetColor(fx,incolor,inalpha,outcolor,outalpha,ccolor,calpha: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.InnerColor.AsWinColor:=trunc64(incolor);
  GLTFX.InnerColor.Alpha:=inalpha;
  GLTFX.OuterColor.AsWinColor:=trunc64(outcolor);
  GLTFX.OuterColor.Alpha:=outalpha;
  GLTFX.CoreColor.AsWinColor:=trunc64(ccolor);
  GLTFX.CoreColor.Alpha:=calpha;
  result:=1;
end;

function ThorFXEnableCore(fx,mode: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.Core:=boolean(trunc64(mode));
  result:=1;
end;

function ThorFXEnableGlow(fx,mode: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.Glow:=boolean(trunc64(mode));
  result:=1;
end;

function ThorFXSetMaxParticles(fx,maxp: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.Maxpoints:=trunc64(maxp);
  result:=1;
end;

function ThorFXSetGlowSize(fx,size: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.GlowSize:=size;
  result:=1;
end;

function ThorFXSetVibrate(fx,vibr: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.Vibrate:=vibr;
  result:=1;
end;

function ThorFXSetWildness(fx,wild: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.Wildness:=wild;
  result:=1;
end;

function ThorFXSetTarget(fx,x,y,z: real): real; stdcall;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(trunc64(fx));
  GLTFX.Target.SetVector(x,y,z);
  result:=1;
end;

function ThorFXCreate(fx, obj: real): real; stdcall;
var
  tfxMngr: TGLThorFXManager;
  objct: TGLBaseSceneObject;
  tfx: TGLBThorFX;
begin
  tfxMngr := TGLThorFXManager(trunc64(fx));
  objct := TGLSceneObject(trunc64(obj));
  tfx := GetOrCreateThorFX(objct);
  tfx.Manager := tfxMngr;
  result := 1.0;
end;
