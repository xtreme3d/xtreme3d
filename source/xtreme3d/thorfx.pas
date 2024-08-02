function ThorFXManagerCreate: real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX := TGLThorFXManager.Create(scene);
  GLTFX.Disabled := false;
  GLTFX.Cadencer := cadencer;
  result := ObjToReal(GLTFX);
end;

function ThorFXSetColor(fx,incolor,inalpha,outcolor,outalpha,ccolor,calpha: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.InnerColor.AsWinColor:=trunc(incolor);
  GLTFX.InnerColor.Alpha:=inalpha;
  GLTFX.OuterColor.AsWinColor:=trunc(outcolor);
  GLTFX.OuterColor.Alpha:=outalpha;
  GLTFX.CoreColor.AsWinColor:=trunc(ccolor);
  GLTFX.CoreColor.Alpha:=calpha;
  result:=1;
end;

function ThorFXEnableCore(fx,mode: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.Core:=boolean(trunc(mode));
  result:=1;
end;

function ThorFXEnableGlow(fx,mode: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.Glow:=boolean(trunc(mode));
  result:=1;
end;

function ThorFXSetMaxParticles(fx,maxp: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.Maxpoints:=trunc(maxp);
  result:=1;
end;

function ThorFXSetGlowSize(fx,size: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.GlowSize:=size;
  result:=1;
end;

function ThorFXSetVibrate(fx,vibr: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.Vibrate:=vibr;
  result:=1;
end;

function ThorFXSetWildness(fx,wild: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.Wildness:=wild;
  result:=1;
end;

function ThorFXSetTarget(fx,x,y,z: real): real; cdecl;
var
  GLTFX: TGLThorFXManager;
begin
  GLTFX:=TGLThorFXManager(RealToPtr(fx));
  GLTFX.Target.X := x;
  GLTFX.Target.Y := y;
  GLTFX.Target.Z := z;
  result:=1;
end;

function ThorFXCreate(fx, obj: real): real; cdecl;
var
  tfxMngr: TGLThorFXManager;
  objct: TGLBaseSceneObject;
  tfx: TGLBThorFX;
begin
  tfxMngr := TGLThorFXManager(RealToPtr(fx));
  objct := TGLSceneObject(RealToPtr(obj));
  tfx := GetOrCreateThorFX(objct);
  tfx.Manager := tfxMngr;
  result := 1.0;
end;
