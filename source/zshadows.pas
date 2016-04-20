function ZShadowsCreate(view,cast,w,h: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows.Create(scene);
  TGLScene(scene).Objects.AddChild(zsh);
  //zsh:=TGLZShadows.CreateAsChild(scene.Objects);
  zsh.Viewer:=TGLSceneViewer(trunc64(view));
  zsh.Caster:=TGLMemoryViewer(trunc64(cast));
  zsh.Width:=trunc64(w);
  zsh.Height:=trunc64(h);
  zsh.FrustShadow := True;
  zsh.SkyShadow := False;
  zsh.Color.SetColor(0.0, 0.0, 0.0, 1.0);
  zsh.Optimise:=opNone;//op16in1;
  result:=integer(zsh);
end;

function ZShadowsCast(zshadow: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.CastShadow;
  result:=1;
end;

function ZShadowsSetFrustShadow(zshadow,mode: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.FrustShadow:=boolean(trunc64(mode));
  result:=1;
end;

function ZShadowsSetSkyShadow(zshadow,mode: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.SkyShadow:=boolean(trunc64(mode));
  result:=1;
end;

function ZShadowsSetColor(zshadow,color,alph: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.Color.AsWinColor:=trunc64(color);
  zsh.Color.Alpha:=alph;
  result:=1;
end;

function ZShadowsSetSoft(zshadow,mode: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.Soft:=boolean(trunc64(mode));
  result:=1;
end;

function ZShadowsSetTolerance(zshadow,tol: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.Tolerance:=tol;
  result:=1;
end;

function ZShadowsSetDepthFade(zshadow,mode: real): real; stdcall;
var
  zsh: TGLZShadows;
begin
  zsh:=TGLZShadows(trunc64(zshadow));
  zsh.DepthFade:=boolean(trunc64(mode));
  result:=1;
end;