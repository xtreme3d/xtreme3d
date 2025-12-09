function LensflareCreate(parent: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  if not (parent=0) then
    lf := TGLLensFlare.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    lf := TGLLensFlare.CreateAsChild(scene.Objects);
  result := ObjToReal(lf);
end;

function LensflareSetSize(lensflare, size: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.Size := trunc(size);
  result := 1.0;
end;

function LensflareSetSeed(lensflare, seed: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.Seed := trunc(seed);
  result := 1.0;
end;

function LensflareSetSqueeze(lensflare, squeeze: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.Squeeze := squeeze;
  result := 1.0;
end;

function LensflareSetStreaks(lensflare, streaks: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.NumStreaks := trunc(streaks);
  result := 1.0;
end;

function LensflareSetStreakWidth(lensflare, width: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.StreakWidth := width;
  result := 1.0;
end;

function LensflareSetSecs(lensflare, secs: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.NumSecs := trunc(secs);
  result := 1.0;
end;

function LensflareSetResolution(lensflare, res: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.Resolution := trunc(res);
  result := 1.0;
end;

function LensflareSetElements(lensflare, glow, ring, streaks, rays, secs: real): real; cdecl;
var
  lf: TGLLensFlare;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  lf.Elements := [];
  if glow = 1    then lf.Elements := lf.Elements + [feGlow];
  if ring = 1    then lf.Elements := lf.Elements + [feRing];
  if streaks = 1 then lf.Elements := lf.Elements + [feStreaks];
  if rays = 1    then lf.Elements := lf.Elements + [feRays];
  if secs = 1    then lf.Elements := lf.Elements + [feSecondaries];
  result := 1.0;
end;

function LensflareSetGradients(lensflare, ind, color1, alpha1, color2, alpha2 : real): real; cdecl;
var
  lf: TGLLensFlare;
  gradient: TGLFlareGradient;
begin
  lf := TGLLensFlare(RealToPtr(lensflare));
  gradient := lf.GlowGradient;
  if ind = 0 then gradient := lf.GlowGradient;
  if ind = 1 then gradient := lf.RingGradient;
  if ind = 2 then gradient := lf.StreaksGradient;
  if ind = 3 then gradient := lf.RaysGradient;
  if ind = 4 then gradient := lf.SecondariesGradient;
  gradient.FromColor.AsWinColor := TColor(trunc(color1));
  gradient.FromColor.Alpha := alpha1;
  gradient.ToColor.AsWinColor := TColor(trunc(color2));
  gradient.ToColor.Alpha := alpha2;
  result := 1.0;
end;
