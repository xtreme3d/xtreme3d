function SkydomeCreate(slices, stacks, parent: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  if not (parent=0) then
    sd := TGLEarthSkyDome.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    sd := TGLEarthSkyDome.CreateAsChild(scene.Objects);
  sd.Slices := trunc(slices);
  sd.Stacks := trunc(stacks);
  result := Integer(sd);
end;

function SkydomeSetOptions(skydome, fade, rotate: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  if (fade = 1) and (rotate = 1) then
    sd.ExtendedOptions := [esoFadeStarsWithSun, esoRotateOnTwelveHours]
  else if (fade = 1) and (rotate = 0) then
    sd.ExtendedOptions := [esoFadeStarsWithSun]
  else if (fade = 0) and (rotate = 1) then
    sd.ExtendedOptions := [esoRotateOnTwelveHours]
  else
    sd.ExtendedOptions := [];
  result := 1.0;
end;

function SkydomeSetDeepColor(skydome, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.DeepColor.AsWinColor := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeSetHazeColor(skydome, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.HazeColor.AsWinColor := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeSetNightColor(skydome, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.NightColor.AsWinColor := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeSetSkyColor(skydome, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.SkyColor.AsWinColor := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeSetSunDawnColor(skydome, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.SunDawnColor.AsWinColor := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeSetSunZenithColor(skydome, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.SunZenithColor.AsWinColor := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeSetSunElevation(skydome, angle: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.SunElevation := angle;
  result := 1.0;
end;

function SkydomeSetTurbidity(skydome, turbidity: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.Turbidity := turbidity;
  result := 1.0;
end;

function SkydomeAddRandomStars(skydome, stars, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.Stars.AddRandomStars(trunc(stars), TColor(trunc(color)));
  result := 1.0;
end;

// Adds a single star to the skydome
// rightAscension, declination - coordinates of a star in equatorial coordinate system
// magnitude - brightness of a star
function SkydomeAddStar(skydome, rightAscension, declination, magnitude, color: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
  star: TGLSkyDomeStar;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  star := sd.Stars.Add;
  star.RA := rightAscension;
  star.Dec := declination;
  star.Magnitude := magnitude;
  star.Color := TColor(trunc(color));
  result := 1.0;
end;

function SkydomeClearStars(skydome: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  sd.Stars.Clear;
  result := 1.0;
end;

function SkydomeTwinkleStars(skydome, mode: real): real; cdecl;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(RealToPtr(skydome));
  if mode = 1 then
    sd.Options := [sdoTwinkle]
  else
    sd.Options := [];
  result := 1.0;
end;
