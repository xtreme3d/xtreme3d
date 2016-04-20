function SkydomeCreate(slices, stacks, parent: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  if not (parent=0) then
    sd := TGLEarthSkyDome.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    sd := TGLEarthSkyDome.CreateAsChild(scene.Objects);
  sd.Slices := trunc64(slices);
  sd.Stacks := trunc64(stacks);
  result := Integer(sd);
end;

function SkydomeSetOptions(skydome, fade, rotate: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
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

function SkydomeSetDeepColor(skydome, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.DeepColor.AsWinColor := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeSetHazeColor(skydome, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.HazeColor.AsWinColor := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeSetNightColor(skydome, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.NightColor.AsWinColor := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeSetSkyColor(skydome, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.SkyColor.AsWinColor := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeSetSunDawnColor(skydome, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.SunDawnColor.AsWinColor := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeSetSunZenithColor(skydome, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.SunZenithColor.AsWinColor := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeSetSunElevation(skydome, angle: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.SunElevation := angle;
  result := 1.0;
end;

function SkydomeSetTurbidity(skydome, turbidity: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.Turbidity := turbidity;
  result := 1.0;
end;

function SkydomeAddRandomStars(skydome, stars, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.Stars.AddRandomStars(trunc64(stars), TColor(trunc64(color)));
  result := 1.0;
end;

// Adds a single star to the skydome
// rightAscension, declination - coordinates of a star in equatorial coordinate system
// magnitude - brightness of a star
function SkydomeAddStar(skydome, rightAscension, declination, magnitude, color: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
  star: TSkyDomeStar;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  star := sd.Stars.Add;
  star.RA := rightAscension;
  star.Dec := declination;
  star.Magnitude := magnitude;
  star.Color := TColor(trunc64(color));
  result := 1.0;
end;

function SkydomeClearStars(skydome: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  sd.Stars.Clear;
  result := 1.0;
end;

function SkydomeTwinkleStars(skydome, mode: real): real; stdcall;
var
  sd: TGLEarthSkyDome;
begin
  sd := TGLEarthSkyDome(trunc64(skydome));
  if mode = 1 then
    sd.Options := [sdoTwinkle]
  else
    sd.Options := [];
  result := 1.0;
end;
