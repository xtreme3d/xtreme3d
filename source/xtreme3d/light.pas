function LightCreate(ls,parent: real): real; cdecl;
var
  GLLightSource1: TGLLightSource;
begin
  if not (parent=0) then
    GLLightSource1:=TGLLightSource.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLLightSource1:=TGLLightSource.CreateAsChild(scene.Objects);
  if ls=0 then GLLightSource1.LightStyle:=lsSpot;
  if ls=1 then GLLightSource1.LightStyle:=lsOmni;
  if ls=2 then GLLightSource1.LightStyle:=lsParallel;
  GLLightSource1.Shining := true;
  result:=Integer(GLLightSource1);
end;

function LightSetAmbientColor(light,color: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).Ambient.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function LightSetDiffuseColor(light,color: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).Diffuse.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function LightSetSpecularColor(light,color: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).Specular.AsWinColor:=TColor(trunc64(color));
  result:=1;
end;

function LightSetAttenuation(light,aconst,alinear,aquadratic: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).ConstAttenuation:=aconst;
  TGLLightSource(trunc64(light)).LinearAttenuation:=alinear;
  TGLLightSource(trunc64(light)).QuadraticAttenuation:=aquadratic;
  result:=1;
end;

function LightSetShining(light,mode: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).Shining:=boolean(trunc64(mode));
  result:=1;
end;

function LightSetSpotCutoff(light,cutoff: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).SpotCutOff:=cutoff;
  result:=1;
end;

function LightSetSpotExponent(light,exp: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).SpotExponent:=exp;
  result:=1;
end;

function LightSetSpotDirection(light,x,y,z: real): real; cdecl
begin
  TGLLightSource(trunc64(light)).SpotDirection.SetVector(x,y,z);
  result:=1;
end;

function LightSetStyle(light,ls: real): real; cdecl
begin
  if ls=0 then TGLLightSource(trunc64(light)).LightStyle:=lsSpot;
  if ls=1 then TGLLightSource(trunc64(light)).LightStyle:=lsOmni;
  if ls=2 then TGLLightSource(trunc64(light)).LightStyle:=lsParallel;
  result:=1;
end;

function LightGetColor(light,index: real): real; cdecl
var 
lght: TGLLightSource;
begin
lght:= TGLLightSource(trunc64(light));
if (index=0) then
  result:=lght.Ambient.AsWinColor;
if (index=1) then
  result:=lght.Diffuse.AsWinColor;
if (index=2) then
  result:=lght.Specular.AsWinColor;
end;

function LightGetAttenuation(light,index: real): real; cdecl
begin
if (index=0) then
    result:=TGLLightSource(trunc64(light)).ConstAttenuation;
if (index=1) then
    result:=TGLLightSource(trunc64(light)).LinearAttenuation;
if (index=2) then
    result:=TGLLightSource(trunc64(light)).QuadraticAttenuation;	
end;

function LightGetShining(light: real): real; cdecl
begin
  result:=integer(TGLLightSource(trunc64(light)).Shining);
end;
