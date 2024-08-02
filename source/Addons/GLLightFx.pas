unit GLLightFx;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  Math,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.RenderContextInfo,
  GLS.Scene,
  GLS.XCollection,
  GLS.PersistentClasses;

type

  TGLLightFX = class(TGLObjectPreEffect)
    private
        { Private Declarations }
    protected
        procedure WriteToFiler(writer: TWriter); override;
        procedure ReadFromFiler(reader: TReader); override;
        procedure Loaded; override;
    public
        constructor Create(aOwner: TXCollection); override;
        destructor Destroy; override;
        class function FriendlyName: String; override;
			  class function FriendlyDescription: String; override;
        function CompareLights(item1, item2: TObject): Boolean;
        procedure SortLights(list: TGLPersistentObjectList);
        procedure Render(var rci: TGLRenderContextInfo); override;
  end;

function GetOrCreateLightFX(effects: TGLEffects): TGLLightFX; overload;
function GetOrCreateLightFX(obj: TGLBaseSceneObject): TGLLightFX; overload;

implementation

function GetOrCreateLightFX(effects: TGLEffects): TGLLightFX;
var
	i: Integer;
begin
	i := effects.IndexOfClass(TGLLightFX);
	if i >= 0 then
		result := TGLLightFX(effects[i])
	else
    result := TGLLightFX.Create(effects);
end;

function GetOrCreateLightFX(obj: TGLBaseSceneObject): TGLLightFX;
begin
	result := GetOrCreateLightFX(obj.Effects);
end;

constructor TGLLightFX.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TGLLightFX.Destroy;
begin
  inherited Destroy;
end;

class function TGLLightFX.FriendlyName: String;
begin
   result := 'LightFx';
end;

class function TGLLightFX.FriendlyDescription: String;
begin
   result := 'Light FX';
end;

procedure TGLLightFX.WriteToFiler(writer: TWriter);
begin
end;

procedure TGLLightFX.ReadFromFiler(reader: TReader);
begin
end;

procedure TGLLightFX.Loaded;
begin
   inherited;
end;

function TGLLightFX.CompareLights(item1, item2: TObject): Boolean;
var
  light1, light2: TGLLightSource;
  d1, d2: Single;
  ownerObj: TGLBaseSceneObject;
  light1AbsPos, light2AbsPos, objAbsPos: TAffineVector;
begin
  light1 := TGLLightSource(item1);
  light2 := TGLLightSource(item2);
  if Assigned(light1) and Assigned(light2) then begin
    if light1.Shining and not light2.Shining then
      result := True
    else if light2.Shining and not light1.Shining then
      result := False
    else if light1.LightStyle = lsParallel then
      result := True
    else if light2.LightStyle = lsParallel then
      result := False
    else begin
      ownerObj := OwnerBaseSceneObject();

      light1AbsPos := AffineVectorMake(light1.AbsolutePosition);
      light2AbsPos := AffineVectorMake(light2.AbsolutePosition);
      objAbsPos := AffineVectorMake(ownerObj.AbsolutePosition);

      d1 := VectorDistance(objAbsPos, light1AbsPos);
      d2 := VectorDistance(objAbsPos, light2AbsPos);
      if d1 < d2 then
        result := True
      else
        result := False;
      Exit;
    end;
  end
  else if Assigned(light1) then
    result := True
  else if Assigned(light2) then
    result := False
  else
    result := False;
end;

procedure TGLLightFX.SortLights(list: TGLPersistentObjectList);
var
  i, j, k: Integer;
  tmp: TObject;
begin
  j := 0;
  for i := 0 to list.Count - 1 do begin
    j := i;
    k := i;
    while k < list.Count do begin
      if CompareLights(list[k], list[j]) then
        j := k;
      Inc(k);
    end;
    tmp := list[i];
    list[i] := list[j];
    list[j] := tmp;
  end;
end;

procedure TGLLightFX.Render(var rci: TGLRenderContextInfo);
var
  i: Integer;
  lightSource: TGLLightSource;
  numLights: Integer;
  ownerObj: TGLBaseSceneObject;
  lightId: Cardinal;
  lightAbsPos: TGLVector;
  mvp: TGLMatrix;
  lights: TGLPersistentObjectList;
begin
  ownerObj := OwnerBaseSceneObject();

  lights := ownerObj.Scene.Lights;

  if Assigned(lights) then begin
    numLights := Min(lights.Count, 8);
    if numLights > 1 then
      SortLights(lights)
  end
  else exit;

  glPushMatrix;
  // revert to the base model matrix
  mvp := ownerObj.Scene.CurrentBuffer.ViewMatrix;
  glLoadMatrixf(@mvp);

  for i := 0 to numLights - 1 do begin
    lightSource := TGLLightSource(lights[i]);
    if Assigned(lightSource) then begin
      if lightSource.Shining then begin
        glPushMatrix;

        lightId := GL_LIGHT0+i;
        glEnable(lightId);

        if lightSource.LightStyle = lsParallel then begin
          glMultMatrixf(PGLFloat(lightSource.AbsoluteMatrixAsAddress));
          glLightfv(lightId, GL_POSITION, PGLfloat(lightSource.SpotDirection.AsAddress))
        end else begin
          glMultMatrixf(PGLFloat(lightSource.Parent.AbsoluteMatrixAsAddress));
          glLightfv(lightId, GL_POSITION, PGLfloat(lightSource.Position.AsAddress));
        end;

        glLightfv(lightId, GL_AMBIENT, PGLfloat(lightSource.Ambient.AsAddress));
        glLightfv(lightId, GL_DIFFUSE, PGLfloat(lightSource.Diffuse.AsAddress));
        glLightfv(lightId, GL_SPECULAR, PGLfloat(lightSource.Specular.AsAddress));

        if lightSource.LightStyle = lsSpot then begin
          if lightSource.SpotCutOff <> 180 then begin
            glLightfv(lightId, GL_SPOT_DIRECTION, PGLfloat(lightSource.SpotDirection.AsAddress));
            glLightfv(lightId, GL_SPOT_EXPONENT, @lightSource.SpotExponent);
          end;
          glLightfv(lightId, GL_SPOT_CUTOFF, @lightSource.SpotCutOff);
        end else begin
          glLightf(lightId, GL_SPOT_CUTOFF, 180);
        end;

        glLightfv(lightId, GL_CONSTANT_ATTENUATION, @lightSource.ConstAttenuation);
        glLightfv(lightId, GL_LINEAR_ATTENUATION, @lightSource.LinearAttenuation);
        glLightfv(lightId, GL_QUADRATIC_ATTENUATION, @lightSource.QuadraticAttenuation);

        glPopMatrix;
      end;
    end;
  end;

  glPopMatrix;

  for i := numLights to 7 do
    glDisable(GL_LIGHT0+i);
end;

initialization

	RegisterXCollectionItemClass(TGLLightFX);

end.
