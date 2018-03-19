unit GLLightFx;

interface

uses
  Classes, Dialogs, Math, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils, GLMisc,
  XCollection, PersistentClasses;

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
        procedure SortLights(list: TPersistentObjectList);
        procedure DoProgress(const progressTimes: TProgressTimes); override;
        procedure Render(sceneBuffer: TGLSceneBuffer; var rci: TRenderContextInfo); override;
  end;

function GetOrCreateLightFX(effects: TGLObjectEffects): TGLLightFX; overload;
function GetOrCreateLightFX(obj: TGLBaseSceneObject): TGLLightFX; overload;

implementation

function GetOrCreateLightFX(effects: TGLObjectEffects): TGLLightFX;
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

procedure TGLLightFX.DoProgress(const progressTimes: TProgressTimes);
begin
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

procedure TGLLightFX.SortLights(list: TPersistentObjectList);
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

procedure TGLLightFX.Render(sceneBuffer: TGLSceneBuffer; var rci: TRenderContextInfo);
var
  i: Integer;
  lightSource: TGLLightSource;
  numLights: Integer;
  ownerObj: TGLBaseSceneObject;
  lights: TPersistentObjectList;
  lightId: Cardinal;
  lightAbsPos: TVector;
begin
  ownerObj := OwnerBaseSceneObject();
  lights := ownerObj.Scene.Lights;
  numLights := Min(lights.Count, 8);

  if Assigned(lights) then
    if lights.Count > 1 then
      SortLights(lights);
      
  glPushMatrix;
  // revert to the base model matrix
  glLoadMatrixf(@ownerObj.Scene.CurrentBuffer.ModelViewMatrix);

  for i := 0 to numLights - 1 do begin
    lightSource := TGLLightSource(lights[i]);
    if Assigned(lightSource) then begin
      if lightSource.Shining then begin
        glPushMatrix;

        lightId := GL_LIGHT0+i;
        glEnable(lightId);

        if lightSource.LightStyle = lsParallel then begin
          glMultMatrixf(PGLFloat(lightSource.AbsoluteMatrixAsAddress));
          glLightfv(lightId, GL_POSITION, lightSource.SpotDirection.AsAddress)
        end else begin
          glMultMatrixf(PGLFloat(lightSource.Parent.AbsoluteMatrixAsAddress));
          glLightfv(lightId, GL_POSITION, lightSource.Position.AsAddress);
        end;

        glLightfv(lightId, GL_AMBIENT,  lightSource.Ambient.AsAddress);
        glLightfv(lightId, GL_DIFFUSE,  lightSource.Diffuse.AsAddress);
        glLightfv(lightId, GL_SPECULAR, lightSource.Specular.AsAddress);

        if lightSource.LightStyle = lsSpot then begin
          if lightSource.SpotCutOff <> 180 then begin
            glLightfv(lightId, GL_SPOT_DIRECTION, lightSource.SpotDirection.AsAddress);
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
