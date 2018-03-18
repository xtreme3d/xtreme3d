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
  end
  else if Assigned(light1) then
    result := True
  else if Assigned(light2) then
    result := False
  else
    result := True;
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
  numLights := Min(lights.Count, 2); // 8

  if Assigned(lights) then
    if lights.Count > 1 then
      SortLights(lights);

  glPushAttrib(GL_ALL_ATTRIB_BITS);

  glPushMatrix;
  // revert to the base model matrix
  glLoadMatrixf(@ownerObj.Scene.CurrentBuffer.ModelViewMatrix);

  glDisable(GL_CULL_FACE);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glEnable(GL_BLEND);
  glDepthFunc(GL_LEQUAL);
  glPointSize(20.0);

  for i := 0 to numLights - 1 do begin
    lightSource := TGLLightSource(lights[i]);
    if Assigned(lightSource) then begin
      if lightSource.Shining then begin
        // Draw light as point
        glColor4f(lightSource.Diffuse.Red,
                  lightSource.Diffuse.Green,
                  lightSource.Diffuse.Blue,
                  1.0);
        glBegin(GL_POINTS);
        lightAbsPos := lightSource.AbsolutePosition;
        glVertex3fv(@lightAbsPos);
        glEnd();

        // TODO: feed light to OpenGL
      end;
    end;
  end;

  glDepthFunc(GL_LESS);
  glPopMatrix;
  glPopAttrib;
end;

initialization

	RegisterXCollectionItemClass(TGLLightFX);

end.
