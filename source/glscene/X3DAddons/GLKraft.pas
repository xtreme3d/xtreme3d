// Kraft physics engine integration into GLScene

unit GLKraft;

interface

uses Classes, GLScene, VectorGeometry, GLMisc, XCollection, PersistentClasses, Kraft;

type

TGLKraftRigidBody = class(TGLBehaviour)
  private
    { Private Declarations }

  protected
    FRigidBody: TKraftRigidBody;

    procedure SetRigidBody(const rBody: TKraftRigidBody);

  public
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TProgressTimes); override;

    property RigidBody: TKraftRigidBody read FRigidBody write SetRigidBody;
end;

function GetOrCreateKraftRigidBody(behaviours: TGLBehaviours): TGLKraftRigidBody; overload;
function GetOrCreateKraftRigidBody(obj: TGLBaseSceneObject): TGLKraftRigidBody; overload;

implementation

function GetOrCreateKraftRigidBody(behaviours: TGLBehaviours): TGLKraftRigidBody;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TGLKraftRigidBody);
  if i >= 0 then
    Result := TGLKraftRigidBody(behaviours[i])
  else 
    Result := TGLKraftRigidBody.Create(behaviours);
end;

function GetOrCreateKraftRigidBody(obj: TGLBaseSceneObject): TGLKraftRigidBody;
begin
  Result := GetOrCreateKraftRigidBody(obj.Behaviours);
end;

class function TGLKraftRigidBody.FriendlyName: String;
begin
   result := 'KraftRigidBody';
end;

class function TGLKraftRigidBody.FriendlyDescription: String;
begin
   result := 'Kraft Rigid Body';
end;

constructor TGLKraftRigidBody.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FRigidBody := nil;
end;

destructor TGLKraftRigidBody.Destroy;
begin
  inherited Destroy;
end;

procedure TGLKraftRigidBody.SetRigidBody(const rBody: TKraftRigidBody);
var
  absMat: TMatrix;
begin
  FRigidBody := rBody;
  absMat := OwnerBaseSceneObject.AbsoluteMatrix;
  FRigidBody.SetWorldTransformation(TKraftMatrix4x4(absMat));
end;

procedure TGLKraftRigidBody.DoProgress(const progressTime : TProgressTimes);
var
  vec: TKraftVector4;
begin
  if Assigned(FRigidBody) then
  begin
    if FRigidBody.IsDynamic then
    begin
      vec := Matrix4x4GetColumn(FRigidBody.WorldTransform, 1);
      OwnerBaseSceneObject.AbsoluteUp := VectorMake(vec.x, vec.y, vec.z, vec.w);
      vec := Matrix4x4GetColumn(FRigidBody.WorldTransform, 2);
      OwnerBaseSceneObject.AbsoluteDirection := VectorMake(vec.x, vec.y, vec.z, vec.w);
      vec := Matrix4x4GetColumn(FRigidBody.WorldTransform, 3);
      OwnerBaseSceneObject.AbsolutePosition := VectorMake(vec.x, vec.y, vec.z, vec.w);
    end;
  end;
end;

initialization
  RegisterXCollectionItemClass(TGLKraftRigidBody);

end.

