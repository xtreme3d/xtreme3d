unit GLKraftManager;

interface

uses
  Classes, Dialogs, PersistentClasses, XCollection,
  VectorTypes, GLScene, GLUtils, GLMisc, OpenGL1x, Kraft;

type
  TGLKraftBehaviour = class;

  TGLKraftManager = class(TComponent)
  protected
    FKraft: TKraft;
    FKraftBehaviours: TPersistentObjectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Step(dt: double);
    function AddRigidBody(bodyType: TKraftRigidBodyType): TKraftRigidBody;
    function AddBehaviour(obj: TGLBaseSceneObject; body: TKraftRigidBody): TGLKraftBehaviour;
  end;
  
  TGLKraftBehaviour = class(TGLBehaviour)
    private
      FManager: TGLKraftManager;
      FShape: TKraftShape;
      FRigidBody: TKraftRigidBody;
      FOwnerBaseSceneObject: TGLBaseSceneObject;
    protected
      procedure DoProgress(const progressTime: TProgressTimes); override;
    public
      constructor Create(AOwner: TXCollection); override;
      destructor Destroy; override;
      //procedure CreateShape
      //procedure CalcMass;
      procedure AlignObject;
    published
      property RigidBody: TKraftRigidBody read FRigidBody write FRigidBody;
      property Shape: TKraftShape read FShape write FShape;
      property Manager: TGLKraftManager read FManager write FManager;
  end;

implementation

// TGLKraftManager

constructor TGLKraftManager.Create(AOwner: TComponent);
begin
  inherited;
  FKraft := TKraft.Create(-1);
  FKraft.SetFrequency(120.0);
  FKraft.VelocityIterations := 8;
  FKraft.PositionIterations := 3;
  FKraft.SpeculativeIterations := 8;
  FKraft.TimeOfImpactIterations := 20;
  FKraft.Gravity.y := -9.81;

  FKraftBehaviours := TPersistentObjectList.Create;
end;

destructor TGLKraftManager.Destroy;
begin
  FKraft.Destroy;
  inherited Destroy;
end;

procedure TGLKraftManager.Step(dt: double);
var
  i: Integer;
begin
  FKraft.Step(dt);

  for i:=0 to FKraftBehaviours.Count-1 do
  begin
    if FKraftBehaviours[i] is TGLKraftBehaviour then
      TGLKraftBehaviour(FKraftBehaviours[i]).AlignObject;
  end;
end;

function TGLKraftManager.AddRigidBody(bodyType: TKraftRigidBodyType): TKraftRigidBody;
var
  body: TKraftRigidBody;
begin
  body := TKraftRigidBody.Create(self.FKraft);
  body.SetRigidBodyType(bodyType);
  result := body;
end;

function TGLKraftManager.AddBehaviour(
  obj: TGLBaseSceneObject;
  body: TKraftRigidBody): TGLKraftBehaviour;
var
  beh: TGLKraftBehaviour;
begin
  beh := TGLKraftBehaviour(obj.GetOrCreateBehaviour(TGLKraftBehaviour));
  FKraftBehaviours.Add(beh);
  beh.Manager := self;
  beh.RigidBody := body;
  //body.SetWorldTransformation(
  // Matrix4x4Translate(Vector3Add(FormMain.CurrentCamera.Position,Vector3ScalarMul(PKraftVector3(pointer(@FormMain.CurrentCamera.Matrix[2,0]))^,1.0))));
  result := beh;
end;

// TGLKraftBehaviour

constructor TGLKraftBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;
end;

destructor TGLKraftBehaviour.Destroy;
begin
  if Assigned(FRigidBody) then
    FRigidBody.Destroy;
  if Assigned(FShape) then
    FShape.Destroy;
  if Assigned(Fmanager) then
    Fmanager := nil;
  inherited;
end;

{
procedure TGLKraftBehaviour.CalcMass;
begin
   FRigidBody.Finish;
end;
}

procedure TGLKraftBehaviour.AlignObject;
//var
  //pos : PdVector3;
  //R : PdMatrix3;
  //m : TMatrix;
begin
  //FRigidBody.
  //pos:=dBodyGetPosition(Body);
  //R:=dBodyGetRotation(Body);
  //ODERToGLSceneMatrix(m,R^,pos^);
  //if OwnerBaseSceneObject.Parent is TGLBaseSceneObject then
  //  m:=MatrixMultiply(m, OwnerBaseSceneObject.Parent.InvAbsoluteMatrix);
  //OwnerBaseSceneObject.Matrix:=m;
end;

procedure TGLKraftBehaviour.DoProgress(const progressTime: TProgressTimes);
begin
  // TODO
end;

end.
