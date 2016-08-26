{: GLMovement<p>

   Movement path behaviour by Roger Cao<p>

   <b>Historique : </b><font size=-1><ul>
      <li>28/09/04 - Mrqzzz - Fixed bug in proc. Interpolation (skipped a line from Carlos' code, oops)  
      <li>09/09/04 - Mrqzzz - CalculateState change by Carlos (NG) to make speed interpolated between nodes
      <li>20/11/01 - Egg - DoProgress fix suggested by Philipp Pammler (NG)
      <li>14/01/01 - Egg - Minor changes, integrated to v0.8RC2, still needed:
                           use of standard classes and documentation
      <li>22/09/00 - RoC - Added StartAllPathTravel and StopAllPathTravel methods
      <li>24/08/00 - RoC - TGLMovement and relative class added
   </ul></font>
}
unit GLMovement;

interface

uses
  Classes, GLScene, VectorGeometry, GLMisc, XCollection, OpenGL1x, Spline, GLObjects;

type

  // TGLPathNode
  //
  TGLPathNode = class (TCollectionItem)
  private
    FPosition: TVector;
    FScale: TVector;
    FRotation: TVector;
    FSpeed: single;

    procedure SetPositionAsVector(const Value: TVector);
    procedure SetRotationAsVector(const Value: TVector);
    procedure SetScaleAsVector(const Value: TVector);

    procedure SetPositionCoordinate(Index: integer; AValue: TGLFloat);
    procedure SetRotationCoordinate(Index: integer; AValue: TGLFloat);
    procedure SetScaleCoordinate(Index: integer; AValue: TGLFloat);

    procedure SetSpeed(Value: single);
  protected
    function GetDisplayName: string; override;
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function PositionAsAddress: PGLFloat;
    function RotationAsAddress: PGLFloat;
    function ScaleAsAddress: PGLFloat;

    procedure Assign(Source: TPersistent); override;

    procedure InitializeByObject(Obj: TGLBaseSceneObject);
    function EqualNode(aNode: TGLPathNode): boolean;

    property PositionAsVector: TVector Read FPosition Write SetPositionAsVector;
    property RotationAsVector: TVector Read FRotation Write SetRotationAsVector;
    property ScaleAsVector: TVector Read FScale Write SetScaleAsVector;
  published
    property X: TGLFloat index 0 Read FPosition[0] Write SetPositionCoordinate;
    property Y: TGLFloat index 1 Read FPosition[1] Write SetPositionCoordinate;
    property Z: TGLFloat index 2 Read FPosition[2] Write SetPositionCoordinate;

    //Rotation.X means PitchAngle;
    //Rotation.Y means TurnAngle;
    //Rotation.Z means RollAngle;
    property PitchAngle: TGLFloat index 0 Read FRotation[0] Write SetRotationCoordinate;
    property TurnAngle: TGLFloat index 1 Read FRotation[1] Write SetRotationCoordinate;
    property RollAngle: TGLFloat index 2 Read FRotation[2] Write SetRotationCoordinate;

    property ScaleX: TGLFloat index 0 Read FScale[0] Write SetScaleCoordinate;
    property ScaleY: TGLFloat index 1 Read FScale[1] Write SetScaleCoordinate;
    property ScaleZ: TGLFloat index 2 Read FScale[2] Write SetScaleCoordinate;

    property Speed: single Read FSpeed Write SetSpeed;
  end;


  TGLMovementPath = class;

  // TGLPathNodes
  //
  TGLPathNodes = class (TCollection)
  private
    Owner: TGLMovementPath;
  protected
    function GetOwner: TPersistent; override;
    procedure SetItems(index: integer; const val: TGLPathNode);
    function GetItems(index: integer): TGLPathNode;
  public
    constructor Create(aOwner: TGLMovementPath);
    function Add: TGLPathNode;
    function FindItemID(ID: integer): TGLPathNode;
    property Items[index: integer]: TGLPathNode Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;

  TGLMovementPath = class(TCollectionItem)
  private
    FPathLine: TGLLines;
    FShowPath: Boolean;
    FPathSplineMode: TLineSplineMode;

    FNodes: TGLPathNodes;

    //All the time saved in ms
    FStartTime: double;
    FEstimateTime: double;
    FCurrentNode: TGLPathNode;
    FInTravel: boolean;
    FLooped: boolean;
    FName: string;

    MotionSplineControl: TCubicSpline;
    RotationSplineControl: TCubicSpline;
    ScaleSplineControl: TCubicSpline;

    FOnTravelStart: TNotifyEvent;
    FOnTravelStop: TNotifyEvent;

    FCurrentNodeIndex: integer;
    //function GetPathNode(Index: integer): TGLPathNode;
    //procedure SetPathNode(Index: integer; AValue: TGLPathNode);
    function GetNodeCount: integer;
    procedure SetStartTime(Value: double);

    procedure SetCurrentNodeIndex(Value: integer);

    procedure SetShowPath(Value: Boolean);
    procedure SetPathSplineMode(Value: TLineSplineMode);
  protected
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
    function CanTravel: boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function AddNode: TGLPathNode; overload;
    function AddNode(Node: TGLPathNode): TGLPathNode; overload;

    function AddNodeFromObject(Obj: TGLBaseSceneObject): TGLPathNode;
    function InsertNodeFromObject(Obj: TGLBaseSceneObject; Index: integer): TGLPathNode;

    function InsertNode(Node: TGLPathNode; Index: integer): TGLPathNode; overload;
    function InsertNode(Index: integer): TGLPathNode; overload;
    function DeleteNode(Index: integer): TGLPathNode; overload;
    function DeleteNode(Node: TGLPathNode): TGLPathNode; overload;
    procedure ClearNodes;
    procedure UpdatePathLine;

    function NodeDistance(Node1, Node2: TGLPathNode): double;
    procedure CalculateState(CurrentTime: double);

    procedure TravelPath(Start: boolean); overload;
    procedure TravelPath(Start: boolean; aStartTime: double); overload;

    //property Nodes[index: Integer]: TGLPathNode read GetPathNode write SetPathNode;
    property NodeCount: integer Read GetNodeCount;
    property CurrentNode: TGLPathNode Read FCurrentNode;
    property InTravel: boolean Read FInTravel;

    function PrevNode: integer;
    function NextNode: integer;

    property CurrentNodeIndex: integer Read FCurrentNodeIndex Write SetCurrentNodeIndex;

    property OnTravelStart: TNotifyEvent Read FOnTravelStart Write FOnTravelStart;
    property OnTravelStop: TNotifyEvent Read FOnTravelStop Write FOnTravelStop;
  published
    property Name: string Read FName Write FName;
    {: This property is currently ignored. }
    property PathSplineMode: TLineSplineMode read FPathSplineMode write SetPathSplineMode;

    property StartTime: double Read FStartTime Write SetStartTime;
    property EstimateTime: double Read FEstimateTime;

    property Looped: boolean Read FLooped Write FLooped;
    property Nodes: TGLPathNodes Read FNodes;
    property ShowPath: Boolean read FShowPath write SetShowPath;
  end;

  TGLMovement = class;

  TGLMovementPaths = class(TCollection)
  protected
    Owner: TGLMovement;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: integer; const val: TGLMovementPath);
    function GetItems(index: integer): TGLMovementPath;
  public
    constructor Create(aOwner: TGLMovement);
    function Add: TGLMovementPath;
    function FindItemID(ID: integer): TGLMovementPath;
    property Items[index: integer]: TGLMovementPath Read GetItems Write SetItems;
    default;
    procedure Delete(index: integer);
    procedure NotifyChange; virtual;
  end;


  //Event for path related event
  TPathTravelStartEvent = procedure (Sender: TObject; 
    Path: TGLMovementPath) of object;
  TPathTravelStopEvent = procedure (Sender: TObject;
    Path: TGLMovementPath; var Looped: boolean) of object;

  TGLMovement = class(TGLBehaviour)
  private
    FPaths: TGLMovementPaths;
    FAutoStartNextPath: boolean;
    FActivePathIndex: integer;

    FOnAllPathTravelledOver: TNotifyEvent;
    FOnPathTravelStart: TPathTravelStartEvent;
    FOnPathTravelStop: TPathTravelStopEvent;
    {
    function GetMovementPath(Index: integer): TGLMovementPath;
    procedure SetMovementPath(Index: integer; AValue: TGLMovementPath);
    }
    function GetPathCount: integer;
    procedure SetActivePathIndex(Value: integer);

    function GetActivePath: TGLMovementPath;
    procedure SetActivePath(Value: TGLMovementPath);
  protected
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure PathTravelStart(Sender: TObject);
    procedure PathTravelStop(Sender: TObject);
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;

      //add an empty path;
    function AddPath: TGLMovementPath; overload;
    //add an path with one node, and the node is based on aObject
    function AddPath(aObject: TGLBaseSceneObject): TGLMovementPath; overload;
    //add one path to the new one
    function AddPath(Path: TGLMovementPath): TGLMovementPath; overload;
    procedure ClearPaths;
      //Result is current path
    function DeletePath(Path: TGLMovementPath): TGLMovementPath; overload;
    function DeletePath(Index: integer): TGLMovementPath; overload;
    function DeletePath: TGLMovementPath; overload;

    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;

    procedure StartPathTravel;
    procedure StopPathTravel;

    procedure DoProgress(const progressTime : TProgressTimes); override;

    function NextPath: integer;
    function PrevPath: integer;
    function FirstPath: integer;
    function LastPath: integer;

      //property Paths[index: Integer]: TGLMovementPath read GetMovementPath write SetMovementPath;
    property PathCount: integer Read GetPathCount;

    //why do these property can't be saved in IDE ?
    property OnAllPathTravelledOver: TNotifyEvent Read FOnAllPathTravelledOver Write FOnAllPathTravelledOver;
    property OnPathTravelStart: TPathTravelStartEvent Read FOnPathTravelStart Write FOnPathTravelStart;
    property OnPathTravelStop: TPathTravelStopEvent Read FOnPathTravelStop Write FOnPathTravelStop;
  published
    property Paths: TGLMovementPaths Read FPaths;
    property AutoStartNextPath: boolean Read FAutoStartNextPath Write FAutoStartNextPath;
    property ActivePathIndex: integer Read FActivePathIndex Write SetActivePathIndex;
    property ActivePath: TGLMovementPath Read GetActivePath Write SetActivePath;
  end;

function GetMovement(behaviours: TGLBehaviours): TGLMovement; overload;
function GetMovement(obj: TGLBaseSceneObject): TGLMovement; overload;
function GetOrCreateMovement(behaviours: TGLBehaviours): TGLMovement; overload;
function GetOrCreateMovement(obj: TGLBaseSceneObject): TGLMovement; overload;
procedure StartAllMovements(Scene: TGLScene; StartCamerasMove, StartObjectsMove: Boolean);
procedure StopAllMovements(Scene: TGLScene; StopCamerasMove, StopObjectsMove: Boolean);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

//----------------------------------- Added by Roger ---------------------------
//----------------------------- TGLPathNode ------------------------------------
constructor TGLPathNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosition := VectorMake(0, 0, 0, 1);
  FRotation := VectorMake(0, 0, 0, 1);
  FScale    := VectorMake(1, 1, 1, 1);
  FSpeed    := 0;
end;

destructor TGLPathNode.Destroy;
begin
  inherited Destroy;
end;

procedure TGLPathNode.SetPositionAsVector(const Value: TVector);
begin
  FPosition := Value;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetRotationAsVector(const Value: TVector);
begin
  FRotation := Value;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetScaleAsVector(const Value: TVector);
begin
  FScale := Value;
    (Collection as TGLPathNodes).NotifyChange;
end;

function TGLPathNode.PositionAsAddress: PGLFloat;
begin
  Result := @FPosition;
end;

function TGLPathNode.RotationAsAddress: PGLFloat;
begin
  Result := @FRotation;
end;

function TGLPathNode.ScaleAsAddress: PGLFloat;
begin
  Result := @FScale;
end;

procedure TGLPathNode.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteStuff := not (VectorEquals(FPosition, NullHmgPoint) and VectorEquals(FRotation, NullHmgPoint)
      and VectorEquals(FScale, VectorMake(1, 1, 1)) and (Speed=0));
    WriteBoolean(writeStuff);
    if WriteStuff then
    begin
      Write(FPosition, sizeof(FPosition));
      Write(FRotation, sizeof(FRotation));
      Write(FScale, sizeof(FScale));
      WriteFloat(FSpeed);
    end;
  end;
end;

procedure TGLPathNode.ReadFromFiler(reader : TReader);
begin
  with Reader do
  begin
    ReadInteger; //Achive Version 0
    if ReadBoolean then
    begin
      Read(FPosition, sizeof(FPosition));
      Read(FRotation, sizeof(FRotation));
      Read(FScale, sizeof(FScale));
      FSpeed := ReadFloat;
    end else
    begin
      FPosition := NullHmgPoint;
      FRotation := NullHmgPoint;
      FScale := VectorMake(1, 1, 1, 1);
      FSpeed := 0;
    end;
  end;
end;

procedure TGLPathNode.InitializeByObject(Obj: TGLBaseSceneObject);
begin
  if Assigned(Obj) then
  begin
    FPosition := Obj.Position.AsVector;
    FScale    := Obj.Scale.AsVector;
    FRotation := Obj.Rotation.AsVector;
    //Speed     := Obj.Speed;
  end;
end;

procedure TGLPathNode.Assign(Source: TPersistent);
begin
  if Source is TGLPathNode then
  begin
    FPosition := TGLPathNode(Source).FPosition;
    FRotation := TGLPathNode(Source).FRotation;
    FScale    := TGLPathNode(Source).FScale;
    FSpeed    := TGLPathNode(Source).FSpeed;
  end else
    inherited Assign(Source);
end;

function TGLPathNode.EqualNode(aNode: TGLPathNode): boolean;
begin
  //the speed diffrent will not be ...
  Result := (VectorEquals(FPosition, aNode.FPosition)) and
    (VectorEquals(FRotation, aNode.FRotation)) and
    (VectorEquals(FScale, aNode.FScale));
end;

procedure TGLPathNode.SetPositionCoordinate(Index: integer; AValue: TGLFloat);
begin
  FPosition[Index] := AValue;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetRotationCoordinate(Index: integer; AValue: TGLFloat);
begin
  FRotation[Index] := AValue;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetScaleCoordinate(Index: integer; AValue: TGLFloat);
begin
  FScale[Index] := AValue;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetSpeed(Value: single);
begin
  FSpeed := Value;
end;

function TGLPathNode.GetDisplayName: string;
begin
  Result := 'PathNode';
end;

//--------------------------- TGLPathNodes -------------------------------------
constructor TGLPathNodes.Create(aOwner: TGLMovementPath);
begin
  Owner := aOwner;
  inherited Create(TGLPathNode);
end;

function TGLPathNodes.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TGLPathNodes.SetItems(index: integer; const val: TGLPathNode);
begin
  inherited Items[index] := val;
end;

function TGLPathNodes.GetItems(index: integer): TGLPathNode;
begin
  Result := TGLPathNode(inherited Items[index]);
end;

function TGLPathNodes.Add: TGLPathNode;
begin
  Result := (inherited Add) as TGLPathNode;
end;

function TGLPathNodes.FindItemID(ID: integer): TGLPathNode;
begin
  Result := (inherited FindItemID(ID)) as TGLPathNode;
end;

procedure TGLPathNodes.NotifyChange;
begin
  //Update the path-line if avlible in TGLMovementPath
  (Owner as TGLMovementPath).UpdatePathLine;
end;

//--------------------------- TGLMovementPath ----------------------------------
constructor TGLMovementPath.Create(Collection: TCollection);
begin
  //This object can only be added to a TGLMovement class
  inherited Create(Collection);
  FNodes := TGLPathNodes.Create(Self);
  FCurrentNodeIndex := -1;

  FPathSplineMode := lsmCubicSpline;
end;

destructor TGLMovementPath.Destroy;
begin
  ClearNodes;
  FNodes.Free;
  inherited Destroy;
end;

procedure TGLMovementPath.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
  I: Integer;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteStuff := (FNodes.Count>0) or (FLooped) or (FCurrentNodeIndex<>-1) or (FShowPath)
      or (FPathSplineMode<>lsmCubicSpline);
    WriteBoolean(writeStuff);
    if WriteStuff then
    begin
      WriteBoolean(FLooped);
      WriteInteger(FCurrentNodeIndex);
      WriteBoolean(FShowPath);
      Write(FPathSplineMode, sizeof(FPathSplineMode));
      WriteInteger(FNodes.Count);
      for I:=0 to FNodes.Count-1 do
        FNodes.Items[I].WriteToFiler(Writer);
    end;
  end;
end;

procedure TGLMovementPath.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Node: TGLPathNode;
begin
  ClearNodes;
  with Reader do
  begin
    ReadInteger; // Archive Version 0
    if ReadBoolean then
    begin
      FLooped := ReadBoolean;
      FCurrentNodeIndex := ReadInteger;
      ShowPath := ReadBoolean;
      Read(FPathSplineMode, sizeof(FPathSplineMode));

      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        Node := AddNode;
        Node.ReadFromFiler(Reader);
      end;
    end else
    begin
      FLooped := False;
      FCurrentNodeIndex := -1;
      FShowPath := False;
      FPathSplineMode := lsmCubicSpline;
    end;
  end;
  UpdatePathLine;
end;

procedure TGLMovementPath.SetPathSplineMode(Value: TLineSplineMode);
begin
  if Value<>FPathSplineMode then
  begin
    FPathSplineMode := Value;
    if FShowPath then
      FPathLine.SplineMode := FPathSplineMode;
  end;
end;

procedure TGLMovementPath.UpdatePathLine;
var
  I: Integer;
  Node: TGLPathNode;
begin
  if FShowPath then
  begin
    FPathLine.Nodes.Clear;
    for I:=0 to Nodes.Count-1 do
    begin
      Node := Nodes.Items[I];
      FPathLine.AddNode(Node.PositionAsVector);
    end;
  end;
end;

procedure TGLMovementPath.SetShowPath(Value: Boolean);
var
  OwnerObj: TGLBaseSceneObject;
  LineObj: TGLLines;
begin
  if FShowPath<>Value then
  begin
    FShowPath := Value;
    //what a mass relationship :-(
    OwnerObj := (Collection as TGLMovementPaths).Owner{TGLMovement}.Owner{TGLBehavours}.Owner as TGLBaseSceneObject;
    if FShowPath then
    begin
      //allways add the line object to the root
      LineObj := OwnerObj.Scene.Objects.AddNewChild(TGLLines) as TGLLines;
      //set the link
      FPathLine := LineObj;
      FPathLine.SplineMode := FPathSplineMode;

      UpdatePathLine;
    end else
    begin
      OwnerObj.Scene.Objects.Remove(FPathLine, False);
      FPathLine.free;
      FPathLine := nil;
    end;
  end;
end;

procedure TGLMovementPath.ClearNodes;
begin
  TravelPath(False);
  FNodes.Clear;
  if Assigned(FCurrentNode) then
  begin
    FCurrentNode.Free;
    FCurrentNode := nil;
  end;
  FCurrentNodeIndex := -1;
  UpdatePathLine;
end;

procedure TGLMovementPath.SetCurrentNodeIndex(Value: integer);
begin
  if FNodes.Count = 0 then
  begin
    FCurrentNodeIndex := -1;
    exit;
  end;
  if (FInTravel) or (Value > FNodes.Count - 1) or (Value < 0) then
    exit
  else
  begin
    FCurrentNodeIndex := Value;
    if not Assigned(FCurrentNode) then
      FCurrentNode := TGLPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[FCurrentNodeIndex]);
  end;
end;

function TGLMovementPath.InsertNode(Node: TGLPathNode; Index: integer): TGLPathNode;
var
  N: TGLPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Node)) and (Assigned(Nodes[Index])) then
  begin
    N := TGLPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
  end
  else
    //add to the tail of list
    N    := FNodes.Add;
  Result := N;
  UpdatePathLine;
end;

function TGLMovementPath.InsertNode(Index: integer): TGLPathNode;
var
  N: TGLPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Nodes[Index])) then
  begin
    N := TGLPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
    Result := N;
  end
  else
    //add to the tail of list
    Result := AddNode;
  UpdatePathLine;
end;

function TGLMovementPath.AddNodeFromObject(Obj: TGLBaseSceneObject): TGLPathNode;
begin
  Result := nil;
  if (FInTravel) or (not Assigned(Obj)) then
    exit;
  Result           := AddNode;
  Result.FPosition := Obj.Position.AsVector;
  Result.FScale    := Obj.Scale.AsVector;
  Result.FRotation := Obj.Rotation.AsVector;
  //Result.FSpeed    := Obj.Speed;
  UpdatePathLine;
end;

function TGLMovementPath.InsertNodeFromObject(Obj: TGLBaseSceneObject; Index: integer): TGLPathNode;
var
  N: TGLPathNode;
begin
  Result := nil;
  if (FInTravel) or (not Assigned(Obj)) then
    exit;
  N           := InsertNode(Index);
  N.FPosition := Obj.Position.AsVector;
  N.FScale    := Obj.Scale.AsVector;
  N.FRotation := Obj.Rotation.AsVector;
  //N.Speed     := Obj.Speed;
  Result      := N;
  UpdatePathLine;
end;

function TGLMovementPath.DeleteNode(Index: integer): TGLPathNode;
var
  Node: TGLPathNode;
begin
  Result := nil;
  //Ontravel, can't delete
  if FInTravel then
    exit;
  Node := Nodes[Index];
  if Assigned(Node) then
  begin
    FNodes.Delete(Index);
    if FCurrentNodeIndex < 0 then
      exit;
    if (Index =0) then
    begin
      if FNodes.Count > 0 then
        FCurrentNodeIndex := 0
      else
        FCurrentNodeIndex := -1;
    end
    else
    begin
      //one has been deleted, so the index should be equal to FNodeList.Count
      if Index =FNodes.Count then
        FCurrentNodeIndex := Index -1
      else
        FCurrentNodeIndex := Index;
    end;
    Result := Nodes[FCurrentNodeIndex];
  end;
  UpdatePathLine;
end;

function TGLMovementPath.DeleteNode(Node: TGLPathNode): TGLPathNode;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count - 1 do
  begin
    if Node = Nodes[I] then
    begin
      Result := DeleteNode(I);
      break;
    end;
  end;
  UpdatePathLine;
end;

function TGLMovementPath.PrevNode: integer;
begin
  Result := FCurrentNodeIndex;
  if FNodes.Count = 0 then
    exit;
  Dec(FCurrentNodeIndex);
  if (FCurrentNodeIndex < 0) then
    FCurrentNodeIndex := 0
  else
    //this line can cause the CurrentNode generated
    CurrentNodeIndex := FCurrentNodeIndex;
  Result             := FCurrentNodeIndex;
end;

function TGLMovementPath.NextNode: integer;
begin
  Result := FCurrentNodeIndex;
  if FNodes.Count = 0 then
    exit;
  Inc(FCurrentNodeIndex);
  if (FCurrentNodeIndex = FNodes.Count) then
    Dec(FCurrentNodeIndex)
  else
    //this line can cause the CurrentNode generated
    CurrentNodeIndex := FCurrentNodeIndex;
  Result             := FCurrentNodeIndex;
end;

function TGLMovementPath.NodeDistance(Node1, Node2: TGLPathNode): double;
var
  Vector: TVector;
begin
  Vector := VectorSubtract(Node1.FPosition, Node2.FPosition);
  Result := VectorLength(Vector);
end;

//need to do
//1 No acceleration implemented
//2 The travel-time of a segment is based a simple linear movement, at the start and the end
//  of the segment, the speed will be more high than in the middle
//3 Rotation Interpolation has not been tested
procedure TGLMovementPath.CalculateState(CurrentTime: double);
var
  I:       integer;
  SumTime: double;
  L:       single;
  Interpolated: boolean;
  T:       double;
  a:double;

  procedure Interpolation(ReturnNode: TGLPathNode; Time1, Time2: double;
Index: integer);
  var
    Ratio: double;
    x, y, z, p, t, r, sx, sy, sz: single;
  begin
    Ratio:=(Nodes[I - 1].Speed*Time2+0.5*a*time2*time2)/L + Index;

    MotionSplineControl.SplineXYZ(Ratio, x, y, z);
    RotationSplineControl.SplineXYZ(Ratio, p, t, r);
    ScaleSplineControl.SplineXYZ(Ratio, sx, sy, sz);

    ReturnNode.FPosition := VectorMake(x, y, z, 1);
    ReturnNode.FRotation := VectorMake(p, t, r, 1);
    ReturnNode.FScale    := VectorMake(sx, sy, sz, 1);
  end;

begin
  I := 1;

  if (FStartTime=0) or (FStartTime>CurrentTime) then
    FStartTime := CurrentTime;
  SumTime      := FStartTime;
  Interpolated := False;
  while I < FNodes.Count do
  begin
    L := NodeDistance(Nodes[I], Nodes[I - 1]);
    T := L / (Nodes[I - 1].Speed+Nodes[I - 0].Speed)*2;
    if (SumTime + T) >= CurrentTime then
    begin
      a:=(Nodes[I - 0].Speed-Nodes[I - 1].Speed)/T;
      Interpolation(FCurrentNode, T, CurrentTime - SumTime, I - 1);
      Interpolated := True;
      break;
    end
    else
    begin
      Inc(I);
      SumTime := SumTime + T;
    end;
  end;
  if (not Interpolated) then
  begin
    Interpolation(FCurrentNode, 1.0, 0.0, FNodes.Count - 1);
    TravelPath(False);
  end;
end;


function TGLMovementPath.CanTravel: boolean;
var
  I: integer;
begin
  Result := True;
  if FNodes.Count < 2 then
  begin
    Result := False;
    exit;
  end;
  for I := 0 to FNodes.Count - 1 do
    if Abs(Nodes[I].Speed) < 0.01 then
    begin
      Result := False;
      break;
    end;
end;

procedure TGLMovementPath.TravelPath(Start: boolean);
var
  x, y, z:    PFloatArray;
  p, t, r:    PFloatArray;
  sx, sy, sz: PFloatArray;
  I:          integer;
begin
  if (FInTravel = Start) or (FNodes.Count = 0) then
    exit;
  //One of the node speed < 0.01;
  if (Start) and (not CanTravel) then
    exit;
  FInTravel := Start;
  if FInTravel then
  begin
    GetMem(x, sizeof(single) * FNodes.Count);
    GetMem(y, sizeof(single) * FNodes.Count);
    GetMem(z, sizeof(single) * FNodes.Count);
    GetMem(p, sizeof(single) * FNodes.Count);
    GetMem(t, sizeof(single) * FNodes.Count);
    GetMem(r, sizeof(single) * FNodes.Count);
    GetMem(sx, sizeof(single) * FNodes.Count);
    GetMem(sy, sizeof(single) * FNodes.Count);
    GetMem(sz, sizeof(single) * FNodes.Count);
    for I := 0 to FNodes.Count - 1 do
    begin
      PFloatArray(x)[I]  := Nodes[I].FPosition[0];
      PFloatArray(y)[I]  := Nodes[I].FPosition[1];
      PFloatArray(z)[I]  := Nodes[I].FPosition[2];
      PFloatArray(p)[I]  := Nodes[I].FRotation[0];
      PFloatArray(t)[I]  := Nodes[I].FRotation[1];
      PFloatArray(r)[I]  := Nodes[I].FRotation[2];
      PFloatArray(sx)[I] := Nodes[I].FScale[0];
      PFloatArray(sy)[I] := Nodes[I].FScale[1];
      PFloatArray(sz)[I] := Nodes[I].FScale[2];
    end;
    MotionSplineControl   := TCubicSpline.Create(x, y, z, nil, FNodes.Count);
    RotationSplineControl := TCubicSpline.Create(p, t, r, nil, FNodes.Count);
    ScaleSplineControl    := TCubicSpline.Create(sx, sy, sz, nil, FNodes.Count);
    FreeMem(x);
    FreeMem(y);
    FreeMem(z);
    FreeMem(p);
    FreeMem(t);
    FreeMem(r);
    FreeMem(sx);
    FreeMem(sy);
    FreeMem(sz);

    FCurrentNode := TGLPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[0]);
    FCurrentNodeIndex := -1;

    FEstimateTime := 0;
    for I := 1 to FNodes.Count - 1 do
      FEstimateTime := FEstimateTime + NodeDistance(Nodes[I], Nodes[I - 1]) / Nodes[I - 1].Speed;

    if Assigned(FOnTravelStart) then
      FOnTravelStart(self);
  end 
  else
  begin
    MotionSplineControl.Free;
    RotationSplineControl.Free;
    ScaleSplineControl.Free;
    if Assigned(FOnTravelStop) then
      FOnTravelStop(self);
  end;
end;

procedure TGLMovementPath.TravelPath(Start: boolean; aStartTime: double);
begin
  if FInTravel = Start then
    exit;
  FStartTime := aStartTime;
  TravelPath(Start);
end;

{
function TGLMovementPath.GetPathNode(Index: integer): TGLPathNode;
begin
  if (Index >=0) and (Index <FNodes.Count) then
    Result := FNodes[Index]
  else
    Result := nil;
end;

procedure TGLMovementPath.SetPathNode(Index: integer; AValue: TGLPathNode);
begin
  if (Index >=0) and (Index <FNodes.Count) and (Assigned(AValue)) then
    Nodes[Index].Assign(AValue);
end;
}
function TGLMovementPath.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

//-------------------------- This function need modified -----------------------
procedure TGLMovementPath.SetStartTime(Value: double);
begin
  FStartTime := Value;
end;

(*
procedure TGLMovementPath.WriteToFiler(writer : TWriter);
var
  WriteStuff : Boolean;
  I: Integer;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteStuff := (FStartTime<>0) or (FNodes.Count>0) {or (Assigned(FOnTravelStart))
      or (Assigned(FOnTravelStop))}
      or (FLooped) or (FName<>'');
    WriteBoolean(WriteStuff);
    if WriteStuff then
    begin
      WriteString(FName);
      Writer.WriteFloat(FStartTime);
      WriteInteger(FNodeList.Count);
      for I:=0 to FNodeList.Count-1 do
        Nodes[I].WriteToFiler(Writer);
      WriteBoolean(FLooped);
      //Writer.WriteString(MethodName(@FOnTravelStart));
      //Writer.WriteString(MethodName(@FOnTravelStop));
    end;
  end;
end;

procedure TGLMovementPath.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  //S: string;
begin
  ClearNodes;
  FEstimateTime := 0;
  with Reader do
  begin
    ReadInteger; // ignore Archive Version
    if ReadBoolean then
    begin
      FName := ReadString;
      FStartTime := ReadFloat;
      //List count read here
      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        AddNode;
        Nodes[I].ReadFromFiler(Reader);
      end;
      FLooped := ReadBoolean;
      {
      S := ReadString;
      @FOnTravelStart := MethodAddress(S);
      S := ReadString;
      @FOnTravelStop := MethodAddress(S);
      }
    end else
    begin
      FStartTime := 0;
      FLooped := False;
      FOnTravelStart := nil;
      FOnTravelStop := nil;
    end;
  end;
  FInTravel := False;
  if Assigned(FCurrentNode) then
  begin
    FCurrentNode.free;
    FCurrentNode := nil;
  end;
  FCurrentNodeIndex := -1;
end;
*)

procedure TGLMovementPath.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TGLMovementPath then
  begin
    ClearNodes;
    for I := 0 to TGLMovementPath(Source).NodeCount - 1 do
    begin
      AddNode;
      Nodes[I].Assign(TGLMovementPath(Source).Nodes[I]);
      FStartTime := TGLMovementPath(Source).FStartTime;
      //FEstimateTime := TGLMovementPath(Source).FEstimateTime;
      FLooped := TGLMovementPath(Source).FLooped;
    end;
  end;
end;

function TGLMovementPath.AddNode: TGLPathNode;
var
  Node: TGLPathNode;
  I:    integer;
begin
  //Add a empty node, if it's not the first one, try locate the node to the previous one
  Node := FNodes.Add;
  I    := FNodes.Count;
  if I > 1 then
    Node.Assign(Nodes[I - 2]);
  Result := Node;
end;

function TGLMovementPath.AddNode(Node: TGLPathNode): TGLPathNode;
begin
  Result := AddNode;
  if Assigned(Node) then
    Result.Assign(Node);
end;

//------------------------- TGLMovementPaths ----------------------------------
constructor TGLMovementPaths.Create(aOwner: TGLMovement);
begin
  Owner := aOwner;
  inherited Create(TGLMovementPath);
end;

procedure TGLMovementPaths.SetItems(index: integer; const val: TGLMovementPath);
begin
  inherited Items[index] := val;
end;

function TGLMovementPaths.GetOwner: TPersistent;
begin
  Result := Owner;
end;

function TGLMovementPaths.GetItems(index: integer): TGLMovementPath;
begin
  Result := TGLMovementPath(inherited Items[index]);
end;

function TGLMovementPaths.Add: TGLMovementPath;
begin
  Result := (inherited Add) as TGLMovementPath;
end;

function TGLMovementPaths.FindItemID(ID: integer): TGLMovementPath;
begin
  Result := (inherited FindItemID(ID)) as TGLMovementPath;
end;

procedure TGLMovementPaths.Delete(index: integer);
var
  item: TCollectionItem;
begin
  item := inherited Items[index];
  item.Collection := nil;
  item.Free;
end;

procedure TGLMovementPaths.NotifyChange;
begin
  //Do nothing here
end;


//--------------------------- TGLMovement --------------------------------------
constructor TGLMovement.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FPaths           := TGLMovementPaths.Create(Self);
  FAutoStartNextPath := True;
  FActivePathIndex := -1;
  FOnAllPathTravelledOver := nil;
  FOnPathTravelStart := nil;
  FOnPathTravelStop := nil;
end;

destructor TGLMovement.Destroy;
begin
  ClearPaths;
  FPaths.Free;
  inherited Destroy;
end;

procedure TGLMovement.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
  I: Integer;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteStuff := (FPaths.Count>0) or (not FAutoStartNextPath) or (FActivePathIndex<>-1);
    WriteBoolean(WriteStuff);
    if WriteStuff then
    begin
      WriteBoolean(FAutoStartNextPath);
      WriteInteger(FActivePathIndex);

      WriteInteger(FPaths.Count);
      for I:=0 to FPaths.Count-1 do
        FPaths.Items[I].WriteToFiler(Writer);
    end;
  end;
end;

procedure TGLMovement.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Path: TGLMovementPath;
begin
  ClearPaths;
  with Reader do
  begin
    ReadInteger; // Archive Version 0
    if ReadBoolean then
    begin
      FAutoStartNextPath := ReadBoolean;
      FActivePathIndex := ReadInteger;

      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        Path := AddPath;
        Path.ReadFromFiler(Reader);
      end;
    end else
    begin
      FAutoStartNextPath := True;
      FActivePathIndex := -1;
    end;
  end;
end;

procedure TGLMovement.ClearPaths;
begin
  FPaths.Clear;
  FActivePathIndex := -1;
end;

procedure TGLMovement.PathTravelStart(Sender: TObject);
begin
  if Assigned(FOnPathTravelStart) then
    FOnPathTravelStart(Self, TGLMovementPath(Sender));
end;

procedure TGLMovement.PathTravelStop(Sender: TObject);
begin
  if Assigned(FOnPathTravelStop) then
    FOnPathTravelStop(Self, TGLMovementPath(Sender), TGLMovementPath(Sender).FLooped);
  if TGLMovementPath(Sender).FLooped then
  begin
    //if looped, then re-start the path
    StartPathTravel;
  end
  else if (FActivePathIndex = FPaths.Count - 1) then
  begin
    if (Assigned(FOnAllPathTravelledOver)) then
      FOnAllPathTravelledOver(Self);
  end
  else //auto-start next path
  if FAutoStartNextPath then
  begin
    Inc(FActivePathIndex);
    StartPathTravel;
  end;
end;

function TGLMovement.AddPath: TGLMovementPath;
var
  Path: TGLMovementPath;
begin
  Path   := FPaths.Add;
  Path.OnTravelStart := PathTravelStart;
  Path.OnTravelStop := PathTravelStop;
  Result := Path;
end;

function TGLMovement.AddPath(aObject: TGLBaseSceneObject): TGLMovementPath;
begin
  Result := AddPath;
  Result.AddNodeFromObject(aObject);
end;

function TGLMovement.AddPath(Path: TGLMovementPath): TGLMovementPath;
begin
  Result := AddPath;
  if Assigned(Path) then
    Result.Assign(Path);
end;

function TGLMovement.DeletePath: TGLMovementPath;
begin
  Result := DeletePath(FActivePathIndex);
end;

function TGLMovement.DeletePath(Path: TGLMovementPath): TGLMovementPath;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FPaths.Count - 1 do
  begin
    if Path = Paths[I] then
    begin
      Result := DeletePath(I);
      break;
    end;
  end;
end;

function TGLMovement.DeletePath(Index: integer): TGLMovementPath;
begin
  Result := nil;
  if (Index <0) or (Index >=FPaths.Count) then
    exit;

  if Index >=0 then
  begin
    TGLMovementPath(FPaths[Index]).Free;
    FPaths.Delete(Index);
    if FActivePathIndex < 0 then
      exit;
    if (Index =0) then
    begin
      if FPaths.Count > 0 then
        FActivePathIndex := 0
      else
        FActivePathIndex := -1;
    end 
    else
    begin
      //one has been deleted, so the index should be equal to FPathList.Count
      if Index =FPaths.Count then
        FActivePathIndex := Index -1
      else
        FActivePathIndex := Index;
    end;
    Result := ActivePath;
  end;
end;

procedure TGLMovement.SetActivePathIndex(Value: integer);
begin
  if FActivePathIndex = Value then
    exit;
  //if current has a Active path in travelling, then exit the method
  if (Assigned(ActivePath)) and (ActivePath.InTravel) then
    exit;
  if (Value >= 0) and (Value < FPaths.Count) then
  begin
    FActivePathIndex := Value;
    //Start the new path or wait for the start-command
  end 
  else if Value < 0 then
  begin
    FActivePathIndex := -1;
    //Stop all the running path
  end;
end;

function TGLMovement.NextPath: integer;
begin
  ActivePathIndex := FActivePathIndex + 1;
  Result           := FActivePathIndex;
end;

function TGLMovement.PrevPath: integer;
begin
  ActivePathIndex := FActivePathIndex - 1;
  if (FActivePathIndex < 0) and (FPaths.Count > 0) then
    Result := 0
  else
    Result := FActivePathIndex;
end;

function TGLMovement.FirstPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := 0;
  Result              := FActivePathIndex;
end;

function TGLMovement.LastPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := FPaths.Count - 1;
  Result              := FActivePathIndex;
end;

function TGLMovement.GetActivePath: TGLMovementPath;
begin
  if FActivePathIndex >= 0 then
    Result := Paths[FActivePathIndex]
  else
    Result := nil;
end;

procedure TGLMovement.SetActivePath(Value: TGLMovementPath);
var
  I: integer;
begin
  ActivePathIndex := -1;
  for I := 0 to FPaths.Count - 1 do
  begin
    if Value = Paths[I] then
    begin
      ActivePathIndex := I;
      break;
    end;
  end;
end;

function TGLMovement.GetPathCount: integer;
begin
  Result := FPaths.Count;
end;

(*
procedure TGLMovement.WriteToFiler(writer : TWriter);
var
  WriteStuff : Boolean;
  I: Integer;
  //S: string;
begin
  inherited;
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteStuff := (FPathList.Count>0) or (FAutoStartNextPath) or
      (FActivePathIndex<>-1) {or (Assigned(OnAllPathTravelledOver))};
    WriteBoolean(WriteStuff);
    if WriteStuff then
    begin
      WriteBoolean(FAutoStartNextPath);
      WriteInteger(FPathList.Count);
      for I:=0 to FPathList.Count-1 do
        Paths[I].WriteToFiler(Writer);
      //the Activepathindex must be write after the path list had been saved
      WriteInteger(FActivePathIndex);
    end;
  end;
end;

procedure TGLMovement.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  //S: string;
begin
  inherited;
  ClearPaths;
  with Reader do
  begin
    ReadInteger; // ignore Archive Version
    if ReadBoolean then
    begin
      FAutoStartNextPath := ReadBoolean;
      //List count read here
      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        AddPath;
        Paths[I].ReadFromFiler(Reader);
      end;
      //the Activepathindex must be write after the path list had been saved
      FActivePathIndex := ReadInteger;
    end else
    begin
      FAutoStartNextPath := False;
      FOnAllPathTravelledOver := nil;
    end;
  end;
end;
*)

procedure TGLMovement.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TGLMovement then
  begin
    ClearPaths;
    for I := 0 to TGLMovement(Source).PathCount - 1 do
    begin
      AddPath;
      Paths[I].Assign(TGLMovement(Source).Paths[I]);
    end;
    FAutoStartNextPath := TGLMovement(Source).FAutoStartNextPath;
  end;
end;

class function TGLMovement.FriendlyName: string;
begin
  Result := 'Movement controls'
end;

class function TGLMovement.FriendlyDescription: string;
begin
  Result := 'Object movement path controls'
end;

class function TGLMovement.UniqueItem: boolean;
begin
  Result := True;
end;

var
  OldTime: double = 0.0;

procedure TGLMovement.StartPathTravel;
var
  newTime: double;
begin
  if FActivePathIndex < 0 then
    exit;
  //convert the time to second
  newTime := 0;
  OldTime := newTime;
  Paths[FActivePathIndex].TravelPath(True, newTime);
end;

procedure TGLMovement.StopPathTravel;
begin
  if FActivePathIndex < 0 then
    exit;
  Paths[FActivePathIndex].TravelPath(False);
end;

//Calculate functions add into this method
procedure TGLMovement.DoProgress(const progressTime : TProgressTimes);
var
  Path: TGLMovementPath;
begin
  if (FActivePathIndex >= 0) and (Paths[FActivePathIndex].InTravel) then
    //if deltaTime >= 0.033 then
    begin
      Path := Paths[FActivePathIndex];
      Path.CalculateState(progressTime.newTime);
      if Assigned(Path.CurrentNode) then
      begin
        if Owner.Owner is TGLBaseSceneObject then
          with TGLBaseSceneObject(Owner.Owner) do
          begin
            Position.AsVector := Path.CurrentNode.FPosition;
            Scale.AsVector    := Path.CurrentNode.FScale;
            PitchAngle := Path.CurrentNode.PitchAngle;
            TurnAngle := Path.CurrentNode.TurnAngle;
            RollAngle := Path.CurrentNode.RollAngle;
          end;
      end;
    end;
end;


function GetMovement(behaviours: TGLBehaviours): TGLMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLMovement);
  if i >= 0 then
    Result := TGLMovement(behaviours[i])
  else
    Result := nil;
end;

function GetMovement(obj: TGLBaseSceneObject): TGLMovement; overload;
begin
  Result := GetMovement(obj.behaviours);
end;

function GetOrCreateMovement(behaviours: TGLBehaviours): TGLMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLMovement);
  if i >= 0 then
    Result := TGLMovement(behaviours[i])
  else
    Result := TGLMovement.Create(behaviours);
end;

function GetOrCreateMovement(obj: TGLBaseSceneObject): TGLMovement; overload;
begin
  Result := GetOrCreateMovement(obj.behaviours);
end;

procedure StartStopTravel(Obj: TGLBaseSceneObject; Start: Boolean);
var
  NewObj: TGLBaseSceneObject;
  I: Integer;
  Movement: TGLMovement;
begin
  Movement := GetMovement(Obj);
  if Assigned(Movement) then
    if Start then
    begin
      if (Movement.PathCount>0) and (Movement.ActivePathIndex=-1) then
        Movement.ActivePathIndex := 0;
      Movement.StartPathTravel;
    end else
      Movement.StopPathTravel;
  for I:=0 to Obj.Count-1 do
  begin
    NewObj := Obj.Children[I];
    StartStopTravel(NewObj, Start);
  end;
end;

procedure StartAllMovements(Scene: TGLScene; StartCamerasMove, StartObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StartCamerasMove then
      StartStopTravel(Scene.Cameras, StartCamerasMove);
    if StartObjectsMove then
      StartStopTravel(Scene.Objects, StartObjectsMove);
  end;
end;

procedure StopAllMovements(Scene: TGLScene; StopCamerasMove, StopObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StopCamerasMove then
      StartStopTravel(Scene.Cameras, False);
    if StopObjectsMove then
      StartStopTravel(Scene.Objects, False);
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TGLMovement);

end.

