//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLExplosionFx<p>

  TGLBExplosionFX Effect<p>

	<b>History : </b><font size=-1><ul>
    <li>23/12/04 - PhP - GLScene Headerized, replaced some VectorXXX functions with XXXVector procedures
    <li>07/03/04 - Matheus Degiovani - Creation
  </ul></font>

  Description: this effect explodes a mesh object into triangles
  that fly over. You can define a default direction, in wich case
  the pieces of the mesh will follow that direction, only rotating,
  or if you define a null vector as the direction, a vector will be
  calculated for each triangle, based on the normal vector of that
  triangle, with a little random addition so things look better.
  Pretty neat :)<p>

  Note: the owner of this behaviour should be any class that derives
  from TGLBaseMesh class or any other class derived from TGLBaseMesh.
  Also, the structure of the mesh is lost after the caching of information,
  so if you'll need the mesh after exploding it, you'll have to save the
  MeshObjects property of the mesh, OR load it again.
}
unit GLExplosionFx;

interface

uses
  OpenGL1x, VectorGeometry, GLMisc, GLScene, GLVectorFileObjects,
  GLTexture, VectorLists, XCollection;

type
  TGLBExplosionFX = class(TGLObjectPreEffect)
  private
    FTriList: TAffineVectorList;
    FRotList: TAffineVectorList;
    FDirList: TAffineVectorList;
    FPosList: TAffineVectorList;
    FEnabled: boolean;
    FFaceCount: integer;
    FSpeed: single;
    FDirection: TGLCoordinates;
    FMaxSteps: integer;
    FStep: integer;
    procedure SetTriList(Value: TAffineVectorList);
    procedure SetRotList(Value: TAffineVectorList);
    procedure SetDirList(Value: TAffineVectorList);
    procedure SetPosList(Value: TAffineVectorList);
    procedure SetDirection(value: TGLCoordinates);
    procedure SetEnabled(value: boolean);
  protected
    property TriList: TAffineVectorList read FTriList write SetTriList;
    property RotList: TAffineVectorList read FRotList write SetRotList;
    property DirList: TAffineVectorList read FDirList write SetDirList;
    property PosList: TAffineVectorList read FPosList write SetPosList;
    property FaceCount: integer read FFAceCount write FFaceCount;
    procedure CacheInfo;
  public
    property Enabled: boolean read FEnabled write SetEnabled;
    property Step: integer read FStep;
    constructor Create(aOwner : TXCollection); override;
    destructor Destroy; override;
    procedure Render(sceneBuffer : TGLSceneBuffer; var rci : TRenderContextInfo); override;
    { resets the behaviour, so the information can be re-cached and
      the mesh can be exploded again }
    procedure Reset;
    class function FriendlyName : String; override;
    class function FriendlyDescription : String; override;
  published
    // property X: single index 0 read FDirection[0] write FDirection[0];
    property MaxSteps: integer read FMaxSteps write FMaxSteps;
    property Speed: single read FSpeed write FSpeed;
    property Direction: TGLCoordinates read FDirection write SetDirection;
  end;

implementation

{ TGLBExplosionFx }

// Create
//
constructor TGLBExplosionFx.Create(aOwner: TXCollection);
begin
  inherited Create(AOwner);
  FTriList := TAffineVectorList.Create;
  FRotList := TAffineVectorList.Create;
  FDirList := TAffineVectorList.Create;
  FPosList := TAffineVectorList.Create;
  FDirection := TGLCoordinates.CreateInitialized(Self, NullHmgVector);
  FDirection.SetVector(0, 0, 0, 0);
end;

// Destroy
//
destructor TGLBExplosionFX.Destroy;
begin
  FEnabled := False;
  FTriList.Free;
  FRotList.Free;
  FDirList.Free;
  FPosList.Free;
  FDirection.Free;
  inherited Destroy;
end;

// FriendlyName
//
class function TGLBExplosionFX.FriendlyName: string;
begin
  Result := 'ExplosionFx';
end;

// FriendlyDescription
//
class function TGLBExplosionFX.FriendlyDescription: string;
begin
  Result := 'Explosion FX';
end;

// SetTriList
//
procedure TGLBExplosionFx.SetTriList(Value: TAffineVectorList);
begin
  FTriList.Assign(Value);
end;

// SetRotList
//
procedure TGLBExplosionFx.SetRotList(Value: TAffineVectorList);
begin
  FRotList.Assign(Value);
end;

// SetDirList
//
procedure TGLBExplosionFx.SetDirList(Value: TAffineVectorList);
begin
  FDirList.Assign(Value);
end;

// SetPosList
//
procedure TGLBExplosionFx.SetPosList(Value: TAffineVectorList);
begin
  FPosList.Assign(Value);
end;

// SetDirection
//
procedure TGLBExplosionFx.SetDirection(Value: TGLCoordinates);
begin
  Value.Normalize;
  FDirection.Assign(Value);
end;

// SetEnabled
//
procedure TGLBExplosionFx.SetEnabled(Value: boolean);
begin
  FEnabled := Value;
end;

// Reset
//
procedure TGLBExplosionFx.Reset;
begin
  FEnabled := False;
  FStep := 0;
  FTriList.Clear;
  FRotList.Clear;
  FDirList.Clear;
  FPosList.Clear;
  FFaceCount := 0;
end;

// CacheInfo
//
procedure TGLBExplosionFx.CacheInfo;
var
  Face: integer;
  p1, p2, p3, v1, v2, posi: TAffineVector;
  Normal: TVector;

begin
  // make sure we can explode this object
  if not OwnerBaseSceneObject.InheritsFrom(TGLBaseMesh) then begin
    FEnabled := False;
    Exit;
  end;
  FTriList.Free;
  // get all the triangles of all the meshObjects
  FTriList := TGLBaseMesh(OwnerBaseSceneObject).MeshObjects.ExtractTriangles;
  FaceCount := FTriList.Count div 3;
  // set initial direction, rotation and position
  for Face := 0 to Facecount - 1 do begin
  // get the vertices of the triangle
    SetVector(p1, FTriList.Items[Face * 3]);
    SetVector(p2, FTriList.Items[Face * 3 + 1]);
    SetVector(p3, FTriList.Items[Face * 3 + 2]);
  // if the direction property is a null vector, than the direction is
  // given by the normal of the face
    if VectorEquals(FDirection.AsVector, NullHmgVector) then begin
      v1 := VectorSubtract(p2, p1);
      v2 := VectorSubtract(p2, p3);
      NormalizeVector(v1); // use of procedure is faster: PhP
      NormalizeVector(v2); // use of procedure is faster: PhP
      SetVector(Normal, VectorCrossProduct(v1, v2)); // use of procedure is faster: PhP
  // randomly rotate the normal vector so the faces are somewhat scattered
      case Random(3) of
        0: RotateVector(Normal, XVector, DegToRad(Random(45)));
        1: RotateVector(Normal, YVector, DegToRad(Random(45)));
        2: RotateVector(Normal, ZVector, DegToRad(Random(45)));
      end;
      NormalizeVector(Normal);
      FDirList.Add(Normal);
    end
    else
      FDirList.Add(FDirection.AsVector);
  // calculate the center (position) of the triangle so it rotates around its center
    posi[0] := (p1[0] + p2[0] + p3[0]) / 3;
    posi[1] := (p1[1] + p2[1] + p3[1]) / 3;
    posi[2] := (p1[2] + p2[2] + p3[2]) / 3;
    FPosList.add(posi);
  // random rotation (in degrees)
    FRotList.Add(DegToRad(Random(3)), DegToRad(Random(3)), DegToRad(Random(3)));
  end;
  // Dispose the struture of the mesh
  TGLBaseMesh(OwnerBaseSceneObject).MeshObjects.Clear;
  TGLBaseMesh(OwnerBaseSceneObject).StructureChanged;
end;

// Render
//
procedure TGLBExplosionFX.Render(sceneBuffer : TGLSceneBuffer; var rci : TRenderContextInfo);
var
  Face: integer;
  dir, p1, p2, p3: TAffineVector;
  mat: TMatrix;

begin
  if not FEnabled then
    Exit;
  // cache de list of vertices
  if FTriList.Count <= 0 then begin
    CacheInfo;
    if not FEnabled then
      Exit;
  end;
  // render explosion
  glDisable(GL_CULL_FACE);
  glBegin(GL_TRIANGLES);
  for Face := 0 to FaceCount - 1 do begin
    SetVector(p1, FTriList.Items[Face * 3]);
    SetVector(p2, FTriList.Items[Face * 3 + 1]);
    SetVector(p3, FTriList.Items[Face * 3 + 2]);
  // rotate the face
    mat := IdentityHmgMatrix;
    mat := MatrixMultiply(mat, CreateRotationMatrixX(FRotList.Items[face][0]));
    mat := MatrixMultiply(mat, CreateRotationMatrixY(FRotList.Items[face][1]));
    mat := MatrixMultiply(mat, CreateRotationMatrixZ(FRotList.Items[face][2]));
    SubtractVector(p1, FPosList.Items[Face]);  // use of procedure is faster: PhP
    SubtractVector(p2, FPosList.Items[Face]);  // -''-
    SubtractVector(p3, FPosList.Items[Face]);  // -''-
    p1 := VectorTransform(p1, mat);
    p2 := VectorTransform(p2, mat);
    p3 := VectorTransform(p3, mat);
    AddVector(p1, FPosList.Items[Face]);  // use of procedure is faster: PhP
    AddVector(p2, FPosList.Items[Face]);  // -''-
    AddVector(p3, FPosList.Items[Face]);  // -''-
  // move the face in the direction it is heading
    SetVector(dir, FDirList.Items[Face]);
    glNormal3f(dir[0], dir[1], dir[2]);
    ScaleVector(dir, Speed);
    AddVector(p1, dir);
    AddVector(p2, dir);
    AddVector(p3, dir);
  // also, move the center of the face
    FPosList.Items[Face] := VectorAdd(FPosList.Items[Face], dir);

  // save the changes
    FTrilist.Items[face * 3] := p1;
    FTrilist.Items[face * 3 +1] := p2;
    FTrilist.Items[face * 3 +2] := p3;

    glVertex3f(p1[0], p1[1], p1[2]);
    glVertex3f(p2[0], p2[1], p2[2]);
    glVertex3f(p3[0], p3[1], p3[2]);
  end;
  glEnd;
  glEnable(GL_CULL_FACE);
  if FMaxSteps <> 0 then begin
    Inc(FStep);
    if FStep = FMaxSteps then
      FEnabled := False;
  end;
end;

initialization
	// class registrations
	RegisterXCollectionItemClass(TGLBExplosionFX);

end.
