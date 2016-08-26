{: GLODESkeletonColliders<p>

   Skeleton colliders for defining and controlling ODE geoms.<p>

   <b>History :</b><font size=-1><ul>
     <li>04/12/03 - SG - Creation.
   </ul></font>
}
unit GLODESkeletonColliders;

interface

uses
  Classes, PersistentClasses, VectorGeometry, GLVectorFileObjects,
  dynode;

type
  
  // TSCODEBase
  //
  {: Base ODE skeleton collider class. }
  TSCODEBase = class(TSkeletonCollider)
    private
      FGeom : PdxGeom;

    public
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;

      procedure AddToSpace(Space : PdxSpace); virtual;
      procedure AlignCollider; override;

      {: The geoms are created through the AddToSpace procedure. }
      property Geom : PdxGeom read FGeom;
  end;

  // TSCODESphere
  //
  {: Sphere shaped ODE geom in a skeleton collider. }
  TSCODESphere = class(TSCODEBase)
    private
      FRadius : Single;

    protected
      procedure SetRadius(const val : Single);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToSpace(Space : PdxSpace); override;

      property Radius : Single read FRadius write SetRadius;
  end;

  // TSCODECCylinder
  //
  {: Capsule (sphere capped cylinder) shaped ODE geom in a skeleton 
     collider. }
  TSCODECCylinder = class(TSCODEBase)
    private
      FRadius,
      FLength : Single;

    protected
      procedure SetRadius(const val : Single);
      procedure SetLength(const val : Single);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToSpace(Space : PdxSpace); override;

      property Radius : Single read FRadius write SetRadius;
      property Length : Single read FLength write SetLength;
  end;

  // TSCODEBox
  //
  {: Box shaped ODE geom in a skeleton collider. }
  TSCODEBox = class(TSCODEBase)
    private
      FBoxWidth,
      FBoxHeight,
      FBoxDepth : Single;

    protected
      procedure SetBoxWidth(const val : Single);
      procedure SetBoxHeight(const val : Single);
      procedure SetBoxDepth(const val : Single);

    public
      constructor Create; override;
      procedure WriteToFiler(writer : TVirtualWriter); override;
      procedure ReadFromFiler(reader : TVirtualReader); override;
      procedure AddToSpace(Space : PdxSpace); override;

      property BoxWidth : Single read FBoxWidth write SetBoxWidth;
      property BoxHeight : Single read FBoxHeight write SetBoxHeight;
      property BoxDepth : Single read FBoxDepth write SetBoxDepth;
  end;

{: After loading call this function to add all the geoms in a
   skeleton collider list to a given ODE space. }
procedure AddSCODEGeomsToODESpace(
  colliders : TSkeletonColliderList; space : PdxSpace);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ Global methods ------------------
// ------------------

// AddSCODEGeomsToODESpace
//
procedure AddSCODEGeomsToODESpace(
  colliders : TSkeletonColliderList; space : PdxSpace);
var
  i : Integer;
begin
  for i:=0 to colliders.Count-1 do
    if colliders[i] is TSCODEBase then
      TSCODEBase(Colliders[i]).AddToSpace(space);
end;

// ------------------
// ------------------ TSCODEBase ------------------
// ------------------

// WriteToFiler
//
procedure TSCODEBase.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
  end;
end;

// ReadFromFiler
//
procedure TSCODEBase.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do
    // Nothing yet
  else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODEBase.AddToSpace(Space : PdxSpace);
begin
  AlignCollider;
end;

// AlignCollider
//
procedure TSCODEBase.AlignCollider;
var
  R : TdMatrix3;
  Mat : TMatrix;
begin
  inherited;
  if Assigned(FGeom) then begin
    Mat:=GlobalMatrix;
    dGeomSetPosition(FGeom,Mat[3][0],Mat[3][1],Mat[3][2]);
    R[0]:=Mat[0][0]; R[1]:=Mat[1][0]; R[2]:= Mat[2][0]; R[3]:= 0;
    R[4]:=Mat[0][1]; R[5]:=Mat[1][1]; R[6]:= Mat[2][1]; R[7]:= 0;
    R[8]:=Mat[0][2]; R[9]:=Mat[1][2]; R[10]:=Mat[2][2]; R[11]:=0;
    dGeomSetRotation(FGeom,R);
  end;
end;


// ------------------
// ------------------ TSCODESphere ------------------
// ------------------

// Create
//
constructor TSCODESphere.Create;
begin
  inherited;
  FRadius:=0.5;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCODESphere.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
  end;
end;

// ReadFromFiler
//
procedure TSCODESphere.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do
    Radius:=ReadFloat
  else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODESphere.AddToSpace(Space : PdxSpace);
begin
  FGeom:=dCreateSphere(Space, FRadius);
  inherited;
end;

// SetRadius
//
procedure TSCODESphere.SetRadius(const val : Single);
begin
  if val<>FRadius then begin
    FRadius:=val;
    if Assigned(FGeom) then
      dGeomSphereSetRadius(Geom, TdReal(FRadius));
  end;
end;


// ------------------
// ------------------ TSCODECCylinder ------------------
// ------------------

// Create
//
constructor TSCODECCylinder.Create;
begin
  inherited;
  FRadius:=0.5;
  FLength:=1;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCODECCylinder.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FRadius);
    WriteFloat(FLength);
  end;
end;

// ReadFromFiler
//
procedure TSCODECCylinder.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do begin
    Radius:=ReadFloat;
    Length:=ReadFloat;
  end else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODECCylinder.AddToSpace(Space : PdxSpace);
begin
  FGeom:=dCreateCCylinder(Space,FRadius,FLength);
  inherited;
end;

// SetRadius
//
procedure TSCODECCylinder.SetRadius(const val : Single);
begin
  if val<>FRadius then begin
    FRadius:=val;
    if Assigned(FGeom) then
      dGeomCcylinderSetParams(FGeom,FRadius,FLength);
  end;
end;

// SetLength
//
procedure TSCODECCylinder.SetLength(const val : Single);
begin
  if val<>FLength then begin
    FLength:=val;
    if Assigned(FGeom) then
      dGeomCcylinderSetParams(FGeom,FRadius,FLength);
  end;
end;

// ------------------
// ------------------ TSCODEBox ------------------
// ------------------

// Create
//
constructor TSCODEBox.Create;
begin
  inherited;
  FBoxWidth:=1;
  FBoxHeight:=1;
  FBoxDepth:=1;
  AlignCollider;
end;

// WriteToFiler
//
procedure TSCODEBox.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FBoxWidth);
    WriteFloat(FBoxHeight);
    WriteFloat(FBoxDepth);
  end;
end;

// ReadFromFiler
//
procedure TSCODEBox.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion : integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion:=reader.ReadInteger;
  if archiveVersion=0 then with reader do begin
    BoxWidth:=ReadFloat;
    BoxHeight:=ReadFloat;
    BoxDepth:=ReadFloat;
  end else RaiseFilerException(archiveVersion);
end;

// AddToSpace
//
procedure TSCODEBox.AddToSpace(Space : PdxSpace);
begin
  FGeom:=dCreateBox(Space, FBoxWidth, FBoxHeight, FBoxDepth);
  inherited;
end;

// SetBoxWidth
//
procedure TSCODEBox.SetBoxWidth(const val : Single);
begin
  if val<>FBoxWidth then begin
    FBoxWidth:=val;
    if Assigned(FGeom) then
      dGeomBoxSetLengths(Geom,
        TdReal(FBoxWidth),
        TdReal(FBoxHeight),
        TdReal(FBoxDepth));
  end;
end;

// SetBoxHeight
//
procedure TSCODEBox.SetBoxHeight(const val : Single);
begin
  if val<>FBoxHeight then begin
    FBoxHeight:=val;
    if Assigned(FGeom) then
      dGeomBoxSetLengths(Geom,
        TdReal(FBoxWidth),
        TdReal(FBoxHeight),
        TdReal(FBoxDepth));
  end;
end;

// SetBoxDepth
//
procedure TSCODEBox.SetBoxDepth(const val : Single);
begin
  if val<>FBoxDepth then begin
    FBoxDepth:=val;
    if Assigned(FGeom) then
      dGeomBoxSetLengths(Geom,
        TdReal(FBoxWidth),
        TdReal(FBoxHeight),
        TdReal(FBoxDepth));
  end;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterClasses([TSCODEBase,TSCODESphere,TSCODECCylinder,TSCODEBox]);

end.
