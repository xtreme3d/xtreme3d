{: GLMultiPolygon<p>

   Object with support for complex polygons.<p>

	<b>History : </b><font size=-1><ul>
      <li>18/11/04 - SG - Fixed TGLMultiPolygonBase.Destroy memory leak (Neil)
      <li>05/09/03 - EG - TNotifyCollection moved to GLMisc
      <li>14/07/02 - EG - Code cleanups, dropped 'absolutes', fixed mem leaks
      <li>28/12/01 - EG - Added registration (Philipp Pammler)
      <li>19/12/01 - EG - Removed dependency to contnrs (D4 compatibility,
                           TObjectList replaced with TPersistentObjectList)
      <li>29/03/01 - Uwe - Fixes and improvements to TGLMultiPolygon
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>08/01/01 - EG - Compatibility fix (TGLLineNodes change),
                           Delphi 4 compatibility (removed TVectorPool) and
                           added/renamed some properties, various fixes
      <li>08/10/00 - EG - Added header, code contributed by Uwe Raabe
   </ul>
}
{ TODO

  ur:

  And I reactivated the TVectorPool object. The VectorLists are not suitable for this job.
  When the tesselator finds an intersection of edges it wants us to give him some storage
  for this new vertex, and he wants a pointer (see tessCombine). The pointers taken from
  TAffineVectorList become invalid after enlarging the capacity (makes a ReAllocMem), which
  can happen implicitly while adding. The TVectorPool keeps all pointers valid until the
  destruction itself.

  If anyone feels responsible: it would be fine to have a method ImportFromFile (dxf?) in
  the TGLContour and TGLMultiPolygonBase objects...
}
unit GLMultiPolygon;

interface

uses
   Classes, OpenGL1x, Spline, VectorGeometry, VectorLists, PersistentClasses,
   GLScene, GLObjects, GLMisc, GLTexture, GLGeomObjects;

type

   // TGLContourNodes
   //
   TGLContourNodes = class (TGLNodes)
      public
	      { Public Declarations }
         procedure NotifyChange; override;
   end;

   // TGLContour
   //
   TGLContour = class (TCollectionItem)
      private
         FNodes: TGLContourNodes;
         FDivision : Integer;
         FSplineMode : TLineSplineMode;
         FDescription: String;
         procedure SetNodes(const Value: TGLContourNodes);
         procedure SetDivision(Value: Integer);
         procedure SetSplineMode(const Value: TLineSplineMode);
         procedure SetDescription(const Value: String);

      protected
         procedure CreateNodes; dynamic;
         procedure NodesChanged(Sender:TObject);
         function GetDisplayName : String; override;

      public
         constructor Create(Collection : TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

      published
         { Published Declarations }
         property Description: String read FDescription write SetDescription;
         {: The nodes list.<p> }
         property Nodes : TGLContourNodes read FNodes write SetNodes;
         {: Number of divisions for each segment in spline modes.<p>
           Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p>
           This mode is used only for the curve, not for the rotation path. }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;
   end;

   TGLContourClass = class of TGLContour;

   // TGLContours
   //
   TGLContours = class (TNotifyCollection)
      private
         function GetItems(index: Integer): TGLContour;
         procedure SetItems(index: Integer; const Value: TGLContour);
      protected

      public
         constructor Create(AOwner : TComponent); overload;
         function Add: TGLContour;
         function FindItemID(ID: Integer): TGLContour;
         property Items[index : Integer] : TGLContour read GetItems write SetItems; default;
   end;

   // TPolygonList
   //
   TPolygonList = class (TPersistentObjectList)
      private
         FAktList : TAffineVectorList;
         function GetList(I : Integer) : TAffineVectorList;

      public
         procedure Add;
         property AktList : TAffineVectorList read FAktList;
         property List[I : Integer] : TAffineVectorList read GetList;
   end;

   // TMultiPolygonBase
   //
   {: Multipolygon is defined with multiple contours.<p>
      The contours have to be in the X-Y plane, otherwise they are projected
      to it (this is done automatically by the tesselator).<br>
      The plane normal is pointing in +Z. All contours are automatically closed,
      so there is no need to specify the last node equal to the first one.<br>
      Contours should be defined counterclockwise, the first contour (index = 0)
      is taken as is, all following are reversed. This means you can define the
      outer contour first and the holes and cutouts after that. If you give the
      following contours in clockwise order, the first contour is extended.<p>

      TMultiPolygonBase will take the input contours and let the tesselator
      make an outline from it (this is done in RetreiveOutline). This outline is
      used for Rendering. Only when there are changes in the contours, the
      outline will be recalculated. The ouline in fact is a list of VectorLists. }
   TMultiPolygonBase = class (TGLSceneObject)
      private
         { Private Declarations }
         FContours : TGLContours;
         FOutline : TPolygonList;
         FContoursNormal : TAffineVector;
         procedure SetContours(const Value: TGLContours);
         function GetPath(i: Integer): TGLContourNodes;
         procedure SetPath(i: Integer; const value: TGLContourNodes);
         function GetOutline : TPolygonList;
         procedure SetContoursNormal(const Value: TAffineVector);

      protected
         { Protected Declarations }
         procedure RenderTesselatedPolygon(textured : Boolean;
                             normal : PAffineVector; invertNormals: Boolean);
         procedure RetrieveOutline(List:TPolygonList);
         procedure ContourChanged(Sender:TObject); virtual;
         //property PNormal:PAffineVector read FPNormal;

      public
         { Public Declarations }
         constructor Create(AOwner:TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         procedure AddNode(const i : Integer; const coords : TGLCoordinates); overload;
         procedure AddNode(const i : Integer; const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const i : Integer; const value : TVector); overload;
         procedure AddNode(const i : Integer; const value : TAffineVector); overload;

         property Path[i : Integer] : TGLContourNodes read GetPath write SetPath;
         property Outline : TPolygonList read GetOutline;
         property ContoursNormal : TAffineVector read FContoursNormal write SetContoursNormal;

      published
         { Published Declarations }
         property Contours : TGLContours read FContours write SetContours;
   end;

   // TGLMultiPolygon
   //
   {: A polygon that can have holes and multiple contours.<p>
      Use the Path property to access a contour or one of the AddNode methods
      to add a node to a contour (contours are allocated automatically). }
   TGLMultiPolygon = class (TMultiPolygonBase)
      private
         { Private Declarations }
         FParts: TPolygonParts;

      protected
         { Protected Declarations }
         procedure SetParts(const value : TPolygonParts);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;

         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

      published
         { Published Declarations }
         property Parts : TPolygonParts read FParts write SetParts default [ppTop, ppBottom];
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, XOpenGL;

type
  { page oriented pointer array, with persistent pointer target memory.
    In TVectorList a pointer to a vector will not be valid any more after
    a call to SetCapacity, which might be done implicitely during Add.
    The TVectorPool keeps memory in its original position during its
    whole lifetime. }

  // removed Notify (only D5)
  // added Destroy (also working with D4)
  TVectorPool = class (TList)
  private
    FEntrySize   : Integer;     // size of each entry
    FPageSize    : Integer;     // number of entries per page
    FArrSize     : Integer;     // size of one page
    FUsedEntries : Integer;     // used entries in actual page
    FAktArray    : VectorGeometry.PByteArray;  // pointer to actual page
    procedure CreatePage;       // create new page
  public
    constructor Create(APageSize, AEntrySize:Integer);
    destructor Destroy; override;

    { retrieve pointer to new entry. will create new page if needed }
    procedure GetNewVector(var P: Pointer);
  end;

{ TVectorPool }

constructor TVectorPool.Create(APageSize, AEntrySize: Integer);
begin
  inherited Create;
  Assert(APageSize>0);
  Assert(AEntrySize>0);
  FPageSize := APageSize;
  FEntrySize := AEntrySize;
  FArrSize := FPageSize*FEntrySize;
  CreatePage;
end;

procedure TVectorPool.CreatePage;
begin
  GetMem(FAktArray, FArrSize);
  Add(FAktArray);
  FUsedEntries := 0;
end;

destructor TVectorPool.Destroy;
var
  i : Integer;
begin
  for i:=Count-1 downto 0 do FreeMem(Items[i],FArrSize);
  inherited;
end;

procedure TVectorPool.GetNewVector(var P: Pointer);
begin
  if FUsedEntries>=FPageSize then CreatePage;
  Inc(FUsedEntries);
  P := @(FAktArray[(FUsedEntries-1)*FEntrySize]);
end;

// ------------------
// ------------------ TPolygonList ------------------
// ------------------

// Add
//
procedure TPolygonList.Add;
begin
   FAktList:=TAffineVectorList.Create;
   inherited Add(FAktList);
end;

// GetList
//
function TPolygonList.GetList(i : Integer): TAffineVectorList;
begin
   Result:=TAffineVectorList(Items[i]);
end;

// ------------------
// ------------------ TGLContour ------------------
// ------------------

constructor TGLContour.Create(Collection: TCollection);
begin
  inherited;
  CreateNodes;
  FDivision:=10;
  FSplineMode:=lsmLines;
end;

procedure TGLContour.CreateNodes;
begin
  FNodes:=TGLContourNodes.Create(Self);
end;

destructor TGLContour.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TGLContour.Assign(Source: TPersistent);
begin
  if Source is TGLContour then begin
    FNodes.Assign(TGLContour(Source).FNodes);
    FDivision := TGLContour(Source).FDivision;
    FSplineMode := TGLContour(Source).FSplineMode;
    FDescription := TGLContour(Source).FDescription;
  end else inherited;
end;

function TGLContour.GetDisplayName: String;
begin
  result := Description;
  if result='' then result := Format('GLContour: %d nodes',[Nodes.Count]);
end;

procedure TGLContour.NodesChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TGLContour.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TGLContour.SetDivision(Value: Integer);
begin
  if Value<1 then Value := 1;
  if Value<>FDivision then begin
    FDivision:=value;
    Changed(false);
  end;
end;

procedure TGLContour.SetNodes(const Value: TGLContourNodes);
begin
  FNodes.Assign(Value);
  Changed(false);
end;

procedure TGLContour.SetSplineMode(const Value: TLineSplineMode);
begin
  if FSplineMode<>value then begin
    FSplineMode:=value;
    Changed(false);
  end;
end;

{ TGLContours }

function TGLContours.Add: TGLContour;
begin
   Result := TGLContour(inherited Add);
end;

constructor TGLContours.Create(AOwner: TComponent);
begin
  Create(AOwner, TGLContour);
end;

function TGLContours.FindItemID(ID: Integer): TGLContour;
begin
  result := TGLContour(inherited FindItemId(Id));
end;

function TGLContours.GetItems(index: Integer): TGLContour;
begin
  result := TGLContour(inherited Items[index]);
end;

procedure TGLContours.SetItems(index: Integer; const Value: TGLContour);
begin
  inherited Items[index] := value;
end;

{ TMultiPolygonBase }

// Create
//
constructor TMultiPolygonBase.Create(AOwner: TComponent);
begin
   inherited;
   FContours:=TGLContours.Create(Self);
   FContours.OnNotifyChange:=ContourChanged;
   FContoursNormal:=AffineVectorMake(0,0,1);
end;

// Destroy
//
destructor TMultiPolygonBase.Destroy;
begin
   FOutline.CleanFree;
   FContours.Free;
   inherited;
end;

// Assign
//
procedure TMultiPolygonBase.Assign(Source: TPersistent);
begin
   if Source is TMultiPolygonBase then begin
      FContours.Assign(TMultiPolygonBase(Source).FContours);
   end;
   inherited;
end;

// ContourChanged
//
procedure TMultiPolygonBase.ContourChanged(Sender: TObject);
begin
   if Assigned(FOutline) then begin
      // force a RetrieveOutline with next Render
      FOutline.CleanFree;
      FOutline:=nil;
      StructureChanged;
   end;
end;

// AddNode (vector)
//
procedure TMultiPolygonBase.AddNode(const i : Integer; const value : TVector);
begin
   Path[i].AddNode(value);
end;

// AddNode (float)
//
procedure TMultiPolygonBase.AddNode(const i : Integer; const x, y, z : TGLfloat);
begin
   Path[i].AddNode(x, y, z);
end;

// AddNode (coords)
//
procedure TMultiPolygonBase.AddNode(const i : Integer; const coords : TGLCoordinates);
begin
   Path[i].AddNode(coords);
end;

// AddNode (affine vector)
//
procedure TMultiPolygonBase.AddNode(const I: Integer; const value: TAffineVector);
begin
   Path[i].AddNode(value);
end;

// Assign
//
procedure TMultiPolygonBase.SetContours(const Value: TGLContours);
begin
   FContours.Assign(Value);
end;

// GetOutline
//
function TMultiPolygonBase.GetOutline : TPolygonList;
begin
   if not Assigned(FOutline) then begin
      FOutline:=TPolygonList.Create;
      RetrieveOutline(FOutline);
   end;
   Result:=FOutline;
end;

// GetContour
//
function TMultiPolygonBase.GetPath(i : Integer): TGLContourNodes;
begin
   Assert(i>=0);
   while i>=Contours.Count do
      Contours.Add;
   Result:=Contours[i].Nodes;
end;

// SetContour
//
procedure TMultiPolygonBase.SetPath(i : Integer; const value: TGLContourNodes);
begin
   Assert(i>=0);
   while i>=Contours.Count do
      Contours.Add;
   Contours[i].Nodes.Assign(value);
end;

//
// Tessellation routines (OpenGL callbacks)
//

var
   vVertexPool : TVectorPool;

procedure tessError(errno : TGLEnum); stdcall;
begin
   Assert(False, IntToStr(errno)+' : '+gluErrorString(errno));
end;

procedure tessIssueVertex(vertexData : Pointer); stdcall;
begin
   xglTexCoord2fv(vertexData);
   glVertex3fv(vertexData);
end;

procedure tessCombine(coords : PDoubleVector; vertex_data : Pointer;
                      weight : PGLFloat; var outData : Pointer); stdcall;
begin
   vVertexPool.GetNewVector(outData);
   SetVector(PAffineVector(outData)^, coords[0], coords[1], coords[2]);
end;

procedure tessBeginList(typ : TGLEnum; polygonData : Pointer); stdcall;
begin
   TPolygonList(polygonData).Add;
end;

procedure tessIssueVertexList(vertexData : Pointer; polygonData : Pointer); stdcall;
begin
   TPolygonList(polygonData).AktList.Add(PAffineVector(vertexData)^);
end;

procedure TMultiPolygonBase.RetrieveOutline(List: TPolygonList);
var
   i, n : Integer;
   tess : PGLUTesselator;

   procedure TesselatePath(contour:TGLContour; inverted:Boolean);

      procedure IssueVertex(v : TAffineVector);
      var
         dblVector : TAffineDblVector;
         p : PAffineVector;
      begin
         vVertexPool.GetNewVector(Pointer(p));
         p^:=v;
         SetVector(dblVector, v);
         gluTessVertex(tess, dblVector, p);
      end;

   var
      i, n : Integer;
      spline : TCubicSpline;
      f : Single;
      splineDivisions : Integer;
      nodes : TGLContourNodes;
   begin
      gluTessBeginContour(tess);
      nodes:=contour.Nodes;
      if contour.SplineMode=lsmLines then
         splineDivisions:=0
      else splineDivisions:=contour.Division;
      if splineDivisions>1 then begin
         spline:=nodes.CreateNewCubicSpline;
         try
            f:=1/splineDivisions;
            n:=splineDivisions*(nodes.Count-1);
            if inverted then begin
               for i:=n downto 0 do
                  IssueVertex(spline.SplineAffineVector(i*f))
            end else begin
               for i:=0 to n do
                  IssueVertex(spline.SplineAffineVector(i*f));
            end;
        finally
           spline.Free;
        end;
      end else begin
         n:=nodes.Count-1;
         if inverted then begin
            for i:=n downto 0 do
               IssueVertex(nodes[i].AsAffineVector)
         end else begin
            for i:=0 to n do
               IssueVertex(nodes[i].AsAffineVector);
         end;
      end;
      gluTessEndContour(tess);
   end;

begin
   List.Clear;
   if (Contours.Count>0) and (Path[0].Count>2) then begin
      // Vertex count
      n:=0;
      for i:=0 to Contours.Count-1 do
         n:=n+Path[i].Count;
      // Create and initialize the GLU tesselator
      vVertexPool:=TVectorPool.Create(n, SizeOf(TAffineVector));
      tess:=gluNewTess;
      try
         // register callbacks
         gluTessCallback(tess, GLU_TESS_BEGIN_DATA, @tessBeginList);
         gluTessCallback(tess, GLU_TESS_END_DATA, nil);
         gluTessCallback(tess, GLU_TESS_VERTEX_DATA, @tessIssueVertexList);
         gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
         gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);

         // issue normal
         gluTessNormal(tess, FContoursNormal[0], FContoursNormal[1], FContoursNormal[2]);

         // set properties
         gluTessProperty(Tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
         gluTessProperty(Tess, GLU_TESS_BOUNDARY_ONLY, GL_TRUE);

         gluTessBeginPolygon(tess, List);
         // outside contour
         TesselatePath(Contours[0], False);
         // inside contours
         for n:=1 to Contours.Count-1 do
            TesselatePath(Contours[n], True);
         gluTessEndPolygon(tess);
      finally
         gluDeleteTess(tess);
         vVertexPool.Free;
         vVertexPool:=nil;
      end;
   end;
end;

// RenderTesselatedPolygon
//
procedure TMultiPolygonBase.RenderTesselatedPolygon(textured : Boolean;
                                                normal : PAffineVector;
                                                invertNormals : Boolean);
var
  tess : PGLUTesselator;

  procedure IssueVertex(v:TAffineVector);
  var
    dblVector : TAffineDblVector;
    p : PAffineVector;
  begin
    vVertexPool.GetNewVector(Pointer(p));
    p^ := v;
    SetVector(dblVector, v);
    gluTessVertex(tess, dblVector, p);
  end;

var
  i,n : Integer;
begin
  // call to Outline will call RetrieveOutline if necessary
  if (Outline.Count=0) or (Outline.List[0].Count<2) then Exit;
  // Vertex count
  n:=0;
  for i:=0 to Outline.Count-1 do n:=n+Outline.List[i].Count;
  // Create and initialize a vertex pool and the GLU tesselator
  vVertexPool := TVectorPool.Create(n,Sizeof(TAffineVector));
  tess:=gluNewTess;
  try
    gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
    if textured then
      gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
    else gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
    gluTessCallback(tess, GLU_TESS_END, @glEnd);
    gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
    gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
    // Issue normal
    if Assigned(normal) then begin
      glNormal3fv(PGLFloat(normal));
      gluTessNormal(tess, normal[0], normal[1], normal[2]);
    end;
    gluTessProperty(Tess,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_POSITIVE);
    // Issue polygon
    gluTessBeginPolygon(tess, nil);
    for n:=0 to Outline.Count-1 do begin
      with Outline.List[n] do begin
        gluTessBeginContour(tess);
        if invertNormals then for i:=Count-1 downto 0 do IssueVertex(Items[i])
        else for i:=0 to Count-1 do IssueVertex(Items[i]);
        gluTessEndContour(tess);
      end;
    end;
    gluTessEndPolygon(tess);
  finally
    gluDeleteTess(tess);
    vVertexPool.Free;
    vVertexPool:=nil;
  end;
end;

// ------------------
// ------------------ TGLMultiPolygon ------------------
// ------------------

// Create
//
constructor TGLMultiPolygon.Create(AOwner: TComponent);
begin
   inherited;
   FParts:=[ppTop, ppBottom];
end;

// Assign
//
procedure TGLMultiPolygon.Assign(Source: TPersistent);
begin
  if Source is TGLMultiPolygon then begin
    FParts:=TGLMultiPolygon(Source).FParts;
  end;
  inherited;
end;

// BuildList
//
procedure TGLMultiPolygon.BuildList(var rci: TRenderContextInfo);
var
  normal : TAffineVector;
begin
  if (Outline.Count<1) then Exit;
  normal := ContoursNormal;
  // Render
  // tessellate top polygon
  if ppTop in FParts then
    RenderTesselatedPolygon(True, @normal, False);
  // tessellate bottom polygon
  if ppBottom in FParts then begin
    NegateVector(normal);
    RenderTesselatedPolygon(True, @normal, True)
  end;
end;

// SetParts
//
procedure TGLMultiPolygon.SetParts(const value : TPolygonParts);
begin
   if FParts<>value then begin
      FParts:=value;
      StructureChanged;
   end;
end;

// SetContoursNormal
//
procedure TMultiPolygonBase.SetContoursNormal(const Value: TAffineVector);
begin
  FContoursNormal := Value;
end;

// ------------------
// ------------------ TGLContourNodes ------------------
// ------------------

// NotifyChange
//
procedure TGLContourNodes.NotifyChange;
begin
   if (GetOwner<>nil) then
      (GetOwner as TGLContour).Changed(False);
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClass(TGLMultiPolygon);

end.

