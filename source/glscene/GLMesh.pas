// GLMesh
{: Raw Mesh support in GLScene.<p>

   This unit is for simple meshes and legacy support, GLVectorFileObjects
   implements more efficient (though more complex) mesh tools.<p> 

	<b>History : </b><font size=-1><ul>
      <li>06/07/02 - EG - Mesh vertex lock only performed if context is active
      <li>18/03/02 - EG - Color "leak" fix (Nelson Chu)
      <li>21/01/02 - EG - TVertexList.OnNotifyChange now handled
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>30/01/01 - EG - Added VertexList locking
      <li>19/07/00 - EG - Introduced enhanced mesh structure
      <li>11/07/00 - EG - Just discovered and made use of "fclex" :)
	   <li>18/06/00 - EG - Creation from split of GLObjects,
                          TVertexList now uses TVertexData,
                          Rewrite of TGLMesh.CalcNormals (smaller & faster)
	</ul></font>
}
unit GLMesh;

interface

uses Classes, GLMisc, GLScene, VectorGeometry, OpenGL1x, GLTexture, GLState;

type

   TVertexData = packed record
      textCoord : TTexPoint;
      color : TVector;
      normal : TAffineVector;
      coord : TVertex;
   end;
   PVertexData = ^TVertexData;
   TVertexDataArray = array [0..(MAXINT shr 6)] of TVertexData;
   PVertexDataArray = ^TVertexDataArray;

	// TVertexList
	//
   {: Stores an interlaced vertex list for direct use in OpenGL.<p>
      Locking (hardware passthrough) is supported, see "Locked" property for details. } 
	TVertexList = class(TGLUpdateAbleObject)
		private
			{ Private Declarations }
			FValues : PVertexDataArray;
			FCount  : Integer;
         FCapacity, FGrowth : Integer;
         FLockedOldValues : PVertexDataArray;

      protected
			{ Protected Declarations }
         procedure SetCapacity(const val : Integer);
         procedure SetGrowth(const val : Integer);
         procedure Grow;
         procedure SetVertices(index : Integer; const val : TVertexData);
         function GetVertices(index : Integer) : TVertexData;
         procedure SetVertexCoord(index : Integer; const val : TAffineVector);
         function GetVertexCoord(index : Integer) : TAffineVector;
         procedure SetVertexNormal(index : Integer; const val : TAffineVector);
         function GetVertexNormal(index : Integer) : TAffineVector;
         procedure SetVertexTexCoord(index : Integer; const val : TTexPoint);
         function GetVertexTexCoord(index : Integer) : TTexPoint;

         function GetFirstEntry: PGLFloat;
         function GetFirstColor: PGLFLoat;
         function GetFirstNormal: PGLFLoat;
         function GetFirstVertex: PGLFLoat;
         function GetFirstTexPoint: PGLFLoat;

         function GetLocked : Boolean;
         procedure SetLocked(val : Boolean);

      public
			{ Public Declarations }
         constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         function CreateInterpolatedCoords(list2 : TVertexList; lerpFactor : Single) : TVertexList;

         {: Adds a vertex to the list, fastest method. }
         procedure AddVertex(const vertexData : TVertexData); overload;
         {: Adds a vertex to the list, fastest method for adding a triangle. }
         procedure AddVertex3(const vd1, vd2, vd3 : TVertexData); overload;
         {: Adds a vertex to the list.<p>
            Use the NullVector, NullHmgVector or NullTexPoint constants for
            params you don't want to set. }
         procedure AddVertex(const aVertex : TVertex; const aNormal : TAffineVector;
                             const aColor: TColorVector; const aTexPoint: TTexPoint); overload;
         {: Adds a vertex to the list, no texturing version.<p> }
         procedure AddVertex(const vertex: TVertex; const normal: TAffineVector;
                             const color: TColorVector); overload;
         {: Adds a vertex to the list, no texturing, not color version.<p> }
         procedure AddVertex(const vertex: TVertex; const normal: TAffineVector); overload;
         {: Duplicates the vertex of given index and adds it at the end of the list. }
         procedure DuplicateVertex(index : Integer);

         procedure Assign(Source: TPersistent); override;
         procedure Clear;

         property Vertices[index : Integer] : TVertexData read GetVertices write SetVertices; default;
         property VertexCoord[index : Integer] : TAffineVector read GetVertexCoord write SetVertexCoord;
         property VertexNormal[index : Integer] : TAffineVector read GetVertexNormal write SetVertexNormal;
         property VertexTexCoord[index : Integer] : TTexPoint read GetVertexTexCoord write SetVertexTexCoord;
         property Count : Integer read FCount;
         {: Capacity of the list (nb of vertex).<p>
            Use this to allocate memory quickly before calling AddVertex. }
         property Capacity : Integer read FCapacity write SetCapacity;
         {: Vertex capacity that will be added each time the list needs to grow.<p>
            default value is 256 (chunks of approx 13 kb). }
         property Growth : Integer read FGrowth write SetGrowth;

         {: Calculates the sum of all vertex coords }
         function SumVertexCoords : TAffineVector;
         {: Calculates the extents of the vertice coords. }
         procedure GetExtents(var min, max : TAffineVector);
         {: Normalizes all normals. }
         procedure NormalizeNormals;
         {: Translate all coords by given vector }
         procedure Translate(const v : TAffineVector);

         procedure DefineOpenGLArrays;

         property FirstColor: PGLFloat read GetFirstColor;
         property FirstEntry: PGLFLoat read GetFirstEntry;
         property FirstNormal: PGLFloat read GetFirstNormal;
         property FirstVertex: PGLFloat read GetFirstVertex;
         property FirstTexPoint: PGLFloat read GetFirstTexPoint;

         {: Locking state of the vertex list.<p>
            You can "Lock" a list to increase rendering performance on some
            OpenGL implementations (NVidia's). A Locked list size shouldn't be
            changed and calculations should be avoided.<br>
            Performance can only be gained from a lock for osDirectDraw object,
            ie. meshes that are updated for each frame (the default build list
            mode is faster on static meshes).<br>
            Be aware that the "Locked" state enforcement is not very strict
            to avoid performance hits, and GLScene may not always notify you
            that you're doing things you shouldn't on a locked list! } 
         property Locked : Boolean read GetLocked write SetLocked;
         procedure EnterLockSection;
         procedure LeaveLockSection;
   end;

   // TGLMesh
   //
   {: Basic mesh object.<p>
      Each mesh holds a set of vertices and a Mode value defines how they make
      up the mesh (triangles, strips...) }
   TGLMesh = class(TGLSceneObject)
      private
			{ Private Declarations }
         FVertices   : TVertexList;
         FMode       : TMeshMode;
         FVertexMode : TVertexMode;

      protected
			{ Protected Declarations }
         procedure SetMode(AValue: TMeshMode);
         procedure SetVertices(AValue: TVertexList);
         procedure SetVertexMode(AValue: TVertexMode);

         procedure VerticesChanged(Sender : TObject);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure CalcNormals(Frontface: TFaceWinding);
         property Vertices: TVertexList read FVertices write SetVertices;

      published
			{ Published Declarations }
         property Mode: TMeshMode read FMode write SetMode;
         property VertexMode: TVertexMode read FVertexMode write SetVertexMode default vmVNCT;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLStrings, XOpenGL, GLContext;

//----------------- TVertexList ------------------------------------------------

constructor TVertexList.Create(AOwner: TPersistent);
begin
   inherited;
   FValues := nil;
   FCount := 0;
   FCapacity := 0;
   FGrowth := 256;
end;

// Destroy
//
destructor TVertexList.Destroy;
begin
   Locked:=False;
   FreeMem(FValues);
   inherited;
end;

// CreateInterpolatedCoords
//
function TVertexList.CreateInterpolatedCoords(list2 : TVertexList; lerpFactor : Single) : TVertexList;
var
   i : Integer;
begin
   Assert(Count=list2.Count);
   Result:=TVertexList.Create(nil);
   Result.Capacity:=Count;
   Move(FValues[0], Result.FValues[0], Count*SizeOf(TVertexData));
   // interpolate vertices
   for i:=0 to Count-1 do
      VectorLerp(FValues[i].coord, list2.FValues[i].coord,
                 lerpFactor, Result.FValues[i].coord);
end;

// SetCapacity
//
procedure TVertexList.SetCapacity(const val : Integer);
begin
   Assert(not Locked, 'Cannot change locked list capacity !');
   FCapacity:=val;
   if FCapacity<FCount then
      FCapacity:=FCount;
   ReallocMem(FValues, FCapacity*SizeOf(TVertexData));
end;

// SetGrowth
//
procedure TVertexList.SetGrowth(const val : Integer);
begin
   if val>16 then
      FGrowth:=val
   else FGrowth:=16;
end;

// Grow
//
procedure TVertexList.Grow;
begin
   Assert(not Locked, 'Cannot add to a locked list !');
   FCapacity:=FCapacity+FGrowth;
   ReallocMem(FValues, FCapacity*SizeOf(TVertexData));
end;

// GetFirstColor
//
function TVertexList.GetFirstColor: PGLFLoat;
begin
   Result:=@(FValues[0].color);
end;

// GetFirstEntry
//
function TVertexList.GetFirstEntry: PGLFloat;
begin
   Result:=Pointer(FValues);
end;

// GetFirstNormal
//
function TVertexList.GetFirstNormal: PGLFLoat;
begin
   Result:=@(FValues[0].normal);
end;

// GetFirstVertex
//
function TVertexList.GetFirstVertex: PGLFLoat;
begin
   Result:=@(FValues[0].coord);
end;

// GetFirstTexPoint
//
function TVertexList.GetFirstTexPoint : PGLFLoat;
begin
   Result:=@(FValues[0].textCoord);
end;

// GetLocked
//
function TVertexList.GetLocked : Boolean;
begin
   Result:=Assigned(FLockedOldValues);
end;

// SetLocked
//
procedure TVertexList.SetLocked(val : Boolean);
var
   size : Integer;
begin
   if val<>Locked then begin
      // Only supported with NVidia's right now
      if GL_NV_vertex_array_range and (CurrentGLContext<>nil) then begin
         size:=FCount*SizeOf(TVertexData);
         if val then begin
            // Lock
            FLockedOldValues:=FValues;
            FValues:=wglAllocateMemoryNV(size, 0, 0, 0.5);
            if FValues=nil then begin
               FValues:=FLockedOldValues;
               FLockedOldValues:=nil;
            end else Move(FLockedOldValues^, FValues^, size);
         end else begin
            // Unlock
            wglFreeMemoryNV(FValues);
            FValues:=FLockedOldValues;
            FLockedOldValues:=nil;
         end;
      end;
   end;
end;

// EnterLockSection
//
procedure TVertexList.EnterLockSection;
begin
   if Locked then begin
      glVertexArrayRangeNV(FCount*SizeOf(TVertexData), FValues);
      glEnableClientState(GL_VERTEX_ARRAY_RANGE_NV);
   end;
end;

// LeaveLockSection
//
procedure TVertexList.LeaveLockSection;
begin
   if Locked then begin
      glDisableClientState(GL_VERTEX_ARRAY_RANGE_NV);
      glFlushVertexArrayRangeNV;
   end;
end;

// SetVertices
//
procedure TVertexList.SetVertices(index : Integer; const val : TVertexData);
begin
   Assert(Cardinal(index)<Cardinal(Count));
   FValues[index]:=val;
   NotifyChange(Self);
end;

// GetVertices
//
function TVertexList.GetVertices(index : Integer) : TVertexData;
begin
   Assert(Cardinal(index)<Cardinal(Count));
   Result:=FValues[index];
end;

// SetVertexCoord
//
procedure TVertexList.SetVertexCoord(index : Integer; const val : TAffineVector);
begin
   FValues[index].coord:=val;
   NotifyChange(Self);
end;

// GetVertexCoord
//
function TVertexList.GetVertexCoord(index : Integer) : TAffineVector;
begin
   Result:=FValues[index].coord;
end;

// SetVertexNormal
//
procedure TVertexList.SetVertexNormal(index : Integer; const val : TAffineVector);
begin
   FValues[index].normal:=val;
   NotifyChange(Self);
end;

// GetVertexNormal
//
function TVertexList.GetVertexNormal(index : Integer) : TAffineVector;
begin
   Result:=FValues[index].normal;
end;

// SetVertexTexCoord
//
procedure TVertexList.SetVertexTexCoord(index : Integer; const val : TTexPoint);
begin
   FValues[index].textCoord:=val;
   NotifyChange(Self);
end;

// GetVertexTexCoord
//
function TVertexList.GetVertexTexCoord(index : Integer) : TTexPoint;
begin
   Result:=FValues[index].textCoord;
end;

// AddVertex (direct)
//
procedure TVertexList.AddVertex(const vertexData : TVertexData);
begin
   if FCount=FCapacity then Grow;
   FValues[FCount]:=vertexData;
   Inc(FCount);
   NotifyChange(Self);
end;

// AddVertex3
//
procedure TVertexList.AddVertex3(const vd1, vd2, vd3 : TVertexData);
begin
   // extend memory space
   if FCount+2>=FCapacity then Grow;
   // calculate destination address for new vertex data
   FValues[FCount]:=vd1;
   FValues[FCount+1]:=vd2;
   FValues[FCount+2]:=vd3;
   Inc(FCount, 3);
   NotifyChange(Self);
end;

// AddVertex (texturing)
//
procedure TVertexList.AddVertex(const aVertex: TVertex; const aNormal: TAffineVector;
                                const aColor: TColorVector; const aTexPoint: TTexPoint);
begin
   if FCount=FCapacity then Grow;
   // calculate destination address for new vertex data
   with FValues[FCount] do begin
      textCoord:=aTexPoint;
      color:=aColor;
      normal:=aNormal;
      coord:=aVertex;
   end;
   Inc(FCount);
   NotifyChange(Self);
end;

// AddVertex (no texturing)
//
procedure TVertexList.AddVertex(const vertex: TVertex; const normal: TAffineVector;
                                const color: TColorVector);
begin
   AddVertex(vertex, normal, color, NullTexPoint);
end;

// AddVertex (no texturing, no color)
//
procedure TVertexList.AddVertex(const vertex: TVertex; const normal: TAffineVector);
begin
   AddVertex(vertex, normal, clrBlack, NullTexPoint);
end;

// DuplicateVertex
//
procedure TVertexList.DuplicateVertex(index : Integer);
begin
   Assert(Cardinal(index)<Cardinal(Count));
   if FCount=FCapacity then Grow;
   FValues[FCount]:=FValues[index];
   Inc(FCount);
   NotifyChange(Self);
end;

// Clear
//
procedure TVertexList.Clear;
begin
   Assert(not Locked, 'Cannot clear a locked list !'); 
   FreeMem(FValues);
   FCount := 0;
   FCapacity := 0;
   FValues := nil;
   NotifyChange(Self);
end;

// SumVertexCoords
//
function TVertexList.SumVertexCoords : TAffineVector;
var
   i : Integer;
begin
   Result:=NullVector;
   for i:=0 to Count-1 do
      AddVector(Result, FValues[i].coord);
end;

// GetExtents
//
procedure TVertexList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      with FValues[i] do for k:=0 to 2 do begin
         f:=coord[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// NormalizeNormals
//
procedure TVertexList.NormalizeNormals;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      NormalizeVector(FValues[i].coord);
end;

// Translate
//
procedure TVertexList.Translate(const v : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AddVector(FValues[i].coord, v);
end;

// DefineOpenGLArrays
//
procedure TVertexList.DefineOpenGLArrays;
begin
   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(3, GL_FLOAT, SizeOf(TVertexData)-SizeOf(TAffineVector), FirstVertex);
   glEnableClientState(GL_NORMAL_ARRAY);
   glNormalPointer(GL_FLOAT, SizeOf(TVertexData)-SizeOf(TAffineVector), FirstNormal);
   xglEnableClientState(GL_TEXTURE_COORD_ARRAY);
   xglTexCoordPointer(2, GL_FLOAT, SizeOf(TVertexData)-SizeOf(TTexPoint), FirstTexPoint);
end;

// Assign
//
procedure TVertexList.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TVertexList) then begin
      FCount := TVertexList(Source).FCount;
      FCapacity := FCount;
      ReallocMem(FValues, FCount*SizeOf(TVertexData));
      Move(TVertexList(Source).FValues^, FValues^, FCount*SizeOf(TVertexData));
   end else inherited Assign(Source);
end;

//----------------- TGLMesh ------------------------------------------------------

// Create
//
constructor TGLMesh.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
//  ObjectStyle:=ObjectStyle+[osDirectDraw];
  FVertices := TVertexList.Create(Self);
  FVertices.AddVertex(XVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(YVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(ZVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.OnNotifyChange:=VerticesChanged;
  FVertexmode := vmVNCT; //should change this later to default to vmVN. But need to
end;                     //change GLMeshPropform so that it greys out unused vertex info

// Destroy
//
destructor TGLMesh.Destroy;
begin
   FVertices.Free;
   inherited Destroy;
end;

// VerticesChanged
//
procedure TGLMesh.VerticesChanged(Sender : TObject);
begin
   StructureChanged;
end;

// BuildList
//
procedure TGLMesh.BuildList(var rci : TRenderContextInfo);
var
   VertexCount : Longint;
begin
   inherited;
   if osDirectDraw in ObjectStyle then
      FVertices.EnterLockSection;
   glPushAttrib(GL_POLYGON_BIT or GL_ENABLE_BIT or GL_CURRENT_BIT);
   case FVertexMode of
      vmV    : glInterleavedArrays(GL_V3F, SizeOf(TVertexData), FVertices.FirstVertex);
      vmVN   : glInterleavedArrays(GL_N3F_V3F, SizeOf(TVertexData), FVertices.FirstNormal);
      vmVNC  : glInterleavedArrays(GL_C4F_N3F_V3F, SizeOf(TVertexData), FVertices.FirstColor);
      vmVNT, vmVNCT : glInterleavedArrays(GL_T2F_C4F_N3F_V3F, 0, FVertices.FirstEntry);
      vmVT   : glInterleavedArrays(GL_T2F_V3F, 0, FVertices.FirstEntry);
   else
      Assert(False, glsInterleaveNotSupported);
   end;
   if FVertexMode in [vmVNC, vmVNCT] then begin
      glEnable(GL_COLOR_MATERIAL);
      glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
      rci.GLStates.ResetGLMaterialColors;
   end;
   VertexCount := FVertices.Count;
   case FMode of
      mmTriangleStrip : glDrawArrays(GL_TRIANGLE_STRIP, 0, VertexCount);
      mmTriangleFan   : glDrawArrays(GL_TRIANGLE_FAN, 0, VertexCount);
      mmTriangles     : glDrawArrays(GL_TRIANGLES, 0, VertexCount);
      mmQuadStrip     : glDrawArrays(GL_QUAD_STRIP, 0, VertexCount);
      mmQuads         : glDrawArrays(GL_QUADS, 0, VertexCount);
      mmPolygon       : glDrawArrays(GL_POLYGON, 0, VertexCount);
   else
      Assert(False);
   end;
   glPopAttrib;
   if osDirectDraw in ObjectStyle then
      FVertices.LeaveLockSection;
end;

// SetMode
//
procedure TGLMesh.SetMode(AValue: TMeshMode);
begin
   if AValue <> FMode then begin
      FMode := AValue;
      StructureChanged;
   end;
end;

// SetVertices
//
procedure TGLMesh.SetVertices(AValue: TVertexList);
begin
   if AValue <> FVertices then begin
      FVertices.Assign(AValue);
      StructureChanged;
   end;
end;

// SetVertexMode
//
procedure TGLMesh.SetVertexMode(AValue: TVertexMode);
begin
   if AValue<>FVertexMode then begin
      FVertexMode := AValue;
      StructureChanged;
   end;
end;

// CalcNormals
//
procedure TGLMesh.CalcNormals(Frontface: TFaceWinding);
var
   vn  : TAffineFltVector;
   i, j : Integer;
begin
   case FMode of
      mmTriangleStrip:
         with Vertices do for i:=0 to Count-3 do begin
            if (FrontFace=fwCounterClockWise) xor ((i and 1)=0) then
               vn:=CalcPlaneNormal(FValues[i+0].coord, FValues[i+1].coord, FValues[i+2].coord)
            else vn:=CalcPlaneNormal(FValues[i+2].coord, FValues[i+1].coord, FValues[i+0].coord);
            FValues[i].Normal:=vn;
         end;
      mmTriangles:
         with Vertices do for i:=0 to ((Count-3) div 3) do begin
            j:=i*3;
            if FrontFace=fwCounterClockWise then
               vn:=CalcPlaneNormal(FValues[j+0].coord, FValues[j+1].coord, FValues[j+2].coord)
            else vn:=CalcPlaneNormal(FValues[j+2].coord, FValues[j+1].coord, FValues[j+0].coord);
            FValues[j+0].normal:=vn;
            FValues[j+1].normal:=vn;
            FValues[j+2].normal:=vn;
         end;
      mmQuads:
         with Vertices do for I := 0 to ((Count-4) div 4) do begin
            j:=i*4;
            if FrontFace=fwCounterClockWise then
               vn:=CalcPlaneNormal(FValues[j+0].coord, FValues[j+1].coord, FValues[j+2].coord)
            else vn:=CalcPlaneNormal(FValues[j+2].coord, FValues[j+1].coord, FValues[j+0].coord);
            FValues[j+0].normal:=vn;
            FValues[j+1].normal:=vn;
            FValues[j+2].normal:=vn;
            FValues[j+3].normal:=vn;
         end;
      mmPolygon:
         with Vertices do if Count>2 then begin
            if FrontFace=fwCounterClockWise then
               vn:=CalcPlaneNormal(FValues[0].coord, FValues[1].coord, FValues[2].coord)
            else vn:=CalcPlaneNormal(FValues[2].coord, FValues[1].coord, FValues[0].coord);
            for i:=0 to Count-1 do
              FValues[i].normal:=vn;
         end;
   else
      Assert(False);
   end;
   // clear fpu exception flag
   asm fclex end;
   StructureChanged;
end;

// Assign
//
procedure TGLMesh.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TGLMesh) then begin
      FVertices.Assign(TGLMesh(Source).Vertices);
      FMode := TGLMesh(Source).FMode;
      FVertexMode := TGLMesh(Source).FVertexMode;
   end else inherited Assign(Source);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TGLMesh]);

end.

