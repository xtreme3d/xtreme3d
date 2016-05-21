//
// This unit is part of the GLScene Project, http://glscene.org
//
{: MeshUtils.<p>

   General utilities for mesh manipulations.<p>

	<b>History : </b><font size=-1><ul>

      <li>29/07/03 - PVD - Fixed bug in RemapReferences limiting lists to 32768 items   
      <li>29/07/03 - SG - Fixed small bug in ConvertStripToList (indexed vectors variant)
      <li>05/03/03 - EG - Added RemapIndicesToIndicesMap
      <li>20/01/03 - EG - Added UnifyTrianglesWinding
      <li>15/01/03 - EG - Added ConvertStripToList, ConvertIndexedListToList
      <li>13/01/03 - EG - Added InvertTrianglesWinding, BuildNonOrientedEdgesList,
                          SubdivideTriangles 
      <li>10/03/02 - EG - Added WeldVertices, RemapTrianglesIndices and IncreaseCoherency
      <li>04/11/01 - EG - Optimized RemapAndCleanupReferences and BuildNormals
      <li>02/11/01 - EG - BuildVectorCountOptimizedIndices three times faster,
                          StripifyMesh slightly faster
	   <li>18/08/01 - EG - Creation
	</ul></font>
}
unit MeshUtils;

interface

uses Classes,PersistentClasses, VectorLists, VectorGeometry;

{: Converts a triangle strips into a triangle list.<p>
   Vertices are added to list, based on the content of strip. Both non-indexed
   and indexed variants are available, the output is *always* non indexed. }
procedure ConvertStripToList(const strip : TAffineVectorList;
                             list : TAffineVectorList); overload;
procedure ConvertStripToList(const strip : TIntegerList;
                             list : TIntegerList); overload;
procedure ConvertStripToList(const strip : TAffineVectorList;
                             const indices : TIntegerList;
                             list : TAffineVectorList); overload;

{: Expands an indexed structure into a non-indexed structure. }
procedure ConvertIndexedListToList(const data : TAffineVectorList;
                                   const  indices : TIntegerList;
                                   list : TAffineVectorList);

{: Builds a vector-count optimized indices list.<p>
   The returned list (to be freed by caller) contains an "optimized" indices
   list in which duplicates coordinates in the original vertices list are used
   only once (the first available duplicate in the list is used).<br>
   The vertices list is left untouched, to remap/cleanup, you may use the
   RemapAndCleanupReferences function. }
function BuildVectorCountOptimizedIndices(const vertices : TAffineVectorList;
                                          const normals : TAffineVectorList = nil;
                                          const texCoords : TAffineVectorList = nil) : TIntegerList;

{: Alters a reference array and removes unused reference values.<p>
   This functions scans the reference list and removes all values that aren't
   referred in the indices list, the indices list is *not* remapped. }
procedure RemapReferences(reference : TAffineVectorList;
                          const indices : TIntegerList); overload;
procedure RemapReferences(reference : TIntegerList;
                          const indices : TIntegerList); overload;
{: Alters a reference/indice pair and removes unused reference values.<p>
   This functions scans the reference list and removes all values that aren't
   referred in the indices list, and the indices list is remapped so as to remain
   coherent. }
procedure RemapAndCleanupReferences(reference : TAffineVectorList;
                                    indices : TIntegerList);
{: Creates an indices map from a remap list.<p>
   The remap list is what BuildVectorCountOptimizedIndices, a list of indices
   to distinct/unique items, the indices map contains the indices of these items
   after a remap and cleanup of the set referred by remapIndices... Clear?<br>
   In short it takes the output of BuildVectorCountOptimizedIndices and can change
   it to something suitable for RemapTrianglesIndices.<br>
   Any simpler documentation of this function welcome ;) }
function RemapIndicesToIndicesMap(remapIndices : TIntegerList) : TIntegerList;

{: Remaps a list of triangles vertex indices and remove degenerate triangles.<p>
   The indicesMap provides newVertexIndex:=indicesMap[oldVertexIndex] }
procedure RemapTrianglesIndices(indices, indicesMap : TIntegerList);
{: Remaps a list of indices.<p>
   The indicesMap provides newVertexIndex:=indicesMap[oldVertexIndex] }
procedure RemapIndices(indices, indicesMap : TIntegerList);

{: Attempts to unify triangle winding.<p>
   Depending on topology, this may or may not be successful (some topologies
   can't be unified, f.i. those that have duplicate triangles, those that
   have edges shared by more than two triangles, those that have unconnected
   submeshes etc.) }
procedure UnifyTrianglesWinding(indices : TIntegerList);
{: Inverts the triangles winding (vertex order). }
procedure InvertTrianglesWinding(indices : TIntegerList);

{: Builds normals for a triangles list.<p>
   Builds one normal per reference vertex (may be NullVector is reference isn't
   used), which is the averaged for normals of all adjacent triangles.<p>
   Returned list must be freed by caller. }
function BuildNormals(reference : TAffineVectorList;
                      indices : TIntegerList) : TAffineVectorList;

{: Builds a list of non-oriented (non duplicated) edges list.<p>
   Each edge is represented by the two integers of its vertices,
   sorted in ascending order.<br>
   If not nil, triangleEdges is filled with the 3 indices of the 3 edges
   of the triangle, the edges ordering respecting the original triangle
   orientation. }
function BuildNonOrientedEdgesList(triangleIndices : TIntegerList;
                                   triangleEdges : TIntegerList = nil;
                                   edgesTriangles : TIntegerList = nil) : TIntegerList;

{: Welds all vertices separated by a distance inferior to weldRadius.<p>
   Any two vertices whose distance is inferior to weldRadius will be merged
   (ie. one of them will be removed, and the other replaced by the barycenter).<p>
   The indicesMap is constructed to allow remapping of indices lists with the
   simple rule: newVertexIndex:=indicesMap[oldVertexIndex].<p>
   The logic is protected from chain welding, and only vertices that were
   initially closer than weldRadius will be welded in the same resulting vertex.<p>
   This procedure can be used for mesh simplification, but preferably at design-time
   for it is not optimized for speed. This is more a "fixing" utility for meshes
   exported from high-polycount CAD tools (to remove duplicate vertices,
   quantification errors, etc.) }
procedure WeldVertices(vertices : TAffineVectorList;
                       indicesMap : TIntegerList;
                       weldRadius : Single);

{: Attempts to create as few as possible triangle strips to cover the mesh.<p>
   The indices parameters define a set of triangles as a set of indices to
   vertices in a vertex pool, free of duplicate vertices (or resulting
   stripification will be of lower quality).<br>
   The function returns a list of TIntegerList, each of these lists hosting
   a triangle strip, returned objects must be freed by caller.<br>
   If agglomerateLoneTriangles is True, the first of the lists actually contains
   the agglomerated list of the triangles that couldn't be stripified. }
function StripifyMesh(indices : TIntegerList; maxVertexIndex : Integer;
                      agglomerateLoneTriangles : Boolean = False) : TPersistentObjectList;
{: Increases indices coherency wrt vertex caches.<p>
   The indices parameters is understood as vertex indices of a triangles set,
   the triangles are reordered to maximize coherency (vertex reuse) over the
   cacheSize latest indices. This allows higher rendering performance from
   hardware renderers that implement vertex cache (nVidia GeForce family f.i.),
   allowing reuse of T&amp;L performance (similar to stripification without
   the normals issues of strips).<p>
   This procedure performs a coherency optimization via a greedy hill-climber
   algorithm (ie. not optimal but fast). }
procedure IncreaseCoherency(indices : TIntegerList; cacheSize : Integer);

type
   TSubdivideEdgeEvent = procedure (const idxA, idxB, newIdx : Integer); register;

{: Subdivides mesh triangles.<p>
   Splits along edges, each triangle becomes four. The smoothFactor can be
   used to control subdivision smoothing, zero means no smoothing (tesselation
   only), while 1 means "sphere" subdivision (a low res sphere will be subdivided
   in a higher-res sphere), values outside of the [0..1] range are for, er,
   artistic purposes.<p>
   The procedure is not intended for real-time use. }
procedure SubdivideTriangles(smoothFactor : Single;
                             vertices : TAffineVectorList;
                             triangleIndices : TIntegerList;
                             normals : TAffineVectorList = nil;
                             onSubdivideEdge : TSubdivideEdgeEvent = nil);



// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

var
   v0to255reciproquals : array of Single;

// Get0to255reciproquals
//
function Get0to255reciproquals : PSingleArray;
var
   i : Integer;
begin
   if Length(v0to255reciproquals)<>256 then begin
      SetLength(v0to255reciproquals, 256);
      for i:=1 to 255 do
         v0to255reciproquals[i]:=1/i;
   end;
   Result:=@v0to255reciproquals[0];
end;

// ConvertStripToList (non-indexed vectors variant)
//
procedure ConvertStripToList(const strip : TAffineVectorList;
                             list : TAffineVectorList);
var
   i : Integer;
   stripList : PAffineVectorArray;
begin
   list.AdjustCapacityToAtLeast(list.Count+3*(strip.Count-2));
   stripList:=strip.List;
   for i:=0 to strip.Count-3 do begin
      if (i and 1)=0 then
         list.Add(stripList[i+0], stripList[i+1], stripList[i+2])
      else list.Add(stripList[i+2], stripList[i+1], stripList[i+0]);
   end;
end;

// ConvertStripToList (indices)
//
procedure ConvertStripToList(const strip : TIntegerList;
                             list : TIntegerList);
var
   i : Integer;
   stripList : PIntegerArray;
begin
   list.AdjustCapacityToAtLeast(list.Count+3*(strip.Count-2));
   stripList:=strip.List;
   for i:=0 to strip.Count-3 do begin
      if (i and 1)=0 then
         list.Add(stripList[i+0], stripList[i+1], stripList[i+2])
      else list.Add(stripList[i+2], stripList[i+1], stripList[i+0]);
   end;
end;

// ConvertStripToList (indexed vectors variant)
//
procedure ConvertStripToList(const strip : TAffineVectorList;
                             const indices : TIntegerList;
                             list : TAffineVectorList);
var
   i : Integer;
   stripList : PAffineVectorArray;
begin
   list.AdjustCapacityToAtLeast(list.Count+3*(indices.Count-2));
   stripList:=strip.List;
   for i:=0 to indices.Count-3 do begin
      if (i and 1)=0 then
         list.Add(stripList[indices[i+0]],
                  stripList[indices[i+1]],
                  stripList[indices[i+2]])
      else list.Add(stripList[indices[i+2]],
                    stripList[indices[i+1]],
                    stripList[indices[i+0]])
   end;
end;

// ConvertIndexedListToList
//
procedure ConvertIndexedListToList(const data : TAffineVectorList;
                                   const indices : TIntegerList;
                                   list : TAffineVectorList);
var
   i : Integer;
   indicesList : PIntegerArray;
   dataList, listList : PAffineVectorArray;
   oldResetMem : Boolean;
begin
   Assert(data<>list); // this is not allowed

   oldResetMem:=list.SetCountResetsMemory;
   list.SetCountResetsMemory:=False;

   list.Count:=indices.Count;

   list.SetCountResetsMemory:=oldResetMem;

   indicesList:=indices.List;
   dataList:=data.List;
   listList:=list.List;

   for i:=0 to indices.Count-1 do
      listList[i]:=dataList[indicesList[i]];
end;

// BuildVectorCountOptimizedIndices
//
function BuildVectorCountOptimizedIndices(const vertices : TAffineVectorList;
                                          const normals : TAffineVectorList = nil;
                                          const texCoords : TAffineVectorList = nil) : TIntegerList;
var
   i, j, k : Integer;
   found : Boolean;
   hashSize : Integer;
   hashTable : array of TIntegerlist;
   list : TIntegerList;
   verticesList, normalsList, texCoordsList : PAffineVectorArray;
const
   cVerticesPerHashKey = 48;
   cInvVerticesPerHashKey = 1/cVerticesPerHashKey;

   function HashKey(const v : TAffineVector; hashSize : Integer) : Integer;
   begin
      Result:=((    Integer(PIntegerArray(@v)[0])
                xor Integer(PIntegerArray(@v)[1])
                xor Integer(PIntegerArray(@v)[2])) shr 16) and hashSize;
   end;

begin
   Result:=TIntegerList.Create;
   Result.Capacity:=vertices.Count;

   if Assigned(normals) then begin
      Assert(normals.Count>=vertices.Count);
      normalsList:=normals.List
   end else normalsList:=nil;
   if Assigned(texCoords) then begin
      Assert(texCoords.Count>=vertices.Count);
      texCoordsList:=texCoords.List
   end else texCoordsList:=nil;

   verticesList:=vertices.List;

   // This method is very fast, at the price of memory requirement its
   // probable complexity is only O(n) (it's a kind of bucket-sort hellspawn)

   // Initialize data structures for a hash table
   // (each vertex will only be compared to vertices of similar hash value)
   hashSize:=(1 shl MaxInteger(Integer(0), Integer(Trunc(log2(vertices.Count*cInvVerticesPerHashKey)))))-1;
   if hashSize<7 then hashSize:=7;
   if hashSize>65535 then hashSize:=65535;
   SetLength(hashTable, hashSize+1);
   // allocate and fill our hashtable (will store "reference" vertex indices)
   for i:=0 to hashSize do begin
      hashTable[i]:=TIntegerList.Create;
      hashTable[i].GrowthDelta:=cVerticesPerHashKey div 2;
   end;
   // here we go for all vertices
   if Assigned(texCoordsList) or Assigned(normalsList) then begin
      for i:=0 to vertices.Count-1 do begin
         list:=hashTable[HashKey(verticesList[i], hashSize)];
         found:=False;
         // Check each vertex against its hashkey siblings
         if list.Count>0 then begin
            if Assigned(texCoordsList) then begin
               if Assigned(normalsList) then begin
                  for j:=0 to list.Count-1 do begin
                     k:=list.List[j];
                     if     VectorEquals(verticesList[k], verticesList[i])
                        and VectorEquals(normalsList[k], normalsList[i])
                        and VectorEquals(texCoordsList[k], texCoordsList[i]) then begin
                        // vertex known, just store its index
                        Result.Add(k);
                        found:=True;
                        Break;
                     end;
                  end;
               end else begin
                  for j:=0 to list.Count-1 do begin
                     k:=list.List[j];
                     if     VectorEquals(verticesList[k], verticesList[i])
                        and VectorEquals(texCoordsList[k], texCoordsList[i]) then begin
                        // vertex known, just store its index
                        Result.Add(k);
                        found:=True;
                        Break;
                     end;
                  end;
               end;
            end else begin
               for j:=0 to list.Count-1 do begin
                  k:=list.List[j];
                  if     VectorEquals(verticesList[k], verticesList[i])
                     and VectorEquals(normalsList[k], normalsList[i]) then begin
                     // vertex known, just store its index
                     Result.Add(k);
                     found:=True;
                     Break;
                  end;
               end;
            end;
         end;
         if not found then begin
            // vertex unknown, store index and add to the hashTable's list
            list.Add(i);
            Result.Add(i);
         end;
      end;
   end else begin
      for i:=0 to vertices.Count-1 do begin
         list:=hashTable[HashKey(verticesList[i], hashSize)];
         found:=False;
         // Check each vertex against its hashkey siblings
         for j:=0 to list.Count-1 do begin
            k:=list.List[j];
            if VectorEquals(verticesList[k], verticesList[i]) then begin
               // vertex known, just store its index
               Result.Add(k);
               found:=True;
               Break;
            end;
         end;
         if not found then begin
            // vertex unknown, store index and add to the hashTable's list
            list.Add(i);
            Result.Add(i);
         end;
      end;
   end;
   // free hash data
   for i:=0 to hashSize do
      hashTable[i].Free;
   SetLength(hashTable, 0);
end;

// RemapReferences (vectors)
//
procedure RemapReferences(reference : TAffineVectorList;
                          const indices : TIntegerList);
var
   i : Integer;
   tag : array of Byte;
   refListI, refListN : PAffineVector;
   indicesList : PIntegerArray;
begin
   Assert(reference.Count=indices.Count);
   SetLength(tag, reference.Count);
   indicesList:=indices.List;
   // 1st step, tag all used references
   for i:=0 to indices.Count-1 do
      Tag[indicesList[i]]:=1;
   // 2nd step, build remap indices and cleanup references
   refListI:=@reference.List[0];
   refListN:=refListI;
   for i:=0 to High(tag) do begin
      if tag[i]<>0 then begin
         if refListN<>refListI then
            refListN^:=refListI^;
         Inc(refListN);
      end;
      Inc(refListI);
   end;
   reference.Count:=(Integer(refListN)-Integer(@reference.List[0])) div SizeOf(TAffineVector);
end;

// RemapReferences (integers)
//
procedure RemapReferences(reference : TIntegerList;
                          const indices : TIntegerList);
var
   i, n : Integer;
   tag : array of Byte;
   refList : PIntegerArray;
   indicesList : PIntegerArray;
begin
   Assert(reference.Count=indices.Count);
   SetLength(tag, reference.Count);
   indicesList:=indices.List;
   // 1st step, tag all used references
   for i:=0 to indices.Count-1 do
      Tag[indicesList[i]]:=1;
   // 2nd step, build remap indices and cleanup references
   n:=0;
   refList:=reference.List;
   for i:=0 to High(tag) do begin
      if tag[i]<>0 then begin
         if n<>i then
            refList[n]:=refList[i];
         Inc(n);
      end;
   end;
   reference.Count:=n;
end;

// RemapAndCleanupReferences
//
procedure RemapAndCleanupReferences(reference : TAffineVectorList;
                                    indices : TIntegerList);
var
   i, n : Integer;
   tag : array of Integer;
   refList : PAffineVectorArray;
   indicesList : PIntegerArray;
begin
   Assert(reference.Count=indices.Count);
   SetLength(tag, reference.Count);
   indicesList:=indices.List;
   // 1st step, tag all used references
   for i:=0 to indices.Count-1 do
      tag[indicesList[i]]:=1;
   // 2nd step, build remap indices and cleanup references
   n:=0;
   refList:=reference.List;
   for i:=0 to High(tag) do begin
      if tag[i]<>0 then begin
         tag[i]:=n;
         if n<>i then    
            refList[n]:=refList[i];
         Inc(n);
      end;
   end;
   reference.Count:=n;
   // 3rd step, remap indices
   for i:=0 to indices.Count-1 do
      indicesList[i]:=tag[indicesList[i]];
end;

// RemapIndicesToIndicesMap
//
function RemapIndicesToIndicesMap(remapIndices : TIntegerList) : TIntegerList;
var
   i, n : Integer;
   tag : array of Integer;
   remapList, indicesMap : PIntegerArray;
begin
   SetLength(tag, remapIndices.Count);
   // 1st step, tag all used indices
   remapList:=remapIndices.List;
   for i:=0 to remapIndices.Count-1 do
      tag[remapList[i]]:=1;
   // 2nd step, build indices offset table
   n:=0;
   for i:=0 to remapIndices.Count-1 do begin
      if tag[i]>0 then begin
         tag[i]:=n;
         Inc(n);
      end;
   end;
   // 3rd step, fillup indices map
   Result:=TIntegerList.Create;
   Result.Count:=remapIndices.Count;
   indicesMap:=Result.List;
   for i:=0 to Result.Count-1 do
      indicesMap[i]:=tag[remapList[i]];
end;

// RemapTrianglesIndices
//
procedure RemapTrianglesIndices(indices, indicesMap : TIntegerList);
var
   i, k, a, b, c, n : Integer;
begin
   Assert((indices.Count mod 3)=0); // must be a multiple of 3
   n:=indices.Count;
   i:=0;
   k:=0;
   while i<n do begin
      a:=indicesMap[indices[i]];
      b:=indicesMap[indices[i+1]];
      c:=indicesMap[indices[i+2]];
      if (a<>b) and (b<>c) and (a<>c) then begin
         indices[k]:=a;
         indices[k+1]:=b;
         indices[k+2]:=c;
         Inc(k, 3);
      end;
      Inc(i, 3);
   end;
   indices.Count:=k;
end;

// RemapIndices
//
procedure RemapIndices(indices, indicesMap : TIntegerList);
var
   i : Integer;
   map, ind : PIntegerArray;
begin
   ind:=indices.List;
   map:=indicesMap.List;
   for i:=0 to indices.Count-1 do
      ind[i]:=map[ind[i]];
end;

// UnifyTrianglesWinding
//
procedure UnifyTrianglesWinding(indices : TIntegerList);
var
   nbTris : Integer;
   mark : array of ByteBool;     // marks triangles that have been processed
   triangleStack : TIntegerList; // marks triangles winded, that must be processed

   procedure TestRewind(a, b : Integer);
   var
      i, n : Integer;
      x, y, z : Integer;
   begin
      i:=indices.Count-3;
      n:=nbTris-1;
      while i>0 do begin
         if not mark[n] then begin
            x:=indices[i];
            y:=indices[i+1];
            z:=indices[i+2];
            if ((x=a) and (y=b)) or ((y=a) and (z=b)) or ((z=a) and (x=b)) then begin
               indices.Exchange(i, i+2);
               mark[n]:=True;
               triangleStack.Push(n);
            end else if ((x=b) and (y=a)) or ((y=b) and (z=a)) or ((z=b) and (x=a)) then begin
               mark[n]:=True;
               triangleStack.Push(n);
            end;
         end;
         Dec(i, 3);
         Dec(n);
      end;
   end;

   procedure ProcessTriangleStack;
   var
      n, i : Integer;
   begin
      while triangleStack.Count>0 do begin
         // get triangle, it is *assumed* properly winded
         n:=triangleStack.Pop;
         i:=n*3;
         mark[n]:=True;
         // rewind neighbours
         TestRewind(indices[i+0], indices[i+1]);
         TestRewind(indices[i+1], indices[i+2]);
         TestRewind(indices[i+2], indices[i+0]);
      end;
   end;

var
   n : Integer;
begin
   nbTris:=indices.Count div 3;
   SetLength(mark, nbTris);
   // Build connectivity data
   triangleStack:=TIntegerList.Create;
   try
      triangleStack.Capacity:=nbTris div 4;
      // Pick a triangle, adjust normals of neighboring triangles, recurse
      for n:=0 to nbTris-1 do begin
         if mark[n] then Continue;
         triangleStack.Push(n);
         ProcessTriangleStack;
      end;
   finally
      triangleStack.Free;
   end;
end;

// InvertTrianglesWinding
//
procedure InvertTrianglesWinding(indices : TIntegerList);
var
   i : Integer;
begin
   Assert((indices.Count mod 3)=0);
   i:=indices.Count-3;
   while i>=0 do begin
      indices.Exchange(i, i+2);
      Dec(i, 3);
   end;
end;

// BuildNormals
//
function BuildNormals(reference : TAffineVectorList;
                      indices : TIntegerList) : TAffineVectorList;
var
   i, n, k : Integer;
   normalsCount : array of Byte;
   v : TAffineVector;
   refList, resultList : PAffineVectorArray;
   indicesList : PIntegerArray;
   reciproquals : PSingleArray;
begin
   Result:=TAffineVectorList.Create;
   Result.Count:=reference.Count;
   SetLength(normalsCount, reference.Count);
   refList:=reference.List;
   indicesList:=indices.List;
   resultList:=Result.List;
   // 1st step, calculate triangle normals and sum
   i:=0; while i<indices.Count do begin
      v:=CalcPlaneNormal(refList[indicesList[i]],
                         refList[indicesList[i+1]],
                         refList[indicesList[i+2]]);
      for n:=i to i+2 do begin
         k:=indicesList[n];
         AddVector(resultList[k], v);
         Inc(normalsCount[k]);
      end;
      Inc(i, 3);
   end;
   // 2nd step, average normals
   reciproquals:=Get0to255reciproquals;
   for i:=0 to reference.Count-1 do
      ScaleVector(resultList[i], reciproquals[normalsCount[i]]);
end;

{: Builds a list of non-oriented (non duplicated) edges list.<p>
   Each edge is represented by the two integers of its vertices,
   sorted in ascending order.<p>
   If not nil, triangleEdges is filled with the 3 indices of the 3 edges
   of the triangle, the edges ordering respecting the original triangle
   orientation.<p>
   If not nil, edgesTriangles is filled with the indices of the first index
   of the triangle in triangleIndices that have this edge. A maximum of two
   triangles can be referred by this list, and its final size will be that
   of the Result (ie. non oriented edges list). }
// BuildNonOrientedEdgesList
//
function BuildNonOrientedEdgesList(triangleIndices : TIntegerList;
                                   triangleEdges : TIntegerList = nil;
                                   edgesTriangles : TIntegerList = nil) : TIntegerList;
const
   cEdgesHashMax = 127; // must be a power of two minus 1
var
   edgesHash : array [0..cEdgesHashMax] of TIntegerList;
   curTri : Integer;
   edges : TIntegerList;

   function ProcessEdge(a, b : Integer) : Integer;
   var
      i, n : Integer;
      hashKey : Integer;
      edgesList, iList : PIntegerArray;
      hashList : TIntegerList;
   begin
      if a>=b then begin
         i:=a;
         a:=b;
         b:=i;
      end;
      hashKey:=(a xor b) and cEdgesHashMax;
      hashList:=edgesHash[hashKey];
      edgesList:=edges.List;
      iList:=hashList.List;
      for i:=0 to hashList.Count-1 do begin
         n:=iList[i];
         if (edgesList[n]=a) and (edgesList[n+1]=b) then begin
            Result:=n;
            Exit;
         end;
      end;
      Result:=edges.Count;
      hashList.Add(Result);
      edges.Add(a, b);
   end;

   function ProcessEdge2(a, b : Integer) : Integer;
   var
      n : Integer;
      hashKey : Integer;
      edgesList : PIntegerArray;
      iList, iListEnd : PInteger;
      hashList : TIntegerList;
   begin
      if a>=b then begin
         n:=a;
         a:=b;
         b:=n;
      end;
      hashKey:=(a xor (b shl 1)) and cEdgesHashMax;
      edgesList:=edges.List;
      hashList:=edgesHash[hashKey];
      iList:=@hashList.List[0];
      iListEnd:=@hashList.List[hashList.Count];
      while Integer(iList)<Integer(iListEnd) do begin
         n:=iList^;
         if (edgesList[n]=a) and (edgesList[n+1]=b) then begin
            edgesTriangles[n+1]:=curTri;
            Result:=n;
            Exit;
         end;
         Inc(iList);
      end;
      Result:=edges.Count;
      hashList.Add(Result);
      edges.Add(a, b);
      edgesTriangles.Add(curTri, -1);
   end;

var
   j, k : Integer;
   triIndicesList : PIntegerArray;
begin
   Result:=TIntegerList.Create;
   Result.Capacity:=1024;
   Result.GrowthDelta:=1024;
   if Assigned(triangleEdges) then
      triangleEdges.Count:=triangleIndices.Count;
   if Assigned(edgesTriangles) then
      edgesTriangles.Count:=0;
   // Create Hash
   k:=(triangleIndices.Count div (cEdgesHashMax+1))+128;
   for j:=0 to High(edgesHash) do begin
      edgesHash[j]:=TIntegerList.Create;
      edgesHash[j].Capacity:=k;
   end;
   // collect all edges
   curTri:=0;
   triIndicesList:=triangleIndices.List;
   edges:=Result;
   if Assigned(triangleEdges) then begin
      if Assigned(edgesTriangles) then begin
         while curTri<triangleIndices.Count do begin
            triangleEdges[curTri  ]:=ProcessEdge2(triIndicesList[curTri  ], triIndicesList[curTri+1]);
            triangleEdges[curTri+1]:=ProcessEdge2(triIndicesList[curTri+1], triIndicesList[curTri+2]);
            triangleEdges[curTri+2]:=ProcessEdge2(triIndicesList[curTri+2], triIndicesList[curTri  ]);
            Inc(curTri, 3);
         end;
      end else begin
         while curTri<triangleIndices.Count do begin
            triangleEdges[curTri  ]:=ProcessEdge(triIndicesList[curTri  ], triIndicesList[curTri+1]);
            triangleEdges[curTri+1]:=ProcessEdge(triIndicesList[curTri+1], triIndicesList[curTri+2]);
            triangleEdges[curTri+2]:=ProcessEdge(triIndicesList[curTri+2], triIndicesList[curTri  ]);
            Inc(curTri, 3);
         end;
      end;
   end else begin
      if Assigned(edgesTriangles) then begin
         while curTri<triangleIndices.Count do begin
            ProcessEdge2(triIndicesList[curTri  ], triIndicesList[curTri+1]);
            ProcessEdge2(triIndicesList[curTri+1], triIndicesList[curTri+2]);
            ProcessEdge2(triIndicesList[curTri+2], triIndicesList[curTri  ]);
            Inc(curTri, 3);
         end;
      end else begin
         while curTri<triangleIndices.Count do begin
            ProcessEdge(triIndicesList[curTri  ], triIndicesList[curTri+1]);
            ProcessEdge(triIndicesList[curTri+1], triIndicesList[curTri+2]);
            ProcessEdge(triIndicesList[curTri+2], triIndicesList[curTri  ]);
            Inc(curTri, 3);
         end;
      end;
   end;
   // Destroy Hash
   for j:=0 to High(edgesHash) do
      edgesHash[j].Free;
end;

// IncreaseCoherency
//
procedure IncreaseCoherency(indices : TIntegerList; cacheSize : Integer);
var
   i, n, maxVertex, bestCandidate, bestScore, candidateIdx, lastCandidate : Integer;
   trisOfVertex : array of TIntegerList;
   candidates : TIntegerList;
   indicesList : PIntegerArray;
begin
   // Alloc lookup structure
   maxVertex:=indices.MaxInteger;
   SetLength(trisOfVertex, maxVertex+1);
   for i:=0 to High(trisOfVertex) do
      trisOfVertex[i]:=TIntegerList.Create;
   candidates:=TIntegerList.Create;
   indicesList:=PIntegerArray(indices.List);
   // Fillup lookup structure
   i:=0;
   while i<indices.Count do begin
      trisOfVertex[indicesList[i+0]].Add(i);
      trisOfVertex[indicesList[i+1]].Add(i);
      trisOfVertex[indicesList[i+2]].Add(i);
      Inc(i, 3);
   end;
   // Optimize
   i:=0;
   while i<indices.Count do begin
      n:=i-cacheSize;
      if n<0 then n:=0;
      candidates.Count:=0;
      while n<i do begin
         candidates.Add(trisOfVertex[indicesList[n]]);
         Inc(n);
      end;
      bestCandidate:=-1;
      if candidates.Count>0 then begin
         candidateIdx:=0;
         bestScore:=0;
         candidates.Sort;
         lastCandidate:=candidates.List[0];
         for n:=1 to candidates.Count-1 do begin
            if candidates.List[n]<>lastCandidate then begin
               if n-candidateIdx>bestScore then begin
                  bestScore:=n-candidateIdx;
                  bestCandidate:=lastCandidate;
               end;
               lastCandidate:=candidates.List[n];
               candidateIdx:=n;
            end;
         end;
         if candidates.Count-candidateIdx>bestScore then
            bestCandidate:=lastCandidate;
      end;
      if bestCandidate>=0 then begin
         trisOfVertex[indicesList[i+0]].Remove(i);
         trisOfVertex[indicesList[i+1]].Remove(i);
         trisOfVertex[indicesList[i+2]].Remove(i);
         trisOfVertex[indicesList[bestCandidate+0]].Remove(bestCandidate);
         trisOfVertex[indicesList[bestCandidate+1]].Remove(bestCandidate);
         trisOfVertex[indicesList[bestCandidate+2]].Remove(bestCandidate);
         trisOfVertex[indicesList[i+0]].Add(bestCandidate);
         trisOfVertex[indicesList[i+1]].Add(bestCandidate);
         trisOfVertex[indicesList[i+2]].Add(bestCandidate);
         indices.Exchange(bestCandidate+0, i+0);
         indices.Exchange(bestCandidate+1, i+1);
         indices.Exchange(bestCandidate+2, i+2);
      end else begin
         trisOfVertex[indicesList[i+0]].Remove(i);
         trisOfVertex[indicesList[i+1]].Remove(i);
         trisOfVertex[indicesList[i+2]].Remove(i);
      end;
      Inc(i, 3);
   end;
   // Release lookup structure
   candidates.Free;
   for i:=0 to High(trisOfVertex) do
      trisOfVertex[i].Free;
end;

// WeldVertices
//
procedure WeldVertices(vertices : TAffineVectorList;
                       indicesMap : TIntegerList;
                       weldRadius : Single);
var
   i, j, n, k : Integer;
   pivot : PAffineVector;
   sum : TAffineVector;
   wr2 : Single;
   mark : packed array of ByteBool;
begin
   indicesMap.Count:=vertices.Count;
   SetLength(mark, vertices.Count);
   wr2:=Sqr(weldRadius);
   // mark duplicates, compute barycenters and indicesMap
   i:=0;
   k:=0;
   while i<vertices.Count do begin
      if not mark[i] then begin
         pivot:=@vertices.List[i];
         indicesMap[i]:=k;
         n:=0;
         j:=vertices.Count-1;
         while j>i do begin
            if not mark[j] then begin
               if VectorDistance2(pivot^, vertices.List[j])<=wr2 then begin
                  if n=0 then begin
                     sum:=VectorAdd(pivot^, vertices.List[j]);
                     n:=2;
                  end else begin
                     AddVector(sum, vertices.List[j]);
                     Inc(n);
                  end;
                  indicesMap[j]:=k;
                  mark[j]:=True;
               end;
            end;
            Dec(j);
         end;
         if n>0 then
            vertices.List[i]:=VectorScale(sum, 1/n);
         Inc(k);
      end;
      Inc(i);
   end;
   // pack vertices list
   k:=0;
   for i:=0 to vertices.Count-1 do begin
      if not mark[i] then begin
         vertices.List[k]:=vertices.List[i];
         Inc(k);
      end;
   end;
   vertices.Count:=k;
end;

// StripifyMesh
//
function StripifyMesh(indices : TIntegerList; maxVertexIndex : Integer;
                      agglomerateLoneTriangles : Boolean = False) : TPersistentObjectList;
var
   accountedTriangles : array of ByteBool;
   vertexTris : array of TIntegerList;
   indicesList : PIntegerArray;
   indicesCount : Integer;
   currentStrip : TIntegerList;
   nextTriangle, nextVertex : Integer;

   function FindTriangleWithEdge(vertA, vertB : Integer) : Boolean;
   var
      i, n : Integer;
      p : PIntegerArray;
      list : TIntegerList;
   begin
      Result:=False;
      list:=vertexTris[vertA];
      for n:=0 to list.Count-1 do begin
         i:=list.List[n];
         if not (accountedTriangles[i]) then begin
            p:=@indicesList[i];
            if (p[0]=vertA) and (p[1]=vertB) then begin
               Result:=True;
               nextVertex:=p[2];
               nextTriangle:=i;
               Break;
            end else if (p[1]=vertA) and (p[2]=vertB) then begin
               Result:=True;
               nextVertex:=p[0];
               nextTriangle:=i;
               Break;
            end else if (p[2]=vertA) and (p[0]=vertB) then begin
               Result:=True;
               nextVertex:=p[1];
               nextTriangle:=i;
               Break;
            end;
         end;
      end;

   end;

   procedure BuildStrip(vertA, vertB : Integer);
   var
      vertC : Integer;
   begin
      currentStrip.Add(vertA, vertB);
      repeat
         vertC:=nextVertex;
         currentStrip.Add(vertC);
         accountedTriangles[nextTriangle]:=True;
         if not FindTriangleWithEdge(vertB, vertC) then Break;
         currentStrip.Add(nextVertex);
         accountedTriangles[nextTriangle]:=True;
         vertB:=nextVertex;
         vertA:=vertC;
      until not FindTriangleWithEdge(vertB, vertA);
   end;

var
   i, n, triangle : Integer;
   loneTriangles : TIntegerList;
begin
   Assert((indices.Count mod 3)=0, 'indices count is not a multiple of 3!');
   Result:=TPersistentObjectList.Create;
   // direct access and cache vars
   indicesList:=indices.List;
   indicesCount:=indices.Count;
   // Build adjacency lookup table (vertex based, not triangle based)
   SetLength(vertexTris, maxVertexIndex+1);
   for i:=0 to High(vertexTris) do
      vertexTris[i]:=TIntegerList.Create;
   n:=0;
   triangle:=0;
   for i:=0 to indicesCount-1 do begin
      vertexTris[indicesList[i]].Add(triangle);
      if n=2 then begin
         n:=0;
         Inc(triangle, 3);
      end else Inc(n);
   end;
   // Now, we use a greedy algo to build triangle strips
   SetLength(accountedTriangles, indicesCount); // yeah, waste of memory
   if agglomerateLoneTriangles then begin
      loneTriangles:=TIntegerList.Create;
      Result.Add(loneTriangles);
   end else loneTriangles:=nil;
   i:=0; while i<indicesCount do begin
      if not accountedTriangles[i] then begin
         accountedTriangles[i]:=True;
         if FindTriangleWithEdge(indicesList[i+1], indicesList[i]) then begin
            currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i+2]);
            BuildStrip(indicesList[i], indicesList[i+1]);
         end else if FindTriangleWithEdge(indicesList[i+2], indicesList[i+1]) then begin
            currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i]);
            BuildStrip(indicesList[i+1], indicesList[i+2]);
         end else if FindTriangleWithEdge(indicesList[i], indicesList[i+2]) then begin
            currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i+1]);
            BuildStrip(indicesList[i+2], indicesList[i]);
         end else begin
            if agglomerateLoneTriangles then
               currentStrip:=loneTriangles
            else currentStrip:=TIntegerList.Create;
            currentStrip.Add(indicesList[i], indicesList[i+1], indicesList[i+2]);
         end;
         if currentStrip<>loneTriangles then
            Result.Add(currentStrip);
      end;
      Inc(i, 3);
   end;
   // cleanup
   for i:=0 to High(vertexTris) do
      vertexTris[i].Free;
end;

// SubdivideTriangles
//
procedure SubdivideTriangles(smoothFactor : Single;
                             vertices : TAffineVectorList;
                             triangleIndices : TIntegerList;
                             normals : TAffineVectorList = nil;
                             onSubdivideEdge : TSubdivideEdgeEvent = nil);
var
   i, a, b, c, nv : Integer;
   edges : TIntegerList;
   triangleEdges : TIntegerList;
   p, n : TAffineVector;
   f : Single;
begin
   // build edges list
   triangleEdges:=TIntegerList.Create;
   try
      edges:=BuildNonOrientedEdgesList(triangleIndices, triangleEdges);
      try
         nv:=vertices.Count;
         // split all edges, add corresponding vertex & normal
         i:=0;
         while i<edges.Count do begin
            a:=edges[i];
            b:=edges[i+1];
            p:=VectorLerp(vertices[a], vertices[b], 0.5);
            if Assigned(normals) then begin
               n:=VectorNormalize(VectorLerp(normals[a], normals[b], 0.5));
               normals.Add(n);
               if smoothFactor<>0 then begin
                  f:=0.25*smoothFactor*VectorDistance(vertices[a], vertices[b])
                     *(1-VectorDotProduct(normals[a], normals[b]));
                  if VectorDotProduct(normals[a], VectorSubtract(vertices[b], vertices[a]))
                     +VectorDotProduct(normals[b], VectorSubtract(vertices[a], vertices[b]))>0 then
                     f:=-f;
                  CombineVector(p, n, f);
               end;
            end;
            if Assigned(onSubdivideEdge) then
               onSubdivideEdge(a, b, vertices.Add(p))
            else vertices.Add(p);
            Inc(i, 2);
         end;
         // spawn new triangles geometry
         i:=triangleIndices.Count-3;
         while i>=0 do begin
            a:=nv+triangleEdges[i+0] div 2;
            b:=nv+triangleEdges[i+1] div 2;
            c:=nv+triangleEdges[i+2] div 2;
            triangleIndices.Add(triangleIndices[i+0], a, c);
            triangleIndices.Add(a, triangleIndices[i+1], b);
            triangleIndices.Add(b, triangleIndices[i+2], c);
            triangleIndices[i+0]:=a;
            triangleIndices[i+1]:=b;
            triangleIndices[i+2]:=c;
            Dec(i, 3);
         end;
      finally
         edges.Free;
      end;
   finally
      triangleEdges.Free;
   end;
end;


end.
