{: Constructive Solid Geometry in GLScene.

   The CSG system uses BSP to optimize what triangles it considers.
   Its kept on a mesh basis to simplyfy things, it allways generates new BSP's,
   even if the meshes allready had BSP optimization.

   Author: Joen Joensen.
   Contributed to the GLScene community.

   Features: CSG_Union, CSG_Subtraction, CSG_Intersection.

	<b>History : </b><font size=-1><ul>
      <li>18/07/04 - JAJ - Bug fix, causing triangles to dissapear, once in a while.
      <li>29/11/03 - JAJ - Created and Submitted to GLScene.
	</ul></font>
}
unit GLMeshCSG;

interface

uses
  SysUtils, Classes, GLMisc, GLScene, GLVectorFileObjects, VectorGeometry, GLBSP, VectorLists;

Type
  TCSGOperation = (CSG_Union, CSG_Subtraction, CSG_Intersection);

Procedure CSG_Operation(obj1, obj2 : TMeshObject; Operation : TCSGOperation; Res : TMeshObject; Const MaterialName1, MaterialName2 : String);

implementation

uses Math;

const
   cOwnTriangleEpsilon = 1e-5;

function IntersectPointToPointLinePlane(const point1, point2 : TAffineVector; const plane : THmgPlane; intersectPoint : PAffineVector = nil) : Boolean;
var
   a, b : Extended;
   t : Single;
   Direction : TAffineVector;
begin
   Result := False;
   VectorSubtract(Point2,Point1,Direction);
   a:=VectorDotProduct(plane, Direction);    // direction projected to plane normal
   if a<>0 then begin          // direction is parallel to plane
      b:=PlaneEvaluatePoint(plane, point1);      // distance to plane
      t:=-b/a;                               // parameter of intersection
      Result := (t-EPSILON2 > 0) and (t+EPSILON2 < 1);
      If Result and (intersectPoint <> nil) then
      Begin
        intersectPoint^ := VectorCombine(Point1,Direction,1,t);

        If VectorEquals(intersectPoint^,point1) or VectorEquals(intersectPoint^,point2) then
          Result := False;
      End;
   end;
end;

Type
  TCSGTri = array[0..2] of PAffineVector;

Function MakeCSGTri(Const V1,V2,V3 : PAffineVector) : TCSGTri;
Begin
  Result[0] := v1;
  Result[1] := v2;
  Result[2] := v3;
End;

procedure CSG_Iterate_tri(const vec, nor : TCSGTri; BSP : TBSPMeshObject; Node : TFGBSPNode; ResMesh : TMeshObject; ResFG : TFGVertexNormalTexIndexList; keepinside, keepoutside, inverttriangle : Boolean);

Var
  vertex_offset : Integer;

Function Iterate_further(const vec, nor : TCSGTri; Node : TFGBSPNode) : Boolean;

Function MustSplit(d1,d2,d3 : Single) : Integer;
Var
  side : Integer;

Function Ok(Int : Single) : Boolean;
Begin
  Result := False;
  If Int < 0 then
  Begin
    If Side > 0 then
    Begin
      Result := True;
      Exit;
    end else Side := -1;
  End else
  If Int > 0 then
  Begin
    If Side < 0 then
    Begin
      Result := True;
      Exit;
    end else Side := 1;
  End;
End;

Begin
  Result := 0; // no split
  side := 0;
  Ok(D1); // can't go wrong yet...

  If Ok(D2) then
  Begin
    Result := 1; // pure split.
    Exit;
  End;

  If Ok(D3) then
  Begin
    Result := 1; // pure split.
    Exit;
  End;

  If side = 0 then
  Begin
    Result := 2; // on plane.
    Exit;
  End;
End;

Var
  d : array[0..2] of Single;
  i,i1 : Integer;
  b1,b2,b3 : Boolean;
  intersect_points : array[0..2] of TAffineVector;
  intersect_lines  : array[0..2] of Integer;
  intersect_count : Integer;
  t : array[0..2] of TCSGTri;
  p0,p1 : Integer;
  NextNode : TFGBSPNode;
  plane : THmgPlane;

Begin
// This have no effect, however it removes a warning...
  Result := False;
  b1 := False;
  b2 := False;
  b3 := False;
// This have no effect, however it removes a warning...

// normally we use the Node.SplitPlane, however on the last branch this is a NullPlane, so we have to calculate it.
  If VectorEquals(Node.SplitPlane,NullHmgVector) then
    plane := PlaneMake(BSP.Vertices[Node.VertexIndices[0]],BSP.Vertices[Node.VertexIndices[1]],BSP.Vertices[Node.VertexIndices[2]])
  else
    plane := Node.SplitPlane;

// check the three points in the triangle against the plane
  for i := 0 to 2 do
  Begin
    d[i] := PlaneEvaluatePoint(plane, Vec[i]^);
    if Abs(d[i])<cOwnTriangleEpsilon then
      d[i] := 0;
  end;

// based on points placement determine action
  Case MustSplit(d[0],d[1],d[2]) of
    0 : // no split
    If (d[0]+d[1]+d[2] >= 0) then
    Begin
      // check for sub node, and either iterate them or stop if none.
      If Node.PositiveSubNodeIndex = 0 then
        Result := keepoutside
      else
        Result := Iterate_further(Vec, nor, TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex]));
    End else
    Begin
      // check for sub node, and either iterate them or stop if none.
      If Node.NegativeSubNodeIndex = 0 then
        Result := keepinside
      else
        Result := Iterate_further(Vec, nor, TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]));
    End;
    1 : // must split.
    Begin
      // determine if 2 or 3 triangles are needed for the split.
      intersect_count := 0;
      For i := 0 to 2 do
      begin
        i1 := (i+1) mod 3;
        If IntersectPointToPointLinePlane(Vec[i]^,Vec[i1]^,plane,@intersect_points[intersect_count])  then
        Begin
          intersect_lines[intersect_count] := i;
          Inc(intersect_count);
        End;
      End;
      // from here of its not thoroughly commented
      // the base concept is isolate the two or three triangles, findout which to keep.
      // If all is kept we can simply return true and the original triangle wont be split.
      // however if only some are needed, we have to return false and add them ourselves...
      Case intersect_count of
        1 :
        Begin
          // simple split, two tri's
          i := intersect_lines[0];
          i1 := (i+2) mod 3;

          // cannot be 0, as then intersect_lines[0] would have other value
          if (d[i] > 0) then
            If Node.PositiveSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b1 := keepoutside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
          else
            If Node.NegativeSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b1 := keepinside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

          If NextNode <> Nil then
            b1 := Iterate_further(MakeCSGTri(Vec[i],@intersect_points[0],Vec[i1]),MakeCSGTri(Nor[i],Nor[i{}],Nor[i1]),NextNode);



          i := (intersect_lines[0]+1) mod 3;
          i1 := (i+1) mod 3;
          // cannot be 0, as then intersect_lines[0] would have other value
          if (d[i] > 0) then
            If Node.PositiveSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b2 := keepoutside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
          else
            If Node.NegativeSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b2 := keepinside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

          If NextNode <> Nil then
            b2 := Iterate_further(MakeCSGTri(Vec[i],Vec[i1],@intersect_points[0]),MakeCSGTri(Nor[i],Nor[i1],Nor[i{}]),NextNode);

          Result := b1 and b2;
          If not Result then
          Begin
            If B1 then
            Begin
              i := intersect_lines[0];
              i1 := (i+2) mod 3;
              vertex_offset := ResMesh.Vertices.count;
              ResMesh.Vertices.Add(Vec[i]^,intersect_points[0],Vec[i1]^);
              ResMesh.TexCoords.Add(Vec[i]^,intersect_points[0],Vec[i1]^);

              If inverttriangle then
              Begin
                ResMesh.Normals.Add(VectorScale(Nor[i]^,-1),VectorScale(Nor[i{}]^,-1),VectorScale(Nor[i1]^,-1));
                ResFG.VertexIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.NormalIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.TexCoordIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
              End else
              Begin
                ResMesh.Normals.Add(Nor[i]^,Nor[i{}]^,Nor[i1]^);
                ResFG.VertexIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.NormalIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.TexCoordIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
              End;
            End else
            If B2 then
            Begin
              i := (intersect_lines[0]+1) mod 3;
              i1 := (i+1) mod 3;
              vertex_offset := ResMesh.Vertices.count;
              ResMesh.Vertices.Add(Vec[i]^,Vec[i1]^,intersect_points[0]);
              ResMesh.TexCoords.Add(Vec[i]^,Vec[i1]^,intersect_points[0]);

              If inverttriangle then
              Begin
                ResMesh.Normals.Add(VectorScale(Nor[i]^,-1),VectorScale(Nor[i{}]^,-1),VectorScale(Nor[i1]^,-1));
                ResFG.VertexIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.NormalIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.TexCoordIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
              End else
              Begin
                ResMesh.Normals.Add(Nor[i]^,Nor[i{}]^,Nor[i1]^);
                ResFG.VertexIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.NormalIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.TexCoordIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
              End;
            End;
          End;
        End;
        2 :
        Begin
          // complex split, three tri's
          If intersect_lines[0]+1 = intersect_lines[1] then
          Begin
            p0 := 0;
            p1 := 1;
          End else
          Begin
            p0 := 1;
            p1 := 0;
          End;
          i := intersect_lines[p0];
          i1 := (i+2) mod 3;

          // cannot be 0 as then there would be no intersection
          if (d[i] > 0) then
            If Node.PositiveSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b1 := keepoutside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
          else
            If Node.NegativeSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b1 := keepinside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

          If NextNode <> Nil then
            b1 := Iterate_further(MakeCSGTri(Vec[i],@intersect_points[p0],Vec[i1]),MakeCSGTri(Nor[i],Nor[i{}],Nor[i1]),NextNode);

          i1 := (i+1) mod 3;
          // cannot be 0 as then there would be no intersection
          if (d[i1] > 0) then
            If Node.PositiveSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b2 := keepoutside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
          else
            If Node.NegativeSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b2 := keepinside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

          If NextNode <> Nil then
            b2 :=  Iterate_further(MakeCSGTri(@intersect_points[p0],Vec[i1],@intersect_points[p1]),MakeCSGTri(Nor[i1{}],Nor[i1],Nor[i1{}]),NextNode);

          i1 := (i+2) mod 3;
          // cannot be 0 as then there would be no intersection
          if (d[i1] > 0) then
            If Node.PositiveSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b3 := keepoutside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
          else
            If Node.NegativeSubNodeIndex = 0 then
            Begin
              NextNode := Nil;
              b3 := keepinside;
            End else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

          If NextNode <> Nil then
            b3 :=  Iterate_further(MakeCSGTri(@intersect_points[p0],@intersect_points[p1],Vec[i1]),MakeCSGTri(Nor[i1{}],Nor[i1{}],Nor[i1]),NextNode);

          Result := b1 and b2 and b3;
          If not Result then
          Begin
            If B1 then
            Begin
              i1 := (i+2) mod 3;
              vertex_offset := ResMesh.Vertices.count;
              ResMesh.Vertices.Add(Vec[i]^,intersect_points[p0],Vec[i1]^);
              ResMesh.TexCoords.Add(Vec[i]^,intersect_points[p0],Vec[i1]^);
              If inverttriangle then
              Begin
                ResMesh.Normals.Add(VectorScale(Nor[i]^,-1),VectorScale(Nor[i{}]^,-1),VectorScale(Nor[i1]^,-1));
                ResFG.VertexIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.NormalIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.TexCoordIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
              End else
              Begin
                ResMesh.Normals.Add(Nor[i]^,Nor[i{}]^,Nor[i1]^);
                ResFG.VertexIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.NormalIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.TexCoordIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
              End;
            End;
            If B2 then
            Begin
              i1 := (i+1) mod 3;
              vertex_offset := ResMesh.Vertices.count;
              ResMesh.Vertices.Add(intersect_points[p0],Vec[i1]^,intersect_points[p1]);
              ResMesh.TexCoords.Add(intersect_points[p0],Vec[i1]^,intersect_points[p1]);

              If inverttriangle then
              Begin
                ResMesh.Normals.Add(VectorScale(Nor[i1{}]^,-1),VectorScale(Nor[i1]^,-1),VectorScale(Nor[i1{}]^,-1));
                ResFG.VertexIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.NormalIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.TexCoordIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
              End else
              Begin
                ResMesh.Normals.Add(Nor[i1{}]^,Nor[i1]^,Nor[i1{}]^);
                ResFG.VertexIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.NormalIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.TexCoordIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
              End;
            End;
            If B3 then
            Begin
              i1 := (i+2) mod 3;
              vertex_offset := ResMesh.Vertices.count;
              ResMesh.Vertices.Add(intersect_points[p0],intersect_points[p1],Vec[i1]^);
              ResMesh.TexCoords.Add(intersect_points[p0],intersect_points[p1],Vec[i1]^);

              If inverttriangle then
              Begin
                ResMesh.Normals.Add(VectorScale(Nor[i1{}]^,-1),VectorScale(Nor[i1{}]^,-1),VectorScale(Nor[i1]^,-1));
                ResFG.VertexIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.NormalIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
                ResFG.TexCoordIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
              End else
              Begin
                ResMesh.Normals.Add(Nor[i1{}]^,Nor[i1{}]^,Nor[i1]^);
                ResFG.VertexIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.NormalIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
                ResFG.TexCoordIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
              End;
            End;
          End;
        End;
      End;
    End;
    2 : // on plane, no split but special logic
    Begin
      // find out if they point the same direction.
      d[0] := PlaneEvaluatePoint(Plane,VectorAdd(Vec[0]^,Nor[0]^));
      // check for sub node, and either iterate them or stop if none.
      If d[0] >= 0 then
        If Node.PositiveSubNodeIndex = 0 then
        Begin
          NextNode := Nil;
          Result := keepoutside;
        End else
          NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
      else
        If Node.NegativeSubNodeIndex = 0 then
        Begin
          NextNode := Nil;
          Result := keepinside;
        End else
          NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

      If NextNode <> Nil then
        Result := Iterate_further(Vec,Nor,NextNode);
    End;
  End;
End;

Begin
  // check this triangle.
  If Iterate_Further(Vec, nor ,Node) then
  Begin
    // Keep this triangle, logic based on the (keepinside, keepoutside) booleans.
    vertex_offset := ResMesh.Vertices.count;
    ResMesh.Vertices.Add(Vec[0]^,Vec[1]^,Vec[2]^);
    ResMesh.TexCoords.Add(Vec[0]^,Vec[1]^,Vec[2]^);
    If inverttriangle then
    Begin
      ResMesh.Normals.Add(VectorScale(Nor[0]^,-1),VectorScale(Nor[1]^,-1),VectorScale(Nor[2]^,-1));
      ResFG.VertexIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
      ResFG.NormalIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
      ResFG.TexCoordIndices.Add(vertex_offset+2,vertex_offset+1,vertex_offset);
    End else
    Begin
      ResMesh.Normals.Add(Nor[0]^, Nor[1]^, Nor[2]^);
      ResFG.VertexIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
      ResFG.NormalIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
      ResFG.TexCoordIndices.Add(vertex_offset,vertex_offset+1,vertex_offset+2);
    End;
  End;
End;


Procedure CSG_Operation(obj1, obj2 : TMeshObject; Operation : TCSGOperation; Res : TMeshObject; Const MaterialName1, MaterialName2 : String);

Var
  v1, t1, n1 : TAffineVectorList;
  v2, t2, n2 : TAffineVectorList;
  BSP1, BSP2 : TBSPMeshObject;
  FG1, FG2 : TFGBSPNode;
  i : Integer;
  FGR : TFGVertexNormalTexIndexList;

Begin
  // prepare containers, fill the triangle list from the objects
  BSP1 := TBSPMeshObject.Create;
  BSP2 := TBSPMeshObject.Create;

  BSP1.Mode := momFaceGroups;
  BSP2.Mode := momFaceGroups;

  FG1 := TFGBSPNode.CreateOwned(BSP1.FaceGroups);
  FG2 := TFGBSPNode.CreateOwned(BSP2.FaceGroups);

  t1   := TAffineVectorList.create;
  n1   := TAffineVectorList.create;
  v1   := obj1.ExtractTriangles(t1,n1);

  v1.TransformAsPoints(obj1.Owner.Owner.Matrix);

  BSP1.Mode := momTriangles;
  BSP1.Vertices := v1;
  BSP1.Normals := n1;
  BSP1.TexCoords := t1;
  FG1.VertexIndices.AddSerie(0, 1, BSP1.Vertices.Count);

  t2   := TAffineVectorList.create;
  n2   := TAffineVectorList.create;
  v2  := obj2.ExtractTriangles(t2,n2);
  v2.TransformAsPoints(obj2.Owner.Owner.Matrix);

  BSP2.Mode := momTriangles;
  BSP2.Vertices := v2;
  BSP2.Normals := n2;
  BSP2.TexCoords := t2;

  FG2.VertexIndices.AddSerie(0, 1, BSP2.Vertices.Count);

  // Build BSPs
  FG1.PerformSplit(FG1.FindSplitPlane,1);
  FG2.PerformSplit(FG2.FindSplitPlane,1);

  // Start creating result.
  FGR := TFGVertexNormalTexIndexList.CreateOwned(Res.FaceGroups);
  FGR.MaterialName := MaterialName1;

//  should be obj1.FaceGroups iteration for perfection and multiple materials!

//  First iterate all triangles of object 1, one at a time,  down through the BSP tree of Object 2, the last booleans are the key to what actuelly happends.
  i := 0;
  while i < v1.Count-2 do
  Begin
    Case Operation of
      CSG_Union :
      Begin
        CSG_Iterate_tri(MakeCSGTri(@v1.List^[i],@v1.List^[i+1],@v1.List^[i+2]),MakeCSGTri(@n1.List^[i],@n1.List^[i+1],@n1.List^[i+2]),BSP2,FG2,Res,FGR,false,true,false);
      End;
      CSG_Subtraction :
      Begin
        CSG_Iterate_tri(MakeCSGTri(@v1.List^[i],@v1.List^[i+1],@v1.List^[i+2]),MakeCSGTri(@n1.List^[i],@n1.List^[i+1],@n1.List^[i+2]),BSP2,FG2,Res,FGR,false,true,false);
      End;
      CSG_Intersection :
      Begin
        CSG_Iterate_tri(MakeCSGTri(@v1.List^[i],@v1.List^[i+1],@v1.List^[i+2]),MakeCSGTri(@n1.List^[i],@n1.List^[i+1],@n1.List^[i+2]),BSP2,FG2,Res,FGR,true,false,false);
      End;
    End;
    inc(i,3);
  End;

//  Then iterate all triangles of object 2, one at a time, down through the BSP tree of Object 1, the last booleans are the key to what actuelly happends.
  FGR := TFGVertexNormalTexIndexList.CreateOwned(Res.FaceGroups);
  FGR.MaterialName := MaterialName2;
  i := 0;
  while i < v2.Count-2 do
  Begin
    Case Operation of
      CSG_Union :
      Begin
        CSG_Iterate_tri(MakeCSGTri(@v2.List^[i],@v2.List^[i+1],@v2.List^[i+2]),MakeCSGTri(@n2.List^[i],@n2.List^[i+1],@n2.List^[i+2]),BSP1,FG1,Res,FGR,false,true,false);
      End;
      CSG_Subtraction :
      Begin
        CSG_Iterate_tri(MakeCSGTri(@v2.List^[i],@v2.List^[i+1],@v2.List^[i+2]),MakeCSGTri(@n2.List^[i],@n2.List^[i+1],@n2.List^[i+2]),BSP1,FG1,Res,FGR,true,false,true);
      End;
      CSG_Intersection :
      Begin
        CSG_Iterate_tri(MakeCSGTri(@v2.List^[i],@v2.List^[i+1],@v2.List^[i+2]),MakeCSGTri(@n2.List^[i],@n2.List^[i+1],@n2.List^[i+2]),BSP1,FG1,Res,FGR,true,false,false);
      End;
    End;
    inc(i,3);
  End;

  // clean up.
  v1.Free;
  n1.Free;
  t1.Free;

  v2.Free;
  n2.Free;
  t2.Free;

  BSP2.Free;
  BSP1.Free;

End;


end.
