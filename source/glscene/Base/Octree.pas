//
// This unit is part of the GLScene Project, http://glscene.org
//
{: Octree<p>

   Octree management classes and structures.<p>

   TODO: move the many public vars/fields to private/protected<p>

	<b>History : </b><font size=-1><ul>
      <li>31/01/04 - Mathx - Bugfix on DisposeTree (thanks dikoe Kanguru)	
      <li>19/06/04 - LucasG - Moved triangleFiler and WalkSphereToLeaf to public
      <li>20/07/03 - DanB - Modified SphereSweepIntersect to deal with embedded spheres better
      <li>08/05/03 - DanB - name changes + added ClosestPointOnTriangle + fixes
      <li>08/05/03 - DanB - added AABBIntersect (Matheus Degiovani)
      <li>22/01/03 - EG - GetTrianglesInCube moved in (Bernd Klaiber)
      <li>29/11/02 - EG - Added triangleInfo
      <li>14/07/02 - EG - Dropped GLvectorFileObjects dependency
      <li>17/03/02 - EG - Added SphereIntersectAABB from Robert Hayes
	   <li>13/03/02 - EG - Made in a standalone unit, based on Robert Hayes code
	</ul></font>
}
unit Octree;

interface

uses Classes, VectorGeometry, VectorLists, GeometryBB;

type

   TProcInt = procedure(i: integer) of object;
   TProcAffineAffineAffine = procedure(v1, v2, v3: TAffineFLTVector) of object;

   // TOctreeTriangleInfo
   //
   {: Stores information about an intersected triangle. }
   TOctreeTriangleInfo = record
      index : Integer;
      vertex : array [0..2] of TAffineVector;
   end;
   POctreeTriangleInfo = ^TOctreeTriangleInfo;

   // TOctreeNode
   //
   POctreeNode = ^TOctreeNode;
   TOctreeNode = record
      MinExtent : TAffineFLTVector;
      MaxExtent : TAffineFLTVector;

      //Duplicates possible?
      TriArray : array of Integer;  // array of triangle references

      ChildArray : array [0..7] of POctreeNode;  //Octree's 8 children
   end;

   // TOctree
   //
   {: Manages an Octree containing references to triangles.<p> }
   TOctree = class (TObject)
      private
         { Private Declarations }
{$ifdef DEBUG}
         intersections: integer;    //for debugging  - number of triangles intersecting an AABB plane
{$endif}

      protected
         { Protected Declarations }
         //Find the exact centre of an AABB
         function GetMidPoint (min, max: single): single;
         //Check if a point lies within the AABB specified by min and max entents
         function PointInNode(const min, max, aPoint : TAffineFLTVector): Boolean;
         //Check if a triangle (vertices v1, v2, v3) lies within the AABB specified by min and max entents
         function TriIntersectNode(const minExtent, maxExtent, v1, v2, v3: TAffineFLTVector): BOOLEAN;
         //Check if a sphere (at point C with radius) lies within the AABB specified by min and max entents
         function SphereInNode(const minExtent, maxExtent : TAffineVector;
                               const c: TVector; radius: Single): Boolean;

         procedure WalkTriToLeafx(Onode: POctreeNode; const v1, v2, v3 : TAffineFLTVector);
         procedure WalkPointToLeafx(ONode: POctreeNode; const p : TAffineVector);
         procedure WalkSphereToLeafx(Onode: POctreeNode; const p : TVector; radius : Single);
         procedure WalkRayToLeafx(Onode: POctreeNode; const p, v: TVector);

         function GetExtent (const flags: array of byte; ParentNode: POctreeNode): TAffineFLTVector;

         {: Recursive routine to build nodes from parent to max depth level. }
         procedure Refine(ParentNode: POctreeNode; level: integer);

         //Main "walking" routines.  Walks the item through the Octree down to a leaf node.
         procedure WalkPointToLeaf(ONode: POctreeNode; const p : TAffineVector);
         procedure WalkTriToLeaf(Onode: POctreeNode; const v1, v2, v3 : TAffineVector);
         procedure WalkRayToLeaf(Onode: POctreeNode; const p, v : TVector);

         //: Example of how to process each node in the tree
         procedure ConvertR4(ONode: POctreeNode; const scale : TAffineFLTVector);

         procedure CreateTree(depth: integer);
         procedure CutMesh;
         
      public
         { Public Declarations }
         WorldMinExtent, WorldMaxExtent: TAffineFLTVector;
         RootNode: POctreeNode;      //always points to root node
         MaxOlevel: integer;   //max depth level of TOctreeNode
         NodeCount : Integer;  //number of nodes (ex: 8 for level 1, 72 for level 2 etc).
         TriCountMesh : Integer;   //total number of triangles in the mesh
         TriCountOctree : Integer; //total number of triangles cut into the octree
         MeshCount : Integer;  //number of meshes currently cut into the Octree

         ResultArray : array of POctreeNode;  //holds the result nodes of various calls

         {19/06/2004 - Lucas G. - Needed this change - Used in ECMisc.pas}
         triangleFiler : TAffineVectorList;
         procedure WalkSphereToLeaf(Onode: POctreeNode; const p : TVector; radius : Single);

         {: Initializes the tree from the triangle list.<p>
            All triangles must be contained in the world extent to be properly
            taken into account. }
         procedure InitializeTree(const worldMinExtent, worldMaxExtent : TAffineVector;
                                  const triangles : TAffineVectorList;
                                  const treeDepth : Integer);
         procedure DisposeTree;

         destructor Destroy;  override;

         function RayCastIntersect(const rayStart, rayVector : TVector;
                                       intersectPoint : PVector = nil;
                                       intersectNormal : PVector = nil;
                                       triangleInfo : POctreeTriangleInfo = nil) : Boolean;
         function SphereSweepIntersect(const rayStart, rayVector : TVector;
                                      const velocity, radius : single;
                                      intersectPoint : PVector = nil;
                                      intersectNormal : PVector = nil) : Boolean;

         function TriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
         {: Returns all triangles in the AABB. }
         function GetTrianglesFromNodesIntersectingAABB(const objAABB : TAABB) : TAffineVectorList;
         {: Returns all triangles in an arbitrarily placed cube}         
         function GetTrianglesFromNodesIntersectingCube(const objAABB : TAABB;
                                     const objToSelf, selfToObj : TMatrix) : TAffineVectorList;
         {: Checks if an AABB intersects a face on the octree}
         function AABBIntersect(const AABB: TAABB; m1to2, m2to1: TMatrix; triangles: TAffineVectorList = nil): boolean;
//         function SphereIntersect(position:TAffineVector; radius:single);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ----------------------------------------------------------------------
// Name  : CheckPointInSphere()
// Input : point - point we wish to check for inclusion
//         sO - Origin of sphere
//         sR - radius of sphere
// Notes :
// Return: TRUE if point is in sphere, FALSE if not.
// -----------------------------------------------------------------------

function CheckPointInSphere(const point, sO : TVector; const sR : Single) : Boolean;
begin
   //Allow small margin of error
   Result:=(VectorDistance2(point, sO)<=Sqr(sR));
end;

// ----------------------------------------------------------------------
// Name  : CheckPointInTriangle()
// Input : point - point we wish to check for inclusion
//         a - first vertex in triangle
//         b - second vertex in triangle 
//         c - third vertex in triangle
// Notes : Triangle should be defined in clockwise order a,b,c
// Return: TRUE if point is in triangle, FALSE if not.
// -----------------------------------------------------------------------  

function CheckPointInTriangle(point, a, b, c: TAffineVector):boolean;
var
  total_angles:Single;
  v1,v2,v3:TAffineVector;
begin
  total_angles := 0;

  // make the 3 vectors
  v1 := VectorSubtract(point,a);
  v2 := VectorSubtract(point,b);
  v3 := VectorSubtract(point,c);

  normalizeVector(v1);
  normalizeVector(v2);
  normalizeVector(v3);

  total_angles := total_angles + arccos(VectorDotProduct(v1,v2));
  total_angles := total_angles + arccos(VectorDotProduct(v2,v3));
  total_angles := total_angles + arccos(VectorDotProduct(v3,v1));

  if (abs(total_angles-2*PI) <= 0.005) then
    result:= TRUE
  else
    result:=FALSE;
end;

// ----------------------------------------------------------------------
// Name  : ClosestPointOnLine()
// Input : a - first end of line segment
//         b - second end of line segment
//         p - point we wish to find closest point on line from
// Notes : Helper function for closestPointOnTriangle()
// Return: closest point on line segment
// -----------------------------------------------------------------------

function ClosestPointOnLine(const a, b, p : TAffineVector): TAffineVector;
var d, t: double;
    c, v: TAffineFLTVector;
begin
    VectorSubtract(p, a, c);
    VectorSubtract(b, a, v);

    d:=VectorLength(v);
    NormalizeVector(v);
    t:=VectorDotProduct(v,c);

    //Check to see if t is beyond the extents of the line segment
    if (t < 0.0) then result:=a
    else if (t > d) then result:=b
    else begin
      v[0]:=v[0]*t;
      v[1]:=v[1]*t;
      v[2]:=v[2]*t;
      result:=VectorAdd(a, v);
    end;
end;

// ----------------------------------------------------------------------
// Name  : ClosestPointOnTriangle()
// Input : a - first vertex in triangle
//         b - second vertex in triangle
//         c - third vertex in triangle
//         p - point we wish to find closest point on triangle from
// Notes :
// Return: closest point on triangle
// -----------------------------------------------------------------------
{
function ClosestPointOnTriangle(const a, b, c, n, p: TAffineVector): TAffineVector;
var
   dAB, dBC, dCA : Single;
   Rab, Rbc, Rca, intPoint : TAffineFLTVector;
   hit:boolean;
begin
    //this would be faster if RayCastTriangleIntersect detected backwards hits
    hit:=RayCastTriangleIntersect(VectorMake(p),VectorMake(n),a,b,c,@intPoint) or
         RayCastTriangleIntersect(VectorMake(p),VectorMake(VectorNegate(n)),a,b,c,@intPoint);
    if (hit) then
    begin
      Result:=intPoint;
    end
    else
    begin
    Rab:=ClosestPointOnLine(a, b, p);
    Rbc:=ClosestPointOnLine(b, c, p);
    Rca:=ClosestPointOnLine(c, a, p);

    dAB:=VectorDistance2(p, Rab);
    dBC:=VectorDistance2(p, Rbc);
    dCA:=VectorDistance2(p, Rca);

      if dBC<dAB then
        if dCA<dBC then
           Result:=Rca
        else Result:=Rbc
      else if dCA<dAB then
        Result:=Rca
      else Result:=Rab;
    end;
end;
}

// ----------------------------------------------------------------------
// Name  : ClosestPointOnTriangleEdge()
// Input : a - first vertex in triangle
//         b - second vertex in triangle
//         c - third vertex in triangle
//         p - point we wish to find closest point on triangle from
// Notes :
// Return: closest point on line triangle edge
// -----------------------------------------------------------------------

function ClosestPointOnTriangleEdge(const a, b, c, p: TAffineVector): TAffineVector;
var
   dAB, dBC, dCA : Single;
   Rab, Rbc, Rca : TAffineFLTVector;
begin
    Rab:=ClosestPointOnLine(a, b, p);
    Rbc:=ClosestPointOnLine(b, c, p);
    Rca:=ClosestPointOnLine(c, a, p);

    dAB:=VectorDistance2(p, Rab);
    dBC:=VectorDistance2(p, Rbc);
    dCA:=VectorDistance2(p, Rca);

    if dBC<dAB then
      if dCA<dBC then
         Result:=Rca
      else Result:=Rbc
    else if dCA<dAB then
      Result:=Rca
    else Result:=Rab;
end;

// HitBoundingBox
//
function HitBoundingBox(const minB, maxB: TAffineFLTVector;
                        const origin, dir: TVector;
                        var coord: TVector): BOOLEAN;
const
   NUMDIM	= 2;
   RIGHT	= 0;
   LEFT	= 1;
   MIDDLE    = 2;
var
   i, whichplane: integer;
   inside: BOOLEAN;
   quadrant: array [0..NUMDIM] of byte;
   maxT: array [0..NUMDIM] of double;
   candidatePlane: array [0..NUMDIM] of double;

begin
	inside := TRUE;

	// Find candidate planes; this loop can be avoided if
   	// rays cast all from the eye(assume perpsective view)
        for i:=0 to NUMDIM do begin
		if(origin[i] < minB[i]) then begin
			quadrant[i] := LEFT;
			candidatePlane[i] := minB[i];
			inside := FALSE;
                end
		else if (origin[i] > maxB[i]) then begin
			quadrant[i] := RIGHT;
			candidatePlane[i] := maxB[i];
			inside := FALSE;
                end
		else	quadrant[i] := MIDDLE;
        end;


	//* Ray origin inside bounding box */
	if inside then begin
		SetVector(coord,  origin);
		result:= TRUE;
                exit;
	end;


	//* Calculate T distances to candidate planes */
        for i:=0 to NUMDIM do begin
		if (quadrant[i] <> MIDDLE) AND (dir[i] <> 0) then
			maxT[i] := (candidatePlane[i]-origin[i]) / dir[i]
		else
			maxT[i] := -1;
        end;

	//* Get largest of the maxT's for final choice of intersection */
	whichPlane := 0;
        for i:=1 to NUMDIM do
            if (maxT[whichPlane] < maxT[i]) then whichPlane := i;

	//* Check final candidate actually inside box */
	if (maxT[whichPlane] < 0) then begin
            result:=FALSE;
            exit;
        end;

        for i:=0 to NUMDIM do begin
		if whichPlane <> i then begin
			coord[i] := origin[i] + maxT[whichPlane] * dir[i];
			if (coord[i] < minB[i]) OR (coord[i] > maxB[i]) then begin
				result:=FALSE;
                                exit;
                        end;
                end
		else 	coord[i] := candidatePlane[i];
        end;

	result:=TRUE;				//* ray hits box */

end;

const USE_EPSILON_TEST = TRUE;
      EPSILON = 0.000001;

// coplanar_tri_tri
//
function coplanar_tri_tri(const N,V0,V1,V2,U0,U1,U2: TAffineFLTVEctor): integer;
var
   A: TAffineFLTVector;
   i0,i1: shortint;

   function EDGE_AGAINST_TRI_EDGES(const V0,V1,U0,U1,U2: TAffineFLTVector): integer;
   var
      Ax,Ay,Bx,By,Cx,Cy,e,d,f: single;

      //* this edge to edge test is based on Franlin Antonio's gem:
      //   "Faster Line Segment Intersection", in Graphics Gems III,
      //   pp. 199-202 */
      function EDGE_EDGE_TEST(const V0, U0, U1 : TAffineFLTVector) : Integer;
      begin
         result:=0;
         Bx:=U0[i0]-U1[i0];
         By:=U0[i1]-U1[i1];
         Cx:=V0[i0]-U0[i0];
         Cy:=V0[i1]-U0[i1];
         f:=Ay*Bx-Ax*By;
         d:=By*Cx-Bx*Cy;
         if((f>0) and (d>=0) and (d<=f)) or ((f<0) and (d<=0) and (d>=f)) then begin
            e:=Ax*Cy-Ay*Cx;
            if(f>0) then begin
               if (e>=0) and (e<=f) then result:=1
            end else if(e<=0) and (e>=f) then result:=1;
         end;
      end;

   begin
     Ax:=V1[i0]-V0[i0];
     Ay:=V1[i1]-V0[i1];
     //* test edge U0,U1 against V0,V1 */
     result:=EDGE_EDGE_TEST(V0,U0,U1);
     if result=1 then exit;
     //* test edge U1,U2 against V0,V1 */
     result:=EDGE_EDGE_TEST(V0,U1,U2);
     if result=1 then exit;
     //* test edge U2,U1 against V0,V1 */
     result:=EDGE_EDGE_TEST(V0,U2,U0);
   end;

   function POINT_IN_TRI(const V0,U0,U1,U2: TAffineFLTVector): integer;
   var
      a,b,c,d0,d1,d2: single;
   begin
      result:=0;
      //* is T1 completly inside T2? */
      //* check if V0 is inside tri(U0,U1,U2) */
      a:=U1[i1]-U0[i1];
      b:=-(U1[i0]-U0[i0]);
      c:=-a*U0[i0]-b*U0[i1];
      d0:=a*V0[i0]+b*V0[i1]+c;

      a:=U2[i1]-U1[i1];
      b:=-(U2[i0]-U1[i0]);
      c:=-a*U1[i0]-b*U1[i1];
      d1:=a*V0[i0]+b*V0[i1]+c;

      a:=U0[i1]-U2[i1];
      b:=-(U0[i0]-U2[i0]);
      c:=-a*U2[i0]-b*U2[i1];
      d2:=a*V0[i0]+b*V0[i1]+c;
      if (d0*d1>0.0) then
         if (d0*d2>0.0) then result:=1;
   end;

/// Begin Main logic ///////////////////////////////
begin
   //* first project onto an axis-aligned plane, that maximizes the area */
   //* of the triangles, compute indices: i0,i1. */
   A[0]:=abs(N[0]);
   A[1]:=abs(N[1]);
   A[2]:=abs(N[2]);
   if(A[0]>A[1]) then begin
      if(A[0]>A[2]) then begin
          i0:=1;      //* A[0] is greatest */
          i1:=2;
      end else begin
          i0:=0;      //* A[2] is greatest */
          i1:=1;
      end
   end else begin  //* A[0]<=A[1] */
      if(A[2]>A[1]) then begin
          i0:=0;      //* A[2] is greatest */
          i1:=1;
      end else begin
          i0:=0;      //* A[1] is greatest */
          i1:=2;
      end
   end;

   //* test all edges of triangle 1 against the edges of triangle 2 */
   result:=EDGE_AGAINST_TRI_EDGES(V0,V1,U0,U1,U2);
   if result=1 then exit;
   result:=EDGE_AGAINST_TRI_EDGES(V1,V2,U0,U1,U2);
   if result=1 then exit;
   result:=EDGE_AGAINST_TRI_EDGES(V2,V0,U0,U1,U2);
   if result=1 then exit;

   //* finally, test if tri1 is totally contained in tri2 or vice versa */
   result:=POINT_IN_TRI(V0,U0,U1,U2);
   if result=1 then exit;
   result:=POINT_IN_TRI(U0,V0,V1,V2);
end;

// tri_tri_intersect 
//
function tri_tri_intersect(const V0,V1,V2,U0,U1,U2: TAFFineFLTVector): integer;
var
   E1,E2: TAffineFLTVector;
   N1,N2: TAffineFLTVector;
   d1,d2: single;
   du0,du1,du2,dv0,dv1,dv2: single;
   D: TAffineFLTVector;
   isect1: array[0..1] of single;
   isect2: array[0..1] of single;
   du0du1,du0du2,dv0dv1,dv0dv2: single;
   index: shortint;
   vp0,vp1,vp2: single;
   up0,up1,up2: single;
   b,c,max: single;

   procedure ISECT(VV0,VV1,VV2,D0,D1,D2: single; var isect0,isect1: single);
   begin
      isect0:=VV0+(VV1-VV0)*D0/(D0-D1);
      isect1:=VV0+(VV2-VV0)*D0/(D0-D2);
   end;

   function COMPUTE_INTERVALS(VV0,VV1,VV2,D0,D1,D2,D0D1,D0D2: single; var isect0,isect1: single): integer;
   begin
     result:=0;
     if(D0D1>0.0) then
       //* here we know that D0D2<=0.0 */
       //* that is D0, D1 are on the same side, D2 on the other or on the plane */ \
       ISECT(VV2,VV0,VV1,D2,D0,D1,isect0,isect1)
     else if(D0D2>0.0) then
      //* here we know that d0d1<=0.0 */
       ISECT(VV1,VV0,VV2,D1,D0,D2,isect0,isect1)
     else if(D1*D2>0.0) or (D0<>0.0) then
       //* here we know that d0d1<=0.0 or that D0!=0.0 */
       ISECT(VV0,VV1,VV2,D0,D1,D2,isect0,isect1)
     else if(D1<>0.0) then
       ISECT(VV1,VV0,VV2,D1,D0,D2,isect0,isect1)
     else if(D2<>0.0) then
       ISECT(VV2,VV0,VV1,D2,D0,D1,isect0,isect1)
     else
       //* triangles are coplanar */
       result:=coplanar_tri_tri(N1,V0,V1,V2,U0,U1,U2);
   end;

   //* sort so that a<=b */
   procedure SORT(var a: single; var b: single);
   var
      c : single;
   begin
      if (a>b) then begin
         c:=a;
         a:=b;
         b:=c;
      end;
   end;

begin
   //* compute plane equation of triangle(V0,V1,V2) */
   E1:=VectorSubtract(V1,V0);
   E2:=VectorSubtract(V2,V0);
   N1:=VectorCrossProduct(E1, E2);
   d1:=-VectorDotProduct(N1,V0);
   //* plane equation 1: N1.X+d1=0 */

   //* put U0,U1,U2 into plane equation 1 to compute signed distances to the plane*/
   du0:=VectorDotProduct(N1,U0)+d1;
   du1:=VectorDotProduct(N1,U1)+d1;
   du2:=VectorDotProduct(N1,U2)+d1;

   //* coplanarity robustness check */
   if USE_EPSILON_TEST=TRUE then begin
      if (abs(du0)<EPSILON) then du0:=0.0;
      if (abs(du1)<EPSILON) then du1:=0.0;
      if (abs(du2)<EPSILON) then du2:=0.0;
   end;
   du0du1:=du0*du1;
   du0du2:=du0*du2;

   if(du0du1>0.0) and (du0du2>0.0) then begin//* same sign on all of them + not equal 0 ? */
      result:=0;                    //* no intersection occurs */
      exit;
   end;

   //* compute plane of triangle (U0,U1,U2) */
   E1:=VectorSubtract(U1,U0);
   E2:=VectorSubtract(U2,U0);
   N2:=VectorCrossProduct(E1, E2);
   d2:=-VectorDotProduct(N2,U0);
   //* plane equation 2: N2.X+d2=0 */

   //* put V0,V1,V2 into plane equation 2 */
   dv0:=VectorDotProduct(N2,V0)+d2;
   dv1:=VectorDotProduct(N2,V1)+d2;
   dv2:=VectorDotProduct(N2,V2)+d2;

   if USE_EPSILON_TEST=TRUE then begin
      if(abs(dv0)<EPSILON) then dv0:=0.0;
      if(abs(dv1)<EPSILON) then dv1:=0.0;
      if(abs(dv2)<EPSILON) then dv2:=0.0;
   end;

   dv0dv1:=dv0*dv1;
   dv0dv2:=dv0*dv2;

   if(dv0dv1>0.0) and (dv0dv2>0.0) then begin //* same sign on all of them + not equal 0 ? */
      result:=0;                    //* no intersection occurs */
      exit;
   end;

   //* compute direction of intersection line */
   D:=VectorCrossProduct(N1, N2);

   //* compute and index to the largest component of D */
   max:=abs(D[0]);
   index:=0;
   b:=abs(D[1]);
   c:=abs(D[2]);
   if(b>max) then begin
     max:=b; index:=1;
   end;
   if(c>max) then begin
     //max:=c;   why?
     index:=2;
   end;
   //* this is the simplified projection onto L*/
   vp0:=V0[index];
   vp1:=V1[index];
   vp2:=V2[index];

   up0:=U0[index];
   up1:=U1[index];
   up2:=U2[index];

   //* compute interval for triangle 1 */
   COMPUTE_INTERVALS(vp0,vp1,vp2,dv0,dv1,dv2,dv0dv1,dv0dv2,isect1[0],isect1[1]);

   //* compute interval for triangle 2 */
   COMPUTE_INTERVALS(up0,up1,up2,du0,du1,du2,du0du1,du0du2,isect2[0],isect2[1]);

   SORT(isect1[0],isect1[1]);
   SORT(isect2[0],isect2[1]);

   if (isect1[1]<isect2[0]) or (isect2[1]<isect1[0]) then
      result:=0
   else result:=1;
end;

// ------------------
// ------------------ TOctree ------------------
// ------------------

const MIN = 0;
const MID = 1;
const MAX = 2;
const POINT = 0;
const TRIANGLE = 1;
const TOPFACE = 0;
const BOTTOMFACE = 1;
const LEFTFACE = 2;
const RIGHTFACE = 3;
const FRONTFACE = 4;
const BACKFACE = 5;
const TOPLEFT = 0;
const TOPRIGHT = 1;
const BOTTOMLEFT = 2;
const BOTTOMRIGHT = 3;

// Theory on FlagMax and FlagMin:
// When a node is subdivided, each of the 8 children assumes 1/8th ownership of its
// parent's bounding box (defined by parent extents).  Calculating a child's min/max
// extent only requires 3 values: the parent's min extent, the parent's max extent
// and the midpoint of the parent's extents (since the cube is divided in half twice).
// The following arrays assume that the children are numbered from 0 to 7, named Upper
// and Lower (Upper = top 4 cubes on Y axis, Bottom = lower 4 cubes), Left and Right, and
// Fore and Back (Fore facing furthest away from you the viewer).
// Each node can use its corresponding element in the array to flag the operation needed
// to find its new min/max extent.  Note that min, mid and max refer to an array of
// 3 coordinates (x,y,z); each of which are flagged separately. Also note that these
// flags are based on the Y vector being the up vector.
const
   FlagMax: array[0..7] of array [0..2] of byte = (
      (MID,MAX,MAX), //Upper Fore Left
      (MAX,MAX,MAX), //Upper Fore Right
      (MID,MAX,MID), //Upper Back Left
      (MAX,MAX,MID), //Upper Back Right

      (MID,MID,MAX), //Lower Fore Left   (similar to above except height/2)
      (MAX,MID,MAX), //Lower Fore Right
      (MID,MID,MID), //Lower Back Left
      (MAX,MID,MID)  //Lower Back Right
    );

   FlagMin: array[0..7] of array [0..2] of byte = (
      (MIN,MID,MID), //Upper Fore Left
      (MID,MID,MID), //Upper Fore Right
      (MIN,MID,MIN), //Upper Back Left
      (MID,MID,MIN), //Upper Back Right

      (MIN,MIN,MID), //Lower Fore Left  (similar to above except height/2)
      (MID,MIN,MID), //Lower Fore Right
      (MIN,MIN,MIN), //Lower Back Left
      (MID,MIN,MIN)  //Lower Back Right
    );

    //Design of the AABB faces, using similar method to above.. Note than normals are not
    //correct, but not needed for current tri-intersection test.
    //Could be removed if the tri-plane collision is replaced with a tri-box routine.
    FlagFaces: array [0..23] of array[0..2] of byte = (
//Top Face
       (MIN,MAX,MAX), //Upper left corner
       (MAX,MAX,MAX), //Upper right corner
       (MAX,MIN,MAX), //Bottom right corner
       (MIN,MIN,MAX),

//Bottom Face
       (MIN,MAX,MIN), //Upper left corner
       (MAX,MAX,MIN), //Upper right corner
       (MAX,MIN,MIN), //Bottom right corner
       (MIN,MIN,MIN),

//Back Face
       (MIN,MAX,MAX), //Upper left corner
       (MAX,MAX,MAX), //Upper right corner
       (MAX,MAX,MIN), //Bottom right corner
       (MIN,MAX,MIN),

//Front Face
       (MIN,MIN,MAX), //Upper left corner
       (MAX,MIN,MAX), //Upper right corner
       (MAX,MIN,MIN), //Bottom right corner
       (MIN,MIN,MIN),

//Left Face
       (MIN,MAX,MAX), //Upper left corner
       (MIN,MIN,MAX), //Upper right corner
       (MIN,MIN,MIN), //Bottom right corner
       (MIN,MAX,MIN),

//Right Face
       (MAX,MIN,MAX), //Upper left corner
       (MAX,MAX,MAX), //Upper right corner
       (MAX,MAX,MIN), //Bottom right corner
       (MAX,MIN,MIN));

// Destroy
//
destructor TOctree.Destroy;
begin
   DisposeTree;
   inherited Destroy;
end;

// DisposeTree
//
procedure TOctree.DisposeTree;

   procedure WalkDispose(var node : POctreeNode);
   var
      i : Integer;
   begin
      if Assigned(node) then begin
         for i:=0 to 7 do
            WalkDispose(node.ChildArray[i]);
         Dispose(node);
      end;
   end;

begin
   WalkDispose(RootNode);
   RootNode:=nil;
   triangleFiler.free;
   triangleFiler:= nil;   
end;

// CreateTree
//
procedure TOctree.CreateTree(depth : Integer);
begin
   MaxOlevel:=depth;  //initialize max depth.
   Refine(rootnode, 0);
end;


// "cuts" all the triangles in the mesh into the octree.
procedure TOctree.CutMesh;

   procedure AddTriangleToNodes(n: integer);
   var
      x, k : integer;
      p : POctreeNode;
   begin
      for x:=0 to High(resultArray) do begin
         p:=resultarray[x];   // Pointer to a node.

         k:=Length(p.TriArray);
         SetLength(p.TriArray, k+1);  // Increase array by 1.
         p.TriArray[k]:=n;    // Store triangle # reference.

{$ifdef DEBUG}
         Inc(intersections);
{$endif}
      end;
   end;

var
   n : Integer;  //n = triangle # in mesh
begin
   TriCountMesh:=triangleFiler.Count div 3;
   n:=0;
   while n<triangleFiler.Count do begin
      WalkTriToLeaf(RootNode, triangleFiler.List[n],
                              triangleFiler.List[n+1],
                              triangleFiler.List[n+2]);
      if resultArray <> NIL then begin
         AddTriangleToNodes(n);
         Inc(TriCountOctree, 1);
      end;
      Inc(n, 3);
   end;
end;

function TOctree.GetMidPoint(min, max: single): single;
begin
result:=max/2+min/2;   //This formula is non-quadrant specific; ie: good.
end;

function TOctree.GetExtent(const flags: array of byte; ParentNode: POctreeNode ): TAffineFLTVector;
var emin, emax: TAffineFLTVector;
    n: integer;
begin
   emin:=ParentNode^.MinExtent;  //Some easy to work with variables.
   emax:=ParentNode^.MaxExtent;

   for n:=0 to 2 do begin
     case flags[n] of
       MIN: result[n]:=emin[n];
       MID: result[n]:=GetMidPoint(emin[n],emax[n]);
       MAX: result[n]:=emax[n];
     end;
   end;
end;

// InitializeTree
//
procedure TOctree.InitializeTree(const worldMinExtent, worldMaxExtent : TAffineVector;
                                 const triangles : TAffineVectorList;
                                 const treeDepth : Integer);
var
   n : Integer;
   newnode : POctreeNode;
begin
   Self.WorldMinExtent:=worldMinExtent;
   Self.WorldMaxExtent:=worldMaxExtent;

   //set up the filer data for this mesh
   if triangleFiler=nil then
      triangleFiler:=TAffineVectorList.Create;
   triangleFiler.Assign(triangles);

   New(newnode);
   newnode^.MinExtent:=WorldMinExtent;
   newnode^.MaxExtent:=WorldMaxExtent;
   newnode^.TriArray:=NIL;
   for n:=0 to 7 do newnode^.ChildArray[n]:=NIL;

   //Initialize work variables for new tree.
   rootnode:=newnode; //rootnode always points to root.
   NodeCount:=0;     //initialize node count

   CreateTree(treeDepth);
   CutMesh;
end;

// Refine
//
procedure TOctree.Refine(parentNode : POctreeNode; level : Integer);
var
   n, x, z : Integer;
   pwork : array [0..7] of POctreeNode;   //Stores addresses of newly created children.
   newnode : POctreeNode;
begin
   if level < MaxOlevel then begin
      for n:=0 to 7 do begin                 //Create 8 new children under this parent.
         Inc(NodeCount);
         New(newnode);
         Pwork[n]:=newnode;                  //Create work pointers for the next for loop.

         //Generate new extents based on parent's extents
         newnode^.MinExtent:=GetExtent(flagMin[n], ParentNode);
         newnode^.MaxExtent:=GetExtent(flagMax[n], ParentNode);

         newnode^.TriArray:=nil;             //Initialize.

         for z:=0 to 7 do
            newnode^.ChildArray[z]:=nil;     //initialize all child pointers to NIL

         ParentNode^.ChildArray[n]:=newnode; //initialize parent's child pointer to this node
      end;
      for x:=0 to 7 do     // Now recursively Refine each child we just made
          Refine(pwork[x], level+1);
   end; //end if
end;

// ConvertR4
//
procedure TOctree.ConvertR4(ONode: POctreeNode; const scale : TAffineFLTVector);
var
   n: smallint;
begin
   ScaleVector(Onode.MinExtent, scale);
   ScaleVector(Onode.MaxExtent, scale);
   if ONode.ChildArray[0] <> NIL then begin //ie: if not a leaf then loop through children.
      for n:=0 to 7 do begin
         ConvertR4(Onode.ChildArray[n], scale);
      end;
   end
end;

// PointInNode
//
function TOctree.PointInNode(const min, max, aPoint: TAffineFLTVector) : BOOLEAN;
begin
   Result:=(aPoint[0]>=min[0]) and (aPoint[1]>=min[1]) and (aPoint[2]>=min[2])
           and (aPoint[0]<=max[0]) and (aPoint[1]<=max[1]) and (aPoint[2]<=max[2]);
end;

// WalkPointToLeaf
//
procedure TOctree.WalkPointToLeaf(Onode: POctreeNode; const p : TAffineVector);
begin
   Finalize(resultarray);
   WalkPointToLeafx(Onode, p);
end;

// WalkPointToLeafx
//
procedure TOctree.WalkPointToLeafx(Onode: POctreeNode; const p : TAffineVector);
var
   n : integer;
begin
   if PointInNode(Onode.MinExtent, Onode.MaxExtent, p) then begin
      if Assigned(Onode.ChildArray[0]) then
        for n:=0 to 7 do
          WalkPointToLeafx(Onode.ChildArray[n], p)
      else begin
         SetLength(resultarray, Length(resultarray)+1);
         resultarray[High(resultarray)]:=Onode;
      end;
   end;
end;

// SphereInNode
//
function TOctree.SphereInNode(const minExtent, maxExtent : TAffineVector;
                              const c : TVector; radius : Single): Boolean;
//Sphere-AABB intersection by Miguel Gomez
var
   s, d : Single;
   i : Integer;
begin
//find the square of the distance
//from the sphere to the box
d:=0;
for i:=0 to 2 do
begin
  if(C[i] < MinExtent[i]) then
  begin
     s := C[i] - MinExtent[i];
     d := d + s*s;
  end
  else if(C[i] > MaxExtent[i]) then
  begin
     s := C[i] - MaxExtent[i];
     d := d + s*s;
  end;
end; //end for

if d<= radius*radius then
   result:=TRUE
else
   result:=FALSE;
end;

// WalkSphereToLeaf
//
procedure TOctree.WalkSphereToLeaf(Onode : POctreeNode; const p : TVector;
                                   radius : Single);
begin
   Finalize(resultarray);
   WalkSphereToLeafx(Onode, p, radius);
end;

// WalkSphereToLeafx
//
procedure TOctree.WalkSphereToLeafx(Onode : POctreeNode; const p : TVector;
                                    radius : Single);
var
   n : integer;
begin
   if SphereInNode(Onode.MinExtent, Onode.MaxExtent, p, radius) then begin
      if Assigned(Onode.ChildArray[0]) then
        for n:=0 to 7 do
          WalkSphereToLeafx(Onode.ChildArray[n], p, radius)
      else begin
         SetLength(resultarray, Length(resultarray)+1);
         resultarray[High(resultarray)]:=Onode;
      end;
   end;
end;

//Cast a ray (point p, vector v) into the Octree (ie: ray-box intersect).
procedure TOctree.WalkRayToLeaf(Onode: POctreeNode; const p, v: TVector);
begin
   finalize(resultarray);

   WalkRayToLeafx(Onode, p, v);
end;

// WalkRayToLeafx
//
procedure TOctree.WalkRayToLeafx(Onode: POctreeNode; const p, v: TVector);
var
   n : integer;
   coord : TVector;
begin
   if HitBoundingBox(Onode.MinExtent, Onode.MaxExtent, p, v, coord) then begin
      if Assigned(Onode.ChildArray[0]) then
         for n:=0 to 7 do
            WalkRayToLeafx(Onode.ChildArray[n], p, v)
      else begin
         SetLength(resultarray, Length(resultarray)+1);
         resultarray[High(resultarray)]:=Onode;
      end;
   end;
end;

//Check triangle intersect with any of the node's faces.
//Could be replaced with a tri-box check.
function TOctree.TriIntersectNode(const minExtent, maxExtent, v1, v2, v3 : TAffineVector) : Boolean;
var
  f0, f1, f2, f3:  TAffineFLTVector;
  n, o, p: integer;
  aFace: array [0..3] of TAffineFLTVector;  //A face's 4 corners.
begin
for n:=0 to 5 do begin               //Do all 6 faces.
  for o:=0 to 3 do begin             //Do all 4 vertices for the face.
   for p:=0 to 2 do begin            //Do x,y,z for each vertex.
     if FlagFaces[o+n*4,p] = MIN then
       aFace[o,p]:=MinExtent[p]
     else
       aFace[o,p]:=MaxExtent[p];
   end; //end for o
  end; //end for p
  f0:=aFace[0];  f1:=aFace[1]; f2:=aFace[2]; f3:=aFace[3];

  //Now check the two triangles in the face against the mesh triangle.
  if tri_tri_intersect(v1, v2, v3, f0, f1, f2) = 1 then
      result:=TRUE
  else
      if tri_tri_intersect(v1, v2, v3, f2, f3, f0) = 1 then
         result:=TRUE
      else
         result:=FALSE;
  if result then exit;

  end; //end for n
end;

// WalkTriToLeaf
//
procedure TOctree.WalkTriToLeaf(Onode: POctreeNode; const v1, v2, v3: TAffineFLTVector);
begin
   finalize(resultarray);
   WalkTriToLeafx(Onode, v1, v2, v3);
end;

// WalkTriToLeafx
//
procedure TOctree.WalkTriToLeafx(Onode: POctreeNode; const v1, v2, v3: TAffineFLTVector);
var
   m : Integer;
begin
   if TriIntersectNode(Onode.MinExtent, Onode.MaxExtent, v1, v2, v3) or
         PointInNode(Onode.MinExtent, Onode.MaxExtent, v1) or
         PointInNode(Onode.MinExtent, Onode.MaxExtent, v2) or
         PointInNode(Onode.MinExtent, Onode.MaxExtent, v3) then begin
      if Onode.ChildArray[0] <> NIL then
        for m:=0 to 7 do
          WalkTriToLeafx(Onode.ChildArray[m], v1, v2, v3)
      else begin
         SetLength(resultarray, Length(resultarray)+1);
         resultarray[High(resultarray)]:=Onode;
      end;
   end;
end;

// RayCastIntersectAABB
//
function TOctree.RayCastIntersect(const rayStart, rayVector : TVector;
                                      intersectPoint : PVector = nil;
                                      intersectNormal : PVector = nil;
                                      triangleInfo : POctreeTriangleInfo = nil) : Boolean;
const
   cInitialMinD : Single = 1e40;
var
   i, t, k : Integer;
   d, minD : Single;
   p : POctreeNode;
   iPoint, iNormal : TVector;
begin
   WalkRayToLeaf(RootNode, rayStart, rayVector);

   if resultarray = nil then begin
      Result:=False;
      Exit;
   end;

   minD:=cInitialMinD;
   for i:=0 to High(resultarray) do begin
      p:=ResultArray[i];
      for t:=0 to High(p.TriArray) do begin
         k:=p.triarray[t];
         if RayCastTriangleIntersect(rayStart, rayVector,
                                     triangleFiler.List[k],
                                     triangleFiler.List[k+1],
                                     triangleFiler.List[k+2],
                                     @iPoint, @iNormal) then begin
            d:=VectorDistance2(rayStart, iPoint);
            if d<minD then begin
               minD:=d;
               if intersectPoint<>nil then
                  intersectPoint^:=iPoint;
               if intersectNormal<>nil then
                  intersectNormal^:=iNormal;
               if triangleInfo<>nil then begin
                  triangleInfo.index:=k;
                  triangleInfo.vertex[0]:=triangleFiler.List[k];
                  triangleInfo.vertex[1]:=triangleFiler.List[k+1];
                  triangleInfo.vertex[2]:=triangleFiler.List[k+2];
               end;
            end;
         end;
      end;
   end;
   Result:=(minD<>cInitialMinD);
end;

// SphereIntersectAABB -- Walk a sphere through an Octree, given a velocity, and return the nearest polygon
// intersection point on that sphere, and its plane normal.
//
// **** This function is the result of a LOT of work and experimentation with both Paul Nettle's method
// (www.fluidstudios.com) and Telemachos' method (www.peroxide.dk) over a period of about 2 months. If
// you find ways to optimize the general structure, please let me know at rmch@cadvision.com. ****
//
// TO DO: R4 conversion (routine already exists for this) for ellipsoid space.
//
//  Method for caclulating CD vs polygon: (Robert Hayes method)
// ...for each triangle:
// 1. Cast a ray from sphere origin to triangle's plane along plane's negative normal (set to length
//    of sphere radius).  If no hit, skip this triangle.  Otherwise this is the default plane
//    intersection point.
// 2. If the distance is =< the sphere radius, the plane is embedded in the sphere.  Go to step 6 with
//    plane intersection point from above step.
// 3. If the distance > sphere radius, calculate the sphere intersection point to this plane by
//    subtracting the plane's normal from the sphere's origin.
// 4. Cast a new ray from the sphere intersection point to the plane; this is the new plane
//    intersection point.
// 5. Cast a ray from the sphere intersection point to the triangle.  If it is a direct hit, then
//    this point is the polygon intersection point.
// 6. Else, find the point on the triangle that is closest to the plane intersection point; this becomes
//    the polygon intersection point (ie: hit an edge or corner)
// 7. Cast a ray from the polygon intersection point along the negative velocity vector of the sphere
//    (note: for accuracy reasons - SphereIntersect replaced with PointInSphere)
// 8. If there is no intersection, the sphere cannot possibly collide with this triangle
// 9. Else, save the distance from step 8 if, and only if, it is the shortest collision distance so far.
//
// Return the polygon intersection point and the closest triangle's normal if hit.
//
function TOctree.SphereSweepIntersect(const rayStart, rayVector : TVector;
                                     const velocity, radius: single;
                                     intersectPoint : PVector = nil;
                                     intersectNormal : PVector = nil) : Boolean;
const
   cInitialMinD2 : Single = 1e40;
   cEpsilon : Single = 0.05;

var
   i, t, k : Integer;
   minD2, sd2, radius2 : Single;
   distanceToTravel, distanceToTravelMinusRadius2 : Single;
   p: POctreeNode;
   pNormal: TAffineVector;
   pNormal4: TVector;
   NEGpNormal4: TVector;
   sIPoint, sIPoint2: TVector;     //sphere intersection point
   pIPoint: TVector;     //plane intersection point
   polyIPoint: TVector;  //polygon intersection point
   NEGVelocity: TVector; //sphere's forward velocity
   directHit : Boolean;

   p1, p2, p3: PAffineVector;

   //SphereSweepAABB:TAABB;
   
   //response identifiers (for future use)
   //destinationPoint, newdestinationPoint: TVector;
   //slidePlaneOrigin, slidePlaneNormal: TVector;
   //newvelocityVector: TVector;
   //v: single;
   //L: double;
begin
   //Note: Current implementation only accurate for FreeForm:Radius at 1:1 ratio.

   Result:=False;  //default: no collision

   //quit if no movement
   if (velocity=0)or(not (VectorNorm(rayVector)>0)) then Exit;
   //How far ahead to check for collisions.
   distanceToTravel:=velocity+radius+cEpsilon;
   distanceToTravelMinusRadius2:=Sqr(velocity+cEpsilon);
   radius2:=Sqr(radius);

   //Determine all the octree nodes this sphere intersects with.
   //NOTE: would be more efficient to find the bounding box that includes the
   //startpoint and endpoint (+sphere diameter)... especially with a large sphere
   //and/or a large velocity.
   WalkSphereToLeaf(RootNode, RayStart, distanceToTravel);

//   This method may be more effective if sphere sweeps from one point to another and stops.
//   NOTE: If it recursively slides of planes, then WalkSphereToLeaf would probably be better, as
//   it will determine all possible triangles that could intersect over the whole motion
//   AABBFromSweep(SphereSweepAABB,rayStart,destinationPoint,Radius+cEpsilon);
//   GetTrianglesFromNodesIntersectingAABB(SphereSweepAABB);

   if not Assigned(resultarray) then exit;

   //Negative velocity vector for use with ray-sphere check.
   VectorScale(rayVector, -velocity/VectorLength(rayVector), NEGVelocity);

   minD2:=cInitialMinD2;
   for i:=0 to High(resultarray) do begin
      p:=ResultArray[i];
      for t:=0 to High(p.TriArray) do begin
         k:=p.triarray[t];
         //These are the vertices of the triangle to check
         p1:=@trianglefiler.List[k];
         p2:=@trianglefiler.List[k+1];
         p3:=@trianglefiler.List[k+2];

         //Create the normal for this triangle
         pNormal:=CalcPlaneNormal(p1^, p2^, p3^);

         //Ignore backfacing planes
         if VectorDotProduct(pNormal, PAffineVector(@rayVector)^)>0.0 then Continue;

         //Set the normal to the radius of the sphere
         ScaleVector(pNormal, radius);
         //Make some TVectors
         MakeVector(pNormal4, pNormal);
         NEGpNormal4:=VectorNegate(pNormal4);

         //Calculate the plane intersection point (sphere origin to plane)
         if RayCastPlaneIntersect(RayStart, NEGpNormal4, VectorMake(p1^),
                                  pNormal4, @pIPoint) then begin
            directHit:=False;
            sd2:=VectorDistance2(rayStart, pIPoint);

            //If sd <= radius, fall through to "not direct hit" code below with pIPoint
            //as the plane intersection point.  Sphere is embedded.
            //Otherwise...
            if sd2 > radius2 then begin
               //Calculate sphere intersection point (ie: point on sphere that hits plane)
               SetVector(sIPoint, VectorSubtract(RayStart, pNormal4));
               //Get new plane intersection point (in case not a direct hit)
               RayCastPlaneIntersect(sIPoint, RayVector, VectorMake(p1^), pNormal4, @pIPoint);
               //Does the velocity vector directly hit the triangle? If yes then that is the
               //polygon intersection point.
               if RayCastTriangleIntersect(sIPoint, RayVector,
                                           p1^, p2^, p3^, @polyIPoint, @pNormal4) then begin
                  sd2:=VectorDistance2(sIPoint, polyIPoint);
                  directHit:=True;
               end;
            end;

            //If not a direct hit then check if sphere "nicks" the triangle's edge or corner...
            //If it does then that is the polygon intersection point.
            if not directHit then begin
               if not CheckPointInTriangle(AffineVectorMake(piPoint),p1^,p2^,p3^) then
               SetVector(polyIPoint,
                         ClosestPointOnTriangleEdge(p1^, p2^, p3^,
                                                PAffineVector(@pIPoint)^),1)
               else
                 polyIPoint:=piPoint;

               //See if this point + negative velocity vector lies within the sphere.
               //(This implementation seems more accurate than RayCastSphereIntersect)
              { if not CheckPointInSphere(VectorAdd(polyIPoint, NEGVelocity), raystart, radius) then
                  continue;
              // sd2:=0;  //stops the test too soon (noticable on triangle edges in flat planes)
               sd2:=sqr(VectorDistance(raystart, polyIPoint)-Radius);
              }
               //see if this point + negative velocity vector intersect the sphere.
               //(PointInSphere doesn't work for fast motion)
               if VectorDistance2(polyIPoint,rayStart)>radius2 then
               begin
                 if RayCastSphereIntersect(polyIPoint,VectorNormalize(NEGVelocity),rayStart,radius,sIPoint,sIPoint2)=0 then
                   continue;
                 sd2:=VectorDistance2(sIPoint,polyIPoint);
               end
               else
                 sd2:=0;
            end;

            // Allow sphere to get close to triangle (less epsilon which is built into distanceToTravel)
            if sd2<=distanceToTravelMinusRadius2 then begin
               Result:=True; //flag a collision
               if sd2<minD2 then begin
                  minD2:=sd2;
                  if intersectPoint<>nil then intersectPoint^:=polyIPoint;
                  if intersectNormal<>nil then intersectNormal^:=pNormal4;
                  if sd2=0 then Exit;
               end;
            end;
         end;
      end;  //end for t triangles
   end; //end for i nodes
end;

function TOctree.TriangleIntersect(const v1, v2, v3: TAffineVector): boolean;
var
   i, t, k:integer;
   p: POctreeNode;
   p1, p2, p3: PAffineVector;
begin
   Result:=False;  //default: no collision
   WalkTriToLeaf(RootNode, v1, v2, v3);
   if not Assigned(resultarray) then exit;

   for i:=0 to High(resultarray) do begin
      p:=ResultArray[i];
      for t:=0 to High(p.TriArray) do begin
         k:=p.triarray[t];
         //These are the vertices of the triangle to check
         p1:=@trianglefiler.List[k];
         p2:=@trianglefiler.List[k+1];
         p3:=@trianglefiler.List[k+2];
         if tri_tri_intersect(v1, v2, v3, p1^, p2^, p3^)<>0 then
         begin
            result:= true;
            exit;
         end;
      end;  //end for t triangles
   end; //end for i nodes
end;

// AABBIntersect
//
function TOctree.AABBIntersect(const AABB: TAABB;
m1to2, m2to1: TMatrix; triangles: TAffineVectorList = nil): boolean;
var
  triList: TAffineVectorList;
  i: integer;
begin
     //get triangles in nodes intersected by the aabb
     triList:= GetTrianglesFromNodesIntersectingCube(aabb, m1to2, m2to1);

     result:= false;
     if Trilist.Count>0 then begin
          trilist.TransformAsPoints(m2to1);
          i:= 0;
          //run all faces and check if they're inside the aabb
          //uncoment the * and comment the {//} to check only vertices
     {//} while i < triList.Count -1 do begin
          //*for i:= 0 to triList.count -1 do begin
          //*  v:=VectorMake(TriList.Items[i]);
          //*  if pointInAABB(AffinevectorMake(v), aabb) then
          {//} if TriangleIntersectAABB(aabb, triList[i], triList[i+1], trilist[i+2]) then begin
                    Result:=True;
                    if not Assigned(triangles) then break
                    else
                      triangles.Add(triList[i], triList[i+1], trilist[i+2]);
               end;
          {//} i:= i+3;
          end;
      end;

     triList.Free;
end;

// GetTrianglesFromNodesIntersectingAABB
//
function TOctree.GetTrianglesFromNodesIntersectingAABB(const objAABB : TAABB) : TAffineVectorList;
var
  AABB1 : TAABB;

   procedure HandleNode(Onode: POctreeNode);
   var
      AABB2: TAABB;
      i: integer;
   begin
      AABB2.min:=Onode.MinExtent;
      AABB2.max:=Onode.MaxExtent;

      if IntersectAABBsAbsolute(AABB1, AABB2) then begin
         if Assigned(Onode.ChildArray[0]) then begin
            for i:=0 to 7 do
               HandleNode(Onode.ChildArray[i])
         end else begin
            SetLength(ResultArray, Length(ResultArray)+1);
            ResultArray[High(ResultArray)]:=Onode;
         end;
      end;
   end;

var
   i, k : Integer;
   p : POctreeNode;
   triangleIndices : TIntegerList;

begin
   //Calc AABBs
   AABB1:=objAABB;
   
   SetLength(ResultArray, 0);
   if Assigned(RootNode) then
      HandleNode(RootNode);

   Result:=TAffineVectorList.Create;
   triangleIndices:=TIntegerList.Create;
   try
      //fill the triangles from all nodes in the resultarray to AL
      for i:=0 to High(ResultArray) do begin
         p:=ResultArray[i];
         triangleIndices.AddIntegers(p.TriArray);
      end;
      triangleIndices.SortAndRemoveDuplicates;
      Result.Capacity:=triangleIndices.Count*3;
      for i:=0 to triangleIndices.Count-1 do begin
         k:=triangleIndices[i];
         Result.Add(triangleFiler.List[k], triangleFiler.List[k+1], triangleFiler.List[k+2]);
      end;
   finally
      triangleIndices.Free;
   end;

end;

// GetTrianglesFromNodesIntersectingCube
//
function TOctree.GetTrianglesFromNodesIntersectingCube(const objAABB : TAABB;
                                    const objToSelf, selfToObj : TMatrix) : TAffineVectorList;
var
  AABB1 : TAABB;
  M1To2, M2To1 : TMatrix;

   procedure HandleNode(Onode: POctreeNode);
   var
      AABB2: TAABB;
      i: integer;
   begin
      AABB2.min:=Onode.MinExtent;
      AABB2.max:=Onode.MaxExtent;

      if IntersectAABBs(AABB1, AABB2, M1To2, M2To1) then begin
         if Assigned(Onode.ChildArray[0]) then begin
            for i:=0 to 7 do
               HandleNode(Onode.ChildArray[i])
         end else begin
            SetLength(ResultArray, Length(ResultArray)+1);
            ResultArray[High(ResultArray)]:=Onode;
         end;
      end;
   end;

var
   i, k : Integer;
   p : POctreeNode;
   triangleIndices : TIntegerList;
begin
   //Calc AABBs
   AABB1:=objAABB;
   // Calc Conversion Matrixes
   M1To2:=objToSelf;
   M2To1:=selfToObj;

   SetLength(ResultArray, 0);
   if Assigned(RootNode) then
      HandleNode(RootNode);

   Result:=TAffineVectorList.Create;
   triangleIndices:=TIntegerList.Create;
   try
      //fill the triangles from all nodes in the resultarray to AL
      for i:=0 to High(ResultArray) do begin
         p:=ResultArray[i];
         triangleIndices.AddIntegers(p.TriArray);
      end;
      triangleIndices.SortAndRemoveDuplicates;
      Result.Capacity:=triangleIndices.Count*3;
      for i:=0 to triangleIndices.Count-1 do begin
         k:=triangleIndices[i];
         Result.Add(triangleFiler.List[k], triangleFiler.List[k+1], triangleFiler.List[k+2]);
      end;
   finally
      triangleIndices.Free;
   end;
end;

end.

