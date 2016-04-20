// CurvesAndSurfaces
{: Bezier and B-Spline Curve and Surface Routines.<p>

   <b>History : </b><font size=-1><ul>
      <li>11/05/04 - SG - Some fixes for BSpline calculations (rational BSplines
                          are still still broken). Minor knot vector changes.
      <li>20/08/03 - SG - Removed weights realizing it's an inefficient way
                          to do things, control points should be weighted
                          before being used to calculate a surface or curve.
      <li>18/08/03 - SG - Added weights to calculations.
      <li>17/07/03 - SG - Added surface routines. 
                          Minor changes to procedure parameters.
      <li>10/07/03 - SG - Creation
   </ul></font>
}
unit CurvesAndSurfaces;

interface

uses
  SysUtils, VectorGeometry, VectorLists;

type
  TBSplineContinuity = (bscUniformNonPeriodic, bscUniformPeriodic);

function BezierCurvePoint(t : single; n : integer; cp : PAffineVectorArray) : TAffineVector;
function BezierSurfacePoint(s,t : single; m,n : integer; cp : PAffineVectorArray) : TAffineVector;
procedure GenerateBezierCurve(Steps : Integer; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateBezierSurface(Steps, Width, Height : Integer; ControlPoints, Vertices : TAffineVectorList);

function BSplinePoint(t : single; n,k : integer; knots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
function BSplineSurfacePoint(s,t : single; m,n,k1,k2 : integer; uknots, vknots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
procedure GenerateBSpline(Steps,Order : Integer; KnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateBSplineSurface(Steps, UOrder, VOrder, Width, Height : Integer; UKnotVector, VKnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
procedure GenerateKnotVector(KnotVector : TSingleList; NumberOfPoints, Order : Integer; Continuity : TBSplineContinuity);

implementation

function Factorial(n : Integer) : Single;
var
  i : integer;
begin
  if (n<0) or (n>32) then
    Exception.Create('Invalid factorial parameter: n = '+IntToStr(n));

  Result:=1;
  for i:=2 to n do
    Result:=Result*i;
end;

// ------------------------------------------------------------
// Bezier routines
// ------------------------------------------------------------

function BernsteinBasis(n,i : Integer; t : Single) : Single;
var
  ti, tni : Single;
begin
  if (t=0) and (i=0) then ti:=1 else ti:=Power(t,i);
  if (n=i) and (t=1) then tni:=1 else tni:=Power(1-t,n-i);
  Result:=(Factorial(n)/(Factorial(i)*Factorial(n-i)))*ti*tni;
end;

function BezierCurvePoint(t : single; n : integer; cp : PAffineVectorArray) : TAffineVector;
var
  i : integer;
  b : Single;
begin
  Result:=NullVector;
  for i:=0 to n-1 do begin
    b:=BernsteinBasis(n-1,i,t);
    Result[0]:=Result[0]+cp[i][0]*b;
    Result[1]:=Result[1]+cp[i][1]*b;
    Result[2]:=Result[2]+cp[i][2]*b;
  end;
end;

function BezierSurfacePoint(s,t : single; m,n : integer; cp : PAffineVectorArray) : TAffineVector;
var
  i,j : integer;
  b1,b2 : Single;
begin
  Result:=NullVector;
  for j:=0 to n-1 do
    for i:=0 to m-1 do begin
      b1:=BernsteinBasis(m-1,i,s);
      b2:=BernsteinBasis(n-1,j,t);
      Result[0]:=Result[0]+cp[j*m+i][0]*b1*b2;
      Result[1]:=Result[1]+cp[j*m+i][1]*b1*b2;
      Result[2]:=Result[2]+cp[j*m+i][2]*b1*b2;
    end;
end;

procedure GenerateBezierCurve(Steps : Integer; ControlPoints, Vertices : TAffineVectorList);
var
  i : Integer;
begin
  Vertices.Count:=Steps;
  for i:=0 to Steps-1 do
    Vertices[i]:=BezierCurvePoint(i/(Steps-1),ControlPoints.Count,ControlPoints.List);
end;

procedure GenerateBezierSurface(Steps, Width, Height : Integer; ControlPoints, Vertices : TAffineVectorList);
var
  i,j : Integer;
begin
  Vertices.Count:=Steps*Steps;
  for j:=0 to Steps-1 do
    for i:=0 to Steps-1 do
      Vertices[i+j*Steps]:=BezierSurfacePoint(i/(Steps-1),j/(Steps-1),Width,Height,ControlPoints.List);
end;

// ------------------------------------------------------------
// B-Spline routines
// ------------------------------------------------------------

function BSplineBasis(i,k,n : integer; u : Single; knots : PSingleArray) : Single;
var
  v1,v2 : single;
begin
  if (u<knots[i]) or (u>knots[i+k]) then begin
    Result:=0;
  end else if k=1 then begin
    Result:=0;
    if (u>=knots[i]) and (u<knots[i+1]) then
      Result:=1;
  end else if (i=n-1) and (u = knots[i+k]) then begin
    Result:=1;
  end else begin
    v1:=(knots[i+k-1]-knots[i]);
    v2:=(knots[i+k]-knots[i+1]);
    if v1<>0 then
      v1:=(u-knots[i])/v1*BSplineBasis(i,k-1,n,u,knots);
    if v2<>0 then
      v2:=(knots[i+k]-u)/v2*BSplineBasis(i+1,k-1,n,u,knots);
    Result:=v1+v2;
  end;
end;

function BSplinePoint(t : single; n,k : integer; knots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
var
  i : integer;
  b : array of Single;
  det : Single;
begin
  SetLength(b,n);
  for i:=0 to n-1 do b[i]:=BSplineBasis(i,k,n,t,knots);
  det:=0;
  for i:=0 to n-1 do det:=det+b[i];
  Result:=NullVector;
  for i:=0 to n-1 do begin
    if det<>0 then b[i]:=b[i]/det else b[i]:=0;
    Result[0]:=Result[0]+cp[i][0]*b[i];
    Result[1]:=Result[1]+cp[i][1]*b[i];
    Result[2]:=Result[2]+cp[i][2]*b[i];
  end;
  SetLength(b,0);
end;

function BSplineSurfacePoint(s,t : single; m,n,k1,k2 : integer; uknots, vknots : PSingleArray; cp : PAffineVectorArray) : TAffineVector;
var
  i,j : integer;
  b1,b2 : array of Single;
  det1,det2 : Single;
begin
  SetLength(b1,m);
  SetLength(b2,n);
  det1:=0; det2:=0;
  for i:=0 to m-1 do b1[i]:=BSplineBasis(i,k1,m,s,uknots);
  for i:=0 to n-1 do b2[i]:=BSplineBasis(i,k2,n,t,vknots);
  for i:=0 to m-1 do det1:=det1+b1[i];
  for i:=0 to n-1 do det2:=det2+b2[i];
  Result:=NullVector;
  for j:=0 to n-1 do begin
    if det2<>0 then b2[j]:=b2[j]/det2 else b2[j]:=0;
    for i:=0 to m-1 do begin
      if det1<>0 then b1[i]:=b1[i]/det1 else b1[i]:=0;
      Result[0]:=Result[0]+cp[j*m+i][0]*b1[i]*b2[j];
      Result[1]:=Result[1]+cp[j*m+i][1]*b1[i]*b2[j];
      Result[2]:=Result[2]+cp[j*m+i][2]*b1[i]*b2[j];
    end;
  end;
end;

procedure GenerateBSpline(Steps,Order : Integer; KnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
var
  i : Integer;
begin
  Vertices.Clear;
  Vertices.Count:=Steps;
  for i:=0 to Steps-1 do
    Vertices[i]:=BSplinePoint(i/(Steps-1),ControlPoints.Count,Order+1,@KnotVector.List[0],ControlPoints.List);
end;

procedure GenerateBSplineSurface(Steps, UOrder, VOrder, Width, Height : Integer; UKnotVector, VKnotVector : TSingleList; ControlPoints, Vertices : TAffineVectorList);
var
  i,j : Integer;
begin
  Vertices.Clear;
  Vertices.Count:=Steps*Steps;
  for j:=0 to Steps-1 do
    for i:=0 to Steps-1 do
      Vertices[i+j*Steps]:=BSplineSurfacePoint(i/(Steps-1),j/(Steps-1),Width,Height,UOrder+1,VOrder+1,@UKnotVector.List[0],@VKnotVector.List[0],ControlPoints.List);
end;

procedure GenerateKnotVector(KnotVector : TSingleList; NumberOfPoints, Order : Integer; Continuity : TBSplineContinuity);
var
  i,n,k : integer;
begin
  KnotVector.Clear;

  k:=Order+1;
  n:=NumberOfPoints-1;

  case Continuity of

    // Open curve
    bscUniformNonPeriodic : begin
      for i:=0 to n+k do begin
        if i<k then KnotVector.Add(0)
        else if i>n then KnotVector.Add(n-k+2)
        else KnotVector.Add(i-k+1);
      end;
    end;

    // Closed curve
    bscUniformPeriodic : begin
      for i:=0 to n+k do begin
        KnotVector.Add(i);
      end;
      KnotVector.Scale(1/KnotVector.Sum);
    end;

  end;
end;

end.
