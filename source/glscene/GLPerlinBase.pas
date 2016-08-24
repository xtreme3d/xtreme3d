// GLPerlinBase
{: Functions for generating perlin noise.<p>

   Just some base functions for Perlin noise<p>

	<b>History : </b><font size=-1><ul>
      <li>29/01/03 - JaJ - Submitted to GLScene.
	</ul></font>
}
unit GLPerlinBase;

interface

type
   T1DPerlinArray = array of Double;
   T2DPerlinArray = array of T1DPerlinArray;

// Useless for final output! Usefull for after interpolation, as its FAST!
function Linear_Interpolate(const a, b, x : Double) : Double;
// does a cubic interpolation
function Cubic_Interpolate(v0, v1, v2, v3,x : Double) : Double;
// does a cosine interpolation
Function Cosine_Interpolate(const a, b, x : Double) : Double;
// just a random controlled by X
Function Perlin_Random1(X : Integer) : Double;
// just a random controlled by X,Y
Function Perlin_Random2(Const X,Y : Integer) : Double;
// generates a random strip
Procedure Perlin_Random1DStrip(X,Width,Step : Integer; Amp : Double; Res : T1DPerlinArray);
// cubic interpolate 4 strips into one...
procedure Cubic_Interpolate_Strip(B1,B2,B3,B4,Res : T1DPerlinArray; Width : Integer);
// smooth interpolate 3 strips into one...
procedure Smooth_Interpolate_Strip(B1,B2,B3,Res : T1DPerlinArray; Width : Integer);

// a function returning some integer based on the root^exponant concept,
// result is crap and is only for "random" usage... eg perlin.
Function ExponateCrap(root, exponant : Integer) : Integer;

implementation

type
   PDouble = ^Double;

Function ExponateCrap(root, exponant : Integer) : Integer;

Var
  D : Extended;
Begin
  if root <= 0 then
    Result := 0
  else begin
    D := exp(ln(root)*exponant);
    If D >= 1e30 then //= Infinity then
    D := root*exponant;
// if you got a better(faster) way of carving some integer value out of a double let me know!
    if D > maxInt then
      Result := maxInt
    else
      Result := Round(D);
  End;
End;

Function Perlin_Random1(X : Integer) : Double;

Begin
  x := ExponateCrap((x shl 13) + (x shr 9), x);
  // mess up the number real good!

  //                     X        X          X       those three number can be played with, primes are incouraged!
  X := ( (x * (x * x * 15731 + 789221) + 1376312589) And $7fffffff);

  Result :=  1.0 - X / 1073741824.0 // make it a [-1;1] affair!
End;

Function Perlin_Random2(const X, Y : Integer) : Double;

Begin
  // it works! I guess any prime will do!
  Result := Perlin_Random1(x+y*57);
End;

Procedure Perlin_Random1DStrip(X,Width,Step : Integer; Amp : Double; Res : T1DPerlinArray);

Var
  Posi : PDouble;
  XC : Integer;
Begin
  Posi := @Res[0];
  For XC := 0 to Width-1 do
  Begin
    Posi^ := Perlin_Random1(X)*Amp;
    inc(Integer(Posi),SizeOf(Double));
    Inc(X,Step);
  End;
End;

procedure Smooth_Interpolate_Strip(B1,B2,B3,Res : T1DPerlinArray; Width : Integer);

Var
  Posi : PDouble;
  T1 : PDouble;
  T2 : PDouble;
  T3 : PDouble;

  C1 : PDouble;
  C2 : PDouble;
  C3 : PDouble;

  L1 : PDouble;
  L2 : PDouble;
  L3 : PDouble;

  XC : Integer;
Begin
  Posi := @Res[0];
  T1 := @B1[0];
  C1 := @B2[0];
  L1 := @B3[0];

  T2 := Pointer(Integer(T1)+SizeOf(Double));
  C2 := Pointer(Integer(C1)+SizeOf(Double));
  L2 := Pointer(Integer(L1)+SizeOf(Double));

  T3 := Pointer(Integer(T2)+SizeOf(Double));
  C3 := Pointer(Integer(C2)+SizeOf(Double));
  L3 := Pointer(Integer(L2)+SizeOf(Double));

  For XC := 0 to Width-1 do
  Begin
    Posi^ := (T1^+T3^+L1^+L3^) / 16
            +(T2^+C1^+C3^+L2^) / 8
            +C2^             / 4;
    Inc(Integer(Posi),SizeOf(Double));

    T1 := T2;
    C1 := C2;
    L1 := L2;

    T2 := T3;
    C2 := C3;
    L2 := L3;

    Inc(Integer(T3),SizeOf(Double));
    Inc(Integer(C3),SizeOf(Double));
    Inc(Integer(L3),SizeOf(Double));
  End;
End;

procedure Cubic_Interpolate_Strip(B1,B2,B3,B4,Res : T1DPerlinArray; Width : Integer);

Var
  Posi : PDouble;
  V1 : PDouble;
  V2 : PDouble;
  V3 : PDouble;
  V4 : PDouble;

  H1 : PDouble;
  H2 : PDouble;
  H3 : PDouble;
  H4 : PDouble;

  XC : Integer;
Begin
  Posi := @Res[0];
  V1 := @B1[1];
  V2 := @B2[1];
  V3 := @B3[1];
  V4 := @B4[1];

  H1 := @B2[0];
  H2 := @B2[1];
  H3 := @B2[2];
  H4 := @B2[3];

  For XC := 0 to Width-1 do
  Begin
    Posi^ := Cubic_Interpolate(V1^,V2^,V3^,V4^,0.5)/2+Cubic_Interpolate(H1^,H2^,H3^,H4^,0.5)/2;
    Inc(Integer(Posi),SizeOf(Double));

    H1 := H2;
    H2 := H3;
    H3 := H4;
    Inc(Integer(H4),SizeOf(Double));

    Inc(Integer(V1),SizeOf(Double));
    Inc(Integer(V2),SizeOf(Double));
    Inc(Integer(V3),SizeOf(Double));
    Inc(Integer(V4),SizeOf(Double));
  End;
End;

function Linear_Interpolate(const a, b, x : Double) : Double;

Begin
  result := a*(1-x) + b*x
End;

Function Cosine_Interpolate(const a, b, x : Double) : Double;

Var
  ft : Double;
  f : Double;

Begin
	ft := x * pi;
	f := (1 - cos(ft)) * 0.5;

	Result := a*(1-f) + b*f;
End;

function Cubic_Interpolate(v0, v1, v2, v3,x : Double) : Double;

Var
  P, Q, R, S : Double;

Begin
{   Result := Cosine_Interpolate(v1,v2,x);
   Exit;
   v0 := -0.5;
   v1 := 0;
   v2 := 0;
   v3 := -0.5; {}
	P := (v3 - v2) - (v0 - v1);
	Q := (v0 - v1) - P;
	R := v2 - v0;
	S := v1;{}

	Result := (P*x*x*x + Q*x*x + R*x + S);
//   If (Abs(Result) > 1) then
//   Raise exception.create('Cubic_Interpolate result to high, '+FloatToStr(Result)+' values ['+FloatToStr(v0)+';'+FloatToStr(v1)+';'+FloatToStr(v2)+';'+FloatToStr(v3)+']');{}
end;


end.
