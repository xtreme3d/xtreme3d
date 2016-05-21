{: GLStarRecord<p>

   Unit to interface with simple star records aimed for background skies.<p>

	<b>History : </b><font size=-1><ul>
	   <li>05/07/03 - EG - Creation
	</ul></font>
}
unit GLStarRecord;

interface

uses VectorGeometry;

type
   TGLStarRecord = packed record
      RA : Word;              // x100 builtin factor, degrees
      DEC : SmallInt;         // x100 builtin factor, degrees
      BVColorIndex : Byte;    // x100 builtin factor
      VMagnitude : Byte;      // x10 builtin factor
   end;
   PGLStarRecord = ^TGLStarRecord;

{: Computes position on the unit sphere of a star record (Z=up). }
function StarRecordPositionZUp(const starRecord : TGLStarRecord) : TAffineVector;
{: Computes position on the unit sphere of a star record (Y=up). }
function StarRecordPositionYUp(const starRecord : TGLStarRecord) : TAffineVector;
{: Computes star color from BV index (RGB) and magnitude (alpha). }
function StarRecordColor(const starRecord : TGLStarRecord; bias : Single) : TVector;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// StarRecordPositionYUp
//
function StarRecordPositionYUp(const starRecord : TGLStarRecord) : TAffineVector;
var
   f : Single;
begin
   SinCos(starRecord.DEC*(0.01*PI/180), Result[1], f);
   SinCos(starRecord.RA*(0.01*PI/180), f,
          Result[0], Result[2]);
end;

// StarRecordPositionZUp
//
function StarRecordPositionZUp(const starRecord : TGLStarRecord) : TAffineVector;
var
   f : Single;
begin
   SinCos(starRecord.DEC*(0.01*PI/180), Result[2], f);
   SinCos(starRecord.RA*(0.01*PI/180), f,
          Result[0], Result[1]);
end;

// StarRecordColor
//
function StarRecordColor(const starRecord : TGLStarRecord; bias : Single) : TVector;
const
   // very *rough* approximation
   cBVm035 : TVector = (0.7, 0.8, 1, 1);
   cBV015  : TVector = (1.0, 1.0, 1.0, 1);
   cBV060  : TVector = (1.0, 1.0, 0.7, 1);
   cBV135  : TVector = (1.0, 0.8, 0.7, 1);
var
   bvIndex100 : Integer;
begin
   bvIndex100:=starRecord.BVColorIndex-50;
   // compute RGB color for B&V index
   if bvIndex100<-035 then
      Result:=cBVm035
   else if bvIndex100<015 then
      VectorLerp(cBVm035, cBV015, (bvIndex100+035)*(1/(015+035)), Result)
   else if bvIndex100<060 then
      VectorLerp(cBV015, cBV060, (bvIndex100-015)*(1/(060-015)), Result)
   else if bvIndex100<135 then
      VectorLerp(cBV060, cBV135, (bvIndex100-060)*(1/(135-060)), Result)
   else Result:=cBV135;
   // compute transparency for VMag
   // the actual factor is 2.512, and not used here
   Result[3]:=Power(1.2, -(starRecord.VMagnitude*0.1-bias));
end;

end.
