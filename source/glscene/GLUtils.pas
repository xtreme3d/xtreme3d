{: GLUtils<p>

   Miscellaneous support utilities & classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}
unit GLUtils;

interface

uses Classes, VectorGeometry, SysUtils, OpenGL1x;

{$i GLScene.inc}

type
	TGLMinFilter   = (miNearest, miLinear, miNearestMipmapNearest,
							miLinearMipmapNearest, miNearestMipmapLinear,
							miLinearMipmapLinear);
	TGLMagFilter   = (maNearest, maLinear);
   
	TSqrt255Array = array [0..255] of Byte;
	PSqrt255Array = ^TSqrt255Array;

//: Copies the values of Source to Dest (converting word values to integer values)
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal);
//: Round ups to the nearest power of two, value must be positive
function RoundUpToPowerOf2(value : Integer): Integer;
//: Round down to the nearest power of two, value must be strictly positive
function RoundDownToPowerOf2(value : Integer): Integer;
//: Returns True if value is a true power of two
function IsPowerOf2(value : Integer) : Boolean;
{: Read a CRLF terminated string from a stream.<p>
   The CRLF is NOT in the returned string. }
function ReadCRLFString(aStream : TStream) : String;
//: Write the string and a CRLF in the stream
procedure WriteCRLFString(aStream : TStream; const aString : String);
//: TryStrToFloat
function TryStrToFloat(const strValue : String; var val : Extended) : Boolean;
//: StrToFloatDef
function StrToFloatDef(const strValue : String; defValue : Extended = 0) : Extended;

{: Parses the next integer in the string.<p>
   Initial non-numeric characters are skipper, p is altered, returns 0 if none
   found. '+' and '-' are acknowledged. }
function ParseInteger(var p : PChar) : Integer;
{: Parses the next integer in the string.<p>
   Initial non-numeric characters are skipper, p is altered, returns 0 if none
   found. Both '.' and ',' are accepted as decimal separators. }
function ParseFloat(var p : PChar) : Extended;

{: Saves "data" to "filename". }
procedure SaveStringToFile(const fileName, data : String);
{: Returns the content of "filename". }
function LoadStringFromFile(const fileName : String) : String;
{: Returns the size of "filename".<p>
   Returns 0 (zero) is file does not exists. }
function SizeOfFile(const fileName : String) : Int64;

{: Returns a pointer to an array containing the results of "255*sqrt(i/255)". }
function GetSqrt255Array : PSqrt255Array;

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

uses ApplicationFileIO;

var
	vSqrt255 : TSqrt255Array;

// WordToIntegerArray
//
procedure WordToIntegerArray(Source: PWordArray; Dest: PIntegerArray; Count: Cardinal); assembler;
// EAX contains Source
// EDX contains Dest
// ECX contains Count
asm
              JECXZ @@Finish
              PUSH ESI
              PUSH EDI
              MOV ESI,EAX
              MOV EDI,EDX
              XOR EAX,EAX
@@1:          LODSW
              STOSD
              DEC ECX
              JNZ @@1
              POP EDI
              POP ESI
@@Finish:
end;

// RoundUpToPowerOf2
//
function RoundUpToPowerOf2(value : Integer) : Integer;
begin
   Result:=1;
   while (Result<value) do Result:=Result shl 1;
end;

// RoundDownToPowerOf2
//
function RoundDownToPowerOf2(value : Integer) : Integer;
begin
   if value>0 then begin
      Result:=1 shl 30;
      while Result>value do Result:=Result shr 1;
   end else Result:=1;
end;

// IsPowerOf2
//
function IsPowerOf2(value : Integer) : Boolean;
begin
   Result:=(RoundUpToPowerOf2(value)=value);
end;

// ReadCRLFString
//
function ReadCRLFString(aStream : TStream) : String;
var
   c : Char;
begin
   Result:='';
   while Copy(Result, Length(Result)-1, 2)<>#13#10 do begin
      aStream.Read(c, 1);
      Result:=Result+c;
   end;
   Result:=Copy(Result, 1, Length(Result)-2);
end;

// WriteCRLFString
//
procedure WriteCRLFString(aStream : TStream; const aString : String);
const
   cCRLF : Integer = $0A0D;
begin
   with aStream do begin
      Write(aString[1], Length(aString));
      Write(cCRLF, 2);
   end;
end;

// TryStrToFloat
//
function TryStrToFloat(const strValue : String; var val : Extended): Boolean;
var
   i, j, divider, lLen, exponent : Integer;
   c : Char;
   v : Extended;
begin
   if strValue='' then begin
      Result:=False;
      Exit;
   end else v:=0;
   lLen:=Length(strValue);
   while (lLen>0) and (strValue[lLen]=' ') do Dec(lLen);
   divider:=lLen+1;
   exponent:=0;
	for i:=1 to lLen do begin
      c:=strValue[i];
      case c of
         ' ' : if v<>0 then begin
            Result:=False;
            Exit;
         end;
         '0'..'9' : v:=(v*10)+Integer(c)-Integer('0');
         ',', '.' : begin
            if (divider>lLen) then
               divider:=i+1
            else begin
               Result:=False;
               Exit;
            end;
         end;
         '-', '+' : if i>1 then begin
            Result:=False;
            Exit;
         end;
         'e', 'E' : begin
            if i+1>lLen then begin
               Result:=False;
               Exit;
            end;
            for j:=i+1 to lLen do begin
               c:=strValue[j];
               case c of
                  '-', '+' : if j<>i+1 then begin
         				Result:=False;
                     Exit;
                  end;
                  '0'..'9' : exponent:=(exponent*10)+Integer(c)-Integer('0');
               else
                  Result:=False;
                  Exit;
               end;
            end;
            if strValue[i+1]<>'-' then
               exponent:=-exponent;
            exponent:=exponent-1;
            lLen:=i;
            if divider>lLen then
               divider:=lLen;
            Break;
         end;
		else
         Result:=False;
         Exit;
      end;
   end;
   divider:=lLen-divider+exponent+1;
   if strValue[1]='-' then begin
      v:=-v;
   end;
   if divider<>0 then
      v:=v*Exp(-divider*Ln(10));
   val:=v;
   Result:=True;
end;

// StrToFloatDef
//
function StrToFloatDef(const strValue : String; defValue : Extended = 0) : Extended;
begin
   if not TryStrToFloat(strValue, Result) then
      result:=defValue;
end;

// ParseInteger
//
function ParseInteger(var p : PChar) : Integer;
var
   neg : Boolean;
   c : Char;
begin
   Result:=0;
   if p=nil then Exit;
   neg:=False;
   // skip non-numerics
   while not (p^ in [#0, '0'..'9', '+', '-']) do Inc(p);
   c:=p^;
   if c='+' then
      Inc(p)
   else if c='-' then begin
      neg:=True;
      Inc(p);
   end;
   // Parse numerics
   while True do begin
      c:=p^;
      if not (c in ['0'..'9']) then Break;
      Result:=Result*10+Integer(c)-Integer('0');
      Inc(p);
   end;
   if neg then
      Result:=-Result;
end;

// ParseFloat
//
function ParseFloat(var p : PChar) : Extended;
var
   decimals, expSign, exponent : Integer;
   c : Char;
   neg : Boolean;
begin
   Result:=0;
   if p=nil then Exit;
   // skip non-numerics
   while not (p^ in [#0, '0'..'9', '+', '-']) do Inc(p);
   c:=p^;
   if c='+' then begin
      neg:=False;
      Inc(p);
   end else if c='-' then begin
      neg:=True;
      Inc(p);
   end else neg:=False;
   // parse numbers
   while (p^ in ['0'..'9']) do begin
      Result:=Result*10+(Integer(p^)-Integer('0'));
      Inc(p);
   end;
   // parse dot, then decimals, if any
   decimals:=0;
   if (p^='.') then begin
      Inc(p);
      while (p^ in ['0'..'9']) do begin
         Result:=Result*10+(Integer(p^)-Integer('0'));
         Inc(p);
         Dec(decimals);
      end;
   end;
   // parse exponent, if any
   if (p^ in ['e', 'E']) then begin
      Inc(p);
      // parse exponent sign
      c:=p^;
      if c='-' then begin
         expSign:=-1;
         Inc(p);
      end else if c='+' then begin
         expSign:=1;
         Inc(p);
      end else expSign:=1;
      // parse exponent
      exponent:=0;
      while (p^ in ['0'..'9']) do begin
         exponent:=exponent*10+(Integer(p^)-Integer('0'));
         Inc(p);
      end;
      decimals:=decimals+expSign*exponent;
   end;
   if decimals<>0 then Result:=Result*Exp(decimals*Ln(10));
   if neg then Result:=-Result;
end;

// SaveStringToFile
//
procedure SaveStringToFile(const fileName, data : String);
var
   n : Cardinal;
	fs : TStream;
begin
	fs:=CreateFileStream(fileName, fmCreate);
   try
      n:=Length(data);
      if n>0 then
      	fs.Write(data[1], n);
   finally
   	fs.Free;
   end;
end;

// LoadStringFromFile
//
function LoadStringFromFile(const fileName : String) : String;
var
   n : Cardinal;
	fs : TStream;
begin
   if FileExists(fileName) then begin
   	fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
      try
         n:=fs.Size;
   	   SetLength(Result, n);
         if n>0 then
         	fs.Read(Result[1], n);
      finally
   	   fs.Free;
      end;
   end else Result:='';
end;

// SizeOfFile
//
function SizeOfFile(const fileName : String) : Int64;
var
	fs : TStream;
begin
   if FileExists(fileName) then begin
   	fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
      try
         Result:=fs.Size;
      finally
   	   fs.Free;
      end;
   end else Result:=0;
end;

// GetSqrt255Array
//
function GetSqrt255Array : PSqrt255Array;
const
   cOneDiv255 = 1/255;
var
	i : Integer;
begin
	if vSqrt255[255]<>255 then begin
		for i:=0 to 255 do
			vSqrt255[i]:=Integer(Trunc(255*Sqrt(i*cOneDiv255)));
	end;
	Result:=@vSqrt255;
end;

end.
