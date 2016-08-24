//
// This unit is part of the GLScene Project, http://glscene.org
//
{: DXTC<p>

   DXTC (also S3TC) decoding.<br>
   Adapted from DevIL image library (http://openil.sourceforge.net)<p>

   <b>History : </b><font size=-1><ul>
      <li>03/09/04 - SG - Delphi 5 compatibilty fixes (Ivan Lee Herring)
      <li>01/09/04 - SG - Creation
   </ul></font>
}
unit DXTC;

interface

uses
   SysUtils, VectorGeometry;

procedure DecodeDXT1toBitmap32(
   encData, decData : PByteArray;
   w,h : Integer; var trans : Boolean);
procedure DecodeDXT3toBitmap32(encData, decData : PByteArray; w,h : Integer);
procedure DecodeDXT5toBitmap32(encData, decData : PByteArray; w,h : Integer);

implementation

// DecodeColor565
procedure DecodeColor565(col : Word; var r,g,b : Byte);
begin
   r:=col and $1F;
   g:=(col shr 5) and $3F;
   b:=(col shr 11) and $1F;
end;

// DecodeDXT1toBitmap32
//
procedure DecodeDXT1toBitmap32(
   encData, decData : PByteArray;
   w,h : Integer; var trans : Boolean);
var
   x,y,i,j,k,select : Integer;
   col0, col1 : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bitmask : Cardinal;
   temp : PByte;
   r0,g0,b0,r1,g1,b1 : Byte;
begin
   trans:=False;

   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PByte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;

         if col0>col1 then begin
            colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
            colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
            colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
            colors[2][3]:=$FF;
            colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
            colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
            colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
            colors[3][3]:=$FF;
         end else begin
            trans:=True;
            colors[2][0]:=(colors[0][0]+colors[1][0]) div 2;
            colors[2][1]:=(colors[0][1]+colors[1][1]) div 2;
            colors[2][2]:=(colors[0][2]+colors[1][2]) div 2;
            colors[2][3]:=$FF;
            colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
            colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
            colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
            colors[3][3]:=0;
         end;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

      end;
   end;
end;

// DecodeDXT3toBitmap32
//
procedure DecodeDXT3toBitmap32(encData, decData : PByteArray; w,h : Integer);
var
   x,y,i,j,k,select : Integer;
   col0, col1, wrd : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bitmask, offset : Cardinal;
   temp : PByte;
   r0,g0,b0,r1,g1,b1 : Byte;
   alpha : array[0..3] of Word;
begin
   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PByte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         alpha[0]:=PWord(temp)^;    Inc(temp, 2);
         alpha[1]:=PWord(temp)^;    Inc(temp, 2);
         alpha[2]:=PWord(temp)^;    Inc(temp, 2);
         alpha[3]:=PWord(temp)^;    Inc(temp, 2);
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;
         colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
         colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
         colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
         colors[2][3]:=$FF;
         colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
         colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
         colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
         colors[3][3]:=$FF;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

         for j:=0 to 3 do begin
            wrd:=alpha[j];
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData[offset]:=wrd and $0F;
                  decData[offset]:=decData[offset] or (decData[offset] shl 4);
               end;
               wrd:=wrd shr 4;
            end;
         end;

      end;
   end;
end;

// DecodeDXT5toBitmap32
//
procedure DecodeDXT5toBitmap32(encData, decData : PByteArray; w,h : Integer);
var
   x,y,i,j,k,select : Integer;
   col0, col1 : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bits, bitmask, offset : Cardinal;
   temp, alphamask : PByte;
   r0,g0,b0,r1,g1,b1 : Byte;
   alphas : array[0..7] of Byte;
begin
   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PByte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         alphas[0]:=temp^; Inc(temp);
         alphas[1]:=temp^; Inc(temp);
         alphamask:=temp; Inc(temp, 6);
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;
         colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
         colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
         colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
         colors[2][3]:=$FF;
         colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
         colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
         colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
         colors[3][3]:=$FF;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

         if (alphas[0] > alphas[1]) then begin
            alphas[2]:=(6*alphas[0]+1*alphas[1]+3) div 7;
            alphas[3]:=(5*alphas[0]+2*alphas[1]+3) div 7;
            alphas[4]:=(4*alphas[0]+3*alphas[1]+3) div 7;
            alphas[5]:=(3*alphas[0]+4*alphas[1]+3) div 7;
            alphas[6]:=(2*alphas[0]+5*alphas[1]+3) div 7;
            alphas[7]:=(1*alphas[0]+6*alphas[1]+3) div 7;
         end else begin
            alphas[2]:=(4*alphas[0]+1*alphas[1]+2) div 5;
            alphas[3]:=(3*alphas[0]+2*alphas[1]+2) div 5;
            alphas[4]:=(2*alphas[0]+3*alphas[1]+2) div 5;
            alphas[5]:=(1*alphas[0]+4*alphas[1]+2) div 5;
            alphas[6]:=0;
            alphas[7]:=$FF;
         end;

         bits:=PCardinal(alphamask)^;
         for j:=0 to 1 do begin
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData[Offset]:=alphas[bits and 7];
               end;
               bits:=bits shr 3;
            end;
         end;

         Inc(alphamask, 3);
         bits:=PCardinal(alphamask)^;
         for j:=2 to 3 do begin
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData[offset]:=alphas[bits and 7];
               end;
               bits:=bits shr 3;
            end;
         end;

      end;
   end;
end;

end.