function TextRead(filename: PAnsiChar): PAnsiChar; cdecl;
var
    fname: String;
begin
    fname := String(AnsiString(filename));
    result := PAnsiChar(AnsiString(LoadStringFromFile(fname)));
end;

{
function TextConvertANSIToUTF8(str: pchar): pchar; cdecl;
begin
  result := PChar(AnsiToUTF8(String(str)));
end;
}
