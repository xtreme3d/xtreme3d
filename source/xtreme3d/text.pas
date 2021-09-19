function TextRead(filename: pchar): pchar; cdecl;
begin
    Result := PChar(LoadStringFromFile2(String(filename)));
end;

function TextConvertANSIToUTF8(str: pchar): pchar; cdecl;
begin
  result := PChar(AnsiToUTF8(String(str)));
end;

