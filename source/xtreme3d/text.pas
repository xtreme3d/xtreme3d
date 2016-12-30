function TextRead(filename: pchar): pchar; stdcall;
begin
    Result := PChar(LoadStringFromFile2(String(filename)));
end;

function TextConvertANSIToUTF8(str: pchar): pchar; stdcall;
begin
  result := PChar(AnsiToUTF8(String(str)));
end;

