function TextRead(filename: pchar): pchar; stdcall;
begin
    Result := PChar(LoadStringFromFile2(String(filename)));
end;
