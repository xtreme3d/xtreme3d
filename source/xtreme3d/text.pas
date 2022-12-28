function TextRead(filename: PAnsiChar): PAnsiChar; cdecl;
var
    fname: String;
begin
    fname := String(AnsiString(filename));
    result := PAnsiChar(AnsiString(LoadStringFromFile(fname)));
end;
