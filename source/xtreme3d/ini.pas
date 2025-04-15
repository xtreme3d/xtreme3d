function IniCreate(filename: PAnsiChar): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile.Create(StrConv(filename), TEncoding.UTF8);
    result := ObjToReal(inifile);
end;

function IniClose(ini: real): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    inifile.Free;
    result := 1;
end;

function IniWriteString(ini: real; section, key, value: PAnsiChar): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    inifile.WriteString(StrConv(section), StrConv(key), StrConv(value));
    result := 1;
end;

function IniWriteNumber(ini: real; section, key: PAnsiChar; value: real): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    inifile.WriteFloat(StrConv(section), StrConv(key), value);
    result := 1;
end;

function IniWriteBool(ini: real; section, key: PAnsiChar; value: real): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    inifile.WriteBool(StrConv(section), StrConv(key), Boolean(trunc(value)));
    result := 1;
end;

function IniReadString(ini: real; section, key, defaultvalue: PAnsiChar): PAnsiChar; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    result := PAnsiChar(UTF8String(inifile.ReadString(StrConv(section), StrConv(key), StrConv(defaultvalue))));
end;

function IniReadNumber(ini: real; section, key: PAnsiChar; defaultvalue: real): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    result := inifile.ReadFloat(StrConv(section), StrConv(key), defaultvalue);
end;

function IniReadBool(ini: real; section, key: PAnsiChar; defaultvalue: real): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    result := Integer(inifile.ReadBool(StrConv(section), StrConv(key), Boolean(trunc(defaultvalue))));
end;

function IniUpdateFile(ini: real): real; cdecl;
var
    inifile: TMemIniFile;
begin
    inifile := TMemIniFile(RealToPtr(ini));
    inifile.UpdateFile;
    result := 1;
end;
