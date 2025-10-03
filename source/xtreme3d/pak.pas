function SetPakArchive(fname: PAnsiChar): real; cdecl;
var
    pak: TGLVfsPak;
begin
    pak := TGLVfsPak.Create(scene);
    pak.LoadFromFile(StrConv(fname), $0002 or $0020);
    result := ObjToReal(pak);
end;

function PakGetFileCount(p: real): real; cdecl;
var
    pak: TGLVfsPak;
begin
    pak := TGLVfsPak(RealToPtr(p));
    result := pak.FileCount;
end;

function PakGetFileName(p, index: real): PAnsiChar; cdecl;
var
    pak: TGLVfsPak;
begin
    pak := TGLVfsPak(RealToPtr(p));
    result := PAnsiChar(AnsiString(pak.Files[Trunc(index)]));
end;

function PakExtract(p: real; dir: PAnsiChar): real; cdecl;
var
    pak: TGLVfsPak;
    i: Integer;
    basedir, fname: String;
begin
    pak := TGLVfsPak(RealToPtr(p));
    basedir := GetCurrentDir + '\' + StrConv(dir);
    for i:=0 to pak.Files.Count-1 do
    begin
        fname := basedir + '\' + pak.Files[i];
        ForceDirectories(ExtractFileDir(normalizeSlashes(fname)));
        try
            pak.Extract(pak.Files[i], fname);
        except
            On E: Exception do
                ShowMessage(E.Message);
        end;
    end;
    result := 1.0;
end;

function PakExtractFile(p, index: real; newname: PAnsiChar): real; cdecl;
var
    pak: TGLVfsPak;
begin
    pak := TGLVfsPak(RealToPtr(p));
    pak.Extract(Trunc(index), StrConv(newname));
    result := 1.0;
end;
