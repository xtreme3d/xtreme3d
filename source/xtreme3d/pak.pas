
function SetPakArchive(fname: pchar): real; cdecl;
var
  pak: TGLVfsPak;
begin
  pak := TGLVfsPak.Create(scene);
  pak.LoadFromFile(String(fname), $0002 or $0020);
  result := integer(pak);
end;

function PakGetFileCount(p: real): real; cdecl;
var
  pak: TGLVfsPak;
begin
  pak := TGLVfsPak(trunc64(p));
  result := pak.FileCount;
end;

function PakGetFileName(p, index: real): pchar; cdecl;
var
  pak: TGLVfsPak;
begin
  pak := TGLVfsPak(trunc64(p));
  result := pchar(pak.Files[trunc64(index)]);
end;

function PakExtract(p: real; dir: pchar): real; cdecl;
var
  pak: TGLVfsPak;
  i: Integer;
  basedir, fname, path: String;
begin
  pak := TGLVfsPak(trunc64(p));
  basedir := GetCurrentDir + '\' + String(dir);
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

function PakExtractFile(p, index: real; newname: pchar): real; cdecl;
var
  pak: TGLVfsPak;
begin
  pak := TGLVfsPak(trunc64(p));
  pak.Extract(trunc64(index), newname);
  result := 1.0;
end;
