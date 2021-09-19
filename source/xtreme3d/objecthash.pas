function ObjectHashCreate(): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash.Create();
  result := Integer(h);
end;

function ObjectHashSetItem(hash: real; key: pchar; obj: real): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash(trunc64(hash));
  h[String(key)] := TObject(trunc64(obj));
  result := 1.0;
end;

function ObjectHashGetItem(hash: real; key: pchar): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash(trunc64(hash));
  if h.Exists(String(key)) then
    result := Integer(h[String(key)])
  else
    result := 0.0;
end;

function ObjectHashDeleteItem(hash: real; key: pchar): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash(trunc64(hash));
  if h.Exists(String(key)) then
    h.Delete(String(key));
  result := 1.0;
end;

function ObjectHashGetItemCount(hash: real): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash(trunc64(hash));
  result := h.ItemCount;
end;

function ObjectHashClear(hash: real): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash(trunc64(hash));
  h.Clear;
  result := 1.0;
end;

function ObjectHashDestroy(hash: real): real; cdecl;
var
  h: TObjectHash;
begin
  h := TObjectHash(trunc64(hash));
  h.Clear;
  h.Destroy;
  result := 1.0;
end;
