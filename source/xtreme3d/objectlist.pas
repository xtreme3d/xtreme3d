function ObjectListCreate: real; cdecl;
var
  list: TGLPersistentObjectList;
begin
  list := TGLPersistentObjectList.Create;
  result := ObjToReal(list);
end;

function ObjectListAdd(list, obj: real): real; cdecl;
var
  li: TGLPersistentObjectList;
begin
  li := TGLPersistentObjectList(RealToPtr(list));
  li.Add(TObject(RealToPtr(list)));
  result := 1;
end;

function ObjectListGetCount(list: real): real; cdecl;
var
  li: TGLPersistentObjectList;
begin
  li := TGLPersistentObjectList(RealToPtr(list));
  result := li.Count;
end;
