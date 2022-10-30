function PickListCreate(ps: real): real; cdecl;
var
    sortType: TPickSortType;
begin
    sortType := psDefault;
    if ps = 0 then sortType := psDefault;
    if ps = 1 then sortType := psName;
    if ps = 2 then sortType := psMinDepth;
    if ps = 3 then sortType := psMaxDepth;
    result := ObjToReal(TGLPickList.Create(sortType));
end;

function PickListClear(list: real): real; cdecl;
begin
    TGLPickList(RealToPtr(list)).Clear;
    result := 1.0;
end;

function PickListGetCount(list: real): real; cdecl;
begin
    result := TGLPickList(RealToPtr(list)).Count;
end;

function PickListGetHit(list, index: real): real; cdecl;
begin
    result := ObjToReal(TGLPickList(RealToPtr(list)).Hit[Trunc(index)]);
end;

function PointerToReal(p: pchar): real; cdecl;
begin
    result := real(NativeInt(p));
end;

