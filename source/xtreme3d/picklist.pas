function PickListCreate(ps: integer): Pointer; cdecl;
var
    sortType: TPickSortType;
begin
    sortType := psDefault;
    if ps = 0 then sortType := psDefault;
    if ps = 1 then sortType := psName;
    if ps = 2 then sortType := psMinDepth;
    if ps = 3 then sortType := psMaxDepth;
    result := TGLPickList.Create(sortType);
end;

function PickListClear(list: Pointer): integer; cdecl;
begin
    TGLPickList(list).Clear;
    result := 1;
end;

function PickListGetCount(list: Pointer): integer; cdecl;
begin
    result := TGLPickList(list).Count;
end;

function PickListGetHit(list: Pointer; index: integer): Pointer; cdecl;
begin
    result := TGLPickList(list).Hit[index];
end;
