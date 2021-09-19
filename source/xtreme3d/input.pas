function MouseSetPosition(mx, my: real): real; cdecl;
begin
  SetCursorPos(trunc64(mx), trunc64(my));
  Result := 1;
end;

function MouseGetPositionX: real; cdecl;
var
  mouse : TPoint;
begin
  GetCursorPos(mouse);
  Result := Integer(mouse.X);
end;

function MouseGetPositionY: real; cdecl;
var
  mouse : TPoint;
begin
  GetCursorPos(mouse);
  Result := Integer(mouse.Y);
end;

function MouseShowCursor(mode: real): real; cdecl;
begin
  ShowCursor(LongBool(trunc64(mode)));
  Result := 1;
end;

function KeyIsPressed(key: real): real; cdecl;
begin
  Result := Integer(IsKeyDown(trunc64(key)));
end;
