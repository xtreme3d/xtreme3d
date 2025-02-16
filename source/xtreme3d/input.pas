// Input functions by Rutraple aka Hacker

function MouseGetPositionX: real; cdecl;
var
    mouse: TPoint;
begin
    GetCursorPos(mouse);
    Result := Integer(mouse.X);
end;

function MouseGetPositionY: real; cdecl;
var
    mouse: TPoint;
begin
    GetCursorPos(mouse);
    Result := Integer(mouse.Y);
end;

function MouseSetPosition(mx, my: real): real; cdecl;
begin
    SetCursorPos(Trunc(mx), Trunc(my));
    result := 1;
end;

function MouseShowCursor(mode: real): real; cdecl;
begin
    ShowCursor(LongBool(Trunc(mode)));
    Result := 1;
end;

function KeyIsPressed(key: real): real; cdecl;
begin
    result := integer(IsKeyDown(TVirtualKeyCode(Trunc(key))));
end;

function MouseIsPressed(btn: real): real; cdecl;
begin
    // translate to VK_MBUTTON (4) in case of mb_middle (3)
    if btn = 3 then btn := 4;
    result := integer(IsKeyDown(TVirtualKeyCode(Trunc(btn))));
end;
