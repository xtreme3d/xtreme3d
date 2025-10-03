function WindowControlCreate(winHandle, top, left, width, height: real): real; cdecl;
var
    wincontrol: TWinControl;
    windowHandle: HWND;
begin
    windowHandle := HWND(RealToPtr(winHandle));
    if IsWindow(windowHandle) then begin
        wincontrol := TWinControl.CreateParentedControl(windowHandle);
        wincontrol.Left := Trunc(left);
        wincontrol.Top := Trunc(top);
        wincontrol.Width := Trunc(width);
        wincontrol.Height := Trunc(height);
        result := ObjToReal(wincontrol);
    end
    else begin
        ShowMessage('Invalid parent window handle');
        result := 0.0;
    end;
end;

function WindowControlSetBackgroundColor(wincontrol, color: real): real; cdecl;
var
    wcontrol: TWinControl;
begin
    wcontrol := TWinControl(RealToPtr(wincontrol));
    wcontrol.Brush.Color := TColor(Trunc(color));
    result := 1.0;
end;

function WindowControlFree(wincontrol: real): real; cdecl;
var
    wcontrol: TWinControl;
begin
    wcontrol := TWinControl(RealToPtr(wincontrol));
    wcontrol.Free;
    result := 1.0;
end;

