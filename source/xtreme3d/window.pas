function WindowCreate(x, y, w, h, resizeable: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm.Create(nil);
  frm.Width := trunc(w);
  frm.Height := trunc(h);
  frm.Left := trunc(x);
  frm.Top := trunc(y);
  if trunc(resizeable) = 0 then
    frm.BorderStyle := bsSingle;
  frm.Show;
  frm.Enabled := True;
  result := ObjToReal(frm);
end;

function WindowCenter(w: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  frm.Position := poDesktopCenter;
  result := 1.0;
end;

function WindowResize(w, x, y, width, height: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  frm.Left := trunc(x);
  frm.Top := trunc(y);
  frm.Width := trunc(width);
  frm.Height := trunc(height);
  result := 1.0;
end;

function WindowGetPosition(w, index: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  if trunc(index) = 0 then
  begin
    result := frm.Left;
    exit;
  end;
  if trunc(index) = 1 then
  begin
    result := frm.Top;
    exit;
  end;
  result := 0.0;
end;

function WindowGetSize(w, index: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  if trunc(index) = 0 then
  begin
    result := frm.Width;
    exit;
  end;
  if trunc(index) = 1 then
  begin
    result := frm.Height;
    exit;
  end;
  result := 0.0;
end;

function WindowGetHandle(w: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  result := Integer(frm.Handle);
end;

function WindowSetTitle(w: real; title: PAnsiChar): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  frm.Caption := String(AnsiString(title));
  result := 1;
end;

function WindowDestroy(w: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  frm.Free;
  result := 1;
end;

function WindowIsShowing(w: real): real; cdecl;
var
  frm: TForm;
begin
  frm := TForm(RealToPtr(w));
  result := Integer(frm.Showing);
end;

function WindowSetIcon(w: real; filename: PAnsiChar): real; cdecl;
var
  frm: TForm;
  icon: HIcon;
begin
  frm := TForm(RealToPtr(w));
  icon := LoadImage(0, PWideChar(String(AnsiString(filename))), IMAGE_ICON,
    GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON),
    LR_LOADFROMFILE);
  SendMessage(frm.Handle, WM_SETICON, 1, icon);
  DestroyIcon(icon);
  result := 1.0;
end;

function WindowDispatch: real; cdecl;
var
  Msg: TMsg;
  lResult: Boolean;
begin
  lResult := true;
  while lResult do
  begin
    lResult := False;
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      lResult := True;
      if Msg.Message <> WM_QUIT then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  end;
  result := 1.0;
end;

