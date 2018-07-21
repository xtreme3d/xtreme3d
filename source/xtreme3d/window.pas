function WindowCreate(x, y, w, h, resizeable: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm.Create(nil);
  frm.Width := trunc64(w);
  frm.Height := trunc64(h);
  frm.Left := trunc64(x);
  frm.Top := trunc64(y);
  if trunc64(resizeable) = 0 then
    frm.BorderStyle := bsSingle;
  frm.Show;
  frm.Enabled := True;
  result := Integer(frm);
end;

function WindowCenter(w: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  frm.Position := poDesktopCenter;
  result := 1.0;
end;

function WindowResize(w, x, y, width, height: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  frm.Left := trunc64(x);
  frm.Top := trunc64(y);
  frm.Width := trunc64(width);
  frm.Height := trunc64(height);
  result := 1.0;
end;

function WindowGetPosition(w, index: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  if trunc64(index) = 0 then
  begin
    result := frm.Left;
    exit;
  end;
  if trunc64(index) = 1 then
  begin
    result := frm.Top;
    exit;
  end;
  result := 0.0;
end;

function WindowGetSize(w, index: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  if trunc64(index) = 0 then
  begin
    result := frm.Width;
    exit;
  end;
  if trunc64(index) = 1 then
  begin
    result := frm.Height;
    exit;
  end;
  result := 0.0;
end;

function WindowGetHandle(w: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  result := Integer(frm.Handle);
end;

function WindowSetTitle(w: real; title: pchar): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  frm.Caption := String(title);
  result := 1;
end;

function WindowDestroy(w: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  frm.Free;
  result := 1;
end;

function WindowIsShowing(w: real): real; stdcall;
var
  frm: TForm;
begin
  frm := TForm(trunc64(w));
  result := Integer(frm.Showing);
end;

function WindowSetIcon(w: real; filename: pchar): real; stdcall;
var
  frm: TForm;
  icon: HIcon;
begin
  frm := TForm(trunc64(w));
  icon := LoadImage(0, filename, IMAGE_ICON,
    GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON),
    LR_LOADFROMFILE);
  SendMessage(frm.Handle, WM_SETICON, 1, icon);
  DestroyIcon(icon);
  result := 1.0;
end;
