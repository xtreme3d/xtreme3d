function VideoCreate(wincontrol: real): real; cdecl;
var
    wcontrol: TWinControl;
    mplayer: TMediaPlayer;
begin
    wcontrol := TWinControl(RealToPtr(wincontrol));
    mplayer := TMediaPlayer.Create(nil);
    mplayer.ParentWindow := wcontrol.Handle;
    mplayer.Display := wcontrol;
    mplayer.Hide;
    result := ObjToReal(mplayer);
end;

function VideoIsPlaying(player: real): real; cdecl;
var
    mplayer: TMediaPlayer;
begin
    mplayer := TMediaPlayer(RealToPtr(player));
    result := Integer(mplayer.Position < mplayer.EndPos);
end;

function VideoPlay(player: real; filename: PAnsiChar): real; cdecl;
var
    mplayer: TMediaPlayer;
begin
    mplayer := TMediaPlayer(RealToPtr(player));
    mplayer.FileName := StrConv(filename);
    mplayer.Open;
    mplayer.EndPos := mplayer.Length;
    mplayer.DisplayRect := Rect(
        mplayer.Display.Left,
        mplayer.Display.Top,
        mplayer.Display.Width,
        mplayer.Display.Height);
    mplayer.Play;
    result := 1.0;
end;

function VideoClose(player: real): real; cdecl;
var
    mplayer: TMediaPlayer;
begin
    mplayer := TMediaPlayer(RealToPtr(player));
    mplayer.Stop;
    mplayer.Close;
    mplayer.Display := nil;
    result := 1.0;
end;

