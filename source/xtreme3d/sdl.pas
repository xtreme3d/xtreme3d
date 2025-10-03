function SDLLogError(logger: real): real; cdecl;
var
    loggerSession: TLogSession;
    errStr: PAnsiChar;
begin
    loggerSession := TLogSession(RealToPtr(logger));
    errStr := SDL_GetError();
    if not IsEmptyStr(errStr) then
        loggerSession.Log(StrConv(errStr), lkError);
    result := 1.0;
end;
