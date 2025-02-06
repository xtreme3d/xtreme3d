function LoggerCreate(fname: PAnsiChar; loglevel: real): real; cdecl;
var
  loggerSession: TLogSession;
  levelCode: integer;
  loglevels: TLogLevels;
begin
  loglevels := llMax;
  levelCode := Trunc(loglevel);
  if levelCode = 0 then
    loglevels := llMin
  else if levelCode = 1 then
    loglevels := llMedium;
  loggerSession := TLogSession.Init(StrConv(fname), lfDateTime, loglevels);
  loggerSession.Enabled := true;
  result := ObjToReal(loggerSession);
end;

function LoggerEnable(logger, mode: real): real; cdecl;
var
  loggerSession: TLogSession;
begin
  loggerSession := TLogSession(RealToPtr(logger));
  loggerSession.Enabled := Boolean(Trunc(mode));
  result := 1;
end;

function LoggerLog(logger, loglevel: real; msg: PAnsiChar): real; cdecl;
var
  loggerSession: TLogSession;
begin
  loggerSession := TLogSession(RealToPtr(logger));
  loggerSession.Log(StrConv(msg), TLogLevel(Trunc(loglevel)));
  //loggerSession.FlushBuffer;
  result := 1;
end;

