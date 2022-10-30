function EngineCreate: integer; cdecl;
begin
    empty := TEmpty.Create(nil);
    scene := TGLScene.Create(nil);
    memviewer := TGLMemoryViewer.Create(nil);
    cadencer := TGLCadencer.Create(nil);
    cadencer.Scene := scene;
    cadencer.Mode := cmManual;
    showLoadingErrors := True;
    previousTicks := GetTickCount;
    result := 1;
end;

function EngineDestroy: integer; cdecl;
begin
    cadencer.Enabled := false;
    cadencer.Scene := nil;
    cadencer.Free;
    scene.Free;
    empty.Free;
    memviewer.Free;
    result := 1;
end;

function EngineUpdate(delta: real): integer; cdecl;
begin
    cadencer.FixedDeltaTime := delta;
    cadencer.Progress;
    result := 1;
end;
