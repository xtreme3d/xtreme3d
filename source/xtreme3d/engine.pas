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

function EngineSetObjectsSorting(os: integer): integer; cdecl;
begin
    if os = 0 then scene.ObjectsSorting := osInherited;
    if os = 1 then scene.ObjectsSorting := osNone;
    if os = 2 then scene.ObjectsSorting := osRenderFarthestFirst;
    if os = 3 then scene.ObjectsSorting := osRenderBlendedLast;
    if os = 4 then scene.ObjectsSorting := osRenderNearestFirst;
    result := 1;
end;

// Change: vcNone and vcInherited are now 0 and 1
function EngineSetCulling(vc: integer): integer; cdecl;
begin
    if vc = 0 then scene.VisibilityCulling := vcNone;
    if vc = 1 then scene.VisibilityCulling := vcInherited;
    if vc = 2 then scene.VisibilityCulling := vcObjectBased;
    if vc = 3 then scene.VisibilityCulling := vcHierarchical;
    result := 1;
end;

function EngineUpdate(delta: real): integer; cdecl;
begin
    cadencer.FixedDeltaTime := delta;
    cadencer.Progress;
    result := 1;
end;

{
// TODO:
function TrisRendered: integer; cdecl;
begin
    result := 1;
end;
}

function EngineSaveScene(filename: PAnsiChar): integer; cdecl;
begin
    scene.SaveToTextFile(String(AnsiString(filename)));
    result := 1;
end;

function EngineLoadScene(filename: PAnsiChar): integer; cdecl;
begin
    scene.LoadFromTextFile(String(AnsiString(filename)));
    result := 1;
end;

function EngineRootObject: Pointer; cdecl;
begin
    result := scene.Objects;
end;

function EngineShowLoadingErrors(mode: integer): integer; cdecl;
begin
    showLoadingErrors := Boolean(mode);
    result := 1;
end;

{
function EngineSetMaxLights(lights: integer): integer; cdecl;
begin
    scene.MaxLights := lights;
    result := 1;
end;
}

function EngineGetTimeStep: real; cdecl;
var
    currentTicks: Longint;
    elapsedMsec: Longint;
begin
    currentTicks := GetTickCount;
    elapsedMsec := currentTicks - previousTicks;
    previousTicks := currentTicks;
    result := elapsedMsec * 0.001;
end;

{
function PtrToReal(p: pchar): real; cdecl;
begin
    result := Integer(p);
end;
}
