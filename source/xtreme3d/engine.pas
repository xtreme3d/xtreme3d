function EngineCreate: real; cdecl;
begin
    empty := TEmpty.Create(nil);
    scene := TGLScene.Create(nil);
    memviewer := TGLMemoryViewer.Create(nil);
    cadencer := TGLCadencer.Create(nil);
    cadencer.Scene := scene;
    cadencer.Mode := cmManual;
    cadencer.enabled := true;
    showLoadingErrors := True;
    previousTicks := GetTickCount;
    result := 1.0;
end;

function EngineDestroy: real; cdecl;
begin
    cadencer.Enabled := false;
    cadencer.Scene := nil;
    cadencer.Free;
    scene.Free;
    empty.Free;
    memviewer.Free;
    result := 1.0;
end;

function EngineSetObjectsSorting(os: real): real; cdecl;
begin
    if Trunc(os) = 0 then scene.ObjectsSorting := osInherited;
    if Trunc(os) = 1 then scene.ObjectsSorting := osNone;
    if Trunc(os) = 2 then scene.ObjectsSorting := osRenderFarthestFirst;
    if Trunc(os) = 3 then scene.ObjectsSorting := osRenderBlendedLast;
    if Trunc(os) = 4 then scene.ObjectsSorting := osRenderNearestFirst;
    result := 1.0;
end;

// Change: vcNone and vcInherited are now 0 and 1
function EngineSetCulling(vc: real): real; cdecl;
begin
    if Trunc(vc) = 0 then scene.VisibilityCulling := vcNone;
    if Trunc(vc) = 1 then scene.VisibilityCulling := vcInherited;
    if Trunc(vc) = 2 then scene.VisibilityCulling := vcObjectBased;
    if Trunc(vc) = 3 then scene.VisibilityCulling := vcHierarchical;
    result := 1.0;
end;

function EngineUpdate(delta: real): real; cdecl;
begin
    cadencer.FixedDeltaTime := delta;
    cadencer.Progress;
    result := 1.0;
end;

{
// TODO:
function TrisRendered: real; cdecl;
begin
    result := 1.0;
end;
}

function EngineSaveScene(filename: PAnsiChar): real; cdecl;
begin
    scene.SaveToTextFile(StrConv(filename));
    result := 1.0;
end;

function EngineLoadScene(filename: PAnsiChar): real; cdecl;
begin
    scene.LoadFromTextFile(StrConv(filename));
    result := 1.0;
end;

function EngineRootObject: real; cdecl;
begin
    result := ObjToReal(scene.Objects);
end;

function EngineShowLoadingErrors(mode: real): real; cdecl;
begin
    showLoadingErrors := Boolean(Trunc(mode));
    result := 1.0;
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

function PointerToReal(p: pchar): real; cdecl;
begin
    result := real(NativeInt(p));
end;
