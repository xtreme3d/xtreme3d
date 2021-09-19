function EngineCreate: real; cdecl;
begin
  empty:=TEmpty.Create(nil);
  scene:=TGLScene.Create(nil);
  memviewer:=TGLMemoryViewer.Create(nil);
  cadencer:=TGLCadencer.Create(nil);
  cadencer.Scene:=scene;
  cadencer.Mode:=cmManual;
  showLoadingErrors := True;
  previousTicks := GetTickCount;
  result:=1;
end;

function EngineDestroy: real; cdecl;
begin
  cadencer.Enabled := false;
  cadencer.Scene := nil;
  cadencer.Free;
  scene.Free;
  empty.Free;
  memviewer.Free;
  result:=1;
end;

function EngineSetObjectsSorting(os: real): real; cdecl;
begin
  if os=0 then scene.ObjectsSorting:=osInherited;
  if os=1 then scene.ObjectsSorting:=osNone;
  if os=2 then scene.ObjectsSorting:=osRenderFarthestFirst;
  if os=3 then scene.ObjectsSorting:=osRenderBlendedLast;
  if os=4 then scene.ObjectsSorting:=osRenderNearestFirst;
  result:=1;
end;

// Change: vcNone and vcInherited are now 0 and 1
function EngineSetCulling(vc: real): real; cdecl;
begin
  if vc=0 then scene.VisibilityCulling:=vcNone;
  if vc=1 then scene.VisibilityCulling:=vcInherited;
  if vc=2 then scene.VisibilityCulling:=vcObjectBased;
  if vc=3 then scene.VisibilityCulling:=vcHierarchical;
  result:=1;
end;

function Update(delta: real): real; cdecl;
begin
  cadencer.FixedDeltaTime := delta;
  cadencer.Progress;
  result:=1;
end;

//todo
function TrisRendered: real; cdecl;
begin
  result:=1;
end;

function EngineSaveScene(filename: pchar): real; cdecl;
begin
  scene.SaveToTextFile(String(filename));
  result := 1.0;
end;

function EngineLoadScene(filename: pchar): real; cdecl;
begin
  scene.LoadFromTextFile(String(filename));
  result := 1.0;
end;

function EngineRootObject: real; cdecl;
begin
  result := Integer(scene.Objects);
end;

function EngineShowLoadingErrors(mode: real): real; cdecl;
begin
  showLoadingErrors := Boolean(trunc64(mode));
  result := 1;
end;

function EngineSetMaxLights(lights: real): real; cdecl;
begin
  scene.MaxLights := trunc64(lights);
  result := 1.0;
end;

function EngineGetTimeStep: real; cdecl;
var
  currentTicks: DWORD;
  elapsedMsec: DWORD; 
begin
  currentTicks := GetTickCount;
  elapsedMsec := currentTicks - previousTicks;
  previousTicks := currentTicks;
  result := elapsedMsec * 0.001;
end;

