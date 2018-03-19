function EngineCreate: real; stdcall;
begin
  empty:=TEmpty.Create(nil);
  scene:=TGLScene.Create(nil);
  memviewer:=TGLMemoryViewer.Create(nil);
  cadencer:=TGLCadencer.Create(nil);
  cadencer.Scene:=scene;
  cadencer.Mode:=cmManual;
  showLoadingErrors := True;
  result:=1;
end;

function EngineDestroy: real; stdcall;
begin
  cadencer.Enabled := false;
  cadencer.Scene := nil;
  cadencer.Free;
  scene.Free;
  empty.Free;
  memviewer.Free;
  result:=1;
end;

function EngineSetObjectsSorting(os: real): real; stdcall;
begin
  if os=0 then scene.ObjectsSorting:=osInherited;
  if os=1 then scene.ObjectsSorting:=osNone;
  if os=2 then scene.ObjectsSorting:=osRenderFarthestFirst;
  if os=3 then scene.ObjectsSorting:=osRenderBlendedLast;
  if os=4 then scene.ObjectsSorting:=osRenderNearestFirst;
  result:=1;
end;

// Change: vcNone and vcInherited are now 0 and 1
function EngineSetCulling(vc: real): real; stdcall;
begin
  if vc=0 then scene.VisibilityCulling:=vcNone;
  if vc=1 then scene.VisibilityCulling:=vcInherited;
  if vc=2 then scene.VisibilityCulling:=vcObjectBased;
  if vc=3 then scene.VisibilityCulling:=vcHierarchical;
  result:=1;
end;

function SetPakArchive(fname: pchar): real; stdcall;
var
  pak: TGLVfsPak;
begin
  pak:=TGLVfsPak.Create(scene);
  pak.LoadFromFile(String(fname), $0002 or $0020);
  result:=1;
end;

function Update(delta: real): real; stdcall;
begin
  cadencer.FixedDeltaTime := delta;
  cadencer.Progress;
  result:=1;
end;

//todo
function TrisRendered: real; stdcall;
begin
  result:=1;
end;

function EngineSaveScene(filename: pchar): real; stdcall;
begin
  scene.SaveToTextFile(String(filename));
  result := 1.0;
end;

function EngineLoadScene(filename: pchar): real; stdcall;
begin
  scene.LoadFromTextFile(String(filename));
  result := 1.0;
end;

function EngineRootObject: real; stdcall;
begin
  result := Integer(scene.Objects);
end;

function EngineShowLoadingErrors(mode: real): real; stdcall;
begin
  showLoadingErrors := Boolean(trunc64(mode));
  result := 1;
end;

function EngineSetMaxLights(lights: real): real; stdcall;
begin
  scene.MaxLights := trunc64(lights);
  result := 1.0;
end;
