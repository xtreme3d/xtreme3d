function EngineCreate: real; stdcall;
begin
  empty:=TEmpty.Create(scene);
  scene:=TGLScene.Create(nil);
  memviewer:=TGLMemoryViewer.Create(scene);
  cadencer:=TGLCadencer.Create(nil);
  cadencer.Scene:=scene;
  cadencer.Mode:=cmManual;
  result:=1;
end;

function EngineDestroy: real; stdcall;
begin
  cadencer.Enabled := false;
  scene.Destroy;
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
  //scene.BeginUpdate;
  //scene.EndUpdate;
  //scene.Progress(delta, delta);
  //cadencer.Reset;
  cadencer.Progress;
  result:=1;
end;

//todo
function TrisRendered: real; stdcall;
begin
  result:=1;
end;