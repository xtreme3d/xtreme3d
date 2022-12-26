function BlurCreate(targetObj: real; parent: real): real; cdecl;
var
  b: TGLBlur;
begin
  b:=TGLBlur.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)));
  b.Parent := TGLBaseSceneObject(RealToPtr(targetObj));
  b.RenderWidth := 256;
  b.RenderHeight := 256;
  b.Preset := pGlossy;
  b.Visible := True;
  //b.BlurDeltaTime := 0.01;
  result:=ObjToReal(b);
end;

function BlurSetPreset(blur, p: real): real; cdecl;
var
  b: TGLBlur;
begin
  b := TGLBlur(RealToPtr(blur));
  if p=0 then b.Preset := pNone;
  if p=1 then b.Preset := pGlossy;
  if p=2 then b.Preset := pBeastView;
  if p=3 then b.Preset := pOceanDepth;
  if p=4 then b.Preset := pDream;
  if p=5 then b.Preset := pOverBlur;
  Result := 1;
end;

function BlurSetOptions(blur, delta, left, top, right, bottom: real): real; cdecl;
var
  b: TGLBlur;
begin
  b := TGLBlur(RealToPtr(blur));
  b.BlurDeltaTime := delta;
  b.BlurLeft := left;
  b.BlurTop := top;
  b.BlurRight := right;
  b.BlurBottom := bottom;
  Result := 1;
end;

function BlurSetResolution(blur, res: real): real; cdecl;
var
  b: TGLBlur;
begin
  b := TGLBlur(RealToPtr(blur));
  b.RenderWidth := trunc(res);
  b.RenderHeight := trunc(res);
  Result := 1;
end;

function BlurSetColor(blur, col: real): real; cdecl;
var
  b: TGLBlur;
begin
  b := TGLBlur(RealToPtr(blur));
  b.Material.FrontProperties.Diffuse.AsWinColor:=TColor(trunc(col));
  Result := 1;
end;

function BlurSetBlendingMode(blur, bm: real): real; cdecl;
var
  b: TGLBlur;
begin
  b := TGLBlur(RealToPtr(blur));
  if bm=0 then b.Material.BlendingMode:=bmOpaque;
  if bm=1 then b.Material.BlendingMode:=bmTransparency;
  if bm=2 then b.Material.BlendingMode:=bmAdditive;
  if bm=3 then b.Material.BlendingMode:=bmAlphaTest50;
  if bm=4 then b.Material.BlendingMode:=bmAlphaTest100;
  if bm=5 then b.Material.BlendingMode:=bmModulate;
  Result := 1;
end;
