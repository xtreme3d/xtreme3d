function BmpHDSCreate(img: PAnsiChar): real; cdecl;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1 := TGLBitmapHDS.Create(scene);
  GLBitmapHDS1.Picture.Bitmap.LoadFromFile(img);
  GLBitmapHDS1.MaxPoolSize := 8 * 1024 * 1024;
  result := ObjToReal(GLBitmapHDS1);
end;

function BmpHDSSetInfiniteWarp(hds,iwarp: real): real; cdecl;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1 := TGLBitmapHDS(RealToPtr(hds));
  GLBitmapHDS1.InfiniteWrap := boolean(trunc(iwarp));
  result := 1;
end;

function BmpHDSInvert(hds: real): real; cdecl;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1 := TGLBitmapHDS(RealToPtr(hds));
  GLBitmapHDS1.Picture.Bitmap := InvertBitmap(GLBitmapHDS1.Picture.Bitmap);
  result := 1;
end;

function BmpHDSCreateEmpty(w, h, fill: real): real; cdecl;
var
  bhds: TGLBitmapHDS;
  bmp: TBitmap;
  logpal: TMaxLogPalette;
  b: integer;
  hb: Integer; 
begin
  bhds := TGLBitmapHDS.Create(scene);
  bmp := TBitmap.Create;
  bmp.Width := trunc(w);
  bmp.Height := trunc(h);
  bmp.PixelFormat := pf8bit;
  bmp.Transparent := false;
  logpal.palVersion := $0300;
  logpal.palNumEntries := 256;
  for b := 0 to 255 do
    with logpal.palPalEntry [b] do
    begin
      peBlue := b;
      peGreen := b;
      peRed := b;
      peFlags := 0;
    end;
  bmp.Palette := CreatePalette(PLogPalette(@logpal)^);
  hb := trunc(fill * 255.0);
  bmp.Canvas.Brush.Color := RGB(hb, hb, hb);
  bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
  bhds.Picture.Bitmap.Assign(bmp);
  bhds.MaxPoolSize := 8 * 1024 * 1024;
  bhds.MarkDirty;
  bmp.Free;
  result := ObjToReal(bhds);
end;

function BmpHDSSetHeight(hds, x, y, h: real): real; cdecl;
var
  bhds: TGLBitmapHDS;
  hb: Integer;
  color: TColor;
  height: Single;
begin
  bhds := TGLBitmapHDS(RealToPtr(hds));
  height := h;
  if (height < 0) then height := 0;
  if (height > 1) then height := 1;
  hb := trunc(height * 255.0);
  color := RGB(hb, hb, hb);
  bhds.Picture.Bitmap.Canvas.Pixels[trunc(x), trunc(y)] := color;
  bhds.MarkDirty;
  result := 1.0;
end;

function BmpHDSGetHeight(hds, x, y: real): real; cdecl;
var
  bhds: TGLBitmapHDS;
  color: TColor;
begin
  bhds := TGLBitmapHDS(RealToPtr(hds));
  color := bhds.Picture.Bitmap.Canvas.Pixels[trunc(x), trunc(y)];
  result := (color and 255) / 255.0;
end;

function BmpHDSSave(hds: real; filename: PAnsiChar): real; cdecl;
var
  bhds: TGLBitmapHDS;
begin
  bhds := TGLBitmapHDS(RealToPtr(hds));
  bhds.Picture.SaveToFile(filename);
  result := 1.0
end;

function TerrainCreate(parent: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  if not (parent=0) then
    TerrainRenderer1 := TGLTerrainRenderer.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    TerrainRenderer1 := TGLTerrainRenderer.CreateAsChild(scene.Objects);
  result := ObjToReal(TerrainRenderer1);
end;

function TerrainSetHeightData(terrain,hds: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1 := TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.HeightDataSource := TGLHeightDataSource(RealToPtr(hds)); //TGLBitmapHDS(trunc64(hds));
  result := 1;
end;

function TerrainSetTileSize(terrain,tsize: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1 := TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.TileSize := trunc(tsize);
  result := 1;
end;

function TerrainSetTilesPerTexture(terrain,tpt: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.TilesPerTexture:=tpt;
  result:=1;
end;

function TerrainSetQualityDistance(terrain,qd: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.QualityDistance:=qd;
  result:=1;
end;

function TerrainSetQualityStyle(terrain,hrs: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  if hrs=0 then TerrainRenderer1.QualityStyle:=hrsFullGeometry;
  if hrs=1 then TerrainRenderer1.QualityStyle:=hrsTesselated;
  result:=1;
end;

function TerrainSetMaxCLodTriangles(terrain,tri: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.MaxCLODTriangles:=trunc(tri);
  result:=1;
end;

function TerrainSetCLodPrecision(terrain,prec: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.CLODPrecision:=trunc(prec);
  result:=1;
end;

function TerrainSetOcclusionFrameSkip(terrain,ofs: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  TerrainRenderer1.OcclusionFrameSkip:=trunc(ofs);
  result:=1;
end;

function TerrainSetOcclusionTesselate(terrain,tot: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  if tot=0 then TerrainRenderer1.OcclusionTesselate:=totTesselateAlways;
  if tot=1 then TerrainRenderer1.OcclusionTesselate:=totTesselateIfVisible;
  result:=1;
end;

function TerrainGetHeightAtObjectPosition(terrain,obj: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
  Object1: TGLSceneObject;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  Object1:=TGLSceneObject(RealToPtr(obj));
  result:=TerrainRenderer1.InterpolatedHeight(Object1.Position.AsVector);
end;

function TerrainGetLastTriCount(terrain: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(RealToPtr(terrain));
  result:=TerrainRenderer1.LastTriangleCount;
end;

function TerrainGetHDSPosition(terrain, x, y, z, index: real): real; cdecl;
var
  terr: TGLTerrainRenderer;
  bhds: TGLBitmapHDS;
  p: TGLVector;
  cx, cy: Integer;
begin
  terr := TGLTerrainRenderer(RealToPtr(terrain));
  bhds := TGLBitmapHDS(terr.HeightDataSource);
  p := terr.AbsoluteToLocal(VectorMake(x, y, z, 1.0));
  cx := Round(p.v[0]);
  cy := Round(p.v[1]);
  if (cx > bhds.Picture.Width-1) then cx := bhds.Picture.Width-1;
  if (cy > bhds.Picture.Height-1) then cy := bhds.Picture.Height-1;
  if (cx < 0) then cx := 0;
  if (cy < 0) then cy := 0;
  if (index = 0) then Result := cx
  else Result := cy;
end;
