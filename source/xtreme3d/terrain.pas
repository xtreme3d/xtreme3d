function BmpHDSCreate(img: pchar): real; cdecl;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1:=TGLBitmapHDS.Create(scene);
  GLBitmapHDS1.Picture.Bitmap.LoadFromFile(img);
  GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
  result:=Integer(GLBitmapHDS1);
end;

function BmpHDSSetInfiniteWarp(hds,iwarp: real): real; cdecl;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1:=TGLBitmapHDS(trunc64(hds));
  GLBitmapHDS1.InfiniteWrap:=boolean(trunc64(iwarp));
  result:=1;
end;

function BmpHDSInvert(hds: real): real; cdecl;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1:=TGLBitmapHDS(trunc64(hds));
  GLBitmapHDS1.Picture.Bitmap:=InvertBitmap(GLBitmapHDS1.Picture.Bitmap);
  result:=1;
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
  bmp.Width := trunc64(w);
  bmp.Height := trunc64(h);
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
  hb := trunc64(fill * 255.0);
  bmp.Canvas.Brush.Color := RGB(hb, hb, hb);
  bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
  bhds.Picture.Bitmap.Assign(bmp);
  bhds.MaxPoolSize := 8 * 1024 * 1024;
  bhds.MarkDirty;
  bmp.Free;
  result := Integer(bhds);
end;

function BmpHDSSetHeight(hds, x, y, h: real): real; cdecl;
var
  bhds: TGLBitmapHDS;
  hb: Integer;
  color: TColor;
  height: Single;
begin
  bhds := TGLBitmapHDS(trunc64(hds));
  height := h;
  if (height < 0) then height := 0;
  if (height > 1) then height := 1;
  hb := trunc64(height * 255.0);
  color := RGB(hb, hb, hb);
  bhds.Picture.Bitmap.Canvas.Pixels[trunc64(x), trunc64(y)] := color;
  bhds.MarkDirty;
  result := 1.0;
end;

function BmpHDSGetHeight(hds, x, y: real): real; cdecl;
var
  bhds: TGLBitmapHDS;
  color: TColor;
begin
  bhds := TGLBitmapHDS(trunc64(hds));
  color := bhds.Picture.Bitmap.Canvas.Pixels[trunc64(x), trunc64(y)];
  result := (color and 255) / 255.0;
end;

function BmpHDSSave(hds: real; filename: pchar): real; cdecl;
var
  bhds: TGLBitmapHDS;
begin
  bhds := TGLBitmapHDS(trunc64(hds));
  bhds.Picture.SaveToFile(filename);
  result := 1.0
end;

function TerrainCreate(parent: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  if not (parent=0) then
    TerrainRenderer1:=TGLTerrainRenderer.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    TerrainRenderer1:=TGLTerrainRenderer.CreateAsChild(scene.Objects);
  result:=Integer(TerrainRenderer1);
end;

function TerrainSetHeightData(terrain,hds: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.HeightDataSource:=THeightDataSource(trunc64(hds)); //TGLBitmapHDS(trunc64(hds));
  result:=1;
end;

function TerrainSetTileSize(terrain,tsize: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.TileSize:=trunc64(tsize);
  result:=1;
end;

function TerrainSetTilesPerTexture(terrain,tpt: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.TilesPerTexture:=tpt;
  result:=1;
end;

function TerrainSetQualityDistance(terrain,qd: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.QualityDistance:=qd;
  result:=1;
end;

function TerrainSetQualityStyle(terrain,hrs: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  if hrs=0 then TerrainRenderer1.QualityStyle:=hrsFullGeometry;
  if hrs=1 then TerrainRenderer1.QualityStyle:=hrsTesselated;
  result:=1;
end;

function TerrainSetMaxCLodTriangles(terrain,tri: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.MaxCLODTriangles:=trunc64(tri);
  result:=1;
end;

function TerrainSetCLodPrecision(terrain,prec: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.CLODPrecision:=trunc64(prec);
  result:=1;
end;

function TerrainSetOcclusionFrameSkip(terrain,ofs: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.OcclusionFrameSkip:=trunc64(ofs);
  result:=1;
end;

function TerrainSetOcclusionTesselate(terrain,tot: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  if tot=0 then TerrainRenderer1.OcclusionTesselate:=totTesselateAlways;
  if tot=1 then TerrainRenderer1.OcclusionTesselate:=totTesselateIfVisible;
  result:=1;
end;

function TerrainGetHeightAtObjectPosition(terrain,obj: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
  Object1: TGLSceneObject;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  Object1:=TGLSceneObject(trunc64(obj));
  result:=TerrainRenderer1.InterpolatedHeight(Object1.Position.AsVector);
end;

function TerrainGetLastTriCount(terrain: real): real; cdecl;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  result:=TerrainRenderer1.LastTriangleCount;
end;

function TerrainGetHDSPosition(terrain, x, y, z, index: real): real; cdecl;
var
  terr: TGLTerrainRenderer;
  bhds: TGLBitmapHDS;
  p: TVector;
  cx, cy: Integer;
begin
  terr := TGLTerrainRenderer(trunc64(terrain));
  bhds := TGLBitmapHDS(terr.HeightDataSource);
  p := terr.AbsoluteToLocal(VectorMake(x, y, z, 1.0));
  cx := Round(p[0]);
  cy := Round(p[1]);
  if (cx > bhds.Picture.Width-1) then cx := bhds.Picture.Width-1;
  if (cy > bhds.Picture.Height-1) then cy := bhds.Picture.Height-1;
  if (cx < 0) then cx := 0;
  if (cy < 0) then cy := 0;
  if (index = 0) then Result := cx
  else Result := cy;
end;
