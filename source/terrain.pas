function InvertBitmap(Bitmap:TBitmap): TBitmap;
begin
  Bitmap.Canvas.CopyMode := cmDstInvert;
  Bitmap.Canvas.CopyRect(Bitmap.Canvas.ClipRect, Bitmap.Canvas, Bitmap.Canvas.ClipRect);
  Bitmap.Canvas.CopyMode := cmSrcCopy;
  Result := Bitmap;
end;

function BmpHDSCreate(img: pchar): real; stdcall;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1:=TGLBitmapHDS.Create(scene);
  GLBitmapHDS1.Picture.Bitmap.LoadFromFile(img);
  GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
  result:=Integer(GLBitmapHDS1);
end;

function BmpHDSSetInfiniteWarp(hds,iwarp: real): real; stdcall;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1:=TGLBitmapHDS(trunc64(hds));
  GLBitmapHDS1.InfiniteWrap:=boolean(trunc64(iwarp));
  result:=1;
end;

function BmpHDSInvert(hds: real): real; stdcall;
var
  GLBitmapHDS1: TGLBitmapHDS;
begin
  GLBitmapHDS1:=TGLBitmapHDS(trunc64(hds));
  GLBitmapHDS1.Picture.Bitmap:=InvertBitmap(GLBitmapHDS1.Picture.Bitmap);
  result:=1;
end;

function TerrainCreate(parent: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  if not (parent=0) then
    TerrainRenderer1:=TGLTerrainRenderer.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    TerrainRenderer1:=TGLTerrainRenderer.CreateAsChild(scene.Objects);
  result:=Integer(TerrainRenderer1);
end;

function TerrainSetHeightData(terrain,hds: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.HeightDataSource:=TGLBitmapHDS(trunc64(hds));
  result:=1;
end;

function TerrainSetTileSize(terrain,tsize: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.TileSize:=trunc64(tsize);
  result:=1;
end;

function TerrainSetTilesPerTexture(terrain,tpt: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.TilesPerTexture:=tpt;
  result:=1;
end;

function TerrainSetQualityDistance(terrain,qd: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.QualityDistance:=qd;
  result:=1;
end;

function TerrainSetQualityStyle(terrain,hrs: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  if hrs=0 then TerrainRenderer1.QualityStyle:=hrsFullGeometry;
  if hrs=1 then TerrainRenderer1.QualityStyle:=hrsTesselated;
  result:=1;
end;

function TerrainSetMaxCLodTriangles(terrain,tri: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.MaxCLODTriangles:=trunc64(tri);
  result:=1;
end;

function TerrainSetCLodPrecision(terrain,prec: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.CLODPrecision:=trunc64(prec);
  result:=1;
end;

function TerrainSetOcclusionFrameSkip(terrain,ofs: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  TerrainRenderer1.OcclusionFrameSkip:=trunc64(ofs);
  result:=1;
end;

function TerrainSetOcclusionTesselate(terrain,tot: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  if tot=0 then TerrainRenderer1.OcclusionTesselate:=totTesselateAlways;
  if tot=1 then TerrainRenderer1.OcclusionTesselate:=totTesselateIfVisible;
  result:=1;
end;

function TerrainGetHeightAtObjectPosition(terrain,obj: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
  Object1: TGLSceneObject;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  Object1:=TGLSceneObject(trunc64(obj));
  result:=TerrainRenderer1.InterpolatedHeight(Object1.Position.AsVector);
end;

function TerrainGetLastTriCount(terrain: real): real; stdcall;
var
  TerrainRenderer1: TGLTerrainRenderer;
begin
  TerrainRenderer1:=TGLTerrainRenderer(trunc64(terrain));
  result:=TerrainRenderer1.LastTriangleCount;
end;
