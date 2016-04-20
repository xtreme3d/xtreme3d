// GLBumpmapHDS
{: Implements and HDS that can automatically generates an elevation bumpmap.<p>

   The object-space elevation bumpmap can be used for dynamic terrain
   lighting.<p>

	<b>History : </b><font size=-1><ul>
      <li>15/04/04 - EG - Fixed hdsNone support (Phil Scadden)
      <li>20/03/04 - EG - Works, reasonnably seamless but still quite inefficient
      <li>20/02/04 - EG - Creation
	</ul></font>
}
unit GLBumpmapHDS;

interface

uses Classes, GLHeightData, GLGraphics, VectorGeometry, GLTexture;

type

   TGLBumpmapHDS = class;

   // TNewTilePreparedEvent
   //
   TNewTilePreparedEvent = procedure (Sender : TGLBumpmapHDS; heightData : THeightData;
                                      normalMapMaterial : TGLLibMaterial) of object;

	// TGLBumpmapHDS
	//
   {: An Height Data Source that generates elevation bumpmaps automatically.<p>
      The HDS must be connected to another HDS, which will provide the elevation
      datat, and to a MaterialLibrary where bumpmaps will be placed. }
	TGLBumpmapHDS = class (THeightDataSource)
	   private
	      { Private Declarations }
         FElevationHDS : THeightDataSource;
         FBumpmapLibrary : TGLMaterialLibrary;
         FOnNewTilePrepared : TNewTilePreparedEvent;
         FBumpScale : Single;
         FSubSampling : Integer;

	   protected
	      { Protected Declarations }
         procedure SetElevationHDS(const val : THeightDataSource);
         procedure SetBumpmapLibrary(const val : TGLMaterialLibrary);
         procedure SetBumpScale(const val : Single);
         function StoreBumpScale : Boolean;
         procedure SetSubSampling(const val : Integer);

         procedure StartPreparingData(heightData : THeightData); override;

         procedure GenerateNormalMap(heightData : THeightData;
                                     normalMap : TGLBitmap32; scale : Single);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure Release(aHeightData : THeightData); override;

	   published
	      { Published Declarations }
         property ElevationHDS : THeightDataSource read FElevationHDS write SetElevationHDS;
         property BumpmapLibrary : TGLMaterialLibrary read FBumpmapLibrary write SetBumpmapLibrary;
         property OnNewTilePrepared : TNewTilePreparedEvent read FOnNewTilePrepared write FOnNewTilePrepared;
         property BumpScale : Single read FBumpScale write SetBumpScale stored StoreBumpScale;
         {: Specifies the amount of subsampling for the bump texture.<p>
            This value must be a power of 2, and is used to divide the height
            tile resolution to determine the bump texture resolution (f.i.
            a tile size of 128 with a subsampling of 4 will result in textures
            of a resolution of 32x32. SubSampling won't allow texture resolution
            to get below 16x16 (minimal bumpmap resolution). }
         property SubSampling : Integer read FSubSampling write SetSubSampling default 1;

         property MaxPoolSize;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLUtils;

const
   cDefaultBumpScale = 0.1;

// ------------------
// ------------------ TGLBumpmapHDS ------------------
// ------------------

// Create
//
constructor TGLBumpmapHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FBumpScale:=cDefaultBumpScale;
   FSubSampling:=1;
end;

// Destroy
//
destructor TGLBumpmapHDS.Destroy;
begin
   ElevationHDS:=nil;
   BumpmapLibrary:=nil;
	inherited Destroy;
end;

// Notification
//
procedure TGLBumpmapHDS.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FElevationHDS then
         ElevationHDS:=nil
      else if AComponent=FBumpmapLibrary then
         BumpmapLibrary:=nil;
   end;
   inherited;
end;

// StartPreparingData
//
procedure TGLBumpmapHDS.StartPreparingData(heightData : THeightData);
var
   i : Integer;
   htfHD : THeightData;
   libMat : TGLLibMaterial;
   bmp32 : TGLBitmap32;
begin
   if not Assigned(FElevationHDS) then Exit;
   with heightData do
      htfHD:=FElevationHDS.GetData(XLeft-1, YTop-1, Size+3, DataType);
   if (htfHD.DataState=hdsNone) then
      heightData.DataState:=hdsNone
   else begin
      heightData.MaterialName:=htfHD.MaterialName;
      heightData.TextureCoordinatesMode:=tcmLocal;
      heightData.TextureCoordinatesOffset:=NullTexPoint;
      heightData.TextureCoordinatesScale:=XYTexPoint;
      heightData.DataType:=hdtSmallInt;
      htfHD.DataType:=hdtSmallInt;
      heightData.Allocate(hdtSmallInt);
      for i:=1 to heightData.Size do
         Move(htfHD.SmallIntRaster[i][1], heightData.SmallIntRaster[i-1][0],
              heightData.Size*2);
      heightData.DataState:=hdsReady;
      heightData.HeightMin:=htfHD.HeightMin;
      heightData.HeightMax:=htfHD.HeightMax;
   end;
   if Assigned(FBumpmapLibrary) and (heightData.DataState<>hdsNone) then begin
      libMat:=FBumpmapLibrary.Materials.Add;
      libMat.Name:='BumpHDS_'+IntToHex(Int64(heightData), 16);
      with libMat.Material.Texture do begin
         ImageClassName:=TGLBlankImage.ClassName;
         Enabled:=True;
         MagFilter:=maLinear;
         MinFilter:=miLinearMipmapNearest;
         TextureMode:=tmReplace;
         TextureWrap:=twNone;
         TextureFormat:=tfRGBA16;
         bmp32:=(Image as TGLBlankImage).GetBitmap32(GL_TEXTURE_2D);
         GenerateNormalMap(htfHD, bmp32, FBumpScale);
      end;
      // scale and translate the bumpmap to reduce seams
      libMat.TextureOffset.SetVector(0.5/bmp32.Width, 0.5/bmp32.Width, 1);
      libMat.TextureScale.SetVector((bmp32.Width-1)/bmp32.Width, (bmp32.Width-1)/bmp32.Width, 1);
   end else libMat:=nil;
   FElevationHDS.Release(htfHD);
   if Assigned(libMat) then begin
      if Assigned(FOnNewTilePrepared) then
         FOnNewTilePrepared(Self, heightData, libMat)
      else heightData.MaterialName:=libMat.Name;
   end;
end;

// Release
//
procedure TGLBumpmapHDS.Release(aHeightData : THeightData);
var
   libMat : TGLLibMaterial;
begin
   if Assigned(FBumpmapLibrary) then begin
      libMat:=FBumpmapLibrary.LibMaterialByName('BumpHDS_'+IntToHex(Int64(aHeightData), 16));
      libMat.Free;      
   end;
   inherited;
end;

// GenerateNormalMap
//
procedure TGLBumpmapHDS.GenerateNormalMap(heightData : THeightData;
                                          normalMap : TGLBitmap32;
                                          scale : Single);
var
   x, y, hdY, hdX, half, mapSize, hdStep : Integer;
   dcx, dcy : Single;
   invLen, scaleDiv255 : Single;
   prevRow, curRow, nextRow : PSmallIntArray;
   nmRow : PGLPixel32Array;
begin
   heightData.DataType:=hdtSmallInt;

   hdStep:=0;
   mapSize:=(heightData.Size-4);
   if (mapSize>8) and (SubSampling>1) then begin
      mapSize:=mapSize div SubSampling;
      if mapSize<8 then mapSize:=8;
      while (mapSize shl hdStep)<(heightData.Size-4) do Inc(hdStep);
   end;
   normalMap.Height:=mapSize;
   normalMap.Width:=mapSize;
   half:=(mapSize shr 1);

   scaleDiv255:=scale*(1/255);
   hdY:=0;
   for y:=0 to normalMap.Height-1 do begin
      prevRow:=heightData.SmallIntRaster[hdY];
      curRow:=heightData.SmallIntRaster[hdY+1];
      nextRow:=heightData.SmallIntRaster[hdY+2];
      nmRow:=normalMap.ScanLine[normalMap.Height-1-y];
      hdX:=0;
      for x:=0 to normalMap.Width-1 do begin
         dcx:=scaleDiv255*(curRow[hdX+2]-curRow[hdX]);
         dcy:=scaleDiv255*(prevRow[hdX+1]-nextRow[hdX+1]);

         invLen:=RSqrt(Sqr(dcx)+Sqr(dcy)+1);

         with nmRow[x] do begin
            r:=Round(128+127*ClampValue(dcx*invLen, -1, 1));
            g:=Round(128+127*ClampValue(dcy*invLen, -1, 1));
            b:=Round(128+127*invLen);
            a:=255;
         end;
         Inc(hdX, (1+Integer(hdX=half)) shl hdStep);
      end;
      Inc(hdY, (1+Integer(hdY=half)) shl hdStep);
   end;
end;

// SetElevationHDS
//
procedure TGLBumpmapHDS.SetElevationHDS(const val : THeightDataSource);
begin
   if val<>FElevationHDS then begin
      if Assigned(FElevationHDS) then
         FElevationHDS.RemoveFreeNotification(Self);
      FElevationHDS:=val;
      if Assigned(FElevationHDS) then
         FElevationHDS.FreeNotification(Self);
      MarkDirty;
   end;
end;

// SetBumpmapLibrary
//
procedure TGLBumpmapHDS.SetBumpmapLibrary(const val : TGLMaterialLibrary);
begin
   if val<>FBumpmapLibrary then begin
      if Assigned(FBumpmapLibrary) then
         FBumpmapLibrary.RemoveFreeNotification(Self);
      FBumpmapLibrary:=val;
      if Assigned(FBumpmapLibrary) then
         FBumpmapLibrary.FreeNotification(Self);
      MarkDirty;
   end;
end;

// SetBumpScale
//
procedure TGLBumpmapHDS.SetBumpScale(const val : Single);
begin
   if FBumpScale<>val then begin
      FBumpScale:=val;
      MarkDirty;
   end;
end;

// StoreBumpScale
//
function TGLBumpmapHDS.StoreBumpScale : Boolean;
begin
   Result:=(FBumpScale<>cDefaultBumpScale);
end;

// SetSubSampling
//
procedure TGLBumpmapHDS.SetSubSampling(const val : Integer);
begin
   if val<>FSubSampling then begin
      FSubSampling:=RoundDownToPowerOf2(val);
      if FSubSampling<1 then
         FSubSampling:=1;
      MarkDirty;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TGLBumpmapHDS);

end.
