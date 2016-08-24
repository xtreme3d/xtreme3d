{: GLGraphics<p>

	Utility class and functions to manipulate a bitmap in OpenGL's default
   byte order (GL_RGBA vs TBitmap's GL_BGRA)<p>

   Nota: TGLBitmap32 has support for Alex Denissov's Graphics32 library
   (http://www.g32.org), just make sure the GLS_Graphics32_SUPPORT conditionnal
   is active in GLScene.inc and recompile.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>06/10/04 - NC - Now uses GL_TEXTURE_RECTANGLE_NV for all float texture types 
      <li>04/10/04 - NC - Added support for float texture
      <li>05/09/03 - EG - Added TGLBitmap32.DownSampleByFactor2
      <li>04/07/03 - EG - Added RGBA brightness/gamma correction support
      <li>13/05/03 - EG - Added GrayScaleToNormalMap
      <li>26/08/02 - EG - Fixed loading of 1D horiz textures from 24 bits bitmaps
      <li>16/06/02 - EG - AssignFrom32BitsBitmap fix for single-line bitmaps
      <li>29/01/02 - EG - Fixed ScanLine Index bug with empty bitmaps
      <li>20/01/02 - EG - Fixed BGR24/RGB24 last pixel transfer
      <li>17/01/02 - EG - Faster assignments from bitmaps (approx. x2),
                          Added AssignFromBitmap24WithoutRGBSwap
      <li>28/12/01 - EG - Graphics32 support added
      <li>15/12/01 - EG - Texture target support
      <li>14/09/01 - EG - Use of vFileStreamClass
      <li>31/08/01 - EG - 24bits Bitmaps are now made opaque by default
      <li>12/08/01 - EG - Now detects and uses GL_SGIS_generate_mipmap
      <li>20/02/01 - EG - Fixed SetHeight & SetWidth (thx Nelson Chu)
      <li>14/02/01 - EG - Simplified RegisterAsOpenGLTexture
      <li>15/01/01 - EG - Fixed RegisterAsOpenGLTexture (clamping)
      <li>14/01/01 - EG - Fixed isEmpty (was invalid for rectangles)
      <li>08/10/00 - EG - Fixed RegisterAsOpenGLTexture and Assign(nil)
      <li>25/09/00 - EG - First operational code
	   <li>19/08/00 - EG - Creation
	</ul></font>
}
unit GLGraphics;

interface

{$i GLScene.inc}

uses Classes,
{$ifdef GLS_Graphics32_SUPPORT}
   GR32,
{$endif}
   OpenGL1x, GLUtils, GLCrossPlatform, GLContext;

type

   TColor = TDelphiColor;

   // TGLPixel24
   //
   TGLPixel24 = packed record
      r, g, b : Byte;
   end;
   PGLPixel24 = ^TGLPixel24;

   // TGLPixel32
   //
   TGLPixel32 = packed record
      r, g, b, a : Byte;
   end;
   PGLPixel32 = ^TGLPixel32;

   TGLPixel32Array = array [0..MaxInt shr 3] of TGLPixel32;
   PGLPixel32Array = ^TGLPixel32Array;

	// TGLBitmap32
	//
   {: Contains and manipulates a 32 bits (24+8) bitmap.<p>
      This is the base class for preparing and manipulating textures in GLScene,
      this function does not rely on a windows handle and should be used for
      in-memory manipulations only.<br>
      16 bits textures are automatically converted to 24 bits and an opaque (255)
      alpha channel is assumed for all planes, the byte order is as specified
      in GL_RGBA. If 32 bits is used in this class, it can however output 16 bits texture
      data for use in OpenGL.<p>
      The class has support for registering its content as a texture, as well
      as for directly drawing/reading from the current OpenGL buffer. }
	TGLBitmap32 = class (TPersistent)
	   private
	      { Private Declarations }
         FData : PGLPixel32Array;
         FWidth, FHeight : Integer;
         FDataSize : Integer;
         FVerticalReverseOnAssignFromBitmap : Boolean;

	   protected
	      { Protected Declarations }
         procedure SetWidth(val : Integer);
         procedure SetHeight(const val : Integer);
         function GetScanLine(index : Integer) : PGLPixel32Array;
         procedure AssignFrom24BitsBitmap(aBitmap : TGLBitmap);
         procedure AssignFrom32BitsBitmap(aBitmap : TGLBitmap);
{$ifdef GLS_Graphics32_SUPPORT}
         procedure AssignFromBitmap32(aBitmap32 : TBitmap32);
{$endif}

	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         {: Accepts TGLBitmap32 and TGraphic subclasses. }
         procedure Assign(Source: TPersistent); override;
         {: Assigns from a 24 bits bitmap without swapping RGB.<p>
            This is faster than a regular assignment, but R and B channels
            will be reversed (from what you would view in a TImage). Suitable
            if you do your own drawing and reverse RGB on the drawing side.<br>
            If you're after speed, don't forget to set the bitmap's dimensions
            to a power of two! }
         procedure AssignFromBitmap24WithoutRGBSwap(aBitmap : TGLBitmap);
         {: Assigns from a 2D Texture.<p>
            The context which holds the texture must be active and the texture
            handle valid. }
         procedure AssignFromTexture2D(textureHandle : Cardinal); overload;
         {: Assigns from a Texture handle.<p>
            If the handle is invalid, the bitmap32 will be empty. }
         procedure AssignFromTexture2D(textureHandle : TGLTextureHandle); overload;

         {: Create a 32 bits TBitmap from self content. }
         function Create32BitsBitmap : TGLBitmap;

         {: True if the bitmap is empty (ie. width or height is zero). }
	      function IsEmpty : Boolean;

         {: Width of the bitmap.<p>
            Will be forced to the nearest superior multiple of 4, f.i. writing
            Width:=6 is equivalent to writing Width:=8. }
         property Width : Integer read FWidth write SetWidth;
         {: Height of the bitmap. }
         property Height : Integer read FHeight write SetHeight;
         {: Size of the bitmap data in bytes. }
         property DataSize : Integer read FDataSize;

         {: Access to a specific Bitmap ScanLine.<p>
            index should be in the [0; Height[ range.<p>
            Warning : this function is NOT protected against invalid indexes,
            and invoking it is invalid if the bitmap is Empty. }
         property ScanLine[index : Integer] : PGLPixel32Array read GetScanLine;

         property VerticalReverseOnAssignFromBitmap : Boolean read FVerticalReverseOnAssignFromBitmap write FVerticalReverseOnAssignFromBitmap;

         {: Grants direct access to the bitmap's data.<p>
            This property is equivalent to ScanLine[0], and may be nil if the
            bitmap is empty. }
         property Data : PGLPixel32Array read FData;

         {: Set Alpha channel values to the pixel intensity.<p>
            The intensity is calculated as the mean of RGB components. }
         procedure SetAlphaFromIntensity;
         {: Set Alpha channel to 0 for pixels of given color, 255 for others).<p>
            This makes pixels of given color totally transparent while the others
            are completely opaque. }
         procedure SetAlphaTransparentForColor(const aColor : TColor); overload;
         procedure SetAlphaTransparentForColor(const aColor : TGLPixel32); overload;
         procedure SetAlphaTransparentForColor(const aColor : TGLPixel24); overload;
         {: Set Alpha channel values to given byte value. }
         procedure SetAlphaToValue(const aValue : Byte);
         {: Set Alpha channel values to given float [0..1] value. }
         procedure SetAlphaToFloatValue(const aValue : Single);
         {: Inverts the AlphaChannel component.<p>
            What was transparent becomes opaque and vice-versa. }
         procedure InvertAlpha;
         {: AlphaChannel components are replaced by their sqrt.<p> }
         procedure SqrtAlpha;

         {: Apply a brightness (scaled saturating) correction to the RGB components. }
         procedure BrightnessCorrection(const factor : Single);
         {: Apply a gamma correction to the RGB components. }
         procedure GammaCorrection(const gamma : Single);

         {: Downsample the bitmap by a factor of 2 in both dimensions.<p>
            If one of the dimensions is 1 or less, does nothing. }
         procedure DownSampleByFactor2;

         {: Registers the bitmap's content as an OpenGL texture map.<p>
            Legal values for bytesPerPixel are :<ul>
            <li>4 : RGB+A (32 bits)
            <li>3 : RGB (24 bits)
            <li>1 : Alpha channel only (8 bits)
            </ul>The texWidth and texHeight parameters are used to return
            the actual width and height of the texture (that can be different
            from the size of the bitmap32). }
         procedure RegisterAsOpenGLTexture(target : TGLUInt;
                                           minFilter : TGLMinFilter;
                                           texFormat : Integer;
                                           var texWidth, texHeight : Integer); overload;
         {: Helper version of RegisterAsOpenGLTexture. }
         procedure RegisterAsOpenGLTexture(target : TGLUInt;
                                           minFilter : TGLMinFilter;
                                           texFormat : Integer); overload;

         {: Reads the given area from the current active OpenGL rendering context.<p>
            The best spot for reading pixels is within a SceneViewer's PostRender
            event : the scene has been fully rendered and the OpenGL context
            is still active. }
         procedure ReadPixels(const area : TGLRect);
         {: Draws the whole bitmap at given position in the current OpenGL context.<p>
            This function must be called with a rendering context active.<p>
            Blending and Alpha channel functions are not altered by this function
            and must be adjusted separately. }
         procedure DrawPixels(const x, y : Single);

         {: Converts a grayscale 'elevation' bitmap to normal map.<p>
            Actually, only the Green component in the original bitmap is used. }
         procedure GrayScaleToNormalMap(const scale : Single;
                                        wrapX : Boolean = True; wrapY : Boolean = True);
         {: Assumes the bitmap content is a normal map and normalizes all pixels.<p> }
         procedure NormalizeNormalMap;
	end;

procedure BGR24ToRGB24(src, dest : Pointer; pixelCount : Integer);
procedure BGR24ToRGBA32(src, dest : Pointer; pixelCount : Integer);
procedure RGB24ToRGBA32(src, dest : Pointer; pixelCount : Integer);
procedure BGRA32ToRGBA32(src, dest : Pointer; pixelCount : Integer);

procedure GammaCorrectRGBArray(base : Pointer; pixelCount : Integer;
                               gamma : Single);
procedure BrightenRGBArray(base : Pointer; pixelCount : Integer;
                           factor : Single);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, VectorGeometry;

// GammaCorrectRGBArray
//
procedure GammaCorrectRGBArray(base : Pointer; pixelCount : Integer;
                               gamma : Single);
type
   PByte = ^Byte;
var
   vGammaLUT : array [0..255] of Byte;
   invGamma : Single;
   i : Integer;
begin
   if pixelCount<1 then Exit;
   Assert(gamma>0);
   // build LUT
   if gamma<0.1 then
      invGamma:=10
   else invGamma:=1/gamma;
   for i:=0 to 255 do
      vGammaLUT[i]:=Round(255*Power(i*(1/255), InvGamma));
   // perform correction
   for i:=Integer(base) to Integer(base)+pixelCount*3-1 do
      PByte(i)^:=vGammaLUT[PByte(i)^];
end;

// GammaCorrectRGBAArray
//
procedure GammaCorrectRGBAArray(base : Pointer; pixelCount : Integer;
                               gamma : Single);
type
   PByte = ^Byte;
var
   vGammaLUT : array [0..255] of Byte;
   pLUT : PByteArray;
   invGamma : Single;
   i, n : Integer;
begin
   if pixelCount<1 then Exit;
   Assert(gamma>0);
   // build LUT
   if gamma<0.1 then
      invGamma:=10
   else invGamma:=1/gamma;
   for i:=0 to 255 do
      vGammaLUT[i]:=Round(255*Power(i*(1/255), InvGamma));
   // perform correction
   n:=Integer(base)+pixelCount*4;
   i:=Integer(base);
   pLUT:=@vGammaLUT[0];
   while i<n do begin
      PByte(i)^:=pLUT[PByte(i)^]; Inc(i);
      PByte(i)^:=pLUT[PByte(i)^]; Inc(i);
      PByte(i)^:=pLUT[PByte(i)^]; Inc(i, 2);
   end;
end;

// BrightenRGBArray
//
procedure BrightenRGBArray(base : Pointer; pixelCount : Integer;
                           factor : Single);
type
   PByte = ^Byte;
var
   vBrightnessLUT : array [0..255] of Byte;
   i, k : Integer;
begin
   if pixelCount<1 then Exit;
   Assert(factor>=0);
   // build LUT
   for i:=0 to 255 do begin
      k:=Round(factor*i);
      if k>255 then k:=255;
      vBrightnessLUT[i]:=k;
   end;
   // perform correction
   for i:=Integer(base) to Integer(base)+pixelCount*3-1 do
      PByte(i)^:=vBrightnessLUT[PByte(i)^];
end;

// BrightenRGBAArray
//
procedure BrightenRGBAArray(base : Pointer; pixelCount : Integer;
                           factor : Single);
type
   PByte = ^Byte;
var
   vBrightnessLUT : array [0..255] of Byte;
   pLUT : PByteArray;
   i, n, k : Integer;
begin
   if pixelCount<1 then Exit;
   Assert(factor>=0);
   // build LUT
   for i:=0 to 255 do begin
      k:=Round(factor*i);
      if k>255 then k:=255;
      vBrightnessLUT[i]:=k;
   end;
   // perform correction
   n:=Integer(base)+pixelCount*4;
   i:=Integer(base);
   pLUT:=@vBrightnessLUT[0];
   while i<n do begin
      PByte(i)^:=pLUT[PByte(i)^]; Inc(i);
      PByte(i)^:=pLUT[PByte(i)^]; Inc(i);
      PByte(i)^:=pLUT[PByte(i)^]; Inc(i, 2);
   end;
end;

// BGR24ToRGB24
//
procedure BGR24ToRGB24(src, dest : Pointer; pixelCount : Integer); register;
begin
   while pixelCount>0 do begin
      PChar(dest)[0]:=PChar(src)[2];
      PChar(dest)[1]:=PChar(src)[1];
      PChar(dest)[2]:=PChar(src)[0];
      dest:=Pointer(Integer(dest)+3);
      src:=Pointer(Integer(src)+3);
      Dec(pixelCount);
   end;
end;

// BGR24ToRGBA32
//
procedure BGR24ToRGBA32(src, dest : Pointer; pixelCount : Integer); register;
{begin
   while pixelCount>0 do begin
      PChar(dest)[0]:=PChar(src)[2];
      PChar(dest)[1]:=PChar(src)[1];
      PChar(dest)[2]:=PChar(src)[0];
      PChar(dest)[3]:=#255;
      dest:=Pointer(Integer(dest)+4);
      src:=Pointer(Integer(src)+3);
      Dec(pixelCount);
   end; }
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
         dec   ecx
         jz    @@Last
@@Loop:
         mov   eax, [edi]
         shl   eax, 8
         or    eax, $FF
         bswap eax
         mov   [edx], eax
         add   edi, 3
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Last:
         mov   cx, [edi+1]
         shl   ecx, 16
         mov   ah, [edi]
         mov   al, $FF
         and   eax, $FFFF
         or    eax, ecx
         bswap eax
         mov   [edx], eax
@@Done:
         pop   edi
end;

// RGB24ToRGBA32
//
procedure RGB24ToRGBA32(src, dest : Pointer; pixelCount : Integer); register;
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
         dec   ecx
         jz    @@Last
@@Loop:
         mov   eax, [edi]
         or    eax, $FF000000
         mov   [edx], eax
         add   edi, 3
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Last:
         mov   ax, [edi+1]
         shl   eax, 8
         mov   al, [edi];
         or    eax, $FF000000
         mov   [edx], eax
@@Done:
         pop   edi
end;

// BGRA32ToRGBA32
//
procedure BGRA32ToRGBA32(src, dest : Pointer; pixelCount : Integer); register;
{begin
   while pixelCount>0 do begin
      PChar(dest)[0]:=PChar(src)[2];
      PChar(dest)[1]:=PChar(src)[1];
      PChar(dest)[2]:=PChar(src)[0];
      PChar(dest)[3]:=PChar(src)[3];
      dest:=Pointer(Integer(dest)+4);
      src:=Pointer(Integer(src)+4);
      Dec(pixelCount);
   end; }
// EAX stores src
// EDX stores dest
// ECX stores pixelCount
asm
         push  edi
         cmp   ecx, 0
         jle   @@Done
         mov   edi, eax
@@Loop:
         mov   eax, [edi]
         shl   eax, 8
         mov   al, [edi+3]
         bswap eax
         mov   [edx], eax
         add   edi, 4
         add   edx, 4
         dec   ecx
         jnz   @@Loop
@@Done:
         pop   edi 
end;

// ------------------
// ------------------ TGLBitmap32 ------------------
// ------------------

// Create
//
constructor TGLBitmap32.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TGLBitmap32.Destroy;
begin
   FreeMem(FData);
	inherited Destroy;
end;

// Assign
//
procedure TGLBitmap32.Assign(Source: TPersistent);
var
   bmp : TGLBitmap;
   graphic : TGLGraphic;
begin
   if Source=nil then begin
      FDataSize:=0;
      FWidth:=0;
      FHeight:=0;
      FreeMem(FData);
   end else if Source is TGLBitmap32 then begin
      // duplicate the data
      FDataSize:=TGLBitmap32(Source).DataSize;
      FWidth:=TGLBitmap32(Source).Width;
      FHeight:=TGLBitmap32(Source).Height;
      ReallocMem(FData, FDataSize);
      Move(TGLBitmap32(Source).Data^, Data^, DataSize);
   end else if Source is TGLGraphic then begin
      if (Source is TGLBitmap) and (TGLBitmap(Source).PixelFormat in [glpf24bit, glpf32bit])
            and ((TGLBitmap(Source).Width and 3)=0) then begin
         if TGLBitmap(Source).PixelFormat=glpf24bit then
            AssignFrom24BitsBitmap(TGLBitmap(Source))
         else AssignFrom32BitsBitmap(TGLBitmap(Source))
      end else begin
         graphic:=TGLGraphic(Source);
         bmp:=TGLBitmap.Create;
         try
            bmp.PixelFormat:=glpf24bit;
            bmp.Height:=graphic.Height;
            if (graphic.Width and 3)=0 then begin
               bmp.Width:=graphic.Width;
               bmp.Canvas.Draw(0, 0, graphic);
            end else begin
               bmp.Width:=(graphic.Width and $FFFC)+4;
               bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), graphic);
            end;
            AssignFrom24BitsBitmap(bmp);
         finally
            bmp.Free;
         end;
      end;
{$ifdef GLS_Graphics32_SUPPORT}
   end else if Source is TBitmap32 then begin
      AssignFromBitmap32(TBitmap32(Source));
{$endif}
   end else inherited;
end;

// AssignFrom24BitsBitmap
//
procedure TGLBitmap32.AssignFrom24BitsBitmap(aBitmap : TGLBitmap);
var
   y, rowOffset : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=glpf24bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   if Height>0 then begin
      pDest:=@PChar(FData)[Width*4*(Height-1)];
      if Height=1 then begin
         BGR24ToRGBA32(BitmapScanLine(aBitmap, 0), pDest, Width);
      end else begin
         if VerticalReverseOnAssignFromBitmap then begin
            pSrc:=BitmapScanLine(aBitmap, Height-1);
            rowOffset:=Integer(BitmapScanLine(aBitmap, Height-2))-Integer(pSrc);
         end else begin
            pSrc:=BitmapScanLine(aBitmap, 0);
            rowOffset:=Integer(BitmapScanLine(aBitmap, 1))-Integer(pSrc);
         end;
         for y:=0 to Height-1 do begin
            BGR24ToRGBA32(pSrc, pDest, Width);
            Dec(pDest, Width*4);
            Inc(pSrc, rowOffset);
         end;
      end;
   end;
end;

// AssignFromBitmap24WithoutRGBSwap
//
procedure TGLBitmap32.AssignFromBitmap24WithoutRGBSwap(aBitmap : TGLBitmap);
var
   y, rowOffset : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=glpf24bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   if Height>0 then begin
      pDest:=@PChar(FData)[Width*4*(Height-1)];
      if Height=1 then begin
         RGB24ToRGBA32(BitmapScanLine(aBitmap, 0), pDest, Width);
      end else begin
         if VerticalReverseOnAssignFromBitmap then begin
            pSrc:=BitmapScanLine(aBitmap, Height-1);
            rowOffset:=Integer(BitmapScanLine(aBitmap, Height-2))-Integer(pSrc);
         end else begin
            pSrc:=BitmapScanLine(aBitmap, 0);
            rowOffset:=Integer(BitmapScanLine(aBitmap, 1))-Integer(pSrc);
         end;
         for y:=0 to Height-1 do begin
            RGB24ToRGBA32(pSrc, pDest, Width);
            Dec(pDest, Width*4);
            Inc(pSrc, rowOffset);
         end;
      end;
   end;
end;

// AssignFrom32BitsBitmap
//
procedure TGLBitmap32.AssignFrom32BitsBitmap(aBitmap : TGLBitmap);
var
   y, rowOffset : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=glpf32bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   if Height>0 then begin
      pDest:=@PChar(FData)[Width*4*(Height-1)];
      if VerticalReverseOnAssignFromBitmap then begin
         pSrc:=BitmapScanLine(aBitmap, Height-1);
         if Height>1 then
            rowOffset:=Integer(BitmapScanLine(aBitmap, Height-2))-Integer(pSrc)
         else rowOffset:=0;
      end else begin
         pSrc:=BitmapScanLine(aBitmap, 0);
         if Height>1 then
            rowOffset:=Integer(BitmapScanLine(aBitmap, 1))-Integer(pSrc)
         else rowOffset:=0;
      end;
      for y:=0 to Height-1 do begin
         BGRA32ToRGBA32(pSrc, pDest, Width);
         Dec(pDest, Width*4);
         Inc(pSrc, rowOffset);
      end;
   end;
end;

{$ifdef GLS_Graphics32_SUPPORT}
// AssignFromBitmap32
//
procedure TGLBitmap32.AssignFromBitmap32(aBitmap32 : TBitmap32);
var
   y : Integer;
   pSrc, pDest : PChar;
begin
   Assert((aBitmap32.Width and 3)=0);
   FWidth:=aBitmap32.Width;
   FHeight:=aBitmap32.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   if Height>0 then begin
      pDest:=@PChar(FData)[Width*4*(Height-1)];
      for y:=0 to Height-1 do begin
         if VerticalReverseOnAssignFromBitmap then
            pSrc:=PChar(aBitmap32.ScanLine[Height-1-y])
         else pSrc:=PChar(aBitmap32.ScanLine[y]);
         BGRA32ToRGBA32(pSrc, pDest, Width);
         Dec(pDest, Width*4);
      end;
   end;
end;
{$endif}

// AssignFromTexture2D
//
procedure TGLBitmap32.AssignFromTexture2D(textureHandle : Cardinal);
var
   oldTex : Cardinal;
   texWidth, texHeight : Integer;
begin
   glGetIntegerv(GL_TEXTURE_2D_BINDING_EXT, @oldTex);
   glBindTexture(GL_TEXTURE_2D, textureHandle);
   glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @texWidth);
   glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @texHeight);
   Width:=texWidth;
   Height:=texHeight;
   glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, FData);
   glBindTexture(GL_TEXTURE_2D, oldTex);
end;

// AssignFromTexture2D
//
procedure TGLBitmap32.AssignFromTexture2D(textureHandle : TGLTextureHandle);
var
   oldContext : TGLContext;
   contextActivate : Boolean;
begin
   if Assigned(textureHandle) and (textureHandle.Handle<>0) then begin
      oldContext:=CurrentGLContext;
      contextActivate:=(oldContext<>textureHandle.RenderingContext);
      if contextActivate then begin
         if Assigned(oldContext) then
            oldContext.Deactivate;
         textureHandle.RenderingContext.Activate;
      end;
      try
         AssignFromTexture2D(textureHandle.Handle);
      finally
         if contextActivate then begin
            textureHandle.RenderingContext.Deactivate;
            if Assigned(oldContext) then
               oldContext.Activate;
         end;
      end;
   end else begin
      Width:=0;
      Height:=0;
   end;
end;

// Create32BitsBitmap
//
function TGLBitmap32.Create32BitsBitmap : TGLBitmap;
var
   y, x, x4 : Integer;
   pSrc, pDest : PChar;
begin
   Result:=TGLBitmap.Create;
   Result.PixelFormat:=glpf32bit;
   Result.Width:=Width;
   Result.Height:=Height;
   if Height>0 then begin
      pSrc:=@PChar(FData)[Width*4*(Height-1)];
      for y:=0 to Height-1 do begin
         pDest:=BitmapScanLine(Result, y);
         for x:=0 to Width-1 do begin
            x4:=x*4;
            pDest[x4+0]:=pSrc[x4+2];
            pDest[x4+1]:=pSrc[x4+1];
            pDest[x4+2]:=pSrc[x4+0];
            pDest[x4+3]:=pSrc[x4+3];
         end;
         Dec(pSrc, Width*4);
      end;
   end;
end;

// IsEmpty
//
function TGLBitmap32.IsEmpty : Boolean;
begin
	Result:=(Width=0) or (Height=0);
end;

// SetWidth
//
procedure TGLBitmap32.SetWidth(val : Integer);
begin
   if (val and 3)>0 then
      val:=(val and $FFFC)+4;
   if val<>FWidth then begin
      Assert(val>=0);
      FWidth:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

// SetHeight
//
procedure TGLBitmap32.SetHeight(const val : Integer);
begin
   if val<>FHeight then begin
      Assert(val>=0);
      FHeight:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

// GetScanLine
//
function TGLBitmap32.GetScanLine(index : Integer) : PGLPixel32Array;
begin
   Result:=PGLPixel32Array(@FData[index*Width]);
end;

// SetAlphaFromIntensity
//
procedure TGLBitmap32.SetAlphaFromIntensity;
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do with FData[i] do
      a:=(Integer(r)+Integer(g)+Integer(b)) div 3;
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TColor);
var
   color : TGLPixel24;
begin
   color.r:=GetRValue(aColor);
   color.g:=GetGValue(aColor);
   color.b:=GetBValue(aColor);
   SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TGLPixel32);
var
   color : TGLPixel24;
begin
   color.r:=aColor.r;
   color.g:=aColor.g;
   color.b:=aColor.b;
   SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TGLPixel24);
var
   i : Integer;
   intCol : Integer;
begin
   intCol:=(PInteger(@aColor)^) and $FFFFFF;
   for i:=0 to (FDataSize div 4)-1 do
      if PInteger(@FData[i])^ and $FFFFFF=intCol then
         FData[i].a:=0
      else FData[i].a:=255;
end;

// SetAlphaToValue
//
procedure TGLBitmap32.SetAlphaToValue(const aValue : Byte);
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do
      FData[i].a:=aValue
end;

// SetAlphaToFloatValue
//
procedure TGLBitmap32.SetAlphaToFloatValue(const aValue : Single);
begin
   SetAlphaToValue(Byte(Trunc(aValue*255) and 255));
end;

// InvertAlpha
//
procedure TGLBitmap32.InvertAlpha;
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do
      FData[i].a:=255-FData[i].a;
end;

// SqrtAlpha
//
procedure TGLBitmap32.SqrtAlpha;
var
   i : Integer;
	sqrt255Array : PSqrt255Array;
begin
   sqrt255Array:=GetSqrt255Array;
   for i:=0 to (FDataSize div 4)-1 do with FData[i] do
      a:=sqrt255Array[(Integer(r)+Integer(g)+Integer(b)) div 3];
end;

// BrightnessCorrection
//
procedure TGLBitmap32.BrightnessCorrection(const factor : Single);
begin
   if Assigned(FData) then
      BrightenRGBAArray(FData, FDataSize div 4, factor);
end;

// GammaCorrection
//
procedure TGLBitmap32.GammaCorrection(const gamma : Single);
begin
   if Assigned(FData) then
      GammaCorrectRGBAArray(FData, FDataSize div 4, gamma);
end;

// DownSampleByFactor2
//
procedure TGLBitmap32.DownSampleByFactor2;
type
   T2Pixel32 = packed array [0..1] of TGLPixel32;
   P2Pixel32 = ^T2Pixel32;

   procedure ProcessRow3DNow(pDest : PGLPixel32; pLineA, pLineB : P2Pixel32; n : Integer);
   asm     // 3DNow! version 30% faster
      db $0F,$EF,$C0           /// pxor        mm0, mm0          // set mm0 to [0, 0, 0, 0]

@@Loop:
      db $0F,$0D,$81,$00,$01,$00,$00/// prefetch    [ecx+256]

      db $0F,$6F,$0A           /// movq        mm1, [edx]
      db $0F,$6F,$11           /// movq        mm2, [ecx]

      db $0F,$6F,$D9           /// movq        mm3, mm1
      db $0F,$6F,$E2           /// movq        mm4, mm2

      db $0F,$60,$C8           /// punpcklbw   mm1, mm0          // promote to 16 bits and add LineA pixels
      db $0F,$68,$D8           /// punpckhbw   mm3, mm0
      db $0F,$FD,$CB           /// paddw       mm1, mm3

      db $0F,$60,$D0           /// punpcklbw   mm2, mm0          // promote to 16 bits and add LineB pixels
      db $0F,$68,$E0           /// punpckhbw   mm4, mm0
      db $0F,$FD,$D4           /// paddw       mm2, mm4

      db $0F,$FD,$CA           /// paddw       mm1, mm2          // add LineA and LineB pixels

      db $0F,$71,$D1,$02       /// psrlw       mm1, 2            // divide by 4
      db $0F,$67,$C9           /// packuswb    mm1, mm1          // reduce to 8 bits and store point
      db $0F,$7E,$08           /// movd        [eax], mm1

      add         edx, 8
      add         ecx, 8
      add         eax, 4

      dec         [n]
      jnz         @@Loop

      db $0F,$0E               /// femms
   end;

   procedure ProcessRowPascal(pDest : PGLPixel32; pLineA, pLineB : P2Pixel32; n : Integer);
   var
      i : Integer;
   begin
      for i:=0 to n-1 do begin
         pDest.r:=(pLineA[0].r+pLineA[1].r+pLineB[0].r+pLineB[1].r) shr 2;
         pDest.g:=(pLineA[0].g+pLineA[1].g+pLineB[0].g+pLineB[1].g) shr 2;
         pDest.b:=(pLineA[0].b+pLineA[1].b+pLineB[0].b+pLineB[1].b) shr 2;
         pDest.a:=(pLineA[0].a+pLineA[1].a+pLineB[0].a+pLineB[1].a) shr 2;
         Inc(pLineA);
         Inc(pLineB);
         Inc(pDest);
      end;
   end;// }

var
   y, w2, h2 : Integer;
   pDest : PGLPixel32;
   pLineA, pLineB : P2Pixel32;
begin
   if (FWidth<=1) or (FHeight<=1) then Exit;
   w2:=FWidth shr 1;
   h2:=FHeight shr 1;
   pDest:=@FData[0];
   pLineA:=@FData[0];
   pLineB:=@FData[Width];
   if vSIMD=1 then begin
      for y:=0 to h2-1 do begin
         ProcessRow3DNow(pDest, pLineA, pLineB, w2);
         Inc(pDest, w2);
         Inc(pLineA, Width);
         Inc(pLineB, Width);
      end;
   end else begin
      for y:=0 to h2-1 do begin
         ProcessRowPascal(pDest, pLineA, pLineB, w2);
         Inc(pDest, w2);
         Inc(pLineA, Width);
         Inc(pLineB, Width);
      end;
   end;
   FWidth:=w2;
   FHeight:=h2;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
end;

// RegisterAsOpenGLTexture
//
procedure TGLBitmap32.RegisterAsOpenGLTexture(target : TGLUInt;
                                              minFilter : TGLMinFilter;
                                              texFormat : Integer);
var
   tw, th : Integer;
begin
   RegisterAsOpenGLTexture(target, minFilter, texFormat, tw, th);
end;

// RegisterAsOpenGLTexture
//
procedure TGLBitmap32.RegisterAsOpenGLTexture(target : TGLUInt;
                                              minFilter : TGLMinFilter;
                                              texFormat : Integer;
                                              var texWidth, texHeight : Integer);
   function IsFloat(texFormat : Integer) : boolean;
   begin
      // Currently only support 16bit and 32bit RGBA formats
      // NV and ATI float types would later be replaced by ARB float types
      Result:=   (texFormat=GL_FLOAT_RGBA16_NV) or (texFormat=GL_FLOAT_RGBA32_NV)
              or (texFormat=GL_RGBA_FLOAT16_ATI) or (texFormat=GL_RGBA_FLOAT32_ATI);
   end;

var
   w2, h2, maxSize : Integer;
   buffer : Pointer;
begin
   if (DataSize>0) then begin
      w2:=RoundUpToPowerOf2(Width);
      h2:=RoundUpToPowerOf2(Height);
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
      if w2>maxSize then w2:=maxSize;
      if h2>maxSize then h2:=maxSize;
      texWidth:=w2;
      texHeight:=h2;

      if not IsFloat(texFormat) then begin // Non-power-of-two for float_type
         if (w2<>Width) or (h2<>Height) then begin
            GetMem(buffer, w2*h2*4);
            gluScaleImage(GL_RGBA, Width, Height, GL_UNSIGNED_BYTE, Data, w2, h2,
                          GL_UNSIGNED_BYTE, buffer);
         end else buffer:=Pointer(FData);
      end else buffer:=Pointer(FData);

      try
         if IsFloat(texFormat) then begin // float_type
            // Note: see note in TGLFloatDataImage.NativeTextureTarget
//            if GL_ATI_texture_float then
//              assert(target=GL_TEXTURE_2D, 'ATI Float-type texture must use GL_TEXTURE_2D')
//            else
            Assert(target=GL_TEXTURE_RECTANGLE_NV, 'NV Float-type texture must use GL_TEXTURE_RECTANGLE_NV');
            // currently doesn't support
        		glTexImage2d( target, 0, texFormat, w2, h2, 0, GL_RGBA, GL_FLOAT, nil)
         end else begin
            case minFilter of
               miNearest, miLinear :
                  glTexImage2d(target, 0, texFormat, w2, h2, 0,
                               GL_RGBA, GL_UNSIGNED_BYTE, buffer)
            else
               if GL_SGIS_generate_mipmap and (target=GL_TEXTURE_2D) then begin
                  // hardware-accelerated when supported
                  glTexParameteri(target, GL_GENERATE_MIPMAP_SGIS, GL_TRUE);
                  glTexImage2d(target, 0, texFormat, w2, h2, 0,
                               GL_RGBA, GL_UNSIGNED_BYTE, buffer);
               end else begin
                  // slower (software mode)
                  gluBuild2DMipmaps(target, texFormat, w2, h2,
                                    GL_RGBA, GL_UNSIGNED_BYTE, buffer);
               end;
            end;
         end;
		finally
         if buffer<>Pointer(FData) then
   			FreeMem(buffer);
		end;
   end;
end;

// ReadPixels
//
procedure TGLBitmap32.ReadPixels(const area : TGLRect);
begin
   FWidth:=(area.Right-area.Left) and $FFFC;
   FHeight:=(area.Bottom-area.Top);
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   glReadPixels(0, 0, FWidth, FHeight, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// DrawPixels
//
procedure TGLBitmap32.DrawPixels(const x, y : Single);
begin
   glRasterPos2f(x, y);
   glDrawPixels(Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// TGLBitmap32
//
procedure TGLBitmap32.GrayScaleToNormalMap(const scale : Single;
                                           wrapX : Boolean = True; wrapY : Boolean = True);
var
   x, y : Integer;
   dcx, dcy : Single;
   invLen : Single;
   maskX, maskY : Integer;
   curRow, nextRow, prevRow : PGLPixel32Array;
   normalMapBuffer : PGLPixel32Array;
   p : PGLPixel32;
begin
   if Assigned(FData) then begin
      normalMapBuffer:=AllocMem(DataSize);
      try
         maskX:=Width-1;
         maskY:=Height-1;
         p:=@normalMapBuffer[0];
         for y:=0 to Height-1 do begin
            curRow:=GetScanLine(y);
            if wrapY then begin
               prevRow:=GetScanLine((y-1) and maskY);
               nextRow:=GetScanLine((y+1) and maskY);
            end else begin
               if y>0 then
                  prevRow:=GetScanLine(y-1)
               else prevRow:=curRow;
               if y<Height-1 then
                  nextRow:=GetScanLine(y+1)
               else nextRow:=curRow;
            end;
            for x:=0 to Width-1 do begin
               if wrapX then
                  dcx:=scale*(curRow[(x-1) and maskX].g-curRow[(x+1) and maskX].g)
               else begin
                  if x=0 then
                     dcx:=scale*(curRow[x].g-curRow[x+1].g)
                  else if x<Width-1 then
                     dcx:=scale*(curRow[x-1].g-curRow[x].g)
                  else dcx:=scale*(curRow[x-1].g-curRow[x+1].g);
               end;
               dcy:=scale*(prevRow[x].g-nextRow[x].g);
               invLen:=127*RSqrt(dcx*dcx+dcy*dcy+1);
               with p^ do begin
                  r:=Integer(Round(128+ClampValue(dcx*invLen, -128, 127)));
                  g:=Integer(Round(128+ClampValue(dcy*invLen, -128, 127)));
                  b:=Integer(Round(128+ClampValue(invLen, -128, 127)));
                  a:=255;
               end;
               Inc(p);
            end;
         end;
         Move(normalMapBuffer^, FData^, DataSize);
      finally
         FreeMem(normalMapBuffer);
      end;
   end;
end;
{
var
   x, y : Integer;
   p1, p2, p3 : Single;
   dcx, dcy : Single;
   invLen, scaleDiv255 : Single;
   curRow, nextRow : PGLPixel32Array;
   curRowZeroG : Byte;
   backupRowZero : PGLPixel32Array;
begin
   if Assigned(FData) then begin
      nextRow:=FData;
      backupRowZero:=AllocMem(Width*4);
      Move(FData[0], backupRowZero[0], Width*4);
      scaleDiv255:=scale*(1/255);
      for y:=0 to Height-1 do begin
         curRow:=nextRow;
         if y<Height-1 then
            nextRow:=@FData[y*Width+Width]
         else nextRow:=backupRowZero;
         curRowZeroG:=curRow[0].g;
         for x:=0 to Width-1 do begin
            p1:=curRow[x].g;
            p2:=nextRow[x].g;
            if x<Width-1 then
               p3:=curRow[x+1].g
            else p3:=curRowZeroG;

            dcx:=scaleDiv255*(p3-p1);
            dcy:=scaleDiv255*(p1-p2);

            invLen:=RSqrt(Sqr(dcx)+Sqr(dcy)+1);

            with curRow[x] do begin
               r:=Round(128+127*ClampValue(dcx*invLen, -1, 1));
               g:=Round(128+127*ClampValue(dcy*invLen, -1, 1));
               b:=Round(128+127*invLen);
               a:=255;
            end;
         end;
      end;
      FreeMem(backupRowZero);
   end;
end;
}
// NormalizeNormalMap
//
procedure TGLBitmap32.NormalizeNormalMap;
var
   x, y : Integer;
   sr, sg, sb : Single;
   invLen : Single;
   curRow : PGLPixel32Array;
   p : PGLPixel32;
const
   cInv128 : Single = 1/128;
begin
   if Assigned(FData) then begin
      for y:=0 to Height-1 do begin
         curRow:=@FData[y*Width];
         for x:=0 to Width-1 do begin
            p:=@curRow[x];
            sr:=(p.r-128)*cInv128;
            sg:=(p.g-128)*cInv128;
            sb:=(p.b-128)*cInv128;
            invLen:=RSqrt(sr*sr+sg*sg+sb*sb);
            p.r:=Round(128+127*ClampValue(sr*invLen, -1, 1));
            p.g:=Round(128+127*ClampValue(sg*invLen, -1, 1));
            p.b:=Round(128+127*ClampValue(sb*invLen, -1, 1));
         end;
      end;
   end;
end;

end.

