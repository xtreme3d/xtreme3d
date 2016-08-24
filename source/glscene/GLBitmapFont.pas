//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLBitmapFont<p>

  Bitmap Fonts management classes for GLScene<p>

	<b>History : </b><font size=-1><ul>
      <li>09/03/05 - EG - Fixed space width during rendering
      <li>12/15/04 - Eugene Kryukov - Moved FCharRects to protected declaration in TGLCustomBitmapFont
      <li>18/10/04 - NelC - Fixed a texture reset bug in RenderString
      <li>28/06/04 - LR - Change TTextLayout to TGLTextLayout for Linux
      <li>27/06/04 - NelC - Added TGLFlatText.Assign
      <li>01/03/04 - SG - TGLCustomBitmapFont.RenderString now saves GL_CURRENT_BIT state
      <li>01/07/03 - EG - TGLCustomBitmapFont.TextOut now saves and restore state
      <li>07/05/03 - EG - TGLFlatText Notification fix, added Options
      <li>30/10/02 - EG - Added TGLFlatText
      <li>29/09/02 - EG - Added TexCoords LUT, faster RenderString,
                          removed TBitmapFontRange.Widths
      <li>28/09/02 - EG - Introduced TGLCustomBitmapFont
      <li>06/09/02 - JAJ - Prepared for TGLWindowsBitmapFont
      <li>28/08/02 - EG - Repaired fixed CharWidth, variable CharWidth not yet repaired
      <li>12/08/02 - JAJ - Merged Dual Development, Alpha Channel and CharWidth are now side by side
      <li>UNKNOWN  - EG - Added Alpha Channel.
      <li>02/06/02 - JAJ - Modified to flexible character width
      <li>20/01/02 - EG - Dropped 'Graphics' dependency
      <li>10/09/01 - EG - Fixed visibility of tile 0
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
	   <li>15/01/01 - EG - Creation
	</ul></font>
}
unit GLBitmapFont;

interface

uses Classes, GLScene, VectorGeometry, GLMisc, GLContext, GLCrossPlatform,
   GLTexture, GLState, GLUtils, GLGraphics;

type

	// TBitmapFontRange
	//
   {: An individual character range in a bitmap font.<p>
      A range allows mapping ASCII characters to character tiles in a font
      bitmap, tiles are enumerated line then column (raster). }
	TBitmapFontRange = class (TCollectionItem)
	   private
	      { Private Declarations }
         FStartASCII, FStopASCII : Char;
         FStartGlyphIdx : Integer;

	   protected
	      { Protected Declarations }
         procedure SetStartASCII(const val : Char);
         procedure SetStopASCII(const val : Char);
         procedure SetStartGlyphIdx(const val : Integer);
         function GetDisplayName : String; override;

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;
         procedure NotifyChange;

	   published
	      { Published Declarations }
         property StartASCII : Char read FStartASCII write SetStartASCII;
         property StopASCII : Char read FStopASCII write SetStopASCII;
         property StartGlyphIdx : Integer read FStartGlyphIdx write SetStartGlyphIdx;
	end;

	// TBitmapFontRanges
	//
	TBitmapFontRanges = class (TCollection)
	   protected
	      { Protected Declarations }
	      FOwner : TComponent;

	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TBitmapFontRange);
	      function GetItems(index : Integer) : TBitmapFontRange;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);
	      destructor  Destroy; override;

         function Add : TBitmapFontRange; overload;
         function Add(startASCII, stopASCII : Char) : TBitmapFontRange; overload;
	      function FindItemID(ID : Integer) : TBitmapFontRange;
	      property Items[index : Integer] : TBitmapFontRange read GetItems write SetItems; default;

         {: Converts an ASCII character into a tile index.<p>
            Return -1 if character cannot be rendered. }
	      function CharacterToTileIndex(aChar : Char) : Integer;
         procedure NotifyChange;

         //: Total number of characters in the ranges
         function CharacterCount : Integer;
   end;

   TDynIntegerArray = array of Integer;

	// TGLCustomBitmapFont
	//
   {: Provides access to individual characters in a BitmapFont.<p>
      Only fixed-width bitmap fonts are supported, the characters are enumerated
      in a raster fashion (line then column).<br>
      Transparency is all or nothing, the transparent color being that of the
      top left pixel of the Glyphs bitmap.<p>
      Performance note: as usual, for best performance, you base font bitmap
      dimensions should be close to a power of two, and have at least 1 pixel
      spacing between characters (horizontally and vertically) to avoid artefacts
      when rendering with linear filtering. }
	TGLCustomBitmapFont = class (TGLUpdateAbleComponent)
	   private
	      { Private Declarations }
         FRanges : TBitmapFontRanges;
         FGlyphs : TGLPicture;
         FCharWidth, FCharHeight : Integer;
         FGlyphsIntervalX, FGlyphsIntervalY : Integer;
         FHSpace, FVSpace, FHSpaceFix : Integer;
         FUsers : TList;
         FTextureHandle : TGLTextureHandle;
         FHandleIsDirty : Boolean;
			FMinFilter : TGLMinFilter;
			FMagFilter : TGLMagFilter;
         FTextureWidth, FTextureHeight : Integer;
         FGlyphsAlpha : TGLTextureImageAlpha;
         FCharWidths : TDynIntegerArray;
	   protected
	      { Protected Declarations }
         FCharRects : array of TVector;
         property CharWidths : TDynIntegerArray read FCharWidths write FCharWidths;
         procedure ResetCharWidths(w : Integer = -1);
         procedure SetCharWidths(index, value : Integer);
         procedure SetCharRects(index : Integer; const p : TVector);

         procedure SetRanges(const val : TBitmapFontRanges);
         procedure SetGlyphs(const val : TGLPicture);
         procedure SetCharWidth(const val : Integer);
         procedure SetCharHeight(const val : Integer);
         procedure SetGlyphsIntervalX(const val : Integer);
         procedure SetGlyphsIntervalY(const val : Integer);
	      procedure OnGlyphsChanged(Sender : TObject);
         procedure SetHSpace(const val : Integer);
         procedure SetVSpace(const val : Integer);
			procedure SetMagFilter(AValue: TGLMagFilter);
			procedure SetMinFilter(AValue: TGLMinFilter);
         procedure SetGlyphsAlpha(val : TGLTextureImageAlpha);

         procedure FreeTextureHandle; dynamic;
         function TextureFormat : Integer; dynamic;

	      procedure InvalidateUsers;
	      function  CharactersPerRow : Integer;
	      procedure GetCharTexCoords(ch : Char; var topLeft, bottomRight : TTexPoint);
         procedure PrepareImage; virtual;
         procedure PrepareParams;

         {: A single bitmap containing all the characters.<p>
            The transparent color is that of the top left pixel. }
         property Glyphs : TGLPicture read FGlyphs write SetGlyphs;
         {: Nb of horizontal pixels between two columns in the Glyphs. }
         property GlyphsIntervalX : Integer read FGlyphsIntervalX write SetGlyphsIntervalX;
         {: Nb of vertical pixels between two rows in the Glyphs. }
         property GlyphsIntervalY : Integer read FGlyphsIntervalY write SetGlyphsIntervalY;
         {: Ranges allow converting between ASCII and tile indexes.<p>
            See TGLCustomBitmapFontRange. }
         property Ranges : TBitmapFontRanges read FRanges write SetRanges;

         {: Width of a single character. }
         property CharWidth : Integer read FCharWidth write SetCharWidth default 16;
         {: Pixels in between rendered characters (horizontally). }
         property HSpace : Integer read FHSpace write SetHSpace default 1;
         {: Pixels in between rendered lines (vertically). }
         property VSpace : Integer read FVSpace write SetVSpace default 1;
         {: Horizontal spacing fix offset.<p>
            This property is for internal use, and is added to the hspacing
            of each character when rendering, typically to fix extra spacing. }
         property HSpaceFix : Integer read FHSpaceFix write FHSpaceFix;

			property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default maLinear;
			property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default miLinear;
         property GlyphsAlpha: TGLTextureImageAlpha read FGlyphsAlpha write FGlyphsAlpha default tiaDefault;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure RegisterUser(anObject : TGLBaseSceneObject); virtual;
	      procedure UnRegisterUser(anObject : TGLBaseSceneObject); virtual;

         {: Renders the given string at current position or at position given by the optional position variable.<p>
            The current matrix is blindly used, meaning you can render all kinds
            of rotated and linear distorted text with this method, OpenGL
            Enable states are also possibly altered. }
	      procedure RenderString(var rci : TRenderContextInfo;
                                const aString : String; alignment : TAlignment;
                                layout : TGLTextLayout; const color : TColorVector;
                                position : PVector = nil; reverseY : Boolean = False);
         {: A simpler canvas-style TextOut helper for RenderString.<p>
            The rendering is reversed along Y by default, to allow direct use
            with TGLCanvas }
         procedure TextOut(var rci : TRenderContextInfo; x, y : Single; const text : String; const color : TColorVector); overload;
         procedure TextOut(var rci : TRenderContextInfo; x, y : Single; const text : String; const color : TColor); overload;
         function TextWidth(const text : String) : Integer;

         {: Get the actual width for this char. }
         function GetCharWidth(ch : Char) : Integer;
         {: Get the actual pixel width for this string. }
         function CalcStringWidth(const st : String) : Integer;

         {: Height of a single character. }
         property CharHeight : Integer read FCharHeight write SetCharHeight default 16;
	end;

	// TGLBitmapFont
	//
   {: See TGLCustomBitmapFont.<p>
      This class only publuishes some of the properties. }
	TGLBitmapFont = class (TGLCustomBitmapFont)
	   published
	      { Published Declarations }
         property Glyphs;
         property GlyphsIntervalX;
         property GlyphsIntervalY;
         property Ranges;
         property CharWidth;
         property CharHeight;
         property HSpace;
         property VSpace;
			property MagFilter;
			property MinFilter;
         property GlyphsAlpha;
	end;

   // TGLFlatTextOptions
   //
   TGLFlatTextOption = (ftoTwoSided);
   TGLFlatTextOptions = set of TGLFlatTextOption;

   // TGLFlatText
   //
   {: A 2D text displayed and positionned in 3D coordinates.<p>
      The FlatText uses a character font defined and stored by a TGLBitmapFont
      component. Default character scale is 1 font pixel = 1 space unit. }
	TGLFlatText = class (TGLImmaterialSceneObject)
	   private
	      { Private Declarations }
         FBitmapFont : TGLCustomBitmapFont;
         FText : String;
         FAlignment : TAlignment;
         FLayout : TGLTextLayout;
         FModulateColor : TGLColor;
         FOptions : TGLFlatTextOptions;

	   protected
	      { Protected Declarations }
         procedure SetBitmapFont(const val : TGLCustomBitmapFont);
         procedure SetText(const val : String);
         procedure SetAlignment(const val : TAlignment);
         procedure SetLayout(const val : TGLTextLayout);
         procedure SetModulateColor(const val : TGLColor);
         procedure SetOptions(const val : TGLFlatTextOptions);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

		public
			{ Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

		     procedure Assign(Source: TPersistent); override;

	   published
	      { Published Declarations }
         {: Refers the bitmap font to use.<p>
            The referred bitmap font component stores and allows access to
            individual character bitmaps. }
         property BitmapFont : TGLCustomBitmapFont read FBitmapFont write SetBitmapFont;
         {: Text to render.<p>
            Be aware that only the characters available in the bitmap font will
            be rendered. CR LF sequences are allowed. }
         property Text : String read FText write SetText;
         {: Controls the text alignment (horizontal).<p>
            Possible values : taLeftJustify, taRightJustify, taCenter }
         property Alignment : TAlignment read FAlignment write SetAlignment;
         {: Controls the text layout (vertical).<p>
            Possible values : tlTop, tlCenter, tlBottom }
         property Layout : TGLTextLayout read FLayout write SetLayout;
         {: Color modulation, can be used for fade in/out too.}
         property ModulateColor : TGLColor read FModulateColor write SetModulateColor;
         {: Flat text options.<p>
            <ul><li>ftoTwoSided : when set the text will be visible from its two
            sides even if faceculling is on (at the scene-level).
            </ul> }
         property Options : TGLFlatTextOptions read FOptions write SetOptions;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, XOpenGL;

// ------------------
// ------------------ TBitmapFontRange ------------------
// ------------------

// Create
//
constructor TBitmapFontRange.Create(Collection : TCollection);
begin
   inherited Create(Collection);
end;

// Destroy
//
destructor TBitmapFontRange.Destroy;
begin
   inherited;
end;

// Assign
//
procedure TBitmapFontRange.Assign(Source: TPersistent);
begin
	if Source is TBitmapFontRange then begin
      FStartASCII:=TBitmapFontRange(Source).FStartASCII;
      FStopASCII:=TBitmapFontRange(Source).FStopASCII;
      FStartGlyphIdx:=TBitmapFontRange(Source).FStartGlyphIdx;
	end;
	inherited;
end;

// NotifyChange
//
procedure TBitmapFontRange.NotifyChange;
begin
   if Assigned(Collection) then
      (Collection as TBitmapFontRanges).NotifyChange;
end;

// GetDisplayName
//
function TBitmapFontRange.GetDisplayName : String;
begin
	Result:=Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
                  [Integer(StartASCII), Integer(StopASCII), StartGlyphIdx,
                   StartGlyphIdx+Integer(StopASCII)-Integer(StartASCII)]);
end;

// SetStartASCII
//
procedure TBitmapFontRange.SetStartASCII(const val : Char);
begin
   if val<>FStartASCII then begin
      FStartASCII:=val;
      if FStartASCII>FStopASCII then
         FStopASCII:=FStartASCII;
      NotifyChange;
   end;
end;

// SetStopASCII
//
procedure TBitmapFontRange.SetStopASCII(const val : Char);
begin
   if FStopASCII<>val then begin
      FStopASCII:=val;
      if FStopASCII<FStartASCII then begin
         FStartASCII:=FStopASCII;
      end;
      NotifyChange;
   end;
end;

// SetStartGlyphIdx
//
procedure TBitmapFontRange.SetStartGlyphIdx(const val : Integer);
begin
   if val>=0 then
      FStartGlyphIdx:=val
   else FStartGlyphIdx:=0;
   TBitmapFontRanges(Collection).NotifyChange;
end;

// ------------------
// ------------------ TBitmapFontRanges ------------------
// ------------------

// Create
//
constructor TBitmapFontRanges.Create(AOwner : TComponent);
begin
	FOwner:=AOwner;
	inherited Create(TBitmapFontRange);
end;

// Destroy
//
destructor  TBitmapFontRanges.Destroy;
begin
  inherited;
end;

// GetOwner
//
function TBitmapFontRanges.GetOwner: TPersistent;
begin
	Result:=FOwner;
end;

// SetItems
//
procedure TBitmapFontRanges.SetItems(index : Integer; const val : TBitmapFontRange);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TBitmapFontRanges.GetItems(index : Integer) : TBitmapFontRange;
begin
	Result:=TBitmapFontRange(inherited Items[index]);
end;

// Add
//
function TBitmapFontRanges.Add : TBitmapFontRange;
begin
	Result:=(inherited Add) as TBitmapFontRange;
end;

// Add
//
function TBitmapFontRanges.Add(startASCII, stopASCII : Char) : TBitmapFontRange;
begin
   Result:=Add;
   Result.StartASCII:=startASCII;
   Result.StopASCII:=stopASCII;
end;

// FindItemID
//
function TBitmapFontRanges.FindItemID(ID: Integer): TBitmapFontRange;
begin
	Result:=(inherited FindItemID(ID)) as TBitmapFontRange;
end;

// CharacterToTileIndex
//
function TBitmapFontRanges.CharacterToTileIndex(aChar : Char) : Integer;
var
   i : Integer;
begin
   Result:=-1;
   for i:=0 to Count-1 do with Items[i] do begin
      if (aChar>=StartASCII) and (aChar<=StopASCII) then begin
         Result:=StartGlyphIdx+Integer(aChar)-Integer(StartASCII);
         Break;
      end;
   end;
end;

// NotifyChange
//
procedure TBitmapFontRanges.NotifyChange;
begin
   if Assigned(FOwner) and (FOwner is TGLBaseSceneObject) then
      TGLBaseSceneObject(FOwner).StructureChanged;
end;

// CharacterCount
//
function TBitmapFontRanges.CharacterCount : Integer;
var
   i : Integer;
begin
   Result:=0;
   for i:=0 to Count-1 do with Items[i] do
      Inc(Result, Integer(StopASCII)-Integer(StartASCII)+1);
end;

// ------------------
// ------------------ TGLCustomBitmapFont ------------------
// ------------------

// Creat
//
constructor TGLCustomBitmapFont.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FRanges:=TBitmapFontRanges.Create(Self);
   FGlyphs:=TGLPicture.Create;
   FGlyphs.OnChange:=OnGlyphsChanged;
   FCharWidth:=16;
   FCharHeight:=16;
   FHSpace:=1;
   FVSpace:=1;
   FUsers:=TList.Create;
   FHandleIsDirty:=True;
   FMinFilter:=miLinear;
   FMagFilter:=maLinear;
   FTextureHandle:=TGLTextureHandle.Create;
end;

// Destroy
//
destructor TGLCustomBitmapFont.Destroy;
begin
	inherited Destroy;
   FTextureHandle.Free;
   FRanges.Free;
   FGlyphs.Free;
   Assert(FUsers.Count=0);
   FUsers.Free;
end;

// GetCharWidth
//
function TGLCustomBitmapFont.GetCharWidth(ch : Char) : Integer;
begin
   if Length(FCharWidths)=0 then
      ResetCharWidths;
   Result:=FCharWidths[Integer(ch)]
end;

// CalcStringWidth
//
function TGLCustomBitmapFont.CalcStringWidth(const st : String) : Integer;
var
   i : Integer;
begin
   if st<>'' then begin
      Result:=-HSpace+Length(st)*(HSpaceFix+HSpace);
      for i:=1 to Length(st) do
         Result:=Result+GetCharWidth(st[i]);
   end else Result:=0;
end;

// ResetCharWidths
//
procedure TGLCustomBitmapFont.ResetCharWidths(w : Integer = -1);
var
   i : Integer;
begin
   if Length(FCharWidths)=0 then
      SetLength(FCharWidths, 256);
   if w<0 then
      w:=CharWidth;
   for i:=0 to 255 do
      FCharWidths[i]:=w;
end;

// SetCharWidths
//
procedure TGLCustomBitmapFont.SetCharWidths(index, value : Integer);
begin
   FCharWidths[index]:=value;
end;

// SetCharRects
//
procedure TGLCustomBitmapFont.SetCharRects(index : Integer; const p : TVector);
begin
   if Length(FCharRects)<256 then
      Setlength(FCharRects, 256);
   FCharRects[index]:=p;
end;

// SetRanges
//
procedure TGLCustomBitmapFont.SetRanges(const val : TBitmapFontRanges);
begin
   FRanges.Assign(val);
   InvalidateUsers;
end;

// SetGlyphs
//
procedure TGLCustomBitmapFont.SetGlyphs(const val : TGLPicture);
begin
   FGlyphs.Assign(val);
end;

// SetCharWidth
//
procedure TGLCustomBitmapFont.SetCharWidth(const val : Integer);
begin
   if val<>FCharWidth then begin
      if val>1 then
         FCharWidth:=val
      else FCharWidth:=1;
      InvalidateUsers;
   end;
end;

// SetCharHeight
//
procedure TGLCustomBitmapFont.SetCharHeight(const val : Integer);
begin
   if val<>FCharHeight then begin
      if val>1 then
         FCharHeight:=val
      else FCharHeight:=1;
      InvalidateUsers;
   end;
end;

// SetGlyphsIntervalX
//
procedure TGLCustomBitmapFont.SetGlyphsIntervalX(const val : Integer);
begin
   if val>0 then
      FGlyphsIntervalX:=val
   else FGlyphsIntervalX:=0;
   InvalidateUsers;
end;

// SetGlyphsIntervalY
//
procedure TGLCustomBitmapFont.SetGlyphsIntervalY(const val : Integer);
begin
   if val>0 then
      FGlyphsIntervalY:=val
   else FGlyphsIntervalY:=0;
   InvalidateUsers;
end;

// SetHSpace
//
procedure TGLCustomBitmapFont.SetHSpace(const val : Integer);
begin
   if val>0 then
      FHSpace:=val
   else FHSpace:=0;
   InvalidateUsers;
end;

// SetVSpace
//
procedure TGLCustomBitmapFont.SetVSpace(const val : Integer);
begin
   if val>0 then
      FVSpace:=val
   else FVSpace:=0;
   InvalidateUsers;
end;

// SetMagFilter
//
procedure TGLCustomBitmapFont.SetMagFilter(AValue: TGLMagFilter);
begin
	if AValue <> FMagFilter then begin
		FMagFilter:=AValue;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// SetMinFilter
//
procedure TGLCustomBitmapFont.SetMinFilter(AValue: TGLMinFilter);
begin
	if AValue <> FMinFilter then begin
		FMinFilter:=AValue;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// SetGlyphsAlpha
//
procedure TGLCustomBitmapFont.SetGlyphsAlpha(val : TGLTextureImageAlpha);
begin
	if val<>FGlyphsAlpha then begin
		FGlyphsAlpha:=val;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// OnGlyphsChanged
//
procedure TGLCustomBitmapFont.OnGlyphsChanged(Sender : TObject);
begin
   InvalidateUsers;
end;

// RegisterUser
//
procedure TGLCustomBitmapFont.RegisterUser(anObject : TGLBaseSceneObject);
begin
   Assert(FUsers.IndexOf(anObject)<0);
   FUsers.Add(anObject);
end;

// UnRegisterUser
//
procedure TGLCustomBitmapFont.UnRegisterUser(anObject : TGLBaseSceneObject);
begin
   FUsers.Remove(anObject);
end;

// PrepareImage
//
procedure TGLCustomBitmapFont.PrepareImage;
var
   bitmap : TGLBitmap;
   bitmap32 : TGLBitmap32;
begin
   bitmap:=TGLBitmap.Create;
   with bitmap do begin
      PixelFormat:=glpf24bit;
      Width:=RoundUpToPowerOf2(Glyphs.Width);
      Height:=RoundUpToPowerOf2(Glyphs.Height);
      Canvas.Draw(0, 0, Glyphs.Graphic);
   end;
   bitmap32:=TGLBitmap32.Create;
   bitmap32.Assign(bitmap);
   bitmap.Free;
   with bitmap32 do begin
      case FGlyphsAlpha of
         tiaAlphaFromIntensity :
            SetAlphaFromIntensity;
         tiaSuperBlackTransparent :
            SetAlphaTransparentForColor($000000);
         tiaLuminance :
            SetAlphaFromIntensity;
         tiaLuminanceSqrt : begin
            SetAlphaFromIntensity;
            SqrtAlpha;
         end;
         tiaOpaque :
            SetAlphaToValue(255);
         tiaDefault,
         tiaTopLeftPointColorTransparent :
            SetAlphaTransparentForColor(Data[Width*(Height-1)]);
      else
         Assert(False);
      end;
      RegisterAsOpenGLTexture(GL_TEXTURE_2D, MinFilter, TextureFormat);
      FTextureWidth:=Width;
      FTextureHeight:=Height;
      Free;
   end;
end;

// PrepareParams
//
procedure TGLCustomBitmapFont.PrepareParams;
const
	cTextureMagFilter : array [maNearest..maLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR );
	cTextureMinFilter : array [miNearest..miLinearMipmapLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
							  GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
							  GL_LINEAR_MIPMAP_LINEAR );
begin
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
end;

// RenderString
//
procedure TGLCustomBitmapFont.RenderString(var rci : TRenderContextInfo;
            const aString : String; alignment : TAlignment;
            layout : TGLTextLayout; const color : TColorVector; position : PVector = nil;
            reverseY : Boolean = False);

   function AlignmentAdjustement(p : Integer) : Single;
   var
      i : Integer;
   begin
      i:=0;
      while (p<=Length(aString)) and (aString[p]<>#13) do begin
         Inc(p); Inc(i);
      end;
      case alignment of
         taLeftJustify : Result:=0;
         taRightJustify : Result:=-CalcStringWidth(Copy(aString, p-i, i))
      else // taCenter
          Result:=Round(-CalcStringWidth(Copy(aString, p-i, i))*0.5);
      end;
   end;

   function LayoutAdjustement : Single;
   var
      i, n : Integer;
   begin
      n:=1;
      for i:=1 to Length(aString) do
         if aString[i]=#13 then Inc(n);
      case TGLTextLayout(layout) of
         tlTop : Result:=0;
         tlBottom : Result:=(n*(CharHeight+VSpace)-VSpace);
      else // tlCenter
         Result:=Round((n*(CharHeight+VSpace)-VSpace)*0.5);
      end;
   end;

var
   i : Integer;
   topLeft, bottomRight : TTexPoint;
   vTopLeft, vBottomRight : TVector;
   deltaH, deltaV, spaceDeltaH : Single;
   currentChar : Char;
begin
   if (Glyphs.Width=0) or (aString='') then Exit;
   // prepare texture if necessary
   if FHandleIsDirty or (FTextureHandle.Handle=0) then begin
      // prepare handle
      if FTextureHandle.Handle=0 then begin
         FTextureHandle.AllocateHandle;
         Assert(FTextureHandle.Handle<>0);
      end;
      rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, FTextureHandle.Handle);
      // texture registration
      if Glyphs.Width<>0 then begin
         PrepareImage;
         PrepareParams;
      end;
      FHandleIsDirty:=False;
   end;
   // precalcs
   if Assigned(position) then
      MakePoint(vTopLeft, position[0]+AlignmentAdjustement(1), position[1]+LayoutAdjustement, 0)
   else MakePoint(vTopLeft, AlignmentAdjustement(1),  LayoutAdjustement, 0);
   deltaV:=-(CharHeight+VSpace);
   if reverseY then
      vBottomRight[1]:=vTopLeft[1]+CharHeight
   else vBottomRight[1]:=vTopLeft[1]-CharHeight;
   vBottomRight[2]:=0;
   vBottomRight[3]:=1;
   spaceDeltaH:=GetCharWidth(#32)+HSpaceFix+HSpace;
   // set states
	glEnable(GL_TEXTURE_2D);
   glDisable(GL_LIGHTING);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, FTextureHandle.Handle);
   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   // start rendering
   glPushAttrib(GL_CURRENT_BIT);
   glColor4fv(@color);
   glBegin(GL_QUADS);
   for i:=1 to Length(aString) do begin
      currentChar:=aString[i];
      case currentChar of
         #0..#12, #14..#31 : ; // ignore
         #13 : begin
            if Assigned(position) then
               vTopLeft[0]:=position[0]+AlignmentAdjustement(i+1)
            else vTopLeft[0]:=AlignmentAdjustement(i+1);
            vTopLeft[1]:=vTopLeft[1]+deltaV;
            if reverseY then
               vBottomRight[1]:=vTopLeft[1]+CharHeight
            else vBottomRight[1]:=vTopLeft[1]-CharHeight;
         end;
         #32 : vTopLeft[0]:=vTopLeft[0]+spaceDeltaH;
      else
         deltaH:=GetCharWidth(currentChar);
         if deltaH>0 then begin
            GetCharTexCoords(currentChar, topLeft, bottomRight);
            vBottomRight[0]:=vTopLeft[0]+deltaH;

            glTexCoord2fv(@topLeft);
            glVertex4fv(@vTopLeft);

            glTexCoord2f(topLeft.S, bottomRight.T);
            glVertex2f(vTopLeft[0], vBottomRight[1]);

            glTexCoord2fv(@bottomRight);
            glVertex4fv(@vBottomRight);

            glTexCoord2f(bottomRight.S, topLeft.T);
            glVertex2f(vBottomRight[0], vTopLeft[1]);

            vTopLeft[0]:=vTopLeft[0]+deltaH+HSpace;
         end;
      end;
   end;
   glEnd;
   glPopAttrib;
   rci.GLStates.ResetGLCurrentTexture;
end;

// TextOut
//
procedure TGLCustomBitmapFont.TextOut(var rci : TRenderContextInfo;
               x, y : Single; const text : String; const color : TColorVector);
var
   v : TVector;
begin
   glPushAttrib(GL_ENABLE_BIT);
   v[0]:=x;
   v[1]:=y;
   v[2]:=0;
   v[3]:=1;
   RenderString(rci, text, taLeftJustify, tlTop, color, @v, True);
   glPopAttrib;
end;

// TextOut
//
procedure TGLCustomBitmapFont.TextOut(var rci : TRenderContextInfo;
            x, y : Single; const text : String; const color : TColor);
begin
   TextOut(rci, x, y, text, ConvertWinColor(color));
end;

// TextWidth
//
function TGLCustomBitmapFont.TextWidth(const text : String) : Integer;
begin
   Result:=CalcStringWidth(text);
end;

// CharactersPerRow
//
function TGLCustomBitmapFont.CharactersPerRow : Integer;
begin
   if FGlyphs.Width>0 then
   	Result:=(FGlyphs.Width+FGlyphsIntervalX) div (FGlyphsIntervalX+FCharWidth)
   else Result:=0;
end;

// TileIndexToTexCoords
//
procedure TGLCustomBitmapFont.GetCharTexCoords(ch : Char; var topLeft, bottomRight : TTexPoint);
var
   i, j, sa : Integer;
   carX, carY : Integer;
   tileIndex : Integer;
   p : PVector;
begin
   if Length(FCharRects)=0 then begin
      SetLength(FCharRects, 256);
      for i:=0 to FRanges.Count-1 do with FRanges.Items[i] do begin
         sa:=Integer(StartASCII);
         tileIndex:=StartGlyphIdx;
         for j:=sa to Integer(StopASCII) do begin
            p:=@FCharRects[j];
            carX:=(tileIndex mod CharactersPerRow)*(CharWidth+GlyphsIntervalX);
            carY:=(tileIndex div CharactersPerRow)*(CharHeight+GlyphsIntervalY);
            p[0]:=(carX+0.05)/FTextureWidth;
            p[1]:=(FTextureHeight-(carY+0.05))/FTextureHeight;
            p[2]:=(carX+GetCharWidth(Char(j))-0.05)/FTextureWidth;
            p[3]:=(FTextureHeight-(carY+CharHeight-0.05))/FTextureHeight;
            Inc(tileIndex);
         end;
      end;
   end;
   p:=@FCharRects[Integer(ch)];
   topLeft.S:=p[0];
   topLeft.T:=p[1];
   bottomRight.S:=p[2];
   bottomRight.T:=p[3];
end;

// InvalidateUsers
//
procedure TGLCustomBitmapFont.InvalidateUsers;
var
   i : Integer;
begin
   ResetCharWidths;
   SetLength(FCharRects, 0);
   for i:=FUsers.Count-1 downto 0 do
      TGLBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

// FreeTextureHandle
//
procedure TGLCustomBitmapFont.FreeTextureHandle;
begin
   FTextureHandle.DestroyHandle;
   FHandleIsDirty := True;
end;

// TextureFormat
//
function TGLCustomBitmapFont.TextureFormat : Integer;
begin
   Result:=GL_RGBA;
end;

// ------------------
// ------------------ TGLFlatText ------------------
// ------------------

// Create
//
constructor TGLFlatText.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   FModulateColor:=TGLColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//
destructor TGLFlatText.Destroy;
begin
   FModulateColor.Free;
   BitmapFont:=nil;
   inherited;
end;

// Notification
//
procedure TGLFlatText.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FBitmapFont) then
      BitmapFont:=nil;
   inherited;
end;

// SetBitmapFont
//
procedure TGLFlatText.SetBitmapFont(const val : TGLCustomBitmapFont);
begin
   if val<>FBitmapFont then begin
      if Assigned(FBitmapFont) then
         FBitmapFont.UnRegisterUser(Self);
      FBitmapFont:=val;
      if Assigned(FBitmapFont) then begin
         FBitmapFont.RegisterUser(Self);
         FBitmapFont.FreeNotification(Self);
      end;
      StructureChanged;
   end;
end;

// SetText
//
procedure TGLFlatText.SetText(const val : String);
begin
   FText:=val;
   StructureChanged;
end;

// SetAlignment
//
procedure TGLFlatText.SetAlignment(const val : TAlignment);
begin
   FAlignment:=val;
   StructureChanged;
end;

// SetLayout
//
procedure TGLFlatText.SetLayout(const val : TGLTextLayout);
begin
   FLayout:=val;
   StructureChanged;
end;

// SetModulateColor
//
procedure TGLFlatText.SetModulateColor(const val: TGLColor);
begin
   FModulateColor.Assign(val);
end;

// SetOptions
//
procedure TGLFlatText.SetOptions(const val : TGLFlatTextOptions);
begin
   if val<>FOptions then begin
      FOptions:=val;
      StructureChanged;
   end;
end;

// DoRender
//
procedure TGLFlatText.DoRender(var rci : TRenderContextInfo;
                               renderSelf, renderChildren : Boolean);
begin
   if Assigned(FBitmapFont) and (Text<>'') then begin
      rci.GLStates.SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      glPushAttrib(GL_ENABLE_BIT);
      if FModulateColor.Alpha<>1 then begin
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      if ftoTwoSided in FOptions then
         glDisable(GL_CULL_FACE);
      FBitmapFont.RenderString(rci, Text, FAlignment, FLayout, FModulateColor.Color);
      glPopAttrib;
   end;
   if Count>0 then
      Self.RenderChildren(0, Count-1, rci);
end;

// Assign
//
procedure TGLFlatText.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLFlatText) then begin
      BitmapFont:=TGLFlatText(Source).BitmapFont;
      Text:=TGLFlatText(Source).Text;
      Alignment:=TGLFlatText(Source).Alignment;
      Layout:=TGLFlatText(Source).Layout;
      ModulateColor:=TGLFlatText(Source).ModulateColor;
      Options:=TGLFlatText(Source).Options;
   end;
   inherited Assign(Source);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TGLBitmapFont, TGLFlatText]);

end.

