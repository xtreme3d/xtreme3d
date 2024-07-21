unit GLTTF;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.Utils,
  GLS.Color,
  GLS.ApplicationFileIO,
  GLS.BitmapFont,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.Context,
  supertypes,
  sdl2,
  sdl2_ttf,
  SimpleDictionary;

type
  TGLFreetypeFont = class;

  TFreetypeCharacter = class
    public
      CodePoint: Integer;
      Initialized: Boolean;
      TextureId: GLuint;
      Width: Single;
      Height: Single;
      texWidth: Single;
      texHeight: Single;
      advance: Single;
      top: Single;
      left: Single;
      constructor Create;
  end;

  TGLFreetypeFont = class(TGLFont)
    private
      { Private Declarations }
    protected
      { Protected Declarations }
      FFontFilename: String;
      FFont: PTTF_Font;
      FCharHeight: Integer;
      FWhitespaceWidthEm: Real;
      FCharacters: TSimpleObjectDictionary;
      FFontFileBuffer: array of Byte;
      FIgnore: Boolean;
    public
      { Public Declarations }
      LineGap: Single;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure RegisterUser(anObject: TGLBaseSceneObject); override;
      procedure UnRegisterUser(anObject: TGLBaseSceneObject); override;
      function LoadFont(filename: String; cheight: Integer): Integer;
      function LoadCharacter(code: Integer): Integer;
      procedure RenderCharacter(chara: TFreetypeCharacter; posx, posy: Single);
      procedure RenderString(var ARci: TGLRenderContextInfo;
                             const aText: UnicodeString; aAlignment: TAlignment;
                             aLayout: TTextLayout; const aColor: TGLColorVector;
                             aPosition: PGLVector = nil; aReverseY: boolean = False); override;
  end;

implementation

function nextPowerOfTwo(a: Integer): Integer;
var
  rval: Integer;
begin
  rval := 1;
  while rval < a do
    rval := rval shl 1;
  result := rval;
end;

function utf8DecodeNext(s: String; var bytePos: Integer): Integer;
var
  c: Integer; // the first byte of the character
  c1, c2, c3: Integer;
begin
  c := Integer(s[bytePos]) and $FF;
  bytePos := bytePos + 1;

  // Zero continuation (0 to 127)
  if (c and $80) = 0 then
  begin
    result := c;
    Exit;
  end;
  // One continuation (128 to 2047)
  if (c and $E0) = $C0 then
  begin
    c1 := (Integer(s[bytePos]) and $FF) and $3F;
    bytePos := bytePos + 1;
    result := ((c and $1F) shl 6) or c1;
    Exit;
  end
  // Two continuation (2048 to 55295 and 57344 to 65535)
  else if (c and $F0) = $E0 then
  begin
    c1 := (Integer(s[bytePos]) and $FF) and $3F;
    bytePos := bytePos + 1;
    c2 := (Integer(s[bytePos]) and $FF) and $3F;
    bytePos := bytePos + 1;
    result := ((c and $0F) shl 12) or (c1 shl 6) or c2;
    Exit;
  end
  // Three continuation (65536 to 1114111)
  else if (c and $F8) = $F0 then
  begin
    c1 := (Integer(s[bytePos]) and $FF) and $3F;
    bytePos := bytePos + 1;
    c2 := (Integer(s[bytePos]) and $FF) and $3F;
    bytePos := bytePos + 1;
    c3 := (Integer(s[bytePos]) and $FF) and $3F;
    bytePos := bytePos + 1;
    result := (((c and $0F) shl 18) or (c1 shl 12) or (c2 shl 6) or c3);
    Exit;
  end;

  result := 0;
end;

constructor TFreetypeCharacter.Create;
begin
  CodePoint := 0;
  Initialized := False;
  TextureId := 0;
  Width := 0;
  Height := 0;
  advance := 0;
end;

constructor TGLFreetypeFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCharHeight := 12;
  FWhitespaceWidthEm := 0.25;
  LineGap := 1.2;
  FCharacters := TSimpleObjectDictionary.Create();
  FIgnore := False;
end;

destructor TGLFreetypeFont.Destroy;
begin
  FCharacters.Destroy;
  SetLength(FFontFileBuffer, 0);
  inherited Destroy;
end;

procedure TGLFreetypeFont.RegisterUser(anObject: TGLBaseSceneObject);
begin
   //Assert(FUsers.IndexOf(anObject)<0);
   //FUsers.Add(anObject);
end;

procedure TGLFreetypeFont.UnRegisterUser(anObject: TGLBaseSceneObject);
begin
   //FUsers.Remove(anObject);
end;

function TGLFreetypeFont.LoadFont(filename: String; cheight: Integer): Integer;
begin
   FCharHeight := cheight;
   FFont := TTF_OpenFont(PAnsiChar(AnsiString(filename)), FCharHeight);
end;

function TGLFreetypeFont.LoadCharacter(code: Integer): Integer;
var
   charIndex: Integer;
   minx, maxx, miny, maxy, advance: Integer;
   color: TSDL_Color;
   psurface, pglyphSurface: PSDL_Surface;
   charWidth, charHeight: Integer;
   tex: GLuint;
   character: TFreetypeCharacter;
begin
   charIndex := code;

   charWidth := 0;
   charHeight := 0;

   TTF_GlyphMetrics(FFont, code, @minx, @maxx, @miny, @maxy, @advance);

   tex := 0;

   character := TFreetypeCharacter.Create();
   character.CodePoint := code;
   character.Initialized := False;
   character.texWidth := 0;
   character.texHeight := 0;
   character.width := maxx - minx;
   character.height := maxy - miny;
   character.advance := advance;
   character.top := 0;
   character.left := 0;

   FCharacters.Values[code] := character;

   if (character.Width <= 0) or (character.Height <= 0) then begin
     result := 1;
     exit;
   end;

   color.r := 255;
   color.g := 255;
   color.b := 255;
   color.a := 255;
   psurface := TTF_RenderGlyph_Blended(FFont, code, color);

   if psurface <> Nil then begin
     charWidth := nextPowerOfTwo(psurface.w);
     charHeight := nextPowerOfTwo(psurface.h);

     character.Width := psurface.w;
     character.Height := psurface.h;

     pglyphSurface := SDL_CreateRGBSurface(0, charWidth, charHeight, 32, $000000ff, $0000ff00, $00ff0000, $ff000000);
     SDL_BlitSurface(psurface, Nil, pglyphSurface, Nil);
     SDL_FreeSurface(psurface);

     gl.Enable(GL_TEXTURE_2D);

     gl.GenTextures(1, @tex);
     gl.BindTexture(GL_TEXTURE_2D, tex);
     gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
     gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
     gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
     gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

     gl.PixelStorei(GL_UNPACK_ALIGNMENT, 1);

     gl.TexImage2D(GL_TEXTURE_2D,
        0, GL_RGBA8, charWidth, charHeight,
        0, GL_RGBA, GL_UNSIGNED_BYTE, pglyphSurface.pixels);

     character.Initialized := True;
     character.TextureId := tex;

     character.texWidth := charWidth;
     character.texHeight := charHeight;

     character.top := 0;
     character.left := 0;

     SDL_FreeSurface(pglyphSurface);

     result := 1;
   end
   else begin
     //ShowMessage(TTF_GetError());
     result := 0;
   end;
end;

procedure TGLFreetypeFont.RenderCharacter(chara: TFreetypeCharacter; posx, posy: Single);
var
  x, y: Single;
begin
   x := 0.5 / chara.texWidth + chara.width / chara.texWidth;
   y := 0.5 / chara.texHeight + chara.height / chara.texHeight;

   gl.BindTexture(GL_TEXTURE_2D, chara.textureId);
   gl.Begin_(GL_QUADS);
   gl.TexCoord2f(0, 0); gl.Vertex2f(posx, posy + chara.height);
   gl.TexCoord2f(0, y); gl.Vertex2f(posx, posy);
   gl.TexCoord2f(x, y); gl.Vertex2f(posx + chara.width, posy);
   gl.TexCoord2f(x, 0); gl.Vertex2f(posx + chara.width, posy + chara.height);
   gl.End_();
end;

procedure TGLFreetypeFont.RenderString(var ARci: TGLRenderContextInfo;
                                       const aText: UnicodeString; aAlignment: TAlignment;
                                       aLayout: TTextLayout; const aColor: TGLColorVector;
                                       aPosition: PGLVector = nil; aReverseY: boolean = False);
var
  i: Integer;
  c: Integer;
  len: Integer;
  x, y: Single;
  chara: TFreetypeCharacter;
  decoding: Boolean;
  whitespaceWidth: Real;
begin
   if (FIgnore = False) and (FCharacters.Count < 128) then
   begin
     for i := 0 to 128-1 do
     begin
       if LoadCharacter(i) = 0 then begin
         FIgnore := True;
         break;
       end;
     end;
   end;

   x := 0.0;
   y := -FCharHeight;

   whitespaceWidth := FCharHeight * FWhitespaceWidthEm;

   with ARci.GLStates do
    begin
      ActiveTextureEnabled[ttTexture2D] := true;
      Disable(stLighting);
      Enable(stBlend);
      SetBlendFunc(bfOne, bfOneMinusSrcAlpha);
    end;

   gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   //gl.TexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @aColor); // TODO
   gl.Color4fv(@aColor);

   i := 0;
   len := Length(aText);
   while i < len+1 do
   begin
     c := Integer(WideChar(aText[i]));
     Inc(i);

     if not FCharacters.Contains(c) then
       LoadCharacter(c);

     chara := FCharacters.Values[c] as TFreetypeCharacter;

     case c of
       0..12, 14..31: ; // ignore non-printable characters
       32: begin // whitespace
         x := x + whitespaceWidth;
       end;
       13: begin // CR
         x := 0;
         y := y - (FCharHeight * LineGap);
       end;
       else begin
         if chara.Initialized then begin
           RenderCharacter(chara, x, y);
           x := x + chara.advance;
         end;
         // TODO: render placeholder shape instead of invalid character
       end;
     end;
   end;
end;

end.
