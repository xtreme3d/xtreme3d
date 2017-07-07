unit GLFreetypeFont;

interface

uses
  SysUtils, Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils, GLCrossPlatform,
  ApplicationFileIO, GLBitmapFont, Freetype, SimpleDictionary;

type
  TFTTextEncoding = (teUTF8, teWindows);

  TGLFreetypeFont = class;

  TFreetypeCharacter = class
    public
      CodePoint: Integer;
      Initialized: Boolean;
      TextureId: GLuint;
      Glyph: FT_Glyph;
      Width: Single;
      Height: Single;
      texWidth: Single;
      texHeight: Single;
      AdvanceX: Single;
      AdvanceY: Single;
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
        FFace: FT_Face_ptr;
        FCharHeight: Integer;
        FCharacters: TSimpleObjectDictionary;
        FWinEnc: array[128..255] of Integer;
        FFontFileBuffer: array of Byte;
        FEncoding: TFTTextEncoding;
    public
        { Public Declarations }
        LineGap: Single;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure LoadCodePageMapping(const fileName: String);
        
        procedure RegisterUser(anObject: TGLBaseSceneObject); override;
	      procedure UnRegisterUser(anObject: TGLBaseSceneObject); override;

        function LoadFont(filename: String; cheight: Integer): Integer;
        function LoadCharacter(code: Integer): Integer;

        procedure RenderCharacter(chara: TFreetypeCharacter; posx, posy: Single);
        
        procedure RenderString(var rci : TRenderContextInfo;
                               const aString : String; alignment : TAlignment;
                               layout : TGLTextLayout; const color : TColorVector;
                               position : PVector = nil; reverseY : Boolean = False); override;

        property Encoding: TFTTextEncoding read FEncoding write FEncoding;
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
  Glyph := Nil;
  Width := 0;
  Height := 0;
  AdvanceX := 0;
end;

constructor TGLFreetypeFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCharHeight := 12;
  LineGap := 2.0;
  FCharacters := TSimpleObjectDictionary.Create();

  FEncoding := teUTF8;
  
  //FWinEnc := TSimpleDictionary.Create();

  FWinEnc[128] := 1026;
  FWinEnc[129] := 1027;
  FWinEnc[130] := 8218;
  FWinEnc[131] := 1107;
  FWinEnc[132] := 8222;
  
  FWinEnc[133] := 8230;
  FWinEnc[134] := 8224;
  FWinEnc[135] := 8225;
  FWinEnc[136] := 8364;
  FWinEnc[137] := 8240;
  FWinEnc[138] := 1033;

  FWinEnc[139] := 8249;
  FWinEnc[140] := 1034;
  FWinEnc[141] := 1036;
  FWinEnc[142] := 1035;
  FWinEnc[143] := 1039;
  FWinEnc[144] := 1106;

  FWinEnc[145] := 8216;
  FWinEnc[146] := 8217;
  FWinEnc[147] := 8220;
  FWinEnc[148] := 8221;
  FWinEnc[149] := 8226;
  FWinEnc[150] := 8211;

  FWinEnc[151] := 8212;
  FWinEnc[152] := 152;
  FWinEnc[153] := 8482;
  FWinEnc[154] := 1113;
  FWinEnc[155] := 8250;
  FWinEnc[156] := 1114;
  FWinEnc[157] := 1116;

  FWinEnc[158] := 1115;
  FWinEnc[159] := 1119;
  FWinEnc[160] := 160;
  FWinEnc[161] := 1038;
  FWinEnc[162] := 1118;
  FWinEnc[163] := 1032;

  FWinEnc[164] := 154;
  FWinEnc[165] := 1168;
  FWinEnc[166] := 166;
  FWinEnc[167] := 167;
  FWinEnc[168] := 1025;
  FWinEnc[169] := 169;

  FWinEnc[170] := 1028;
  FWinEnc[171] := 171;
  FWinEnc[172] := 172;
  FWinEnc[173] := 173;
  FWinEnc[174] := 174;
  FWinEnc[175] := 1031;

  FWinEnc[176] := 176;
  FWinEnc[177] := 177;
  FWinEnc[178] := 1030;
  FWinEnc[179] := 1110;
  FWinEnc[180] := 1169;
  FWinEnc[181] := 181;

  FWinEnc[182] := 182;
  FWinEnc[183] := 183;
  FWinEnc[184] := 1105;
  FWinEnc[185] := 8470;
  FWinEnc[186] := 1108;
  FWinEnc[187] := 187;

  FWinEnc[188] := 1112;
  FWinEnc[189] := 1029;
  FWinEnc[190] := 1109;
  FWinEnc[191] := 1111;
  FWinEnc[192] := 1040;
  FWinEnc[193] := 1041;

  FWinEnc[194] := 1042;
  FWinEnc[195] := 1043;
  FWinEnc[196] := 1044;
  FWinEnc[197] := 1045;
  FWinEnc[199] := 1046;
  FWinEnc[199] := 1047;

  FWinEnc[200] := 1048;
  FWinEnc[201] := 1049;
  FWinEnc[202] := 1050;
  FWinEnc[203] := 1051;
  FWinEnc[204] := 1052;
  FWinEnc[205] := 1053;

  FWinEnc[206] := 1054;
  FWinEnc[207] := 1055;
  FWinEnc[208] := 1056;
  FWinEnc[209] := 1057;
  FWinEnc[210] := 1058;
  FWinEnc[211] := 1059;

  FWinEnc[212] := 1060;
  FWinEnc[213] := 1061;
  FWinEnc[214] := 1062;
  FWinEnc[215] := 1063;
  FWinEnc[216] := 1064;
  FWinEnc[217] := 1065;

  FWinEnc[218] := 1066;
  FWinEnc[219] := 1067;
  FWinEnc[220] := 1068;
  FWinEnc[221] := 1069;
  FWinEnc[222] := 1070;
  FWinEnc[223] := 1071;

  FWinEnc[224] := 1072;
  FWinEnc[225] := 1073;
  FWinEnc[226] := 1074;
  FWinEnc[227] := 1075;
  FWinEnc[228] := 1076;
  FWinEnc[229] := 1077;

  FWinEnc[230] := 1078;
  FWinEnc[231] := 1079;
  FWinEnc[232] := 1080;
  FWinEnc[233] := 1081;
  FWinEnc[234] := 1082;
  FWinEnc[235] := 1083;

  FWinEnc[236] := 1084;
  FWinEnc[237] := 1085;
  FWinEnc[238] := 1086;
  FWinEnc[239] := 1087;
  FWinEnc[240] := 1088;
  FWinEnc[241] := 1089;

  FWinEnc[242] := 1090;
  FWinEnc[243] := 1091;
  FWinEnc[244] := 1092;
  FWinEnc[245] := 1093;
  FWinEnc[246] := 1094;
  FWinEnc[247] := 1095;

  FWinEnc[248] := 1096;
  FWinEnc[249] := 1097;
  FWinEnc[250] := 1098;
  FWinEnc[251] := 1099;
  FWinEnc[252] := 1100;
  FWinEnc[253] := 1101;

  FWinEnc[254] := 1102;
  FWinEnc[255] := 1103;

end;

destructor TGLFreetypeFont.Destroy;
begin
  FCharacters.Destroy;
  SetLength(FFontFileBuffer, 0);
  inherited Destroy;
end;

procedure TGLFreetypeFont.LoadCodePageMapping(const fileName : String);
var
  Strings: TStringList;
	fs: TStream;
  s: String;
  i: Integer;
begin
  if FileStreamExists(fileName) then begin
   	fs := CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
    Strings := TStringList.Create;
    Strings.LoadFromStream(fs);
    for i := 1 to Strings.Count do
    begin
      if i < 128 then
        FWinEnc[128+i] := StrToInt(Strings[i]);  
    end;
    Strings.Free;
  end;
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
var
   fs: TStream;
   n: Cardinal;
begin
   FCharHeight := cheight;

   if not IsFreetypeInitialized then
   begin
     if not InitFreetype('freetype.dll') then
     begin
       result := 0;
       Exit;
     end;
   end;

   n := 0;
   if FileStreamExists(filename) then begin
     fs := CreateFileStream(filename, fmOpenRead+fmShareDenyNone);
     try
       n := fs.Size;
       if n > 0 then
       begin
        SetLength(FFontFileBuffer, n);
       	fs.ReadBuffer(FFontFileBuffer[0], n);
       end;
     except
       On E: Exception do begin
         ShowMessage(E.Message);
         Halt(1);
       end;
     end;
 	   fs.Free;
   end;

   if n > 0 then
   begin
     //FT_Library_SetLcdFilter(ftLibrary, FT_LCD_FILTER_LIGHT);
     //FT_New_Face(ftLibrary, PChar(filename), 0, FFace); // load directly from file
     FT_New_Memory_Face(ftLibrary, @FFontFileBuffer[0], n, 0, FFace);
     FT_Set_Char_Size(FFace, FCharHeight shl 6, FCharHeight shl 6, 96, 96);
     result := 1;
   end
   else
     result := 0;
end;

function TGLFreetypeFont.LoadCharacter(code: Integer): Integer;
var
   charIndex: Integer;
   glyph: FT_Glyph;
   bitmapGlyph: FT_BitmapGlyph;
   bitmap: FT_Bitmap_ptr;
   charWidth, charHeight: Integer;
   tex: GLuint;
   img: Array of Byte;
   i, j: Integer;
   character: TFreetypeCharacter;
begin
   charIndex := FT_Get_Char_Index(FFace, code);
   FT_Load_Glyph(FFace, charIndex, FT_LOAD_DEFAULT);
   FT_Get_Glyph(FFace.glyph, glyph);

   FT_Render_Glyph(FFace.glyph, FT_RENDER_MODE_NORMAL); //FT_RENDER_MODE_LCD

   bitmap := @FFace.glyph.bitmap;

   charWidth := nextPowerOfTwo(bitmap.width);
   charHeight := nextPowerOfTwo(bitmap.rows);

   SetLength(img, 2 * charWidth * charHeight);

   for j := 0 to charHeight-1 do
   begin
     for i := 0 to charWidth-1 do
     begin
       img[2 * (i + j * charWidth)] := 255;

       try
       if (i >= bitmap.width) or (j >= bitmap.rows) then
         img[2 * (i + j * charWidth) + 1] := 0
       else
         img[2 * (i + j * charWidth) + 1] :=
         byte(PByteArray(bitmap.buffer)[i + bitmap.width * j]);
       except
         On E: Exception do begin
           ShowMessage(E.Message);
           Halt(1);
         end;
       end;
     end;
   end;

   glEnable(GL_TEXTURE_2D);

   glGenTextures(1, @tex);
   glBindTexture(GL_TEXTURE_2D, tex);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

   glTexImage2D(GL_TEXTURE_2D,
      0, GL_RGBA, charWidth, charHeight,
      0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @img[0]);

   character := TFreetypeCharacter.Create();
   character.CodePoint := code;
   character.Initialized := True;
   character.TextureId := tex;
   character.Glyph := glyph;
   character.Width := bitmap.width;
   character.Height := bitmap.rows;
   character.texWidth := charWidth;
   character.texHeight := charHeight;
   character.AdvanceX := FFace.glyph.advance.x shr 6;
   character.AdvanceY := FFace.glyph.advance.y shr 6;
   character.top := -(character.Height - FFace.glyph.bitmap_top);
   character.left := FFace.glyph.bitmap_left;

   FCharacters.Values[code] := character;

   result := charIndex;
end;

procedure TGLFreetypeFont.RenderCharacter(chara: TFreetypeCharacter; posx, posy: Single);
var
  x, y: Single;
begin
   x := 0.5 / chara.texWidth + chara.width / chara.texWidth;
   y := 0.5 / chara.texHeight + chara.height / chara.texHeight;

   glDisable(GL_LIGHTING);
   glEnable(GL_TEXTURE_2D);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glBindTexture(GL_TEXTURE_2D, chara.textureId);
   glBegin(GL_QUADS);
   glTexCoord2f(0, 0); glVertex2f(posx + chara.left, posy + chara.top + chara.height);
   glTexCoord2f(0, y); glVertex2f(posx + chara.left, posy + chara.top);
   glTexCoord2f(x, y); glVertex2f(posx + chara.left + chara.width, posy + chara.top);
   glTexCoord2f(x, 0); glVertex2f(posx + chara.left + chara.width, posy + chara.top + chara.height);
   glEnd();
end;

procedure TGLFreetypeFont.RenderString(var rci : TRenderContextInfo;
            const aString : String; alignment : TAlignment;
            layout : TGLTextLayout; const color : TColorVector; position : PVector = nil;
            reverseY : Boolean = False);
var
  i: Integer;
  c: Integer;
  len: Integer;
  x, y: Single;
  chara: TFreetypeCharacter;
  decoding: Boolean;
begin
   if FCharacters.Count < 128 then
   begin
     for i := 0 to 128-1 do
     begin
       LoadCharacter(i);
     end;
   end;

   x := 0.0;
   y := -FCharHeight;

   glColor4fv(@color[0]);

   i := 0;
   len := Length(aString);
   while i < len+1 do
   begin
     if FEncoding = teUTF8 then
     begin
       c := utf8DecodeNext(aString, i);
     end
     else
     begin
       c := Integer(aString[i]);
       if c > 127 then
         c := FWinEnc[c];
       i := i + 1;
     end;

     if not FCharacters.Contains(c) then
       LoadCharacter(c);
       
     chara := FCharacters.Values[c] as TFreetypeCharacter;

     case c of
       0..12, 14..31: ; // ignore non-printable characters
       13: begin
         x := 0;
         y := y - (FCharHeight * LineGap);
       end;
     else begin
       RenderCharacter(chara, x, y);
       x := x + chara.AdvanceX;
       end;
     end;
   end;
   
end;

end.
