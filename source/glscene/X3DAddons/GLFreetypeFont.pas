unit GLFreetypeFont;

interface

uses
  SysUtils, Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils, GLCrossPlatform,
  ApplicationFileIO, GLBitmapFont, Freetype, SimpleDictionary;

type
  TFTTextEncoding = (teUTF8, te1251);

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
        FWin1251: array[128..255] of Integer;
        FFontFileBuffer: array of Byte;
        FEncoding: TFTTextEncoding;
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
  
  //FWin1251 := TSimpleDictionary.Create();

  FWin1251[128] := 1026;
  FWin1251[129] := 1027;
  FWin1251[130] := 8218;
  FWin1251[131] := 1107;
  FWin1251[132] := 8222;
  
  FWin1251[133] := 8230;
  FWin1251[134] := 8224;
  FWin1251[135] := 8225;
  FWin1251[136] := 8364;
  FWin1251[137] := 8240;
  FWin1251[138] := 1033;

  FWin1251[139] := 8249;
  FWin1251[140] := 1034;
  FWin1251[141] := 1036;
  FWin1251[142] := 1035;
  FWin1251[143] := 1039;
  FWin1251[144] := 1106;

  FWin1251[145] := 8216;
  FWin1251[146] := 8217;
  FWin1251[147] := 8220;
  FWin1251[148] := 8221;
  FWin1251[149] := 8226;
  FWin1251[150] := 8211;

  FWin1251[151] := 8212;
  FWin1251[152] := 152;
  FWin1251[153] := 8482;
  FWin1251[154] := 1113;
  FWin1251[155] := 8250;
  FWin1251[156] := 1114;
  FWin1251[157] := 1116;

  FWin1251[158] := 1115;
  FWin1251[159] := 1119;
  FWin1251[160] := 160;
  FWin1251[161] := 1038;
  FWin1251[162] := 1118;
  FWin1251[163] := 1032;

  FWin1251[164] := 154;
  FWin1251[165] := 1168;
  FWin1251[166] := 166;
  FWin1251[167] := 167;
  FWin1251[168] := 1025;
  FWin1251[169] := 169;

  FWin1251[170] := 1028;
  FWin1251[171] := 171;
  FWin1251[172] := 172;
  FWin1251[173] := 173;
  FWin1251[174] := 174;
  FWin1251[175] := 1031;

  FWin1251[176] := 176;
  FWin1251[177] := 177;
  FWin1251[178] := 1030;
  FWin1251[179] := 1110;
  FWin1251[180] := 1169;
  FWin1251[181] := 181;

  FWin1251[182] := 182;
  FWin1251[183] := 183;
  FWin1251[184] := 1105;
  FWin1251[185] := 8470;
  FWin1251[186] := 1108;
  FWin1251[187] := 187;

  FWin1251[188] := 1112;
  FWin1251[189] := 1029;
  FWin1251[190] := 1109;
  FWin1251[191] := 1111;
  FWin1251[192] := 1040;
  FWin1251[193] := 1041;

  FWin1251[194] := 1042;
  FWin1251[195] := 1043;
  FWin1251[196] := 1044;
  FWin1251[197] := 1045;
  FWin1251[199] := 1046;
  FWin1251[199] := 1047;

  FWin1251[200] := 1048;
  FWin1251[201] := 1049;
  FWin1251[202] := 1050;
  FWin1251[203] := 1051;
  FWin1251[204] := 1052;
  FWin1251[205] := 1053;

  FWin1251[206] := 1054;
  FWin1251[207] := 1055;
  FWin1251[208] := 1056;
  FWin1251[209] := 1057;
  FWin1251[210] := 1058;
  FWin1251[211] := 1059;

  FWin1251[212] := 1060;
  FWin1251[213] := 1061;
  FWin1251[214] := 1062;
  FWin1251[215] := 1063;
  FWin1251[216] := 1064;
  FWin1251[217] := 1065;

  FWin1251[218] := 1066;
  FWin1251[219] := 1067;
  FWin1251[220] := 1068;
  FWin1251[221] := 1069;
  FWin1251[222] := 1070;
  FWin1251[223] := 1071;

  FWin1251[224] := 1072;
  FWin1251[225] := 1073;
  FWin1251[226] := 1074;
  FWin1251[227] := 1075;
  FWin1251[228] := 1076;
  FWin1251[229] := 1077;

  FWin1251[230] := 1078;
  FWin1251[231] := 1079;
  FWin1251[232] := 1080;
  FWin1251[233] := 1081;
  FWin1251[234] := 1082;
  FWin1251[235] := 1083;

  FWin1251[236] := 1084;
  FWin1251[237] := 1085;
  FWin1251[238] := 1086;
  FWin1251[239] := 1087;
  FWin1251[240] := 1088;
  FWin1251[241] := 1089;

  FWin1251[242] := 1090;
  FWin1251[243] := 1091;
  FWin1251[244] := 1092;
  FWin1251[245] := 1093;
  FWin1251[246] := 1094;
  FWin1251[247] := 1095;

  FWin1251[248] := 1096;
  FWin1251[249] := 1097;
  FWin1251[250] := 1098;
  FWin1251[251] := 1099;
  FWin1251[252] := 1100;
  FWin1251[253] := 1101;

  FWin1251[254] := 1102;
  FWin1251[255] := 1103;

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
         c := FWin1251[c];
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
