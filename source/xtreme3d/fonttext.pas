function BmpfontCreate(w,h,hspace,vspace,intx,inty,chstart,chend: real): real; cdecl;
var
  GLBitmapFont1: TGLBitmapFont;
begin
  GLBitmapFont1:=TGLBitmapFont.Create(scene);
  GLBitmapFont1.CharWidth:=Trunc(w);
  GLBitmapFont1.CharHeight:=Trunc(h);
  GLBitmapFont1.HSpace:=Trunc(hspace);
  GLBitmapFont1.VSpace:=Trunc(vspace);
  GLBitmapFont1.GlyphsIntervalX:=Trunc(intx);
  GLBitmapFont1.GlyphsIntervalY:=Trunc(inty);
  with GLBitmapFont1.Ranges.Add do
  begin
    StartASCII:=Char(Trunc(chstart));
    StopASCII:=Char(Trunc(chend));
  end;
  result:=ObjToReal(GLBitmapFont1);
end;

function BmpfontLoad(font: real; mtrl: PAnsiChar): real; cdecl;
var
  GLBitmapFont1: TGLBitmapFont;
  begin
  GLBitmapFont1:=TGLBitmapFont(RealToPtr(font));
  GLBitmapFont1.Glyphs.Bitmap.LoadFromFile(StrConv(mtrl));
  result:=1;
end;

function WindowsBitmapfontCreate(nm: PAnsiChar; size,chstart,chend: real): real; cdecl;
var
  WindowsBitmapFont1: TGLWindowsBitmapFont;
begin
  WindowsBitmapFont1:=TGLWindowsBitmapFont.Create(scene);
  WindowsBitmapFont1.Ranges[0].StartASCII := Char(Trunc(chstart));
  WindowsBitmapFont1.Ranges[0].StopASCII := Char(Trunc(chend));
  WindowsBitmapFont1.Font.Height := Trunc(size);
  WindowsBitmapFont1.Font.Name := StrConv(nm);
  result:=ObjToReal(WindowsBitmapFont1);
end;

function HUDTextCreate(font: real; txt: PAnsiChar; parent: real): real; cdecl;
var
  GLHUDText1: TGLHUDText2;
  GLFont: TGLFont;
begin
  if not (parent=0) then
    GLHUDText1:=TGLHUDText2.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLHUDText1:=TGLHUDText2.CreateAsChild(scene.Objects);
  GLFont := TGLFont(RealToPtr(font));
  GLHUDText1.Font := GLFont;
  GLHUDText1.Text := StrConv(txt);
  result:=ObjToReal(GLHUDText1);
end;

function HUDTextSetRotation(text,angle: real): real; cdecl;
var
  GLHUDText1: TGLHUDText2;
begin
  GLHUDText1:=TGLHUDText2(RealToPtr(text));
  GLHUDText1.Rotation:=angle;
  result:=1;
end;

function HUDTextSetFont(text,font: real): real; cdecl;
var
  GLHUDText1: TGLHUDText2;
begin
  GLHUDText1:=TGLHUDText2(RealToPtr(text));
  GLHUDText1.Font:=TGLFont(RealToPtr(font));
  result:=1;
end;

function HUDTextSetColor(text,color,alph: real): real; cdecl;
var
  GLHUDText1: TGLHUDText2;
begin
  GLHUDText1:=TGLHUDText2(RealToPtr(text));
  GLHUDText1.ModulateColor.AsWinColor:=Trunc(color);
  GLHUDText1.ModulateColor.Alpha:=alph;
  result:=1;
end;

function HUDTextSetText(text: real; txt: PAnsiChar): real; cdecl;
var
  GLHUDText1: TGLHUDText2;
begin
  GLHUDText1 := TGLHUDText2(RealToPtr(text));
  GLHUDText1.Text := StrConv(txt);
  result:=1;
end;

function FlatTextCreate(font: real; txt: PAnsiChar; parent: real): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  if not (parent=0) then
    GLFlatText1:=TGLFlatText.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLFlatText1:=TGLFlatText.CreateAsChild(scene.Objects);
  GLFlatText1.BitmapFont:=TGLCustomBitmapFont(RealToPtr(font));
  GLFlatText1.Text:=StrConv(txt);
  result:=ObjToReal(GLFlatText1);
end;

function FlatTextSetFont(text,font: real): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  GLFlatText1:=TGLFlatText(RealToPtr(text));
  GLFlatText1.BitmapFont:=TGLCustomBitmapFont(RealToPtr(font));
  result:=1;
end;

function FlatTextSetColor(text,color,alph: real): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  GLFlatText1:=TGLFlatText(RealToPtr(text));
  GLFlatText1.ModulateColor.AsWinColor:=Trunc(color);
  GLFlatText1.ModulateColor.Alpha:=alph;
  result:=1;
end;

function FlatTextSetText(text: real; txt: PAnsiChar): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  GLFlatText1:=TGLFlatText(RealToPtr(text));
  GLFlatText1.Text:=StrConv(txt);
  result:=1;
end;

function SpaceTextCreate(font: real; txt: PAnsiChar; extr,parent: real): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  if not (parent=0) then
    GLSpaceText1:=TGLSpaceText.CreateAsChild(TGLBaseSceneObject(RealToPtr(parent)))
  else
    GLSpaceText1:=TGLSpaceText.CreateAsChild(scene.Objects);
  GLSpaceText1.Font.Name:=TGLWindowsBitmapFont(RealToPtr(font)).Font.Name;
  GLSpaceText1.Font.Height:=TGLWindowsBitmapFont(RealToPtr(font)).Font.Height;
  GLSpaceText1.Text:=StrConv(txt);
  GLSpaceText1.Extrusion:=extr;
  result:=ObjToReal(GLSpaceText1);
end;

function SpaceTextSetExtrusion(text: real; extr: real): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  GLSpaceText1:=TGLSpaceText(RealToPtr(text));
  GLSpaceText1.Extrusion:=extr;
  result:=1;
end;

function SpaceTextSetFont(text,font: real): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  GLSpaceText1:=TGLSpaceText(RealToPtr(text));
  GLSpaceText1.Font.Name:=TGLWindowsBitmapFont(RealToPtr(font)).Font.Name;
  GLSpaceText1.Font.Height:=TGLWindowsBitmapFont(RealToPtr(font)).Font.Height;
  result:=1;
end;

function SpaceTextSetText(text: real; txt: PAnsiChar): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  GLSpaceText1:=TGLSpaceText(RealToPtr(text));
  GLSpaceText1.Text:=StrConv(txt);
  result:=1;
end;

function TTFontCreate(filename: PAnsiChar; height: real): real; cdecl;
var
  ftfont: TGLFreetypeFont;
begin
  ftfont := TGLFreetypeFont.Create(scene);
  ftfont.LoadFont(StrConv(filename), Trunc(height));
  result := ObjToReal(ftfont);
end;

function TTFontSetLineGap(font, gap: real): real; cdecl;
var
  ftfont: TGLFreetypeFont;
begin
  ftfont := TGLFreetypeFont(RealToPtr(font));
  ftfont.LineGap := gap;
  result := 1.0
end;

