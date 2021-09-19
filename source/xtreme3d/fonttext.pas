function BmpfontCreate(w,h,hspace,vspace,intx,inty,chstart,chend: real): real; cdecl;
var
  GLBitmapFont1: TGLBitmapFont;
begin
  GLBitmapFont1:=TGLBitmapFont.Create(scene);
  GLBitmapFont1.CharWidth:=trunc64(w);
  GLBitmapFont1.CharHeight:=trunc64(h);
  GLBitmapFont1.HSpace:=trunc64(hspace);
  GLBitmapFont1.VSpace:=trunc64(vspace);
  GLBitmapFont1.GlyphsIntervalX:=trunc64(intx);
  GLBitmapFont1.GlyphsIntervalY:=trunc64(inty);
  with GLBitmapFont1.Ranges.Add do
  begin
    StartASCII:=Char(trunc64(chstart));
    StopASCII:=Char(trunc64(chend));
  end;
  result:=Integer(GLBitmapFont1);
end;

function BmpfontLoad(font: real; mtrl: pchar): real; cdecl;
var
  GLBitmapFont1: TGLBitmapFont;
  begin
  GLBitmapFont1:=TGLBitmapFont(trunc64(font));
  GLBitmapFont1.Glyphs.Bitmap.LoadFromFile(mtrl);
  result:=1;
end;

function WindowsBitmapfontCreate(nm: pchar; size,chstart,chend: real): real; cdecl;
var
  WindowsBitmapFont1: TGLWindowsBitmapFont;
begin
  WindowsBitmapFont1:=TGLWindowsBitmapFont.Create(scene);
  WindowsBitmapFont1.Ranges[0].StartASCII:=Char(trunc64(chstart));
  WindowsBitmapFont1.Ranges[0].StopASCII:=Char(trunc64(chend));
  WindowsBitmapFont1.Font.Height:=trunc64(size);
  WindowsBitmapFont1.Font.Name:=nm;
  result:=Integer(WindowsBitmapFont1);
end;

function HUDTextCreate(font: real; txt: pchar; parent: real): real; cdecl;
var
  GLHUDText1: TGLHUDText;
begin
  if not (parent=0) then
    GLHUDText1:=TGLHUDText.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLHUDText1:=TGLHUDText.CreateAsChild(scene.Objects);
  GLHUDText1.Font:=TGLFont(trunc64(font));
  GLHUDText1.Text:=txt;
  result:=Integer(GLHUDText1);
end;

function HUDTextSetRotation(text,angle: real): real; cdecl;
var
  GLHUDText1: TGLHUDText;
begin
  GLHUDText1:=TGLHUDText(trunc64(text));
  GLHUDText1.Rotation:=angle;
  result:=1;
end;

function HUDTextSetFont(text,font: real): real; cdecl;
var
  GLHUDText1: TGLHUDText;
begin
  GLHUDText1:=TGLHUDText(trunc64(text));
  GLHUDText1.Font:=TGLFont(trunc64(font));
  result:=1;
end;

function HUDTextSetColor(text,color,alph: real): real; cdecl;
var
  GLHUDText1: TGLHUDText;
begin
  GLHUDText1:=TGLHUDText(trunc64(text));
  GLHUDText1.ModulateColor.AsWinColor:=trunc64(color);
  GLHUDText1.ModulateColor.Alpha:=alph;
  result:=1;
end;

function HUDTextSetText(text: real; txt: pchar): real; cdecl;
var
  GLHUDText1: TGLHUDText;
begin
  GLHUDText1:=TGLHUDText(trunc64(text));
  GLHUDText1.Text:=txt;
  result:=1;
end;

function FlatTextCreate(font: real; txt: pchar; parent: real): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  if not (parent=0) then
    GLFlatText1:=TGLFlatText.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLFlatText1:=TGLFlatText.CreateAsChild(scene.Objects);
  GLFlatText1.BitmapFont:=TGLCustomBitmapFont(trunc64(font));
  GLFlatText1.Text:=txt;
  result:=Integer(GLFlatText1);
end;

function FlatTextSetFont(text,font: real): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  GLFlatText1:=TGLFlatText(trunc64(text));
  GLFlatText1.BitmapFont:=TGLCustomBitmapFont(trunc64(font));
  result:=1;
end;

function FlatTextSetColor(text,color,alph: real): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  GLFlatText1:=TGLFlatText(trunc64(text));
  GLFlatText1.ModulateColor.AsWinColor:=trunc64(color);
  GLFlatText1.ModulateColor.Alpha:=alph;
  result:=1;
end;

function FlatTextSetText(text: real; txt: pchar): real; cdecl;
var
  GLFlatText1: TGLFlatText;
begin
  GLFlatText1:=TGLFlatText(trunc64(text));
  GLFlatText1.Text:=txt;
  result:=1;
end;

function SpaceTextCreate(font: real; txt: pchar; extr,parent: real): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  if not (parent=0) then
    GLSpaceText1:=TGLSpaceText.CreateAsChild(TGLBaseSceneObject(trunc64(parent)))
  else
    GLSpaceText1:=TGLSpaceText.CreateAsChild(scene.Objects);
  GLSpaceText1.Font.Name:=TGLWindowsBitmapFont(trunc64(font)).Font.Name;
  GLSpaceText1.Font.Height:=TGLWindowsBitmapFont(trunc64(font)).Font.Height;
  GLSpaceText1.Text:=txt;
  GLSpaceText1.Extrusion:=extr;
  result:=Integer(GLSpaceText1);
end;

function SpaceTextSetExtrusion(text: real; extr: real): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  GLSpaceText1:=TGLSpaceText(trunc64(text));
  GLSpaceText1.Extrusion:=extr;
  result:=1;
end;

function SpaceTextSetFont(text,font: real): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  GLSpaceText1:=TGLSpaceText(trunc64(text));
  GLSpaceText1.Font.Name:=TGLWindowsBitmapFont(trunc64(font)).Font.Name;
  GLSpaceText1.Font.Height:=TGLWindowsBitmapFont(trunc64(font)).Font.Height;
  result:=1;
end;

function SpaceTextSetText(text: real; txt: pchar): real; cdecl;
var
  GLSpaceText1: TGLSpaceText;
begin
  GLSpaceText1:=TGLSpaceText(trunc64(text));
  GLSpaceText1.Text:=txt;
  result:=1;
end;

function TTFontCreate(filename: pchar; height: real): real; cdecl;
var
  ftfont: TGLFreetypeFont;
begin
  ftfont := TGLFreetypeFont.Create(scene);
  ftfont.LoadFont(String(filename), trunc64(height));
  result := Integer(ftfont);
end;

function TTFontSetLineGap(font, gap: real): real; cdecl;
var
  ftfont: TGLFreetypeFont;
begin
  ftfont := TGLFreetypeFont(trunc64(font));
  ftfont.LineGap := gap;
  result := 1.0
end;

function TTFontSetEncoding(font, te: real): real; cdecl;
var
  ftfont: TGLFreetypeFont;
begin
  ftfont := TGLFreetypeFont(trunc64(font));
  if te = 0.0 then ftfont.Encoding := teUTF8
  else if te = 1.0 then ftfont.Encoding := teWindows;
  result := 1.0;
end;

function TTFontLoadCodePage(font: real; filename: pchar): real; cdecl;
var
  ftfont: TGLFreetypeFont;
begin
  ftfont := TGLFreetypeFont(trunc64(font));
  ftfont.LoadCodePageMapping(String(filename));
  result := 1.0;
end;
