unit PNG;

interface

{$i GLScene.inc}

uses Classes, Dialogs, SysUtils, Graphics, GLCrossPlatform, GLGraphics, pngimage;

type

	TPNGImage = class (TGLBitmap)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	    constructor Create; override;
        destructor Destroy; override;

        procedure LoadFromStream(stream: TStream); override;
        procedure SaveToStream(stream : TStream); override;
	end;

implementation

constructor TPNGImage.Create;
begin
  inherited Create;
end;

destructor TPNGImage.Destroy;
begin
	inherited Destroy;
end;

procedure TPNGImage.LoadFromStream(stream: TStream);
var
  png: TPNGObject;
  i, j: integer;
  p: PGLPixel32Array;
  pb: PByteArray;
  col: TColor;
begin
  png := TPNGObject.Create;
  png.LoadFromStream(stream);

  Width := png.Width;
  Height := png.Height;

  if png.TransparencyMode = ptmPartial then
  begin
    PixelFormat := pf32bit;
    for j:=0 to png.Height-1 do
    begin
      p := ScanLine[j];
      for i:=0 to png.Width-1 do
      begin
        col := png.Pixels[i, j];
        p[i].b := col;
        p[i].g := col shr 8;
        p[i].r := col shr 16;
        p[i].a := png.AlphaScanline[j][i];
      end;
    end;
  end
  else
  begin
    Assign(png);
  end;

  png.Free;
end;

procedure TPNGImage.SaveToStream(stream : TStream);
begin
end;

initialization

   TGLPicture.RegisterFileFormat('png', 'Portable Network Graphics', TPNGImage);

finalization

   TGLPicture.UnregisterGraphicClass(TPNGImage);

end.
