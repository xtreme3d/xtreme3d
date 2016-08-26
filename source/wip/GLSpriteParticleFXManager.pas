unit GLSpriteParticleFXManager;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, GLGraphics, GLUtils, GLParticleFX,
  OpenGL1x;

type

  TGLSpriteParticleFXManager = class(TGLBaseSpritePFXManager)
  protected
    FMaterial: TGLLibMaterial;
    procedure PrepareImage(bmp32: TGLBitmap32; var texFormat: Integer); override;
  public
    property Material: TGLLibMaterial read FMaterial write FMaterial;
  end;

implementation

procedure TGLSpriteParticleFXManager.PrepareImage(bmp32: TGLBitmap32; var texFormat: Integer);
{
begin
  //if Assigned(FMaterial) then
    //bmp32.AssignFromTexture2D(FMaterial.Material.Texture.Handle);
  bmp32.
end;
}
var
   s : Integer;
   x, y, d, h2 : Integer;
   ih2, f, fy : Single;
   scanLine1, scanLine2 : PGLPixel32Array;
   TexMapSize: Integer;
begin
   TexMapSize := 5;
   s:=(1 shl TexMapSize);
   bmp32.Width:=s;
   bmp32.Height:=s;
   texFormat:=GL_LUMINANCE_ALPHA;

   h2:=s div 2;
   ih2:=1/h2;
   for y:=0 to h2-1 do begin
      fy:=Sqr((y+0.5-h2)*ih2);
      scanLine1:=bmp32.ScanLine[y];
      scanLine2:=bmp32.ScanLine[s-1-y];
      for x:=0 to h2-1 do begin
         f:=Sqr((x+0.5-h2)*ih2)+fy;
         if f<1 then begin
            d:=Trunc((1-Sqrt(f))*256);
            d:=d+(d shl 8)+(d shl 16)+(d shl 24);
         end else d:=0;
         PInteger(@scanLine1[x])^:=d;
         PInteger(@scanLine2[x])^:=d;
         PInteger(@scanLine1[s-1-x])^:=d;
         PInteger(@scanLine2[s-1-x])^:=d;
      end;
   end;
end;

end.
