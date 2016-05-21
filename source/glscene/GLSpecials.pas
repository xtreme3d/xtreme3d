unit GLSpecials;

// GLSpecials  - Contains structures and functions for special
//               effects for GLScene
// Version     - 0.0.3
// Last Change - 13. August 1998
// for more information see help file

interface

uses Windows, Classes, VectorGeometry, GLScene, OpenGL1x, GLTexture;

type // lens flares, used in TLightSource
     PFlare = ^TFlare;
     TFlare = record
                FlareType : Integer; // flare texture index, 0..5
                Scale     : Single;
                Location  : Single;  // postion on axis
                Color     : TAffineVector;
                ColorAddr : Pointer;
              end;

     TShineTex = array[0..9] of Integer;
     TFlareTex = array[0..5] of Integer;

     TLensFlares = class(TObject)
     private
       FList: TList;
       FShineTex : TShineTex;
       FFlareTex : TFlareTex;
       function Get(Index: Integer): TFlare;
       function GetCount: Integer;
       function GetFlareTex(Index: Integer): Integer;
       function GetShineTex(Index: Integer): Integer;
     public
       FlareTic : Integer;
       constructor Create;
       destructor Destroy; override;
       function AddFlare(FlareType: Integer; Location, Scale: Single; Color: TVector; ColorScale: Single): Integer;
       property Count: Integer read GetCount;
       property Flare[Index: Integer]: TFlare read Get; default;
       property FlareTexture[Index: Integer]: Integer read GetFlareTex;
       property ShineTexture[Index: Integer]: Integer read GetShineTex;
     end;


    const
      Distance = 16;
      MAP = 256;
      MH = 256;
      Comp = 64;

    type
     PCOLOUR = ^COLOUR;
     COLOUR = record
       r, g, b: TGLubyte;
     end;
     // landscape, used in where ?????????????????????????
     TLandScape = class(TObject)
     private
       FFullView: Boolean;
       FMinFilter   : TMinFilter;
       FMagFilter   : TMagFilter;
       FMinFilterValue: TGLenum;
       FMagFilterValue: TGLenum;

       FLandTexture: Integer;
       FOcenTexture: Integer;
       FObject: array[0..66048] of byte;
       y: array[0..66048] of integer;
       c: array[0..66048] of COLOUR;

       procedure DrawFLAT(X, Z: Integer; X1, X2, Z1, Z2: Integer);
       procedure DrawFLAT2(X, Z: Integer; X1, X2, Z1, Z2: Integer);
       procedure Create_Fractal;
       procedure SetMagFilter(Value: TMagFilter);
       procedure SetMinFilter(Value: TMinFilter);
       procedure GetTextureFilter;
       //function GetHandle: TObjectHandle;
       //procedure BuildList
     public
       constructor Create;
       destructor Destroy; override;

       function Get_Height(SX, SZ: TGLfloat): TGLfloat;
       procedure RenderLandScape(Camera: TCamera);
       property LandTexture: Integer read FLandTexture;
       property OcenTexture: Integer read FOcenTexture;
       property FullView: Boolean read FFullView write FFullView;
       property MinFilter: TMinFilter read FMinFilter write SetMinFilter;
       property MagFilter: TMagFilter read FMagFilter write SetMagFilter;
     end;

procedure InitLensFlares;
procedure InitLandScape;

var
  LensFlares : TLensFlares;
  LandScape: TLandScape;

//------------------------------------------------------------------------------

implementation

uses Graphics, SysUtils, GLMisc;

{$R Special.res}

//------------------------------------------------------------------------------

constructor TLensFlares.Create;

begin
  inherited Create;
  FList := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TLensFlares.Destroy;

var I : Integer;

begin
  glDeleteTextures(10,@FShineTex);
  glDeleteTextures(6,@FFlareTex);
  for I:=0 to Count-1 do
    Dispose(PFlare(FList[I]));
  FList.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TLensFlares.Get(Index: Integer): TFlare;

begin
  Result:=PFlare(FList[Index])^;
end;

//------------------------------------------------------------------------------

function TLensFlares.GetCount: Integer;

begin
  Result:=FList.Count;
end;

//------------------------------------------------------------------------------

function TLensFlares.GetFlareTex(Index: Integer): Integer;

begin
  Result:=FFlareTex[Index];
end;

//------------------------------------------------------------------------------

function TLensFlares.GetShineTex(Index: Integer): Integer;

begin
  Result:=FShineTex[Index];
end;

//------------------------------------------------------------------------------

function TLensFlares.AddFlare(FlareType: Integer; Location, Scale: Single; Color: TVector; ColorScale: Single): Integer;

var AFlare : PFlare;

begin
  New(AFlare);
  AFlare.FlareType:=FlareType;
  AFlare.Location:=Location;
  AFlare.Scale:=Scale;
  AFlare.Color:=MakeAffineVector([Color[0],Color[1],Color[2]]);
  AFlare.ColorAddr:=@AFlare.Color;
  VectorScale(AFlare.Color,ColorScale);
  Result:=FList.Add(AFlare);
end;

//------------------------------------------------------------------------------
//The stream contains a bitmap file
function SetupTextureFromStream(Stream: TStream; MinFilter, MaxFilter: TGLuint): Integer;
type
  PPixelArray  = ^TByteArray;
var
  Temp: Byte;
  Bmp: TBitmap;
  Buffer: PPixelArray;
  BMInfo      : TBitmapInfo;
  I,ImageSize : Integer;
  MemDC       : HDC;
begin
  glGenTextures(1,@Result);
  SetGLCurrentTexture(Result);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
  glPixelStorei(GL_UNPACK_ALIGNMENT,4);
  glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS,0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS,0);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,MinFilter);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,MaxFilter);

  Bmp := TBitmap.Create;
  Stream.Seek(0, soFromBeginning);
  Bmp.LoadFromStream(Stream);

  with BMinfo.bmiHeader do
  begin
    // create description of the required image format
    FillChar(BMInfo, sizeof(BMInfo),0);
    biSize:=sizeof(TBitmapInfoHeader);
    biBitCount:=24;
    biWidth:=RoundUpToPowerOf2(Bmp.Width);
    biHeight:=RoundUpToPowerOf2(Bmp.Height);
    ImageSize:=biWidth*biHeight;
    biPlanes:=1;
    biCompression:=BI_RGB;
    MemDC:=CreateCompatibleDC(0);
    Getmem(Buffer, ImageSize*3);
    // get the actual bits of the image
    GetDIBits(MemDC, Bmp.Handle, 0, biHeight, Buffer, BMInfo, DIB_RGB_COLORS);

    {$IFOPT R+} {$DEFINE RangeCheck} {$R-} {$ENDIF}
    for I:=0 TO ImageSize-1 do //swap blue with red to go from bgr to rgb
    begin
      Temp := Buffer^[I*3];
      Buffer^[I*3] := Buffer^[I*3+2];
      Buffer^[I*3+2] := Temp;
    end;
    // restore range check, if previously activated
    {$IFDEF RangeCheck} {$UNDEF RangeCheck} {$R+} {$ENDIF}
    if (MinFilter = GL_NEAREST) or (MinFilter = GL_LINEAR) then
      glTexImage2d(GL_TEXTURE_2D,0, 3, biWidth,biHeight,0, GL_RGB, GL_UNSIGNED_BYTE, Buffer)
    else
      gluBuild2DMipmaps(GL_TEXTURE_2D, 3, biWidth, biHeight, GL_RGB, GL_UNSIGNED_BYTE, Buffer);

    FreeMem(Buffer);
    DeleteDC(MemDC);
  end;
  Bmp.free;
end;

//------------------------------------------------------------------------------
procedure InitLensFlares;
var
   i : Integer;
   Stream: TMemoryStream;
   Bmp: TBitmap;

   procedure StreamSpecialBitmap(const resName : String);
   var
      bmp24 : TBitmap;
   begin
      Bmp.LoadFromResourceName(HInstance, Format('GLS_%s%d', [resName, i]));
      bmp24:=TBitmap.Create;
      bmp24.PixelFormat:=pf24Bit;
      bmp24.Width:=bmp.Width; bmp24.Height:=bmp.Height;
      bmp24.Canvas.Draw(0, 0, bmp);
      Stream.Clear;
      Bmp24.SaveToStream(Stream);
      bmp24.free;
      Stream.Seek(0, soFromBeginning);
  end;

begin
   LensFlares:=TLensFlares.Create;
   with LensFlares do begin
      // textures
      Stream := TMemoryStream.Create;
      Bmp := TBitmap.Create;
      try
         for I:=0 to 9 do begin
            StreamSpecialBitmap('SHINE');
            FShineTex[I] := SetupTextureFromStream(Stream ,GL_NEAREST, GL_NEAREST);
         end;
         for I:=0 to 5 do begin
            StreamSpecialBitmap('FLARE');
            FFlareTex[I] := SetupTextureFromStream(Stream ,GL_NEAREST, GL_NEAREST);
         end;
      finally
         Bmp.free;
         Stream.free;
      end;
      // Shines
      AddFlare(-1,1,0.2,clrBlue,1);
      AddFlare(-1,1,0.1,clrGreen,1);
      AddFlare(-1,1,0.15,clrRed,1);
      // Flares
      AddFlare(2,1.3,0.04,clrRed,0.6);
      AddFlare(3,1,0.1,clrRed,0.4);
      AddFlare(1,0.5,0.2,clrRed,0.3);
      AddFlare(3,0.2,0.05,clrRed,0.3);
      AddFlare(0,0,0.04,clrRed,0.3);
      AddFlare(5,-0.25,0.07,clrRed,0.5);
      AddFlare(5,-0.4,0.02,clrRed,0.6);
      AddFlare(5,-0.6,0.04,clrRed,0.4);
      AddFlare(5,-1,0.03,clrRed,0.2);
   end;
end;

//------------------------------------------------------------------------------
procedure InitLandScape;
var
  Bmp: TBitmap;
  Stream: TMemoryStream;
begin
  LandScape := TLandScape.Create;
  //lands textures
  Stream := TMemoryStream.Create;
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(HInstance, 'GLS_LAND1');
  Stream.Clear;
  Bmp.SaveToStream(Stream);
  Stream.Seek(0, soFromBeginning);
  Bmp.free;
  LandScape.FLandTexture := SetupTextureFromStream(Stream ,GL_NEAREST, GL_LINEAR);
  //Ocen texture
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(HInstance, 'GLS_OCEN');
  Stream.Clear;
  Bmp.SaveToStream(Stream);
  Stream.Seek(0, soFromBeginning);
  Bmp.free;
  LandScape.FOcenTexture := SetupTextureFromStream(Stream ,GL_NEAREST, GL_LINEAR);
  Stream.free;
end;


//------------------------------------------------------------------------------

constructor TLandScape.Create;

begin
  inherited Create;
  FLandTexture := 0;
  FOcenTexture := 0;
  FFullView := False;
  Create_Fractal;
  FMinFilter := miLinear;
  FMagFilter := maLinear;
  GetTextureFilter;
end;


//------------------------------------------------------------------------------

destructor TLandScape.Destroy;
begin
  glDeleteTextures(1, @FLandTexture);
  glDeleteTextures(1, @FOcenTexture);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function NodeIndex(x, y: Integer): Integer;
begin
  Result := (y shl 8) + x;
end;

//------------------------------------------------------------------------------

procedure TLandScape.Create_Fractal;
var
  bsize, csize: Integer;
  x, z, i: Integer;
  g1: Integer;
  r: Integer;  //The maximum random value.
begin
  r := 256;

  //Make the matrix flat.
  for X:=0 to MAP do
    for Z:=0 to MAP do
      y[NodeIndex(x, z)] := 0;

  bsize := MAP;
  Randomize;
  for i:=0 to 7 do
  begin
    X := 0;
    while X<MAP do
    begin
      Z := 0;
      while Z<MAP do
      begin
        y[NodeIndex(x, z)] :=  y[NodeIndex(x, z)] + Random(19990315) mod (r+1) - (r shr 1);
        //if (FAllInWater) and (Y[NodeIndex(x, z)]>0) then
        //  Y[NodeIndex(x, z)] := -Y[NodeIndex(x, z)];
        Inc(Z, bsize);
      end;
      if (i>3) and (r>64) then
        r := r shr 1;
      Inc(X, bsize);
    end;

    csize := bsize div 2;

    if (csize>0) then
    begin
      X := 0;
      while X<MAP do
      begin
        Z := 0;
        while Z<MAP do
        begin
          if ( x < MAP) then
            y[NodeIndex(x+csize,z)] := (y[NodeIndex(x+bsize, z)]+y[NodeIndex(x, z)]) div 2;
          if ( z < MAP) then
            y[NodeIndex(x,z+csize)] := (y[NodeIndex(x,z+bsize)]+y[NodeIndex(x, z)]) div 2;
          if ( x < MAP) and (z < MAP ) then
            y[NodeIndex(x+csize,z+csize)] := ( y[NodeIndex(x, z)] + y[NodeIndex(x+bsize, z)] + y[NodeIndex(x,z+bsize)] +
              y[NodeIndex(x+bsize,z+bsize)]) div 4;
          Inc(Z, bsize);
        end;
        Inc(X, bsize);
      end;
    end;
    bsize := csize;
  end;

  //I am leaving trees out for now.
  for X:=0 to MAP-1 do
    for Z:=0 to MAP-1 do
      FObject[NodeIndex(x,z)] := Ord('n');

  for X:=0 to MAP-1 do
    for Z:=0 to MAP-1 do
    begin
      if (y[NodeIndex(x,z)]>MH) then
      begin
        i := y[NodeIndex(x,z)];
        y[NodeIndex(x,z)] := i * i;
        y[NodeIndex(x,z)] :=  Round(y[NodeIndex(x,z)]/MH);
      end;
    end;

  //This next bit smooths down the landscape.

  for X:=1 to MAP-1 do
    for Z:=1 to MAP-1 do
    begin
      y[NodeIndex(x, z)] := y[NodeIndex(x, z-1)] + y[NodeIndex(x, z+1)] + y[NodeIndex(x-1, z)] + y[NodeIndex(x+1, z)];
      y[NodeIndex(x, z)] := y[NodeIndex(x, z)] div 4;
    end;

  //This bit actually does the shading etc. */
  for x:=0 to MAP-1 do
    for z:=0 to MAP-1 do
    begin
      g1 := Round((y[NodeIndex(x, z)]-y[NodeIndex(x+1, z)]) * 2.0 + 127);
      if (g1>255) then
        g1 := 255
      else
        if(g1<0) then
          g1 := 0;

      if(y[NodeIndex(x, z)]>=MH) then
      begin
        c[NodeIndex(x,z)].r := g1;
        c[NodeIndex(x,z)].g := c[NodeIndex(x,z)].r;
        c[NodeIndex(x,z)].b := c[NodeIndex(x,z)].r;
      end else
        if(y[NodeIndex(x, z)]>(MH div 2)) then
        begin
          c[NodeIndex(x,z)].r := g1;
          c[NodeIndex(x,z)].g := Round(g1*0.75);
          c[NodeIndex(x,z)].b := Round(g1*0.5);
        end else
          if(y[NodeIndex(x, z)]>0) then
          begin
            c[NodeIndex(x,z)].g := g1;
            c[NodeIndex(x,z)].r := Round(g1*0.75);
            c[NodeIndex(x,z)].b := 0;
          end else
          begin
            c[NodeIndex(x,z)].r := g1;
            c[NodeIndex(x,z)].g := Round(g1*0.75);
            c[NodeIndex(x,z)].b := Round(g1*0.5);
          end;
    end;
end;

//------------------------------------------------------------------------------
procedure TLandScape.DrawFLAT(X, Z: Integer; X1, X2, Z1, Z2: Integer);
{
var
  V1, V2, N: TAffineVector;
  A, B, D: TAffineVector;
begin
  A[0] := x1;
  A[1] := y[NodeIndex(x,z+1)];
  A[2] := z2;
  B[0] := x2;
  B[1] := y[NodeIndex(x+1,z+1)];
  B[2] := z2;
  D[0] := x2;
  D[1] := y[NodeIndex(x+1,z)];
  D[2] := z1;
  // normal vector from face
  V1 := VectorAffineSubtract(B, A);
  V2 := VectorAffineSubtract(D, A);
  N := VectorCrossProduct(V1, V2);
  glBegin(GL_QUADS);
  glNormal3fv(@N);
  glColor3ub(c[NodeIndex(x,z+1)].r, c[NodeIndex(x,z+1)].g, c[NodeIndex(x,z+1)].b);
  xglTexCoord2f(0.0, 1.0);
  glVertex3fv(@A);
  glColor3ub(c[NodeIndex(x+1,z+1)].r, c[NodeIndex(x+1,z+1)].g, c[NodeIndex(x+1,z+1)].b);
  xglTexCoord2f(1.0, 1.0);
  glVertex3fv(@B);
  glColor3ub(c[NodeIndex(x+1,z)].r, c[NodeIndex(x+1,z)].g, c[NodeIndex(x+1,z)].b);
  xglTexCoord2f(1.0, 0.0);
  glVertex3fv(@D);
  glColor3ub(c[NodeIndex(x,z)].r, c[NodeIndex(x,z)].g, c[NodeIndex(x,z)].b);
  xglTexCoord2f(0.0, 0.0);
  glVertex3f(x1, y[NodeIndex(x, z)], z1);
}
begin
  glBegin(GL_QUADS);
  glColor3ub(c[NodeIndex(x,z+1)].r, c[NodeIndex(x,z+1)].g, c[NodeIndex(x,z+1)].b);
  xglTexCoord2f(0.0, 1.0);
  glVertex3f(x1, y[NodeIndex(x,z+1)], z2);
  glColor3ub(c[NodeIndex(x+1,z+1)].r, c[NodeIndex(x+1,z+1)].g, c[NodeIndex(x+1,z+1)].b);
  xglTexCoord2f(1.0, 1.0);
  glVertex3f(x2, y[NodeIndex(x+1, z+1)], z2);
  glColor3ub(c[NodeIndex(x+1,z)].r, c[NodeIndex(x+1,z)].g, c[NodeIndex(x+1,z)].b);
  xglTexCoord2f(1.0, 0.0);
  glVertex3f(x2, y[NodeIndex(x+1, z)], z1);
  glColor3ub(c[NodeIndex(x,z)].r, c[NodeIndex(x,z)].g, c[NodeIndex(x,z)].b);
  xglTexCoord2f(0.0, 0.0);
  glVertex3f(x1, y[NodeIndex(x, z)], z1);
  glEnd();
end;

//------------------------------------------------------------------------------
procedure TLandScape.DrawFLAT2(X, Z: Integer; X1, X2, Z1, Z2: Integer);
{
var
  V1, V2, N: TAffineVector;
  A, B, D: TAffineVector;
begin
  A[0] := x1;
  A[1] := y[NodeIndex(x,z+4)];
  A[2] := z2;
  B[0] := x2;
  B[1] := y[NodeIndex(x+4,z+4)];
  B[2] := z2;
  D[0] := x2;
  D[1] := y[NodeIndex(x+4,z)];
  D[2] := z1;
  // normal vector from face
  V1 := VectorAffineSubtract(B, A);
  V2 := VectorAffineSubtract(D, A);
  N := VectorCrossProduct(V1, V2);
  glBegin(GL_QUADS);
  glNormal3fv(@N);
  glColor3ub(c[NodeIndex(x,z+4)].r, c[NodeIndex(x,z+4)].g, c[NodeIndex(x,z+4)].b);
  xglTexCoord2f(0.0, 1.0);
  glVertex3fv(@A);
  glColor3ub(c[NodeIndex(x+4,z+4)].r, c[NodeIndex(x+4,z+4)].g, c[NodeIndex(x+4,z+4)].b);
  xglTexCoord2f(1.0, 1.0);
  glVertex3fv(@B);
  glColor3ub(c[NodeIndex(x+4,z)].r, c[NodeIndex(x+4,z)].g, c[NodeIndex(x+4,z)].b);
  xglTexCoord2f(1.0, 0.0);
  glVertex3fv(@D);
  glColor3ub(c[NodeIndex(x,z)].r, c[NodeIndex(x,z)].g, c[NodeIndex(x,z)].b);
  xglTexCoord2f(0.0, 0.0);
  glVertex3f(x1, y[NodeIndex(x, z)], z1);
  }
begin
  glBegin(GL_QUADS);

  glColor3ub(c[NodeIndex(x,z+4)].r, c[NodeIndex(x,z+4)].g, c[NodeIndex(x,z+4)].b);
  xglTexCoord2f(0.0, 1.0);
  glVertex3f(x1, y[NodeIndex(x,z+4)], z2);
  glColor3ub(c[NodeIndex(x+4,z+4)].r, c[NodeIndex(x+4,z+4)].g, c[NodeIndex(x+4,z+4)].b);
  xglTexCoord2f(1.0, 1.0);
  glVertex3f(x2, y[NodeIndex(x+4, z+4)], z2);
  glColor3ub(c[NodeIndex(x+4,z)].r, c[NodeIndex(x+4,z)].g, c[NodeIndex(x+4,z)].b);
  xglTexCoord2f(1.0, 0.0);
  glVertex3f(x2, y[NodeIndex(x+4, z)], z1);
  glColor3ub(c[NodeIndex(x,z)].r, c[NodeIndex(x,z)].g, c[NodeIndex(x,z)].b);
  xglTexCoord2f(0.0, 0.0);
  glVertex3f(x1, y[NodeIndex(x, z)], z1);

  glEnd();
end;


//------------------------------------------------------------------------------
function TLandScape.Get_Height(SX, SZ: TGLfloat): TGLfloat;
var
  x0, x1, lx, lz, x, z, midpoint: TGLfloat;
  fx, fz: Integer;
begin
  x := SX/comp;
  z := SZ/comp;
  fx := Round(x-0.5);
  fz := Round(z-0.5);
  //Why does the Trunc function cause a division by zero error
  //fx := Trunc(x);
  //fz := Trunc(z);
  lx := x - fx;
  lz := z - fz;

  x0 := y[NodeIndex(fx,fz)] + (y[NodeIndex(fx,fz+1)] - y[NodeIndex(fx,fz)])*lz;
  x1 := y[NodeIndex(fx+1,fz)] + (y[NodeIndex(fx+1,fz+1)] - y[NodeIndex(fx+1,fz)])*lz;
  midpoint := x0 + (x1 - x0)*lx;

  Result := midpoint;
  {
  if(x<distance) then
    posx := ((MAP - distance) shl 6)
  else
    if(x>(MAP-distance)) then
      posx := (distance shl 6);
  if(z<distance) then
    posz := ((MAP - distance) shl 6)
  else
    if(z>(MAP-distance)) then
      posz := (distance shl 6);

  if (FOceans) then
    if (posy < w_h + 16.0) then
      posy := w_h + 16.0;
  }
end;

//------------------------------------------------------------------------------
procedure TLandScape.GetTextureFilter;
begin
  case FMinFilter of
    miNearest              : FMinFilterValue := GL_NEAREST;
    miLinear               : FMinFilterValue := GL_LINEAR;
    miNearestMipmapNearest : FMinFilterValue := GL_NEAREST_MIPMAP_NEAREST;
    miLinearMipmapNearest  : FMinFilterValue := GL_LINEAR_MIPMAP_NEAREST;
    miNearestMipmapLinear  : FMinFilterValue := GL_NEAREST_MIPMAP_LINEAR;
    miLinearMipmapLinear   : FMinFilterValue := GL_LINEAR_MIPMAP_LINEAR;
  end;

  case FMagFilter of
    maNearest : FMagFilterValue := GL_NEAREST;
    maLinear  : FMagFilterValue := GL_LINEAR;
  end;
end;

//------------------------------------------------------------------------------
procedure TLandScape.SetMagFilter(Value: TMagFilter);
begin
  FMagFilter := Value;
  GetTextureFilter;
end;

//------------------------------------------------------------------------------
procedure TLandScape.SetMinFilter(Value: TMinFilter);
begin
  FMinFilter := Value;
  GetTextureFilter;
end;

//------------------------------------------------------------------------------
procedure TLandScape.RenderLandScape(Camera: TCamera);
var
  X, Z, Position_X, Position_Z: Integer;
  X1, X2, Z1, Z2: TGLfloat;
  fogColor: array[0..3] of TGLfloat;
const
  distance_l = comp * distance;
begin
  fogColor[0] := 0;
  fogColor[1] := 0;
  fogColor[2] := 0;
  fogColor[3] := 1.0;

  glPushAttrib(GL_ALL_ATTRIB_BITS);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, FMagFilterValue);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, FMinFilterValue);
  glEnable(GL_TEXTURE_2D);

  glDisable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  position_x := Round(Camera.Position.X/comp);
  position_z := Round(Camera.Position.Z/comp);

  glPushMatrix;
  Camera.RestoreMatrix;

  SetGLCurrentTexture(FLandTexture);
  if(FFullView) then
  begin
    X := 0;
    while (X<MAP) do
    begin
      Z := 0;
      while Z<MAP do
      begin
        x1 := (x shl 6);
        x2 := (x shl 6) + (comp shl 2);
	z1 := (z shl 6);
	z2 := (z shl 6) + (comp shl 2);
	DrawFLAT2(x, z, Round(x1), Round(x2), Round(z1), Round(z2));
        Inc(Z, 4);
      end;
      Inc(X, 4);
    end;
  end else
  begin
    //********** Warning, this bit needs optimising ! **********/
    //* The following bit of code has been written with the    */
    //* intention of making sure that OpenGL doesn't draw what */
    //* cannot be seen by the camera.                          */
    //**********************************************************/
    if (Camera.Direction.X>=(-0.75)) and (Camera.Direction.X<=(0.75)) then
    begin
      if (Camera.Direction.Z<=0) then
      begin
        X := Position_X - Distance;
        while X<Position_X + Distance do
        begin
          Z := Position_Z - Distance;
          while Z<Position_Z + 2 do
          begin
            x1 := (x shl 6);
            x2 := (x shl 6) + comp;
            z1 := (z shl 6);
            z2 := (z shl 6) + comp;
	    DrawFLAT(x, z, Round(x1), Round(x2), Round(z1), Round(z2));
            Inc(Z);
          end;
          Inc(X);
        end;
      end else
      begin
        if (Camera.Direction.Z>0) then
        begin
          X := Position_X - Distance;
          while X<Position_X + Distance do
          begin
            Z := Position_Z - 2;
            while Z<Position_Z + Distance do
            begin
              x1 := (x shl 6);
              x2 := (x shl 6) + comp;
              z1 := (z shl 6);
              z2 := (z shl 6) + comp;
	      DrawFLAT(x, z, Round(x1), Round(x2), Round(z1), Round(z2));
              Inc(Z);
            end;
            Inc(X);
          end;
        end;
      end;
    end else
    begin
      if (Camera.Direction.Z>=(-0.75)) and (Camera.Direction.Z<=(0.75)) then
      begin
        if (Camera.Direction.X<=0) then
        begin
          X := Position_X - Distance;
          while X<Position_X + 2 do
          begin
            Z := Position_Z - Distance;
            while Z<Position_Z + Distance do
            begin
              x1 := (x shl 6);
              x2 := (x shl 6) + comp;
              z1 := (z shl 6);
              z2 := (z shl 6) + comp;
	      DrawFLAT(x, z, Round(x1), Round(x2), Round(z1), Round(z2));
              Inc(Z);
            end;
            Inc(X);
          end;
        end else
        begin
	  if (Camera.Direction.X>0) then
          begin
            X := Position_X - 2;
            while X<Position_X + Distance do
            begin
              Z := Position_Z - Distance;
              while Z<Position_Z + Distance do
              begin
                x1 := (x shl 6);
                x2 := (x shl 6) + comp;
                z1 := (z shl 6);
                z2 := (z shl 6) + comp;
	        DrawFLAT(x, z, Round(x1), Round(x2), Round(z1), Round(z2));
                Inc(Z);
              end;
              Inc(X);
            end;
          end;
        end;
      end;
    end;
  end;
  {
  if (FOceans) then
  begin
  }
  (*
    glEnable(GL_TEXTURE_2D);
    SetGLCurrentTexture(FOcenTexture);
    glEnable(GL_BLEND);

    glColor4f(0.0, 0.3, 0.6, 0.5);

    {

    X := Position_X - Distance;
    while X<Position_X + DIstance do
    begin
      Z := Position_Z - Distance;
      while Z<Position_Z + Distance do
      begin
        x1 := (x shl 6);
        x2 := (x shl 6) + (comp shl 1);
	z1 := (z shl 6);
	z2 := (z shl 6) + (comp shl 1);
    }
        glBegin(GL_POLYGON);
        {
        glVertex3f(x1, 100, z1);//w_h, z1);
        glVertex3f(x2, 100, z1);
        glVertex3f(x2, 100, z2);
        glVertex3f(x1, 100, z2);
        }
        xglTexCoord2f(0.0, 1.0);
        glVertex3f(0, 0, 100000);
        xglTexCoord2f(1.0, 1.0);
        glVertex3f(100000, 0, 100000);
        xglTexCoord2f(1.0, 0.0);
        glVertex3f(100000, 0, 0);
        xglTexCoord2f(0.0, 0.0);
        glVertex3f(0, 0, 0);//w_h, z1);
        glEnd;
    {
        Inc(Z, 2);
      end;
      Inc(X, 2);
    end;
    }
    glDisable(GL_BLEND);
  {
  end;
  }
  *)
  glPopMatrix;
  glPopAttrib;
end;

begin
  LandScape := nil;
  LensFlares := nil;
end.

