//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  FileNMF - NormalMapper vector file format loading/saving structures
  
  Notes:
    NormalMapper can be found at http://www.ati.com/developer/tools.html
    
  History:
    14/05/2003 - SG - Creation
}
unit FileNMF;

interface

uses
  Classes, VectorGeometry;

const
  NMF_HEADER_TAG   = 'NMF ';
  NMF_TRIANGLE_TAG = 'TRIS';

type
  TNmHeader = record
    hdr  : array[0..3] of char;
    size : cardinal;
  end;
  
  TNmRawTriangle = record
    vert,
    norm     : array[0..2] of TAffineVector;
    texCoord : array[0..2] of TTexPoint;
  end;
  
  TFileNMF = class
    public
      FileHeader,
      TrisHeader   : TNmHeader;
      NumTris      : Integer;
      RawTriangles : array of TNmRawTriangle;
      
      procedure LoadFromStream(Stream : TStream);
      procedure SaveToStream(Stream : TStream);
  end;
  
implementation

procedure TFileNMF.LoadFromStream(Stream : TStream);
var
  Done : Boolean;
begin
  Stream.Read(FileHeader, SizeOf(TNmHeader));
  if FileHeader.hdr<>NMF_HEADER_TAG then exit;
  
  Done:=False;
  while not Done do begin
    Stream.Read(TrisHeader, SizeOf(TNmHeader));
    if TrisHeader.hdr=NMF_TRIANGLE_TAG then begin
      Stream.Read(NumTris, SizeOf(NumTris));
      if NumTris<0 then exit;
      SetLength(RawTriangles,NumTris);
      Stream.Read(RawTriangles[0],SizeOf(TNmRawTriangle)*NumTris);
      Done:=True;
    end;
  end;
end;

procedure TFileNMF.SaveToStream(Stream : TStream);
begin
  NumTris:=Length(RawTriangles);
  TrisHeader.hdr:=NMF_TRIANGLE_TAG;
  TrisHeader.size:=SizeOf(TNmRawTriangle)*NumTris+SizeOf(FileHeader);
  FileHeader.hdr:=NMF_HEADER_TAG;
  FileHeader.size:=TrisHeader.size+SizeOf(TrisHeader);

  Stream.Write(FileHeader, SizeOf(TNmHeader));
  Stream.Write(TrisHeader, SizeOf(TNmHeader));
  NumTris:=Length(RawTriangles);
  Stream.Write(NumTris, SizeOf(NumTris));
  Stream.Write(RawTriangles[0], SizeOf(TNmRawTriangle)*NumTris);
end;

end.