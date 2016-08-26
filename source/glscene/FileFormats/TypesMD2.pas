//
// This unit is part of the GLScene Project, http://glscene.org
//
unit TypesMD2;

{ 25.08.2003 - PhP - dropped TTrivert_t & TDalias_t, cleaned code & degibbered declarations }

interface

uses
  VectorTypes;

const
  MAX_MD2_TRIANGLES = 4096;
  MAX_MD2_VERTICES = 2048;
  MAX_MD2_FRAMES = 512;
  MAX_MD2_SKINS = 32;
  MAX_MD2_SKINNAME = 64;

type
  PMD2VertexIndex = ^TMD2VertexIndex;
  TMD2VertexIndex = record
    A, B, C: integer;
    A_S, A_T,
    B_S, B_T,
    C_S, C_T: single;
  end;

  PMD2Vertex = ^TVector3f;

  PMD2Frames = ^TMD2Frames;
  TMD2Frames = PMD2Vertex;

  TMD2Triangle = record
    VertexIndex: TVector3s;
    TextureCoordIndex: TVector3s;
  end;

  TMD2TriangleVertex = record
    V: array[0..2] of byte;
    LightnormalIndex: byte;
  end;

  PMD2AliasFrame = ^TMD2AliasFrame;
  TMD2AliasFrame = record
    Scale: TVector3f;
    Translate: TVector3f;
    Name: array[0..15] of Char;
    Vertices: array[0..0] of TMD2TriangleVertex;
  end;

  TMD2Header = record
    Ident: integer;
    Version: integer;

    SkinWidth: integer;
    SkinHeight: integer;
    FrameSize: integer;

    Num_Skins: integer;
    Num_Vertices: integer;
    Num_TextureCoords: integer;
    Num_VertexIndices: integer;
    Num_GLCommdands: integer;
    Num_Frames: integer;

    Offset_skins: integer;
    Offset_st: integer;
    Offset_tris: integer;
    Offset_frames: integer;
    Offset_glcmds: integer;
    Offset_end: integer;
  end;

  FrameList = array of TMD2Frames;
  IndexList = array of TMD2VertexIndex;
  VertexList = array of TVector3f;

implementation

end.
