//
// This unit is part of the GLScene Project, http://glscene.org
//
{: TypesMDC<p>

    Types for Return to Castle Wolfenstein's MDC file format.<p>

    Original code by Osman Turan (osmanturancom@yahoo.com)<p>

	<b>History :</b><font size=-1><ul>
      <li>18/12/04 - PhP - fixed constants, improve performance
      <li>11/05/04 - SG - Added to CVS
      <li>07/02/04 - OT - Creation (Osman Turan)
	</ul></font>
}
unit TypesMDC;

interface

uses
  VectorTypes;

const
  MDCFILE_IDENTITY = 'IDPC';
  MDCFILE_VERSION  = 2;

  MDC_BASEVERTEX_FACTOR = 0.015625; // 1/64;
  MDC_COMPVERTEX_FACTOR = 0.046875; // 3/64;

type
  TMDCPoint = array[0..2] of Single;
  TMDCAngle = TMDCPoint;

  TMDCFileHeader = packed record
    Ident              : array[0..3] of Char;
    Version            : Cardinal;
    Name               : array[0..63] of Char;
    Flags              : Cardinal;
    NumFrames          : Cardinal;
    NumTags            : Cardinal;
    NumSurfaces        : Cardinal;
    NumSkins           : Cardinal;
    OffsetBorderFrames : Cardinal;
    OffsetTagNames     : Cardinal;
    OffsetTagFrames    : Cardinal;
    OffsetSurfaces     : Cardinal;
    OffsetEnd          : Cardinal;
  end;

  TMDCBorderFrame = packed record
    BBMin, BBMax : TMDCPoint;
    LocalOrigin  : TMDCPoint;
    Radius       : Single;
    Name         : array[0..15] of Char;
  end;

  PMDCTagName = ^TMDCTagName;
  TMDCTagName = packed record
    Name: array[0..63] of Char;
  end;

  PMDCTagFrame = ^TMDCTagFrame;
  TMDCTagFrame = packed record
    TagPosition: array[0..2] of Word; //or ShortInt?
    TagAngle: array[0..2] of Word;    //or ShortInt?
  end;

  TMDCTag = packed record
    TagName: PMDCTagName;
    TagFrame: PMDCTagFrame;
  end;

  TMDCSurfaceHeader = packed record
    Ident                 : array[0..3] of Char;
    Name                  : array[0..63] of Char;
    Flags                 : Cardinal;
    NumCompFrames         : Cardinal;
    NumBaseFrames         : Cardinal;
    NumSkins              : Cardinal;
    NumVertices           : Cardinal;
    NumTriangles          : Cardinal;
    OffsetTriangles       : Cardinal;
    OffsetSkins           : Cardinal;
    OffsetTexCoords       : Cardinal;
    OffsetBaseVerts       : Cardinal;
    OffsetCompVerts       : Cardinal;
    OffsetFrameBaseFrames : Cardinal;
    OffsetFrameCompFrames : Cardinal;
    OffsetEnd             : Cardinal;
  end;

  TMDCTriangle = array[0..2] of Cardinal;

  TMDCSkin = packed record
    Shader : array[0..63] of Char;
    Flags  : Cardinal;
  end;

  TMDCTexCoord = array[0..1] of Single;

  TMDCBaseVertex = array[0..3] of SmallInt;

  TMDCCompVertex = array[0..3] of Byte;

  TMDCBaseFrame = packed record
    BaseVertices: array of TMDCBaseVertex;
  end;

  TMDCCompFrame = packed record
    CompVertices: array of TMDCCompVertex;
  end;

implementation

end.
