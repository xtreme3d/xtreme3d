//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VectorGeometry<p>

	Base classes and structures for GLScene.<p>

   Most common functions/procedures come in various flavours (using overloads),
   the naming convention is :<ul>
   <li>TypeOperation: functions returning a result, or accepting a "var" as last
      parameter to place result (VectorAdd, VectorCrossProduct...)
   <li>OperationType : procedures taking as first parameter a "var" that will be
      used as operand and result (AddVector, CombineVector...)
   </ul><p>
   As a general rule, procedures implementations (asm or not) are the fastest
   (up to 800% faster than function equivalents), due to reduced return value
   duplication overhead (the exception being the matrix operations).<p>

   For better performance, it is recommended <b>not</b> to use the "Math" unit
   that comes with Delphi, and only use functions/procedures from this unit
   (the single-based functions have been optimized and are up to 100% faster,
   than extended-based ones from "Math").<p>

   3DNow! SIMD instructions are automatically detected and used in *some* of the
   functions/procedures, typical gains (over FPU implementation) are approx a
   100% speed increase on K6-2/3, and 20-60% on K7, and sometimes more
   (f.i. 650% on 4x4 matrix multiplication for the K6, 300% for RSqrt on K7).<p>

   Cyrix, NexGen and other "exotic" CPUs may fault in the 3DNow! detection
   (initialization section), comment out or replace with your own detection
   routines if you want to support these. All AMD processors after K5, and
   all Intel processors after Pentium should be immune to this.<p>

	<b>History : </b><font size=-1><ul>
      <li>07/04/06 - DB - Fixed VectorArrayLerp_3DNow (affine) for n<=1 (dikoe Kenguru)
      <li>02/12/04 - MF - Added IsVolumeClipped overload that uses Frustum instead
                          of rcci
      <li>08/07/04 - LR - Removed ../ from the GLScene.inc
      <li>26/10/03 - EG - Renamed from "Geometry.pas" to "VectorGeometry.pas"
      <li>17/10/03 - EG - Optimized Min/MaxInteger, some of the Min/MaxFloat
      <li>13/08/03 - SG - Added TQuaternionArray, PQuaternionArray and PQuaternion
      <li>21/07/03 - EG - Added RoundInt, faster Round/Round64, updated Power
      <li>04/07/03 - EG - New VectorCombine overload, some optimizations
      <li>18/06/03 - MF - Added PointSegmentClosestPoint, PointSegmentDistance,
                          PointLineClosestPoint and PointLineDistance.
      <li>26/05/03 - EG - NO_ASM variant creation completed
      <li>22/05/03 - EG - All vSIMD asm tests should now be under GEOMETRY_NO_ASM control
      <li>20/05/03 - EG - Added MakeParallelProjectionMatrix
      <li>13/05/03 - EG - 3DNow! optimization for ClampValue
      <li>30/04/03 - EG - Hyperbolic trig functions (Aaron Hochwimmer)
      <li>14/02/03 - EG - Added ScaleAndRound
      <li>28/01/03 - EG - Affine matrix inversion and related functions (Dan Barlett)
      <li>29/10/02 - EG - New MinFloat overloads (Bob)
      <li>04/09/02 - EG - New Abs/Max functions, VectorTransform(affine, hmgMatrix)
                          now considers the matrix as 4x3 (was 3x3)
      <li>21/08/02 - EG - Added Pack/UnPackRotationMatrix
      <li>13/08/02 - EG - Added Area functions
      <li>20/07/02 - EG - Fixed RayCastTriangleIntersect "backward" hits
      <li>05/07/02 - EG - Started adding non-asm variants (GEOMETRY_NO_ASM)
      <li>22/02/02 - EG - Temporary Quaternion fix for VectorAngleLerp
      <li>12/02/02 - EG - Added QuaternionFromEuler (Alex Grigny de Castro)
      <li>11/02/02 - EG - Non-spinned QuaternionSlerp (Alex Grigny de Castro)
      <li>07/02/02 - EG - Added AnglePreservingMatrixInvert
      <li>30/01/02 - EG - New Quaternion<->Matrix code (Alex Grigny de Castro)
      <li>29/01/02 - EG - Fixed AngleLerp, added DistanceBetweenAngles (Alex Grigny de Castro)
      <li>20/01/02 - EG - Added VectorArrayAdd, ScaleFloatArray, OffsetFloatArray
      <li>11/01/02 - EG - 3DNow Optim for VectorAdd (hmg)
      <li>10/01/02 - EG - Fixed VectorEquals ("True" wasn't Pascal compliant "1"),
                          3DNow optims for vector mormalizations (affine),
                          Added RSqrt
      <li>04/01/02 - EG - Updated/fixed RayCastTriangleIntersect
      <li>13/12/01 - EG - Fixed MakeReflectionMatrix
      <li>02/11/01 - EG - Faster mode for PrepareSinCosCache (by Nelson Chu)
      <li>22/08/01 - EG - Some new overloads
      <li>19/08/01 - EG - Added sphere raycasting functions
      <li>08/08/01 - EG - Added MaxFloat overloads
      <li>24/07/01 - EG - VectorAngle renamed to VectorAngleCosine to avoid confusions
      <li>06/07/01 - EG - Added NormalizeDegAngle
      <li>04/07/01 - EG - Now uses VectorTypes
      <li>18/03/01 - EG - Added AngleLerp and NormalizeAngle
      <li>15/03/01 - EG - Added Int, Ceil and Floor, faster "Frac"
      <li>06/03/01 - EG - Fix in PointInPolygon by Pavel Vassiliev
      <li>04/03/01 - EG - Added NormalizeVectorArray
      <li>03/03/01 - EG - Added MakeReflectionMatrix
      <li>02/03/01 - EG - New PointInPolygon code by Pavel Vassiliev
      <li>25/02/01 - EG - Fixed 'VectorSubstract', added VectorArrayLerp and a few minors
      <li>22/02/01 - EG - Added MinXYZ/MaxXYZ variants and Plane-Line intersection
      <li>21/02/01 - EG - Added Sign, MinFloat & MaxFloat
      <li>15/02/01 - EG - Faster Vector Transforms (3DNow! optimizations)
      <li>14/02/01 - EG - Faster Matrix multiplications (3DNow! & FPU optimizations),
                          Added support for FPU-only sections
      <li>05/02/01 - EG - Faster VectorEquals
      <li>21/01/01 - EG - Fixed MakePoint/Vector affine variants (thx Jacques Tur)
      <li>17/01/00 - EG - VectoAdd return type fix (thx Jacques Tur),
                          also added a few new overloads
      <li>05/11/00 - EG - Added RayCastPlaneIntersect
      <li>08/10/00 - EG - Added SetMatrix
      <li>13/08/00 - EG - Added Plane geometry support
      <li>06/08/00 - EG - Various minor additions
      <li>16/07/00 - EG - Added some new mixed vector/scalar funcs and new overloads
      <li>12/07/00 - EG - New overloads and replacements for Power, Trunc, Frac & Round
      <li>25/06/00 - EG - End of major update
      <li>13/06/00 - EG - Start of major update
      <li>09/06/00 - EG - Some additions and fixes in preparation for major changes
      <li>05/06/00 - EG - Added VectorLength overloads
      <li>26/05/00 - EG - [0..0] arrays changed to [0..cMaxArray]
      <li>23/05/00 - EG - Added intersection functions,
                          Replaced some xxxAffinexxx funcs with overloads
      <li>22/03/00 - EG - Added MakeShadowMatrix (adapted from "OpenGL SuperBible" book)
      <li>21/03/00 - EG - Removed PWordArray (was a SysUtils's duplicate)
      <li>06/02/00 - EG - Added VectorEquals
      <li>05/02/00 - EG - Added some "const", more still needed,
                          Added overloads for some of the MakeXXXVector funcs,
                          Added homogeneous vector consts, VectorSpacing
   </ul>
}
unit VectorGeometry;

// This unit contains many needed types, functions and procedures for
// quaternion, vector and matrix arithmetics. It is specifically designed
// for geometric calculations within R3 (affine vector space)
// and R4 (homogeneous vector space).
//
// Note: The terms 'affine' or 'affine coordinates' are not really correct here
//       because an 'affine transformation' describes generally a transformation which leads
//       to a uniquely solvable system of equations and has nothing to do with the dimensionality
//       of a vector. One could use 'projective coordinates' but this is also not really correct
//       and since I haven't found a better name (or even any correct one), 'affine' is as good
//       as any other one.
//
// Identifiers containing no dimensionality (like affine or homogeneous)
// and no datatype (integer..extended) are supposed as R4 representation
// with 'single' floating point type (examples are TVector, TMatrix,
// and TQuaternion). The default data type is 'single' ('GLFloat' for OpenGL)
// and used in all routines (except conversions and trigonometric functions).
//
// Routines with an open array as argument can either take Func([1,2,3,4,..]) or Func(Vect).
// The latter is prefered, since no extra stack operations is required.
// Note: Be careful while passing open array elements! If you pass more elements
//       than there's room in the result the behaviour will be unpredictable.
//
// If not otherwise stated, all angles are given in radians
// (instead of degrees). Use RadToDeg or DegToRad to convert between them.
//
// Geometry.pas was assembled from different sources (like GraphicGems)
// and relevant books or based on self written code, respectivly.
//
// Note: Some aspects need to be considered when using Delphi and pure
//       assembler code. Delphi esnures that the direction flag is always
//       cleared while entering a function and expects it cleared on return.
//       This is in particular important in routines with (CPU) string commands (MOVSD etc.)
//       The registers EDI, ESI and EBX (as well as the stack management
//       registers EBP and ESP) must not be changed! EAX, ECX and EDX are
//       freely available and mostly used for parameter.
//
// Version 2.5
// last change : 04. January 2000
//
// (c) Copyright 1999, Dipl. Ing. Mike Lischke (public@lischke-online.de)

interface

uses VectorTypes;

const
   cMaxArray = (MaxInt shr 4);

{$i GLScene.inc}

// define for turning off assembly routines in this unit
// *experimental* and incomplete
{.$define GEOMETRY_NO_ASM}

type
   // data types needed for 3D graphics calculation,
   // included are 'C like' aliases for each type (to be
   // conformal with OpenGL types)
{$ifndef FPC}
   PByte = ^Byte;
   PWord = ^Word;
   PInteger = ^Integer;
   PCardinal = ^Cardinal;
   PSingle = ^Single;
   PDouble = ^Double;
   PExtended = ^Extended;
   PPointer = ^Pointer;
{$endif}
   PFloat = ^Single;

  	PTexPoint = ^TTexPoint;
	TTexPoint = packed record
                  S, T : Single;
               end;

   // types to specify continous streams of a specific type
   // switch off range checking to access values beyond the limits
   PByteVector = ^TByteVector;
   PByteArray = PByteVector;
   TByteVector = array[0..cMaxArray] of Byte;

   PWordVector = ^TWordVector;
   TWordVector = array[0..cMaxArray] of Word;

   PIntegerVector = ^TIntegerVector;
   PIntegerArray = PIntegerVector;
   TIntegerVector = array[0..cMaxArray] of Integer;

   PFloatVector = ^TFloatVector;
   PFloatArray = PFloatVector;
   PSingleArray = PFloatArray;
   TFloatVector = array[0..cMaxArray] of Single;
   TSingleArray = array of Single;

   PDoubleVector = ^TDoubleVector;
   PDoubleArray = PDoubleVector;
   TDoubleVector = array[0..cMaxArray] of Double;

   PExtendedVector = ^TExtendedVector;
   PExtendedArray = PExtendedVector;
   TExtendedVector = array[0..cMaxArray] of Extended;

   PPointerVector = ^TPointerVector;
   PPointerArray = PPointerVector;
   TPointerVector = array[0..cMaxArray] of Pointer;

   PCardinalVector = ^TCardinalVector;
   PCardinalArray = PCardinalVector;
   TCardinalVector = array[0..cMaxArray] of Cardinal;

   // common vector and matrix types with predefined limits
   // indices correspond like: x -> 0
   //                          y -> 1
   //                          z -> 2
   //                          w -> 3

   PHomogeneousByteVector = ^THomogeneousByteVector;
   THomogeneousByteVector = array[0..3] of Byte;
   TVector4b = THomogeneousByteVector;

   PHomogeneousWordVector = ^THomogeneousWordVector;
   THomogeneousWordVector = array[0..3] of Word;
   TVector4w = THomogeneousWordVector;

   PHomogeneousIntVector = ^THomogeneousIntVector;
   THomogeneousIntVector = TVector4i;

   PHomogeneousFltVector = ^THomogeneousFltVector;
   THomogeneousFltVector = TVector4f;

   PHomogeneousDblVector = ^THomogeneousDblVector;
   THomogeneousDblVector = TVector4d;

   PHomogeneousExtVector = ^THomogeneousExtVector;
   THomogeneousExtVector = array[0..3] of Extended;
   TVector4e = THomogeneousExtVector;

   PHomogeneousPtrVector = ^THomogeneousPtrVector;
   THomogeneousPtrVector = array[0..3] of Pointer;
   TVector4p = THomogeneousPtrVector;

   PAffineByteVector = ^TAffineByteVector;
   TAffineByteVector = array[0..2] of Byte;
   TVector3b = TAffineByteVector;

   PAffineWordVector = ^TAffineWordVector;
   TAffineWordVector = array[0..2] of Word;
   TVector3w = TAffineWordVector;

   PAffineIntVector = ^TAffineIntVector;
   TAffineIntVector = TVector3i;

   PAffineFltVector = ^TAffineFltVector;
   TAffineFltVector = TVector3f;

   PAffineDblVector = ^TAffineDblVector;
   TAffineDblVector = TVector3d;

   PAffineExtVector = ^TAffineExtVector;
   TAffineExtVector = array[0..2] of Extended;
   TVector3e = TAffineExtVector;

   PAffinePtrVector = ^TAffinePtrVector;
   TAffinePtrVector = array[0..2] of Pointer;
   TVector3p = TAffinePtrVector;

   // some simplified names
   PVector = ^TVector;
   TVector = THomogeneousFltVector;

   PHomogeneousVector = ^THomogeneousVector;
   THomogeneousVector = THomogeneousFltVector;

   PAffineVector = ^TAffineVector;
   TAffineVector = TVector3f;

   PVertex    = ^TVertex;
	TVertex    = TAffineVector;

   // arrays of vectors
   PAffineVectorArray = ^TAffineVectorArray;
   TAffineVectorArray = array[0..MAXINT shr 4] of TAffineVector;

   PVectorArray = ^TVectorArray;
   TVectorArray = array[0..MAXINT shr 5] of TVector;

	PTexPointArray = ^TTexPointArray;
	TTexPointArray = array [0..MaxInt shr 4] of TTexPoint;

   // matrices
   THomogeneousByteMatrix = array[0..3] of THomogeneousByteVector;
   TMatrix4b = THomogeneousByteMatrix;

   THomogeneousWordMatrix = array[0..3] of THomogeneousWordVector;
   TMatrix4w = THomogeneousWordMatrix;

   THomogeneousIntMatrix = TMatrix4i;

   THomogeneousFltMatrix = TMatrix4f;

   THomogeneousDblMatrix = TMatrix4d;

   THomogeneousExtMatrix = array[0..3] of THomogeneousExtVector;
   TMatrix4e = THomogeneousExtMatrix;

   TAffineByteMatrix = array[0..2] of TAffineByteVector;
   TMatrix3b = TAffineByteMatrix;

   TAffineWordMatrix = array[0..2] of TAffineWordVector;
   TMatrix3w = TAffineWordMatrix;

   TAffineIntMatrix = TMatrix3i;

   TAffineFltMatrix = TMatrix3f;

   TAffineDblMatrix = TMatrix3d;

   TAffineExtMatrix = array[0..2] of TAffineExtVector;
   TMatrix3e = TAffineExtMatrix;

   // some simplified names
   PMatrix = ^TMatrix;
   TMatrix = THomogeneousFltMatrix;

   TMatrixArray = array [0..MaxInt shr 7] of TMatrix;
   PMatrixArray = ^TMatrixArray;

   PHomogeneousMatrix = ^THomogeneousMatrix;
   THomogeneousMatrix = THomogeneousFltMatrix;

   PAffineMatrix = ^TAffineMatrix;
   TAffineMatrix = TAffineFltMatrix;

   {: A plane equation.<p>
      Defined by its equation A.x+B.y+C.z+D<p>, a plane can be mapped to the
      homogeneous space coordinates, and this is what we are doing here.<br>
      The typename is just here for easing up data manipulation. }
   THmgPlane = TVector;
   TDoubleHmgPlane = THomogeneousDblVector;

   // q = ([x, y, z], w)
   PQuaternion = ^TQuaternion;
   TQuaternion = record
      ImagPart: TAffineVector;
      RealPart: Single;
   end;

   PQuaternionArray = ^TQuaternionArray;
   TQuaternionArray = array[0..MAXINT shr 5] of TQuaternion;

   TRectangle = record
     Left, Top, Width, Height: Integer;
   end;

   TFrustum = record
      pLeft, pTop, pRight, pBottom, pNear, pFar : THmgPlane;
   end;

   TTransType = (ttScaleX, ttScaleY, ttScaleZ,
                 ttShearXY, ttShearXZ, ttShearYZ,
                 ttRotateX, ttRotateY, ttRotateZ,
                 ttTranslateX, ttTranslateY, ttTranslateZ,
                 ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

   // used to describe a sequence of transformations in following order:
   // [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
   // constants are declared for easier access (see MatrixDecompose below)
   TTransformations  = array [TTransType] of Single;

   // TRenderContextClippingInfo
   //
   TRenderContextClippingInfo = record
      origin : TVector;
      clippingDirection : TVector;
      viewPortRadius : Single; // viewport bounding radius per distance unit
      farClippingDistance : Single;
      frustum : TFrustum;
   end;

   TPackedRotationMatrix = array [0..2] of SmallInt;

const
  // useful constants

  // TexPoints (2D space)
  XTexPoint    : TTexPoint = (S:1; T:0);
  YTexPoint    : TTexPoint = (S:0; T:1);
  XYTexPoint   : TTexPoint = (S:1; T:1);
  NullTexPoint : TTexPoint = (S:0; T:0);
  MidTexPoint  : TTexPoint = (S:0.5; T:0.5);

  // standard vectors
  XVector :    TAffineVector = (1, 0, 0);
  YVector :    TAffineVector = (0, 1, 0);
  ZVector :    TAffineVector = (0, 0, 1);
  XYVector :   TAffineVector = (1, 1, 0);
  XZVector :   TAffineVector = (1, 0, 1);
  YZVector :   TAffineVector = (0, 1, 1);
  XYZVector :  TAffineVector = (1, 1, 1);
  NullVector : TAffineVector = (0, 0, 0);
  MinusXVector : TAffineVector = (-1,  0,  0);
  MinusYVector : TAffineVector = ( 0, -1,  0);
  MinusZVector : TAffineVector = ( 0,  0, -1);
  // standard homogeneous vectors
  XHmgVector : THomogeneousVector = (1, 0, 0, 0);
  YHmgVector : THomogeneousVector = (0, 1, 0, 0);
  ZHmgVector : THomogeneousVector = (0, 0, 1, 0);
  WHmgVector : THomogeneousVector = (0, 0, 0, 1);
  XYHmgVector  : THomogeneousVector =  (1, 1, 0, 0);
  XYZHmgVector  : THomogeneousVector = (1, 1, 1, 0);
  XYZWHmgVector : THomogeneousVector = (1, 1, 1, 1);
  NullHmgVector : THomogeneousVector = (0, 0, 0, 0);
  // standard homogeneous points
  XHmgPoint :  THomogeneousVector = (1, 0, 0, 1);
  YHmgPoint :  THomogeneousVector = (0, 1, 0, 1);
  ZHmgPoint :  THomogeneousVector = (0, 0, 1, 1);
  WHmgPoint :  THomogeneousVector = (0, 0, 0, 1);
  NullHmgPoint : THomogeneousVector = (0, 0, 0, 1);

  IdentityMatrix: TAffineMatrix = ((1, 0, 0),
                                   (0, 1, 0),
                                   (0, 0, 1));
  IdentityHmgMatrix: TMatrix = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));
  IdentityHmgDblMatrix: THomogeneousDblMatrix = ((1, 0, 0, 0),
                                                 (0, 1, 0, 0),
                                                 (0, 0, 1, 0),
                                                 (0, 0, 0, 1));
  EmptyMatrix: TAffineMatrix = ((0, 0, 0),
                                (0, 0, 0),
                                (0, 0, 0));
  EmptyHmgMatrix: TMatrix = ((0, 0, 0, 0),
                             (0, 0, 0, 0),
                             (0, 0, 0, 0),
                             (0, 0, 0, 0));

  // Quaternions

  IdentityQuaternion: TQuaternion = (ImagPart:(0,0,0); RealPart: 1);

  // some very small numbers
  EPSILON  : Single = 1e-40;
  EPSILON2 : Single = 1e-30;

//------------------------------------------------------------------------------
// Vector functions
//------------------------------------------------------------------------------

function TexPointMake(const s, t : Single) : TTexPoint;
function AffineVectorMake(const x, y, z : Single) : TAffineVector; overload;
function AffineVectorMake(const v : TVector) : TAffineVector; overload;
procedure SetAffineVector(var v : TAffineVector; const x, y, z : Single); overload;
procedure SetVector(var v : TAffineVector; const x, y, z : Single); overload;
procedure SetVector(var v : TAffineVector; const vSrc : TVector); overload;
procedure SetVector(var v : TAffineVector; const vSrc : TAffineVector); overload;
procedure SetVector(var v : TAffineDblVector; const vSrc : TAffineVector); overload;
procedure SetVector(var v : TAffineDblVector; const vSrc : TVector); overload;
function VectorMake(const v : TAffineVector; w : Single = 0) : TVector; overload;
function VectorMake(const x, y, z: Single; w : Single = 0) : TVector; overload;
function PointMake(const x, y, z: Single) : TVector; overload;
function PointMake(const v : TAffineVector) : TVector; overload;
function PointMake(const v : TVector) : TVector; overload;
procedure SetVector(var v : TVector; const x, y, z : Single; w : Single = 0); overload;
procedure SetVector(var v : TVector; const av : TAffineVector; w : Single = 0); overload;
procedure SetVector(var v : TVector; const vSrc : TVector); overload;
procedure MakePoint(var v : TVector; const x, y, z: Single); overload;
procedure MakePoint(var v : TVector; const av : TAffineVector); overload;
procedure MakePoint(var v : TVector; const av : TVector); overload;
procedure MakeVector(var v : TAffineVector; const x, y, z: Single); overload;
procedure MakeVector(var v : TVector; const x, y, z: Single); overload;
procedure MakeVector(var v : TVector; const av : TAffineVector); overload;
procedure MakeVector(var v : TVector; const av : TVector); overload;
procedure RstVector(var v : TAffineVector); overload;
procedure RstVector(var v : TVector); overload;

//: Returns the sum of two affine vectors
function VectorAdd(const v1, v2 : TAffineVector) : TAffineVector; overload;
//: Adds two vectors and places result in vr
procedure VectorAdd(const v1, v2 : TAffineVector; var vr : TAffineVector); overload;
procedure VectorAdd(const v1, v2 : TAffineVector; vr : PAffineVector); overload;
//: Returns the sum of two homogeneous vectors
function VectorAdd(const v1, v2 : TVector) : TVector; overload;
procedure VectorAdd(const v1, v2 : TVector; var vr : TVector); overload;
//: Sums up f to each component of the vector
function VectorAdd(const v : TAffineVector; const f : Single) : TAffineVector; overload;
//: Sums up f to each component of the vector
function VectorAdd(const v : TVector; const f : Single) : TVector; overload;
//: Adds V2 to V1, result is placed in V1
procedure AddVector(var v1 : TAffineVector; const v2 : TAffineVector); overload;
//: Adds V2 to V1, result is placed in V1
procedure AddVector(var v1 : TAffineVector; const v2 : TVector); overload;
//: Adds V2 to V1, result is placed in V1
procedure AddVector(var v1 : TVector; const v2 : TVector); overload;
//: Sums up f to each component of the vector
procedure AddVector(var v : TAffineVector; const f : Single); overload;
//: Sums up f to each component of the vector
procedure AddVector(var v : TVector; const f : Single); overload;

//: Adds delta to nb texpoints in src and places result in dest
procedure TexPointArrayAdd(const src : PTexPointArray; const delta : TTexPoint;
                           const nb : Integer;
                           dest : PTexPointArray); overload;
procedure TexPointArrayScaleAndAdd(const src : PTexPointArray; const delta : TTexPoint;
                           const nb : Integer; const scale : TTexPoint;
                           dest : PTexPointArray); overload;
//: Adds delta to nb vectors in src and places result in dest
procedure VectorArrayAdd(const src : PAffineVectorArray; const delta : TAffineVector;
                         const nb : Integer;
                         dest : PAffineVectorArray); overload;

//: Returns V1-V2
function VectorSubtract(const V1, V2 : TAffineVector) : TAffineVector; overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TAffineVector; var result : TAffineVector); overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TAffineVector; var result : TVector); overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1 : TVector; v2 : TAffineVector; var result : TVector); overload;
//: Returns V1-V2
function VectorSubtract(const V1, V2 : TVector) : TVector; overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TVector; var result : TVector); overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TVector; var result : TAffineVector); overload;
function VectorSubtract(const v1 : TAffineVector; delta : Single) : TAffineVector; overload;
function VectorSubtract(const v1 : TVector; delta : Single) : TVector; overload;
//: Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1 : TAffineVector; const V2 : TAffineVector); overload;
//: Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1 : TVector; const V2 : TVector); overload;

//: Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr : TAffineVector; const v : TAffineVector; var f : Single); overload;
procedure CombineVector(var vr : TAffineVector; const v : TAffineVector; pf : PFloat); overload;
//: Makes a linear combination of two texpoints
function TexPointCombine(const t1, t2 : TTexPoint; f1, f2 : Single) : TTexPoint;
//: Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TAffineVector; const F1, F2: Single): TAffineVector; overload;
//: Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TAffineVector; const F1, F2, F3: Single): TAffineVector; overload;
procedure VectorCombine3(const V1, V2, V3: TAffineVector; const F1, F2, F3: Single; var vr : TAffineVector); overload;

//: Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr : TVector; const v : TVector; var f : Single); overload;
//: Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr : TVector; const v : TAffineVector; var f : Single); overload;
//: Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TVector; const F1, F2: Single): TVector; overload;
//: Makes a linear combination of two vectors and return the result
function VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single): TVector; overload;
//: Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single; var vr : TVector); overload;
//: Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1, V2: TVector; const F1, F2: Single; var vr : TVector); overload;
//: Makes a linear combination of two vectors and place result in vr, F1=1.0
procedure VectorCombine(const V1, V2: TVector; const F2: Single; var vr : TVector); overload;
//: Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TVector; const F1, F2, F3: Single): TVector; overload;
//: Makes a linear combination of three vectors and return the result
procedure VectorCombine3(const V1, V2, V3: TVector; const F1, F2, F3: Single; var vr : TVector); overload;

{: Calculates the dot product between V1 and V2.<p>
   Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1, V2 : TAffineVector) : Single; overload;
{: Calculates the dot product between V1 and V2.<p>
   Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1, V2 : TVector) : Single; overload;
{: Calculates the dot product between V1 and V2.<p>
   Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1 : TVector; const V2 : TAffineVector) : Single; overload;

{: Projects p on the line defined by o and direction.<p>
   Performs VectorDotProduct(VectorSubtract(p, origin), direction), which,
   if direction is normalized, computes the distance between origin and the
   projection of p on the (origin, direction) line. }
function PointProject(const p, origin, direction : TAffineVector) : Single; overload;
function PointProject(const p, origin, direction : TVector) : Single; overload;

//: Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2 : TAffineVector): TAffineVector; overload;
//: Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2 : TVector): TVector; overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TVector; var vr : TVector); overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TVector); overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TVector; var vr : TAffineVector); overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TAffineVector); overload;

//: Calculates linear interpolation between start and stop at point t
function Lerp(const start, stop, t : Single) : Single;
//: Calculates angular interpolation between start and stop at point t
function AngleLerp(start, stop, t : Single) : Single;

{: Calculates the angular distance between two angles in radians.<p>
   Result is in the [0; PI] range. }
function DistanceBetweenAngles(angle1, angle2 : Single) : Single;

//: Calculates linear interpolation between texpoint1 and texpoint2 at point t
function TexPointLerp(const t1, t2 : TTexPoint; t : Single) : TTexPoint; overload;
//: Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const v1, v2 : TAffineVector; t : Single) : TAffineVector; overload;
//: Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const v1, v2 : TAffineVector; t : Single; var vr : TAffineVector); overload;
//: Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const v1, v2 : TVector; t : Single) : TVector; overload;
//: Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const v1, v2 : TVector; t : Single; var vr : TVector); overload;

function VectorAngleLerp(const v1, v2 : TAffineVector; t : Single) : TAffineVector; overload;
function VectorAngleCombine(const v1, v2 : TAffineVector; f : Single) : TAffineVector; overload;

//: Calculates linear interpolation between vector arrays
procedure VectorArrayLerp(const src1, src2 : PVectorArray; t : Single; n : Integer; dest : PVectorArray); overload;
procedure VectorArrayLerp(const src1, src2 : PAffineVectorArray; t : Single; n : Integer; dest : PAffineVectorArray); overload;

{: Calculates the length of a vector following the equation sqrt(x*x+y*y). }
function VectorLength(const x, y : Single) : Single; overload;
{: Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z). }
function VectorLength(const x, y, z : Single) : Single; overload;
//: Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z).
function VectorLength(const v : TAffineVector) : Single; overload;
//: Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z+w*w).
function VectorLength(const v : TVector) : Single; overload;
{: Calculates the length of a vector following the equation: sqrt(x*x+y*y+...).<p>
   Note: The parameter of this function is declared as open array. Thus
   there's no restriction about the number of the components of the vector. }
function VectorLength(const v : array of Single) : Single; overload;

{: Calculates norm of a vector which is defined as norm = x * x + y * y<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const x, y : Single) : Single; overload;
{: Calculates norm of a vector which is defined as norm = x*x + y*y + z*z<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const v : TAffineVector) : Single; overload;
{: Calculates norm of a vector which is defined as norm = x*x + y*y + z*z<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const v : TVector) : Single; overload;
{: Calculates norm of a vector which is defined as norm = v[0]*v[0] + ...<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(var V: array of Single) : Single; overload;

//: Transforms a vector to unit length
procedure NormalizeVector(var v : TAffineVector); overload;
//: Transforms a vector to unit length
procedure NormalizeVector(var v : TVector); overload;
//: Returns the vector transformed to unit length
function VectorNormalize(const v : TAffineVector) : TAffineVector; overload;
//: Returns the vector transformed to unit length (w component dropped)
function VectorNormalize(const v : TVector) : TVector; overload;

//: Transforms vectors to unit length
procedure NormalizeVectorArray(list : PAffineVectorArray; n : Integer); overload;

{: Calculates the cosine of the angle between Vector1 and Vector2.<p>
   Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) }
function VectorAngleCosine(const V1, V2: TAffineVector) : Single;

//: Negates the vector
function VectorNegate(const v : TAffineVector) : TAffineVector; overload;
function VectorNegate(const v : TVector) : TVector; overload;

//: Negates the vector
procedure NegateVector(var V : TAffineVector); overload;
//: Negates the vector
procedure NegateVector(var V : TVector); overload;
//: Negates the vector
procedure NegateVector(var V : array of Single); overload;

//: Scales given vector by a factor
procedure ScaleVector(var v : TAffineVector; factor : Single); overload;
{: Scales given vector by another vector.<p>
   v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. }
procedure ScaleVector(var v : TAffineVector; const factor : TAffineVector); overload;
//: Scales given vector by a factor
procedure ScaleVector(var v : TVector; factor : Single); overload;
{: Scales given vector by another vector.<p>
   v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. }
procedure ScaleVector(var v : TVector; const factor : TVector); overload;
//: Returns a vector scaled by a factor
function VectorScale(const v : TAffineVector; factor : Single) : TAffineVector; overload;
//: Scales a vector by a factor and places result in vr
procedure VectorScale(const v : TAffineVector; factor : Single; var vr : TAffineVector); overload;
//: Returns a vector scaled by a factor
function VectorScale(const v : TVector; factor : Single) : TVector; overload;
//: Scales a vector by a factor and places result in vr
procedure VectorScale(const v : TVector; factor : Single; var vr : TVector); overload;
//: Scales a vector by a factor and places result in vr
procedure VectorScale(const v : TVector; factor : Single; var vr : TAffineVector); overload;

{: Divides given vector by another vector.<p>
   v[x]:=v[x]/divider[x], v[y]:=v[y]/divider[y] etc. }
procedure DivideVector(var v : TVector; const divider : TVector); overload;

//: True if all components are equal.
function VectorEquals(const V1, V2: TVector) : Boolean; overload;
//: True if all components are equal.
function VectorEquals(const V1, V2: TAffineVector) : Boolean; overload;
//: True if X, Y and Z components are equal.
function AffineVectorEquals(const V1, V2: TVector) : Boolean; overload;
//: True if x=y=z=0, w ignored
function VectorIsNull(const v : TVector) : Boolean; overload;
//: True if x=y=z=0, w ignored
function VectorIsNull(const v : TAffineVector) : Boolean; overload;

{: Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y]), also know as "Norm1".<p> }
function VectorSpacing(const v1, v2 : TTexPoint): Single; overload;
{: Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".<p> }
function VectorSpacing(const v1, v2 : TAffineVector): Single; overload;
{: Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".<p> }
function VectorSpacing(const v1, v2 : TVector): Single; overload;

{: Calculates distance between two vectors.<p>
   ie. sqrt(sqr(v1[x]-v2[x])+...) }
function VectorDistance(const v1, v2 : TAffineVector): Single; overload;
{: Calculates distance between two vectors.<p>
   ie. sqrt(sqr(v1[x]-v2[x])+...) (w component ignored) }
function VectorDistance(const v1, v2 : TVector): Single; overload;

{: Calculates the "Norm 2" between two vectors.<p>
   ie. sqr(v1[x]-v2[x])+... }
function VectorDistance2(const v1, v2 : TAffineVector): Single; overload;
{: Calculates the "Norm 2" between two vectors.<p>
   ie. sqr(v1[x]-v2[x])+... (w component ignored) }
function VectorDistance2(const v1, v2 : TVector): Single; overload;

{: Calculates a vector perpendicular to N.<p>
   N is assumed to be of unit length, subtract out any component parallel to N }
function VectorPerpendicular(const V, N: TAffineVector): TAffineVector;
//: Reflects vector V against N (assumes N is normalized)
function VectorReflect(const V, N: TAffineVector): TAffineVector;
//: Rotates Vector about Axis with Angle radians
procedure RotateVector(var vector : TVector; const axis : TAffineVector; angle : Single); overload;
//: Rotates Vector about Axis with Angle radians
procedure RotateVector(var vector : TVector; const axis : TVector; angle : Single); overload;

//: Rotate given vector around the Y axis (alpha is in rad)
procedure RotateVectorAroundY(var v : TAffineVector; alpha : Single);
//: Returns given vector rotated around the X axis (alpha is in rad)
function VectorRotateAroundX(const v : TAffineVector; alpha : Single) : TAffineVector; overload;
//: Returns given vector rotated around the Y axis (alpha is in rad)
function VectorRotateAroundY(const v : TAffineVector; alpha : Single) : TAffineVector; overload;
//: Returns given vector rotated around the Y axis in vr (alpha is in rad)
procedure VectorRotateAroundY(const v : TAffineVector; alpha : Single; var vr : TAffineVector); overload;
//: Returns given vector rotated around the Z axis (alpha is in rad)
function VectorRotateAroundZ(const v : TAffineVector; alpha : Single) : TAffineVector; overload;

//: Vector components are replaced by their Abs() value. }
procedure AbsVector(var v : TVector); overload;
//: Vector components are replaced by their Abs() value. }
procedure AbsVector(var v : TAffineVector); overload;
//: Returns a vector with components replaced by their Abs value. }
function VectorAbs(const v : TVector) : TVector; overload;
//: Returns a vector with components replaced by their Abs value. }
function VectorAbs(const v : TAffineVector) : TAffineVector; overload;

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

procedure SetMatrix(var dest : THomogeneousDblMatrix; const src : TMatrix); overload;
procedure SetMatrix(var dest : TAffineMatrix; const src : TMatrix); overload;
procedure SetMatrix(var dest : TMatrix; const src : TAffineMatrix); overload;

procedure SetMatrixRow(var dest : TMatrix; rowNb : Integer; const aRow : TVector); overload;

//: Creates scale matrix
function CreateScaleMatrix(const v : TAffineVector) : TMatrix; overload;
//: Creates scale matrix
function CreateScaleMatrix(const v : TVector) : TMatrix; overload;
//: Creates translation matrix
function CreateTranslationMatrix(const V : TAffineVector): TMatrix; overload;
//: Creates translation matrix
function CreateTranslationMatrix(const V : TVector): TMatrix; overload;
{: Creates a scale+translation matrix.<p>
   Scale is applied BEFORE applying offset }
function CreateScaleAndTranslationMatrix(const scale, offset : TVector): TMatrix; overload;
//: Creates matrix for rotation about x-axis (angle in rad)
function CreateRotationMatrixX(const sine, cosine: Single) : TMatrix; overload;
function CreateRotationMatrixX(const angle: Single) : TMatrix; overload;
//: Creates matrix for rotation about y-axis (angle in rad)
function CreateRotationMatrixY(const sine, cosine: Single) : TMatrix; overload;
function CreateRotationMatrixY(const angle: Single) : TMatrix; overload;
//: Creates matrix for rotation about z-axis (angle in rad)
function CreateRotationMatrixZ(const sine, cosine: Single) : TMatrix; overload;
function CreateRotationMatrixZ(const angle: Single) : TMatrix; overload;
//: Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateRotationMatrix(const anAxis : TAffineVector; angle : Single) : TMatrix; overload;
function CreateRotationMatrix(const anAxis : TVector; angle : Single) : TMatrix; overload;
//: Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateAffineRotationMatrix(const anAxis: TAffineVector; angle: Single): TAffineMatrix;

//: Multiplies two 3x3 matrices
function MatrixMultiply(const M1, M2 : TAffineMatrix) : TAffineMatrix; overload;
//: Multiplies two 4x4 matrices
function MatrixMultiply(const M1, M2 : TMatrix) : TMatrix; overload;
//: Multiplies M1 by M2 and places result in MResult
procedure MatrixMultiply(const M1, M2 : TMatrix; var MResult : TMatrix); overload;

//: Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const V: TVector; const M: TMatrix): TVector; overload;
//: Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const V: TVector; const M: TAffineMatrix): TVector; overload;
//: Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const V: TAffineVector; const M: TMatrix): TAffineVector; overload;
//: Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const V: TAffineVector; const M: TAffineMatrix): TAffineVector; overload;

//: Determinant of a 3x3 matrix
function MatrixDeterminant(const M: TAffineMatrix): Single; overload;
//: Determinant of a 4x4 matrix
function MatrixDeterminant(const M: TMatrix): Single; overload;

{: Adjoint of a 4x4 matrix.<p>
   used in the computation of the inverse of a 4x4 matrix }
procedure AdjointMatrix(var M : TMatrix); overload;
{: Adjoint of a 3x3 matrix.<p>
   used in the computation of the inverse of a 3x3 matrix }
procedure AdjointMatrix(var M : TAffineMatrix); overload;

//: Multiplies all elements of a 3x3 matrix with a factor
procedure ScaleMatrix(var M : TAffineMatrix; const factor : Single); overload;
//: Multiplies all elements of a 4x4 matrix with a factor
procedure ScaleMatrix(var M : TMatrix; const factor : Single); overload;

//: Adds the translation vector into the matrix
procedure TranslateMatrix(var M : TMatrix; const v : TAffineVector); overload;
procedure TranslateMatrix(var M : TMatrix; const v : TVector); overload;

{: Normalize the matrix and remove the translation component.<p>
   The resulting matrix is an orthonormal matrix (Y direction preserved, then Z) }
procedure NormalizeMatrix(var M : TMatrix);

//: Computes transpose of 3x3 matrix
procedure TransposeMatrix(var M: TAffineMatrix); overload;
//: Computes transpose of 4x4 matrix
procedure TransposeMatrix(var M: TMatrix); overload;

//: Finds the inverse of a 4x4 matrix
procedure InvertMatrix(var M : TMatrix); overload;
//: Finds the inverse of a 3x3 matrix;
procedure InvertMatrix(var M : TAffineMatrix); overload;

{: Finds the inverse of an angle preserving matrix.<p>
   Angle preserving matrices can combine translation, rotation and isotropic
   scaling, other matrices won't be properly inverted by this function. }  
function AnglePreservingMatrixInvert(const mat : TMatrix) : TMatrix;

{: Decompose a non-degenerated 4x4 transformation matrix into the sequence of transformations that produced it.<p>
   Modified by ml then eg, original Author: Spencer W. Thomas, University of Michigan<p>
   The coefficient of each transformation is returned in the corresponding
   element of the vector Tran.<p>
   Returns true upon success, false if the matrix is singular. }
function  MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;

//------------------------------------------------------------------------------
// Plane functions
//------------------------------------------------------------------------------

//: Computes the parameters of a plane defined by three points.
function PlaneMake(const p1, p2, p3 : TAffineVector) : THmgPlane; overload;
function PlaneMake(const p1, p2, p3 : TVector) : THmgPlane;overload;
//: Computes the parameters of a plane defined by a point and a normal.
function PlaneMake(const point, normal : TAffineVector) : THmgPlane; overload;
function PlaneMake(const point, normal : TVector) : THmgPlane; overload;
//: Converts from single to double representation
procedure SetPlane(var dest : TDoubleHmgPlane; const src : THmgPlane);

//: Normalize a plane so that point evaluation = plane distance. }
procedure NormalizePlane(var plane : THmgPlane);

{: Calculates the cross-product between the plane normal and plane to point vector.<p>
   This functions gives an hint as to were the point is, if the point is in the
   half-space pointed by the vector, result is positive.<p>
   This function performs an homogeneous space dot-product. }
function PlaneEvaluatePoint(const plane : THmgPlane; const point : TAffineVector) : Single; overload;
function PlaneEvaluatePoint(const plane : THmgPlane; const point : TVector) : Single; overload;

{: Calculate the normal of a plane defined by three points. }
function CalcPlaneNormal(const p1, p2, p3 : TAffineVector) : TAffineVector; overload;
procedure CalcPlaneNormal(const p1, p2, p3 : TAffineVector; var vr : TAffineVector); overload;
procedure CalcPlaneNormal(const p1, p2, p3 : TVector; var vr : TAffineVector); overload;

{: Returns true if point is in the half-space defined by a plane with normal.<p>
   The plane itself is not considered to be in the tested halfspace. }
function PointIsInHalfSpace(const point, planePoint, planeNormal : TVector) : Boolean;overload;
function PointIsInHalfSpace(const point, planePoint, planeNormal : TAffineVector) : Boolean; overload;

{: Computes algebraic distance between point and plane.<p>
   Value will be positive if the point is in the halfspace pointed by the normal,
   negative on the other side. }
function PointPlaneDistance(const point, planePoint, planeNormal : TVector) : Single; overload;
function PointPlaneDistance(const point, planePoint, planeNormal : TAffineVector) : Single; overload;

{: Computes closest point on a segment (a segment is a limited line).}
function PointSegmentClosestPoint(const point, segmentStart, segmentStop : TAffineVector) : TAffineVector;
{: Computes algebraic distance between segment and line (a segment is a limited line).}
function PointSegmentDistance(const point, segmentStart, segmentStop : TAffineVector) : single;
{: Computes closest point on a line.}
function PointLineClosestPoint(const point, linePoint, lineDirection : TAffineVector) : TAffineVector;
{: Computes algebraic distance between point and line.}
function PointLineDistance(const point, linePoint, lineDirection : TAffineVector) : Single;

{: Computes the closest points (2) given two segments.}
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop : TAffineVector; var Segment0Closest, Segment1Closest : TAffineVector);

{: Computes the closest distance between two segments.}
function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop : TAffineVector) : single;

//------------------------------------------------------------------------------
// Quaternion functions
//------------------------------------------------------------------------------

type
   TEulerOrder = (eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX);

//: Creates a quaternion from the given values
function QuaternionMake(const Imag: array of Single; Real : Single) : TQuaternion;
//: Returns the conjugate of a quaternion
function QuaternionConjugate(const Q : TQuaternion) : TQuaternion;
//: Returns the magnitude of the quaternion
function QuaternionMagnitude(const Q : TQuaternion) : Single;
//: Normalizes the given quaternion
procedure NormalizeQuaternion(var Q : TQuaternion);

//: Constructs a unit quaternion from two points on unit sphere
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
//: Converts a unit quaternion into two points on a unit sphere
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);
//: Constructs a unit quaternion from a rotation matrix
function QuaternionFromMatrix(const mat : TMatrix) : TQuaternion;
{: Constructs a rotation matrix from (possibly non-unit) quaternion.<p>
   Assumes matrix is used to multiply column vector on the left:<br>
   vnew = mat vold.<p>
   Works correctly for right-handed coordinate system and right-handed rotations. }
function QuaternionToMatrix(quat : TQuaternion) : TMatrix;
{: Constructs an affine rotation matrix from (possibly non-unit) quaternion.<p> }
function QuaternionToAffineMatrix(quat : TQuaternion) : TAffineMatrix;
//: Constructs quaternion from angle (in deg) and axis
function QuaternionFromAngleAxis(const angle  : Single; const axis : TAffineVector) : TQuaternion;
//: Constructs quaternion from Euler angles
function QuaternionFromRollPitchYaw(const r, p, y : Single) : TQuaternion;
//: Constructs quaternion from Euler angles in arbitrary order (angles in degrees)
function QuaternionFromEuler(const x, y, z: Single; eulerOrder : TEulerOrder) : TQuaternion;

{: Returns quaternion product qL * qR.<p>
   Note: order is important!<p>
   To combine rotations, use the product QuaternionMuliply(qSecond, qFirst),
   which gives the effect of rotating by qFirst then qSecond. }
function QuaternionMultiply(const qL, qR : TQuaternion): TQuaternion;

{: Spherical linear interpolation of unit quaternions with spins.<p>
   QStart, QEnd - start and end unit quaternions<br>
   t            - interpolation parameter (0 to 1)<br>
   Spin         - number of extra spin rotations to involve<br> }
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion; overload;
function QuaternionSlerp(const source, dest: TQuaternion; const t : Single) : TQuaternion; overload;

//------------------------------------------------------------------------------
// Logarithmic and exponential functions
//------------------------------------------------------------------------------

{: Return ln(1 + X),  accurate for X near 0. }
function LnXP1(X: Extended): Extended;
{: Log base 10 of X}
function Log10(X: Extended): Extended;
{: Log base 2 of X }
function Log2(X: Extended): Extended; overload;
{: Log base 2 of X }
function Log2(X: Single): Single; overload;
{: Log base N of X }
function LogN(Base, X: Extended): Extended;
{: Raise base to an integer. }
function IntPower(Base: Extended; Exponent: Integer): Extended;
{: Raise base to any power.<p>
   For fractional exponents, or |exponents| > MaxInt, base must be > 0. }
function Power(const Base, Exponent: Single): Single; overload;
{: Raise base to an integer. }
function Power(Base: Single; Exponent: Integer): Single; overload;

//------------------------------------------------------------------------------
// Trigonometric functions
//------------------------------------------------------------------------------

function DegToRad(const Degrees: Extended): Extended; overload;
function DegToRad(const Degrees: Single): Single; overload;
function RadToDeg(const Radians: Extended): Extended; overload;
function RadToDeg(const Radians: Single): Single; overload;

//: Normalize to an angle in the [-PI; +PI] range
function NormalizeAngle(angle : Single) : Single;
//: Normalize to an angle in the [-180; 180] range
function NormalizeDegAngle(angle : Single) : Single;

//: Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: Extended; var Sin, Cos: Extended); overload;
//: Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: Double; var Sin, Cos: Double); overload;
//: Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: Single; var Sin, Cos: Single); overload;
{: Calculates sine and cosine from the given angle Theta and Radius.<p>
   sin and cos values calculated from theta are multiplicated by radius. }
procedure SinCos(const theta, radius : Double; var Sin, Cos: Extended); overload;
{: Calculates sine and cosine from the given angle Theta and Radius.<p>
   sin and cos values calculated from theta are multiplicated by radius. }
procedure SinCos(const theta, radius : Double; var Sin, Cos: Double); overload;
{: Calculates sine and cosine from the given angle Theta and Radius.<p>
   sin and cos values calculated from theta are multiplicated by radius. }
procedure SinCos(const theta, radius : Single; var Sin, Cos: Single); overload;

{: Fills up the two given dynamic arrays with sin cos values.<p>
   start and stop angles must be given in degrees, the number of steps is
   determined by the length of the given arrays. }
procedure PrepareSinCosCache(var s, c : array of Single;
                             startAngle, stopAngle : Single);

function  ArcCos(const X: Extended) : Extended; overload;
function  ArcCos(const x : Single) : Single; overload;
function  ArcSin(const X : Extended) : Extended; overload;
function  ArcSin(const X : Single) : Single; overload;
function  ArcTan2(const Y, X : Extended) : Extended; overload;
function  ArcTan2(const Y, X : Single) : Single; overload;
{: Fast ArcTan2 approximation, about 0.07 rads accuracy. }
function  FastArcTan2(y, x : Single) : Single;
function  Tan(const X : Extended) : Extended; overload;
function  Tan(const X : Single) : Single; overload;
function  CoTan(const X : Extended) : Extended; overload;
function  CoTan(const X : Single) : Single; overload;

//------------------------------------------------------------------------------
// Hyperbolic Trigonometric functions
//------------------------------------------------------------------------------

function  Sinh(const x : Single) : Single; overload;
function  Sinh(const x : Double) : Double; overload;
function  Cosh(const x : Single) : Single; overload;
function  Cosh(const x : Double) : Double; overload;

//------------------------------------------------------------------------------
// Miscellanious math functions
//------------------------------------------------------------------------------

{: Computes 1/Sqrt(v).<p> }
function RSqrt(v : Single) : Single;
{: Computes 1/Sqrt(Sqr(x)+Sqr(y)). }
function RLength(x, y : Single) : Single;
{: Computes an integer sqrt approximation.<p> }
function ISqrt(i : Integer) : Integer;
{: Computes an integer length Result:=Sqrt(x*x+y*y). }
function ILength(x, y : Integer) : Integer; overload;
function ILength(x, y, z : Integer) : Integer; overload;

{$ifndef GEOMETRY_NO_ASM}
{: Computes Exp(ST(0)) and leaves result on ST(0) }
procedure RegisterBasedExp;
{$endif}

{: Generates a random point on the unit sphere.<p>
   Point repartition is correctly isotropic with no privilegied direction. }
procedure RandomPointOnSphere(var p : TAffineVector);

{: Rounds the floating point value to the closest integer.<p>
   Behaves like Round but returns a floating point value like Int. }
function RoundInt(v : Single) : Single; overload;
function RoundInt(v : Extended) : Extended; overload;

{$ifndef GEOMETRY_NO_ASM}
function Trunc(v : Single) : Integer; overload;
function Trunc64(v : Extended) : Int64; overload;
function Int(v : Single) : Single; overload;
function Int(v : Extended) : Extended; overload;
function Frac(v : Single) : Single; overload;
function Frac(v : Extended) : Extended; overload;
function Round(v : Single) : Integer; overload;
function Round64(v : Single) : Int64; overload;
function Round64(v : Extended) : Int64; overload;
{$else}
function Trunc(X: Extended): Int64;
function Round(X: Extended): Int64;
function Frac(X: Extended): Extended;
{$endif}

function Ceil(v : Single) : Integer; overload;
function Ceil64(v : Extended) : Int64; overload;
function Floor(v : Single) : Integer; overload;
function Floor64(v : Extended) : Int64; overload;

{: Multiples i by s and returns the rounded result.<p> }
function ScaleAndRound(i : Integer; var s : Single) : Integer;

{: Returns the sign of the x value using the (-1, 0, +1) convention }
function Sign(x : Single) : Integer;

{: Returns True if x is in [a; b] }
function IsInRange(const x, a, b : Single) : Boolean; overload;
function IsInRange(const x, a, b : Double) : Boolean; overload;

{: Returns True if p is in the cube defined by d. }
function IsInCube(const p, d : TAffineVector) : Boolean; overload;
function IsInCube(const p, d : TVector) : Boolean; overload;

{: Returns the minimum value of the array. }
function MinFloat(values : PSingleArray; nbItems : Integer) : Single; overload;
function MinFloat(values : PDoubleArray; nbItems : Integer) : Double; overload;
function MinFloat(values : PExtendedArray; nbItems : Integer) : Extended; overload;
{: Returns the minimum of given values. }
function MinFloat(const v1, v2 : Single) : Single; overload;
function MinFloat(const v : array of Single) : Single; overload;
function MinFloat(const v1, v2 : Double) : Double; overload;
function MinFloat(const v1, v2 : Extended) : Extended; overload;
function MinFloat(const v1, v2, v3 : Single) : Single; overload;
function MinFloat(const v1, v2, v3 : Double) : Double; overload;
function MinFloat(const v1, v2, v3 : Extended) : Extended; overload;
{: Returns the maximum value of the array. }
function MaxFloat(values : PSingleArray; nbItems : Integer) : Single; overload;
function MaxFloat(values : PDoubleArray; nbItems : Integer) : Double; overload;
function MaxFloat(values : PExtendedArray; nbItems : Integer) : Extended; overload;
function MaxFloat(const v : array of Single) : Single; overload;
{: Returns the maximum of given values. }
function MaxFloat(const v1, v2 : Single) : Single; overload;
function MaxFloat(const v1, v2 : Double) : Double; overload;
function MaxFloat(const v1, v2 : Extended) : Extended; overload;
function MaxFloat(const v1, v2, v3 : Single) : Single; overload;
function MaxFloat(const v1, v2, v3 : Double) : Double; overload;
function MaxFloat(const v1, v2, v3 : Extended) : Extended; overload;

function MinInteger(const v1, v2 : Integer) : Integer; overload;
function MinInteger(const v1, v2 : Cardinal) : Cardinal; overload;

function MaxInteger(const v1, v2 : Integer) : Integer; overload;
function MaxInteger(const v1, v2 : Cardinal) : Cardinal; overload;

{: Computes the triangle's area. }
function TriangleArea(const p1, p2, p3 : TAffineVector) : Single; overload;
{: Computes the polygons's area.<p>
   Points must be coplanar. Polygon needs not be convex. }
function PolygonArea(const p : PAffineVectorArray; nSides : Integer) : Single; overload;
{: Computes a 2D triangle's signed area.<p>
   Only X and Y coordinates are used, Z is ignored. }
function TriangleSignedArea(const p1, p2, p3 : TAffineVector) : Single; overload;
{: Computes a 2D polygon's signed area.<p>
   Only X and Y coordinates are used, Z is ignored. Polygon needs not be convex. }
function PolygonSignedArea(const p : PAffineVectorArray; nSides : Integer) : Single; overload;

{: Multiplies values in the array by factor.<p>
   This function is especially efficient for large arrays, it is not recommended
   for arrays that have less than 10 items.<br>
   Expected performance is 4 to 5 times that of a Deliph-compiled loop on AMD
   CPUs, and 2 to 3 when 3DNow! isn't available. }
procedure ScaleFloatArray(values : PSingleArray; nb : Integer;
                          var factor : Single); overload;
procedure ScaleFloatArray(var values : TSingleArray;
                          factor : Single); overload;

{: Adds delta to values in the array.<p>
   Array size must be a multiple of four. }
procedure OffsetFloatArray(values : PSingleArray; nb : Integer;
                           var delta : Single); overload;
procedure OffsetFloatArray(var values : array of Single;
                           delta : Single); overload;
procedure OffsetFloatArray(valuesDest, valuesDelta : PSingleArray; nb : Integer); overload;

{: Returns the max of the X, Y and Z components of a vector (W is ignored). }
function MaxXYZComponent(const v : TVector) : Single; overload;
function MaxXYZComponent(const v: TAffineVector): single; overload;
{: Returns the min of the X, Y and Z components of a vector (W is ignored). }
function MinXYZComponent(const v : TVector) : Single;overload;
function MinXYZComponent(const v: TAffineVector): single; overload;
{: Returns the max of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored). }
function MaxAbsXYZComponent(v : TVector) : Single;
{: Returns the min of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored). }
function MinAbsXYZComponent(v : TVector) : Single;
{: Replace components of v with the max of v or v1 component.<p>
   Maximum is computed per component. }
procedure MaxVector(var v : TVector; const v1 : TVector); overload;
procedure MaxVector(var v : TAffineVector; const v1 : TAffineVector); overload;
{: Replace components of v with the min of v or v1 component.<p>
   Minimum is computed per component. }
procedure MinVector(var v : TVector; const v1 : TVector); overload;
procedure MinVector(var v : TAffineVector; const v1 : TAffineVector); overload;

{: Sorts given array in ascending order.<p>
   NOTE : current implementation is a slow bubble sort... }
procedure SortArrayAscending(var a : array of Extended);

{: Clamps aValue in the aMin-aMax interval.<p> }
function ClampValue(const aValue, aMin, aMax : Single) : Single; overload;
{: Clamps aValue in the aMin-INF interval.<p> }
function ClampValue(const aValue, aMin : Single) : Single; overload;

{: Returns the detected optimization mode.<p>
   Returned values is either 'FPU', '3DNow!' or 'SSE'. }
function GeometryOptimizationMode : String;

{: Begins a FPU-only section.<p>
   You can use a FPU-only section to force use of FPU versions of the math
   functions, though typically slower than their SIMD counterparts, they have
   a higher precision (80 bits internally) that may be required in some cases.<p>
   Each BeginFPUOnlySection call must be balanced by a EndFPUOnlySection (calls
   can be nested). }
procedure BeginFPUOnlySection;
{: Ends a FPU-only section.<p>
   See BeginFPUOnlySection. }
procedure EndFPUOnlySection;

//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines

// mixed functions

{: Turn a triplet of rotations about x, y, and z (in that order) into an equivalent rotation around a single axis (all in radians).<p> }
function ConvertRotation(const Angles : TAffineVector) : TVector;

// miscellaneous functions

function MakeAffineDblVector(var v : array of Double) : TAffineDblVector;
function MakeDblVector(var v : array of Double) : THomogeneousDblVector;
function VectorAffineDblToFlt(const v : TAffineDblVector) : TAffineVector;
function VectorDblToFlt(const v : THomogeneousDblVector) : THomogeneousVector;
function VectorAffineFltToDbl(const v : TAffineVector) : TAffineDblVector;
function VectorFltToDbl(const v : TVector): THomogeneousDblVector;

function PointInPolygon(var xp, yp : array of Single; x, y : Single) : Boolean;

procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);

// coordinate system manipulation functions

//: Rotates the given coordinate system (represented by the matrix) around its Y-axis
function Turn(const Matrix: TMatrix; angle : Single) : TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around MasterUp
function Turn(const Matrix: TMatrix; const MasterUp : TAffineVector; Angle : Single) : TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around its X-axis
function Pitch(const Matrix: TMatrix; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around MasterRight
function Pitch(const Matrix: TMatrix; const MasterRight: TAffineVector; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around its Z-axis
function Roll(const Matrix: TMatrix; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around MasterDirection
function Roll(const Matrix: TMatrix; const MasterDirection: TAffineVector; Angle: Single): TMatrix; overload;

// intersection functions

{: Compute the intersection point "res" of a line with a plane.<p>
   Return value:<ul>
   <li>0 : no intersection, line parallel to plane
   <li>1 : res is valid
   <li>-1 : line is inside plane
   </ul><br>
   Adapted from:<br>
   E.Hartmann, Computerunterstützte Darstellende Geometrie, B.G. Teubner Stuttgart 1988 }
function IntersectLinePlane(const point, direction : TVector;
                            const plane : THmgPlane;
                            intersectPoint : PVector = nil) : Integer; overload;

{: Compute intersection between a ray and a plane.<p>
   Returns True if an intersection was found, the intersection point is placed
   in intersectPoint is the reference is not nil. }
function RayCastPlaneIntersect(const rayStart, rayVector : TVector;
                               const planePoint, planeNormal : TVector;
                               intersectPoint : PVector = nil) : Boolean; overload;
function RayCastPlaneXZIntersect(const rayStart, rayVector : TVector;
                                 const planeY : Single;
                                 intersectPoint : PVector = nil) : Boolean; overload;

{: Compute intersection between a ray and a triangle. }
function RayCastTriangleIntersect(const rayStart, rayVector : TVector;
                                  const p1, p2, p3 : TAffineVector;
                                  intersectPoint : PVector = nil;
                                  intersectNormal : PVector = nil) : Boolean; overload;
{: Compute the min distance a ray will pass to a point.<p> }
function RayCastMinDistToPoint(const rayStart, rayVector : TVector;
                               const point : TVector) : Single;
{: Determines if a ray will intersect with a given sphere.<p> }
function RayCastIntersectsSphere(const rayStart, rayVector : TVector;
                                 const sphereCenter : TVector;
                                 const sphereRadius : Single) : Boolean; overload;
{: Calculates the intersections between a sphere and a ray.<p>
   Returns 0 if no intersection is found (i1 and i2 untouched), 1 if one
   intersection was found (i1 defined, i2 untouched), and 2 is two intersections
   were found (i1 and i2 defined). }
function RayCastSphereIntersect(const rayStart, rayVector : TVector;
                                const sphereCenter : TVector;
                                const sphereRadius : Single;
                                var i1, i2 : TVector) : Integer; overload;
{: Computes the visible radius of a sphere in a perspective projection.<p>
   This radius can be used for occlusion culling (cone extrusion) or 2D
   intersection testing. }
function SphereVisibleRadius(distance, radius : Single) : Single;

{: Extracts a TFrustum for combined modelview and projection matrices. }
function ExtractFrustumFromModelViewProjection(const modelViewProj : TMatrix) : TFrustum;

//: Determines if volume is clipped or not
function IsVolumeClipped(const objPos : TVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;
function IsVolumeClipped(const min, max : TAffineVector;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const Frustum : TFrustum) : Boolean; overload;

// misc funcs

{: Creates a parallel projection matrix.<p>
   Transformed points will projected on the plane along the specified direction. }
function MakeParallelProjectionMatrix(const plane : THmgPlane;
                                      const dir : TVector) : TMatrix;

{: Creates a shadow projection matrix.<p>
   Shadows will be projected onto the plane defined by planePoint and planeNormal,
   from lightPos. }
function MakeShadowMatrix(const planePoint, planeNormal, lightPos : TVector) : TMatrix;

{: Builds a reflection matrix for the given plane.<p>
   Reflection matrix allow implementing planar reflectors in OpenGL (mirrors). }
function MakeReflectionMatrix(const planePoint, planeNormal : TAffineVector) : TMatrix;

{: Packs an homogeneous rotation matrix to 6 bytes.<p>
   The 6:64 (or 6:36) compression ratio is achieved by computing the quaternion
   associated to the matrix and storing its Imaginary components at 16 bits
   precision each.<br>
   Deviation is typically below 0.01% and around 0.1% in worst case situations.<p>
   Note: quaternion conversion is faster and more robust than an angle decomposition. }
function PackRotationMatrix(const mat : TMatrix) : TPackedRotationMatrix;
{: Restores a packed rotation matrix.<p>
   See PackRotationMatrix. }
function UnPackRotationMatrix(const packedMatrix : TPackedRotationMatrix) : TMatrix;

const
   cPI       : Single =  3.141592654;
   cPIdiv180 : Single =  0.017453292;
   c180divPI : Single = 57.29577951;
   c2PI :      Single =  6.283185307;
   cPIdiv2 :   Single =  1.570796326;
   cPIdiv4 :   Single =  0.785398163;
   c3PIdiv4 :  Single =  2.35619449;
   cInv2PI :   Single = 1/6.283185307;
   cInv360 :   Single = 1/360;
   c180 :      Single = 180;
   c360 :      Single = 360;
   cOneHalf :  Single = 0.5;
   cLn10 :     Single = 2.302585093;

   // Ranges of the IEEE floating point types, including denormals
   // with Math.pas compatible name
   MinSingle   =  1.5e-45;
   MaxSingle   =  3.4e+38;
   MinDouble   =  5.0e-324;
   MaxDouble   =  1.7e+308;
   MinExtended =  3.4e-4932;
   MaxExtended =  1.1e+4932;
   MinComp     = -9.223372036854775807e+18;
   MaxComp     =  9.223372036854775807e+18;

var
   // this var is adjusted during "initialization", current values are
   // + 0 : use standard optimized FPU code
   // + 1 : use 3DNow! optimized code (requires K6-2/3 CPU)
   // + 2 : use Intel SSE code (Pentium III, NOT IMPLEMENTED YET !)
   vSIMD : Byte = 0;


//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

uses SysUtils{$ifdef GEOMETRY_NO_ASM}, Math{$endif};

const
{$ifndef GEOMETRY_NO_ASM}
  // FPU status flags (high order byte)
  cwChop : Word = $1F3F;
{$endif}

  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

  cZero : Single = 0.0;
  cOne : Single = 1.0;
  cOneDotFive : Single = 0.5;

// OptimizationMode
//
function GeometryOptimizationMode : String;
begin
   case vSIMD of
      0 : Result:='FPU';
      1 : Result:='3DNow!';
      2 : Result:='SSE';
   else
      Result:='*ERR*';
   end;
end;

// BeginFPUOnlySection
//
var
   vOldSIMD : Byte;
   vFPUOnlySectionCounter : Integer;
procedure BeginFPUOnlySection;
begin
   if vFPUOnlySectionCounter=0 then
      vOldSIMD:=vSIMD;
   Inc(vFPUOnlySectionCounter);
   vSIMD:=0;
end;

// EndFPUOnlySection
//
procedure EndFPUOnlySection;
begin
   Dec(vFPUOnlySectionCounter);
   Assert(vFPUOnlySectionCounter>=0);
   if vFPUOnlySectionCounter=0 then
      vSIMD:=vOldSIMD;
end;

//------------------------------------------------------------------------------
//----------------- vector functions -------------------------------------------
//------------------------------------------------------------------------------

// TexPointMake
//
function TexPointMake(const s, t : Single) : TTexPoint;
begin
   Result.S:=s;
   Result.T:=t;
end;

// AffineVectorMake
//
function AffineVectorMake(const x, y, z : Single) : TAffineVector; overload;
begin
   Result[0]:=x;
   Result[1]:=y;
   Result[2]:=z;
end;

// AffineVectorMake
//
function AffineVectorMake(const v : TVector) : TAffineVector;
begin
   Result[0]:=v[0];
   Result[1]:=v[1];
   Result[2]:=v[2];
end;

// SetAffineVector
//
procedure SetAffineVector(var v : TAffineVector; const x, y, z : Single); overload;
begin
   v[0]:=x;
   v[1]:=y;
   v[2]:=z;
end;

// SetVector (affine)
//
procedure SetVector(var v : TAffineVector; const x, y, z : Single);
begin
   v[0]:=x;
   v[1]:=y;
   v[2]:=z;
end;

// SetVector (affine-hmg)
//
procedure SetVector(var v : TAffineVector; const vSrc : TVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// SetVector (affine-affine)
//
procedure SetVector(var v : TAffineVector; const vSrc : TAffineVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// SetVector (affine double - affine single)
//
procedure SetVector(var v : TAffineDblVector; const vSrc : TAffineVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// SetVector (affine double - hmg single)
//
procedure SetVector(var v : TAffineDblVector; const vSrc : TVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// VectorMake
//
function VectorMake(const v : TAffineVector; w : Single = 0) : TVector;
begin
	Result[0]:=v[0];
	Result[1]:=v[1];
	Result[2]:=v[2];
	Result[3]:=w;
end;

// VectorMake
//
function VectorMake(const x, y, z : Single; w : Single = 0) : TVector;
begin
	Result[0]:=x;
	Result[1]:=y;
	Result[2]:=z;
	Result[3]:=w;
end;

// PointMake (xyz)
//
function PointMake(const x, y, z: Single) : TVector; overload;
begin
	Result[0]:=x;
	Result[1]:=y;
	Result[2]:=z;
   Result[3]:=1;
end;

// PointMake (affine)
//
function PointMake(const v : TAffineVector) : TVector; overload;
begin
	Result[0]:=v[0];
	Result[1]:=v[1];
	Result[2]:=v[2];
   Result[3]:=1;
end;

// PointMake (hmg)
//
function PointMake(const v : TVector) : TVector; overload;
begin
	Result[0]:=v[0];
	Result[1]:=v[1];
	Result[2]:=v[2];
   Result[3]:=1;
end;

// SetVector
//
procedure SetVector(var v : TVector; const x, y, z : Single; w : Single = 0);
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
	v[3]:=w;
end;

// SetVector
//
procedure SetVector(var v : TVector; const av : TAffineVector; w : Single = 0);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=w;
end;

// SetVector
//
procedure SetVector(var v : TVector; const vSrc : TVector);
begin
   // faster than memcpy, move or ':=' on the TVector...
	v[0]:=vSrc[0];
	v[1]:=vSrc[1];
	v[2]:=vSrc[2];
	v[3]:=vSrc[3];
end;

// MakePoint
//
procedure MakePoint(var v : TVector; const x, y, z: Single);
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
	v[3]:=cOne;
end;

// MakePoint
//
procedure MakePoint(var v : TVector; const av : TAffineVector);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=cOne;
end;

// MakePoint
//
procedure MakePoint(var v : TVector; const av : TVector);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=cOne;
end;

// MakeVector
//
procedure MakeVector(var v : TAffineVector; const x, y, z: Single); overload;
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
end;

// MakeVector
//
procedure MakeVector(var v : TVector; const x, y, z: Single);
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
	v[3]:=cZero;
end;

// MakeVector
//
procedure MakeVector(var v : TVector; const av : TAffineVector);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=cZero;
end;

// MakeVector
//
procedure MakeVector(var v : TVector; const av : TVector);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=cZero;
end;

// RstVector (affine)
//
procedure RstVector(var v : TAffineVector);
{$ifndef GEOMETRY_NO_ASM}
asm
         xor   edx, edx
         mov   [eax], edx
         mov   [eax+4], edx
         mov   [eax+8], edx
{$else}
begin
   v[0]:=0;
   v[1]:=0;
   v[2]:=0;
{$endif}
end;

// RstVector (hmg)
//
procedure RstVector(var v : TVector);
{$ifndef GEOMETRY_NO_ASM}
asm
         xor   edx, edx
         mov   [eax], edx
         mov   [eax+4], edx
         mov   [eax+8], edx
         mov   [eax+12], edx
{$else}
begin
   v[0]:=0;
   v[1]:=0;
   v[2]:=0;
   v[3]:=0;
{$endif}
end;

// VectorAdd (func, affine)
//
function VectorAdd(const v1, v2 : TAffineVector) : TAffineVector;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
{$else}
begin
   Result[0]:=v1[0]+v2[0];
   Result[1]:=v1[1]+v2[1];
   Result[2]:=v1[2]+v2[2];
{$endif}
end;

// VectorAdd (proc, affine)
//
procedure VectorAdd(const v1, v2 : TAffineVector; var vr : TAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
{$else}
begin
   vr[0]:=v1[0]+v2[0];
   vr[1]:=v1[1]+v2[1];
   vr[2]:=v1[2]+v2[2];
{$endif}
end;

// VectorAdd (proc, affine)
//
procedure VectorAdd(const v1, v2 : TAffineVector; vr : PAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
{$else}
begin
   vr^[0]:=v1[0]+v2[0];
   vr^[1]:=v1[1]+v2[1];
   vr^[2]:=v1[2]+v2[2];
{$endif}
end;

// VectorAdd (hmg)
//
function VectorAdd(const v1, v2 : TVector) : TVector;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
         test vSIMD, 1
         jz @@FPU
@@3DNow:
         db $0F,$6F,$00           /// movq  mm0, [eax]
         db $0F,$0F,$02,$9E       /// pfadd mm0, [edx]
         db $0F,$7F,$01           /// movq  [ecx], mm0
         db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
         db $0F,$0F,$4A,$08,$9E   /// pfadd mm1, [edx+8]
         db $0F,$7F,$49,$08       /// movq  [ecx+8], mm1
         db $0F,$0E               /// femms
         ret

@@FPU:
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
         FLD  DWORD PTR [EAX+12]
         FADD DWORD PTR [EDX+12]
         FSTP DWORD PTR [ECX+12]
{$else}
begin
   Result[0]:=v1[0]+v2[0];
   Result[1]:=v1[1]+v2[1];
   Result[2]:=v1[2]+v2[2];
   Result[3]:=v1[3]+v2[3];
{$endif}
end;

// VectorAdd (hmg, proc)
//
procedure VectorAdd(const v1, v2: TVector; var vr : TVector);
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
         test vSIMD, 1
         jz @@FPU
@@3DNow:
         db $0F,$6F,$00           /// movq  mm0, [eax]
         db $0F,$0F,$02,$9E       /// pfadd mm0, [edx]
         db $0F,$7F,$01           /// movq  [ecx], mm0
         db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
         db $0F,$0F,$4A,$08,$9E   /// pfadd mm1, [edx+8]
         db $0F,$7F,$49,$08       /// movq  [ecx+8], mm1
         db $0F,$0E               /// femms
         ret

@@FPU:
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
         FLD  DWORD PTR [EAX+12]
         FADD DWORD PTR [EDX+12]
         FSTP DWORD PTR [ECX+12]
{$else}
begin
   vr[0]:=v1[0]+v2[0];
   vr[1]:=v1[1]+v2[1];
   vr[2]:=v1[2]+v2[2];
   vr[3]:=v1[3]+v2[3];
{$endif}
end;

// VectorAdd (affine, single)
//
function VectorAdd(const v : TAffineVector; const f : Single) : TAffineVector;
begin
   Result[0]:=v[0]+f;
   Result[1]:=v[1]+f;
   Result[2]:=v[2]+f;
end;

// VectorAdd (hmg, single)
//
function VectorAdd(const v : TVector; const f : Single) : TVector;
begin
   Result[0]:=v[0]+f;
   Result[1]:=v[1]+f;
   Result[2]:=v[2]+f;
   Result[3]:=v[3]+f;
end;

// AddVector (affine)
//
procedure AddVector(var v1 : TAffineVector; const v2 : TAffineVector);
// EAX contains address of V1
// EDX contains address of V2
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FADD DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FADD DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FADD DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
{$else}
begin
   v1[0]:=v1[0]+v2[0];
   v1[1]:=v1[1]+v2[1];
   v1[2]:=v1[2]+v2[2];
{$endif}
end;

// AddVector (affine)
//
procedure AddVector(var v1 : TAffineVector; const v2 : TVector);
// EAX contains address of V1
// EDX contains address of V2
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FADD DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FADD DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FADD DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
{$else}
begin
   v1[0]:=v1[0]+v2[0];
   v1[1]:=v1[1]+v2[1];
   v1[2]:=v1[2]+v2[2];
{$endif}
end;

// AddVector (hmg)
//
procedure AddVector(var v1 : TVector; const v2 : TVector);
// EAX contains address of V1
// EDX contains address of V2
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9E       /// PFADD MM0, [EDX]
      db $0F,$7F,$00           /// MOVQ  [EAX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9E   /// PFADD MM1, [EDX+8]
      db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FADD DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FADD DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FADD DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EAX+12]
      FADD DWORD PTR [EDX+12]
      FSTP DWORD PTR [EAX+12]
{$else}
begin
   v1[0]:=v1[0]+v2[0];
   v1[1]:=v1[1]+v2[1];
   v1[2]:=v1[2]+v2[2];
   v1[3]:=v1[3]+v2[3];
{$endif}
end;

// AddVector (affine)
//
procedure AddVector(var v : TAffineVector; const f : Single);
begin
   v[0]:=v[0]+f;
   v[1]:=v[1]+f;
   v[2]:=v[2]+f;
end;

// AddVector (hmg)
//
procedure AddVector(var v : TVector; const f : Single);
begin
   v[0]:=v[0]+f;
   v[1]:=v[1]+f;
   v[2]:=v[2]+f;
   v[3]:=v[3]+f;
end;

// TexPointArrayAdd
//
procedure TexPointArrayAdd(const src : PTexPointArray; const delta : TTexPoint;
                           const nb : Integer;
                           dest : PTexPointArray); overload;
{$ifndef GEOMETRY_NO_ASM}
asm
      or    ecx, ecx
      jz    @@End

      test  vSIMD, 1
      jnz   @@3DNow

      push edi
      mov   edi, dest

@@FPULoop:
      fld   dword ptr [eax]
      fadd  dword ptr [edx]
      fstp  dword ptr [edi]
      fld   dword ptr [eax+4]
      fadd  dword ptr [edx+4]
      fstp  dword ptr [edi+4]

      add   eax, 8
      add   edi, 8
      dec   ecx
      jnz   @@FPULoop

      pop edi
      jmp   @@End

@@3DNow:
      db $0F,$6F,$02           /// movq  mm0, [edx]
      mov   edx, dest

@@3DNowLoop:
      db $0F,$6F,$10           /// movq  mm2, [eax]
      db $0F,$0F,$D0,$9E       /// pfadd mm2, mm0
      db $0F,$7F,$12           /// movq  [edx], mm2

      add   eax, 8
      add   edx, 8
      dec   ecx
      jnz   @@3DNowLoop

      db $0F,$0E               /// femms

@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to nb-1 do begin
      dest^[i].S:=src^[i].S+delta.S;
      dest^[i].T:=src^[i].T+delta.T;
   end;
{$endif}
end;

// TexPointArrayScaleAndAdd
//
procedure TexPointArrayScaleAndAdd(const src : PTexPointArray; const delta : TTexPoint;
                                   const nb : Integer; const scale : TTexPoint;
                                   dest : PTexPointArray); overload;
{$ifndef GEOMETRY_NO_ASM}
asm
      or    ecx, ecx
      jz    @@End

      test  vSIMD, 1
      jnz   @@3DNow

      push  edi
      push  esi
      mov   edi, dest
      mov   esi, scale

@@FPULoop:
      fld   dword ptr [eax]
      fmul  dword ptr [esi]
      fadd  dword ptr [edx]
      fstp  dword ptr [edi]
      fld   dword ptr [eax+4]
      fmul  dword ptr [esi+4]
      fadd  dword ptr [edx+4]
      fstp  dword ptr [edi+4]

      add   eax, 8
      add   edi, 8
      dec   ecx
      jnz   @@FPULoop

      pop   esi
      pop   edi
      jmp   @@End

@@3DNow:
      db $0F,$6F,$02           /// movq  mm0, [edx]
      mov   edx, scale
      db $0F,$6F,$0A           /// movq  mm1, [edx]
      mov   edx, dest

@@3DNowLoop:
      db $0F,$6F,$10           /// movq  mm2, [eax]
      db $0F,$0F,$D1,$B4       /// pfmul mm2, mm1
      db $0F,$0F,$D0,$9E       /// pfadd mm2, mm0
      db $0F,$7F,$12           /// movq  [edx], mm2

      add   eax, 8
      add   edx, 8
      dec   ecx
      jnz   @@3DNowLoop

      db $0F,$0E               /// femms 
@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to nb-1 do begin
      dest^[i].S:=src^[i].S*scale.S+delta.S;
      dest^[i].T:=src^[i].T*scale.T+delta.T;
   end;
{$endif}
end;

// VectorArrayAdd
//
procedure VectorArrayAdd(const src : PAffineVectorArray; const delta : TAffineVector;
                         const nb : Integer; dest : PAffineVectorArray);
{$ifndef GEOMETRY_NO_ASM}
asm
      or    ecx, ecx
      jz    @@End

      test  vSIMD, 1
      jnz   @@3DNow

      push edi
      mov   edi, dest

@@FPULoop:
      fld   dword ptr [eax]
      fadd  dword ptr [edx]
      fstp  dword ptr [edi]
      fld   dword ptr [eax+4]
      fadd  dword ptr [edx+4]
      fstp  dword ptr [edi+4]
      fld   dword ptr [eax+8]
      fadd  dword ptr [edx+8]
      fstp  dword ptr [edi+8]

      add   eax, 12
      add   edi, 12
      dec   ecx
      jnz   @@FPULoop

      pop edi
      jmp   @@End

@@3DNow:
      db $0F,$6F,$02           /// movq  mm0, [edx]
      db $0F,$6E,$4A,$08       /// movd  mm1, [edx+8]
      mov   edx, dest

@@3DNowLoop:
      db $0F,$6F,$10           /// movq  mm2, [eax]
      db $0F,$6E,$58,$08       /// movd  mm3, [eax+8]
      db $0F,$0F,$D0,$9E       /// pfadd mm2, mm0
      db $0F,$0F,$D9,$9E       /// pfadd mm3, mm1
      db $0F,$7F,$12           /// movq  [edx], mm2
      db $0F,$7E,$5A,$08       /// movd  [edx+8], mm3

      add   eax, 12
      add   edx, 12
      dec   ecx
      jnz   @@3DNowLoop

      db $0F,$0E               /// femms

@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to nb-1 do begin
      dest^[i][0]:=src^[i][0]+delta[0];
      dest^[i][1]:=src^[i][1]+delta[1];
      dest^[i][2]:=src^[i][2]+delta[2];
   end;
{$endif}
end;

// VectorSubtract (func, affine)
//
function VectorSubtract(const v1, v2 : TAffineVector): TAffineVector;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
{$else}
begin
   Result[0]:=v1[0]-v2[0];
   Result[1]:=v1[1]-v2[1];
   Result[2]:=v1[2]-v2[2];
{$endif}
end;

// VectorSubtract (proc, affine)
//
procedure VectorSubtract(const v1, v2 : TAffineVector; var result : TAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
{$else}
begin
   result[0]:=v1[0]-v2[0];
   result[1]:=v1[1]-v2[1];
   result[2]:=v1[2]-v2[2];
{$endif}
end;

// VectorSubtract (proc, affine-hmg)
//
procedure VectorSubtract(const v1, v2 : TAffineVector; var result : TVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
      xor   eax, eax
      mov   [ECX+12], eax
{$else}
begin
   result[0]:=v1[0]-v2[0];
   result[1]:=v1[1]-v2[1];
   result[2]:=v1[2]-v2[2];
   result[3]:=0;
{$endif}
end;

// VectorSubtract
//
procedure VectorSubtract(const v1 : TVector; v2 : TAffineVector; var result : TVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
      mov   edx, [eax+12]
      mov   [ECX+12], edx
{$else}
begin
   result[0]:=v1[0]-v2[0];
   result[1]:=v1[1]-v2[1];
   result[2]:=v1[2]-v2[2];
   result[3]:=v1[0];
{$endif}
end;

// VectorSubtract (hmg)
//
function VectorSubtract(const v1, v2 : TVector) : TVector;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
      db $0F,$7F,$01           /// MOVQ  [ECX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
      db $0F,$7F,$49,$08       /// MOVQ  [ECX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FSTP DWORD PTR [ECX+12]
{$else}
begin
   Result[0]:=v1[0]-v2[0];
   Result[1]:=v1[1]-v2[1];
   Result[2]:=v1[2]-v2[2];
{$endif}
end;

// VectorSubtract (proc, hmg)
//
procedure VectorSubtract(const v1, v2 : TVector; var result : TVector);
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
      db $0F,$7F,$01           /// MOVQ  [ECX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
      db $0F,$7F,$49,$08       /// MOVQ  [ECX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FSTP DWORD PTR [ECX+12]
{$else}
begin
   result[0]:=v1[0]-v2[0];
   result[1]:=v1[1]-v2[1];
   result[2]:=v1[2]-v2[2];
   result[3]:=v1[3]-v2[3];
{$endif}
end;

// VectorSubtract (proc, affine)
//
procedure VectorSubtract(const v1, v2 : TVector; var result : TAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EAX]
         FSUB DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FSUB DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FSUB DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
{$else}
begin
   result[0]:=v1[0]-v2[0];
   result[1]:=v1[1]-v2[1];
   result[2]:=v1[2]-v2[2];
{$endif}
end;

// VectorSubtract (affine, single)
//
function VectorSubtract(const v1 : TAffineVector; delta : Single) : TAffineVector;
begin
   Result[0]:=v1[0]-delta;
   Result[1]:=v1[1]-delta;
   Result[2]:=v1[2]-delta;
end;

// VectorSubtract (hmg, single)
//
function VectorSubtract(const v1 : TVector; delta : Single) : TVector;
begin
   Result[0]:=v1[0]-delta;
   Result[1]:=v1[1]-delta;
   Result[2]:=v1[2]-delta;
   Result[3]:=v1[3]-delta;
end;

// SubtractVector (affine)
//
procedure SubtractVector(var V1 : TAffineVector; const V2 : TAffineVector);
// EAX contains address of V1
// EDX contains address of V2
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EAX]
         FSUB DWORD PTR [EDX]
         FSTP DWORD PTR [EAX]
         FLD  DWORD PTR [EAX+4]
         FSUB DWORD PTR [EDX+4]
         FSTP DWORD PTR [EAX+4]
         FLD  DWORD PTR [EAX+8]
         FSUB DWORD PTR [EDX+8]
         FSTP DWORD PTR [EAX+8]
{$else}
begin
   v1[0]:=v1[0]-v2[0];
   v1[1]:=v1[1]-v2[1];
   v1[2]:=v1[2]-v2[2];
{$endif}
end;

// SubtractVector (hmg)
//
procedure SubtractVector(var V1 : TVector; const V2 : TVector);
// EAX contains address of V1
// EDX contains address of V2
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
      db $0F,$7F,$00           /// MOVQ  [EAX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
      db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FSTP DWORD PTR [EAX+12]
{$else}
begin
   v1[0]:=v1[0]-v2[0];
   v1[1]:=v1[1]-v2[1];
   v1[2]:=v1[2]-v2[2];
   v1[3]:=v1[3]-v2[3];
{$endif}
end;

// CombineVector (var)
//
procedure CombineVector(var vr : TAffineVector; const v : TAffineVector; var f : Single);
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EDX]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX]
         FSTP DWORD PTR [EAX]
         FLD  DWORD PTR [EDX+4]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX+4]
         FSTP DWORD PTR [EAX+4]
         FLD  DWORD PTR [EDX+8]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX+8]
         FSTP DWORD PTR [EAX+8]
{$else}
begin
   vr[0]:=vr[0]+v[0]*f;
   vr[1]:=vr[1]+v[1]*f;
   vr[2]:=vr[2]+v[2]*f;
{$endif}
end;

// CombineVector (pointer)
//
procedure CombineVector(var vr : TAffineVector; const v : TAffineVector; pf : PFloat);
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD  DWORD PTR [EDX]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX]
         FSTP DWORD PTR [EAX]
         FLD  DWORD PTR [EDX+4]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX+4]
         FSTP DWORD PTR [EAX+4]
         FLD  DWORD PTR [EDX+8]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX+8]
         FSTP DWORD PTR [EAX+8]
{$else}
begin
   vr[0]:=vr[0]+v[0]*pf^;
   vr[1]:=vr[1]+v[1]*pf^;
   vr[2]:=vr[2]+v[2]*pf^;
{$endif}
end;

// TexPointCombine
//
function TexPointCombine(const t1, t2 : TTexPoint; f1, f2 : Single) : TTexPoint;
begin
   Result.S:=(f1 * t1.S) + (f2 * t2.S);
   Result.T:=(f1 * t1.T) + (f2 * t2.T);
end;

// VectorCombine
//
function VectorCombine(const V1, V2: TAffineVector; const F1, F2: Single): TAffineVector;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
end;

// VectorCombine3 (func)
//
function VectorCombine3(const V1, V2, V3: TAffineVector; const F1, F2, F3: Single): TAffineVector;
begin
  Result[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
  Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
  Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
end;

// VectorCombine3 (vector)
//
procedure VectorCombine3(const V1, V2, V3: TAffineVector; const F1, F2, F3: Single; var vr : TAffineVector);
begin
   vr[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
   vr[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
   vr[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
end;

// CombineVector
//
procedure CombineVector(var vr : TVector; const v : TVector; var f : Single); overload;
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6E,$11           /// MOVD  MM2, [ECX]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
      db $0F,$6F,$02           /// MOVQ  MM0, [EDX]
      db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
      db $0F,$0F,$00,$9E       /// PFADD MM0, [EAX]
      db $0F,$7F,$00           /// MOVQ  [EAX], MM0
      db $0F,$6F,$4A,$08       /// MOVQ  MM1, [EDX+8]
      db $0F,$0F,$CA,$B4       /// PFMUL MM1, MM2
      db $0F,$0F,$48,$08,$9E   /// PFADD MM1, [EAX+8]
      db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EDX]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EDX+4]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EDX+8]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EDX+12]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+12]
      FSTP DWORD PTR [EAX+12]
{$else}
begin
   vr[0]:=vr[0]+v[0]*f;
   vr[1]:=vr[1]+v[1]*f;
   vr[2]:=vr[2]+v[2]*f;
   vr[3]:=vr[3]+v[3]*f;
{$endif}
end;

// CombineVector
//
procedure CombineVector(var vr : TVector; const v : TAffineVector; var f : Single); overload;
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EDX]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EDX+4]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EDX+8]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
{$else}
begin
   vr[0]:=vr[0]+v[0]*f;
   vr[1]:=vr[1]+v[1]*f;
   vr[2]:=vr[2]+v[2]*f;
{$endif}
end;

// VectorCombine
//
function VectorCombine(const V1, V2: TVector; const F1, F2: Single): TVector;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
   Result[W]:=(F1 * V1[W]) + (F2 * V2[W]);
end;

// VectorCombine
//
function VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single): TVector; overload;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
   Result[W]:=F1*V1[W];
end;

// VectorCombine
//
procedure VectorCombine(const V1, V2: TVector; const F1, F2: Single; var vr : TVector); overload;
// EAX contains address of v1
// EDX contains address of v2
// ECX contains address of vr
// ebp+$c points to f1
// ebp+$8 points to f2
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 246354
      db $0F,$6E,$4D,$0C       /// MOVD  MM1, [EBP+$0C]
      db $0F,$62,$C9           /// PUNPCKLDQ MM1, MM1
      db $0F,$6E,$55,$08       /// MOVD  MM2, [EBP+$08]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2

      db $0F,$6F,$18           /// MOVQ  MM3, [EAX]
      db $0F,$0F,$D9,$B4       /// PFMUL MM3, MM1
      db $0F,$6F,$22           /// MOVQ  MM4, [EDX]
      db $0F,$0F,$E2,$B4       /// PFMUL MM4, MM2
      db $0F,$0F,$DC,$9E       /// PFADD MM3, MM4
      db $0F,$7F,$19           /// MOVQ  [ECX], MM3

      db $0F,$6F,$68,$08       /// MOVQ  MM5, [EAX+8]
      db $0F,$0F,$E9,$B4       /// PFMUL MM5, MM1
      db $0F,$6F,$72,$08       /// MOVQ  MM6, [EDX+8]
      db $0F,$0F,$F2,$B4       /// PFMUL MM6, MM2
      db $0F,$0F,$EE,$9E       /// PFADD MM5, MM6
      db $0F,$7F,$69,$08       /// MOVQ  [ECX+8], MM5

      db $0F,$0E               /// FEMMS
      pop ebp
      ret $08

@@FPU:      // 327363
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX]

      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX+4]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX+4]

      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX+8]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX+8]

      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX+12]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX+12]
{$else}
begin
   vr[0]:=(F1 * V1[0]) + (F2 * V2[0]);
   vr[1]:=(F1 * V1[1]) + (F2 * V2[1]);
   vr[2]:=(F1 * V1[2]) + (F2 * V2[2]);
   vr[3]:=(F1 * V1[3]) + (F2 * V2[3]);
{$endif}
end;

// VectorCombine (F1=1.0)
//
procedure VectorCombine(const V1, V2: TVector; const F2: Single; var vr : TVector); overload;
// EAX contains address of v1
// EDX contains address of v2
// ECX contains address of vr
// ebp+$8 points to f2
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 121559
      db $0F,$6E,$55,$08       /// MOVD  MM2, [EBP+$08]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2

      db $0F,$6F,$22           /// MOVQ  MM4, [EDX]
      db $0F,$6F,$72,$08       /// MOVQ  MM6, [EDX+8]

      db $0F,$0F,$E2,$B4       /// PFMUL MM4, MM2
      db $0F,$0F,$F2,$B4       /// PFMUL MM6, MM2

      db $0F,$0F,$20,$9E       /// PFADD MM4, [EAX]
      db $0F,$0F,$70,$08,$9E   /// PFADD MM6, [EAX+8]

      db $0F,$7F,$21           /// MOVQ  [ECX], MM4
      db $0F,$7F,$71,$08       /// MOVQ  [ECX+8], MM6

      db $0F,$0E               /// FEMMS
      pop ebp
      ret $04

@@FPU:      // 171379
      FLD  DWORD PTR [EBP+$08]

      FLD  DWORD PTR [EDX]
      FMUL ST, ST(1)
      FADD DWORD PTR [EAX]
      FSTP DWORD PTR [ECX]

      FLD  DWORD PTR [EDX+4]
      FMUL ST, ST(1)
      FADD DWORD PTR [EAX+4]
      FSTP DWORD PTR [ECX+4]

      FLD  DWORD PTR [EDX+8]
      FMUL ST, ST(1)
      FADD DWORD PTR [EAX+8]
      FSTP DWORD PTR [ECX+8]

      FLD  DWORD PTR [EDX+12]
      FMULP
      FADD DWORD PTR [EAX+12]
      FSTP DWORD PTR [ECX+12]
{$else}
begin      // 201283
   vr[0]:=V1[0] + (F2 * V2[0]);
   vr[1]:=V1[1] + (F2 * V2[1]);
   vr[2]:=V1[2] + (F2 * V2[2]);
   vr[3]:=V1[3] + (F2 * V2[3]);
{$endif}
end;

// VectorCombine
//
procedure VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single; var vr : TVector);
begin
   vr[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   vr[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   vr[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
   vr[W]:=F1*V1[W];
end;

// VectorCombine3
//
function VectorCombine3(const V1, V2, V3 : TVector; const F1, F2, F3 : Single) : TVector;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
   Result[W]:=(F1 * V1[W]) + (F2 * V2[W]) + (F3 * V3[W]);
end;

// VectorCombine3
//
procedure VectorCombine3(const V1, V2, V3: TVector; const F1, F2, F3: Single; var vr : TVector);
// EAX contains address of v1
// EDX contains address of v2
// ECX contains address of v3
// EBX contains address of vr
// ebp+$14 points to f1
// ebp+$10 points to f2
// ebp+$0c points to f3
begin
{$ifndef GEOMETRY_NO_ASM}
   asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 197
      db $0F,$6E,$4D,$14       /// MOVD  MM1, [EBP+$14]
      db $0F,$62,$C9           /// PUNPCKLDQ MM1, MM1
      db $0F,$6E,$55,$10       /// MOVD  MM2, [EBP+$10]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
      db $0F,$6E,$5D,$0C       /// MOVD  MM3, [EBP+$0C]
      db $0F,$62,$DB           /// PUNPCKLDQ MM3, MM3

      db $0F,$6F,$20           /// MOVQ  MM4, [EAX]
      db $0F,$0F,$E1,$B4       /// PFMUL MM4, MM1
      db $0F,$6F,$2A           /// MOVQ  MM5, [EDX]
      db $0F,$0F,$EA,$B4       /// PFMUL MM5, MM2
      db $0F,$0F,$E5,$9E       /// PFADD MM4, MM5
      db $0F,$6F,$31           /// MOVQ  MM6, [ECX]
      db $0F,$0F,$F3,$B4       /// PFMUL MM6, MM3
      db $0F,$0F,$E6,$9E       /// PFADD MM4, MM6
      db $0F,$7F,$23           /// MOVQ  [EBX], MM4

      db $0F,$6F,$78,$08       /// MOVQ  MM7, [EAX+8]
      db $0F,$0F,$F9,$B4       /// PFMUL MM7, MM1
      db $0F,$6F,$42,$08       /// MOVQ  MM0, [EDX+8]
      db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
      db $0F,$0F,$F8,$9E       /// PFADD MM7, MM0
      db $0F,$6F,$69,$08       /// MOVQ  MM5, [ECX+8]
      db $0F,$0F,$EB,$B4       /// PFMUL MM5, MM3
      db $0F,$0F,$FD,$9E       /// PFADD MM7, MM5
      db $0F,$7F,$7B,$08       /// MOVQ  [EBX+8], MM7

      db $0F,$0E               /// FEMMS
      pop ebx
      pop ebp
      ret $10
@@FPU:      // 263
   end;
{$endif}
   vr[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
   vr[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
   vr[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
   vr[W]:=(F1 * V1[W]) + (F2 * V2[W]) + (F3 * V3[W]);
end;

// VectorDotProduct (affine)
//
function VectorDotProduct(const V1, V2 : TAffineVector): Single;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
       FLD DWORD PTR [eax]
       FMUL DWORD PTR [edx]
       FLD DWORD PTR [eax+4]
       FMUL DWORD PTR [edx+4]
       faddp
       FLD DWORD PTR [eax+8]
       FMUL DWORD PTR [edx+8]
       faddp
end;
{$else}
begin
   Result:=V1[0]*V2[0]+V1[1]*V2[1]+V1[2]*V2[2];
end;
{$endif}

// VectorDotProduct (hmg)
//
function VectorDotProduct(const V1, V2 : TVector) : Single;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD DWORD PTR [EAX]
         FMUL DWORD PTR [EDX]
         FLD DWORD PTR [EAX + 4]
         FMUL DWORD PTR [EDX + 4]
         FADDP
         FLD DWORD PTR [EAX + 8]
         FMUL DWORD PTR [EDX + 8]
         FADDP
         FLD DWORD PTR [EAX + 12]
         FMUL DWORD PTR [EDX + 12]
         FADDP
{$else}
begin
   Result:=V1[0]*V2[0]+V1[1]*V2[1]+V1[2]*V2[2]+V1[3]*V2[3];
{$endif}
end;

// VectorDotProduct
//
function VectorDotProduct(const V1 : TVector; const V2 : TAffineVector) : Single;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD DWORD PTR [EAX]
         FMUL DWORD PTR [EDX]
         FLD DWORD PTR [EAX + 4]
         FMUL DWORD PTR [EDX + 4]
         FADDP
         FLD DWORD PTR [EAX + 8]
         FMUL DWORD PTR [EDX + 8]
         FADDP
{$else}
begin
   Result:=V1[0]*V2[0]+V1[1]*V2[1]+V1[2]*V2[2];
{$endif}
end;

// PointProject (affine)
//
function PointProject(const p, origin, direction : TAffineVector) : Single;
// EAX -> p, EDX -> origin, ECX -> direction
{$ifndef GEOMETRY_NO_ASM}
asm
      fld   dword ptr [eax]
      fsub  dword ptr [edx]
      fmul  dword ptr [ecx]
      fld   dword ptr [eax+4]
      fsub  dword ptr [edx+4]
      fmul  dword ptr [ecx+4]
      fadd
      fld   dword ptr [eax+8]
      fsub  dword ptr [edx+8]
      fmul  dword ptr [ecx+8]
      fadd
{$else}
begin
   Result:= direction[0]*(p[0]-origin[0])
           +direction[1]*(p[1]-origin[1])
           +direction[2]*(p[2]-origin[2]);
{$endif}
end;

// PointProject (vector)
//
function PointProject(const p, origin, direction : TVector) : Single;
// EAX -> p, EDX -> origin, ECX -> direction
{$ifndef GEOMETRY_NO_ASM}
asm
      fld   dword ptr [eax]
      fsub  dword ptr [edx]
      fmul  dword ptr [ecx]
      fld   dword ptr [eax+4]
      fsub  dword ptr [edx+4]
      fmul  dword ptr [ecx+4]
      fadd
      fld   dword ptr [eax+8]
      fsub  dword ptr [edx+8]
      fmul  dword ptr [ecx+8]
      fadd
{$else}
begin
   Result:= direction[0]*(p[0]-origin[0])
           +direction[1]*(p[1]-origin[1])
           +direction[2]*(p[2]-origin[2]);
{$endif}
end;

// VectorCrossProduct
//
function VectorCrossProduct(const v1, v2 : TAffineVector) : TAffineVector;
begin
   Result[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   Result[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   Result[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
end;

// VectorCrossProduct
//
function VectorCrossProduct(const v1, v2 : TVector) : TVector;
begin
   Result[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   Result[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   Result[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
   Result[W]:=0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2: TVector; var vr : TVector);
begin
   vr[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   vr[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   vr[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
   vr[W]:=0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TVector); overload;
begin
   vr[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   vr[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   vr[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
   vr[W]:=0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2 : TVector; var vr : TAffineVector); overload;
begin
   vr[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   vr[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   vr[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TAffineVector); overload;
begin
   vr[X]:=v1[Y]*v2[Z]-v1[Z]*v2[Y];
   vr[Y]:=v1[Z]*v2[X]-v1[X]*v2[Z];
   vr[Z]:=v1[X]*v2[Y]-v1[Y]*v2[X];
end;

// Lerp
//
function Lerp(const start, stop, t : Single) : Single;
begin
   Result:=start+(stop-start)*t;
end;

// Angle Lerp
//
function AngleLerp(start, stop, t : Single) : Single;
var
   d : Single;
begin
   start:=NormalizeAngle(start);
   stop:=NormalizeAngle(stop);
   d:=stop-start;
   if d>PI then begin
      // positive d, angle on opposite side, becomes negative i.e. changes direction
      d:=-d-c2PI;
   end else if d<-PI then begin
      // negative d, angle on opposite side, becomes positive i.e. changes direction
      d:=d+c2PI;
   end;
   Result:=start+d*t;
end;

// DistanceBetweenAngles
//
function DistanceBetweenAngles(angle1, angle2 : Single) : Single;
begin
   angle1:=NormalizeAngle(angle1);
   angle2:=NormalizeAngle(angle2);
   Result:=Abs(angle2-angle1);
   if Result>PI then
      Result:=c2PI-Result;
end;

// TexPointLerp
//
function TexPointLerp(const t1, t2 : TTexPoint; t : Single) : TTexPoint; overload;
begin
   Result.S:=t1.S+(t2.S-t1.S)*t;
   Result.T:=t1.T+(t2.T-t1.T)*t;
end;

// VectorAffineLerp
//
function VectorLerp(const V1, V2: TAffineVector; t: Single): TAffineVector;
{$ifndef GEOMETRY_NO_ASM}
asm
   fld   t

   fld   dword ptr [eax+0]
   fld   dword ptr [edx+0]
   fsub  st(0), st(1)
   fmul  st(0), st(2)
   faddp
   fstp  dword ptr [ecx+0]

   fld   dword ptr [eax+4]
   fld   dword ptr [edx+4]
   fsub  st(0), st(1)
   fmul  st(0), st(2)
   faddp
   fstp  dword ptr [ecx+4]

   fld   dword ptr [eax+8]
   fld   dword ptr [edx+8]
   fsub  st(0), st(1)
   fmul  st(0), st(2)
   faddp
   fstp  dword ptr [ecx+8]

   ffree st(0)
{$else}
begin
   Result[X]:=V1[X]+(V2[X]-V1[X])*t;
   Result[Y]:=V1[Y]+(V2[Y]-V1[Y])*t;
   Result[Z]:=V1[Z]+(V2[Z]-V1[Z])*t;
{$endif}
end;

// VectorLerp
//
procedure VectorLerp(const v1, v2 : TAffineVector; t : Single; var vr : TAffineVector);
// EAX contains address of v1
// EDX contains address of v2
// EBX contains address of t
// ECX contains address of vr
{$ifndef GEOMETRY_NO_ASM}
asm
      fld   t

      fld   dword ptr [eax+0]
      fld   dword ptr [edx+0]
      fsub  st(0), st(1)
      fmul  st(0), st(2)
      faddp
      fstp  dword ptr [ecx+0]

      fld   dword ptr [eax+4]
      fld   dword ptr [edx+4]
      fsub  st(0), st(1)
      fmul  st(0), st(2)
      faddp
      fstp  dword ptr [ecx+4]

      fld   dword ptr [eax+8]
      fld   dword ptr [edx+8]
      fsub  st(0), st(1)
      fmul  st(0), st(2)
      faddp
      fstp  dword ptr [ecx+8]

      ffree st(0)
{$else}
begin
   vr[X]:=V1[X]+(V2[X]-V1[X])*t;
   vr[Y]:=V1[Y]+(V2[Y]-V1[Y])*t;
   vr[Z]:=V1[Z]+(V2[Z]-V1[Z])*t;
{$endif}
end;

// VectorLerp
//
function VectorLerp(const V1, V2: TVector; t: Single): TVector;
begin
   Result[X]:=V1[X]+(V2[X]-V1[X])*t;
   Result[Y]:=V1[Y]+(V2[Y]-V1[Y])*t;
   Result[Z]:=V1[Z]+(V2[Z]-V1[Z])*t;
   Result[W]:=V1[W]+(V2[W]-V1[W])*t;
end;

// VectorLerp
//
procedure VectorLerp(const v1, v2 : TVector; t : Single; var vr : TVector);
begin
   vr[X]:=V1[X]+(V2[X]-V1[X])*t;
   vr[Y]:=V1[Y]+(V2[Y]-V1[Y])*t;
   vr[Z]:=V1[Z]+(V2[Z]-V1[Z])*t;
   vr[W]:=V1[W]+(V2[W]-V1[W])*t;
end;

// VectorAngleLerp
//
function VectorAngleLerp(const v1, v2 : TAffineVector; t : Single) : TAffineVector;
var
   q1, q2, qr : TQuaternion;
   m : TMatrix;
   tran : TTransformations;
begin
   if VectorEquals(v1, v2) then begin
      Result:=v1;
   end else begin
      q1:=QuaternionFromEuler(RadToDeg(v1[0]), RadToDeg(v1[1]), RadToDeg(v1[2]), eulZYX);
      q2:=QuaternionFromEuler(RadToDeg(v2[0]), RadToDeg(v2[1]), RadToDeg(v2[2]), eulZYX);
      qr:=QuaternionSlerp(q1, q2, t);
      m:=QuaternionToMatrix(qr);
      MatrixDecompose(m, tran);
      Result[0]:=tran[ttRotateX];
      Result[1]:=tran[ttRotateY];
      Result[2]:=tran[ttRotateZ];
   end;
end;

// VectorAngleCombine
//
function VectorAngleCombine(const v1, v2 : TAffineVector; f : Single) : TAffineVector;
begin
   Result:=VectorCombine(v1, v2, 1, f);
end;

// VectorArrayLerp_3DNow (hmg)
//
{$ifndef GEOMETRY_NO_ASM}
procedure VectorArrayLerp_3DNow(const src1, src2 : PVectorArray; t : Single; n : Integer; dest : PVectorArray); stdcall; overload;
var
   pt : ^Single;
begin
   pt:=@t;
   asm
      push ebx
      push edi

      mov   eax, src1
      mov   edx, src2
      mov   ecx, n
      mov   ebx, dest
      mov   edi, pt

      db $0F,$0E               /// femms

      db $0F,$6E,$3F           /// movd     mm7, [edi]
      db $0F,$62,$FF           /// punpckldq mm7, mm7

@@Loop:
      db $0F,$6F,$00           /// movq     mm0, [eax]
      db $0F,$6F,$50,$08       /// movq     mm2, [eax+8]
      db $0F,$6F,$C8           /// movq     mm1, mm0
      db $0F,$6F,$DA           /// movq     mm3, mm2
      db $0F,$0F,$02,$AA       /// pfsubr   mm0, [edx]
      db $0F,$0F,$52,$08,$AA   /// pfsubr   mm2, [edx+8]
      db $0F,$0D,$4B,$20       /// prefetchw [ebx+32]
      db $0F,$0F,$C7,$B4       /// pfmul    mm0, mm7
      db $0F,$0F,$D7,$B4       /// pfmul    mm2, mm7
      add   eax, 16
      add   edx, 16
      db $0F,$0D,$40,$20       /// prefetch [eax+32]
      db $0F,$0F,$C1,$9E       /// pfadd    mm0, mm1
      db $0F,$0F,$D3,$9E       /// pfadd    mm2, mm3
      db $0F,$0D,$42,$20       /// prefetch [edx+32]
      db $0F,$7F,$03           /// movq     [ebx], mm0
      db $0F,$7F,$53,$08       /// movq     [ebx+8], mm2

      add   ebx, 16

      dec   ecx
      jnz @@Loop

      db $0F,$0E               /// femms

      pop edi
      pop ebx
   end;
end;
{$endif}

// VectorArrayLerp (hmg)
//
procedure VectorArrayLerp(const src1, src2 : PVectorArray; t : Single; n : Integer; dest : PVectorArray);
var
   i : Integer;
begin
{$ifndef GEOMETRY_NO_ASM}
   if vSIMD=1 then
      VectorArrayLerp_3DNow(src1, src2, t, n, dest)
   else {$endif} begin
      for i:=0 to n-1 do begin
         dest^[i][0]:=src1^[i][0]+(src2^[i][0]-src1^[i][0])*t;
         dest^[i][1]:=src1^[i][1]+(src2^[i][1]-src1^[i][1])*t;
         dest^[i][2]:=src1^[i][2]+(src2^[i][2]-src1^[i][2])*t;
         dest^[i][3]:=src1^[i][3]+(src2^[i][3]-src1^[i][3])*t;
      end;
   end;
end;

// VectorArrayLerp_3DNow (affine)
//
{$ifndef GEOMETRY_NO_ASM}
procedure VectorArrayLerp_3DNow(const src1, src2 : PAffineVectorArray; t : Single; n : Integer; dest : PAffineVectorArray); stdcall; overload;
var
   pt : ^Single;
begin
   pt:=@t;
   asm
      push ebx
      push edi

      mov   eax, src1
      mov   edx, src2
      mov   ecx, n

      cmp   ecx, 1
      jbe   @@End

      shr   ecx, 1
      mov   ebx, dest
      mov   edi, pt

      db $0F,$0E               /// femms

      db $0F,$6E,$3F           /// movd     mm7, [edi]
      db $0F,$62,$FF           /// punpckldq mm7, mm7

@@Loop:
      db $0F,$6F,$00           /// movq     mm0, [eax]
      db $0F,$6F,$50,$08       /// movq     mm2, [eax+8]
      db $0F,$6F,$60,$10       /// movq     mm4, [eax+16]
      db $0F,$6F,$C8           /// movq     mm1, mm0
      db $0F,$6F,$DA           /// movq     mm3, mm2
      db $0F,$6F,$EC           /// movq     mm5, mm4
      db $0F,$0F,$02,$AA       /// pfsubr   mm0, [edx]
      db $0F,$0F,$52,$08,$AA   /// pfsubr   mm2, [edx+8]
      db $0F,$0F,$62,$10,$AA   /// pfsubr   mm4, [edx+16]
      db $0F,$0D,$4B,$40       /// prefetchw [ebx+64]
      db $0F,$0F,$C7,$B4       /// pfmul    mm0, mm7
      db $0F,$0F,$D7,$B4       /// pfmul    mm2, mm7
      db $0F,$0F,$E7,$B4       /// pfmul    mm4, mm7
      db $0F,$0D,$40,$40       /// prefetch [eax+64]
      add   eax, 24
      add   edx, 24
      db $0F,$0F,$C1,$9E       /// pfadd    mm0, mm1
      db $0F,$0F,$D3,$9E       /// pfadd    mm2, mm3
      db $0F,$0F,$E5,$9E       /// pfadd    mm4, mm5
      db $0F,$0D,$42,$40       /// prefetch [edx+64]
      db $0F,$7F,$03           /// movq     [ebx], mm0
      db $0F,$7F,$53,$08       /// movq     [ebx+8], mm2
      db $0F,$7F,$63,$10       /// movq     [ebx+16], mm4

      add   ebx, 24

      dec   ecx
      jnz @@Loop

      db $0F,$0E               /// femms

@@End:
      pop edi
      pop ebx
   end;
   if (n and 1)=1 then
      VectorLerp(src1[n-1], src2[n-1], t, dest[n-1]);
end;
{$endif}

// VectorArrayLerp (affine)
//
procedure VectorArrayLerp(const src1, src2 : PAffineVectorArray; t : Single; n : Integer; dest : PAffineVectorArray);
var
   i : Integer;
begin
{$ifndef GEOMETRY_NO_ASM}
   if vSIMD=1 then
      VectorArrayLerp_3DNow(src1, src2, t, n, dest)
   else {$endif} begin
      for i:=0 to n-1 do begin
         dest^[i][0]:=src1^[i][0]+(src2^[i][0]-src1^[i][0])*t;
         dest^[i][1]:=src1^[i][1]+(src2^[i][1]-src1^[i][1])*t;
         dest^[i][2]:=src1^[i][2]+(src2^[i][2]-src1^[i][2])*t;
      end;
   end;
end;

// VectorLength (array)
//
function VectorLength(const V : array of Single) : Single;
// EAX contains address of V
// EDX contains the highest index of V
// the result is returned in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
         FLDZ                           // initialize sum
@@Loop:
         FLD  DWORD PTR [EAX  +  4 * EDX] // load a component
         FMUL ST, ST
         FADDP
         SUB  EDX, 1
         JNL  @@Loop
         FSQRT
{$else}
var
   i : Integer;
begin
   Result:=0;
   for i:=Low(V) to High(V) do
      Result:=Result+Sqr(V[i]);
   Result:=Sqrt(Result);
{$endif}
end;

// VectorLength  (x, y)
//
function VectorLength(const x, y : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD X
         FMUL ST, ST
         FLD Y
         FMUL ST, ST
         FADD
         FSQRT
{$else}
begin
   Result:=Sqrt(x*x+y*y);
{$endif}
end;

// VectorLength (x, y, z)
//
function VectorLength(const x, y, z : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
         FLD X
         FMUL ST, ST
         FLD Y
         FMUL ST, ST
         FADD
         FLD Z
         FMUL ST, ST
         FADD
         FSQRT
{$else}
begin
   Result:=Sqrt(x*x+y*y+z*z);
{$endif}
end;

// VectorLength
//
function VectorLength(const v : TAffineVector) : Single;
// EAX contains address of V
// result is passed in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
       FLD  DWORD PTR [EAX]
       FMUL ST, ST
       FLD  DWORD PTR [EAX+4]
       FMUL ST, ST
       FADDP
       FLD  DWORD PTR [EAX+8]
       FMUL ST, ST
       FADDP
       FSQRT
{$else}
begin
   Result:=Sqrt(VectorNorm(v));
{$endif}
end;

// VectorLength
//
function VectorLength(const v : TVector) : Single;
// EAX contains address of V
// result is passed in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
       FLD  DWORD PTR [EAX]
       FMUL ST, ST
       FLD  DWORD PTR [EAX+4]
       FMUL ST, ST
       FADDP
       FLD  DWORD PTR [EAX+8]
       FMUL ST, ST
       FADDP
       FSQRT
{$else}
begin
   Result:=Sqrt(VectorNorm(v));
{$endif}
end;

// VectorNorm
//
function VectorNorm(const x, y : Single) : Single;
begin
   Result:=Sqr(x)+Sqr(y);
end;

// VectorNorm (affine)
//
function VectorNorm(const v : TAffineVector) : Single;
// EAX contains address of V
// result is passed in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX];
      FMUL ST, ST
      FLD DWORD PTR [EAX+4];
      FMUL ST, ST
      FADD
      FLD DWORD PTR [EAX+8];
      FMUL ST, ST
      FADD
{$else}
begin
   Result:=v[0]*v[0]+v[1]*v[1]+v[2]*v[2];
{$endif}
end;

// VectorNorm (hmg)
//
function VectorNorm(const v : TVector) : Single;
// EAX contains address of V
// result is passed in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX];
      FMUL ST, ST
      FLD DWORD PTR [EAX+4];
      FMUL ST, ST
      FADD
      FLD DWORD PTR [EAX+8];
      FMUL ST, ST
      FADD
{$else}
begin
   Result:=v[0]*v[0]+v[1]*v[1]+v[2]*v[2];
{$endif}
end;

// VectorNorm
//
function VectorNorm(var V: array of Single): Single;
// EAX contains address of V
// EDX contains highest index in V
// result is passed in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLDZ                           // initialize sum
@@Loop:
      FLD  DWORD PTR [EAX + 4 * EDX] // load a component
      FMUL ST, ST                    // make square
      FADDP                          // add previous calculated sum
      SUB  EDX, 1
      JNL  @@Loop
{$else}
var
   i : Integer;
begin
   Result:=0;
   for i:=Low(v) to High(v) do
      Result:=Result+v[i]*v[i];
{$endif}
end;

// NormalizeVector (affine)
//
procedure NormalizeVector(var v : TAffineVector);
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// movq        mm0,[eax]
      db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
      db $0F,$6F,$E0           /// movq        mm4,mm0
      db $0F,$6F,$D9           /// movq        mm3,mm1
      db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
      db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
      db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
      db $0F,$6F,$D1           /// movq        mm2,mm1

      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
      db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
      db $0F,$62,$C9           /// punpckldq   mm1,mm1
      db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
      db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
      db $0F,$7E,$58,$08       /// movd        [eax+8],mm3
      db $0F,$7F,$20           /// movq        [eax],mm4
@@norm_end:
      db $0F,$0E               /// femms
      ret

@@FPU:
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
{$else}
var
   invLen : Single;
begin
   invLen:=RSqrt(VectorNorm(v));
   v[0]:=v[0]*invLen;
   v[1]:=v[1]*invLen;
   v[2]:=v[2]*invLen;
{$endif}
end;

// VectorNormalize
//
function VectorNormalize(const v : TAffineVector) : TAffineVector;
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// movq        mm0,[eax]
      db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
      db $0F,$6F,$E0           /// movq        mm4,mm0
      db $0F,$6F,$D9           /// movq        mm3,mm1
      db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
      db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
      db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
      db $0F,$6F,$D1           /// movq        mm2,mm1

      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
      db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
      db $0F,$62,$C9           /// punpckldq   mm1,mm1
      db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
      db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
      db $0F,$7E,$5A,$08       /// movd        [edx+8],mm3
      db $0F,$7F,$22           /// movq        [edx],mm4
@@norm_end:
      db $0F,$0E               /// femms
      ret

@@FPU:
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EDX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EDX+4]
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EDX+8]
{$else}
var
   invLen : Single;
begin
   invLen:=RSqrt(VectorNorm(v));
   Result[0]:=v[0]*invLen;
   Result[1]:=v[1]*invLen;
   Result[2]:=v[2]*invLen;
{$endif}
end;

// NormalizeVectorArray
//
procedure NormalizeVectorArray(list : PAffineVectorArray; n : Integer);
// EAX contains list
// EDX contains n
{$ifndef GEOMETRY_NO_ASM}
asm
      OR    EDX, EDX
      JZ    @@End
      test vSIMD, 1
      jz @@FPULoop
@@3DNowLoop:
      db $0F,$6F,$00           /// movq        mm0,[eax]
      db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
      db $0F,$6F,$E0           /// movq        mm4,mm0
      db $0F,$6F,$D9           /// movq        mm3,mm1
      db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
      db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
      db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
      db $0F,$6F,$D1           /// movq        mm2,mm1

      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
      db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
      db $0F,$62,$C9           /// punpckldq   mm1,mm1
      db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
      db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
      db $0F,$7E,$58,$08       /// movd        [eax+8],mm3
      db $0F,$7F,$20           /// movq        [eax],mm4
@@norm_end:
      db $0F,$0E               /// femms
      add   eax, 12
      db $0F,$0D,$40,$60       /// PREFETCH    [EAX+96]
      dec   edx
      jnz   @@3DNowLOOP
      ret

@@FPULoop:
      FLD   DWORD PTR [EAX]
      FMUL  ST, ST
      FLD   DWORD PTR [EAX+4]
      FMUL  ST, ST
      FADD
      FLD   DWORD PTR [EAX+8]
      FMUL  ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD   ST
      FMUL  DWORD PTR [EAX]
      FSTP  DWORD PTR [EAX]
      FLD   ST
      FMUL  DWORD PTR [EAX+4]
      FSTP  DWORD PTR [EAX+4]
      FMUL  DWORD PTR [EAX+8]
      FSTP  DWORD PTR [EAX+8]
      ADD   EAX, 12
      DEC   EDX
      JNZ   @@FPULOOP
@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to n-1 do
      NormalizeVector(list^[i]);
{$endif}
end;

// NormalizeVector (hmg)
//
procedure NormalizeVector(var v : TVector);
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// movq        mm0,[eax]
      db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
      db $0F,$6F,$E0           /// movq        mm4,mm0
      db $0F,$6F,$D9           /// movq        mm3,mm1
      db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
      db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
      db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
      db $0F,$6F,$D1           /// movq        mm2,mm1

      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
      db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
      db $0F,$62,$C9           /// punpckldq   mm1,mm1
      db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
      db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
      db $0F,$7E,$58,$08       /// movd        [eax+8],mm3
      db $0F,$7F,$20           /// movq        [eax],mm4
@@norm_end:
      db $0F,$0E               /// femms
      xor   edx, edx
      mov   [eax+12], edx
      ret

@@FPU:
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
      xor   edx, edx
      mov   [eax+12], edx
{$else}
var
   invLen : Single;
begin
   invLen:=RSqrt(VectorNorm(v));
   v[0]:=v[0]*invLen;
   v[1]:=v[1]*invLen;
   v[2]:=v[2]*invLen;
   v[3]:=0;
{$endif}
end;

// VectorNormalize (hmg, func)
//
function VectorNormalize(const v : TVector) : TVector;
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// movq        mm0,[eax]
      db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
      db $0F,$6F,$E0           /// movq        mm4,mm0
      db $0F,$6F,$D9           /// movq        mm3,mm1
      db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
      db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
      db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
      db $0F,$6F,$D1           /// movq        mm2,mm1

      db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
      db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
      db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
      db $0F,$62,$C9           /// punpckldq   mm1,mm1
      db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
      db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
      db $0F,$7E,$5A,$08       /// movd        [edx+8],mm3
      db $0F,$7F,$22           /// movq        [edx],mm4
@@norm_end:
      db $0F,$0E               /// femms
      xor   eax, eax
      mov   [edx+12], eax
      ret

@@FPU:
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EDX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EDX+4]
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EDX+8]
      xor   eax, eax
      mov   [edx+12], eax
{$else}
var
   invLen : Single;
begin
   invLen:=RSqrt(VectorNorm(v));
   Result[0]:=v[0]*invLen;
   Result[1]:=v[1]*invLen;
   Result[2]:=v[2]*invLen;
   Result[3]:=0;
{$endif}
end;

// VectorAngleCosine
//
function VectorAngleCosine(const V1, V2: TAffineVector): Single;
// EAX contains address of Vector1
// EDX contains address of Vector2
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]           // V1[0]
      FLD ST                        // double V1[0]
      FMUL ST, ST                   // V1[0]^2 (prep. for divisor)
      FLD DWORD PTR [EDX]           // V2[0]
      FMUL ST(2), ST                // ST(2):=V1[0] * V2[0]
      FMUL ST, ST                   // V2[0]^2 (prep. for divisor)
      FLD DWORD PTR [EAX + 4]       // V1[1]
      FLD ST                        // double V1[1]
      FMUL ST, ST                   // ST(0):=V1[1]^2
      FADDP ST(3), ST               // ST(2):=V1[0]^2 + V1[1] *  * 2
      FLD DWORD PTR [EDX + 4]       // V2[1]
      FMUL ST(1), ST                // ST(1):=V1[1] * V2[1]
      FMUL ST, ST                   // ST(0):=V2[1]^2
      FADDP ST(2), ST               // ST(1):=V2[0]^2 + V2[1]^2
      FADDP ST(3), ST               // ST(2):=V1[0] * V2[0] + V1[1] * V2[1]
      FLD DWORD PTR [EAX + 8]       // load V2[1]
      FLD ST                        // same calcs go here
      FMUL ST, ST                   // (compare above)
      FADDP ST(3), ST
      FLD DWORD PTR [EDX + 8]
      FMUL ST(1), ST
      FMUL ST, ST
      FADDP ST(2), ST
      FADDP ST(3), ST
      FMULP                         // ST(0):=(V1[0]^2 + V1[1]^2 + V1[2]) *
                                    //          (V2[0]^2 + V2[1]^2 + V2[2])
      FSQRT                         // sqrt(ST(0))
      FDIVP                         // ST(0):=Result:=ST(1) / ST(0)
  // the result is expected in ST(0), if it's invalid, an error is raised
{$else}
begin
   Result:=VectorDotProduct(V1, V2)/(VectorLength(V1)*VectorLength(V2));
{$endif}
end;

// VectorNegate (affine)
//
function VectorNegate(const v : TAffineVector) : TAffineVector;
// EAX contains address of v
// EDX contains address of Result
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EDX]
      FLD DWORD PTR [EAX+4]
      FCHS
      FSTP DWORD PTR [EDX+4]
      FLD DWORD PTR [EAX+8]
      FCHS
      FSTP DWORD PTR [EDX+8]
{$else}
begin
   Result[0]:=-v[0];
   Result[1]:=-v[1];
   Result[2]:=-v[2];
{$endif}
end;

// VectorNegate (hmg)
//
function VectorNegate(const v : TVector) : TVector;
// EAX contains address of v
// EDX contains address of Result
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EDX]
      FLD DWORD PTR [EAX+4]
      FCHS
      FSTP DWORD PTR [EDX+4]
      FLD DWORD PTR [EAX+8]
      FCHS
      FSTP DWORD PTR [EDX+8]
      FLD DWORD PTR [EAX+12]
      FCHS
      FSTP DWORD PTR [EDX+12]
{$else}
begin
   Result[0]:=-v[0];
   Result[1]:=-v[1];
   Result[2]:=-v[2];
   Result[3]:=-v[3];
{$endif}
end;

// NegateVector
//
procedure NegateVector(var v : TAffineVector);
// EAX contains address of v
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EAX]
      FLD DWORD PTR [EAX+4]
      FCHS
      FSTP DWORD PTR [EAX+4]
      FLD DWORD PTR [EAX+8]
      FCHS
      FSTP DWORD PTR [EAX+8]
{$else}
begin
   v[0]:=-v[0];
   v[1]:=-v[1];
   v[2]:=-v[2];
{$endif}
end;

// NegateVector
//
procedure NegateVector(var v : TVector);
// EAX contains address of v
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EAX]
      FLD DWORD PTR [EAX+4]
      FCHS
      FSTP DWORD PTR [EAX+4]
      FLD DWORD PTR [EAX+8]
      FCHS
      FSTP DWORD PTR [EAX+8]
      FLD DWORD PTR [EAX+12]
      FCHS
      FSTP DWORD PTR [EAX+12]
{$else}
begin
   v[0]:=-v[0];
   v[1]:=-v[1];
   v[2]:=-v[2];
   v[3]:=-v[3];
{$endif}
end;

// NegateVector
//
procedure NegateVector(var V : array of Single);
// EAX contains address of V
// EDX contains highest index in V
{$ifndef GEOMETRY_NO_ASM}
asm
@@Loop:
      FLD DWORD PTR [EAX + 4 * EDX]
      FCHS
      WAIT
      FSTP DWORD PTR [EAX + 4 * EDX]
      DEC EDX
      JNS @@Loop
{$else}
var
   i : Integer;
begin
   for i:=Low(v) to High(v) do
      v[i]:=-v[i];
{$endif}
end;

// ScaleVector (affine)
//
procedure ScaleVector(var v : TAffineVector; factor: Single);
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+8]
{$else}
begin
   v[0]:=v[0]*factor;
   v[1]:=v[1]*factor;
   v[2]:=v[2]*factor;
{$endif}
end;

// ScaleVector (hmg)
//
procedure ScaleVector(var v : TVector; factor: Single);
{$ifndef GEOMETRY_NO_ASM}
asm
      test     vSIMD, 1
      jz @@FPU

@@3DNow:      // 121824

      db $0F,$6E,$4D,$08       /// movd        mm1, [ebp+8]
      db $0F,$62,$C9           /// punpckldq   mm1, mm1

      db $0F,$6F,$00           /// movq        mm0, [eax]
      db $0F,$6F,$50,$08       /// movq        mm2, [eax+8]
      db $0F,$0F,$C1,$B4       /// pfmul       mm0, mm1
      db $0F,$0F,$D1,$B4       /// pfmul       mm2, mm1
      db $0F,$7F,$00           /// movq        [eax], mm0
      db $0F,$7F,$50,$08       /// movq        [eax+8], mm2

      db $0F,$0E               /// femms

      pop   ebp
      ret   $04

@@FPU:        // 155843
      FLD  DWORD PTR [EBP+8]

      FLD  DWORD PTR [EAX]
      FMUL ST, ST(1)
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST(1)
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST(1)
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EAX+12]
      FMULP
      FSTP DWORD PTR [EAX+12]
{$else}
begin
   v[0]:=v[0]*factor;
   v[1]:=v[1]*factor;
   v[2]:=v[2]*factor;
   v[3]:=v[3]*factor;
{$endif}
end;

// ScaleVector (affine vector)
//
procedure ScaleVector(var v : TAffineVector; const factor : TAffineVector);
begin
   v[0]:=v[0]*factor[0];
   v[1]:=v[1]*factor[1];
   v[2]:=v[2]*factor[2];
end;

// ScaleVector (hmg vector)
//
procedure ScaleVector(var v : TVector; const factor : TVector);
begin
   v[0]:=v[0]*factor[0];
   v[1]:=v[1]*factor[1];
   v[2]:=v[2]*factor[2];
   v[3]:=v[3]*factor[3];
end;

// VectorScale (affine)
//
function VectorScale(const v : TAffineVector; factor : Single) : TAffineVector;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
{$else}
begin
   Result[0]:=v[0]*factor;
   Result[1]:=v[1]*factor;
   Result[2]:=v[2]*factor;
{$endif}
end;

// VectorScale (proc, affine)
//
procedure VectorScale(const v : TAffineVector; factor : Single; var vr : TAffineVector);
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
{$else}
begin
   vr[0]:=v[0]*factor;
   vr[1]:=v[1]*factor;
   vr[2]:=v[2]*factor;
{$endif}
end;

// VectorScale (hmg)
//
function VectorScale(const v : TVector; factor : Single) : TVector;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+12]
{$else}
begin
   Result[0]:=v[0]*factor;
   Result[1]:=v[1]*factor;
   Result[2]:=v[2]*factor;
   Result[3]:=v[3]*factor;
{$endif}
end;

// VectorScale (proc, hmg)
//
procedure VectorScale(const v : TVector; factor : Single; var vr : TVector);
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+12]
{$else}
begin
   vr[0]:=v[0]*factor;
   vr[1]:=v[1]*factor;
   vr[2]:=v[2]*factor;
   vr[3]:=v[3]*factor;
{$endif}
end;

// VectorScale (proc, hmg-affine)
//
procedure VectorScale(const v : TVector; factor : Single; var vr : TAffineVector);
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
{$else}
begin
   vr[0]:=v[0]*factor;
   vr[1]:=v[1]*factor;
   vr[2]:=v[2]*factor;
{$endif}
end;

// DivideVector
//
procedure DivideVector(var v : TVector; const divider : TVector);
begin
   v[0]:=v[0]/divider[0];
   v[1]:=v[1]/divider[1];
   v[2]:=v[2]/divider[2];
   v[3]:=v[3]/divider[3];
end;

// VectorEquals (hmg vector)
//
function VectorEquals(const V1, V2: TVector) : Boolean;
// EAX contains address of v1
// EDX contains highest of v2
{$ifndef GEOMETRY_NO_ASM}
asm
      mov ecx, [edx]
      cmp ecx, [eax]
      jne @@Diff
      mov ecx, [edx+$4]
      cmp ecx, [eax+$4]
      jne @@Diff
      mov ecx, [edx+$8]
      cmp ecx, [eax+$8]
      jne @@Diff
      mov ecx, [edx+$C]
      cmp ecx, [eax+$C]
      jne @@Diff
@@Equal:             
      mov eax, 1
      ret
@@Diff:
      xor eax, eax
{$else}
begin
   Result:=(v1[0]=v2[0]) and (v1[1]=v2[1]) and (v1[2]=v2[2]) and (v1[3]=v2[3]);
{$endif}
end;

// VectorEquals (affine vector)
//
function VectorEquals(const V1, V2: TAffineVector) : Boolean;
// EAX contains address of v1
// EDX contains highest of v2
{$ifndef GEOMETRY_NO_ASM}
asm
      mov ecx, [edx]
      cmp ecx, [eax]
      jne @@Diff
      mov ecx, [edx+$4]
      cmp ecx, [eax+$4]
      jne @@Diff
      mov ecx, [edx+$8]
      cmp ecx, [eax+$8]
      jne @@Diff
@@Equal:
      mov al, 1
      ret
@@Diff:
      xor eax, eax
@@End:
{$else}
begin
   Result:=(v1[0]=v2[0]) and (v1[1]=v2[1]) and (v1[2]=v2[2]);
{$endif}
end;

// AffineVectorEquals (hmg vector)
//
function AffineVectorEquals(const V1, V2 : TVector) : Boolean;
// EAX contains address of v1
// EDX contains highest of v2
{$ifndef GEOMETRY_NO_ASM}
asm
      mov ecx, [edx]
      cmp ecx, [eax]
      jne @@Diff
      mov ecx, [edx+$4]
      cmp ecx, [eax+$4]
      jne @@Diff
      mov ecx, [edx+$8]
      cmp ecx, [eax+$8]
      jne @@Diff
@@Equal:
      mov eax, 1
      ret
@@Diff:
      xor eax, eax
{$else}
begin
   Result:=(v1[0]=v2[0]) and (v1[1]=v2[1]) and (v1[2]=v2[2]);
{$endif}
end;

// VectorIsNull (hmg)
//
function VectorIsNull(const v : TVector) : Boolean;
begin
   Result:=((v[0]=0) and (v[1]=0) and (v[2]=0));
end;

// VectorIsNull (affine)
//
function VectorIsNull(const v : TAffineVector) : Boolean; overload;
begin
   Result:=((v[0]=0) and (v[1]=0) and (v[2]=0));
end;

// VectorSpacing (texpoint)
//
function VectorSpacing(const v1, v2 : TTexPoint): Single; overload;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FABS
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FABS
      FADD
{$else}
begin
   Result:=Abs(v2.S-v1.S)+Abs(v2.T-v1.T);
{$endif}
end;

// VectorSpacing (affine)
//
function VectorSpacing(const v1, v2 : TAffineVector) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FABS
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FABS
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FABS
      FADD
{$else}
begin
   Result:=Abs(v2[0]-v1[0])+Abs(v2[1]-v1[1])+Abs(v2[2]-v1[2]);
{$endif}
end;

// VectorSpacing (Hmg)
//
function VectorSpacing(const v1, v2 : TVector) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FABS
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FABS
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FABS
      FADD
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FABS
      FADD
{$else}
begin
   Result:=Abs(v2[0]-v1[0])+Abs(v2[1]-v1[1])+Abs(v2[2]-v1[2])+Abs(v2[3]-v1[3]);
{$endif}
end;

// VectorDistance (affine)
//
function VectorDistance(const v1, v2 : TAffineVector) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
      FSQRT
{$else}
begin
   Result:=Sqrt(Sqr(v2[0]-v1[0])+Sqr(v2[1]-v1[1])+Sqr(v2[2]-v1[2]));
{$endif}
end;

// VectorDistance (hmg)
//
function VectorDistance(const v1, v2 : TVector) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
      FSQRT
{$else}
begin
   Result:=Sqrt(Sqr(v2[0]-v1[0])+Sqr(v2[1]-v1[1])+Sqr(v2[2]-v1[2]));
{$endif}
end;

// VectorDistance2 (affine)
//
function VectorDistance2(const v1, v2 : TAffineVector) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
{$else}
begin
   Result:=Sqr(v2[0]-v1[0])+Sqr(v2[1]-v1[1])+Sqr(v2[2]-v1[2]);
{$endif}
end;

// VectorDistance2 (hmg)
//
function VectorDistance2(const v1, v2 : TVector) : Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
{$else}
begin
   Result:=Sqr(v2[0]-v1[0])+Sqr(v2[1]-v1[1])+Sqr(v2[2]-v1[2]);
{$endif}
end;

// VectorPerpendicular
//
function VectorPerpendicular(const V, N : TAffineVector) : TAffineVector;
var
   dot : Single;
begin
   dot:=VectorDotProduct(V, N);
   Result[X]:=V[X]-Dot * N[X];
   Result[Y]:=V[Y]-Dot * N[Y];
   Result[Z]:=V[Z]-Dot * N[Z];
end;

// VectorReflect
//
function VectorReflect(const V, N: TAffineVector): TAffineVector;
begin
   Result:=VectorCombine(V, N, 1, -2*VectorDotProduct(V, N));
end;

// RotateVector
//
procedure RotateVector(var vector : TVector; const axis : TAffineVector; angle: Single);
var
   rotMatrix : TMatrix4f;
begin
   rotMatrix:=CreateRotationMatrix(axis, Angle);
   vector:=VectorTransform(vector, rotMatrix);
end;

// RotateVector
//
procedure RotateVector(var vector : TVector; const axis : TVector; angle : Single); overload;
var
   rotMatrix : TMatrix4f;
begin
   rotMatrix:=CreateRotationMatrix(PAffineVector(@axis)^, Angle);
   vector:=VectorTransform(vector, rotMatrix);
end;

// RotateVectorAroundY
//
procedure RotateVectorAroundY(var v : TAffineVector; alpha : Single);
var
   c, s, v0 : Single;
begin
   SinCos(alpha, s, c);
   v0:=v[0];
   v[0]:=c*v0+s*v[2];
   v[2]:=c*v[2]-s*v0;
end;

// VectorRotateAroundX (func)
//
function VectorRotateAroundX(const v : TAffineVector; alpha : Single) : TAffineVector;
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   Result[0]:=v[0];
   Result[1]:=c*v[1]+s*v[2];
   Result[2]:=c*v[2]-s*v[1];
end;

// VectorRotateAroundY (func)
//
function VectorRotateAroundY(const v : TAffineVector; alpha : Single) : TAffineVector;
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   Result[1]:=v[1];
   Result[0]:=c*v[0]+s*v[2];
   Result[2]:=c*v[2]-s*v[0];
end;

// VectorRotateAroundY (proc)
//
procedure VectorRotateAroundY(const v : TAffineVector; alpha : Single; var vr : TAffineVector);
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   vr[1]:=v[1];
   vr[0]:=c*v[0]+s*v[2];
   vr[2]:=c*v[2]-s*v[0];
end;

// VectorRotateAroundZ (func)
//
function VectorRotateAroundZ(const v : TAffineVector; alpha : Single) : TAffineVector;
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   Result[0]:=c*v[0]+s*v[1];
   Result[1]:=c*v[1]-s*v[0];
   Result[2]:=v[2];
end;

// AbsVector (hmg)
//
procedure AbsVector(var v : TVector);
begin
  v[0]:=Abs(v[0]);
  v[1]:=Abs(v[1]);
  v[2]:=Abs(v[2]);
  v[3]:=Abs(v[3]);
end;

// AbsVector (affine)
//
procedure AbsVector(var v : TAffineVector);
begin
  v[0]:=Abs(v[0]);
  v[1]:=Abs(v[1]);
  v[2]:=Abs(v[2]);
end;

// VectorAbs (hmg)
//
function VectorAbs(const v : TVector) : TVector;
begin
   Result[0]:=Abs(v[0]);
   Result[1]:=Abs(v[1]);
   Result[2]:=Abs(v[2]);
   Result[3]:=Abs(v[3]);
end;

// VectorAbs (affine)
//
function VectorAbs(const v : TAffineVector) : TAffineVector;
begin
   Result[0]:=Abs(v[0]);
   Result[1]:=Abs(v[1]);
   Result[2]:=Abs(v[2]);
end;

// SetMatrix (single->double)
//
procedure SetMatrix(var dest : THomogeneousDblMatrix; const src : TMatrix);
var
   i : Integer;
begin
   for i:=X to W do begin
      dest[i, X]:=src[i, X];
      dest[i, Y]:=src[i, Y];
      dest[i, Z]:=src[i, Z];
      dest[i, W]:=src[i, W];
   end;
end;

// SetMatrix (hmg->affine)
//
procedure SetMatrix(var dest : TAffineMatrix; const src : TMatrix);
begin
   dest[0, 0]:=src[0, 0]; dest[0, 1]:=src[0, 1]; dest[0, 2]:=src[0, 2];
   dest[1, 0]:=src[1, 0]; dest[1, 1]:=src[1, 1]; dest[1, 2]:=src[1, 2];
   dest[2, 0]:=src[2, 0]; dest[2, 1]:=src[2, 1]; dest[2, 2]:=src[2, 2];
end;

// SetMatrix (affine->hmg)
//
procedure SetMatrix(var dest : TMatrix; const src : TAffineMatrix);
begin
   dest[0, 0]:=src[0, 0]; dest[0, 1]:=src[0, 1]; dest[0, 2]:=src[0, 2]; dest[0, 3]:=0;
   dest[1, 0]:=src[1, 0]; dest[1, 1]:=src[1, 1]; dest[1, 2]:=src[1, 2]; dest[1, 3]:=0;
   dest[2, 0]:=src[2, 0]; dest[2, 1]:=src[2, 1]; dest[2, 2]:=src[2, 2]; dest[2, 3]:=0;
   dest[3, 0]:=0;         dest[3, 1]:=0;         dest[3, 2]:=0;         dest[3, 3]:=1;
end;

// SetMatrixRow
//
procedure SetMatrixRow(var dest : TMatrix; rowNb : Integer; const aRow : TVector);
begin
   dest[0, rowNb]:=aRow[0];
   dest[1, rowNb]:=aRow[1];
   dest[2, rowNb]:=aRow[2];
   dest[3, rowNb]:=aRow[3];
end;

// CreateScaleMatrix (affine)
//
function CreateScaleMatrix(const v : TAffineVector) : TMatrix;
begin
   Result:=IdentityHmgMatrix;
   Result[X, X]:=v[X];
   Result[Y, Y]:=v[Y];
   Result[Z, Z]:=v[Z];
end;

// CreateScaleMatrix (Hmg)
//
function CreateScaleMatrix(const v : TVector) : TMatrix;
begin
   Result:=IdentityHmgMatrix;
   Result[X, X]:=V[X];
   Result[Y, Y]:=V[Y];
   Result[Z, Z]:=V[Z];
end;

// CreateTranslationMatrix (affine)
//
function CreateTranslationMatrix(const V: TAffineVector): TMatrix;
begin
   Result:=IdentityHmgMatrix;
   Result[W, X]:=V[X];
   Result[W, Y]:=V[Y];
   Result[W, Z]:=V[Z];
end;

// CreateTranslationMatrix (hmg)
//
function CreateTranslationMatrix(const V: TVector): TMatrix;
begin
   Result:=IdentityHmgMatrix;
   Result[W, X]:=V[X];
   Result[W, Y]:=V[Y];
   Result[W, Z]:=V[Z];
end;

// CreateScaleAndTranslationMatrix
//
function CreateScaleAndTranslationMatrix(const scale, offset : TVector): TMatrix;
begin
   Result:=IdentityHmgMatrix;
   Result[X, X]:=scale[X];   Result[W, X]:=offset[X];
   Result[Y, Y]:=scale[Y];   Result[W, Y]:=offset[Y];
   Result[Z, Z]:=scale[Z];   Result[W, Z]:=offset[Z];
end;

// CreateRotationMatrixX
//
function CreateRotationMatrixX(const sine, cosine: Single) : TMatrix;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=1;
   Result[Y, Y]:=cosine;
   Result[Y, Z]:=sine;
   Result[Z, Y]:=-sine;
   Result[Z, Z]:=cosine;
   Result[W, W]:=1;
end;

// CreateRotationMatrixX
//
function CreateRotationMatrixX(const angle : Single) : TMatrix;
var
   s, c : Single;
begin
   SinCos(angle, s, c);
   Result:=CreateRotationMatrixX(s, c);
end;

// CreateRotationMatrixY
//
function CreateRotationMatrixY(const sine, cosine: Single): TMatrix;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=cosine;
   Result[X, Z]:=-sine;
   Result[Y, Y]:=1;
   Result[Z, X]:=sine;
   Result[Z, Z]:=cosine;
   Result[W, W]:=1;
end;

// CreateRotationMatrixY
//
function CreateRotationMatrixY(const angle : Single) : TMatrix;
var
   s, c : Single;
begin
   SinCos(angle, s, c);
   Result:=CreateRotationMatrixY(s, c);
end;

// CreateRotationMatrixZ
//
function CreateRotationMatrixZ(const sine, cosine: Single): TMatrix;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=cosine;
   Result[X, Y]:=sine;
   Result[Y, X]:=-sine;
   Result[Y, Y]:=cosine;
   Result[Z, Z]:=1;
   Result[W, W]:=1;
end;

// CreateRotationMatrixZ
//
function CreateRotationMatrixZ(const angle : Single) : TMatrix;
var
   s, c : Single;
begin
   SinCos(angle, s, c);
   Result:=CreateRotationMatrixZ(s, c);
end;

// CreateRotationMatrix (affine)
//
function CreateRotationMatrix(const anAxis : TAffineVector; angle : Single) : TMatrix;
var
   axis : TAffineVector;
   cosine, sine, one_minus_cosine : Single;
begin
   SinCos(angle, sine, cosine);
   one_minus_cosine:=1-cosine;
   axis:=VectorNormalize(anAxis);

   Result[X, X]:=(one_minus_cosine * axis[0] * axis[0]) + cosine;
   Result[X, Y]:=(one_minus_cosine * axis[0] * axis[1]) - (axis[2] * sine);
   Result[X, Z]:=(one_minus_cosine * axis[2] * axis[0]) + (axis[1] * sine);
   Result[X, W]:=0;

   Result[Y, X]:=(one_minus_cosine * axis[0] * axis[1]) + (axis[2] * sine);
   Result[Y, Y]:=(one_minus_cosine * axis[1] * axis[1]) + cosine;
   Result[Y, Z]:=(one_minus_cosine * axis[1] * axis[2]) - (axis[0] * sine);
   Result[Y, W]:=0;

   Result[Z, X]:=(one_minus_cosine * axis[2] * axis[0]) - (axis[1] * sine);
   Result[Z, Y]:=(one_minus_cosine * axis[1] * axis[2]) + (axis[0] * sine);
   Result[Z, Z]:=(one_minus_cosine * axis[2] * axis[2]) + cosine;
   Result[Z, W]:=0;

   Result[W, X]:=0;
   Result[W, Y]:=0;
   Result[W, Z]:=0;
   Result[W, W]:=1;
end;

// CreateRotationMatrix (hmg)
//
function CreateRotationMatrix(const anAxis : TVector; angle : Single) : TMatrix;
begin
   Result:=CreateRotationMatrix(PAffineVector(@anAxis)^, angle);
end;

// CreateAffineRotationMatrix
//
function CreateAffineRotationMatrix(const anAxis: TAffineVector; angle: Single): TAffineMatrix;
var
   axis : TAffineVector;
   cosine, sine, one_minus_cosine : Single;
begin
   SinCos(Angle, Sine, Cosine);
   one_minus_cosine:=1 - cosine;
   axis:=VectorNormalize(anAxis);

   Result[X, X]:=(one_minus_cosine * Sqr(Axis[0])) + Cosine;
   Result[X, Y]:=(one_minus_cosine * Axis[0] * Axis[1]) - (Axis[2] * Sine);
   Result[X, Z]:=(one_minus_cosine * Axis[2] * Axis[0]) + (Axis[1] * Sine);

   Result[Y, X]:=(one_minus_cosine * Axis[0] * Axis[1]) + (Axis[2] * Sine);
   Result[Y, Y]:=(one_minus_cosine * Sqr(Axis[1])) + Cosine;
   Result[Y, Z]:=(one_minus_cosine * Axis[1] * Axis[2]) - (Axis[0] * Sine);

   Result[Z, X]:=(one_minus_cosine * Axis[2] * Axis[0]) - (Axis[1] * Sine);
   Result[Z, Y]:=(one_minus_cosine * Axis[1] * Axis[2]) + (Axis[0] * Sine);
   Result[Z, Z]:=(one_minus_cosine * Sqr(Axis[2])) + Cosine;
end;

// MatrixMultiply (3x3 func)
//
function MatrixMultiply(const M1, M2 : TAffineMatrix) : TAffineMatrix;
begin
{$ifndef GEOMETRY_NO_ASM}
   if vSIMD=1 then begin
      asm
         db $0F,$0E               /// femms
         xchg eax, ecx

         db $0F,$6E,$7A,$08       /// movd        mm7,[edx+8]
         db $0F,$6E,$72,$20       /// movd        mm6,[edx+32]
         db $0F,$62,$7A,$14       /// punpckldq   mm7,[edx+20]
         db $0F,$6F,$01           /// movq        mm0,[ecx]
         db $0F,$6E,$59,$08       /// movd        mm3,[ecx+8]
         db $0F,$6F,$C8           /// movq        mm1,mm0
         db $0F,$0F,$C7,$B4       /// pfmul       mm0,mm7
         db $0F,$6F,$D1           /// movq        mm2,mm1
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$0A,$B4       /// pfmul       mm1,[edx]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$52,$0C,$B4   /// pfmul       mm2,[edx+12]
         db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
         db $0F,$6F,$E3           /// movq        mm4,mm3
         db $0F,$62,$DB           /// punpckldq   mm3,mm3
         db $0F,$0F,$5A,$18,$B4   /// pfmul       mm3,[edx+24]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$E6,$B4       /// pfmul       mm4,mm6
         db $0F,$6F,$69,$0C       /// movq        mm5,[ecx+12]
         db $0F,$0F,$D3,$9E       /// pfadd       mm2,mm3
         db $0F,$6E,$59,$14       /// movd        mm3,[ecx+20]
         db $0F,$0F,$E0,$9E       /// pfadd       mm4,mm0
         db $0F,$6F,$CD           /// movq        mm1,mm5
         db $0F,$7F,$10           /// movq        [eax],mm2
         db $0F,$0F,$EF,$B4       /// pfmul       mm5,mm7
         db $0F,$7E,$60,$08       /// movd        [eax+8],mm4
         db $0F,$6F,$D1           /// movq        mm2,mm1
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$6F,$41,$18       /// movq        mm0,[ecx+24]
         db $0F,$0F,$0A,$B4       /// pfmul       mm1,[edx]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$52,$0C,$B4   /// pfmul       mm2,[edx+12]
         db $0F,$0F,$ED,$AE       /// pfacc       mm5,mm5
         db $0F,$6F,$E3           /// movq        mm4,mm3
         db $0F,$62,$DB           /// punpckldq   mm3,mm3
         db $0F,$0F,$5A,$18,$B4   /// pfmul       mm3,[edx+24]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$E6,$B4       /// pfmul       mm4,mm6
         db $0F,$6F,$C8           /// movq        mm1,mm0
         db $0F,$0F,$D3,$9E       /// pfadd       mm2,mm3
         db $0F,$6E,$59,$20       /// movd        mm3,[ecx+32]
         db $0F,$0F,$E5,$9E       /// pfadd       mm4,mm5
         db $0F,$0F,$C7,$B4       /// pfmul       mm0,mm7
         db $0F,$7F,$50,$0C       /// movq        [eax+12],mm2
         db $0F,$6F,$D1           /// movq        mm2,mm1
         db $0F,$7E,$60,$14       /// movd        [eax+20],mm4
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$0A,$B4       /// pfmul       mm1,[edx]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$52,$0C,$B4   /// pfmul       mm2,[edx+12]
         db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
         db $0F,$0F,$F3,$B4       /// pfmul       mm6,mm3
         db $0F,$62,$DB           /// punpckldq   mm3,mm3
         db $0F,$0F,$5A,$18,$B4   /// pfmul       mm3,[edx+24]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$F0,$9E       /// pfadd       mm6,mm0
         db $0F,$0F,$D3,$9E       /// pfadd       mm2,mm3
         
         db $0F,$7E,$70,$20       /// movd        [eax+32],mm6
         db $0F,$7F,$50,$18       /// movq        [eax+24],mm2
         db $0F,$0E               /// femms
      end;
   end else {$endif} begin
      Result[X, X]:= M1[X, X]*M2[X, X]+M1[X, Y]*M2[Y, X]+M1[X, Z]*M2[Z, X];
      Result[X, Y]:= M1[X, X]*M2[X, Y]+M1[X, Y]*M2[Y, Y]+M1[X, Z]*M2[Z, Y];
      Result[X, Z]:= M1[X, X]*M2[X, Z]+M1[X, Y]*M2[Y, Z]+M1[X, Z]*M2[Z, Z];
      Result[Y, X]:= M1[Y, X]*M2[X, X]+M1[Y, Y]*M2[Y, X]+M1[Y, Z]*M2[Z, X];
      Result[Y, Y]:= M1[Y, X]*M2[X, Y]+M1[Y, Y]*M2[Y, Y]+M1[Y, Z]*M2[Z, Y];
      Result[Y, Z]:= M1[Y, X]*M2[X, Z]+M1[Y, Y]*M2[Y, Z]+M1[Y, Z]*M2[Z, Z];
      Result[Z, X]:= M1[Z, X]*M2[X, X]+M1[Z, Y]*M2[Y, X]+M1[Z, Z]*M2[Z, X];
      Result[Z, Y]:= M1[Z, X]*M2[X, Y]+M1[Z, Y]*M2[Y, Y]+M1[Z, Z]*M2[Z, Y];
      Result[Z, Z]:= M1[Z, X]*M2[X, Z]+M1[Z, Y]*M2[Y, Z]+M1[Z, Z]*M2[Z, Z];
   end;
end;

// MatrixMultiply (4x4, func)
//
function MatrixMultiply(const M1, M2: TMatrix): TMatrix;
begin
{$ifndef GEOMETRY_NO_ASM}
   if vSIMD=1 then begin
      asm
         xchg eax, ecx
         db $0F,$6F,$01           /// movq        mm0,[ecx]
         db $0F,$6F,$49,$08       /// movq        mm1,[ecx+8]
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0, [edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$10       /// movq        mm0,[ecx+16]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$18       /// movq        mm1,[ecx+24]
         db $0F,$7F,$38           /// movq        [eax],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$08       /// movq        [eax+8],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$20       /// movq        mm0,[ecx+32]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$28       /// movq        mm1,[ecx+40]
         db $0F,$7F,$78,$10       /// movq        [eax+16],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$18       /// movq        [eax+24],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$30       /// movq        mm0,[ecx+48]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$38       /// movq        mm1,[ecx+56]
         db $0F,$7F,$78,$20       /// movq        [eax+32],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$28       /// movq        [eax+40],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$7F,$78,$30       /// movq        [eax+48],mm7
         db $0F,$7F,$58,$38       /// movq        [eax+56],mm3
         db $0F,$0E               /// femms
      end;
   end else {$endif} begin
      Result[X,X]:=M1[X,X]*M2[X,X]+M1[X,Y]*M2[Y,X]+M1[X,Z]*M2[Z,X]+M1[X,W]*M2[W,X];
      Result[X,Y]:=M1[X,X]*M2[X,Y]+M1[X,Y]*M2[Y,Y]+M1[X,Z]*M2[Z,Y]+M1[X,W]*M2[W,Y];
      Result[X,Z]:=M1[X,X]*M2[X,Z]+M1[X,Y]*M2[Y,Z]+M1[X,Z]*M2[Z,Z]+M1[X,W]*M2[W,Z];
      Result[X,W]:=M1[X,X]*M2[X,W]+M1[X,Y]*M2[Y,W]+M1[X,Z]*M2[Z,W]+M1[X,W]*M2[W,W];
      Result[Y,X]:=M1[Y,X]*M2[X,X]+M1[Y,Y]*M2[Y,X]+M1[Y,Z]*M2[Z,X]+M1[Y,W]*M2[W,X];
      Result[Y,Y]:=M1[Y,X]*M2[X,Y]+M1[Y,Y]*M2[Y,Y]+M1[Y,Z]*M2[Z,Y]+M1[Y,W]*M2[W,Y];
      Result[Y,Z]:=M1[Y,X]*M2[X,Z]+M1[Y,Y]*M2[Y,Z]+M1[Y,Z]*M2[Z,Z]+M1[Y,W]*M2[W,Z];
      Result[Y,W]:=M1[Y,X]*M2[X,W]+M1[Y,Y]*M2[Y,W]+M1[Y,Z]*M2[Z,W]+M1[Y,W]*M2[W,W];
      Result[Z,X]:=M1[Z,X]*M2[X,X]+M1[Z,Y]*M2[Y,X]+M1[Z,Z]*M2[Z,X]+M1[Z,W]*M2[W,X];
      Result[Z,Y]:=M1[Z,X]*M2[X,Y]+M1[Z,Y]*M2[Y,Y]+M1[Z,Z]*M2[Z,Y]+M1[Z,W]*M2[W,Y];
      Result[Z,Z]:=M1[Z,X]*M2[X,Z]+M1[Z,Y]*M2[Y,Z]+M1[Z,Z]*M2[Z,Z]+M1[Z,W]*M2[W,Z];
      Result[Z,W]:=M1[Z,X]*M2[X,W]+M1[Z,Y]*M2[Y,W]+M1[Z,Z]*M2[Z,W]+M1[Z,W]*M2[W,W];
      Result[W,X]:=M1[W,X]*M2[X,X]+M1[W,Y]*M2[Y,X]+M1[W,Z]*M2[Z,X]+M1[W,W]*M2[W,X];
      Result[W,Y]:=M1[W,X]*M2[X,Y]+M1[W,Y]*M2[Y,Y]+M1[W,Z]*M2[Z,Y]+M1[W,W]*M2[W,Y];
      Result[W,Z]:=M1[W,X]*M2[X,Z]+M1[W,Y]*M2[Y,Z]+M1[W,Z]*M2[Z,Z]+M1[W,W]*M2[W,Z];
      Result[W,W]:=M1[W,X]*M2[X,W]+M1[W,Y]*M2[Y,W]+M1[W,Z]*M2[Z,W]+M1[W,W]*M2[W,W];
   end;
end;

// MatrixMultiply (4x4, proc)
//
procedure MatrixMultiply(const M1, M2: TMatrix; var MResult: TMatrix);
begin
   MResult:=MatrixMultiply(M1, M2);
end;

// VectorTransform
//
function VectorTransform(const V: TVector; const M: TMatrix) : TVector;
begin
{$ifndef GEOMETRY_NO_ASM}
   if vSIMD=1 then begin
      asm
        db $0F,$6F,$00           /// movq        mm0,[eax]
        db $0F,$6F,$48,$08       /// movq        mm1,[eax+8]
        db $0F,$6F,$22           /// movq        mm4,[edx]
        db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
        db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
        db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
        db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
        db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
        db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
        db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
        db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2

        db $0F,$7F,$39           /// movq        [ecx],mm7
        db $0F,$7F,$59,$08       /// movq        [ecx+8],mm3
        db $0F,$0E               /// femms
      end
   end else {$endif} begin
      Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + V[W] * M[W, X];
      Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + V[W] * M[W, Y];
      Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + V[W] * M[W, Z];
      Result[W]:=V[X] * M[X, W] + V[Y] * M[Y, W] + V[Z] * M[Z, W] + V[W] * M[W, W];
   end;
end;

// VectorTransform
//
function VectorTransform(const V: TVector; const M: TAffineMatrix): TVector;
begin
   Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X];
   Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y];
   Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z];
   Result[W]:=V[W];
end;

// VectorTransform
//
function VectorTransform(const V: TAffineVector; const M: TMatrix): TAffineVector;
begin
   Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + M[W, X];
   Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + M[W, Y];
   Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + M[W, Z];
end;

// VectorTransform
//
function VectorTransform(const V: TAffineVector; const M: TAffineMatrix): TAffineVector;
begin
{$ifndef GEOMETRY_NO_ASM}
   if vSIMD=1 then begin
      asm
        db $0F,$6F,$00           /// movq        mm0,[eax]
        db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
        db $0F,$6E,$62,$08       /// movd        mm4,[edx+8]
        db $0F,$6F,$D8           /// movq        mm3,mm0
        db $0F,$6E,$52,$20       /// movd        mm2,[edx+32]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$62,$62,$14       /// punpckldq   mm4,[edx+20]
        db $0F,$0F,$02,$B4       /// pfmul       mm0,[edx]
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$D1,$B4       /// pfmul       mm2,mm1
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$20,$B4       /// pfmul       mm4,[eax]
        db $0F,$0F,$5A,$0C,$B4   /// pfmul       mm3,[edx+12]
        db $0F,$0F,$4A,$18,$B4   /// pfmul       mm1,[edx+24]
        db $0F,$0F,$E4,$AE       /// pfacc       mm4,mm4
        db $0F,$0F,$D8,$9E       /// pfadd       mm3,mm0
        db $0F,$0F,$E2,$9E       /// pfadd       mm4,mm2
        db $0F,$0F,$D9,$9E       /// pfadd       mm3,mm1

        db $0F,$7E,$61,$08       /// movd        [ecx+8],mm4
        db $0F,$7F,$19           /// movq        [ecx],mm3
        db $0F,$0E               /// femms
      end;
   end else {$endif} begin
      Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X];
      Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y];
      Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z];
   end;
end;

// MatrixDeterminant (affine)
//
function MatrixDeterminant(const M: TAffineMatrix): Single;
begin
  Result:=  M[X, X] * (M[Y, Y] * M[Z, Z] - M[Z, Y] * M[Y, Z])
          - M[X, Y] * (M[Y, X] * M[Z, Z] - M[Z, X] * M[Y, Z])
          + M[X, Z] * (M[Y, X] * M[Z, Y] - M[Z, X] * M[Y, Y]);
end;

// MatrixDetInternal
//
function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  Result:=  a1 * (b2 * c3 - b3 * c2)
          - b1 * (a2 * c3 - a3 * c2)
          + c1 * (a2 * b3 - a3 * b2);
end;

// MatrixDeterminant (hmg)
//
function MatrixDeterminant(const M: TMatrix): Single;
begin
  Result:= M[X, X]*MatrixDetInternal(M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          -M[X, Y]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          +M[X, Z]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, W], M[Z, W], M[W, W])
          -M[X, W]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z]);
end;

// AdjointMatrix
//
procedure AdjointMatrix(var M : TMatrix); 
var
   a1, a2, a3, a4,
   b1, b2, b3, b4,
   c1, c2, c3, c4,
   d1, d2, d3, d4: Single;
begin
    a1:= M[X, X]; b1:= M[X, Y];
    c1:= M[X, Z]; d1:= M[X, W];
    a2:= M[Y, X]; b2:= M[Y, Y];
    c2:= M[Y, Z]; d2:= M[Y, W];
    a3:= M[Z, X]; b3:= M[Z, Y];
    c3:= M[Z, Z]; d3:= M[Z, W];
    a4:= M[W, X]; b4:= M[W, Y];
    c4:= M[W, Z]; d4:= M[W, W];

    // row column labeling reversed since we transpose rows & columns
    M[X, X]:= MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X]:=-MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X]:= MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X]:=-MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[X, Y]:=-MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y]:= MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y]:=-MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y]:= MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[X, Z]:= MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z]:=-MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z]:= MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z]:=-MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[X, W]:=-MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W]:= MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W]:=-MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W]:= MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

// AdjointMatrix (affine)
//
procedure AdjointMatrix(var M : TAffineMatrix);
var
   a1, a2, a3,
   b1, b2, b3,
   c1, c2, c3: Single;
begin
   a1:= M[X, X]; a2:= M[X, Y]; a3:= M[X, Z];
   b1:= M[Y, X]; b2:= M[Y, Y]; b3:= M[Y, Z];
   c1:= M[Z, X]; c2:= M[Z, Y]; c3:= M[Z, Z];
   M[X, X]:= (b2*c3-c2*b3);
   M[Y, X]:=-(b1*c3-c1*b3);
   M[Z, X]:= (b1*c2-c1*b2);

   M[X, Y]:=-(a2*c3-c2*a3);
   M[Y, Y]:= (a1*c3-c1*a3);
   M[Z, Y]:=-(a1*c2-c1*a2);

   M[X, Z]:= (a2*b3-b2*a3);
   M[Y, Z]:=-(a1*b3-b1*a3);
   M[Z, Z]:= (a1*b2-b1*a2);
end;

// ScaleMatrix (affine)
//
procedure ScaleMatrix(var M : TAffineMatrix; const factor : Single);
var
   i : Integer;
begin
   for i:=0 to 2 do begin
      M[I, 0]:=M[I, 0] * Factor;
      M[I, 1]:=M[I, 1] * Factor;
      M[I, 2]:=M[I, 2] * Factor;
   end;
end;

// ScaleMatrix (hmg)
//
procedure ScaleMatrix(var M : TMatrix; const factor : Single);
var
   i : Integer;
begin
   for i:=0 to 3 do begin
      M[I, 0]:=M[I, 0] * Factor;
      M[I, 1]:=M[I, 1] * Factor;
      M[I, 2]:=M[I, 2] * Factor;
      M[I, 3]:=M[I, 3] * Factor;
   end;
end;

// TranslateMatrix (affine vec)
//
procedure TranslateMatrix(var M : TMatrix; const v : TAffineVector);
begin
   M[3][0]:=M[3][0]+v[0];
   M[3][1]:=M[3][1]+v[1];
   M[3][2]:=M[3][2]+v[2];
end;

// TranslateMatrix
//
procedure TranslateMatrix(var M : TMatrix; const v : TVector);
begin
   M[3][0]:=M[3][0]+v[0];
   M[3][1]:=M[3][1]+v[1];
   M[3][2]:=M[3][2]+v[2];
end;

// NormalizeMatrix
//
procedure NormalizeMatrix(var M : TMatrix);
begin
   M[0][3]:=0; NormalizeVector(M[0]);
   M[1][3]:=0; NormalizeVector(M[1]);
   M[2]:=VectorCrossProduct(M[0], M[1]);
   M[0]:=VectorCrossProduct(M[1], M[2]);
   M[3]:=WHmgVector;
end;

// TransposeMatrix
//
procedure TransposeMatrix(var M: TAffineMatrix);
var
   f : Single;
begin
   f:=M[0, 1]; M[0, 1]:=M[1, 0]; M[1, 0]:=f;
   f:=M[0, 2]; M[0, 2]:=M[2, 0]; M[2, 0]:=f;
   f:=M[1, 2]; M[1, 2]:=M[2, 1]; M[2, 1]:=f;
end;

// TransposeMatrix
//
procedure TransposeMatrix(var M: TMatrix);
var
   f : Single;
begin
   f:=M[0, 1]; M[0, 1]:=M[1, 0]; M[1, 0]:=f;
   f:=M[0, 2]; M[0, 2]:=M[2, 0]; M[2, 0]:=f;
   f:=M[0, 3]; M[0, 3]:=M[3, 0]; M[3, 0]:=f;
   f:=M[1, 2]; M[1, 2]:=M[2, 1]; M[2, 1]:=f;
   f:=M[1, 3]; M[1, 3]:=M[3, 1]; M[3, 1]:=f;
   f:=M[2, 3]; M[2, 3]:=M[3, 2]; M[3, 2]:=f;
end;

// InvertMatrix
//
procedure InvertMatrix(var M : TMatrix);
var
   det : Single;
begin
   det:=MatrixDeterminant(M);
   if Abs(Det)<EPSILON then
      M:=IdentityHmgMatrix
   else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1/det);
   end;
end;

// InvertMatrix (affine)
//
procedure InvertMatrix(var M : TAffineMatrix);
var
   det : Single;
begin
   det:=MatrixDeterminant(M);
   if Abs(Det)<EPSILON then
      M:=IdentityMatrix
   else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1/det);
   end;
end;

// transpose_scale_m33
//
procedure transpose_scale_m33(const src : TMatrix; var dest : TMatrix; var scale : Single);
// EAX src
// EDX dest
// ECX scale
begin
{$ifndef GEOMETRY_NO_ASM}
   asm
      //   dest[0][0]:=scale*src[0][0];
      fld   dword ptr [ecx]
      fld   st(0)
      fmul  dword ptr [eax]
      fstp  dword ptr [edx]
      //   dest[1][0]:=scale*src[0][1];
      fld   st(0)
      fmul  dword ptr [eax+4]
      fstp  dword ptr [edx+16]
      //   dest[2][0]:=scale*src[0][2];
      fmul  dword ptr [eax+8]
      fstp  dword ptr [edx+32]

      //   dest[0][1]:=scale*src[1][0];
      fld   dword ptr [ecx]
      fld   st(0)
      fmul  dword ptr [eax+16]
      fstp  dword ptr [edx+4]
      //   dest[1][1]:=scale*src[1][1];
      fld   st(0)
      fmul  dword ptr [eax+20]
      fstp  dword ptr [edx+20]
      //   dest[2][1]:=scale*src[1][2];
      fmul  dword ptr [eax+24]
      fstp  dword ptr [edx+36]

      //   dest[0][2]:=scale*src[2][0];
      fld   dword ptr [ecx]
      fld   st(0)
      fmul  dword ptr [eax+32]
      fstp  dword ptr [edx+8]
      //   dest[1][2]:=scale*src[2][1];
      fld   st(0)
      fmul  dword ptr [eax+36]
      fstp  dword ptr [edx+24]
      //   dest[2][2]:=scale*src[2][2];
      fmul  dword ptr [eax+40]
      fstp  dword ptr [edx+40]
   end;
{$else}
   dest[0][0]:=scale*src[0][0];
   dest[1][0]:=scale*src[0][1];
   dest[2][0]:=scale*src[0][2];
   dest[0][1]:=scale*src[1][0];
   dest[1][1]:=scale*src[1][1];
   dest[2][1]:=scale*src[1][2];
   dest[0][2]:=scale*src[2][0];
   dest[1][2]:=scale*src[2][1];
   dest[2][2]:=scale*src[2][2];
{$endif}
end;

// AnglePreservingMatrixInvert
//
function AnglePreservingMatrixInvert(const mat : TMatrix) : TMatrix;
var
   scale : Single;
begin
   scale:=VectorNorm(mat[0]);

   // Is the submatrix A singular?
   if Abs(scale)<EPSILON then begin
      // Matrix M has no inverse
      Result:=IdentityHmgMatrix;
      Exit;
   end else begin
      // Calculate the inverse of the square of the isotropic scale factor
      scale:=1.0/scale;
   end;

   // Fill in last row while CPU is busy with the division
   Result[0][3]:=0.0;
   Result[1][3]:=0.0;
   Result[2][3]:=0.0;
   Result[3][3]:=1.0;

   // Transpose and scale the 3 by 3 upper-left submatrix
   transpose_scale_m33(mat, Result, scale);

   // Calculate -(transpose(A) / s*s) C
   Result[3][0]:=-( Result[0][0]*mat[3][0]
                   +Result[1][0]*mat[3][1]
                   +Result[2][0]*mat[3][2]);
   Result[3][1]:=-( Result[0][1]*mat[3][0]
                   +Result[1][1]*mat[3][1]
                   +Result[2][1]*mat[3][2]);
   Result[3][2]:=-( Result[0][2]*mat[3][0]
                   +Result[1][2]*mat[3][1]
                   +Result[2][2]*mat[3][2]);
end;

// MatrixDecompose
//
function MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;
var
   I, J: Integer;
   LocMat, pmat, invpmat : TMatrix;
   prhs, psol: TVector;
   row0, row1, row2 : TAffineVector;
   f : Single;
begin
  Result:=False;
  locmat:=M;
  // normalize the matrix
  if locmat[W, W] = 0 then Exit;
  for I:=0 to 3 do
    for J:=0 to 3 do
      locmat[I, J]:=locmat[I, J] / locmat[W, W];

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat:=locmat;
  for I:=0 to 2 do pmat[I, W]:=0;
  pmat[W, W]:=1;

  if MatrixDeterminant(pmat) = 0 then Exit;

  // First, isolate perspective.  This is the messiest.
  if (locmat[X, W] <> 0) or (locmat[Y, W] <> 0) or (locmat[Z, W] <> 0) then begin
    // prhs is the right hand side of the equation.
    prhs[X]:=locmat[X, W];
    prhs[Y]:=locmat[Y, W];
    prhs[Z]:=locmat[Z, W];
    prhs[W]:=locmat[W, W];

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat:=pmat;
    InvertMatrix(invpmat);
    TransposeMatrix(invpmat);
    psol:=VectorTransform(prhs, invpmat);

    // stuff the answer away
    Tran[ttPerspectiveX]:=psol[X];
    Tran[ttPerspectiveY]:=psol[Y];
    Tran[ttPerspectiveZ]:=psol[Z];
    Tran[ttPerspectiveW]:=psol[W];

    // clear the perspective partition
    locmat[X, W]:=0;
    locmat[Y, W]:=0;
    locmat[Z, W]:=0;
    locmat[W, W]:=1;
  end else begin
    // no perspective
    Tran[ttPerspectiveX]:=0;
    Tran[ttPerspectiveY]:=0;
    Tran[ttPerspectiveZ]:=0;
    Tran[ttPerspectiveW]:=0;
  end;

  // next take care of translation (easy)
  for I:=0 to 2 do begin
    Tran[TTransType(Ord(ttTranslateX) + I)]:=locmat[W, I];
    locmat[W, I]:=0;
  end;

  // now get scale and shear
  SetVector(row0, locmat[0]);
  SetVector(row1, locmat[1]);
  SetVector(row2, locmat[2]);

  // compute X scale factor and normalize first row
  Tran[ttScaleX]:=VectorNorm(row0);
  VectorScale(row0, RSqrt(Tran[ttScaleX]));

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY]:=VectorDotProduct(row0, row1);
  f:=-Tran[ttShearXY];
  CombineVector(row1, row0, f);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY]:=VectorNorm(row1);
  VectorScale(row1, RSqrt(Tran[ttScaleY]));
  Tran[ttShearXY]:=Tran[ttShearXY]/Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ]:=VectorDotProduct(row0, row2);
  f:=-Tran[ttShearXZ];
  CombineVector(row2, row0, f);
  Tran[ttShearYZ]:=VectorDotProduct(row1, row2);
  f:=-Tran[ttShearYZ];
  CombineVector(row2, row1, f);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ]:=VectorNorm(row2);
  VectorScale(row2, RSqrt(Tran[ttScaleZ]));
  Tran[ttShearXZ]:=Tran[ttShearXZ] / tran[ttScaleZ];
  Tran[ttShearYZ]:=Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if VectorDotProduct(row0, VectorCrossProduct(row1, row2)) < 0 then begin
    for I:=0 to 2 do
      Tran[TTransType(Ord(ttScaleX) + I)]:=-Tran[TTransType(Ord(ttScaleX) + I)];
    NegateVector(row0);
    NegateVector(row1);
    NegateVector(row2);
  end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY]:=VectorGeometry.ArcSin(-row0[Z]);
  if cos(Tran[ttRotateY]) <> 0 then begin
    Tran[ttRotateX]:=ArcTan2(row1[Z], row2[Z]);
    Tran[ttRotateZ]:=ArcTan2(row0[Y], row0[X]);
  end else begin
    tran[ttRotateX]:=ArcTan2(row1[X], row1[Y]);
    tran[ttRotateZ]:=0;
  end;
  // All done!
  Result:=True;
end;

// CalcPlaneNormal (func, affine)
//
function CalcPlaneNormal(const p1, p2, p3 : TAffineVector) : TAffineVector;
var
   v1, v2 : TAffineVector;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(v1, v2, Result);
   NormalizeVector(Result);
end;

// CalcPlaneNormal (proc, affine)
//
procedure CalcPlaneNormal(const p1, p2, p3 : TAffineVector; var vr : TAffineVector);
var
   v1, v2 : TAffineVector;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(v1, v2, vr);
   NormalizeVector(vr);
end;

// CalcPlaneNormal (proc, hmg)
//
procedure CalcPlaneNormal(const p1, p2, p3 : TVector; var vr : TAffineVector); overload;
var
   v1, v2 : TVector;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(v1, v2, vr);
   NormalizeVector(vr);
end;

// PlaneMake (point + normal, affine)
//
function PlaneMake(const point, normal : TAffineVector) : THmgPlane;
begin
   PAffineVector(@Result)^:=normal;
   Result[3]:=-VectorDotProduct(point, normal);
end;

// PlaneMake (point + normal, hmg)
//
function PlaneMake(const point, normal : TVector) : THmgPlane;
begin
   PAffineVector(@Result)^:=PAffineVector(@normal)^;
   Result[3]:=-VectorDotProduct(PAffineVector(@point)^, PAffineVector(@normal)^);
end;

// PlaneMake (3 points, affine)
//
function PlaneMake(const p1, p2, p3 : TAffineVector) : THmgPlane;
begin
   CalcPlaneNormal(p1, p2, p3, PAffineVector(@Result)^);
   Result[3]:=-VectorDotProduct(p1, PAffineVector(@Result)^);
end;

// PlaneMake (3 points, hmg)
//
function PlaneMake(const p1, p2, p3 : TVector) : THmgPlane;
begin
   CalcPlaneNormal(p1, p2, p3, PAffineVector(@Result)^);
   Result[3]:=-VectorDotProduct(p1, PAffineVector(@Result)^);
end;

// SetPlane
//
procedure SetPlane(var dest : TDoubleHmgPlane; const src : THmgPlane);
begin
   dest[0]:=src[0];
   dest[1]:=src[1];
   dest[2]:=src[2];
   dest[3]:=src[3];
end;

// NormalizePlane
//
procedure NormalizePlane(var plane : THmgPlane);
var
   n : Single;
begin
   n:=RSqrt(plane[0]*plane[0]+plane[1]*plane[1]+plane[2]*plane[2]);
   ScaleVector(plane, n);
end;

// PlaneEvaluatePoint (affine)
//
function PlaneEvaluatePoint(const plane : THmgPlane; const point : TAffineVector) : Single;
// EAX contains address of plane
// EDX contains address of point
// result is stored in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]
      FMUL DWORD PTR [EDX]
      FLD DWORD PTR [EAX + 4]
      FMUL DWORD PTR [EDX + 4]
      FADDP
      FLD DWORD PTR [EAX + 8]
      FMUL DWORD PTR [EDX + 8]
      FADDP
      FLD DWORD PTR [EAX + 12]
      FADDP
{$else}
begin
   Result:=plane[0]*point[0]+plane[1]*point[1]+plane[2]*point[2]+plane[3];
{$endif}
end;

// PlaneEvaluatePoint (hmg)
//
function PlaneEvaluatePoint(const plane : THmgPlane; const point : TVector) : Single;
// EAX contains address of plane
// EDX contains address of point
// result is stored in ST(0)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD DWORD PTR [EAX]
      FMUL DWORD PTR [EDX]
      FLD DWORD PTR [EAX + 4]
      FMUL DWORD PTR [EDX + 4]
      FADDP
      FLD DWORD PTR [EAX + 8]
      FMUL DWORD PTR [EDX + 8]
      FADDP
      FLD DWORD PTR [EAX + 12]
      FADDP
{$else}
begin
   Result:=plane[0]*point[0]+plane[1]*point[1]+plane[2]*point[2]+plane[3];
{$endif}
end;

// PointIsInHalfSpace
//
function PointIsInHalfSpace(const point, planePoint, planeNormal : TVector) : Boolean;
{$ifndef GEOMETRY_NO_ASM}
asm
   fld   dword ptr [eax]         // 27
   fsub  dword ptr [edx]
   fmul  dword ptr [ecx]
   fld   dword ptr [eax+4]
   fsub  dword ptr [edx+4]
   fmul  dword ptr [ecx+4]
   faddp
   fld   dword ptr [eax+8]
   fsub  dword ptr [edx+8]
   fmul  dword ptr [ecx+8]
   faddp
   ftst
   fstsw ax
   sahf
   setnbe al
   ffree st(0)
{$else}
begin
   Result:=(PointPlaneDistance(point, planePoint, planeNormal)>0); // 44
{$endif}
end;

// PointIsInHalfSpace
//
function PointIsInHalfSpace(const point, planePoint, planeNormal : TAffineVector) : Boolean;
begin
   Result:=(PointPlaneDistance(point, planePoint, planeNormal)>0);
end;

// PointPlaneDistance
//
function PointPlaneDistance(const point, planePoint, planeNormal : TVector) : Single;
begin
   Result:= (point[0]-planePoint[0])*planeNormal[0]
           +(point[1]-planePoint[1])*planeNormal[1]
           +(point[2]-planePoint[2])*planeNormal[2];
end;

// PointPlaneDistance
//
function PointPlaneDistance(const point, planePoint, planeNormal : TAffineVector) : Single;
begin
   Result:= (point[0]-planePoint[0])*planeNormal[0]
           +(point[1]-planePoint[1])*planeNormal[1]
           +(point[2]-planePoint[2])*planeNormal[2];
end;

// PointLineClosestPoint
//
function PointLineClosestPoint(const point, linePoint, lineDirection : TAffineVector) : TAffineVector;
var
   w : TAffineVector;
   c1, c2, b : Single;
begin
   w:=VectorSubtract(point, linePoint);

   c1:=VectorDotProduct(w, LineDirection);
   c2:=VectorDotProduct(lineDirection, lineDirection);
   b:=c1/c2;

   VectorAdd(linePoint, VectorScale(lineDirection, b), Result);
end;

// PointLineDistance
//
function PointLineDistance(const point, linePoint, lineDirection : TAffineVector) : Single;
var
   pb : TAffineVector;
begin
   pb:=PointLineClosestPoint(point, linePoint, lineDirection);
   Result:=VectorDistance(point, pb);
end;

// PointSegmentClosestPoint
//
function PointSegmentClosestPoint(const point, segmentStart, segmentStop : TAffineVector) : TAffineVector;
var
   w, lineDirection : TAffineVector;
   c1, c2, b : Single;
begin
   lineDirection:=VectorSubtract(segmentStop, segmentStart);
   w:=VectorSubtract(point, segmentStart);

   c1:=VectorDotProduct(w, lineDirection);
   c2:=VectorDotProduct(lineDirection, lineDirection);
   b:=ClampValue(c1/c2, 0, 1);

   VectorAdd(segmentStart, VectorScale(lineDirection, b), Result);
end;

// PointSegmentDistance
//
function PointSegmentDistance(const point, segmentStart, segmentStop : TAffineVector) : Single;
var
   pb : TAffineVector;
begin
   pb:=PointSegmentClosestPoint(point, segmentStart, segmentStop);
   Result:=VectorDistance(point, pb);
end;

// http://geometryalgorithms.com/Archive/algorithm_0104/algorithm_0104B.htm
// SegmentSegmentClosestPoint
//
procedure SegmentSegmentClosestPoint(const S0Start, S0Stop, S1Start, S1Stop : TAffineVector; var Segment0Closest, Segment1Closest : TAffineVector);
const
  cSMALL_NUM = 0.000000001;
var
  u, v,w : TAffineVector;
  a,b,c,smalld,e, largeD, sc, sn, sD, tc, tN, tD : single;
begin
  VectorSubtract(S0Stop, S0Start, u);
  VectorSubtract(S1Stop, S1Start, v);
  VectorSubtract(S0Start, S1Start, w);

  a := VectorDotProduct(u,u);
  b := VectorDotProduct(u,v);
  c := VectorDotProduct(v,v);
  smalld := VectorDotProduct(u,w);
  e := VectorDotProduct(v,w);
  largeD := a*c - b*b;

  sD := largeD;
  tD := largeD;

  if LargeD<cSMALL_NUM then
  begin
    sN := 0.0;
    sD := 1.0;
    tN := e;
    tD := c;
  end else
  begin
    sN := (b*e - c*smallD);
    tN := (a*e - b*smallD);
    if (sN < 0.0) then
    begin
      sN := 0.0;
      tN := e;
      tD := c;
    end
    else if (sN > sD) then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if (tN < 0.0) then
  begin
      tN := 0.0;
      // recompute sc for this edge
      if (-smalld < 0.0) then
          sN := 0.0
      else if (-smalld > a) then
          sN := sD
      else
      begin
          sN := -smalld;
          sD := a;
      end;
  end
  else if (tN > tD) then
  begin
      tN := tD;
      // recompute sc for this edge
      if ((-smallD + b) < 0.0) then
          sN := 0
      else if ((-smallD + b) > a) then
          sN := sD
      else
      begin
          sN := (-smallD + b);
          sD := a;
      end;
   end;

  // finally do the division to get sc and tc
  //sc := (abs(sN) < SMALL_NUM ? 0.0 : sN / sD);
  if abs(sN) < cSMALL_NUM then
    sc := 0
  else
    sc := sN/sD;

  //tc := (abs(tN) < SMALL_NUM ? 0.0 : tN / tD);
  if abs(tN) < cSMALL_NUM then
    tc := 0
  else
    tc := tN/tD;

  // get the difference of the two closest points
  //Vector   dP = w + (sc * u) - (tc * v);  // = S0(sc) - S1(tc)

  Segment0Closest := VectorAdd(S0Start, VectorScale(u, sc));
  Segment1Closest := VectorAdd(S1Start, VectorScale(v, tc));
end;

// SegmentSegmentDistance
//
function SegmentSegmentDistance(const S0Start, S0Stop, S1Start, S1Stop : TAffineVector) : single;
var
  Pb0, PB1 : TAffineVector;
begin
  SegmentSegmentClosestPoint(S0Start, S0Stop, S1Start, S1Stop, PB0, PB1);
  result := VectorDistance(PB0, PB1);
end;

// QuaternionMake
//
function QuaternionMake(const Imag: array of Single; Real: Single): TQuaternion;
// EAX contains address of Imag
// ECX contains address to result vector
// EDX contains highest index of Imag
// Real part is passed on the stack
{$ifndef GEOMETRY_NO_ASM}
asm
      PUSH EDI
      PUSH ESI
      MOV EDI, ECX
      MOV ESI, EAX
      MOV ECX, EDX
      INC ECX
      REP MOVSD
      MOV EAX, [Real]
      MOV [EDI], EAX
      POP ESI
      POP EDI
{$else}
var
   n : Integer;
begin
   n:=Length(Imag);
   if n>=1 then Result.ImagPart[0]:=Imag[0];
   if n>=2 then Result.ImagPart[1]:=Imag[1];
   if n>=3 then Result.ImagPart[2]:=Imag[2];
   Result.RealPart:=real;
{$endif}
end;

// QuaternionConjugate
//
function QuaternionConjugate(const Q : TQuaternion) : TQuaternion;
begin
   Result.ImagPart[0]:=-Q.ImagPart[0];
   Result.ImagPart[1]:=-Q.ImagPart[1];
   Result.ImagPart[2]:=-Q.ImagPart[2];
   Result.RealPart:=Q.RealPart;
end;

// QuaternionMagnitude
//
function QuaternionMagnitude(const q : TQuaternion) : Single;
begin
   Result:=Sqrt(VectorNorm(q.ImagPart)+Sqr(q.RealPart));
end;

// NormalizeQuaternion
//
procedure NormalizeQuaternion(var q : TQuaternion);
var
   m, f : Single;
begin
   m:=QuaternionMagnitude(q);
   if m>EPSILON2 then begin
      f:=1/m;
      ScaleVector(q.ImagPart, f);
      q.RealPart:=q.RealPart*f;
   end else q:=IdentityQuaternion;
end;

// QuaternionFromPoints
//
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
begin
   Result.ImagPart:=VectorCrossProduct(V1, V2);
   Result.RealPart:=Sqrt((VectorDotProduct(V1, V2) + 1)/2);
end;

// QuaternionFromMatrix
//
function QuaternionFromMatrix(const mat : TMatrix) : TQuaternion;
// the matrix must be a rotation matrix!
var
   traceMat, s, invS : Double;
begin
   traceMat := 1 + mat[0,0] + mat[1,1] + mat[2,2];
   if traceMat>EPSILON2 then begin
      s:=Sqrt(traceMat)*2;
      invS:=1/s;
      Result.ImagPart[0]:=(mat[1,2]-mat[2,1])*invS;
      Result.ImagPart[1]:=(mat[2,0]-mat[0,2])*invS;
      Result.ImagPart[2]:=(mat[0,1]-mat[1,0])*invS;
      Result.RealPart   :=0.25*s;
   end else if (mat[0,0]>mat[1,1]) and (mat[0,0]>mat[2,2]) then begin  // Row 0:
      s:=Sqrt(MaxFloat(EPSILON2, cOne+mat[0,0]-mat[1,1]-mat[2,2]))*2;
      invS:=1/s;
      Result.ImagPart[0]:=0.25*s;
      Result.ImagPart[1]:=(mat[0,1]+mat[1,0])*invS;
      Result.ImagPart[2]:=(mat[2,0]+mat[0,2])*invS;
      Result.RealPart   :=(mat[1,2]-mat[2,1])*invS;
   end else if (mat[1,1]>mat[2,2]) then begin  // Row 1:
      s:=Sqrt(MaxFloat(EPSILON2, cOne+mat[1,1]-mat[0,0]-mat[2,2]))*2;
      invS:=1/s;
      Result.ImagPart[0]:=(mat[0,1]+mat[1,0])*invS;
      Result.ImagPart[1]:=0.25*s;
      Result.ImagPart[2]:=(mat[1,2]+mat[2,1])*invS;
      Result.RealPart   :=(mat[2,0]-mat[0,2])*invS;
   end else begin  // Row 2:
      s:=Sqrt(MaxFloat(EPSILON2, cOne+mat[2,2]-mat[0,0]-mat[1,1]))*2;
      invS:=1/s;
      Result.ImagPart[0]:=(mat[2,0]+mat[0,2])*invS;
      Result.ImagPart[1]:=(mat[1,2]+mat[2,1])*invS;
      Result.ImagPart[2]:=0.25*s;
      Result.RealPart   :=(mat[0,1]-mat[1,0])*invS;
   end;
   NormalizeQuaternion(Result);
end;

// QuaternionMultiply
//
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
var
   Temp : TQuaternion;
begin
   Temp.RealPart:=qL.RealPart * qR.RealPart - qL.ImagPart[X] * qR.ImagPart[X]
                  - qL.ImagPart[Y] * qR.ImagPart[Y] - qL.ImagPart[Z] * qR.ImagPart[Z];
   Temp.ImagPart[X]:=qL.RealPart * qR.ImagPart[X] + qL.ImagPart[X] * qR.RealPart
                     + qL.ImagPart[Y] * qR.ImagPart[Z] - qL.ImagPart[Z] * qR.ImagPart[Y];
   Temp.ImagPart[Y]:=qL.RealPart * qR.ImagPart[Y] + qL.ImagPart[Y] * qR.RealPart
                     + qL.ImagPart[Z] * qR.ImagPart[X] - qL.ImagPart[X] * qR.ImagPart[Z];
   Temp.ImagPart[Z]:=qL.RealPart * qR.ImagPart[Z] + qL.ImagPart[Z] * qR.RealPart
                     + qL.ImagPart[X] * qR.ImagPart[Y] - qL.ImagPart[Y] * qR.ImagPart[X];
   Result:=Temp;
end;

// QuaternionToMatrix
//
function QuaternionToMatrix(quat : TQuaternion) : TMatrix;
var
   w, x, y, z, xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
begin
   NormalizeQuaternion(quat);
   w := quat.RealPart;
   x := quat.ImagPart[0];
   y := quat.ImagPart[1];
   z := quat.ImagPart[2];
   xx := x * x;
   xy := x * y;
   xz := x * z;
   xw := x * w;
   yy := y * y;
   yz := y * z;
   yw := y * w;
   zz := z * z;
   zw := z * w;
   Result[0, 0] := 1 - 2 * ( yy + zz );
   Result[1, 0] :=     2 * ( xy - zw );
   Result[2, 0] :=     2 * ( xz + yw );
   Result[3, 0] := 0;
   Result[0, 1] :=     2 * ( xy + zw );
   Result[1, 1] := 1 - 2 * ( xx + zz );
   Result[2, 1] :=     2 * ( yz - xw );
   Result[3, 1] := 0;
   Result[0, 2] :=     2 * ( xz - yw );
   Result[1, 2] :=     2 * ( yz + xw );
   Result[2, 2] := 1 - 2 * ( xx + yy );
   Result[3, 2] := 0;
   Result[0, 3] := 0;
   Result[1, 3] := 0;
   Result[2, 3] := 0;
   Result[3, 3] := 1;
end;

//QuaternionToAffineMatrix
//
function QuaternionToAffineMatrix(quat : TQuaternion) : TAffineMatrix;
var
   w, x, y, z, xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
begin
   NormalizeQuaternion(quat);
   w := quat.RealPart;
   x := quat.ImagPart[0];
   y := quat.ImagPart[1];
   z := quat.ImagPart[2];
   xx := x * x;
   xy := x * y;
   xz := x * z;
   xw := x * w;
   yy := y * y;
   yz := y * z;
   yw := y * w;
   zz := z * z;
   zw := z * w;
   Result[0, 0] := 1 - 2 * ( yy + zz );
   Result[1, 0] :=     2 * ( xy - zw );
   Result[2, 0] :=     2 * ( xz + yw );
   Result[0, 1] :=     2 * ( xy + zw );
   Result[1, 1] := 1 - 2 * ( xx + zz );
   Result[2, 1] :=     2 * ( yz - xw );
   Result[0, 2] :=     2 * ( xz - yw );
   Result[1, 2] :=     2 * ( yz + xw );
   Result[2, 2] := 1 - 2 * ( xx + yy );
end;

// QuaternionFromAngleAxis
//
function QuaternionFromAngleAxis(const angle  : Single; const axis : TAffineVector) : TQuaternion;
var
   f, s, c : Single;
begin
   SinCos(DegToRad(angle*cOneDotFive), s, c);
	Result.RealPart:=c;
   f:=s/VectorLength(axis);
   Result.ImagPart[0]:=axis[0]*f;
   Result.ImagPart[1]:=axis[1]*f;
   Result.ImagPart[2]:=axis[2]*f;
end;

// QuaternionFromRollPitchYaw
//
function QuaternionFromRollPitchYaw(const r, p, y : Single) : TQuaternion;
var
   qp, qy : TQuaternion;
begin
   Result:=QuaternionFromAngleAxis(r, ZVector);
   qp:=QuaternionFromAngleAxis(p, XVector);
   qy:=QuaternionFromAngleAxis(y, YVector);

   Result:=QuaternionMultiply(qp, Result);
   Result:=QuaternionMultiply(qy, Result);
end;

// QuaternionFromEuler
//
function QuaternionFromEuler(const x, y, z: Single; eulerOrder: TEulerOrder): TQuaternion;
// input angles in degrees
var
   gimbalLock: Boolean;
   quat1, quat2: TQuaternion;

   function EulerToQuat(const X, Y, Z: Single; eulerOrder: TEulerOrder) : TQuaternion;
   const
      cOrder : array [Low(TEulerOrder)..High(TEulerOrder)] of array [1..3] of Byte =
         ( (1, 2, 3), (1, 3, 2), (2, 1, 3),     // eulXYZ, eulXZY, eulYXZ,
           (3, 1, 2), (2, 3, 1), (3, 2, 1) );   // eulYZX, eulZXY, eulZYX
   var
      q : array [1..3] of TQuaternion;
   begin
      q[cOrder[eulerOrder][1]]:=QuaternionFromAngleAxis(X, XVector);
      q[cOrder[eulerOrder][2]]:=QuaternionFromAngleAxis(Y, YVector);
      q[cOrder[eulerOrder][3]]:=QuaternionFromAngleAxis(Z, ZVector);
      Result:=QuaternionMultiply(q[2], q[3]);
      Result:=QuaternionMultiply(q[1], Result);
   end;

const
   SMALL_ANGLE = 0.001;
begin
   NormalizeDegAngle(x);
   NormalizeDegAngle(y);
   NormalizeDegAngle(z);
   case EulerOrder of
      eulXYZ, eulZYX: GimbalLock := Abs(Abs(y) - 90.0) <= EPSILON2; // cos(Y) = 0;
      eulYXZ, eulZXY: GimbalLock := Abs(Abs(x) - 90.0) <= EPSILON2; // cos(X) = 0;
      eulXZY, eulYZX: GimbalLock := Abs(Abs(z) - 90.0) <= EPSILON2; // cos(Z) = 0;
   else
      Assert(False);
      gimbalLock:=False;
   end;
   if gimbalLock then begin
      case EulerOrder of
        eulXYZ, eulZYX: quat1 := EulerToQuat(x, y - SMALL_ANGLE, z, EulerOrder);
        eulYXZ, eulZXY: quat1 := EulerToQuat(x - SMALL_ANGLE, y, z, EulerOrder);
        eulXZY, eulYZX: quat1 := EulerToQuat(x, y, z - SMALL_ANGLE, EulerOrder);
      end;
      case EulerOrder of
        eulXYZ, eulZYX: quat2 := EulerToQuat(x, y + SMALL_ANGLE, z, EulerOrder);
        eulYXZ, eulZXY: quat2 := EulerToQuat(x + SMALL_ANGLE, y, z, EulerOrder);
        eulXZY, eulYZX: quat2 := EulerToQuat(x, y, z + SMALL_ANGLE, EulerOrder);
      end;
      Result := QuaternionSlerp(quat1, quat2, 0.5);
   end else begin
      Result := EulerToQuat(x, y, z, EulerOrder);
   end;
end;

// QuaternionToPoints
//
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);
var
   s, invS : Single;
begin
   s:=Q.ImagPart[X]*Q.ImagPart[X]+Q.ImagPart[Y]*Q.ImagPart[Y];
   if s=0 then
      SetAffineVector(ArcFrom, 0, 1, 0)
   else begin
      invS:=RSqrt(s);
      SetAffineVector(ArcFrom, -Q.ImagPart[Y]*invS, Q.ImagPart[X]*invS, 0);
   end;
   ArcTo[X]:=Q.RealPart*ArcFrom[X]-Q.ImagPart[Z]*ArcFrom[Y];
   ArcTo[Y]:=Q.RealPart*ArcFrom[Y]+Q.ImagPart[Z]*ArcFrom[X];
   ArcTo[Z]:=Q.ImagPart[X]*ArcFrom[Y]-Q.ImagPart[Y]*ArcFrom[X];
   if Q.RealPart<0 then
      SetAffineVector(ArcFrom, -ArcFrom[X], -ArcFrom[Y], 0);
end;

// LnXP1
//
function LnXP1(X: Extended): Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
        FLDLN2
        MOV     AX, WORD PTR X+8  // exponent
        FLD     X
        CMP     AX, $3FFD         // .4225
        JB      @@1
        FLD1
        FADD
        FYL2X
        JMP     @@2
@@1:
        FYL2XP1
@@2:
        FWAIT
{$else}
begin
   Result:=Math.LnXP1(X);
{$endif}
end;

// Log10
//
function Log10(X: Extended): Extended;
// Log.10(X):=Log.2(X) * Log.10(2)
{$ifndef GEOMETRY_NO_ASM}
asm
        FLDLG2     { Log base ten of 2 }
        FLD     X
        FYL2X
{$else}
begin
   Result:=Math.Log10(X);
{$endif}
end;

// Log2
//
function Log2(X: Extended): Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
        FLD1
        FLD     X
        FYL2X
{$else}
begin
   Result:=Math.Log2(X);
{$endif}
end;

// Log2
//
function Log2(X: Single): Single;
{$ifndef GEOMETRY_NO_ASM}
asm
        FLD1
        FLD     X
        FYL2X
{$else}
begin
   {$HINTS OFF}
   Result:=Math.Log2(X);
   {$HINTS ON}
{$endif}
end;

// LogN
//
function LogN(Base, X: Extended): Extended;
// Log.N(X):=Log.2(X) / Log.2(N)
{$ifndef GEOMETRY_NO_ASM}
asm
        FLD1
        FLD     X
        FYL2X
        FLD1
        FLD     Base
        FYL2X
        FDIV
{$else}
begin
   Result:=Math.LogN(Base, X);
{$endif}
end;

// IntPower
//
function IntPower(Base: Extended; Exponent: Integer) : Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
        mov     ecx, eax
        cdq
        fld1                      { Result:=1 }
        xor     eax, edx
        sub     eax, edx          { eax:=Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X:=Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result:=Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result:=1 / Result }
@@3:
{$else}
begin
   Result:=Math.IntPower(Base, Exponent);
{$endif}
end;

// Power
//
function Power(const base, exponent : Single) : Single;
begin
   {$HINTS OFF}
   if exponent=cZero then
      Result:=cOne
   else if (base=cZero) and (exponent>cZero) then
      Result:=cZero
   else if RoundInt(exponent)=exponent then
     Result:=Power(base, Integer(Round(exponent)))
   else Result:=Exp(exponent*Ln(base));
   {$HINTS ON}
end;

// Power (int exponent)
//
function Power(Base: Single; Exponent: Integer): Single;
{$ifndef GEOMETRY_NO_ASM}
asm
        mov     ecx, eax
        cdq
        fld1                      { Result:=1 }
        xor     eax, edx
        sub     eax, edx          { eax:=Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X:=Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result:=Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result:=1 / Result }
@@3:
{$else}
begin
   {$HINTS OFF}
   Result:=Math.Power(Base, Exponent);
   {$HINTS ON}
{$endif}
end;

// DegToRad (extended)
//
function DegToRad(const Degrees: Extended): Extended;
begin
   Result:=Degrees*(PI/180);
end;

// DegToRad (single)
//
function DegToRad(const Degrees : Single) : Single;
//   Result:=Degrees * cPIdiv180;
// don't laugh, Delphi's compiler manages to make a nightmare of this one
// with pushs, pops, etc. in its default compile... (this one is twice faster !)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EBP+8]
      FMUL cPIdiv180
{$else}
begin
   Result:=Degrees*cPIdiv180;
{$endif}
end;

// RadToDeg (extended)
//
function RadToDeg(const Radians: Extended): Extended;
begin
   Result:=Radians*(180/PI);
end;

// RadToDeg (single)
//
function RadToDeg(const Radians: Single): Single;
//   Result:=Radians * c180divPI;
// don't laugh, Delphi's compiler manages to make a nightmare of this one
// with pushs, pops, etc. in its default compile... (this one is twice faster !)
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  DWORD PTR [EBP+8]
      FMUL c180divPI
{$else}
begin
   Result:=Radians*c180divPI;
{$endif}
end;

// NormalizeAngle
//
function NormalizeAngle(angle : Single) : Single;
begin
   Result:=angle-Int(angle*cInv2PI)*c2PI;
   if Result>PI then
      Result:=Result-2*PI
   else if Result<-PI then
      Result:=Result+2*PI;
end;

// NormalizeDegAngle
//
function NormalizeDegAngle(angle : Single) : Single;
begin
   Result:=angle-Int(angle*cInv360)*c360;
   if Result>c180 then
      Result:=Result-c360
   else if Result<-c180 then
      Result:=Result+c360;
end;

// SinCos (Extended)
//
procedure SinCos(const Theta: Extended; var Sin, Cos: Extended);
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
{$ifndef GEOMETRY_NO_ASM}
asm
   FLD  Theta
   FSINCOS
   FSTP TBYTE PTR [EDX]    // cosine
   FSTP TBYTE PTR [EAX]    // sine
{$else}
begin
   Math.SinCos(Theta, Sin, Cos);
{$endif}
end;

// SinCos (Double)
//
procedure SinCos(const Theta: Double; var Sin, Cos: Double);
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
{$ifndef GEOMETRY_NO_ASM}
asm
   FLD  Theta
   FSINCOS
   FSTP QWORD PTR [EDX]    // cosine
   FSTP QWORD PTR [EAX]    // sine
{$else}
var
   s, c : Extended;
begin
   Math.SinCos(Theta, s, c);
   {$HINTS OFF}
   Sin:=s; Cos:=c;
   {$HINTS ON}
{$endif}
end;

// SinCos (Single)
//
procedure SinCos(const Theta: Single; var Sin, Cos: Single);
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
{$ifndef GEOMETRY_NO_ASM}
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
{$else}
var
   s, c : Extended;
begin
   Math.SinCos(Theta, s, c);
   {$HINTS OFF}
   Sin:=s; Cos:=c;
   {$HINTS ON}
{$endif}
end;

// SinCos (Extended w radius)
//
procedure SinCos(const theta, radius : Double; var Sin, Cos: Extended);
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
{$ifndef GEOMETRY_NO_ASM}
asm
   FLD  theta
   FSINCOS
   FMUL radius
   FSTP TBYTE PTR [EDX]    // cosine
   FMUL radius
   FSTP TBYTE PTR [EAX]    // sine
{$else}
var
   s, c : Extended;
begin
   Math.SinCos(Theta, s, c);
   Sin:=s*radius; Cos:=c*radius;
{$endif}
end;

// SinCos (Double w radius)
//
procedure SinCos(const theta, radius : Double; var Sin, Cos: Double);
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
{$ifndef GEOMETRY_NO_ASM}
asm
   FLD  theta
   FSINCOS
   FMUL radius
   FSTP QWORD PTR [EDX]    // cosine
   FMUL radius
   FSTP QWORD PTR [EAX]    // sine
{$else}
var
   s, c : Extended;
begin
   Math.SinCos(Theta, s, c);
   Sin:=s*radius; Cos:=c*radius;
{$endif}
end;

// SinCos (Single w radius)
//
procedure SinCos(const theta, radius : Single; var Sin, Cos: Single);
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
{$ifndef GEOMETRY_NO_ASM}
asm
   FLD  theta
   FSINCOS
   FMUL radius
   FSTP DWORD PTR [EDX]    // cosine
   FMUL radius
   FSTP DWORD PTR [EAX]    // sine
{$else}
var
   s, c : Extended;
begin
   Math.SinCos(Theta, s, c);
   Sin:=s*radius; Cos:=c*radius;
{$endif}
end;

// PrepareSinCosCache
//
procedure PrepareSinCosCache(var s, c : array of Single;
                             startAngle, stopAngle : Single);
var
   i : Integer;
   d, alpha, beta : Single;
begin
   Assert((High(s)=High(c)) and (Low(s)=Low(c)));
   stopAngle:=stopAngle+1e-5;
   if High(s)>Low(s) then
      d:=cPIdiv180*(stopAngle-startAngle)/(High(s)-Low(s))
   else d:=0;

   if High(s)-Low(s)<1000 then begin
      // Fast computation (approx 5.5x)
      alpha:=2*Sqr(Sin(d*0.5));
      beta:=Sin(d);
      SinCos(startAngle*cPIdiv180, s[Low(s)], c[Low(s)]);
      for i:=Low(s) to High(s)-1 do begin
         // Make use of the incremental formulae:
         // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
         // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
         c[i+1]:= c[i] - alpha * c[i] - beta * s[i];
         s[i+1]:= s[i] - alpha * s[i] + beta * c[i];
      end;
   end else begin
      // Slower, but maintains precision when steps are small
      startAngle:=startAngle*cPIdiv180;
      for i:=Low(s) to High(s) do
         SinCos((i-Low(s))*d+startAngle, s[i], c[i]);
   end;
end;

// ArcCos (Extended)
//
function ArcCos(const x : Extended): Extended;
begin
   Result:=ArcTan2(Sqrt(1 - Sqr(X)), X);
end;

// ArcCos (Single)
//
function ArcCos(const x : Single): Single;
// Result:=ArcTan2(Sqrt(c1 - X * X), X);
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD   X
      FMUL  ST, ST
      FSUBR cOne
      FSQRT
      FLD   X
      FPATAN
{$else}
begin
   {$HINTS OFF}
   Result:=Math.ArcCos(X);
   {$HINTS ON}
{$endif}
end;

// ArcSin (Extended)
//
function ArcSin(const x : Extended) : Extended;
begin
   Result:=ArcTan2(X, Sqrt(1 - Sqr(X)))
end;

// ArcSin (Single)
//
function ArcSin(const x : Single) : Single;
//   Result:=ArcTan2(X, Sqrt(1 - X * X))
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD   X
      FLD   ST
      FMUL  ST, ST
      FSUBR cOne
      FSQRT
      FPATAN
{$else}
begin
   {$HINTS OFF}
   Result:=Math.ArcSin(X);
   {$HINTS ON}
{$endif}
end;

// ArcTan2 (Extended)
//
function ArcTan2(const y, x : Extended) : Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  Y
      FLD  X
      FPATAN
{$else}
begin
   Result:=Math.ArcTan2(y, x);
{$endif}
end;

// ArcTan2 (Single)
//
function ArcTan2(const y, x : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  Y
      FLD  X
      FPATAN
{$else}
begin
   {$HINTS OFF}
   Result:=Math.ArcTan2(y, x);
   {$HINTS ON}
{$endif}
end;

// FastArcTan2
//
function FastArcTan2(y, x : Single) : Single;
// accuracy of about 0.07 rads
const
   cEpsilon : Single = 1e-10;
var
   abs_y : Single;
begin
   abs_y:=Abs(y)+cEpsilon;      // prevent 0/0 condition
   if y<0 then begin
      if x>=0 then
         Result:=cPIdiv4*(x-abs_y)/(x+abs_y)-cPIdiv4
      else Result:=cPIdiv4*(x+abs_y)/(abs_y-x)-c3PIdiv4;
   end else begin
      if x>=0 then
         Result:=cPIdiv4-cPIdiv4*(x-abs_y)/(x+abs_y)
      else Result:=c3PIdiv4-cPIdiv4*(x+abs_y)/(abs_y-x);
   end;
end;

// Tan (Extended)
//
function Tan(const x : Extended) : Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  X
      FPTAN
      FSTP ST(0)      // FPTAN pushes 1.0 after result
{$else}
begin
   Result:=Math.Tan(x);
{$endif}
end;

// Tan (Single)
//
function Tan(const x : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  X
      FPTAN
      FSTP ST(0)      // FPTAN pushes 1.0 after result
{$else}
begin
   {$HINTS OFF}
   Result:=Math.Tan(x);
   {$HINTS ON}
{$endif}
end;

// CoTan (Extended)
//
function CoTan(const x : Extended) : Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  X
      FPTAN
      FDIVRP
{$else}
begin
   Result:=Math.CoTan(x);
{$endif}
end;

// CoTan (Single)
//
function CoTan(const x : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD  X
      FPTAN
      FDIVRP
{$else}
begin
   {$HINTS OFF}
   Result:=Math.CoTan(x);
   {$HINTS ON}
{$endif}
end;

// Sinh
//
function Sinh(const x : Single) : Single;
{$ifdef GEOMETRY_NO_ASM}
begin
   Result:=0.5*(Exp(x)-Exp(-x));
{$else}
asm
      fld   x
      call  RegisterBasedExp
      fld   x
      fchs
      call  RegisterBasedExp
      fsub
      fmul  cOneDotFive
{$endif}
end;

// Sinh
//
function Sinh(const x : Double) : Double;
{$ifdef GEOMETRY_NO_ASM}
begin
   Result:=0.5*(Exp(x)-Exp(-x));
{$else}
asm
      fld   x
      call  RegisterBasedExp
      fld   x
      fchs
      call  RegisterBasedExp
      fsub
      fmul  cOneDotFive
{$endif}
end;

// Cosh
//
function Cosh(const x : Single) : Single;
{$ifdef GEOMETRY_NO_ASM}
begin
   Result:=0.5*(Exp(x)+Exp(-x));
{$else}
asm
      fld   x
      call  RegisterBasedExp
      fld   x
      fchs
      call  RegisterBasedExp
      fadd
      fmul  cOneDotFive
{$endif}
end;

// Cosh
//
function Cosh(const x : Double) : Double;
{$ifdef GEOMETRY_NO_ASM}
begin
   Result:=0.5*(Exp(x)+Exp(-x));
{$else}
asm
      fld   x
      call  RegisterBasedExp
      fld   x
      fchs
      call  RegisterBasedExp
      fadd
      fmul  cOneDotFive
{$endif}
end;

// RSqrt
//
function RSqrt(v : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      lea eax, [ebp+8]
      db $0F,$6E,$00           /// movd mm0, [eax]
      db $0F,$0F,$C8,$97       /// pfrsqrt  mm1, mm0

      db $0F,$6F,$D1           /// movq     mm2, mm1
      db $0F,$0F,$C9,$B4       /// pfmul    mm1, mm1
      db $0F,$0F,$C8,$A7       /// pfrsqit1 mm1, mm0
      db $0F,$0F,$CA,$B6       /// pfrcpit2 mm1, mm2

      db $0F,$7E,$08           /// movd [eax], mm1
      db $0F,$0E               /// femms
      fld dword ptr [eax]
      jmp @@End

@@FPU:
      fld v
      fsqrt
      fld1
      fdivr
@@End:
{$else}
begin
   Result:=1/Sqrt(v);
{$endif}
end;

// ISqrt
//
function ISqrt(i : Integer) : Integer;
{$ifndef GEOMETRY_NO_ASM}
asm
      push     eax
      test     vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6E,$04,$24       /// movd     mm0, [esp]
      db $0F,$0F,$C8,$0D       /// pi2fd    mm1, mm0
      db $0F,$0F,$D1,$97       /// pfrsqrt  mm2, mm1
      db $0F,$0F,$DA,$96       /// pfrcp    mm3, mm2
      db $0F,$0F,$E3,$1D       /// pf2id    mm4, mm3
      db $0F,$7E,$24,$24       /// movd     [esp], mm4
      db $0F,$0E               /// femms
      pop      eax
      ret
@@FPU:
      fild     dword ptr [esp]
      fsqrt
      fistp    dword ptr [esp]
      pop      eax
{$else}
begin
   {$HINTS OFF}
   Result:=Round(Sqrt(i));
   {$HINTS ON}
{$endif}
end;

// ILength
//
function ILength(x, y : Integer) : Integer;
{$ifndef GEOMETRY_NO_ASM}
asm
      push     edx
      push     eax
      fild     dword ptr [esp]
      fmul     ST(0), ST(0)
      fild     dword ptr [esp+4]
      fmul     ST(0), ST(0)
      faddp
      fsqrt
      fistp    dword ptr [esp+4]
      pop      edx
      pop      eax
{$else}
begin
   {$HINTS OFF}
   Result:=Round(Sqrt(x*x+y*y));
   {$HINTS ON}
{$endif}
end;

// ILength
//
function ILength(x, y, z : Integer) : Integer;
{$ifndef GEOMETRY_NO_ASM}
asm
      push     ecx
      push     edx
      push     eax
      fild     dword ptr [esp]
      fmul     ST(0), ST(0)
      fild     dword ptr [esp+4]
      fmul     ST(0), ST(0)
      faddp
      fild     dword ptr [esp+8]
      fmul     ST(0), ST(0)
      faddp
      fsqrt
      fistp    dword ptr [esp+8]
      pop      ecx
      pop      edx
      pop      eax
{$else}
begin
   {$HINTS OFF}
   Result:=Round(Sqrt(x*x+y*y+z*z));
   {$HINTS ON}
{$endif}
end;

// RLength
//
function RLength(x, y : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
      fld  x
      fmul x
      fld  y
      fmul y
      fadd
      fsqrt
      fld1
      fdivr
{$else}
begin
   Result:=1/Sqrt(x*x+y*y);
{$endif}
end;

// RegisterBasedExp
//
{$ifndef GEOMETRY_NO_ASM}
procedure RegisterBasedExp;
asm   // Exp(x) = 2^(x.log2(e))
      fldl2e
      fmul
      fld      st(0)
      frndint
      fsub     st(1), st
      fxch     st(1)
      f2xm1
      fld1
      fadd
      fscale
      fstp     st(1)
end;
{$endif}

// RandomPointOnSphere
//
procedure RandomPointOnSphere(var p : TAffineVector);
var
   t, w : Single;
begin
   p[2]:=2*Random-1;
   t:=2*PI*Random;
   w:=Sqrt(1-p[2]*p[2]);
   SinCos(t, w, p[1], p[0]);
end;

// RoundInt (single)
//
function RoundInt(v : Single) : Single;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD     v
      FRNDINT
{$else}
begin
   {$HINTS OFF}
   Result:=Int(v+cOneDotFive);
   {$HINTS ON}
{$endif}
end;

// RoundInt (extended)
//
function RoundInt(v : Extended) : Extended;
{$ifndef GEOMETRY_NO_ASM}
asm
      FLD     v
      FRNDINT
{$else}
begin
   Result:=Int(v+0.5);
{$endif}
end;

{$ifndef GEOMETRY_NO_ASM}

// Trunc64 (extended)
//
function Trunc64(v : Extended) : Int64;
asm
      SUB     ESP,12
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FISTP   qword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
      POP     EDX
end;

// Trunc (single)
//
function Trunc(v : Single) : Integer;
asm
      SUB     ESP,8
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FISTP   dword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
end;

// Int (Extended)
//
function Int(v : Extended) : Extended;
asm
      SUB     ESP,4
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FRNDINT
      FLDCW   [ESP]
      ADD     ESP,4
end;

// Int (Single)
//
function Int(v : Single) : Single;
asm
      SUB     ESP,4
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FRNDINT
      FLDCW   [ESP]
      ADD     ESP,4
end;

// Frac (Extended)
//
function Frac(v : Extended) : Extended;
asm
      SUB     ESP,4
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FLD     ST
      FRNDINT
      FSUB
      FLDCW   [ESP]
      ADD     ESP,4
end;

// Frac (Extended)
//
function Frac(v : Single) : Single;
asm
      SUB     ESP,4
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     v
      FLD     ST
      FRNDINT
      FSUB
      FLDCW   [ESP]
      ADD     ESP,4
end;

// Round64 (Single);
//
function Round64(v : Single) : Int64;
asm
      SUB     ESP,8
      FLD     v
      FISTP   qword ptr [ESP]
      POP     EAX
      POP     EDX
end;

// Round64 (Extended);
//
function Round64(v : Extended) : Int64;
asm
      FLD      v
      FISTP    qword ptr [v]           // use v as storage to place the result
      MOV      EAX, dword ptr [v]
      MOV      EDX, dword ptr [v+4]
end;

// Round (Single);
//
function Round(v : Single) : Integer;
asm
      FLD     v
      FISTP   DWORD PTR [v]     // use v as storage to place the result
      MOV     EAX, [v]
end;

{$else}

function Trunc(X: Extended): Int64;
begin
   Result:=System.Trunc(X);
end;

function Round(X: Extended): Int64;
begin
   Result:=System.Round(X);
end;

function Frac(X: Extended): Extended;
begin
   Result:=System.Frac(X);
end;

{$endif}

// Ceil64 (Extended)
//
function Ceil64(v : Extended) : Int64; overload;
begin
   if Frac(v)>0 then
      Result:=Trunc(v)+1
   else Result:=Trunc(v);
end;

// Ceil (Single)
//
function Ceil(v : Single) : Integer; overload;
begin
   {$HINTS OFF}
   if Frac(v)>0 then
      Result:=Trunc(v)+1
   else Result:=Trunc(v);
   {$HINTS ON}
end;

// Floor64 (Extended)
//
function Floor64(v : Extended) : Int64; overload;
begin
  if v<0 then
      Result:=Trunc(v)-1
   else Result:=Trunc(v);
end;

// Floor (Single)
//
function Floor(v : Single) : Integer; overload;
begin
   {$HINTS OFF}
   if v<0 then
      Result:=Trunc(v)-1
   else Result:=Trunc(v);
   {$HINTS ON}
end;

// Sign
//
function Sign(x : Single) : Integer;
begin
   if x<0 then
      Result:=-1
   else if x>0 then
      Result:=1
   else Result:=0;
end;

// ScaleAndRound
//
function ScaleAndRound(i : Integer; var s : Single) : Integer;
{$ifndef GEOMETRY_NO_ASM}
asm
   push  eax
   fild  dword ptr [esp]
   fmul  dword ptr [edx]
   fistp dword ptr [esp]
   pop   eax
{$else}
begin
   {$HINTS OFF}
   Result:=Round(i*s);
   {$HINTS ON}
{$endif}
end;

// IsInRange (single)
//
function IsInRange(const x, a, b : Single) : Boolean;
begin
   if a<b then
      Result:=(a<=x) and (x<=b)
   else Result:=(b<=x) and (x<=a);
end;

// IsInRange (double)
//
function IsInRange(const x, a, b : Double) : Boolean;
begin
   if a<b then
      Result:=(a<=x) and (x<=b)
   else Result:=(b<=x) and (x<=a);
end;

// IsInCube (affine)
//
function IsInCube(const p, d : TAffineVector) : Boolean; overload;
begin
   Result:=    ((p[0]>=-d[0]) and (p[0]<=d[0]))
           and ((p[1]>=-d[1]) and (p[1]<=d[1]))
           and ((p[2]>=-d[2]) and (p[2]<=d[2]));
end;

// IsInCube (hmg)
//
function IsInCube(const p, d : TVector) : Boolean; overload;
begin
   Result:=    ((p[0]>=-d[0]) and (p[0]<=d[0]))
           and ((p[1]>=-d[1]) and (p[1]<=d[1]))
           and ((p[2]>=-d[2]) and (p[2]<=d[2]));
end;

// MinFloat (single)
//
function MinFloat(values : PSingleArray; nbItems : Integer) : Single;
var
   i, k : Integer;
begin
   if nbItems>0 then begin
      k:=0;
      for i:=1 to nbItems-1 do
         if values^[i]<values^[k] then k:=i;
      Result:=values^[k];
   end else Result:=0;
end;

// MinFloat (double)
//
function MinFloat(values : PDoubleArray; nbItems : Integer) : Double;
var
   i, k : Integer;
begin
   if nbItems>0 then begin
      k:=0;
      for i:=1 to nbItems-1 do
         if values^[i]<values^[k] then k:=i;
      Result:=values^[k];
   end else Result:=0;
end;

// MinFloat (extended)
//
function MinFloat(values : PExtendedArray; nbItems : Integer) : Extended;
var
   i, k : Integer;
begin
   if nbItems>0 then begin
      k:=0;
      for i:=1 to nbItems-1 do
         if values^[i]<values^[k] then k:=i;
      Result:=values^[k];
   end else Result:=0;
end;

// MinFloat (array)
//
function MinFloat(const v : array of Single) : Single;
var
   i : Integer;
begin
   if Length(v)>0 then begin
      Result:=v[0];
      for i:=1 to High(v) do
         if v[i]<Result then Result:=v[i];
   end else Result:=0;
end;

// MinFloat (single 2)
//
function MinFloat(const v1, v2 : Single) : Single;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MinFloat (double 2)
//
function MinFloat(const v1, v2 : Double) : Double;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MinFloat (extended 2)
//
function MinFloat(const v1, v2 : Extended) : Extended;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MinFloat
//
function MinFloat(const v1, v2, v3 : Single) : Single;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<=v2 then
      if v1<=v3 then
         Result:=v1
      else if v3<=v2 then
         Result:=v3
      else Result:=v2
   else if v2<=v3 then
      Result:=v2
   else if v3<=v1 then
      Result:=v3
   else result:=v1;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
   fld     v3
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MinFloat (double)
//
function MinFloat(const v1, v2, v3 : Double) : Double;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<=v2 then
      if v1<=v3 then
         Result:=v1
      else if v3<=v2 then
         Result:=v3
      else Result:=v2
   else if v2<=v3 then
      Result:=v2
   else if v3<=v1 then
      Result:=v3
   else result:=v1;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
   fld     v3
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MinFloat
//
function MinFloat(const v1, v2, v3 : Extended) : Extended;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<=v2 then
      if v1<=v3 then
         Result:=v1
      else if v3<=v2 then
         Result:=v3
      else Result:=v2
   else if v2<=v3 then
      Result:=v2
   else if v3<=v1 then
      Result:=v3
   else result:=v1;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
   fld     v3
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DB,$C1                 /// fcmovnb st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MaxFloat (single)
//
function MaxFloat(values : PSingleArray; nbItems : Integer) : Single; overload;
var
   i, k : Integer;
begin
   if nbItems>0 then begin
      k:=0;
      for i:=1 to nbItems-1 do
         if values^[i]>values^[k] then k:=i;
      Result:=values^[k];
   end else Result:=0;
end;

// MaxFloat (double)
//
function MaxFloat(values : PDoubleArray; nbItems : Integer) : Double; overload;
var
   i, k : Integer;
begin
   if nbItems>0 then begin
      k:=0;
      for i:=1 to nbItems-1 do
         if values^[i]>values^[k] then k:=i;
      Result:=values^[k];
   end else Result:=0;
end;

// MaxFloat (extended)
//
function MaxFloat(values : PExtendedArray; nbItems : Integer) : Extended; overload;
var
   i, k : Integer;
begin
   if nbItems>0 then begin
      k:=0;
      for i:=1 to nbItems-1 do
         if values^[i]>values^[k] then k:=i;
      Result:=values^[k];
   end else Result:=0;
end;

// MaxFloat
//
function MaxFloat(const v : array of Single) : Single;
var
   i : Integer;
begin
   if Length(v)>0 then begin
      Result:=v[0];
      for i:=1 to High(v) do
         if v[i]>Result then Result:=v[i];
   end else Result:=0;
end;

// MaxFloat
//
function MaxFloat(const v1, v2 : Single) : Single;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MaxFloat
//
function MaxFloat(const v1, v2 : Double) : Double;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MaxFloat
//
function MaxFloat(const v1, v2 : Extended) : Extended;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MaxFloat
//
function MaxFloat(const v1, v2, v3 : Single) : Single;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>=v2 then
      if v1>=v3 then
         Result:=v1
      else if v3>=v2 then
         Result:=v3
      else Result:=v2
   else if v2>=v3 then
      Result:=v2
   else if v3>=v1 then
      Result:=v3
   else Result:=v1;
{$else}
asm
   fld     v1
   fld     v2
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   ffree   st(1)
   fld     v3
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   ffree   st(1)
{$endif}
end;

// MaxFloat
//
function MaxFloat(const v1, v2, v3 : Double) : Double;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>=v2 then
      if v1>=v3 then
         Result:=v1
      else if v3>=v2 then
         Result:=v3
      else Result:=v2
   else if v2>=v3 then
      Result:=v2
   else if v3>=v1 then
      Result:=v3
   else Result:=v1;
{$else}
asm
   fld     v1
   fld     v2
   fld     v3
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   db $DB,$F2                 /// fcomi   st(0), st(2)
   db $DA,$C2                 /// fcmovb  st(0), st(2)
   ffree   st(2)
   ffree   st(1)
{$endif}
end;

// MaxFloat
//
function MaxFloat(const v1, v2, v3 : Extended) : Extended;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>=v2 then
      if v1>=v3 then
         Result:=v1
      else if v3>=v2 then
         Result:=v3
      else Result:=v2
   else if v2>=v3 then
      Result:=v2
   else if v3>=v1 then
      Result:=v3
   else Result:=v1;
{$else}
asm
   fld     v1
   fld     v2
   fld     v3
   db $DB,$F1                 /// fcomi   st(0), st(1)
   db $DA,$C1                 /// fcmovb  st(0), st(1)
   db $DB,$F2                 /// fcomi   st(0), st(2)
   db $DA,$C2                 /// fcmovb  st(0), st(2)
   ffree   st(2)
   ffree   st(1)
{$endif}
end;

// MinInteger (2 int)
//
function MinInteger(const v1, v2 : Integer) : Integer;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   cmp   eax, edx
   db $0F,$4F,$C2             /// cmovg eax, edx
 {$endif}
end;

// MinInteger (2 card)
//
function MinInteger(const v1, v2 : Cardinal) : Cardinal;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1<v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   cmp   eax, edx
   db $0F,$47,$C2             /// cmova eax, edx
 {$endif}
end;

// MaxInteger (2 int)
//
function MaxInteger(const v1, v2 : Integer) : Integer;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   cmp   eax, edx
   db $0F,$4C,$C2             /// cmovl eax, edx
 {$endif}
end;

// MaxInteger (2 card)
//
function MaxInteger(const v1, v2 : Cardinal) : Cardinal;
{$ifdef GEOMETRY_NO_ASM}
begin
   if v1>v2 then
      Result:=v1
   else Result:=v2;
{$else}
asm
   cmp   eax, edx
   db $0F,$42,$C2             /// cmovb eax, edx
 {$endif}
end;

// TriangleArea
//
function TriangleArea(const p1, p2, p3 : TAffineVector) : Single;
begin
   Result:=0.5*VectorLength(VectorCrossProduct(VectorSubtract(p2, p1),
                                               VectorSubtract(p3, p1)));
end;

// PolygonArea
//
function PolygonArea(const p : PAffineVectorArray; nSides : Integer) : Single;
var
   r : TAffineVector;
   i : Integer;
   p1, p2, p3 : PAffineVector;
begin
   Result:=0;
   if nSides>2 then begin
      RstVector(r);
      p1:=@p[0];
      p2:=@p[1];
      for i:=2 to nSides-1 do begin
         p3:=@p[i];
         AddVector(r, VectorCrossProduct(VectorSubtract(p2^, p1^),
                                         VectorSubtract(p3^, p1^)));
         p2:=p3;
      end;
      Result:=VectorLength(r)*0.5;
   end;
end;

// TriangleSignedArea
//
function TriangleSignedArea(const p1, p2, p3 : TAffineVector) : Single;
begin
   Result:=0.5*( (p2[0]-p1[0])*(p3[1]-p1[1])
                -(p3[0]-p1[0])*(p2[1]-p1[1]));
end;

// PolygonSignedArea
//
function PolygonSignedArea(const p : PAffineVectorArray; nSides : Integer) : Single;
var
   i : Integer;
   p1, p2, p3 : PAffineVector;
begin
   Result:=0;
   if nSides>2 then begin
      p1:=@(p^[0]);
      p2:=@(p^[1]);
      for i:=2 to nSides-1 do begin
         p3:=@(p^[i]);
         Result:=Result+(p2^[0]-p1^[0])*(p3^[1]-p1^[1])
                       -(p3^[0]-p1^[0])*(p2^[1]-p1^[1]);
         p2:=p3;
      end;
      Result:=Result*0.5;
   end;
end;

// ScaleFloatArray (raw)
//
procedure ScaleFloatArray(values : PSingleArray; nb : Integer;
                          var factor : Single);
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU

      push  edx
      shr   edx, 2
      or    edx, edx
      jz    @@FPU

      db $0F,$6E,$39           /// movd        mm7, [ecx]
      db $0F,$62,$FF           /// punpckldq   mm7, mm7

@@3DNowLoop:
      db $0F,$0D,$48,$40       /// prefetchw [eax+64]
      db $0F,$6F,$00           /// movq  mm0, [eax]
      db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
      db $0F,$0F,$C7,$B4       /// pfmul mm0, mm7
      db $0F,$0F,$CF,$B4       /// pfmul mm1, mm7
      db $0F,$7F,$00           /// movq  [eax], mm0
      db $0F,$7F,$48,$08       /// movq  [eax+8], mm1

      add   eax, 16
      dec   edx
      jnz   @@3DNowLoop

      pop   edx
      and   edx, 3
      db $0F,$0E               /// femms

@@FPU:
      push  edx
      shr   edx, 1
      or    edx, edx
      jz    @@FPULone

@@FPULoop:
      fld   dword ptr [eax]
      fmul  dword ptr [ecx]
      fstp  dword ptr [eax]
      fld   dword ptr [eax+4]
      fmul  dword ptr [ecx]
      fstp  dword ptr [eax+4]

      add   eax, 8
      dec   edx
      jnz   @@FPULoop

@@FPULone:
      pop   edx
      test  edx, 1
      jz    @@End

      fld   dword ptr [eax]
      fmul  dword ptr [ecx]
      fstp  dword ptr [eax]

@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to nb-1 do
      values^[i]:=values^[i]*factor;
{$endif}
end;

// ScaleFloatArray (array)
//
procedure ScaleFloatArray(var values : TSingleArray;
                          factor : Single);
begin
   if Length(values)>0 then
      ScaleFloatArray(@values[0], Length(values), factor);
end;

// OffsetFloatArray (raw)
//
procedure OffsetFloatArray(values : PSingleArray; nb : Integer;
                           var delta : Single);
{$ifndef GEOMETRY_NO_ASM}
asm
      test vSIMD, 1
      jz @@FPU

      push  edx
      shr   edx, 2
      or    edx, edx
      jz    @@FPU

      db $0F,$6E,$39           /// movd  mm7, [ecx]
      db $0F,$62,$FF           /// punpckldq   mm7, mm7

@@3DNowLoop:
      db $0F,$0D,$48,$40       /// prefetchw [eax+64]
      db $0F,$6F,$00           /// movq  mm0, [eax]
      db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
      db $0F,$0F,$C7,$9E       /// pfadd mm0, mm7
      db $0F,$0F,$CF,$9E       /// pfadd mm1, mm7
      db $0F,$7F,$00           /// movq  [eax], mm0
      db $0F,$7F,$48,$08       /// movq  [eax+8], mm1

      add   eax, 16
      dec   edx
      jnz   @@3DNowLoop

      pop   edx
      and   edx, 3
      db $0F,$0E               /// femms

@@FPU:
      push  edx
      shr   edx, 1
      or    edx, edx
      jz    @@FPULone

@@FPULoop:
      fld   dword ptr [eax]
      fadd  dword ptr [ecx]
      fstp  dword ptr [eax]
      fld   dword ptr [eax+4]
      fadd  dword ptr [ecx]
      fstp  dword ptr [eax+4]

      add   eax, 8
      dec   edx
      jnz   @@FPULoop

@@FPULone:
      pop   edx
      test  edx, 1
      jz    @@End

      fld   dword ptr [eax]
      fadd  dword ptr [ecx]
      fstp  dword ptr [eax]

@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to nb-1 do
      values^[i]:=values^[i]+delta;
{$endif}
end;

// ScaleFloatArray (array)
//
procedure OffsetFloatArray(var values : array of Single;
                           delta : Single);
begin
   if Length(values)>0 then
      ScaleFloatArray(@values[0], Length(values), delta);
end;

// OffsetFloatArray (raw, raw)
//
procedure OffsetFloatArray(valuesDest, valuesDelta : PSingleArray; nb : Integer);
{$ifndef GEOMETRY_NO_ASM}
asm
      test  ecx, ecx
      jz    @@End

@@FPULoop:
      dec   ecx
      fld   dword ptr [eax+ecx*4]
      fadd  dword ptr [edx+ecx*4]
      fstp  dword ptr [eax+ecx*4]
      jnz   @@FPULoop

@@End:
{$else}
var
   i : Integer;
begin
   for i:=0 to nb-1 do
      valuesDest^[i]:=valuesDest^[i]+valuesDelta^[i];
{$endif}
end;

// MaxXYZComponent
//
function MaxXYZComponent(const v : TVector) : Single; overload;
begin
   Result:=MaxFloat(v[0], v[1], v[2]);
end;

// MaxXYZComponent
//
function MaxXYZComponent(const v : TAffineVector): Single; overload;
begin
   Result:=MaxFloat(v[0], v[1], v[2]);
end;

// MinXYZComponent
//
function MinXYZComponent(const v : TVector) : Single; overload;
begin
   if v[0]<=v[1] then
      if v[0]<=v[2] then
         Result:=v[0]
      else if v[2]<=v[1] then
         Result:=v[2]
      else Result:=v[1]
   else if v[1]<=v[2] then
      Result:=v[1]
   else if v[2]<=v[0] then
      Result:=v[2]
   else Result:=v[0];
end;

// MinXYZComponent
//
function MinXYZComponent(const v : TAffineVector) : Single; overload;
begin
   Result:=MinFloat(v[0], v[1], v[2]);
end;

// MaxAbsXYZComponent
//
function MaxAbsXYZComponent(v : TVector) : Single;
begin
   AbsVector(v);
   Result:=MaxXYZComponent(v);
end;

// MinAbsXYZComponent
//
function MinAbsXYZComponent(v : TVector) : Single;
begin
   AbsVector(v);
   Result:=MinXYZComponent(v);
end;

// MaxVector (hmg)
//
procedure MaxVector(var v : TVector; const v1 : TVector);
begin
   if v1[0]>v[0] then v[0]:=v1[0];
   if v1[1]>v[1] then v[1]:=v1[1];
   if v1[2]>v[2] then v[2]:=v1[2];
   if v1[3]>v[3] then v[3]:=v1[3];
end;

// MaxVector (affine)
//
procedure MaxVector(var v : TAffineVector; const v1 : TAffineVector); overload;
begin
   if v1[0]>v[0] then v[0]:=v1[0];
   if v1[1]>v[1] then v[1]:=v1[1];
   if v1[2]>v[2] then v[2]:=v1[2];
end;

// MinVector (hmg)
//
procedure MinVector(var v : TVector; const v1 : TVector);
begin
   if v1[0]<v[0] then v[0]:=v1[0];
   if v1[1]<v[1] then v[1]:=v1[1];
   if v1[2]<v[2] then v[2]:=v1[2];
   if v1[3]<v[3] then v[3]:=v1[3];
end;

// MinVector (affine)
//
procedure MinVector(var v : TAffineVector; const v1 : TAffineVector);
begin
   if v1[0]<v[0] then v[0]:=v1[0];
   if v1[1]<v[1] then v[1]:=v1[1];
   if v1[2]<v[2] then v[2]:=v1[2];
end;

// SortArrayAscending (extended)
//
procedure SortArrayAscending(var a : array of Extended);
var
   i, j, m : Integer;
   buf : Extended;
begin
   for i:=Low(a) to High(a)-1 do begin
      m:=i;
      for j:=i+1 to High(a) do
         if a[j]<a[m] then m:=j;
      if m<>i then begin
         buf:=a[m];
         a[m]:=a[i];
         a[i]:=buf;
      end;
   end;
end;

// ClampValue (min-max)
//
function ClampValue(const aValue, aMin, aMax : Single) : Single;
//begin
{$ifndef GEOMETRY_NO_ASM}
asm   // 118
      fld   aValue
      fcom  aMin
      fstsw ax
      sahf
      jb    @@ReturnMin
@@CompMax:
      fcom  aMax
      fstsw ax
      sahf
      jnbe  @@ReturnMax
      pop   ebp
      ret   $0C

@@ReturnMax:
      fld   aMax
      jmp @@End
@@ReturnMin:
      fld   aMin
@@End:
      ffree st(1)
end;
{$else}
begin // 134
   if aValue<aMin then
      Result:=aMin
   else if aValue>aMax then
      Result:=aMax
   else Result:=aValue;
end;              
{$endif}

// ClampValue (min-)
//
function ClampValue(const aValue, aMin : Single) : Single;
begin
   if aValue<aMin then
      Result:=aMin
   else Result:=aValue;
end;

// MakeAffineDblVector
//
function MakeAffineDblVector(var V: array of Double): TAffineDblVector;
begin
   Result[0]:=V[0];
   Result[1]:=V[1];
   Result[2]:=V[2];
end;

// MakeDblVector
//
function MakeDblVector(var v : array of Double) : THomogeneousDblVector;
// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V
{$ifndef GEOMETRY_NO_ASM}
asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, 8
              REP MOVSD
              POP ESI
              POP EDI
{$else}
begin
   Result[0]:=V[0];
   Result[1]:=V[1];
   Result[2]:=V[2];
   Result[3]:=V[3];
{$endif}
end;

// PointInPolygon
//
function PointInPolygon(var xp, yp : array of Single; x, y: Single) : Boolean;
// The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
// with some minor modifications for speed.  It returns 1 for strictly
// interior points, 0 for strictly exterior, and 0 or 1 for points on
// the boundary.
var
   I, J: Integer;
begin
   Result:=False;
   if High(XP)=High(YP) then begin
      J:=High(XP);
      for I:=0 to High(XP) do begin
         if ((((YP[I]<=Y) and (Y<YP[J])) or ((YP[J]<=Y) and (Y<YP[I])) )
             and (X<(XP[J]-XP[I])*(Y-YP[I])/(YP[J]-YP[I])+XP[I])) then
            Result:=not Result;
         J:=I;
      end;
   end;
end;

// DivMod
//
procedure DivMod(dividend : Integer; divisor: Word; var result, remainder : Word);
{$ifndef GEOMETRY_NO_ASM}
asm
   push  ebx
   mov   ebx, edx
   mov   edx, eax
   shr   edx, 16
   div   bx
   mov   ebx, remainder
   mov   [ecx], ax
   mov   [ebx], dx
   pop   ebx
{$else}
begin
   Result:=Dividend div Divisor;
   Remainder:=Dividend mod Divisor;
{$endif}
end;

// ConvertRotation
//
function ConvertRotation(const Angles: TAffineVector): TVector;

{   Rotation of the Angle t about the axis (X, Y, Z) is given by:

     | X^2 + (1-X^2) Cos(t),    XY(1-Cos(t))  +  Z Sin(t), XZ(1-Cos(t))-Y Sin(t) |
 M = | XY(1-Cos(t))-Z Sin(t), Y^2 + (1-Y^2) Cos(t),      YZ(1-Cos(t)) + X Sin(t) |
     | XZ(1-Cos(t)) + Y Sin(t), YZ(1-Cos(t))-X Sin(t),   Z^2 + (1-Z^2) Cos(t)    |

   Rotation about the three axes (Angles a1, a2, a3) can be represented as
   the product of the individual rotation matrices:

      | 1  0       0       | | Cos(a2) 0 -Sin(a2) | |  Cos(a3) Sin(a3) 0 |
      | 0  Cos(a1) Sin(a1) | * | 0       1  0       | * | -Sin(a3) Cos(a3) 0 |
      | 0 -Sin(a1) Cos(a1) | | Sin(a2) 0  Cos(a2) | |  0       0       1 |
	     Mx                       My                     Mz

   We now want to solve for X, Y, Z, and t given 9 equations in 4 unknowns.
   Using the diagonal elements of the two matrices, we get:

      X^2 + (1-X^2) Cos(t) = M[0][0]
      Y^2 + (1-Y^2) Cos(t) = M[1][1]
      Z^2 + (1-Z^2) Cos(t) = M[2][2]

   Adding the three equations, we get:

      X^2  +  Y^2  +  Z^2 - (M[0][0]  +  M[1][1]  +  M[2][2]) =
	 - (3 - X^2 - Y^2 - Z^2) Cos(t)

   Since (X^2  +  Y^2  +  Z^2) = 1, we can rewrite as:

      Cos(t) = (1 - (M[0][0]  +  M[1][1]  +  M[2][2])) / 2

   Solving for t, we get:

      t = Acos(((M[0][0]  +  M[1][1]  +  M[2][2]) - 1) / 2)

    We can substitute t into the equations for X^2, Y^2, and Z^2 above
    to get the values for X, Y, and Z.  To find the proper signs we note
    that:

	2 X Sin(t) = M[1][2] - M[2][1]
	2 Y Sin(t) = M[2][0] - M[0][2]
	2 Z Sin(t) = M[0][1] - M[1][0]
}
var
   Axis1, Axis2: TVector3f;
   M, M1, M2: TMatrix;
   cost, cost1, sint, s1, s2, s3: Single;
   I: Integer;
begin
   // see if we are only rotating about a single Axis
   if Abs(Angles[X]) < EPSILON then begin
      if Abs(Angles[Y]) < EPSILON then begin
         SetVector(Result, 0, 0, 1, Angles[Z]);
         Exit;
      end else if Abs(Angles[Z]) < EPSILON then begin
         SetVector(Result, 0, 1, 0, Angles[Y]);
         Exit;
      end
   end else if (Abs(Angles[Y]) < EPSILON) and (Abs(Angles[Z]) < EPSILON) then begin
      SetVector(Result, 1, 0, 0, Angles[X]);
      Exit;
   end;

   // make the rotation matrix
   Axis1:=XVector;
   M:=CreateRotationMatrix(Axis1, Angles[X]);

   Axis2:=YVector;
   M2:=CreateRotationMatrix(Axis2, Angles[Y]);
   M1:=MatrixMultiply(M, M2);

   Axis2:=ZVector;
   M2:=CreateRotationMatrix(Axis2, Angles[Z]);
   M:=MatrixMultiply(M1, M2);

   cost:=((M[X, X] + M[Y, Y] + M[Z, Z])-1) / 2;
   if cost < -1 then
      cost:=-1
   else if cost > 1 - EPSILON then begin
      // Bad Angle - this would cause a crash
      SetVector(Result, XHmgVector);
      Exit;
   end;

   cost1:=1 - cost;
   SetVector(Result, sqrt((M[X, X]-cost) / cost1),
                     sqrt((M[Y, Y]-cost) / cost1),
                     sqrt((M[Z, Z]-cost) / cost1),
                     arccos(cost));

   sint:=2 * Sqrt(1 - cost * cost); // This is actually 2 Sin(t)

   // Determine the proper signs
   for I:=0 to 7 do begin
     if (I and 1) > 1 then s1:=-1 else s1:=1;
     if (I and 2) > 1 then s2:=-1 else s2:=1;
     if (I and 4) > 1 then s3:=-1 else s3:=1;
     if (Abs(s1 * Result[X] * sint-M[Y, Z] + M[Z, Y]) < EPSILON2)
        and (Abs(s2 * Result[Y] * sint-M[Z, X] + M[X, Z]) < EPSILON2)
        and (Abs(s3 * Result[Z] * sint-M[X, Y] + M[Y, X]) < EPSILON2) then begin
           // We found the right combination of signs
           Result[X]:=Result[X] * s1;
           Result[Y]:=Result[Y] * s2;
           Result[Z]:=Result[Z] * s3;
           Exit;
         end;
   end;
end;

// QuaternionSlerp
//
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;
var
    beta,                   // complementary interp parameter
    theta,                  // Angle between A and B
    sint, cost,             // sine, cosine of theta
    phi: Single;            // theta plus spins
    bflip: Boolean;         // use negativ t?
begin
  // cosine theta
  cost:=VectorAngleCosine(QStart.ImagPart, QEnd.ImagPart);

   // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
   if cost < 0 then begin
      cost:=-cost;
      bflip:=True;
   end else bflip:=False;

   // if QEnd is (within precision limits) the same as QStart,
   // just linear interpolate between QStart and QEnd.
   // Can't do spins, since we don't know what direction to spin.

   if (1 - cost) < EPSILON then
      beta:=1 - t
   else begin
      // normal case
      theta:=arccos(cost);
      phi:=theta + Spin * Pi;
      sint:=sin(theta);
      beta:=sin(theta - t * phi) / sint;
      t:=sin(t * phi) / sint;
   end;

   if bflip then t:=-t;

   // interpolate
   Result.ImagPart[X]:=beta * QStart.ImagPart[X] + t * QEnd.ImagPart[X];
   Result.ImagPart[Y]:=beta * QStart.ImagPart[Y] + t * QEnd.ImagPart[Y];
   Result.ImagPart[Z]:=beta * QStart.ImagPart[Z] + t * QEnd.ImagPart[Z];
   Result.RealPart:=beta * QStart.RealPart + t * QEnd.RealPart;
end;

// QuaternionSlerp
//
function QuaternionSlerp(const source, dest: TQuaternion; const t: Single): TQuaternion;
var
   to1: array[0..4] of Single;
   omega, cosom, sinom, scale0, scale1: Extended;
// t goes from 0 to 1
// absolute rotations
begin
   // calc cosine
   cosom:= source.ImagPart[0]*dest.ImagPart[0]
          +source.ImagPart[1]*dest.ImagPart[1]
          +source.ImagPart[2]*dest.ImagPart[2]
	       +source.RealPart   *dest.RealPart;
   // adjust signs (if necessary)
   if cosom<0 then begin
      cosom := -cosom;
      to1[0] := - dest.ImagPart[0];
      to1[1] := - dest.ImagPart[1];
      to1[2] := - dest.ImagPart[2];
      to1[3] := - dest.RealPart;
   end else begin
      to1[0] := dest.ImagPart[0];
      to1[1] := dest.ImagPart[1];
      to1[2] := dest.ImagPart[2];
      to1[3] := dest.RealPart;
   end;
   // calculate coefficients
   if ((1.0-cosom)>EPSILON2) then begin // standard case (slerp)
      omega:=VectorGeometry.ArcCos(cosom);
      sinom:=1/Sin(omega);
      scale0:=Sin((1.0-t)*omega)*sinom;
      scale1:=Sin(t*omega)*sinom;
   end else begin // "from" and "to" quaternions are very close
	          //  ... so we can do a linear interpolation
      scale0:=1.0-t;
      scale1:=t;
   end;
   // calculate final values
   Result.ImagPart[0] := scale0 * source.ImagPart[0] + scale1 * to1[0];
   Result.ImagPart[1] := scale0 * source.ImagPart[1] + scale1 * to1[1];
   Result.ImagPart[2] := scale0 * source.ImagPart[2] + scale1 * to1[2];
   Result.RealPart := scale0 * source.RealPart + scale1 * to1[3];
   NormalizeQuaternion(Result);
end;

// VectorDblToFlt
//
function VectorDblToFlt(const V: THomogeneousDblVector): THomogeneousVector;
// converts a vector containing double sized values into a vector with single sized values
{$ifndef GEOMETRY_NO_ASM}
asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX + 8]
              FSTP DWORD PTR [EDX + 4]
              FLD  QWORD PTR [EAX + 16]
              FSTP DWORD PTR [EDX + 8]
              FLD  QWORD PTR [EAX + 24]
              FSTP DWORD PTR [EDX + 12]
{$else}
begin
   {$HINTS OFF}
   Result[0]:=V[0];
   Result[1]:=V[1];
   Result[2]:=V[2];
   Result[3]:=V[3];
   {$HINTS ON}
{$endif}
end;

// VectorAffineDblToFlt
//
function VectorAffineDblToFlt(const V: TAffineDblVector): TAffineVector;
// converts a vector containing double sized values into a vector with single sized values
{$ifndef GEOMETRY_NO_ASM}
asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX + 8]
              FSTP DWORD PTR [EDX + 4]
              FLD  QWORD PTR [EAX + 16]
              FSTP DWORD PTR [EDX + 8]
{$else}
begin
   {$HINTS OFF}
   Result[0]:=V[0];
   Result[1]:=V[1];
   Result[2]:=V[2];
   {$HINTS ON}
{$endif}
end;

// VectorAffineFltToDbl
//
function VectorAffineFltToDbl(const V: TAffineVector): TAffineDblVector;
// converts a vector containing single sized values into a vector with double sized values
{$ifndef GEOMETRY_NO_ASM}
asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX + 8]
              FSTP QWORD PTR [EDX + 4]
              FLD  DWORD PTR [EAX + 16]
              FSTP QWORD PTR [EDX + 8]
{$else}
begin
   Result[0]:=V[0];
   Result[1]:=V[1];
   Result[2]:=V[2];
{$endif}
end;

// VectorFltToDbl
//
function VectorFltToDbl(const V: TVector): THomogeneousDblVector;
// converts a vector containing single sized values into a vector with double sized values
{$ifndef GEOMETRY_NO_ASM}
asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX + 8]
              FSTP QWORD PTR [EDX + 4]
              FLD  DWORD PTR [EAX + 16]
              FSTP QWORD PTR [EDX + 8]
              FLD  DWORD PTR [EAX + 24]
              FSTP QWORD PTR [EDX + 12]
{$else}
begin
   Result[0]:=V[0];
   Result[1]:=V[1];
   Result[2]:=V[2];
   Result[3]:=V[3];
{$endif}
end;

//----------------- coordinate system manipulation functions -----------------------------------------------------------

// Turn (Y axis)
//
function Turn(const Matrix: TMatrix; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[1][0], Matrix[1][1], Matrix[1][2]), Angle));
end;

// Turn (direction)
//
function Turn(const Matrix: TMatrix; const MasterUp: TAffineVector; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(MasterUp, Angle));
end;

// Pitch (X axis)
//
function Pitch(const Matrix: TMatrix; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[0][0], Matrix[0][1], Matrix[0][2]), Angle));
end;

// Pitch (direction)
//
function Pitch(const Matrix: TMatrix; const MasterRight: TAffineVector; Angle: Single): TMatrix; overload;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(MasterRight, Angle));
end;

// Roll (Z axis)
//
function Roll(const Matrix: TMatrix; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[2][0], Matrix[2][1], Matrix[2][2]), Angle));
end;

// Roll (direction)
//
function Roll(const Matrix: TMatrix; const MasterDirection: TAffineVector; Angle: Single): TMatrix; overload;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(MasterDirection, Angle));
end;

// RayCastPlaneIntersect (plane defined by point+normal)
//
function RayCastPlaneIntersect(const rayStart, rayVector : TVector;
                               const planePoint, planeNormal : TVector;
                               intersectPoint : PVector = nil) : Boolean;
var
   sp : TVector;
   t, d : Single;
begin
   d:=VectorDotProduct(rayVector, planeNormal);
   Result:=((d>EPSILON2) or (d<-EPSILON2));
   if Result and Assigned(intersectPoint) then begin
      VectorSubtract(planePoint, rayStart, sp);
      d:=1/d; // will keep one FPU unit busy during dot product calculation
      t:=VectorDotProduct(sp, planeNormal)*d;
      if t>0 then
         VectorCombine(rayStart, rayVector, t, intersectPoint^)
      else Result:=False;
   end;
end;

// RayCastPlaneXZIntersect
//
function RayCastPlaneXZIntersect(const rayStart, rayVector : TVector;
                                 const planeY : Single;
                                 intersectPoint : PVector = nil) : Boolean;
var
   t : Single;
begin
   if rayVector[1]=0 then
      Result:=False
   else begin
      t:=(rayStart[1]-planeY)/rayVector[1];
      if t<0 then begin
         if Assigned(intersectPoint) then
            VectorCombine(rayStart, rayVector, t, intersectPoint^);
         Result:=True;
      end else Result:=False;
   end;
end;

// RayCastTriangleIntersect
//
function RayCastTriangleIntersect(const rayStart, rayVector : TVector;
                                  const p1, p2, p3 : TAffineVector;
                                  intersectPoint : PVector = nil;
                                  intersectNormal : PVector = nil) : Boolean;
var
   pvec : TAffineVector;
   v1, v2, qvec, tvec : TVector;
   t, u, v, det, invDet : Single;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(rayVector, v2, pvec);
   det:=VectorDotProduct(v1, pvec);
   if ((det<EPSILON2) and (det>-EPSILON2)) then begin // vector is parallel to triangle's plane
      Result:=False;
      Exit;
   end;
   invDet:=cOne/det;
   VectorSubtract(rayStart, p1, tvec);
   u:=VectorDotProduct(tvec, pvec)*invDet;
   if (u<0) or (u>1) then
      Result:=False
   else begin
      qvec:=VectorCrossProduct(tvec, v1);
      v:=VectorDotProduct(rayVector, qvec)*invDet;
      Result:=(v>=0) and (u+v<=1);
      if Result then begin
         t:=VectorDotProduct(v2, qvec)*invDet;
         if t>0 then begin
            if intersectPoint<>nil then
               VectorCombine(rayStart, rayVector, t, intersectPoint^);
            if intersectNormal<>nil then
               VectorCrossProduct(v1, v2, intersectNormal^);
         end else Result:=False;
      end;
   end;
end;

// RayCastMinDistToPoint
//
function RayCastMinDistToPoint(const rayStart, rayVector : TVector;
                               const point : TVector) : Single;
var
   proj : Single;
begin
   proj:=PointProject(point, rayStart, rayVector);
   if proj<=0 then proj:=0; // rays don't go backward!
   Result:=VectorDistance(point, VectorCombine(rayStart, rayVector, 1, proj));
end;

// RayCastIntersectsSphere
//
function RayCastIntersectsSphere(const rayStart, rayVector : TVector;
                                 const sphereCenter : TVector;
                                 const sphereRadius : Single) : Boolean;
var
   proj : Single;
begin
   proj:=PointProject(sphereCenter, rayStart, rayVector);
   if proj<=0 then proj:=0; // rays don't go backward!
   Result:=(VectorDistance2(sphereCenter, VectorCombine(rayStart, rayVector, 1, proj))<=Sqr(sphereRadius));
end;

// RayCastSphereIntersect
//
function RayCastSphereIntersect(const rayStart, rayVector : TVector;
                                const sphereCenter : TVector;
                                const sphereRadius : Single;
                                var i1, i2 : TVector) : Integer;
var
   proj, d2 : Single;
   id2 : Integer;
   projPoint : TVector;
begin
   proj:=PointProject(sphereCenter, rayStart, rayVector);
   VectorCombine(rayStart, rayVector, proj, projPoint);
   d2:=sphereRadius*sphereRadius-VectorDistance2(sphereCenter, projPoint);
   id2:=PInteger(@d2)^;
   if id2>=0 then begin
      if id2=0 then begin
         if PInteger(@proj)^>0 then begin
            VectorCombine(rayStart, rayVector, proj, i1);
            Result:=1;
            Exit;
         end;
      end else if id2>0 then begin
         d2:=Sqrt(d2);
         if proj>=d2 then begin
            VectorCombine(rayStart, rayVector, proj-d2, i1);
            VectorCombine(rayStart, rayVector, proj+d2, i2);
            Result:=2;
            Exit;
         end else if proj+d2>=0 then begin
            VectorCombine(rayStart, rayVector, proj+d2, i1);
            Result:=1;
            Exit;
         end;
      end;
   end;
   Result:=0;
end;

// SphereVisibleRadius
//
function SphereVisibleRadius(distance, radius : Single) : Single;
var
   d2, r2, ir, tr : Single;
begin
   {  ir² + r² = d²
      r² + tr² = vr²
      vr² + d² = (ir+tr)² = ir² + 2.ir.tr + tr²

      ir² + 2.ir.tr + tr² = d² + r² + tr²
      2.ir.tr = d² + r² - ir²  }
   d2:=distance*distance;
   r2:=radius*radius;
   ir:=Sqrt(d2-r2);
   tr:=(d2+r2-Sqr(ir))/(2*ir);

   Result:=Sqrt(r2+Sqr(tr));
end;


// IntersectLinePlane
//
function IntersectLinePlane(const point, direction : TVector;
                            const plane : THmgPlane;
                            intersectPoint : PVector = nil) : Integer;
var
   a, b : Extended;
   t : Single;
begin
   a:=VectorDotProduct(plane, direction);    // direction projected to plane normal
   b:=PlaneEvaluatePoint(plane, point);      // distance to plane
   if a=0 then begin          // direction is parallel to plane
      if b=0 then
         Result:=-1           // line is inside plane
      else Result:=0;         // line is outside plane
   end else begin
      if Assigned(intersectPoint) then begin
         t:=-b/a;                               // parameter of intersection
         intersectPoint^:=point;
         // calculate intersection = p + t*d
         CombineVector(intersectPoint^, direction, t);
      end;
      Result:=1;
   end;
end;

// ExtractFrustumFromModelViewProjection
//
function ExtractFrustumFromModelViewProjection(const modelViewProj : TMatrix) : TFrustum;
begin
   with Result do begin
      // extract left plane
      pLeft[0]:=modelViewProj[0][3]+modelViewProj[0][0];
      pLeft[1]:=modelViewProj[1][3]+modelViewProj[1][0];
      pLeft[2]:=modelViewProj[2][3]+modelViewProj[2][0];
      pLeft[3]:=modelViewProj[3][3]+modelViewProj[3][0];
      NormalizePlane(pLeft);
      // extract top plane
      pTop[0]:=modelViewProj[0][3]-modelViewProj[0][1];
      pTop[1]:=modelViewProj[1][3]-modelViewProj[1][1];
      pTop[2]:=modelViewProj[2][3]-modelViewProj[2][1];
      pTop[3]:=modelViewProj[3][3]-modelViewProj[3][1];
      NormalizePlane(pTop);
      // extract right plane
      pRight[0]:=modelViewProj[0][3]-modelViewProj[0][0];
      pRight[1]:=modelViewProj[1][3]-modelViewProj[1][0];
      pRight[2]:=modelViewProj[2][3]-modelViewProj[2][0];
      pRight[3]:=modelViewProj[3][3]-modelViewProj[3][0];
      NormalizePlane(pRight);
      // extract bottom plane
      pBottom[0]:=modelViewProj[0][3]+modelViewProj[0][1];
      pBottom[1]:=modelViewProj[1][3]+modelViewProj[1][1];
      pBottom[2]:=modelViewProj[2][3]+modelViewProj[2][1];
      pBottom[3]:=modelViewProj[3][3]+modelViewProj[3][1];
      NormalizePlane(pBottom);
      // extract far plane
      pFar[0]:=modelViewProj[0][3]-modelViewProj[0][2];
      pFar[1]:=modelViewProj[1][3]-modelViewProj[1][2];
      pFar[2]:=modelViewProj[2][3]-modelViewProj[2][2];
      pFar[3]:=modelViewProj[3][3]-modelViewProj[3][2];
      NormalizePlane(pFar);
      // extract near plane
      pNear[0]:=modelViewProj[0][3]+modelViewProj[0][2];
      pNear[1]:=modelViewProj[1][3]+modelViewProj[1][2];
      pNear[2]:=modelViewProj[2][3]+modelViewProj[2][2];
      pNear[3]:=modelViewProj[3][3]+modelViewProj[3][2];
      NormalizePlane(pNear);
   end;
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos : TVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean;
begin
   Result:=IsVolumeClipped(PAffineVector(@objPos)^, objRadius, rcci);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean;
var
   negRadius : Single;
begin
   negRadius:=-objRadius;
   Result:=   (PlaneEvaluatePoint(rcci.frustum.pLeft, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pTop, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pRight, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pBottom, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pNear, objPos)<negRadius)
           or (PlaneEvaluatePoint(rcci.frustum.pFar, objPos)<negRadius);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const Frustum : TFrustum) : Boolean;
var
   negRadius : Single;
begin
   negRadius:=-objRadius;
   Result:=   (PlaneEvaluatePoint(frustum.pLeft, objPos)<negRadius)
           or (PlaneEvaluatePoint(frustum.pTop, objPos)<negRadius)
           or (PlaneEvaluatePoint(frustum.pRight, objPos)<negRadius)
           or (PlaneEvaluatePoint(frustum.pBottom, objPos)<negRadius)
           or (PlaneEvaluatePoint(frustum.pNear, objPos)<negRadius)
           or (PlaneEvaluatePoint(frustum.pFar, objPos)<negRadius);
end;

// IsVolumeClipped
//
function IsVolumeClipped(const min, max : TAffineVector;
                         const rcci : TRenderContextClippingInfo) : Boolean;
begin
   // change box to sphere
   Result:=IsVolumeClipped(VectorScale(VectorAdd(min, max), 0.5),
                           VectorDistance(min, max)*0.5, rcci);
end;

// MakeParallelProjectionMatrix
//
function MakeParallelProjectionMatrix(const plane : THmgPlane;
                                      const dir : TVector) : TMatrix;
// Based on material from a course by William D. Shoaff (www.cs.fit.edu)
var
   dot, invDot : Single;
begin
   dot:=plane[0]*dir[0]+plane[1]*dir[1]+plane[2]*dir[2];
   if Abs(dot)<1e-5 then begin
      Result:=IdentityHmgMatrix;
      Exit;
   end;
   invDot:=1/dot;

   Result[0][0]:=(plane[1]*dir[1]+plane[2]*dir[2])*invDot;
   Result[1][0]:=(-plane[1]*dir[0])*invDot;
   Result[2][0]:=(-plane[2]*dir[0])*invDot;
   Result[3][0]:=(-plane[3]*dir[0])*invDot;

   Result[0][1]:=(-plane[0]*dir[1])*invDot;
   Result[1][1]:=(plane[0]*dir[0]+plane[2]*dir[2])*invDot;
   Result[2][1]:=(-plane[2]*dir[1])*invDot;
   Result[3][1]:=(-plane[3]*dir[1])*invDot;

   Result[0][2]:=(-plane[0]*dir[2])*invDot;
   Result[1][2]:=(-plane[1]*dir[2])*invDot;
   Result[2][2]:=(plane[0]*dir[0]+plane[1]*dir[1])*invDot;
   Result[3][2]:=(-plane[3]*dir[2])*invDot;

   Result[0][3]:=0;
   Result[1][3]:=0;
   Result[2][3]:=0;
   Result[3][3]:=1;
end;


// MakeShadowMatrix
//
function MakeShadowMatrix(const planePoint, planeNormal, lightPos : TVector) : TMatrix;
var
   planeNormal3, dot : Single;
begin
	// Find the last coefficient by back substitutions
	planeNormal3:=-( planeNormal[0]*planePoint[0]
                   +planeNormal[1]*planePoint[1]
                   +planeNormal[2]*planePoint[2]);
	// Dot product of plane and light position
	dot:= planeNormal[0]*lightPos[0]
        +planeNormal[1]*lightPos[1]
        +planeNormal[2]*lightPos[2]
        +planeNormal3  *lightPos[3];
	// Now do the projection
	// First column
   Result[0][0]:= dot - lightPos[0] * planeNormal[0];
   Result[1][0]:=     - lightPos[0] * planeNormal[1];
   Result[2][0]:=     - lightPos[0] * planeNormal[2];
   Result[3][0]:=     - lightPos[0] * planeNormal3;
	// Second column
	Result[0][1]:=     - lightPos[1] * planeNormal[0];
	Result[1][1]:= dot - lightPos[1] * planeNormal[1];
	Result[2][1]:=     - lightPos[1] * planeNormal[2];
	Result[3][1]:=     - lightPos[1] * planeNormal3;
	// Third Column
	Result[0][2]:=     - lightPos[2] * planeNormal[0];
	Result[1][2]:=     - lightPos[2] * planeNormal[1];
	Result[2][2]:= dot - lightPos[2] * planeNormal[2];
	Result[3][2]:=     - lightPos[2] * planeNormal3;
	// Fourth Column
	Result[0][3]:=     - lightPos[3] * planeNormal[0];
	Result[1][3]:=     - lightPos[3] * planeNormal[1];
	Result[2][3]:=     - lightPos[3] * planeNormal[2];
	Result[3][3]:= dot - lightPos[3] * planeNormal3;
end;

// MakeReflectionMatrix
//
function MakeReflectionMatrix(const planePoint, planeNormal : TAffineVector) : TMatrix;
var
   pv2 : Single;
begin
   // Precalcs
   pv2:=2*VectorDotProduct(planePoint, planeNormal);
   // 1st column
   Result[0][0]:=1-2*Sqr(planeNormal[0]);
   Result[0][1]:=-2*planeNormal[0]*planeNormal[1];
   Result[0][2]:=-2*planeNormal[0]*planeNormal[2];
   Result[0][3]:=0;
   // 2nd column
   Result[1][0]:=-2*planeNormal[1]*planeNormal[0];
   Result[1][1]:=1-2*Sqr(planeNormal[1]);
   Result[1][2]:=-2*planeNormal[1]*planeNormal[2];
   Result[1][3]:=0;
   // 3rd column
   Result[2][0]:=-2*planeNormal[2]*planeNormal[0];
   Result[2][1]:=-2*planeNormal[2]*planeNormal[1];
   Result[2][2]:=1-2*Sqr(planeNormal[2]);
   Result[2][3]:=0;
   // 4th column
   Result[3][0]:=pv2*planeNormal[0];
   Result[3][1]:=pv2*planeNormal[1];
   Result[3][2]:=pv2*planeNormal[2];
   Result[3][3]:=1;
end;

// PackRotationMatrix
//
function PackRotationMatrix(const mat : TMatrix) : TPackedRotationMatrix;
var
   q : TQuaternion;
const
   cFact : Single = 32767;
begin
   q:=QuaternionFromMatrix(mat);
   NormalizeQuaternion(q);
   {$HINTS OFF}
   if q.RealPart<0 then begin
      Result[0]:=Round(-q.ImagPart[0]*cFact);
      Result[1]:=Round(-q.ImagPart[1]*cFact);
      Result[2]:=Round(-q.ImagPart[2]*cFact);
   end else begin
      Result[0]:=Round(q.ImagPart[0]*cFact);
      Result[1]:=Round(q.ImagPart[1]*cFact);
      Result[2]:=Round(q.ImagPart[2]*cFact);
   end;
   {$HINTS ON}
end;

// UnPackRotationMatrix
//
function UnPackRotationMatrix(const packedMatrix : TPackedRotationMatrix) : TMatrix;
var
   q : TQuaternion;
const
   cFact : Single = 1/32767;
begin
   q.ImagPart[0]:=packedMatrix[0]*cFact;
   q.ImagPart[1]:=packedMatrix[1]*cFact;
   q.ImagPart[2]:=packedMatrix[2]*cFact;
   q.RealPart:=1-VectorNorm(q.ImagPart);
   if q.RealPart<0 then
      q.RealPart:=0
   else q.RealPart:=Sqrt(q.RealPart);
   Result:=QuaternionToMatrix(q);
end;

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
initialization
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

{$ifndef GEOMETRY_NO_ASM}
   try
      // detect 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
      asm
         pusha
         mov  eax, $80000000
         db $0F,$A2               /// cpuid
         cmp  eax, $80000000
         jbe @@No3DNow
         mov  eax, $80000001
         db $0F,$A2               /// cpuid
         test edx, $80000000
         jz @@No3DNow
         mov vSIMD, 1
@@No3DNow:
         popa
      end;
   except
      // trap for old/exotics CPUs
      vSIMD:=0;
   end;
{$else}
   vSIMD:=0;
{$endif}

end.
