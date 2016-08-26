//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VectorTypes<p>

   Defines base vector types for use in Geometry.pas and OpenGL1x.pas.<p>

   The sole aim of this unit is to limit dependency between the Geometry
   and OpenGL1x units by introducing the base compatibility types
   (and only the *base* types).<p>

   Conventions:<ul>
      <li><b>d</b> is used for Double precision floating points values (64 bits)
      <li><b>f</b> is used for Single precision floating points values (32 bits)
      <li><b>i</b> is used for 32 bits signed integers (longint)
      <li><b>s</b> is uses for 16 bits signed integers (smallint)
   </ul>

   Note : D3D types untested.<p>

	<b>History : </b><font size=-1><ul>
    <li>19/12/04 - PhP - Added byte vectors
    <li>28/06/04 - LR - Removed ..\ from the GLScene.inc
    <li>24/08/03 - PhP - Added smallint vectors
    <li>04/07/01 - EG - Creation
  </ul>
}
unit VectorTypes;

interface

{$i GLScene.inc}

type
  TVector2d = array[0..1] of double;
  TVector2f = array[0..1] of single;
  TVector2i = array[0..1] of longint;
  TVector2s = array[0..1] of smallint;
  TVector2b = array[0..1] of byte;

  TVector3d = array[0..2] of double;
  TVector3f = array[0..2] of single;
  TVector3i = array[0..2] of longint;
  TVector3s = array[0..2] of smallint;
  TVector3b = array[0..2] of byte;

  TVector4d = array[0..3] of double;
  TVector4f = array[0..3] of single;
  TVector4i = array[0..3] of longint;
  TVector4s = array[0..3] of smallint;
  TVector4b = array[0..3] of byte;

  TMatrix3d = array[0..2] of TVector3d;
  TMatrix3f = array[0..2] of TVector3f;
  TMatrix3i = array[0..2] of TVector3i;
  TMatrix3s = array[0..2] of TVector3s;
  TMatrix3b = array[0..2] of TVector3b;

  TMatrix4d = array[0..3] of TVector4d;
  TMatrix4f = array[0..3] of TVector4f;
  TMatrix4i = array[0..3] of TVector4i;
  TMatrix4s = array[0..3] of TVector4s;
  TMatrix4b = array[0..3] of TVector4b;

  TD3DVector = packed record
    case Integer of
      0 : (X: single;
           Y: single;
           Z: single);
      1 : (V: TVector3f);
  end;

  TD3DMatrix = packed record
    case Integer of
      0 : (_11, _12, _13, _14: single;
           _21, _22, _23, _24: single;
           _31, _32, _33, _34: single;
           _41, _42, _43, _44: single);
      1 : (M : TMatrix4f);
  end;

implementation

end.

