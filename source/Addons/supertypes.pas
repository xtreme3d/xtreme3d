unit supertypes;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

type
PInt8 = ^Int8;
PInt16 = ^Int16;
PInt32 = ^Int32;
PInt64 = ^Int64;

PUInt8 = ^UInt8;
PUInt16 = ^UInt16;
PUInt32 = ^UInt32;
PUInt64 = ^UInt64;

PInt = PInt32;

SInt32 = Int32;

PtrInt = ^Int32;
PtrUInt = ^Uint32;

implementation

end.
