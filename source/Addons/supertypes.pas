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

{$IFNDEF FPC}
{$IFDEF CPUX64}
  PtrInt = Int64;
  PtrUInt = UInt64;
{$ELSE}
  PtrInt = longint;
  PtrUInt = Longword;
{$ENDIF}
{$ENDIF}
  SuperInt = Int64;

{$if (sizeof(Char) = 1)}
  SOChar = WideChar;
  SOIChar = Word;
  PSOChar = PWideChar;
{$IFDEF FPC}
  SOString = UnicodeString;
{$ELSE}
  SOString = WideString;
{$ENDIF}
{$else}
  SOChar = Char;
  SOIChar = Word;
  PSOChar = PChar;
  SOString = string;
{$ifend}
implementation

end.
