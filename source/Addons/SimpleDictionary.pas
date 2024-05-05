{*******************************************************}
{                                                       }
{                   TSimpleDictionary                   }
{    https://github.com/martinusso/simple-dictionary    }
{                                                       }
{             This software is open source,             }
{       licensed under the The MIT License (MIT).       }
{                                                       }
{*******************************************************}

unit SimpleDictionary;

interface

uses
  Classes, Variants;

type
  TSimpleDictionaryBase = class(TObject)
  private
    procedure Error(const Msg: string; Data: Integer);
  protected
    procedure Clear(); virtual; abstract;
    procedure Delete(const Index: Integer); overload; virtual; abstract;
    procedure Delete(const Key: Variant); overload; virtual; abstract;
    function IndexOf(const Key: Variant): Integer; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    destructor Destroy(); override;
    function Contains(const Key: Variant): Boolean;
  end;


  TVariantRecord = record
    Key: Variant;
    Value: Variant;
  end;
  TVariantArray = array of TVariantRecord;

  TSimpleDictionary = class(TSimpleDictionaryBase)
  private
    FItems: TVariantArray;
    function GetCount: Integer;
    function GetValue(const Key: Variant): Variant;
    procedure SetValue(const Key: Variant; const Value: Variant);
    function GetKey(const Index: Integer): Variant;
  public
    constructor Create(); override;
    procedure Clear(); override;
    function Add(const Key: Variant; const Value: Variant): Integer;
    procedure Append(const Key: Variant; const Value: Variant);
    procedure Delete(const Index: Integer); overload; override;
    procedure Delete(const Key: Variant); overload; override;

    function IndexOf(const Key: Variant): Integer; override;
    property Items: TVariantArray read FItems;
    property Count: Integer read GetCount;
    property Keys[const Index: Integer]: Variant read GetKey;
    property Values[const Key: Variant]: Variant read GetValue write SetValue;
  end;

  TDictionary = class(TSimpleDictionary);
  TFloatDictionary = class(TSimpleDictionary);

  TObjectRecord = record
    Key: Variant;
    Value: TObject;
  end;
  TObjectArray = array of TObjectRecord;

  TSimpleObjectDictionary = class(TSimpleDictionaryBase)
  private
    FItems: TObjectArray;
    function GetCount: Integer;
    function GetValue(const Key: Variant): TObject;
    procedure SetValue(const Key: Variant; const Value: TObject);
    function GetKey(const Index: Integer): Variant;
  public
    constructor Create(); override;
    procedure Clear(); override;
    function Add(const Key: Variant; const Value: TObject): Integer;
    procedure Append(const Key: Variant; const Value: TObject);
    procedure Delete(const Index: Integer); overload; override;
    procedure Delete(const Key: Variant); overload; override;

    function IndexOf(const Key: Variant): Integer; override;
    property Items: TObjectArray read FItems;
    property Count: Integer read GetCount;
    property Keys[const Index: Integer]: Variant read GetKey;
    property Values[const Key: Variant]: TObject read GetValue write SetValue;
  end;

  TObjectDictionary = class(TSimpleObjectDictionary);

implementation

const
  LIST_INDEX_ERROR = 'List index out of bounds (%d)';

{ TSimpleDictionary }

function TSimpleDictionary.Add(const Key, Value: Variant): Integer;
var
  Size: Integer;
begin
  if Key = null then
  begin
    Result := -1;
    Exit;
  end;

  Result := Self.IndexOf(Key);
  if Result > -1 then
  begin
    FItems[Result].Value := Value;
  end
  else begin
    Size := Length(FItems) + 1;
    Result := Size - 1;

    SetLength(FItems, Size);
    FItems[Result].Key := Key;
    FItems[Result].Value := Value;
  end;
end;

procedure TSimpleDictionary.Append(const Key, Value: Variant);
begin
  Self.Add(Key, Value);
end;

procedure TSimpleDictionary.Clear;
begin
  FillChar(FItems, SizeOf(FItems), 0);
end;

constructor TSimpleDictionary.Create;
begin
  Self.Clear();
end;

procedure TSimpleDictionary.Delete(const Key: Variant);
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);
  Self.Delete(Index);
end;

procedure TSimpleDictionary.Delete(const Index: Integer);
var
  Size: Integer;
  TailElements: Cardinal;
begin
  if Index > High(FItems) then Exit;
  if Index < Low(FItems) then Exit;
  if Index = High(FItems) then
  begin
    SetLength(FItems, Length(FItems) - 1);
    Exit;
  end;
  Size := Length(FItems);
  TailElements := Size - Index;

  Finalize(FItems[Index]);
  Move(FItems[Index + 1], FItems[Index], SizeOf(TVariantArray) * TailElements);
  Initialize(FItems[Size - 1]);
  SetLength(FItems, Size - 1);
end;

function TSimpleDictionary.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TSimpleDictionary.GetKey(const Index: Integer): Variant;
begin
  if (Index < 0) or (Index >= Self.GetCount) then
    Error(LIST_INDEX_ERROR, Index);

  Result := FItems[Index].Key;
end;

function TSimpleDictionary.GetValue(const Key: Variant): Variant;
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);

  if (Index < 0) or (Index >= Self.GetCount) then
    Error(LIST_INDEX_ERROR, Index);

  Result := FItems[Index].Value;
end;

function TSimpleDictionary.IndexOf(const Key: Variant): Integer;
var
  I: Integer;
  Size: Integer;
begin
  Result := -1;

  Size := Length(FItems);
  if Size > 0 then
  begin
    for I := 0 to Size - 1 do
    begin
      if FItems[I].Key = Key then
        Result := I;
    end;
  end;
end;

procedure TSimpleDictionary.SetValue(const Key, Value: Variant);
begin
  Self.Append(Key, Value);
end;

{ TSimpleObjectDictionary }

function TSimpleObjectDictionary.Add(const Key: Variant; const Value: TObject): Integer;
var
  Size: Integer;
begin
  if Key = null then
  begin
    Result := -1;
    Exit;
  end;

  Result := Self.IndexOf(Key);
  if Result > -1 then
  begin
    FItems[Result].Value := Value;
  end
  else begin
    Size := Length(FItems) + 1;
    Result := Size - 1;

    SetLength(FItems, Size);
    FItems[Result].Key := Key;
    FItems[Result].Value := Value;
  end;
end;

procedure TSimpleObjectDictionary.Append(const Key: Variant; const Value: TObject);
begin
  Self.Add(Key, Value);
end;

procedure TSimpleObjectDictionary.Clear;
begin
  FillChar(FItems, SizeOf(FItems), 0);
end;

constructor TSimpleObjectDictionary.Create;
begin
  Self.Clear();
end;

procedure TSimpleObjectDictionary.Delete(const Key: Variant);
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);
  Self.Delete(Index);
end;

procedure TSimpleObjectDictionary.Delete(const Index: Integer);
var
  Size: Integer;
  TailElements: Cardinal;
begin
  if Index > High(FItems) then Exit;
  if Index < Low(FItems) then Exit;

  if Index = High(FItems) then
  begin
    SetLength(FItems, Length(FItems) - 1);
    Exit;
  end;
  Size := Length(FItems);
  TailElements := Size - Index;

  Finalize(FItems[Index]);
  Move(FItems[Index + 1], FItems[Index], SizeOf(TObjectArray) * TailElements);
  Initialize(FItems[Size - 1]);
  SetLength(FItems, Size - 1);
end;

function TSimpleObjectDictionary.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TSimpleObjectDictionary.GetKey(const Index: Integer): Variant;
begin
  if (Index < 0) or (Index >= Self.GetCount) then
    Error(LIST_INDEX_ERROR, Index);

  Result := FItems[Index].Key;
end;

function TSimpleObjectDictionary.GetValue(const Key: Variant): TObject;
var
  Index: Integer;
begin
  Index := Self.IndexOf(Key);

  if (Index < 0) or (Index >= Self.GetCount) then
    Error(LIST_INDEX_ERROR, Index);

  Result := FItems[Index].Value;
end;

function TSimpleObjectDictionary.IndexOf(const Key: Variant): Integer;
var
  I: Integer;
  Size: Integer;
begin
  Result := -1;

  Size := Length(FItems);
  if Size > 0 then
  begin
    for I := 0 to Size - 1 do
    begin
      if FItems[I].Key = Key then begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure TSimpleObjectDictionary.SetValue(const Key: Variant; const Value: TObject);
begin
  Self.Append(Key, Value);
end;

{ TSimpleDictionaryBase }

function TSimpleDictionaryBase.Contains(const Key: Variant): Boolean;
begin
  Result := IndexOf(Key) > -1;
end;

destructor TSimpleDictionaryBase.Destroy;
begin
  Self.Clear();
  inherited;
end;

procedure TSimpleDictionaryBase.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

end.
