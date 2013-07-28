unit TypesToTest;

interface

uses Generics.Collections, SuperObject, RestJsonUtils;

type
  TEnumType = (etOne, etTwo, etThree);

  TEnumTypeSet = set of TEnumType;

  TRecordTypes = record
    one: string;
    two: Integer;
  end;

  TAllTypes = class;

  TAllTypesArray = array of TAllTypes;

  TAllTypes = class
  public
    valueInteger: Integer;
    valueDouble: Double;
    valueCurrency: Currency;
    valueString: string;
    valueAnsiChar: AnsiChar;
    valueChar: Char;
    valueInt64: Int64;
    valueSingle: Single;
    valueExtended: Extended;
    valueComp: Comp; //Only for test. Use the Int64 type for better performance.

    [SOName('renamed')]
    [JsonName('renamed')]
    fieldNameRenamed: String;

    [SODefault('"default"')]
    [JsonDefault('"default"')]
    fieldDefaultValue: String;

    valueBoolean: Boolean;
    valueTObject: TAllTypes;
    valueList: TList<TAllTypes>;
    valueObjectList: TObjectList<TAllTypes>;
    valueDateTime: TDateTime;
    valueEnum: TEnumType;
    valueSet: TEnumTypeSet;

{
    valueTRecord: TRecordTypes;
    valueTObjectArray: TAllTypesArray;
    valueArray: Array[1..3] of Integer;
    valueDynArray: TIntegerDynArray;
    valueClassRef: TClass;
    valueGuid: TGUID;
}

    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

{ TAllTypes }

procedure TAllTypes.AfterConstruction;
begin
  inherited;
end;

destructor TAllTypes.Destroy;
var
  vItem: TAllTypes;
begin
  if valueList <> nil then
  begin
    for vItem in valueList do
    begin
      vItem.Free;
    end;
    valueList.Free;
  end;
  if valueObjectList <> nil then
  begin
    valueObjectList.OwnsObjects := True;
    valueObjectList.Free;
  end;
  valueTObject.Free;
  inherited;
end;

end.
