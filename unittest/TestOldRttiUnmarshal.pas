unit TestOldRttiUnmarshal;

interface

uses TestFramework, Contnrs, Classes, OldRttiUnMarshal, DbxJsonUtils;

type
  {$M+}
  TEnumType = (etOne, etTwo, etThree);

  TEnumTypeSet = set of TEnumType;

  TRecordTypes = record
    one: string;
    two: Integer;
  end;

  TAllTypes = class;

  TAllTypes = class(TPersistent)
  private
    FvalueChar: Char;
    FvalueBoolean: Boolean;
    FvalueAnsiChar: AnsiChar;
    FvalueEnum: TEnumType;
    FvalueDateTime: TDateTime;
    FvalueExtended: Extended;
    FvalueObjectList: TObjectList;
    FvalueTObject: TAllTypes;
    FvalueInt64: Int64;
    FvalueList: TList;
    FvalueSingle: Single;
    FvalueString: string;
    FvalueCurrency: Currency;
    FvalueInteger: Integer;
    FvalueSet: TEnumTypeSet;
    FvalueAnsiString: AnsiString;
    FvalueDouble: Double;
  public
    destructor Destroy; override;
  published
    property valueInteger: Integer read FvalueInteger write FvalueInteger;
    property valueDouble: Double read FvalueDouble write FvalueDouble;
    property valueCurrency: Currency read FvalueCurrency write FvalueCurrency;
    property valueAnsiString: AnsiString read FvalueAnsiString write FvalueAnsiString;
    property valueString: string read FvalueString write FvalueString;
    property valueAnsiChar: AnsiChar read FvalueAnsiChar write FvalueAnsiChar;
    property valueChar: Char read FvalueChar write FvalueChar;
    property valueInt64: Int64 read FvalueInt64 write FvalueInt64;
    property valueSingle: Single read FvalueSingle write FvalueSingle;
    property valueExtended: Extended read FvalueExtended write FvalueExtended;
    property valueBoolean: Boolean read FvalueBoolean write FvalueBoolean;
    property valueTObject: TAllTypes read FvalueTObject write FvalueTObject;
    property AllTypesList: TList read FvalueList write FvalueList;
    property AllTypesObjectList: TObjectList read FvalueObjectList write FvalueObjectList;
    property valueDateTime: TDateTime read FvalueDateTime write FvalueDateTime;
    property valueEnum: TEnumType read FvalueEnum write FvalueEnum;
    property valueSet: TEnumTypeSet read FvalueSet write FvalueSet;
  end;
  {$M-}

  TTestOldRttiUnmarshal = class(TTestCase)
  private
    FObject: TAllTypes;

    function FromJson(AJson: string): TAllTypes;
  protected
    procedure TearDown; override;
  published
    procedure empty;
    procedure invalidJsonSyntax;
    procedure valueInteger;
	  procedure valueIntegerFromString;
    procedure valueIntegerFromTrue;
    procedure valueIntegerFromFalse;
    procedure valueDouble;
	  procedure valueDoubleFromString;
    procedure valueInt64;
    procedure valueInt64FromString;
    procedure valueCurrency;
    procedure valueAnsiString;
    procedure valueString;
    procedure valueStringNull;
    procedure valueStringWithInvalidContent;
    procedure valueChar;
    procedure valueAnsiChar;
    procedure valueSingle;
	  procedure valueSingleFromString;
    procedure valueExtended;
	  procedure valueExtendedFromString;
    procedure valueBooleanTrue;
    procedure valueBooleanFalse;
    procedure valueObject;
    procedure valueList;
    procedure valueObjectList;
    procedure valueDateTime;
    procedure valueEnum;
    procedure valueEnumFromString;
    procedure valueSet;
    procedure valueSetFromString;
    procedure FromList;
    procedure FromObjectList;
  end;

implementation

uses Math, SysUtils, DateUtils;

{ TTestOldRttiUnmarshal }

procedure TTestOldRttiUnmarshal.empty;
begin
  FObject := FromJson('{}');

  CheckNotNull(FObject);
end;

function TTestOldRttiUnmarshal.FromJson(AJson: string): TAllTypes;
begin
  Result := TAllTypes(TOldRttiUnMarshal.FromJson(TAllTypes, AJson));
end;

procedure TTestOldRttiUnmarshal.FromList;
var
  vList: TList;
begin
  vList := TList(TOldRttiUnMarshal.FromJsonArray(TList, TAllTypes, '[{"valueInteger":123}]'));
  try
    CheckNotNull(vList);
    CheckEquals(1, vList.Count);
    CheckEquals(123, TAllTypes(vList[0]).valueInteger);
  finally
    TAllTypes(vList[0]).Free;
    vList.Free;
  end;
end;

procedure TTestOldRttiUnmarshal.FromObjectList;
var
  vList: TObjectList;
begin
  vList := TObjectList(TOldRttiUnMarshal.FromJsonArray(TObjectList, TAllTypes, '[{"valueInteger":123}]'));
  try
    vList.OwnsObjects := True;
    CheckNotNull(vList);
    CheckEquals(1, vList.Count);
    CheckEquals(123, TAllTypes(vList[0]).valueInteger);
  finally
    vList.Free;
  end;
end;

procedure TTestOldRttiUnmarshal.invalidJsonSyntax;
begin
  ExpectedException := EJsonInvalidSyntax;

  FromJson('{"a : null}');
end;

procedure TTestOldRttiUnmarshal.TearDown;
begin
  inherited;
  FObject.Free;
end;

procedure TTestOldRttiUnmarshal.valueAnsiChar;
begin
  FObject := FromJson('{"valueAnsiChar" : "F"}');

  CheckNotNull(FObject);
  CheckEquals('F', String(FObject.valueAnsiChar));
end;

procedure TTestOldRttiUnmarshal.valueAnsiString;
begin
  FObject := FromJson('{"valueAnsiString" : "marshal - #"}');

  CheckNotNull(FObject);
  CheckEquals(AnsiString('marshal - #'), FObject.valueAnsiString);
end;

procedure TTestOldRttiUnmarshal.valueBooleanFalse;
begin
  FObject := FromJson('{"valueBoolean" : false}');

  CheckNotNull(FObject);
  CheckFalse(FObject.valueBoolean);
end;

procedure TTestOldRttiUnmarshal.valueBooleanTrue;
begin
  FObject := FromJson('{"valueBoolean" : true}');

  CheckNotNull(FObject);
  CheckTrue(FObject.valueBoolean);
end;

procedure TTestOldRttiUnmarshal.valueChar;
begin
  FObject := FromJson('{"valueChar" : "F"}');

  CheckNotNull(FObject);
  CheckEquals('F', FObject.valueChar);
end;

procedure TTestOldRttiUnmarshal.valueCurrency;
begin
  FObject := FromJson('{"valueCurrency" : 1.23}');

  CheckNotNull(FObject);
  CheckEquals(RoundTo(1.23, -2), RoundTo(FObject.valueCurrency, -2));
end;

procedure TTestOldRttiUnmarshal.valueDateTime;
var
  vDateTime: TDateTime;
  vJavaDate: Int64;
begin
  vDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 15);
  vJavaDate := DelphiToJavaDateTime(vDateTime);

  FObject := FromJson('{"valueDateTime" : ' + IntToStr(vJavaDate) + '}');

  CheckNotNull(FObject);
  CheckEquals(vDateTime, FObject.valueDateTime);
end;

procedure TTestOldRttiUnmarshal.valueDouble;
begin
  FObject := FromJson('{"valueDouble" : 1.23}');

  CheckNotNull(FObject);

  CheckEquals(RoundTo(1.23, -2), RoundTo(FObject.valueDouble, -2));
end;

procedure TTestOldRttiUnmarshal.valueDoubleFromString;
begin
  FObject := FromJson('{"valueDouble" : "1.23"}');

  CheckNotNull(FObject);

  CheckEquals(RoundTo(1.23, -2), RoundTo(FObject.valueDouble, -2));
end;

procedure TTestOldRttiUnmarshal.valueEnum;
begin
  FObject := FromJson('{"valueEnum" : 1}');

  CheckNotNull(FObject);
  CheckTrue(etTwo = FObject.valueEnum);
end;

procedure TTestOldRttiUnmarshal.valueEnumFromString;
begin
  FObject := FromJson('{"valueEnum" : "1"}');

  CheckNotNull(FObject);
  CheckTrue(etTwo = FObject.valueEnum);
end;

procedure TTestOldRttiUnmarshal.valueExtended;
begin
  FObject := FromJson('{"valueExtended" : 123456.45}');

  CheckNotNull(FObject);
  CheckEquals(RoundTo(123456.45,-2), RoundTo(FObject.valueExtended,-2));
end;

procedure TTestOldRttiUnmarshal.valueExtendedFromString;
begin
  FObject := FromJson('{"valueExtended" : "123456.45"}');

  CheckNotNull(FObject);
  CheckEquals(RoundTo(123456.45,-2), RoundTo(FObject.valueExtended,-2));
end;

procedure TTestOldRttiUnmarshal.valueInt64;
begin
  FObject := FromJson('{"valueInt64" : 9223372036854775806}');

  CheckNotNull(FObject);
  CheckEquals(9223372036854775806, FObject.valueInt64);
end;

procedure TTestOldRttiUnmarshal.valueInt64FromString;
begin
  FObject := FromJson('{"valueInt64" : "9223372036854775807"}');

  CheckNotNull(FObject);
  CheckEquals(9223372036854775807, FObject.valueInt64);
end;

procedure TTestOldRttiUnmarshal.valueInteger;
begin
  FObject := FromJson('{"valueInteger" : 123456}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueInteger);
end;

procedure TTestOldRttiUnmarshal.valueIntegerFromFalse;
begin
  FObject := FromJson('{"valueInteger" : "false"}');

  CheckNotNull(FObject);
  CheckEquals(0, FObject.valueInteger);
end;

procedure TTestOldRttiUnmarshal.valueIntegerFromString;
begin
  FObject := FromJson('{"valueInteger" : "123456"}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueInteger);
end;

procedure TTestOldRttiUnmarshal.valueIntegerFromTrue;
begin
  FObject := FromJson('{"valueInteger" : "true"}');

  CheckNotNull(FObject);
  CheckEquals(1, FObject.valueInteger);
end;

procedure TTestOldRttiUnmarshal.valueList;
begin
  FObject := FromJson('{"AllTypesList" : [{"valueString" : "one"},{"valueString" : "two"}]}');

  CheckNotNull(FObject);
  CheckNotNull(FObject.AllTypesList);
  CheckEquals(2, FObject.AllTypesList.Count);
  CheckEquals('one', TAllTypes(FObject.AllTypesList[0]).valueString);
  CheckEquals('two', TAllTypes(FObject.AllTypesList[1]).valueString);
end;

procedure TTestOldRttiUnmarshal.valueObject;
begin
  FObject := FromJson('{"valueTObject" : {"valueString" : "teste"}}');

  CheckNotNull(FObject);
  CheckNotNull(FObject.valueTObject);
  CheckEquals('teste', FObject.valueTObject.valueString);
end;

procedure TTestOldRttiUnmarshal.valueObjectList;
begin
  FObject := FromJson('{"AllTypesObjectList" : [{"valueString" : "one"},{"valueString" : "two"}]}');

  CheckNotNull(FObject);
  CheckNotNull(FObject.AllTypesObjectList);
  CheckEquals(2, FObject.AllTypesObjectList.Count);
  CheckEquals('one', TAllTypes(FObject.AllTypesObjectList[0]).valueString);
  CheckEquals('two', TAllTypes(FObject.AllTypesObjectList[1]).valueString);
end;

procedure TTestOldRttiUnmarshal.valueSet;
begin
  FObject := FromJson('{"valueSet" : 7}');

  CheckNotNull(FObject);
  CheckTrue(etOne in FObject.valueSet);
  CheckTrue(etTwo in FObject.valueSet);
  CheckTrue(etThree in FObject.valueSet);
end;

procedure TTestOldRttiUnmarshal.valueSetFromString;
begin
  FObject := FromJson('{"valueSet" : "7"}');

  CheckNotNull(FObject);
  CheckTrue(etOne in FObject.valueSet);
  CheckTrue(etTwo in FObject.valueSet);
  CheckTrue(etThree in FObject.valueSet);
end;

procedure TTestOldRttiUnmarshal.valueSingle;
begin
  FObject := FromJson('{"valueSingle" : 123456}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueSingle);
end;

procedure TTestOldRttiUnmarshal.valueSingleFromString;
begin
  FObject := FromJson('{"valueSingle" : "123456"}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueSingle);
end;

procedure TTestOldRttiUnmarshal.valueString;
begin
  FObject := FromJson('{"valueString" : "marshal - "}');

  CheckNotNull(FObject);
  CheckEquals('marshal - ', FObject.valueString);
end;

procedure TTestOldRttiUnmarshal.valueStringNull;
begin
  FObject := FromJson('{"valueString" : null}');

  CheckNotNull(FObject);
  CheckEquals('', FObject.valueString);
end;

procedure TTestOldRttiUnmarshal.valueStringWithInvalidContent;
begin
  try
    FromJson('{"valueString" : {"dummy" : null}}');
    Fail('Exception not raised');
  except
    on E: Exception do
    begin
      CheckTrue(E is EJsonInvalidValueForField);
    end;
  end;

  try
    FromJson('{"valueString" : ["dummy", null]}');
    Fail('Exception not raised');
  except
    on E: Exception do
    begin
      CheckTrue(E is EJsonInvalidValueForField);
    end;
  end;
end;

{ TAllTypes }

destructor TAllTypes.Destroy;
begin
  if AllTypesList <> nil then
  begin
    while AllTypesList.Count > 0 do
    begin
      TObject(AllTypesList[AllTypesList.Count-1]).Free;
      AllTypesList.Delete(AllTypesList.Count-1);
    end;
    AllTypesList.Free;
  end;
  if AllTypesObjectList <> nil then
  begin
    AllTypesObjectList.OwnsObjects := True;
    AllTypesObjectList.Free;
  end;
  valueTObject.Free;
  inherited;
end;

initialization
  RegisterTest(TTestOldRttiUnmarshal.Suite);
  RegisterClass(TAllTypes);

end.
