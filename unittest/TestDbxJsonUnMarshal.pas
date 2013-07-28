unit TestDbxJsonUnMarshal;

interface

uses TestFramework, DbxJsonUnMarshal, TypesToTest, RestJsonUtils,
     Generics.Collections, SuperObject, Data.DbxJson;

type
  TTestDbxJsonUnMarshal = class(TTestCase)
  private
    FObject: TAllTypes;
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
    procedure valueString;
    procedure valueStringNull;
    procedure valueStringWithInvalidContent;
    procedure valueStringRenamed;
    procedure valueStringDefault;
    procedure valueChar;
    procedure valueAnsiChar;
    procedure valueSingle;
	  procedure valueSingleFromString;
    procedure valueExtended;
	  procedure valueExtendedFromString;
    procedure valueComp;
	  procedure valueCompFromString;
    procedure valueBooleanTrue;
    procedure valueBooleanFalse;
    procedure valueObject;
    procedure valueList;
    procedure valueObjectList;
    procedure valueDateTime;
    procedure valueEnum;
    procedure valueEnumFromString;
    procedure valueEnumFromName;
    procedure valueSet;
    procedure valueSetFromString;
  end;

  TTestDbxJsonUnMarshalCompatibility = class(TTestCase)
  protected
  published
     procedure CompareResultWithSuperObject;
  end;

implementation

uses System.Math, System.SysUtils, System.DateUtils;

{ TTestDbxJsonUnMarshal }

procedure TTestDbxJsonUnMarshal.empty;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{}');

  CheckNotNull(FObject);
end;

procedure TTestDbxJsonUnMarshal.invalidJsonSyntax;
begin
  ExpectedException := EJsonInvalidSyntax;

  TDBXJsonUnmarshal.FromJson<TAllTypes>('{"a : null}');
end;

procedure TTestDbxJsonUnMarshal.TearDown;
begin
  inherited;
  FObject.Free;
end;

procedure TTestDbxJsonUnMarshal.valueAnsiChar;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueAnsiChar" : "F"}');

  CheckNotNull(FObject);
  CheckEquals('F', String(FObject.valueAnsiChar));
end;

procedure TTestDbxJsonUnMarshal.valueBooleanFalse;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueBoolean" : false}');

  CheckNotNull(FObject);
  CheckFalse(FObject.valueBoolean);
end;

procedure TTestDbxJsonUnMarshal.valueBooleanTrue;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueBoolean" : true}');

  CheckNotNull(FObject);
  CheckTrue(FObject.valueBoolean);
end;

procedure TTestDbxJsonUnMarshal.valueChar;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueChar" : "F"}');

  CheckNotNull(FObject);
  CheckEquals('F', FObject.valueChar);
end;

procedure TTestDbxJsonUnMarshal.valueComp;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueComp" : 9223372036854775807}');

  CheckNotNull(FObject);
  CheckEquals(9223372036854775807, FObject.valueComp);
end;

procedure TTestDbxJsonUnMarshal.valueCompFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueComp" : "9223372036854775807"}');

  CheckNotNull(FObject);
  CheckEquals(9223372036854775807, FObject.valueComp);
end;

procedure TTestDbxJsonUnMarshal.valueCurrency;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueCurrency" : 1.23}');

  CheckNotNull(FObject);
  CheckEquals(RoundTo(1.23, -2), RoundTo(FObject.valueCurrency, -2));
end;

procedure TTestDbxJsonUnMarshal.valueDateTime;
var
  vDateTime: TDateTime;
  vJavaDate: Int64;
begin
  vDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 15);
  vJavaDate := DelphiToJavaDateTime(vDateTime);

  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueDateTime" : ' + IntToStr(vJavaDate) + '}');

  CheckNotNull(FObject);
  CheckEquals(vDateTime, FObject.valueDateTime);
end;

procedure TTestDbxJsonUnMarshal.valueDouble;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueDouble" : 1.23}');

  CheckNotNull(FObject);

  CheckEquals(RoundTo(1.23, -2), RoundTo(FObject.valueDouble, -2));
end;

procedure TTestDbxJsonUnMarshal.valueDoubleFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueDouble" : "1.23"}');

  CheckNotNull(FObject);

  CheckEquals(RoundTo(1.23, -2), RoundTo(FObject.valueDouble, -2));
end;

procedure TTestDbxJsonUnMarshal.valueEnum;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueEnum" : 1}');

  CheckNotNull(FObject);
  CheckTrue(etTwo = FObject.valueEnum);
end;

procedure TTestDbxJsonUnMarshal.valueEnumFromName;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueEnum" : "etTwo"}');

  CheckNotNull(FObject);
  CheckTrue(etTwo = FObject.valueEnum);
end;

procedure TTestDbxJsonUnMarshal.valueEnumFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueEnum" : "1"}');

  CheckNotNull(FObject);
  CheckTrue(etTwo = FObject.valueEnum);
end;

procedure TTestDbxJsonUnMarshal.valueExtended;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueExtended" : 123456.45}');

  CheckNotNull(FObject);
  CheckEquals(RoundTo(123456.45,-2), RoundTo(FObject.valueExtended,-2));
end;

procedure TTestDbxJsonUnMarshal.valueExtendedFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueExtended" : "123456.45"}');

  CheckNotNull(FObject);
  CheckEquals(RoundTo(123456.45,-2), RoundTo(FObject.valueExtended,-2));
end;

procedure TTestDbxJsonUnMarshal.valueInt64;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInt64" : 9223372036854775807}');

  CheckNotNull(FObject);
  CheckEquals(9223372036854775807, FObject.valueInt64);
end;

procedure TTestDbxJsonUnMarshal.valueInt64FromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInt64" : "9223372036854775807"}');

  CheckNotNull(FObject);
  CheckEquals(9223372036854775807, FObject.valueInt64);
end;

procedure TTestDbxJsonUnMarshal.valueInteger;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger" : 123456}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueInteger);
end;

procedure TTestDbxJsonUnMarshal.valueIntegerFromFalse;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger" : "false"}');

  CheckNotNull(FObject);
  CheckEquals(0, FObject.valueInteger);
end;

procedure TTestDbxJsonUnMarshal.valueIntegerFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger" : "123456"}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueInteger);
end;

procedure TTestDbxJsonUnMarshal.valueIntegerFromTrue;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger" : "true"}');

  CheckNotNull(FObject);
  CheckEquals(1, FObject.valueInteger);
end;

procedure TTestDbxJsonUnMarshal.valueList;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueList" : [{"valueString" : "one"},{"valueString" : "two"}]}');

  CheckNotNull(FObject);
  CheckNotNull(FObject.valueList);
  CheckEquals(2, FObject.valueList.Count);
  CheckEquals('one', FObject.valueList[0].valueString);
  CheckEquals('two', FObject.valueList[1].valueString);
end;

procedure TTestDbxJsonUnMarshal.valueObject;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueTObject" : {"valueString" : "teste"}}');

  CheckNotNull(FObject);
  CheckNotNull(FObject.valueTObject);
  CheckEquals('teste', FObject.valueTObject.valueString);
end;

procedure TTestDbxJsonUnMarshal.valueObjectList;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueObjectList" : [{"valueString" : "one"},{"valueString" : "two"}]}');

  CheckNotNull(FObject);
  CheckNotNull(FObject.valueObjectList);
  CheckEquals(2, FObject.valueObjectList.Count);
  CheckEquals('one', FObject.valueObjectList[0].valueString);
  CheckEquals('two', FObject.valueObjectList[1].valueString);
end;

procedure TTestDbxJsonUnMarshal.valueSet;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueSet" : 7}');

  CheckNotNull(FObject);
  CheckTrue(etOne in FObject.valueSet);
  CheckTrue(etTwo in FObject.valueSet);
  CheckTrue(etThree in FObject.valueSet);
end;

procedure TTestDbxJsonUnMarshal.valueSetFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueSet" : "7"}');

  CheckNotNull(FObject);
  CheckTrue(etOne in FObject.valueSet);
  CheckTrue(etTwo in FObject.valueSet);
  CheckTrue(etThree in FObject.valueSet);
end;

procedure TTestDbxJsonUnMarshal.valueSingle;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueSingle" : 123456}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueSingle);
end;

procedure TTestDbxJsonUnMarshal.valueSingleFromString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueSingle" : "123456"}');

  CheckNotNull(FObject);
  CheckEquals(123456, FObject.valueSingle);
end;

procedure TTestDbxJsonUnMarshal.valueString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueString" : "Fabricio"}');

  CheckNotNull(FObject);
  CheckEquals('Fabricio', FObject.valueString);
end;

procedure TTestDbxJsonUnMarshal.valueStringDefault;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{}');

  CheckNotNull(FObject);
  CheckEquals('default', FObject.fieldDefaultValue);
end;

procedure TTestDbxJsonUnMarshal.valueStringNull;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueString" : null}');

  CheckNotNull(FObject);
  CheckEquals('', FObject.valueString);

end;

procedure TTestDbxJsonUnMarshal.valueStringRenamed;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"renamed" : "Fabricio"}');

  CheckNotNull(FObject);
  CheckEquals('Fabricio', FObject.fieldNameRenamed);
end;

procedure TTestDbxJsonUnMarshal.valueStringWithInvalidContent;
begin
  try
    TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueString" : {"dummy" : null}}');
    Fail('Exception not raised');
  except
    on E: Exception do
    begin
      CheckTrue(E is EJsonInvalidValueForField);
    end;
  end;

  try
    TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueString" : ["dummy", null]}');
    Fail('Exception not raised');
  except
    on E: Exception do
    begin
      CheckTrue(E is EJsonInvalidValueForField);
    end;
  end;
end;

{ TTestDbxJsonUnMarshalCompatibility }

procedure TTestDbxJsonUnMarshalCompatibility.CompareResultWithSuperObject;
var
  vRoot, vRestored: TAllTypes;
  vChild1, vChild2, vChild3, vChild4: TAllTypes;
  vExpectedJson,
  vRestoredJson: string;
begin
  vChild1 := TAllTypes.Create;
  vChild1.valueInteger := 123;

  vChild2 := TAllTypes.Create;
  vChild2.valueDouble := 1.23;

  vChild3 := TAllTypes.Create;
  vChild3.valueString := 'third';

  vChild4 := TAllTypes.Create;
  vChild4.valueDateTime := Now;

  vRoot := TAllTypes.Create;
  try
    vRoot.valueInteger := 123456;
    vRoot.valueDouble := 123456.78;
    vRoot.valueCurrency := 123456.78;
    vRoot.valueString := 'json object';
    vRoot.valueAnsiChar := 'F';
    vRoot.valueChar := '资';
    vRoot.valueInt64 := 123456789;
    vRoot.valueSingle := 1234;
    vRoot.valueExtended := 123456789;
    vRoot.valueComp := 123456789;
    vRoot.fieldNameRenamed := 'renamed';
    vRoot.fieldDefaultValue := 'default';
    vRoot.valueBoolean := True;
    vRoot.valueDateTime := Now;
    vRoot.valueEnum := etThree;
    vRoot.valueSet := [etOne, etThree];

    vRoot.valueTObject := TAllTypes.Create;
    vRoot.valueList := TList<TAllTypes>.Create;
    vRoot.valueList.Add(vChild1);
    vRoot.valueList.Add(vChild2);
    vRoot.valueObjectList := TObjectList<TAllTypes>.Create;
    vRoot.valueObjectList.Add(vChild3);
    vRoot.valueObjectList.Add(vChild4);

    vExpectedJson := vRoot.ToJson().AsJSon(True);

    vRestored := TDbxJsonUnMarshal.FromJson<TAllTypes>(vExpectedJson);
    try
      vRestoredJson := vRestored.ToJson().AsJSon(True);

      CheckEquals(vExpectedJson, vRestoredJson);
    finally
      vRestored.Free;
    end;
  finally
    vRoot.Free;
  end;
end;

initialization
  RegisterTest(TTestDbxJsonUnMarshal.Suite);
  RegisterTest(TTestDbxJsonUnMarshalCompatibility.Suite);

end.
