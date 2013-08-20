unit TestOldRttiMarshal;

interface

uses TestFramework, SuperObject, OldRttiMarshal, Classes, Contnrs, DbxJsonUtils;

type
  {$M+}
  TEnumType = (etOne, etTwo, etThree);

  TEnumTypeSet = set of TEnumType;

  TRecordTypes = record
    one: string;
    two: Integer;
  end;

  TAllTypes = class;

  TAllTypes = class
  private
    FvalueChar: Char;
    FvalueComp: Comp;
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
     //Only for test. Use the Int64 type for better performance.
    property valueComp: Comp read FvalueComp write FvalueComp;
    property valueBoolean: Boolean read FvalueBoolean write FvalueBoolean;
    property valueTObject: TAllTypes read FvalueTObject write FvalueTObject;
    property valueList: TList read FvalueList write FvalueList;
    property valueObjectList: TObjectList read FvalueObjectList write FvalueObjectList;
    property valueDateTime: TDateTime read FvalueDateTime write FvalueDateTime;
    property valueEnum: TEnumType read FvalueEnum write FvalueEnum;
    property valueSet: TEnumTypeSet read FvalueSet write FvalueSet;
  end;
  {$M-}

  TNoSerializable = class
  end;

type
  TTestOldRttiMarshal = class(TTestCase)
  private
    FObject: TAllTypes;
    FJson: ISuperObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure valueInteger;
    procedure valueInt64;
    procedure valueDouble;
    procedure valueCurrency;
    procedure valueSingle;
    procedure valueExtended;
    procedure valueComp;
    procedure valueString;
    procedure valueAnsiString;
    procedure valueChar;
    procedure valueAnsiChar;
    procedure valueBooleanTrue;
    procedure valueBooleanFalse;
    procedure valueDateTime;
    procedure valueEnum;
    procedure valueSet;
    procedure valueObject;
    procedure valueList;
    procedure valueObjectList;
    procedure returnEmptyForNullObject;
    procedure List;
    procedure ObjectList;
  end;

implementation

uses SysUtils;

{ TTestOldRttiMarshal }

procedure TTestOldRttiMarshal.SetUp;
begin
  inherited;
  FObject := TAllTypes.Create;
end;

procedure TTestOldRttiMarshal.TearDown;
begin
  inherited;
  FObject.Free;
  FJson := nil;
end;

procedure TTestOldRttiMarshal.valueInteger;
begin
  FObject.valueInteger := 123;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123, FJson.I['valueInteger']);
end;

procedure TTestOldRttiMarshal.valueInt64;
begin
  FObject.valueInt64 := 123456789;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123456789, FJson.I['valueInt64']);
end;

procedure TTestOldRttiMarshal.valueAnsiChar;
begin
  FObject.valueAnsiChar := 'F';

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals('F', FJson.S['valueAnsiChar']);
end;

procedure TTestOldRttiMarshal.valueAnsiString;
begin
  FObject.valueAnsiString := 'marshal - #';

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals('marshal - #', FJson.S['valueAnsiString']);
end;

procedure TTestOldRttiMarshal.valueBooleanFalse;
begin
  FObject.valueBoolean := False;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckFalse(FJson.B['valueBoolean']);
end;

procedure TTestOldRttiMarshal.valueBooleanTrue;
begin
  FObject.valueBoolean := True;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckTrue(FJson.B['valueBoolean']);
end;

procedure TTestOldRttiMarshal.valueChar;
begin
  FObject.valueChar := 'F';

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals('F', FJson.S['valueChar']);
end;

procedure TTestOldRttiMarshal.valueComp;
begin
  FObject.valueComp := 123456789;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123456789, FJson.I['valueComp']);
end;

procedure TTestOldRttiMarshal.valueCurrency;
begin
  FObject.valueCurrency := 123.45;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123.45, FJson.C['valueCurrency'], 0.001);
end;

procedure TTestOldRttiMarshal.valueDateTime;
var
  vJavaDate: Int64;
begin
  FObject.valueDateTime := Now;

  vJavaDate := DelphiToJavaDateTime(FObject.valueDateTime);

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(vJavaDate, FJson.D['valueDateTime']);

end;

procedure TTestOldRttiMarshal.valueDouble;
begin
  FObject.valueDouble := 123.45;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123.45, FJson.D['valueDouble'], 0.001);
end;

procedure TTestOldRttiMarshal.valueEnum;
begin
  FObject.valueEnum := etTwo;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(1, FJson.I['valueEnum']);
end;

procedure TTestOldRttiMarshal.valueExtended;
begin
  FObject.valueExtended := 123.45;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123.45, FJson.D['valueExtended'], 0.001);
end;

procedure TTestOldRttiMarshal.valueSet;
begin
  FObject.valueSet := [etOne, etThree];

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(5, FJson.I['valueSet']);
end;

procedure TTestOldRttiMarshal.valueSingle;
begin
  FObject.valueSingle := 123.45;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123.45, FJson.D['valueSingle'], 0.001);
end;

procedure TTestOldRttiMarshal.valueString;
begin
  FObject.valueString := 'marshal - ';

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals('marshal - ', FJson.S['valueString']);
end;

procedure TTestOldRttiMarshal.valueObject;
var
  vObject: ISuperObject;
begin
  FObject.valueInteger := 123;
  FObject.valueTObject := TAllTypes.Create;
  FObject.valueTObject.valueInteger := 456;

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123, FJson.I['valueInteger']);

  vObject := FJson.O['valueTObject'];
  CheckNotNull(vObject);
  CheckTrue(vObject.IsType(stObject));
  CheckEquals(456, vObject.I['valueInteger']);
end;

procedure TTestOldRttiMarshal.valueList;
var
  vItem: TAllTypes;
  vObject: ISuperObject;
begin
  vItem := TAllTypes.Create;
  vItem.valueInteger := 456;

  FObject.valueInteger := 123;
  FObject.valueList := TList.Create;
  FObject.valueList.Add(vItem);

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123, FJson.I['valueInteger']);

  vObject := FJson.O['valueList'];
  CheckNotNull(vObject, 'valueList is null');
  CheckTrue(vObject.IsType(stArray));
  CheckEquals(1, vObject.AsArray.Length);
  CheckEquals(456, vObject.AsArray.O[0].I['valueInteger']);
end;

procedure TTestOldRttiMarshal.valueObjectList;
var
  vItem: TAllTypes;
  vObject: ISuperObject;
begin
  vItem := TAllTypes.Create;
  vItem.valueInteger := 456;

  FObject.valueInteger := 123;
  FObject.valueObjectList := TObjectList.Create;
  FObject.valueObjectList.Add(vItem);

  FJson := TOldRttiMarshal.ToJson(FObject);

  CheckNotNull(FJson);
  CheckEquals(123, FJson.I['valueInteger']);

  vObject := FJson.O['valueObjectList'];
  CheckNotNull(vObject);
  CheckTrue(vObject.IsType(stArray));
  CheckEquals(1, vObject.AsArray.Length);
  CheckEquals(456, vObject.AsArray.O[0].I['valueInteger']);
end;

procedure TTestOldRttiMarshal.returnEmptyForNullObject;
begin
  CheckEquals('{}', TOldRttiMarshal.ToJson(nil).AsJSon());
end;

procedure TTestOldRttiMarshal.List;
var
  vList: TList;
  vItem: ISuperObject;
begin
  vList := TList.Create;
  try
    FObject.valueInteger := 123;

    vList.Add(FObject);
    
    FJson := TOldRttiMarshal.ToJson(vList);

    CheckNotNull(FJson);
    CheckTrue(FJson.IsType(stArray),'is array');
    CheckEquals(1, FJson.AsArray.Length, 'array length');

    vItem := FJson.AsArray.O[0];
    
    CheckEquals(123, vItem.I['valueInteger']);
  finally
    vList.Free;
  end;
end;

procedure TTestOldRttiMarshal.ObjectList;
var
  vList: TObjectList;
  vItem: ISuperObject;
begin
  vList := TObjectList.Create(False);
  try
    FObject.valueInteger := 123;

    vList.Add(FObject);
    
    FJson := TOldRttiMarshal.ToJson(vList);

    CheckNotNull(FJson);
    CheckTrue(FJson.IsType(stArray),'is array');
    CheckEquals(1, FJson.AsArray.Length, 'array length');

    vItem := FJson.AsArray.O[0];
    
    CheckEquals(123, vItem.I['valueInteger']);
  finally
    vList.Free;
  end;
end;

{ TAllTypes }

destructor TAllTypes.Destroy;
begin
  if valueList <> nil then
  begin
    while valueList.Count > 0 do
    begin
      TObject(valueList[valueList.Count-1]).Free;
      valueList.Delete(valueList.Count-1);
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

initialization
  RegisterTest(TTestOldRttiMarshal.Suite);

end.
