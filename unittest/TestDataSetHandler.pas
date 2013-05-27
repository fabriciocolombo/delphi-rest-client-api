unit TestDataSetHandler;

interface


{$I DelphiRest.inc}

uses TestFramework, Classes, RestClient, RestUtils, SuperObject,
     RestJsonUtils, DB, DBClient, Person, SysUtils, DateUtils;

type
  TTestDataSetHandler = class(TTestCase)
  private
    FDataSet: TClientDataSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure JsonToDataSet;
    procedure JsonToDataSetTwoRecords;
    procedure JsonSomeTypesToDataSet;
    procedure JsonToDataSetWithNestedDataSet;
    procedure JsonToDynamicDataSet;
    procedure JsonToDynamicDataSetWithNestedDataSet;
  end;

implementation

{ TTestDataSetHandler }

uses JsonToDataSetConverter, DataSetUtils;

procedure TTestDataSetHandler.JsonToDataSet;
var
  vJson: string;
begin
  vJson := '{"id":123,"name":"Fabricio Colombo"}';

  CheckEquals(0, FDataSet.RecordCount);
  TJsonToDataSetConverter.UnMarshalToDataSet(FDataSet, vJson);
  CheckEquals(1, FDataSet.RecordCount);

  CheckEquals(123, FDataSet.FieldByName('id').AsInteger);
  CheckEqualsString('Fabricio Colombo', FDataSet.FieldByName('name').AsString);
end;

procedure TTestDataSetHandler.JsonToDataSetTwoRecords;
var
  vJson: string;
begin
  vJson := '[{"id":1,"name":"Fabricio Colombo"},' +
           ' {"id":2,"name":"Jacob"}]';

  CheckEquals(0, FDataSet.RecordCount);
  TJsonToDataSetConverter.UnMarshalToDataSet(FDataSet, vJson);
  CheckEquals(2, FDataSet.RecordCount);

  CheckEquals(1, FDataSet.FieldByName('id').AsInteger);
  CheckEqualsString('Fabricio Colombo', FDataSet.FieldByName('name').AsString);

  FDataSet.Next;

  CheckEquals(2, FDataSet.FieldByName('id').AsInteger);
  CheckEqualsString('Jacob', FDataSet.FieldByName('name').AsString);
end;

procedure TTestDataSetHandler.JsonToDataSetWithNestedDataSet;
var
  vJson: string;
  vAddresses: TDataSet;
begin
  vJson := '{' +
           '   "id": 123,' +
           '   "name": "Fabricio Colombo",' +
           '   "addresses": [' +
           '       {' +
           '           "id": 1,' +
           '           "address": "street one"' +
           '       },' +
           '       {' +
           '           "id": 2,' +
           '           "address": "street two"' +
           '       }' +
           '   ]' +
           '}';

  CheckEquals(0, FDataSet.RecordCount);
  TJsonToDataSetConverter.UnMarshalToDataSet(FDataSet, vJson);
  CheckEquals(1, FDataSet.RecordCount);

  CheckEquals(123, FDataSet.FieldByName('id').AsInteger);
  CheckEqualsString('Fabricio Colombo', FDataSet.FieldByName('name').AsString);

  vAddresses := TDataSetField(FDataSet.FieldByName('addresses')).NestedDataSet;

  CheckFalse(vAddresses.IsEmpty, 'Addresses is empty');

  CheckEquals(1, vAddresses.FieldByName('id').AsInteger);
  CheckEqualsString('street one', vAddresses.FieldByName('address').AsString);

  vAddresses.Next;

  CheckEquals(2, vAddresses.FieldByName('id').AsInteger);
  CheckEqualsString('street two', vAddresses.FieldByName('address').AsString);
end;

procedure TTestDataSetHandler.JsonToDynamicDataSet;
var
  vJson: string;
  vDataSet: TClientDataSet;
begin
  vJson := '{"id":123,"name":"Fabricio Colombo"}';

  vDataSet := TJsonToDataSetConverter.CreateDataSetMetadata(vJson);
  try
    CheckNotNull(vDataSet);
    CheckTrue(vDataSet.Active);
    CheckEquals(2, vDataSet.FieldCount);
    CheckEquals('id', vDataSet.Fields[0].FieldName);
    CheckEquals('name', vDataSet.Fields[1].FieldName);
  finally
    vDataSet.Free;
  end;
end;

procedure TTestDataSetHandler.JsonToDynamicDataSetWithNestedDataSet;
var
  vJson: string;
  vDataSet: TClientDataSet;
  vAddresses: TDataSet;
begin
  vJson := '{' +
           '   "id": 123,' +
           '   "name": "Fabricio Colombo",' +
           '   "addresses": [' +
           '       {' +
           '           "id": 1,' +
           '           "address": "street one"' +
           '       },' +
           '       {' +
           '           "id": 2,' +
           '           "address": "street two"' +
           '       }' +
           '   ]' +
           '}';

  vDataSet := TJsonToDataSetConverter.CreateDataSetMetadata(vJson);
  try
    CheckNotNull(vDataSet);
    CheckTrue(vDataSet.Active);
    CheckEquals(3, vDataSet.FieldCount);
    CheckNotNull(vDataSet.FindField('id'));
    CheckNotNull(vDataSet.FindField('name'));
    CheckNotNull(vDataSet.FindField('addresses'));

    vAddresses := TDataSetField(FDataSet.FieldByName('addresses')).NestedDataSet;

    CheckTrue(vAddresses.Active);

    CheckEquals('id', vAddresses.Fields[0].FieldName);
    CheckEquals('address', vAddresses.Fields[1].FieldName);
  finally
    vDataSet.Free;
  end;
end;

procedure TTestDataSetHandler.JsonSomeTypesToDataSet;
var
  vJson: string;
  vDate: TDateTime;
  vStringDate: string;
begin
  vDate := EncodeDateTime(2012, 9, 16, 13, 48, 20, 0);

  vStringDate := StringReplace(FloatToStr(vDate), ',', '.', []);

  vJson := '{"id":123,"name":"Fabricio Colombo","value" : 123.45, "active" : true, "createDate" : "' + vStringDate + '"}';

  CheckEquals(0, FDataSet.RecordCount);
  TJsonToDataSetConverter.UnMarshalToDataSet(FDataSet, vJson);
  CheckEquals(1, FDataSet.RecordCount);

  CheckEquals(123, FDataSet.FieldByName('id').AsInteger);
  CheckEqualsString('Fabricio Colombo', FDataSet.FieldByName('name').AsString);
  CheckEquals(123.45, FDataSet.FieldByName('value').AsFloat, 0.001);
  CheckTrue(FDataSet.FieldByName('active').AsBoolean);
  CheckEquals(vDate, FDataSet.FieldByName('createDate').AsDateTime);
end;

procedure TTestDataSetHandler.SetUp;
var
  vAddress: TDataSetField;
begin
  inherited;
  FDataSet := TClientDataSet.Create(nil);

  TDataSetUtils.CreateField(FDataSet, ftInteger, 'id');
  TDataSetUtils.CreateField(FDataSet, ftString, 'name', 60);
  TDataSetUtils.CreateField(FDataSet, ftFloat, 'value');
  TDataSetUtils.CreateField(FDataSet, ftBoolean, 'active');
  TDataSetUtils.CreateField(FDataSet, ftDateTime, 'createDate');

  vAddress := TDataSetUtils.CreateDataSetField(FDataSet, 'addresses');

  TDataSetUtils.CreateField(vAddress.NestedDataSet, ftInteger, 'id');
  TDataSetUtils.CreateField(vAddress.NestedDataSet, ftString, 'address', 60);

  FDataSet.CreateDataSet;
end;

procedure TTestDataSetHandler.TearDown;
begin
  FDataSet.Free;
  inherited;
end;

initialization
  RegisterTest(TTestDataSetHandler.Suite);

end.
