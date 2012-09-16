unit TestDataSetHandler;

interface


{$I DelphiRest.inc}

uses BaseTestRest, Classes, IdHttp, RestClient, RestUtils, SuperObject,
     RestJsonUtils, DB, DBClient, Person, SysUtils, DateUtils;

type
  TTestDataSetHandler = class(TBaseTestRest)
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
  end;

implementation

{ TTestDataSetHandler }

uses JsonToDataSetConverter;

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

procedure TTestDataSetHandler.JsonSomeTypesToDataSet;
var
  vJson: string;
  vDate: TDateTime;
  vIsoDate: string;
begin
  vDate := EncodeDateTime(2012, 9, 16, 13, 48, 20, 0);
  vIsoDate := SuperObject.DelphiDateTimeToISO8601Date(vDate);

  vJson := '{"id":123,"name":"Fabricio Colombo","value" : 123.45, "active" : true, "createDate" : "' + vIsoDate + '"}';

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

  function CreateField(DataSet: TDataSet; FieldType: TFieldType; const FieldName: string = '';ASize: Integer=0): TField;
  begin
    Result:= DefaultFieldClasses[FieldType].Create(DataSet);
    Result.FieldName:= FieldName;
    if Result.FieldName = '' then
      Result.FieldName:= 'Field' + IntToStr(DataSet.FieldCount +1);
    Result.FieldKind := fkData;
    Result.DataSet:= DataSet;
    Result.Name:= DataSet.Name + Result.FieldName;
    Result.Size := ASize;

    if (FieldType = ftString) and (ASize <= 0) then
      raise Exception.CreateFmt('Size não definido para o campo "%s".',[FieldName]);
  end;

var
  vAddress: TDataSetField;
begin
  inherited;
  FDataSet := TClientDataSet.Create(nil);

  CreateField(FDataSet, ftInteger, 'id');
  CreateField(FDataSet, ftString, 'name', 60);
  CreateField(FDataSet, ftFloat, 'value');
  CreateField(FDataSet, ftBoolean, 'active');
  CreateField(FDataSet, ftDateTime, 'createDate');

  vAddress := TDataSetField(CreateField(FDataSet, ftDataSet, 'addresses'));

  CreateField(vAddress.NestedDataSet, ftInteger, 'id');
  CreateField(vAddress.NestedDataSet, ftString, 'address', 60);

  FDataSet.CreateDataSet;
end;

procedure TTestDataSetHandler.TearDown;
begin
  FDataSet.Free;
  inherited;
end;

initialization
  TTestDataSetHandler.RegisterTest;

end.
