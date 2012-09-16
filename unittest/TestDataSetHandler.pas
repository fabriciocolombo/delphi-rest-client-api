unit TestDataSetHandler;

interface


{$I DelphiRest.inc}

uses BaseTestRest, Classes, IdHttp, RestClient, RestUtils, SuperObject,
     RestJsonUtils, DB, DBClient, Person, SysUtils, DateUtils;

type
  TTestDataSetHandler = class(TBaseTestRest)
  private
    FDataSet: TDataSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure JsonToDataSet;
    procedure JsonSomeTypesToDataSet;
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
begin
  inherited;
  FDataSet := TClientDataSet.Create(nil);
  FDataSet.FieldDefs.Add('id', ftInteger);
  FDataSet.FieldDefs.Add('name', ftString, 60);
  FDataSet.FieldDefs.Add('value', ftFloat);
  FDataSet.FieldDefs.Add('active', ftBoolean);
  FDataSet.FieldDefs.Add('createDate', ftDateTime);
  TClientDataSet(FDataSet).CreateDataSet;
end;

procedure TTestDataSetHandler.TearDown;
begin
  FDataSet.Free;
  inherited;
end;

initialization
  TTestDataSetHandler.RegisterTest;

end.
