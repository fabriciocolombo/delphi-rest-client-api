unit TestDataSetHandler;

interface


{$I DelphiRest.inc}

uses BaseTestRest, Classes, IdHttp, RestClient, RestUtils, Generics.Collections,
     RestJsonUtils, DB, DBClient, Person;

type
  TTestDataSetHandler = class(TBaseTestRest)
  private
    FDataSet: TDataSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure JsonToDataSet;
  end;

implementation

{ TTestDataSetHandler }

uses JsonToDataSetConverter;

procedure TTestDataSetHandler.JsonToDataSet;
var
  vJson: string;
begin
  vJson := '{"id":123,"email":"fabricio.colombo.mva@gmail.com","name":"Fabricio Colombo"}';

  CheckEquals(0, FDataSet.RecordCount);
  TJsonToDataSetConverter.UnMarshalToDataSet(FDataSet, vJson);
  CheckEquals(1, FDataSet.RecordCount);

  CheckEquals(123, FDataSet.FieldByName('id').AsInteger);
  CheckEqualsString('Fabricio Colombo', FDataSet.FieldByName('name').AsString);
  CheckEqualsString('fabricio.colombo.mva@gmail.com', FDataSet.FieldByName('email').AsString);
end;

procedure TTestDataSetHandler.SetUp;
begin
  inherited;
  FDataSet := TClientDataSet.Create(nil);
  FDataSet.FieldDefs.Add('id', ftInteger);
  FDataSet.FieldDefs.Add('name', ftString, 60);
  FDataSet.FieldDefs.Add('email', ftString, 60);
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
