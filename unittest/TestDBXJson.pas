unit TestDBXJson;

interface

{$I DelphiRest.inc}

uses BaseTestRest, Classes, IdHttp, RestClient, RestUtils, Generics.Collections,
     RestJsonUtils, Person;

type
  TTestDbxJson = class(TBaseTestRest)
  private
  protected
    procedure SetUp; override;
  published
    procedure MarshalAndUnmarshal;
    procedure EchoJsonObjectWithPost;
    procedure EchoJsonObjectListWithPost;
    procedure EchoJsonObjectWithPut;
    procedure GetJsonObject;
    procedure GetJsonList;
    procedure DeleteJsonObject;
    procedure RemovePerson;
  end;

implementation

{ TTestDbxJson }

procedure TTestDbxJson.EchoJsonObjectWithPut;
var
  vReqJson,
  vRespJson: TPerson;
begin
  vReqJson := TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
  try
    vRespJson := RestClient.Resource(CONTEXT_PATH + 'json/person')
                           .Accept(RestUtils.MediaType_Json)
                           .ContentType(RestUtils.MediaType_Json)
                           .Put<TPerson>(vReqJson);
    try
      CheckEqualsString(vReqJson.ToString, vRespJson.ToString);
    finally
      vRespJson.Free;
    end;
  finally
    vReqJson.Free;
  end;
end;

procedure TTestDbxJson.GetJsonList;
var
  vResponse: TObjectList<TPerson>;
  vPerson: TPerson;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'persons')
                          .Accept(RestUtils.MediaType_Json)
                          .Get<TObjectList<TPerson>>();
  try
    CheckNotNull(vResponse);
    vResponse.OwnsObjects := True;
    CheckEquals(4, vResponse.Count);

    for vPerson in vResponse do
    begin
      CheckTrue(vPerson.id > 0);
      CheckNotEqualsString('', vPerson.name);
      CheckNotEqualsString('', vPerson.email);
    end;
  finally
    vResponse.Free;
  end;
end;

procedure TTestDbxJson.GetJsonObject;
var
  vResponse: TPerson;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'json/person')
                          .Accept(RestUtils.MediaType_Json)
                          .Get<TPerson>();
  try
    CheckEquals(123, vResponse.Id);
    CheckEqualsString('Fabricio', vResponse.Name);
    CheckEqualsString('fabricio.colombo.mva@gmail.com', vResponse.EMail);
  finally
    vResponse.Free;
  end;
end;

procedure TTestDbxJson.MarshalAndUnmarshal;
var
  vPerson,
  vResponse: TPerson;
  vAsJson: string;
begin
  vPerson := TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
  try
    vAsJson := TJsonUtil.Marshal(vPerson);

    vResponse := TJsonUtil.UnMarshal<TPerson>(vAsJson);
    try
      CheckEquals(vPerson.Id, vResponse.Id);
      CheckEqualsString(vPerson.Name, vResponse.Name);
      CheckEqualsString(vPerson.EMail, vResponse.EMail);
    finally
      vResponse.Free;
    end;
  finally
    vPerson.Free;
  end;
end;

procedure TTestDbxJson.DeleteJsonObject;
var
  vPerson: TPerson;
begin
  vPerson := TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
  try
    RestClient.Resource(CONTEXT_PATH + 'json/person')
               .Accept(RestUtils.MediaType_Json)
               .ContentType(RestUtils.MediaType_Json)
               .Delete(vPerson);

    CheckEquals(204, RestClient.ResponseCode);
  finally
    vPerson.Free;
  end;

end;

procedure TTestDbxJson.EchoJsonObjectListWithPost;
var
  vReqJson,
  vRespJson: TObjectList<TPerson>;
begin
  vReqJson := TObjectList<TPerson>.Create;
  try
    vReqJson.Add(TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com'));

    vRespJson := RestClient.Resource(CONTEXT_PATH + 'json/persons')
                           .Accept(RestUtils.MediaType_Json)
                           .ContentType(RestUtils.MediaType_Json)
                           .Post<TObjectList<TPerson>>(vReqJson);
    try
      vRespJson.OwnsObjects := True;
      CheckEqualsString(vReqJson[0].ToString, vRespJson[0].ToString);
    finally
      vRespJson.Free;
    end;
  finally
    vReqJson.Free;
  end;
end;

procedure TTestDbxJson.EchoJsonObjectWithPost;
var
  vReqJson,
  vRespJson: TPerson;
begin
  vReqJson := TPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
  try
    vRespJson := RestClient.Resource(CONTEXT_PATH + 'json/person')
                           .Accept(RestUtils.MediaType_Json)
                           .ContentType(RestUtils.MediaType_Json)
                           .Post<TPerson>(vReqJson);
    try
      CheckEqualsString(vReqJson.ToString, vRespJson.ToString);
    finally
      vRespJson.Free;
    end;
  finally
    vReqJson.Free;
  end;
end;

procedure TTestDbxJson.RemovePerson;
var
  vPerson: TPerson;
  vResult: String;
begin
  vPerson := TPerson.Create;
  try
    vPerson.id := 4;

    RestClient.Resource(CONTEXT_PATH + 'person')
              .ContentType(RestUtils.MediaType_Json)
              .Delete(vPerson);

    CheckEquals(RestUtils.TStatusCode.NO_CONTENT.StatusCode, RestClient.ResponseCode);

    vResult := RestClient.Resource(CONTEXT_PATH + 'persons')
                         .Accept(RestUtils.MediaType_Json)
                         .GET();
  finally
    vPerson.Free;
  end;
end;

procedure TTestDbxJson.SetUp;
begin
  inherited;
  RestClient.Resource(CONTEXT_PATH + 'persons/reset').GET();
end;

initialization
  TTestDbxJson.RegisterTest;

end.
