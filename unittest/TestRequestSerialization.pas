unit TestRequestSerialization;

interface

{$I DelphiRest.inc}

uses BaseTestRest, Classes, RestClient, RestUtils, Person, Contnrs;

type 
  TTestRequestSerialization = class(TBaseTestRest)
  protected
    procedure SetUp; override;
  published
    procedure EchoJsonObjectWithPost;
    procedure EchoJsonObjectListWithPost;
    procedure EchoJsonObjectWithPut;
    procedure GetJsonObject;
    procedure GetJsonList;
    procedure DeleteJsonObject;
    procedure RemovePerson;
  end;

implementation

uses SysUtils;

{ TTestRequestSerialization }

procedure TTestRequestSerialization.DeleteJsonObject;
var
  vPerson: TOldPerson;
begin
  vPerson := TOldPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
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

procedure TTestRequestSerialization.EchoJsonObjectListWithPost;
var
  vReqJson,
  vRespJson: TObjectList;
begin
  vReqJson := TObjectList.Create;
  try
    vReqJson.Add(TOldPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com'));

    vRespJson := TObjectList(RestClient.Resource(CONTEXT_PATH + 'json/persons')
                                       .Accept(RestUtils.MediaType_Json)
                                       .ContentType(RestUtils.MediaType_Json)
                                       .Post(TJsonListAdapter.NewFrom(vReqJson, TOldPerson)));
    try
      vRespJson.OwnsObjects := True;
      CheckEqualsString(TOldPerson(vReqJson[0]).ToString, TOldPerson(vRespJson[0]).ToString);
    finally
      vRespJson.Free;
    end;
  finally
    vReqJson.Free;
  end;
end;

procedure TTestRequestSerialization.EchoJsonObjectWithPost;
var
  vReqJson,
  vRespJson: TOldPerson;
begin
  vReqJson := TOldPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
  try
    vRespJson := TOldPerson(RestClient.Resource(CONTEXT_PATH + 'json/person')
                           .Accept(RestUtils.MediaType_Json)
                           .ContentType(RestUtils.MediaType_Json)
                           .Post(vReqJson));
    try
      Status(vRespJson.ToString);
      CheckEqualsString(vReqJson.ToString, vRespJson.ToString);
    finally
      vRespJson.Free;
    end;
  finally
    vReqJson.Free;
  end;
end;

procedure TTestRequestSerialization.EchoJsonObjectWithPut;
var
  vReqJson,
  vRespJson: TOldPerson;
begin
  vReqJson := TOldPerson.NewFrom(123, 'Fabricio', 'fabricio.colombo.mva@gmail.com');
  try
    vRespJson := TOldPerson(RestClient.Resource(CONTEXT_PATH + 'json/person')
                                       .Accept(RestUtils.MediaType_Json)
                                       .ContentType(RestUtils.MediaType_Json)
                                       .Put(vReqJson));
    try
      CheckEqualsString(vReqJson.ToString, vRespJson.ToString);
    finally
      vRespJson.Free;
    end;
  finally
    vReqJson.Free;
  end;
end;

procedure TTestRequestSerialization.GetJsonList;
var
  vResponse: TObjectList;
  vPerson: TOldPerson;
  i: Integer;
begin
  vResponse := TObjectList(RestClient.Resource(CONTEXT_PATH + 'persons')
                                      .Accept(RestUtils.MediaType_Json)
                                      .GetList(TObjectList, TOldPerson));
  try
    CheckNotNull(vResponse);
    vResponse.OwnsObjects := True;
    CheckEquals(4, vResponse.Count);

    for i := 0 to vResponse.Count-1 do
    begin
      vPerson := TOldPerson(vResponse[i]);
      CheckTrue(vPerson.id > 0);
      CheckNotEqualsString('', vPerson.name);
      CheckNotEqualsString('', vPerson.email);
    end;
  finally
    vResponse.Free;
  end;
end;

procedure TTestRequestSerialization.GetJsonObject;
var
  vResponse: TOldPerson;
begin
  vResponse := TOldPerson(RestClient.Resource(CONTEXT_PATH + 'json/person')
                                    .Accept(RestUtils.MediaType_Json)
                                    .Get(TOldPerson));
  try
    CheckEquals(123, vResponse.Id);
    CheckEqualsString('Fabricio', vResponse.Name);
    CheckEqualsString('fabricio.colombo.mva@gmail.com', vResponse.EMail);
  finally
    vResponse.Free;
  end;
end;

procedure TTestRequestSerialization.RemovePerson;
var
  vPerson: TOldPerson;
begin
  vPerson := TOldPerson.Create;
  try
    vPerson.id := 4;

    RestClient.Resource(CONTEXT_PATH + 'person')
              .ContentType(RestUtils.MediaType_Json)
              .Delete(vPerson);

    CheckEquals(RestUtils.TStatusCode.NO_CONTENT.StatusCode, RestClient.ResponseCode);
  finally
    vPerson.Free;
  end;
end;

procedure TTestRequestSerialization.SetUp;
begin
  inherited;
  RestClient.Resource(CONTEXT_PATH + 'persons/reset').GET();
end;

initialization
  TTestRequestSerialization.RegisterTest;

end.
