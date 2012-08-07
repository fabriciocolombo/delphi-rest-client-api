unit TestDBXJson;

interface

{$I DelphiRest.inc}

{$IFDEF USE_GENERICS}
uses BaseTestRest, Classes, IdHttp, RestClient, RestUtils;

type
  TPerson = class(TObject)
  private
    (* Reflect the java object field names, case-sensitive *)
    id: Integer;
    name: String;
    email: String;
  public
    class function NewFrom(Id: Integer; Name, EMail: String): TPerson;
  end;

  TTestDbxJson = class(TBaseTestRest)
  private
  published
    procedure MarshalAndUnmarshal;
    procedure EchoJsonObjectWithPost;
    procedure EchoJsonObjectWithPut;
    procedure GetJsonObject;
    procedure DeleteJsonObject;
  end;
{$ENDIF}

implementation

{$IFDEF USE_GENERICS}
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

procedure TTestDbxJson.GetJsonObject;
var
  vResponse: TPerson;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'json/person')
                          .Accept(RestUtils.MediaType_Json)
                          .Get<TPerson>(TPerson);
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

    vResponse := TJsonUtil.UnMarshal<TPerson>(TPerson, vAsJson);
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

{ TPerson }

class function TPerson.NewFrom(Id: Integer; Name, EMail: String): TPerson;
begin
  Result := TPerson.Create;
  Result.Id := Id;
  Result.Name := Name;
  Result.EMail := EMail;
end;

initialization
  TTestDbxJson.RegisterTest;

{$ENDIF}

  
end.
