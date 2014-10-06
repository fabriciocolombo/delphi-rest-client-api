unit TestPeople;

interface

uses BaseTestRest, Classes, DBClient, Db;

{$I DelphiRest.inc}

type
  TTestPeople = class(TBaseTestRest)
  private
    function WordCount(AText, AWord: String): Integer;

    procedure ExtractPerson(Response: string; List: TStrings);

    procedure CheckPerson(Name, EMail: String;Id: Integer=0);overload;
    procedure CheckPerson(Response: String; Name, EMail: String;Id: Integer=0);overload;
  protected
    procedure SetUp; override;
  published
    procedure GettingAllPeoples;
    procedure GetPerson;
    procedure AddPerson;
    procedure AddPersonFromString;
    procedure UpdatePerson;
    procedure UpdatePersonFromString;
    procedure RemovePersonById;
    procedure PersonNotFound;

    {$IFDEF USE_SUPER_OBJECT}
    procedure GetAsDataSet;
    procedure GetAsDynamicDataSet;
    {$ENDIF}
  end;

implementation

uses RestUtils, StrUtils, SysUtils, Math, DataSetUtils, Person,
  TestFramework, HttpConnection;

{ TTestPeople }

procedure TTestPeople.AddPerson;
const
  NewPerson = '{"name":"%s","email":"%s"}';
  Name = 'Fabricio';
  EMail = 'fabricio.colombo.mva@gmail.com';
var
  vResult: String;
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(EmptyStr);
  try
    vStream.WriteString(Format(NewPerson,[Name, EMail]));

    vResult := RestClient.Resource(CONTEXT_PATH + 'person')
                         .Accept(RestUtils.MediaType_Json)
                         .ContentType(RestUtils.MediaType_Json)
                         .POST(vStream);

    CheckEquals(RestUtils.TStatusCode.CREATED.StatusCode, RestClient.ResponseCode);

    CheckPerson(vResult, Name, EMail);
  finally
    vStream.Free;
  end;
end;

procedure TTestPeople.CheckPerson(Name, EMail: String;Id: Integer=0);
var
  vUrlRequest: string;
  vResult: string;
begin
  vUrlRequest := CONTEXT_PATH + Format('person/%d',[Id]);

  vResult := RestClient.Resource(vUrlRequest)
                       .Accept(RestUtils.MediaType_Json).GET();

  CheckEquals(RestUtils.TStatusCode.OK.StatusCode, RestClient.ResponseCode);

  CheckPerson(vResult, Name, EMail, Id);
end;

procedure TTestPeople.AddPersonFromString;
const
  NewPerson = '{"name":"%s","email":"%s"}';
  Name = 'Fabricio';
  EMail = 'fabricio.colombo.mva@gmail.com';
var
  vResult: String;
begin
  vResult := RestClient.Resource(CONTEXT_PATH + 'person')
                       .Accept(RestUtils.MediaType_Json)
                       .ContentType(RestUtils.MediaType_Json)
                       .POST(Format(NewPerson,[Name, EMail]));

  CheckEquals(RestUtils.TStatusCode.CREATED.StatusCode, RestClient.ResponseCode);

  CheckPerson(vResult, Name, EMail);
end;

procedure TTestPeople.CheckPerson(Response, Name, EMail: String; Id: Integer);
var
  vList: TStringList;
begin
  Response := Copy(Response,2, Length(Response)-2);

  CheckEquals(1, WordCount(Response, '"id"'));

  vList := TStringList.Create;
  try
    ExtractPerson(Response, vList);

    if (Id > 0) then
    begin
      CheckEqualsString(Format('"id":%d',[Id]), vList[0], 'Id invalid');
    end;
    CheckEqualsString(Format('"name":"%s"',[Name]), vList[1], 'Name invalid');
    CheckEqualsString(Format('"email":"%s"',[EMail]), vList[2], 'E-Mail invalid');
  finally
    vList.Free;
  end;
end;

procedure TTestPeople.ExtractPerson(Response: string; List: TStrings);
begin
  List.Clear;

  ExtractStrings([','], [' '], PChar(Response), List);
end;

{$IFDEF USE_SUPER_OBJECT}
procedure TTestPeople.GetAsDataSet;
const
  ExpectedPeoples = '[{"id":1,"name":"John Doe","email":"john@hotmail.com"},'+
                    '{"id":2,"name":"Mike Myers","email":"myers@hotmail.com"},'+
                    '{"id":3,"name":"Joseph Climber","email":"climber@hotmail.com"},'+
                    '{"id":4,"name":"Mikaela Romanova","email":"romanova@hotmail.com"}]';
var
  vDataSet: TClientDataSet;
begin
  vDataSet := TClientDataSet.Create(nil);
  try
    TDataSetUtils.CreateField(vDataSet, ftInteger, 'id');
    TDataSetUtils.CreateField(vDataSet, ftString, 'name', 100);
    TDataSetUtils.CreateField(vDataSet, ftString, 'email', 100);
    vDataSet.CreateDataSet;

    RestClient.Resource(CONTEXT_PATH + 'persons')
              .Accept(RestUtils.MediaType_Json)
              .GetAsDataSet(vDataSet);

    CheckFalse(vDataSet.IsEmpty);
    CheckEquals(4, vDataSet.RecordCount);
  finally
    vDataSet.Free;
  end;
end;

procedure TTestPeople.GetAsDynamicDataSet;
const
  ExpectedPeoples = '[{"id":1,"name":"John Doe","email":"john@hotmail.com"},'+
                    '{"id":2,"name":"Mike Myers","email":"myers@hotmail.com"},'+
                    '{"id":3,"name":"Joseph Climber","email":"climber@hotmail.com"},'+
                    '{"id":4,"name":"Mikaela Romanova","email":"romanova@hotmail.com"}]';
var
  vDataSet: TDataSet;
begin
  vDataSet := RestClient.Resource(CONTEXT_PATH + 'persons')
                        .Accept(RestUtils.MediaType_Json)
                        .GetAsDataSet();

  try
    CheckFalse(vDataSet.IsEmpty);
    CheckEquals(4, vDataSet.RecordCount);
  finally
    vDataSet.Free;
  end;
end;
{$ENDIF}

procedure TTestPeople.GetPerson;
begin
  CheckPerson('John Doe', 'john@hotmail.com', 1);
  CheckPerson('Mike Myers', 'myers@hotmail.com', 2);
  CheckPerson('José Climber', 'climber@hotmail.com', 3);
  CheckPerson('Mikaela Romanova', 'romanova@hotmail.com', 4);
end;

procedure TTestPeople.GettingAllPeoples;
const
  ExpectedPeoples = '[{"id":1,"name":"John Doe","email":"john@hotmail.com"},'+
                    '{"id":2,"name":"Mike Myers","email":"myers@hotmail.com"},'+
                    '{"id":3,"name":"Joseph Climber","email":"climber@hotmail.com"},'+
                    '{"id":4,"name":"Mikaela Romanova","email":"romanova@hotmail.com"}]';
var
  vResult: string;
  vSize: Integer;
begin
  vResult := RestClient.Resource(CONTEXT_PATH + 'persons')
                       .Accept(RestUtils.MediaType_Json)
                       .Get();

  CheckTrue(vResult <> '');

  vSize := WordCount(vResult, '"id"');

  CheckTrue(vSize >= 4, Format('Expected 4 Peoples but was found %d.', [vSize]));
end;

procedure TTestPeople.PersonNotFound;
var
  response: string;
begin
  StartExpectingException(EHTTPError);
  RestClient.Resource(CONTEXT_PATH + 'person/999')
        .Accept(RestUtils.MediaType_Json)
        .GET();
  StopExpectingException('Person not found!');
end;

procedure TTestPeople.RemovePersonById;
var
  vResult: String;
begin
  RestClient.Resource(CONTEXT_PATH + Format('person/%d',[4]))
            .Accept(RestUtils.MediaType_Json)
            .Delete();

  CheckEquals(RestUtils.TStatusCode.NO_CONTENT.StatusCode, RestClient.ResponseCode);

  vResult := RestClient.Resource(CONTEXT_PATH + 'persons')
                       .Accept(RestUtils.MediaType_Json)
                       .GET();

  CheckEquals(3, WordCount(vResult, '"id"'));

end;

procedure TTestPeople.SetUp;
begin
  inherited;
  RestClient.Resource(CONTEXT_PATH + 'persons/reset').GET();
end;

procedure TTestPeople.UpdatePerson;
const
  UpdatePerson = '{"id":3,"name":"%s","email":"%s"}';
  Name = 'Joseph Kirk';
  EMail = 'kirk@hotmail.com';
var
  vResult: String;
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(EmptyStr);
  try
    vStream.WriteString(Format(UpdatePerson,[Name, EMail]));

    vResult := RestClient.Resource(CONTEXT_PATH + 'person')
                         .Accept(RestUtils.MediaType_Json)
                         .ContentType(RestUtils.MediaType_Json)
                         .Put(vStream);

    CheckEquals(201, RestClient.ResponseCode);

    CheckPerson(vResult, Name, EMail, 3);
  finally
    vStream.Free;
  end;
end;

procedure TTestPeople.UpdatePersonFromString;
const
  UpdatePerson = '{"id":3,"name":"%s","email":"%s"}';
  Name = 'Joseph Kirk';
  EMail = 'kirk@hotmail.com';
var
  vResult: String;
begin
  vResult := RestClient.Resource(CONTEXT_PATH + 'person')
                       .Accept(RestUtils.MediaType_Json)
                       .ContentType(RestUtils.MediaType_Json)
                       .Put(Format(UpdatePerson,[Name, EMail]));

  CheckEquals(201, RestClient.ResponseCode);

  CheckPerson(vResult, Name, EMail, 3);
end;

function TTestPeople.WordCount(AText, AWord: String): Integer;
var
  vPos: Integer;
begin
  Result := 0;

  vPos := Pos(AWord, AText);
  while vPos > 0 do
  begin
    Inc(Result);

    vPos := PosEx(AWord, AText, vPos+1);
  end;
end;

initialization
  TTestPeople.RegisterTest;

end.
