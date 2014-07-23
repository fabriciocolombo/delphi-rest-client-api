unit TestDbxJsonUnMarshal;

interface

uses TestFramework, DbxJsonUnMarshal, TypesToTest,
     Generics.Collections, SuperObject, DbxJson, DbxJsonUtils;

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
    procedure valueAnsiString;
    procedure valueString;
    procedure valueStringNull;
    procedure valueStringWithInvalidContent;
    procedure valueStringRenamed;
    procedure valueStringDefault;
    procedure valueStringDefaultFromEmpty;
    procedure valueStringDefaultFromNull;
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
    procedure WhiteSpaces;
  end;

  TTestDbxJsonUnMarshalCompatibility = class(TTestCase)
  protected
  published
     procedure CompareResultWithSuperObject;
  end;

  TJiraIssueStatus = class
  public
    name: string;
  end;

  TJiraIssueType = class
  public
    name: string;
  end;

  TJiraIssueFields = class
  public
    summary: string;
    issuetype: TJiraIssueType;
    status: TJiraIssueStatus;
    labels: TList<string>;
    updated: TDateTime;
    created: TDateTime;

    destructor Destroy; override;
  end;

  TJiraIssueResponse = class
  public
    id: Integer;
    key: string;
    fields: TJiraIssueFields;
    destructor Destroy; override;
  end;

  TTestJiraReponse = class(TTestCase)
  private
    function GetJsonIssueResponse: string;
  published
    procedure ParseIssueResponse;
  end;

implementation

uses Math, SysUtils, DateUtils, StrUtils;

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

procedure TTestDbxJsonUnMarshal.valueAnsiString;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueAnsiString" : "marshal - #"}');

  CheckNotNull(FObject);
  CheckEquals(AnsiString('marshal - #'), FObject.valueAnsiString);
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
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueString" : "marshal - ç"}');

  CheckNotNull(FObject);
  CheckEquals('marshal - ç', FObject.valueString);
end;

procedure TTestDbxJsonUnMarshal.valueStringDefault;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{}');

  CheckNotNull(FObject);
  CheckEquals('default', FObject.fieldDefaultValue);
end;

procedure TTestDbxJsonUnMarshal.valueStringDefaultFromEmpty;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"fieldDefaultValue" : ""}');

  CheckNotNull(FObject);
  CheckEquals('default', FObject.fieldDefaultValue);
end;

procedure TTestDbxJsonUnMarshal.valueStringDefaultFromNull;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"fieldDefaultValue" : null}');

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

procedure TTestDbxJsonUnMarshal.WhiteSpaces;
begin
  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger" : 123456}');
  CheckNotNull(FObject, 'Both');
  FreeAndNil(FObject);

  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger": 123456}');
  CheckNotNull(FObject, 'Right');
  FreeAndNil(FObject);

  FObject := TDbxJsonUnMarshal.FromJson<TAllTypes>('{"valueInteger" :123456}');
  CheckNotNull(FObject, 'Left');
  FreeAndNil(FObject);
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
  vChild1.fieldDefaultValue := 'default';

  vChild2 := TAllTypes.Create;
  vChild2.valueDouble := 1.23;
  vChild2.fieldDefaultValue := 'default';

  vChild3 := TAllTypes.Create;
  vChild3.valueString := 'third';
  vChild3.fieldDefaultValue := 'default';

  vChild4 := TAllTypes.Create;
  vChild4.valueDateTime := Now;
  vChild4.fieldDefaultValue := 'default';

  vRoot := TAllTypes.Create;
  try
    vRoot.valueInteger := 123456;
    vRoot.valueDouble := 123456.78;
    vRoot.valueCurrency := 123456.78;
    vRoot.valueString := 'json object';
    vRoot.valueAnsiChar := 'F';
    vRoot.valueChar := 'ç';
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
    vRoot.valueTObject.fieldDefaultValue := 'default';
    vRoot.valueList := TList<TAllTypes>.Create;
    vRoot.valueList.Add(vChild1);
    vRoot.valueList.Add(vChild2);
    vRoot.valueObjectList := TObjectList<TAllTypes>.Create;
    vRoot.valueObjectList.Add(vChild3);
    vRoot.valueObjectList.Add(vChild4);

    vExpectedJson := vRoot.ToJson().AsJSon(True);

    vRestored := TDbxJsonUnMarshal.FromJson<TAllTypes>(vRoot.ToJson().AsJSon(True));
    try
      CheckEquals('default', vRestored.fieldDefaultValue);
      vRestoredJson := vRestored.ToJson().AsJSon(True);

      CheckEquals(vExpectedJson, vRestoredJson);
    finally
      vRestored.Free;
    end;
  finally
    vRoot.Free;
  end;
end;

{ TTestJiraReponse }

function TTestJiraReponse.GetJsonIssueResponse: string;
begin
  Result := '{' + sLineBreak +
            '    "expand": "renderedFields,names,schema,transitions,editmeta,changelog",' + sLineBreak +
            '    "id": "10000",' + sLineBreak +
            '    "self": "http://localhost:8090/jira/rest/api/2/issue/10000",' + sLineBreak +
            '    "key": "MKY-1",' + sLineBreak +
            '    "fields": {' + sLineBreak +
            '        "summary": "First Test Issue",' + sLineBreak +
            '        "issuetype": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/issuetype/1",' + sLineBreak +
            '            "id": "1",' + sLineBreak +
            '            "description": "A problem which impairs or prevents the functions of the product.",' + sLineBreak +
            '            "iconUrl": "http://localhost:8090/jira/images/icons/bug.gif",' + sLineBreak +
            '            "name": "Bug",' + sLineBreak +
            '            "subtask": false' + sLineBreak +
            '        },' + sLineBreak +
            '        "status": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/status/1",' + sLineBreak +
            '            "description": "The issue is open and ready for the assignee to start work on it.",' + sLineBreak +
            '            "iconUrl": "http://localhost:8090/jira/images/icons/status_open.gif",' + sLineBreak +
            '            "name": "Open",' + sLineBreak +
            '            "id": "1"' + sLineBreak +
            '        },' + sLineBreak +
            '        "labels": ["label1", "label2"],' + sLineBreak +
            '        "votes": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/issue/MKY-1/votes",' + sLineBreak +
            '            "votes": 0,' + sLineBreak +
            '            "hasVoted": false' + sLineBreak +
            '        },' + sLineBreak +
            '        "workratio": -1,' + sLineBreak +
            '        "assignee": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/user?username=admin",' + sLineBreak +
            '            "name": "admin",' + sLineBreak +
            '            "emailAddress": "admin@example.com",' + sLineBreak +
            '            "avatarUrls": {' + sLineBreak +
            '                "16x16": "http://localhost:8090/jira/secure/useravatar?size=small&avatarId=10062",' + sLineBreak +
            '                "48x48": "http://localhost:8090/jira/secure/useravatar?avatarId=10062"' + sLineBreak +
            '            },' + sLineBreak +
            '            "displayName": "Administrator",' + sLineBreak +
            '            "active": true' + sLineBreak +
            '        },' + sLineBreak +
            '        "fixVersions": [],' + sLineBreak +
            '        "resolution": null,' + sLineBreak +
            '        "attachment": [],' + sLineBreak +
            '        "resolutiondate": null,' + sLineBreak +
            '        "project": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/project/MKY",' + sLineBreak +
            '            "id": "10001",' + sLineBreak +
            '            "key": "MKY",' + sLineBreak +
            '            "name": "monkey",' + sLineBreak +
            '            "avatarUrls": {' + sLineBreak +
            '                "16x16": "http://localhost:8090/jira/secure/projectavatar?size=small&pid=10001&avatarId=10011",' + sLineBreak +
            '                "48x48": "http://localhost:8090/jira/secure/projectavatar?pid=10001&avatarId=10011"' + sLineBreak +
            '            }' + sLineBreak +
            '        },' + sLineBreak +
            '        "versions": [],' + sLineBreak +
            '        "environment": null,' + sLineBreak +
            '        "updated": "2011-11-22T09:23:02.302-0300",' + sLineBreak +
            '        "created": "2011-11-22T09:22:59.899-0300",' + sLineBreak +
            '        "priority": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/priority/3",' + sLineBreak +
            '            "iconUrl": "http://localhost:8090/jira/images/icons/priority_major.gif",' + sLineBreak +
            '            "name": "Major",' + sLineBreak +
            '            "id": "3"' + sLineBreak +
            '        },' + sLineBreak +
            '        "description": null,' + sLineBreak +
            '        "duedate": null,' + sLineBreak +
            '        "components": [],' + sLineBreak +
            '        "watches": {' + sLineBreak +
            '            "self": "http://localhost:8090/jira/rest/api/2/issue/MKY-1/watchers",' + sLineBreak +
            '            "watchCount": 0,' + sLineBreak +
            '            "isWatching": false' + sLineBreak +
            '        }' + sLineBreak +
            '    }' + sLineBreak +
            '}';
end;

procedure TTestJiraReponse.ParseIssueResponse;
var
  vIssue: TJiraIssueResponse;
begin
  vIssue := TDbxJsonUnMarshal.FromJson<TJiraIssueResponse>(Self.GetJsonIssueResponse);
  try
    CheckNotNull(vIssue);

    CheckEquals(10000, vIssue.Id, 'Issue.Id');
    CheckEquals('MKY-1', vIssue.Key, 'Issue.Key');

    CheckNotNull(vIssue.Fields, 'Issue.Fields');
    CheckEquals('First Test Issue', vIssue.Fields.Summary, 'Fields.Summary');

    CheckNotNull(vIssue.Fields.Labels, 'Fields.Labels');
    CheckEquals(2, vIssue.Fields.Labels.Count, 'Fields.Labels.Count');
    CheckEquals('label1', vIssue.Fields.Labels.First, 'Fields.Labels.First');

    CheckEquals(EncodeDate(2011, 11, 22), DateOf(vIssue.Fields.Created), 'Fields.Created');

    CheckNotNull(vIssue.Fields.IssueType, 'Fields.IssueType');
    CheckEquals('Bug', vIssue.Fields.IssueType.Name, 'Fields.IssueType.Name');

    CheckNotNull(vIssue.Fields.Status, 'Fields.Status');
    CheckEquals('Open', vIssue.Fields.Status.Name, 'Fields.Status.Name');
  finally
    vIssue.Free;
  end;
end;

{ TJiraIssueResponse }

destructor TJiraIssueResponse.Destroy;
begin
  Fields.Free;
  inherited;
end;

{ TJiraIssueFields }

destructor TJiraIssueFields.Destroy;
begin
  IssueType.Free;
  Status.Free;
  Labels.Free;
  inherited;
end;

initialization
  RegisterTest(TTestDbxJsonUnMarshal.Suite);
  RegisterTest(TTestDbxJsonUnMarshalCompatibility.Suite);
  RegisterTest(TTestJiraReponse.Suite);

end.
