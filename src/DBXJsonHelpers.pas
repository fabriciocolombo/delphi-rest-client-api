unit DBXJsonHelpers;

interface

{$I DelphiRest.inc}

uses Rtti, TypInfo, DBXJson, DbxJsonUtils;

type
  TJsonValueHelper = class helper for TJsonValue
  private
  public
    function IsJsonNumber: Boolean;
    function IsJsonTrue: Boolean;
    function IsJsonFalse: Boolean;
    function IsJsonString: Boolean;
    function IsJsonNull: Boolean;
    function IsJsonObject: Boolean;
    function IsJsonArray: Boolean;

    function AsJsonNumber: TJSONNumber;
    function AsJsonString: TJSONString;
    function AsJsonObject: TJSONObject;
    function AsJsonArray: TJSONArray;
  end;

  {$IF defined(DELPHI_2010) or defined(DELPHI_XE)}
  TJsonObjectHelper = class helper for TJsonObject
  public
    function Get(const PairName: UnicodeString): TJSONPair; overload;
    class function ParseJSONValue(const Data: String): TJSONValue; overload; static;
  end;

  TJsonNumberHelper = class helper for TJsonNumber
  public
    function AsInt64: Int64;
  end;
  {$IFEND}

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetFieldName: string;
    function FormatUsingISO8601: Boolean;
  end;

implementation

uses SysUtils;

{ TJsonValueHelper }

function TJsonValueHelper.AsJsonArray: TJSONArray;
begin
  Result := Self as TJSONArray;
end;

function TJsonValueHelper.AsJsonNumber: TJSONNumber;
begin
  Result := Self as TJSONNumber;
end;

function TJsonValueHelper.AsJsonObject: TJSONObject;
begin
  Result := Self as TJSONObject;
end;

function TJsonValueHelper.AsJsonString: TJSONString;
begin
  Result := Self as TJSONString;
end;

function TJsonValueHelper.IsJsonArray: Boolean;
begin
  Result := ClassType = TJSONArray;
end;

function TJsonValueHelper.IsJsonFalse: Boolean;
begin
  Result := ClassType = TJSONFalse;
end;

function TJsonValueHelper.IsJsonNull: Boolean;
begin
  Result := ClassType = TJSONNull;
end;

function TJsonValueHelper.IsJsonNumber: Boolean;
begin
  Result := ClassType = TJSONNumber;
end;

function TJsonValueHelper.IsJsonObject: Boolean;
begin
  Result := ClassType = TJSONObject;
end;

function TJsonValueHelper.IsJsonString: Boolean;
begin
  Result := ClassType = TJSONString;
end;

function TJsonValueHelper.IsJsonTrue: Boolean;
begin
  Result := ClassType = TJSONTrue;
end;

{ TRttiFieldHelper }

function TRttiFieldHelper.FormatUsingISO8601: Boolean;
var
  attr: TCustomAttribute;
begin
  for attr in GetAttributes do
  begin
    if attr is JsonISO8601 then
    begin
      Exit(True);
    end;
  end;

  Result := False;
end;

function TRttiFieldHelper.GetFieldName: string;
var
  attr: TCustomAttribute;
begin
  for attr in GetAttributes do
  begin
    if attr is JsonName then
    begin
      Exit(JsonName(attr).Name);
    end;
  end;

  Result := Name;
end;

{$IF defined(DELPHI_2010) or defined(DELPHI_XE) }
{ TJsonObjectHelper }

function TJsonObjectHelper.Get(const PairName: UnicodeString): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Size - 1 do
  begin
    Candidate := Get(i);
    if (Candidate.JsonString.Value = PairName) then
      Exit(Candidate);
  end;
  Result := nil;
end;

class function TJsonObjectHelper.ParseJSONValue(const Data: String): TJSONValue;

  function Sanitize(AData: String): string;
  begin
    Result := AData;
    //Result := StringReplace(Result, '" : ', '":', [rfReplaceAll]);
   // Result := StringReplace(Result, '": ', '":', [rfReplaceAll]);
  end;

var
  vJsonData: string;
begin
  vJsonData := Sanitize(Data);

  Result := ParseJSONValue(TEncoding.Default.GetBytes(vJsonData), 0);
end;

{ TJsonNumberHelper }

function TJsonNumberHelper.AsInt64: Int64;
begin
  Result := StrToInt64(ToString);
end;

{$IFEND}

end.
