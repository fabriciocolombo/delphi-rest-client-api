unit DBXJsonHelpers;

interface

uses System.Rtti, System.TypInfo, Data.DBXJson, DbxJsonUtils;

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

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetFieldName: string;
  end;

implementation

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

end.
