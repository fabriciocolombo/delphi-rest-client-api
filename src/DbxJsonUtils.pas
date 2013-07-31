unit DbxJsonUtils;

interface

uses System.SysUtils, System.DateUtils;

type
  TJsonAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  JsonName = class(TJsonAttribute);
  JsonDefault = class(TJsonAttribute);

  EJsonInvalidValue = class(Exception);
  EJsonInvalidValueForField = class(Exception);
  EJsonInvalidSyntax = class(Exception);

function JavaToDelphiDateTime(const dt: int64): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime): int64;

implementation

function JavaToDelphiDateTime(const dt: int64): TDateTime;
var
  univTime: TDateTime;
begin
  univTime := UnixDateDelta + (dt / 86400000);

  Result := TTimeZone.Local.ToLocalTime(univTime);
end;

function DelphiToJavaDateTime(const dt: TDateTime): int64;
var
  univTime: TDateTime;
begin
  univTime := TTimeZone.Local.ToUniversalTime(dt);

  Result := Round((univTime - UnixDateDelta) * 86400000);
end;

{ TJsonAttribute }

constructor TJsonAttribute.Create(const AName: string);
begin
  FName := AName;
end;

end.
