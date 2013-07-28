unit RestJsonUtils;

interface

{$I DelphiRest.inc}

uses {$IFDEF USE_GENERICS}RestJsonGenerics, {$ENDIF}RestJsonOldRTTI, SysUtils;

type
  {$IFDEF USE_GENERICS}
  TJsonUtil = TJsonUtilGenerics;
  {$ELSE}
  TJsonUtil = TJsonUtilOldRTTI;
  {$ENDIF}

  EJsonInvalidValue = class(Exception);
  EJsonInvalidValueForField = class(Exception);
  EJsonInvalidSyntax = class(Exception);

  TJsonAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  JsonName = class(TJsonAttribute);
  JsonDefault = class(TJsonAttribute);


function JavaToDelphiDateTime(const dt: int64): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime): int64;

implementation

uses Windows;

function TzSpecificLocalTimeToSystemTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpLocalTime, lpUniversalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';

function SystemTimeToTzSpecificLocalTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpUniversalTime, lpLocalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';

function JavaToDelphiDateTime(const dt: int64): TDateTime;
var
  t: TSystemTime;
begin
  DateTimeToSystemTime(25569 + (dt / 86400000), t);
  SystemTimeToTzSpecificLocalTime(nil, @t, @t);
  Result := SystemTimeToDateTime(t);
end;

function DelphiToJavaDateTime(const dt: TDateTime): int64;
var
  t: TSystemTime;
begin
  DateTimeToSystemTime(dt, t);
  TzSpecificLocalTimeToSystemTime(nil, @t, @t);
  Result := Round((SystemTimeToDateTime(t) - 25569) * 86400000)
end;


{ TJsonAttribute }

constructor TJsonAttribute.Create(const AName: string);
begin
  FName := AName;
end;

end.
