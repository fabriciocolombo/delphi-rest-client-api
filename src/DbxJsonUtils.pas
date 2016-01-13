unit DbxJsonUtils;

interface

uses SysUtils, DateUtils;

{$I DelphiRest.inc}

{$IFDEF SUPPORTS_GENERICS}
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
  JsonISO8601 = class(TCustomAttribute);
{$ENDIF}  

implementation

{$IFNDEF MACOS}
uses Windows;

{$IFDEF WINDOWSNT_COMPATIBILITY}
function DayLightCompareDate(const date: PSystemTime;
  const compareDate: PSystemTime): Integer;
var
  limit_day, dayinsecs, weekofmonth: Integer;
  First: Word;
begin
  if (date^.wMonth < compareDate^.wMonth) then
  begin
    Result := -1; (* We are in a month before the date limit. *)
    Exit;
  end;

  if (date^.wMonth > compareDate^.wMonth) then
  begin
    Result := 1; (* We are in a month after the date limit. *)
    Exit;
  end;

  (* if year is 0 then date is in day-of-week format, otherwise
   * it's absolute date.
   *)
  if (compareDate^.wYear = 0) then
  begin
    (* compareDate.wDay is interpreted as number of the week in the month
     * 5 means: the last week in the month *)
    weekofmonth := compareDate^.wDay;
    (* calculate the day of the first DayOfWeek in the month *)
    First := (6 + compareDate^.wDayOfWeek - date^.wDayOfWeek + date^.wDay) mod 7 + 1;
    limit_day := First + 7 * (weekofmonth - 1);
    (* check needed for the 5th weekday of the month *)
    if (limit_day > MonthDays[(date^.wMonth=2) and IsLeapYear(date^.wYear)][date^.wMonth]) then
      dec(limit_day, 7);
  end
  else
    limit_day := compareDate^.wDay;

  (* convert to seconds *)
  limit_day := ((limit_day * 24  + compareDate^.wHour) * 60 + compareDate^.wMinute ) * 60;
  dayinsecs := ((date^.wDay * 24  + date^.wHour) * 60 + date^.wMinute ) * 60 + date^.wSecond;
  (* and compare *)

  if dayinsecs < limit_day then
    Result :=  -1 else
    if dayinsecs > limit_day then
      Result :=  1 else
      Result :=  0; (* date is equal to the date limit. *)
end;

function CompTimeZoneID(const pTZinfo: PTimeZoneInformation;
  lpFileTime: PFileTime; islocal: Boolean): LongWord;
var
  ret: Integer;
  beforeStandardDate, afterDaylightDate: Boolean;
  llTime: Int64;
  SysTime: TSystemTime;
  ftTemp: TFileTime;
begin
  llTime := 0;

  if (pTZinfo^.DaylightDate.wMonth <> 0) then
  begin
    (* if year is 0 then date is in day-of-week format, otherwise
     * it's absolute date.
     *)
    if ((pTZinfo^.StandardDate.wMonth = 0) or
        ((pTZinfo^.StandardDate.wYear = 0) and
        ((pTZinfo^.StandardDate.wDay < 1) or
        (pTZinfo^.StandardDate.wDay > 5) or
        (pTZinfo^.DaylightDate.wDay < 1) or
        (pTZinfo^.DaylightDate.wDay > 5)))) then
    begin
      SetLastError(ERROR_INVALID_PARAMETER);
      Result := TIME_ZONE_ID_INVALID;
      Exit;
    end;

    if (not islocal) then
    begin
      llTime := PInt64(lpFileTime)^;
      dec(llTime, Int64(pTZinfo^.Bias + pTZinfo^.DaylightBias) * 600000000);
      PInt64(@ftTemp)^ := llTime;
      lpFileTime := @ftTemp;
    end;

    FileTimeToSystemTime(lpFileTime^, SysTime);

    (* check for daylight savings *)
    ret := DayLightCompareDate(@SysTime, @pTZinfo^.StandardDate);
    if (ret = -2) then
    begin
      Result := TIME_ZONE_ID_INVALID;
      Exit;
    end;

    beforeStandardDate := ret < 0;

    if (not islocal) then
    begin
      dec(llTime, Int64(pTZinfo^.StandardBias - pTZinfo^.DaylightBias) * 600000000);
      PInt64(@ftTemp)^ := llTime;
      FileTimeToSystemTime(lpFileTime^, SysTime);
    end;

    ret := DayLightCompareDate(@SysTime, @pTZinfo^.DaylightDate);
    if (ret = -2) then
    begin
      Result := TIME_ZONE_ID_INVALID;
      Exit;
    end;

    afterDaylightDate := ret >= 0;

    Result := TIME_ZONE_ID_STANDARD;
    if( pTZinfo^.DaylightDate.wMonth < pTZinfo^.StandardDate.wMonth ) then
    begin
      (* Northern hemisphere *)
      if( beforeStandardDate and afterDaylightDate) then
        Result := TIME_ZONE_ID_DAYLIGHT;
    end else    (* Down south *)
      if( beforeStandardDate or afterDaylightDate) then
        Result := TIME_ZONE_ID_DAYLIGHT;
  end else
    (* No transition date *)
    Result := TIME_ZONE_ID_UNKNOWN;
end;

function GetTimezoneBias(const pTZinfo: PTimeZoneInformation;
  lpFileTime: PFileTime; islocal: Boolean; pBias: PLongint): Boolean;
var
  bias: LongInt;
  tzid: LongWord;
begin
  bias := pTZinfo^.Bias;
  tzid := CompTimeZoneID(pTZinfo, lpFileTime, islocal);

  if( tzid = TIME_ZONE_ID_INVALID) then
  begin
    Result := False;
    Exit;
  end;
  if (tzid = TIME_ZONE_ID_DAYLIGHT) then
    inc(bias, pTZinfo^.DaylightBias)
  else if (tzid = TIME_ZONE_ID_STANDARD) then
    inc(bias, pTZinfo^.StandardBias);
  pBias^ := bias;
  Result := True;
end;

function SystemTimeToTzSpecificLocalTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpUniversalTime, lpLocalTime: PSystemTime): BOOL;
var
  ft: TFileTime;
  lBias: LongInt;
  llTime: Int64;
  tzinfo: TTimeZoneInformation;
begin
  if (lpTimeZoneInformation <> nil) then
    tzinfo := lpTimeZoneInformation^ else
    if (GetTimeZoneInformation(tzinfo) = TIME_ZONE_ID_INVALID) then
    begin
      Result := False;
      Exit;
    end;

  if (not SystemTimeToFileTime(lpUniversalTime^, ft)) then
  begin
    Result := False;
    Exit;
  end;
  llTime := PInt64(@ft)^;
  if (not GetTimezoneBias(@tzinfo, @ft, False, @lBias)) then
  begin
    Result := False;
    Exit;
  end;
  (* convert minutes to 100-nanoseconds-ticks *)
  dec(llTime, Int64(lBias) * 600000000);
  PInt64(@ft)^ := llTime;
  Result := FileTimeToSystemTime(ft, lpLocalTime^);
end;

function TzSpecificLocalTimeToSystemTime(
    const lpTimeZoneInformation: PTimeZoneInformation;
    const lpLocalTime: PSystemTime; lpUniversalTime: PSystemTime): BOOL;
var
  ft: TFileTime;
  lBias: LongInt;
  t: Int64;
  tzinfo: TTimeZoneInformation;
begin
  if (lpTimeZoneInformation <> nil) then
    tzinfo := lpTimeZoneInformation^
  else
    if (GetTimeZoneInformation(tzinfo) = TIME_ZONE_ID_INVALID) then
    begin
      Result := False;
      Exit;
    end;

  if (not SystemTimeToFileTime(lpLocalTime^, ft)) then
  begin
    Result := False;
    Exit;
  end;
  t := PInt64(@ft)^;
  if (not GetTimezoneBias(@tzinfo, @ft, True, @lBias)) then
  begin
    Result := False;
    Exit;
  end;
  (* convert minutes to 100-nanoseconds-ticks *)
  inc(t, Int64(lBias) * 600000000);
  PInt64(@ft)^ := t;
  Result := FileTimeToSystemTime(ft, lpUniversalTime^);
end;
{$ELSE}
function TzSpecificLocalTimeToSystemTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpLocalTime, lpUniversalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';

function SystemTimeToTzSpecificLocalTime(
  lpTimeZoneInformation: PTimeZoneInformation;
  lpUniversalTime, lpLocalTime: PSystemTime): BOOL; stdcall; external 'kernel32.dll';
{$ENDIF WINDOWSNT_COMPATIBILITY}
{$ENDIF MACOS}

{$IFDEF SUPPORTS_GENERICS}
{ TJsonAttribute }

constructor TJsonAttribute.Create(const AName: string);
begin
  FName := AName;
end;
{$ENDIF}

end.
