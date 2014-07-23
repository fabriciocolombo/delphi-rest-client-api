unit DbxJsonUtils;

interface

uses SysUtils, DateUtils;

type
{$I DelphiRest.inc}

{$IFDEF USE_GENERICS}
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

  EJsonInvalidValue = class(Exception);
  EJsonInvalidValueForField = class(Exception);
  EJsonInvalidSyntax = class(Exception);
  ENoSerializableClass = class(Exception)
  public
    constructor Create(AClass: TClass);
  end;

function JavaToDelphiDateTime(const dt: int64): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime): int64;
function ISO8601DateToJavaDateTime(const str: String; var ms: Int64): Boolean;
function ISO8601DateToDelphiDateTime(const str: string; var dt: TDateTime): Boolean;
function DelphiDateTimeToISO8601Date(dt: TDateTime): string;

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

{$IFDEF DELPHI_XE_UP}
function JavaToDelphiDateTime(const dt: int64): TDateTime;
var
  univTime: TDateTime;
begin
  univTime := UnixDateDelta + (dt / 86400000);

  if DateOf(univTime) = 0 then
    Exit(0);

  Result := TTimeZone.Local.ToLocalTime(univTime);
end;

function DelphiToJavaDateTime(const dt: TDateTime): int64;
var
  univTime: TDateTime;
begin
  univTime := TTimeZone.Local.ToUniversalTime(dt);

  Result := Round((univTime - UnixDateDelta) * 86400000);
end;
{$ELSE}
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
{$ENDIF}

{$IFDEF USE_GENERICS}
{ TJsonAttribute }

constructor TJsonAttribute.Create(const AName: string);
begin
  FName := AName;
end;
{$ENDIF}

{$IFDEF UNIX}
function GetTimeBias: integer;
var
  TimeVal: TTimeVal;
  TimeZone: TTimeZone;
begin
  fpGetTimeOfDay(@TimeVal, @TimeZone);
  Result := TimeZone.tz_minuteswest;
end;
{$ELSE}
function GetTimeBias: integer;
var
  tzi : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(tzi) of
    TIME_ZONE_ID_UNKNOWN : Result := tzi.Bias;
    TIME_ZONE_ID_STANDARD: Result := tzi.Bias + tzi.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := tzi.Bias + tzi.DaylightBias;
  else
    Result := 0;
  end;
end;
{$ENDIF}

function ISO8601DateToJavaDateTime(const str: string; var ms: Int64): Boolean;
type
  TState = (
    stStart, stYear, stMonth, stWeek, stWeekDay, stDay, stDayOfYear,
    stHour, stMin, stSec, stMs, stUTC, stGMTH, stGMTM,
    stGMTend, stEnd);

  TPerhaps = (yes, no, perhaps);
  TDateTimeInfo = record
    year: Word;
    month: Word;
    week: Word;
    weekday: Word;
    day: Word;
    dayofyear: Integer;
    hour: Word;
    minute: Word;
    second: Word;
    ms: Word;
    bias: Integer;
  end;

{$if (sizeof(Char) = 1)}
  PSOChar = PWideChar;
  SOChar = WideChar;
{$else}
  SOChar = Char;
  PSOChar = PChar;
{$ifend}

var
  p: PSOChar;
  state: TState;
  pos, v: Word;
  sep: TPerhaps;
  inctz, havetz, havedate: Boolean;
  st: TDateTimeInfo;
  DayTable: PDayTable;

  function get(var v: Word; c: SOChar): Boolean; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
  begin
    if (c < #256) and (AnsiChar(c) in ['0'..'9']) then
    begin
      Result := True;
      v := v * 10 + Ord(c) - Ord('0');
    end else
      Result := False;
  end;

label
  error;
begin
  p := PSOChar(str);
  sep := perhaps;
  state := stStart;
  pos := 0;
  FillChar(st, SizeOf(st), 0);
  havedate := True;
  inctz := False;
  havetz := False;

  while true do
  case state of
    stStart:
      case p^ of
        '0'..'9': state := stYear;
        'T', 't':
          begin
            state := stHour;
            pos := 0;
            inc(p);
            havedate := False;
          end;
      else
        goto error;
      end;
    stYear:
      case pos of
        0..1,3:
              if get(st.year, p^) then
              begin
                Inc(pos);
                Inc(p);
              end else
                goto error;
        2:    case p^ of
                '0'..'9':
                  begin
                    st.year := st.year * 10 + ord(p^) - ord('0');
                    Inc(pos);
                    Inc(p);
                  end;
                ':':
                  begin
                    havedate := false;
                    st.hour := st.year;
                    st.year := 0;
                    inc(p);
                    pos := 0;
                    state := stMin;
                    sep := yes;
                  end;
              else
                goto error;
              end;
        4: case p^ of
             '-': begin
                    pos := 0;
                    Inc(p);
                    sep := yes;
                    state := stMonth;
                  end;
             '0'..'9':
                  begin
                    sep := no;
                    pos := 0;
                    state := stMonth;
                  end;
             'W', 'w' :
                  begin
                    pos := 0;
                    Inc(p);
                    state := stWeek;
                  end;
             'T', 't', ' ':
                  begin
                    state := stHour;
                    pos := 0;
                    inc(p);
                    st.month := 1;
                    st.day := 1;
                  end;
             #0:
                  begin
                    st.month := 1;
                    st.day := 1;
                    state := stEnd;
                  end;
           else
             goto error;
           end;
      end;
    stMonth:
      case pos of
        0:  case p^ of
              '0'..'9':
                begin
                  st.month := ord(p^) - ord('0');
                  Inc(pos);
                  Inc(p);
                end;
              'W', 'w':
                begin
                  pos := 0;
                  Inc(p);
                  state := stWeek;
                end;
            else
              goto error;
            end;
        1:  if get(st.month, p^) then
            begin
              Inc(pos);
              Inc(p);
            end else
              goto error;
        2: case p^ of
             '-':
                  if (sep in [yes, perhaps])  then
                  begin
                    pos := 0;
                    Inc(p);
                    state := stDay;
                    sep := yes;
                  end else
                    goto error;
             '0'..'9':
                  if sep in [no, perhaps] then
                  begin
                    pos := 0;
                    state := stDay;
                    sep := no;
                  end else
                  begin
                    st.dayofyear := st.month * 10 + Ord(p^) - Ord('0');
                    st.month := 0;
                    inc(p);
                    pos := 3;
                    state := stDayOfYear;
                  end;
             'T', 't', ' ':
                  begin
                    state := stHour;
                    pos := 0;
                    inc(p);
                    st.day := 1;
                 end;
             #0:
               begin
                 st.day := 1;
                 state := stEnd;
               end;
           else
             goto error;
           end;
      end;
    stDay:
      case pos of
        0:  if get(st.day, p^) then
            begin
              Inc(pos);
              Inc(p);
            end else
              goto error;
        1:  if get(st.day, p^) then
            begin
              Inc(pos);
              Inc(p);
            end else
            if sep in [no, perhaps] then
            begin
              st.dayofyear := st.month * 10 + st.day;
              st.day := 0;
              st.month := 0;
              state := stDayOfYear;
            end else
              goto error;

        2: case p^ of
             'T', 't', ' ':
                  begin
                    pos := 0;
                    Inc(p);
                    state := stHour;
                  end;
             #0:  state := stEnd;
           else
             goto error;
           end;
      end;
    stDayOfYear:
      begin
        if (st.dayofyear <= 0) then goto error;
        case p^ of
          'T', 't', ' ':
               begin
                 pos := 0;
                 Inc(p);
                 state := stHour;
               end;
          #0:  state := stEnd;
        else
          goto error;
        end;
      end;
    stWeek:
      begin
        case pos of
          0..1: if get(st.week, p^) then
                begin
                  inc(pos);
                  inc(p);
                end else
                  goto error;
          2: case p^ of
               '-': if (sep in [yes, perhaps]) then
                    begin
                      Inc(p);
                      state := stWeekDay;
                      sep := yes;
                    end else
                      goto error;
               '1'..'7':
                    if sep in [no, perhaps] then
                    begin
                      state := stWeekDay;
                      sep := no;
                    end else
                      goto error;
             else
               goto error;
             end;
        end;
      end;
    stWeekDay:
      begin
        if (st.week > 0) and get(st.weekday, p^) then
        begin
          inc(p);
          v := st.year - 1;
          v := ((v * 365) + (v div 4) - (v div 100) + (v div 400)) mod 7 + 1;
          st.dayofyear := (st.weekday - v) + ((st.week) * 7) + 1;
          if v <= 4 then dec(st.dayofyear, 7);
          case p^ of
            'T', 't', ' ':
                 begin
                   pos := 0;
                   Inc(p);
                   state := stHour;
                 end;
            #0:  state := stEnd;
          else
            goto error;
          end;
        end else
          goto error;
      end;
    stHour:
      case pos of
        0:    case p^ of
                '0'..'9':
                    if get(st.hour, p^) then
                    begin
                      inc(pos);
                      inc(p);
                      end else
                        goto error;
                '-':
                  begin
                    inc(p);
                    state := stMin;
                  end;
              else
                goto error;
              end;
        1:    if get(st.hour, p^) then
              begin
                inc(pos);
                inc(p);
              end else
                goto error;
        2: case p^ of
             ':': if sep in [yes, perhaps] then
                  begin
                    sep := yes;
                    pos := 0;
                    Inc(p);
                    state := stMin;
                  end else
                    goto error;
             ',', '.':
                begin
                  Inc(p);
                  state := stMs;
                end;
             '+':
               if havedate then
               begin
                 state := stGMTH;
                 pos := 0;
                 v := 0;
                 inc(p);
               end else
                 goto error;
             '-':
               if havedate then
               begin
                 state := stGMTH;
                 pos := 0;
                 v := 0;
                 inc(p);
                 inctz := True;
               end else
                 goto error;
             'Z', 'z':
                  if havedate then
                    state := stUTC else
                    goto error;
             '0'..'9':
                  if sep in [no, perhaps] then
                  begin
                    pos := 0;
                    state := stMin;
                    sep := no;
                  end else
                    goto error;
             #0:  state := stEnd;
           else
             goto error;
           end;
      end;
    stMin:
      case pos of
        0: case p^ of
             '0'..'9':
                if get(st.minute, p^) then
                begin
                  inc(pos);
                  inc(p);
                end else
                  goto error;
             '-':
                begin
                  inc(p);
                  state := stSec;
                end;
           else
             goto error;
           end;
        1: if get(st.minute, p^) then
           begin
             inc(pos);
             inc(p);
           end else
             goto error;
        2: case p^ of
             ':': if sep in [yes, perhaps] then
                  begin
                    pos := 0;
                    Inc(p);
                    state := stSec;
                    sep := yes;
                  end else
                    goto error;
             ',', '.':
                begin
                  Inc(p);
                  state := stMs;
                end;
             '+':
               if havedate then
               begin
                 state := stGMTH;
                 pos := 0;
                 v := 0;
                 inc(p);
               end else
                 goto error;
             '-':
               if havedate then
               begin
                 state := stGMTH;
                 pos := 0;
                 v := 0;
                 inc(p);
                 inctz := True;
               end else
                 goto error;
             'Z', 'z':
                  if havedate then
                    state := stUTC else
                    goto error;
             '0'..'9':
                  if sep in [no, perhaps] then
                  begin
                    pos := 0;
                    state := stSec;
                  end else
                    goto error;
             #0:  state := stEnd;
           else
             goto error;
           end;
      end;
    stSec:
      case pos of
        0..1: if get(st.second, p^) then
              begin
                inc(pos);
                inc(p);
              end else
                goto error;
        2:    case p^ of
               ',', '.':
                  begin
                    Inc(p);
                    state := stMs;
                  end;
               '+':
                 if havedate then
                 begin
                   state := stGMTH;
                   pos := 0;
                   v := 0;
                   inc(p);
                 end else
                   goto error;
               '-':
                 if havedate then
                 begin
                   state := stGMTH;
                   pos := 0;
                   v := 0;
                   inc(p);
                   inctz := True;
                 end else
                   goto error;
               'Z', 'z':
                    if havedate then
                      state := stUTC else
                      goto error;
               #0: state := stEnd;
              else
               goto error;
              end;
      end;
    stMs:
      case p^ of
        '0'..'9':
        begin
          st.ms := st.ms * 10 + ord(p^) - ord('0');
          inc(p);
        end;
        '+':
          if havedate then
          begin
            state := stGMTH;
            pos := 0;
            v := 0;
            inc(p);
          end else
            goto error;
        '-':
          if havedate then
          begin
            state := stGMTH;
            pos := 0;
            v := 0;
            inc(p);
            inctz := True;
          end else
            goto error;
        'Z', 'z':
             if havedate then
               state := stUTC else
               goto error;
        #0: state := stEnd;
      else
        goto error;
      end;
    stUTC: // = GMT 0
      begin
        havetz := True;
        inc(p);
        if p^ = #0 then
          Break else
          goto error;
      end;
    stGMTH:
      begin
        havetz := True;
        case pos of
          0..1: if get(v, p^) then
                begin
                  inc(p);
                  inc(pos);
                end else
                  goto error;
          2:
            begin
              st.bias := v * 60;
              case p^ of
                ':': if sep in [yes, perhaps] then
                     begin
                       state := stGMTM;
                       inc(p);
                       pos := 0;
                       v := 0;
                       sep := yes;
                     end else
                       goto error;
                '0'..'9':
                     begin
                       state := stGMTM;
                       pos := 1;
                       sep := no;
                       inc(p);
                       v := ord(p^) - ord('0');
                     end;
                #0: state := stGMTend;
              else
                goto error;
              end;

            end;
        end;
      end;
    stGMTM:
      case pos of
        0..1:  if get(v, p^) then
               begin
                 inc(p);
                 inc(pos);
               end else
                 goto error;
        2:  case p^ of
              #0:
                begin
                  state := stGMTend;
                  inc(st.Bias, v);
                end;
            else
              goto error;
            end;
      end;
    stGMTend:
      begin
        if not inctz then
          st.Bias := -st.bias;
        Break;
      end;
    stEnd:
    begin

      Break;
    end;
  end;

  if (st.hour >= 24) or (st.minute >= 60) or (st.second >= 60) or (st.ms >= 1000) or (st.week > 53)
    then goto error;

  if not havetz then
    st.bias := GetTimeBias;

  ms := st.ms + st.second * 1000 + (st.minute + st.bias) * 60000 + st.hour * 3600000;
  if havedate then
  begin
    DayTable := @MonthDays[IsLeapYear(st.year)];
    if st.month <> 0 then
    begin
      if not (st.month in [1..12]) or (DayTable^[st.month] < st.day) then
        goto error;

      for v := 1 to  st.month - 1 do
        Inc(ms, DayTable^[v] * 86400000);
    end;
    dec(st.year);
    ms := ms + (int64((st.year * 365) + (st.year div 4) - (st.year div 100) +
      (st.year div 400) + st.day + st.dayofyear - 719163) * 86400000);
  end;

 Result := True;
 Exit;
error:
  Result := False;
end;

function ISO8601DateToDelphiDateTime(const str: string; var dt: TDateTime): Boolean;
var
  ms: Int64;
begin
  ms := 0;
  Result := ISO8601DateToJavaDateTime(str, ms);
  if Result then
    dt := JavaToDelphiDateTime(ms)
end;

function DelphiDateTimeToISO8601Date(dt: TDateTime): String;
const
  FMT_DATE = '%.4d-%.2d-%.2d';
  FMT_TIME = 'T%.2d:%.2d:%.2d.%.3d';
  FMT_ZONE = '%s%.2d:%.2d';
var
  year, month, day, hour, min, sec, msec: Word;
  tzh: SmallInt;
  tzm: Word;
  sign: Char;
  bias: Integer;
begin
  try
    DecodeDate(dt, year, month, day);
    DecodeTime(dt, hour, min, sec, msec);
    bias := GetTimeBias;
    tzh := Abs(bias) div 60;
    tzm := Abs(bias) - tzh * 60;
    if Bias > 0 then
      sign := '-' else
      sign := '+';
    Result := Format(FMT_DATE + FMT_TIME + FMT_ZONE,
      [year, month, day, hour, min, sec, msec, sign, tzh, tzm]);
  except
    if dt = 0 then
      raise
    else
      DelphiDateTimeToISO8601Date(0);
  end;
end;

{ ENoSerializableClass }

constructor ENoSerializableClass.Create(AClass: TClass);
begin
  inherited CreateFmt('Class "%s" has no RTTI (Run-time Type Information).', [AClass.ClassName]);
end;

end.
