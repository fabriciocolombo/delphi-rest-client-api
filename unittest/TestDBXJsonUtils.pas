unit TestDBXJsonUtils;

interface

uses TestFramework;

type
  TTestDBXJsonUtils = class(TTestCase)
  published
    procedure EncodeToISO8601;
    procedure DecodeFromISO8601;
    procedure DecodeFromISO8601UsingTimeZoneSeparator;
    procedure DecodeFromISO8601WithNoSeparators;
    procedure DecodeFromISO8601WithTimeZone;
    procedure EncodeAndDecodeFromNow;
    procedure EncodeAndDecodeZero;
  end;

implementation

uses
  SysUtils, DateUtils, DbxJsonUtils, DbxTest, RestJsonDateTimeUtils;

{ TTestDBXJsonUtils }

procedure TTestDBXJsonUtils.DecodeFromISO8601;
var
  vExpectedDateTime,
  vDateTime: TDateTime;
begin
  vExpectedDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 15);
  CheckTrue(ISO8601DateToDelphiDateTime('2013-07-27T22:58:56.015-0300', vDateTime));
  CheckEquals(DateTimeToStr(vExpectedDateTime), DateTimeToStr(vDateTime));
end;

procedure TTestDBXJsonUtils.DecodeFromISO8601UsingTimeZoneSeparator;
var
  vExpectedDateTime,
  vDateTime: TDateTime;
begin
  vExpectedDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 150);
  CheckTrue(ISO8601DateToDelphiDateTime('2013-07-27T22:58:56.150-03:00', vDateTime));

  CheckEquals(FormatDateTime('dd-mm-yyyy hh:nn:ss:zzz', vExpectedDateTime),
              FormatDateTime('dd-mm-yyyy hh:nn:ss:zzz', vDateTime));
  CheckEquals(vExpectedDateTime, vDateTime);
end;

procedure TTestDBXJsonUtils.DecodeFromISO8601WithNoSeparators;
var
  vExpectedDateTime,
  vDateTime: TDateTime;
begin
  vExpectedDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 15);
  CheckTrue(ISO8601DateToDelphiDateTime('20130727T225856.15-0300', vDateTime));

  CheckEquals(vExpectedDateTime, vDateTime);
end;

procedure TTestDBXJsonUtils.DecodeFromISO8601WithTimeZone;
var
  vExpectedDateTime,
  vDateTime: TDateTime;
begin
  vExpectedDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 15);
  CheckTrue(ISO8601DateToDelphiDateTime('2013-07-27T22:58:56.15-0400', vDateTime));

  CheckEquals(DateTimeToStr(IncHour(vExpectedDateTime)), DateTimeToStr(vDateTime));
end;

procedure TTestDBXJsonUtils.EncodeAndDecodeFromNow;
var
  vNow: TDateTime;
  vIsoString: string;
  vIsoDateTime: TDateTime;
begin
  vNow := EncodeDateTime(2014,07,22,21,15,42,205);

  vIsoString   := DelphiDateTimeToISO8601Date(vNow);

  CheckTrue(ISO8601DateToDelphiDateTime(vIsoString, vIsoDateTime));

  CheckEquals(FormatDateTime('dd-mm-yy hh:nn:ss:zzz', vNow),
              FormatDateTime('dd-mm-yy hh:nn:ss:zzz', vIsoDateTime)
              );

  CheckEquals('2014-07-22T21:15:42.205-03:00', vIsoString);
end;

procedure TTestDBXJsonUtils.EncodeAndDecodeZero;
const
  ISO_ZERO_HOUR = '1899-12-30T00:00:00.000-03:00';
var
  vNow: TDateTime;
  vIsoDateTime: TDateTime;
begin
  vNow := 0;

  CheckEquals(ISO_ZERO_HOUR, DelphiDateTimeToISO8601Date(vNow), 'Invalid ISO 8601 encoding');

  CheckTrue(ISO8601DateToDelphiDateTime(ISO_ZERO_HOUR, vIsoDateTime));

  CheckEquals(FormatDateTime('dd-mm-yy hh:nn:ss:zzz', vNow),
              FormatDateTime('dd-mm-yy hh:nn:ss:zzz', vIsoDateTime),
              'Invalid ISO 8601 decoding');
end;

procedure TTestDBXJsonUtils.EncodeToISO8601;
var
  vDateTime: TDateTime;
begin
  vDateTime := EncodeDateTime(2013, 07, 27, 22, 58, 56, 15);

  CheckEquals('2013-07-27T22:58:56.015-03:00', DelphiDateTimeToISO8601Date(vDateTime));
end;

initialization
  RegisterTest(TTestDBXJsonUtils.Suite);

end.
