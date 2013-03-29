unit HttpConnectionFactory;

interface

{$I DelphiRest.inc}

uses HttpConnection;

type
  THttpConnectionFactory = class
  public
    class function NewConnection(AType: THttpConnectionType): IHttpConnection;
  end;

implementation

uses SysUtils,
    {$IFDEF USE_INDY}
    HttpConnectionIndy,
    {$ENDIF}
    {$IFDEF USE_WIN_HTTP}
    HttpConnectionWinHttp,
    {$ENDIF}
    Classes, TypInfo;
    
{ THttpConnectionFactory }

class function THttpConnectionFactory.NewConnection(AType: THttpConnectionType): IHttpConnection;
begin
  case AType of
    hctIndy:  {$IFDEF USE_INDY}
              Result := THttpConnectionIndy.Create;
              {$ELSE}
              raise Exception.Create('Indy not supported. If do you have an indy installation, enable USE_INDY compiler directive.');
              {$ENDIF}
    hctWinHttp: {$IFDEF USE_WIN_HTTP}
                Result := THttpConnectionWinHttp.Create;
                {$ELSE}
                raise Exception.Create('WinHTTP not supported. If do you run under windows, enable USE_WIN_HTTP compiler directive.');
                {$ENDIF}
  else
    raise Exception.CreateFmt('Connection Type "%s" is not supported.', [GetEnumName(TypeInfo(THttpConnectionType), Ord(AType))]);
  end;
end;

end.
