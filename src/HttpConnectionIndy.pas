unit HttpConnectionIndy;

interface

uses IdHTTP, HttpConnection, Classes, RestUtils, IdCompressorZLib, SysUtils,
     IdSSLOpenSSL;

type
  TIdHTTP = class(idHTTP.TIdHTTP)
  public
    procedure Delete(AURL: string);
  end;

  THttpConnectionIndy = class(TInterfacedObject, IHttpConnection)
  private
    FIdHttp: TIdHTTP;
    FIdSSL: TIdSSLIOHandlerSocketOpenSSL;
    FEnabledCompression: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
    function SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
    function SetContentTypes(AContentTypes: string): IHttpConnection;
    function SetHeaders(AHeaders: TStrings): IHttpConnection;

    procedure Get(AUrl: string; AResponse: TStream);
    procedure Post(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Put(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Delete(AUrl: string; AContent: TStream);

    function GetResponseCode: Integer;

    function GetEnabledCompression: Boolean;
    procedure SetEnabledCompression(const Value: Boolean);
  end;

implementation

{ THttpConnectionIndy }

constructor THttpConnectionIndy.Create;
begin
  FIdSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FIdHttp := TIdHTTP.Create(nil);
  FIdHttp.IOHandler := FIdSSL;
  FIdHttp.HandleRedirects := True;
  FIdHttp.Request.CustomHeaders.FoldLines := false;
end;

procedure THttpConnectionIndy.Delete(AUrl: string; AContent: TStream);
begin
  FIdHttp.Request.Source := AContent;

  FIdHttp.Delete(AUrl);
end;

destructor THttpConnectionIndy.Destroy;
begin
  FIdHttp.Free;
  FIdSSL.Free;
  inherited;
end;

procedure THttpConnectionIndy.Get(AUrl: string; AResponse: TStream);
begin
  try
    FIdHttp.Get(AUrl, AResponse);
  except
    on E: EIdHTTPProtocolException do
      raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode);
  end;
end;

function THttpConnectionIndy.GetEnabledCompression: Boolean;
begin
  Result := FEnabledCompression;
end;

function THttpConnectionIndy.GetResponseCode: Integer;
begin
  Result := FIdHttp.ResponseCode;
end;

procedure THttpConnectionIndy.Post(AUrl: string; AContent, AResponse: TStream);
begin
  try
    FIdHttp.Post(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
      raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode);
  end;
end;

procedure THttpConnectionIndy.Put(AUrl: string; AContent, AResponse: TStream);
begin
  try
    FIdHttp.Put(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
      raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode);
  end;
end;

function THttpConnectionIndy.SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
begin
  FIdHttp.Request.AcceptLanguage := AAcceptedLanguages;
  Result := Self;
end;

function THttpConnectionIndy.SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
begin
  FIdHttp.Request.Accept := AAcceptTypes;
  Result := Self;
end;

function THttpConnectionIndy.SetContentTypes(AContentTypes: string): IHttpConnection;
begin
  FIdHttp.Request.ContentType := AContentTypes;
  Result := Self;
end;

procedure THttpConnectionIndy.SetEnabledCompression(const Value: Boolean);
begin
  if (FEnabledCompression <> Value) then
  begin
    FEnabledCompression := Value;

    if FEnabledCompression then
    begin
      {$IFDEF DELPHI_XE2}
        {$Message Warn 'TIdCompressorZLib does not work properly in Delphi XE2. Access violation occurs.'}
      {$ENDIF}
      FIdHttp.Compressor := TIdCompressorZLib.Create(FIdHttp);
    end
    else
    begin
      FIdHttp.Compressor.Free;
      FIdHttp.Compressor := nil;
    end;
  end;
end;

function THttpConnectionIndy.SetHeaders(AHeaders: TStrings): IHttpConnection;
begin
  FIdHttp.Request.CustomHeaders.Clear;
  FIdHttp.Request.CustomHeaders.AddStrings(AHeaders);
  Result := Self;
end;

{ TIdHTTP }

procedure TIdHTTP.Delete(AURL: string);
begin
  try
    DoRequest(Id_HTTPMethodDelete, AURL, Request.Source, nil, []);
  except
    on E: EIdHTTPProtocolException do
      raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode);
  end;
end;

end.
