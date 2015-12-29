unit HttpConnectionIndy;

interface

uses IdHTTP, HttpConnection, Classes, RestUtils, IdCompressorZLib, SysUtils,
     IdSSLOpenSSL, IdStack;

type
  TIdHTTP = class(idHTTP.TIdHTTP)
  public
    procedure Delete(AURL: string);
    procedure Patch(AURL: string; ASource, AResponseContent: TStream);
  end;

  THttpConnectionIndy = class(TInterfacedObject, IHttpConnection)
  private
    FIdHttp: TIdHTTP;
    FEnabledCompression: Boolean;
    FVerifyCert: boolean;
    function IdSSLIOHandlerSocketOpenSSL1VerifyPeer(Certificate: TIdX509;
      AOk: Boolean; ADepth, AError: Integer): Boolean;
  public
    OnConnectionLost: THTTPConnectionLostEvent;

    constructor Create;
    destructor Destroy; override;

    function SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
    function SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
    function SetContentTypes(AContentTypes: string): IHttpConnection;
    function SetHeaders(AHeaders: TStrings): IHttpConnection;

    procedure Get(AUrl: string; AResponse: TStream);
    procedure Post(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Put(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Patch(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Delete(AUrl: string; AContent: TStream; AResponse: TStream);

    function GetResponseCode: Integer;
    function GetResponseHeader(const Header: string): string;

    function GetEnabledCompression: Boolean;
    procedure SetEnabledCompression(const Value: Boolean);

    procedure SetVerifyCert(const Value: boolean);
    function GetVerifyCert: boolean;

    procedure SetAsync(const Value: Boolean);
    procedure CancelRequest;

    function GetOnConnectionLost: THTTPConnectionLostEvent;
    procedure SetOnConnectionLost(AConnectionLostEvent: THTTPConnectionLostEvent);

    function ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
    function ConfigureProxyCredentials(AProxyCredentials: TProxyCredentials): IHttpConnection;
  end;

implementation

uses
  ProxyUtils;

{ THttpConnectionIndy }

procedure THttpConnectionIndy.CancelRequest;
begin
end;

function THttpConnectionIndy.ConfigureProxyCredentials(
  AProxyCredentials: TProxyCredentials): IHttpConnection;
begin
  if assigned(AProxyCredentials) then
    if AProxyCredentials.Informed and ProxyActive then
    begin
      FIdHttp.ProxyParams.BasicAuthentication := True;
      FIdHttp.ProxyParams.ProxyUsername := AProxyCredentials.UserName;
      FIdHttp.ProxyParams.ProxyPassword := AProxyCredentials.Password;
    end;
  Result := Self;
end;

function THttpConnectionIndy.ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
begin
  FIdHttp.ConnectTimeout := ATimeOut.ConnectTimeout;
  FIdHttp.ReadTimeout := ATimeOut.ReceiveTimeout;
  Result := Self;
end;

constructor THttpConnectionIndy.Create;
var
  ssl: TIdSSLIOHandlerSocketOpenSSL;
  ProxyServerIP: string;
begin
  FIdHttp := TIdHTTP.Create(nil);
  ssl := TIdSSLIOHandlerSocketOpenSSL.Create(FIdHttp);
  ssl.OnVerifyPeer := IdSSLIOHandlerSocketOpenSSL1VerifyPeer;
  ssl.SSLOptions.SSLVersions := [sslvTLSv1,sslvTLSv1_1,sslvTLSv1_2];
  FIdHttp.IOHandler := ssl;
  FIdHttp.HandleRedirects := True;
  FIdHttp.Request.CustomHeaders.FoldLines := false;

  if ProxyActive then
  begin
    ProxyServerIP := GetProxyServerIP;
    if ProxyServerIP <> '' then
    begin
      FIdHttp.ProxyParams.ProxyServer := ProxyServerIP;
      FIdHttp.ProxyParams.ProxyPort := GetProxyServerPort;
    end;
  end;
end;

procedure THttpConnectionIndy.Delete(AUrl: string; AContent, AResponse: TStream);
var
  retryMode: THTTPRetryMode;
  temp: TStringStream;
begin
  try
    FIdHttp.Request.Source := AContent;
    FIdHttp.Delete(AUrl);
  except
    on E: EIdHTTPProtocolException do
    begin
      if Length(E.ErrorMessage) > 0 then
      begin
        temp := TStringStream.Create(E.ErrorMessage);
        AResponse.CopyFrom(temp, temp.Size);
        temp.Free;
      end;
    end;
    on E: EIdSocketError do
    begin
      FIdHttp.Disconnect(false);
      retryMode := hrmRaise;
      if assigned(OnConnectionLost) then
        OnConnectionLost(e, retryMode);
      if retryMode = hrmRaise then
        raise
      else if retryMode = hrmRetry then
        Delete(AUrl, AContent, AResponse);
    end;
  end;
end;

destructor THttpConnectionIndy.Destroy;
begin
  FIdHttp.Free;
  inherited;
end;

procedure THttpConnectionIndy.Get(AUrl: string; AResponse: TStream);
var
  retryMode: THTTPRetryMode;
  temp: TStringStream;
begin
  try
    FIdHttp.Get(AUrl, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if Length(E.ErrorMessage) > 0 then
      begin
        temp := TStringStream.Create(E.ErrorMessage);
        AResponse.CopyFrom(temp, temp.Size);
        temp.Free;
      end;
    end;
    on E: EIdSocketError do
    begin
      FIdHttp.Disconnect(false);
      retryMode := hrmRaise;
      if assigned(OnConnectionLost) then
        OnConnectionLost(e, retryMode);
      if retryMode = hrmRaise then
        raise
      else if retryMode = hrmRetry then
        Get(AUrl, AResponse);
    end;
  end;
end;

function THttpConnectionIndy.GetEnabledCompression: Boolean;
begin
  Result := FEnabledCompression;
end;

function THttpConnectionIndy.GetOnConnectionLost: THTTPConnectionLostEvent;
begin
  result := OnConnectionLost;
end;

function THttpConnectionIndy.GetResponseCode: Integer;
begin
  Result := FIdHttp.ResponseCode;
end;

function THttpConnectionIndy.GetResponseHeader(const Header: string): string;
begin
  raise ENotSupportedException.Create('');
end;

function THttpConnectionIndy.GetVerifyCert: boolean;
begin
  result := FVerifyCert;
end;

function THttpConnectionIndy.IdSSLIOHandlerSocketOpenSSL1VerifyPeer(
  Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
begin
  result := AOk;
  if not FVerifyCert then
  begin
    result := True;
  end;
end;

procedure THttpConnectionIndy.Patch(AUrl: string; AContent, AResponse: TStream);
var
  retryMode: THTTPRetryMode;
  temp: TStringStream;
begin
  try
    FIdHttp.Patch(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if Length(E.ErrorMessage) > 0 then
      begin
        temp := TStringStream.Create(E.ErrorMessage);
        AResponse.CopyFrom(temp, temp.Size);
        temp.Free;
      end;
    end;
    on E: EIdSocketError do
    begin
      FIdHttp.Disconnect(false);
      retryMode := hrmRaise;
      if assigned(OnConnectionLost) then
        OnConnectionLost(e, retryMode);
      if retryMode = hrmRaise then
        raise
      else if retryMode = hrmRetry then
        Patch(AUrl, AContent, AResponse);
    end;
  end;
end;

procedure THttpConnectionIndy.Post(AUrl: string; AContent, AResponse: TStream);
var
  retryMode: THTTPRetryMode;
  temp: TStringStream;
begin
  try
    FIdHttp.Post(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if Length(E.ErrorMessage) > 0 then
      begin
        temp := TStringStream.Create(E.ErrorMessage);
        AResponse.CopyFrom(temp, temp.Size);
        temp.Free;
      end;
    end;
    on E: EIdSocketError do
    begin
      FIdHttp.Disconnect(false);
      retryMode := hrmRaise;
      if assigned(OnConnectionLost) then
        OnConnectionLost(e, retryMode);
      if retryMode = hrmRaise then
        raise
      else if retryMode = hrmRetry then
        Post(AUrl, AContent, AResponse);
    end;
  end;
end;

procedure THttpConnectionIndy.Put(AUrl: string; AContent, AResponse: TStream);
var
  retryMode: THTTPRetryMode;
  temp: TStringStream;
begin
  try
    FIdHttp.Put(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if Length(E.ErrorMessage) > 0 then
      begin
        temp := TStringStream.Create(E.ErrorMessage);
        AResponse.CopyFrom(temp, temp.Size);
        temp.Free;
      end;
    end;
    on E: EIdSocketError do
    begin
      FIdHttp.Disconnect(false);
      retryMode := hrmRaise;
      if assigned(OnConnectionLost) then
        OnConnectionLost(e, retryMode);
      if retryMode = hrmRaise then
        raise
      else if retryMode = hrmRetry then
        Put(AUrl, AContent, AResponse);
    end;
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

procedure THttpConnectionIndy.SetAsync(const Value: Boolean);
begin
  if Value then
    raise ENotImplemented.Create('Async requests not implemented for Indy.');
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
var
  i: Integer;
begin
  FIdHttp.Request.Authentication.Free;
  FIdHttp.Request.Authentication := nil;
  FIdHttp.Request.CustomHeaders.Clear;

  for i := 0 to AHeaders.Count-1 do
  begin
    FIdHttp.Request.CustomHeaders.AddValue(AHeaders.Names[i], AHeaders.ValueFromIndex[i]);
  end;

  Result := Self;
end;

procedure THttpConnectionIndy.SetOnConnectionLost(
  AConnectionLostEvent: THTTPConnectionLostEvent);
begin
  OnConnectionLost := AConnectionLostEvent;
end;

procedure THttpConnectionIndy.SetVerifyCert(const Value: boolean);
begin
  FVerifyCert := Value;
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

procedure TIdHTTP.Patch(AURL: string; ASource, AResponseContent: TStream);
begin
  DoRequest('PATCH', AURL, ASource, AResponseContent, []);
end;

end.
