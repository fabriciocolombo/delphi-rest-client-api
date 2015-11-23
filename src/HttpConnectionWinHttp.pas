unit HttpConnectionWinHttp;

interface

uses HttpConnection, Classes, SysUtils, Variants, ActiveX, AxCtrls, WinHttp_TLB,
  ComObj;

type
  THttpConnectionWinHttp = class(TInterfacedObject, IHttpConnection)
  private
    FWinHttpRequest: IWinHttpRequest;
    FAcceptTypes: string;
    FAcceptedLanguages: string;
    FContentTypes: string;
    FHeaders: TStrings;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FProxyCredentials: TProxyCredentials;
    FLogin: String;
    FPassword: String;
    FVerifyCert: boolean;

    procedure Configure;

    procedure CopyResourceStreamToStream(AResponse: TStream);
  protected
    procedure DoRequest(sMethod, AUrl: string; AContent, AResponse: TStream);
  public
    OnConnectionLost: THTTPConnectionLostEvent;
    OnError: THTTPErrorEvent;

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
    function GetResponseHeader(const Name: string): string;


    function GetEnabledCompression: Boolean;
    procedure SetEnabledCompression(const Value: Boolean);

    function GetOnConnectionLost: THTTPConnectionLostEvent;
    procedure SetOnConnectionLost(AConnectionLostEvent: THTTPConnectionLostEvent);

    procedure SetVerifyCert(const Value: boolean);
    function GetVerifyCert: boolean;

    function GetOnError: THTTPErrorEvent;
    procedure SetOnError(AErrorEvent: THTTPErrorEvent);
    function ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
    function ConfigureProxyCredentials(AProxyCredentials: TProxyCredentials): IHttpConnection;
  end;

implementation

uses
  ProxyUtils;

const
  HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
  HTTPREQUEST_PROXYSETTING_PROXY = 2;
  HTTPREQUEST_SETCREDENTIALS_FOR_PROXY = 1;

{ THttpConnectionWinHttp }

procedure THttpConnectionWinHttp.Configure;
var
  i: Integer;
  ProxyServer: string;
begin
  if FAcceptTypes <> EmptyStr then
    FWinHttpRequest.SetRequestHeader('Accept', FAcceptTypes);

  if FAcceptedLanguages <> EmptyStr then
    FWinHttpRequest.SetRequestHeader('Accept-Language', FAcceptedLanguages);

  if FContentTypes <> EmptyStr then
    FWinHttpRequest.SetRequestHeader('Content-Type', FContentTypes);

  for i := 0 to FHeaders.Count-1 do
  begin
    FWinHttpRequest.SetRequestHeader(FHeaders.Names[i], FHeaders.ValueFromIndex[i]);
  end;

  FWinHttpRequest.SetTimeouts(0,
                              FConnectTimeout,
                              FSendTimeout,
                              FReceiveTimeout);

  if ProxyActive then
  begin
    ProxyServer := GetProxyServer;
    if ProxyServer <> '' then
    begin
      FWinHttpRequest.SetProxy(HTTPREQUEST_PROXYSETTING_PROXY, ProxyServer, GetProxyOverride);
      if assigned(FProxyCredentials) then
        if FProxyCredentials.Informed then
          FWinHttpRequest.SetCredentials(FProxyCredentials.UserName, FProxyCredentials.Password,
            HTTPREQUEST_SETCREDENTIALS_FOR_PROXY);
    end;
  end;
  if not FVerifyCert then
    FWinHttpRequest.Option[WinHttpRequestOption_SslErrorIgnoreFlags] := SslErrorFlag_Ignore_All;
end;

function THttpConnectionWinHttp.ConfigureProxyCredentials(AProxyCredentials: TProxyCredentials): IHttpConnection;
begin
  FProxyCredentials := AProxyCredentials;
  Result := Self;
end;

function THttpConnectionWinHttp.ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
begin
  FConnectTimeout := ATimeOut.ConnectTimeout;
  FReceiveTimeout := ATimeOut.ReceiveTimeout;
  FSendTimeout    := ATimeOut.SendTimeout;
  Result := Self;
end;

procedure THttpConnectionWinHttp.CopyResourceStreamToStream(AResponse: TStream);
var
  vStream: IStream;
  vOleStream: TOleStream;
begin
  vStream := IUnknown(FWinHttpRequest.ResponseStream) as IStream;

  vOleStream := TOleStream.Create(vStream);
  try
    vOleStream.Position := 0;

    AResponse.CopyFrom(vOleStream, vOleStream.Size);
  finally
    vOleStream.Free;
  end;
end;

constructor THttpConnectionWinHttp.Create;
begin
  FHeaders := TStringList.Create;
  FLogin:='';
  FPassword:='';
  FVerifyCert := True;
end;

destructor THttpConnectionWinHttp.Destroy;
begin
  FHeaders.Free;
  FWinHttpRequest := nil;
  inherited;
end;

procedure THttpConnectionWinHttp.DoRequest(sMethod, AUrl: string; AContent,
  AResponse: TStream);
var
  vAdapter: IStream;
  retryMode: THTTPRetryMode;
begin
  FWinHttpRequest := CoWinHttpRequest.Create;
  FWinHttpRequest.Open(sMethod, AUrl, false);

  Configure;

  vAdapter := nil;
  if assigned(AContent) and (AContent.Size>0) then
    vAdapter := TStreamAdapter.Create(AContent, soReference);

  try
    if assigned(vAdapter) then
      FWinHttpRequest.Send(vAdapter)
    else
      FWinHttpRequest.Send(EmptyParam);
    if assigned(AResponse) then
      CopyResourceStreamToStream(AResponse);
  except
    on E: EOleException do
    begin
      case E.ErrorCode of
        -2147012858: // WININET_E_SEC_CERT_CN_INVALID
          raise EHTTPVerifyCertError.Create('The host name in the certificate is invalid or does not match');
        -2147012859: // WININET_E_SEC_CERT_DATE_INVALID
          raise EHTTPVerifyCertError.Create('The date in the certificate is invalid or has expired');
        -2147012865, // WININET_E_CONNECTION_RESET
        -2147012866, // WININET_E_CONNECTION_ABORTED
        -2147012867: // WININET_E_CANNOT_CONNECT
        begin
          retryMode := hrmRaise;
          if assigned(OnConnectionLost) then
            OnConnectionLost(e, retryMode);
          if retryMode = hrmRaise then
            raise
          else if retryMode = hrmRetry then
            DoRequest(sMethod, AUrl, AContent, AResponse);
        end
        else
          raise;
      end;
    end;
  end;
end;

procedure THttpConnectionWinHttp.Get(AUrl: string; AResponse: TStream);
begin
  DoRequest('GET', AUrl, nil, AResponse);
end;

procedure THttpConnectionWinHttp.Patch(AUrl: string; AContent,
  AResponse: TStream);
begin
  DoRequest('PATCH', AUrl, AContent, AResponse);
end;

procedure THttpConnectionWinHttp.Post(AUrl: string; AContent, AResponse: TStream);
begin
  DoRequest('POST', AUrl, AContent, AResponse);
end;

procedure THttpConnectionWinHttp.Put(AUrl: string; AContent,AResponse: TStream);
begin
  DoRequest('PUT', AUrl, AContent, AResponse);
end;

procedure THttpConnectionWinHttp.Delete(AUrl: string; AContent, AResponse: TStream);
begin
  DoRequest('DELETE', AUrl, AContent, AResponse);
end;

function THttpConnectionWinHttp.GetEnabledCompression: Boolean;
begin
  Result := False;
end;

function THttpConnectionWinHttp.GetOnConnectionLost: THTTPConnectionLostEvent;
begin
  result := OnConnectionLost;
end;

function THttpConnectionWinHttp.GetOnError: THTTPErrorEvent;
begin
  result := OnError;
end;

function THttpConnectionWinHttp.GetResponseCode: Integer;
begin
  Result := FWinHttpRequest.Status;
end;

function THttpConnectionWinHttp.GetVerifyCert: boolean;
begin
  result := FVerifyCert;
end;

function THttpConnectionWinHttp.GetResponseHeader(const Name: string): string;
begin
  Result := FWinHttpRequest.GetResponseHeader(Name)
end;

function THttpConnectionWinHttp.SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
begin
  FAcceptedLanguages := AAcceptedLanguages;

  Result := Self;
end;

function THttpConnectionWinHttp.SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
begin
  FAcceptTypes := AAcceptTypes;

  Result := Self;
end;

function THttpConnectionWinHttp.SetContentTypes(AContentTypes: string): IHttpConnection;
begin
  FContentTypes := AContentTypes;

  Result := Self;
end;

procedure THttpConnectionWinHttp.SetEnabledCompression(const Value: Boolean);
begin
  //Nothing to do
end;

function THttpConnectionWinHttp.SetHeaders(AHeaders: TStrings): IHttpConnection;
begin
  FHeaders.Assign(AHeaders);

  Result := Self;
end;

procedure THttpConnectionWinHttp.SetOnConnectionLost(
  AConnectionLostEvent: THTTPConnectionLostEvent);
begin
  OnConnectionLost := AConnectionLostEvent;
end;

procedure THttpConnectionWinHttp.SetOnError(AErrorEvent: THTTPErrorEvent);
begin
  OnError := AErrorEvent;
end;

procedure THttpConnectionWinHttp.SetVerifyCert(const Value: boolean);
begin
  FVerifyCert := Value;
end;

end.
