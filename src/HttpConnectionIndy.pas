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
    procedure Delete(AUrl: string; AContent: TStream);

    function GetResponseCode: Integer;

    function GetEnabledCompression: Boolean;
    procedure SetEnabledCompression(const Value: Boolean);

    function GetOnConnectionLost: THTTPConnectionLostEvent;
    procedure SetOnConnectionLost(AConnectionLostEvent: THTTPConnectionLostEvent);

    function GetOnError: THTTPErrorEvent;
    procedure SetOnError(AErrorEvent: THTTPErrorEvent);
    function ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
  end;

implementation

{ THttpConnectionIndy }

function THttpConnectionIndy.ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
begin
  FIdHttp.ConnectTimeout := ATimeOut.ConnectTimeout;
  FIdHttp.ReadTimeout := ATimeOut.ReceiveTimeout;
end;

constructor THttpConnectionIndy.Create;
begin
  FIdHttp := TIdHTTP.Create(nil);
  FIdHttp.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FIdHttp);
  FIdHttp.HandleRedirects := True;
  FIdHttp.Request.CustomHeaders.FoldLines := false;
end;

procedure THttpConnectionIndy.Delete(AUrl: string; AContent: TStream);
var
  retryMode: THTTPRetryMode;
begin
  try
    FIdHttp.Request.Source := AContent;
    FIdHttp.Delete(AUrl);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 404 then
        exit;
      retryMode := hrmRaise;
      if assigned(OnError) then
        OnError(e.Message, e.ErrorMessage, e.ErrorCode, retryMode);
      if retryMode = hrmRaise then
        raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode)
      else if retryMode = hrmRetry then
        Delete(AUrl, AContent);
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
        Delete(AUrl, AContent);
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
begin
  try
    FIdHttp.Get(AUrl, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 404 then
        exit;
      retryMode := hrmRaise;
      if assigned(OnError) then
        OnError(e.Message, e.ErrorMessage, e.ErrorCode, retryMode);
      if retryMode = hrmRaise then
        raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode)
      else if retryMode = hrmRetry then
        Get(AUrl, AResponse);
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

function THttpConnectionIndy.GetOnError: THTTPErrorEvent;
begin
  result := OnError;
end;

function THttpConnectionIndy.GetResponseCode: Integer;
begin
  Result := FIdHttp.ResponseCode;
end;

procedure THttpConnectionIndy.Patch(AUrl: string; AContent, AResponse: TStream);
var
  retryMode: THTTPRetryMode;
begin
  try
    FIdHttp.Patch(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 404 then
        exit;
      retryMode := hrmRaise;
      if assigned(OnError) then
        OnError(e.Message, e.ErrorMessage, e.ErrorCode, retryMode);
      if retryMode = hrmRaise then
        raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode)
      else if retryMode = hrmRetry then
        Patch(AUrl, AContent, AResponse);
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
begin
  try
    FIdHttp.Post(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 404 then
        exit;
      retryMode := hrmRaise;
      if assigned(OnError) then
        OnError(e.Message, e.ErrorMessage, e.ErrorCode, retryMode);
      if retryMode = hrmRaise then
        raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode)
      else if retryMode = hrmRetry then
        Post(AUrl, AContent, AResponse);
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
begin
  try
    FIdHttp.Put(AUrl, AContent, AResponse);
  except
    on E: EIdHTTPProtocolException do
    begin
      if E.ErrorCode = 404 then
        exit;
      retryMode := hrmRaise;
      if assigned(OnError) then
        OnError(e.Message, e.ErrorMessage, e.ErrorCode, retryMode);
      if retryMode = hrmRaise then
        raise EHTTPError.Create(e.Message, e.ErrorMessage, e.ErrorCode)
      else if retryMode = hrmRetry then
        Put(AUrl, AContent, AResponse);
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

procedure THttpConnectionIndy.SetOnConnectionLost(
  AConnectionLostEvent: THTTPConnectionLostEvent);
begin
  OnConnectionLost := AConnectionLostEvent;
end;

procedure THttpConnectionIndy.SetOnError(AErrorEvent: THTTPErrorEvent);
begin
  OnError := AErrorEvent;
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
