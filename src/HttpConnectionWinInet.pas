//Thanks to Tijmen van Gulik
unit HttpConnectionWinInet;

interface

{$I DelphiRest.inc}

uses HttpConnection, Classes, SysUtils, Variants, ActiveX, AxCtrls, Wcrypt2;

{$IFNDEF DELPHI_XE2_UP}
const
  INTERNET_OPTION_FROM_CACHE_TIMEOUT          = 63;
  {$EXTERNALSYM INTERNET_OPTION_FROM_CACHE_TIMEOUT}
  INTERNET_OPTION_BYPASS_EDITED_ENTRY         = 64;
  {$EXTERNALSYM INTERNET_OPTION_BYPASS_EDITED_ENTRY}
  INTERNET_OPTION_HTTP_DECODING               = 65;
  {$EXTERNALSYM INTERNET_OPTION_HTTP_DECODING}
  INTERNET_OPTION_DIAGNOSTIC_SOCKET_INFO      = 67;
  {$EXTERNALSYM INTERNET_OPTION_DIAGNOSTIC_SOCKET_INFO}
  INTERNET_OPTION_CODEPAGE                    = 68;
  {$EXTERNALSYM INTERNET_OPTION_CODEPAGE}
  INTERNET_OPTION_CACHE_TIMESTAMPS            = 69;
  {$EXTERNALSYM INTERNET_OPTION_CACHE_TIMESTAMPS}
  INTERNET_OPTION_DISABLE_AUTODIAL            = 70;
  {$EXTERNALSYM INTERNET_OPTION_DISABLE_AUTODIAL}
  INTERNET_OPTION_MAX_CONNS_PER_SERVER        = 73;
  {$EXTERNALSYM INTERNET_OPTION_MAX_CONNS_PER_SERVER}
  INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER    = 74;
  {$EXTERNALSYM INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER}
  INTERNET_OPTION_PER_CONNECTION_OPTION       = 75;
  {$EXTERNALSYM INTERNET_OPTION_PER_CONNECTION_OPTION}
  INTERNET_OPTION_DIGEST_AUTH_UNLOAD          = 76;
  {$EXTERNALSYM INTERNET_OPTION_DIGEST_AUTH_UNLOAD}
  INTERNET_OPTION_IGNORE_OFFLINE              = 77;
  {$EXTERNALSYM INTERNET_OPTION_IGNORE_OFFLINE}
  INTERNET_OPTION_IDENTITY                    = 78;
  {$EXTERNALSYM INTERNET_OPTION_IDENTITY}
  INTERNET_OPTION_REMOVE_IDENTITY             = 79;
  {$EXTERNALSYM INTERNET_OPTION_REMOVE_IDENTITY}
  INTERNET_OPTION_ALTER_IDENTITY              = 80;
  {$EXTERNALSYM INTERNET_OPTION_ALTER_IDENTITY}
  INTERNET_OPTION_SUPPRESS_BEHAVIOR           = 81;
  {$EXTERNALSYM INTERNET_OPTION_SUPPRESS_BEHAVIOR}
  INTERNET_OPTION_AUTODIAL_MODE               = 82;
  {$EXTERNALSYM INTERNET_OPTION_AUTODIAL_MODE}
  INTERNET_OPTION_AUTODIAL_CONNECTION         = 83;
  {$EXTERNALSYM INTERNET_OPTION_AUTODIAL_CONNECTION}
  INTERNET_OPTION_CLIENT_CERT_CONTEXT         = 84;
  {$EXTERNALSYM INTERNET_OPTION_CLIENT_CERT_CONTEXT}
  INTERNET_OPTION_AUTH_FLAGS                  = 85;
  {$EXTERNALSYM INTERNET_OPTION_AUTH_FLAGS}
  INTERNET_OPTION_COOKIES_3RD_PARTY           = 86;
  {$EXTERNALSYM INTERNET_OPTION_COOKIES_3RD_PARTY}
  INTERNET_OPTION_DISABLE_PASSPORT_AUTH       = 87;
  {$EXTERNALSYM INTERNET_OPTION_DISABLE_PASSPORT_AUTH}
  INTERNET_OPTION_SEND_UTF8_SERVERNAME_TO_PROXY = 88;
  {$EXTERNALSYM INTERNET_OPTION_SEND_UTF8_SERVERNAME_TO_PROXY}
  INTERNET_OPTION_EXEMPT_CONNECTION_LIMIT     = 89;
  {$EXTERNALSYM INTERNET_OPTION_EXEMPT_CONNECTION_LIMIT}
  INTERNET_OPTION_ENABLE_PASSPORT_AUTH        = 90;
  {$EXTERNALSYM INTERNET_OPTION_ENABLE_PASSPORT_AUTH}
  INTERNET_OPTION_HIBERNATE_INACTIVE_WORKER_THREADS = 91;
  {$EXTERNALSYM INTERNET_OPTION_HIBERNATE_INACTIVE_WORKER_THREADS}
  INTERNET_OPTION_ACTIVATE_WORKER_THREADS     = 92;
  {$EXTERNALSYM INTERNET_OPTION_ACTIVATE_WORKER_THREADS}
  INTERNET_OPTION_RESTORE_WORKER_THREAD_DEFAULTS = 93;
  {$EXTERNALSYM INTERNET_OPTION_RESTORE_WORKER_THREAD_DEFAULTS}
  INTERNET_OPTION_SOCKET_SEND_BUFFER_LENGTH   = 94;
  {$EXTERNALSYM INTERNET_OPTION_SOCKET_SEND_BUFFER_LENGTH}
  INTERNET_OPTION_PROXY_SETTINGS_CHANGED      = 95;
  {$EXTERNALSYM INTERNET_OPTION_PROXY_SETTINGS_CHANGED}

  INTERNET_OPTION_DATAFILE_EXT                = 96;
  {$EXTERNALSYM INTERNET_OPTION_DATAFILE_EXT}

  INTERNET_OPTION_CODEPAGE_PATH               = 100;
  {$EXTERNALSYM INTERNET_OPTION_CODEPAGE_PATH}
  INTERNET_OPTION_CODEPAGE_EXTRA              = 101;
  {$EXTERNALSYM INTERNET_OPTION_CODEPAGE_EXTRA}
  INTERNET_OPTION_IDN                         = 102;
  {$EXTERNALSYM INTERNET_OPTION_IDN}

  INTERNET_OPTION_MAX_CONNS_PER_PROXY         = 103;
  {$EXTERNALSYM INTERNET_OPTION_MAX_CONNS_PER_PROXY}
  INTERNET_OPTION_SUPPRESS_SERVER_AUTH        = 104;
  {$EXTERNALSYM INTERNET_OPTION_SUPPRESS_SERVER_AUTH}
  INTERNET_OPTION_SERVER_CERT_CHAIN_CONTEXT   = 105;
  {$EXTERNALSYM INTERNET_OPTION_SERVER_CERT_CHAIN_CONTEXT}
{$ENDIF}

type
  THttpConnectionWinInet = class(TInterfacedObject, IHttpConnection)
  private
    FAcceptTypes: string;
    FAcceptedLanguages: string;
    FContentTypes: string;
    FHeaders: TStrings;
    FBasicAuthentication_UserName : string;
    FBasicAuthentication_Password : string;
    FCertificateContext : PCERT_CONTEXT;
    FCertificateCheckAuthority: boolean;

    FCertificateIgnoreRevocation: boolean;

    FCertificateCheckHostName: boolean;
    FCertificateCheckDate: Boolean;
    FResponseCode : Integer;
    FResponseErrorStatusText : string;
    FRaiseExceptionOnError : Boolean;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FProxyCredentials: TProxyCredentials;
  protected
    procedure DoRequest(sMethod,AUrl: string; AContent: TStream; AResponse: TStream);virtual;
  public
    OnConnectionLost: THTTPConnectionLostEvent;

    constructor Create(ARaiseExceptionOnError: Boolean = false);
    destructor Destroy; override;

    function SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
    function SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
    function SetContentTypes(AContentTypes: string): IHttpConnection;
    function SetHeaders(AHeaders: TStrings): IHttpConnection;

    property Headers: TStrings read FHeaders;

    procedure Get(AUrl: string; AResponse: TStream);
    procedure Post(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Put(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Patch(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Delete(AUrl: string; AContent: TStream; AResponse: TStream);

    function GetResponseCode: Integer;
    function GetResponseHeader(const Header: string): string;

    function GetEnabledCompression: Boolean;
    procedure SetEnabledCompression(const Value: Boolean);

    function GetOnConnectionLost: THTTPConnectionLostEvent;
    procedure SetOnConnectionLost(AConnectionLostEvent: THTTPConnectionLostEvent);

    function ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
    function ConfigureProxyCredentials(AProxyCredentials: TProxyCredentials): IHttpConnection;

    procedure SetVerifyCert(const Value: boolean);
    function GetVerifyCert: boolean;

    procedure SetAsync(const Value: Boolean);
    procedure CancelRequest;

    property ResponseErrorStatusText : string read FResponseErrorStatusText write FResponseErrorStatusText;
    property CertificateContext: PCERT_CONTEXT read FCertificateContext write FCertificateContext;
//  published
    property BasicAuthentication_UserName : string read FBasicAuthentication_UserName write FBasicAuthentication_UserName;
    property BasicAuthentication_Password : string read FBasicAuthentication_Password write FBasicAuthentication_Password;
    property CertificateCheckDate : Boolean read FCertificateCheckDate write FCertificateCheckDate default true;
    property CertificateCheckHostName : boolean read FCertificateCheckHostName write FCertificateCheckHostName  default true;
    property CertificateCheckAuthority : boolean read FCertificateCheckAuthority  write FCertificateCheckAuthority  default true;
    property CertificateIgnoreRevocation : boolean read FCertificateIgnoreRevocation write FCertificateIgnoreRevocation  default true;
    property RaiseExceptionOnError : Boolean read FRaiseExceptionOnError write FRaiseExceptionOnError  default true;
  end;

type
  EInetException = class(Exception)
  private
    iFErrorCode: Integer;
  public
    constructor Create; overload;
    constructor Create(const AErrorMessage: string; const AErrorCode: Integer = -1); overload;
    property ErrorCode: Integer read iFErrorCode write iFErrorCode;
  end;


implementation

uses WinInet,Windows;

{ THttpConnectionWinInet }

const
  BUFSIZE = 4096;

constructor EInetException.Create;
var
  vErrorMessage: string;
begin
  iFErrorCode := GetLastError;

  case iFErrorCode of
    ERROR_INTERNET_TIMEOUT: vErrorMessage := 'The request has timed out.';
  else
    vErrorMessage := SysErrorMessage(iFErrorCode);
  end;
  Create(vErrorMessage, iFErrorCode);
end;

constructor EInetException.Create(const AErrorMessage: string; const AErrorCode: Integer);
begin
  iFErrorCode := AErrorCode;
  inherited CreateFmt('%s (%d)', [AErrorMessage, iFErrorCode]);
end;

procedure THttpConnectionWinInet.CancelRequest;
begin
end;

function THttpConnectionWinInet.ConfigureProxyCredentials(
  AProxyCredentials: TProxyCredentials): IHttpConnection;
begin
  FProxyCredentials := AProxyCredentials;
  Result := Self;
end;

function THttpConnectionWinInet.ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
begin
  FConnectTimeout := ATimeOut.ConnectTimeout;
  FReceiveTimeout := ATimeOut.ReceiveTimeout;
  FSendTimeout    := ATimeOut.SendTimeout;
  Result := Self;
end;

constructor THttpConnectionWinInet.Create(ARaiseExceptionOnError: Boolean = False);
begin
  FHeaders := TStringList.Create;
  FCertificateCheckAuthority:=True;
  FCertificateIgnoreRevocation:=True;
  FCertificateCheckHostName:=True;
  FCertificateCheckDate:=True;
  FRaiseExceptionOnError:=ARaiseExceptionOnError;
end;

procedure THttpConnectionWinInet.Delete(AUrl: string; AContent, AResponse: TStream);
begin
  DoRequest('DELETE', AUrl, AContent, AResponse);
end;

destructor THttpConnectionWinInet.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

procedure THttpConnectionWinInet.Get(AUrl: string; AResponse: TStream);
begin
  DoRequest('GET', AUrl, nil,AResponse);
end;

function THttpConnectionWinInet.GetEnabledCompression: Boolean;
begin
  Result := False;
end;

function THttpConnectionWinInet.GetOnConnectionLost: THTTPConnectionLostEvent;
begin
  result := OnConnectionLost;
end;

function THttpConnectionWinInet.GetResponseCode: Integer;
begin
  Result := FResponseCode;
end;

function THttpConnectionWinInet.GetVerifyCert: boolean;
begin
  result := FCertificateCheckDate and FCertificateCheckAuthority and
    FCertificateCheckHostName;
end;

function THttpConnectionWinInet.GetResponseHeader(const Header: string): string;
begin
  raise ENotSupportedException.Create('');
end;

procedure THttpConnectionWinInet.Patch(AUrl: string; AContent,
  AResponse: TStream);
begin
  DoRequest('PATCH', AUrl, AContent,AResponse);
end;

procedure THttpConnectionWinInet.Post(AUrl: string; AContent, AResponse: TStream);
begin
  DoRequest('POST', AUrl, AContent,AResponse);
end;

procedure THttpConnectionWinInet.Put(AUrl: string; AContent,AResponse: TStream);
begin
  DoRequest('PUT', AUrl, AContent,AResponse);
end;

function THttpConnectionWinInet.SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
begin
  FAcceptedLanguages := AAcceptedLanguages;
  
  Result := Self;
end;

function THttpConnectionWinInet.SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
begin
  FAcceptTypes := AAcceptTypes;
  
  Result := Self;
end;

procedure THttpConnectionWinInet.SetAsync(const Value: Boolean);
begin
  raise ENotImplemented.Create('Async requests not implemented for WinInet.');
end;

function THttpConnectionWinInet.SetContentTypes(AContentTypes: string): IHttpConnection;
begin
  FContentTypes := AContentTypes;

  Result := Self;
end;

procedure THttpConnectionWinInet.SetEnabledCompression(const Value: Boolean);
begin
  //Nothing to do
end;

function THttpConnectionWinInet.SetHeaders(AHeaders: TStrings): IHttpConnection;
begin
  FHeaders.Assign(AHeaders);

  Result := Self;
end;

procedure THttpConnectionWinInet.SetOnConnectionLost(
  AConnectionLostEvent: THTTPConnectionLostEvent);
begin
  OnConnectionLost := AConnectionLostEvent;
end;

procedure THttpConnectionWinInet.SetVerifyCert(const Value: boolean);
begin
  FCertificateCheckDate := Value;
  FCertificateCheckAuthority := Value;
  FCertificateCheckHostName := Value;
end;

procedure THttpConnectionWinInet.DoRequest(sMethod, AUrl: string; AContent,
  AResponse: TStream);
var
  iNetworkHandle,
  iConnectionHandle,
  iRequestHandle: HINTERNET;
  arBuf: array[0..BUFSIZE-1] of Char;
  iBytesRead, iFlags: DWord;
  iStatus, iStatusIndex, iStatusLength: DWORD;
  sStatusText: String;
  pURIC: URL_COMPONENTS;
  iRetry : Integer;
  i: Integer;
  AData: AnsiString;
  SecurityFlags : DWord;
  FlagsLen : DWord;
  retryMode: THTTPRetryMode;

   procedure SetRequestHeader(sName , sValue : string);
   var sHeader : string;
   begin
     sHeader:=sName+': '+sValue;
     HttpAddRequestHeaders(iRequestHandle, PChar(sHeader), Length(sHeader), HTTP_ADDREQ_FLAG_ADD);
   end;

begin
  FResponseCode:=0;
  SecurityFlags:=0;
  FillChar(pURIC, SizeOf(URL_COMPONENTS), 0);
  iFlags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_RAW_DATA;
  if Assigned(AContent) and (AContent.Size>0) then
  begin
    SetLength(AData,AContent.Size);
    AContent.Position:=0;
    AContent.ReadBuffer(AData[1], AContent.Size);
  end;
  with pURIC do begin
    dwStructSize:= SizeOf(pURIC);                     // size of this structure. Used in version check
    lpszScheme:= nil;                                 // pointer to scheme name
    dwSchemeLength:= INTERNET_MAX_SCHEME_LENGTH;      // length of scheme name
    lpszHostName:= nil;                               // pointer to host name
    dwHostNameLength:= INTERNET_MAX_HOST_NAME_LENGTH; // length of host name
    lpszUserName:= nil;                               // pointer to user name
    dwUserNameLength:= INTERNET_MAX_USER_NAME_LENGTH; // length of user name
    lpszPassword:= nil;                               // pointer to password
    dwPasswordLength:= INTERNET_MAX_PASSWORD_LENGTH;  // length of password
    lpszUrlPath:= nil;                                // pointer to URL-path
    dwUrlPathLength:= INTERNET_MAX_PATH_LENGTH;       // length of URL-path
    lpszExtraInfo:= nil;                              // pointer to extra information (e.g. ?foo or #foo)
    dwExtraInfoLength:= INTERNET_MAX_PATH_LENGTH;     // length of extra information
  end;
  if InternetCrackUrl(PChar(AUrl), Length(AUrl), 0, pURIC) then begin
    iNetworkHandle := InternetOpen(PChar('Delphi REST client'),
       INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY, nil, nil, 0); //INTERNET_FLAG_RAW_DATA
    try
      if Assigned(iNetworkHandle) then begin
        iConnectionHandle := InternetConnect(iNetworkHandle,
          PChar(Copy(pURIC.lpszHostName, 1, pURIC.dwHostNameLength)),
          pURIC.nPort, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
        try
          if Assigned(iConnectionHandle) then begin
            if pURIC.nScheme=INTERNET_SCHEME_HTTPS then
            begin
              iFlags:=iFlags or INTERNET_FLAG_SECURE;


              if not CertificateCheckDate then
                SecurityFlags:=SecurityFlags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
              if not CertificateCheckHostName then
                SecurityFlags:=SecurityFlags or INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
              if not CertificateCheckAuthority then
                SecurityFlags:=SecurityFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA ;
              if not CertificateIgnoreRevocation then
                SecurityFlags:=SecurityFlags or SECURITY_FLAG_IGNORE_REVOCATION;

              iFlags:=iFlags or SecurityFlags;
            end;

            iRequestHandle := HTTPOpenRequest(iConnectionHandle, PChar(sMethod), PChar(pURIC.lpszUrlPath),
               PChar('HTTP/1.1'), nil, nil, iFlags, 0);
            try

              if Assigned(iRequestHandle) then
              begin

                if CertificateContext<>nil then
                begin
                  if (InternetSetOption(iRequestHandle, INTERNET_OPTION_CLIENT_CERT_CONTEXT, CertificateContext, Sizeof(CERT_CONTEXT)) = False) then
                    raise EInetException.Create(Format('Internal error when installing the certificate using InternetSetOption: %s', [SysErrorMessage(GetLastError)]));
                end;

                InternetSetOption(iRequestHandle, INTERNET_OPTION_RECEIVE_TIMEOUT, @FReceiveTimeout, SizeOf(FReceiveTimeout));
                InternetSetOption(iRequestHandle, INTERNET_OPTION_SEND_TIMEOUT, @FSendTimeout, SizeOf(FSendTimeout));
                InternetSetOption(iRequestHandle, INTERNET_OPTION_CONNECT_TIMEOUT, @FConnectTimeout, SizeOf(FConnectTimeout));

                if assigned(FProxyCredentials) then
                  if FProxyCredentials.Informed then
                  begin
                    InternetSetOption(iRequestHandle, INTERNET_OPTION_PROXY_USERNAME, PChar(FProxyCredentials.UserName),
                      Length(FProxyCredentials.UserName));
                    InternetSetOption(iRequestHandle, INTERNET_OPTION_PROXY_PASSWORD, PChar(FProxyCredentials.Password),
                      Length(FProxyCredentials.Password));
                  end;

                if FAcceptTypes <> EmptyStr then
                  SetRequestHeader('Accept', FAcceptTypes);

                if FAcceptedLanguages <> EmptyStr then
                  SetRequestHeader('Accept-Language', FAcceptedLanguages);

                if FContentTypes <> EmptyStr then
                  SetRequestHeader('Content-Type', FContentTypes);

                for i := 0 to FHeaders.Count-1 do
                begin
                  SetRequestHeader(FHeaders.Names[i], FHeaders.ValueFromIndex[i]);
                end;

                InternetSetOption(iRequestHandle, INTERNET_OPTION_SECURITY_FLAGS, Pointer(@SecurityFlags), SizeOf(SecurityFlags));

                if BasicAuthentication_UserName<>'' then
                  InternetSetOption(iRequestHandle, INTERNET_OPTION_USERNAME,
                                    PChar(BasicAuthentication_UserName), Length(BasicAuthentication_UserName));
                if BasicAuthentication_Password<>'' then
                  InternetSetOption(iRequestHandle, INTERNET_OPTION_PASSWORD,
                                  PChar(BasicAuthentication_Password), Length(BasicAuthentication_Password));


                iRetry:=0;
                repeat

                  if HTTPSendRequest(iRequestHandle, nil, 0, PAnsiString(AData), Length(AData)) then begin
                      iStatusIndex := 0;
                      iStatusLength := SizeOf(iStatus);
                      if HttpQueryInfo(iRequestHandle, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @iStatus, iStatusLength, iStatusIndex) then begin
                        FResponseCode:=iStatus;
                        if  (iStatus >= 300) then
                        begin
                          SetLength(sStatusText, BUFSIZE);
                          iStatusIndex := 0;
                          iStatusLength := Length(sStatusText) ;
                          if HttpQueryInfo(iRequestHandle, HTTP_QUERY_STATUS_TEXT, @sStatusText[1], iStatusLength, iStatusIndex) then begin
                            SetLength(sStatusText, iStatusLength div SizeOf(Char));
                            FResponseErrorStatusText:= sStatusText;
                            if FRaiseExceptionOnError then
                              raise EInetException.Create(Format('Can not send REST message %s, error: %s', [AUrl, sStatusText]), iStatus);
                          end;
                        end;
                      end;
                      if Assigned(AResponse) then
                      begin
                        while InternetReadFile(iRequestHandle, @arBuf, BUFSIZE, iBytesRead) and (iBytesRead > 0) do begin
                          AResponse.Write(arBuf,iBytesRead);
                        end;
                        iRetry:=0;
                      end;
                  end else
                  begin
                    if (iRetry=0) and (GetLastError= ERROR_INTERNET_INVALID_CA) then
                    begin
                      Inc(iRetry);
                      FlagsLen := SizeOf(iFlags);
                      InternetQueryOption(iRequestHandle, INTERNET_OPTION_SECURITY_FLAGS, Pointer(@iFlags), FlagsLen);
                      iFlags := iFlags or SecurityFlags;
                      InternetSetOption(iRequestHandle, INTERNET_OPTION_SECURITY_FLAGS, Pointer(@iFlags), FlagsLen);
                    end
                    else
                    begin
                      case GetLastError of
                        ERROR_INTERNET_SEC_CERT_CN_INVALID:
                          raise EHTTPVerifyCertError.Create('SSL certificate common name (host name field) is incorrect.');
                        ERROR_INTERNET_SEC_CERT_DATE_INVALID:
                          raise EHTTPVerifyCertError.Create('SSL certificate date that was received from the server is bad. The certificate is expired.');
                        ERROR_INTERNET_CANNOT_CONNECT,
                        ERROR_INTERNET_CONNECTION_ABORTED,
                        ERROR_INTERNET_CONNECTION_RESET:
                        begin
                          retryMode := hrmRaise;
                          if assigned(OnConnectionLost) then
                            OnConnectionLost(nil, retryMode);
                          if retryMode = hrmRaise then
                            raise EInetException.Create(inttostr(GetLastError))
                          else if retryMode = hrmRetry then
                            DoRequest(sMethod, AUrl, AContent, AResponse);
                        end;
                        else
                          raise EInetException.Create(inttostr(GetLastError));
                      end;
                    end;
                  end;
                until (iRetry=0) or (iRetry>1);
              end else raise EInetException.Create;
            finally
              InternetCloseHandle(iRequestHandle);
            end;
          end else raise EInetException.Create;
        finally
          InternetCloseHandle(iConnectionHandle);
        end;
      end else raise EInetException.Create;
    finally
      InternetCloseHandle(iNetworkHandle);
    end;
  end else raise EInetException.Create;
end;

end.
