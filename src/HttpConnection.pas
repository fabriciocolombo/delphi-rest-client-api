unit HttpConnection;

interface

uses Classes, SysUtils;

const
  TIMEOUT_CONNECT_DEFAULT = 60000;
  TIMEOUT_SEND_DEFAULT = 30000;
  TIMEOUT_RECEIVE_DEFAULT = 30000;

type
  THttpConnectionType = (hctUnknown, hctIndy, hctWinHttp, hctWinInet, hctCustom);

  EHTTPError = class(Exception)
    private
      FErrorCode: integer;
      FErrorMessage: string;
    public
      constructor Create(const AMsg, AErrorMessage: string; const AErrorCode: integer); overload; virtual;
      property ErrorMessage: string read FErrorMessage;
      property ErrorCode: integer read FErrorCode;
  end;

  THTTPRetryMode = (hrmRaise, hrmIgnore, hrmRetry);

  THTTPConnectionLostEvent = procedure(AException: Exception; var ARetryMode: THTTPRetryMode) of object;
  THTTPErrorEvent = procedure(const AMessage, AErrorMessage: string; AErrorCode: integer; var ARetryMode: THTTPRetryMode) of object;

  TProxyCredentials = class(TComponent)
  private
    FUserName: string;
    FPassword: string;
  public
    function Informed: Boolean;
  published
    property UserName: string read FUserName write FUsername;
    property Password: string read FPassword write FPassword;
  end;

  TTimeOut = class(TComponent)
  private
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
  public
    procedure AfterConstruction; override;

  published
    ///	<summary>
    ///	  Time-out value applied when establishing a communication socket with
    ///	  the target server, in milliseconds. The default value is 60,000 (60
    ///	  seconds).
    ///	</summary>
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default TIMEOUT_CONNECT_DEFAULT;

    ///	<summary>
    ///	  Time-out value applied when sending an individual packet of request
    ///	  data on the communication socket to the target server, in
    ///	  milliseconds. A large request sent to an HTTP server are normally be
    ///	  broken up into multiple packets; the send time-out applies to sending
    ///	  each packet individually. The default value is 30,000 (30 seconds).
    ///	</summary>
    ///	<remarks>
    ///	  Property ignored for Indy connection
    ///	</remarks>
    property SendTimeout: Integer read FSendTimeout write FSendTimeout default TIMEOUT_SEND_DEFAULT;

    ///	<summary>
    ///	  Time-out value applied when receiving a packet of response data from
    ///	  the target server, in milliseconds. Large responses are be broken up
    ///	  into multiple packets; the receive time-out applies to fetching each
    ///	  packet of data off the socket. The default value is 30,000 (30
    ///	  seconds).
    ///	</summary>
    property ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default TIMEOUT_RECEIVE_DEFAULT;
  end;

  IHttpConnection = interface
  ['{B9611100-5243-4874-A777-D91448517116}']
    function SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
    function SetContentTypes(AContentTypes: string): IHttpConnection;
    function SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
    function SetHeaders(AHeaders: TStrings): IHttpConnection;
    function ConfigureTimeout(const ATimeOut: TTimeOut): IHttpConnection;
    function ConfigureProxyCredentials(AProxyCredentials: TProxyCredentials): IHttpConnection;

    procedure Get(AUrl: string; AResponse: TStream);
    procedure Post(AUrl: string; AContent, AResponse: TStream);
    procedure Put(AUrl: string; AContent, AResponse: TStream);
    procedure Patch(AUrl: string; AContent, AResponse: TStream);
    procedure Delete(AUrl: string; AContent: TStream);

    function GetResponseCode: Integer;
    function GetResponseHeader(const Header: string): string;
    function GetEnabledCompression: Boolean;
    function GetVerifyCert: Boolean;

    procedure SetEnabledCompression(const Value: Boolean);
    procedure SetVerifyCert(const Value: boolean);

    property ResponseCode: Integer read GetResponseCode;
    property ResponseHeader[const Header: string]: string read GetResponseHeader;
    property EnabledCompression: Boolean read GetEnabledCompression write SetEnabledCompression;
    property VerifyCert: boolean read GetVerifyCert write SetVerifyCert;

    function GetOnConnectionLost: THTTPConnectionLostEvent;
    procedure SetOnConnectionLost(AConnectionLostEvent: THTTPConnectionLostEvent);
    property OnConnectionLost: THTTPConnectionLostEvent read GetOnConnectionLost write SetOnConnectionLost;

    function GetOnError: THTTPErrorEvent;
    procedure SetOnError(AConnectionLostEvent: THTTPErrorEvent);
    property OnError: THTTPErrorEvent read GetOnError write SetOnError;
  end;

implementation

{ THttpError }

constructor EHTTPError.Create(const AMsg, AErrorMessage: string; const AErrorCode: integer);
begin
  inherited Create(AMsg);
  FErrorMessage := AErrorMessage;
  FErrorCode := AErrorCode;
end;

{ TTimeOut }

procedure TTimeOut.AfterConstruction;
begin
  inherited;
  FConnectTimeout := TIMEOUT_CONNECT_DEFAULT;
  FSendTimeout := TIMEOUT_SEND_DEFAULT;
  FReceiveTimeout := TIMEOUT_RECEIVE_DEFAULT;
end;

{ TProxyCredentials }

function TProxyCredentials.Informed: Boolean;
begin
  Result := (FUserName <> '') and (FPassword <> '');
end;

end.
