unit HttpConnection;

interface

uses Classes, SysUtils;

type
  THttpConnectionType = (hctUnknown, hctIndy, hctWinHttp);

  EHTTPError = class(Exception)
    private
      FErrorCode: integer;
      FErrorMessage: string;
    public
      constructor Create(const Msg, ErrorMessage: string; const ErrorCode: integer); overload; virtual;
      property ErrorMessage: string read FErrorMessage;
      property ErrorCode: integer read FErrorCode;
  end;

  IHttpConnection = interface
  ['{B9611100-5243-4874-A777-D91448517116}']
    function SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
    function SetContentTypes(AContentTypes: string): IHttpConnection;
    function SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
    function SetHeaders(AHeaders: TStrings): IHttpConnection;

    procedure Get(AUrl: string; AResponse: TStream);
    procedure Post(AUrl: string; AContent, AResponse: TStream);
    procedure Put(AUrl: string; AContent, AResponse: TStream);
    procedure Delete(AUrl: string; AContent: TStream);

    function GetResponseCode: Integer;
    function GetEnabledCompression: Boolean;

    procedure SetEnabledCompression(const Value: Boolean);

    property ResponseCode: Integer read GetResponseCode;
    property EnabledCompression: Boolean read GetEnabledCompression write SetEnabledCompression;
  end;

implementation

{ THttpError }

constructor EHTTPError.Create(const Msg, ErrorMessage: string; const ErrorCode: integer);
begin
  inherited Create(Msg);
  FErrorMessage := ErrorMessage;
  FErrorCode := FErrorCode;
end;

end.
