unit HttpConnection;

interface

uses Classes;

type
  THttpConnectionType = (hctUnknown, hctIndy, hctWinHttp, hctCustom);

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

end.
