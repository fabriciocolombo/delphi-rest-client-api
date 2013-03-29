unit HttpConnection;

interface

uses Classes;

type
  THttpConnectionType = (hctUnknown, hctIndy, hctWinHttp);

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

    property ResponseCode: Integer read GetResponseCode;
  end;

implementation

end.
