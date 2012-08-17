unit RestClient;

interface

{$I DelphiRest.inc}

uses Classes, SysUtils, Windows, IdHTTP,
     SuperObject,
     RestUtils,
     {$IFDEF USE_GENERICS}
     Generics.Collections, Rtti,
     {$ENDIF}
     Contnrs;

const
  DEFAULT_COOKIE_VERSION = 1; {Cookies using the default version correspond to RFC 2109.}

type
  TRequestMethod = (METHOD_GET, METHOD_POST, METHOD_PUT, METHOD_DELETE);

  {$IFDEF DELPHI_2009_UP}
  TRestResponseHandlerFunc = reference to procedure(ResponseContent: TStream);
  {$ENDIF}
  TRestResponseHandler = procedure (ResponseContent: TStream) of object;

  TResource = class;

  TRestClient = class
  private
    FIdHttp: TIdHTTP;
    FResources: TObjectList;

    {$IFDEF DELPHI_2009_UP}
    FTempHandler: TRestResponseHandlerFunc;
    procedure DoRequestFunc(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandlerFunc);
    procedure HandleAnonymousMethod(ResponseContent: TStream);
    {$ENDIF}
    function DoRequest(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandler = nil): WideString;overload;

    function GetResponseCode: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property ResponseCode: Integer read GetResponseCode;

    function Resource(URL: String): TResource;
  end;

  TCookie = class
  private
    FName: String;
    FValue: String;
    FVersion: Integer;
    FPath: String;
    FDomain: String;
  public
    property Name: String read FName;
    property Value: String read FValue;
    property Version: Integer read FVersion default DEFAULT_COOKIE_VERSION;
    property Path: String read FPath;
    property Domain: String read FDomain;
  end;

  TResource = class
  private
    FRestClient: TRestClient;
    FURL: string;
    FAcceptTypes: string;
    FContent: TMemoryStream;
    FContentTypes: string;
    FHeaders: TStrings;
    FAcceptedLanguages: string;

    constructor Create(RestClient: TRestClient; URL: string);
    procedure SetContent(entity: TObject);
  public
    destructor Destroy; override;

    function GetAcceptTypes: string;
    function GetURL: string;
    function GetContent: TStream;
    function GetContentTypes: string;
    function GetHeaders: TStrings;
    function GetAcceptedLanguages: string;

    function Accept(AcceptType: String): TResource;overload;
    function ContentType(ContentType: String): TResource;overload;
    function AcceptLanguage(Language: String): TResource;overload;

    function Header(Name: String; Value: string): TResource;

    //function Cookie(Cookie: TCookie): TResource;

    function Get: string;overload;
    function Post(Content: TStream): String;overload;
    function Put(Content: TStream): String;overload;
    procedure Delete();overload;

    procedure Get(AHandler: TRestResponseHandler);overload;
    procedure Post(Content: TStream; AHandler: TRestResponseHandler);overload;
    procedure Put(Content: TStream; AHandler: TRestResponseHandler);overload;

    {$IFDEF DELPHI_2009_UP}
    procedure Get(AHandler: TRestResponseHandlerFunc);overload;
    procedure Post(Content: TStream; AHandler: TRestResponseHandlerFunc);overload;
    procedure Put(Content: TStream; AHandler: TRestResponseHandlerFunc);overload;
    {$ENDIF}

    {$IFDEF USE_GENERICS}
    function Get<T>(): T;overload;
    function GetAsList<T>(): TList<T>;
    function Post<T>(Entity: TObject): T;overload;
    function Put<T>(Entity: TObject): T;overload;
    {$ENDIF}

    //Delete has no support content
    procedure Delete(Entity: TObject);overload;
  end;

implementation

uses StrUtils, Math, RestJsonUtils;

{ TRestClient }

constructor TRestClient.Create;
begin
  FIdHttp := TIdHTTP.Create(nil);
  FIdHttp.HandleRedirects := True;

  FResources := TObjectList.Create;
end;

destructor TRestClient.Destroy;
begin
  FResources.Free;
  FIdHttp.Free;
  inherited;
end;

function TRestClient.DoRequest(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandler): WideString;
var
  vResponse: TStringStream;
  vUrl: String;
begin
  vResponse := TStringStream.Create('');
  try
    FIdHttp.Request.Accept := ResourceRequest.GetAcceptTypes;
    FIdHttp.Request.ContentType := ResourceRequest.GetContentTypes;
    FIdHttp.Request.CustomHeaders.AddStrings(ResourceRequest.GetHeaders);
    FIdHttp.Request.AcceptLanguage := ResourceRequest.GetAcceptedLanguages;

    vUrl := ResourceRequest.GetURL;

    ResourceRequest.GetContent.Position := 0;

    case Method of
      METHOD_GET: FIdHttp.Get(vUrl, vResponse);
      METHOD_POST: FIdHttp.Post(vURL, ResourceRequest.GetContent, vResponse);
      METHOD_PUT: FIdHttp.Put(vURL, ResourceRequest.GetContent, vResponse);
      METHOD_DELETE: FIdHttp.Delete(vUrl);
    end;

    if Assigned(AHandler) then
    begin
      AHandler(vResponse);
    end
    else
    begin
      {$IFDEF UNICODE}
        Result :=  UTF8ToWideString(RawByteString(vResponse.DataString));
      {$ELSE}
        Result :=  UTF8Decode(vResponse.DataString);
      {$ENDIF}
    end;
  finally
    vResponse.Free;
  end;
end;

{$IFDEF DELPHI_2009_UP}
procedure TRestClient.DoRequestFunc(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandlerFunc);
begin
  FTempHandler := AHandler;

  DoRequest(Method, ResourceRequest, HandleAnonymousMethod);
end;

procedure TRestClient.HandleAnonymousMethod(ResponseContent: TStream);
begin
  FTempHandler(ResponseContent);
  FTempHandler := nil;
end;
{$ENDIF}

function TRestClient.GetResponseCode: Integer;
begin
  Result := FIdHttp.ResponseCode;
end;

function TRestClient.Resource(URL: String): TResource;
begin
  Result := TResource.Create(Self, URL);

  FResources.Add(Result);
end;

{ TResource }

function TResource.Accept(AcceptType: String): TResource;
begin
  FAcceptTypes := FAcceptTypes + IfThen(FAcceptTypes <> EmptyStr,',') + AcceptType;
  Result := Self;
end;

procedure TResource.Get(AHandler: TRestResponseHandler);
begin
  FRestClient.DoRequest(METHOD_GET, Self, AHandler);
end;

procedure TResource.Post(Content: TStream; AHandler: TRestResponseHandler);
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  FRestClient.DoRequest(METHOD_POST, Self, AHandler);
end;

procedure TResource.Put(Content: TStream; AHandler: TRestResponseHandler);
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  FRestClient.DoRequest(METHOD_PUT, Self, AHandler);
end;

{$IFDEF DELPHI_2009_UP}
procedure TResource.Get(AHandler: TRestResponseHandlerFunc);
begin
  FRestClient.DoRequestFunc(METHOD_GET, Self, AHandler);
end;

procedure TResource.Post(Content: TStream; AHandler: TRestResponseHandlerFunc);
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  FRestClient.DoRequestFunc(METHOD_POST, Self, AHandler);
end;

procedure TResource.Put(Content: TStream; AHandler: TRestResponseHandlerFunc);
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  FRestClient.DoRequestFunc(METHOD_PUT, Self, AHandler );
end;
{$ENDIF}

function TResource.GetAcceptedLanguages: string;
begin
  Result := FAcceptedLanguages;
end;

function TResource.AcceptLanguage(Language: String): TResource;
begin
  Result := Header('Accept-Language', Language);
end;

function TResource.ContentType(ContentType: String): TResource;
begin
  FContentTypes := ContentType;

  Result := Self;
end;

constructor TResource.Create(RestClient: TRestClient; URL: string);
begin
  inherited Create;
  FRestClient := RestClient;
  FURL := URL;
  FContent := TMemoryStream.Create;
  FHeaders := TStringList.Create;
end;

procedure TResource.Delete();
begin
  FRestClient.DoRequest(METHOD_DELETE, Self);
end;

procedure TResource.Delete(Entity: TObject);
begin
  SetContent(Entity);
 
  FRestClient.DoRequest(METHOD_DELETE, Self);
end;

destructor TResource.Destroy;
begin
  FContent.Free;
  FHeaders.Free;
  inherited;
end;

function TResource.Get: string;
begin
  Result := FRestClient.DoRequest(METHOD_GET, Self);
end;

function TResource.GetAcceptTypes: string;
begin
  Result := FAcceptTypes;
end;

function TResource.GetContent: TStream;
begin
  Result := FContent;
end;

function TResource.GetContentTypes: string;
begin
  Result := FContentTypes;
end;

function TResource.GetHeaders: TStrings;
begin
  Result := FHeaders;
end;

function TResource.GetURL: string;
begin
  Result := FURL;
end;

function TResource.Header(Name, Value: string): TResource;
begin
  FHeaders.Add(Format('%s=%s', [Name, Value]));

  Result := Self;
end;

function TResource.Post(Content: TStream): String;
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  Result := FRestClient.DoRequest(METHOD_POST, Self);
end;

{$IFDEF USE_GENERICS}
function TResource.Get<T>(): T;
var
  vResponse: string;
begin
  vResponse := Self.Get;

  Result := TJsonUtil.UnMarshal<T>(vResponse);
end;

function TResource.GetAsList<T>: TList<T>;
var
  vResponse: string;
begin
  vResponse := Self.Get;

  Result := TJsonUtil.UnMarshalList<T>(vResponse);
end;

function TResource.Post<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_POST, Self);

  Result := TJsonUtil.UnMarshal<T>(vResponse);
end;

function TResource.Put<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_PUT, Self);

  Result := TJsonUtil.UnMarshal<T>(vResponse);
end;
{$ENDIF}

procedure TResource.SetContent(entity: TObject);
var
  vJson: string;
  vStream: TStringStream;
begin
  vJson := TJsonUtil.Marshal(Entity);

  vStream := TStringStream.Create(vJson);
  try
    vStream.Position := 0;
    FContent.Clear;
    FContent.CopyFrom(vStream, vStream.Size);
  finally
    vStream.Free;
  end;
end;

function TResource.Put(Content: TStream): String;
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  Result := FRestClient.DoRequest(METHOD_PUT, Self);
end;

end.
