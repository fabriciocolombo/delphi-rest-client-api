unit RestClient;

interface

{$I DelphiRest.inc}

uses Classes, SysUtils, Windows, IdHTTP,
     SuperObject,
     RestUtils,
     Contnrs;

const
  DEFAULT_COOKIE_VERSION = 1; {Cookies using the default version correspond to RFC 2109.}

type
  TRequestMethod = (METHOD_GET, METHOD_POST, METHOD_PUT, METHOD_DELETE);

  TResource = class;

  TRestClient = class
  private
    FIdHttp: TIdHTTP;
    FResources: TObjectList;

    function GetResponseCode: Integer;

    function DoRequest(Method: TRequestMethod; ResourceRequest: TResource): WideString;
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

    {$IFDEF USE_GENERICS}
    function Post<T>(Entity: TObject): T;overload;
    function Get<T>(classType: TClass): T;overload;
    function Put<T>(Entity: TObject): T;overload;
    {$ENDIF}

    //Delete has no support content
    procedure Delete(Entity: TObject);overload;
  end;

  TJsonUtil = class
  public
    class function Marshal(entity: TObject): string;

    {$IFDEF USE_GENERICS}
    class function UnMarshal<T>(ClassType: TClass; text: String): T;
    {$ENDIF}
  end;

implementation

uses StrUtils, Math;

{ TRestClient }

constructor TRestClient.Create;
begin
  FIdHttp := TIdHTTP.Create(nil);
  FIdHttp.HandleRedirects := True;
//  FIdHttp.OnRedirect := OnRedirect;

  FResources := TObjectList.Create;
end;

destructor TRestClient.Destroy;
begin
  FResources.Free;
  FIdHttp.Free;
  inherited;
end;

function TRestClient.DoRequest(Method: TRequestMethod; ResourceRequest: TResource): WideString;
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

    {$IFDEF UNICODE}
      Result :=  UTF8ToWideString(RawByteString(vResponse.DataString));
    {$ELSE}
      Result :=  UTF8Decode(vResponse.DataString);
    {$ENDIF}
  finally
    vResponse.Free;
  end;

end;

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

{.$ENDIF}

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
function TResource.Get<T>(classType: TClass): T;
var
  vResponse: string;
begin
  vResponse := Self.Get;
  
  Result := TJsonUtil.UnMarshal<T>(classType, vResponse);
end;

function TResource.Post<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);
  
  vResponse := FRestClient.DoRequest(METHOD_POST, Self);

  Result := TJsonUtil.UnMarshal<T>(Entity.ClassType, vResponse);
end;

function TResource.Put<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_PUT, Self);

  Result := TJsonUtil.UnMarshal<T>(Entity.ClassType, vResponse);
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

{ TJsonUtil }

class function TJsonUtil.Marshal(entity: TObject): string;
begin
  {$IFDEF USE_GENERICS}
    Result := entity.ToJson().AsJSon();
  {$ELSE}
    Result := entity.className;
  {$ENDIF}
end;

{$IFDEF USE_GENERICS}
class function TJsonUtil.UnMarshal<T>(ClassType: TClass; text: String): T;
var
  ctx: TSuperRttiContext;
begin
  ctx := TSuperRttiContext.Create;
  try
    Result := ctx.AsType<T>(SuperObject.SO(text));
  finally
    ctx.Free;
  end;
end;
{$ENDIF}

end.
