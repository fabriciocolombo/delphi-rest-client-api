unit RestClient;

interface

{$I DelphiRest.inc}

uses Classes, SysUtils, HttpConnection,
     RestUtils, RestJsonUtils,
     {$IFDEF USE_GENERICS}
     Generics.Collections, Rtti,
     {$ELSE}
     Contnrs, OldRttiUnMarshal,
     {$ENDIF}
     DB, JsonListAdapter;

const
  DEFAULT_COOKIE_VERSION = 1; {Cookies using the default version correspond to RFC 2109.}

type
  TRequestMethod = (METHOD_GET, METHOD_POST, METHOD_PUT, METHOD_PATCH, METHOD_DELETE);

  {$IFDEF DELPHI_2009_UP}
  TRestResponseHandlerFunc = reference to procedure(ResponseContent: TStream);
  {$ENDIF}
  TRestResponseHandler = procedure (ResponseContent: TStream) of object;

  TResource = class;

  TCustomCreateConnection = procedure(Sender: TObject; AConnectionType: THttpConnectionType; out AConnection: IHttpConnection) of object;

  ERestClientException = class(Exception);
  EInvalidHttpConnectionConfiguration = class(ERestClientException);
  ECustomCreateConnectionException = class(ERestClientException);
  EInactiveConnection = class(ERestClientException);

  TJsonListAdapter = class(TInterfacedObject, IJsonListAdapter)
  private
    FItemClass: TClass;
    FWrappedList: TList;
  public
    function ItemClass: TClass;
    function UnWrapList: TList;

    constructor Create(AList: TList; AItemClass: TClass);

    class function NewFrom(AList: TList; AItemClass: TClass): IJsonListAdapter;
  end;

  TRestClient = class(TComponent)
  private
    FHttpConnection: IHttpConnection;
    {$IFDEF USE_GENERICS}
    FResources: TObjectList<TResource>;
    {$ELSE}
    FResources: TObjectList;
    {$ENDIF}
    FConnectionType: THttpConnectionType;
    FEnabledCompression: Boolean;
    FOnCustomCreateConnection: TCustomCreateConnection;
    FTimeOut: TTimeOut;

    {$IFDEF DELPHI_2009_UP}
    FTempHandler: TRestResponseHandlerFunc;
    procedure DoRequestFunc(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandlerFunc);
    procedure HandleAnonymousMethod(ResponseContent: TStream);
    {$ENDIF}
    function DoRequest(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandler = nil): String;overload;

    function GetResponseCode: Integer;

    procedure SetConnectionType(const Value: THttpConnectionType);

    procedure RecreateConnection;

    procedure CheckConnection;

    procedure SetEnabledCompression(const Value: Boolean);

    function DoCustomCreateConnection: IHttpConnection;

    function GetOnConnectionLost: THTTPConnectionLostEvent;
    procedure SetOnConnectionLost(AConnectionLostEvent: THTTPConnectionLostEvent);

    function GetOnError: THTTPErrorEvent;
    procedure SetOnError(AErrorEvent: THTTPErrorEvent);

  protected
    procedure Loaded; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    property ResponseCode: Integer read GetResponseCode;

    function Resource(URL: String): TResource;

    function UnWrapConnection: IHttpConnection;

    property OnConnectionLost: THTTPConnectionLostEvent read GetOnConnectionLost write SetOnConnectionLost;
    property OnError: THTTPErrorEvent read GetOnError write SetOnError;
  published
    property ConnectionType: THttpConnectionType read FConnectionType write SetConnectionType;
    property EnabledCompression: Boolean read FEnabledCompression write SetEnabledCompression default True;
    property OnCustomCreateConnection: TCustomCreateConnection read FOnCustomCreateConnection write FOnCustomCreateConnection;
    property TimeOut: TTimeOut read FTimeOut;
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

    function Accept(AcceptType: String): TResource;
    function ContentType(ContentType: String): TResource;
    function AcceptLanguage(Language: String): TResource;

    function Header(Name: String; Value: string): TResource;

    //function Cookie(Cookie: TCookie): TResource;

    function Get: string;overload;
    procedure Get(AHandler: TRestResponseHandler);overload;
    function Get(EntityClass: TClass): TObject;overload;

    function Post(Content: TStream): String;overload;
    function Post(Content: string): String;overload;
    procedure Post(Content: TStream; AHandler: TRestResponseHandler);overload;
    function Post(Entity: TObject): TObject;overload;

    function Put(Content: TStream): String;overload;
    function Put(Content: string): string;overload;
    procedure Put(Content: TStream; AHandler: TRestResponseHandler);overload;
    function Put(Entity: TObject): TObject;overload;

    function Patch(Content: TStream): String;overload;
    function Patch(Content: string): string;overload;
    procedure Patch(Content: TStream; AHandler: TRestResponseHandler);overload;
    function Patch(Entity: TObject): TObject;overload;

    procedure Delete();overload;
    procedure Delete(Entity: TObject);overload;

    {$IFDEF DELPHI_2009_UP}
    procedure Get(AHandler: TRestResponseHandlerFunc);overload;
    procedure Post(Content: TStream; AHandler: TRestResponseHandlerFunc);overload;
    procedure Put(Content: TStream; AHandler: TRestResponseHandlerFunc);overload;
    procedure Patch(Content: TStream; AHandler: TRestResponseHandlerFunc);overload;
    {$ENDIF}

    {$IFDEF USE_GENERICS}
    function Get<T>(): T;overload;
    function Post<T>(Entity: TObject): T;overload;
    function Put<T>(Entity: TObject): T;overload;
    function Patch<T>(Entity: TObject): T;overload;
    {$ELSE}
    function Get(AListClass, AItemClass: TClass): TObject;overload;
    function Post(Adapter: IJsonListAdapter): TObject;overload;
    function Put(Adapter: IJsonListAdapter): TObject;overload;
    function Patch(Adapter: IJsonListAdapter): TObject;overload;
    {$ENDIF}

    {$IFDEF USE_SUPER_OBJECT}
    procedure GetAsDataSet(ADataSet: TDataSet);overload;
    function GetAsDataSet(): TDataSet;overload;
    {$ENDIF}
  end;

implementation

uses StrUtils, Math,
     {$IFDEF USE_SUPER_OBJECT}
     SuperObject, JsonToDataSetConverter,
     {$ENDIF}
     HttpConnectionFactory;

{ TRestClient }

constructor TRestClient.Create(Owner: TComponent);
begin
  inherited;
  {$IFDEF USE_GENERICS}
  FResources := TObjectList<TResource>.Create;
  {$ELSE}
  FResources := TObjectList.Create;
  {$ENDIF}

  FTimeOut := TTimeOut.Create(Self);
  FTimeOut.Name := 'TimeOut';
  FTimeOut.SetSubComponent(True);

  FEnabledCompression := True;
end;

destructor TRestClient.Destroy;
begin
  FResources.Free;
  FHttpConnection := nil;
  inherited;
end;

function TRestClient.DoCustomCreateConnection: IHttpConnection;
begin
  if Assigned(FOnCustomCreateConnection) then
  begin
    FOnCustomCreateConnection(Self, FConnectionType, Result);

    if Result = nil then
    begin
      raise ECustomCreateConnectionException.Create('HttpConnection not supplied by OnCustomCreateConnection');
    end;
  end
  else
  begin
    raise EInvalidHttpConnectionConfiguration.Create('ConnectionType is set to Custom but OnCustomCreateConnection event is not implemented.');
  end;
end;

function TRestClient.DoRequest(Method: TRequestMethod; ResourceRequest: TResource; AHandler: TRestResponseHandler): String;
var
  vResponse: TStringStream;
  vUrl: String;
  vResponseString: string;
  vRetryMode: THTTPRetryMode;
begin
  CheckConnection;

  vResponse := TStringStream.Create('');
  try
    FHttpConnection.SetAcceptTypes(ResourceRequest.GetAcceptTypes)
                   .SetContentTypes(ResourceRequest.GetContentTypes)
                   .SetHeaders(ResourceRequest.GetHeaders)
                   .SetAcceptedLanguages(ResourceRequest.GetAcceptedLanguages)
                   .ConfigureTimeout(FTimeOut);

    vUrl := ResourceRequest.GetURL;

    ResourceRequest.GetContent.Position := 0;

    case Method of
      METHOD_GET: FHttpConnection.Get(vUrl, vResponse);
      METHOD_POST: FHttpConnection.Post(vURL, ResourceRequest.GetContent, vResponse);
      METHOD_PUT: FHttpConnection.Put(vURL, ResourceRequest.GetContent, vResponse);
      METHOD_PATCH: FHttpConnection.Patch(vURL, ResourceRequest.GetContent, vResponse);
      METHOD_DELETE: FHttpConnection.Delete(vUrl, ResourceRequest.GetContent);
    end;

    if Assigned(AHandler) then
    begin
      AHandler(vResponse);
    end
    else
    begin
      {$IFDEF UNICODE}
        vResponseString := vResponse.DataString;

        {$IFDEF NEXTGEN}
          Result := TEncoding.UTF8.GetString(TEncoding.UTF8.GetBytes(vResponseString.ToCharArray), 0, vResponseString.Length);
        {$ELSE}
          Result := UTF8ToUnicodeString(RawByteString(vResponseString));
        {$ENDIF}
      {$ELSE}
        vResponseString := vResponse.DataString;
         
        Result := UTF8Decode(vResponse.DataString);
      {$ENDIF}
    end;
    if (FHttpConnection.ResponseCode >= 400) and
       (FHttpConnection.ResponseCode <> 404) then
    begin
      vRetryMode := hrmRaise;
      if assigned(OnError) then
        FHttpConnection.OnError(format('HTTP Error: %d', [FHttpConnection.ResponseCode]), Result, FHttpConnection.ResponseCode, vRetryMode);

      if vRetryMode = hrmRaise then
        raise EHTTPError.Create(
          format('HTTP Error: %d', [FHttpConnection.ResponseCode]),
          Result,
          FHttpConnection.ResponseCode
        )
      else if vRetryMode = hrmRetry then
        result := DoRequest(Method, ResourceRequest, AHandler);
    end;
  finally
    vResponse.Free;
    FResources.Remove(ResourceRequest);
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

function TRestClient.GetOnConnectionLost: THTTPConnectionLostEvent;
begin
  result := FHttpConnection.OnConnectionLost;
end;

function TRestClient.GetOnError: THTTPErrorEvent;
begin
  result := FHttpConnection.OnError;
end;

function TRestClient.GetResponseCode: Integer;
begin
  CheckConnection;
  
  Result := FHttpConnection.ResponseCode;
end;

procedure TRestClient.RecreateConnection;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FConnectionType = hctCustom then
    begin
      FHttpConnection := DoCustomCreateConnection;
    end
    else
    begin
      FHttpConnection := THttpConnectionFactory.NewConnection(FConnectionType);
      FHttpConnection.EnabledCompression := FEnabledCompression;
    end;
  end;
end;

procedure TRestClient.CheckConnection;
begin
  if (FHttpConnection = nil) then
  begin
    raise EInactiveConnection.CreateFmt('%s: Connection is not active.', [Name]);
  end;
end;

procedure TRestClient.Loaded;
begin
  RecreateConnection;
end;

function TRestClient.Resource(URL: String): TResource;
begin
  Result := TResource.Create(Self, URL);

  FResources.Add(Result);
end;

procedure TRestClient.SetConnectionType(const Value: THttpConnectionType);
begin
  if (FConnectionType <> Value) then
  begin
    FConnectionType := Value;

    RecreateConnection;
  end;
end;

procedure TRestClient.SetEnabledCompression(const Value: Boolean);
begin
  if (FEnabledCompression <> Value) then
  begin
    FEnabledCompression := Value;

    if Assigned(FHttpConnection) then
    begin
      FHttpConnection.EnabledCompression := FEnabledCompression;
    end;
  end;
end;

procedure TRestClient.SetOnConnectionLost(
  AConnectionLostEvent: THTTPConnectionLostEvent);
begin
  FHttpConnection.OnConnectionLost := AConnectionLostEvent;
end;

procedure TRestClient.SetOnError(AErrorEvent: THTTPErrorEvent);
begin
  FHttpConnection.OnError := AErrorEvent;
end;

function TRestClient.UnWrapConnection: IHttpConnection;
begin
  Result := FHttpConnection;
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

function TResource.Put(Content: string): string;
var
  vStringStream: TStringStream;
begin
  vStringStream := TStringStream.Create(Content);
  try
    Result := Put(vStringStream);
  finally
    vStringStream.Free;
  end;
end;

function TResource.Post(Content: string): String;
var
  vStringStream: TStringStream;
begin
  vStringStream := TStringStream.Create(Content);
  try
    Result := Post(vStringStream);
  finally
    vStringStream.Free;
  end;
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

procedure TResource.Patch(Content: TStream; AHandler: TRestResponseHandlerFunc);
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  FRestClient.DoRequestFunc(METHOD_PATCH, Self, AHandler );
end;
{$ENDIF}

function TResource.Get(EntityClass: TClass): TObject;
var
  vResponse: string;
begin
  vResponse := Self.Get;

  Result := TJsonUtil.UnMarshal(EntityClass, vResponse);
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
  FRestClient.FResources.Extract(Self);
  
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
  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal<T>(vResponse)
  else
    result := Default(T);
end;

function TResource.Post<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_POST, Self);

  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal<T>(vResponse)
  else
    Result := Default(T);
end;

function TResource.Put<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_PUT, Self);

  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal<T>(vResponse)
  else
    Result := Default(T);
end;

function TResource.Patch<T>(Entity: TObject): T;
var
  vResponse: string;
begin
  SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_PATCH, Self);

  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal<T>(vResponse)
  else
    Result := Default(T);
end;
{$ELSE}
function TResource.Get(AListClass, AItemClass: TClass): TObject;
var
  vResponse: string;
begin
  vResponse := Self.Get;

  Result := TOldRttiUnMarshal.FromJsonArray(AListClass, AItemClass, vResponse);
end;

function TResource.Post(Adapter: IJsonListAdapter): TObject;
var
  vResponse: string;
begin
  if Adapter <> nil then
    SetContent(Adapter.UnWrapList);

  vResponse := FRestClient.DoRequest(METHOD_POST, Self);
  if trim(vResponse) <> '' then
    Result := TOldRttiUnMarshal.FromJsonArray(Adapter.UnWrapList.ClassType, Adapter.ItemClass, vResponse)
  else
    Result := nil;
end;

function TResource.Put(Adapter: IJsonListAdapter): TObject;
var
  vResponse: string;
begin
  if Adapter <> nil then
    SetContent(Adapter.UnWrapList);

  vResponse := FRestClient.DoRequest(METHOD_PUT, Self);
  if trim(vResponse) <> '' then
    Result := TOldRttiUnMarshal.FromJsonArray(Adapter.UnWrapList.ClassType, Adapter.ItemClass, vResponse)
  else
    Result := nil;
end;

function TResource.Patch(Adapter: IJsonListAdapter): TObject;
var
  vResponse: string;
begin
  if Adapter <> nil then
    SetContent(Adapter.UnWrapList);

  vResponse := FRestClient.DoRequest(METHOD_PATCH, Self);
  if trim(vResponse) <> '' then
    Result := TOldRttiUnMarshal.FromJsonArray(Adapter.UnWrapList.ClassType, Adapter.ItemClass, vResponse)
  else
    Result := nil;
end;
{$ENDIF}

{$IFDEF USE_SUPER_OBJECT}
function TResource.GetAsDataSet: TDataSet;
var
  vJson: ISuperObject;
begin
  vJson := SuperObject.SO(Get);

  Result := TJsonToDataSetConverter.CreateDataSetMetadata(vJson);

  TJsonToDataSetConverter.UnMarshalToDataSet(Result, vJson);
end;

procedure TResource.GetAsDataSet(ADataSet: TDataSet);
var
  vJson: string;
begin
  vJson := Self.Get;

  TJsonToDataSetConverter.UnMarshalToDataSet(ADataSet, vJson);
end;
{$ENDIF}

procedure TResource.SetContent(entity: TObject);
var
  vJson: string;
  vStream: TStringStream;
begin
  FContent.Clear;
  if Assigned(entity) then
  begin
     vJson := TJsonUtil.Marshal(Entity);

    vStream := TStringStream.Create(vJson);
    try
      vStream.Position := 0;
      FContent.CopyFrom(vStream, vStream.Size);
    finally
      vStream.Free;
    end;
  end;
end;

function TResource.Put(Content: TStream): String;
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  Result := FRestClient.DoRequest(METHOD_PUT, Self);
end;

function TResource.Post(Entity: TObject): TObject;
var
  vResponse: string;
begin
  if Entity <> nil then
    SetContent(Entity);
  vResponse := FRestClient.DoRequest(METHOD_POST, Self);
  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal(Entity.ClassType, vResponse)
  else
    Result := nil;
end;

function TResource.Put(Entity: TObject): TObject;
var
  vResponse: string;
begin
  if Entity <> nil then
    SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_PUT, Self);
  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal(Entity.ClassType, vResponse)
  else
    Result := nil;
end;

function TResource.Patch(Content: TStream): String;
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  Result := FRestClient.DoRequest(METHOD_PATCH, Self);
end;

procedure TResource.Patch(Content: TStream; AHandler: TRestResponseHandler);
begin
  Content.Position := 0;
  FContent.CopyFrom(Content, Content.Size);

  FRestClient.DoRequest(METHOD_PATCH, Self, AHandler);
end;

function TResource.Patch(Entity: TObject): TObject;
var
  vResponse: string;
begin
  if Entity <> nil then
    SetContent(Entity);

  vResponse := FRestClient.DoRequest(METHOD_PATCH, Self);
  if trim(vResponse) <> '' then
    Result := TJsonUtil.UnMarshal(Entity.ClassType, vResponse)
  else
    Result := nil;
end;

function TResource.Patch(Content: string): string;
var
  vStringStream: TStringStream;
begin
  vStringStream := TStringStream.Create(Content);
  try
    Result := Patch(vStringStream);
  finally
    vStringStream.Free;
  end;
end;

{ TJsonListAdapter }

function TJsonListAdapter.ItemClass: TClass;
begin
  Result := FItemClass;
end;

function TJsonListAdapter.UnWrapList: TList;
begin
  Result := FWrappedList;
end;

constructor TJsonListAdapter.Create(AList: TList; AItemClass: TClass);
begin
  FWrappedList := AList;
  FItemClass := AItemClass;
end;

class function TJsonListAdapter.NewFrom(AList: TList; AItemClass: TClass): IJsonListAdapter;
begin
  Result := TJsonListAdapter.Create(AList, AItemClass);
end;

end.
