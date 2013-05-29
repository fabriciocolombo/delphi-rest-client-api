unit HttpConnectionWinHttp;

interface

uses HttpConnection, Classes, SysUtils, Variants, ActiveX, AxCtrls, WinHttp_TLB;

type
  THttpConnectionWinHttp = class(TInterfacedObject, IHttpConnection)
  private
    FWinHttpRequest: IWinHttpRequest;
    FAcceptTypes: string;
    FAcceptedLanguages: string;
    FContentTypes: string;
    FHeaders: TStrings;

    procedure Configure;

    procedure CopyResourceStreamToStream(AResponse: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    function SetAcceptTypes(AAcceptTypes: string): IHttpConnection;
    function SetAcceptedLanguages(AAcceptedLanguages: string): IHttpConnection;
    function SetContentTypes(AContentTypes: string): IHttpConnection;
    function SetHeaders(AHeaders: TStrings): IHttpConnection;

    procedure Get(AUrl: string; AResponse: TStream);
    procedure Post(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Put(AUrl: string; AContent: TStream; AResponse: TStream);
    procedure Delete(AUrl: string; AContent: TStream);

    function GetResponseCode: Integer;

    function GetEnabledCompression: Boolean;
    procedure SetEnabledCompression(const Value: Boolean);
  end;

implementation


{ THttpConnectionWinHttp }

procedure THttpConnectionWinHttp.Configure;
var
  i: Integer;
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
end;

procedure THttpConnectionWinHttp.Delete(AUrl: string; AContent: TStream);
begin
  FWinHttpRequest := CoWinHttpRequest.Create;
  FWinHttpRequest.Open('DELETE', AUrl, false);

  Configure;

  FWinHttpRequest.Send(TStreamAdapter.Create(AContent, soReference) as IStream);
end;

destructor THttpConnectionWinHttp.Destroy;
begin
  FHeaders.Free;
  FWinHttpRequest := nil;
  inherited;
end;

procedure THttpConnectionWinHttp.Get(AUrl: string; AResponse: TStream);
begin
  FWinHttpRequest := CoWinHttpRequest.Create;
  FWinHttpRequest.Open('GET', AUrl, false);

  Configure;

  FWinHttpRequest.Send(EmptyParam);

  CopyResourceStreamToStream(AResponse);
end;

function THttpConnectionWinHttp.GetEnabledCompression: Boolean;
begin
  Result := False;
end;

function THttpConnectionWinHttp.GetResponseCode: Integer;
begin
  Result := FWinHttpRequest.Status;
end;

procedure THttpConnectionWinHttp.Post(AUrl: string; AContent, AResponse: TStream);
var
  vAdapter: IStream;
begin
  FWinHttpRequest := CoWinHttpRequest.Create;
  FWinHttpRequest.Open('POST', AUrl, false);

  Configure;

  vAdapter := TStreamAdapter.Create(AContent, soReference);

  FWinHttpRequest.Send(vAdapter);

  CopyResourceStreamToStream(AResponse);
end;

procedure THttpConnectionWinHttp.Put(AUrl: string; AContent,AResponse: TStream);
var
  vAdapter: IStream;
begin
  FWinHttpRequest := CoWinHttpRequest.Create;
  FWinHttpRequest.Open('PUT', AUrl, false);

  Configure;

  vAdapter := TStreamAdapter.Create(AContent, soReference);

  FWinHttpRequest.Send(vAdapter);

  CopyResourceStreamToStream(AResponse);
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

end.
