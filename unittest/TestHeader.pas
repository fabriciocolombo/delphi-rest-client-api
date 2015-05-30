unit TestHeader;

interface

{$I DelphiRest.inc}

uses BaseTestRest, RestUtils, IdHTTPServer, IdCustomHTTPServer, IdContext, HttpConnection, DCPbase64, IdHeaderList;

type
  TTestHeader = class(TBaseTestRest)
  private
    FHttpServer: TIdHTTPServer;
    FAuthExists: Boolean;
    FAuthType: string;
    FAuthData: string;

    function CreateHttpServer: TIdHTTPServer;
    procedure DestroyHttpServer;

    {$IFDEF DELPHI_XE_UP}
    procedure OnHeadersAvailable(AContext: TIdContext; const AUri: string; AHeaders: TIdHeaderList; var VContinueProcessing: Boolean);
    {$ELSE}
    procedure OnHeadersAvailable(AContext: TIdContext; AHeaders: TIdHeaderList; var VContinueProcessing: Boolean);
    {$ENDIF}
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);

  published
    procedure OneHeaderParams;
    procedure OneAcceptLanguages;
    procedure MultipleHeaderParams;
    procedure MultipleAcceptLanguages;
    procedure MultipleAcceptTypes;
    procedure ContentType;
    procedure BasicAuthentication;
    procedure BasicAuthenticationWithOutCredetentialsReturns401;
  end;

implementation

uses SysUtils, StrUtils;

{ TTestHeader }

procedure TTestHeader.MultipleAcceptLanguages;
var
  vResponse: String;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/languages')
                         .AcceptLanguage(RestUtils.LOCALE_PORTUGUESE_BRAZILIAN)
                         .AcceptLanguage(RestUtils.LOCALE_US)
                         .Get;

  CheckEqualsString('["pt_BR","en_US"]', vResponse);
end;

procedure TTestHeader.MultipleAcceptTypes;
var
  vResponse: string;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/accept')
                         .Accept(RestUtils.MediaType_Json)
                         .Accept(RestUtils.MediaType_Xml)
                         .Get;

  CheckEquals('["application/json","text/xml"]', vResponse);
end;

procedure TTestHeader.BasicAuthentication;
var
  vResponse: string;
begin
  CreateHttpServer;
  try
    vResponse := RestClient.SetCredentials('user', 'password')
                           .Resource('http://localhost:9999/auth')
                           .Get;

    CheckEquals(FAuthType, 'Basic');
    CheckEquals(Base64DecodeStr(FAuthData), 'user:password');
    CheckEquals(RestUtils.TStatusCode.OK.StatusCode, RestClient.ResponseCode);
  finally
    DestroyHttpServer;
  end;
end;

procedure TTestHeader.BasicAuthenticationWithOutCredetentialsReturns401;
var
  vResponse: string;
begin
  CreateHttpServer;
  try
    try
      vResponse := RestClient.Resource('http://localhost:9999/auth')
                             .Get;
      Fail('Not throws exception');
    except
      on E: Exception do
      begin
        CheckTrue(E is EHTTPError);
        CheckEquals(EHTTPError(E).ErrorCode, RestClient.ResponseCode);
      end;
    end;

    CheckEquals(FAuthType, '');
    CheckEquals(FAuthData, '');
    CheckEquals(RestUtils.TStatusCode.UNAUTHORIZED.StatusCode, RestClient.ResponseCode);
  finally
    DestroyHttpServer;
  end;
end;

procedure TTestHeader.ContentType;
var
  vResponse: string;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/content')
                         .ContentType(RestUtils.MediaType_Json)
                         .Get;

  CheckEquals('application/json', vResponse);
end;


function TTestHeader.CreateHttpServer: TIdHTTPServer;
begin
  FHttpServer := TIdHTTPServer.Create(nil);
  FHttpServer.DefaultPort := 9999;
  FHttpServer.OnCommandGet := OnCommandGet;
  FHttpServer.OnHeadersAvailable := OnHeadersAvailable;
  FHttpServer.Active := True;

  Result := FHttpServer;
end;

procedure TTestHeader.DestroyHttpServer;
begin
  FHttpServer.Free;
end;

procedure TTestHeader.MultipleHeaderParams;
var
  vResponse: String;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/custom')
                         .Header('custom-a', '1')
                         .Header('custom-b', '2')
                         .Get;

  CheckEqualsString('{"custom-a":"1","custom-b":"2"}', vResponse);
end;

procedure TTestHeader.OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  if not FAuthExists then
  begin
    AResponseInfo.ResponseNo := TStatusCode.UNAUTHORIZED.StatusCode;
    AResponseInfo.ResponseText := 'Not authenticated user';
  end;
end;

procedure TTestHeader.OneAcceptLanguages;
var
  vResponse: String;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/languages')
                         .AcceptLanguage(RestUtils.LOCALE_PORTUGUESE_BRAZILIAN)
                         .Get;

  CheckEqualsString('["pt_BR"]', vResponse);
end;

procedure TTestHeader.OneHeaderParams;
var
  vResponse: String;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/x-foo')
                         .Header('x-foo', 'Bar')
                         .Get;

  CheckEqualsString('{"x-foo":"Bar"}', vResponse);
end;

{$IFDEF DELPHI_XE_UP}
procedure TTestHeader.OnHeadersAvailable(AContext: TIdContext; const AUri: string; AHeaders: TIdHeaderList; var VContinueProcessing: Boolean);
{$ELSE}
procedure TTestHeader.OnHeadersAvailable(AContext: TIdContext; AHeaders: TIdHeaderList; var VContinueProcessing: Boolean);
{$ENDIF}
var
  vAuthorizationValue: string;
  i: Integer;
begin
  for i := 0 to AHeaders.Count-1 - 1 do
  begin
    if StartsText('Authorization', AHeaders.Strings[i]) then
    begin
      vAuthorizationValue := AHeaders.Values[AHeaders.Names[i]];
      if StartsText('Basic', vAuthorizationValue) then
      begin
        Delete(vAuthorizationValue,1,6);
        FAuthExists := True;
        FAuthType := 'Basic';
        FAuthData := vAuthorizationValue;
      end;
      Break;
    end;
  end;

end;

initialization
  TTestHeader.RegisterTest;

end.
