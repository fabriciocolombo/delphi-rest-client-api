unit TestHeader;

interface

uses BaseTestRest, RestUtils, IdHTTPServer, IdCustomHTTPServer, IdContext, HttpConnection, DCPbase64;

type
  TTestHeader = class(TBaseTestRest)
  private
    FHttpServer: TIdHTTPServer;
    FAuthType: string;
    FAuthData: string;

    function CreateHttpServer: TIdHTTPServer;
    procedure DestroyHttpServer;

    procedure OnParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername,
      VPassword: string; var VHandled: Boolean);

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

uses SysUtils;

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
  FHttpServer.OnParseAuthentication := OnParseAuthentication;
  FHttpServer.OnCommandGet := OnCommandGet;
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
  if not ARequestInfo.AuthExists then
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

procedure TTestHeader.OnParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername,
  VPassword: string; var VHandled: Boolean);
begin
  FAuthType := AAuthType;
  FAuthData := AAuthData;
end;

initialization
  TTestHeader.RegisterTest;

end.
