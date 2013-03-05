unit TestHeader;

interface

uses BaseTestRest, RestUtils;

type
  TTestHeader = class(TBaseTestRest)
  published
    procedure OneHeaderParams;
    procedure OneAcceptLanguages;
    procedure MultipleHeaderParams;
    procedure MultipleAcceptLanguages;
    procedure MultipleAcceptTypes;
    procedure ContentType;
  end;

implementation

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

procedure TTestHeader.ContentType;
var
  vResponse: string;
begin
  vResponse := RestClient.Resource(CONTEXT_PATH + 'helloworld/header/content')
                         .ContentType(RestUtils.MediaType_Json)
                         .Get;

  CheckEquals('application/json', vResponse);
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

initialization
  TTestHeader.RegisterTest;

end.
