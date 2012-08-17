unit TestResponseHandler;

interface

{$I DelphiRest.inc}

uses BaseTestRest, Classes;

type
  TTestResponseHandler = class(TBaseTestRest)
  private
    FResponse: String;

    procedure HandleResponse(ResponseContent: TStream);
  published
    {$IFDEF DELPHI_2009_UP}
    procedure TestResponseHandlerAnonymous;
    {$ENDIF}
    procedure TestResponseHandler;
  end;

implementation

uses SysUtils;

{ TTestResponseHandler }

{$IFDEF DELPHI_2009_UP}
procedure TTestResponseHandler.TestResponseHandlerAnonymous;
var
  vStringStream: TStringStream;
  vResponse: String;
begin
  RestClient.Resource(CONTEXT_PATH + 'helloworld')
            .Accept('application/json')
            .GET(procedure(ResponseContent: TStream)
                 begin
                   vStringStream := TStringStream.Create('');
                   try
                     ResponseContent.Position := 0;

                     vStringStream.CopyFrom(ResponseContent, ResponseContent.Size);

                     vResponse := UTF8Decode(vStringStream.DataString);
                   finally
                     vStringStream.Free;
                   end;
                 end);

  CheckEqualsString('{"msg":"Olá Mundo!"}', vResponse);
end;
{$ENDIF}

procedure TTestResponseHandler.HandleResponse(ResponseContent: TStream);
var
  vStringStream: TStringStream;
begin
  vStringStream := TStringStream.Create('');
  try
    ResponseContent.Position := 0;

    vStringStream.CopyFrom(ResponseContent, ResponseContent.Size);

    FResponse := UTF8Decode(vStringStream.DataString);
  finally
    vStringStream.Free;
  end;
end;

procedure TTestResponseHandler.TestResponseHandler;
begin
  FResponse := EmptyStr;

  RestClient.Resource(CONTEXT_PATH + 'helloworld')
            .Accept('application/json')
            .GET(HandleResponse);

  CheckEqualsString('{"msg":"Olá Mundo!"}', FResponse);
end;

initialization
  TTestResponseHandler.RegisterTest;

end.
