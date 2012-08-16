unit TestResponseHandler;

interface

{$I DelphiRest.inc}

uses BaseTestRest, Classes;

type
  TTestResponseHandler = class(TBaseTestRest)
  published
    procedure TestResponseHandler;
  end;

implementation

{ TTestResponseHandler }

procedure TTestResponseHandler.TestResponseHandler;
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

initialization
  TTestResponseHandler.RegisterTest;

end.
