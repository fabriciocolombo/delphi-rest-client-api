unit TestAsync;

interface

uses BaseTestRest;

type
  TTestAsync = class(TBaseTestRest)
  private
  published
    procedure GET_CancelRequest;
  end;

implementation

uses
  System.Classes, System.SysUtils, HttpConnection;

{ TTestAsync }

procedure TTestAsync.GET_CancelRequest;
begin
  if RestClient.ConnectionType = hctWinHttp then
  begin
    ExpectedException := EAbort;

    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(100);
        RestClient.CancelRequest;
      end).Start;
  end
  else
    ExpectedException := ENotImplemented;

  RestClient.Resource(CONTEXT_PATH + 'async')
            .Accept('text/plain')
            .Async
            .GET();
end;

initialization
  TTestAsync.RegisterTest;

end.

