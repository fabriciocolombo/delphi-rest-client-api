unit TestAsync;

interface

uses BaseTestRest;

type
  TTestAsync = class(TBaseTestRest)
  private
  published
    procedure GET_CancelRequestUsingOnAsyncRequestProcessEvent;
    procedure GET_CancelRequestWhenDestroyingTheRestClientObject;
  end;

implementation

uses
  System.Classes, System.SysUtils, HttpConnection;

{ TTestAsync }

procedure TTestAsync.GET_CancelRequestUsingOnAsyncRequestProcessEvent;
begin
  if RestClient.ConnectionType = hctWinHttp then
    ExpectedException := EAbort
  else
    ExpectedException := ENotImplemented;

  RestClient.OnAsyncRequestProcess :=
    procedure(var Cancel: Boolean)
    begin
      Cancel := True;
    end;

  RestClient.Resource(CONTEXT_PATH + 'async')
            .Accept('text/plain')
            .Async
            .GET();
end;

procedure TTestAsync.GET_CancelRequestWhenDestroyingTheRestClientObject;
begin
  if RestClient.ConnectionType = hctWinHttp then
    ExpectedException := EAbort
  else
    ExpectedException := ENotImplemented;

  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(50);
      FreeAndNilRestClient;
    end).Start;

  RestClient.Resource(CONTEXT_PATH + 'async')
            .Accept('text/plain')
            .Async
            .GET();
end;

initialization
  TTestAsync.RegisterTest;

end.

