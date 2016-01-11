unit BaseTestRest;

interface

uses RestClient, TestFramework, HttpConnection, TestExtensions, TypInfo;

{$I DelphiRest.inc}

type
  IBaseTestRest = interface(ITest)
  ['{519FE812-AC27-484D-9C4B-C7195E0068C4}']
    procedure SetHttpConnectionType(AHttpConnectionType: THttpConnectionType);
  end;

  TBaseTestSuite = class(TTestSuite)
  private
    FHttpConnectionType: THttpConnectionType;
  public
    constructor Create(ATest: TTestCaseClass; AHttpConnectionType: THttpConnectionType);
  end;

  TBaseTestRest = class(TTestCase, IBaseTestRest)
  private
    FRestClient: TRestClient;
    FHttpConnectionType: THttpConnectionType;
  protected
    procedure FreeAndNilRestClient;
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure SetHttpConnectionType(AHttpConnectionType: THttpConnectionType);

    constructor Create(MethodName: string); override;

    property RestClient: TRestClient read FRestClient;

    class procedure RegisterTest;
  end;

const
  CONTEXT_PATH = 'http://localhost:8080/java-rest-server/rest/';

implementation

uses
  SysUtils;

{ TBaseTestRest }

constructor TBaseTestRest.Create(MethodName: string);
begin
  inherited;

end;

procedure TBaseTestRest.FreeAndNilRestClient;
begin
  FreeAndNil(FRestClient);
end;

class procedure TBaseTestRest.RegisterTest;
begin
  {$IFDEF USE_INDY}
  //TestFramework.RegisterTest('Indy', TRepeatedTest.Create(TBaseTestSuite.Create(Self, hctIndy), 100));
  TestFramework.RegisterTest('Indy', TBaseTestSuite.Create(Self, hctIndy));
  {$ENDIF}
  {$IFDEF USE_WIN_HTTP}
  //TestFramework.RegisterTest('WinHTTP', TRepeatedTest.Create(TBaseTestSuite.Create(Self, hctWinHttp), 100));
  TestFramework.RegisterTest('WinHTTP', TBaseTestSuite.Create(Self, hctWinHttp));
  {$ENDIF}
  {$IFDEF USE_WIN_INET}
  //TestFramework.RegisterTest('WinInet', TRepeatedTest.Create(TBaseTestSuite.Create(Self, hctWinInet), 100));
  TestFramework.RegisterTest('WinInet', TBaseTestSuite.Create(Self, hctWinInet));
  {$ENDIF}
end;

procedure TBaseTestRest.SetHttpConnectionType(AHttpConnectionType: THttpConnectionType);
begin
  FHttpConnectionType := AHttpConnectionType;
end;

procedure TBaseTestRest.SetUp;
begin
  inherited;
  FRestClient := TRestClient.Create(nil);
// AV in Delphi XE2
  FRestClient.EnabledCompression := False;
  FRestClient.ConnectionType := FHttpConnectionType;
end;

procedure TBaseTestRest.TearDown;
begin
  FreeAndNilRestClient;
  inherited;
end;

{ TBaseTestSuite }

constructor TBaseTestSuite.Create(ATest: TTestCaseClass; AHttpConnectionType: THttpConnectionType);
var
  i: Integer;
begin
  inherited Create(ATest);
  FHttpConnectionType := AHttpConnectionType;

  for i := 0 to Tests.Count-1 do
  begin
    (Tests[i] as IBaseTestRest).SetHttpConnectionType(FHttpConnectionType);
  end;
end;

end.
