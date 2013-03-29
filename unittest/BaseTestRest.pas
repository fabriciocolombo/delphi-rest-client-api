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

{ TBaseTestRest }

constructor TBaseTestRest.Create(MethodName: string);
begin
  inherited;

end;

class procedure TBaseTestRest.RegisterTest;
begin
  {$IFDEF USE_INDY}
  TestFramework.RegisterTest('Indy', TBaseTestSuite.Create(Self, hctIndy));
  {$ENDIF}
  {$IFDEF USE_WIN_HTTP}
  TestFramework.RegisterTest('WinHTTP', TBaseTestSuite.Create(Self, hctWinHttp));
  {$ENDIF}
end;

procedure TBaseTestRest.SetHttpConnectionType(AHttpConnectionType: THttpConnectionType);
begin
  FHttpConnectionType := AHttpConnectionType;
end;

procedure TBaseTestRest.SetUp;
begin
  inherited;
  FRestClient := TRestClient.Create;
  FRestClient.ConnectionType := FHttpConnectionType;
end;

procedure TBaseTestRest.TearDown;
begin
  FRestClient.Free;
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
