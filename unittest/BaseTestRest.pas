unit BaseTestRest;

interface

uses RestClient, TestFramework;

type
  TBaseTestRest = class(TTestCase)
  private
    FRestClient: TRestClient;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    property RestClient: TRestClient read FRestClient;

    class procedure RegisterTest;
  end;

const
  CONTEXT_PATH = 'http://localhost:8080/java-rest-server/rest/';

implementation

{ TBaseTestRest }

class procedure TBaseTestRest.RegisterTest;
begin
  TestFramework.RegisterTest(Self.Suite);
end;

procedure TBaseTestRest.SetUp;
begin
  inherited;
  FRestClient := TRestClient.Create;
end;

procedure TBaseTestRest.TearDown;
begin
  FRestClient.Free;
  inherited;
end;

end.
