unit uDM;

interface

uses
  SysUtils, Classes, RestClient;

const
  CONTEXT_PATH = 'http://localhost:8080/java-rest-server/rest/';
    
type
  TDM = class(TDataModule)
    RestClient: TRestClient;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

{$R *.dfm}

end.
