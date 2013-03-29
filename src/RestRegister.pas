unit RestRegister;

interface

procedure Register;

implementation

uses Classes, RestClient;

procedure Register;
begin
  RegisterComponents('Rest', [TRestClient]);
end;

end.
