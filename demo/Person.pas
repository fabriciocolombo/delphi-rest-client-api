unit Person;

interface

uses SysUtils;

type
  TPerson = class(TObject)
  public
    id: Integer;
    name: String;
    email: String;
    createDate: TDateTime;

    class function NewFrom(Id: Integer; Name, EMail: String): TPerson;
  end;

implementation

{ TPerson }

class function TPerson.NewFrom(Id: Integer; Name, EMail: String): TPerson;
begin
  Result := TPerson.Create;
  Result.Id := Id;
  Result.Name := Name;
  Result.EMail := EMail;
end;

end.
