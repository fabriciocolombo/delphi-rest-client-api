unit Person;

interface

type
  TPerson = class(TObject)
  public
    (* Reflect the java object field names, case-sensitive *)
    id: Integer;
    name: String;
    email: String;

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
