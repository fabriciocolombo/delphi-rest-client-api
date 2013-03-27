unit Person;

interface

uses SysUtils;

{$I DelphiRest.inc}

type
  TPerson = class(TObject)
  public
    (* Reflect the java object field names, case-sensitive *)
    id: Integer;
    name: String;
    email: String;
    createDate: TDateTime;

    class function NewFrom(Id: Integer; Name, EMail: String): TPerson;

    function ToString: string;{$IFDEF DELPHI_2009_UP}override;{$ENDIF}
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

function TPerson.ToString: string;
begin
  Result := 'id:' + IntToStr(id) + ' name:' + name + ' email:' + email + ' createDate:' + DateTimeToStr(createDate);
end;

end.
