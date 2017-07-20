unit Person;

(*

   IMPORTANT WARNING (for older versions like Delphi 7):

   You have to tell delphi's compiler to include RTTI information for the unit.
   This is done by adding the $M+ directive (see below).
   This directive must be added for every unit that will support RTTI introspection.

*)

{$I DelphiRest.inc}

{$IFDEF DELPHI_7}
   {$M+}
{$ENDIF}

interface

uses SysUtils;

type
  TPerson = class(TObject)
  {$IFDEF DELPHI_7}
  private
    FId: Integer;
    FEmail: String;
    FName: String;
    FCreateDate: TDateTime;
  published
    property id: Integer read FId write FId;
    property name: String read FName write FName;
    property email: String read FEmail write FEmail;
    property createDate: TDateTime read FCreateDate write FCreateDate;
  {$ELSE}
  public
    id: Integer;
    name: String;
    email: String;
    createDate: TDateTime;
  {$ENDIF}

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

