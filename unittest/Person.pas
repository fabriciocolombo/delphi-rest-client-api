unit Person;

interface

uses SysUtils, Classes;

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

  TOldPerson = class(TPersistent)
  private
    Fid: Integer;
    Femail: String;
    Fname: String;
    FcreateDate: TDateTime;
  public
    class function NewFrom(Id: Integer; Name, EMail: String): TOldPerson;

    function ToString: string;{$IFDEF DELPHI_2009_UP}override;{$ENDIF}
  published
    property id: Integer read Fid write Fid;
    property name: String read Fname write Fname;
    property email: String read Femail write Femail;
    property createDate: TDateTime read FcreateDate write FcreateDate;
  end;
    
implementation

{ TPerson }

class function TPerson.NewFrom(Id: Integer; Name, EMail: String): TPerson;
begin
  Result := TPerson.Create;
  Result.Id := Id;
  Result.Name := Name;
  Result.EMail := EMail;
  Result.createDate := Now;
end;

function TPerson.ToString: string;
begin
  Result := 'id:' + IntToStr(id) + ' name:' + name + ' email:' + email + ' createDate:' + DateTimeToStr(createDate);
end;

{ TOldPerson }

class function TOldPerson.NewFrom(Id: Integer; Name, EMail: String): TOldPerson;
begin
  Result := TOldPerson.Create;
  Result.Fid := Id;
  Result.Fname := Name;
  Result.Femail := EMail;
  Result.FcreateDate := Now;
end;

function TOldPerson.ToString: string;
begin
  Result := 'id:' + IntToStr(id) + ' name:' + name + ' email:' + email + ' createDate:' + DateTimeToStr(createDate);
end;
end.
