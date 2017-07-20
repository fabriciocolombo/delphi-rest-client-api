unit uFrm_Person;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RestUtils, Person;

type
  TFrm_Person = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edId: TEdit;
    edName: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    Label4: TLabel;
    edEmail: TEdit;
    edCreateDate: TEdit;
  private
    FIsNew: Boolean;

    procedure Fill(APerson: TPerson);
    procedure ReadFromForm(APerson: TPerson);
  public
    class function Modify(var APerson: TPerson): Boolean;
  end;

implementation

uses uDM;

{$R *.dfm}

{ TFrm_Person }

procedure TFrm_Person.Fill(APerson: TPerson);
begin
  FIsNew := (APerson = nil);

  edCreateDate.Visible := not FIsNew;

  if not FIsNew then
  begin
    edId.Text := IntToStr(APerson.id);
    edName.Text := APerson.name;
    edEmail.Text := APerson.email;
    edCreateDate.Text := DateTimeToStr(APerson.createDate);
  end;
end;

class function TFrm_Person.Modify(var APerson: TPerson): Boolean;
var
  vForm: TFrm_Person;
begin
  vForm := TFrm_Person.Create(nil);
  try
    vForm.Fill(APerson);

    Result := vForm.ShowModal = mrOk;

    if Result then
    begin
      if vForm.FIsNew then
        APerson := TPerson.Create;

      vForm.ReadFromForm(APerson);
    end;
  finally
    vForm.Free;
  end;
end;

procedure TFrm_Person.ReadFromForm(APerson: TPerson);
begin
  APerson.id := StrToIntDef(edId.Text, 0);
  APerson.name := edName.Text;
  APerson.email := edEmail.Text;
  APerson.createDate := Now;
end;

end.
