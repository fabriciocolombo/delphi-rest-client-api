unit uFrm_Person;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.ExtCtrls,
  Person;

type
  TFrm_Person = class(TForm)
    edEmail: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edName: TEdit;
    Label4: TLabel;
    btnCancel: TCornerButton;
    btnOK: TCornerButton;
    lblId: TLabel;
    lblCreateDate: TLabel;
  private
    FIsNew: Boolean;
    procedure Fill(APerson: TPerson);
    procedure ReadFromForm(APerson: TPerson);
  public
    class function Modify(var APerson: TPerson): Boolean;
  end;

var
  Frm_Person: TFrm_Person;

implementation

{$R *.fmx}

{ TFrm_Person }

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
      vForm.ReadFromForm(APerson);
    end;
  finally
    vForm.Free;
  end;
end;

procedure TFrm_Person.Fill(APerson: TPerson);
begin
  FIsNew := (APerson.id = 0);

  if not FIsNew then
  begin
    lblId.Text := IntToStr(APerson.id);
    edName.Text := APerson.name;
    edEmail.Text := APerson.email;
    lblCreateDate.Text := DateTimeToStr(APerson.createDate);
  end;
end;

procedure TFrm_Person.ReadFromForm(APerson: TPerson);
begin
  APerson.name := edName.Text;
  APerson.email := edEmail.Text;

  if APerson.createDate = 0 then
    APerson.createDate := Now;
end;

end.
