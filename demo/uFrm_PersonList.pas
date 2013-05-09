unit uFrm_PersonList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, RestUtils, Generics.Collections;

type
  TFrm_PersonList = class(TForm)
    ListView1: TListView;
    btnAdd: TButton;
    btnUpdate: TButton;
    btnRemove: TButton;
    btnRefresh: TButton;
    btnReset: TButton;
    chkEnableCompression: TCheckBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chkEnableCompressionClick(Sender: TObject);
  private
    procedure RefreshList;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Frm_PersonList: TFrm_PersonList;

implementation

uses Person, uFrm_Person, uDM;

{$R *.dfm}

procedure TFrm_PersonList.btnAddClick(Sender: TObject);
var
  vPerson: TPerson;
begin
  vPerson := nil;

  if TFrm_Person.Modify(vPerson) then
  begin
    DM.RestClient.Resource(CONTEXT_PATH + 'person')
                 .Accept(RestUtils.MediaType_Json)
                 .ContentType(RestUtils.MediaType_Json)
                 .Post<TPerson>(vPerson);

    RefreshList;
  end;
end;

procedure TFrm_PersonList.btnRefreshClick(Sender: TObject);
begin
  RefreshList;
end;

procedure TFrm_PersonList.btnRemoveClick(Sender: TObject);
var
  vPerson: TPerson;
begin
  if ListView1.ItemIndex >= 0 then
  begin
    vPerson := TPerson(ListView1.Items[ListView1.ItemIndex].Data);

    DM.RestClient.Resource(CONTEXT_PATH + 'person')
                 .Accept(RestUtils.MediaType_Json)
                 .ContentType(RestUtils.MediaType_Json)
                 .Delete(vPerson);

    RefreshList;
  end;
end;

procedure TFrm_PersonList.btnResetClick(Sender: TObject);
begin
  DM.RestClient.Resource(CONTEXT_PATH + 'persons/reset').GET();

  RefreshList;
end;

procedure TFrm_PersonList.btnUpdateClick(Sender: TObject);
var
  vPerson: TPerson;
begin
  if ListView1.ItemIndex >= 0 then
  begin
    vPerson := TPerson(ListView1.Items[ListView1.ItemIndex].Data);

    if TFrm_Person.Modify(vPerson) then
    begin
      DM.RestClient.Resource(CONTEXT_PATH + 'person')
                   .Accept(RestUtils.MediaType_Json)
                   .ContentType(RestUtils.MediaType_Json)
                   .Put<TPerson>(vPerson);

      RefreshList;
    end;
  end;
end;

procedure TFrm_PersonList.chkEnableCompressionClick(Sender: TObject);
begin
  DM.RestClient.EnabledCompression := chkEnableCompression.Checked;
end;

constructor TFrm_PersonList.Create(AOwner: TComponent);
begin
  inherited;
  chkEnableCompression.Checked := DM.RestClient.EnabledCompression;
end;

procedure TFrm_PersonList.RefreshList;
var
  vResponse: TList<TPerson>;
  vPerson: TPerson;
begin
  ListView1.Items.Clear;

  vResponse := Dm.RestClient.Resource(CONTEXT_PATH + 'persons')
                            .Accept(RestUtils.MediaType_Json)
                            .Get<TList<TPerson>>();
  try
    for vPerson in vResponse do
    begin
      with ListView1.Items.Add do
      begin
        Caption := IntToStr(vPerson.id);
        Data := vPerson;
        SubItems.Add(vPerson.name);
        SubItems.Add(vPerson.email);
        SubItems.Add(DateTimeToStr(vPerson.createDate));
      end;
    end;
  finally
    vResponse.Free;
  end;
end;

end.
