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
    procedure ClearList;
    procedure RefreshList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    try
      DM.RestClient.Resource(CONTEXT_PATH + 'person')
                              .Accept(RestUtils.MediaType_Json)
                              .Header('Accept-Encoding', 'gzip')
                              .ContentType(RestUtils.MediaType_Json)
                              .Post<TPerson>(vPerson)
                              .Free;
    finally
      vPerson.Free;
    end;
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
                   .Put<TPerson>(vPerson)
                   .Free;

      RefreshList;
    end;
  end;
end;

procedure TFrm_PersonList.chkEnableCompressionClick(Sender: TObject);
begin
  DM.RestClient.EnabledCompression := chkEnableCompression.Checked;
end;

procedure TFrm_PersonList.ClearList;
var
  i: Integer;
begin
  for i := ListView1.Items.Count-1 downto 0 do
  begin
    TObject(ListView1.Items[i].Data).Free;
  end;
  ListView1.Items.Clear;
end;

constructor TFrm_PersonList.Create(AOwner: TComponent);
begin
  inherited;
  chkEnableCompression.Checked := DM.RestClient.EnabledCompression;
end;

destructor TFrm_PersonList.Destroy;
begin
  ClearList;
  inherited;
end;

procedure TFrm_PersonList.RefreshList;
var
  vResponse: TList<TPerson>;
  vPerson: TPerson;
begin
  ClearList;

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
