unit uFrm_PersonList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ExtCtrls, uDM, Person,
  Generics.Collections, RestUtils, FMX.Grid, FMX.Layouts, uFrm_Person;

type
  TFrm_PersonList = class(TForm)
    Panel1: TPanel;
    btnRefresh: TCornerButton;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    btnRemove: TCornerButton;
    btnReset: TCornerButton;
    btnAdd: TCornerButton;
    btnUpdate: TCornerButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
  private
    FPeoples: TObjectList<TPerson>;
  public
    procedure RefreshList;
    procedure ClearGrid;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Frm_PersonList: TFrm_PersonList;

implementation

{$R *.fmx}

{ TForm1 }

procedure TFrm_PersonList.btnAddClick(Sender: TObject);
var
  vPerson: TPerson;
begin
  vPerson := TPerson.Create;
  try
    if TFrm_Person.Modify(vPerson) then
    begin
      DM.RestClient.Resource(CONTEXT_PATH + 'person')
                   .Accept(RestUtils.MediaType_Json)
                   .ContentType(RestUtils.MediaType_Json)
                   .Post<TPerson>(vPerson)
                   .Free;

      RefreshList;
    end;
  finally
    vPerson.Free;
  end;
end;

procedure TFrm_PersonList.btnRefreshClick(Sender: TObject);
begin
  RefreshList;
end;

procedure TFrm_PersonList.btnRemoveClick(Sender: TObject);
var
  vId: Integer;
begin
  if StringGrid1.Selected >= 0 then
  begin
    if TryStrToInt(StringGrid1.Cells[0, StringGrid1.Selected], vId) then
    begin
      DM.RestClient.Resource(CONTEXT_PATH + 'person/' + IntToStr(vId))
                   .Accept(RestUtils.MediaType_Json)
                   .ContentType(RestUtils.MediaType_Json)
                   .Delete();

      RefreshList;
    end;
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
  if StringGrid1.Selected >= 0 then
  begin
    vPerson := TPerson(FPeoples.Items[StringGrid1.Selected]);

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

procedure TFrm_PersonList.ClearGrid;
var
  i, j: Integer;
begin
  FPeoples.Clear;

  for i := 0 to StringGrid1.ColumnCount-1 do
    for j := 0 to StringGrid1.RowCount-1 do
      StringGrid1.Cells[i,j] := '';
end;

constructor TFrm_PersonList.Create(AOwner: TComponent);
begin
  inherited;
  FPeoples := TObjectList<TPerson>.Create;
end;

destructor TFrm_PersonList.Destroy;
begin
  FPeoples.Free;
  inherited;
end;

procedure TFrm_PersonList.RefreshList;
var
  vResponse: TObjectList<TPerson>;
  vPerson: TPerson;
  i: Integer;
begin
  ClearGrid;

  vResponse := Dm.RestClient.Resource(CONTEXT_PATH + 'persons')
                            .Accept(RestUtils.MediaType_Json)
                            .Get<TObjectList<TPerson>>();
  try
    FPeoples.AddRange(vResponse.ToArray);
    for i := 0 to vResponse.Count-1 do
    begin
      vPerson := vResponse[i];

      StringGrid1.Cells[0, i] := IntToStr(vPerson.id);
      StringGrid1.Cells[1, i] := vPerson.name;
      StringGrid1.Cells[2, i] := vPerson.email;
      StringGrid1.Cells[3, i] := DateTimeToStr(vPerson.createDate);
    end;
  finally
    vResponse.Free;
  end;
end;

end.
