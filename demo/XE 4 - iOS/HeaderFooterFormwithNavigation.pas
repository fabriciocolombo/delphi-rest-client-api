unit HeaderFooterFormwithNavigation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  System.Actions, FMX.ActnList, FMX.StdCtrls, FMX.ListBox, FMX.Layouts,
  Generics.Collections, Person, uDM, RestUtils, FMX.ListView.Types, FMX.ListView,
  FMX.Grid, FMX.Edit, RestClient;

type
  THeaderFooterwithNavigation = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TopToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TopToolBar1: TToolBar;
    ToolBarLabel1: TLabel;
    ActionList1: TActionList;
    acDetailTab: TChangeTabAction;
    acBackTab: TChangeTabAction;
    ActionList2: TActionList;
    StringGrid1: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    Label1: TLabel;
    lblId: TLabel;
    Label2: TLabel;
    lblCreateDate: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edName: TClearingEdit;
    edEmail: TClearingEdit;
    BottomToolBar: TToolBar;
    btnRefresh: TSpeedButton;
    btnAdd: TSpeedButton;
    btnUpdate: TSpeedButton;
    btnRemove: TSpeedButton;
    btnReset: TSpeedButton;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    FPeoples: TList<TPerson>;
    FSelectedPerson: TPerson;

    procedure RefreshList;
    procedure ClearGrid;

    procedure PopulateFields;
    procedure ReadFromForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  HeaderFooterwithNavigation: THeaderFooterwithNavigation;

implementation

{$R *.fmx}
procedure THeaderFooterwithNavigation.btnAddClick(Sender: TObject);
begin
  PopulateFields;

  acDetailTab.ExecuteTarget(TabItem2);
end;

procedure THeaderFooterwithNavigation.btnRefreshClick(Sender: TObject);
begin
  RefreshList;
end;

procedure THeaderFooterwithNavigation.btnRemoveClick(Sender: TObject);
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

procedure THeaderFooterwithNavigation.btnResetClick(Sender: TObject);
begin
  DM.RestClient.Resource(CONTEXT_PATH + 'persons/reset').GET();

  RefreshList;
end;

procedure THeaderFooterwithNavigation.btnUpdateClick(Sender: TObject);
begin
  if StringGrid1.Selected >= 0 then
  begin
    FSelectedPerson := TPerson(FPeoples.Items[StringGrid1.Selected]);

    PopulateFields;

    acDetailTab.ExecuteTarget(TabItem2);
  end;
end;

procedure THeaderFooterwithNavigation.ClearGrid;
var
  i, j: Integer;
begin
  FPeoples.Clear;
  FSelectedPerson := nil;

  for i := 0 to StringGrid1.ColumnCount-1 do
    for j := 0 to StringGrid1.RowCount-1 do
      StringGrid1.Cells[i,j] := '';
end;

constructor THeaderFooterwithNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FPeoples := TObjectList<TPerson>.Create();

  TabControl1.ActiveTab := TabItem1;
end;

destructor THeaderFooterwithNavigation.Destroy;
begin
  FPeoples.Free;
  inherited;
end;

procedure THeaderFooterwithNavigation.PopulateFields;
begin
  lblId.Text := '';
  edName.Text := '';
  edEmail.Text := '';
  lblCreateDate.Text := '';

  if FSelectedPerson <> nil then
  begin
    lblId.Text := IntToStr(FSelectedPerson.id);
    edName.Text := FSelectedPerson.name;
    edEmail.Text := FSelectedPerson.email;
    lblCreateDate.Text := DateTimeToStr(FSelectedPerson.createDate);
  end;
end;

procedure THeaderFooterwithNavigation.ReadFromForm;
begin
  Assert(FSelectedPerson <> nil, 'No selected person');

  FSelectedPerson.name := edName.Text;
  FSelectedPerson.email := edEmail.Text;

  if FSelectedPerson.createDate = 0 then
  begin
    FSelectedPerson.createDate := Now;
  end;
end;

procedure THeaderFooterwithNavigation.RefreshList;
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
    for i := 0 to vResponse.Count-1 do
    begin
      vPerson := vResponse[i];

      StringGrid1.Cells[0, i] := IntToStr(vPerson.id);
      StringGrid1.Cells[1, i] := vPerson.name;
      StringGrid1.Cells[2, i] := vPerson.email;
      StringGrid1.Cells[3, i] := DateTimeToStr(vPerson.createDate);
    end;

    FPeoples.Clear;
    FPeoples.AddRange(vResponse.ToArray);
  finally
    vResponse.OwnsObjects := False;
    vResponse.Free;
  end;
end;

procedure THeaderFooterwithNavigation.SpeedButton1Click(Sender: TObject);
var
  vNew: Boolean;
  vResource: TResource;
begin
  vNew := (FSelectedPerson = nil);

  if vNew then
    FSelectedPerson := TPerson.Create;
  try
    ReadFromForm;

    vResource := DM.RestClient.Resource(CONTEXT_PATH + 'person')
                              .Accept(RestUtils.MediaType_Json)
                              .ContentType(RestUtils.MediaType_Json);

    if vNew then
      vResource.Post<TPerson>(FSelectedPerson).Free
    else
      vResource.Put<TPerson>(FSelectedPerson).Free;

    RefreshList;

    acBackTab.ExecuteTarget(nil);
  finally
    FSelectedPerson.Free;
  end;
end;

end.
