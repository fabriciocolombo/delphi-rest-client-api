program DelphiRestDemo;

uses
  Forms,
  uFrm_Person in 'uFrm_Person.pas' {Frm_Person},
  uDM in '..\Common\uDM.pas' {DM: TDataModule},
  Person in '..\Common\Person.pas',
  uFrm_PersonList in 'uFrm_PersonList.pas' {Frm_PersonList};

{$R *.res}

begin
  Application.Initialize;

  ReportMemoryLeaksOnShutdown := True;

  Application.CreateForm(TDM, DM);
  Application.CreateForm(TFrm_PersonList, Frm_PersonList);
  Application.Run;
end.
