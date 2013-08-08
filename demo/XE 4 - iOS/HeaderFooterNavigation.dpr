program HeaderFooterNavigation;

uses
  FMX.Forms,
  Graphics,
  HeaderFooterFormwithNavigation in 'HeaderFooterFormwithNavigation.pas' {HeaderFooterwithNavigation},
  uDM in '..\Common\uDM.pas' {DM: TDataModule},
  Person in '..\Common\Person.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THeaderFooterwithNavigation, HeaderFooterwithNavigation);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
