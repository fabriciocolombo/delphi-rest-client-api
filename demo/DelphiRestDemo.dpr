program DelphiRestDemo;

{$I ..\src\DelphiRest.inc}

uses
  Forms;

{$R *.res}

begin
  Application.Initialize;

  {$IFDEF D2007_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.Run;
end.
