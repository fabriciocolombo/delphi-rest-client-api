program UnitTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestHelloWorld in 'TestHelloWorld.pas',
  TestPeople in 'TestPeople.pas',
  BaseTestRest in 'BaseTestRest.pas',
  TestHeader in 'TestHeader.pas',
  TestDBXJson in 'TestDBXJson.pas';

{$R *.RES}

begin
  Application.Initialize;

//  ReportMemoryLeaksOnShutdown := True;

  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

