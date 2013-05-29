program UnitTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{.$DEFINE CONSOLE_TESTRUNNER}

{$IFDEF CONSOLE_TESTRUNNER}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$I DelphiRest.inc}

uses
  {$IFDEF DELPHI_7}
  FastMM4,
  {$ENDIF}
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestHelloWorld in 'TestHelloWorld.pas',
  TestPeople in 'TestPeople.pas',
  BaseTestRest in 'BaseTestRest.pas',
  TestHeader in 'TestHeader.pas',
  TestResponseHandler in 'TestResponseHandler.pas',
  TestDataSetHandler in 'TestDataSetHandler.pas',
  Person in 'Person.pas',
  DataSetUtils in '..\src\DataSetUtils.pas',
  RestClient in '..\src\RestClient.pas',
  HttpConnection in '..\src\HttpConnection.pas',
  HttpConnectionFactory in '..\src\HttpConnectionFactory.pas',
  WinHttp_TLB in '..\lib\WinHttp_TLB.pas',
  TestRegister in 'TestRegister.pas',
  TestRestClient in 'TestRestClient.pas',
  Wcrypt2 in '..\lib\Wcrypt2.pas';

{$R *.RES}

begin
  Application.Initialize;

  {$IFNDEF DELPHI_7}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

