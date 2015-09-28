program UnitTest;

{$APPTYPE CONSOLE}

{$I DelphiRest.inc}

(*
  {$IFDEF DELPHI_7}
  FastMM4,
  {$ENDIF}
*)

uses
  Forms,
  GUITestRunner,
  TextTestRunner,
  SysUtils,
  TestHelloWorld in 'TestHelloWorld.pas',
  TestPeople in 'TestPeople.pas',
  BaseTestRest in 'BaseTestRest.pas',
  TestHeader in 'TestHeader.pas',
  TestResponseHandler in 'TestResponseHandler.pas',
  Person in 'Person.pas',
  DataSetUtils in '..\src\DataSetUtils.pas',
  RestClient in '..\src\RestClient.pas',
  HttpConnection in '..\src\HttpConnection.pas',
  HttpConnectionFactory in '..\src\HttpConnectionFactory.pas',
  WinHttp_TLB in '..\lib\WinHttp_TLB.pas',
  TestRegister in 'TestRegister.pas',
  TestRestClient in 'TestRestClient.pas',
  Wcrypt2 in '..\lib\Wcrypt2.pas',
  TestOldRttiMarshal in 'TestOldRttiMarshal.pas',
  OldRttiMarshal in '..\src\OldRttiMarshal.pas',
  TestOldRttiUnmarshal in 'TestOldRttiUnmarshal.pas',
  OldRttiUnMarshal in '..\src\OldRttiUnMarshal.pas',
  JsonListAdapter in '..\src\JsonListAdapter.pas',
  TestDbxJsonUnMarshal in 'TestDbxJsonUnMarshal.pas',
  DbxJsonUnMarshal in '..\src\DbxJsonUnMarshal.pas',
  TestDBXJsonUtils in 'TestDBXJsonUtils.pas',
  TestDbxJsonMarshal in 'TestDbxJsonMarshal.pas',
  TestRestUtils in 'TestRestUtils.pas';

{$R *.RES}

begin
  Application.Initialize;

  {$IFNDEF DELPHI_7}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  if FindCmdLineSwitch('text', ['-', '/'], True) then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

