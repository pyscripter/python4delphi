program Tests;

uses
  Forms,
  GuiTestRunner,
  MethodCallBackTest in 'MethodCallBackTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  GuiTestRunner.RunRegisteredTests;
end.
