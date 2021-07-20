program P4DAndroidTest;
uses
  System.StartUpCopy,
  FMX.Forms,
  DUNitX.Loggers.MobileGUI,
  MethodCallBackTest in 'MethodCallBackTest.pas',
  NumberServicesTest in 'NumberServicesTest.pas',
  PyEnvTest in 'PyEnvTest.pas',
  PythonLoad in 'PythonLoad.pas',
  VarPythTest in 'VarPythTest.pas',
  WrapDelphiTest in 'WrapDelphiTest.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TMobileGUITestRunner, MobileGUITestRunner);
  Application.Run;
end.
