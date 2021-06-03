program P4DAndroidTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  DUNitX.Loggers.MobileGUI,
  PythonLoad in 'PythonLoad.pas',
  PyEnv in 'PyEnv.pas',
  VarPythTest in 'VarPythTest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMobileGUITestRunner, MobileGUITestRunner);
  Application.Run;
end.
