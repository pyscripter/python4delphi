program PyLoad;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {PyMainForm},
  PythonLoad in 'PythonLoad.pas',
  ProgressFrame in 'ProgressFrame.pas' {ProgressViewFrame: TFrame},
  AppEnvironment in 'AppEnvironment.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPyMainForm, PyMainForm);
  Application.Run;
end.
