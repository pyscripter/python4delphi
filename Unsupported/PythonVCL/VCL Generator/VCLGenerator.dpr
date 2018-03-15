program VCLGenerator;

uses
  Forms,
  fmMain in 'fmMain.pas' {MainForm},
  unUnitParser in 'unUnitParser.pas',
  unParser in 'unParser.pas',
  fmAbout in 'fmAbout.pas' {AboutBox},
  fmOptions in 'fmOptions.pas' {Options},
  fmSettings in 'fmSettings.pas' {Settings},
  unHash in 'unHash.pas',
  Py_Misc in '..\Components\Sources\VCL\Py_Misc.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'VCL Generator';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TOptions, Options);
  Application.CreateForm(TSettings, Settings);
  Application.Run;
end.
