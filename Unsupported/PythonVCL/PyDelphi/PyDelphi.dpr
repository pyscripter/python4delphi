program PyDelphi;

uses
  Forms,
  Main in 'MAIN.PAS' {MainForm},
  Childwin in 'CHILDWIN.PAS' {MDIChild},
  fmOutput in 'fmOutput.pas' {OutputForm},
  fmTraceBack in 'fmTraceBack.pas' {TraceBack},
  fmAbout in 'fmAbout.pas' {AboutBox},
  fmFind in 'fmFind.pas' {FormFind},
  Misc in 'Misc.pas',
  fmReplace in 'fmReplace.pas' {FormReplace},
  qrPrintText in 'qrPrintText.pas' {PrintText},
  MRUFList in 'Components used\Mruflist.pas';

{$R *.RES}

begin
  Application.Title := 'Python for Delphi';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOutputForm, OutputForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TFormFind, FormFind);
  Application.CreateForm(TFormReplace, FormReplace);
  Application.CreateForm(TPrintText, PrintText);
  Application.Run;
end.
