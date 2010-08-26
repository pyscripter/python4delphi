program VarPythUnitTest;

uses
{$IFDEF MSWINDOWS}
  Forms,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
{$ENDIF}
  fmMain in 'fmMain.pas' {TMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
