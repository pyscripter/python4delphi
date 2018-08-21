program VarPythUnitTest;

uses
  Forms,
  fmMain in 'fmMain.pas' {TMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
