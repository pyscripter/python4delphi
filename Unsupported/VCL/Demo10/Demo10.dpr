program Demo10;

uses
  Forms,
  fmMain in 'fmMain.pas' {Main},
  pyDBTables in 'pyDBTables.pas',
  pyDB in 'pyDB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
