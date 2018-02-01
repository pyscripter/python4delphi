program Demo10;

uses
  Forms,
  fmMain in 'fmMain.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
