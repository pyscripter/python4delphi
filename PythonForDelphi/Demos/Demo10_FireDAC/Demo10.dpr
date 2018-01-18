program Demo10;

uses
  Forms,
  fmMain in 'fmMain.pas' {Main},
  PythonEngine in '..\..\Components\Sources\Core\PythonEngine.pas',
  WrapDelphiClasses in '..\..\Components\Sources\Core\WrapDelphiClasses.pas',
  pyDBFireDac in '..\..\Components\Sources\FireDAC\pyDBFireDac.pas',
  WrapDelphi in '..\..\Components\Sources\Core\WrapDelphi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
