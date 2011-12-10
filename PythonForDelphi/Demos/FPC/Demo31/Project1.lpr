program Project1;

{$I Definition.Inc}
uses
  Interfaces,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  WrapDelphiExtCtrls in '..\..\Components\Sources\Core\WrapDelphiExtCtrls.pas',
  WrapDelphiTypes in '..\..\Components\Sources\Core\WrapDelphiTypes.pas',
  Unit2 in 'Unit2.pas' {TestForm},
  WrapDelphiWindows in '..\..\Components\Sources\Core\WrapDelphiWindows.pas',
  WrapDelphiComCtrls in '..\..\Components\Sources\Core\WrapDelphiComCtrls.pas',
  WrapDelphiGrids in '..\..\Components\Sources\Core\WrapDelphiGrids.pas',
  WrapDelphiGraphics in '..\..\Components\Sources\Core\WrapDelphiGraphics.pas',
  WrapDelphiButtons in '..\..\Components\Sources\Core\WrapDelphiButtons.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
