program Project1;

{.$I Definition.Inc}
uses
  Interfaces,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  WrapDelphiTypes in '..\..\Components\Sources\Core\WrapDelphiTypes.pas',
  Unit2 in 'Unit2.pas' {TestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
