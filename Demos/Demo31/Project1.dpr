program Project1;

{$I Definition.Inc}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {TestForm},
  WrapDelphiExtCtrls,
  WrapDelphiTypes,
  WrapDelphiWindows,
  WrapDelphiComCtrls,
  WrapDelphiGrids,
  WrapDelphiGraphics,
  WrapDelphiButtons,
  WrapDelphiDialogs;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
