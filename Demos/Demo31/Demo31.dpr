program Demo31;



uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {TestForm},
  WrapDelphiVclExtCtrls,
  WrapDelphiTypes,
  WrapDelphiWindows,
  WrapDelphiVclComCtrls,
  WrapDelphiVclGrids,
  WrapDelphiVclGraphics,
  WrapDelphiVclButtons,
  WrapDelphiVclDialogs;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTestForm, TestForm);
  Application.Run;
end.
