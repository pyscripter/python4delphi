program Project1;

{$I Definition.Inc}

uses
{$IFDEF MSWINDOWS}
  Forms,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
{$ENDIF}
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
