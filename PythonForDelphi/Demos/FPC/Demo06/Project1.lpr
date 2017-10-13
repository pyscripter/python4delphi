program Project1;

{$MODE Delphi}

uses
{$IFDEF MSWINDOWS}
  Forms, Interfaces,
{$ENDIF}
{$IFDEF LINUX}
  Forms, Interfaces,
{$ENDIF}
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
