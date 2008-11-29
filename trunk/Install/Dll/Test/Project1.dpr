program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  fmDelphiInstall in '..\fmDelphiInstall.pas' {DelphiInstall};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDelphiInstall, DelphiInstall);
  Application.Run;
end.
