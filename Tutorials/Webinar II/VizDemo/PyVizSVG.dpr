// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program PyVizSVG;

uses
  Vcl.Forms,
  MainFormSVG in 'MainFormSVG.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
