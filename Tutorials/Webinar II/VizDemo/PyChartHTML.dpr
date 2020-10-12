// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program PyChartHTML;

uses
  Vcl.Forms,
  MainFormHTML in 'MainFormHTML.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
