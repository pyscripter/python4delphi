unit Unit1;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QStdCtrls, QControls, QExtCtrls,
{$ENDIF}
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    Memo1: TMemo;
    PythonEngine1: TPythonEngine;
    Panel1: TPanel;
    Button1: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( memo1.Lines );
end;


end.
