unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PythonEngine, StdCtrls, ExtCtrls, ComCtrls, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    Memo1: TMemo;
    PythonEngine1: TPythonEngine;
    Panel1: TPanel;
    Button1: TButton;
    RichEdit1: TRichEdit;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( memo1.Lines );
end;


end.
