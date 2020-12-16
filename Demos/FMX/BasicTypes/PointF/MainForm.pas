unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, PythonEngine, FMX.PythonGUIInputOutput,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, WrapFmxTypes, WrapDelphi;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonModule1: TPythonModule;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PyDelphiWrapper1: TPyDelphiWrapper;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do begin
    if Execute then
      Memo2.Lines.LoadFromFile( FileName );
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with SaveDialog1 do begin
    if Execute then
      Memo2.Lines.SaveToFile(FileName);
  end;
end;

end.
