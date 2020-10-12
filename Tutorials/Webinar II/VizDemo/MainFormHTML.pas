unit MainFormHTML;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WrapDelphi, PythonEngine,
  PythonGUIInputOutput, SynEditPythonBehaviour, SynEditHighlighter,
  SynEditCodeFolding, SynHighlighterPython, Vcl.StdCtrls, Vcl.ExtCtrls, SynEdit,
  SVGIconImage, Vcl.ComCtrls, WebView2, Winapi.ActiveX, Vcl.Edge, Vcl.OleCtrls,
  SHDocVw;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    SynEdit2: TSynEdit;
    Panel1: TPanel;
    Splitter2: TSplitter;
    Panel2: TPanel;
    Memo2: TMemo;
    Button1: TButton;
    SynPythonSyn1: TSynPythonSyn;
    SynEditPythonBehaviour1: TSynEditPythonBehaviour;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonModule: TPythonModule;
    PyDelphiWrapper: TPyDelphiWrapper;
    TabSheet2: TTabSheet;
    Panel3: TPanel;
    Splitter4: TSplitter;
    SynEdit1: TSynEdit;
    Panel4: TPanel;
    Button2: TButton;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    Splitter7: TSplitter;
    Panel7: TPanel;
    Splitter8: TSplitter;
    SynEdit3: TSynEdit;
    Panel8: TPanel;
    Button4: TButton;
    Memo3: TMemo;
    Splitter3: TSplitter;
    Panel5: TPanel;
    EdgeBrowser: TEdgeBrowser;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Script: string;
begin
  case PageControl1.TabIndex of
    0:
      begin
        Script := SynEdit1.Text;
        PythonGUIInputOutput1.Output := Memo1;
      end;
    1:
      begin
        Script := SynEdit2.Text;
        PythonGUIInputOutput1.Output := Memo2;
      end;
    2:
      begin
        Script := SynEdit3.Text;
        PythonGUIInputOutput1.Output := Memo3;
      end;
  end;
  GetPythonEngine.ExecString(Utf8Encode(Script));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  var Py := PyDelphiWrapper.Wrap(EdgeBrowser, soReference);
  PythonModule.SetVar('edge_browser', Py);
  GetPythonEngine.Py_DECREF(Py);
end;

initialization
  MaskFPUExceptions(True);
end.

