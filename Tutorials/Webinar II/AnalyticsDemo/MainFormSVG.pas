unit MainFormSVG;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WrapDelphi, PythonEngine,
  PythonGUIInputOutput, SynEditPythonBehaviour, SynEditHighlighter,
  SynEditCodeFolding, SynHighlighterPython, Vcl.StdCtrls, Vcl.ExtCtrls, SynEdit,
  SVGIconImage, Vcl.ComCtrls, WebView2, Winapi.ActiveX;

type
  TForm1 = class(TForm)
    SynPythonSyn1: TSynPythonSyn;
    SynEditPythonBehaviour1: TSynEditPythonBehaviour;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonModule: TPythonModule;
    PyDelphiWrapper: TPyDelphiWrapper;
    SVGIconImage1: TSVGIconImage;
    SVGIconImage: TSVGIconImage;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    Panel2: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Splitter2: TSplitter;
    Splitter1: TSplitter;
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
  end;
  GetPythonEngine.ExecString(Utf8Encode(Script));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  var Py := PyDelphiWrapper.Wrap(SVGIconImage, soReference);
  PythonModule.SetVar('svg_image', Py);
  GetPythonEngine.Py_DECREF(Py);
end;

initialization
  MaskFPUExceptions(True);
end.

