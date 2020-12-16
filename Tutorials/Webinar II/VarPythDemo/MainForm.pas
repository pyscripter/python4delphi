unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, SynEdit, Vcl.StdCtrls,
  PythonEngine, PythonGUIInputOutput, SynEditPythonBehaviour,
  SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    sePythonCode: TSynEdit;
    HeaderControl1: THeaderControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    HeaderControl2: THeaderControl;
    mePythonOutput: TMemo;
    SynPythonSyn: TSynPythonSyn;
    SynEditPythonBehaviour: TSynEditPythonBehaviour;
    PythonEngine: TPythonEngine;
    PythonGUIInputOutput: TPythonGUIInputOutput;
    btnRun: TButton;
    ListBox: TListBox;
    Splitter2: TSplitter;
    Button1: TButton;
    PythonModule: TPythonModule;
    procedure FormCreate(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.Threading,
  System.Math,
  VarPyth;

procedure TForm1.FormCreate(Sender: TObject);
begin
   var np := Import('numpy');
   var np_array: Variant := np.array(VarPythonCreate([1,2,3,4,5,6,7,8,9,10]));
   PythonModule.SetVar('np_array', ExtractPythonObjectFrom(np_array));
end;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  GetPythonEngine.ExecString(UTF8Encode(sePythonCode.Text));
  for var V in VarPyIterate(MainModule.res_array) do
    ListBox.Items.Add(V);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(SysModule.version);
end;

initialization
  MaskFPUExceptions(True);
  ReportMemoryLeaksOnShutdown := True;
end.
