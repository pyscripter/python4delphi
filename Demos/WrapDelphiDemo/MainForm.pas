unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, SynEdit, Vcl.StdCtrls,
  PythonEngine, PythonGUIInputOutput, SynEditPythonBehaviour,
  SynEditHighlighter, SynEditCodeFolding, SynHighlighterPython, Vcl.ExtCtrls,
  WrapDelphi;

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
    PyDelphiWrapper: TPyDelphiWrapper;
    PythonModule: TPythonModule;
    procedure FormCreate(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
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
  System.Rtti,
  System.Threading,
  System.Math;

type
  TDelphiFunctions = record
    class function count_primes(MaxN: integer): integer; static;
  end;

var
  DelphiFunctions: TDelphiFunctions;


procedure TForm1.FormCreate(Sender: TObject);
begin
  var Py := PyDelphiWrapper.WrapRecord(@DelphiFunctions, TRttiContext.Create.GetType(TypeInfo(TDelphiFunctions)) as TRttiStructuredType);
  PythonModule.SetVar('delphi_functions', Py);
  PythonEngine.Py_DecRef(Py);
end;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  GetPythonEngine.ExecString(UTF8Encode(sePythonCode.Text));
end;

function IsPrime(x: Integer): Boolean;
begin
  if (x <= 1) then Exit(False);

  var q := Floor(Sqrt(x));
  for var i := 2 to q do
    if (x mod i = 0) then
      Exit(False);
  Exit(True);
end;

class function TDelphiFunctions.count_primes(MaxN: integer): integer;
begin
  var Count := 0;
  TParallel.&For(2, MaxN, procedure(i: integer)
    begin
      if IsPrime(i) then
        AtomicIncrement(Count);
    end);
  Result := Count;
end;

end.
