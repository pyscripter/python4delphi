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
    PythonModule: TPythonModule;
    procedure btnRunClick(Sender: TObject);
    procedure PythonModuleEvents0Execute(Sender: TObject; PSelf, Args: PPyObject;
        var Result: PPyObject);
    procedure PythonModuleEvents1Execute(Sender: TObject; PSelf, Args: PPyObject;
        var Result: PPyObject);
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
  System.Math;

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

function CountPrimes(MaxN: integer): integer;
begin
  var Count := 0;
  TParallel.&For(2, MaxN, procedure(i: integer)
    begin
      if IsPrime(i) then
        AtomicIncrement(Count);
    end);
  Result := Count;
end;

procedure TForm1.PythonModuleEvents0Execute(Sender: TObject; PSelf, Args:
    PPyObject; var Result: PPyObject);
Var
  N: Integer;
begin
   with GetPythonEngine do
     if PyArg_ParseTuple(Args, 'i:delphi_is_prime',@N) <> 0 then
     begin
       if IsPrime(N) then
         Result := PPyObject(Py_True)
       else
         Result := PPyObject(Py_False);
       Py_INCREF(Result);
     end else
       Result := nil;
end;

procedure TForm1.PythonModuleEvents1Execute(Sender: TObject; PSelf, Args:
    PPyObject; var Result: PPyObject);
Var
  N: Integer;
begin
   with GetPythonEngine do
     if PyArg_ParseTuple(Args, 'i:delphi_is_prime',@N) <> 0 then
     begin
       Result := PyLong_FromLong(CountPrimes(N));
       Py_INCREF(Result);
     end else
       Result := nil;
end;

end.
