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
    Button1: TButton;
    Button2: TButton;
    procedure btnRunClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  sePythonCode.Text :=
    'from delphi_module import delphi_is_prime'#13#10 + //0
    'from timeit import Timer'#13#10 + //1
    ''#13#10 + //3
    'def count_primes(max_n):'#13#10 + //4
    '    res = 0'#13#10 + //5
    '    for i in range(2, max_n + 1):'#13#10 + //6
    '        if delphi_is_prime(i):'#13#10 + //7
    '            res += 1'#13#10 + //8
    '    return res'#13#10 + //9
    ''#13#10 + //10
    'def test():'#13#10 + //11
    '    max_n = 1000000'#13#10 + //12
    '    print(f''Number of primes between 0 and {max_n} = {count_primes(max_n)}'')'#13#10 + //13
    ''#13#10 + //14
    'def main():'#13#10 + //15
    '    print(f''Elapsed time: {Timer(stmt=test).timeit(1)} secs'')'#13#10 + //16
    ''#13#10 + //17
    'if __name__ == ''__main__'':'#13#10 + //18
    '    main()'#13#10; //19
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  sePythonCode.Text :=
    'from delphi_module import delphi_count_primes'#13#10 + //0
    'from timeit import Timer'#13#10 + //1
    ''#13#10 + //10
    'def test():'#13#10 + //11
    '    max_n = 1000000'#13#10 + //12
    '    print(f''Number of primes between 0 and {max_n} = {delphi_count_primes(max_n)}'')'#13#10 + //13
    ''#13#10 + //14
    'def main():'#13#10 + //15
    '    print(f''Elapsed time: {Timer(stmt=test).timeit(1)} secs'')'#13#10 + //16
    ''#13#10 + //17
    'if __name__ == ''__main__'':'#13#10 + //18
    '    main()'#13#10; //19
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

