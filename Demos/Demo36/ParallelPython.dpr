program ParallelPython;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Variants,
  System.SyncObjs,
  PythonEngine,
  VarPyth;

var
  PythonEngine: TPythonEngine;

procedure CreatePyEngine;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.Name := 'PythonEngine';
  PythonEngine.LoadDll;
  TPythonThread.Py_Begin_Allow_Threads;
end;

procedure DestroyEngine;
begin
  TPythonThread.Py_End_Allow_Threads;
  PythonEngine.Free;
end;

const
  N = 5;
  Script =
          'import math'#10 +
          ''#10 +
          'def is_prime(n):'#10 +
          '    """ totally naive implementation """'#10 +
          '    if n <= 1:'#10 +
          '        return False'#10 +
          ''#10 +
          '    q = math.floor(math.sqrt(n))'#10 +
          '    for i in range(2, q + 1):'#10 +
          '        if (n % i == 0):'#10 +
          '            return False'#10 +
          '    return True'#10 +
          ''#10 +
          ''#10 +
          'def count_primes(max_n):'#10 +
          '    res = 0'#10 +
          '    for i in range(2, max_n + 1):'#10 +
          '        if is_prime(i):'#10 +
          '            res += 1'#10 +
          '    return res'#10 +
          ''#10 +
          'print("prime count", count_primes(1000000))'#10 +
          ''#10;

var
  Event: TCountdownEvent;

type
  TPyThread = class(TPythonThread)
  protected
    procedure ExecuteWithPython; override;
  public
    constructor Create(ThreadMode: TThreadExecMode);
  end;

procedure TPyThread.ExecuteWithPython;
begin
  GetPythonEngine.ExecString(Script);
  Event.Signal;
end;

constructor TPyThread.Create(ThreadMode: TThreadExecMode);
begin
  inherited Create;
  ThreadExecMode := ThreadMode;
  FreeOnTerminate := True;
end;

var
  SW: TStopwatch;
  I: Integer;
begin
  try
    CreatePyEngine;
    try
      Event := TCountdownEvent.Create(N);
      WriteLn('Classic Subinterpreter:');
      SW := TStopwatch.StartNew;
      for I := 1 to N do
        TPyThread.Create(emNewInterpreter);
      Event.WaitFor;
      Event.Free;
      SW.Stop;
      WriteLn('Elapsed ms: ' + SW.ElapsedMilliseconds.ToString);
      WriteLn;

      Event := TCountdownEvent.Create(N);
      WriteLn('Subinterpreter with own GIL:');
      SW := TStopwatch.StartNew;
      for I := 1 to N do
        TPyThread.Create(emNewInterpreterOwnGIL);
      Event.WaitFor;
      Event.Free;
      SW.Stop;
      WriteLn('Elapsed ms: ' + SW.ElapsedMilliseconds.ToString);
      WriteLn;
    finally
      Sleep(1000);  // allow some time for the threads to terminate
      DestroyEngine;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.

