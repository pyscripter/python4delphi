program PyBufferDemo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Variants,
  PythonEngine,
  VarPyth;

var
  PythonEngine: TPythonEngine;

procedure CreatePyEngine;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.Name := 'PythonEngine';
  PythonEngine.LoadDll;
end;

procedure DestroyEngine;
begin
  PythonEngine.Free;
end;

const
  N = 100000;

type
 PIntArray = ^TIntArray;
 TIntArray = array[0..N - 1] of Integer;

var
  SW: TStopwatch;
  Sum: Int64;
  np: Variant;
  arr: Variant;
  np_arr: PPyObject;
  PyBuffer: Py_buffer;
  V: Variant;
  I: Integer;
begin
  try
    CreatePyEngine;
    try
      // Import numpy and create an array
      np := Import('numpy');
      arr := np.array(BuiltinModule.range(N));

      // This is the slow way to iterate the array
      WriteLn('Lazy but slow:');
      SW := TStopwatch.StartNew;
      Sum := 0;
      for V in VarPyIterate(arr) do
        Sum := Sum + BuiltinModule.int(V);
      SW.Stop;
      WriteLn(Format('Sum from 0 to %d = %d', [N, Sum]));
      WriteLn('Elapsed ms: ' + SW.ElapsedMilliseconds.ToString);
      WriteLn;

      WriteLn('Using Py_Buffer:');
      SW := TStopwatch.StartNew;
      np_arr := ExtractPythonObjectFrom(arr);
      PythonEngine.PyObject_GetBuffer(np_arr, @PyBuffer, PyBUF_CONTIG);
      PythonEngine.CheckError;
      try
        Sum := 0;
        for I := 0 to N - 1 do
          Sum := Sum + PIntArray(PyBuffer.buf)^[I];
        SW.Stop;
        WriteLn(Format('Sum from 0 to %d = %d', [N, Sum]));
      finally
        PythonEngine.PyBuffer_Release(@PyBuffer);
      end;
      WriteLn('Elapsed ms: ' + SW.ElapsedMilliseconds.ToString);
      WriteLn;

      // test write access
      PIntArray(PyBuffer.buf)^[0] := 999;
      if BuiltinModule.int(arr.GetItem(0)) = 999 then
        WriteLn('Successfully modified the numpy array using Py_buffer');
    finally
      DestroyEngine;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
