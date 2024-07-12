program project1;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  Variants,
  PythonEngine,
  VarPyth,
  stopwatch;

var
  PyEngine: TPythonEngine;

procedure CreatePyEngine;
begin
  MaskFPUExceptions(True, True);
  PyEngine := TPythonEngine.Create(nil);
  PyEngine.Name := 'PythonEngine';
  PyEngine.UseLastKnownVersion := False;
  PyEngine.RegVersion:= '3.12';
  PyEngine.DllName:= 'python312.dll';
  PyEngine.LoadDll;
end;

procedure DestroyEngine;
begin
  PyEngine.Free;
end;

const
  N = 100000;

type
   PIntArray = ^TIntArray;
  {$IFDEF MSWINDOWS}
  TIntArray = array[0..N - 1] of Integer;
  {$ELSE}
  TIntArray = array[0..N - 1] of NativeInt;
  {$ENDIF}
 
  { TBufferProtocol }

  TBufferProtocol = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TBufferProtocol }

procedure TBufferProtocol.DoRun;
var
  SW: TStopwatch;
  Sum: Int64;
  np: Variant;
  arr: Variant;
  np_arr: PPyObject;
  PyBuffer: Py_buffer;
  V: Variant;
  I: Integer;
  Count: Integer;
  ArrItem: Variant;
begin
  try
    CreatePyEngine;
    try
      // Import numpy and create an array
      np := Import('numpy');
      arr := np.&array(BuiltinModule.range(N));

      // This is the slow way to iterate the array
      WriteLn('Lazy but slow:');
      SW := TStopwatch.StartNew;
      Sum := 0;
      Count := VarPythonToVariant(arr.Length);
      for I := 0 to Count - 1 do
      begin
        ArrItem := BuiltinModule.int(arr.GetItem(I));
        Sum := Sum + Int64(VarPythonToVariant(ArrItem));
      end;

      //for V in VarPyIterate(arr) do
      //  Sum := Sum + Int64(VarPythonToVariant(BuiltinModule.int(V)));
      SW.Stop;
      WriteLn(Format('Sum from 0 to %d = %d', [N, Sum]));
      WriteLn('Elapsed ms: ' + SW.ElapsedMilliseconds.ToString);
      WriteLn;

      WriteLn('Using Py_Buffer:');
      SW := TStopwatch.StartNew;
      np_arr := ExtractPythonObjectFrom(arr);
      PyEngine.PyObject_GetBuffer(np_arr, @PyBuffer, PyBUF_CONTIG);
      PyEngine.CheckError;
      try
        Sum := 0;
        for I := 0 to N - 1 do
          Sum := Sum + PIntArray(PyBuffer.buf)^[I];
        SW.Stop;
        WriteLn(Format('Sum from 0 to %d = %d', [N, Sum]));
      finally
        PyEngine.PyBuffer_Release(@PyBuffer);
      end;
      WriteLn('Elapsed ms: ' + SW.ElapsedMilliseconds.ToString);
      WriteLn;

      // test write access
      PIntArray(PyBuffer.buf)^[0] := 999;
      if VarPythonToVariant(BuiltinModule.int(arr.GetItem(0))) = 999 then
        WriteLn('Successfully modified the numpy array using Py_buffer');
    finally
      DestroyEngine;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
  // stop program loop
  Terminate;
end;

constructor TBufferProtocol.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBufferProtocol.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TBufferProtocol;
begin
  Application:=TBufferProtocol.Create(nil);
  Application.Title:='BufferProtocol';
  Application.Run;
  Application.Free;
end.

