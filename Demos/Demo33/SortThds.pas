unit SortThds;

{$I Definition.Inc}

interface

uses
  Classes,
  Graphics, ExtCtrls, Forms,
  PythonEngine;

type

{ TSortThread }

  PSortArray = ^TSortArray;
  TSortArray = array[0..MaxInt div SizeOf(Integer) - 1] of Integer;

  TSortThread = class(TPythonThread)
  private
    FModule: TPythonModule;
    FScript: TStrings;
    FBox: TPaintBox;
    FSortArray: PSortArray;
    FSize: Integer;
    FI, FJ: Integer;
    fpyfuncname: string;
    running : Boolean;

    procedure DoVisualSwap;

    function  getvalue(i: integer): integer;
  protected
    procedure ExecuteWithPython; override;
  public
    property value[i: integer]: integer read getvalue; default;

    constructor Create( AThreadExecMode: TThreadExecMode; script: TStrings;
                        module: TPythonModule; apyfuncname: string;
                        Box: TPaintBox; var SortArray: array of Integer);

    procedure VisualSwap(I, J: Integer);
    procedure Stop;
  end;

procedure PaintLine(Canvas: TCanvas; I, Len: Integer);

implementation

procedure PaintLine(Canvas: TCanvas; I, Len: Integer);
begin
  Canvas.PolyLine([Point(0, I * 2 + 1), Point(Len, I * 2 + 1)]);
end;

{ TSortThread }

constructor TSortThread.Create( AThreadExecMode: TThreadExecMode; script: TStrings;
                                module: TPythonModule; apyfuncname: string;
                                Box: TPaintBox; var SortArray: array of Integer);
begin
  fpyfuncname := apyfuncname;
  fScript := script;
  FModule := module;
  FBox := Box;
  FSortArray := @SortArray;
  FSize := High(SortArray) - Low(SortArray) + 1;
  FreeOnTerminate := True;
  ThreadExecMode := AThreadExecMode;
  inherited Create(False);
end;



{ Since DoVisualSwap uses a VCL component (i.e., the TPaintBox) it should never
  be called directly by this thread.  DoVisualSwap should be called by passing
  it to the Synchronize method which causes DoVisualSwap to be executed by the
  main VCL thread, avoiding multi-thread conflicts. See VisualSwap for an
  example of calling Synchronize. }

procedure TSortThread.DoVisualSwap;
type
  pinteger = ^integer;
var t: integer;
    pi,pj: pinteger;
begin
  Application.ProcessMessages;
  with FBox do
  begin
    Canvas.Pen.Color := clBtnFace;
    pi := @(FSortArray^[FI]);
    pj := @(FSortArray^[FJ]);
    PaintLine(Canvas, FI, pi^);
    PaintLine(Canvas, FJ, pj^);
    Canvas.Pen.Color := clRed;
    PaintLine(Canvas, FI, pj^);
    PaintLine(Canvas, FJ, pi^);

    t   := pi^;
    pi^ := pj^;
    pj^ := t;
  end;
end;

{ VisusalSwap is a wrapper on DoVisualSwap making it easier to use.  The
  parameters are copied to instance variables so they are accessable
  by the main VCL thread when it executes DoVisualSwap }

procedure TSortThread.VisualSwap(I, J: Integer);
begin
  Py_BEGIN_ALLOW_THREADS;
  if Terminated then
    raise EPythonError.Create( 'Pythonthread terminated');
  FI := I;
  FJ := J;
  Synchronize(DoVisualSwap);
  Py_END_ALLOW_THREADS;
end;

{ The Execute method is called when the thread starts }

procedure TSortThread.ExecuteWithPython;
var pyfunc: PPyObject;
begin
  running := true;
  try
    with GetPythonEngine do
    begin
      if Assigned(FModule) and (ThreadExecMode = emNewInterpreter) then
        FModule.InitializeForNewInterpreter;
      if Assigned(fScript) then
        ExecStrings(fScript);
      pyfunc :=  FindFunction( ExecModule, fpyfuncname);
      if Assigned(pyfunc) then
        try
          EvalFunction(pyfunc,[NativeInt(self),0,FSize]);
        except
        end;

        Py_DecRef(pyfunc);
    end;
  finally
    running := false;
  end;
end;

procedure TSortThread.Stop;
begin
  with GetPythonEngine do
  begin
    if running then
    begin
      PyEval_AcquireThread(self.ThreadState);
      PyErr_SetString(PyExc_KeyboardInterrupt^, 'Terminated');
      PyEval_ReleaseThread(self.ThreadState);
    end;
  end;
end;

function TSortThread.getvalue(i: integer): integer;
begin
  if Terminated then
    raise EPythonError.Create( 'Pythonthread terminated');
  Result := FSortArray^[i];
end;

end.
