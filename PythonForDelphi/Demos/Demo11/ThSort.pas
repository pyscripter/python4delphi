unit ThSort;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,
  PythonEngine;

type
  TThreadSortForm = class(TForm)
    StartBtn: TButton;
    BubbleSortBox: TPaintBox;
    SelectionSortBox: TPaintBox;
    QuickSortBox: TPaintBox;
    Label1: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    PythonMemo: TMemo;
    PythonEngine1: TPythonEngine;
    Start2Btn: TButton;
    LoadBtn: TButton;
    PythonDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SaveBtn: TButton;
    SortModule: TPythonModule;
    procedure BubbleSortBoxPaint(Sender: TObject);
    procedure SelectionSortBoxPaint(Sender: TObject);
    procedure QuickSortBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure Start2BtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SortModuleInitialization(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    OwnThreadState: PPyThreadState;
    ThreadsRunning: Integer;
    procedure RandomizeArrays;
    procedure ThreadDone(Sender: TObject);
    procedure InitThreads(ThreadExecMode: TThreadExecMode; script: TStrings);

    function SortModule_GetValue( pself, args : PPyObject ) : PPyObject; cdecl;
    function SortModule_Swap( pself, args : PPyObject ) : PPyObject; cdecl;
  public
    procedure PaintArray(Box: TPaintBox; const A: array of Integer);
  end;

var
  ThreadSortForm: TThreadSortForm;

implementation

uses SortThds;

{$R *.dfm}

type
  PSortArray = ^TSortArray;
  TSortArray =  array[0..176] of Integer;

var
  ArraysRandom: Boolean;
  BubbleSortArray, SelectionSortArray, QuickSortArray: TSortArray;

{ TThreadSortForm }

procedure TThreadSortForm.PaintArray(Box: TPaintBox; const A: array of Integer);
var
  I: Integer;
begin
  with Box do
  begin
    Canvas.Pen.Color := clRed;
    for I := Low(A) to High(A) do PaintLine(Canvas, I, A[I]);
  end;
end;

procedure TThreadSortForm.BubbleSortBoxPaint(Sender: TObject);
begin
  PaintArray(BubbleSortBox, BubbleSortArray);
end;

procedure TThreadSortForm.SelectionSortBoxPaint(Sender: TObject);
begin
  PaintArray(SelectionSortBox, SelectionSortArray);
end;

procedure TThreadSortForm.QuickSortBoxPaint(Sender: TObject);
begin
  PaintArray(QuickSortBox, QuickSortArray);
end;

procedure TThreadSortForm.FormCreate(Sender: TObject);
begin
  RandomizeArrays;
end;

procedure TThreadSortForm.InitThreads(ThreadExecMode: TThreadExecMode; script: TStrings);
begin
  RandomizeArrays;
  ThreadsRunning := 3;
  with GetPythonEngine do
  begin
    OwnThreadState := PyEval_SaveThread;

    with TSortThread.Create( ThreadExecMode, script, SortModule, 'SortFunc1',
                             BubbleSortBox, BubbleSortArray) do
      OnTerminate := ThreadDone;
    with TSortThread.Create( ThreadExecMode, script, SortModule, 'SortFunc2',
                                SelectionSortBox, SelectionSortArray) do
      OnTerminate := ThreadDone;
    with TSortThread.Create( ThreadExecMode, script, SortModule, 'SortFunc3',
                            QuickSortBox, QuickSortArray) do
      OnTerminate := ThreadDone;
  end;

  StartBtn.Enabled := False;
  Start2Btn.Enabled := False;
end;

procedure TThreadSortForm.Start2BtnClick(Sender: TObject);
begin
  with GetPythonEngine do
  begin
    ExecStrings(PythonMemo.Lines);
    self.InitThreads(emNewState,nil);
  end;
end;

procedure TThreadSortForm.StartBtnClick(Sender: TObject);
begin
  InitThreads(emNewInterpreter,PythonMemo.Lines);
//PythonEngine1.ExecStrings(PythonMemo.Lines);
end;

procedure TThreadSortForm.LoadBtnClick(Sender: TObject);
begin
  with PythonDialog do if Execute then
    PythonMemo.Lines.LoadFromFile(FileName);
end;

procedure TThreadSortForm.SaveBtnClick(Sender: TObject);
begin
  with SaveDialog do if Execute then
    PythonMemo.Lines.SaveToFile(FileName);
end;

procedure TThreadSortForm.RandomizeArrays;
var
  I: Integer;
begin
  if not ArraysRandom then
  begin
    Randomize;
    for I := Low(BubbleSortArray) to High(BubbleSortArray) do
      BubbleSortArray[I] := Random(170);
    SelectionSortArray := BubbleSortArray;
    QuickSortArray := BubbleSortArray;
    ArraysRandom := True;
    Repaint;
  end;
end;

procedure TThreadSortForm.ThreadDone(Sender: TObject);
begin
  Dec(ThreadsRunning);
  if ThreadsRunning = 0 then
  begin
    GetPythonEngine.PyEval_RestoreThread(OwnThreadState);
    StartBtn.Enabled := True;
    Start2Btn.Enabled := True;
    ArraysRandom := False;
  end;
end;

function TThreadSortForm.SortModule_GetValue( pself, args : PPyObject ) : PPyObject; cdecl;
var psort,index: integer;
begin
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple( args, 'ii',@psort, @index) <> 0 then
    begin
      Result := PyInt_FromLong(TSortThread(psort)[index]);
    end else
      Result := nil;
  end;
end;

function TThreadSortForm.SortModule_Swap( pself, args : PPyObject ) : PPyObject; cdecl;
var psort,i,j: integer;
begin
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple( args, 'iii',@psort, @i, @j) <> 0 then
    begin
      TSortThread(psort).VisualSwap(i,j);
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

procedure TThreadSortForm.SortModuleInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'getvalue',
                       SortModule_GetValue,
                       'GetValue(handle,index)' );
      AddDelphiMethod( 'swap',
                       SortModule_Swap,
                       'swap(handle,index1,index2)');
    end;
end;

procedure TThreadSortForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := ThreadsRunning = 0;
end;

end.
