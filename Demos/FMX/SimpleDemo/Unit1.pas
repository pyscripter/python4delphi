unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  PythonEngine, PythonVersions, System.SyncObjs, FMX.Memo.Types;

type
  TForm1 = class(TForm)
    memoIn: TMemo;
    PythonEngine: TPythonEngine;
    Splitter1: TSplitter;
    memoOut: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    PythonInputOutput: TPythonInputOutput;
    procedure Button1Click(Sender: TObject);
    procedure PythonInputOutputSendUniData(Sender: TObject; const Data: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FOutputStream: TMemoryStream;
    FCriticalSection : TCriticalSection;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine.ExecString(UTF8Encode(MemoIn.Text));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FOutputStream := TMemoryStream.Create;
  FCriticalSection := TCriticalSection.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FOutputStream.Free;
  FCriticalSection.Free;
end;

procedure TForm1.PythonInputOutputSendUniData(Sender: TObject;
  const Data: string);
{
    FMX TMemo is slow when adding many lines one by one.
    To avoid this we accumulate output and print it in one go.
    The critical section allows for multiple threads to print.
    If you use this code then set PythonInputOuput.RawOutput to True

    A simpler solution would be to set RawOuput to False and then do

    MemoOut.Lines.Add(Data);
    MemoOut.GoToTextEnd;

    This should be good enough if python does not produce a massive
    amount of output.
}
Var
  ScheduleWrite: Boolean;
begin
  if Data = '' then Exit;

  fCriticalSection.Enter;
  try
    ScheduleWrite := FOutputStream.Size = 0;
    FOutputStream.Write(Data[1], Length (Data) * 2);

    if ScheduleWrite then

    TThread.ForceQueue(nil, procedure
      var
        WS: string;
      begin
        fCriticalSection.Enter;
        try
          if fOutputStream.Size > 0 then begin
            SetLength(WS, fOutputStream.Size div 2);
            fOutputStream.Position := 0;
            fOutputStream.Read(WS[1], Length(WS) * 2);
            fOutputStream.Size := 0;
            if (MemoOut.Lines.Count > 0) and (MemoOut.Lines[MemoOut.Lines.Count -1] <> '') then
            MemoOut.BeginUpdate;
            try
              MemoOut.Lines.Add('');
              MemoOut.Text := MemoOut.Text + WS;
              MemoOut.GoToTextEnd;
            finally
              MemoOut.EndUpdate;
            end;
          end;
        finally
          fCriticalSection.Leave;
        end;
      end);
  finally
    fCriticalSection.Leave;
  end;
end;

end.
