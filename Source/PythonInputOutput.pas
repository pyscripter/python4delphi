{$I Definition.Inc}
unit PythonInputOutput;

interface

uses
  System.Classes, System.SyncObjs,
  PythonTypes, PythonConsts;

type
  //-------------------------------------------------------
  //--                                                   --
  //--      class:  TPythonInputOutput                   --
  //--      Works as a console for Python outputs        --
  //--      It's a virtual Base class                    --
  //-------------------------------------------------------

  TPythonInputOutput = class(TComponent)
  protected
    FMaxLines        : Integer;
    FLine_Buffer     : IOString;
    FLinesPerThread  : TIOStringList;
    FLock            : TCriticalSection;
    FQueue           : TIOStringList;
    FDelayWrites     : Boolean;
    FMaxLineLength   : Integer;
    FOnSendData      : TSendDataEvent;
    FOnReceiveData   : TReceiveDataEvent;
    FOnSendUniData   : TSendUniDataEvent;
    FOnReceiveUniData: TReceiveUniDataEvent;
    FUnicodeIO       : Boolean;
    FRawOutput       : Boolean;

    procedure Lock;
    procedure Unlock;
    procedure AddWrite( const str : IOString );
    // Virtual methods for handling the input/output of text
    procedure SendData( const Data : AnsiString ); virtual;
    procedure SendUniData( const Data : UnicodeString ); virtual;
    procedure AddPendingWrite; virtual;
    function  GetCurrentThreadSlotIdx : Integer;
    function  GetCurrentThreadLine : IOString;
    procedure UpdateCurrentThreadLine;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Write( const str : IOString );
    procedure WriteLine( const str : IOString );

    function  ReceiveUniData : UnicodeString; virtual;
    function  ReceiveData : AnsiString; virtual;

  published
    property MaxLines : Integer read FMaxLines write FMaxLines default kMaxLines;
    property MaxLineLength : Integer read FMaxLineLength write FMaxLineLength default kMaxLineLength;
    property DelayWrites : Boolean read FDelayWrites write FDelayWrites default False;
    property OnSendData    : TSendDataEvent read FOnSendData write FOnSendData;
    property OnReceiveData : TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    property OnSendUniData    : TSendUniDataEvent read FOnSendUniData write FOnSendUniData;
    property OnReceiveUniData : TReceiveUniDataEvent read FOnReceiveUniData write FOnReceiveUniData;
    property UnicodeIO: Boolean read FUnicodeIO write FUnicodeIO;
    property RawOutput: Boolean read FRawOutput write FRawOutput;
  end;

implementation

(*******************************************************)
(**                                                   **)
(**            class TPythonInputOutput               **)
(**                                                   **)
(*******************************************************)

constructor TPythonInputOutput.Create( AOwner : TComponent );
begin
  inherited;
  FMaxLines      := kMaxLines;
  FQueue         := TIOStringList.Create;
  FDelayWrites   := False;
  FMaxLineLength := kMaxLineLength;
  FLinesPerThread:= TIOStringList.Create;
  FLock          := TCriticalSection.Create;
end;

destructor TPythonInputOutput.Destroy;
begin
  FLinesPerThread.Free;
  FQueue.Free;
  FLock.Free;
  inherited;
end;

procedure TPythonInputOutput.Lock;
begin
  FLock.Enter;
end;

procedure TPythonInputOutput.Unlock;
begin
  FLock.Leave;
end;

procedure TPythonInputOutput.Write( const str : IOString );

  procedure DropLine;
  begin
{$IFDEF MSWINDOWS}
    if DelayWrites then
      AddWrite( FLine_Buffer )
    else
{$ENDIF}
      if UnicodeIO then
        SendUniData( FLine_Buffer )
      else
        SendData( AnsiString(FLine_Buffer) );
    FLine_Buffer := '';
    UpdateCurrentThreadLine;
  end;

var
  i : Integer;
  c : IOChar;
begin
  Lock;
  try
    FLine_Buffer := GetCurrentThreadLine;
    if FRawOutput then begin
      FLine_Buffer := FLine_Buffer  + str;
      DropLine;
    end else begin
      for i := 1 to length(str) do
        begin
          c := str[i];
          if c = #10 then
            DropLine
          else if (c >= ' ') or (c = #09) then
            begin
              Insert( c, FLine_Buffer, length(FLine_Buffer)+1 );
              if Length(FLine_Buffer) > MaxLineLength then
                DropLine;
            end;
        end;
    end;
    UpdateCurrentThreadLine;
  finally
    Unlock;
  end;
end;

procedure TPythonInputOutput.WriteLine( const str : IOString );
begin
  Write( str+#10 );
end;

procedure TPythonInputOutput.AddWrite( const str : IOString );
begin
  FQueue.Add( string(str) );
  if FQueue.Count > FMaxLines then
    FQueue.Delete(0)
  else
    AddPendingWrite;
end;

procedure TPythonInputOutput.SendData( const Data : AnsiString );
begin
  if Assigned(FOnSendData) then
    FOnSendData( Self, Data );
end;

procedure TPythonInputOutput.SendUniData(const Data: UnicodeString);
begin
  if Assigned(FOnSendUniData) then
    FOnSendUniData( Self, Data );
end;

function  TPythonInputOutput.ReceiveData : AnsiString;
begin
  Result := '';
  if Assigned(FOnReceiveData) then
    FOnReceiveData( Self, Result );
end;

function TPythonInputOutput.ReceiveUniData: UnicodeString;
begin
  Result := '';
  if Assigned(FOnReceiveUniData) then
    FOnReceiveUniData( Self, Result );
end;

procedure TPythonInputOutput.AddPendingWrite;
begin
end;

function  TPythonInputOutput.GetCurrentThreadSlotIdx : Integer;
var
  thread_id : NativeInt;
  i : Integer;
begin
  thread_id := TThread.Current.ThreadID;
  for i := 0 to FLinesPerThread.Count-1 do
    if NativeInt(FLinesPerThread.Objects[i]) = thread_id then
      begin
        Result := i;
        Exit;
      end;
  Result := FLinesPerThread.AddObject( '', TObject(thread_id) );
end;

function  TPythonInputOutput.GetCurrentThreadLine : IOString;
begin
  Result := IOString(FLinesPerThread.Strings[ GetCurrentThreadSlotIdx ]);
end;

procedure TPythonInputOutput.UpdateCurrentThreadLine;
begin
  FLinesPerThread.Strings[ GetCurrentThreadSlotIdx ] := string(FLine_Buffer);
end;

end.
