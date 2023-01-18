{$I ..\Definition.Inc}
unit FMX.PythonGUIInputOutput;

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Winapi.Messages,
  Winapi.Windows,
  {$ENDIF}
  Fmx.Memo,
  PythonEngine;

const
  PID_SUPPORTED_PLATFORMS = pidAllPlatforms;

type
  [ComponentPlatformsAttribute(PID_SUPPORTED_PLATFORMS)]
  TPythonGUIInputOutput = class(TPythonInputOutput)
  private
    { Private declarations }
    FCustomMemo : TCustomMemo;
    {$IFDEF MSWINDOWS}
    FWinHandle : THandle;
    {$ENDIF}
  protected
    { Protected declarations }
    {$IFDEF MSWINDOWS}
    procedure pyGUIOutputWndProc (var Message: TMessage);
    {$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SendData( const Data : AnsiString ); override;
    procedure SendUniData( const Data : UnicodeString ); override;
    procedure AddPendingWrite; override;
    procedure WriteOutput;
  public
    function ReceiveData: AnsiString; override;
    function ReceiveUniData: UnicodeString; override;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure DisplayString( const str : string );

  published
    { Published declarations }
    property Output : TCustomMemo read FCustomMemo write FCustomMemo;
  end;

implementation

uses
  System.SysUtils, System.UITypes, FMX.DialogService;

{$IFDEF MSWINDOWS}
const
  WM_WriteOutput = WM_USER + 1;
{$ENDIF}

{ TPythonGUIInputOutput }


{PROTECTED METHODS}

{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if aComponent = fCustomMemo then
      fCustomMemo := nil;
end;

{------------------------------------------------------------------------------}
{$IFDEF MSWINDOWS}
procedure TPythonGUIInputOutput.pyGUIOutputWndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_WriteOutput : WriteOutput;
  end;{case}
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.SendData( const Data : AnsiString );
begin
  if Assigned(FOnSendData) then
    inherited
  else
    DisplayString( string(Data) );
end;

procedure TPythonGUIInputOutput.SendUniData(const Data: UnicodeString);
begin
  if Assigned(FOnSendUniData) then
    inherited
  else
    DisplayString( string(Data) );
end;

{------------------------------------------------------------------------------}
function  TPythonGUIInputOutput.ReceiveData : AnsiString;
Var
  LResult : AnsiString;
begin
  if Assigned(FOnReceiveData) then
    Result := inherited ReceiveData
  else
  begin
    TDialogService.InputQuery('Query from Python', ['Enter text'], [''],
      procedure(const AResult: TModalResult; const AValues: array of string) begin
        LResult := AnsiString(AValues[0]);
      end
    );
    Result := LResult;
  end;
end;

function TPythonGUIInputOutput.ReceiveUniData: UnicodeString;
Var
  LResult: string;
begin
  if Assigned( FOnReceiveUniData ) then
    Result := inherited ReceiveUniData
  else
  begin
    TDialogService.InputQuery('Query from Python', ['Enter text'], [''],
      procedure(const AResult: TModalResult; const AValues: array of string) begin
        LResult := AValues[0];
      end
    );
    Result := LResult;
  end;
end;

{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.AddPendingWrite;
begin
{$IFDEF MSWINDOWS}
  PostMessage( fWinHandle, WM_WriteOutput, 0, 0 );
{$ENDIF}
end;

{------------------------------------------------------------------------------}
procedure TPythonGUIInputOutput.WriteOutput;
var
  S : string;
begin
  if FQueue.Count = 0 then
    Exit;

  Lock;
  try
    while FQueue.Count > 0 do
    begin
      S := FQueue.Strings[ 0 ];
      FQueue.Delete(0);
      DisplayString( S );
    end;
  finally
    Unlock;
  end;
end;

{PUBLIC METHODS}

{------------------------------------------------------------------------------}
constructor TPythonGUIInputOutput.Create( AOwner : TComponent );
begin
  inherited Create(AOwner);
{$IFDEF MSWINDOWS}
  // Create an internal window for use in delayed writes
  // This will allow writes from multiple threads to be queue up and
  // then written out to the associated TCustomMemo by the main UI thread.
  fWinHandle := System.Classes.AllocateHWnd(pyGUIOutputWndProc);
{$ENDIF}
   UnicodeIO := True;
end;

{------------------------------------------------------------------------------}
destructor TPythonGUIInputOutput.Destroy;
begin
{$IFDEF MSWINDOWS}
  // Destroy the internal window used for Delayed write operations
  System.Classes.DeallocateHWnd(fWinHandle);
{$ENDIF}
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
type
  TMyCustomMemo = class(TCustomMemo);

procedure TPythonGUIInputOutput.DisplayString( const str : string );
begin
  if Assigned(Output) then
  begin
    if TMyCustomMemo(Output).Lines.Count >= MaxLines then
      TMyCustomMemo(Output).Lines.Delete(0);
    TMyCustomMemo(Output).Lines.Add(str);
    Output.GoToTextEnd();
  end;
end;

{------------------------------------------------------------------------------}

end.
