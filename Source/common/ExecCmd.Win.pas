unit ExecCmd.Win;

{$WEAKPACKAGEUNIT}

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, ExecCmd;

//Ref: https://docs.microsoft.com/en-us/windows/win32/procthread/creating-a-child-process-with-redirected-input-and-output

type
  TExecCmdWin = class(TInterfacedObject, IExecCmd)
  private
    FCmd: string;
    FArg: TArray<string>;
    FEnv: TArray<string>;
    FSecurityAttributes: TSecurityAttributes;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
    //Pipes
    //StdOut
    FStdOutPipeRead: THandle;
    FStdOutPipeWrite: THandle;
    //StdIn
    FStdInPipeRead: THandle;
    FStdInPipeWrite: THandle;
    //Job
    FJob: THandle;
  private
    function GetStatus: cardinal;
    procedure Execute();
    //IO
    function PeekMessage(const ACheckPipe: boolean = true): string;
  protected
    function GetIsAlive: boolean;
    function GetExitCode: Integer;
  public
    constructor Create(const ACmd: string; const AArg, AEnv: TArray<string>);
    destructor Destroy(); override;

    procedure Redirect(out AReader: TReader; out AWriter: TWriter);
    function Run(): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(out AReader: TReader; out AWriter: TWriter; const ARedirections: TRedirections): IExecCmd; overload;
    procedure Kill();
    function Wait(): Integer;

    property Status: cardinal read GetStatus;
    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

const
  cCANCELATION_SIGNAL_EXIT_CODE = $001A;

implementation

uses
  Math;

{ TExecCmdWin }

constructor TExecCmdWin.Create(const ACmd: string; const AArg, AEnv: TArray<string>);
begin
  FCmd := ACmd;
  FArg := AArg;
  FEnv := AEnv;
  //Set the bInheritedHandle to true so pipe handles are inherited
  FSecurityAttributes := Default(TSecurityAttributes);
  with FSecurityAttributes do begin
    nLength := SizeOf(FSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  //Create the startup information to the process
  FStartupInfo := Default(TStartupInfo);
  with FStartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
  end;

  //Create a job that kills the subprocess when parent dies
  FJob := CreateJobObject(nil, nil);
  if (FJob <> 0) then begin
    var LExInfo: TJobObjectExtendedLimitInformation;
    LExInfo.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if not SetInformationJobObject(FJob, JobObjectExtendedLimitInformation,
      @LExInfo, SizeOf(TJobObjectExtendedLimitInformation)) then
        RaiseLastOSError();
  end;

  // Close handles to the stdin and stdout pipes no longer needed by the child process.
  // If they are not explicitly closed, there is no way to recognize that the child process has ended.
  CloseHandle(FStdOutPipeWrite);
  CloseHandle(FStdInPipeRead);
end;

destructor TExecCmdWin.Destroy;
begin
  if IsAlive then
    Kill();

  CloseHandle(FProcessInfo.hThread);
  CloseHandle(FProcessInfo.hProcess);
  inherited;
end;

function TExecCmdWin.GetExitCode: Integer;
begin
  if not GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(Result)) then
    RaiseLastOSError();
end;

function TExecCmdWin.GetIsAlive: boolean;
var
  LExitCode: cardinal;
begin
  GetExitCodeProcess(FProcessInfo.hProcess, LExitCode);
  Result := (LExitCode = STILL_ACTIVE);
end;

function TExecCmdWin.GetStatus: cardinal;
begin
  Result := WaitForSingleObject(FProcessInfo.hProcess, 0);
end;

procedure TExecCmdWin.Execute;
begin
  //Create the process
  var LCmd := FCmd + ' ' + String.Join(' ', FArg);
  UniqueString(LCmd);
  if not CreateProcess(nil, PWideChar(LCmd), nil, nil, True, 0, nil, nil,
    FStartupInfo, FProcessInfo) then
      RaiseLastOSError();

  //Assign the process to the job. It takes the proc. down when parent is killed.
  AssignProcessToJobObject(FJob, FProcessInfo.hProcess);
end;

procedure TExecCmdWin.Redirect(out AReader: TReader; out AWriter: TWriter);
var
  LBytesWritten: cardinal;
  LBuffer: string;
begin
  AReader := function(): string begin
    Result := String.Empty;

    while GetIsAlive() and Result.IsEmpty() do begin
      Result := Result + PeekMessage();
    end;

    if not Result.IsEmpty() then
      Exit;

    //Preventing race condition...
    repeat
      LBuffer := PeekMessage();
      if not LBuffer.IsEmpty() then
        Result := Result + LBuffer;
    until (LBuffer.IsEmpty());
  end;

  AWriter := procedure(AIn: string) begin
    var LIn := AnsiString(AIn);
    WriteFile(FStdInPipeWrite, LIn[1], Length(LIn), LBytesWritten, nil);
  end;
end;

function TExecCmdWin.Run: IExecCmd;
var
  LReader: TReader;
  LWriter: TWriter;
begin
  Result := Run(LReader, LWriter, []);
end;

function TExecCmdWin.Run(out AOutput: string): IExecCmd;
var
  LReader: TReader;
  LWriter: TWriter;
  LOutput: string;
begin
  AOutput := String.Empty;
  Result := Run(LReader, LWriter, [TRedirect.stdout]);
  repeat
    LOutput := LReader();
    if not LOutput.IsEmpty() then
      AOutput := AOutput + LOutput;
  until LOutput.IsEmpty();
end;

function TExecCmdWin.Run(out AReader: TReader; out AWriter: TWriter; const ARedirections: TRedirections): IExecCmd;
begin
  //STD OUT REDIRECTION
  if (TRedirect.stdout in ARedirections) then begin
    if not CreatePipe(FStdOutPipeRead, FStdOutPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();

    //Ensure the read handle to the pipe for STDOUT is not inherited
    //if not SetHandleInformation(FStdOutPipeRead, HANDLE_FLAG_INHERIT, 0) then
    //  RaiseLastOSError();

    //Redirect to our pipe
    FStartupInfo.hStdOutput := FStdOutPipeWrite;
    FStartupInfo.hStdError := FStdOutPipeWrite;
  end else begin
    FStartupInfo.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
    FStartupInfo.hStdError := GetStdHandle(STD_ERROR_HANDLE);
  end;

  //STD IN REDIRECTION
  if (TRedirect.stdin in ARedirections) then begin
    //Create a pipe for the child process's STDIN
    if not CreatePipe(FStdInPipeRead, FStdInPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();

    //Ensure the write handle to the pipe for STDIN is not inherited
    //if not SetHandleInformation(FStdInPipeWrite, HANDLE_FLAG_INHERIT, 0) then
    //  RaiseLastOSError();

    //Redirect to our pipe
    FStartupInfo.hStdInput := FStdInPipeRead;
  end else begin
    FStartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  end;

  Redirect(AReader, AWriter);

  Execute();

  Result := Self;
end;

function TExecCmdWin.Wait: Integer;
begin
  while GetIsAlive do
    Sleep(100);
  Result := GetExitCode;
end;

procedure TExecCmdWin.Kill;
begin
  if (Status = WAIT_TIMEOUT) then
    if not TerminateProcess(FProcessInfo.hProcess, cCANCELATION_SIGNAL_EXIT_CODE) then
      RaiseLastOSError();
end;

function TExecCmdWin.PeekMessage(const ACheckPipe: boolean): string;
const
  BUFFSIZE = 4096;
type
  TBuffArr = array[0..BUFFSIZE - 1] of AnsiChar;
var
  LBytesRead: cardinal;
  LBuffer: TBuffArr;
begin
  Result := String.Empty;

  if ACheckPipe and not PeekNamedPipe(FStdOutPipeRead, nil, 0, nil, @LBytesRead, nil) then
    Exit;

  if (LBytesRead > 0) then
    if ReadFile(FStdOutPipeRead, LBuffer, BUFFSIZE, LBytesRead, nil) then
      if (LBytesRead > 0) then begin
        LBuffer[LBytesRead] := #0;
        Result := Result + String(LBuffer);
      end;
end;

end.
