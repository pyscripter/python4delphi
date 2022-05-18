unit ExecCmd.Posix;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base, Posix.Fcntl, Posix.Unistd, Posix.SysWait, Posix.Stdlib,
  Posix.Stdio, Posix.SysTypes, Posix.Signal, Posix.Errno, Posix.SysStat,
  Posix.String_,
  ExecCmd;

type
  TStreamHandle = pointer;

  TExecCmdPosix = class(TInterfacedObject, IExecCmd)
  private  
    FCmd: string;
    FArg: TArray<string>;
    FEnv: TArray<string>;
    FPid: Integer;
    FRead: TPipeDescriptors;
    FWrite: TPipeDescriptors;
    FExitCode: integer;
    procedure Redirect(out AReader: TReader; out AWriter: TWriter);
    function PeekMessage(): string;
  protected
    function GetIsAlive: boolean;
    function GetExitCode: Integer;
  public
    constructor Create(const ACmd: string; AArg, AEnv: TArray<string>);
    destructor Destroy(); override;

    function Run(): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(out AReader: TReader; out AWriter: TWriter; const ARedirections: TRedirections): IExecCmd; overload;
    procedure Kill();
    function Wait(): Integer;

    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

  EForkFailed = class(EExecCmd);
  EPipeFailed = class(EExecCmd);
  EInvalidArgument = class(EExecCmd);
  EOperationNotPermitted = class(EExecCmd);
  ENoSuchProcess = class(EExecCmd);
  EWaitFailed = class(EExecCmd);

implementation

uses
  System.SysUtils, System.IOUtils;

const
  INITIAL_EXIT_CODE = -999;

{ TExecCmdPosix }

constructor TExecCmdPosix.Create(const ACmd: string; AArg, AEnv: TArray<string>);
begin
  inherited Create();
  FCmd := ACmd;
  FArg := AArg;
  FEnv := AEnv;
  FExitCode := INITIAL_EXIT_CODE;
end;

destructor TExecCmdPosix.Destroy;
begin
  if IsAlive then
    Kill();

  __close(FRead.ReadDes);
  __close(FWrite.WriteDes);     
  inherited;
end;

function TExecCmdPosix.GetExitCode: Integer;
begin
  Result := Wait();
end;

function TExecCmdPosix.GetIsAlive: boolean;
var
  LWaitedPid: integer;
  LStatus: integer;
begin
  if (FExitCode <> INITIAL_EXIT_CODE) then
    Exit(false);
    
  LWaitedPid := waitpid(FPid, @LStatus, WNOHANG);
  if (LWaitedPid = -1) then
    raise EWaitFailed.Create('Failed waiting for proess.')
  else if (LWaitedPid = 0) then
    Exit(true)
  else begin    
    if WIFEXITED(LStatus) then begin
      FExitCode := WEXITSTATUS(LStatus);
    end else if WIFSIGNALED(LStatus) then begin
      FExitCode := WTERMSIG(LStatus);
    end else if WIFSTOPPED(LStatus) then begin
      FExitCode := WSTOPSIG(LStatus);
    end else begin
      FExitCode := EXIT_FAILURE;
    end;
  end;
  Result := false;
end;

procedure TExecCmdPosix.Kill;
begin
  if (Posix.Signal.kill(FPid, Posix.Signal.SIGKILL) <> 0) then
    if (errno = EINVAL) then //Invalid signal
      raise EInvalidArgument.Create('Invalid argument.')
    else if (errno = EPERM) then //The process does not have permission to send the signal to any of the target processes.
      raise EOperationNotPermitted.Create('Operation not permitted.') 
    else if (errno = ESRCH) then //The pid or process group does not exist. Note that an existing process might be a zombie, a process which already committed termination, but has not yet been wait(2)ed for.
      raise ENoSuchProcess.Create('No such process.') 
end;

function TExecCmdPosix.PeekMessage: string;
var
  LBuffer: array[0..511] of UInt8;
  LCount: integer;
begin
  while True do begin
    LCount := __read(FRead.ReadDes, @LBuffer[0], SizeOf(LBuffer));
    if (LCount = -1) then begin     
      if (errno = EINTR) then
        Continue
      else
        Exit(String.Empty);
    end else if (LCount = 0) then
      Exit(String.Empty)
    else begin
      Exit(Copy(UTF8ToString(@LBuffer[0]), 1, UTF8ToString(@LBuffer[0]).Length -1));
    end;      
  end;
end;

procedure TExecCmdPosix.Redirect(out AReader: ExecCmd.TReader;
  out AWriter: ExecCmd.TWriter);
var
  LBuffer: string;
  M: TMarshaller;
begin
  AReader := function(): string
  begin
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
    __write(FWrite.WriteDes, M.AsUtf8(PWideChar(AIn)).ToPointer(), AIn.Length);
  end;
end;

function TExecCmdPosix.Run(out AOutput: string): IExecCmd;
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

function TExecCmdPosix.Run(out AReader: ExecCmd.TReader;
  out AWriter: ExecCmd.TWriter;
  const ARedirections: TRedirections): IExecCmd;
var
  M: TMarshaller;
  LArg, LEnv: array of PAnsiChar;
  I: Integer;
begin
  //#define PARENT_READ read_pipe[0]
  //#define PARENT_WRITE write_pipe[1]
  //#define CHILD_WRITE read_pipe[1]
  //#define CHILD_READ  write_pipe[0]
  
  if (pipe(FRead) = -1) or (pipe(FWrite) = -1) then
    raise EPipeFailed.Create('Failed to create pipe.');

  FPid := fork();
  if (FPid < 0) then
    raise EForkFailed.Create('Failed to fork process.')
  else if (FPid = 0) then begin
    while ((dup2(FRead.WriteDes, STDOUT_FILENO) = -1) and (errno = EINTR)) do begin end;
    while ((dup2(FRead.WriteDes, STDERR_FILENO) = -1) and (errno = EINTR)) do begin end;
    while ((dup2(FWrite.ReadDes, STDIN_FILENO) = -1) and (errno = EINTR)) do begin end;
    __close(FRead.WriteDes);
    __close(FRead.ReadDes);
    __close(FWrite.ReadDes);

    //https://man7.org/linux/man-pages/man2/execve.2.html

    //argv is an array of pointers to strings passed to the new program
    //as its command-line arguments. By convention, THE FIRST OF THESE
    //STRINGS (i.e., argv[0]) SHOULD CONTAIN THE FILENAME ASSOCIATED
    //WITH THE FILE BEING EXECUTED. The argv array must be terminated
    //by a NULL pointer. (Thus, in the new program, argv[argc] will be
    //NULL.)

    SetLength(LArg, Length(FArg) + 1);
    for I := Low(FArg) to High(FArg) do
      LArg[I] := M.AsAnsi(PWideChar(FArg[I]) + #0).ToPointer();
    LArg[High(LArg)] := PAnsiChar(nil);

    SetLength(LEnv, Length(FEnv) + 1);
    for I := Low(FEnv) to High(FEnv) do
      LEnv[I] := M.AsAnsi(PWideChar(FEnv[I]) + #0).ToPointer();
    LEnv[High(LEnv)] := PAnsiChar(nil);

    if execve(M.AsAnsi(PWideChar(FCmd)).ToPointer(), PPAnsiChar(LArg), PPAnsiChar(LEnv)) = -1 then begin
      Halt(errno);
    end else
      Halt(EXIT_FAILURE);
  end else if (FPid > 0) then begin
    __close(FRead.WriteDes);
    __close(FWrite.ReadDes);
    __close(FWrite.WriteDes);
    Redirect(AReader, AWriter);
  end;      
  Result := Self;
end;

function TExecCmdPosix.Run: IExecCmd;
var
  LReader: TReader;
  LWriter: TWriter;
begin
  Result := Run(LReader, LWriter, []);
end;

function TExecCmdPosix.Wait: Integer;
var
  LWaitedPid: integer;
  LStatus: integer;
begin
  if (FExitCode <> INITIAL_EXIT_CODE) then
    Exit(FExitCode);
    
  LWaitedPid := waitpid(FPid, @LStatus, WNOHANG);
  repeat
    if (LWaitedPid = -1) then
      raise EWaitFailed.Create('Failed waiting for process.')
    else if (LWaitedPid = 0) then
      LWaitedPid := waitpid(FPid, @LStatus, WNOHANG)
    else begin
      if WIFEXITED(LStatus) then begin
        FExitCode := WEXITSTATUS(LStatus);
      end else if WIFSIGNALED(LStatus) then begin
        FExitCode := WTERMSIG(LStatus);
      end else if WIFSTOPPED(LStatus) then begin
        FExitCode := WSTOPSIG(LStatus);
      end else begin
        FExitCode := EXIT_FAILURE;
      end;
    end;
  until (FExitCode <> INITIAL_EXIT_CODE);
  
  Result := FExitCode;
end;

end.
