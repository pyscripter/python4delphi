unit Vcl.PythonLoaderService;

interface

uses
  Classes, PythonPlatformServices;

type
  TPythonLoaderService = class(TInterfacedPersistent, IPythonLoaderService)
  public
    function IsHandleValid(const AHandle: THandle): boolean;
    function LoadDll(const ADllPath: string; out AHandle: THandle): boolean;
    procedure UnloadDll(var AHandle: THandle);
    function Import(const AHandle: THandle; const AFuncName: AnsiString; ACanFail: boolean = true): pointer;

    procedure FatalMsgDlg(const AMsg: string);
    procedure FatalAbort(const AMsg: string);
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
{$IFDEF FPC}
  Dl,
  DynLibs,
{$ELSE}
  Posix.DLfcn,
  Posix.Pthread,
{$ENDIF}
{$ENDIF}
  SysUtils, IOUtils, PythonException;

{ TPythonLoaderService }

function TPythonLoaderService.LoadDll(const ADllPath: string;
  out AHandle: THandle): boolean;
begin
  {$IFDEF MSWINDOWS}
  AHandle := SafeLoadLibrary(
    {$IFDEF FPC}
    PAnsiChar(AnsiString(ADllPath))
    {$ELSE}
    ADllPath
    {$ENDIF});
  {$ELSE}
  //Linux: need here RTLD_GLOBAL, so Python can do "import ctypes"
  AHandle := THandle(dlopen(PAnsiChar(AnsiString(ADllPath)),
    RTLD_LAZY + RTLD_GLOBAL));
  {$ENDIF}

  Result := IsHandleValid(AHandle);
end;

procedure TPythonLoaderService.UnloadDll(var AHandle: THandle);
begin
  FreeLibrary(AHandle);
  AHandle := 0;
end;

function TPythonLoaderService.Import(const AHandle: THandle;
  const AFuncName: AnsiString; ACanFail: boolean): pointer;
var
  E : EDllImportError;
  {$IF not Defined(FPC) and not Defined(MSWINDOWS)}
  S : string;
  {$IFEND}
begin
  {$IF Defined(FPC) or Defined(MSWINDOWS)}
  Result := GetProcAddress(AHandle, PAnsiChar(AFuncName));
  {$ELSE}
  S := string(AFuncName);
  Result := GetProcAddress(AHandle, PWideChar(S));
  {$IFEND}
  if (Result = nil) and ACanFail then begin
    {$IFDEF MSWINDOWS}
    E := EDllImportError.CreateFmt('Error %d: could not map symbol "%s"', [GetLastError, AFuncName]);
    E.ErrorCode := GetLastError;
    {$ELSE}
    E := EDllImportError.CreateFmt('Error: could not map symbol "%s"', [AFuncName]);
    {$ENDIF}
    E.WrongFunc := AFuncName;
    raise E;
  end;
end;

function TPythonLoaderService.IsHandleValid(const AHandle: THandle): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (AHandle >= 32);
  {$ELSE}
  Result := AHandle <> 0;
  {$ENDIF}
end;

procedure TPythonLoaderService.FatalMsgDlg(const AMsg: string);
begin
  {$IFDEF MSWINDOWS}
  MessageBox( GetActiveWindow, PChar(AMsg), 'Error', MB_TASKMODAL or MB_ICONSTOP );
  {$ELSE}
  WriteLn(ErrOutput, AMsg);
  {$ENDIF}
end;

procedure TPythonLoaderService.FatalAbort(const AMsg: string);
begin
  {$IFDEF MSWINDOWS}
  MessageBox( GetActiveWindow, PChar(AMsg), 'Error', MB_TASKMODAL or MB_ICONSTOP );
  ExitProcess(1);
  {$ELSE}
  WriteLn(ErrOutput, AMsg);
  Halt(1);
  {$ENDIF}
end;

end.
