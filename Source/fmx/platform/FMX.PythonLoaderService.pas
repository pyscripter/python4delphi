{$I ..\..\Definition.Inc}
unit FMX.PythonLoaderService;

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
  Winapi.Windows,
{$ELSE}
{$IFDEF FPC}
  Dl,
  DynLibs,
{$ELSE}
  Posix.DLfcn,
  Posix.Pthread,
{$ENDIF}
{$ENDIF}
  SysUtils, IOUtils,
  PythonException,
  System.UITypes, FMX.Platform, FMX.DialogService;

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
  TDialogService.ShowMessage(AMsg);
end;

procedure TPythonLoaderService.FatalAbort(const AMsg: string);
begin
  TDialogService.MessageDialog(AMsg,
    TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1,
    procedure(const AResult: TModalResult) begin
      Halt(1);
    end);
end;

end.
