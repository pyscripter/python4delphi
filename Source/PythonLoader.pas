{$I Definition.Inc}
unit PythonLoader;

interface

uses
  System.Classes, PythonTypes;

//-------------------------------------------------------
//--                                                   --
//--      Base class:  TDynamicDll                     --
//--                                                   --
//-------------------------------------------------------

type
  TCustomDll = class(TComponent)
  private
    FDLLHandle: THandle;
    procedure SetDllName(Value: string);
    function IsAPIVersionStored: Boolean;
    function IsDllNameStored: Boolean;
    function IsRegVersionStored: Boolean;
  protected
    FDllName: string;
    FDllPath: string;
    FAPIVersion: Integer;
    FRegVersion: string;
    FAutoLoad: Boolean;
    FAutoUnload: Boolean;
    FFatalMsgDlg: Boolean;
    FFatalAbort: Boolean;
    FUseLastKnownVersion: Boolean;
    FOnBeforeLoad: TNotifyEvent;
    FOnAfterLoad: TNotifyEvent;
    FOnBeforeUnload: TNotifyEvent;
    FOnPathInitialization: TPathInitializationEvent;
  protected
    procedure DoOpenDll(const aDllName : string); virtual; abstract;
    procedure DoCloseDll(); virtual; abstract;
    function IsHandleValid: Boolean; virtual; abstract;
    procedure InvalidDllFatalMsgDlg(); virtual; abstract;
    procedure Quit; virtual; abstract;
  protected
    procedure Loaded; override;
    procedure BeforeLoad; virtual;
    procedure AfterLoad; virtual;
    procedure BeforeUnload; virtual;
    function GetQuitMessage : string; virtual;
  protected
    procedure CheckRegistry;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadDll;
    procedure UnloadDll;
  public
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default True;
    property AutoUnload: Boolean read FAutoUnload write FAutoUnload default True;
    property DllName: string read FDllName write SetDllName stored IsDllNameStored;
    property DllPath: string read FDllPath write FDllPath;
    property APIVersion: Integer read FAPIVersion write FAPIVersion stored IsAPIVersionStored;
    property RegVersion: string read FRegVersion write FRegVersion stored IsRegVersionStored;
    property FatalAbort: Boolean read FFatalAbort write FFatalAbort default True;
    property FatalMsgDlg: Boolean read FFatalMsgDlg write FFatalMsgDlg default True;
    property UseLastKnownVersion: Boolean read FUseLastKnownVersion write FUseLastKnownVersion default True;
    property OnAfterLoad: TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeLoad: TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnBeforeUnload: TNotifyEvent read FOnBeforeUnload write FOnBeforeUnload;
    property OnPathInitialization: TPathInitializationEvent read FOnPathInitialization write FOnPathInitialization;
  end;

  TDynamicDll = class(TCustomDll)
  protected
    function Import(const funcname: AnsiString; canFail: Boolean = True): Pointer;
    function GetDllPath(): string;
    procedure DoOpenDll(const aDllName : string); override;
    procedure DoCloseDll(); override;
    procedure InvalidDllFatalMsgDlg(); override;
    procedure Quit; override;
    function IsHandleValid: Boolean; override;
  end;

(*
    Checks whether the PythonVersion x.x is Registered
*)
{$IFDEF MSWINDOWS}
function IsPythonVersionRegistered(PythonVersion : string;
  out InstallPath: string; out AllUserInstall: Boolean) : Boolean;
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows, Registry,
  {$ELSEIF Defined(ANDROID)}
  System.IOUtils, Posix.Dlfcn,
  {$IFEND}
  SysUtils,
  PythonExceptions;

constructor TCustomDll.Create(AOwner: TComponent);
begin
  inherited;
  FFatalMsgDlg          := True;
  FFatalAbort           := True;
  FAutoLoad             := True;
  FUseLastKnownVersion  := True;
end;

destructor TCustomDll.Destroy;
begin
  if AutoUnload then
    UnloadDll;
  inherited;
end;

procedure TCustomDll.Loaded;
begin
  inherited;
  if AutoLoad and not (csDesigning in ComponentState) then
    LoadDll;
end;

procedure TCustomDll.LoadDll;
begin
  UnloadDll;
  BeforeLoad;
  DoOpenDll(DllName);
  if not IsHandleValid then begin
    if FatalMsgDlg then
      InvalidDllFatalMsgDlg();
    if FatalAbort then
      Quit;
  end else
    AfterLoad;
end;

procedure TCustomDll.UnloadDll;
begin
  if IsHandleValid then begin
    BeforeUnload;
    DoCloseDll();
  end;
end;

procedure TCustomDll.BeforeLoad;
begin
  if Assigned( FOnBeforeLoad ) then
    FOnBeforeLoad( Self );
end;

procedure TCustomDll.AfterLoad;
begin
  if Assigned( FOnAfterLoad ) then
    FOnAfterLoad( Self );
end;

procedure TCustomDll.BeforeUnload;
begin
  if Assigned( FOnBeforeUnload ) then
    FOnBeforeUnload( Self );
end;

function  TCustomDll.GetQuitMessage : string;
begin
  Result := Format( 'Dll %s could not be loaded. We must quit.', [DllName]);
end;

procedure TCustomDll.SetDllName(Value: string);
begin
  FDllName := Value;
end;

function TCustomDll.IsAPIVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TCustomDll.IsDllNameStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TCustomDll.IsRegVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

(*******************************************************)
(**                                                   **)
(**            class TDynamicDll                      **)
(**                                                   **)
(*******************************************************)

procedure TDynamicDll.DoOpenDll(const aDllName : string);
begin
  if not IsHandleValid then
  begin
    FDllName := aDllName;
    {$IFDEF MSWINDOWS}
    FDLLHandle := SafeLoadLibrary(
      {$IFDEF FPC}
      PAnsiChar(AnsiString(GetDllPath+DllName))
      {$ELSE}
      GetDllPath+DllName
      {$ENDIF});
    {$ELSEIF Defined(ANDROID)}
      if not IsHandleValid then
      begin
        FDllName := aDllName;
        FDLLHandle := dlopen(PAnsiChar(TPath.Combine(DllPath, DllName)), RTLD_LAZY + RTLD_GLOBAL);
      end;
    {$ELSE}
    //Linux: need here RTLD_GLOBAL, so Python can do "import ctypes"
    FDLLHandle := THandle(dlopen(PAnsiChar(AnsiString(GetDllPath+DllName)),
      RTLD_LAZY+RTLD_GLOBAL));
    {$IFEND}
  end;
end;

procedure TDynamicDll.DoCloseDll();
begin
  {$IFDEF ANDROID}
    dlclose(FDLLHandle);
  {$ELSE}
    FreeLibrary(FDLLHandle);
  {$ENDIF}
  FDLLHandle := 0;
end;

function  TDynamicDll.GetDllPath : string;
{$IFDEF MSWINDOWS}
var
  AllUserInstall: Boolean;
{$ENDIF}
begin
  Result := DllPath;

  {$IFDEF MSWINDOWS}
  if DLLPath = '' then begin
    IsPythonVersionRegistered(RegVersion, Result, AllUserInstall);
  end;
  {$ENDIF}

  if Result <> '' then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

function TDynamicDll.Import(const funcname: AnsiString; canFail : Boolean = True): Pointer;
var
  E : EDllImportError;
  {$IF not Defined(FPC) and not Defined(MSWINDOWS) and not Defined(ANDROID)}
  S : string;
  {$IFEND}
begin
  {$IF Defined(FPC) or Defined(MSWINDOWS)}
  Result := GetProcAddress( FDLLHandle, PAnsiChar(funcname) );
  {$ELSEIF Defined(ANDROID)}
  Result := dlsym(FDLLHandle, PAnsiChar(funcname));
  {$ELSE}
  S := string(funcname);
  Result := GetProcAddress( FDLLHandle, PWideChar(S) );
  {$IFEND}
  if (Result = nil) and canFail then begin
    {$IFDEF MSWINDOWS}
    E := EDllImportError.CreateFmt('Error %d: could not map symbol "%s"', [GetLastError, funcname]);
    E.ErrorCode := GetLastError;
    {$ELSE}
    E := EDllImportError.CreateFmt('Error: could not map symbol "%s"', [funcname]);
    {$ENDIF}
    E.WrongFunc := funcname;
    raise E;
  end;
end;

procedure TDynamicDll.InvalidDllFatalMsgDlg;
var
  s: string;
begin
  {$IFDEF MSWINDOWS}
  s := Format('Error %d: Could not open Dll "%s"',[GetLastError, DllName]);
  {$ELSE}
  s := Format('Error: Could not open Dll "%s"',[DllName]);
  {$ENDIF}
  if FatalMsgDlg then
    {$IFDEF MSWINDOWS}
    MessageBox( GetActiveWindow, PChar(s), 'Error', MB_TASKMODAL or MB_ICONSTOP );
    {$ELSE}
    WriteLn(ErrOutput, s);
    {$ENDIF}
end;

function  TDynamicDll.IsHandleValid : Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (FDLLHandle >= 32);
{$ELSE}
  Result := FDLLHandle <> 0;
{$ENDIF}
end;

procedure TDynamicDll.Quit;
begin
  if not( csDesigning in ComponentState ) then begin
{$IFDEF MSWINDOWS}
    MessageBox( GetActiveWindow, PChar(GetQuitMessage), 'Error', MB_TASKMODAL or MB_ICONSTOP );
    ExitProcess( 1 );
{$ELSE}
    WriteLn(ErrOutput, GetQuitMessage);
    Halt( 1 );
{$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}
function IsPythonVersionRegistered(PythonVersion : string;
  out InstallPath: string; out AllUserInstall: Boolean) : Boolean;
  // Python provides for All user and Current user installations
  // All User installations place the Python DLL in the Windows System directory
  // and write registry info to HKEY_LOCAL_MACHINE
  // Current User installations place the DLL in the install path and
  // the registry info in HKEY_CURRENT_USER.
  // Hence, for Current user installations we need to try and find the install path
  // since it may not be on the system path.

  // The above convension was changed in Python 3.5.  Now even for all user
  // installations the dll is located at the InstallPath.
  // Also from version 3.5 onwards 32 bit version have a suffix -32 e.g. "3.6-32"
  // See also PEP 514

var
  key: string;
  VersionSuffix: string;
  MajorVersion : integer;
  MinorVersion : integer;
begin
  Result := False;
  InstallPath := '';
  AllUserInstall := False;
  MajorVersion := StrToInt(PythonVersion[1]);
  MinorVersion := StrToInt(PythonVersion[3]);
  VersionSuffix := '';
{$IFDEF CPUX86}
  if (MajorVersion > 3) or ((MajorVersion = 3)  and (MinorVersion >= 5)) then
    VersionSuffix := '-32';
{$ENDIF}
  key := Format('\Software\Python\PythonCore\%s%s\InstallPath', [PythonVersion, VersionSuffix]);

  // First try HKEY_CURRENT_USER as per PEP514
  try
    with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(Key, False) then begin
          InstallPath := ReadString('');
          Result := True;
          Exit;
        end;
      finally
        Free;
      end;
  except
  end;

  //Then try for an all user installation
  try
    with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey(Key, False) then begin
          AllUserInstall := True;
          if (MajorVersion > 3) or ((MajorVersion = 3)  and (MinorVersion >= 5)) then
            InstallPath := ReadString('');
          Result := True;
        end;
      finally
        Free;
      end;
  except
  end;
end;
{$ENDIF}

procedure TCustomDll.CheckRegistry;
{$IFDEF MSWINDOWS}
var
  key : string;
  Path : string;
  NewPath : string;
{$IFDEF CPUX86}
  MajorVersion : integer;
  MinorVersion : integer;
{$ENDIF}
  VersionSuffix: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Assigned( FOnPathInitialization ) then
  try
    with TRegistry.Create(KEY_ALL_ACCESS and not KEY_NOTIFY) do
      try
        VersionSuffix := '';
{$IFDEF CPUX86}
        MajorVersion := StrToInt(RegVersion[1]);
        MinorVersion := StrToInt(RegVersion[3]);
        if (MajorVersion > 3) or ((MajorVersion = 3)  and (MinorVersion >= 5)) then
          VersionSuffix := '-32';
{$ENDIF}
        key := Format('\Software\Python\PythonCore\%s%s\PythonPath', [RegVersion, VersionSuffix]);

        RootKey := HKEY_LOCAL_MACHINE;
        if not KeyExists( key ) then
        begin
          // try a current user installation
          RootKey := HKEY_CURRENT_USER;
          if not KeyExists( key ) then  Exit;
        end;
        // Key found
        OpenKey( key, True );
        try
          Path := ReadString('');
          NewPath := Path;
          FOnPathInitialization( Self, NewPath );
          if NewPath <> Path then
          begin
            WriteString( '', NewPath );
          end;
        finally
          CloseKey;
        end;
      finally
        Free;
      end;
  except
    // under WinNT, with a user without admin rights, the access to the
    // LocalMachine keys would raise an exception.
  end;
{$ENDIF}
end;


end.
