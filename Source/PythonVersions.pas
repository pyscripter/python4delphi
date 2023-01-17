{-----------------------------------------------------------------------------
 Unit Name: PythonVersions
 Author:    PyScripter
 Purpose:   Discover and get info about Python versions
            Part of the Python for Delphi library

 History:
-----------------------------------------------------------------------------}

unit PythonVersions;

interface
Uses
  Classes;

type

  TPythonVersion = record
  private
    FDisplayName: string;
    FHelpFile: string;
    fSysArchitecture : string;
    function GetDLLName: string;
    function ExpectedArchitecture:string;
    function GetIsPython3K: Boolean;
    function GetHelpFile: string;
    function GetDisplayName: string;
    function GetApiVersion: integer;
    function GetSysArchitecture: string;
    function GetPythonExecutable: string;
  public
    IsRegistered: Boolean;
    IsAllUsers: Boolean;
    SysVersion: string;
    Version: string;
    DLLPath: string;
    InstallPath: string;
    PythonPath: string;
    function Is_venv: Boolean;
    function Is_virtualenv: Boolean;
    function Is_conda: Boolean;
    procedure AssignTo(PythonEngine: TPersistent);
    property PythonExecutable: string read GetPythonExecutable;
    property DLLName: string read GetDLLName;
    property SysArchitecture: string read GetSysArchitecture;
    property IsPython3K: Boolean read GetIsPython3K;
    property HelpFile: string read GetHelpFile write FHelpFile;
    property DisplayName: string read GetDisplayName write FDisplayName;
    property ApiVersion: integer read GetApiVersion;
  end;

  TPythonVersions = array of TPythonVersion;

  (*
    Compares two Version strings and returns -1, 0, 1 depending on result
    The function result has the semantics of Delphi compare functions
    -1: A is bigger (newer), 0: equal versions, 1: B is bigger (newer)
  *)
  function CompareVersions(A, B : string) : Integer;

  {$IFDEF MSWINDOWS}
  (* Checks whether an executable was compiled for X64 *)
  function IsEXEx64(const EXEName: string): Boolean;
  (* Checks whether a DLL was compiled for X64 *)
  function Isx64(const FileName: string): Boolean;
  (* Checks whether a Python version is registered and returns the related info *)
  function GetRegisteredPythonVersion(SysVersion: string;
    out PythonVersion: TPythonVersion): Boolean;
  (* Returns all registered Python versions *)
  function GetRegisteredPythonVersions(const MinVersion: string = '0.0';
    const MaxVersion: string = '100.100'): TPythonVersions;
  (* Returns the highest numbered registered Python version *)
  function GetLatestRegisteredPythonVersion(out PythonVersion: TPythonVersion;
    const MinVersion: string = '0.0'; const MaxVersion: string = '100.100'): Boolean;
  function PythonVersionFromPath(const Path: string; out PythonVersion: TPythonVersion;
    AcceptVirtualEnvs: Boolean = True; const MinVersion: string = '0.0';
    const MaxVersion: string = '100.100'): Boolean;
  {$ENDIF}

implementation

Uses
  {$IFDEF MSWINDOWS}
  Windows,
  Registry,
  {$ENDIF}
  SysUtils,
  Math,
  PythonEngine;

{ TPythonVersion }

function TPythonVersion.GetDLLName: string;
begin
  {$IFDEF MSWINDOWS}
  Result := SysVersion;
  Delete(Result, 2, 1);
  Result := 'python' + Result + '.dll';
  {$ELSE}
  Result := 'libpython' + SysVersion + '.so';
  {$ENDIF}
end;

function TPythonVersion.ExpectedArchitecture: string;
begin
  Result := '';
  {$IF Defined(CPUX64) or Defined(CPU64) or Defined(CPU64bits)}
  Result := '64bit';
  {$ENDIF}
  if Result = '' then
    Result := '32bit';
end;

procedure TPythonVersion.AssignTo(PythonEngine: TPersistent);
begin
  if PythonEngine is TPythonEngine then
  begin
    TPythonEngine(PythonEngine).UseLastKnownVersion := False;
    TPythonEngine(PythonEngine).RegVersion := SysVersion;
    TPythonEngine(PythonEngine).DllName := DLLName;
    TPythonEngine(PythonEngine).DllPath := DLLPath;
    TPythonEngine(PythonEngine).APIVersion := ApiVersion;
    if Is_venv then begin
      TPythonEngine(PythonEngine).VenvPythonExe := PythonExecutable;
      TPythonEngine(PythonEngine).SetPythonHome(DLLPath);
    end else
      {
         Note that for conda distributions to work properly,
         you need to add Format('%s;%0:s\Library\bin;', [Version.InstallPath]
         to your Windows path if it is not there already.

         Following the advice in https://github.com/python/cpython/issues/100171
         SetPythonHome is called even for registered versions
      }
      TPythonEngine(PythonEngine).SetPythonHome(InstallPath);
  end;
end;

function TPythonVersion.GetApiVersion: integer;
begin
  if  CompareVersions(SysVersion, '2.4') < 0 then
    Result := 1013
  else
    Result := 1012;
end;

function TPythonVersion.GetDisplayName: string;
Var
  FormatStr: string;
begin
  if FDisplayName = '' then
  begin
    if Is_conda then
      FormatStr := 'Conda %s (%s)'
    else
      FormatStr := 'Python %s (%s)';
    if Is_virtualenv then
      FormatStr := FormatStr + ' -virtualenv'
    else if Is_venv then
      FormatStr := FormatStr + ' -venv';

    FDisplayName := Format(FormatStr, [SysVersion, SysArchitecture]);
  end;
  Result := FDisplayName;
end;

function TPythonVersion.GetHelpFile: string;
var
  PythonHelpFilePath: string;
  HtmlIndex: string;
  Res: Integer;
  SR: TSearchRec;
begin
  Result := FHelpFile;
  // for unregistered Python or python 11
  if ((Result = '') or (ExtractFileExt(Result) = '.html')) and (InstallPath <> '') then
  begin
    PythonHelpFilePath := IncludeTrailingPathDelimiter(InstallPath) + 'Doc\python*.chm';
    Res := FindFirst(PythonHelpFilePath, faAnyFile, SR);
    if Res = 0 then
      Result := IncludeTrailingPathDelimiter(InstallPath) + 'Doc\' + SR.Name
    else if Result = '' then
    begin
      HtmlIndex := IncludeTrailingPathDelimiter(InstallPath) + 'Doc\html\index.html';
      if FileExists(HtmlIndex) then
        Result := HtmlIndex;
    end;
    FindClose(SR);
  end;
end;

function TPythonVersion.GetIsPython3K: Boolean;
begin
  try
    Result := StrToInt(SysVersion[1]) >= 3;
  except
    Result := False;
  end;
end;

function TPythonVersion.GetPythonExecutable: string;
begin
  Result := IncludeTrailingPathDelimiter(InstallPath) + 'python.exe';
  if not FileExists(Result) then begin
    Result := IncludeTrailingPathDelimiter(InstallPath) +  'Scripts' + PathDelim + 'python.exe';
    if not FileExists(Result) then Result := '';
  end;
end;

function TPythonVersion.GetSysArchitecture: string;
begin
  Result := fSysArchitecture;
  if Result = '' then
    Result := 'Unknown platform';
end;

function TPythonVersion.Is_conda: Boolean;
begin
  Result := DirectoryExists(IncludeTrailingPathDelimiter(InstallPath) + 'conda-meta');
end;

function TPythonVersion.Is_venv: Boolean;
{
  Check weather this is python venv folder introduced in python 3.5
  Note: venv is different from virtualenv
}
begin
  Result := not IsRegistered and (InstallPath <> DLLPath) and
    FileExists(IncludeTrailingPathDelimiter(InstallPath) + 'pyvenv.cfg');
end;

function TPythonVersion.Is_virtualenv: Boolean;
begin
  Result := Is_venv and FileExists(IncludeTrailingPathDelimiter(InstallPath) + 'Scripts' + PathDelim + DLLName);
end;

function  CompareVersions(A, B : string) : Integer;

  function GetNextNumber(var Version: string): Integer;
  var
    P: Integer;
    S: string;
  begin
    P := Pos('.', Version);
    if P > 0 then
    begin
      S := Copy(Version, 1, P - 1);
      Version := Copy(Version, P + 1, Length(Version) - P);
    end
    else
    begin
      S := Version;
      Version := '';
    end;
    Result := StrToIntDef(S, 0)
  end;

var
  N1, N2: Integer;
begin
  Result := 0;
  repeat
    N1 := GetNextNumber(A);
    N2 := GetNextNumber(B);
    if N2 > N1 then
    begin
      Result := 1;
      Exit;
    end
    else
    if N2 < N1 then
    begin
      Result := -1;
      Exit;
    end
  until (A = '') and (B = '');
end;

{$IFDEF MSWINDOWS}
function IsEXEx64(const EXEName: string): Boolean;
var
  BinaryType: DWORD;
begin
  Result := FileExists(EXEName) and
    GetBinaryType(PChar(ExeName), Binarytype) and
      (BinaryType = SCS_64BIT_BINARY);
end;

function Isx64(const FileName: string): Boolean;
var
  Strm : TFileStream;
  Header: TImageDosHeader;
  ImageNtHeaders: TImageNtHeaders;
begin
  Strm := TFileStream.Create(FileName, fmOpenRead);
  try
    Strm.ReadBuffer(Header, SizeOf(Header));
    if (Header.e_magic <> IMAGE_DOS_SIGNATURE) or (Header._lfanew = 0) then
      Exit(False);
    Strm.Position := Header._lfanew;
    Strm.ReadBuffer(ImageNtHeaders, SizeOf(ImageNtHeaders));
    if ImageNtHeaders.Signature <> IMAGE_NT_SIGNATURE then
      Exit(False);
    Result := ImageNtHeaders.FileHeader.Machine <> IMAGE_FILE_MACHINE_I386;
  finally
    Strm.Free;
  end;
end;

function GetRegisteredPythonVersion(SysVersion: string;
  out PythonVersion: TPythonVersion): Boolean;
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

  function ReadFromRegistry(Root: HKEY; key: string): Boolean;
  begin
    Result := False;
    try
      with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
        try
          RootKey := Root;
          if OpenKey(Key + '\InstallPath', False) then begin
            PythonVersion.InstallPath := ReadString('');
            if PythonVersion.IsAllUsers and (CompareVersions(SysVersion, '3.5') > 0) then
              PythonVersion.DLLPath := ''
            else
              PythonVersion.DLLPath := PythonVersion.InstallPath;
            CloseKey;
          end;
          Result := (PythonVersion.InstallPath <> '') and
            DirectoryExists(ExcludeTrailingPathDelimiter(PythonVersion.InstallPath));
          if not Result then Exit;

          if OpenKey(Key, False) then begin
            PythonVersion.DisplayName := ReadString('DisplayName');
            PythonVersion.Version := ReadString('Version');
            PythonVersion.fSysArchitecture :=  ReadString('SysArchitecture');
            CloseKey;
          end;
          if OpenKey(Key + '\PythonPath', False) then begin
            PythonVersion.PythonPath := ReadString('');
            CloseKey;
          end;
          if OpenKey(Key + '\Help\Main Python Documentation', False) then begin
            PythonVersion.HelpFile := ReadString('');
            CloseKey;
          end;
        finally
          Free;
        end;
    except
    end;

  end;

var
  key: string;
  VersionSuffix: string;
  APythonVersion: TPythonVersion;
begin
  // Initialize PythohVersion
  Finalize(PythonVersion);
  FillChar(PythonVersion, SizeOf(TPythonVersion), 0);

  VersionSuffix := '';
  {$IFDEF CPUX86}
  if CompareVersions(SysVersion, '3.5') <= 0 then
    VersionSuffix := '-32';
  {$ENDIF}
  key := Format('\Software\Python\PythonCore\%s%s', [SysVersion, VersionSuffix]);


  PythonVersion.SysVersion := SysVersion;
  // First try HKEY_CURRENT_USER as per PEP514
  PythonVersion.IsAllUsers := False;
  Result := ReadFromRegistry(HKEY_CURRENT_USER, key);

  //Then try for an all user installation
  if not Result then begin
    PythonVersion.IsAllUsers := True;
    Result := ReadFromRegistry(HKEY_LOCAL_MACHINE, key);
    if PythonVersion.fSysArchitecture = '' then
      // for all user installations we can be sure.
      /// But not for local user installations
      PythonVersion.fSysArchitecture := PythonVersion.ExpectedArchitecture;
  end;

  if Result and (PythonVersion.fSysArchitecture = '') then begin
    // We need to check it is the proper platform
    Result := PythonVersionFromPath(PythonVersion.InstallPath, APythonVersion);
    if Result  then
      PythonVersion.fSysArchitecture := PythonVersion.ExpectedArchitecture;
  end;

  PythonVersion.IsRegistered := Result;
end;

function GetRegisteredPythonVersions(const MinVersion: string = '0.0';
  const MaxVersion: string = '100.100'): TPythonVersions;
Var
  Count: Integer;
  I: Integer;
  PythonVersion : TPythonVersion;
begin
  Count := 0;
  SetLength(Result, High(PYTHON_KNOWN_VERSIONS));
  for I := High(PYTHON_KNOWN_VERSIONS) downto 1 do
  begin
    if CompareVersions(PYTHON_KNOWN_VERSIONS[I].RegVersion, MaxVersion) < 0 then
      continue;
    if CompareVersions(PYTHON_KNOWN_VERSIONS[I].RegVersion, MinVersion) > 0 then
      break;
    if GetRegisteredPythonVersion(PYTHON_KNOWN_VERSIONS[I].RegVersion, PythonVersion) then
    begin
      Result[Count] := PythonVersion;
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

function GetLatestRegisteredPythonVersion(out PythonVersion: TPythonVersion;
  const MinVersion: string = '0.0'; const MaxVersion: string = '100.100'): Boolean;
Var
  I: Integer;
begin
  Result := False;
  for I := High(PYTHON_KNOWN_VERSIONS) downto 1 do
  begin
    if CompareVersions(PYTHON_KNOWN_VERSIONS[I].RegVersion, MaxVersion) < 0 then
      continue;
    if CompareVersions(PYTHON_KNOWN_VERSIONS[I].RegVersion, MinVersion) > 0 then
      break;
    if GetRegisteredPythonVersion(PYTHON_KNOWN_VERSIONS[I].RegVersion, PythonVersion) then
      Exit(True);
  end;
end;

function PythonVersionFromPath(const Path: string; out PythonVersion: TPythonVersion;
  AcceptVirtualEnvs: Boolean = True; const MinVersion: string = '0.0';
  const MaxVersion: string = '100.100'): Boolean;

  function FindPythonDLL(APath : string): string;
  Var
    FindFileData: TWIN32FindData;
    Handle : THandle;
    DLLFileName: string;
  begin
    Result := '';
    Handle := FindFirstFile(PWideChar(APath+'\python*.dll'), FindFileData);
    if Handle = INVALID_HANDLE_VALUE then Exit;  // not python dll
    DLLFileName:= FindFileData.cFileName;
    // skip if python3.dll was found
    if Length(DLLFileName) <= 11 then FindNextFile(Handle, FindFileData);
    if Handle = INVALID_HANDLE_VALUE then Exit;
    Windows.FindClose(Handle);
    DLLFileName:= FindFileData.cFileName;
    if Length(DLLFileName) > 11 then
      Result := DLLFileName;
  end;

  function GetVenvBasePrefix(InstallPath: string): string;
  var
    SL : TStringList;
  begin
    SL := TStringList.Create;
    try
       try
         SL.LoadFromFile(IncludeTrailingPathDelimiter(InstallPath)+'pyvenv.cfg');
         Result := Trim(SL.Values['home']);
         if Result = '' then
           Result := Trim(SL.Values['home ']);
       except
       end;
    finally
      SL.Free;
    end;
  end;

Var
  DLLFileName: string;
  DLLPath: string;
  SysVersion: string;
  BasePrefix: string;
  I: integer;
begin
  Result := False;
  Finalize(PythonVersion);
  FillChar(PythonVersion, SizeOf(TPythonVersion), 0);
  DLLPath := ExcludeTrailingPathDelimiter(Path);

  PythonVersion.InstallPath := DLLPath;

  DLLFileName := FindPythonDLL(DLLPath);

  if DLLFileName = '' then begin
    if AcceptVirtualEnvs and PythonVersion.Is_venv then
    begin
      BasePrefix := GetVenvBasePrefix(PythonVersion.InstallPath);
      if (BasePrefix <> '') and PythonVersionFromPath(BasePrefix, PythonVersion, False) then
      begin
        //  Install path points to venv but the rest of the info is from base_prefix
        PythonVersion.InstallPath := ExcludeTrailingPathDelimiter(Path);
        Result := True;
      end;
    end;
    Exit;
  end;

  // check if same platform
  try
    if {$IFDEF CPUX64}not {$ENDIF}IsEXEx64(DLLPath+'\python.exe') then Exit;
  except
    Exit;
  end;
  PythonVersion.DLLPath := DLLPath;

  SysVersion := SysVersionFromDLLName(DLLFileName);

  PythonVersion.SysVersion := SysVersion;
  PythonVersion.fSysArchitecture := PythonVersion.ExpectedArchitecture;

  if (CompareVersions(MinVersion, SysVersion) >= 0) and
    (CompareVersions(MaxVersion, SysVersion) <= 0)
  then
    // Full search in case some python version is not supported
    for I := High(PYTHON_KNOWN_VERSIONS) downto 1 do
      if PYTHON_KNOWN_VERSIONS[I].RegVersion = SysVersion then begin
        Result := True;
        break;
      end;
end;

{$ENDIF}

end.
