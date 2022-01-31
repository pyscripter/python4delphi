unit ModuleSpecs;

interface

uses
  PythonEngine;

type
  TModulePaths = class
  public
    class function GetModuleDir(): string;
    class function GetModuleDefsDir(): string;
  end;

  TPythonLoad = class
  private
    class function GetModuleDefsJSONFilePath(): string;
  public
    class function TryLoadVerFromModuleDefs(const APythonEngine: TPythonEngine): boolean;
  end;

  TPythonLog = class
  private
    class function GetModuleDefsLogFilePath(): string;
  public
    class procedure Log(const AText: string);
  end;

  TPythonEngineModuleAdapter = class(TPythonEngine)
  private
    function CheckForHostSymbols(): boolean;
    procedure LoadSymbols();
  public
    procedure LoadDll();
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.JSON, System.Threading;

{ TModulePaths }

class function TModulePaths.GetModuleDir: string;
begin       //Avoid invalid chars error on MacOS
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

class function TModulePaths.GetModuleDefsDir: string;
begin
  Result := TDirectory.GetParent(ExcludeTrailingPathDelimiter(GetModuleDir()));
end;

{ TPythonLoad }

class function TPythonLoad.GetModuleDefsJSONFilePath: string;
const
  MODULE_DEFS_JSON = 'moduledefs.json';
begin
  Result := TPath.Combine(TModulePaths.GetModuleDefsDir(), MODULE_DEFS_JSON, false);
end;

class function TPythonLoad.TryLoadVerFromModuleDefs(const APythonEngine: TPythonEngine): boolean;
begin
  //Load Python using the moduledefs.json file - it cracks the single version limitation
  var LFilePath := GetModuleDefsJSONFilePath();
  if TFile.Exists(LFilePath) then begin
    var LJson := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFilePath));
    if not Assigned(LJson) then
      Exit(false);

    var LPythonHome := String.Empty;
    if LJson.TryGetValue<string>('python_home', LPythonHome) then begin
      APythonEngine.PythonHome := LPythonHome;
    end;

    var LPythonBin := String.Empty;
    if LJson.TryGetValue<string>('python_bin', LPythonBin) then begin
      APythonEngine.ProgramName := LPythonBin;
    end;

    var LPythonLib := String.Empty;
    if LJson.TryGetValue<string>('python_lib', LPythonLib) then begin
      APythonEngine.DllPath := LPythonLib;
    end;

    var LPythonSharedLib := String.Empty;
    if LJson.TryGetValue<string>('python_shared_lib', LPythonSharedLib) then begin
      APythonEngine.DllName := LPythonSharedLib;
    end;

    var LPythonVer: string;
    if LJson.TryGetValue<string>('python_ver', LPythonVer) then begin
      for var I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
        if (PYTHON_KNOWN_VERSIONS[I].RegVersion = LPythonVer) then begin
          APythonEngine.RegVersion := PYTHON_KNOWN_VERSIONS[I].RegVersion;
          if LPythonSharedLib.IsEmpty() then
            APythonEngine.DllName := PYTHON_KNOWN_VERSIONS[I].DllName;
          TPythonLog.Log(Format('Module has been set to Python %s using the module defs file', [APythonEngine.RegVersion]));
          Exit(true);
        end;
      end;
    end;
  end;
  Result := false;
end;

{ TPythonLog }

class function TPythonLog.GetModuleDefsLogFilePath: string;
const
  MODULE_DEFS_LOG = 'moduledefs.log';
begin
  Result := TPath.Combine(TModulePaths.GetModuleDefsDir(), MODULE_DEFS_LOG, false);
end;

class procedure TPythonLog.Log(const AText: string);
begin
  try
    TFile.AppendAllText(GetModuleDefsLogFilePath(),
      DateTimeToStr(Now())
    + ': '
    + AText
    + sLineBreak);
  except
    on E: Exception do
      TFile.AppendAllText(TPath.Combine(
        GetCurrentDir(), 'dump.txt', false), E.Message
        + sLineBreak
        + 'while dumping: '
        + AText
        + sLineBreak);
  end;
end;

{ TPythonEngineModuleAdapter }

function TPythonEngineModuleAdapter.CheckForHostSymbols: boolean;
type
  TPy_IsInitialized = function: integer; cdecl;
begin
  if not ModuleIsLib then
    Exit(false);

  var LPy_IsInitialized := GetProcAddress(0, 'Py_IsInitialized');
  Result := Assigned(LPy_IsInitialized)
    and (TPy_IsInitialized(LPy_IsInitialized)() <> 0);
end;

procedure TPythonEngineModuleAdapter.LoadSymbols;
begin
  BeforeLoad();
  FDLLHandle := 0;
  AfterLoad();
end;

procedure TPythonEngineModuleAdapter.LoadDll;
begin
  //If host has symbols, we load symbols from host
  {$IFDEF LINUX}
  if CheckForHostSymbols() then begin
    TPythonLog.Log('*** USING HOST SYMBOLS ***');
    PythonHome := String.Empty;
    ProgramName := String.Empty;
    DllPath := String.Empty;
    LoadSymbols();
  end else
  {$ENDIF}
    inherited LoadDll();
end;

end.
