unit uMain;

interface

uses PythonEngine;

function PyInit_DelphiVCL: PPyObject; cdecl;

implementation

uses System.IOUtils, System.SysUtils, System.JSON, WrapDelphi, WrapDelphiVCL;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

function GetModuleDir(): string;
begin
  Result := ExtractFilePath(GetModuleName(HInstance));
end;

function GetModuleDefsDir(): string;
begin
  Result := TDirectory.GetParent(ExcludeTrailingPathDelimiter(GetModuleDir()));
end;

function GetModuleDefsLogFilePath(): string;
const
  MODULE_DEFS_LOG = 'moduledefs.log';
begin
  Result := TPath.Combine(GetModuleDefsDir(), MODULE_DEFS_LOG, false);
end;

function GetModuleDefsJSONFilePath(): string;
const
  MODULE_DEFS_JSON = 'moduledefs.json';
begin
  Result := TPath.Combine(GetModuleDefsDir(), MODULE_DEFS_JSON, false);
end;

procedure Dump(const AText: string);
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

function TryLoadVerFromModuleDefs(): boolean;
begin
  //Load Python using the moduledefs.json file - it cracks the single version limitation
  var LFilePath := GetModuleDefsJSONFilePath();
  if TFile.Exists(LFilePath) then begin
    var LJson := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFilePath));
    if not Assigned(LJson) then
      Exit(false);

    var LPythonHome := String.Empty;
    if LJson.TryGetValue<string>('python_home', LPythonHome) then begin
      gEngine.PythonHome := LPythonHome;
    end;

    var LPythonBin := String.Empty;
    if LJson.TryGetValue<string>('python_bin', LPythonBin) then begin
      gEngine.ProgramName := LPythonBin;
    end;

    var LPythonLib := String.Empty;
    if LJson.TryGetValue<string>('python_lib', LPythonLib) then begin
      gEngine.DllPath := LPythonLib;
    end;

    var LPythonSharedLib := String.Empty;
    if LJson.TryGetValue<string>('python_shared_lib', LPythonSharedLib) then begin
      gEngine.DllName := LPythonSharedLib;
    end;

    var LPythonVer: string;
    if LJson.TryGetValue<string>('python_ver', LPythonVer) then begin
      for var I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
        if (PYTHON_KNOWN_VERSIONS[I].RegVersion = LPythonVer) then begin
          gEngine.RegVersion := PYTHON_KNOWN_VERSIONS[I].RegVersion;
          if LPythonSharedLib.IsEmpty() then
            gEngine.DllName := PYTHON_KNOWN_VERSIONS[I].DllName;
          Dump(Format('Module has been set to Python %s using the module defs file', [gEngine.RegVersion]));
          Exit(true);
        end;
      end;
    end;
  end;
  Result := false;
end;

function TryLoadVerFromIncFile(): boolean;
begin
  // Adapt to the desired python version - Will only work with this version
  var PythonVersionIndex := {$I PythonVersionIndex.inc}; // 7 = 3.9
  gEngine.RegVersion := PYTHON_KNOWN_VERSIONS[PythonVersionIndex].RegVersion;
  gEngine.DllName := PYTHON_KNOWN_VERSIONS[PythonVersionIndex].DllName;
  Dump(Format('Module has been set to Python %s using the version index file', [gEngine.RegVersion]));
  Exit(true);
end;

// This must match the pattern "PyInit_[ProjectName]"
// So if the project is named DelphiVCL then
//   the function must be PyInit_DelphiVCL
function PyInit_DelphiVCL: PPyObject;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := False;

    if not TryLoadVerFromModuleDefs() then
      if not TryLoadVerFromIncFile() then
        Exit(gEngine.ReturnNone);

    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    // This must match the ProjectName and the function name pattern
    gModule.ModuleName := 'DelphiFMX';

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;
    gDelphiWrapper.Module := gModule;

    gEngine.LoadDll;
    Result := gModule.Module;
  except
    on E: Exception do begin
      WriteLn('An error occurred: ' + E.Message);
      Dump(E.Message);
      Result := gEngine.ReturnNone;
    end;
  end;
end;

initialization
  gEngine := nil;
  gModule := nil;
  gDelphiWrapper := nil;
finalization
  gEngine.Free;
  gModule.Free;
  gDelphiWrapper.Free;
end.


