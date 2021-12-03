unit uMain;

interface

uses PythonEngine, System.SysUtils;

function PyInit_DelphiVCL: PPyObject; cdecl;

implementation

uses WrapDelphi, WrapDelphiVCL, System.IOUtils, System.JSON;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

function GetModuleDir(): string;
begin
  Result := TPath.GetDirectoryName(GetModuleName(HInstance));
end;

function GetModuleDefsLogFilePath(): string;
const
  MODULE_DEFS_LOG = 'moduledefs.log';
begin
  Result := TPath.Combine(GetModuleDir(), MODULE_DEFS_LOG);
end;

function GetModuleDefsJSONFilePath(): string;
const
  MODULE_DEFS_JSON = 'moduledefs.json';
begin
  Result := TPath.Combine(GetModuleDir(), MODULE_DEFS_JSON);
end;

procedure Dump(const AText: string);
begin
  try
    TFile.AppendAllText(GetModuleDefsLogFilePath(),
      DateTimeToStr(Now()) + ': ' + AText + sLineBreak);
  except
  end;
end;

function TryLoadVerFromModuleDefs(): boolean;
var
  LPythonVer: string;
begin
  //Load Python using the moduledefs.json file - it cracks the single version limitation
  var LFilePath := GetModuleDefsJSONFilePath();
  if TFile.Exists(LFilePath) then begin
    var LJson := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFilePath));
    if LJson.TryGetValue<string>('python_ver', LPythonVer) then begin
      for var I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
        if (PYTHON_KNOWN_VERSIONS[I].RegVersion = LPythonVer) then begin
          gEngine.RegVersion := PYTHON_KNOWN_VERSIONS[I].RegVersion;
          gEngine.DllName := PYTHON_KNOWN_VERSIONS[I].DllName;
          Dump('Module has been set to Python' + gEngine.RegVersion);
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
  Dump('Module has been set to Python' + gEngine.RegVersion);
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
    gModule.ModuleName := 'DelphiVCL';

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;
    gDelphiWrapper.Module := gModule;

    gEngine.LoadDll;
  except
  end;
  Result := gModule.Module;
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


