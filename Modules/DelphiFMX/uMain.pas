unit uMain;

interface

uses PythonEngine;

function PyInit_DelphiFMX: PPyObject; cdecl;

implementation

uses WrapDelphi, WrapDelphiFMX;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

// This must match the pattern "PyInit_[ProjectName]"
// So if the project is named DelphiFMX then
//   the function must be PyInit_DelphiFMX
function PyInit_DelphiFMX: PPyObject;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := False;
    // Adapt to the desired python version - Will only work with this version
    gEngine.RegVersion := '3.9';
    gEngine.DllName := 'python39.dll';

    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    // This must match the ProjectName and the function name pattern
    gModule.ModuleName := 'DelphiFMX';

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


