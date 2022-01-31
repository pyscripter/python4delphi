unit uMain;

interface

uses PythonEngine;

function PyInit_DelphiFMX: PPyObject; cdecl;

implementation

uses
  System.SysUtils, ModuleSpecs, WrapDelphi, WrapDelphiFMX;

var
  gEngine : TPythonEngineModuleAdapter;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

// This must match the pattern "PyInit_[ProjectName]"
// So if the project is named DelphiFMX then
//   the function must be PyInit_DelphiFMX
function PyInit_DelphiFMX: PPyObject;
begin
  try
    gEngine := TPythonEngineModuleAdapter.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := False;

    if not TPythonLoad.TryLoadVerFromModuleDefs(gEngine) then
      Exit(gEngine.ReturnNone);

    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    // This must match the ProjectName and the function name pattern
    gModule.ModuleName := 'DelphiFMX';

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;
    gDelphiWrapper.Module := gModule;

    gEngine.LoadDll();
    Result := gModule.Module;
  except
    on E: Exception do begin
      WriteLn('An error occurred: ' + E.Message);
      TPythonLog.Log(E.Message);
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


