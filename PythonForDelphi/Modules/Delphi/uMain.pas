unit uMain;

interface

uses PythonEngine, WrapDelphi, WrapDelphiVCL;

function PyInit_Delphi: PPyObject; cdecl;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

implementation

function PyInit_Delphi: PPyObject;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;

    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    gModule.ModuleName := 'Delphi';

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;
    gDelphiWrapper.Module := gModule;

    gEngine.LoadDll;
  except
  end;
  Result := gModule.Module;
end;

initialization
finalization
  gEngine.Free;
  gModule.Free;
end.


