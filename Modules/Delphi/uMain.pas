unit uMain;

interface

uses PythonEngine;

function PyInit_Delphi: PPyObject; cdecl;

implementation

uses WrapDelphi, WrapDelphiVCL;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

function PyInit_Delphi: PPyObject;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := False;
    // Adapt to the desired python version
    gEngine.RegVersion := '3.8';
    gEngine.DllName := 'python38.dll';

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
  gDelphiWrapper.Free;
end.


