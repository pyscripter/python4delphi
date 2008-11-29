unit uMain;

interface

uses PythonEngine, WrapDelphi, WrapDelphiVCL;

procedure initDelphi; cdecl;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;

implementation

procedure initDelphi;
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
end;

initialization
finalization
  gEngine.Free;
  gModule.Free;
end.


