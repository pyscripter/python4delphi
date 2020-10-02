unit module;

interface
uses Windows,Dialogs,PythonEngine, PythonDatabase;

procedure initdatabase; cdecl;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDB : TPythonDatabase;

implementation

procedure initdatabase;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gDB := TPythonDatabase.Create(nil);
    gDB.Engine := gEngine;
    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    gModule.ModuleName := 'database';
    gEngine.LoadDll;
  except
  end;
end;

initialization
finalization
  gEngine.Free;
  gModule.Free;
end.


