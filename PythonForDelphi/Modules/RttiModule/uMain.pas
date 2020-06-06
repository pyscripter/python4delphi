unit uMain;

interface

uses PythonEngine, WrapDelphi;

type
  TTestRttiAccess = class
  public
    class function sum(const A, B: double): double; static;
  end;

function PyInit_DemoModule: PPyObject; cdecl;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;
  TestRttiAccess: TTestRttiAccess;

implementation


function PyInit_DemoModule: PPyObject;
var
  Py : PPyObject;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;

    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    gModule.ModuleName := 'DemoModule';

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;
    gDelphiWrapper.Module := gModule;


    gEngine.LoadDll;
    Py := gDelphiWrapper.Wrap(TestRttiAccess, TObjectOwnership.soReference);
    gModule.SetVar('delphi_funcs', Py);
    gEngine.Py_DecRef(Py);
  except
  end;
  Result := gModule.Module;
end;

{ TTestRttiAccess }

class function TTestRttiAccess.Sum(const A, B: double): double;
begin
  Result := A + B;
end;

initialization
  TestRttiAccess := TTestRTTIAccess.Create;
finalization
  TestRttiAccess.Free;
  gEngine.Free;
  gModule.Free;
  gDelphiWrapper.Free;
end.


