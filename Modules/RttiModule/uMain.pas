unit uMain;

interface

uses PythonEngine;

function PyInit_DemoModule: PPyObject; cdecl;


implementation

Uses
  System.Math,
  WrapDelphi;

type
  TDelphiFunctions = class
  public
    class function is_prime(const N: Integer): Boolean; static;
  end;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;
  DelphiFunctions: TDelphiFunctions;



function PyInit_DemoModule: PPyObject;
var
  Py : PPyObject;
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
    gModule.ModuleName := 'DemoModule';

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;
    gDelphiWrapper.Module := gModule;

    gEngine.LoadDll;
    Py := gDelphiWrapper.Wrap(DelphiFunctions, TObjectOwnership.soReference);
    gModule.SetVar('delphi_funcs', Py);
    gEngine.Py_DecRef(Py);
  except
  end;
  Result := gModule.Module;
end;

{ TTestRttiAccess }


{ TDelphiFunctions }

class function TDelphiFunctions.is_prime(const N: Integer): Boolean;
// Naive implementation.  It is just a demo...
begin
  if (N <= 1) then Exit(False);

  var q := Floor(Sqrt(N));
  for var I := 2 to q do
    if (N mod I = 0) then
      Exit(False);
  Exit(True);
end;

initialization
  DelphiFunctions := TDelphiFunctions.Create;
finalization
  DelphiFunctions.Free;
  gEngine.Free;
  gModule.Free;
  gDelphiWrapper.Free;
end.


