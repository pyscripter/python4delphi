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
    class procedure AfterModuleInit(Sender: TObject);
  end;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;
  gDelphiWrapper : TPyDelphiWrapper;
  DelphiFunctions: TDelphiFunctions;

function PyInit_DemoModule: PPyObject;
begin
  if not Assigned(gEngine) then
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := True;

    gDelphiWrapper := TPyDelphiWrapper.Create(nil);
    gDelphiWrapper.Engine := gEngine;

    // !!It is important that the extension module is the last
    // Engine client created
    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    gModule.ModuleName := 'DemoModule';

    // Set IsExtensionModule so that the module is not created by Initialzize
    gModule.IsExtensionModule := True;
    gModule.MultInterpretersSupport := mmiPerInterpreterGIL;
    gModule.OnAfterInitialization := TDelphiFunctions.AfterModuleInit;

    gDelphiWrapper.Module := gModule;

    gEngine.LoadDllInExtensionModule;
  except
    Exit(nil);
  end;

  // The python import machinery will create the python module from ModuleDef
  Result := gEngine.PyModuleDef_Init(@gModule.ModuleDef);
end;

{ TTestRttiAccess }


{ TDelphiFunctions }

class procedure TDelphiFunctions.AfterModuleInit(Sender: TObject);
var
  Py : PPyObject;
begin
  Py := gDelphiWrapper.Wrap(DelphiFunctions, TObjectOwnership.soReference);
  gModule.SetVar('delphi_funcs', Py);
  gEngine.Py_DecRef(Py);
end;

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


