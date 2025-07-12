unit uMain;

interface

uses PythonEngine;

function PyInit_DemoModule: PPyObject; cdecl;

implementation
uses
  Winapi.Windows,
  System.Math,
  WrapDelphi;

var
  gEngine : TPythonEngine = nil;
  gModule : TPythonModule = nil;

function IsPrime(x: Integer): Boolean;
// Naive implementation.  It is just a demo
begin
  if (x <= 1) then Exit(False);

  var q := Floor(Sqrt(x));
  for var i := 2 to q do
    if (x mod i = 0) then
      Exit(False);
  Exit(True);
end;

function delphi_is_prime(self, args : PPyObject) : PPyObject; cdecl;
var
  N: Integer;
begin
  with gEngine do
    if PyArg_ParseTuple( args, 'i', @N ) <> 0 then
    begin
      if IsPrime(N) then
        Result := PPyObject(Py_True)
      else
         Result := PPyObject(Py_False);
      Py_INCREF(Result);
    end
    else
       Result := nil;
end;

function PyInit_DemoModule: PPyObject;
begin
  if not Assigned(gEngine) then
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.UseLastKnownVersion := True;

    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    gModule.ModuleName := 'DemoModule';
    gModule.AddMethod('is_prime', delphi_is_prime, 'is_prime(n) -> bool' );

    // We need to set this so that the module is not created by Initialzize
    gModule.IsExtensionModule := True;
    gModule.MultInterpretersSupport := mmiPerInterpreterGIL;

    gEngine.LoadDllInExtensionModule;
  except
    Exit(nil);
  end;

  // The python import machinery will create the python module from ModuleDef
  Result := gEngine.PyModuleDef_Init(@gModule.ModuleDef);
end;

initialization
finalization
  gEngine.Free;
  gModule.Free;
end.


