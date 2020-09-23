unit uMain;

interface

uses PythonEngine, WrapDelphi;

function PyInit_DemoModule: PPyObject; cdecl;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;

implementation


function delphi_sum(self, args : PPyObject) : PPyObject; cdecl;
var
  a, b: double;
begin
  with gEngine do
    begin
      if PyArg_ParseTuple( args, 'dd',@a, @b ) <> 0 then
        begin
          Result := VariantAsPyObject(a + b);
        end
      else
        Result := nil;
    end;
end;

function PyInit_DemoModule: PPyObject;
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
    gModule.AddMethod('sum', delphi_sum, 'sum(a, b) -> a + b' );

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


