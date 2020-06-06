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


