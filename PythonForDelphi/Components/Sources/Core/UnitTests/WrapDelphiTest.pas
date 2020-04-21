unit WrapDelphiTest;
{
   Unit Tests for the WrapDelphi module
   Adapted for Python 3
}
interface

uses
  DUnitX.TestFramework,
  PythonEngine;

type

  [TestFixture]
  TTestWrapDelphi = class(TObject)
  private
    PythonEngine : TPythonEngine;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
  end;

implementation

Uses
  SysUtils,
  Variants,
  Classes,
  VarPyth,
  WrapDelphi;


{ TTestVarPyth }

procedure TTestWrapDelphi.SetupFixture;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.Name := 'PythonEngine';
  PythonEngine.AutoLoad := False;
  PythonEngine.FatalAbort := True;
  PythonEngine.FatalMsgDlg := True;

//  PythonEngine.DllName := 'libpython3.7.dylib';
//  PythonEngine.DllPath :=
//    '/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/lib/';
//  PythonEngine.RegVersion := '3.7';
//  PythonEngine.UseLastKnownVersion := False;


  PythonEngine.UseLastKnownVersion := True;

  PythonEngine.AutoFinalize := True;
  PythonEngine.InitThreads := True;
  PythonEngine.PyFlags := [pfInteractive];
  PythonEngine.LoadDll;
end;

procedure TTestWrapDelphi.TearDownFixture;
begin
  PythonEngine.Free;
end;

end.
