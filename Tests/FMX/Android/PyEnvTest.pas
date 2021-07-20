(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvTest'    Copyright (c) 2021                        *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  BH, Brazil                            *)
(*                                                                        *)
(*                                  PyScripter                            *)
(*                                  e-mail: pyscripter@gmail.com          *)
(*                                                                        *)
(*  Project pages:      https://github.com/Embarcadero/python4delphi      *)
(*                      https://github.com/pyscripter/python4delphi       *)
(**************************************************************************)
(*  Functionality:  Test unit for Python's environment                    *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)

unit PyEnvTest;

interface

uses
  DUnitX.TestFramework, PythonEngine;

type
  [TestFixture]
  TPyEnvTest = class
  private
    FPythonEngine: TPythonEngine;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Test]
    procedure TestLibFile();
    [Test]
    procedure TestZipFile();
    [Test]
    procedure TestExtraction();
    [Test]
    procedure TestConfigure();
  end;

implementation

uses
  PythonLoad;

{ TPyEnvTest }

procedure TPyEnvTest.SetupFixture;
begin
  FPythonEngine := TPythonEngine.Create(nil);
  FPythonEngine.Name := 'PythonEngine';
end;

procedure TPyEnvTest.TearDownFixture;
begin
  FPythonEngine.Free();
end;

procedure TPyEnvTest.TestConfigure;
begin
  TPythonLoad.Configure(FPythonEngine);
  FPythonEngine.LoadDll;
  Assert.IsTrue(FPythonEngine.IsHandleValid());
end;

procedure TPyEnvTest.TestExtraction;
begin
  TPythonLoad.Extract();
  Assert.IsTrue(TPythonLoad.HasPythonDist());
end;

procedure TPyEnvTest.TestLibFile;
begin
  Assert.IsTrue(TPythonLoad.HasPythonLib());
end;

procedure TPyEnvTest.TestZipFile;
begin
  Assert.IsTrue(TPythonLoad.HasPythonZip());
end;

initialization
  TDUnitX.RegisterTestFixture(TPyEnvTest);

end.
