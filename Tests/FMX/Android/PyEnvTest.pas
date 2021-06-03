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
  {$IFDEF ANDROID}
  TDUnitX.RegisterTestFixture(TPyEnvTest);
  {$ENDIF}

end.
