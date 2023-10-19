unit AutoWrapEventHandlerTest;

interface

uses
  Types,
  DUnitX.TestFramework,
  PythonEngine,
  WrapDelphi, System.Classes;

type
  TTestGetObjectEvent = procedure(Sender: TObject; var AObject: TObject) of object;
  TTestGetValueEvent = procedure(Sender: TObject; var AValue: Double) of object;

  TTestAuto = class(TComponent)
  private
    FObject: TObject;
    FValue: Double;
    FOnGetObject: TTestGetObjectEvent;
    FOnGetValue: TTestGetValueEvent;
    ProcessCalled: Boolean;
  public
    procedure Process;
  published
    property OnGetObject: TTestGetObjectEvent read FOnGetObject write FOnGetObject;
    property OnGetValue: TTestGetValueEvent read FOnGetValue write FOnGetValue;
  end;

  [TestFixture]
  TTestAutoWrapEventHandlers = class(TObject)
  private
    PythonEngine: TPythonEngine;
    DelphiModule: TPythonModule;
    DelphiWrapper: TPyDelphiWrapper;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Test]
    procedure TestProcessWithValue;
    [Test]
    procedure TestProcessWithObject;
  end;

implementation

uses
  System.Diagnostics,
  System.SysUtils,
  TypInfo;


{ TTest }
procedure TTestAuto.Process;
begin
  ProcessCalled := True;
  if Assigned(FOnGetObject) then
    FOnGetObject(Self, FObject);
  if Assigned(FOnGetValue) then
    FOnGetValue(Self, FValue);
end;

{ TTestAutoWrapEventHandlers }
procedure TTestAutoWrapEventHandlers.SetupFixture;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.Name := 'PythonEngine';
  PythonEngine.AutoLoad := False;
  PythonEngine.FatalAbort := True;
  PythonEngine.FatalMsgDlg := True;
  PythonEngine.UseLastKnownVersion := True;
  PythonEngine.AutoFinalize := True;
  PythonEngine.InitThreads := True;
  PythonEngine.PyFlags := [pfInteractive];
  DelphiModule := TPythonModule.Create(nil);
  DelphiModule.Name := 'DelphiModule';
  DelphiModule.Engine := PythonEngine;
  DelphiModule.ModuleName := 'delphi';
  DelphiWrapper := TPyDelphiWrapper.Create(nil);
  DelphiWrapper.Name := 'PyDelphiWrapper';
  DelphiWrapper.Engine := PythonEngine;
  DelphiWrapper.Module := DelphiModule;
  DelphiWrapper.RegisterDelphiWrapper(TPyClassWrapper<TTestAuto>);
  PythonEngine.LoadDll;
end;

procedure TTestAutoWrapEventHandlers.TearDownFixture;
begin
  PythonEngine.Free;
  DelphiWrapper.Free;
  DelphiModule.Free;
end;

procedure TTestAutoWrapEventHandlers.TestProcessWithValue;
var
  Test: TTestAuto;
begin
  Test := TTestAuto.Create(nil);
  try
    DelphiWrapper.DefineVar('test', Test);
    PythonEngine.ExecString(
      'import delphi' + LF +
      '' + LF +
      'def MyOnGetValue(sender, value):' + LF +
      '    value.Value = 3.14' + LF +
      '' + LF +
      'delphi.test.OnGetValue = MyOnGetValue' + LF +
      'delphi.test.Process()' + LF +
      ''
      );
    Assert.IsTrue(Test.ProcessCalled);
    Assert.AreEqual(Test.FValue, 3.14);
  finally
    Test.Free;
  end;
end;

procedure TTestAutoWrapEventHandlers.TestProcessWithObject;
var
  Test: TTestAuto;
begin
  Test := TTestAuto.Create(nil);
  try
    DelphiWrapper.DefineVar('test', Test);
    PythonEngine.ExecString(
      'import delphi' + LF +
      '' + LF +
      'def MyOnGetObject(sender, value):' + LF +
      '    value.Value = sender' + LF +
      '' + LF +
      'delphi.test.OnGetObject = MyOnGetObject' + LF +
      'delphi.test.Process()' + LF +
      ''
      );
    var StopWatch := TStopwatch.StartNew;
    var Count := 100000;
    for var I := 0 to Count do
      Test.Process;
    StopWatch.Stop;
    WriteLn(Format('*********** Elaplsed time for %d event calls: %d', [Count, StopWatch.ElapsedMilliseconds]));
    Assert.IsTrue(Test.ProcessCalled);
    Assert.AreSame(Test, Test.FObject);
  finally
    Test.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestAutoWrapEventHandlers);
end.
