unit WrapDelphiEventHandlerTest;

interface

uses
  Types,
  DUnitX.TestFramework,
  PythonEngine,
  WrapDelphi, System.Classes;

type
  TTestGetObjectEvent = procedure(Sender: TObject; var AObject: TObject) of object;
  TTestGetValueEvent = procedure(Sender: TObject; var AValue: Double) of object;

  TTest = class(TComponent)
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
  TTestWrapDelphiEventHandlers = class(TObject)
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
  TypInfo;

type
  TTestRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

  TTestGetValueEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var Value: Double);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;
    class function GetTypeInfo: PTypeInfo; override;
  end;

  TTestGetObjectEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var Obj: TObject);

  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;
    class function GetTypeInfo: PTypeInfo; override;
  end;


{ TTestRegistration }

function TTestRegistration.Name: string;
begin
  Result := 'Test';
end;

procedure TTestRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTestGetValueEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTestGetObjectEventHandler);
end;


{ TTestGetValueEventHandler }

constructor TTestGetValueEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method: TMethod;
begin
  inherited;
  Method.Code := @TTestGetValueEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TTestGetValueEventHandler.DoEvent(Sender: TObject; var Value: Double);
var
  PySender: PPyObject;
  PyValue: PPyObject;
  PyArgs: PPyObject;
  PyResult: PPyObject;
  PyValueVarParam: TPyDelphiVarParameter;
begin
  if not Assigned(PyDelphiWrapper) or not Assigned(Callable) or not PythonOk then
    Exit;
  with PyDelphiWrapper.Engine do
  begin
    PySender := PyDelphiWrapper.Wrap(Sender);
    PyValue := CreateVarParam(PyDelphiWrapper, Value);
    PyValueVarParam := PythonToDelphi(PyValue) as TPyDelphiVarParameter;
    PyArgs := PyTuple_New(2);
    PyTuple_SetItem(PyArgs, 0, PySender);
    PyTuple_SetItem(PyArgs, 1, PyValue);
    try
      PyResult := PyObject_CallObject(Callable, PyArgs);
      if Assigned(PyResult) then
      begin
        Py_XDECREF(PyResult);
        Value := PyObjectAsVariant(PyValueVarParam.Value);
      end;
    finally
      Py_DECREF(PyArgs)
    end;
    CheckError;
  end;
end;

class function TTestGetValueEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTestGetValueEvent);
end;


{ TTestGetObjectEventHandler }

constructor TTestGetObjectEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method: TMethod;
begin
  inherited;
  Method.Code := @TTestGetObjectEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TTestGetObjectEventHandler.DoEvent(Sender: TObject; var Obj: TObject);
var
  PySender: PPyObject;
  PyObj: PPyObject;
  PyArgs: PPyObject;
  PyResult: PPyObject;
  PyObjVarParam: TPyDelphiVarParameter;
begin
  if not Assigned(PyDelphiWrapper) or not Assigned(Callable) or not PythonOk then
    Exit;
  with PyDelphiWrapper.Engine do
  begin
    PySender := PyDelphiWrapper.Wrap(Sender);
    PyObj := CreateVarParam(PyDelphiWrapper, Obj);
    PyObjVarParam := PythonToDelphi(PyObj) as TPyDelphiVarParameter;
    PyArgs := PyTuple_New(2);
    PyTuple_SetItem(PyArgs, 0, PySender);
    PyTuple_SetItem(PyArgs, 1, PyObj);
    try
      PyResult := PyObject_CallObject(Callable, PyArgs);
      if Assigned(PyResult) then
      begin
        Py_XDECREF(PyResult);
        Obj := (PythonToDelphi(PyObjVarParam.Value) as TPyDelphiObject).DelphiObject;
      end;
    finally
      Py_DECREF(PyArgs)
    end;
    CheckError;
  end;
end;

class function TTestGetObjectEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTestGetObjectEvent);
end;


{ TTest }

procedure TTest.Process;
begin
  ProcessCalled := True;
  if Assigned(FOnGetObject) then
    FOnGetObject(Self, FObject);
  if Assigned(FOnGetValue) then
    FOnGetValue(Self, FValue);
end;


{ TTestWrapDelphiEventHandlers }

procedure TTestWrapDelphiEventHandlers.SetupFixture;
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

  PythonEngine.LoadDll;
end;

procedure TTestWrapDelphiEventHandlers.TearDownFixture;
begin
  PythonEngine.Free;
  DelphiWrapper.Free;
  DelphiModule.Free;
end;

procedure TTestWrapDelphiEventHandlers.TestProcessWithValue;
var
  Test: TTest;
  pyTest: PPyObject;
begin
  Test := TTest.Create(nil);
  try
    pyTest := DelphiWrapper.Wrap(Test);
    DelphiModule.SetVar('test', pyTest);
    PythonEngine.Py_DECREF(pyTest);
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


procedure TTestWrapDelphiEventHandlers.TestProcessWithObject;
var
  Test: TTest;
  pyTest: PPyObject;
begin
  Test := TTest.Create(nil);
  try
    pyTest := DelphiWrapper.Wrap(Test);
    DelphiModule.SetVar('test', pyTest);
    PythonEngine.Py_DECREF(pyTest);
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
    Assert.IsTrue(Test.ProcessCalled);
    Assert.AreSame(Test, Test.FObject);
  finally
    Test.Free;
  end;
end;

initialization

RegisteredUnits.Add(TTestRegistration.Create);

TDUnitX.RegisterTestFixture(TTestWrapDelphiEventHandlers);

end.
