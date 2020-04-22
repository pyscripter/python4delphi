unit WrapDelphiTest;
{
   Unit Tests for the WrapDelphi module
   Demo 31 also includes extensive unit testing of WrapDelphi
}
interface

uses
  DUnitX.TestFramework,
  PythonEngine,
  WrapDelphi;

type
  TFruit = (Apple, Banana, Orange);
  TFruits = set of TFruit;

  TTestRttiAccess = class
  private
    FFruit: TFruit;
    FFruits: TFruits;
  public
    FruitField :TFruit;
    FruitsField: TFruits;
    StringField: string;
    DoubleField: double;
    ObjectField : TObject;
    procedure BuyFruits(AFruits: TFruits);
    property Fruit: TFruit read FFruit write FFruit;
    property Fruits: TFruits read FFruits write FFruits;
  end;

  [TestFixture]
  TTestWrapDelphi = class(TObject)
  private
    PythonEngine: TPythonEngine;
    DelphiModule: TPythonModule;
    PyDelphiWrapper: TPyDelphiWrapper;
    Rtti_Var: Variant;
    TestRttiAccess : TTestRttiAccess;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Test]
    procedure TestEnumProperty;
    [Test]
    procedure TestSetProperty;
    [Test]
    procedure TestDoubleField;
    [Test]
    procedure TestEnumField;
    [Test]
    procedure TestSetField;
    [Test]
    procedure TestStringField;
    [Test]
    procedure TestObjectField;
    [Test]
    procedure TestMethodCall;
  end;

implementation

Uses
  SysUtils,
  Variants,
  Classes,
  VarPyth,
  WrapDelphiClasses;


{ TTestRTTIAccess }

procedure TTestRttiAccess.BuyFruits(AFruits: TFruits);
begin
  Fruits := AFruits;
end;


{ TTestVarPyth }

procedure TTestWrapDelphi.SetupFixture;
var
  Py : PPyObject;
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

  PyDelphiWrapper := TPyDelphiWrapper.Create(nil);

  PyDelphiWrapper.Name := 'PyDelphiWrapper';
  PyDelphiWrapper.Engine := PythonEngine;
  PyDelphiWrapper.Module := DelphiModule;

  PythonEngine.LoadDll;
  //  Then wrap the an instance our TTestRTTIAccess
  //  It will allow us to to test access to public fields and methods as well
  //  public (as well as published) properties.
  //  This time we would like the object to be destroyed when the PyObject
  //  is destroyed, so we need to set its Owned property to True;
  TestRttiAccess := TTestRTTIAccess.Create;
  Py := PyDelphiWrapper.Wrap(TestRttiAccess, TObjectOwnership.soReference);
  DelphiModule.SetVar('rtti_var', Py);
  PythonEngine.Py_DecRef(Py);
  PythonEngine.ExecString('from delphi import rtti_var');
  Rtti_Var := MainModule.rtti_var;
end;

procedure TTestWrapDelphi.TearDownFixture;
begin
  PythonEngine.Free;
  PyDelphiWrapper.Free;
  DelphiModule.Free;
  TestRttiAccess.Free;
end;

procedure TTestWrapDelphi.TestDoubleField;
begin
  TestRttiAccess.DoubleField := 3.14;
  Assert.AreEqual(double(Rtti_Var.DoubleField), double(3.14));
  Rtti_Var.DoubleField := 1.23;
  Assert.AreEqual(double(TestRttiAccess.DoubleField), double(1.23));
end;

procedure TTestWrapDelphi.TestEnumField;
begin
  TestRttiAccess.FruitField := Apple;
  Assert.IsTrue(RTTI_var.FruitField = 'Apple');
  Rtti_Var.FruitField := 'Banana';
  Assert.IsTrue(TestRttiAccess.FruitField = Banana);
end;

procedure TTestWrapDelphi.TestEnumProperty;
// Enumeration values are converted to/from strings
begin
  TestRttiAccess.Fruit := Apple;
  Assert.IsTrue(RTTI_var.Fruit = 'Apple');
  Rtti_Var.Fruit := 'Banana';
  Assert.IsTrue(TestRttiAccess.Fruit = Banana);
end;

procedure TTestWrapDelphi.TestMethodCall;
begin
  TestRttiAccess.Fruits := [];
  Assert.AreEqual(string(Rtti_Var.Fruits), '[]');
  Rtti_Var.BuyFruits(VarPythonCreate(['Apple', 'Banana'], stList));
  Assert.AreEqual(string(Rtti_Var.Fruits), '[''Apple'', ''Banana'']');
end;

procedure TTestWrapDelphi.TestObjectField;
{
  Demonstrating and testing:
  Subclassing Delphi components in Python
  Creating Delphi objects in Python
  Assigning objects to object fields
}
Var
  Script: AnsiString;
  myComp: Variant;
begin
  Script :=
  'from delphi import Component'            + sLineBreak +
  'class MyComponent(Component):'           + SLineBreak +
  '    def __init__(self, Owner):'          + SLineBreak +
  '        self._x = None'                  + SLineBreak +
  ''                                        + SLineBreak +
  '    @property'                           + SLineBreak +
  '    def x(self):'                        + SLineBreak +
  '        return self._x'                  + SLineBreak +
  ''                                        + SLineBreak +
  '    @x.setter'                           + SLineBreak +
  '    def x(self, value):'                 + SLineBreak +
  '        self._x = value'                 + SLineBreak +
  ''                                        + SLineBreak +
  'myComp = MyComponent(None)';
  ;

  PythonEngine.ExecString(Script);
  myComp := MainModule.myComp;
  // accessing inherited property
  Assert.IsTrue(myComp.Name = '');
  myComp.Name := 'NoName';
  Assert.IsTrue(myComp.Name = 'NoName');
  // accessing subclass property
  myComp.x := 3.14;
  Assert.IsTrue(myComp.x = 3.14);

  // Setting an object field
  rtti_var.ObjectField := myComp;
  Assert.IsTrue(rtti_var.ObjectField.Name = 'NoName');
  Assert.AreEqual(TComponent(TestRttiAccess.ObjectField).Name, 'NoName');
  rtti_var.ObjectField := None;
  Assert.IsTrue(rtti_var.ObjectField = None);
end;

procedure TTestWrapDelphi.TestSetField;
// Sets are converted to/from list of strings
begin
  TestRttiAccess.FruitsField := [];
  Assert.AreEqual(string(Rtti_Var.FruitsField), '[]');
  Rtti_Var.FruitsField := VarPythonCreate(['Apple', 'Banana'], stList);
  Assert.AreEqual(string(Rtti_Var.FruitsField), '[''Apple'', ''Banana'']');
  Assert.IsTrue(TestRttiAccess.FruitsField = [Apple, Banana]);
end;

procedure TTestWrapDelphi.TestSetProperty;
begin
  TestRttiAccess.Fruits := [];
  Assert.AreEqual(string(Rtti_Var.Fruits), '[]');
  Rtti_Var.Fruits := VarPythonCreate(['Apple', 'Banana'], stList);
  Assert.AreEqual(string(Rtti_Var.Fruits), '[''Apple'', ''Banana'']');
  Assert.IsTrue(TestRttiAccess.Fruits = [Apple, Banana]);
end;

procedure TTestWrapDelphi.TestStringField;
begin
  TestRttiAccess.StringField := 'Hi';
  Assert.AreEqual(string(Rtti_Var.StringField), 'Hi');
  Rtti_Var.StringField := 'P4D';
  Assert.AreEqual(TestRttiAccess.StringField, 'P4D');
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
end.
