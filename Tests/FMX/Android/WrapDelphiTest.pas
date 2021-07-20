(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'WrapDelphiTest'    Copyright (c) 2021                   *)
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
(*  Functionality:  Test unit for WrapDelphi module                       *)
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

unit WrapDelphiTest;

interface

uses
  Types,
  DUnitX.TestFramework,
  PythonEngine,
  WrapDelphi;

type
  TFruit = (Apple, Banana, Orange);
  TFruits = set of TFruit;

  {$M+}
  ITestInterface = interface(IInterface)
    ['{AD50ADF2-2691-47CA-80AB-07AF1EDA8C89}']
    procedure SetString(const S: string);
    function GetString: string;
  end;
  {$M-}

  TSubRecord = record
    DoubleField: double;
  end;

  TTestRecord = record
    StringField: string;
    SubRecord: TSubRecord;
    procedure SetStringField(S: string);
  end;

  TFruitDynArray = TArray<TFruit>;
  TStaticArray = array[0..999] of Int64;
  TTestRttiAccess = class
  private
    FFruit: TFruit;
    FFruits: TFruits;
  public
    FruitField :TFruit;
    FruitsField: TFruits;
    StringField: string;
    DoubleField: double;
    ObjectField: TObject;
    RecordField: TTestRecord;
    InterfaceField: ITestInterface;
    function GetData: TObject;
    procedure BuyFruits(AFruits: TFruits);
    procedure SellFruits(const AFruits: TFruitDynArray);
    procedure SellFruitsInt(const AFruits:TIntegerDynArray);
    function GetDynArray: TInt64DynArray;
    function GetStaticArray: TStaticArray;
    property Fruit: TFruit read FFruit write FFruit;
    property Fruits: TFruits read FFruits write FFruits;
    function SetStringField(var Value: Integer): string; overload;
    function SetStringField(const Value: string): string; overload;
    procedure PassVariantArray(const Value: Variant);
  end;

  TTestInterfaceImpl = class(TInterfacedObject, ITestInterface)
  private
    FString: string;
    procedure SetString(const S: string);
    function GetString: string;
  end;

  [TestFixture]
  TTestWrapDelphi = class(TObject)
  private
    PythonEngine: TPythonEngine;
    DelphiModule: TPythonModule;
    PyDelphiWrapper: TPyDelphiWrapper;
    Rtti_Var: Variant;
    TestRttiAccess: TTestRttiAccess;
    Rec: TTestRecord;
    Rtti_Rec: Variant;
    FTestInterface: ITestInterface;
    Rtti_Interface: Variant;
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
    procedure TestSetProps;
    [Test]
    procedure TestObjectField;
    [Test]
    procedure TestMethodCall;
    [Test]
    procedure TestRecord;
    [Test]
    procedure TestRecordField;
    [Test]
    procedure TestInterface;
    [Test]
    procedure TestInterfaceField;
    [Test]
    procedure TestDynArrayParameters;
    [Test]
    procedure TestGetDynArray;
    [Test]
    procedure TestGetStaticArray;
    [Test]
    procedure TestMethodWithVarAndOverload;
    [Test]
    procedure TestFreeReturnedObject;
    [Test]
    procedure TestPassVariantArray;
  end;

implementation

Uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Rtti,
  VarPyth,
  WrapDelphiClasses,
  PythonLoad;


{ TTestRTTIAccess }

procedure TTestRttiAccess.BuyFruits(AFruits: TFruits);
begin
  Fruits := AFruits;
end;

{ TTestWrapDelphi }

procedure TTestWrapDelphi.TestFreeReturnedObject;
begin
  PythonEngine.ExecString(
    'from delphi import rtti_var' + sLineBreak +
    'obj = rtti_var.GetData()' + sLineBreak +
    'obj.Free()'
    );
  Assert.Pass;
end;

procedure TTestWrapDelphi.SetupFixture;
var
  Py : PPyObject;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.Name := 'PythonEngine';
  TPythonLoad.Configure(PythonEngine);

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
  TestRttiAccess.InterfaceField := TTestInterfaceImpl.Create;
  Py := PyDelphiWrapper.Wrap(TestRttiAccess, TObjectOwnership.soReference);
  DelphiModule.SetVar('rtti_var', Py);
  PythonEngine.Py_DecRef(Py);
  Py := PyDelphiWrapper.WrapRecord(@Rec, TRttiContext.Create.GetType(TypeInfo(TTestRecord)) as TRttiStructuredType);
  DelphiModule.SetVar('rtti_rec', Py);
  PythonEngine.Py_DecRef(Py);
  FTestInterface := TTestInterfaceImpl.Create;
  Py := PyDelphiWrapper.WrapInterface(TValue.From(FTestInterface));
  DelphiModule.SetVar('rtti_interface', Py);
  PythonEngine.Py_DecRef(Py);
  PythonEngine.ExecString('from delphi import rtti_var, rtti_rec, rtti_interface');
  Rtti_Var := MainModule.rtti_var;
  Rtti_Rec := MainModule.rtti_rec;
  Rtti_Interface := MainModule.rtti_interface;
end;

procedure TTestWrapDelphi.TearDownFixture;
begin
  VarClear(Rtti_Var);
  VarClear(Rtti_Rec);
  VarClear(Rtti_Interface);
  PythonEngine.Free;
  PyDelphiWrapper.Free;
  DelphiModule.Free;
  TestRttiAccess.Free;
end;

procedure TTestWrapDelphi.TestDoubleField;
begin
  TestRttiAccess.DoubleField := 3.14;
  Assert.AreEqual(double(TestRttiAccess.DoubleField), double(3.14));
  Rtti_Var.DoubleField := variant(double(1.23)); //implicitly cast to a variant to avoid a bug present in 10.4.2
  Assert.AreEqual(double(Rtti_Var.DoubleField), double(1.23));
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

procedure TTestWrapDelphi.TestGetDynArray;
var
  List: Variant;
begin
  List := Rtti_Var.GetDynArray();
  Assert.IsTrue(VarIsPythonList(List));
  Assert.AreEqual(1000000, Integer(len(List)));
  Assert.AreEqual(Int64(999999), Int64(PythonEngine.PyObjectAsVariant(PythonEngine.PyList_GetItem(ExtractPythonObjectFrom(List), 999999))));
end;

procedure TTestWrapDelphi.TestGetStaticArray;
var
  List: Variant;
begin
  List := Rtti_Var.GetStaticArray();
  Assert.IsTrue(VarIsPythonList(List));
  Assert.AreEqual(1000, Integer(len(List)));
  Assert.AreEqual(Int64(999), Int64(PythonEngine.PyObjectAsVariant(PythonEngine.PyList_GetItem(ExtractPythonObjectFrom(List), 999))));
end;

procedure TTestWrapDelphi.TestInterface;
begin
  Rtti_Interface.SetString('Test');
  Assert.IsTrue(Rtti_Interface.GetString() = 'Test');
end;

procedure TTestWrapDelphi.TestInterfaceField;
begin
  Rtti_Interface.SetString('New Value');
  Assert.IsTrue(Rtti_Interface.GetString() = 'New Value');
  Rtti_Var.InterfaceField.SetString('Old Value');
  Assert.IsTrue(Rtti_Var.InterfaceField.GetString() = 'Old Value');
  // Assign interface
  Rtti_Var.InterfaceField := Rtti_Interface;
  Assert.IsTrue(Rtti_Var.InterfaceField.GetString() = 'New Value');
  Rtti_Var.InterfaceField := None;
  Assert.IsTrue(VarIsNone(Rtti_Var.InterfaceField));
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
  myComp.x := variant(double(3.14)); //implicitly cast to a variant to avoid a bug present in 10.4.2
  Assert.IsTrue(myComp.x = 3.14);

  // Setting an object field
  rtti_var.ObjectField := myComp;
  Assert.IsTrue(rtti_var.ObjectField.Name = 'NoName');
  Assert.AreEqual(TComponent(TestRttiAccess.ObjectField).Name, 'NoName');
  rtti_var.ObjectField := None;
  Assert.IsTrue(rtti_var.ObjectField = None);
end;

procedure TTestWrapDelphi.TestPassVariantArray;
begin
  PythonEngine.ExecString(
    'from delphi import rtti_var' + sLineBreak +
    'rtti_var.PassVariantArray([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])'
    );
  Assert.Pass;
end;

procedure TTestWrapDelphi.TestRecord;
begin
  Rtti_rec.StringField := 'abcd';
  Assert.IsTrue(rtti_rec.StringField = 'abcd');
  Rtti_rec.SetStringField('1234');
  Assert.IsTrue(rtti_rec.StringField = '1234');
  Assert.AreEqual(Rec.StringField, '1234');
  Rtti_rec.SubRecord.DoubleField := variant(double(3.14)); //implicitly cast to a variant to avoid a bug present in 10.4.2
  Assert.IsTrue(rtti_rec.SubRecord.DoubleField = 3.14);
  Assert.AreEqual(Rec.SubRecord.DoubleField, 3.14);
end;

procedure TTestWrapDelphi.TestRecordField;
Var
  RecValue: Variant;
begin
  RecValue := rtti_var.RecordField;
  RecValue.StringField := 'abc';
  rtti_var.RecordField := RecValue;
  Assert.IsTrue(rtti_var.RecordField.StringField = 'abc');
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

procedure TTestWrapDelphi.TestSetProps;
begin
  rtti_var.SetProps(StringField := 'abc', DoubleField := 1.234);
  Assert.AreEqual(TestRttiAccess.StringField, 'abc');
  Assert.AreEqual(TestRttiAccess.DoubleField, 1.234);
end;

procedure TTestWrapDelphi.TestStringField;
begin
  TestRttiAccess.StringField := 'Hi';
  Assert.AreEqual(string(Rtti_Var.StringField), 'Hi');
  Rtti_Var.StringField := 'P4D';
  Assert.AreEqual(TestRttiAccess.StringField, 'P4D');
end;

procedure TTestWrapDelphi.TestDynArrayParameters;
{var
  rc: TRttiContext;
  rt: TRttiType;
  rm: TRttiMethod;
  rp: TArray<TRttiParameter>;
  ra: TArray<TValue>;}
begin
{  rc := TRttiContext.Create;
  rt := rc.GetType(TypeInfo(TTestRttiAccess));
  rm := rt.GetMethod('SellFruitsInt');
  rp := rm.GetParameters;
  SetLength(ra, 1);
  ra[0] := TValue.FromArray(TypeInfo(TIntegerDynArray), [TValue.From<Integer>(0)]);
  rm.Invoke(TestRttiAccess, ra);}
  TestRttiAccess.Fruits := [TFruit.Apple, TFruit.Banana, TFruit.Orange];
  Rtti_Var.SellFruitsInt(VarPythonCreate([0, 1], stList));
  Assert.IsTrue(TestRttiAccess.Fruits = [Orange]);
  TestRttiAccess.Fruits := [TFruit.Apple, TFruit.Banana, TFruit.Orange];
  Rtti_Var.SellFruits(VarPythonCreate([Ord(TFruit.Apple), Ord(TFruit.Banana)], stList));
  Assert.IsTrue(TestRttiAccess.Fruits = [Orange]);
end;

procedure TTestWrapDelphi.TestMethodWithVarAndOverload;
begin
  Rtti_Var.SetStringField('test');
  Assert.AreEqual('test', TestRttiAccess.StringField);
end;

function TTestRttiAccess.SetStringField(const Value: string): string;
begin
  StringField := Value;
  Result := StringField;
end;

function TTestRttiAccess.SetStringField(var Value: Integer): string;
begin
  StringField := IntToStr(Value);
  Result := StringField;
end;

function TTestRttiAccess.GetData: TObject;
begin
  Result := TStringList.Create;
end;

function TTestRttiAccess.GetDynArray: TInt64DynArray;
var
  I: Integer;
begin
  SetLength(Result, 1000000);
  for I := 0 to Length(Result) - 1 do
    Result[I] := I;
end;

function TTestRttiAccess.GetStaticArray: TStaticArray;
var
  I: Integer;
begin
  for I := 0 to Length(Result) - 1 do
    Result[I] := I;
end;

procedure TTestRttiAccess.PassVariantArray(const Value: Variant);
begin
  Assert.IsTrue(VarIsArray(Value) and (VarArrayHighBound(Value, 1) = 9));
end;

procedure TTestRttiAccess.SellFruits(const AFruits: TFruitDynArray);
var
  Fruit: TFruit;
begin
  for Fruit in AFruits do
    Exclude(FFruits, Fruit);
end;

procedure TTestRttiAccess.SellFruitsInt(const AFruits:TIntegerDynArray);
var
  Fruit: Integer;
begin
  for Fruit in AFruits do
    Exclude(FFruits, TFruit(Fruit));
end;

{ TTestRecord }

procedure TTestRecord.SetStringField(S: string);
begin
  Self.StringField := S;
end;

{ TTestInterfaceImpl }

function TTestInterfaceImpl.GetString: string;
begin
  Result := FString;
end;

procedure TTestInterfaceImpl.SetString(const S: string);
begin
  FString := S;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestWrapDelphi);
  ReportMemoryLeaksOnShutdown := True;

end.
