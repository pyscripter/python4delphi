(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'NumberServicesTest'    Copyright (c) 2021               *)
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
(*  Functionality:  Test unit for numeric operations                      *)
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
unit NumberServicesTest;
interface
uses
  DUnitX.TestFramework,
  PythonEngine;
type
  TRandomInteger = class(TObject)
  strict private
    FValue: Integer;
  private
    procedure SetValue(const Value: Integer);
  public
    constructor Create;
    property Value: Integer read FValue write SetValue;
  end;
  PyTRandomInteger = class(TPyObject)
  private
    FRandomInteger: TRandomInteger;
  public
    constructor CreateWith(PythonType: TPythonType; args: PPyObject); override;
    // Basic services
    function Repr: PPyObject; override;
    // Number services
    function NbAdd(obj: PPyObject): PPyObject; override; // 0
    function NbSubtract(obj: PPyObject): PPyObject; override; // 1
    function NbMultiply(obj: PPyObject): PPyObject; override; // 2
    function NbRemainder(obj: PPyObject): PPyObject; override; // 4 {3 is for obsolete nsDivide}
    function NbDivmod(obj: PPyObject): PPyObject; override; // 5
    function NbPower(ob1, ob2: PPyObject): PPyObject; override; // 6
    function NbNegative: PPyObject; override; // 7
    function NbPositive: PPyObject; override; // 8
    function NbAbsolute: PPyObject; override; // 9
    function NbInvert: PPyObject; override; // 11 (10 for obsolete nsNonZero)
    function NbLShift(obj: PPyObject): PPyObject; override; // 12
    function NbRShift(obj: PPyObject): PPyObject; override; // 13
    function NbAnd(obj: PPyObject): PPyObject; override; // 14
    function NbXor(obj: PPyObject): PPyObject; override; // 15
    function NbOr(obj: PPyObject): PPyObject; override; // 16
    function NbInt : PPyObject; override; // 18 (17 is for obsolete nsCoerce)
    function NbFloat : PPyObject; override; // 20 (19 is for obsolete nsLong)
    function NbFloorDivide(obj: PPyObject): PPyObject; override; // 23 (21 is for obsolete nsOct, 22 for nsHex)
    function NbTrueDivide(obj: PPyObject): PPyObject; override; // 24
    function NbMatrixMultiply(obj: PPyObject): PPyObject; override; // 25
    function NbBool: Integer; override; // 26
  private
    function PerformArithmeticOp(obj: PPyObject; op: string): PPyObject;
  public
    destructor Destroy; override;
  end;
  [TestFixture]
  TTestNumberServices = class(TObject)
  private
    PythonType_TRndInt: TPythonType;
    FPythonModule : TPythonModule;
    PythonEngine : TPythonEngine;
    pdvainteger: integer;
    pdvbinteger: integer;
    pdvc : TPythonDelphiVar;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Test]
    procedure TestAdd; // 0
    [Test]
    procedure TestSubtract; // 1
    [Test]
    procedure TestMultiply; // 2
    [Test]
    procedure TestRemainder; // 4
    [Test]
    procedure TestDivMod; // 5
    [Test]
    procedure TestPower; // 6
    [Test]
    procedure TestNegative; // 7
    [Test]
    procedure TestPositive; // 8
    [Test]
    procedure TestAbsolute; // 9
    [Test]
    procedure TestInvert; // 11
    [Test]
    procedure TestLShift; // 12
    [Test]
    procedure TestRShift; // 13
    [Test]
    procedure TestAnd; // 14
    [Test]
    procedure TestXor; // 15
    [Test]
    procedure TestOr; // 16
    [Test]
    procedure TestInt; // 18
    [Test]
    procedure TestFloat; // 20
    [Test]
    procedure TestFloorDivide; // 23
    [Test]
    procedure TestTrueDivide; // 24
    [Test]
    procedure TestMatrixMultiply; // 25
    [Test]
    procedure TestBool; // 26
  end;
var PrintResults: Boolean = False;
implementation
uses
  System.Variants,
  System.SysUtils,
  System.Math,
  PythonLoad;
procedure AddPythonType(var PythonType: TPythonType;
                        Name: string;
                        TypeName: AnsiString;
                        Engine: TPythonEngine;
                        Module: TPythonModule;
                        PyObjectClass: TPyObjectClass;
                        Basic: TBasicServices;
                        Number: TNumberServices);
begin
  if not Assigned(PythonType) then
  begin
    PythonType := TPythonType.Create(Module);
    PythonType.Engine := Engine;
    PythonType.Module := Module;
    PythonType.Name := Name;
    PythonType.Services.Basic := Basic;
    PythonType.Services.Mapping := [];
    PythonType.Services.Sequence := [];
    PythonType.Services.Number := Number;
    PythonType.TypeName := TypeName;
    PythonType.PyObjectClass := PyObjectClass;
    PythonType.GenerateCreateFunction := False;
    PythonType.Initialize;
  end;
end;
  function GetVal(AModule : PPyObject; AVarName : AnsiString) : PPyObject;
  begin
    with GetPythonEngine do
    begin
      Result := PyObject_GetAttrString(AModule, PAnsiChar(AVarName));
      if PyErr_Occurred <> nil then
        PyErr_Clear
      else
        Py_XDecRef(Result); // keep a borrowed reference.
    end
  end;
procedure TTestNumberServices.SetupFixture;
var
  Py : PPyObject;
  valpy: TPyObject;
  val: PyTRandomInteger;
begin
  PythonEngine := TPythonEngine.Create(nil);
  PythonEngine.Name := 'PythonEngine';
  TPythonLoad.Configure(PythonEngine);
  PythonEngine.LoadDll;
  // python module
  FPythonModule := TPythonModule.Create(GetPythonEngine);
  FPythonModule.Engine := GetPythonEngine;
  FPythonModule.ModuleName := 'testing123';
  FPythonModule.Initialize;
  PythonType_TRndInt := nil;
  AddPythonType(PythonType_TRndInt, 'PythonType_RndInt', 'TRandomInteger',
    GetPythonEngine, FPythonModule, PyTRandomInteger,
    [ bsGetAttrO, bsSetAttrO, bsRepr ],
    [ nsAdd,
      nsSubtract,
      nsMultiply,
      nsTrueDivide,
      nsRemainder,
      nsDivmod,
      nsPower,
      nsNegative,
      nsPositive,
      nsAbsolute,
      nsInvert,
      nsLShift,
      nsRShift,
      nsAnd,
      nsXor,
      nsOr,
      nsInt,
      nsFloat,
      nsFloorDivide,
      nsMatrixMultiply,
      nsBool ]);
  // import our module
  GetPythonEngine.Run_CommandAsString('import ' + FPythonModule.ModuleName, single_input);
  pdvc := TPythonDelphiVar.Create(nil);
  pdvc.Engine := GetPythonEngine;
  pdvc.Module := '__main__';
  pdvc.VarName := 'c';
  pdvc.Initialize;
  GetPythonEngine.Run_CommandAsString('aa = testing123.TRandomInteger()', single_input);
  GetPythonEngine.Run_CommandAsString('bb = testing123.TRandomInteger()', single_input);
  py := GetVal(GetPythonEngine.GetMainModule, 'aa');
  valpy := PythonToDelphi(py);
  val := valpy as PyTRandomInteger;
  if Assigned(Py) then
  begin
    pdvainteger := val.FRandomInteger.Value;
  end;
  py := GetVal(GetPythonEngine.GetMainModule, 'bb');
  valpy := PythonToDelphi(py);
  val := valpy as PyTRandomInteger;
  if Assigned(Py) then
  begin
    pdvbinteger := val.FRandomInteger.Value;
  end;
end;
{ PyTRandomInteger }
function PythonToTRandomInteger(obj: PPyObject): TRandomInteger;
begin
  if obj = GetPythonEngine.Py_None then
    Result := nil
  else
    Result := TRandomInteger(PyTRandomInteger(PythonToDelphi(obj)).FRandomInteger);
end;
function PyTRandomInteger.Repr: PPyObject;
var
  info: string;
begin
  with GetPythonEngine do
  begin
    info := 'NIL';
    // regular
    if Assigned(FRandomInteger) then
    begin
      info := IntToStr(FRandomInteger.Value);
    end;
    Result := VariantAsPyObject(info);
  end;
end;

{ PyTRandomInteger }
constructor PyTRandomInteger.CreateWith(PythonType: TPythonType; args:
    PPyObject);
var
  val1: PPyObject;
begin
  with GetPythonEngine do
  begin
    try
      inherited;
      // create object
      FRandomInteger := TRandomInteger.Create;
      // try to parse the parameter
      val1 := nil;
      if Assigned(args) then
        // try to parse
        if (PyArg_ParseTuple(args, '|O:CreateWith', @val1) <> 0) and Assigned(val1) then
          FRandomInteger.Value := PythonToTRandomInteger(val1).Value;
    except
      on e: Exception do
        PyErr_SetString(PyExc_Exception^, PAnsiChar(AnsiString(e.Message)));
    end;
  end;
end;
function PyTRandomInteger.NbAdd(obj: PPyObject): PPyObject; // 0
begin
  Result := PerformArithmeticOp(obj, '+');
end;
function PyTRandomInteger.NbSubtract(obj: PPyObject): PPyObject; // 1
begin
  Result := PerformArithmeticOp(obj, '-');
end;
function PyTRandomInteger.NbMultiply(obj: PPyObject): PPyObject; // 2
begin
  Result := PerformArithmeticOp(obj, '*');
end;
function PyTRandomInteger.NbRemainder(obj: PPyObject): PPyObject; // 4
begin
  Result := PerformArithmeticOp(obj, '%');
end;
function PyTRandomInteger.NbDivmod(obj: PPyObject): PPyObject; // 5
begin
  Result := PerformArithmeticOp(obj, 'divmod');
end;
function PyTRandomInteger.NbPower(ob1, ob2: PPyObject): PPyObject; // 6
begin
  Result := PerformArithmeticOp(ob1, '^');
end;
function  PyTRandomInteger.NbNegative: PPyObject; // 7
var
  arg1: Integer;
begin
  with GetPythonEngine do
  begin
    arg1 := FRandomInteger.Value;
    Result := VariantAsPyObject(-arg1);
  end
end;
function PyTRandomInteger.NbPositive: PPyObject; // 8
var
  arg1: Integer;
begin
  with GetPythonEngine do
  begin
    arg1 := FRandomInteger.Value;
    Result := VariantAsPyObject(+arg1);
  end
end;
destructor PyTRandomInteger.Destroy;
begin
  FRandomInteger.Free;
  inherited;
end;
function PyTRandomInteger.NbAbsolute: PPyObject; // 9
begin
  with GetPythonEngine do
  begin
    Result := VariantAsPyObject(Abs(FRandomInteger.Value));
  end;
end;
function PyTRandomInteger.NbInvert: PPyObject; // 11
begin
  with GetPythonEngine do
  begin
    Result := VariantAsPyObject(1-(FRandomInteger.Value+1));
  end;
end;
function PyTRandomInteger.NbLShift(obj: PPyObject): PPyObject; // 12
begin
  Result := PerformArithmeticOp(obj, '<<');
end;
function PyTRandomInteger.NbRShift(obj: PPyObject): PPyObject; // 13
begin
  Result := PerformArithmeticOp(obj, '>>');
end;
function PyTRandomInteger.NbAnd(obj: PPyObject): PPyObject; // 14
begin
  Result := PerformArithmeticOp(obj, 'and');
end;
function PyTRandomInteger.NbXor(obj: PPyObject): PPyObject; // 15
begin
  Result := PerformArithmeticOp(obj, 'xor');
end;
function PyTRandomInteger.NbOr(obj: PPyObject): PPyObject; // 16
begin
  Result := PerformArithmeticOp(obj, 'or');
end;
function PyTRandomInteger.NbInt: PPyObject; // 18
begin
  with GetPythonEngine do
  begin
    Result := VariantAsPyObject(FRandomInteger.Value);
  end;
end;
function PyTRandomInteger.NbFloat: PPyObject; // 20
begin
  with GetPythonEngine do
  begin
    Result := VariantAsPyObject(Single(FRandomInteger.Value));
  end;
end;
function PyTRandomInteger.nbFloorDivide(obj: PPyObject): PPyObject; // 23
begin
  Result := PerformArithmeticOp(obj, 'floordivide');
end;
function PyTRandomInteger.NbTrueDivide(obj: PPyObject) : PPyObject; // 24
begin
  Result := PerformArithmeticOp(obj, '/');
end;
function PyTRandomInteger.NbMatrixMultiply(obj: PPyObject): PPyObject; // 25
begin
  Result := PerformArithmeticOp(obj, '@');
end;

function PyTRandomInteger.NbBool: Integer; // 26
begin
  with GetPythonEngine do
  begin
    Result := IfThen(FRandomInteger.Value=0, Ord(False), Ord(True));
  end;
end;

function PyTRandomInteger.PerformArithmeticOp(obj: PPyObject; op: string): PPyObject;
var
  val: TPyObject;
  arg1, arg2: Integer;
  VarArray: Variant;
begin
  with GetPythonEngine do
  begin
    Result := ReturnNone;
    // convert to delphi object
    val := PythonToDelphi(obj);
    // we can only add the same type
    if (val.PythonType = Self.PythonType) then
    begin
      arg1 := FRandomInteger.Value;
      arg2 := PyTRandomInteger(val).FRandomInteger.Value;
      case op[1] of
        '+': Result := VariantAsPyObject(arg1 + arg2);
        '-': Result := VariantAsPyObject(arg1 - arg2);
        '/': Result := VariantAsPyObject(arg1 div arg2);
        '*': Result := VariantAsPyObject(arg1 * arg2);
        '@': Result := VariantAsPyObject(not (arg1 * arg2)); // intentionally different from '*'
        '%': Result := VariantAsPyObject(arg1 mod arg2);
        '^': Result := VariantAsPyObject(System.Math.Power(Double(arg1), Double(arg2)));
      else
        begin
          if op='divmod' then
          begin
            VarArray := VarArrayCreate([0, 1], varInteger);
            VarArrayPut(VarArray, arg1 div arg2, [0]);
            VarArrayPut(VarArray, arg1 mod arg2, [1]);
            Result := VariantAsPyObject(VarArray);
          end
          else if op='>>' then
          begin
            Result := VariantAsPyObject(arg1 shr arg2);
          end
          else if op='<<' then
          begin
            Result := VariantAsPyObject(arg1 shl arg2);
          end
          else if op='and' then
          begin
            Result := VariantAsPyObject(arg1 and arg2);
          end
          else if op='xor' then
          begin
            Result := VariantAsPyObject(arg1 xor arg2);
          end
          else if op='or' then
          begin
            Result := VariantAsPyObject(arg1 or arg2);
          end
          else if op='floordivide' then
          begin
            Result := VariantAsPyObject(arg1 div arg2);
          end;
        end;
      end;
    end
    else // the arguments were not right
      Result := nil;
  end;
end;
{ TRandomInteger }
constructor TRandomInteger.Create;
begin
  inherited;
  FValue := 1+Random(100); // Result interval [1, 101] so safe to test division
end;
procedure TRandomInteger.SetValue(const Value: Integer);
begin
  FValue := Value;
end;
procedure TTestNumberServices.TearDownFixture;
begin
  PythonEngine.Free;
  pdvc.Free;
end;
// nsAdd
procedure TTestNumberServices.TestAdd; // 0
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa+bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger+pdvbinteger);
end;
// nsSubtract
procedure TTestNumberServices.TestSubtract; // 1
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa-bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger-pdvbinteger);
end;
// nsMultiply
procedure TTestNumberServices.TestMultiply; // 2
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa*bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger*pdvbinteger);
end;
// nsRemainder
procedure TTestNumberServices.TestRemainder; // 4
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa%bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger mod pdvbinteger);
end;
// nsDivmod
procedure TTestNumberServices.TestDivMod; // 5
var
  VarArr: Variant;
  res0, res1: Integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=divmod(aa,bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  VarArr := pdvc.Value;
  res0 := VarArr[0];
  res1 := VarArr[1];
  Assert.AreEqual(res0, pdvainteger div pdvbinteger);
  Assert.AreEqual(res1, pdvainteger mod pdvbinteger);
end;
// nsPower
procedure TTestNumberServices.TestPower; // 6
var
  pdvdouble: double;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=float(aa**bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvdouble := pdvc.Value;
  Assert.AreEqual(Double(pdvdouble), {Round}(System.Math.Power(Double(pdvainteger), Double(pdvbinteger))));
end;
// nsNegative
procedure TTestNumberServices.TestNegative; // 7
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(-aa)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, -pdvainteger);
end;
// nsPositive
procedure TTestNumberServices.TestPositive; // 8
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=int(+aa)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, +pdvainteger);
end;
// nsAbsolute
procedure TTestNumberServices.TestAbsolute; // 9
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=abs(-aa)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, +pdvainteger);
end;
// nsInvert
procedure TTestNumberServices.TestInvert; // 11
var
  pdvinteger: Integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=~aa', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, 1-(pdvainteger+1));
end;
// nsLShift
procedure TTestNumberServices.TestLShift; // 12
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa<<bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger shl pdvbinteger);
end;
// nsRShift
procedure TTestNumberServices.TestRShift; // 13
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa>>bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger shr pdvbinteger);
end;
// nsAnd
procedure TTestNumberServices.TestAnd; // 14
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa&bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger and pdvbinteger);
end;
// nsXor
procedure TTestNumberServices.TestXor; // 15
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa^bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger xor pdvbinteger);
end;
// nsOr
procedure TTestNumberServices.TestOr; // 16
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa|bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger or pdvbinteger);
end;
// nsInt
procedure TTestNumberServices.TestInt; // 18
var
  pdvinteger: Integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=int(aa)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger);
end;
// nsFloat
procedure TTestNumberServices.TestFloat; // 20
var
  pdvsingle: Single;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=float(aa)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvsingle := pdvc.Value;
  Assert.AreEqual(pdvsingle, single(pdvainteger));
end;
// nsFloorDivide
procedure TTestNumberServices.TestFloorDivide; // 23
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa//bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger div pdvbinteger);
end;
// nsTrueDivide
procedure TTestNumberServices.TestTrueDivide; // 24
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa/bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, pdvainteger div pdvbinteger);
end;
// nsMatrixMultiply
procedure TTestNumberServices.TestMatrixMultiply; // 25
var
  pdvinteger: integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value=(aa @ bb)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger, not (pdvainteger * pdvbinteger)); // not really a matrix mult, but who cares!
end;
// nsBool
procedure TTestNumberServices.TestBool; // 26
var
  pdvinteger: Integer;
begin
  GetPythonEngine.Run_CommandAsString('c.Value= bool(aa)', single_input);
  if PrintResults then GetPythonEngine.Run_CommandAsString('print(c)', single_input);
  pdvinteger := pdvc.Value;
  Assert.AreEqual(pdvinteger=0, pdvainteger=0)
end;
initialization
  TDUnitX.RegisterTestFixture(TTestNumberServices);
  ReportMemoryLeaksOnShutdown := True;
end.
