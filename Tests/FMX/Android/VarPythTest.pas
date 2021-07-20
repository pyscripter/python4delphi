(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'VarPythTest'    Copyright (c) 2021                      *)
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
(*  Functionality:  Test unit for variants                                *)
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

unit VarPythTest;

interface

uses
  DUnitX.TestFramework,
  PythonEngine,
  PythonLoad;

type
  {$M+}
  [TestFixture]
  TVarPythTest = class
  private
    FPythonEngine: TPythonEngine;
  public
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    [Test]
    procedure TestIterator;
    [Test]
    procedure TestIntegers;
    [Test]
    procedure TestFloats;
    [Test]
    procedure TestStrings;
    [Test]
    procedure TestSequences;
    [Test]
    procedure TestMappings;
    [Test]
    procedure TestDates;
    [Test]
    procedure TestObjects;
  end;

implementation

uses
  SysUtils, StrUtils,
  Variants,
  VarPyth;

{ TVarPythTest }

procedure TVarPythTest.SetupFixture;
begin
  FPythonEngine := TPythonEngine.Create(nil);
  FPythonEngine.Name := 'PythonEngine';
  TPythonLoad.Configure(FPythonEngine);
  FPythonEngine.LoadDll;
end;
procedure TVarPythTest.TearDownFixture;
begin
  FPythonEngine.Free();
end;
procedure TVarPythTest.TestDates;
var
  a, b, _timeMod : Variant;
  c : Variant;
  _date, _date2 : TDateTime;
  _year, _month, _day : Word;
  _year2, _month2, _day2 : Word;
  _hour, _min, _sec, _msec : Word;
  _hour2, _min2, _sec2, _msec2 : Word;
begin
  _timeMod := Import('time'); // get the time module of Python
  _date := Now;
  DecodeDate( _date, _year, _month, _day );
  DecodeTime( _date, _hour, _min, _sec, _msec );
  b := _timeMod.localtime(_timeMod.time()); // same as Now in Delphi
  a := VarPythonCreate(_date);
  Assert.IsTrue( a.Length = 9 );
  Assert.IsTrue( a.GetItem(0) = _year );
  Assert.IsTrue( a.GetItem(1) = _month );
  Assert.IsTrue( a.GetItem(2) = _day );
  Assert.IsTrue( a.GetItem(3) = _hour );
  Assert.IsTrue( a.GetItem(4) = _min );
  Assert.IsTrue( a.GetItem(5) = _sec );
  Assert.IsTrue( b.Length = 9 );
  Assert.IsTrue( b.GetItem(0) = a.GetItem(0) );
  Assert.IsTrue( b.GetItem(1) = a.GetItem(1) );
  Assert.IsTrue( b.GetItem(2) = a.GetItem(2) );
  Assert.IsTrue( b.GetItem(3) = a.GetItem(3) );
  Assert.IsTrue( b.GetItem(4) = a.GetItem(4) );
  Assert.IsTrue( b.GetItem(5) = a.GetItem(5) );
  Assert.IsTrue( b.GetItem(6) = a.GetItem(6) );
  Assert.IsTrue( b.GetItem(7) = a.GetItem(7) );
  // don't test the 9th item of the tuple, because it's the daylight saving,
  // and it's not computed by the Python for Delphi.
  //Assert.IsTrue( b.GetItem(8) = a.GetItem(8) );
  _date2 := b;
  DecodeDate( _date2, _year2, _month2, _day2 );
  DecodeTime( _date2, _hour2, _min2, _sec2, _msec2 );
  Assert.IsTrue( _year2 = _year );
  Assert.IsTrue( _month2 = _month );
  Assert.IsTrue( _day2 = _day );
  Assert.IsTrue( _hour2 = _hour );
  Assert.IsTrue( _min2 = _min );
  Assert.IsTrue( _sec2 = _sec );
  // test new datetime module
  _timeMod := Import('datetime'); // get the datetime module of Python
  //or _timeMod := DatetimeModule; // get the datetime module of Python
  a := _timeMod.datetime(2002, 12, 30, 22, 15, 38, 827738);
  Assert.IsTrue(VarIsPythonDateTime(a));
  Assert.IsTrue(VarIsPythonDate(a));
  Assert.IsTrue(not VarIsPythonTime(a));
  Assert.IsTrue(not VarIsPythonDateTimeDelta(a));
  Assert.IsTrue(a.year  = 2002);
  Assert.IsTrue(a.month = 12);
  Assert.IsTrue(a.day   = 30);
  Assert.IsTrue(a.hour  = 22);
  Assert.IsTrue(a.minute   = 15);
  Assert.IsTrue(a.second   = 38);
  Assert.IsTrue(a.microsecond  = 827738);
  _date := a;
  DecodeDate( _date, _year, _month, _day );
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert.IsTrue(_year  = 2002);
  Assert.IsTrue(_month = 12);
  Assert.IsTrue(_day   = 30);
  Assert.IsTrue(_hour  = 22);
  Assert.IsTrue(_min   = 15);
  Assert.IsTrue(_sec   = 38);
  Assert.IsTrue(_msec  = 827738 div 1000);
  a := _timeMod.date(2002, 12, 30);
  Assert.IsTrue(not VarIsPythonDateTime(a));
  Assert.IsTrue(VarIsPythonDate(a));
  Assert.IsTrue(not VarIsPythonTime(a));
  Assert.IsTrue(not VarIsPythonDateTimeDelta(a));
  _date := a;
  DecodeDate( _date, _year, _month, _day );
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert.IsTrue(_year  = 2002);
  Assert.IsTrue(_month = 12);
  Assert.IsTrue(_day   = 30);
  Assert.IsTrue(_hour  = 0);
  Assert.IsTrue(_min   = 0);
  Assert.IsTrue(_sec   = 0);
  Assert.IsTrue(_msec  = 0);
  Assert.IsTrue(a.year  = 2002);
  Assert.IsTrue(a.month = 12);
  Assert.IsTrue(a.day   = 30);
  a := _timeMod.time(22, 15, 38, 827738);
  Assert.IsTrue(not VarIsPythonDateTime(a));
  Assert.IsTrue(not VarIsPythonDate(a));
  Assert.IsTrue(VarIsPythonTime(a));
  Assert.IsTrue(not VarIsPythonDateTimeDelta(a));
  Assert.IsTrue(a.hour  = 22);
  Assert.IsTrue(a.minute   = 15);
  Assert.IsTrue(a.second   = 38);
  Assert.IsTrue(a.microsecond  = 827738);
  _date := a;
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert.IsTrue(_hour  = 22);
  Assert.IsTrue(_min   = 15);
  Assert.IsTrue(_sec   = 38);
  Assert.IsTrue(_msec  = 827738 div 1000);
  a := DatetimeModule.datetime(2002, 12, 30, 22, 15, 38, 827738);
  b := _timeMod.datetime(2002, 12, 30, 22, 16, 38, 827738);
  c := b - a;
  Assert.IsTrue(VarIsPythonDateTimeDelta(c));
  Assert.IsTrue(c.days = 0);
  Assert.IsTrue(c.seconds = 60);
  Assert.IsTrue(c.microseconds = 0);
  _date := c;
  Assert.IsTrue(Trunc(_date)=0);
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert.IsTrue(_hour = 0);
  Assert.IsTrue(_min = 1);
  Assert.IsTrue(_sec = 0);
  Assert.IsTrue(_msec = 0);
  c := a - b;
  Assert.IsTrue(VarIsPythonDateTimeDelta(c));
  Assert.IsTrue(c.days = -1);
  Assert.IsTrue(c.seconds = 86340);
  Assert.IsTrue(c.microseconds = 0);
  _date := c;
  Assert.IsTrue(Trunc(_date)=0);
  Assert.IsTrue(_date<0);
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert.IsTrue(_hour = 0);
  Assert.IsTrue(_min = 1);
  Assert.IsTrue(_sec = 0);
  Assert.IsTrue(_msec = 0);
  c := a + (b-a);
  Assert.IsTrue(VarIsPythonDateTime(c));
  Assert.IsTrue(c = b);
  Assert.IsTrue(c <> a);
  Assert.IsTrue(a < b);
  Assert.IsTrue(b > a);
  GetPythonEngine.DatetimeConversionMode := dcmToDatetime;
  try
    _date := EncodeDate(2003, 01, 28) + EncodeTime(12, 22, 33, 450);
    a := VarPythonCreate(_date);
    Assert.IsTrue(VarIsPythonDateTime(c));
    _date2 := a;
    DecodeDate( _date, _year, _month, _day );
    DecodeTime( _date, _hour, _min, _sec, _msec );
    DecodeDate( _date2, _year2, _month2, _day2 );
    DecodeTime( _date2, _hour2, _min2, _sec2, _msec2 );
    Assert.IsTrue( _year2 = _year );
    Assert.IsTrue( _month2 = _month );
    Assert.IsTrue( _day2 = _day );
    Assert.IsTrue( _hour2 = _hour );
    Assert.IsTrue( _min2 = _min );
    Assert.IsTrue( _sec2 = _sec );
    Assert.IsTrue( _msec2 = _msec );
    Assert.IsTrue(a.year  = 2003);
    Assert.IsTrue(a.month = 01);
    Assert.IsTrue(a.day   = 28);
    Assert.IsTrue(a.hour  = 12);
    Assert.IsTrue(a.minute   = 22);
    Assert.IsTrue(a.second   = 33);
    Assert.IsTrue(a.microsecond  = 450000);
  finally
    GetPythonEngine.DatetimeConversionMode := dcmToTuple;
  end;
end;
procedure TVarPythTest.TestFloats;
var
  a, b, c : Variant;
  dbl_a, dbl_b, dbl_c : Double;
  int : Integer;
begin
  // initialize the operands
  dbl_a := 2.5;
  a := VarPythonCreate(dbl_a);
  Assert.IsTrue(VarIsPython(a));
  Assert.IsTrue(VarIsPythonNumber(a));
  Assert.IsTrue(VarIsPythonFloat(a));
  Assert.IsTrue(Double(a) = 2.5);
  dbl_b := 3.2;
  b := VarPythonCreate(dbl_b);
  Assert.IsTrue(VarIsPython(b));
  Assert.IsTrue(VarIsPythonNumber(b));
  Assert.IsTrue(VarIsPythonFloat(b));
  Assert.IsTrue(Double(b) = dbl_b); // note that Assert.IsTrue(Double(b) = 3.2) fails.
  // arithmetic operations
  //----------------------
  // addition
  c := a + b;
  // check result of operation
  Assert.IsTrue( Double(c) = (dbl_a + dbl_b) );
  // check that operation did not change the content of operands.
  Assert.IsTrue(Double(a) = dbl_a);
  Assert.IsTrue(Double(b) = dbl_b);
  // now with a litteral
  c := a + b + 1;
  Assert.IsTrue( Double(c) = (dbl_a+dbl_b+1) );
  c := a + 1 + b;
  Assert.IsTrue( Double(c) = (dbl_a+1+dbl_b) );
  c := 1 + a + b;
  Assert.IsTrue( Double(c) = (1+dbl_a+dbl_b) );
  // substraction
  c := b - a;
  Assert.IsTrue( Double(c) = (dbl_b - dbl_a) );
  // now with a litteral
  c := b - a - 1;
  Assert.IsTrue( Double(c) = (dbl_b-dbl_a-1) );
  c := b - 1 - a;
  Assert.IsTrue( Double(c) = (dbl_b-1-dbl_a) );
  c := 1 - b - a;
  Assert.IsTrue( Double(c) = (1-dbl_b-dbl_a) );
  // multiplication
  c := a * b;
  dbl_c := dbl_a * dbl_b;
  Assert.IsTrue( Double(c) = dbl_c );
  // now with a litteral
  c := a * b * 2;
  dbl_c := dbl_a * dbl_b * 2;
  Assert.IsTrue( Double(c) = dbl_c );
  c := a * 2 * b;
  dbl_c := dbl_a * 2 * dbl_b;
  Assert.IsTrue( Double(c) = dbl_c );
  c := 2 * a * b;
  dbl_c := 2 * dbl_a * dbl_b;
  Assert.IsTrue( Double(c) = dbl_c );
  // division: in Python a division between 2 integers is the same as the integer division
  c := b / a;
  dbl_c := dbl_b / dbl_a;
  Assert.IsTrue( Double(c) = dbl_c );
  // negation
  c := -a;
  Assert.IsTrue( Double(c) = -dbl_a );
  // comparisons
  //------------
  // equal
  c := a = b;
  Assert.IsTrue(c = False);
  c := a = a;
  Assert.IsTrue(c = True);
  Assert.IsTrue( a = dbl_a);
  // not equal
  c := a <> b;
  Assert.IsTrue(c = True);
  Assert.IsTrue( not (c = b) );
  c := a <> a;
  Assert.IsTrue(c = False);
  Assert.IsTrue( a = dbl_a);
  // greater than
  c := a > b; Assert.IsTrue(c = False);
  c := b > a; Assert.IsTrue(c = True);
  Assert.IsTrue( a > (dbl_a-1));
  // greater or equal than
  c := a >= b; Assert.IsTrue(c = False);
  c := b >= a; Assert.IsTrue(c = True);
  c := a >= a; Assert.IsTrue(c = True);
  Assert.IsTrue( a >= dbl_a );
  // less than
  c := a < b; Assert.IsTrue(c = True);
  c := b < a; Assert.IsTrue(c = False);
  Assert.IsTrue( a < dbl_b);
  // less or equal than
  c := a <= b; Assert.IsTrue(c = True);
  c := b <= a; Assert.IsTrue(c = False);
  c := a <= a; Assert.IsTrue(c = True);
  Assert.IsTrue( a <= dbl_a);
  // parenthesis
  c := a * ((a * b) / b);
  dbl_c := dbl_a * ((dbl_a * dbl_b) / dbl_b);
  Assert.IsTrue( c = dbl_c );
  // copy
  c := a;
  Assert.IsTrue( c = a);
  Assert.IsTrue( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.
  // casts
  int := a;
  Assert.IsTrue(int = 2);
end;
procedure TVarPythTest.TestIntegers;
var
  a, b, c : Variant;
  big : Int64;
begin
  // initialize the operands
  a := VarPythonCreate(2);
  Assert.IsTrue(VarIsPython(a));
  Assert.IsTrue(VarIsPythonNumber(a));
  Assert.IsTrue(VarIsPythonInteger(a));
  Assert.IsTrue(Integer(a) = 2);
  b := VarPythonCreate(3);
  Assert.IsTrue(VarIsPython(b));
  Assert.IsTrue(VarIsPythonNumber(b));
  Assert.IsTrue(VarIsPythonInteger(b));
  Assert.IsTrue(Integer(b) = 3);
  // arithmetic operations
  //----------------------
  // addition
  c := a + b;
  // check result of operation
  Assert.IsTrue( Integer(c) = 5 );
  // check that operation did not change the content of operands.
  Assert.IsTrue(Integer(a) = 2);
  Assert.IsTrue(Integer(b) = 3);
  // now with a litteral
  c := a + b + 1;
  Assert.IsTrue( Integer(c) = 6 );
  c := a + 1 + b;
  Assert.IsTrue( Integer(c) = 6 );
  c := 1 + a + b;
  Assert.IsTrue( Integer(c) = 6 );
  // substraction
  c := b - a;
  Assert.IsTrue( Integer(c) = 1 );
  // now with a litteral
  c := b - a - 1;
  Assert.IsTrue( Integer(c) = 0 );
  c := b - 1 - a;
  Assert.IsTrue( Integer(c) = 0 );
  c := 1 - b - a;
  Assert.IsTrue( Integer(c) = -4 );
  // multiplication
  c := a * b;
  Assert.IsTrue( Integer(c) = 6 );
  // now with a litteral
  c := a * b * 2;
  Assert.IsTrue( Integer(c) = 12 );
  c := a * 2 * b;
  Assert.IsTrue( Integer(c) = 12 );
  c := 2 * a * b;
  Assert.IsTrue( Integer(c) = 12 );
  // integer division
  c := b div a;
  Assert.IsTrue( Integer(c) = 1 );
  // division: in Python a division between 2 integers is the same as the integer division
  c := b / a;
  Assert.IsTrue( c = 1.5 );
  Assert.IsTrue( Integer(c) = 2 );
  // modulus
  c := b mod a;
  Assert.IsTrue( Integer(c) = 1 );
  c := BuiltinModule.divmod(b, a); // this returns a tuple whose first item is the result of the division,
                                   // and second item the modulo.
  if VarIsPythonSequence(c) and (c.Length = 2) then
  begin
    Assert.IsTrue(Integer(c.GetItem(0)) = 1); // division
    Assert.IsTrue(Integer(c.GetItem(1)) = 1); // modulo
  end;
  // power
  c := BuiltinModule.pow(a, b);
  Assert.IsTrue(c = 8);
  // negation
  c := -a;
  Assert.IsTrue( Integer(c) = -2 );
  // logical operations
  //------------------
  // inverse
  c := not a; // in python it would be: c = ~2
  Assert.IsTrue( Integer(c) = -3 );
  // shift left (<<)
  c := a shl b;
  Assert.IsTrue( Integer(c) = 16 );
  c := a shl 1;
  Assert.IsTrue( Integer(c) = 4 );
  // shift right (>>)
  c := a shl b;
  c := c shr b;
  Assert.IsTrue( Integer(c) = Integer(a) );
  c := b shr 1;
  Assert.IsTrue( Integer(c) = 1 );
  // and
  c := a and (a*5);
  Assert.IsTrue( Integer(c) = Integer(a) );
  c := a and 6;
  Assert.IsTrue( Integer(c) = Integer(a) );
  // or
  c := a or b;
  Assert.IsTrue( Integer(c) = 3 );
  c := a or 3;
  Assert.IsTrue( Integer(c) = 3 );
  // xor
  c := a xor b;
  Assert.IsTrue( Integer(c) = 1 );
  c := a xor 3;
  Assert.IsTrue( Integer(c) = 1 );
  // comparisons
  //------------
  // equal
  c := a = b;
  Assert.IsTrue(c = False);
  c := a = a;
  Assert.IsTrue(c = True);
  Assert.IsTrue( a = 2);
  // not equal
  c := a <> b;
  Assert.IsTrue(c = True);
  Assert.IsTrue( not (c = b) );
  c := a <> a;
  Assert.IsTrue(c = False);
  Assert.IsTrue( a = 2);
  // greater than
  c := a > b; Assert.IsTrue(c = False);
  c := b > a; Assert.IsTrue(c = True);
  Assert.IsTrue( a > 1);
  // greater or equal than
  c := a >= b; Assert.IsTrue(c = False);
  c := b >= a; Assert.IsTrue(c = True);
  c := a >= a; Assert.IsTrue(c = True);
  Assert.IsTrue( a >= 2 );
  // less than
  c := a < b; Assert.IsTrue(c = True);
  c := b < a; Assert.IsTrue(c = False);
  Assert.IsTrue( a < 6);
  // less or equal than
  c := a <= b; Assert.IsTrue(c = True);
  c := b <= a; Assert.IsTrue(c = False);
  c := a <= a; Assert.IsTrue(c = True);
  Assert.IsTrue( a <= 2);
  // parenthesis
  c := a * ((a * b) div b);
  Assert.IsTrue( c = a*2 );
  // copy
  c := a;
  Assert.IsTrue( c = a);
  Assert.IsTrue( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.
  // test long long (Int64)
  big := Int64(MaxInt)*4;
  b := VarPythonCreate(big);
  Assert.IsTrue( b = big );
  Assert.IsTrue( b <> big+1 );
  Assert.IsTrue( b > MaxInt );
  Assert.IsTrue( MaxInt < b );
  Assert.IsTrue( b+1 = big+1 );
  Assert.IsTrue( b*2 = big*2 );
  Assert.IsTrue( b div 2 = big div 2 );
  c := VarPythonCreate(True);
  Assert.IsTrue(VarIsBool(c));
  Assert.IsTrue(VarIsTrue(c));
  c := VarPythonCreate(False);
  Assert.IsTrue(VarIsBool(c));
  Assert.IsTrue(not VarIsTrue(c));
end;
procedure TVarPythTest.TestIterator;
var
  Module: Variant;
  Count: integer;
begin
  Count := 0;
  for Module in VarPyIterate(SysModule.modules) do begin
    Count := Count + 1;
    Log(Module);
  end;
  Assert.IsTrue(Count = len(SysModule.modules));
end;
procedure TVarPythTest.TestMappings;
var
  a, b, c, keys, values : Variant;
begin
  // initialize the operands
  a := NewPythonDict;
  Assert.IsTrue(VarIsPython(a));
  Assert.IsTrue(VarIsPythonMapping(a));
  Assert.IsTrue(VarIsPythonDict(a));
  // There is a bug in D2010 in which Char('a') gets translated to integer parameter
  a.SetItem( string('a'), 1 );
  a.SetItem( string('b'), 2 );
  a.SetItem( string('c'), 3 );
  Assert.IsTrue(a.Length = 3); // this is a special property that does the same as: len(a) in Python
  Assert.IsTrue(a.Length() = 3); // this is a special method that does the same as the special property
  Assert.IsTrue(len(a) = 3);
  Assert.IsTrue(a.GetItem(string('a')) = 1); // this is a special method that lets you do the same as: a[0] in Python
  Assert.IsTrue(a.GetItem(string('b')) = 2);
  Assert.IsTrue(a.GetItem(string('c')) = 3);


  b := NewPythonDict;
  Assert.IsTrue(VarIsPython(b));
  Assert.IsTrue(VarIsPythonMapping(b));
  Assert.IsTrue(VarIsPythonDict(b));
  b.SetItem( string('d'), 4 );
  b.SetItem( string('e'), 5 );
  b.SetItem( string('f'), 6 );
  Assert.IsTrue(b.Length = 3);
  Assert.IsTrue(b.Length() = 3);
  Assert.IsTrue(len(b) = 3);
  Assert.IsTrue(b.GetItem(string('d')) = 4);
  Assert.IsTrue(b.GetItem(string('e')) = 5);
  Assert.IsTrue(b.GetItem(string('f')) = 6);

  // copy
  c := a;
  Assert.IsTrue( c = a);
  Assert.IsTrue( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // dict methods
  Assert.IsTrue( Boolean(a.__contains__(string('a'))) );
  Assert.IsTrue( not Boolean(a.__contains__('abc')) );
  keys := BuiltinModule.list(a.keys());
  keys.sort();
  Assert.IsTrue( keys = VarPythonCreate(VarArrayOf(['a', 'b', 'c'])));
  values := BuiltinModule.list(a.values());
  values.sort();
  Assert.IsTrue( values = VarPythonCreate(VarArrayOf([1, 2, 3])));
  c := a;
  c.DeleteItem(string('a'));
  Assert.IsTrue( not Boolean(c.__contains__(string('a'))) );

  // test string values
  a := NewPythonDict;
  a.SetItem( string('a'), 'Hello');
  a.SetItem( string('b'), 'World!');
  a.SetItem( string('c'), '');
  Assert.IsTrue(a.GetItem(string('a')) = 'Hello');
  Assert.IsTrue(a.GetItem(string('b')) = 'World!');
  Assert.IsTrue(a.GetItem(string('c')) = '');
end;

procedure TVarPythTest.TestObjects;
var
  _main, f, a, b, c : Variant;
  val : Integer;
  _str : String;
const
  Script =
    'class XYZ(object):' + sLineBreak +
    '  pass' + sLineBreak +
    '' + sLineBreak +
    'class Foo:' + sLineBreak +
    '  def __init__(Self, Value=0):' + sLineBreak +
    '    Self.Value = Value' + sLineBreak +
    '  def __del__(Self):' + sLineBreak +
    '    print("delete", Self)' + sLineBreak +
    '  def __add__(self, other):' + sLineBreak +
    '    return Foo(self.Value + other.Value)' + sLineBreak +
    '  def Inc(Self, AValue = 1):' + sLineBreak +
    '    Self.Value = Self.Value + AValue' + sLineBreak +
    '  def GetSelf(Self):' + sLineBreak +
    '    return Self' + sLineBreak +
    '  def GetValue(Self):' + sLineBreak +
    '    return Self.Value' + sLineBreak +
    '  def SetABC(Self, A, B, C):' + sLineBreak +
    '    Self.A = A' + sLineBreak +
    '    Self.B = B' + sLineBreak +
    '    Self.C = C' + sLineBreak +
    '  def Add(Self, AFooInst):' + sLineBreak +
    '    Self.Value = Self.Value + AFooInst.Value' + sLineBreak +
    'class Bar(Foo):' + sLineBreak +
    '  def Inc(Self, AValue = 1):' + sLineBreak +
    '    Self.Value = Self.Value - AValue' + sLineBreak +
    'def Add(a, b):' + sLineBreak +
    '  return a + b' + sLineBreak +
    'def MakeList(a, b, c, d):' + sLineBreak +
    '  return [a, b, c, d]' + sLineBreak +
    '' + sLineBreak +
    'f = Foo()' + sLineBreak +
    'print("Created", f)' + sLineBreak +
    'f.Inc()' + sLineBreak +
    'f.Inc(2)' + sLineBreak +
    'b = Bar()' + sLineBreak +
    'b.Inc()' + sLineBreak +
    'b.Inc(2)';

begin
  FPythonEngine.ExecString(Script);
  _main := MainModule;
  Assert.IsTrue( VarIsPythonModule(_main) );
  Assert.IsTrue( VarIsPythonModule(SysModule) );
  Assert.IsTrue( Import('sys').version = SysModule.version );
  Log(SysModule.version);
  Assert.IsTrue( Boolean(SysModule.modules.Contains(GetPythonEngine.ExecModule)) ); // if __main__ in sys.modules
  Assert.IsTrue( VarIsSameType(_main, SysModule) );
  Assert.IsTrue( _type(_main).__name__ = 'module');
  Assert.IsTrue( BuiltinModule.type(_main).__name__ = 'module');

  Assert.IsTrue( VarIsPythonClass(_main.Foo) );
  Assert.IsTrue( VarIsPythonClass(_main.Bar) );
  Assert.IsTrue( VarIsPythonClass(_main.XYZ) );
  Assert.IsTrue( not VarIsPythonClass(_main.Foo.__add__) );
  Assert.IsTrue( not VarIsPythonClass(_main.f) );
  Assert.IsTrue( VarIsPythonCallable(_main.Foo) );
  Assert.IsTrue( VarIsPythonCallable(_main.Foo) );
  Assert.IsTrue( VarIsTrue(BuiltinModule.callable(_main.Foo)) );
  Assert.IsTrue( VarIsSame(_main.f.__class__, _main.Foo) );
  Assert.IsTrue( VarIsPythonMethod(_main.f.Inc) );
  Assert.IsTrue( VarIsPythonCallable(_main.f.Inc) );
  Assert.IsTrue( VarIsTrue(BuiltinModule.callable(_main.f.Inc)) );
  Assert.IsTrue( VarIsPythonFunction(_main.Add) );
  Assert.IsTrue( VarIsPythonCallable(_main.Add) );
  Assert.IsTrue( VarIsInstanceOf(_main.f, _main.Foo) );
  Assert.IsTrue( VarIsTrue(BuiltinModule.isinstance(_main.f, _main.Foo)) );
  Assert.IsTrue( VarIsSubclassOf(_main.Bar, _main.Foo) );
  Assert.IsTrue( VarIsTrue(BuiltinModule.issubclass(_main.Bar, _main.Foo)) );
  Assert.IsTrue( not VarIsSubclassOf(_main.Foo, _main.Bar) );
  Assert.IsTrue( VarIsInstanceOf(_main.b, _main.Foo) );
  Assert.IsTrue( not VarIsInstanceOf(_main.f, _main.Bar) );
  Assert.IsTrue( VarIsTrue( BuiltinModule.vars(_main).__contains__(string('f')) ) );
  Assert.IsTrue( VarIsTrue( BuiltinModule.dir(_main).Contains(string('f')) ) );

  f := _main.Foo(); // new instance of class Foo
  Log('Instanciate class Foo: ' + f);
  f.Inc(); // call a method without any arg, because there's a default arg.
  f.Inc(2); // call a method with one arg, overriding the default arg.
  Assert.IsTrue( VarIsPythonNumber(f.Value) );
  Assert.IsTrue( VarIsPythonInteger(f.Value) );
  Assert.IsTrue( f.Value = _main.f.Value ); // compare the result with what we did in the script
  Assert.IsTrue( f.GetValue() = _main.f.GetValue() ); // compare the result with what we did in the script
  Assert.IsTrue( VarIsPython( f.GetSelf() ) );
  Assert.IsTrue( VarIsSame( f.GetSelf(), f ) );
  Assert.IsTrue( BuiltinModule.getattr(f, 'Value') = f.Value );
  // python (+) operator overloading
  a := _main.Foo(10);
  b := _main.Foo(5);
  c := a + b;
  Assert.IsTrue(a.Value = 10);
  Assert.IsTrue(b.Value = 5);
  Assert.IsTrue(c.Value = 15);
  Log('Test -> a, b, c : ' + a.Value + ', '  + b.Value + ', '  + c.Value);
  // cascading calls
  Assert.IsTrue( f.GetSelf().GetSelf().GetSelf().GetSelf().GetValue() = _main.f.GetValue() );
  Assert.IsTrue( Boolean(f.__dict__.__contains__('Value')) );
  Assert.IsTrue( VarIsTrue( BuiltinModule.hasattr(f, 'Value') ) );
  _str := 'Value';
  Assert.IsTrue( Boolean(f.__dict__.__contains__(_str)) ); // check with a string var
  Assert.IsTrue( Boolean( BuiltinModule.hasattr(f, _str) ) );
  val := f.Value;
  f.Add(f); // passing itself as an argument
  Assert.IsTrue( f.Value = val*2 );
  // check param order
  f.SetABC(1, 2, 3);
  Assert.IsTrue(f.A = 1);
  Assert.IsTrue(f.B = 2);
  Assert.IsTrue(f.C = 3);
  // add a property to an instance
  f.Z := 99;
  Assert.IsTrue(f.Z = 99);
  // add a var to a module
  _main.Z := 99;
  Assert.IsTrue(_main.Z = 99);
  // check none
  Assert.IsTrue( VarIsNone(None) );
  Assert.IsTrue( VarIsNone(VarPythonCreate([1, Null, 3]).GetItem(1)) ); // Null is casted to None
  Assert.IsTrue( VarIsNone(VarPythonCreate([1, None, 3]).GetItem(1)) );
  Assert.IsTrue( VarIsNone(f.Inc()) );
  Assert.IsTrue( f.Inc() = None );
  Assert.IsTrue( not Boolean(None) ); // if not None:
  Assert.IsTrue( not VarIsTrue(None) ); // if not None:
  Assert.IsTrue( Boolean(f) ); // if f:
  Assert.IsTrue( VarIsTrue(f) ); // if f:

  // call a function
  Assert.IsTrue( _main.Add(2, 2) = 4 );
  // call a function with a mix of regular parameters and named parameters
  f := _main.MakeList(1, 2, 3, 4);
  Assert.IsTrue(VarIsPythonList(f));
  Assert.IsTrue(f.Length = 4);
  Assert.IsTrue(f.GetItem(0) = 1);
  Assert.IsTrue(f.GetItem(1) = 2);
  Assert.IsTrue(f.GetItem(2) = 3);
  Assert.IsTrue(f.GetItem(3) = 4);
  f := _main.MakeList(1, d:=3, c:=4, b:=2);
  Assert.IsTrue(VarIsPythonList(f));
  Assert.IsTrue(f.Length = 4);
  Assert.IsTrue(f.GetItem(0) = 1);
  Assert.IsTrue(f.GetItem(1) = 2);
  Assert.IsTrue(f.GetItem(2) = 4);
  Assert.IsTrue(f.GetItem(3) = 3);
  f := _main.MakeList(1, 2, d:= 3, c:=4);
  Assert.IsTrue(VarIsPythonList(f));
  Assert.IsTrue(f.Length = 4);
  Assert.IsTrue(f.GetItem(0) = 1);
  Assert.IsTrue(f.GetItem(1) = 2);
  Assert.IsTrue(f.GetItem(2) = 4);
  Assert.IsTrue(f.GetItem(3) = 3);
  f := _main.MakeList(1, 2, 3, d:=4);
  Assert.IsTrue(VarIsPythonList(f));
  Assert.IsTrue(f.Length = 4);
  Assert.IsTrue(f.GetItem(0) = 1);
  Assert.IsTrue(f.GetItem(1) = 2);
  Assert.IsTrue(f.GetItem(2) = 3);
  Assert.IsTrue(f.GetItem(3) = 4);
  f := _main.MakeList(b:=1, a:=2, d:= 3, c:=4);
  Assert.IsTrue(VarIsPythonList(f));
  Assert.IsTrue(f.Length = 4);
  Assert.IsTrue(f.GetItem(0) = 2);
  Assert.IsTrue(f.GetItem(1) = 1);
  Assert.IsTrue(f.GetItem(2) = 4);
  Assert.IsTrue(f.GetItem(3) = 3);
end;
procedure TVarPythTest.TestSequences;
var
  a, b, c : Variant;
  iter : Variant;
  cpt : Integer;
begin
  // initialize the operands
  // you can either use the overloaded function with an array of const
  // or use the VarArrayOf function that returns an array of variants that will
  // be casted to a Python list.
  a := VarPythonCreate([1, 2, 3]);
  Assert.IsTrue(VarIsPython(a));
  Assert.IsTrue(VarIsPythonSequence(a));
  Assert.IsTrue(VarIsPythonList(a));
  Assert.IsTrue(a.Length = 3); // this is a special property that does the same as: len(a) in Python
  Assert.IsTrue(a.Length() = 3); // this is a special method that does the same as the special property
  Assert.IsTrue(len(a) = 3);
  Assert.IsTrue(a.GetItem(0) = 1); // this is a special method that lets you do the same as: a[0] in Python
  Assert.IsTrue(a.GetItem(1) = 2);
  Assert.IsTrue(a.GetItem(2) = 3);
  Assert.IsTrue(string(a) = '[1, 2, 3]');
  // indexed access using brackets when the sequence is a property of an object (module, instance...)
  MainModule.a := VarPythonCreate([1, 2, 3]);
  Assert.IsTrue(MainModule.a[1] = 2);

  b := VarPythonCreate(VarArrayOf([4, 5, 6]));
  Assert.IsTrue(VarIsPython(b));
  Assert.IsTrue(VarIsPythonSequence(b));
  Assert.IsTrue(VarIsPythonList(b));
  Assert.IsTrue(b.Length = 3);
  Assert.IsTrue(b.Length() = 3);
  Assert.IsTrue(len(b) = 3);
  Assert.IsTrue(b.GetItem(0) = 4);
  Assert.IsTrue(b.GetItem(1) = 5);
  Assert.IsTrue(b.GetItem(2) = 6);
  Assert.IsTrue(string(b) = '[4, 5, 6]');
  // concatenation
  c := a + b;
  // check result of operation
  Assert.IsTrue(string(c) = '[1, 2, 3, 4, 5, 6]');
  // check that operation did not change the content of operands.
  Assert.IsTrue(string(a) = '[1, 2, 3]');
  Assert.IsTrue(string(b) = '[4, 5, 6]');
  // now with a litteral: note that with D6 SP1, we can't concatenate a custom variant with an var array of variants
  c := a + b + VarPythonCreate(['Hello', 'World!', 3.14]);
  Assert.IsTrue( string(c) = '[1, 2, 3, 4, 5, 6, ''Hello'', ''World!'', 3.14]' );
  c := a + VarPythonCreate(['Hello', 'World!', 3.14]) + b;
  Assert.IsTrue( string(c) = '[1, 2, 3, ''Hello'', ''World!'', 3.14, 4, 5, 6]' );
  c := VarPythonCreate(['Hello', 'World!', 3.14]) + a + b;
  Assert.IsTrue( string(c) = '[''Hello'', ''World!'', 3.14, 1, 2, 3, 4, 5, 6]' );

  // multiplication
  c := a * 3; // in Python the multiplication of sequence concatenates n times the sequence
  Assert.IsTrue( string(c) = '[1, 2, 3, 1, 2, 3, 1, 2, 3]' );

  // comparisons
  //------------

  // equal
  c := a = b;
  Assert.IsTrue(c = False);
  c := a = a;
  Assert.IsTrue(c = True);
  Assert.IsTrue( string(a) = '[1, 2, 3]');

  // not equal
  c := a <> b;
  Assert.IsTrue(c = True);
  Assert.IsTrue( not (c = b) );
  c := a <> a;
  Assert.IsTrue(c = False);
  Assert.IsTrue( string(a) = '[1, 2, 3]');

  // greater than
  c := a > b; Assert.IsTrue(c = False);
  c := b > a; Assert.IsTrue(c = True);
  Assert.IsTrue( string(a) > '[1, 1, 1]');

  // greater or equal than
  c := a >= b; Assert.IsTrue(c = False);
  c := b >= a; Assert.IsTrue(c = True);
  c := a >= a; Assert.IsTrue(c = True);
  Assert.IsTrue( string(a) >= '[1, 2, 3]' );

  // less than
  c := a < b; Assert.IsTrue(c = True);
  c := b < a; Assert.IsTrue(c = False);
  Assert.IsTrue( string(a) < '[4, 4, 4]');

  // less or equal than
  c := a <= b; Assert.IsTrue(c = True);
  c := b <= a; Assert.IsTrue(c = False);
  c := a <= a; Assert.IsTrue(c = True);
  Assert.IsTrue( string(a) <= '[1, 2, 3]');

  // copy
  c := a;
  Assert.IsTrue( c = a);
  Assert.IsTrue( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // sequence methods:
  c := b + a;
  c.sort(); // note that you must you the parenthesis to distinguish the call between a method or a property.
  Assert.IsTrue( c = (a+b) );

  c := NewPythonList; // facility for building sequences
  Assert.IsTrue( not VarIsTrue(c) ); // c is false because it's an empty collection
  c.append(1);
  c.append(2);
  c.append(3);
  Assert.IsTrue( VarIsTrue(c) ); // c is true because it's not an empty collection
  Assert.IsTrue(c = a);
  Assert.IsTrue( c.pop() = 3 );
  Assert.IsTrue( string(c) = '[1, 2]');

  c := NewPythonList(3); // facility for building sequences
  c.SetItem(0, 1);
  c.SetItem(1, 2);
  c.SetItem(2, 3);
  Assert.IsTrue(c = a);
  c.DeleteItem(1);
  Assert.IsTrue(c = VarPythonCreate([1,3]));

  Assert.IsTrue(VarPythonCreate([1,2,3,4]).GetSlice(1, 3) = VarPythonCreate([2,3])); // same as x = [1,2,3,4]; x[1:3]
  Assert.IsTrue(VarPythonCreate([1,2,3,4]).GetSlice(1, Ellipsis) = VarPythonCreate([2,3,4])); // same as x = [1,2,3,4]; x[1:]
  Assert.IsTrue(VarPythonCreate([1,2,3,4]).GetSlice(1, -1) = VarPythonCreate([2,3])); // same as x = [1,2,3,4]; x[1:-1]
  c := VarPythonCreate([1,2,3,4]);
  c.SetSlice(1, 3, VarPythonCreate([7, 8, 9]));
  Assert.IsTrue( c = VarPythonCreate([1, 7, 8, 9, 4]) );
  Assert.IsTrue( Boolean(c.Contains( 7 )) ); // same as 7 in c
  Assert.IsTrue( not Boolean(c.Contains( 77 )) );
  c.DelSlice(1,3);
  Assert.IsTrue( c = VarPythonCreate([1,9,4]) );

  c := VarPythonCreate([1, 2, 3, 4], stTuple); // test a tuple
  Assert.IsTrue( VarIsPythonTuple(c) );
  Assert.IsTrue( VarIsPythonSequence(c) );
  Assert.IsTrue( c.GetItem(1) = 2 );
  Assert.IsTrue( c.Length = 4 );
  c := NewPythonTuple(3);
  c.SetItem(0, 1);
  c.SetItem(1, 2);
  c.SetItem(2, 3);
  Assert.IsTrue( VarIsPythonTuple(c) );
  Assert.IsTrue( VarIsPythonSequence(c) );
  Assert.IsTrue( c.GetItem(1) = 2 );
  Assert.IsTrue( c.Length = 3 );

  // test iterator
  iter := BuiltinModule.iter(VarPythonCreate([1, 2, 3, 4], stTuple));
  Assert.IsTrue(VarIsPythonIterator(iter));
  Assert.IsTrue(iter.__next__() = 1);
  Assert.IsTrue(iter.__next__() = 2);
  Assert.IsTrue(iter.__next__() = 3);
  Assert.IsTrue(iter.__next__() = 4);
  try
    iter.__next__();
  except
    on E: EPyStopIteration do
    begin
      Assert.IsTrue(True); //Ok.
    end
    else
      Assert.IsTrue(False, 'expected stop exception');
  end;
  cpt := 0;
  iter := VarPyth.iter(VarPythonCreate([1, 2, 3, 4], stTuple));
  Assert.IsTrue(VarIsPythonIterator(iter));
  try
    while True do
    begin
      a := iter.__next__();
      Inc(cpt);
      Assert.IsTrue(a = cpt);
    end;
  except
    on E: EPyStopIteration do
    begin
      Assert.IsTrue(True); //Ok.
    end
    else
      Assert.IsTrue(False, 'expected stop exception');
  end;
  Assert.IsTrue(cpt = 4);
end;
procedure TVarPythTest.TestStrings;
var
  a, b, c : Variant;
  w : WideString;
  _obj : PPyObject;
begin
  // initialize the operands
  a := VarPythonCreate('abc');
  Assert.IsTrue(VarIsPython(a));
  Assert.IsTrue(VarIsPythonString(a));
  Assert.IsTrue(string(a) = 'abc');
  b := VarPythonCreate('def');
  Assert.IsTrue(VarIsPython(b));
  Assert.IsTrue(VarIsPythonString(b));
  Assert.IsTrue(string(b) = 'def');
  // concatenation
  c := a + b;
  // check result of operation
  Assert.IsTrue( string(c) = 'abcdef' );
  // check that operation did not change the content of operands.
  Assert.IsTrue(string(a) = 'abc');
  Assert.IsTrue(string(b) = 'def');
  // now with a litteral
  c := a + b + '!';
  Assert.IsTrue( string(c) = 'abcdef!' );
  c := a + '!' + b;
  Assert.IsTrue( string(c) = 'abc!def' );
  c := '!' + a + b;
  Assert.IsTrue( string(c) = '!abcdef' );
  // multiplication
  c := a * 3; // in Python the multiplication of string concatenates n times the string
  Assert.IsTrue( string(c) = 'abcabcabc' );
  // comparisons
  //------------
  // equal
  c := a = b;
  Assert.IsTrue(c = False);
  c := a = a;
  Assert.IsTrue(c = True);
  Assert.IsTrue( a = 'abc');
  // not equal
  c := a <> b;
  Assert.IsTrue(c = True);
  Assert.IsTrue( not (c = b) );
  c := a <> a;
  Assert.IsTrue(c = False);
  Assert.IsTrue( a = 'abc');
  // greater than
  c := a > b; Assert.IsTrue(c = False);
  c := b > a; Assert.IsTrue(c = True);
  Assert.IsTrue( a > 'aaa');
  // greater or equal than
  c := a >= b; Assert.IsTrue(c = False);
  c := b >= a; Assert.IsTrue(c = True);
  c := a >= a; Assert.IsTrue(c = True);
  Assert.IsTrue( a >= 'abc' );
  // less than
  c := a < b; Assert.IsTrue(c = True);
  c := b < a; Assert.IsTrue(c = False);
  Assert.IsTrue( a < 'bbb');
  // less or equal than
  c := a <= b; Assert.IsTrue(c = True);
  c := b <= a; Assert.IsTrue(c = False);
  c := a <= a; Assert.IsTrue(c = True);
  Assert.IsTrue( a <= 'abc');
  // copy
  c := a;
  Assert.IsTrue( c = a);
  Assert.IsTrue( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.
  // empty strings
  a := VarPythonCreate('');
  Assert.IsTrue(a.length = 0);
  Assert.IsTrue(a = '');
  Assert.IsTrue(string(a) = '');
  // Unicode strings
  b := VarPythonEval( 'u"Hello world!"' );
  Assert.IsTrue( VarIsPythonUnicode(b) );
  w := FPythonEngine.PyUnicodeAsString(ExtractPythonObjectFrom(b));
  Assert.IsTrue( w = 'Hello world!');
  Assert.IsTrue( b = 'Hello world!');
  Assert.IsTrue( b <> a );
  _obj := FPythonEngine.PyUnicodeFromString(w);
  try
    c := VarPythonCreate( _obj  );
  finally
    FPythonEngine.Py_XDecRef(_obj);
  end;
  Assert.IsTrue(b = c);
  Assert.IsTrue(c = w);
  Assert.IsTrue( c = 'Hello world!');
  w := b;
  Assert.IsTrue( b = w);
  Assert.IsTrue( w = 'Hello world!');
  Assert.IsTrue( Length(w) = 12 );
  Assert.IsTrue( Length(w) = b.Length() );
  c := FPythonEngine.PyObjectAsVariant(ExtractPythonObjectFrom(b));
  Assert.IsTrue( c = b );
  Assert.IsTrue( c = w );
  Assert.IsTrue( c = 'Hello world!');
  Assert.IsTrue( VarType(c) and VarTypeMask = varUString );
  c := VarPythonCreate(w);
  Assert.IsTrue( c = 'Hello world!');
  Assert.IsTrue( c = w );
  c := VarPythonCreate([w]);
  Assert.IsTrue( VarIsPythonUnicode(c.GetItem(0)) );
  Assert.IsTrue( c.GetItem(0) = 'Hello world!');
  Assert.IsTrue( c.GetItem(0) = w );
  c := w;
  b := VarPythonCreate(c);
  Assert.IsTrue( VarIsPythonUnicode(b) );
  Assert.IsTrue( b = c );
  Assert.IsTrue( b = w );
  // empty strings
  a := VarPythonEval( 'u""' );
  Assert.IsTrue(a.length = 0);
  Assert.IsTrue(a = '');
  Assert.IsTrue(string(a) = '');
  Assert.IsTrue(WideString(a) = '');
end;

initialization
  TDUnitX.RegisterTestFixture(TVarPythTest);

end.
