unit fmMain;

{$I Definition.Inc}

interface

uses
  Classes, SysUtils, Variants,
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  PythonEngine, PythonGUIInputOutput, Contnrs;

type
  TMain = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Memo2: TMemo;
    btnExecuteScript: TButton;
    btnTestIntegers: TButton;
    btnTestFloats: TButton;
    btnTestSequences: TButton;
    btnTestMappings: TButton;
    btnTestObjects: TButton;
    btnTestStrings: TButton;
    cbIntegers: TCheckBox;
    cbFloats: TCheckBox;
    cbStrings: TCheckBox;
    cbSequences: TCheckBox;
    cbMappings: TCheckBox;
    cbObjects: TCheckBox;
    btnRunSelectedTests: TButton;
    edtTestCount: TEdit;
    btnRunNTimes: TButton;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    btnTestDates: TButton;
    cbDates: TCheckBox;
    procedure btnExecuteScriptClick(Sender: TObject);
    procedure btnTestIntegersClick(Sender: TObject);
    procedure btnTestFloatsClick(Sender: TObject);
    procedure btnTestStringsClick(Sender: TObject);
    procedure btnTestSequencesClick(Sender: TObject);
    procedure btnTestMappingsClick(Sender: TObject);
    procedure btnTestObjectsClick(Sender: TObject);
    procedure btnRunSelectedTestsClick(Sender: TObject);
    procedure btnRunNTimesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnTestDatesClick(Sender: TObject);
  private
    fVerbose : Boolean;
    procedure Log( const AText : String );
    procedure RunSelectedTests;
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses VarPyth;

{$R *.dfm}

procedure TMain.Log( const AText : String );
begin
  if fVerbose then
    Memo1.Lines.Add(AText);
end;

procedure TMain.btnExecuteScriptClick(Sender: TObject);
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
end;

procedure TMain.btnTestIntegersClick(Sender: TObject);
var
  a, b, c : Variant;
  big : Int64;
begin
  // initialize the operands
  a := VarPythonCreate(2);
  Assert(VarIsPython(a));
  Assert(VarIsPythonNumber(a));
  Assert(VarIsPythonInteger(a));
  Assert(Integer(a) = 2);

  b := VarPythonCreate(3);
  Assert(VarIsPython(b));
  Assert(VarIsPythonNumber(b));
  Assert(VarIsPythonInteger(b));
  Assert(Integer(b) = 3);

  // arithmetic operations
  //----------------------

  // addition
  c := a + b;
  // check result of operation
  Assert( Integer(c) = 5 );
  // check that operation did not change the content of operands.
  Assert(Integer(a) = 2);
  Assert(Integer(b) = 3);
  // now with a litteral
  c := a + b + 1;
  Assert( Integer(c) = 6 );
  c := a + 1 + b;
  Assert( Integer(c) = 6 );
  c := 1 + a + b;
  Assert( Integer(c) = 6 );

  // substraction
  c := b - a;
  Assert( Integer(c) = 1 );
  // now with a litteral
  c := b - a - 1;
  Assert( Integer(c) = 0 );
  c := b - 1 - a;
  Assert( Integer(c) = 0 );
  c := 1 - b - a;
  Assert( Integer(c) = -4 );

  // multiplication
  c := a * b;
  Assert( Integer(c) = 6 );
  // now with a litteral
  c := a * b * 2;
  Assert( Integer(c) = 12 );
  c := a * 2 * b;
  Assert( Integer(c) = 12 );
  c := 2 * a * b;
  Assert( Integer(c) = 12 );

  // integer division
  c := b div a;
  Assert( Integer(c) = 1 );

  // division: in Python a division between 2 integers is the same as the integer division
  c := b / a;
  Assert( c = 1.5 );
  Assert( Integer(c) = 2 );

  // modulus
  c := b mod a;
  Assert( Integer(c) = 1 );
  c := BuiltinModule.divmod(b, a); // this returns a tuple whose first item is the result of the division,
                                   // and second item the modulo.
  if VarIsPythonSequence(c) and (c.Length = 2) then
  begin
    Assert(Integer(c.GetItem(0)) = 1); // division
    Assert(Integer(c.GetItem(1)) = 1); // modulo
  end;

  // power
  c := BuiltinModule.pow(a, b);
  Assert(c = 8);

  // negation
  c := -a;
  Assert( Integer(c) = -2 );

  // logical operations
  //------------------

  // inverse
  c := not a; // in python it would be: c = ~2
  Assert( Integer(c) = -3 );

  // shift left (<<)
  c := a shl b;
  Assert( Integer(c) = 16 );
  c := a shl 1;
  Assert( Integer(c) = 4 );

  // shift right (>>)
  c := a shl b;
  c := c shr b;
  Assert( Integer(c) = Integer(a) );
  c := b shr 1;
  Assert( Integer(c) = 1 );

  // and
  c := a and (a*5);
  Assert( Integer(c) = Integer(a) );
  c := a and 6;
  Assert( Integer(c) = Integer(a) );

  // or
  c := a or b;
  Assert( Integer(c) = 3 );
  c := a or 3;
  Assert( Integer(c) = 3 );

  // xor
  c := a xor b;
  Assert( Integer(c) = 1 );
  c := a xor 3;
  Assert( Integer(c) = 1 );

  // comparisons
  //------------

  // equal
  c := a = b;
  Assert(c = False);
  c := a = a;
  Assert(c = True);
  Assert( a = 2);

  // not equal
  c := a <> b;
  Assert(c = True);
  Assert( not (c = b) );
  c := a <> a;
  Assert(c = False);
  Assert( a = 2);

  // greater than
  c := a > b; Assert(c = False);
  c := b > a; Assert(c = True);
  Assert( a > 1);

  // greater or equal than
  c := a >= b; Assert(c = False);
  c := b >= a; Assert(c = True);
  c := a >= a; Assert(c = True);
  Assert( a >= 2 );

  // less than
  c := a < b; Assert(c = True);
  c := b < a; Assert(c = False);
  Assert( a < 6);

  // less or equal than
  c := a <= b; Assert(c = True);
  c := b <= a; Assert(c = False);
  c := a <= a; Assert(c = True);
  Assert( a <= 2);

  // parenthesis
  c := a * ((a * b) div b);
  Assert( c = a*2 );

  // copy
  c := a;
  Assert( c = a);
  Assert( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // test long long (Int64)
  big := Int64(MaxInt)*4;
  b := VarPythonCreate(big);
  Assert( b = big );
  Assert( b <> big+1 );
  Assert( b > MaxInt );
  Assert( MaxInt < b );
  Assert( b+1 = big+1 );
  Assert( b*2 = big*2 );
  Assert( b div 2 = big div 2 );
  c := VarPythonCreate(True);
  Assert(VarIsBool(c));
  Assert(VarIsTrue(c));
  c := VarPythonCreate(False);
  Assert(VarIsBool(c));
  Assert(not VarIsTrue(c));

  // Done!
  Log('Integer test was Ok.');
end;

procedure TMain.btnTestFloatsClick(Sender: TObject);
var
  a, b, c : Variant;
  dbl_a, dbl_b, dbl_c : Double;
  int : Integer;
begin
  // initialize the operands
  dbl_a := 2.5;
  a := VarPythonCreate(dbl_a);
  Assert(VarIsPython(a));
  Assert(VarIsPythonNumber(a));
  Assert(VarIsPythonFloat(a));
  Assert(Double(a) = 2.5);

  dbl_b := 3.2;
  b := VarPythonCreate(dbl_b);
  Assert(VarIsPython(b));
  Assert(VarIsPythonNumber(b));
  Assert(VarIsPythonFloat(b));
  Assert(Double(b) = dbl_b); // note that Assert(Double(b) = 3.2) fails.

  // arithmetic operations
  //----------------------

  // addition
  c := a + b;
  // check result of operation
  Assert( Double(c) = (dbl_a + dbl_b) );
  // check that operation did not change the content of operands.
  Assert(Double(a) = dbl_a);
  Assert(Double(b) = dbl_b);
  // now with a litteral
  c := a + b + 1;
  Assert( Double(c) = (dbl_a+dbl_b+1) );
  c := a + 1 + b;
  Assert( Double(c) = (dbl_a+1+dbl_b) );
  c := 1 + a + b;
  Assert( Double(c) = (1+dbl_a+dbl_b) );

  // substraction
  c := b - a;
  Assert( Double(c) = (dbl_b - dbl_a) );
  // now with a litteral
  c := b - a - 1;
  Assert( Double(c) = (dbl_b-dbl_a-1) );
  c := b - 1 - a;
  Assert( Double(c) = (dbl_b-1-dbl_a) );
  c := 1 - b - a;
  Assert( Double(c) = (1-dbl_b-dbl_a) );

  // multiplication
  c := a * b;
  dbl_c := dbl_a * dbl_b;
  Assert( Double(c) = dbl_c );
  // now with a litteral
  c := a * b * 2;
  dbl_c := dbl_a * dbl_b * 2;
  Assert( Double(c) = dbl_c );
  c := a * 2 * b;
  dbl_c := dbl_a * 2 * dbl_b;
  Assert( Double(c) = dbl_c );
  c := 2 * a * b;
  dbl_c := 2 * dbl_a * dbl_b;
  Assert( Double(c) = dbl_c );

  // division: in Python a division between 2 integers is the same as the integer division
  c := b / a;
  dbl_c := dbl_b / dbl_a;
  Assert( Double(c) = dbl_c );

  // negation
  c := -a;
  Assert( Double(c) = -dbl_a );

  // comparisons
  //------------

  // equal
  c := a = b;
  Assert(c = False);
  c := a = a;
  Assert(c = True);
  Assert( a = dbl_a);

  // not equal
  c := a <> b;
  Assert(c = True);
  Assert( not (c = b) );
  c := a <> a;
  Assert(c = False);
  Assert( a = dbl_a);

  // greater than
  c := a > b; Assert(c = False);
  c := b > a; Assert(c = True);
  Assert( a > (dbl_a-1));

  // greater or equal than
  c := a >= b; Assert(c = False);
  c := b >= a; Assert(c = True);
  c := a >= a; Assert(c = True);
  Assert( a >= dbl_a );

  // less than
  c := a < b; Assert(c = True);
  c := b < a; Assert(c = False);
  Assert( a < dbl_b);

  // less or equal than
  c := a <= b; Assert(c = True);
  c := b <= a; Assert(c = False);
  c := a <= a; Assert(c = True);
  Assert( a <= dbl_a);

  // parenthesis
  c := a * ((a * b) / b);
  dbl_c := dbl_a * ((dbl_a * dbl_b) / dbl_b);
  Assert( c = dbl_c );

  // copy
  c := a;
  Assert( c = a);
  Assert( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // casts
  int := a;
  Assert(int = 2);

  // Done!
  Log('Float test was Ok.');
end;

procedure TMain.btnTestStringsClick(Sender: TObject);
var
  a, b, c : Variant;
  w : WideString;
  _obj : PPyObject;
begin
  // initialize the operands
  a := VarPythonCreate('abc');
  Assert(VarIsPython(a));
  Assert(VarIsPythonString(a));
  Assert(String(a) = 'abc');

  b := VarPythonCreate('def');
  Assert(VarIsPython(b));
  Assert(VarIsPythonString(b));
  Assert(String(b) = 'def');

  // concatenation
  c := a + b;
  // check result of operation
  Assert( String(c) = 'abcdef' );
  // check that operation did not change the content of operands.
  Assert(String(a) = 'abc');
  Assert(String(b) = 'def');
  // now with a litteral
  c := a + b + '!';
  Assert( String(c) = 'abcdef!' );
  c := a + '!' + b;
  Assert( String(c) = 'abc!def' );
  c := '!' + a + b;
  Assert( String(c) = '!abcdef' );

  // multiplication
  c := a * 3; // in Python the multiplication of string concatenates n times the string
  Assert( String(c) = 'abcabcabc' );

  // comparisons
  //------------

  // equal
  c := a = b;
  Assert(c = False);
  c := a = a;
  Assert(c = True);
  Assert( a = 'abc');

  // not equal
  c := a <> b;
  Assert(c = True);
  Assert( not (c = b) );
  c := a <> a;
  Assert(c = False);
  Assert( a = 'abc');

  // greater than
  c := a > b; Assert(c = False);
  c := b > a; Assert(c = True);
  Assert( a > 'aaa');

  // greater or equal than
  c := a >= b; Assert(c = False);
  c := b >= a; Assert(c = True);
  c := a >= a; Assert(c = True);
  Assert( a >= 'abc' );

  // less than
  c := a < b; Assert(c = True);
  c := b < a; Assert(c = False);
  Assert( a < 'bbb');

  // less or equal than
  c := a <= b; Assert(c = True);
  c := b <= a; Assert(c = False);
  c := a <= a; Assert(c = True);
  Assert( a <= 'abc');

  // copy
  c := a;
  Assert( c = a);
  Assert( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // empty strings
  a := VarPythonCreate('');
  Assert(a.length = 0);
  Assert(a = '');
  Assert(String(a) = '');

  // Unicode strings
  b := VarPythonEval( 'u"Hello world!"' );
  Assert( VarIsPythonUnicode(b) );
  w := PythonEngine1.PyUnicode_AsWideString(ExtractPythonObjectFrom(b));
  Assert( w = 'Hello world!');
  Assert( b = 'Hello world!');
  Assert( b <> a );
  _obj := PythonEngine1.PyUnicode_FromWideString(w);
  try
    c := VarPythonCreate( _obj  );
  finally
    PythonEngine1.Py_XDecRef(_obj);
  end;
  Assert(b = c);
  Assert(c = w);
  Assert( c = 'Hello world!');
  w := b;
  Assert( b = w);
  Assert( w = 'Hello world!');
  Assert( Length(w) = 12 );
  Assert( Length(w) = b.Length() );
  c := PythonEngine1.PyObjectAsVariant(ExtractPythonObjectFrom(b));
  Assert( c = b );
  Assert( c = w );
  Assert( c = 'Hello world!');
  {$IFDEF UNICODE}
  Assert( VarType(c) and VarTypeMask = varUString );
  {$ELSE}
  Assert( VarType(c) and VarTypeMask = varOleStr );
  {$ENDIF}
  c := VarPythonCreate(w);
  Assert( c = 'Hello world!');
  Assert( c = w );
  c := VarPythonCreate([w]);
  Assert( VarIsPythonUnicode(c.GetItem(0)) );
  Assert( c.GetItem(0) = 'Hello world!');
  Assert( c.GetItem(0) = w );
  {$IFDEF PREFER_UNICODE}
  c := w;
  b := VarPythonCreate(c);
  Assert( VarIsPythonUnicode(b) );
  Assert( b = c );
  Assert( b = w );
  {$ENDIF}
  // empty strings
  a := VarPythonEval( 'u""' );
  Assert(a.length = 0);
  Assert(a = '');
  Assert(String(a) = '');
  Assert(WideString(a) = '');

  // Done!
  Log('String test was Ok.');
end;

procedure TMain.btnTestSequencesClick(Sender: TObject);
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
  Assert(VarIsPython(a));
  Assert(VarIsPythonSequence(a));
  Assert(VarIsPythonList(a));
  Assert(a.Length = 3); // this is a special property that does the same as: len(a) in Python
  Assert(a.Length() = 3); // this is a special method that does the same as the special property
  Assert(len(a) = 3);
  Assert(a.GetItem(0) = 1); // this is a special method that lets you do the same as: a[0] in Python
  Assert(a.GetItem(1) = 2);
  Assert(a.GetItem(2) = 3);
  Assert(String(a) = '[1, 2, 3]');
  // indexed access using brackets when the sequence is a property of an object (module, instance...)
  MainModule.a := VarPythonCreate([1, 2, 3]);
  Assert(MainModule.a[1] = 2);

  b := VarPythonCreate(VarArrayOf([4, 5, 6]));
  Assert(VarIsPython(b));
  Assert(VarIsPythonSequence(b));
  Assert(VarIsPythonList(b));
  Assert(b.Length = 3);
  Assert(b.Length() = 3);
  Assert(len(b) = 3);
  Assert(b.GetItem(0) = 4);
  Assert(b.GetItem(1) = 5);
  Assert(b.GetItem(2) = 6);
  Assert(String(b) = '[4, 5, 6]');

  // concatenation
  c := a + b;
  // check result of operation
  Assert(String(c) = '[1, 2, 3, 4, 5, 6]');
  // check that operation did not change the content of operands.
  Assert(String(a) = '[1, 2, 3]');
  Assert(String(b) = '[4, 5, 6]');
  // now with a litteral: note that with D6 SP1, we can't concatenate a custom variant with an var array of variants
  c := a + b + VarPythonCreate(['Hello', 'World!', 3.14]);
  {$IFDEF UNICODE}
  Assert( String(c) = '[1, 2, 3, 4, 5, 6, u''Hello'', u''World!'', 3.1400000000000001]' );
  {$ELSE}
  Assert( String(c) = '[1, 2, 3, 4, 5, 6, ''Hello'', ''World!'', 3.1400000000000001]' );
  {$ENDIF}
  c := a + VarPythonCreate(['Hello', 'World!', 3.14]) + b;
  {$IFDEF UNICODE}
  Assert( String(c) = '[1, 2, 3, u''Hello'', u''World!'', 3.1400000000000001, 4, 5, 6]' );
  {$ELSE}
  Assert( String(c) = '[1, 2, 3, ''Hello'', ''World!'', 3.1400000000000001, 4, 5, 6]' );
  {$ENDIF}
  c := VarPythonCreate(['Hello', 'World!', 3.14]) + a + b;
  {$IFDEF UNICODE}
  Assert( String(c) = '[u''Hello'', u''World!'', 3.1400000000000001, 1, 2, 3, 4, 5, 6]' );
  {$ELSE}
  Assert( String(c) = '[''Hello'', ''World!'', 3.1400000000000001, 1, 2, 3, 4, 5, 6]' );
  {$ENDIF}

  // multiplication
  c := a * 3; // in Python the multiplication of sequence concatenates n times the sequence
  Assert( String(c) = '[1, 2, 3, 1, 2, 3, 1, 2, 3]' );

  // comparisons
  //------------

  // equal
  c := a = b;
  Assert(c = False);
  c := a = a;
  Assert(c = True);
  Assert( String(a) = '[1, 2, 3]');

  // not equal
  c := a <> b;
  Assert(c = True);
  Assert( not (c = b) );
  c := a <> a;
  Assert(c = False);
  Assert( String(a) = '[1, 2, 3]');

  // greater than
  c := a > b; Assert(c = False);
  c := b > a; Assert(c = True);
  Assert( String(a) > '[1, 1, 1]');

  // greater or equal than
  c := a >= b; Assert(c = False);
  c := b >= a; Assert(c = True);
  c := a >= a; Assert(c = True);
  Assert( String(a) >= '[1, 2, 3]' );

  // less than
  c := a < b; Assert(c = True);
  c := b < a; Assert(c = False);
  Assert( String(a) < '[4, 4, 4]');

  // less or equal than
  c := a <= b; Assert(c = True);
  c := b <= a; Assert(c = False);
  c := a <= a; Assert(c = True);
  Assert( String(a) <= '[1, 2, 3]');

  // copy
  c := a;
  Assert( c = a);
  Assert( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // sequence methods:
  c := b + a;
  c.sort(); // note that you must you the parenthesis to distinguish the call between a method or a property.
  Assert( c = (a+b) );

  c := NewPythonList; // facility for building sequences
  Assert( not VarIsTrue(c) ); // c is false because it's an empty collection
  c.append(1);
  c.append(2);
  c.append(3);
  Assert( VarIsTrue(c) ); // c is true because it's not an empty collection
  Assert(c = a);
  Assert( c.pop() = 3 );
  Assert( String(c) = '[1, 2]');

  c := NewPythonList(3); // facility for building sequences
  c.SetItem(0, 1);
  c.SetItem(1, 2);
  c.SetItem(2, 3);
  Assert(c = a);
  c.DeleteItem(1);
  Assert(c = VarPythonCreate([1,3]));

  Assert(VarPythonCreate([1,2,3,4]).GetSlice(1, 3) = VarPythonCreate([2,3])); // same as x = [1,2,3,4]; x[1:3]
  Assert(VarPythonCreate([1,2,3,4]).GetSlice(1, Ellipsis) = VarPythonCreate([2,3,4])); // same as x = [1,2,3,4]; x[1:]
  Assert(VarPythonCreate([1,2,3,4]).GetSlice(1, -1) = VarPythonCreate([2,3])); // same as x = [1,2,3,4]; x[1:-1]
  c := VarPythonCreate([1,2,3,4]);
  c.SetSlice(1, 3, VarPythonCreate([7, 8, 9]));
  Assert( c = VarPythonCreate([1, 7, 8, 9, 4]) );
  Assert( Boolean(c.Contains( 7 )) ); // same as 7 in c
  Assert( not Boolean(c.Contains( 77 )) );
  c.DelSlice(1,3);
  Assert( c = VarPythonCreate([1,9,4]) );

  c := VarPythonCreate([1, 2, 3, 4], stTuple); // test a tuple
  Assert( VarIsPythonTuple(c) );
  Assert( VarIsPythonSequence(c) );
  Assert( c.GetItem(1) = 2 );
  Assert( c.Length = 4 );
  c := NewPythonTuple(3);
  c.SetItem(0, 1);
  c.SetItem(1, 2);
  c.SetItem(2, 3);
  Assert( VarIsPythonTuple(c) );
  Assert( VarIsPythonSequence(c) );
  Assert( c.GetItem(1) = 2 );
  Assert( c.Length = 3 );
  Assert(not VarIsPythonIterator(c));

  // test iterator
  iter := BuiltinModule.iter(VarPythonCreate([1, 2, 3, 4], stTuple));
  Assert(VarIsPythonIterator(iter));
  Assert(iter.next() = 1);
  Assert(iter.next() = 2);
  Assert(iter.next() = 3);
  Assert(iter.next() = 4);
  try
    iter.next();
  except
    on E: EPyStopIteration do
    begin
      Assert(True); //Ok.
    end
    else
      Assert(False, 'expected stop exception');
  end;
  cpt := 0;
  iter := VarPyth.iter(VarPythonCreate([1, 2, 3, 4], stTuple));
  Assert(VarIsPythonIterator(iter));
  try
    while True do
    begin
      a := iter.next();
      Inc(cpt);
      Assert(a = cpt);
    end;
  except
    on E: EPyStopIteration do
    begin
      Assert(True); //Ok.
    end
    else
      Assert(False, 'expected stop exception');
  end;
  Assert(cpt = 4);
  // Done!
  Log('Sequence test was Ok.');
end;

procedure TMain.btnTestMappingsClick(Sender: TObject);
var
  a, b, c, keys, values : Variant;
begin
  // initialize the operands
  a := NewPythonDict;
  Assert(VarIsPython(a));
  Assert(VarIsPythonMapping(a));
  Assert(VarIsPythonDict(a));
  // There is a bug in D2010 in which Char('a') gets translated to integer parameter
  a.SetItem( string('a'), 1 );
  a.SetItem( string('b'), 2 );
  a.SetItem( string('c'), 3 );
  Assert(a.Length = 3); // this is a special property that does the same as: len(a) in Python
  Assert(a.Length() = 3); // this is a special method that does the same as the special property
  Assert(len(a) = 3);
  Assert(a.GetItem(string('a')) = 1); // this is a special method that lets you do the same as: a[0] in Python
  Assert(a.GetItem(string('b')) = 2);
  Assert(a.GetItem(string('c')) = 3);


  b := NewPythonDict;
  Assert(VarIsPython(b));
  Assert(VarIsPythonMapping(b));
  Assert(VarIsPythonDict(b));
  b.SetItem( string('d'), 4 );
  b.SetItem( string('e'), 5 );
  b.SetItem( string('f'), 6 );
  Assert(b.Length = 3);
  Assert(b.Length() = 3);
  Assert(len(b) = 3);
  Assert(b.GetItem(string('d')) = 4);
  Assert(b.GetItem(string('e')) = 5);
  Assert(b.GetItem(string('f')) = 6);

  // copy
  c := a;
  Assert( c = a);
  Assert( VarIsSame(c, a) ); // checks if 2 variants share the same Python object.

  // dict methods
  Assert( Boolean(a.has_key(string('a'))) );
  Assert( not Boolean(a.has_key('abc')) );
  keys := a.keys();
  keys.sort();
  Assert( keys = VarPythonCreate(VarArrayOf(['a', 'b', 'c'])));
  values := a.values();
  values.sort();
  Assert( values = VarPythonCreate(VarArrayOf([1, 2, 3])));
  c := a;
  c.DeleteItem(string('a'));
  Assert( not Boolean(c.has_key(string('a'))) );

  // test string values
  a := NewPythonDict;
  a.SetItem( string('a'), 'Hello');
  a.SetItem( string('b'), 'World!');
  a.SetItem( string('c'), '');
  Assert(a.GetItem(string('a')) = 'Hello');
  Assert(a.GetItem(string('b')) = 'World!');
  Assert(a.GetItem(string('c')) = '');

  // Done!
  Log('Mapping test was Ok.');
end;

procedure TMain.btnTestDatesClick(Sender: TObject);
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
  Assert( a.Length = 9 );
  Assert( a.GetItem(0) = _year );
  Assert( a.GetItem(1) = _month );
  Assert( a.GetItem(2) = _day );
  Assert( a.GetItem(3) = _hour );
  Assert( a.GetItem(4) = _min );
  Assert( a.GetItem(5) = _sec );
  Assert( b.Length = 9 );
  Assert( b.GetItem(0) = a.GetItem(0) );
  Assert( b.GetItem(1) = a.GetItem(1) );
  Assert( b.GetItem(2) = a.GetItem(2) );
  Assert( b.GetItem(3) = a.GetItem(3) );
  Assert( b.GetItem(4) = a.GetItem(4) );
  Assert( b.GetItem(5) = a.GetItem(5) );
  Assert( b.GetItem(6) = a.GetItem(6) );
  Assert( b.GetItem(7) = a.GetItem(7) );
  // don't test the 9th item of the tuple, because it's the daylight saving,
  // and it's not computed by the Python for Delphi.
  //Assert( b.GetItem(8) = a.GetItem(8) );

  _date2 := b;
  DecodeDate( _date2, _year2, _month2, _day2 );
  DecodeTime( _date2, _hour2, _min2, _sec2, _msec2 );
  Assert( _year2 = _year );
  Assert( _month2 = _month );
  Assert( _day2 = _day );
  Assert( _hour2 = _hour );
  Assert( _min2 = _min );
  Assert( _sec2 = _sec );

  // test new datetime module
  _timeMod := Import('datetime'); // get the datetime module of Python
  //or _timeMod := DatetimeModule; // get the datetime module of Python

  a := _timeMod.datetime(2002, 12, 30, 22, 15, 38, 827738);
  Assert(VarIsPythonDateTime(a));
  Assert(VarIsPythonDate(a));
  Assert(not VarIsPythonTime(a));
  Assert(not VarIsPythonDateTimeDelta(a));
  Assert(a.year  = 2002);
  Assert(a.month = 12);
  Assert(a.day   = 30);
  Assert(a.hour  = 22);
  Assert(a.minute   = 15);
  Assert(a.second   = 38);
  Assert(a.microsecond  = 827738);

  _date := a;
  DecodeDate( _date, _year, _month, _day );
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert(_year  = 2002);
  Assert(_month = 12);
  Assert(_day   = 30);
  Assert(_hour  = 22);
  Assert(_min   = 15);
  Assert(_sec   = 38);
  Assert(_msec  = 827738 div 1000);

  a := _timeMod.date(2002, 12, 30);
  Assert(not VarIsPythonDateTime(a));
  Assert(VarIsPythonDate(a));
  Assert(not VarIsPythonTime(a));
  Assert(not VarIsPythonDateTimeDelta(a));
  _date := a;
  DecodeDate( _date, _year, _month, _day );
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert(_year  = 2002);
  Assert(_month = 12);
  Assert(_day   = 30);
  Assert(_hour  = 0);
  Assert(_min   = 0);
  Assert(_sec   = 0);
  Assert(_msec  = 0);
  Assert(a.year  = 2002);
  Assert(a.month = 12);
  Assert(a.day   = 30);

  a := _timeMod.time(22, 15, 38, 827738);
  Assert(not VarIsPythonDateTime(a));
  Assert(not VarIsPythonDate(a));
  Assert(VarIsPythonTime(a));
  Assert(not VarIsPythonDateTimeDelta(a));
  Assert(a.hour  = 22);
  Assert(a.minute   = 15);
  Assert(a.second   = 38);
  Assert(a.microsecond  = 827738);
  _date := a;
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert(_hour  = 22);
  Assert(_min   = 15);
  Assert(_sec   = 38);
  Assert(_msec  = 827738 div 1000);

  a := DatetimeModule.datetime(2002, 12, 30, 22, 15, 38, 827738);
  b := _timeMod.datetime(2002, 12, 30, 22, 16, 38, 827738);
  c := b - a;
  Assert(VarIsPythonDateTimeDelta(c));
  Assert(c.days = 0);
  Assert(c.seconds = 60);
  Assert(c.microseconds = 0);
  _date := c;
  Assert(Trunc(_date)=0);
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert(_hour = 0);
  Assert(_min = 1);
  Assert(_sec = 0);
  Assert(_msec = 0);

  c := a - b;
  Assert(VarIsPythonDateTimeDelta(c));
  Assert(c.days = -1);
  Assert(c.seconds = 86340);
  Assert(c.microseconds = 0);
  _date := c;
  Assert(Trunc(_date)=0);
  Assert(_date<0);
  DecodeTime( _date, _hour, _min, _sec, _msec );
  Assert(_hour = 0);
  Assert(_min = 1);
  Assert(_sec = 0);
  Assert(_msec = 0);

  c := a + (b-a);
  Assert(VarIsPythonDateTime(c));
  Assert(c = b);
  Assert(c <> a);
  Assert(a < b);
  Assert(b > a);

  GetPythonEngine.DatetimeConversionMode := dcmToDatetime;
  try
    _date := EncodeDate(2003, 01, 28) + EncodeTime(12, 22, 33, 450);
    a := VarPythonCreate(_date);
    Assert(VarIsPythonDateTime(c));
    _date2 := a;
    DecodeDate( _date, _year, _month, _day );
    DecodeTime( _date, _hour, _min, _sec, _msec );
    DecodeDate( _date2, _year2, _month2, _day2 );
    DecodeTime( _date2, _hour2, _min2, _sec2, _msec2 );
    Assert( _year2 = _year );
    Assert( _month2 = _month );
    Assert( _day2 = _day );
    Assert( _hour2 = _hour );
    Assert( _min2 = _min );
    Assert( _sec2 = _sec );
    Assert( _msec2 = _msec );
    Assert(a.year  = 2003);
    Assert(a.month = 01);
    Assert(a.day   = 28);
    Assert(a.hour  = 12);
    Assert(a.minute   = 22);
    Assert(a.second   = 33);
    Assert(a.microsecond  = 450000);
  finally
    GetPythonEngine.DatetimeConversionMode := dcmToTuple;
  end;
  // Done!
  Log('Dates test was Ok.');
end;

procedure TMain.btnTestObjectsClick(Sender: TObject);
var
  _main, f, a, b, c : Variant;
  val : Integer;
  _folder, _str : String;
  _myModule : Variant;
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
  _main := MainModule;
  Assert( VarIsPythonModule(_main) );
  Assert( VarIsPythonModule(SysModule) );
  Assert( Import('sys').version = SysModule.version );
  Assert( Boolean(SysModule.modules.has_key(GetPythonEngine.ExecModule)) ); // if __main__ in sys.modules
  Assert( VarIsSameType(_main, SysModule) );
  Assert( _type(_main).__name__ = 'module');
  Assert( BuiltinModule.type(_main).__name__ = 'module');

  Assert( VarIsPythonClass(_main.Foo) );
  Assert( VarIsPythonClass(_main.Bar) );
  Assert( VarIsPythonClass(_main.XYZ) );
  Assert( not VarIsPythonClass(_main.Foo.__add__) );
  Assert( not VarIsPythonClass(_main.f) );
  Assert( VarIsPythonCallable(_main.Foo) );
  Assert( VarIsPythonCallable(_main.Foo) );
  Assert( VarIsTrue(BuiltinModule.callable(_main.Foo)) );
  Assert( VarIsPythonInstance(_main.f) );
  Assert( VarIsSame(_main.f.__class__, _main.Foo) );
  Assert( VarIsPythonMethod(_main.f.Inc) );
  Assert( VarIsPythonCallable(_main.f.Inc) );
  Assert( VarIsTrue(BuiltinModule.callable(_main.f.Inc)) );
  Assert( VarIsPythonFunction(_main.Add) );
  Assert( VarIsPythonCallable(_main.Add) );
  Assert( VarIsInstanceOf(_main.f, _main.Foo) );
  Assert( VarIsTrue(BuiltinModule.isinstance(_main.f, _main.Foo)) );
  Assert( VarIsSubclassOf(_main.Bar, _main.Foo) );
  Assert( VarIsTrue(BuiltinModule.issubclass(_main.Bar, _main.Foo)) );
  Assert( not VarIsSubclassOf(_main.Foo, _main.Bar) );
  Assert( VarIsInstanceOf(_main.b, _main.Foo) );
  Assert( not VarIsInstanceOf(_main.f, _main.Bar) );
  Assert( VarIsTrue( BuiltinModule.vars(_main).has_key(string('f')) ) );
  Assert( VarIsTrue( BuiltinModule.dir(_main).Contains(string('f')) ) );

  f := _main.Foo(); // new instance of class Foo
  Log('Instanciate class Foo: ' + f);
  f.Inc(); // call a method without any arg, because there's a default arg.
  f.Inc(2); // call a method with one arg, overriding the default arg.
  Assert( VarIsPythonNumber(f.Value) );
  Assert( VarIsPythonInteger(f.Value) );
  Assert( f.Value = _main.f.Value ); // compare the result with what we did in the script
  Assert( f.GetValue() = _main.f.GetValue() ); // compare the result with what we did in the script
  Assert( VarIsPython( f.GetSelf() ) );
  Assert( VarIsSame( f.GetSelf(), f ) );
  Assert( BuiltinModule.getattr(f, 'Value') = f.Value );
  // python (+) operator overloading
  a := _main.Foo(10);
  b := _main.Foo(5);
  c := a + b;
  Assert(a.Value = 10);
  Assert(b.Value = 5);
  Assert(c.Value = 15);
  Log('Test -> a, b, c : ' + a.Value + ', '  + b.Value + ', '  + c.Value);
  // cascading calls
  Assert( f.GetSelf().GetSelf().GetSelf().GetSelf().GetValue() = _main.f.GetValue() );
  Assert( Boolean(f.__dict__.has_key('Value')) );
  Assert( VarIsTrue( BuiltinModule.hasattr(f, 'Value') ) );
  _str := 'Value';
  Assert( Boolean(f.__dict__.has_key(_str)) ); // check with a string var
  Assert( Boolean( BuiltinModule.hasattr(f, _str) ) );
  val := f.Value;
  f.Add(f); // passing itself as an argument
  Assert( f.Value = val*2 );
  // check param order
  f.SetABC(1, 2, 3);
  Assert(f.A = 1);
  Assert(f.B = 2);
  Assert(f.C = 3);
  // add a property to an instance
  f.Z := 99;
  Assert(f.Z = 99);
  // add a var to a module
  _main.Z := 99;
  Assert(_main.Z = 99);
  // check none
  Assert( VarIsNone(None) );
  Assert( VarIsNone(VarPythonCreate([1, Null, 3]).GetItem(1)) ); // Null is casted to None
  Assert( VarIsNone(VarPythonCreate([1, None, 3]).GetItem(1)) );
  Assert( VarIsNone(f.Inc()) );
  Assert( f.Inc() = None );
  Assert( not Boolean(None) ); // if not None:
  Assert( not VarIsTrue(None) ); // if not None:
  Assert( Boolean(f) ); // if f:
  Assert( VarIsTrue(f) ); // if f:

  // call a function
  Assert( _main.Add(2, 2) = 4 );
  // call a function with a mix of regular parameters and named parameters
  f := _main.MakeList(1, 2, 3, 4);
  Assert(VarIsPythonList(f));
  Assert(f.Length = 4);
  Assert(f.GetItem(0) = 1);
  Assert(f.GetItem(1) = 2);
  Assert(f.GetItem(2) = 3);
  Assert(f.GetItem(3) = 4);
  f := _main.MakeList(1, d:=3, c:=4, b:=2);
  Assert(VarIsPythonList(f));
  Assert(f.Length = 4);
  Assert(f.GetItem(0) = 1);
  Assert(f.GetItem(1) = 2);
  Assert(f.GetItem(2) = 4);
  Assert(f.GetItem(3) = 3);
  f := _main.MakeList(1, 2, d:= 3, c:=4);
  Assert(VarIsPythonList(f));
  Assert(f.Length = 4);
  Assert(f.GetItem(0) = 1);
  Assert(f.GetItem(1) = 2);
  Assert(f.GetItem(2) = 4);
  Assert(f.GetItem(3) = 3);
  f := _main.MakeList(1, 2, 3, d:=4);
  Assert(VarIsPythonList(f));
  Assert(f.Length = 4);
  Assert(f.GetItem(0) = 1);
  Assert(f.GetItem(1) = 2);
  Assert(f.GetItem(2) = 3);
  Assert(f.GetItem(3) = 4);
  f := _main.MakeList(b:=1, a:=2, d:= 3, c:=4);
  Assert(VarIsPythonList(f));
  Assert(f.Length = 4);
  Assert(f.GetItem(0) = 2);
  Assert(f.GetItem(1) = 1);
  Assert(f.GetItem(2) = 4);
  Assert(f.GetItem(3) = 3);

  // importing an external module and using it
  // first, extend the path with our current folder
  _folder := ExtractFilePath(Application.ExeName);
  if (Length(_folder) > 0) and (_folder[Length(_folder)] = '\') then
    Delete(_folder, Length(_folder), 1);
  if not Boolean(SysModule.path.Contains(_folder)) then
    SysModule.path.insert(0, _folder);
  // import the module
  _myModule := Import('MyModule');
  // call one of his functions
  Assert( _myModule.Add(2, 2) = 4 );
  // delete module var f
  _main.__dict__.DeleteItem(string('f'));
  Assert( _main.__dict__.has_key(string('f')) = False );
  // open a file using Python
  if FileExists('MyModule.py') then
  begin
    f := BuiltinModule.open('MyModule.py', string('r')).readlines();
    with TStringList.Create do
    try
      LoadFromFile('MyModule.py');
      Assert( len(f) = Count);
    finally
      Free; // TStringList
    end; // of try
  end; // of if
  // Done!
  Log('Objects test was Ok.');
end;

procedure TMain.RunSelectedTests;
begin
  if cbIntegers.Checked then
    btnTestIntegersClick(nil);
  if cbFloats.Checked then
    btnTestFloatsClick(nil);
  if cbStrings.Checked then
    btnTestStringsClick(nil);
  if cbSequences.Checked then
    btnTestSequencesClick(nil);
  if cbMappings.Checked then
    btnTestMappingsClick(nil);
  if cbDates.Checked then
    btnTestDatesClick(nil);
  if cbObjects.Checked then
    btnTestObjectsClick(nil);
end;

procedure TMain.btnRunSelectedTestsClick(Sender: TObject);
begin
  RunSelectedTests;
end;

procedure TMain.btnRunNTimesClick(Sender: TObject);
var
  i, _count : Integer;
begin
  _count := StrToInt(edtTestCount.Text)-1;
  fVerbose := False;
  PythonEngine1.RedirectIO := False;
  ProgressBar1.Visible := True;
  try
    for i := 0 to _count do
    begin
      ProgressBar1.Position := i * 100 div _count;
      ProgressBar1.Update;
      RunSelectedTests;
    end;
  finally
    fVerbose := True;
    PythonEngine1.RedirectIO := True;
    ProgressBar1.Visible := False;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  fVerbose := True;
end;

end.
