unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, PairSplitter, PythonEngine, lcl.PythonGUIInputOutput;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    btnTestIntegers: TButton;
    cbTestIntegers: TCheckBox;
    Memo1: TMemo;
    Memo2: TMemo;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure btnTestIntegersClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    fVerbose : Boolean;
    procedure Log( const AText : String );
    procedure RunSelectedTests;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

Uses
  VarPyth;

{ TForm1 }


procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( Memo2.Lines );
end;

procedure TForm1.btnTestIntegersClick(Sender: TObject);
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
  Log('Integer test was Ok.');end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fVerbose := True;
end;

procedure TForm1.Log(const AText: String);
begin
  if fVerbose then
    Memo1.Lines.Add(AText);
end;

procedure TForm1.RunSelectedTests;
begin

end;

initialization
  {$I unit1.lrs}

end.

