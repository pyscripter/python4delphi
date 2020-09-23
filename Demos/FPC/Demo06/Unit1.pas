unit Unit1;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  PythonEngine, PythonGUIInputOutput;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    PythonType1: TPythonType;
    PythonModule1: TPythonModule;
    Panel1: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonDelphiVar1: TPythonDelphiVar;
    Button4: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
    procedure PythonType1Initialization(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure PythonDelphiVar1Change(Sender: TObject);
    procedure PythonDelphiVar1GetData(Sender: TObject; var Data: Variant);
    procedure PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

  PyPointRec = record
    ob_refcnt      : NativeInt;
    ob_type        : PPyTypeObject;
    po_x           : Integer;
    po_y           : Integer;
  end;
  PPyPoint = ^PyPointRec;

  function  spam_foo( self, args : PPyObject ) : PPyObject; cdecl;
  function  spam_CreatePoint( self, args : PPyObject ) : PPyObject; cdecl;
  function  spam_getdouble( self, args : PPyObject ) : PPyObject; cdecl;

  procedure PyPoint_dealloc(obj : PPyObject); cdecl;
  function  PyPoint_getattr(obj : PPyObject; key : PAnsiChar) : PPyObject; cdecl;
  function  PyPoint_setattrfunc(obj : PPyObject; key : PAnsiChar; value : PPyObject) : Integer; cdecl;
  function  PyPoint_repr(obj : PPyObject) : PPyObject; cdecl;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( Memo1.Lines );
end;

// Here's an example of functions defined for the module spam

function spam_foo( self, args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      ShowMessage( 'args of foo: '+PyObjectAsString(args) );
      Result := ReturnNone;
    end;
end;

// This function is used to create a PyPoint instance
function  spam_CreatePoint( self, args : PPyObject ) : PPyObject; cdecl;
var
  x, y : Integer;
  p : PPyPoint;
begin
  with GetPythonEngine do
    begin
      // We want x and y values as argument
      if PyArg_ParseTuple( args, 'ii:CreatePoint',@x, @y) <> 0 then
        begin
          new(p);
          with p^ do
            begin
              ob_refcnt := 1;
              ob_type := TypeByName('Point');
              // or we could write, because it's quicker:
              // ob_type := Form1.PythonType1.TheTypePtr;
              po_x := x;
              po_y := y;
            end;
          Result := PPyObject(p);
        end
      else
        Result := nil;
    end;
end;

function spam_getdouble( self, args : PPyObject ) : PPyObject; cdecl;
// you need to pass floating point numbers as doubles to Py_BuildValue
Const
  x : double = 2.7172;
  y : double = 3.14159;
  z : double = 1.2e-12;
begin
  with GetPythonEngine do
    begin
      Result := Py_BuildValue('(iiddid)',42,815,x,y,4711,z);
    end;
end;

procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  // In a module initialization, we just need to add our
  // new methods
  with Sender as TPythonModule do
    begin
      AddMethod( 'foo', spam_foo, 'foo' );
      AddMethod( 'CreatePoint', spam_CreatePoint,
                 'function CreatePoint'+LF+
                 'Args: x, y'+LF+
                 'Result: a new Point object' );
      AddMethod( 'getdouble', spam_getdouble, 'getdouble' );
    end;
end;

// Here's an example of a new type object.
// That's more complex than a new module, but here's a
// template that you can follow.

// Here's the destructor of the object
procedure PyPoint_dealloc(obj : PPyObject); cdecl;
begin
  Dispose(obj);
end;

// Here's the read access to the attributes of an object.
// In fact it is called each time you write:
// object.value
// object.method(args)
function  PyPoint_getattr(obj : PPyObject; key : PAnsiChar) : PPyObject; cdecl;
begin
  with GetPythonEngine, PPyPoint(obj)^ do
    begin
      // Check for attribute x
      if key = 'x' then
        Result := PyInt_FromLong( po_x )
      // Check for attribute y
      else if key = 'y' then
        Result := PyInt_FromLong( po_y )
      else
        begin
          // Else check for a method
          Result := PyObject_GenericGetAttr(obj, PyString_FromString(key));
          if not Assigned(Result) then
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown attribute "%s"',[key])));
        end;
    end;
end;

// Here's the write access to the attributes of an object.
// In fact it is called each time you write:
// object.value = 1
function  PyPoint_setattrfunc(obj : PPyObject; key : PAnsiChar; value : PPyObject) : Integer; cdecl;
begin
  Result := -1;
  with GetPythonEngine, PPyPoint(obj)^ do
    begin
      // Check for attribute x
      if key = 'x' then begin
        if PyInt_Check(value) then
          begin
            po_x := PyInt_AsLong(value);
            Result := 0;
          end
        else
          PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Attribute "%s" needs an integer',[key])));
      // Check for attribute y
      end else if key = 'y' then begin
        if PyInt_Check(value) then
          begin
            po_y := PyInt_AsLong(value);
            Result := 0;
          end
        else
          PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Attribute "%s" needs an integer',[key])));
      end else
        PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown attribute "%s"',[key])));
    end;
end;

// Here's how an object should be represented, when printed for instance.
function  PyPoint_repr(obj : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine, PPyPoint(obj)^ do
    begin
      Result := PyString_FromString( PAnsiChar(Format('(%d, %d)',[po_x, po_y]) ) );
    end;
end;

// Here's a method of the object PyPoint
function  PyPoint_OffsetBy(self, args : PPyObject) : PPyObject; cdecl;
var
  x, y : Integer;
begin
  with GetPythonEngine, PPyPoint(self)^ do
    begin
      if PyArg_ParseTuple( args, 'ii:OffsetBy',@x, @y) <> 0 then
        begin
          Inc( po_x, x );
          Inc( po_y, y );
          Result := ReturnNone;
        end
      else
        Result := nil;
    end;
end;

procedure TForm1.PythonType1Initialization(Sender: TObject);
Var
  PyType : PyTypeObject;
begin
  with (Sender as TPythonType) do
    begin
      // In the initialization of a new type, we must
      // define the attributes of this type
      PyType := TheType;
      with PyType do
        begin
          tp_basicsize := sizeof(PyPointRec);
          tp_dealloc   := PyPoint_dealloc;
          tp_getattr   := PyPoint_getattr;
          tp_setattr   := PyPoint_setattrfunc;
          tp_repr      := PyPoint_repr;
          tp_str       := PyPoint_repr;
        end;
        TheType := PyType;
      // And then add the methods of the object, if needed
      AddMethod( 'OffsetBy', PyPoint_OffsetBy, 'OffsetBy(dx, dy)' );
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      if Execute then
        Memo1.Lines.LoadFromFile( FileName );
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      if Execute then
        Memo1.Lines.SaveToFile( FileName );
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowMessage( 'Value = ' + PythonDelphiVar1.ValueAsString );
end;

procedure TForm1.PythonDelphiVar1Change(Sender: TObject);
begin
  with Sender as TPythonDelphiVar do
    ShowMessage( 'Var test changed: ' + ValueAsString );
end;

procedure TForm1.PythonDelphiVar1GetData(Sender: TObject;
  var Data: Variant);
begin
  Data := Edit1.Text;
end;

procedure TForm1.PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
begin
  Edit1.Text := Data;
end;

end.
