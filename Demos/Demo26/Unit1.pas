unit Unit1;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    Memo1: TMemo;
    PythonEngine1: TPythonEngine;
    PythonModule1: TPythonModule;
    PythonType1: TPythonType;
    Panel1: TPanel;
    Button1: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PythonType1Initialization(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

  // This is a Delphi class implementing a new Python type
  // it must derive from TPyObject or one of its descendants.
  // Then it must override some methods, like the constructors,
  // the RegisterMethods and the type services' virtual methods.
  TPyPoint = class(TPyObject)
    x, y : Integer;
    Name : String;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;

    // Type services
    ////////////////

    // Basic services
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure RegisterMembers( PythonType : TPythonType ); override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;

    // Methods of TPyPoint
    procedure OffsetBy( dx, dy : Integer );

    // Interface methods
    function DoOffsetBy( args : PPyObject ) : PPyObject; cdecl;
    function DoRaiseError( args : PPyObject ) : PPyObject; cdecl;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// First, we need to initialize the property PyObjectClass with
// the class of our Type object
procedure TForm1.PythonType1Initialization(Sender: TObject);
begin
  PythonType1.PyObjectClass := TPyPoint;
end;

// We override the constructors

constructor TPyPoint.Create( APythonType : TPythonType );
begin
  inherited;
  x := 0;
  y := 0;
end;

// Don't call the Create constructor of TPyPoint, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyPoint.Create will be automatically be called.

constructor TPyPoint.CreateWith( PythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'ii:CreatePoint',@x, @y ) = 0 then
        exit;
    end;
end;

// Then we override the needed services

function  TPyPoint.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := VariantAsPyObject(Format('(%d, %d)',[x, y]));
    // or Result := PyString_FromString( PAnsiChar(Format('(%d, %d)',[x, y])) );
end;

// get/set functions
function TPyPoint_GetName( obj : PPyObject; context : Pointer) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result := PyString_FromString(PAnsiChar(AnsiString(TPyPoint(PythonToDelphi(obj)).Name)));
end;

function TPyPoint_SetName( obj, value : PPyObject; context : Pointer) : integer; cdecl;
begin
  with GetPythonEngine do
  begin
    if PyString_Check(value) then
    begin
      TPyPoint(PythonToDelphi(obj)).Name := PyObjectAsVariant(value);
      Result := 0;
    end
    else
    begin
      Result := -1;
      PyErr_SetString(PyExc_AttributeError^, 'attribute Name expected a string value');
    end;
  end;
end;

// Class methods
// We register the methods of our type

class procedure TPyPoint.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      AddMethod( 'OffsetBy', @TPyPoint.DoOffsetBy, 'Point.OffsetBy( dx, dy )' );
      AddMethod( 'RaiseError', @TPyPoint.DoRaiseError, 'Point.RaiseError()' );
    end;
end;

class procedure TPyPoint.RegisterMembers( PythonType : TPythonType );
begin
  with PythonType do
    begin
      AddMember( 'x', mtInt, NativeInt(@TPyPoint(nil).x), mfDefault, 'x coordinate');
      AddMember( 'y', mtInt, NativeInt(@TPyPoint(nil).y), mfDefault, 'y coordinate');
    end;
end;

class procedure TPyPoint.RegisterGetSets( PythonType : TPythonType );
begin
  with PythonType do
    begin
      AddGetSet('Name', TPyPoint_GetName, TPyPoint_SetName, 'Name of a point', nil);
    end;
end;

// Methods of TPyPoint
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

procedure TPyPoint.OffsetBy( dx, dy : Integer );
begin
  Inc( x, dx );
  Inc( y, dy );
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

function TPyPoint.DoOffsetBy( args : PPyObject ) : PPyObject;
var
  dx, dy : Integer;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      // first we extract the arguments
      if PyArg_ParseTuple( args, 'ii:Point.Offset',@dx, @dy ) <> 0 then
        begin
          // if it's ok, then we call the method that does the job
          // with the correct arguments
          OffsetBy( dx, dy );
          // Finally, we return nothing
          Result := ReturnNone;
        end
      else // the arguments were not right
        Result := nil;
    end;
end;

// Here's an example of how you can raise errors defined
// in the module linked to our type.
function TPyPoint.DoRaiseError( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      // This is a simple call:
      //GetModule.RaiseError( 'PointError', 'this is an example of raising an error !' );
      // This is an advanced call:
      // We provide the instance vars as a dictionary, so that we can intercept the
      // error with "except" and extract informations from the error object.
      // ArrayToPyDict needs a list of pairs: varName (string), varValue (anything)
      GetModule.RaiseErrorObj( 'EBadPoint', 'this is an example of raising an error !',
                               ArrayToPyDict( ['a', 1, 'b', 2, 'c', 3] ) );
      Result := nil;
    end;
end;

/////////////////////////////////////////////////


procedure TForm1.Button1Click(Sender: TObject);
var
  DelphiPoint : TPyPoint;
  p : PPyObject;
begin
  // Here's how you can create/read Python vars from Delphi with
  // Delphi/Python objects.

  // You should ask to the TPythonType to create an instance of its type
  // because it will do some initialization. You can use CreateInstanceWith
  // if you want to transmit some Python arguments.
  // We receive a Python object pointer
  p := PythonType1.CreateInstance;
  PythonEngine1.CheckError;
  // Then we cast the python object to the right delphi type
  DelphiPoint := TPyPoint( PythonToDelphi(p) );
  // We do some changes on the delphi object
  DelphiPoint.X:=10;
  DelphiPoint.Y:=20;
  // Add variable "myPoint" in the module "spam".
  // So you'll access to the var in the module called spam with:
  //   import spam
  //   print spam.myPoint
  PythonModule1.SetVar( 'myPoint', p );
  PythonEngine1.Py_DecRef(p);
      {
        Of course, if you want to retrieve a Python var from a module,
        just use the PythonModule1.GetVar or PythonModule1.GetVarAsVariant
        Example:
          p := PythonModule1.GetVar('myPoint');
          if Assigned(p) then
          begin
            DelphiPoint := PythonToDelphi(p) as TPyPoint;
            ...
            Py_XDecRef(p);
          end;

    end; }
  // Excecute the script
  PythonEngine1.ExecStrings( memo1.Lines );
  // Add the following line at the end of the script:
  // print spam.myPoint

  // Note, that you must not free the delphi point yourself.
  // Instead use the GetPythonEngine.Py_XDECREF(obj) method,
  // because the object may be used by another Python object.
end;

end.
