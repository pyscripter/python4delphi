unit Unit1;
{
  Demo 26 reworked using WrapDelphi
  See also Demo 31.
  For further information on using WrapDelphi look
  at the comments at the top of the unit.
}

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  WrapDelphi, WrapDelphiClasses,
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    Memo1: TMemo;
    PythonEngine1: TPythonEngine;
    PythonModule1: TPythonModule;
    Panel1: TPanel;
    Button1: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    PyDelphiWrapper: TPyDelphiWrapper;
    procedure Button1Click(Sender: TObject);
    procedure PyDelphiWrapperInitialization(Sender: TObject);
  private
  public
  end;

  {
     Delphi class to be exported using WrapDelphi
     Note the METHODINFO directive for automatically
     exporting methods
  }
  {$METHODINFO ON}
  TPoint = class(TPersistent)
    private
      fx, fy : Integer;
      fName : String;
    public
      procedure OffsetBy( dx, dy : integer );
    published
      property x : integer read fx write fx;
      property y : integer read fy write fy;
      property Name : string read fName write fName;
  end;
  {$METHODINFO OFF}

  // TPyPoint is TPyObject decscendent, more specifically
  // a TPyDelphiPersistent (from WrapDelphi) descendent.
  // It overrides a few key methods Repr, Create and CreateWith
  TPyPoint = class(TPyDelphiPersistent)
    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;
    // Basic services
    function  Repr : PPyObject; override;

    class function  DelphiObjectClass : TClass; override;
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

// First, we need to initialize the property PyObjectClass with
// the class of our Type object

{ TPoint }

procedure TPoint.OffsetBy(dx, dy: integer);
begin
  Inc(fx, dx);
  Inc(fy, dy);
end;

{ TPyPoint }

// We override the constructors

constructor TPyPoint.Create( APythonType : TPythonType );
begin
  inherited;
  // we need to set DelphiObject property
  DelphiObject := TPoint.Create;
  with TPoint(DelphiObject) do begin
    x := 0;
    y := 0;
  end;
  Owned := True; // We own the objects we create
end;

// Don't call the Create constructor of TPyPoint, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyPoint.Create will be automatically be called.

constructor TPyPoint.CreateWith( PythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine, DelphiObject as TPoint do
    begin
      if PyArg_ParseTuple( args, 'ii:CreatePoint',@fx, @fy ) = 0 then
        Exit;
    end;
end;

// Then we override the needed services

class function TPyPoint.DelphiObjectClass: TClass;
begin
  Result := TPoint;
end;

function  TPyPoint.Repr : PPyObject;
begin
  with GetPythonEngine, DelphiObject as TPoint do
    Result := VariantAsPyObject(Format('(%d, %d)',[x, y]));
    // or Result := PyString_FromString( PAnsiChar(Format('(%d, %d)',[x, y])) );
end;

/////////////////////////////////////////////////
procedure TForm1.Button1Click(Sender: TObject);
var
  DelphiPoint : TPoint;
  p : PPyObject;
begin
  // Here's how you can create/read Python vars from Delphi with
  // Delphi/Python objects.
  DelphiPoint := TPoint.Create;
  DelphiPoint.x := 10;
  DelphiPoint.y := 20;

  // DelphiPoint will be owned and eventually destroyed by Python
  p := PyDelphiWrapper.Wrap(DelphiPoint, soOwned);

  PythonModule1.SetVar( 'myPoint', p );

  // Note, that you must not free the delphi point yourself.
  // Instead use the GetPythonEngine.Py_XDECREF(obj) method,
  // because the object may be used by another Python object.
  PythonEngine1.Py_DecRef(p);

  // Excecute the script
  PythonEngine1.ExecStrings( memo1.Lines );

end;

procedure TForm1.PyDelphiWrapperInitialization(Sender: TObject);
begin
  // Register PythonType1 encapsulating Delphi Type TPoint
  PyDelphiWrapper.RegisterDelphiWrapper(TPyPoint);
end;

end.
