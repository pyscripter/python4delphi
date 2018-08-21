unit Unit1;

{$I Definition.Inc}

interface

uses
  Classes, SysUtils,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    PythonModule1: TPythonModule;
    PythonType1: TPythonType;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PythonModule1Events0Execute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure PythonType1Initialization(Sender: TObject);
    procedure PythonType1Events0Execute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
  private
  public
  end;

  // This is a Delphi class implementing a new Python type
  // it must derive from TPyObject or one of its descendants.
  // Then it must override some methods, like the constructors,
  // the RegisterMethods and the type services' virtual methods.
  TPyPoint = class(TPyObject)
    x, y : Integer;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Methods of TPyPoint
    procedure OffsetBy( dx, dy : Integer );
  end;


var
  Form1: TForm1;

implementation
{$R *.dfm}

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

function  TPyPoint.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if key = 'x' then
        Result := VariantAsPyObject( x )
        // Or  Result := PyInt_FromLong( x )
      else if key = 'y' then
        Result := PyInt_FromLong( y )
        // or  Result := PyInt_FromLong( y )
      else
        Result := inherited GetAttr(key);
    end;
end;

function  TPyPoint.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := 0;
  with GetPythonEngine do
    begin
      if key = 'x' then
        begin
          if PyArg_Parse( value, 'i:Point.SetAttr', @x ) = 0 then
            Result := -1;
        end
      else if key = 'y' then
        begin
          if PyArg_Parse( value, 'i:Point.SetAttr', @y ) = 0 then
            Result := -1;
        end
      else
        Result := inherited SetAttr(key, value);
    end;
end;

function  TPyPoint.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := VariantAsPyObject(Format('(%d, %d)',[x, y]));
    // or Result := PyString_FromString( PAnsiChar(Format('(%d, %d)',[x, y])) );
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

/////////////////////////////////////////////////

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( Memo1.Lines );
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

procedure TForm1.PythonModule1Events0Execute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  with GetPythonEngine do
  begin
    Result := PyString_FromString('Hello world !');
  end;
end;

procedure TForm1.PythonType1Initialization(Sender: TObject);
begin
  PythonType1.PyObjectClass := TPyPoint;
end;

procedure TForm1.PythonType1Events0Execute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
var
  dx, dy : Integer;
  Instance : TPyPoint;
begin
  with GetPythonEngine do
    begin
      // Convert the PSelf Python object to a Delphi instance pointer.
      Instance := TPyPoint(PythonToDelphi(PSelf));
      // first we extract the arguments
      if PyArg_ParseTuple( args, 'ii:Point.Offset',@dx, @dy ) <> 0 then
        begin
          // if it's ok, then we call the method that does the job
          // with the correct arguments
          Instance.OffsetBy( dx, dy );
          // Finally, we return nothing
          Result := ReturnNone;
        end
      else // the arguments were not right
        Result := nil;
    end;
end;

end.
