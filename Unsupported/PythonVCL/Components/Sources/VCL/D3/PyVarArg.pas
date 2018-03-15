unit PyVarArg;

interface
uses Classes, SysUtils, PythonEngine;

type
  TPyVarArg = class( TPyObject )
    Value : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    destructor Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PChar) : PPyObject; override;
    function  SetAttr(key : PChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Public methods
    procedure SetValue( val : PPyObject );
  end;

  TPyVarArgType = class( TPythonType )
    procedure Initialize; override;
  end;

  function  PyVarArg_Check( obj : PPyObject ) : Boolean;
  function  CreateVarArg( val : PPyObject ) : TPyVarArg;
  function  ExtractValueOfVarArg( obj : PPyObject ) : PPyObject;

  procedure Init( AOwner : TComponent; AEngine : TPythonEngine );

var
  gPyVarArgType : TPythonType;

implementation

// Constructors & Destructors
constructor TPyVarArg.Create( APythonType : TPythonType );
begin
  inherited;
  with GetPythonEngine do
    SetValue( Py_None );
end;

destructor TPyVarArg.Destroy;
begin
  with GetPythonEngine do
    begin
      Py_XDecRef( value );
    end;
  inherited;
end;

// Type services
////////////////

// Basic services
function  TPyVarArg.GetAttr(key : PChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( key, 'Value' ) = 0 then
        begin
          Result := Value;
          Py_XIncRef( Result );
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
          Result := nil;
        end;
    end;
end;

function  TPyVarArg.SetAttr(key : PChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if CompareText( key, 'Value' ) = 0 then
        begin
          SetValue( value );
          Result := 0;
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
        end;
    end;
end;

function  TPyVarArg.Repr : PPyObject;
begin
  with GetPythonEngine do
    begin
      if Assigned(Value) then
        Result := PyString_FromString(PChar(Format('<PyVarArg: %s>',
                  [PyObjectAsString( Value )])));
    end;
end;


// Class methods
class procedure TPyVarArg.RegisterMethods( PythonType : TPythonType );
begin
end;


// Public methods

procedure TPyVarArg.SetValue( val : PPyObject );
begin
  with GetPythonEngine do
    begin
      Py_XDecRef( Value );
      Value := val;
      Py_XIncRef( Value );
    end;
end;



procedure TPyVarArgType.Initialize;
begin
  inherited;
  PyObjectClass := TPyVarArg;
end;

function PyVarArg_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = gPyVarArgType.TheTypePtr);
end;

function  CreateVarArg( val : PPyObject ) : TPyVarArg;
begin
  Result := PythonToDelphi( gPyVarArgType.CreateInstance ) as TPyVarArg;
  Result.SetValue( val );
end;

function ExtractValueOfVarArg( obj : PPyObject ) : PPyObject;
begin
  if Assigned(obj) and PyVarArg_Check( obj ) then
    with PythonToDelphi(obj) as TPyVarArg do
      Result := Value
  else
    Result := obj;
end;

//////// Initialization of the type ///////////////

var
  IsInitialized : Boolean;

procedure Init( AOwner : TComponent; AEngine : TPythonEngine );
begin
  if IsInitialized then
    Exit;
  IsInitialized := True;
  gPyVarArgType := TPyVarArgType.Create( AOwner );
  with gPyVarArgType do
    begin
      Engine := AEngine;
      TypeName := 'VarArg';
    end;
end;

end.
