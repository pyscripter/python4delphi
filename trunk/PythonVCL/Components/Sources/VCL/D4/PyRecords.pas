unit PyRecords;

interface
uses Windows, Classes, SysUtils, PythonEngine;

type
  //////////////////////////////////////////////////////////////////
  //
  //   record TPoint
  //
  //////////////////////////////////////////////////////////////////

  TPyPoint = class( TPyObject )
    Point : TPoint;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
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
  end;

  TPyPointType = class( TPythonType )
    procedure Initialize; override;
  end;

  function  PyPoint_Check( obj : PPyObject ) : Boolean;
  function  CreatePoint( p : TPoint ) : TPyPoint;
  function  PyObjectAsPoint( obj : PPyObject ) : TPoint;
  function  PyObjectAsRecordWith( const aType, varName : String ) : String;



  //////////////////////////////////////////////////////////////////
  //
  //   record TRect
  //
  //////////////////////////////////////////////////////////////////

type
  TPyRect = class( TPyObject )
    Rect : TRect;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
    destructor Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PChar) : PPyObject; override;
    function  SetAttr(key : PChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

  end;

  TPyRectType = class( TPythonType )
    procedure Initialize; override;
  end;

  function  PyRect_Check( obj : PPyObject ) : Boolean;
  function  CreateRect( r : TRect ) : TPyRect;
  function  PyObjectAsRect( obj : PPyObject ) : TRect;

  //////////////////////////////////////////////////////////////////
  procedure Init( AOwner : TComponent; AEngine : TPythonEngine );
  function  IsImplementedRecord( const val : String ) : Boolean;
  function  MakeRecord( const Value, aType : String ) : String;
  function  PyObjectAsRecord( const aType : String ) : String;

var
  gPyRecords   : TPythonModule;
  gPyPointType : TPythonType;
  gPyRectType  : TPythonType;

implementation

  //////////////////////////////////////////////////////////////////
  //
  //   record TPoint
  //
  //////////////////////////////////////////////////////////////////

// Constructors & Destructors
constructor TPyPoint.Create( APythonType : TPythonType );
begin
  inherited;
end;

constructor TPyPoint.CreateWith( APythonType : TPythonType; args : PPyObject );
var
  x, y : Integer;
begin
  inherited;
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'ii:CreateTPoint', [@x, @y] ) = 0 then
      begin
        PyErr_Clear;
        Exit;
      end;
    Point.x := x;
    Point.y := y;
  end;
end;

destructor TPyPoint.Destroy;
begin
  inherited;
end;

// Type services
////////////////

// Basic services
function  TPyPoint.GetAttr(key : PChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( key, 'x' ) = 0 then
        begin
          Result := PyInt_FromLong( Point.x );
        end
      else if CompareText( key, 'y' ) = 0 then
        begin
          Result := PyInt_FromLong( Point.y );
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
          Result := nil;
        end;
    end;
end;

function  TPyPoint.SetAttr(key : PChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not PyInt_Check(value) then
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('the attribute "%s" must be an integer',[key])));
          Exit;
        end;
      if CompareText( key, 'x' ) = 0 then
        begin
          Point.x := PyInt_AsLong( value );
          Result := 0;
        end
      else if CompareText( key, 'y' ) = 0 then
        begin
          Point.y := PyInt_AsLong( value );
          Result := 0;
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
        end;
    end;
end;

function  TPyPoint.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString(PChar(Format('(%d, %d)', [Point.x, Point.y])));
end;


// Class methods
class procedure TPyPoint.RegisterMethods( PythonType : TPythonType );
begin
end;


procedure TPyPointType.Initialize;
begin
  inherited;
  PyObjectClass := TPyPoint;
end;

function PyPoint_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = gPyPointType.TheTypePtr);
end;

function  CreatePoint( p : TPoint ) : TPyPoint;
begin
  Result := PythonToDelphi( gPyPointType.CreateInstance ) as TPyPoint;
  Result.Point := p;
end;

function  PyObjectAsPoint( obj : PPyObject ) : TPoint;
begin
  if Assigned(obj) and PyPoint_Check( obj ) then
    with PythonToDelphi(obj) as TPyPoint do
      Result := Point
  else
    Result := Point(0,0);
end;

  //////////////////////////////////////////////////////////////////
  //
  //   record TRect
  //
  //////////////////////////////////////////////////////////////////

// Constructors & Destructors
constructor TPyRect.Create( APythonType : TPythonType );
begin
  inherited;
end;

constructor TPyRect.CreateWith( APythonType : TPythonType; args : PPyObject );
var
  left, top, right, bottom : Integer;
begin
  inherited;
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiii:CreateTRect', [@left, @top, @right, @bottom] ) = 0 then
      begin
        PyErr_Clear;
        Exit;
      end;
    Rect.Left   := left;
    Rect.Top    := top;
    Rect.Right  := right;
    Rect.Bottom := bottom;
  end;
end;

destructor TPyRect.Destroy;
begin
  inherited;
end;

// Type services
////////////////

// Basic services
function  TPyRect.GetAttr(key : PChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( key, 'Left' ) = 0 then
        begin
          Result := PyInt_FromLong( Rect.Left );
        end
      else if CompareText( key, 'Top' ) = 0 then
        begin
          Result := PyInt_FromLong( Rect.Top );
        end
      else if CompareText( key, 'Right' ) = 0 then
        begin
          Result := PyInt_FromLong( Rect.Right );
        end
      else if CompareText( key, 'Bottom' ) = 0 then
        begin
          Result := PyInt_FromLong( Rect.Bottom );
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
          Result := nil;
        end;
    end;
end;

function  TPyRect.SetAttr(key : PChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not PyInt_Check(value) then
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('the attribute "%s" must be an integer',[key])));
          Exit;
        end;
      if CompareText( key, 'Left' ) = 0 then
        begin
          Rect.Left := PyInt_AsLong( value );
          Result := 0;
        end
      else if CompareText( key, 'Top' ) = 0 then
        begin
          Rect.Top := PyInt_AsLong( value );
          Result := 0;
        end
      else if CompareText( key, 'Right' ) = 0 then
        begin
          Rect.Right := PyInt_AsLong( value );
          Result := 0;
        end
      else if CompareText( key, 'Bottom' ) = 0 then
        begin
          Rect.Bottom := PyInt_AsLong( value );
          Result := 0;
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
        end;
    end;
end;

function  TPyRect.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromString(PChar(Format('(%d, %d, %d, %d)',
                  [Rect.Left, Rect.Top, Rect.Right, Rect.Bottom])));
end;


// Class methods
class procedure TPyRect.RegisterMethods( PythonType : TPythonType );
begin
end;


procedure TPyRectType.Initialize;
begin
  inherited;
  PyObjectClass := TPyRect;
end;

function PyRect_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = gPyRectType.TheTypePtr);
end;

function  CreateRect( r : TRect ) : TPyRect;
begin
  Result := PythonToDelphi( gPyRectType.CreateInstance ) as TPyRect;
  Result.Rect := r;
end;

function  PyObjectAsRect( obj : PPyObject ) : TRect;
begin
  if Assigned(obj) and PyRect_Check( obj ) then
    with PythonToDelphi(obj) as TPyRect do
      Result := Rect
  else
    Result := Rect(0,0,0,0);
end;





//////// Initialization of the type ///////////////

var
  IsInitialized : Boolean;

procedure Init( AOwner : TComponent; AEngine : TPythonEngine );
begin
  if IsInitialized then
    Exit;
  IsInitialized := True;
  gPyRecords := TPythonModule.Create( AOwner );
  with gPyRecords do
    begin
      Engine := AEngine;
      ModuleName := 'Records';
    end;
  gPyPointType := TPyPointType.Create( AOwner );
  with gPyPointType do
    begin
      Engine := AEngine;
      TypeName := 'TPoint';
      Module := gPyRecords;
    end;
  gPyRectType := TPyRectType.Create( AOwner );
  with gPyRectType do
    begin
      Engine := AEngine;
      TypeName := 'TRect';
      Module := gPyRecords;
    end;
end;


function IsImplementedRecord( const val : String ) : Boolean;
begin
  Result := (CompareText( val, 'TPoint' ) = 0) or
            (CompareText( val, 'TRect' ) = 0);
end;

function  MakeRecord( const Value, aType : String ) : String;
begin
  if CompareText( aType, 'TPoint' ) = 0 then
    Result := Format( 'CreatePoint( %s ).GetSelf', [Value] )
  else if CompareText( aType, 'TRect' ) = 0 then
    Result := Format( 'CreateRect( %s ).GetSelf', [Value] )
  else
    Result := 'Py_None';
end;

function  PyObjectAsRecord( const aType : String ) : String;
begin
  Result := PyObjectAsRecordWith( aType, 'Value' );
end;

function  PyObjectAsRecordWith( const aType, varName : String ) : String;
begin
  if CompareText( aType, 'TPoint' ) = 0 then
    Result := Format( 'PyObjectAsPoint( %s )', [varName] )
  else if CompareText( aType, 'TRect' ) = 0 then
    Result := Format( 'PyObjectAsRect( %s )', [varName] )
  else
    Result := 'nil';
end;

end.
