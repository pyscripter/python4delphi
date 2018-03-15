unit PyDelphiAssoc;

interface
uses Classes, SysUtils, PythonEngine;

type
  TDelphiAssoc = class( TPyObject )
    da_delphi_obj  : TObject;
    da_auto_free   : Boolean;
    da_python_inst : PPyObject;

    // Constructors & Destructors
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
    procedure ClearDelphiObj;
    procedure ClearPythonInst;
    procedure SetDelphiObj( val : TObject );
    procedure SetPythonInst( val : PPyObject);
  end;

  TDelphiAssocType = class( TPythonType )
    procedure Initialize; override;
  end;

  function  PyDelphiAssoc_Check( obj : PPyObject ) : Boolean;
  function  CreateDelphiAssocWith( obj : TObject; inst : PPyObject; auto_free : Boolean ) : TDelphiAssoc;
  function  CreateDelphiAssoc( obj : TObject; inst : PPyObject ) : TDelphiAssoc;

  procedure Init( AOwner : TComponent; AEngine : TPythonEngine );

var
  gDelphiAssocType : TPythonType;
  gCount : Integer;

implementation

// Constructors & Destructors
destructor TDelphiAssoc.Destroy;
begin
  Dec(gCount);
  if PythonOK then
    with GetPythonEngine do
      begin
        SetDelphiObj( nil );
        SetPythonInst( nil );
      end;
  inherited;
end;

// Type services
////////////////

// Basic services
function  TDelphiAssoc.GetAttr(key : PChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( key, 'DELPHI_OBJECT' ) = 0 then
        Result := PyInt_FromLong(Longint(da_delphi_obj))
      else if CompareText( key, 'PYTHON_INST' ) = 0 then
        begin
          Result := da_python_inst;
          Py_INCREF(Result);
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
          Result := nil;
        end;
    end;
end;

function  TDelphiAssoc.SetAttr(key : PChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if CompareText( key, 'DELPHI_OBJECT' ) = 0 then
        begin
          if PyInt_Check(value) then
            begin
              SetDelphiObj( TObject(PyInt_AsLong(value)) );
              Result := 0;
            end
          else
            PyErr_SetString (PyExc_AttributeError^, PChar(Format('Attribute "%s" needs an integer',[key])));
        end
      else
        begin
          PyErr_SetString (PyExc_AttributeError^, PChar(Format('Unknown attribute "%s"',[key])));
        end;
    end;
end;

function  TDelphiAssoc.Repr : PPyObject;
begin
  with GetPythonEngine do
    begin
      if Assigned(da_delphi_obj) and Assigned(da_python_inst) then
        Result := PyString_FromString(PChar(Format('<DelphiAssoc: (Delphi: %s at %p) / (Python: %s at %p)>',
                  [da_delphi_obj.ClassName,@(da_delphi_obj),
                   PyObjectAsString(da_python_inst),da_python_inst])))
      else if Assigned(da_delphi_obj) then
        Result := PyString_FromString(PChar(Format('<DelphiAssoc: (Delphi: %s at %p) / (Python: None)>',
                  [da_delphi_obj.ClassName,@(da_delphi_obj)])))
      else if Assigned(da_python_inst) then
        Result := PyString_FromString(PChar(Format('<DelphiAssoc: (Delphi: None) / (Python: %s at %p)>',
                  [PyObjectAsString(da_python_inst),da_python_inst])))
      else
        Result :=  PyString_FromString(PChar('<DelphiAssoc: Empty>'));
    end;
end;


// Class methods
class procedure TDelphiAssoc.RegisterMethods( PythonType : TPythonType );
begin
end;


// Public methods
procedure TDelphiAssoc.ClearDelphiObj;
begin
  da_delphi_obj := nil;
end;

procedure TDelphiAssoc.ClearPythonInst;
begin
  da_python_inst := nil;
end;

procedure TDelphiAssoc.SetDelphiObj( val : TObject );
begin
  if da_auto_free and Assigned( da_delphi_obj ) then
    da_delphi_obj.Free;
  da_delphi_obj := val;
end;

procedure TDelphiAssoc.SetPythonInst( val : PPyObject);
begin
  with GetPythonEngine do
    begin
      if da_auto_free and Assigned( da_python_inst ) then
        Py_DECREF(da_python_inst);
      da_python_inst := val;
      if da_auto_free and Assigned( val ) then
        Py_INCREF( val );
    end;
end;


procedure TDelphiAssocType.Initialize;
begin
  inherited;
  PyObjectClass := TDelphiAssoc;
end;

function PyDelphiAssoc_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = gDelphiAssocType.TheTypePtr);
end;

function  CreateDelphiAssocWith( obj : TObject; inst : PPyObject; auto_free : Boolean ) : TDelphiAssoc;
begin
  Result := PythonToDelphi( gDelphiAssocType.CreateInstance ) as TDelphiAssoc;
  with GetPythonEngine, Result do
    begin
      da_auto_free := auto_free;
      SetDelphiObj( obj );
      SetPythonInst( inst );
    end;
  Inc(gCount);
end;

function  CreateDelphiAssoc( obj : TObject; inst : PPyObject ) : TDelphiAssoc;
begin
  Result := CreateDelphiAssocWith( obj, inst, True );
end;

//////// Initialization of the type ///////////////

var
  IsInitialized : Boolean;

procedure Init( AOwner : TComponent; AEngine : TPythonEngine );
begin
  if IsInitialized then
    Exit;
  IsInitialized := True;
  gDelphiAssocType := TDelphiAssocType.Create( AOwner );
  with gDelphiAssocType do
    begin
      Engine := AEngine;
      TypeName := 'DelphiAssoc';
    end;
end;

end.
