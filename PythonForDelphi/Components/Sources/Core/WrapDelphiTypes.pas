{$I Definition.Inc}

unit WrapDelphiTypes;

interface

uses
  Classes,
  SysUtils,
  PythonEngine,
  Types,
  WrapDelphi;

type
  TPyDelphiPoint = class(TPyObject)
  private
    fValue: TPoint;
  protected
    // Exposed Getters
    function Get_X(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Y(Acontext : Pointer) : PPyObject; cdecl;
    // Exposed Setters
    function Set_X(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Y(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;

    function  Compare( obj: PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;

    property Value : TPoint read fValue write fValue;
  end;

  TPyDelphiRect = class(TPyObject)
  private
    fValue: TRect;
  protected
    // Exposed Getters
    function Get_Left(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Top(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Right(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Bottom(Acontext : Pointer) : PPyObject; cdecl;
    function Get_TopLeft(Acontext : Pointer) : PPyObject; cdecl;
    function Get_BottomRight(Acontext : Pointer) : PPyObject; cdecl;
    // Exposed Setters
    function Set_Left(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Top(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Right(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_Bottom(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_TopLeft(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_BottomRight(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    PyDelphiWrapper : TPyDelphiWrapper;

    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;

    function  Compare( obj: PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;

    property Value : TRect read fValue write fValue;
  end;

  TPyDelphiSize = class(TPyObject)
  private
    fValue: TSize;
  protected
    // Exposed Getters
    function Get_CX(Acontext : Pointer) : PPyObject; cdecl;
    function Get_CY(Acontext : Pointer) : PPyObject; cdecl;
    // Exposed Setters
    function Set_CX(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_CY(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;

    function  Compare( obj: PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;

    property Value : TSize read fValue write fValue;
  end;

  function WrapPoint(APyDelphiWrapper : TPyDelphiWrapper; const APoint : TPoint) : PPyObject;
  function WrapRect(APyDelphiWrapper : TPyDelphiWrapper; const ARect : TRect) : PPyObject;
  function WrapSize(APyDelphiWrapper : TPyDelphiWrapper; const ASize : TSize) : PPyObject;
  function CheckPointAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : TPoint) : Boolean;
  function CheckRectAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : TRect) : Boolean;
  function CheckSizeAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : TSize) : Boolean;

implementation

uses
  Math;

 { Register the wrappers, the globals and the constants }
type
  TTypesRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TExtCtrlsRegistration }

procedure TTypesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TTypesRegistration.Name: String;
begin
  Result := 'Types';
end;

procedure TTypesRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterHelperType(TPyDelphiPoint);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiRect);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiSize);
end;

{ Helper functions }
function WrapPoint(APyDelphiWrapper : TPyDelphiWrapper; const APoint : TPoint) : PPyObject;
var
  _type : TPythonType;
begin
  _type := APyDelphiWrapper.GetHelperType('PointType');
  Result := _type.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiPoint).Value := APoint;
end;

function WrapRect(APyDelphiWrapper : TPyDelphiWrapper; const ARect : TRect) : PPyObject;
var
  _type : TPythonType;
begin
  _type := APyDelphiWrapper.GetHelperType('RectType');
  Result := _type.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiRect do
  begin
    PyDelphiWrapper := APyDelphiWrapper;
    Value := ARect;
  end;
end;

function WrapSize(APyDelphiWrapper : TPyDelphiWrapper; const ASize : TSize) : PPyObject;
var
  _type : TPythonType;
begin
  _type := APyDelphiWrapper.GetHelperType('SizeType');
  Result := _type.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiSize).Value := ASize;
end;

function CheckPointAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : TPoint) : Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiPoint) then
    begin
      AValue := TPyDelphiPoint(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only Point objects', [AAttributeName]))));
    end;
  end;
end;

function CheckRectAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : TRect) : Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiRect) then
    begin
      AValue := TPyDelphiRect(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only Rect objects', [AAttributeName]))));
    end;
  end;
end;

function CheckSizeAttribute(AAttribute : PPyObject; const AAttributeName : String; var AValue : TSize) : Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiSize) then
    begin
      AValue := TPyDelphiSize(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only Size objects', [AAttributeName]))));
    end;
  end;
end;

{ TPyDelphiPoint }

function TPyDelphiPoint.Compare(obj: PPyObject): Integer;
var
  _other : TPyDelphiPoint;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiPoint) then
  begin
    _other := TPyDelphiPoint(PythonToDelphi(obj));
    Result := CompareValue(Value.X, _other.Value.X);
    if Result = 0 then
      Result := CompareValue(Value.Y, _other.Value.Y);
  end
  else
    Result := 1;
end;

constructor TPyDelphiPoint.CreateWith(APythonType: TPythonType;
  args: PPyObject);
var
  x, y : Integer;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple( args, 'ii:Create',@x, @y ) <> 0 then
  begin
   fValue.X := x;
   fValue.Y := y;
  end
end;

function TPyDelphiPoint.Get_X(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.X);
end;

function TPyDelphiPoint.Get_Y(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.Y);
end;

class procedure TPyDelphiPoint.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('X', @TPyDelphiPoint.Get_X, @TPyDelphiPoint.Set_X,
        'Provides access to the X coordinate of a point', nil);
      AddGetSet('Y', @TPyDelphiPoint.Get_Y, @TPyDelphiPoint.Set_Y,
        'Provides access to the Y coordinate of a point', nil);
    end;
end;

function TPyDelphiPoint.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyString_FromDelphiString(Format('<Point (%d, %d)>',
    [Value.X, Value.Y]));
end;

class procedure TPyDelphiPoint.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'Point';
  PythonType.Name := string(PythonType.TypeName) + 'Type';
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi TPoint type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

function TPyDelphiPoint.Set_X(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  x : Integer;
begin
  if CheckIntAttribute(AValue, 'X', x) then
    with GetPythonEngine do begin
      Adjust(@Self);
      fValue.X := x;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiPoint.Set_Y(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  y : Integer;
begin
  if CheckIntAttribute(AValue, 'Y', y) then
    with GetPythonEngine do begin
      Adjust(@Self);
      fValue.Y := y;
      Result := 0;
    end
    else
      Result := -1;
end;

{ TPyDelphiRect }

function TPyDelphiRect.Compare(obj: PPyObject): Integer;
var
  _other : TPyDelphiRect;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiRect) then
  begin
    _other := TPyDelphiRect(PythonToDelphi(obj));
    Result := CompareValue(Value.Left, _other.Value.Left);
    if Result = 0 then
      Result := CompareValue(Value.Top, _other.Value.Top);
    if Result = 0 then
      Result := CompareValue(Value.Right, _other.Value.Right);
    if Result = 0 then
      Result := CompareValue(Value.Bottom, _other.Value.Bottom);
  end
  else
    Result := 1;
end;

constructor TPyDelphiRect.CreateWith(APythonType: TPythonType;
  args: PPyObject);
begin
  inherited;
  APythonType.Engine.PyArg_ParseTuple( args, 'iiii:Create',@fValue.Left, @fValue.Top, @fValue.Right, @fValue.Bottom );
end;

function TPyDelphiRect.Get_Bottom(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.Bottom);
end;

function TPyDelphiRect.Get_BottomRight(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapPoint(PyDelphiWrapper, Value.BottomRight);
end;

function TPyDelphiRect.Get_Left(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.Left);
end;

function TPyDelphiRect.Get_Right(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.Right);
end;

function TPyDelphiRect.Get_Top(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.Top);
end;

function TPyDelphiRect.Get_TopLeft(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapPoint(PyDelphiWrapper, Value.TopLeft);
end;

class procedure TPyDelphiRect.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Left', @TPyDelphiRect.Get_Left, @TPyDelphiRect.Set_Left,
        'Provides access to the Left coordinate of a rectangle', nil);
      AddGetSet('Top', @TPyDelphiRect.Get_Top, @TPyDelphiRect.Set_Top,
        'Provides access to the Top coordinate of a rectangle', nil);
      AddGetSet('Right', @TPyDelphiRect.Get_Right, @TPyDelphiRect.Set_Right,
        'Provides access to the Right coordinate of a rectangle', nil);
      AddGetSet('Bottom', @TPyDelphiRect.Get_Bottom, @TPyDelphiRect.Set_Bottom,
        'Provides access to the Bottom coordinate of a rectangle', nil);
      AddGetSet('TopLeft', @TPyDelphiRect.Get_TopLeft, @TPyDelphiRect.Set_TopLeft,
        'Provides access to the TopLeft coordinate of a rectangle', nil);
      AddGetSet('BottomRight', @TPyDelphiRect.Get_BottomRight, @TPyDelphiRect.Set_BottomRight,
        'Provides access to the BottomRight coordinate of a rectangle', nil);
    end;
end;

function TPyDelphiRect.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyString_FromDelphiString(Format('<Rect (%d, %d, %d, %d)>',
    [Value.Left, Value.Top, Value.Right, Value.Bottom]));
end;

function TPyDelphiRect.Set_Bottom(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  if CheckIntAttribute(AValue, 'Bottom', _value) then
  begin
    Adjust(@Self);
    fValue.Bottom := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiRect.Set_BottomRight(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : TPoint;
begin
  if CheckPointAttribute(AValue, 'BottomRight', _value) then
  begin
    Adjust(@Self);
    fValue.BottomRight := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiRect.Set_Left(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  if CheckIntAttribute(AValue, 'Left', _value) then
  begin
    Adjust(@Self);
    fValue.Left := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiRect.Set_Right(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  if CheckIntAttribute(AValue, 'Right', _value) then
  begin
    Adjust(@Self);
    fValue.Right := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiRect.Set_Top(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  if CheckIntAttribute(AValue, 'Top', _value) then
  begin
    Adjust(@Self);
    fValue.Top := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiRect.Set_TopLeft(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : TPoint;
begin
  if CheckPointAttribute(AValue, 'TopLeft', _value) then
  begin
    Adjust(@Self);
    fValue.TopLeft := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

class procedure TPyDelphiRect.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'Rect';
  PythonType.Name := string(PythonType.TypeName) + 'Type';
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi TRect type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;


{ TPyDelphiSize }

function TPyDelphiSize.Compare(obj: PPyObject): Integer;
var
  _other : TPyDelphiSize;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiSize) then
  begin
    _other := TPyDelphiSize(PythonToDelphi(obj));
    Result := CompareValue(Value.cx, _other.Value.cx);
    if Result = 0 then
      Result := CompareValue(Value.cy, _other.Value.cy);
  end
  else
    Result := 1;
end;

constructor TPyDelphiSize.CreateWith(APythonType: TPythonType;
  args: PPyObject);
var
  cx, cy : Integer;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple( args, 'ii:Create',@cx, @cy ) <> 0 then
  begin
   fValue.cx := cx;
   fValue.cy := cy;
  end
end;

function TPyDelphiSize.Get_CX(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.cx);
end;

function TPyDelphiSize.Get_CY(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Value.cy);
end;

class procedure TPyDelphiSize.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('cx', @TPyDelphiSize.Get_CX, @TPyDelphiSize.Set_CX,
        'Provides access to the width of the size', nil);
      AddGetSet('cy', @TPyDelphiSize.Get_CY, @TPyDelphiSize.Set_CY,
        'Provides access to the height of the size', nil);
    end;
end;

function TPyDelphiSize.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyString_FromDelphiString(Format('<Size (%d, %d)>',
    [Value.cx, Value.cy]));
end;

class procedure TPyDelphiSize.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'Size';
  PythonType.Name := string(PythonType.TypeName) + 'Type';
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi TSize type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

function TPyDelphiSize.Set_CX(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  cx : Integer;
begin
  if CheckIntAttribute(AValue, 'cx', cx) then
  begin
    Adjust(@Self);
    fValue.cx := cx;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiSize.Set_CY(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  cy : Integer;
begin
  if CheckIntAttribute(AValue, 'cy', cy) then
  begin
    Adjust(@Self);
    fValue.cy := cy;
    Result := 0;
  end
  else
    Result := -1;
end;

initialization
  RegisteredUnits.Add(TTypesRegistration.Create);
end.
