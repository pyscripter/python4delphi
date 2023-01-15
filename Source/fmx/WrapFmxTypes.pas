{$I ..\Definition.Inc}
unit WrapFmxTypes;

interface

uses
  System.Types, FMX.Types, PythonEngine, WrapDelphi, WrapDelphiClasses;

type
  {
    The TPointF type is primarily used for the coordinates of FireMonkey objects.
    http://docwiki.embarcadero.com/Libraries/Sydney/en/System.Types.TPointF
  }
  TPyDelphiPointF = class(TPyObject)
  private
    FValue: TPointF;
  protected
    // Exposed Getters
    function Get_X(Acontext: Pointer): PPyObject; cdecl;
    function Get_Y(Acontext: Pointer): PPyObject; cdecl;
    // Exposed Setters
    function Set_X(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Y(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    function Compare(obj: PPyObject): Integer; override;
    function Repr: PPyObject; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure SetupType(PythonType: TPythonType); override;
    property Value : TPointF read FValue write FValue;
  end;

  TPyDelphiSizeF = class(TPyObject)
  private
    FValue: TSizeF;
  protected
    // Exposed Getters
    function Get_Width(Acontext: Pointer): PPyObject; cdecl;
    function Get_Height(Acontext: Pointer): PPyObject; cdecl;
    // Exposed Setters
    function Set_Width(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Height(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    function Compare(obj: PPyObject): Integer; override;
    function Repr: PPyObject; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure SetupType(PythonType: TPythonType); override;
    property Value : TSizeF read FValue write FValue;
  end;

  TPyDelphiRectF = class(TPyObject)
  private
    FValue: TRectF;
  protected
    // Exposed Getters
    function Get_Top(Acontext: Pointer): PPyObject; cdecl;
    function Get_Bottom(Acontext: Pointer): PPyObject; cdecl;
    function Get_Left(Acontext: Pointer): PPyObject; cdecl;
    function Get_Right(Acontext: Pointer): PPyObject; cdecl;
    // Exposed Setters
    function Set_Top(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Bottom(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Left(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Right(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    function Compare(obj: PPyObject): Integer; override;
    function Repr: PPyObject; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure SetupType(PythonType: TPythonType); override;
    property Value: TRectF read FValue write FValue;
  end;

  TPyDelphiFmxObject = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TFmxObject;
    procedure SetDelphiObject(const Value: TFmxObject);
  protected
    // Property Getters
    function Get_Parent(AContext: Pointer): PPyObject; cdecl;
    // Property Setters
    function Set_Parent(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    // Properties
    property DelphiObject: TFmxObject read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPosition = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TPosition;
    procedure SetDelphiObject(const Value: TPosition);
  protected
    //Exposed Getters
    function Get_X(Acontext: Pointer): PPyObject; cdecl;
    function Get_Y(Acontext: Pointer): PPyObject; cdecl;
    function Get_Point(Acontext: Pointer): PPyObject; cdecl;
    //Exposed Setters
    function Set_X(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Y(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Point(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    // Properties
    property DelphiObject: TPosition read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPopupMenu = class(TPyDelphiFMXObject)
  private
    function GetDelphiObject: TCustomPopupMenu;
    procedure SetDelphiObject(const Value: TCustomPopupMenu);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomPopupMenu read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBounds = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TBounds;
    procedure SetDelphiObject(const Value: TBounds);
  protected
    function Get_Rect(Acontext: Pointer): PPyObject; cdecl;
    function Set_Rect(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    property DelphiObject: TBounds read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiControlSize = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TControlSize;
    procedure SetDelphiObject(const Value: TControlSize);
  protected
    function Get_SizeF(Acontext: Pointer): PPyObject; cdecl;
    function Set_SizeF(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    property DelphiObject: TControlSize read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTimer = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TTimer;
    procedure SetDelphiObject(const Value: TTimer);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TTimer read GetDelphiObject write SetDelphiObject;
  end;

  {Helper functions}
  function WrapPointF(APyDelphiWrapper: TPyDelphiWrapper; const APoint : TPointF) : PPyObject;
  function WrapSizeF(APyDelphiWrapper: TPyDelphiWrapper; const ASize : TSizeF) : PPyObject;
  function WrapRectF(APyDelphiWrapper: TPyDelphiWrapper; const ARect : TRectF) : PPyObject;
  function CheckPointFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TPointF): Boolean;
  function CheckSizeFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TSizeF): Boolean;
  function CheckRectFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TRectF): Boolean;
implementation
uses
  System.Math, System.SysUtils;

{ Register the wrappers, the globals and the constants }
type
  TTypesRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TPyDelphiPointF }
function TPyDelphiPointF.Compare(obj: PPyObject): Integer;
var
  _other : TPyDelphiPointF;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiPointF) then
  begin
    _other := TPyDelphiPointF(PythonToDelphi(obj));
    Result := CompareValue(Value.X, _other.Value.X);
    if Result = 0 then
      Result := CompareValue(Value.Y, _other.Value.Y);
  end
  else
    Result := 1;
end;

constructor TPyDelphiPointF.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  x, y : single;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'ff:Create', @x, @y) <> 0 then
  begin
   FValue.X := x;
   FValue.Y := y;
  end
end;

function TPyDelphiPointF.Get_X(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.X);
end;

function TPyDelphiPointF.Get_Y(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Y);
end;

class procedure TPyDelphiPointF.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('X', @TPyDelphiPointF.Get_X, @TPyDelphiPointF.Set_X,
        'Provides access to the X coordinate of a pointf', nil);
      AddGetSet('Y', @TPyDelphiPointF.Get_Y, @TPyDelphiPointF.Set_Y,
        'Provides access to the Y coordinate of a pointf', nil);
    end;
end;

function TPyDelphiPointF.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(Format('<PointF (%f, %f)>',
    [Value.X, Value.Y]));
end;

class procedure TPyDelphiPointF.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'PointF';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi FMX TPointF type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

function TPyDelphiPointF.Set_X(AValue: PPyObject; AContext: Pointer): integer;
var
  x: double;
begin
  if CheckFloatAttribute(AValue, 'X', x) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.X := x;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiPointF.Set_Y(AValue: PPyObject; AContext: Pointer): integer;
var
  y: double;
begin
  if CheckFloatAttribute(AValue, 'Y', y) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Y := y;
      Result := 0;
    end
    else
      Result := -1;
end;

{ TTypesRegistration }
procedure TTypesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TTypesRegistration.Name: string;
begin
  Result := 'FMX Types';
end;

procedure TTypesRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterHelperType(TPyDelphiPointF);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiSizeF);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiRectF);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFmxObject);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPosition);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPopupMenu);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBounds);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiControlSize);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTimer);
end;

{ Helper functions }
function WrapPointF(APyDelphiWrapper : TPyDelphiWrapper; const APoint : TPointF) : PPyObject;
var
  _type : TPythonType;
begin
  _type := APyDelphiWrapper.GetHelperType('PointFType');
  Result := _type.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiPointF).Value := APoint;
end;

function WrapSizeF(APyDelphiWrapper: TPyDelphiWrapper; const ASize : TSizeF) : PPyObject;
var
  LType : TPythonType;
begin
  LType := APyDelphiWrapper.GetHelperType('SizeFType');
  Result := LType.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiSizeF).Value := ASize;
end;

function WrapRectF(APyDelphiWrapper: TPyDelphiWrapper; const ARect : TRectF) : PPyObject;
var
  LType : TPythonType;
begin
  LType := APyDelphiWrapper.GetHelperType('RectFType');
  Result := LType.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiRectF).Value := ARect;
end;

function CheckPointFAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : TPointF) : Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiPointF) then
    begin
      AValue := TPyDelphiPointF(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only PointF objects', [AAttributeName]))));
    end;
  end;
end;

function CheckSizeFAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : TSizeF) : Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiSizeF) then
    begin
      AValue := TPyDelphiSizeF(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only SizeF objects', [AAttributeName]))));
    end;
  end;
end;

function CheckRectFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TRectF): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiRectF) then
    begin
      AValue := TPyDelphiRectF(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only RectF objects', [AAttributeName]))));
    end;
  end;
end;

{ TPyDelphiFmxObject }
class function TPyDelphiFmxObject.DelphiObjectClass: TClass;
begin
  Result := TFmxObject;
end;

function TPyDelphiFmxObject.GetDelphiObject: TFmxObject;
begin
  Result := TFmxObject(inherited DelphiObject);
end;

function TPyDelphiFmxObject.Get_Parent(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Parent);
end;

class procedure TPyDelphiFmxObject.RegisterGetSets(PythonType: TPythonType);
begin
  PythonType.AddGetSet('Parent', @TPyDelphiFmxObject.Get_Parent, @TPyDelphiFmxObject.Set_Parent,
        'Returns/Sets the Control Visibility', nil);
end;

class procedure TPyDelphiFmxObject.RegisterMethods(PythonType: TPythonType);
begin
end;

procedure TPyDelphiFmxObject.SetDelphiObject(const Value: TFmxObject);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiFmxObject.Set_Parent(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LObject : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Parent', TFmxObject, LObject) then
  begin
    Self.DelphiObject.Parent := TFmxObject(LObject);
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiPosition }
constructor TPyDelphiPosition.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  LPPosition: PPyObject;
  LPointF: TPointF;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'O:Create', @LPPosition) <> 0 then
    if CheckPointFAttribute(LPPosition, 'PointF', LPointF) then
      DelphiObject := TPosition.Create(LPointF);
end;

class function TPyDelphiPosition.DelphiObjectClass: TClass;
begin
  Result := TPosition;
end;

function TPyDelphiPosition.GetDelphiObject: TPosition;
begin
  Result := TPosition(inherited DelphiObject);
end;

function TPyDelphiPosition.Get_Point(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapPointF(PyDelphiWrapper, DelphiObject.Point);
end;

function TPyDelphiPosition.Get_X(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(DelphiObject.X);
end;

function TPyDelphiPosition.Get_Y(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(DelphiObject.Y);
end;

class procedure TPyDelphiPosition.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('X', @TPyDelphiPosition.Get_X, @TPyDelphiPosition.Set_X,
      'Provides access to the X coordinate of a control inside its parent', nil);
    AddGetSet('Y', @TPyDelphiPosition.Get_Y, @TPyDelphiPosition.Set_Y,
      'Provides access to the Y coordinate of a control inside its parent', nil);
    AddGetSet('Point', @TPyDelphiPosition.Get_Point, @TPyDelphiPosition.Set_Point,
      'Provides access to the position of a control inside its parent', nil);
  end;
end;

class procedure TPyDelphiPosition.RegisterMethods(PythonType: TPythonType);
begin
end;

procedure TPyDelphiPosition.SetDelphiObject(const Value: TPosition);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiPosition.Set_Point(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: TPointF;
begin
  Adjust(@Self);
  if CheckPointFAttribute(AValue, 'Point', LValue) then
  begin
    DelphiObject.Point := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiPosition.Set_X(AValue: PPyObject; AContext: Pointer): integer;
var
  x: double;
begin
  if CheckFloatAttribute(AValue, 'X', x) then
    with GetPythonEngine do begin
      Adjust(@Self);
      DelphiObject.X := x;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiPosition.Set_Y(AValue: PPyObject; AContext: Pointer): integer;
var
  y: double;
begin
  if CheckFloatAttribute(AValue, 'Y', y) then
    with GetPythonEngine do begin
      Adjust(@Self);
      DelphiObject.Y := y;
      Result := 0;
    end
    else
      Result := -1;
end;

{ TPyDelphiSizeF }
function TPyDelphiSizeF.Compare(obj: PPyObject): Integer;
var
  LOther : TPyDelphiSizeF;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiPointF) then
  begin
    LOther := TPyDelphiSizeF(PythonToDelphi(obj));
    Result := CompareValue(Value.Width, LOther.Value.Width);
    if Result = 0 then
      Result := CompareValue(Value.Height, LOther.Value.Height);
  end
  else
    Result := 1;
end;

constructor TPyDelphiSizeF.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  LWidth, LHeight : single;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'ff:Create', @LWidth, @LHeight) <> 0 then
  begin
   FValue.Width := LWidth;
   FValue.Height := LHeight;
  end
end;

function TPyDelphiSizeF.Get_Height(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Height);
end;

function TPyDelphiSizeF.Get_Width(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Width);
end;

class procedure TPyDelphiSizeF.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Width', @TPyDelphiSizeF.Get_Width, @TPyDelphiSizeF.Set_Width,
        'Provides access to the width of a sizef', nil);
      AddGetSet('Height', @TPyDelphiSizeF.Get_Height, @TPyDelphiSizeF.Set_Height,
        'Provides access to the height of a sizef', nil);
    end;
end;

function TPyDelphiSizeF.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(Format('<SizeF (%f, %f)>',
    [Value.Width, Value.Height]));
end;

class procedure TPyDelphiSizeF.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'SizeF';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi FMX TSizeF type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

function TPyDelphiSizeF.Set_Height(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: double;
begin
  if CheckFloatAttribute(AValue, 'Height', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Height := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiSizeF.Set_Width(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: double;
begin
  if CheckFloatAttribute(AValue, 'Width', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Width := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

{ TPyDelphiCustomPopupMenu }
class function TPyDelphiCustomPopupMenu.DelphiObjectClass: TClass;
begin
  Result := TCustomPopupMenu;
end;

function TPyDelphiCustomPopupMenu.GetDelphiObject: TCustomPopupMenu;
begin
  Result := TCustomPopupMenu(inherited DelphiObject)
end;

procedure TPyDelphiCustomPopupMenu.SetDelphiObject(
  const Value: TCustomPopupMenu);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBounds }
constructor TPyDelphiBounds.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  LPBounds: PPyObject;
  LRectF: TRectF;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'O:Create', @LPBounds) <> 0 then
    if CheckRectFAttribute(LPBounds, 'RectF', LRectF) then
      DelphiObject := TBounds.Create(LRectF);
end;

class function TPyDelphiBounds.DelphiObjectClass: TClass;
begin
  Result := TBounds
end;

function TPyDelphiBounds.GetDelphiObject: TBounds;
begin
  Result := TBounds(inherited DelphiObject);
end;

function TPyDelphiBounds.Get_Rect(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRectF(PyDelphiWrapper, DelphiObject.Rect);
end;

class procedure TPyDelphiBounds.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Rect', @TPyDelphiBounds.Get_Rect, @TPyDelphiBounds.Set_Rect,
        'Provides access to the rect of a control', nil);
  end;
end;

procedure TPyDelphiBounds.SetDelphiObject(const Value: TBounds);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiBounds.Set_Rect(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: TRectF;
begin
  Adjust(@Self);
  if CheckRectFAttribute(AValue, 'Rect', LValue) then
  begin
    DelphiObject.Rect := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiControlSize }
constructor TPyDelphiControlSize.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  LPControlSize: PPyObject;
  LSizeF: TSizeF;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'O:Create', @LPControlSize) <> 0 then
    if CheckSizeFAttribute(LPControlSize, 'SizeF', LSizeF) then
      DelphiObject := TControlSize.Create(LSizeF);
end;

class function TPyDelphiControlSize.DelphiObjectClass: TClass;
begin
  Result := TControlSize;
end;

function TPyDelphiControlSize.GetDelphiObject: TControlSize;
begin
  Result := TControlSize(inherited DelphiObject);
end;

function TPyDelphiControlSize.Get_SizeF(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapSizeF(PyDelphiWrapper, DelphiObject.Size);
end;

class procedure TPyDelphiControlSize.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('Size', @TPyDelphiControlSize.Get_SizeF, @TPyDelphiControlSize.Set_SizeF,
        'Provides access to the size of a control', nil);
  end;
end;

procedure TPyDelphiControlSize.SetDelphiObject(const Value: TControlSize);
begin
  inherited DelphiObject := Value;
end;
function TPyDelphiControlSize.Set_SizeF(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: TSizeF;
begin
  Adjust(@Self);
  if CheckSizeFAttribute(AValue, 'Size', LValue) then
  begin
    DelphiObject.Size := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiRectF }
function TPyDelphiRectF.Compare(obj: PPyObject): Integer;
var
  LOther : TPyDelphiRectF;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiPointF) then
  begin
    LOther := TPyDelphiRectF(PythonToDelphi(obj));
    Result := CompareValue(Value.Left, LOther.Value.Left)
          and CompareValue(Value.Top, LOther.Value.Top)
          and CompareValue(Value.Right, LOther.Value.Right)
          and CompareValue(Value.Bottom, LOther.Value.Bottom);
  end
  else
    Result := 1;
end;

constructor TPyDelphiRectF.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  LLeft, LTop, LRight, LBottom : single;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'ffff:Create', @LLeft, @LTop, @LRight, @LBottom) <> 0 then
  begin
   FValue.Left := LLeft;
   FValue.Top := LTop;
   FValue.Right := LRight;
   FValue.Bottom := LBottom;
  end
end;

function TPyDelphiRectF.Get_Bottom(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Bottom);
end;

function TPyDelphiRectF.Get_Left(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Left);
end;

function TPyDelphiRectF.Get_Right(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Right);
end;

function TPyDelphiRectF.Get_Top(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyFloat_FromDouble(Value.Top);
end;

class procedure TPyDelphiRectF.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Left', @TPyDelphiRectF.Get_Left, @TPyDelphiRectF.Set_Left,
        'Provides access to the left of a rectf', nil);
      AddGetSet('Top', @TPyDelphiRectF.Get_Top, @TPyDelphiRectF.Set_Top,
        'Provides access to the top of a rectf', nil);
      AddGetSet('Right', @TPyDelphiRectF.Get_Right, @TPyDelphiRectF.Set_Right,
        'Provides access to the right of a rectf', nil);
      AddGetSet('Bottom', @TPyDelphiRectF.Get_Bottom, @TPyDelphiRectF.Set_Bottom,
        'Provides access to the bottom of a rectf', nil);
    end;
end;

function TPyDelphiRectF.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(Format('<RectF (%f, %f, %f, %f)>',
    [Value.Left, Value.Top, Value.Right, Value.Bottom]));
end;

class procedure TPyDelphiRectF.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'RectF';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi FMX TRectF type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

function TPyDelphiRectF.Set_Bottom(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: double;
begin
  if CheckFloatAttribute(AValue, 'Bottom', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Bottom := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiRectF.Set_Left(AValue: PPyObject; AContext: Pointer): integer;
var
  LValue: double;
begin
  if CheckFloatAttribute(AValue, 'Left', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Left := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiRectF.Set_Right(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: double;
begin
  if CheckFloatAttribute(AValue, 'Right', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Right := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiRectF.Set_Top(AValue: PPyObject; AContext: Pointer): integer;
var
  LValue: double;
begin
  if CheckFloatAttribute(AValue, 'Top', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Top := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

{ TPyDelphiTimer }

class function TPyDelphiTimer.DelphiObjectClass: TClass;
begin
  Result := TTimer;
end;

function TPyDelphiTimer.GetDelphiObject: TTimer;
begin
  Result := TTimer(inherited DelphiObject);
end;

procedure TPyDelphiTimer.SetDelphiObject(const Value: TTimer);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TTypesRegistration.Create);
end.
