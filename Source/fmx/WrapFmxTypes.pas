(**************************************************************************)
(*  This unit is part of the Python for Delphi (P4D) library              *)
(*  Project home: https://github.com/pyscripter/python4delphi             *)
(*                                                                        *)
(*  Project Maintainer:  PyScripter (pyscripter@gmail.com)                *)
(*  Original Authors:    Dr. Dietmar Budelsky (dbudelsky@web.de)          *)
(*                       Morgan Martinet (https://github.com/mmm-experts) *)
(*  Core developer:      Lucas Belo (lucas.belo@live.com)                 *)
(*  Contributors:        See contributors.md at project home              *)
(*                                                                        *)
(*  LICENCE and Copyright: MIT (see project home)                         *)
(**************************************************************************)

{$I ..\Definition.Inc}

unit WrapFmxTypes;

interface

uses
  System.Types, FMX.Types, PythonEngine, WrapDelphi, WrapDelphiClasses,
  System.TypInfo, System.UITypes, System.Classes;

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
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
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
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
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
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    function Compare(obj: PPyObject): Integer; override;
    function Repr: PPyObject; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure SetupType(PythonType: TPythonType); override;
    property Value: TRectF read FValue write FValue;
  end;

  TPyDelphiTouch = class(TPyObject)
  private
    FValue: TTouch;
    FPyDelphiWrapper: TPyDelphiWrapper;
  protected
    // Exposed Getters
    function Get_Location(AContext: Pointer): PPyObject; cdecl;
    // Exposed Setters
    function Set_Location(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    constructor Create(APythonType: TPythonType); override;
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    function Compare(obj: PPyObject): Integer; override;
    function Repr: PPyObject; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure SetupType(PythonType: TPythonType); override;
    property Value : TTouch read FValue write FValue;
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
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
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
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
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
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
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

  TMouseEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TMouseMoveEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Shift: TShiftState; X, Y: Single);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TMouseWheelEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TKeyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TProcessTickEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; time, deltaTime: Single);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TVirtualKeyboardEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; KeyboardVisible: Boolean; const Bounds : TRect);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TTapEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; const Point: TPointF);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TTouchEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  {Helper functions}

  function WrapPointF(APyDelphiWrapper: TPyDelphiWrapper; const APoint : TPointF) : PPyObject;
  function WrapSizeF(APyDelphiWrapper: TPyDelphiWrapper; const ASize : TSizeF) : PPyObject;
  function WrapRectF(APyDelphiWrapper: TPyDelphiWrapper; const ARect : TRectF) : PPyObject;
  function WrapTouch(APyDelphiWrapper: TPyDelphiWrapper; const ATouch: TTouch): PPyObject;
  function WrapTouches(APyDelphiWrapper: TPyDelphiWrapper; const ATouches: TTouches): PPyObject;
  function CheckPointFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TPointF): Boolean;
  function CheckSizeFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TSizeF): Boolean;
  function CheckRectFAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TRectF): Boolean;
  function CheckTouchAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TTouch): Boolean;

  function TouchActionToPython(ATouchAction: TTouchAction): PPyObject;

implementation

uses
  System.Math, System.SysUtils, System.Rtti,
  WrapDelphiTypes;

{ Register the wrappers, the globals and the constants }

type
  TTypesRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
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
  // Event handlers
  APyDelphiWrapper.EventHandlers.RegisterHandler(TMouseEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TMouseMoveEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TMouseWheelEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TKeyEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TProcessTickEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TVirtualKeyboardEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTapEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTouchEventHandler);
  // Helper types
  APyDelphiWrapper.RegisterHelperType(TPyDelphiPointF);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiSizeF);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiRectF);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiTouch);
  // Classes
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFmxObject);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPosition);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPopupMenu);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBounds);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiControlSize);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTimer);
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

constructor TPyDelphiPointF.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
var
  x, y : single;
begin
  Create(APythonType);
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

function WrapTouch(APyDelphiWrapper: TPyDelphiWrapper; const ATouch: TTouch): PPyObject;
var
  LType : TPythonType;
begin
  LType := APyDelphiWrapper.GetHelperType('TouchType');
  Result := LType.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiTouch).Value := ATouch;
end;

function WrapTouches(APyDelphiWrapper: TPyDelphiWrapper; const ATouches: TTouches): PPyObject;

  procedure Append(AList : PPyObject; const ATouch : TTouch);
  var
    LPyItem : PPyObject;
  begin
    with GetPythonEngine do
    begin
      LPyItem := WrapTouch(APyDelphiWrapper, ATouch);
      PyList_Append(AList, LPyItem);
      Py_XDecRef(LPyItem);
    end;
  end;

var
  LTouch: TTouch;
begin
  Result := GetPythonEngine.PyList_New(0);
  for LTouch in ATouches do
    Append(Result, LTouch);
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

function CheckTouchAttribute(AAttribute: PPyObject; const AAttributeName: string; out AValue: TTouch): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AAttribute) and (PythonToDelphi(AAttribute) is TPyDelphiTouch) then
    begin
      AValue := TPyDelphiTouch(PythonToDelphi(AAttribute)).Value;
      Result := True;
    end
    else
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar(AnsiString(Format('%s receives only Touch objects', [AAttributeName]))));
    end;
  end;
end;

function TouchActionToPython(ATouchAction: TTouchAction): PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(
    TRttiEnumerationType.GetName<TTouchAction>(ATouchAction));
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
constructor TPyDelphiPosition.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
var
  LPPosition: PPyObject;
  LPointF: TPointF;
begin
  Create(APythonType);
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

constructor TPyDelphiSizeF.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
var
  LWidth, LHeight : single;
begin
  Create(PythonType);
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

constructor TPyDelphiBounds.CreateWith(APythonType: TPythonType; args,
 kwds: PPyObject);
var
  LPBounds: PPyObject;
  LRectF: TRectF;
begin
  Create(APythonType);
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

constructor TPyDelphiControlSize.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
var
  LPControlSize: PPyObject;
  LSizeF: TSizeF;
begin
  Create(APythonType);
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

constructor TPyDelphiRectF.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
var
  LLeft, LTop, LRight, LBottom : single;
begin
  Create(APythonType);
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

{ TPyDelphiTouch }

function TPyDelphiTouch.Compare(obj: PPyObject): Integer;
var
  LOther : TPyDelphiTouch;
begin
  if IsDelphiObject(obj) and (PythonToDelphi(obj) is TPyDelphiPointF) then
  begin
    LOther := TPyDelphiTouch(PythonToDelphi(obj));
    Result := CompareValue(Value.Location.X, LOther.Value.Location.X);
    if Result = 0 then
      Result := CompareValue(Value.Location.Y, LOther.Value.Location.Y);
  end
  else
    Result := 1;
end;

constructor TPyDelphiTouch.Create(APythonType: TPythonType);
begin
  inherited;
  if Assigned(PythonType) and (PythonType.Owner is TPyDelphiWrapper) then
    FPyDelphiWrapper := TPyDelphiWrapper(PythonType.Owner);
end;

constructor TPyDelphiTouch.CreateWith(APythonType: TPythonType;
  args, kwds: PPyObject);
var
  LPointF : TPointF;
  LPyPointF : PPyObject;
begin
  Create(APythonType);
  with GetPythonEngine do
    if PyArg_ParseTuple(args, 'O:Create', @LPyPointF) <> 0 then
      if CheckPointFAttribute(LPyPointF, 'pointf', LPointF) then begin
        FValue.Location := LPointF
      end;
end;

function TPyDelphiTouch.Get_Location(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapPointF(FPyDelphiWrapper, Value.Location);
end;

class procedure TPyDelphiTouch.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Location', @TPyDelphiTouch.Get_Location, @TPyDelphiTouch.Set_Location,
        'Provides access to the location of a touch', nil);
    end;
end;

function TPyDelphiTouch.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(Format('<Touch.Location (%f, %f)>',
    [Value.Location.X, Value.Location.Y]));
end;

class procedure TPyDelphiTouch.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'Touch';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.TypeFlags := PythonType.TypeFlags + [tpfBaseType];
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'wrapper for Delphi FMX TTouch type';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

function TPyDelphiTouch.Set_Location(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: TPointF;
begin
  if CheckPointFAttribute(AValue, 'Location', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      FValue.Location := LValue;
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

{ TMouseEventHandler }

constructor TMouseEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TMouseEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TMouseEventHandler.DoEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  LPyObject, LPyTuple, LPyButton, LPyX, LPyY, LPyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyButton := MouseButtonToPython(Button);
      LPyX := PyFloat_FromDouble(X);
      LPyY := PyFloat_FromDouble(Y);
      LPyTuple := PyTuple_New(5);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyButton);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, ShiftToPython(Shift));
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 3, LPyX);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 4, LPyY);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TMouseEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TMouseEvent);
end;

{ TMouseMoveEventHandler }

constructor TMouseMoveEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TMouseMoveEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TMouseMoveEventHandler.DoEvent(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  LPyObject, LPyTuple, LPyX, LPyY, LPyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyX := PyFloat_FromDouble(X);
      LPyY := PyFloat_FromDouble(Y);
      LPyTuple := PyTuple_New(4);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, ShiftToPython(Shift));
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyX);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 3, LPyY);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TMouseMoveEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TMouseMoveEvent);
end;

{ TMouseWheelEventHandler }

constructor TMouseWheelEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TMouseWheelEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TMouseWheelEventHandler.DoEvent(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  LPyObject, LPyTuple, LPyShift, LPyWheelDelta, LPyHandled, LPyResult: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyShift := ShiftToPython(Shift);
      LPyWheelDelta := PyLong_FromLong(WheelDelta);
      LPyHandled := CreateVarParam(PyDelphiWrapper, Handled);
      LVarParam := PythonToDelphi(LPyHandled) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyShift);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyWheelDelta);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 3, LPyHandled);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
          if LVarParam.Value = Py_None then
            Handled := false
          else if PyBool_Check(LVarParam.Value) then
            Handled := Boolean(PyLong_AsLong(LVarParam.Value));
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TMouseWheelEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TMouseWheelEvent);
end;

{ TKeyEventHandler }

constructor TKeyEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TKeyEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TKeyEventHandler.DoEvent(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  LPyObject, LPyTuple, LPyKey, LPyKeyChar, LPyShift, LPyResult: PPyObject;
  LKeyVarParam: TPyDelphiVarParameter;
  LKeyCharVarParam: TPyDelphiVarParameter;
  LKeyChar: string;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyKey := CreateVarParam(PyDelphiWrapper, Key);
      LPyKeyChar := CreateVarParam(PyDelphiWrapper, KeyChar);
      LPyShift := ShiftToPython(Shift);
      LKeyVarParam := PythonToDelphi(LPyKey) as TPyDelphiVarParameter;
      LKeyCharVarParam := PythonToDelphi(LPyKeyChar) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyKey);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyKeyChar);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 3, LPyShift);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
          //Key var arg
          if LKeyVarParam.Value = Py_None then
            Key := 0
          else if PyLong_Check(LKeyVarParam.Value) then
            Key := Word(PyLong_AsLong(LKeyVarParam.Value));

          //KeyChar var arg
          if LKeyCharVarParam.Value = Py_None then
            LKeyChar := #0
          else if PyUnicode_Check(LKeyCharVarParam.Value) then
          begin
            LKeyChar := PyUnicodeAsString(LKeyCharVarParam.Value);
            if Length(LKeyChar) > 0 then
              KeyChar := LKeyChar[1];
          end;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TKeyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TKeyEvent);
end;

{ TProcessTickEventHandler }

constructor TProcessTickEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TProcessTickEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TProcessTickEventHandler.DoEvent(Sender: TObject; time,
  deltaTime: Single);
var
  LPyObject, LPyTuple, LPyTime, LPyDeltaTime, LPyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyTime := PyFloat_FromDouble(time);
      LPyDeltaTime := PyFloat_FromDouble(deltaTime);
      LPyTuple := PyTuple_New(3);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyTime);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyDeltaTime);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TProcessTickEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TProcessTickEvent);
end;

{ TVirtualKeyboardEventHandler }

constructor TVirtualKeyboardEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TVirtualKeyboardEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TVirtualKeyboardEventHandler.DoEvent(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
var
  LPyObject, LPyTuple, LPyKeyboardVisible, LPyBounds, LPyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyKeyboardVisible := PyBool_FromLong(Ord(KeyboardVisible));
      LPyBounds := WrapRect(PyDelphiWrapper, Bounds);
      LPyTuple := PyTuple_New(3);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyKeyboardVisible);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyBounds);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TVirtualKeyboardEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TVirtualKeyboardEvent);
end;

{ TTapEventHandler }

constructor TTapEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TTapEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TTapEventHandler.DoEvent(Sender: TObject; const Point: TPointF);
var
  LPyObject, LPyTuple, LPyPoint, LPyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyPoint := WrapPointF(PyDelphiWrapper, Point);
      LPyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyPoint);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TTapEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTapEvent);
end;

{ TTouchEventHandler }

constructor TTouchEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TTouchEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TTouchEventHandler.DoEvent(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
var
  LPyObject, LPyTuple, LPyTouches, LPyTouchAction, LPyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyTouches := WrapTouches(PyDelphiWrapper, Touches);
      LPyTouchAction := TouchActionToPython(Action);
      LPyTuple := PyTuple_New(3);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyTouches);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyTouchAction);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TTouchEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTouchEvent);
end;

initialization
  RegisteredUnits.Add(TTypesRegistration.Create);

end.
