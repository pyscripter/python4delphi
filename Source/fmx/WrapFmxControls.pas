{$I ..\Definition.Inc}

unit WrapFmxControls;

interface

uses
  Classes, SysUtils, TypInfo, Types,
  FMX.Types, FMX.Controls, FMX.Controls.Presentation,
  PythonEngine, WrapDelphi, WrapDelphiClasses, WrapFmxTypes, WrapFmxActnList;

type
  {
     PyObject wrapping FMX.Controls.TControl
     Exposes methods BringToFront, SendToBack, LocalToAbsolute, AbsoluteToLocal,
      SetBounds and Repaint
     Exposes properties Parent and Visible
  }

  { TKeyEvent wrapper }
  TKeyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
  public
    constructor Create(APyDelphiWrapper: TPyDelphiWrapper; AComponent: TObject;
      APropertyInfo: PPropInfo; ACallable: PPyObject); override;
    class function GetTypeInfo: PTypeInfo; override;
  end;

  TPyDelphiControl = class (TPyDelphiFmxObject)
  private
    function  GetDelphiObject: TControl;
    procedure SetDelphiObject(const Value: TControl);
  protected
    // Exposed Methods
    function LocalToAbsolute_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function AbsoluteToLocal_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BringToFront_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SendToBack_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SetBounds_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Repaint_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function CanFocus_Wrapper(args: PPyObject): PPyObject; cdecl;
    function SetFocus_Wrapper(args: PPyObject): PPyObject; cdecl;
    function ResetFocus_Wrapper(args: PPyObject): PPyObject; cdecl;
    function PrepareForPaint_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_Visible(AContext: Pointer): PPyObject; cdecl;
    function Get_ControlsCount( AContext : Pointer) : PPyObject; cdecl;
    function Get_Controls(AContext: Pointer): PPyObject; cdecl;
    function Get_IsFocused( AContext : Pointer) : PPyObject; cdecl;
    function Get_ParentControl( AContext : Pointer) : PPyObject; cdecl;
    function Get_Position(AContext: Pointer): PPyObject; cdecl;
    // Property Setters
    function Set_Visible(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_Position(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TControl read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the child controls of a FMX.Controls.TControl.Controls collection.
  }
  TControlsAccess = class(TContainerAccess)
  private
    function GetContainer: TControl;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;
    class function Name : string; override;

    property Container : TControl read GetContainer;
  end;

  TPyDelphiStyledControl = class(TPyDelphiControl)
  private
    function GetDelphiObject: TStyledControl;
    procedure SetDelphiObject(const Value: TStyledControl);
  protected
    // Exposed Methods
    function ApplyStyleLookup_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function NeedStyleLookup_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Inflate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_DefaultStyleLookupName(AContext: Pointer): PPyObject; cdecl;
    function Get_StyleLookup(AContext: Pointer): PPyObject; cdecl;
    function Get_AutoTranslate(AContext: Pointer): PPyObject; cdecl;
    function Get_AdjustSizeValue(AContext: Pointer): PPyObject; cdecl;
    function Get_AdjustType(AContext: Pointer): PPyObject; cdecl;
    function Get_StyleState(AContext: Pointer): PPyObject; cdecl;
    // Property Setters
    function Set_StyleLookup(AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_AutoTranslate(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    // Properties
    property DelphiObject: TStyledControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTextControl = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TTextControl;
    procedure SetDelphiObject(const Value: TTextControl);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTextControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStyleBook = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TStyleBook;
    procedure SetDelphiObject(const Value: TStyleBook);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TStyleBook read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPopup = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TPopup;
    procedure SetDelphiObject(const Value: TPopup);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TPopup read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPresentedControl = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TPresentedControl;
    procedure SetDelphiObject(const Value: TPresentedControl);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TPresentedControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomControlAction = class(TPyDelphiCustomAction)
  private
    function GetDelphiObject: TCustomControlAction;
    procedure SetDelphiObject(const Value: TCustomControlAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TCustomControlAction read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

type
{ Register the wrappers, the globals and the constants }
  TControlsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TControlsRegistration }

procedure TControlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TControlsRegistration.Name: string;
begin
  Result := 'Controls';
end;

procedure TControlsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStyledControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTextControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStyleBook);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPopup);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomControlAction);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TKeyEventHandler);
end;

{ TPyDelphiControl }

function TPyDelphiControl.BringToFront_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':BringToFront') <> 0 then begin
      DelphiObject.BringToFront;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.CanFocus_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':CanFocus') <> 0 then begin
      Result := VariantAsPyObject(DelphiObject.CanFocus)
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.LocalToAbsolute_Wrapper(
  args: PPyObject): PPyObject;
var
  p : TPointF;
  pt : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple(args, 'O:LocalToAbsolute', @pt) <> 0 then begin
      if CheckPointFAttribute(pt, 'pointf', p) then
        Result := WrapPointF(PyDelphiWrapper, DelphiObject.LocalToAbsolute(p))
      else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.PrepareForPaint_Wrapper(args: PPyObject): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':PrepareForPaint') <> 0 then begin
      DelphiObject.PrepareForPaint;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

class function TPyDelphiControl.DelphiObjectClass: TClass;
begin
  Result := TControl;
end;

function TPyDelphiControl.GetDelphiObject: TControl;
begin
  Result := TControl(inherited DelphiObject);
end;

function TPyDelphiControl.Get_ControlsCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.ControlsCount);
end;

function TPyDelphiControl.Get_IsFocused(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.IsFocused);
end;

function TPyDelphiControl.Get_Controls(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TControlsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiControl.Get_ParentControl(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.ParentControl);
end;

function TPyDelphiControl.Get_Position(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Position);
end;

function TPyDelphiControl.Get_Visible(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(Self.DelphiObject.Visible);
end;

class procedure TPyDelphiControl.RegisterGetSets(PythonType: TPythonType);
begin
  PythonType.AddGetSet('Visible', @TPyDelphiControl.Get_Visible, @TPyDelphiControl.Set_Visible,
        'Returns/Sets the Control Visibility', nil);
  PythonType.AddGetSet('ControlsCount', @TPyDelphiControl.Get_ControlsCount, nil,
        'Returns the count of contained controls', nil);
  PythonType.AddGetSet('Controls', @TPyDelphiControl.Get_Controls, nil,
        'Returns an iterator over contained controls', nil);
  PythonType.AddGetSet('IsFocused', @TPyDelphiControl.Get_IsFocused, nil,
        'Determines whether the control has input focus.', nil);
  PythonType.AddGetSet('Position', @TPyDelphiControl.Get_Position, @TPyDelphiControl.Set_Position,
        'Returns an access to the position of the control inside its parent', nil);
end;

class procedure TPyDelphiControl.RegisterMethods(PythonType: TPythonType);
begin
  PythonType.AddMethod('BringToFront', @TPyDelphiControl.BringToFront_Wrapper,
    'TControl.BringToFront()'#10 +
    'Puts the control in front of all other controls in its parent control.');
  PythonType.AddMethod('SendToBack', @TPyDelphiControl.SendToBack_Wrapper,
    'TControl.SendToBack()'#10 +
    'Puts a windowed control behind all other windowed controls, or puts a non-windowed control behind all other non-windowed controls.');
  PythonType.AddMethod('SetBounds', @TPyDelphiControl.SetBounds_Wrapper,
    'TControl.SetBounds(Left, Top, Width, Height)'#10 +
    'Sets the Left, Top, Width, and Height properties all at once.');
  PythonType.AddMethod('Repaint', @TPyDelphiControl.Repaint_Wrapper,
    'TControl.Repaint()'#10 +
    'Forces the control to repaint its image on the screen. ');
  PythonType.AddMethod('LocalToAbsolute', @TPyDelphiControl.LocalToAbsolute_Wrapper,
    'TControl.LocalToAbsolute()'#10 +
    'Translates a given point from client area coordinates to global screen coordinates.');
  PythonType.AddMethod('AbsoluteToLocal', @TPyDelphiControl.AbsoluteToLocal_Wrapper,
    'TControl.AbsoluteToLocal()'#10 +
    'Converts the screen coordinates of a specified point on the screen to client coordinates.');
  PythonType.AddMethod('CanFocus', @TPyDelphiControl.CanFocus_Wrapper,
    'TControl.CanFocus()'#10 +
    'Indicates whether a control can receive focus. ');
  PythonType.AddMethod('SetFocus', @TPyDelphiControl.SetFocus_Wrapper,
    'TControl.SetFocus()'#10 +
    'Gives the input focus to the control.');
  PythonType.AddMethod('ResetFocus', @TPyDelphiControl.ResetFocus_Wrapper,
    'TControl.ResetFocus()'#10 +
    'Removes the focus from a control of from any children of the control.');
  PythonType.AddMethod('PrepareForPaint', @TPyDelphiControl.PrepareForPaint_Wrapper,
    'TControl.PrepareForPaint()'#10 +
    'Prepares the current control for painting.');
end;

function TPyDelphiControl.Repaint_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Repaint') <> 0 then begin
      DelphiObject.Repaint;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.ResetFocus_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ResetFocus') <> 0 then begin
      DelphiObject.ResetFocus();
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.AbsoluteToLocal_Wrapper(
  args: PPyObject): PPyObject;
var
  p : TPointF;
  pt : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'O:AbsoluteToLocal', @pt) <> 0 then begin
    if CheckPointFAttribute(pt, 'pointf', p) then
      Result := WrapPointF(PyDelphiWrapper, DelphiObject.AbsoluteToLocal(p))
    else
      Result := nil;
  end else
    Result := nil;
end;

function TPyDelphiControl.SendToBack_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':SendToBack') <> 0 then begin
      DelphiObject.SendToBack;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.SetBounds_Wrapper(args: PPyObject): PPyObject;
var
  _left : Integer;
  _top : Integer;
  _width : Integer;
  _height : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'iiii:SetBounds',@_left, @_top, @_width, @_height ) <> 0 then begin
      DelphiObject.SetBounds(_left, _top, _width, _height);
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

procedure TPyDelphiControl.SetDelphiObject(const Value: TControl);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiControl.SetFocus_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':SetFocus') <> 0 then begin
      DelphiObject.SetFocus;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiControl.Set_Position(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Position', TPosition, LValue) then
  begin
    DelphiObject.Position := TPosition(LValue);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiControl.Set_Visible(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'Visible', LValue) then
  begin
    DelphiObject.Visible := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TControlsAccess }

class function TControlsAccess.ExpectedContainerClass: TClass;
begin
  Result := TControl;
end;

function TControlsAccess.GetContainer: TControl;
begin
  Result := TControl(inherited Container);
end;

function TControlsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.Controls[AIndex]);
end;

function TControlsAccess.GetSize: Integer;
begin
  Result := Container.ControlsCount;
end;

function TControlsAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  S : string;
  _obj : TPyObject;
  _value : TObject;
  _ctrl : TControl;
begin
  Result := -1;
  with GetPythonEngine do begin
    if PyUnicode_Check(AValue) then begin
      S := PyUnicodeAsString(AValue);
      for i := 0 to Container.ControlsCount - 1 do
        if SameText(Container.Controls[i].Name, S) then begin
          Result := i;
          Break;
        end;
    end else if IsDelphiObject(AValue) then begin
      _obj := PythonToDelphi(AValue);
      if _obj is TPyDelphiObject then begin
        _value := TPyDelphiObject(_obj).DelphiObject;
        if _value is TControl then begin
          _ctrl := TControl(_value);
          for i := 0 to Container.ControlsCount-1 do
            if Container.Controls[i] = _ctrl then begin
              Result := i;
              Break;
            end;
        end;
      end;
    end;
  end;
end;

class function TControlsAccess.Name: string;
begin
  Result := 'Controls';
end;

class function TControlsAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TPyDelphiStyledControl }

function TPyDelphiStyledControl.ApplyStyleLookup_Wrapper(
  args: PPyObject): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ApplyStyleLookup') <> 0 then begin
      DelphiObject.ApplyStyleLookup;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

class function TPyDelphiStyledControl.DelphiObjectClass: TClass;
begin
  Result := TStyledControl;
end;

function TPyDelphiStyledControl.GetDelphiObject: TStyledControl;
begin
  Result := TStyledControl(inherited DelphiObject);
end;

function TPyDelphiStyledControl.Get_AdjustSizeValue(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapSizeF(PyDelphiWrapper, DelphiObject.AdjustSizeValue);
end;

function TPyDelphiStyledControl.Get_AdjustType(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(Ord(DelphiObject.AdjustType));
end;

function TPyDelphiStyledControl.Get_AutoTranslate(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(Self.DelphiObject.AutoTranslate);
end;

function TPyDelphiStyledControl.Get_DefaultStyleLookupName(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(DelphiObject.DefaultStyleLookupName);
end;

function TPyDelphiStyledControl.Get_StyleLookup(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(DelphiObject.StyleLookup);
end;

function TPyDelphiStyledControl.Get_StyleState(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(Ord(DelphiObject.StyleState));
end;

function TPyDelphiStyledControl.Inflate_Wrapper(args: PPyObject): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Inflate') <> 0 then begin
      DelphiObject.Inflate;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiStyledControl.NeedStyleLookup_Wrapper(
  args: PPyObject): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':NeedStyleLookup') <> 0 then begin
      DelphiObject.NeedStyleLookup;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

class procedure TPyDelphiStyledControl.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('DefaultStyleLookupName', @TPyDelphiStyledControl.Get_StyleLookup, nil,
      'Returns a string with the name of the default style of this control', nil);
    AddGetSet('StyleLookup', @TPyDelphiStyledControl.Get_StyleLookup, @TPyDelphiStyledControl.Set_StyleLookup,
      'Specifies the name of the resource object to which the current TStyledControl is linked', nil);
    AddGetSet('AutoTranslate', @TPyDelphiStyledControl.Get_AutoTranslate, @TPyDelphiStyledControl.Set_AutoTranslate,
      'Specifies whether the control''s text should be translated', nil);
    AddGetSet('AdjustSizeValue', @TPyDelphiStyledControl.Get_AdjustSizeValue, nil,
      'Updates the width and height of this control according to its current style', nil);
    AddGetSet('AdjustType', @TPyDelphiStyledControl.Get_AdjustType, nil,
      'Determines if and how the width and height of this control should be '
      + 'modified to take the fixed space dictated by the style of this control', nil);
    AddGetSet('StyleState', @TPyDelphiStyledControl.Get_StyleState, nil,
      'This property allows you to define the current state of style', nil);
  end;
end;

class procedure TPyDelphiStyledControl.RegisterMethods(PythonType: TPythonType);
begin
  PythonType.AddMethod('ApplyStyleLookup', @TPyDelphiStyledControl.ApplyStyleLookup_Wrapper,
    'TStyledControl.ApplyStyleLookup()'#10 +
    'Gets and applies the style of a TStyledControl.');
  PythonType.AddMethod('NeedStyleLookup', @TPyDelphiStyledControl.NeedStyleLookup_Wrapper,
    'TStyledControl.NeedStyleLookup()'#10 +
    'Call this procedure to indicate that this control requires to get and apply its style lookup.');
  PythonType.AddMethod('Inflate', @TPyDelphiStyledControl.Inflate_Wrapper,
    'TStyledControl.Inflate()'#10 +
    'Call this procedure to get and apply its style lookup.');
end;

procedure TPyDelphiStyledControl.SetDelphiObject(const Value: TStyledControl);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiStyledControl.Set_AutoTranslate(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'AutoTranslate', LValue) then
  begin
    DelphiObject.AutoTranslate := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiStyledControl.Set_StyleLookup(AValue: PPyObject;
  AContext: Pointer): integer;
var
  LValue: string;
begin
  if CheckStrAttribute(AValue, 'StyleLookup', LValue) then
    with GetPythonEngine do begin
      Adjust(@Self);
      DelphiObject.StyleLookup := LValue;
      Result := 0;
    end
    else
      Result := -1;
end;

{ TPyDelphiTextControl }

class function TPyDelphiTextControl.DelphiObjectClass: TClass;
begin
  Result := TTextControl;
end;

function TPyDelphiTextControl.GetDelphiObject: TTextControl;
begin
  Result := TTextControl(inherited DelphiObject);
end;

procedure TPyDelphiTextControl.SetDelphiObject(const Value: TTextControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStyleBook }

class function TPyDelphiStyleBook.DelphiObjectClass: TClass;
begin
  Result := TStyleBook;
end;

function TPyDelphiStyleBook.GetDelphiObject: TStyleBook;
begin
  Result := TStyleBook(inherited DelphiObject);
end;

procedure TPyDelphiStyleBook.SetDelphiObject(const Value: TStyleBook);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPopup }

class function TPyDelphiPopup.DelphiObjectClass: TClass;
begin
  Result := TPopup;
end;

function TPyDelphiPopup.GetDelphiObject: TPopup;
begin
  Result := TPopup(inherited DelphiObject);
end;

procedure TPyDelphiPopup.SetDelphiObject(const Value: TPopup);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedControl }

class function TPyDelphiPresentedControl.DelphiObjectClass: TClass;
begin
  Result := TPresentedControl;
end;

function TPyDelphiPresentedControl.GetDelphiObject: TPresentedControl;
begin
  Result := TPresentedControl(inherited DelphiObject);
end;

procedure TPyDelphiPresentedControl.SetDelphiObject(
  const Value: TPresentedControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomControlAction }

class function TPyDelphiCustomControlAction.DelphiObjectClass: TClass;
begin
  Result := TCustomControlAction;
end;

function TPyDelphiCustomControlAction.GetDelphiObject: TCustomControlAction;
begin
  Result := TCustomControlAction(inherited DelphiObject);
end;

procedure TPyDelphiCustomControlAction.SetDelphiObject(
  const Value: TCustomControlAction);
begin
  inherited DelphiObject := Value;
end;

{ TKeyEventHandler }

constructor TKeyEventHandler.Create(APyDelphiWrapper: TPyDelphiWrapper;
  AComponent: TObject; APropertyInfo: PPropInfo; ACallable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TKeyEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(AComponent, APropertyInfo, LMethod);
end;

class function TKeyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TKeyEvent);
end;

procedure TKeyEventHandler.DoEvent(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  LPyObject: PPyObject;
  LPyTuple: PPyObject;
  LPyResult: PPyObject;
  LPyKey: PPyObject;
  LVarKeyParam: TPyDelphiVarParameter;
  LPyKeyChar: PPyObject;
  LVarKeyCharParam: TPyDelphiVarParameter;
  LKeyCharStr: string;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      //var parameters
      LPyKey := CreateVarParam(PyDelphiWrapper, Key);
      LVarKeyParam := PythonToDelphi(LPyKey) as TPyDelphiVarParameter;
      LPyKeyChar := CreateVarParam(PyDelphiWrapper, KeyChar);
      LVarKeyCharParam := PythonToDelphi(LPyKeyChar) as TPyDelphiVarParameter;

      LPyTuple := PyTuple_New(4);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyKey);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LPyKeyChar);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 3, ShiftToPython(Shift));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
          if PyLong_Check(LVarKeyParam.Value) then
            Key := PyLong_AsLong(LVarKeyParam.Value);

          if (LVarKeyCharParam.Value = Py_None) then
            KeyChar := #0
          else if PyUnicode_Check(LVarKeyCharParam.Value) then
          begin
            LKeyCharStr := PyUnicodeAsString(LVarKeyCharParam.Value);
            if Length(LKeyCharStr) > 0 then
              KeyChar := LKeyCharStr[1];
          end;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

initialization
  RegisteredUnits.Add(TControlsRegistration.Create);

end.
