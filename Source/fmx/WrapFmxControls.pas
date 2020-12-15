{$I Definition.Inc}

unit WrapFmxControls;

interface

uses
  Classes, SysUtils, TypInfo, Types,
  Fmx.Controls,
  PythonEngine, WrapDelphi, WrapDelphiClasses;

type
  {
     PyObject wrapping FMX.Controls.TControl
     Exposes methods BringToFront, SendToBack, LocalToAbsolute, AbsoluteToLocal,
      SetBounds and Repaint
     Exposes properties Parent and Visible
  }
  TPyDelphiControl = class (TPyDelphiComponent)
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
    // Property Getters
    function Get_Parent( AContext : Pointer) : PPyObject; cdecl;
    function Get_Visible(AContext: Pointer): PPyObject; cdecl;
    // Property Setters
    function Set_Parent( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Visible(AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TControl read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapFmxTypes, FMX.Types;

type
{ Register the wrappers, the globals and the constants }
  TControlsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
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

class function TPyDelphiControl.DelphiObjectClass: TClass;
begin
  Result := TControl;
end;

function TPyDelphiControl.GetDelphiObject: TControl;
begin
  Result := TControl(inherited DelphiObject);
end;

function TPyDelphiControl.Get_Parent(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Parent);
end;

function TPyDelphiControl.Get_Visible(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(Self.DelphiObject.Visible);
end;

class procedure TPyDelphiControl.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddGetSet('Parent', @TPyDelphiControl.Get_Parent, @TPyDelphiControl.Set_Parent,
        'Returns/Sets the Control Visibility', nil);
  PythonType.AddGetSet('Visible', @TPyDelphiControl.Get_Visible, @TPyDelphiControl.Set_Visible,
        'Returns/Sets the Control Visibility', nil);
end;

class procedure TPyDelphiControl.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
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

function TPyDelphiControl.Set_Parent(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Parent', TFmxObject, _object) then
  begin
    Self.DelphiObject.Parent := TFmxObject(_object);
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
end;

initialization
  RegisteredUnits.Add(TControlsRegistration.Create);

end.
