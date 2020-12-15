{$I Definition.Inc}

unit WrapFmxTypes;

interface

uses
  System.Types, PythonEngine, WrapDelphi;

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

  {Helper functions}
  function WrapPointF(APyDelphiWrapper: TPyDelphiWrapper; const APoint : TPointF) : PPyObject;
  function CheckPointFAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : TPointF) : Boolean;

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

constructor TPyDelphiPointF.CreateWith(APythonType: TPythonType;
  args: PPyObject);
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
  Result := GetPythonEngine.VariantAsPyObject(Value.X);
end;

function TPyDelphiPointF.Get_Y(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(Value.Y);
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
  PythonType.Name := string(PythonType.TypeName) + 'Type';
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
end;

{ Helper functions }
function WrapPointF(APyDelphiWrapper : TPyDelphiWrapper; const APoint : TPointF) : PPyObject;
var
  _type : TPythonType;
begin
  _type := APyDelphiWrapper.GetHelperType('PointTypeF');
  Result := _type.CreateInstance;
  (PythonToDelphi(Result) as TPyDelphiPointF).Value := APoint;
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

initialization
  RegisteredUnits.Add(TTypesRegistration.Create);

end.
