{$I ..\Definition.Inc}
unit WrapVclWinXCtrls;

interface

uses
  Vcl.WinXCtrls,
  {$IF DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}
  Vcl.NumberBox,
  {$IFEND DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}
  WrapVclControls, WrapVclStdCtrls;

type
  TPyDelphiCustomActivityIndicator = class (TPyDelphiCustomControl)
  private
    function GetDelphiObject: TCustomActivityIndicator;
    procedure SetDelphiObject(const Value: TCustomActivityIndicator);
  public
    class function DelphiObjectClass : TClass; override;
    property DelphiObject: TCustomActivityIndicator read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiActivityIndicator = class (TPyDelphiCustomActivityIndicator)
  private
    function GetDelphiObject: TActivityIndicator;
    procedure SetDelphiObject(const Value: TActivityIndicator);
  public
    class function DelphiObjectClass : TClass; override;
    property DelphiObject: TActivityIndicator read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomCustomToggleSwitch = class (TPyDelphiCustomControl)
  private
    function GetDelphiObject: TCustomToggleSwitch;
    procedure SetDelphiObject(const Value: TCustomToggleSwitch);
  public
    class function DelphiObjectClass : TClass; override;
    property DelphiObject: TCustomToggleSwitch read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiToggleSwitch = class (TPyDelphiCustomCustomToggleSwitch)
  private
    function GetDelphiObject: TToggleSwitch;
    procedure SetDelphiObject(const Value: TToggleSwitch);
  public
    class function DelphiObjectClass : TClass; override;
    property DelphiObject: TToggleSwitch read GetDelphiObject write SetDelphiObject;
  end;

  {$IF DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}
  TPyDelphiCustomNumberBox = class(TPyDelphiCustomEdit)
  private
    function GetDelphiObject: TCustomNumberBox;
    procedure SetDelphiObject(const Value: TCustomNumberBox);
  public
    class function DelphiObjectClass : TClass; override;
    property DelphiObject: TCustomNumberBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiNumberBox = class(TPyDelphiCustomNumberBox)
  private
    function GetDelphiObject: TNumberBox;
    procedure SetDelphiObject(const Value: TNumberBox);
  public
    class function DelphiObjectClass : TClass; override;
    property DelphiObject: TNumberBox read GetDelphiObject write SetDelphiObject;
  end;
  {$IFEND DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TWinXCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TComCtrlsRegistration }

procedure TWinXCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TWinXCtrlsRegistration.Name: string;
begin
  Result := 'WinXCtrls';
end;

procedure TWinXCtrlsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomActivityIndicator);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiActivityIndicator);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomCustomToggleSwitch);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiToggleSwitch);
  {$IF DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomNumberBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiNumberBox);
  {$IFEND DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}
end;

{ TPyDelphiCustomActivityIndicator }

class function TPyDelphiCustomActivityIndicator.DelphiObjectClass: TClass;
begin
  Result := TCustomActivityIndicator;
end;

function TPyDelphiCustomActivityIndicator.GetDelphiObject: TCustomActivityIndicator;
begin
  Result := TCustomActivityIndicator(inherited DelphiObject);
end;

procedure TPyDelphiCustomActivityIndicator.SetDelphiObject(
  const Value: TCustomActivityIndicator);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiActivityIndicator }

class function TPyDelphiActivityIndicator.DelphiObjectClass: TClass;
begin
  Result := TActivityIndicator;
end;

function TPyDelphiActivityIndicator.GetDelphiObject: TActivityIndicator;
begin
  Result := TActivityIndicator(inherited DelphiObject);
end;

procedure TPyDelphiActivityIndicator.SetDelphiObject(
  const Value: TActivityIndicator);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomCustomToggleSwitch }

class function TPyDelphiCustomCustomToggleSwitch.DelphiObjectClass: TClass;
begin
  Result := TCustomToggleSwitch;
end;

function TPyDelphiCustomCustomToggleSwitch.GetDelphiObject: TCustomToggleSwitch;
begin
  Result := TCustomToggleSwitch(inherited DelphiObject);
end;

procedure TPyDelphiCustomCustomToggleSwitch.SetDelphiObject(
  const Value: TCustomToggleSwitch);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiToggleSwitch }

class function TPyDelphiToggleSwitch.DelphiObjectClass: TClass;
begin
  Result := TToggleSwitch;
end;

function TPyDelphiToggleSwitch.GetDelphiObject: TToggleSwitch;
begin
  Result := TToggleSwitch(inherited DelphiObject);
end;

procedure TPyDelphiToggleSwitch.SetDelphiObject(const Value: TToggleSwitch);
begin
  inherited DelphiObject := Value;
end;

{$IF DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}

{ TPyDelphiCustomNumberBox }

class function TPyDelphiCustomNumberBox.DelphiObjectClass: TClass;
begin
  Result := TCustomNumberBox;
end;

function TPyDelphiCustomNumberBox.GetDelphiObject: TCustomNumberBox;
begin
  Result := TCustomNumberBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomNumberBox.SetDelphiObject(
  const Value: TCustomNumberBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiNumberBox }

class function TPyDelphiNumberBox.DelphiObjectClass: TClass;
begin
  Result := TNumberBox;
end;

function TPyDelphiNumberBox.GetDelphiObject: TNumberBox;
begin
  Result := TNumberBox(inherited DelphiObject);
end;

procedure TPyDelphiNumberBox.SetDelphiObject(const Value: TNumberBox);
begin
  inherited DelphiObject := Value;
end;

{$IFEND DEFINED(DELPHI11_OR_HIGHER) or DEFINED(DELPHI10_4_2)}

initialization
  RegisteredUnits.Add(TWinXCtrlsRegistration.Create());

end.
