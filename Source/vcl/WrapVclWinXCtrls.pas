{$I ..\Definition.Inc}
unit WrapVclWinXCtrls;

interface

uses
  Vcl.WinXCtrls, WrapVclControls;

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

initialization
  RegisteredUnits.Add(TWinXCtrlsRegistration.Create());

end.
