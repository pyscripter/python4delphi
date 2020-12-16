{$I Definition.Inc}

unit WrapFmxStdCtrls;

interface

uses
  Classes, SysUtils, FMX.StdCtrls,
  PythonEngine, WrapDelphi, WrapDelphiClasses, WrapFmxControls;

type
  TPyDelphiPresentedTextControl = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TPresentedTextControl;
    procedure SetDelphiObject(const Value: TPresentedTextControl);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TPresentedTextControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPanel = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TPanel;
    procedure SetDelphiObject(const Value: TPanel);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TPanel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCalloutPanel = class (TPyDelphiPanel)
  private
    function GetDelphiObject: TCalloutPanel;
    procedure SetDelphiObject(const Value: TCalloutPanel);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCalloutPanel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiLabel = class (TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TLabel;
    procedure SetDelphiObject(const Value: TLabel);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TLabel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomButton = class (TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TCustomButton;
    procedure SetDelphiObject(const Value: TCustomButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiButton = class (TPyDelphiCustomButton)
  private
    function GetDelphiObject: TButton;
    procedure SetDelphiObject(const Value: TButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TButton read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TStdCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TStdCtrlsRegistration }

procedure TStdCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TStdCtrlsRegistration.Name: string;
begin
  Result := 'StdCtrls';
end;

procedure TStdCtrlsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedTextControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPanel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCalloutPanel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLabel);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiButton);
end;

{ TPyDelphiPresentedTextControl }

class function TPyDelphiPresentedTextControl.DelphiObjectClass: TClass;
begin
  Result := TPresentedTextControl;
end;

function TPyDelphiPresentedTextControl.GetDelphiObject: TPresentedTextControl;
begin
  Result := TPresentedTextControl(inherited DelphiObject);
end;

procedure TPyDelphiPresentedTextControl.SetDelphiObject(
  const Value: TPresentedTextControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPanel }

class function TPyDelphiPanel.DelphiObjectClass: TClass;
begin
  Result := TPanel;
end;

function TPyDelphiPanel.GetDelphiObject: TPanel;
begin
  Result := TPanel(inherited DelphiObject);
end;

procedure TPyDelphiPanel.SetDelphiObject(const Value: TPanel);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCalloutPanel }

class function TPyDelphiCalloutPanel.DelphiObjectClass: TClass;
begin
  Result := TCalloutPanel;
end;

function TPyDelphiCalloutPanel.GetDelphiObject: TCalloutPanel;
begin
  Result := TCalloutPanel(inherited DelphiObject);
end;

procedure TPyDelphiCalloutPanel.SetDelphiObject(const Value: TCalloutPanel);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLabel }

class function TPyDelphiLabel.DelphiObjectClass: TClass;
begin
  Result := TLabel;
end;

function TPyDelphiLabel.GetDelphiObject: TLabel;
begin
  Result := TLabel(inherited DelphiObject);
end;

procedure TPyDelphiLabel.SetDelphiObject(const Value: TLabel);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomButton }

class function TPyDelphiCustomButton.DelphiObjectClass: TClass;
begin
  Result := TCustomButton;
end;

function TPyDelphiCustomButton.GetDelphiObject: TCustomButton;
begin
  Result := TCustomButton(inherited DelphiObject);
end;

procedure TPyDelphiCustomButton.SetDelphiObject(const Value: TCustomButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiButton }

class function TPyDelphiButton.DelphiObjectClass: TClass;
begin
  Result := TButton;
end;

function TPyDelphiButton.GetDelphiObject: TButton;
begin
  Result := TButton(inherited DelphiObject);
end;

procedure TPyDelphiButton.SetDelphiObject(const Value: TButton);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TStdCtrlsRegistration.Create);

end.
