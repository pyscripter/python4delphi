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

  TPyDelphiCalloutPanel = class(TPyDelphiPanel)
  private
    function GetDelphiObject: TCalloutPanel;
    procedure SetDelphiObject(const Value: TCalloutPanel);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCalloutPanel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiLabel = class(TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TLabel;
    procedure SetDelphiObject(const Value: TLabel);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TLabel read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomButton = class(TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TCustomButton;
    procedure SetDelphiObject(const Value: TCustomButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiButton = class(TPyDelphiCustomButton)
  private
    function GetDelphiObject: TButton;
    procedure SetDelphiObject(const Value: TButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSpeedButton = class(TPyDelphiCustomButton)
  private
    function GetDelphiObject: TSpeedButton;
    procedure SetDelphiObject(const Value: TSpeedButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TSpeedButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomCornerButton = class(TPyDelphiCustomButton)
  private
    function GetDelphiObject: TCustomCornerButton;
    procedure SetDelphiObject(const Value: TCustomCornerButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomCornerButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCornerButton = class(TPyDelphiCustomCornerButton)
  private
    function GetDelphiObject: TCornerButton;
    procedure SetDelphiObject(const Value: TCornerButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCornerButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCheckBox = class(TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TCheckBox;
    procedure SetDelphiObject(const Value: TCheckBox);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCheckBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiRadioButton = class(TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TRadioButton;
    procedure SetDelphiObject(const Value: TRadioButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TRadioButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiGroupBox = class(TPyDelphiPresentedTextControl)
  private
    function GetDelphiObject: TGroupBox;
    procedure SetDelphiObject(const Value: TGroupBox);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TGroupBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStatusBar = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TStatusBar;
    procedure SetDelphiObject(const Value: TStatusBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TStatusBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiToolBar = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TToolBar;
    procedure SetDelphiObject(const Value: TToolBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TToolBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSizeGrip = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TSizeGrip;
    procedure SetDelphiObject(const Value: TSizeGrip);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TSizeGrip read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSplitter = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TSplitter;
    procedure SetDelphiObject(const Value: TSplitter);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TSplitter read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiProgressBar = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TProgressBar;
    procedure SetDelphiObject(const Value: TProgressBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TProgressBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiThumb = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TThumb;
    procedure SetDelphiObject(const Value: TThumb);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TThumb read GetDelphiObject write SetDelphiObject;
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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSpeedButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomCornerButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCornerButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCheckBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiRadioButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGroupBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStatusBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiToolBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSizeGrip);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSplitter);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiProgressBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiThumb);
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

{ TPyDelphiSpeedButton }

class function TPyDelphiSpeedButton.DelphiObjectClass: TClass;
begin
  Result := TSpeedButton;
end;

function TPyDelphiSpeedButton.GetDelphiObject: TSpeedButton;
begin
  Result := TSpeedButton(inherited DelphiObject);
end;

procedure TPyDelphiSpeedButton.SetDelphiObject(const Value: TSpeedButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomCornerButton }

class function TPyDelphiCustomCornerButton.DelphiObjectClass: TClass;
begin
  Result := TCustomCornerButton;
end;

function TPyDelphiCustomCornerButton.GetDelphiObject: TCustomCornerButton;
begin
  Result := TCustomCornerButton(inherited DelphiObject);
end;

procedure TPyDelphiCustomCornerButton.SetDelphiObject(
  const Value: TCustomCornerButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCornerButton }

class function TPyDelphiCornerButton.DelphiObjectClass: TClass;
begin
  Result := TCornerButton;
end;

function TPyDelphiCornerButton.GetDelphiObject: TCornerButton;
begin
  Result := TCornerButton(inherited DelphiObject);
end;

procedure TPyDelphiCornerButton.SetDelphiObject(const Value: TCornerButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCheckBox }

class function TPyDelphiCheckBox.DelphiObjectClass: TClass;
begin
  Result := TCheckBox;
end;

function TPyDelphiCheckBox.GetDelphiObject: TCheckBox;
begin
  Result := TCheckBox(inherited DelphiObject);
end;

procedure TPyDelphiCheckBox.SetDelphiObject(const Value: TCheckBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiRadioButton }

class function TPyDelphiRadioButton.DelphiObjectClass: TClass;
begin
  Result := TRadioButton;
end;

function TPyDelphiRadioButton.GetDelphiObject: TRadioButton;
begin
  Result := TRadioButton(inherited DelphiObject);
end;

procedure TPyDelphiRadioButton.SetDelphiObject(const Value: TRadioButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiGroupBox }

class function TPyDelphiGroupBox.DelphiObjectClass: TClass;
begin
  Result := TGroupBox;
end;

function TPyDelphiGroupBox.GetDelphiObject: TGroupBox;
begin
  Result := TGroupBox(inherited DelphiObject);
end;

procedure TPyDelphiGroupBox.SetDelphiObject(const Value: TGroupBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStatusBar }

class function TPyDelphiStatusBar.DelphiObjectClass: TClass;
begin
  Result := TStatusBar;
end;

function TPyDelphiStatusBar.GetDelphiObject: TStatusBar;
begin
  Result := TStatusBar(inherited DelphiObject);
end;

procedure TPyDelphiStatusBar.SetDelphiObject(const Value: TStatusBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiToolBar }

class function TPyDelphiToolBar.DelphiObjectClass: TClass;
begin
  Result := TToolBar;
end;

function TPyDelphiToolBar.GetDelphiObject: TToolBar;
begin
  Result := TToolBar(inherited DelphiObject);
end;

procedure TPyDelphiToolBar.SetDelphiObject(const Value: TToolBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSizeGrip }

class function TPyDelphiSizeGrip.DelphiObjectClass: TClass;
begin
  Result := TSizeGrip;
end;

function TPyDelphiSizeGrip.GetDelphiObject: TSizeGrip;
begin
  Result := TSizeGrip(inherited DelphiObject);
end;

procedure TPyDelphiSizeGrip.SetDelphiObject(const Value: TSizeGrip);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSplitter }

class function TPyDelphiSplitter.DelphiObjectClass: TClass;
begin
  Result := TSplitter;
end;

function TPyDelphiSplitter.GetDelphiObject: TSplitter;
begin
  Result := TSplitter(inherited DelphiObject);
end;

procedure TPyDelphiSplitter.SetDelphiObject(const Value: TSplitter);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiProgressBar }

class function TPyDelphiProgressBar.DelphiObjectClass: TClass;
begin
  Result := TProgressBar;
end;

function TPyDelphiProgressBar.GetDelphiObject: TProgressBar;
begin
  Result := TProgressBar(inherited DelphiObject);
end;

procedure TPyDelphiProgressBar.SetDelphiObject(const Value: TProgressBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiThumb }

class function TPyDelphiThumb.DelphiObjectClass: TClass;
begin
  Result := TThumb;
end;

function TPyDelphiThumb.GetDelphiObject: TThumb;
begin
  Result := TThumb(inherited DelphiObject);
end;

procedure TPyDelphiThumb.SetDelphiObject(const Value: TThumb);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TStdCtrlsRegistration.Create);

end.
