{$I ..\Definition.Inc}

unit WrapFmxStdCtrls;

interface

uses
  Classes, SysUtils, FMX.StdCtrls,
  PythonEngine, WrapDelphi, WrapDelphiClasses, WrapFmxControls, WrapFmxActnList;

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

  TPyDelphiCustomTrack = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TCustomTrack;
    procedure SetDelphiObject(const Value: TCustomTrack);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomTrack read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTrack = class(TPyDelphiCustomTrack)
  private
    function GetDelphiObject: TTrack;
    procedure SetDelphiObject(const Value: TTrack);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTrack read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTrackBar = class(TPyDelphiCustomTrack)
  private
    function GetDelphiObject: TTrackBar;
    procedure SetDelphiObject(const Value: TTrackBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTrackBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBitmapTrackBar = class(TPyDelphiTrackBar)
  private
    function GetDelphiObject: TBitmapTrackBar;
    procedure SetDelphiObject(const Value: TBitmapTrackBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TBitmapTrackBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomSwitch = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TCustomSwitch;
    procedure SetDelphiObject(const Value: TCustomSwitch);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomSwitch read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSwitch = class(TPyDelphiCustomSwitch)
  private
    function GetDelphiObject: TSwitch;
    procedure SetDelphiObject(const Value: TSwitch);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TSwitch read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiScrollBar = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TScrollBar;
    procedure SetDelphiObject(const Value: TScrollBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TScrollBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSmallScrollBar = class(TPyDelphiScrollBar)
  private
    function GetDelphiObject: TSmallScrollBar;
    procedure SetDelphiObject(const Value: TSmallScrollBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TSmallScrollBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiAniIndicator = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TAniIndicator;
    procedure SetDelphiObject(const Value: TAniIndicator);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TAniIndicator read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiArcDial = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TArcDial;
    procedure SetDelphiObject(const Value: TArcDial);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TArcDial read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiExpanderButton = class(TPyDelphiCustomButton)
  private
    function GetDelphiObject: TExpanderButton;
    procedure SetDelphiObject(const Value: TExpanderButton);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TExpanderButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiExpander = class(TPyDelphiTextControl)
  private
    function GetDelphiObject: TExpander;
    procedure SetDelphiObject(const Value: TExpander);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TExpander read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiImageControl = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TImageControl;
    procedure SetDelphiObject(const Value: TImageControl);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TImageControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPathLabel = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TPathLabel;
    procedure SetDelphiObject(const Value: TPathLabel);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TPathLabel read GetDelphiObject write SetDelphiObject;
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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomTrack);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTrack);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTrackBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBitmapTrackBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomSwitch);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSwitch);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScrollBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSmallScrollBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiAniIndicator);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiArcDial);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiExpanderButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiExpander);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiImageControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPathLabel);
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

{ TPyDelphiCustomTrack }

class function TPyDelphiCustomTrack.DelphiObjectClass: TClass;
begin
  Result := TCustomTrack;
end;

function TPyDelphiCustomTrack.GetDelphiObject: TCustomTrack;
begin
  Result := TCustomTrack(inherited DelphiObject);
end;

procedure TPyDelphiCustomTrack.SetDelphiObject(const Value: TCustomTrack);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTrack }

class function TPyDelphiTrack.DelphiObjectClass: TClass;
begin
  Result := TTrack;
end;

function TPyDelphiTrack.GetDelphiObject: TTrack;
begin
  Result := TTrack(inherited DelphiObject);
end;

procedure TPyDelphiTrack.SetDelphiObject(const Value: TTrack);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTrackBar }

class function TPyDelphiTrackBar.DelphiObjectClass: TClass;
begin
  Result := TTrackBar;
end;

function TPyDelphiTrackBar.GetDelphiObject: TTrackBar;
begin
  Result := TTrackBar(inherited DelphiObject);
end;

procedure TPyDelphiTrackBar.SetDelphiObject(const Value: TTrackBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBitmapTrackBar }

class function TPyDelphiBitmapTrackBar.DelphiObjectClass: TClass;
begin
  Result := TBitmapTrackBar;
end;

function TPyDelphiBitmapTrackBar.GetDelphiObject: TBitmapTrackBar;
begin
  Result := TBitmapTrackBar(inherited DelphiObject);
end;

procedure TPyDelphiBitmapTrackBar.SetDelphiObject(const Value: TBitmapTrackBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomSwitch }

class function TPyDelphiCustomSwitch.DelphiObjectClass: TClass;
begin
  Result := TCustomSwitch;
end;

function TPyDelphiCustomSwitch.GetDelphiObject: TCustomSwitch;
begin
  Result := TCustomSwitch(inherited DelphiObject);
end;

procedure TPyDelphiCustomSwitch.SetDelphiObject(const Value: TCustomSwitch);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSwitch }

class function TPyDelphiSwitch.DelphiObjectClass: TClass;
begin
  Result := TSwitch;
end;

function TPyDelphiSwitch.GetDelphiObject: TSwitch;
begin
  Result := TSwitch(inherited DelphiObject);
end;

procedure TPyDelphiSwitch.SetDelphiObject(const Value: TSwitch);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiScrollBar }

class function TPyDelphiScrollBar.DelphiObjectClass: TClass;
begin
  Result := TScrollBar;
end;

function TPyDelphiScrollBar.GetDelphiObject: TScrollBar;
begin
  Result := TScrollBar(inherited DelphiObject);
end;

procedure TPyDelphiScrollBar.SetDelphiObject(const Value: TScrollBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSmallScrollBar }

class function TPyDelphiSmallScrollBar.DelphiObjectClass: TClass;
begin
  Result := TSmallScrollBar;
end;

function TPyDelphiSmallScrollBar.GetDelphiObject: TSmallScrollBar;
begin
  Result := TSmallScrollBar(inherited DelphiObject);
end;

procedure TPyDelphiSmallScrollBar.SetDelphiObject(const Value: TSmallScrollBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiAniIndicator }

class function TPyDelphiAniIndicator.DelphiObjectClass: TClass;
begin
  Result := TAniIndicator;
end;

function TPyDelphiAniIndicator.GetDelphiObject: TAniIndicator;
begin
  Result := TAniIndicator(inherited DelphiObject);
end;

procedure TPyDelphiAniIndicator.SetDelphiObject(const Value: TAniIndicator);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiArcDial }

class function TPyDelphiArcDial.DelphiObjectClass: TClass;
begin
  Result := TArcDial;
end;

function TPyDelphiArcDial.GetDelphiObject: TArcDial;
begin
  Result := TArcDial(inherited DelphiObject);
end;

procedure TPyDelphiArcDial.SetDelphiObject(const Value: TArcDial);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiExpanderButton }

class function TPyDelphiExpanderButton.DelphiObjectClass: TClass;
begin
  Result := TExpanderButton;
end;

function TPyDelphiExpanderButton.GetDelphiObject: TExpanderButton;
begin
  Result := TExpanderButton(inherited DelphiObject);
end;

procedure TPyDelphiExpanderButton.SetDelphiObject(const Value: TExpanderButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiExpander }

class function TPyDelphiExpander.DelphiObjectClass: TClass;
begin
  Result := TExpander;
end;

function TPyDelphiExpander.GetDelphiObject: TExpander;
begin
  Result := TExpander(inherited DelphiObject);
end;

procedure TPyDelphiExpander.SetDelphiObject(const Value: TExpander);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiImageControl }

class function TPyDelphiImageControl.DelphiObjectClass: TClass;
begin
  Result := TImageControl;
end;

function TPyDelphiImageControl.GetDelphiObject: TImageControl;
begin
  Result := TImageControl(inherited DelphiObject);
end;

procedure TPyDelphiImageControl.SetDelphiObject(const Value: TImageControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPathLabel }

class function TPyDelphiPathLabel.DelphiObjectClass: TClass;
begin
  Result := TPathLabel;
end;

function TPyDelphiPathLabel.GetDelphiObject: TPathLabel;
begin
  Result := TPathLabel(inherited DelphiObject);
end;

procedure TPyDelphiPathLabel.SetDelphiObject(const Value: TPathLabel);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TStdCtrlsRegistration.Create);

end.
