{$I ..\Definition.Inc}

unit WrapFmxComCtrls;

interface

uses
  FMX.Controls.Presentation, FMX.MultiView, FMX.TabControl, WrapFmxControls;

type
  TPyDelphiTabControl = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TTabControl;
    procedure SetDelphiObject(const Value: TTabControl);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTabControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTabItem = class(TPyDelphiTextControl)
  private
    function GetDelphiObject: TTabItem;
    procedure SetDelphiObject(const Value: TTabItem);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTabItem read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomMultiView = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TCustomMultiView;
    procedure SetDelphiObject(const Value: TCustomMultiView);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomMultiView read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMultiView = class(TPyDelphiCustomMultiView)
  private
    function GetDelphiObject: TMultiView;
    procedure SetDelphiObject(const Value: TMultiView);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TMultiView read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TComCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TComCtrlsRegistration }

procedure TComCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TComCtrlsRegistration.Name: string;
begin
  Result := 'ComCtrls';
end;

procedure TComCtrlsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTabControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTabItem);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomMultiView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMultiView);
end;

{ TPyDelphiTabControl }

class function TPyDelphiTabControl.DelphiObjectClass: TClass;
begin
  Result := TTabControl;
end;

function TPyDelphiTabControl.GetDelphiObject: TTabControl;
begin
  Result := TTabControl(inherited DelphiObject);
end;

procedure TPyDelphiTabControl.SetDelphiObject(const Value: TTabControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTabItem }

class function TPyDelphiTabItem.DelphiObjectClass: TClass;
begin
  Result := TTabItem;
end;

function TPyDelphiTabItem.GetDelphiObject: TTabItem;
begin
  Result := TTabItem(inherited DelphiObject);
end;

procedure TPyDelphiTabItem.SetDelphiObject(const Value: TTabItem);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomMultiView }

class function TPyDelphiCustomMultiView.DelphiObjectClass: TClass;
begin
  Result := TCustomMultiView;
end;

function TPyDelphiCustomMultiView.GetDelphiObject: TCustomMultiView;
begin
  Result := TCustomMultiView(inherited DelphiObject);
end;

procedure TPyDelphiCustomMultiView.SetDelphiObject(
  const Value: TCustomMultiView);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMultiView }

class function TPyDelphiMultiView.DelphiObjectClass: TClass;
begin
  Result := TMultiView;
end;

function TPyDelphiMultiView.GetDelphiObject: TMultiView;
begin
  Result := TMultiView(inherited DelphiObject);
end;

procedure TPyDelphiMultiView.SetDelphiObject(const Value: TMultiView);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add( TComCtrlsRegistration.Create );

end.
