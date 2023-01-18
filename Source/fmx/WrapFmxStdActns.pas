{$I ..\Definition.Inc}
unit WrapFmxStdActns;

interface

uses
  FMX.StdActns,
  PythonEngine, WrapDelphi, WrapFmxControls, WrapFmxActnList, WrapDelphiClasses;

type
  TPyDelphiHintAction = class(TPyDelphiCustomAction)
  private
    function GetDelphiObject: THintAction;
    procedure SetDelphiObject(const Value: THintAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: THintAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiSysCommonAction = class(TPyDelphiCustomAction)
  private
    function GetDelphiObject: TSysCommonAction;
    procedure SetDelphiObject(const Value: TSysCommonAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TSysCommonAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiFileExit = class(TPyDelphiSysCommonAction)
  private
    function GetDelphiObject: TFileExit;
    procedure SetDelphiObject(const Value: TFileExit);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TFileExit read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiWindowClose = class(TPyDelphiSysCommonAction)
  private
    function GetDelphiObject: TWindowClose;
    procedure SetDelphiObject(const Value: TWindowClose);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TWindowClose read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiFileHideApp = class(TPyDelphiSysCommonAction)
  private
    function GetDelphiObject: TFileHideApp;
    procedure SetDelphiObject(const Value: TFileHideApp);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TFileHideApp read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiFileHideAppOthers = class(TPyDelphiFileHideApp)
  private
    function GetDelphiObject: TFileHideAppOthers;
    procedure SetDelphiObject(const Value: TFileHideAppOthers);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TFileHideAppOthers read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiObjectViewAction = class(TPyDelphiCustomViewAction)
  private
    function GetDelphiObject: TObjectViewAction;
    procedure SetDelphiObject(const Value: TObjectViewAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TObjectViewAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiVirtualKeyboard = class(TPyDelphiObjectViewAction)
  private
    function GetDelphiObject: TVirtualKeyboard;
    procedure SetDelphiObject(const Value: TVirtualKeyboard);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TVirtualKeyboard read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiViewAction = class(TPyDelphiObjectViewAction)
  private
    function GetDelphiObject: TViewAction;
    procedure SetDelphiObject(const Value: TViewAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TViewAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiBaseValueRange = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TBaseValueRange;
    procedure SetDelphiObject(const Value: TBaseValueRange);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TBaseValueRange read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiCustomValueRange = class(TPyDelphiBaseValueRange)
  private
    function GetDelphiObject: TCustomValueRange;
    procedure SetDelphiObject(const Value: TCustomValueRange);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TCustomValueRange read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiValueRange = class(TPyDelphiCustomValueRange)
  private
    function GetDelphiObject: TValueRange;
    procedure SetDelphiObject(const Value: TValueRange);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TValueRange read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiCustomValueRangeAction = class(TPyDelphiCustomControlAction)
  private
    function GetDelphiObject: TCustomValueRangeAction;
    procedure SetDelphiObject(const Value: TCustomValueRangeAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TCustomValueRangeAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiValueRangeAction = class(TPyDelphiCustomValueRangeAction)
  private
    function GetDelphiObject: TValueRangeAction;
    procedure SetDelphiObject(const Value: TValueRangeAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TValueRangeAction read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

type
  TStdActnsRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TFMXStdActns }

function TStdActnsRegistration.Name: string;
begin
  Result := 'StdActns';
end;

procedure TStdActnsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TStdActnsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiHintAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSysCommonAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFileExit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiWindowClose);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFileHideApp);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFileHideAppOthers);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiObjectViewAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiVirtualKeyboard);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiViewAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBaseValueRange);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomValueRange);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiValueRange);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomValueRangeAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiValueRangeAction);
end;

{ TPyDelphiHintAction }

class function TPyDelphiHintAction.DelphiObjectClass: TClass;
begin
  Result := THintAction;
end;

function TPyDelphiHintAction.GetDelphiObject: THintAction;
begin
  Result := THintAction(inherited DelphiObject);
end;

procedure TPyDelphiHintAction.SetDelphiObject(const Value: THintAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSysCommonAction }

class function TPyDelphiSysCommonAction.DelphiObjectClass: TClass;
begin
  Result := TSysCommonAction;
end;

function TPyDelphiSysCommonAction.GetDelphiObject: TSysCommonAction;
begin
  Result := TSysCommonAction(inherited DelphiObject);
end;

procedure TPyDelphiSysCommonAction.SetDelphiObject(
  const Value: TSysCommonAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFileExit }

class function TPyDelphiFileExit.DelphiObjectClass: TClass;
begin
  Result := TFileExit;
end;

function TPyDelphiFileExit.GetDelphiObject: TFileExit;
begin
  Result := TFileExit(inherited DelphiObject);
end;

procedure TPyDelphiFileExit.SetDelphiObject(const Value: TFileExit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiWindowClose }

class function TPyDelphiWindowClose.DelphiObjectClass: TClass;
begin
  Result := TWindowClose;
end;

function TPyDelphiWindowClose.GetDelphiObject: TWindowClose;
begin
  Result := TWindowClose(inherited DelphiObject);
end;

procedure TPyDelphiWindowClose.SetDelphiObject(const Value: TWindowClose);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFileHideApp }

class function TPyDelphiFileHideApp.DelphiObjectClass: TClass;
begin
  Result := TFileHideApp;
end;

function TPyDelphiFileHideApp.GetDelphiObject: TFileHideApp;
begin
  Result := TFileHideApp(inherited DelphiObject);
end;

procedure TPyDelphiFileHideApp.SetDelphiObject(const Value: TFileHideApp);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFileHideAppOthers }

class function TPyDelphiFileHideAppOthers.DelphiObjectClass: TClass;
begin
  Result := TFileHideAppOthers;
end;

function TPyDelphiFileHideAppOthers.GetDelphiObject: TFileHideAppOthers;
begin
  Result := TFileHideAppOthers(inherited DelphiObject);
end;

procedure TPyDelphiFileHideAppOthers.SetDelphiObject(
  const Value: TFileHideAppOthers);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiObjectViewAction }

class function TPyDelphiObjectViewAction.DelphiObjectClass: TClass;
begin
  Result := TObjectViewAction;
end;

function TPyDelphiObjectViewAction.GetDelphiObject: TObjectViewAction;
begin
  Result := TObjectViewAction(inherited DelphiObject);
end;

procedure TPyDelphiObjectViewAction.SetDelphiObject(
  const Value: TObjectViewAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiVirtualKeyboard }

class function TPyDelphiVirtualKeyboard.DelphiObjectClass: TClass;
begin
  Result := TVirtualKeyboard;
end;

function TPyDelphiVirtualKeyboard.GetDelphiObject: TVirtualKeyboard;
begin
  Result := TVirtualKeyboard(inherited DelphiObject);
end;

procedure TPyDelphiVirtualKeyboard.SetDelphiObject(
  const Value: TVirtualKeyboard);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiViewAction }

class function TPyDelphiViewAction.DelphiObjectClass: TClass;
begin
  Result := TViewAction;
end;

function TPyDelphiViewAction.GetDelphiObject: TViewAction;
begin
  Result := TViewAction(inherited DelphiObject);
end;

procedure TPyDelphiViewAction.SetDelphiObject(const Value: TViewAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBaseValueRange }

class function TPyDelphiBaseValueRange.DelphiObjectClass: TClass;
begin
  Result := TBaseValueRange;
end;

function TPyDelphiBaseValueRange.GetDelphiObject: TBaseValueRange;
begin
  Result := TBaseValueRange(inherited DelphiObject);
end;

procedure TPyDelphiBaseValueRange.SetDelphiObject(const Value: TBaseValueRange);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomValueRange }

class function TPyDelphiCustomValueRange.DelphiObjectClass: TClass;
begin
  Result := TCustomValueRange;
end;

function TPyDelphiCustomValueRange.GetDelphiObject: TCustomValueRange;
begin
  Result := TCustomValueRange(inherited DelphiObject);
end;

procedure TPyDelphiCustomValueRange.SetDelphiObject(
  const Value: TCustomValueRange);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiValueRange }

class function TPyDelphiValueRange.DelphiObjectClass: TClass;
begin
  Result := TValueRange;
end;

function TPyDelphiValueRange.GetDelphiObject: TValueRange;
begin
  Result := TValueRange(inherited DelphiObject);
end;

procedure TPyDelphiValueRange.SetDelphiObject(const Value: TValueRange);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomValueRangeAction }

class function TPyDelphiCustomValueRangeAction.DelphiObjectClass: TClass;
begin
  Result := TCustomValueRangeAction;
end;

function TPyDelphiCustomValueRangeAction.GetDelphiObject: TCustomValueRangeAction;
begin
  Result := TCustomValueRangeAction(inherited DelphiObject);
end;

procedure TPyDelphiCustomValueRangeAction.SetDelphiObject(
  const Value: TCustomValueRangeAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiValueRangeAction }

class function TPyDelphiValueRangeAction.DelphiObjectClass: TClass;
begin
  Result := TValueRangeAction;
end;

function TPyDelphiValueRangeAction.GetDelphiObject: TValueRangeAction;
begin
  Result := TValueRangeAction(inherited DelphiObject);
end;

procedure TPyDelphiValueRangeAction.SetDelphiObject(
  const Value: TValueRangeAction);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TStdActnsRegistration.Create);

end.
