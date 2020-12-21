unit WrapFmxForms;

interface

uses
  FMX.Forms, PythonEngine, WrapDelphiClasses, WrapFmxTypes;

type
  TPyDelphiApplication = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TApplication;
    procedure SetDelphiObject(const Value: TApplication);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TApplication read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCommonCustomForm = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TCommonCustomForm;
    procedure SetDelphiObject(const Value: TCommonCustomForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCommonCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomForm = class(TPyDelphiCommonCustomForm)
  private
    function GetDelphiObject: TCustomForm;
    procedure SetDelphiObject(const Value: TCustomForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPopupForm = class(TPyDelphiCustomForm)
  private
    function GetDelphiObject: TCustomPopupForm;
    procedure SetDelphiObject(const Value: TCustomPopupForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomPopupForm read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TFormsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TFormsRegistration }

procedure TFormsRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TFormsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  // Singletons
  APyDelphiWrapper.DefineVar('Application', Application);
  APyDelphiWrapper.DefineVar('Screen', Screen);
end;

function TFormsRegistration.Name: string;
begin
  Result := 'Forms';
end;

procedure TFormsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiApplication);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCommonCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPopupForm);
end;

{ TPyDelphiApplication }

class function TPyDelphiApplication.DelphiObjectClass: TClass;
begin
  Result := TApplication;
end;

function TPyDelphiApplication.GetDelphiObject: TApplication;
begin
  Result := TApplication(inherited DelphiObject);
end;

procedure TPyDelphiApplication.SetDelphiObject(const Value: TApplication);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCommonCustomForm }

class function TPyDelphiCommonCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCommonCustomForm;
end;

function TPyDelphiCommonCustomForm.GetDelphiObject: TCommonCustomForm;
begin
  Result := TCommonCustomForm(inherited DelphiObject);
end;

procedure TPyDelphiCommonCustomForm.SetDelphiObject(
  const Value: TCommonCustomForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomForm }

class function TPyDelphiCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCustomForm;
end;

function TPyDelphiCustomForm.GetDelphiObject: TCustomForm;
begin
  Result := TCustomForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomForm.SetDelphiObject(const Value: TCustomForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPopupForm }

class function TPyDelphiCustomPopupForm.DelphiObjectClass: TClass;
begin
  Result := TCustomPopupForm;
end;

function TPyDelphiCustomPopupForm.GetDelphiObject: TCustomPopupForm;
begin
  Result := TCustomPopupForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomPopupForm.SetDelphiObject(
  const Value: TCustomPopupForm);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TFormsRegistration.Create);

end.
