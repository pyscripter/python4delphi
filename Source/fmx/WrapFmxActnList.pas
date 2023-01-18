{$I ..\Definition.Inc}
unit WrapFmxActnList;

interface

uses
  System.Classes, FMX.ActnList, PythonEngine, WrapDelphi, WrapDelphiClasses,
  System.Actions, WrapActions;

type
  TPyDelphiCustomActionList = class(TPyDelphiContainedActionList)
  private
    function GetDelphiObject: TCustomActionList;
    procedure SetDelphiObject(const Value: TCustomActionList);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomActionList read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiActionList = class(TPyDelphiCustomActionList)
  private
    function GetDelphiObject: TActionList;
    procedure SetDelphiObject(const Value: TActionList);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TActionList read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiCustomAction = class(TPyDelphiContainedAction)
  private
    function GetDelphiObject: TCustomAction;
    procedure SetDelphiObject(const Value: TCustomAction);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiAction = class(TPyDelphiContainedAction)
  private
    function GetDelphiObject: TAction;
    procedure SetDelphiObject(const Value: TAction);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TAction read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomViewAction = class(TPyDelphiCustomAction)
  private
    function GetDelphiObject: TCustomViewAction;
    procedure SetDelphiObject(const Value: TCustomViewAction);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomViewAction read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TActnListRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

  { TActnListRegistration }
procedure TActnListRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TActnListRegistration.Name: string;
begin
  Result := 'FMX ActnList';
end;

procedure TActnListRegistration.RegisterWrappers(APyDelphiWrapper
  : TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomActionList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiActionList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomViewAction);
end;

{ TPyDelphiCustomActionList }
class function TPyDelphiCustomActionList.DelphiObjectClass: TClass;
begin
  Result := TCustomActionList;
end;

function TPyDelphiCustomActionList.GetDelphiObject: TCustomActionList;
begin
  Result := TCustomActionList(inherited DelphiObject);
end;

procedure TPyDelphiCustomActionList.SetDelphiObject
  (const Value: TCustomActionList);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiActionList }

class function TPyDelphiActionList.DelphiObjectClass: TClass;
begin
  Result := TActionList;
end;

function TPyDelphiActionList.GetDelphiObject: TActionList;
begin
  Result := TActionList(inherited DelphiObject);
end;

procedure TPyDelphiActionList.SetDelphiObject(const Value: TActionList);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomAction }

class function TPyDelphiCustomAction.DelphiObjectClass: TClass;
begin
  Result := TCustomAction;
end;

function TPyDelphiCustomAction.GetDelphiObject: TCustomAction;
begin
  Result := TCustomAction(inherited DelphiObject);
end;

procedure TPyDelphiCustomAction.SetDelphiObject(const Value: TCustomAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiAction }

class function TPyDelphiAction.DelphiObjectClass: TClass;
begin
  Result := TAction;
end;

function TPyDelphiAction.GetDelphiObject: TAction;
begin
  Result := TAction(inherited DelphiObject);
end;

procedure TPyDelphiAction.SetDelphiObject(const Value: TAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomViewAction }

class function TPyDelphiCustomViewAction.DelphiObjectClass: TClass;
begin
  Result := TCustomViewAction;
end;

function TPyDelphiCustomViewAction.GetDelphiObject: TCustomViewAction;
begin
  Result := TCustomViewAction(inherited DelphiObject);
end;

procedure TPyDelphiCustomViewAction.SetDelphiObject(
  const Value: TCustomViewAction);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TActnListRegistration.Create());

end.
