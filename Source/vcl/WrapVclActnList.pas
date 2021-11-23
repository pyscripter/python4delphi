{$I ..\Definition.Inc}

unit WrapVclActnList;

interface

uses
  System.Classes,
  PythonEngine, WrapDelphi,
  WrapDelphiClasses,
  WrapActions,
  Vcl.ActnList;

type
  {
     Same as TPyDelphiContainedActionList but having wrappers, exposes
     the types and allows the use of the constructors e.g. ActionList()
  }
  TPyDelphiCustomActionList = class(TPyDelphiContainedActionList)
  private
    function GetDelphiObject: TCustomActionList;
    procedure SetDelphiObject(const Value: TCustomActionList);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomActionList read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiActionList = class (TPyDelphiCustomActionList)
  private
    function  GetDelphiObject: TActionList;
    procedure SetDelphiObject(const Value: TActionList);
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TActionList read GetDelphiObject write SetDelphiObject;
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

implementation

{ Register the wrappers, the globals and the constants }
type
  TActnListRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TActnListRegistration }

function TActnListRegistration.Name: string;
begin
  Result := 'Vcl.ActnList';
end;

procedure TActnListRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomActionList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiActionList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiAction);
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

procedure TPyDelphiActionList.SetDelphiObject(
  const Value: TActionList);
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

initialization
  RegisteredUnits.Add(TActnListRegistration.Create);
end.
