unit WrapActions;

interface

uses
  WrapDelphiClasses, System.Classes, System.Actions;

type
  TPyDelphiContainedAction = class(TPyDelphiBasicAction)
  private
    function GetDelphiObject: TContainedAction;
    procedure SetDelphiObject(const Value: TContainedAction);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TContainedAction read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

type
  TActionsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TClassesRegistration }

function TActionsRegistration.Name: string;
begin
  Result := 'Actions';
end;

procedure TActionsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiContainedAction);
end;

{ TPyDelphiContainedAction }

class function TPyDelphiContainedAction.DelphiObjectClass: TClass;
begin
  Result := TContainedAction;
end;

function TPyDelphiContainedAction.GetDelphiObject: TContainedAction;
begin
  Result := TContainedAction(inherited DelphiObject);
end;

procedure TPyDelphiContainedAction.SetDelphiObject
  (const Value: TContainedAction);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TActionsRegistration.Create());

end.
