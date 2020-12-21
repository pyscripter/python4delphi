unit WrapFmxLayouts;

interface

uses
  FMX.Layouts, WrapFmxControls, WrapDelphi;

type
  TPyDelphiLayout = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TLayout;
    procedure SetDelphiObject(const Value: TLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TLayout read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TLayoutsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TPyDelphiLayout }

class function TPyDelphiLayout.DelphiObjectClass: TClass;
begin
  Result := TLayout;
end;

function TPyDelphiLayout.GetDelphiObject: TLayout;
begin
  Result := TLayout(inherited DelphiObject);
end;

procedure TPyDelphiLayout.SetDelphiObject(const Value: TLayout);
begin
  inherited DelphiObject := Value;
end;

{ TLayoutsRegistration }

procedure TLayoutsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TLayoutsRegistration.Name: string;
begin
  Result := 'Layouts';
end;

procedure TLayoutsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLayout);
end;

initialization
  RegisteredUnits.Add(TLayoutsRegistration.Create);

end.
