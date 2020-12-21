unit WrapFmxGrids;

interface

uses
  FMX.Grid, WrapFmxControls;

type
  TPyDelphiColumn = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TColumn;
    procedure SetDelphiObject(const Value: TColumn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TColumn read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TGridsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TGridsRegistration }

procedure TGridsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TGridsRegistration.Name: string;
begin
  Result := 'Grids';
end;

procedure TGridsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiColumn);
end;

{ TPyDelphiColumn }

class function TPyDelphiColumn.DelphiObjectClass: TClass;
begin
  Result := TColumn;
end;

function TPyDelphiColumn.GetDelphiObject: TColumn;
begin
  Result := TColumn(inherited DelphiObject);
end;

procedure TPyDelphiColumn.SetDelphiObject(const Value: TColumn);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TGridsRegistration.Create);

end.
