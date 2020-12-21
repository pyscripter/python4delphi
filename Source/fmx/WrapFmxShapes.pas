unit WrapFmxShapes;

interface

uses
  FMX.Objects, WrapFmxControls;

type
  TPyDelphiShape = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TShape;
    procedure SetDelphiObject(const Value: TShape);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TShape read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TShapesRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TShapesRegistration }

procedure TShapesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TShapesRegistration.Name: string;
begin
  Result := 'Shapes';
end;

procedure TShapesRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

{ TPyDelphiShape }

class function TPyDelphiShape.DelphiObjectClass: TClass;
begin
  Result := TShape;
end;

function TPyDelphiShape.GetDelphiObject: TShape;
begin
  Result := TShape(inherited DelphiObject);
end;

procedure TPyDelphiShape.SetDelphiObject(const Value: TShape);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TShapesRegistration.Create);

end.
