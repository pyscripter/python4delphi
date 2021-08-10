{$I ..\Definition.Inc}

unit WrapFmxMedia;

interface

uses
  PythonEngine, WrapFmxTypes, FMX.Media;

type
  TPyDelphiCameraComponent = class (TPyDelphiFmxObject)
  private
    function GetDelphiObject: TCameraComponent;
    procedure SetDelphiObject(const Value: TCameraComponent);
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
  public
    property DelphiObject: TCameraComponent read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

type
	TFMXMediaRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TFMXMediaRegistration }

function TFMXMediaRegistration.Name: string;
begin
  Result := 'Media';
end;

procedure TFMXMediaRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TFMXMediaRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCameraComponent);
end;

{ TPyDelphiCameraComponent }

class function TPyDelphiCameraComponent.DelphiObjectClass: TClass;
begin
  Result := TCameraComponent;
end;

class procedure TPyDelphiCameraComponent.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
end;

class procedure TPyDelphiCameraComponent.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
end;

function TPyDelphiCameraComponent.GetDelphiObject: TCameraComponent;
begin
  Result := TCameraComponent(inherited DelphiObject);
end;

procedure TPyDelphiCameraComponent.SetDelphiObject(
  const Value: TCameraComponent);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TFMXMediaRegistration.Create());

end.
