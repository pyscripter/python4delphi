{$I Definition.Inc}

unit WrapFmxStdCtrls;

interface

uses
  Classes, SysUtils, FMX.StdCtrls,
  PythonEngine, WrapDelphi, WrapDelphiClasses, WrapFmxControls;

type
  TPyDelphiPresentedTextControl = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TPresentedTextControl;
    procedure SetDelphiObject(const Value: TPresentedTextControl);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TPresentedTextControl read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TStdCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TStdCtrlsRegistration }

procedure TStdCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TStdCtrlsRegistration.Name: string;
begin
  Result := 'StdCtrls';
end;

procedure TStdCtrlsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedTextControl);
end;

{ TPyDelphiPresentedTextControl }

class function TPyDelphiPresentedTextControl.DelphiObjectClass: TClass;
begin
  Result := TPresentedTextControl;
end;

function TPyDelphiPresentedTextControl.GetDelphiObject: TPresentedTextControl;
begin
  Result := TPresentedTextControl(inherited DelphiObject);
end;

procedure TPyDelphiPresentedTextControl.SetDelphiObject(
  const Value: TPresentedTextControl);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TStdCtrlsRegistration.Create);

end.
