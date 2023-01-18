{$I ..\Definition.Inc}

unit WrapVclSamplesSpin;

interface

uses
  Classes, Vcl.Samples.Spin, WrapDelphi, WrapVclControls, WrapVclStdCtrls;

type
  TPyDelphiSpinButton = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TSpinButton;
    procedure SetDelphiObject(const Value: TSpinButton);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TSpinButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiSpinEdit = class (TPyDelphiCustomEdit)
  private
    function  GetDelphiObject: TSpinEdit;
    procedure SetDelphiObject(const Value: TSpinEdit);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TSpinEdit read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TSamplesSpinRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TStdCtrlsRegistration }

procedure TSamplesSpinRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TSamplesSpinRegistration.Name: string;
begin
  Result := 'Samples.Spin';
end;

procedure TSamplesSpinRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSpinButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSpinEdit);
end;

{ TPyDelphiSpinButton }

class function TPyDelphiSpinButton.DelphiObjectClass: TClass;
begin
  Result := TSpinButton;
end;

function TPyDelphiSpinButton.GetDelphiObject: TSpinButton;
begin
  Result := TSpinButton(inherited DelphiObject);
end;

procedure TPyDelphiSpinButton.SetDelphiObject(const Value: TSpinButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSpinEdit }

class function TPyDelphiSpinEdit.DelphiObjectClass: TClass;
begin
  Result := TSpinEdit;
end;

function TPyDelphiSpinEdit.GetDelphiObject: TSpinEdit;
begin
  Result := TSpinEdit(inherited DelphiObject);
end;

procedure TPyDelphiSpinEdit.SetDelphiObject(const Value: TSpinEdit);
begin
  inherited DelphiObject := Value;
end;



initialization
  RegisteredUnits.Add( TSamplesSpinRegistration.Create );
  Classes.RegisterClasses([TSpinButton, TSpinEdit]);
end.
