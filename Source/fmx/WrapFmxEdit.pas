{$I Definition.Inc}

unit WrapFmxEdit;

interface

uses
	FMX.Edit, WrapFmxTypes, PythonEngine;


type
	TPyDelphiEdit = class(TPyDelphiFMXObject)
	private
		function GetDelphiObject: TEdit;
		procedure SetDelphiObject(const Value: TEdit);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TEdit read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
	TEditRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TEditRegistration }

procedure TEditRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
	inherited;
end;

function TEditRegistration.Name: string;
begin
	Result := 'Edit';
end;

procedure TEditRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiEdit);
end;

{ TPyDelphiEdit }

class function TPyDelphiEdit.DelphiObjectClass: TClass;
begin
	Result := TEdit;
end;

function TPyDelphiEdit.GetDelphiObject: TEdit;
begin
	Result := TEdit(inherited DelphiObject);
end;

procedure TPyDelphiEdit.SetDelphiObject(const Value: TEdit);
begin
	inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TEditRegistration.Create);

end.
