{$I Definition.Inc}

unit WrapFmxListBox;

interface

uses
	FMX.ListBox, WrapFmxTypes, PythonEngine;

type
	TPyListBoxItem = class(TPyDelphiFMXObject)
	private
		function GetDelphiObject: TListBoxItem;
		procedure SetDelphiObject(const Value: TListBoxItem);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TListBoxItem read GetDelphiObject
			write SetDelphiObject;
	end;
	TPyDelphiListBox = class(TPyDelphiFMXObject)
	private
		function GetDelphiObject: TListBox;
		procedure SetDelphiObject(const Value: TListBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TListBox read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
	WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
	TListBoxRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TListBoxRegistration }

procedure TListBoxRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TListBoxRegistration.Name: string;
begin
  Result := 'ListBox';
end;

procedure TListBoxRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListBox);
end;

{ TPyDelphiListBox }

class function TPyDelphiListBox.DelphiObjectClass: TClass;
begin
	Result := TListBox;
end;

function TPyDelphiListBox.GetDelphiObject: TListBox;
begin
	Result := TListBox(inherited DelphiObject);
end;


procedure TPyDelphiListBox.SetDelphiObject(const Value: TListBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyListBoxItem }

class function TPyListBoxItem.DelphiObjectClass: TClass;
begin
	Result := TListBox;
end;

function TPyListBoxItem.GetDelphiObject: TListBoxItem;
begin
	Result := TListBox(inherited DelphiObject);
end;

procedure TPyListBoxItem.SetDelphiObject(const Value: TListBoxItem);
begin
	inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TListBoxRegistration.Create);

end.
