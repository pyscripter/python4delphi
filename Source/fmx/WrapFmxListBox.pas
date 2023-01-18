{$I ..\Definition.Inc}

unit WrapFmxListBox;

interface

uses
	FMX.ListBox, WrapFmxTypes, WrapFmxControls, WrapFmxLayouts, PythonEngine;

type
	TPyListBoxItem = class(TPyDelphiTextControl)
	private
		function GetDelphiObject: TListBoxItem;
		procedure SetDelphiObject(const Value: TListBoxItem);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TListBoxItem read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomListBox = class(TPyDelphiScrollBox)
  private
		function GetDelphiObject: TCustomListBox;
		procedure SetDelphiObject(const Value: TCustomListBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TCustomListBox read GetDelphiObject
			write SetDelphiObject;
  end;

	TPyDelphiListBox = class(TPyDelphiCustomListBox)
	private
		function GetDelphiObject: TListBox;
		procedure SetDelphiObject(const Value: TListBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TListBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomComboBox = class(TPyDelphiStyledControl)
  private
		function GetDelphiObject: TCustomComboBox;
		procedure SetDelphiObject(const Value: TCustomComboBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TCustomComboBox read GetDelphiObject
			write SetDelphiObject;
  end;

	TPyDelphiComboBox = class(TPyDelphiCustomComboBox)
	private
		function GetDelphiObject: TComboBox;
		procedure SetDelphiObject(const Value: TComboBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TComboBox read GetDelphiObject
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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyListBoxItem);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomListBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomComboBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComboBox);
end;

{ TPyListBoxItem }

class function TPyListBoxItem.DelphiObjectClass: TClass;
begin
	Result := TListBoxItem;
end;

function TPyListBoxItem.GetDelphiObject: TListBoxItem;
begin
	Result := TListBoxItem(inherited DelphiObject);
end;

procedure TPyListBoxItem.SetDelphiObject(const Value: TListBoxItem);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiCustomListBox }

class function TPyDelphiCustomListBox.DelphiObjectClass: TClass;
begin
	Result := TCustomListBox;
end;

function TPyDelphiCustomListBox.GetDelphiObject: TCustomListBox;
begin
	Result := TCustomListBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomListBox.SetDelphiObject(const Value: TCustomListBox);
begin
	inherited DelphiObject := Value;
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

{ TPyDelphiCustomComboBox }

class function TPyDelphiCustomComboBox.DelphiObjectClass: TClass;
begin
  Result := TCustomComboBox;
end;

function TPyDelphiCustomComboBox.GetDelphiObject: TCustomComboBox;
begin
  Result := TCustomComboBox(inherited DelphiObject)
end;

procedure TPyDelphiCustomComboBox.SetDelphiObject(const Value: TCustomComboBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiComboBox }

class function TPyDelphiComboBox.DelphiObjectClass: TClass;
begin
  Result := TComboBox;
end;

function TPyDelphiComboBox.GetDelphiObject: TComboBox;
begin
  Result := TComboBox(inherited DelphiObject)
end;

procedure TPyDelphiComboBox.SetDelphiObject(const Value: TComboBox);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TListBoxRegistration.Create);

end.
