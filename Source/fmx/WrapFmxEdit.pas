(**************************************************************************)
(*  This unit is part of the Python for Delphi (P4D) library              *)
(*  Project home: https://github.com/pyscripter/python4delphi             *)
(*                                                                        *)
(*  Project Maintainer:  PyScripter (pyscripter@gmail.com)                *)
(*  Original Authors:    Dr. Dietmar Budelsky (dbudelsky@web.de)          *)
(*                       Morgan Martinet (https://github.com/mmm-experts) *)
(*  Core developer:      Lucas Belo (lucas.belo@live.com)                 *)
(*  Contributors:        See contributors.md at project home              *)
(*                                                                        *)
(*  LICENCE and Copyright: MIT (see project home)                         *)
(**************************************************************************)

{$I ..\Definition.Inc}

unit WrapFmxEdit;

interface

uses
	FMX.Edit, FMX.SearchBox, FMX.ComboEdit, FMX.EditBox, FMX.SpinBox, FMX.NumberBox,
  PythonEngine, WrapFmxTypes, WrapFmxControls;

type
  TPyDelphiCustomEdit = class(TPyDelphiPresentedControl)
	private
		function GetDelphiObject: TCustomEdit;
		procedure SetDelphiObject(const Value: TCustomEdit);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TCustomEdit read GetDelphiObject
			write SetDelphiObject;
	end;

	TPyDelphiEdit = class(TPyDelphiCustomEdit)
	private
		function GetDelphiObject: TEdit;
		procedure SetDelphiObject(const Value: TEdit);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TEdit read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiSearchBox = class(TPyDelphiEdit)
	private
		function GetDelphiObject: TSearchBox;
		procedure SetDelphiObject(const Value: TSearchBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TSearchBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiComboEditBase = class(TPyDelphiCustomEdit)
	private
		function GetDelphiObject: TComboEditBase;
		procedure SetDelphiObject(const Value: TComboEditBase);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TComboEditBase read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomComboEdit = class(TPyDelphiComboEditBase)
	private
		function GetDelphiObject: TCustomComboEdit;
		procedure SetDelphiObject(const Value: TCustomComboEdit);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TCustomComboEdit read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiComboEdit = class(TPyDelphiCustomComboEdit)
	private
		function GetDelphiObject: TComboEdit;
		procedure SetDelphiObject(const Value: TComboEdit);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TComboEdit read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomEditBox = class(TPyDelphiCustomEdit)
	private
		function GetDelphiObject: TCustomEditBox;
		procedure SetDelphiObject(const Value: TCustomEditBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TCustomEditBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiSpinBox = class(TPyDelphiCustomEditBox)
	private
		function GetDelphiObject: TSpinBox;
		procedure SetDelphiObject(const Value: TSpinBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TSpinBox read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiNumberBox = class(TPyDelphiCustomEditBox)
	private
		function GetDelphiObject: TNumberBox;
		procedure SetDelphiObject(const Value: TNumberBox);
	public
		class function DelphiObjectClass: TClass; override;
		// Properties
		property DelphiObject: TNumberBox read GetDelphiObject
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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSearchBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComboEditBase);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomComboEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComboEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomEditBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSpinBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiNumberBox);
end;

{ TPyDelphiCustomEdit }

class function TPyDelphiCustomEdit.DelphiObjectClass: TClass;
begin
	Result := TCustomEdit;
end;

function TPyDelphiCustomEdit.GetDelphiObject: TCustomEdit;
begin
	Result := TCustomEdit(inherited DelphiObject);
end;

procedure TPyDelphiCustomEdit.SetDelphiObject(const Value: TCustomEdit);
begin
	inherited DelphiObject := Value;
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

{ TPyDelphiSearchBox }

class function TPyDelphiSearchBox.DelphiObjectClass: TClass;
begin
  Result := TSearchBox;
end;

function TPyDelphiSearchBox.GetDelphiObject: TSearchBox;
begin
	Result := TSearchBox(inherited DelphiObject);
end;

procedure TPyDelphiSearchBox.SetDelphiObject(const Value: TSearchBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiComboEditBase }

class function TPyDelphiComboEditBase.DelphiObjectClass: TClass;
begin
  Result := TComboEditBase;
end;

function TPyDelphiComboEditBase.GetDelphiObject: TComboEditBase;
begin
	Result := TComboEditBase(inherited DelphiObject);
end;

procedure TPyDelphiComboEditBase.SetDelphiObject(const Value: TComboEditBase);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiCustomComboEdit }

class function TPyDelphiCustomComboEdit.DelphiObjectClass: TClass;
begin
  Result := TCustomComboEdit;
end;

function TPyDelphiCustomComboEdit.GetDelphiObject: TCustomComboEdit;
begin
	Result := TCustomComboEdit(inherited DelphiObject);
end;

procedure TPyDelphiCustomComboEdit.SetDelphiObject(
  const Value: TCustomComboEdit);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiComboEdit }

class function TPyDelphiComboEdit.DelphiObjectClass: TClass;
begin
  Result := TComboEdit;
end;

function TPyDelphiComboEdit.GetDelphiObject: TComboEdit;
begin
	Result := TComboEdit(inherited DelphiObject);
end;

procedure TPyDelphiComboEdit.SetDelphiObject(const Value: TComboEdit);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiCustomEditBox }

class function TPyDelphiCustomEditBox.DelphiObjectClass: TClass;
begin
  Result := TCustomEditBox;
end;

function TPyDelphiCustomEditBox.GetDelphiObject: TCustomEditBox;
begin
	Result := TCustomEditBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomEditBox.SetDelphiObject(const Value: TCustomEditBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiSpinBox }

class function TPyDelphiSpinBox.DelphiObjectClass: TClass;
begin
	Result := TSpinBox;
end;

function TPyDelphiSpinBox.GetDelphiObject: TSpinBox;
begin
	Result := TSpinBox(inherited DelphiObject);
end;

procedure TPyDelphiSpinBox.SetDelphiObject(const Value: TSpinBox);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiNumberBox }

class function TPyDelphiNumberBox.DelphiObjectClass: TClass;
begin
	Result := TNumberBox;
end;

function TPyDelphiNumberBox.GetDelphiObject: TNumberBox;
begin
	Result := TNumberBox(inherited DelphiObject);
end;

procedure TPyDelphiNumberBox.SetDelphiObject(const Value: TNumberBox);
begin
	inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TEditRegistration.Create);

end.
