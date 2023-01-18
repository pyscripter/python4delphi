unit WrapDelphiDataBind;

interface

uses
  Data.Bind.Components, Data.Bind.ObjectScope,
  WrapDelphiClasses;

type
  TPyDelphiBaseBindScopeComponent = class(TPyDelphiComponent)
	private
		function GetDelphiObject: TBaseBindScopeComponent;
		procedure SetDelphiObject(const Value: TBaseBindScopeComponent);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TBaseBindScopeComponent read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiBaseLinkingBindSource = class(TPyDelphiBaseBindScopeComponent)
	private
		function GetDelphiObject: TBaseLinkingBindSource;
		procedure SetDelphiObject(const Value: TBaseLinkingBindSource);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TBaseLinkingBindSource read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiBaseObjectBindSource = class(TPyDelphiBaseLinkingBindSource)
	private
		function GetDelphiObject: TBaseObjectBindSource;
		procedure SetDelphiObject(const Value: TBaseObjectBindSource);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TBaseObjectBindSource read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomPrototypeBindSource = class(TPyDelphiBaseObjectBindSource)
	private
		function GetDelphiObject: TCustomPrototypeBindSource;
		procedure SetDelphiObject(const Value: TCustomPrototypeBindSource);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomPrototypeBindSource read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiPrototypeBindSource = class(TPyDelphiCustomPrototypeBindSource)
	private
		function GetDelphiObject: TPrototypeBindSource;
		procedure SetDelphiObject(const Value: TPrototypeBindSource);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TPrototypeBindSource read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomBindingsList = class(TPyDelphiComponent)
	private
		function GetDelphiObject: TCustomBindingsList;
		procedure SetDelphiObject(const Value: TCustomBindingsList);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomBindingsList read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiBindingsList = class(TPyDelphiCustomBindingsList)
	private
		function GetDelphiObject: TBindingsList;
		procedure SetDelphiObject(const Value: TBindingsList);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TBindingsList read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiBasicBindComponent = class(TPyDelphiComponent)
	private
		function GetDelphiObject: TBasicBindComponent;
		procedure SetDelphiObject(const Value: TBasicBindComponent);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TBasicBindComponent read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiContainedBindComponent = class(TPyDelphiBasicBindComponent)
	private
		function GetDelphiObject: TContainedBindComponent;
		procedure SetDelphiObject(const Value: TContainedBindComponent);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TContainedBindComponent read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiBindComponentDelegate = class(TPyDelphiContainedBindComponent)
	private
		function GetDelphiObject: TBindComponentDelegate;
		procedure SetDelphiObject(const Value: TBindComponentDelegate);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TBindComponentDelegate read GetDelphiObject
			write SetDelphiObject;
	end;

  //Link property
  TPyDelphiLinkPropertyToFieldDelegate = class(TPyDelphiBindComponentDelegate)
	private
		function GetDelphiObject: TLinkPropertyToFieldDelegate;
		procedure SetDelphiObject(const Value: TLinkPropertyToFieldDelegate);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TLinkPropertyToFieldDelegate read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomLinkPropertyToField = class(TPyDelphiLinkPropertyToFieldDelegate)
	private
		function GetDelphiObject: TCustomLinkPropertyToField;
		procedure SetDelphiObject(const Value: TCustomLinkPropertyToField);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomLinkPropertyToField read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiLinkPropertyToField = class(TPyDelphiCustomLinkPropertyToField)
	private
		function GetDelphiObject: TLinkPropertyToField;
		procedure SetDelphiObject(const Value: TLinkPropertyToField);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TLinkPropertyToField read GetDelphiObject
			write SetDelphiObject;
	end;

  //Link control
  TPyDelphiLinkControlDelegate = class(TPyDelphiBindComponentDelegate)
	private
		function GetDelphiObject: TLinkControlDelegate;
		procedure SetDelphiObject(const Value: TLinkControlDelegate);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TLinkControlDelegate read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiLinkControlToFieldDelegate = class(TPyDelphiLinkControlDelegate)
	private
		function GetDelphiObject: TLinkControlToFieldDelegate;
		procedure SetDelphiObject(const Value: TLinkControlToFieldDelegate);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TLinkControlToFieldDelegate read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomLinkControlToField = class(TPyDelphiLinkControlToFieldDelegate)
	private
		function GetDelphiObject: TCustomLinkControlToField;
		procedure SetDelphiObject(const Value: TCustomLinkControlToField);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomLinkControlToField read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiLinkControlToField = class(TPyDelphiCustomLinkControlToField)
	private
		function GetDelphiObject: TLinkControlToField;
		procedure SetDelphiObject(const Value: TLinkControlToField);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TLinkControlToField read GetDelphiObject
			write SetDelphiObject;
	end;

  //Link list control
  TPyDelphiCustomLinkListControlToField = class(TPyDelphiLinkControlToFieldDelegate)
	private
		function GetDelphiObject: TCustomLinkListControlToField;
		procedure SetDelphiObject(const Value: TCustomLinkListControlToField);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomLinkListControlToField read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiLinkListControlToField = class(TPyDelphiCustomLinkListControlToField)
	private
		function GetDelphiObject: TLinkListControlToField;
		procedure SetDelphiObject(const Value: TLinkListControlToField);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TLinkListControlToField read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TDataBindRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TDataBindRegistration }

procedure TDataBindRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TDataBindRegistration.Name: string;
begin
  Result := 'DataBind';
end;

procedure TDataBindRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBaseBindScopeComponent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBaseLinkingBindSource);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBaseObjectBindSource);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPrototypeBindSource);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPrototypeBindSource);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomBindingsList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBindingsList);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBasicBindComponent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiContainedBindComponent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBindComponentDelegate);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLinkPropertyToFieldDelegate);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomLinkPropertyToField);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLinkPropertyToField);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLinkControlDelegate);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLinkControlToFieldDelegate);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomLinkControlToField);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLinkControlToField);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomLinkListControlToField);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLinkListControlToField);
end;

{ TPyDelphiBaseBindScopeComponent }

class function TPyDelphiBaseBindScopeComponent.DelphiObjectClass: TClass;
begin
  Result := TBaseBindScopeComponent;
end;

function TPyDelphiBaseBindScopeComponent.GetDelphiObject: TBaseBindScopeComponent;
begin
  Result := TBaseBindScopeComponent(inherited DelphiObject);
end;

procedure TPyDelphiBaseBindScopeComponent.SetDelphiObject(
  const Value: TBaseBindScopeComponent);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBaseLinkingBindSource }

class function TPyDelphiBaseLinkingBindSource.DelphiObjectClass: TClass;
begin
  Result := TBaseLinkingBindSource;
end;

function TPyDelphiBaseLinkingBindSource.GetDelphiObject: TBaseLinkingBindSource;
begin
  Result := TBaseLinkingBindSource(inherited DelphiObject);
end;

procedure TPyDelphiBaseLinkingBindSource.SetDelphiObject(
  const Value: TBaseLinkingBindSource);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBaseObjectBindSource }

class function TPyDelphiBaseObjectBindSource.DelphiObjectClass: TClass;
begin
  Result := TBaseObjectBindSource;
end;

function TPyDelphiBaseObjectBindSource.GetDelphiObject: TBaseObjectBindSource;
begin
  Result := TBaseObjectBindSource(inherited DelphiObject);
end;

procedure TPyDelphiBaseObjectBindSource.SetDelphiObject(
  const Value: TBaseObjectBindSource);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPrototypeBindSource }

class function TPyDelphiCustomPrototypeBindSource.DelphiObjectClass: TClass;
begin
  Result := TCustomPrototypeBindSource;
end;

function TPyDelphiCustomPrototypeBindSource.GetDelphiObject: TCustomPrototypeBindSource;
begin
  Result := TCustomPrototypeBindSource(inherited DelphiObject);
end;

procedure TPyDelphiCustomPrototypeBindSource.SetDelphiObject(
  const Value: TCustomPrototypeBindSource);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPrototypeBindSource }

class function TPyDelphiPrototypeBindSource.DelphiObjectClass: TClass;
begin
  Result := TPrototypeBindSource;
end;

function TPyDelphiPrototypeBindSource.GetDelphiObject: TPrototypeBindSource;
begin
  Result := TPrototypeBindSource(inherited DelphiObject);
end;

procedure TPyDelphiPrototypeBindSource.SetDelphiObject(
  const Value: TPrototypeBindSource);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomBindingsList }

class function TPyDelphiCustomBindingsList.DelphiObjectClass: TClass;
begin
  Result := TCustomBindingsList;
end;

function TPyDelphiCustomBindingsList.GetDelphiObject: TCustomBindingsList;
begin
  Result := TCustomBindingsList(inherited DelphiObject);
end;

procedure TPyDelphiCustomBindingsList.SetDelphiObject(
  const Value: TCustomBindingsList);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBindingsList }

class function TPyDelphiBindingsList.DelphiObjectClass: TClass;
begin
  Result := TBindingsList;
end;

function TPyDelphiBindingsList.GetDelphiObject: TBindingsList;
begin
  Result := TBindingsList(inherited DelphiObject);
end;

procedure TPyDelphiBindingsList.SetDelphiObject(const Value: TBindingsList);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBasicBindComponent }

class function TPyDelphiBasicBindComponent.DelphiObjectClass: TClass;
begin
  Result := TBasicBindComponent;
end;

function TPyDelphiBasicBindComponent.GetDelphiObject: TBasicBindComponent;
begin
  Result := TBasicBindComponent(inherited DelphiObject);
end;

procedure TPyDelphiBasicBindComponent.SetDelphiObject(
  const Value: TBasicBindComponent);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiContainedBindComponent }

class function TPyDelphiContainedBindComponent.DelphiObjectClass: TClass;
begin
  Result := TContainedBindComponent;
end;

function TPyDelphiContainedBindComponent.GetDelphiObject: TContainedBindComponent;
begin
  Result := TContainedBindComponent(inherited DelphiObject);
end;

procedure TPyDelphiContainedBindComponent.SetDelphiObject(
  const Value: TContainedBindComponent);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBindComponentDelegate }

class function TPyDelphiBindComponentDelegate.DelphiObjectClass: TClass;
begin
  Result := TBindComponentDelegate;
end;

function TPyDelphiBindComponentDelegate.GetDelphiObject: TBindComponentDelegate;
begin
  Result := TBindComponentDelegate(inherited DelphiObject);
end;

procedure TPyDelphiBindComponentDelegate.SetDelphiObject(
  const Value: TBindComponentDelegate);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLinkPropertyToFieldDelegate }

class function TPyDelphiLinkPropertyToFieldDelegate.DelphiObjectClass: TClass;
begin
  Result := TLinkPropertyToFieldDelegate;
end;

function TPyDelphiLinkPropertyToFieldDelegate.GetDelphiObject: TLinkPropertyToFieldDelegate;
begin
  Result := TLinkPropertyToFieldDelegate(inherited DelphiObject);
end;

procedure TPyDelphiLinkPropertyToFieldDelegate.SetDelphiObject(
  const Value: TLinkPropertyToFieldDelegate);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomLinkPropertyToField }

class function TPyDelphiCustomLinkPropertyToField.DelphiObjectClass: TClass;
begin
  Result := TCustomLinkPropertyToField;
end;

function TPyDelphiCustomLinkPropertyToField.GetDelphiObject: TCustomLinkPropertyToField;
begin
  Result := TCustomLinkPropertyToField(inherited DelphiObject);
end;

procedure TPyDelphiCustomLinkPropertyToField.SetDelphiObject(
  const Value: TCustomLinkPropertyToField);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLinkPropertyToField }

class function TPyDelphiLinkPropertyToField.DelphiObjectClass: TClass;
begin
  Result := TLinkPropertyToField;
end;

function TPyDelphiLinkPropertyToField.GetDelphiObject: TLinkPropertyToField;
begin
  Result := TLinkPropertyToField(inherited DelphiObject);
end;

procedure TPyDelphiLinkPropertyToField.SetDelphiObject(
  const Value: TLinkPropertyToField);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLinkControlDelegate }

class function TPyDelphiLinkControlDelegate.DelphiObjectClass: TClass;
begin
  Result := TLinkControlDelegate;
end;

function TPyDelphiLinkControlDelegate.GetDelphiObject: TLinkControlDelegate;
begin
  Result := TLinkControlDelegate(inherited DelphiObject);
end;

procedure TPyDelphiLinkControlDelegate.SetDelphiObject(
  const Value: TLinkControlDelegate);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLinkControlToFieldDelegate }

class function TPyDelphiLinkControlToFieldDelegate.DelphiObjectClass: TClass;
begin
  Result := TLinkControlToFieldDelegate;
end;

function TPyDelphiLinkControlToFieldDelegate.GetDelphiObject: TLinkControlToFieldDelegate;
begin
  Result := TLinkControlToFieldDelegate(inherited DelphiObject);
end;

procedure TPyDelphiLinkControlToFieldDelegate.SetDelphiObject(
  const Value: TLinkControlToFieldDelegate);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomLinkControlToField }

class function TPyDelphiCustomLinkControlToField.DelphiObjectClass: TClass;
begin
  Result := TCustomLinkControlToField;
end;

function TPyDelphiCustomLinkControlToField.GetDelphiObject: TCustomLinkControlToField;
begin
  Result := TCustomLinkControlToField(inherited DelphiObject);
end;

procedure TPyDelphiCustomLinkControlToField.SetDelphiObject(
  const Value: TCustomLinkControlToField);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLinkControlToField }

class function TPyDelphiLinkControlToField.DelphiObjectClass: TClass;
begin
  Result := TLinkControlToField;
end;

function TPyDelphiLinkControlToField.GetDelphiObject: TLinkControlToField;
begin
  Result := TLinkControlToField(inherited DelphiObject);
end;

procedure TPyDelphiLinkControlToField.SetDelphiObject(
  const Value: TLinkControlToField);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomLinkListControlToField }

class function TPyDelphiCustomLinkListControlToField.DelphiObjectClass: TClass;
begin
  Result := TCustomLinkListControlToField;
end;

function TPyDelphiCustomLinkListControlToField.GetDelphiObject: TCustomLinkListControlToField;
begin
  Result := TCustomLinkListControlToField(inherited DelphiObject);
end;

procedure TPyDelphiCustomLinkListControlToField.SetDelphiObject(
  const Value: TCustomLinkListControlToField);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiLinkListControlToField }

class function TPyDelphiLinkListControlToField.DelphiObjectClass: TClass;
begin
  Result := TLinkListControlToField;
end;

function TPyDelphiLinkListControlToField.GetDelphiObject: TLinkListControlToField;
begin
  Result := TLinkListControlToField(inherited DelphiObject);
end;

procedure TPyDelphiLinkListControlToField.SetDelphiObject(
  const Value: TLinkListControlToField);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TDataBindRegistration.Create);

end.
