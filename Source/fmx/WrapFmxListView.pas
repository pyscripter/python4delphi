unit WrapFmxListView;

interface

uses
	FMX.ListView, WrapFmxControls, PythonEngine;

type
  TPyAdapterListView = class(TPyDelphiStyledControl)
	private
		function GetDelphiObject: TAdapterListView;
		procedure SetDelphiObject(const Value: TAdapterListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TAdapterListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyListViewBase = class(TPyAdapterListView)
	private
		function GetDelphiObject: TListViewBase;
		procedure SetDelphiObject(const Value: TListViewBase);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TListViewBase read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyPresentedListView = class(TPyListViewBase)
	private
		function GetDelphiObject: TPresentedListView;
		procedure SetDelphiObject(const Value: TPresentedListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TPresentedListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyAppearanceListView = class(TPyPresentedListView)
	private
		function GetDelphiObject: TAppearanceListView;
		procedure SetDelphiObject(const Value: TAppearanceListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TAppearanceListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyCustomListView = class(TPyAppearanceListView)
	private
		function GetDelphiObject: TCustomListView;
		procedure SetDelphiObject(const Value: TCustomListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyListView = class(TPyAppearanceListView)
	private
		function GetDelphiObject: TListView;
		procedure SetDelphiObject(const Value: TListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TListView read GetDelphiObject
			write SetDelphiObject;
	end;

implementation

uses
	WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
	TListViewRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TListViewRegistration }

procedure TListViewRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TListViewRegistration.Name: string;
begin
  Result := 'ListView';
end;

procedure TListViewRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyAdapterListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyListViewBase);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyPresentedListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyAppearanceListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyCustomListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyListView);
end;

{ TPyAdapterListView }

class function TPyAdapterListView.DelphiObjectClass: TClass;
begin
	Result := TAdapterListView;
end;

function TPyAdapterListView.GetDelphiObject: TAdapterListView;
begin
	Result := TAdapterListView(inherited DelphiObject);
end;

procedure TPyAdapterListView.SetDelphiObject(const Value: TAdapterListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyListViewBase }

class function TPyListViewBase.DelphiObjectClass: TClass;
begin
	Result := TListViewBase;
end;

function TPyListViewBase.GetDelphiObject: TListViewBase;
begin
	Result := TListViewBase(inherited DelphiObject);
end;

procedure TPyListViewBase.SetDelphiObject(const Value: TListViewBase);
begin
	inherited DelphiObject := Value;
end;

{ TPyPresentedListView }

class function TPyPresentedListView.DelphiObjectClass: TClass;
begin
	Result := TPresentedListView;
end;

function TPyPresentedListView.GetDelphiObject: TPresentedListView;
begin
	Result := TPresentedListView(inherited DelphiObject);
end;

procedure TPyPresentedListView.SetDelphiObject(const Value: TPresentedListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyAppearanceListView }

class function TPyAppearanceListView.DelphiObjectClass: TClass;
begin
	Result := TAppearanceListView;
end;

function TPyAppearanceListView.GetDelphiObject: TAppearanceListView;
begin
	Result := TAppearanceListView(inherited DelphiObject);
end;

procedure TPyAppearanceListView.SetDelphiObject(
  const Value: TAppearanceListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyCustomListView }

class function TPyCustomListView.DelphiObjectClass: TClass;
begin
	Result := TCustomListView;
end;

function TPyCustomListView.GetDelphiObject: TCustomListView;
begin
	Result := TCustomListView(inherited DelphiObject);
end;

procedure TPyCustomListView.SetDelphiObject(const Value: TCustomListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyListView }

class function TPyListView.DelphiObjectClass: TClass;
begin
	Result := TListView;
end;

function TPyListView.GetDelphiObject: TListView;
begin
	Result := TListView(inherited DelphiObject);
end;

procedure TPyListView.SetDelphiObject(const Value: TListView);
begin
	inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TListViewRegistration.Create);

end.
