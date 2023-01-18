unit WrapFmxListView;

interface

uses
	System.TypInfo, FMX.ListView, FMX.ListView.Types, FMX.ListView.Appearances,
  PythonEngine, WrapDelphi, WrapFmxControls;

type
  TPyDelphiAdapterListView = class(TPyDelphiStyledControl)
	private
		function GetDelphiObject: TAdapterListView;
		procedure SetDelphiObject(const Value: TAdapterListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TAdapterListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiListViewBase = class(TPyDelphiAdapterListView)
	private
		function GetDelphiObject: TListViewBase;
		procedure SetDelphiObject(const Value: TListViewBase);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TListViewBase read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiPresentedListView = class(TPyDelphiListViewBase)
	private
		function GetDelphiObject: TPresentedListView;
		procedure SetDelphiObject(const Value: TPresentedListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TPresentedListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiAppearanceListView = class(TPyDelphiPresentedListView)
	private
		function GetDelphiObject: TAppearanceListView;
		procedure SetDelphiObject(const Value: TAppearanceListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TAppearanceListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiCustomListView = class(TPyDelphiAppearanceListView)
	private
		function GetDelphiObject: TCustomListView;
		procedure SetDelphiObject(const Value: TCustomListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TCustomListView read GetDelphiObject
			write SetDelphiObject;
	end;

  TPyDelphiListView = class(TPyDelphiCustomListView)
	private
		function GetDelphiObject: TListView;
		procedure SetDelphiObject(const Value: TListView);
	public
		class function DelphiObjectClass: TClass; override;
		property DelphiObject: TListView read GetDelphiObject
			write SetDelphiObject;
	end;

  //Events
  TItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(const Sender: TObject; const AItem: TListViewItem);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

implementation

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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiAdapterListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListViewBase);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiAppearanceListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListView);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TItemEventHandler);
end;

{ TPyDelphiAdapterListView }

class function TPyDelphiAdapterListView.DelphiObjectClass: TClass;
begin
	Result := TAdapterListView;
end;

function TPyDelphiAdapterListView.GetDelphiObject: TAdapterListView;
begin
	Result := TAdapterListView(inherited DelphiObject);
end;

procedure TPyDelphiAdapterListView.SetDelphiObject(const Value: TAdapterListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiListViewBase }

class function TPyDelphiListViewBase.DelphiObjectClass: TClass;
begin
	Result := TListViewBase;
end;

function TPyDelphiListViewBase.GetDelphiObject: TListViewBase;
begin
	Result := TListViewBase(inherited DelphiObject);
end;

procedure TPyDelphiListViewBase.SetDelphiObject(const Value: TListViewBase);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedListView }

class function TPyDelphiPresentedListView.DelphiObjectClass: TClass;
begin
	Result := TPresentedListView;
end;

function TPyDelphiPresentedListView.GetDelphiObject: TPresentedListView;
begin
	Result := TPresentedListView(inherited DelphiObject);
end;

procedure TPyDelphiPresentedListView.SetDelphiObject(const Value: TPresentedListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiAppearanceListView }

class function TPyDelphiAppearanceListView.DelphiObjectClass: TClass;
begin
	Result := TAppearanceListView;
end;

function TPyDelphiAppearanceListView.GetDelphiObject: TAppearanceListView;
begin
	Result := TAppearanceListView(inherited DelphiObject);
end;

procedure TPyDelphiAppearanceListView.SetDelphiObject(
  const Value: TAppearanceListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiCustomListView }

class function TPyDelphiCustomListView.DelphiObjectClass: TClass;
begin
	Result := TCustomListView;
end;

function TPyDelphiCustomListView.GetDelphiObject: TCustomListView;
begin
	Result := TCustomListView(inherited DelphiObject);
end;

procedure TPyDelphiCustomListView.SetDelphiObject(const Value: TCustomListView);
begin
	inherited DelphiObject := Value;
end;

{ TPyDelphiListView }

class function TPyDelphiListView.DelphiObjectClass: TClass;
begin
	Result := TListView;
end;

function TPyDelphiListView.GetDelphiObject: TListView;
begin
	Result := TListView(inherited DelphiObject);
end;

procedure TPyDelphiListView.SetDelphiObject(const Value: TListView);
begin
	inherited DelphiObject := Value;
end;

{ TItemEventHandler }

constructor TItemEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TAppearanceListView.TItemEvent);
end;

procedure TItemEventHandler.DoEvent(const Sender: TObject;
  const AItem: TListViewItem);
var
  LPyObject: PPyObject;
  LPyItem: PPyObject;
  LPyTuple: PPyObject;
  LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(AItem);
      LPyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyItem);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        Py_XDECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

initialization
  RegisteredUnits.Add(TListViewRegistration.Create);

end.
