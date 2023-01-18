unit WrapActions;

interface

uses
  System.Classes,
  System.Actions,
  System.TypInfo,
  PythonEngine,
  WrapDelphi,
  WrapDelphiClasses;

type
  TPyDelphiContainedAction = class(TPyDelphiBasicAction)
  private
    function GetDelphiObject: TContainedAction;
    procedure SetDelphiObject(const Value: TContainedAction);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TContainedAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TActionEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Action: TBasicAction; var Handled: Boolean);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TActionListAccess = class(TContainerAccess)
  private
    function GetContainer: TContainedActionList;
  public
    function GetItem(AIndex: Integer): PPyObject; override;
    function GetSize: Integer; override;
    function IndexOf(AValue: PPyObject): Integer; override;
    class function ExpectedContainerClass: TClass; override;
    class function SupportsIndexOf: Boolean; override;
    class function Name: string; override;
    property Container: TContainedActionList read GetContainer;
  end;

  TPyDelphiContainedActionList = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TContainedActionList;
    procedure SetDelphiObject(const Value: TContainedActionList);
  protected
    function Get_ActionCount(AContext: Pointer): PPyObject; cdecl;
    function Get_Actions(AContext: Pointer): PPyObject; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class function GetContainerAccessClass: TContainerAccessClass; override;

    property DelphiObject: TContainedActionList read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

type
  TActionsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TClassesRegistration }

function TActionsRegistration.Name: string;
begin
  Result := 'Actions';
end;

procedure TActionsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiContainedAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiContainedActionList);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TActionEventHandler);
end;

{ TPyDelphiContainedAction }

class function TPyDelphiContainedAction.DelphiObjectClass: TClass;
begin
  Result := TContainedAction;
end;

function TPyDelphiContainedAction.GetDelphiObject: TContainedAction;
begin
  Result := TContainedAction(inherited DelphiObject);
end;

procedure TPyDelphiContainedAction.SetDelphiObject
  (const Value: TContainedAction);
begin
  inherited DelphiObject := Value;
end;

{ TActionListAccess }

class function TActionListAccess.ExpectedContainerClass: TClass;
begin
  Result := TContainedActionList;
end;

function TActionListAccess.GetContainer: TContainedActionList;
begin
  Result := TContainedActionList(inherited Container);
end;

function TActionListAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.Actions[AIndex]);
end;

function TActionListAccess.GetSize: Integer;
begin
  Result := Container.ActionCount;
end;

function TActionListAccess.IndexOf(AValue: PPyObject): Integer;
var
  _obj: TPyObject;
  _item: TContainedAction;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if (_obj is TPyDelphiObject) and
        (TPyDelphiObject(_obj).DelphiObject is TContainedAction) then
      begin
        _item := TContainedAction(TPyDelphiObject(_obj).DelphiObject);
        Result := _item.Index;
      end;
    end;
  end;
end;

class function TActionListAccess.Name: string;
begin
  Result := 'TContainedActionList.Actions';
end;

class function TActionListAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;


{ TPyDelphiContainedActionList }

class function TPyDelphiContainedActionList.DelphiObjectClass: TClass;
begin
  Result := TContainedActionList;
end;

class function TPyDelphiContainedActionList.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TActionListAccess;
end;

function TPyDelphiContainedActionList.GetDelphiObject: TContainedActionList;
begin
  Result := TContainedActionList(inherited DelphiObject);
end;

function TPyDelphiContainedActionList.Get_ActionCount(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.ActionCount);
end;

function TPyDelphiContainedActionList.Get_Actions(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TActionListAccess.Create(Self.PyDelphiWrapper,
      Self.DelphiObject));
end;

class procedure TPyDelphiContainedActionList.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
  begin
    AddGetSet('ActionCount', @TPyDelphiContainedActionList.Get_ActionCount, nil,
      'Indicates the number of actions in the action list.', nil);
    AddGetSet('Actions', @TPyDelphiContainedActionList.Get_Actions, nil,
      'Lists the actions maintained by the action list.', nil);
  end;
end;

procedure TPyDelphiContainedActionList.SetDelphiObject(
  const Value: TContainedActionList);
begin
  inherited DelphiObject := Value;
end;

{ TActionEventHandler }

constructor TActionEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TActionEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TActionEventHandler.DoEvent(Action: TBasicAction; var Handled: Boolean);
var
  PyAction, PyTuple, PyResult, PyHandled : PPyObject;
  LVarParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PyAction := PyDelphiWrapper.Wrap(Action);
      PyHandled := CreateVarParam(PyDelphiWrapper, Handled);
      LVarParam := PythonToDelphi(PyHandled) as TPyDelphiVarParameter;
      PyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyAction);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyHandled);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        if Assigned(PyResult) then
        begin
          Py_DECREF(PyResult);
          Handled := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TActionEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TActionEvent);
end;

initialization
  RegisteredUnits.Add(TActionsRegistration.Create());

end.
