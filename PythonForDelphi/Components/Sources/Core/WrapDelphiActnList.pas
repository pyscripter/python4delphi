{$I Definition.Inc}

unit WrapDelphiActnList;

interface

uses
  Classes, PythonEngine, WrapDelphi, WrapDelphiClasses, ActnList;

type
  {
    Access to the Action items of the TCustomActionList collection.
  }
  TActionListAccess = class(TContainerAccess)
  private
    function GetContainer: TCustomActionList;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;

    property Container : TCustomActionList read GetContainer;
  end;

  {
     PyObject wrapping TCustomActionList
     Exposes properties ActionCount and Actions
  }
  TPyDelphiCustomActionList = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TCustomActionList;
    procedure SetDelphiObject(const Value: TCustomActionList);
  protected
    // property getters
    function Get_ActionCount(AContext : Pointer) : PPyObject; cdecl;
    function Get_Actions(AContext : Pointer) : PPyObject; cdecl;
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Properties
    property DelphiObject: TCustomActionList read GetDelphiObject write SetDelphiObject;
  end;


implementation

{ Register the wrappers, the globals and the constants }
type
  TActnListRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TActnListRegistration }

procedure TActnListRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TActnListRegistration.Name: String;
begin
  Result := 'ActnList';
end;

procedure TActnListRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomActionList);
end;

{ TPyDelphiCustomActionList }

class function TPyDelphiCustomActionList.DelphiObjectClass: TClass;
begin
  Result := TCustomActionList;
end;

class function TPyDelphiCustomActionList.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TActionListAccess;
end;

function TPyDelphiCustomActionList.GetDelphiObject: TCustomActionList;
begin
  Result := TCustomActionList(inherited DelphiObject);
end;

function TPyDelphiCustomActionList.Get_ActionCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.ActionCount);
end;

function TPyDelphiCustomActionList.Get_Actions(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TActionListAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

class procedure TPyDelphiCustomActionList.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('ActionCount', @TPyDelphiCustomActionList.Get_ActionCount, nil,
        'Indicates the number of actions in the action list.', nil);
      AddGetSet('Actions', @TPyDelphiCustomActionList.Get_Actions, nil,
        'Lists the actions maintained by the action list.', nil);
    end;
end;

procedure TPyDelphiCustomActionList.SetDelphiObject(
  const Value: TCustomActionList);
begin
  inherited DelphiObject := Value;
end;

{ TActionListAccess }

class function TActionListAccess.ExpectedContainerClass: TClass;
begin
  Result := TCustomActionList;
end;

function TActionListAccess.GetContainer: TCustomActionList;
begin
  Result := TCustomActionList(inherited Container);
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
  i : Integer;
  _obj : TPyObject;
  _item : TBasicAction;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if (_obj is TPyDelphiObject) and (TPyDelphiObject(_obj).DelphiObject is TBasicAction) then
      begin
        _item := TBasicAction(TPyDelphiObject(_obj).DelphiObject);
        for i := 0 to Container.ActionCount-1 do
          if Container.Actions[i] = _item then
          begin
            Result := i;
            Break;
          end;
      end;
    end;
  end;
end;

class function TActionListAccess.Name: String;
begin
  Result := 'TCustomActionList.Actions';
end;

class function TActionListAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;


initialization
  RegisteredUnits.Add( TActnListRegistration.Create );
end.
