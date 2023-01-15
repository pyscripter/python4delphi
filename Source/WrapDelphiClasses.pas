{$I Definition.Inc}

unit WrapDelphiClasses;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi;

type
  {
     PyObject wrapping TPersistent
     Exposes Assign Method
  }
  TPyDelphiPersistent = class (TPyDelphiObject)
  private
    function  GetDelphiObject: TPersistent;
    procedure SetDelphiObject(const Value: TPersistent);
  protected
    // Exposed Methods
    function Assign_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function GetNamePath_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Virtual Methods
    function Assign(ASource : PPyObject) : PPyObject; virtual;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TPersistent read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the TCollectionItem items of a TCollection.
  }
  TCollectionAccess = class(TContainerAccess)
  private
    function GetContainer: TCollection;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;
    class function Name : string; override;

    property Container : TCollection read GetContainer;
  end;

  {
     PyObject wrapping TCollection
  }
  TPyDelphiCollection = class (TPyDelphiPersistent)
  private
    function  GetDelphiObject: TCollection;
    procedure SetDelphiObject(const Value: TCollection);
  protected
    // Exposed Methods
    function Insert_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Add_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Clear_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Delete_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BeginUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function EndUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_Count( AContext : Pointer) : PPyObject; cdecl;
    function Get_Items( AContext : Pointer) : PPyObject; cdecl;
    function Get_Owner( AContext : Pointer) : PPyObject; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Properties
    property DelphiObject: TCollection read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the items owned by a component.
  }
  TComponentsAccess = class(TContainerAccess)
  private
    function GetContainer: TComponent;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;

    property Container : TComponent read GetContainer;
  end;

  {
     PyObject wrapping TComponent
     Exposes read-only properties ComponentCount and Owner as well as
     sub-components as pseudo-properties
  }
  TPyDelphiComponent = class (TPyDelphiPersistent)
  private
    fFreeNotificationComp : TComponent;
    function  GetDelphiObject: TComponent;
    procedure SetDelphiObject(const Value: TComponent);
    procedure HandleFreeNotificationEvent(Sender: TObject; AComponent: TComponent);
  protected
    function  CreateComponent(AOwner : TComponent) : TComponent; virtual;
    procedure SubscribeToFreeNotification; override;
    procedure UnSubscribeToFreeNotification; override;
{$IFNDEF FPC}
    function InternalReadComponent(const AResFile: string;
      const AInstance: TComponent): boolean; virtual;
{$ENDIF}
    // Exposed Methods
    function GetParentComponent_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HasParent_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BindMethodsToEvents(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_ComponentCount( AContext : Pointer) : PPyObject; cdecl;
    function Get_Owner( AContext : Pointer) : PPyObject; cdecl;
    function Get_Components( AContext : Pointer) : PPyObject; cdecl;
  public
    constructor CreateWith( APythonType: TPythonType; args, kwds: PPyObject); override;
    destructor Destroy; override;

    function  GetAttrO( key: PPyObject) : PPyObject; override;
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Mapping services
    function  MpLength : NativeInt; override;
    function  MpSubscript( obj : PPyObject) : PPyObject; override;
    // Properties
    property DelphiObject: TComponent read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the string items of the TStrings collection.
  }
  TStringsAccess = class(TContainerAccess)
  private
    function GetContainer: TStrings;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function IndexOf(AValue : PPyObject) : Integer; override;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsIndexOf : Boolean; override;
    class function SupportsWrite : Boolean; override;

    property Container : TStrings read GetContainer;
  end;

  {
    Access to the TObject items of the TStrings.Objects collection.
  }
  TStringsObjectsAccess = class(TStringsAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function IndexOf(AValue : PPyObject) : Integer; override;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; override;
    class function Name : string; override;
  end;

  {
     PyObject wrapping TStrings
     Note that you can assign a Python sequence to a TStrings (X.Assign([1, 2, 3]))
     Note that X[1] will return a string, where as X['key'] will return the object associated
     with the string 'key'.
     Provides a mapping interface to a Delphi strings object
     Exposes Methods Add, AddObject, Delete, IndexOf and Clear
  }
  TPyDelphiStrings = class (TPyDelphiPersistent)
  private
    function  GetDelphiObject: TStrings;
    procedure SetDelphiObject(const Value: TStrings);
  protected
    // Exposed Methods
    function Add_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function AddObject_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Clear_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Delete_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function IndexOf_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BeginUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function EndUpdate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function LoadFromFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SaveToFile_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_Capacity( AContext : Pointer) : PPyObject; cdecl;
    function Get_Text( AContext : Pointer) : PPyObject; cdecl;
    function Get_Objects( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_Capacity( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    function Set_Text( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
    // Virtual Methods
    function Assign(ASource : PPyObject) : PPyObject; override;
  public
    function  Repr : PPyObject; override;
    // Mapping services
    function  MpLength : NativeInt; override;
    function  MpSubscript( obj : PPyObject) : PPyObject; override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
    class function  GetContainerAccessClass : TContainerAccessClass; override;
    // Properties
    property DelphiObject: TStrings read GetDelphiObject write SetDelphiObject;
  end;

  {
     PyObject wrapping TBasicAction
     Exposes methods Execute, Update
     Exposes property ActionComponent
  }
  TPyDelphiBasicAction = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TBasicAction;
    procedure SetDelphiObject(const Value: TBasicAction);
  protected
    // Exposed Methods
    function Execute_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Update_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_ActionComponent( AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_ActionComponent( AValue : PPyObject; AContext : Pointer) : integer; cdecl;
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TBasicAction read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStream = class(TPyDelphiObject)
  private
    function GetDelphiObject: TStream;
    procedure SetDelphiObject(const Value: TStream);

    function GetReadCount(const AArgs: PPyObject): integer; inline;
  protected
    function ReadBytes_Wrapper(const AArgs: PPyObject): PPyObject;
    function ReadInt_Wrapper(const AArgs: PPyObject): PPyObject;
    function ReadString_Wrapper(const AArgs: PPyObject): PPyObject;
    function ReadFloat_Wrapper(const AArgs: PPyObject): PPyObject;

    function WriteBytes_Wrapper(const AArgs: PPyObject): PPyObject;
    function WriteInt_Wrapper(const AArgs: PPyObject): PPyObject;
    function WriteString_Wrapper(const AArgs: PPyObject): PPyObject;
    function WriteFloat_Wrapper(const AArgs: PPyObject): PPyObject;
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure SetupType(PythonType: TPythonType); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiHandleStream = class(TPyDelphiStream)
  private
    function GetDelphiObject: THandleStream;
    procedure SetDelphiObject(const Value: THandleStream);
  public
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: THandleStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiFileStream = class(TPyDelphiHandleStream)
  private
    function GetDelphiObject: TFileStream;
    procedure SetDelphiObject(const Value: TFileStream);
  public
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TFileStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBufferedFileStream = class(TPyDelphiFileStream)
  private
    function GetDelphiObject: TBufferedFileStream;
    procedure SetDelphiObject(const Value: TBufferedFileStream);
  public
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBufferedFileStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomMemoryStream = class(TPyDelphiStream)
  private
    function GetDelphiObject: TCustomMemoryStream;
    procedure SetDelphiObject(const Value: TCustomMemoryStream);
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomMemoryStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMemoryStream = class(TPyDelphiCustomMemoryStream)
  private
    function GetDelphiObject: TMemoryStream;
    procedure SetDelphiObject(const Value: TMemoryStream);
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TMemoryStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiByteStream = class(TPyDelphiMemoryStream)
  private
    function GetDelphiObject: TBytesStream;
    procedure SetDelphiObject(const Value: TBytesStream);
  public
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBytesStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStringStream = class(TPyDelphiByteStream)
  private
    function GetDelphiObject: TStringStream;
    procedure SetDelphiObject(const Value: TStringStream);
  public
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TStringStream read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiResourceStream = class(TPyDelphiCustomMemoryStream)
  private
    function GetDelphiObject: TResourceStream;
    procedure SetDelphiObject(const Value: TResourceStream);
  public
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TResourceStream read GetDelphiObject write SetDelphiObject;
  end;

  { Helper functions }

  function ShiftToPython(AShift : TShiftState) : PPyObject;

implementation

uses
  TypInfo {$IFNDEF FPC}, System.Rtti{$ENDIF};

{$IFNDEF FPC}
type
  TPyReader = class(TReader)
  private
    FPyObject: TPyDelphiObject;
    FInstance: TComponent;
    procedure DoFind(Reader: TReader; const ClassName: string; var ComponentClass: TComponentClass);
  protected
    procedure SetName(Component: TComponent; var Name: string); override;
    function FindMethod(Root: TComponent; const AMethodName: string): Pointer; override;
  public
    constructor Create(APyObject: TPyDelphiObject; Stream: TStream; BufSize: Integer);
  end;
{$ENDIF}

{ Register the wrappers, the globals and the constants }
type
  TClassesRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TClassesRegistration }

procedure TClassesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('ssShift',  'ssShift');
  APyDelphiWrapper.DefineVar('ssAlt',    'ssAlt');
  APyDelphiWrapper.DefineVar('ssCtrl',   'ssCtrl');
  APyDelphiWrapper.DefineVar('ssLeft',   'ssLeft');
  APyDelphiWrapper.DefineVar('ssRight',  'ssRight');
  APyDelphiWrapper.DefineVar('ssMiddle', 'ssMiddle');
  APyDelphiWrapper.DefineVar('ssDouble', 'ssDouble');
  //TFileStream mode
  APyDelphiWrapper.DefineVar('fmCreate', fmCreate);
  APyDelphiWrapper.DefineVar('fmOpenRead', fmOpenRead);
  APyDelphiWrapper.DefineVar('fmOpenWrite', fmOpenWrite);
  APyDelphiWrapper.DefineVar('fmOpenReadWrite', fmOpenReadWrite);
  {$WARN SYMBOL_PLATFORM OFF}
  {$IFDEF MSWINDOWS}
  APyDelphiWrapper.DefineVar('fmShareCompat', fmShareCompat);
  {$ENDIF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM ON}
  APyDelphiWrapper.DefineVar('fmShareExclusive', fmShareExclusive);
  APyDelphiWrapper.DefineVar('fmShareDenyWrite', fmShareDenyWrite);
  {$WARN SYMBOL_PLATFORM OFF}
  {$IFDEF MSWINDOWS}
  APyDelphiWrapper.DefineVar('fmShareDenyRead', fmShareDenyRead);
  {$ENDIF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM ON}
  APyDelphiWrapper.DefineVar('fmShareDenyNone', fmShareDenyNone);
end;

function TClassesRegistration.Name: string;
begin
  Result := 'Classes';
end;

procedure TClassesRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPersistent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCollection);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiComponent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStrings);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBasicAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiHandleStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFileStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBufferedFileStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomMemoryStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMemoryStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiByteStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStringStream);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiResourceStream);
end;

{ Helper functions }

function ShiftToPython(AShift : TShiftState) : PPyObject;

  procedure Append(AList : PPyObject; const AString : string);
  var
    _item : PPyObject;
  begin
    with GetPythonEngine do
    begin
      _item := PyUnicodeFromString(AString);
      PyList_Append(AList, _item);
      Py_XDecRef(_item);
    end;
  end;

begin
  with GetPythonEngine do
  begin
    Result := PyList_New(0);
    if ssShift in AShift then
      Append(Result, 'ssShift');
    if ssAlt in AShift then
      Append(Result, 'ssAlt');
    if ssCtrl in AShift then
      Append(Result, 'ssCtrl');
    if ssLeft in AShift then
      Append(Result, 'ssLeft');
    if ssRight in AShift then
      Append(Result, 'ssRight');
    if ssMiddle in AShift then
      Append(Result, 'ssMiddle');
    if ssDouble in AShift then
      Append(Result, 'ssDouble');
  end;
end;

{ TPyDelphiPersistent }

function TPyDelphiPersistent.Assign(ASource: PPyObject): PPyObject;
var
  _object : TObject;
begin
  if CheckObjAttribute(ASource, 'First parameter', TPersistent, _object) then
  begin
    DelphiObject.Assign(TPersistent(_object));
    Result := GetPythonEngine.ReturnNone;
  end
  else
    Result := nil;
end;

function TPyDelphiPersistent.Assign_Wrapper(args: PPyObject): PPyObject;
var
  _obj : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'O:Assign',@_obj ) <> 0 then
    Result := Self.Assign(_obj)
  else
    Result := nil;
end;

class function TPyDelphiPersistent.DelphiObjectClass: TClass;
begin
  Result := TPersistent;
end;

function TPyDelphiPersistent.GetDelphiObject: TPersistent;
begin
  Result := TPersistent(inherited DelphiObject);
end;

function TPyDelphiPersistent.GetNamePath_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':GetNamePath' ) <> 0 then begin
      Result := PyUnicodeFromString(DelphiObject.GetNamePath)
    end else
      Result := nil;
  end;
end;

class procedure TPyDelphiPersistent.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('Assign', @TPyDelphiPersistent.Assign_Wrapper,
    'TPersistent.Assign(persistent)'#10 +
    'Assigns to this object the values of another TPersistent object');
  PythonType.AddMethod('GetNamePath', @TPyDelphiPersistent.GetNamePath_Wrapper,
    'TPersistent.GetNamePath()'#10 +
    'Returns the name of the object as it appears in the Object Inspector.');
end;

procedure TPyDelphiPersistent.SetDelphiObject(const Value: TPersistent);
begin
  inherited DelphiObject := Value;
end;

{ TCollectionAccess }

class function TCollectionAccess.ExpectedContainerClass: TClass;
begin
  Result := TCollection;
end;

function TCollectionAccess.GetContainer: TCollection;
begin
  Result := TCollection(inherited Container);
end;

function TCollectionAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Items[AIndex] );
end;

function TCollectionAccess.GetSize: Integer;
begin
  Result := Container.Count;
end;

function TCollectionAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  _obj : TPyObject;
  _item : TCollectionItem;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if PyLong_Check(AValue) then
    begin
      _item := Container.FindItemID(PyLong_AsLong(AValue));
      if Assigned(_item) then
        Result := _item.Index;
    end
    else if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if (_obj is TPyDelphiObject) and (TPyDelphiObject(_obj).DelphiObject is TCollectionItem) then
      begin
        _item := TCollectionItem(TPyDelphiObject(_obj).DelphiObject);
        for i := 0 to Container.Count-1 do
          if Container.Items[i] = _item then
          begin
            Result := i;
            Break;
          end;
      end;
    end;
  end;
end;

class function TCollectionAccess.Name: string;
begin
  Result := 'TCollection.Items';
end;

class function TCollectionAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TPyDelphiCollection }

function TPyDelphiCollection.Add_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':Add' ) <> 0 then
    Result := Wrap(DelphiObject.Add)
  else
    Result := nil;
end;

function TPyDelphiCollection.BeginUpdate_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':BeginUpdate' ) <> 0 then begin
    DelphiObject.BeginUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiCollection.Clear_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':Clear') <> 0 then begin
    (DelphiObject as TCollection).Clear;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiCollection.Delete_Wrapper(args: PPyObject): PPyObject;
Var
  Index : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:Delete',@Index ) <> 0 then
  begin
    if not CheckIndex(Index, DelphiObject.Count) then
      Result := nil
    else
    begin
      DelphiObject.Delete(Index);
      Result := GetPythonEngine.ReturnNone;
    end;
  end
  else
    Result := nil;
end;

class function TPyDelphiCollection.DelphiObjectClass: TClass;
begin
  Result := TCollection;
end;

function TPyDelphiCollection.EndUpdate_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':EndUpdate') <> 0 then begin
    DelphiObject.EndUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

class function TPyDelphiCollection.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TCollectionAccess;
end;

function TPyDelphiCollection.GetDelphiObject: TCollection;
begin
  Result := TCollection(inherited DelphiObject);
end;

function TPyDelphiCollection.Get_Count(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.Count);
end;

function TPyDelphiCollection.Get_Items(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, Self.ContainerAccess.Clone);
end;

function TPyDelphiCollection.Get_Owner(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Owner);
end;

function TPyDelphiCollection.Insert_Wrapper(args: PPyObject): PPyObject;
Var
  Index : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:Insert',@Index ) <> 0 then
    Result := Wrap(DelphiObject.Insert(Index))
  else
    Result := nil;
end;

class procedure TPyDelphiCollection.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do
    begin
      AddGetSet('Count', @TPyDelphiCollection.Get_Count, nil,
        'Returns the count of collection items', nil);
      AddGetSet('Items', @TPyDelphiCollection.Get_Items, nil,
        'Returns an iterator over the collection items', nil);
      AddGetSet('Owner', @TPyDelphiCollection.Get_Owner, nil,
        'Returns the Owner of the collection', nil);
    end;
end;

class procedure TPyDelphiCollection.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('Insert', @TPyDelphiCollection.Insert_Wrapper,
    'TCollection.Insert(Index)'#10 +
    'Inserts a new collection item to the collection at the Index position');
  PythonType.AddMethod('Add', @TPyDelphiCollection.Add_Wrapper,
    'TCollection.Add()'#10 +
    'Adds a collection item to the collection');
  PythonType.AddMethod('Clear', @TPyDelphiCollection.Clear_Wrapper,
    'TCollection.Clear()'#10 +
    'Clears all collection items');
  PythonType.AddMethod('Delete', @TPyDelphiCollection.Delete_Wrapper,
    'TCollection.Delete(Index)'#10 +
    'Deletes a single item from the collection.');
  PythonType.AddMethod('BeginUpdate', @TPyDelphiCollection.BeginUpdate_Wrapper,
    'TCollection.BeginUpdate()'#10 +
    'Suspends screen repainting.');
  PythonType.AddMethod('EndUpdate', @TPyDelphiCollection.EndUpdate_Wrapper,
    'TCollection.EndUpdate()'#10 +
    'Re-enables screen repainting.');
end;

procedure TPyDelphiCollection.SetDelphiObject(const Value: TCollection);
begin
  inherited DelphiObject := Value;
end;

{ TComponentsAccess }

class function TComponentsAccess.ExpectedContainerClass: TClass;
begin
  Result := TComponent;
end;

function TComponentsAccess.GetContainer: TComponent;
begin
  Result := TComponent(inherited Container);
end;

function TComponentsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Components[AIndex] );
end;

function TComponentsAccess.GetSize: Integer;
begin
  Result := Container.ComponentCount;
end;

function TComponentsAccess.IndexOf(AValue: PPyObject): Integer;
Var
  i : Integer;
  S : string;
  _obj : TPyObject;
  _value : TObject;
  _comp : TComponent;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if PyUnicode_Check(AValue) then
    begin
      S := PyUnicodeAsString(AValue);
      for i := 0 to Container.ComponentCount-1 do
        if SameText( Container.Components[i].Name, S) then
        begin
          Result := i;
          Break;
        end;
    end
    else if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if _obj is TPyDelphiObject then
      begin
        _value := TPyDelphiObject(_obj).DelphiObject;
        if _value is TComponent then
        begin
          _comp := TComponent(_value);
          for i := 0 to Container.ComponentCount-1 do
            if Container.Components[i] = _comp then
            begin
              Result := i;
              Break;
            end;
        end;
      end;
    end;
  end;
end;

class function TComponentsAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TPyDelphiObjectNexus }
{ used by TPyDelphiObject to get free notification }

type
  TPyDelphiObjectNexusEvent = procedure(Sender: TObject; AComponent: TComponent) of object;
  TPyDelphiObjectNexus = class(TComponent)
  private
    FOnFreeNotify: TPyDelphiObjectNexusEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property OnFreeNotify: TPyDelphiObjectNexusEvent read FOnFreeNotify write FOnFreeNotify;
  end;

{ TPyDelphiObjectNexus }

procedure TPyDelphiObjectNexus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Assigned(FOnFreeNotify) then
    FOnFreeNotify(Self, AComponent);
  inherited Notification(AComponent, Operation);
end;

{ TPyDelphiComponent }

function TPyDelphiComponent.BindMethodsToEvents(args: PPyObject): PPyObject;
var
  i : Integer;
  j : Integer;
  d : PPyObject;
  s : PPyObject;
  obj : PPyObject;
  objMethod : PPyObject;
  objComp : PPyObject;
  key : PPyObject;
  keys : PPyObject;
  _idx : Integer;
  _name : string;
  _prefix : string;
  _compName : string;
  _eventName : string;
  _comp : TComponent;
  _pair : PPyObject;
  _bindings : PPyObject;
  _type : PPyTypeObject;
begin
  Adjust(@Self);
  _prefix := 'handle_';
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Result := nil;
    s := nil;
    if PyArg_ParseTuple( args, '|O:BindMethodsToEvents',@s ) <> 0 then
    begin
      if Assigned(S) then
        _prefix := PyObjectAsString(s);
      _bindings := PyList_New(0);
      try
        _type := GetSelf.ob_type;
        while _type <> nil do
        begin
          d := _type.tp_dict;
          if Assigned(d) and PyDict_Check(d) then
          begin
            keys := PyDict_Keys(d);
            try
              if PySequence_Check(keys) = 1 then
                for i := 0 to PySequence_Length(keys)-1 do
                begin
                  key := PySequence_GetItem(keys, i);
                  obj := PyDict_GetItem(d, key); // borrowed ref
                  objComp := nil;
                  try
                    if PyCallable_Check(obj) = 1 then
                    begin
                      _name := PyObjectAsString(key);
                      if SameText(Copy(_name, 1, Length(_prefix)), _prefix) then
                      begin
                        System.Delete(_name, 1, Length(_prefix));
                        _idx := -1;
                        for j := Length(_name) downto 1 do
                          if _name[j] = '_' then
                          begin
                            _idx := j;
                            Break;
                          end;
                        if _idx > -1 then
                        begin
                          _compName := Copy(_name, 1, _idx-1);
                          _eventName := Copy(_name, _idx+1, MaxInt);
                          if SameText(_compName, 'Self') then
                          begin
                            _comp := Self.DelphiObject;
                            objComp := GetSelf;
                            Py_IncRef(objComp);
                          end
                          else
                          begin
                            _comp := Self.DelphiObject.FindComponent(_compName);
                            if Assigned(_comp) then
                              objComp := Wrap(_comp);
                          end;
                          if not Assigned(_comp) and not Assigned(objComp) then
                          begin
                            objComp := PyObject_GetAttrString(GetSelf, PAnsiChar(AnsiString(_compName)));
                            if Assigned(objComp) then
                            begin
                              if IsDelphiObject(objComp) and (PythonToDelphi(objComp) is TPyDelphiComponent) then
                                _comp := TPyDelphiComponent(PythonToDelphi(objComp)).DelphiObject;
                            end
                            else
                              PyErr_Clear;
                          end;
                          if Assigned(_comp) and Assigned(objComp) and IsPublishedProp(_comp, _eventName) then
                          begin
                            objMethod := PyObject_GenericGetAttr(GetSelf, key);
                            try
                              if PyErr_Occurred <> nil then
                                Exit;
                              PyObject_SetAttrString(objComp, PAnsiChar(AnsiString(_eventName)), objMethod);
                              if PyErr_Occurred <> nil then
                                Exit
                              else
                              begin
                                _pair := PyTuple_New(3);
                                PyTuple_SetItem(_pair, 0, PyUnicodeFromString(_compName));
                                PyTuple_SetItem(_pair, 1, PyUnicodeFromString(_eventName));
                                PyTuple_SetItem(_pair, 2, objMethod);
                                PyList_Append(_bindings, _pair);
                              end;
                            finally
                              Py_XDecRef(objMethod);
                            end;
                          end;
                        end;
                      end;
                    end;
                  finally
                    Py_XDecRef(objComp);
                    Py_DecRef(key);
                  end;
                end; // for
            finally
              Py_DecRef(keys);
            end;
          end;
          _type := _type.tp_base;
        end;
        Result := _bindings;
        _bindings := nil;
      finally
        Py_XDecRef(_bindings);
      end;
    end;
  end;
end;

function TPyDelphiComponent.Get_ComponentCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.ComponentCount);
end;

function TPyDelphiComponent.Get_Components(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, Self.ContainerAccess.Clone);
end;

function TPyDelphiComponent.Get_Owner(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Owner);
end;

function TPyDelphiComponent.GetParentComponent_Wrapper(
  args: PPyObject): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    if PyArg_ParseTuple( args, ':GetParentComponent') <> 0 then begin
      Result := Wrap(DelphiObject.GetParentComponent)
    end else
      Result := nil;
  end;
end;

function TPyDelphiComponent.HasParent_Wrapper(args: PPyObject): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    if PyArg_ParseTuple( args, ':HasParent') <> 0 then begin
      Result := VariantAsPyObject(DelphiObject.HasParent)
    end else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiComponent.InternalReadComponent(const AResFile: string;
  const AInstance: TComponent): boolean;

  procedure ReadRootComponent(const AStream: TStream);
  var
    LReader: TPyReader;
  begin
    AStream.Position := 0;
    LReader := TPyReader.Create(Self, AStream, 4096);
    try
      LReader.ReadRootComponent(DelphiObject);
    finally
      LReader.Free;
    end;
  end;

  function HasValidSignature(const AStream: TStream): boolean;
  const
    FilerSignature: UInt32 = $30465054; // ($54, $50, $46, $30) 'TPF0'
  var
    LSignature: UInt32;
    LReader : TReader;
  begin
    AStream.Position := 0;
    LReader := TReader.Create(AStream, AStream.Size);
    try
      LReader.Read(LSignature, SizeOf(LSignature));
      Result := (LSignature = FilerSignature);
      AStream.Position := 0;
    finally
      LReader.Free();
    end;
  end;

var
  LInput: TFileStream;
  LOutput: TMemoryStream;
begin
  if AResFile.IsEmpty or not FileExists(AResFile) then
    Exit(false);

  LInput := TFileStream.Create(AResFile, fmOpenRead);
  try
    //The current form file is a valid binary file
    if HasValidSignature(LInput) then
      ReadRootComponent(LInput)
    else begin
      LOutput := TMemoryStream.Create();
      try
        //we assume the form file is a text file, then we try to get the bin info
        ObjectTextToBinary(LInput, LOutput);
        if HasValidSignature(LOutput) then
          ReadRootComponent(LOutput)
        else
          Exit(false);
      finally
        LOutput.Free();
      end;
    end;
  finally
    LInput.Free();
  end;
  Result := true;
end;
{$ENDIF}

function TPyDelphiComponent.GetAttrO(key: PPyObject): PPyObject;
Var
  Component: TComponent;
  Name: string;
begin
  Result := nil;
  if Assigned(DelphiObject) then
  begin
    if GetPythonEngine.PyUnicode_Check(Key) then
    begin
      Name := GetPythonEngine.PyUnicodeAsString(Key);
      // try a sub component
      Component := DelphiObject.FindComponent(Name);
      if Component <> nil then
        Result := Wrap(Component);
    end;
  end;

  if not Assigned(Result) then
    Result := inherited GetAttrO(key);
end;

function TPyDelphiComponent.GetDelphiObject: TComponent;
begin
  Result := TComponent(inherited DelphiObject);
end;

procedure TPyDelphiComponent.SetDelphiObject(const Value: TComponent);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiComponent.RegisterGetSets(
  PythonType: TPythonType);
begin
  with PythonType do
    begin
      AddGetSet('ComponentCount', @TPyDelphiComponent.Get_ComponentCount, nil,
        'Returns the owned component count', nil);
      AddGetSet('Owner', @TPyDelphiComponent.Get_Owner, nil,
        'Returns the Component Owner', nil);
      AddGetSet('Components', @TPyDelphiComponent.Get_Components, nil,
        'Returns an iterator over the owned components', nil);
    end;
end;

class procedure TPyDelphiComponent.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('GetParentComponent', @TPyDelphiComponent.GetParentComponent_Wrapper,
    'TComponent.GetParentComponent()'#10 +
    'Returns the parent of a component.');
  PythonType.AddMethod('HasParent', @TPyDelphiComponent.HasParent_Wrapper,
    'TComponent.HasParent()'#10 +
    'Indicates whether the component has a parent to handle its filing.');
  PythonType.AddMethod('BindMethodsToEvents', @TPyDelphiComponent.BindMethodsToEvents,
    'TComponent.BindMethodsToEvents(prefix)'#10 +
    'Connects methods to component events if they are named using the following pattern: Prefix_ComponentName_EventName.'+#10+
    'Example: def handle_button1_OnClick(Sender): pass'+#10+
    'The function returns a list of tuples. Each tuple contains the name of the component, the name of the event and the method object assigned to the event.'+#10+
    'Note that the prefix parameter is optional and will default to "handle_".');
end;

class procedure TPyDelphiComponent.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Services.Mapping  := PythonType.Services.Mapping + [msLength, msSubscript];
end;

class function TPyDelphiComponent.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TComponentsAccess;
end;

function TPyDelphiComponent.MpLength: NativeInt;
begin
  Result := SqLength;
end;

function TPyDelphiComponent.MpSubscript(obj: PPyObject): PPyObject;
var
  _name : UnicodeString;
  _comp : TComponent;
begin
  with GetPythonEngine do
  begin
    if PyLong_Check(obj) then
      Result := SqItem(PyLong_AsLong(obj))
    else if PyUnicode_Check(obj) then
    begin
      _name := PyUnicodeAsString(obj);
      _comp := DelphiObject.FindComponent(_name);
      if Assigned(_comp) then
        Result := Wrap(_comp)
      else
      begin
        Result := nil;
        PyErr_SetString (PyExc_KeyError^, PAnsiChar(AnsiString(_name)));
      end;
    end
    else
    begin
      Result := nil;
      PyErr_SetString (PyExc_KeyError^, 'Key must be a string');
    end;
  end;
end;

procedure TPyDelphiComponent.SubscribeToFreeNotification;
begin
  Assert(Assigned(DelphiObject));
  if not Assigned(fFreeNotificationComp) then
  begin
    fFreeNotificationComp := TPyDelphiObjectNexus.Create(nil);
    TPyDelphiObjectNexus(fFreeNotificationComp).OnFreeNotify := HandleFreeNotificationEvent;
  end;
  DelphiObject.FreeNotification(fFreeNotificationComp);
end;

procedure TPyDelphiComponent.UnSubscribeToFreeNotification;
begin
  Assert(Assigned(DelphiObject));
  if Assigned(fFreeNotificationComp) then
    DelphiObject.RemoveFreeNotification(fFreeNotificationComp);
end;

function TPyDelphiComponent.CreateComponent(AOwner: TComponent): TComponent;
begin
  Result := TComponentClass(DelphiObjectClass).Create(AOwner);
end;

constructor TPyDelphiComponent.CreateWith(APythonType: TPythonType; args, kwds:
    PPyObject);
var
  _obj : PPyObject;
  _owner : TObject;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple( args, 'O:Create',@_obj ) <> 0 then
  begin
    _owner := nil;
    if CheckObjAttribute(_obj, 'Owner', TComponent, _owner) then
    begin
      DelphiObject := CreateComponent(TComponent(_owner));
      Owned := not Assigned(_owner);
    end;
  end;
end;

destructor TPyDelphiComponent.Destroy;
begin
  inherited;
  fFreeNotificationComp.Free; // Free fFreeNotificationComp after inherited, because inherited will do DelphiObject := nil which call UnsubscribeFreeNotification
end;

procedure TPyDelphiComponent.HandleFreeNotificationEvent(Sender: TObject;
  AComponent: TComponent);
begin
  Notify(AComponent);
end;

class function TPyDelphiComponent.DelphiObjectClass: TClass;
begin
  Result := TComponent;
end;

{ TStringsAccess }

class function TStringsAccess.ExpectedContainerClass: TClass;
begin
  Result := TStrings;
end;

function TStringsAccess.GetContainer: TStrings;
begin
  Result := TStrings(inherited Container);
end;

function TStringsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString( Container[AIndex] );
end;

function TStringsAccess.GetSize: Integer;
begin
  Result := Container.Count;
end;

function TStringsAccess.IndexOf(AValue: PPyObject): Integer;
begin
  Result := Container.IndexOf(GetPythonEngine.PyObjectAsString(AValue));
end;

function TStringsAccess.SetItem(AIndex: Integer; AValue: PPyObject): Boolean;
begin
  with GetPythonEngine do
  begin
    if PyUnicode_Check(AValue) then
    begin
      Container[AIndex] := PyUnicodeAsString(AValue);
      Result := True;
    end
    else
    begin
      Result := False;
      PyErr_SetString (PyExc_AttributeError^, 'You can only assign strings to TStrings items');
    end;
  end
end;

class function TStringsAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

class function TStringsAccess.SupportsWrite: Boolean;
begin
  Result := True;
end;

{ TStringsObjectsAccess }

function TStringsObjectsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Objects[AIndex] );
end;

function TStringsObjectsAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  _obj : TPyObject;
  _value : TObject;
begin
  Result := -1;
  if IsDelphiObject(AValue) then
  begin
    _obj := PythonToDelphi(AValue);
    if _obj is TPyDelphiObject then
    begin
      _value := TPyDelphiObject(_obj).DelphiObject;
      for i := 0 to Container.Count-1 do
      begin
        if Container.Objects[i] = _value then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

class function TStringsObjectsAccess.Name: string;
begin
  Result := 'Objects';
end;

function TStringsObjectsAccess.SetItem(AIndex: Integer; AValue: PPyObject): Boolean;
begin
  with GetPythonEngine do
  begin
    if IsDelphiObject(AValue) and (PythonToDelphi(AValue) is TPyDelphiObject) then
    begin
      Container.Objects[AIndex] := TPyDelphiObject(PythonToDelphi(AValue)).DelphiObject;
      Result := True;
    end
    else
    begin
      Result := False;
      PyErr_SetString (PyExc_AttributeError^, 'You can only assign Delphi wrappers to Objects items');
    end;
  end
end;

{ TPyDelphiStrings }

function TPyDelphiStrings.AddObject_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PPyObject;
  _obj : PPyObject;
  _value : TObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'OO:AddObject',@PStr, @_obj ) <> 0 then
    begin
      if CheckObjAttribute(_obj, 'The second argument of AddObject', TObject, _value) then
        Result := PyLong_FromLong(DelphiObject.AddObject(PyObjectAsString(PStr), _value))
      else
        Result := nil;
    end
    else
      Result := nil;
end;

function TPyDelphiStrings.Add_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'O:Add',@PStr ) <> 0 then
      Result := PyLong_FromLong(DelphiObject.Add(PyObjectAsString(PStr)))
    else
      Result := nil;
end;

function TPyDelphiStrings.Assign(ASource: PPyObject): PPyObject;
var
  i : Integer;
  _item : PPyObject;
begin
  with GetPythonEngine do
  begin
    if not IsDelphiObject(ASource) and (PySequence_Check(ASource) <> 0) then
    begin
      DelphiObject.BeginUpdate;
      try
        DelphiObject.Clear;
        DelphiObject.Capacity := PySequence_Length(ASource);
        for i := 0 to PySequence_Length(ASource)-1 do
        begin
          _item := PySequence_GetItem(ASource, i);
          try
            DelphiObject.Add(PyObjectAsString(_item));
          finally
            Py_DecRef(_item);
          end;
        end;
      finally
        DelphiObject.EndUpdate;
      end;
      Result := ReturnNone;
    end
    else
      Result := inherited Assign(ASource);
  end;
end;

function TPyDelphiStrings.BeginUpdate_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':BeginUpdate') <> 0 then begin
    DelphiObject.BeginUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiStrings.Clear_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':Clear') <> 0 then begin
    DelphiObject.Clear;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

function TPyDelphiStrings.Delete_Wrapper(args: PPyObject): PPyObject;
Var
  Index : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:Delete',@Index ) <> 0 then
  begin
    if CheckIndex(Index, DelphiObject.Count) then
    begin
      DelphiObject.Delete(Index);
      Result := GetPythonEngine.ReturnNone;
    end
    else
      Result := nil
  end
  else
    Result := nil;
end;

class function TPyDelphiStrings.DelphiObjectClass: TClass;
begin
  Result := TStrings;
end;

function TPyDelphiStrings.EndUpdate_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, ':EndUpdate') <> 0 then begin
    DelphiObject.EndUpdate;
    Result := GetPythonEngine.ReturnNone;
  end else
    Result := nil;
end;

class function TPyDelphiStrings.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TStringsAccess;
end;

function TPyDelphiStrings.GetDelphiObject: TStrings;
begin
  Result := TStrings(inherited DelphiObject);
end;

function TPyDelphiStrings.Get_Capacity(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.Capacity);
end;

function TPyDelphiStrings.Get_Objects(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TStringsObjectsAccess.Create(Self.PyDelphiWrapper,
       Self.DelphiObject));
end;

function TPyDelphiStrings.Get_Text(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(
    CleanString(DelphiObject.Text, False));
end;

function TPyDelphiStrings.IndexOf_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'O:IndexOf',@PStr ) <> 0 then
      Result := GetPythonEngine.PyLong_FromLong(DelphiObject.IndexOf(PyObjectAsString(PStr)))
    else
      Result := nil;
end;

function TPyDelphiStrings.LoadFromFile_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 's:LoadFromFile',@PStr ) <> 0 then
  begin
    DelphiObject.LoadFromFile(string(PStr));
    Result := GetPythonEngine.ReturnNone;
  end
  else
    Result := nil;
end;

function TPyDelphiStrings.MpLength: NativeInt;
begin
  Result := DelphiObject.Count;
end;

function TPyDelphiStrings.MpSubscript(obj: PPyObject): PPyObject;
Var
  S : string;
  Index : integer;
begin
  with GetPythonEngine do
  begin
    if PyLong_Check(obj) then
      Result := SqItem(PyLong_AsLong(obj))
    else
    begin
      S := PyObjectAsString(obj);
      if S <> '' then begin
        Index := DelphiObject.IndexOf(S);
        if Index >= 0 then begin
          if Assigned(DelphiObject.Objects[Index]) then
            Result := Wrap(DelphiObject.Objects[Index])
          else
            Result := GetPythonEngine.ReturnNone;
        end else with GetPythonEngine do begin
          PyErr_SetString (PyExc_KeyError^, PAnsiChar(AnsiString(S)));
          Result := nil;
        end;
      end else with GetPythonEngine do begin
        PyErr_SetString (PyExc_KeyError^, '<Empty String>');
        Result := nil;
      end;
    end;
  end;
end;

class procedure TPyDelphiStrings.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do
    begin
      AddGetSet('Capacity', @TPyDelphiStrings.Get_Capacity, @TPyDelphiStrings.Set_Capacity,
        'Indicates the number of strings the TStrings object can hold.', nil);
      AddGetSet('Text', @TPyDelphiStrings.Get_Text, @TPyDelphiStrings.Set_Text,
        'Lists the strings in the TStrings object as a single string with the individual strings delimited by carriage returns and line feeds.', nil);
      AddGetSet('Objects', @TPyDelphiStrings.Get_Objects, nil,
        'Represents a set of objects that are associated one with each of the strings in the Strings property.', nil);
    end;
end;

class procedure TPyDelphiStrings.RegisterMethods(PythonType: TPythonType);
begin
  PythonType.AddMethod('Add', @TPyDelphiStrings.Add_Wrapper,
    'TStrings.Add(s)'#10 +
    'Adds a string to the TStrings object and returns the index position');
  PythonType.AddMethod('AddObject', @TPyDelphiStrings.AddObject_Wrapper,
    'TStrings.AddObject(s, delphiobject)'#10 +
    'Adds a string and an associated Delphi object to the Strings and returns the index position');
  PythonType.AddMethod('Clear', @TPyDelphiStrings.Clear_Wrapper,
    'TStrings.Clear()'#10 +
    'Clears all strings from a TStrings (and the associated objects');
  PythonType.AddMethod('Delete', @TPyDelphiStrings.Delete_Wrapper,
    'TStrings.Delete(i)'#10 +
    'Deletes the string at Index i (and the associated object');
  PythonType.AddMethod('IndexOf', @TPyDelphiStrings.IndexOf_Wrapper,
    'TStrings.IndexOf(s)'#10 +
    'Returns the Index of a string s or -1 if not found');
  PythonType.AddMethod('BeginUpdate', @TPyDelphiStrings.BeginUpdate_Wrapper,
    'TStrings.BeginUpdate()'#10 +
    'Enables the TStrings object to track when the list of strings is changing.');
  PythonType.AddMethod('EndUpdate', @TPyDelphiStrings.EndUpdate_Wrapper,
    'TStrings.EndUpdate()'#10 +
    'Enables the TStrings object to keep track of when the list of strings has finished changing.');
  PythonType.AddMethod('LoadFromFile', @TPyDelphiStrings.LoadFromFile_Wrapper,
    'TStrings.LoadFromFile(filename)'#10 +
    'Fills the list with the lines of text in a specified file.');
  PythonType.AddMethod('SaveToFile', @TPyDelphiStrings.SaveToFile_Wrapper,
    'TStrings.SaveToFile(filename)'#10 +
    'Saves the strings in the list to the specified file.');
end;

function TPyDelphiStrings.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString( Format('<Delphi TStrings at %x>',
         [NativeInt(self)]) );
end;

function TPyDelphiStrings.SaveToFile_Wrapper(args: PPyObject): PPyObject;
Var
  PStr : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 's:SaveToFile',@PStr ) <> 0 then
  begin
    DelphiObject.SaveToFile(string(PStr));
    Result := GetPythonEngine.ReturnNone;
  end
  else
    Result := nil;
end;

procedure TPyDelphiStrings.SetDelphiObject(const Value: TStrings);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiStrings.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Services.Mapping  := PythonType.Services.Mapping  + [msLength, msSubscript];
end;

{ TPyDelphiBasicAction }

class function TPyDelphiBasicAction.DelphiObjectClass: TClass;
begin
  Result := TBasicAction;
end;

function TPyDelphiBasicAction.Execute_Wrapper(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':Execute') <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.Execute );
    end else
      Result := nil;
  end;
end;

function TPyDelphiBasicAction.GetDelphiObject: TBasicAction;
begin
  Result := TBasicAction(inherited DelphiObject);
end;

function TPyDelphiBasicAction.Get_ActionComponent(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Wrap(DelphiObject.ActionComponent);
  end;
end;

class procedure TPyDelphiBasicAction.RegisterGetSets(
  PythonType: TPythonType);
begin
  with PythonType do
    begin
      AddGetSet('ActionComponent', @TPyDelphiBasicAction.Get_ActionComponent, @TPyDelphiBasicAction.Set_ActionComponent,
        'Indicates the client component that caused this action to execute.', nil);
    end;
end;

class procedure TPyDelphiBasicAction.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('Execute', @TPyDelphiBasicAction.Execute_Wrapper,
    'TBasicAction.Execute()'#10 +
    'Generates an OnExecute event.');
  PythonType.AddMethod('Update', @TPyDelphiBasicAction.Update_Wrapper,
    'TBasicAction.Update()'#10 +
    'Provides an opportunity to execute centralized code when an application is idle.');
end;

procedure TPyDelphiBasicAction.SetDelphiObject(const Value: TBasicAction);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiBasicAction.Set_ActionComponent(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _actionComponent : TObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckObjAttribute(AValue, 'ActionComponent', TComponent, _actionComponent) then
    begin
      Self.DelphiObject.ActionComponent := TComponent(_actionComponent);
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

function TPyDelphiBasicAction.Update_Wrapper(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, ':Update') <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.Update );
    end else
      Result := nil;
  end;
end;

function TPyDelphiStrings.Set_Capacity(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _capacity : Integer;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckIntAttribute(AValue, 'Capacity', _capacity) then
    begin
      DelphiObject.Capacity := _capacity;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

function TPyDelphiStrings.Set_Text(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _text : string;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    if CheckStrAttribute(AValue, 'Text', _text) then
    begin
      DelphiObject.Text := _text;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

{$IFNDEF FPC}
{ TPyReader }

constructor TPyReader.Create(APyObject: TPyDelphiObject; Stream: TStream;
  BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  OnFindComponentClass := DoFind;
  FPyObject := APyObject;
  FInstance := APyObject.DelphiObject as TComponent;
end;

procedure TPyReader.DoFind(Reader: TReader; const ClassName: string;
  var ComponentClass: TComponentClass);
var
  LClass: TClass;
  LCtx: TRttiContext;
  LType: TRttiType;
begin
  LClass := GetClass(ClassName);
  if Assigned(LClass) and (LClass.InheritsFrom(TComponent)) then begin
    ComponentClass := TComponentClass(LClass);
    Exit;
  end;

  LCtx := TRttiContext.Create();
  try
    for LType in LCtx.GetTypes() do
    begin
      if LType.IsInstance and LType.Name.EndsWith(ClassName) then begin
        if LType.AsInstance.MetaclassType.InheritsFrom(TComponent) then begin
          ComponentClass := TComponentClass(LType.AsInstance.MetaclassType);
          Break;
        end;
      end;
    end;
  finally
    LCtx.Free();
  end;
end;

function TPyReader.FindMethod(Root: TComponent;
  const AMethodName: string): Pointer;
var
  LPyMethodName: PPyObject;
  LPyPropName: PPyObject;
  LCallable: PPyObject;
begin
  Result := nil;
  if Assigned(GetPropInfo(FInstance, PropName)) then begin
    with GetPythonEngine() do begin
      LPyMethodName := PyUnicodeFromString(AMethodName);
      try
        LCallable := FPyObject.GetAttrO(LPyMethodName);
        try
          if not Assigned(LCallable) then
            Exit();

          LPyPropName := PyUnicodeFromString(PropName);
          try
            PyObject_SetAttr(FPyObject.Wrap(FInstance), LPyPropName, LCallable);

            if PyErr_Occurred <> nil then
              CheckError(false);
          finally
            Py_XDecRef(LPyPropName);
          end;
        finally
          Py_XDecRef(LCallable);
        end;
      finally
        Py_XDecRef(LPyMethodName);
      end;
    end;
  end;
end;

procedure TPyReader.SetName(Component: TComponent; var Name: string);
var
  LPyKey: PPyObject;
begin
  inherited;
  with GetPythonEngine() do begin
    LPyKey := PyUnicodeFromString(Name);
    try
      PyObject_GenericSetAttr(
        FPyObject.GetSelf(), LPyKey, FPyObject.Wrap(Component));

      if PyErr_Occurred <> nil then
        CheckError(false);
    finally
      Py_XDecRef(LPyKey);
    end;
  end;
  FInstance := Component;
end;
{$ENDIF}

{ TPyDelphiStream }

class function TPyDelphiStream.DelphiObjectClass: TClass;
begin
  Result := TStream;
end;

function TPyDelphiStream.GetDelphiObject: TStream;
begin
  Result := TStream(inherited DelphiObject);
end;

procedure TPyDelphiStream.SetDelphiObject(const Value: TStream);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiStream.SetupType(PythonType: TPythonType);
begin
  inherited;
  if (PythonType.PyObjectClass = TPyDelphiStream) then
    PythonType.TypeFlags := PythonType.TypeFlags + [TPFlag.tpIsAbstract]
  else
    PythonType.TypeFlags := PythonType.TypeFlags + [TPFlag.tpTypeSubclass]
end;

class procedure TPyDelphiStream.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('ReadBytes', @TPyDelphiStream.ReadBytes_Wrapper,
    'TPyDelphiStream.ReadBytes()' + #10 + 'Read content as bytearray.');
  PythonType.AddMethod('ReadInt', @TPyDelphiStream.ReadInt_Wrapper,
    'TPyDelphiStream.ReadInt()' + #10 + 'Read content as integer.');
  PythonType.AddMethod('ReadString', @TPyDelphiStream.ReadString_Wrapper,
    'TPyDelphiStream.ReadString()' + #10 + 'Read content as string.');
  PythonType.AddMethod('ReadFloat', @TPyDelphiStream.ReadFloat_Wrapper,
    'TPyDelphiStream.ReadFloat()' + #10 + 'Read content as float.');

  PythonType.AddMethod('WriteBytes', @TPyDelphiStream.WriteBytes_Wrapper,
    'TPyDelphiStream.WriteBytes()' + #10 + 'Write content as bytearray.');
  PythonType.AddMethod('WriteInt', @TPyDelphiStream.WriteInt_Wrapper,
    'TPyDelphiStream.WriteInt()' + #10 + 'Write content as integer.');
  PythonType.AddMethod('WriteString', @TPyDelphiStream.WriteString_Wrapper,
    'TPyDelphiStream.WriteString()' + #10 + 'Write content as string.');
  PythonType.AddMethod('WriteFloat', @TPyDelphiStream.WriteFloat_Wrapper,
    'TPyDelphiStream.WriteFloat()' + #10 + 'Write content as float.');
end;

function TPyDelphiStream.GetReadCount(const AArgs: PPyObject): integer;
begin
  if GetPythonEngine().PyArg_ParseTuple(AArgs, 'i:Create', @Result) = 0 then
    Result := 0;
end;

function TPyDelphiStream.ReadBytes_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LCount: Integer;
  LValue: TBytes;
  LItem: PPyObject;
  LBytes: PPyObject;
  LByte: byte;
begin
  Adjust(@Self);
  //Returns multiple results
  with GetPythonEngine() do begin
    Result := PyList_New(0);
    LCount := GetReadCount(AArgs);
    if (LCount > 0) then begin
      //The read result
      SetLength(LValue, LCount);
      LItem := PyLong_FromLong(DelphiObject.Read(LValue, LCount));
      PyList_Append(Result, LItem);
      Py_XDecRef(LItem);
      //Create a list of bytes, then convert it to bytearray
      LBytes := PyList_New(0);
      for LByte in LValue do begin
        LItem := PyLong_FromLong(LByte);
        PyList_Append(LBytes, LItem);
        Py_XDecRef(LItem);
      end;
      //The content
      LItem := PyByteArray_FromObject(LBytes);
      Py_XDecRef(LBytes);
      PyList_Append(Result, LItem);
      Py_XDecRef(LItem);
    end;
  end;
end;

function TPyDelphiStream.ReadFloat_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LValue: single;
  LItem: PPyObject;
begin
  Adjust(@Self);
  //Returns multiple results
  with GetPythonEngine() do begin
    Result := PyList_New(0);
    //The read result
    LItem := PyLong_FromLong(DelphiObject.Read(LValue, SizeOf(single)));
    PyList_Append(Result, LItem);
    Py_XDecRef(LItem);
    //The content
    LItem := PyFloat_FromDouble(LValue);
    PyList_Append(Result, LItem);
    Py_XDecRef(LItem);
  end;
end;

function TPyDelphiStream.ReadInt_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LValue: integer;
  LItem: PPyObject;
begin
  Adjust(@Self);
  //Returns multiple results
  with GetPythonEngine() do begin
    Result := PyList_New(0);
    //The read result
    LItem := PyLong_FromLong(DelphiObject.Read(LValue, SizeOf(Integer)));
    PyList_Append(Result, LItem);
    Py_XDecRef(LItem);
    //The content
    LItem := PyLong_FromLong(LValue);
    PyList_Append(Result, LItem);
    Py_XDecRef(LItem);
  end;
end;

function TPyDelphiStream.ReadString_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LCount: Integer;
  LValue: string;
  LItem: PPyObject;
begin
  Adjust(@Self);
  //Returns multiple results
  with GetPythonEngine() do begin
    Result := PyList_New(0);
    LCount := GetReadCount(AArgs);
    if (LCount > 0) then begin
      //The read result
      SetLength(LValue, LCount);
      LItem := PyLong_FromLong(DelphiObject.Read(Pointer(LValue)^, LCount * SizeOf(Char)));
      PyList_Append(Result, LItem);
      Py_XDecRef(LItem);
      //The content
      LItem := PyUnicodeFromString(LValue);
      PyList_Append(Result, LItem);
      Py_XDecRef(LItem);
    end;
  end;
end;

function TPyDelphiStream.WriteBytes_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LValue: PPyObject;
  LCount: integer;
  LBuffer: TBytes;
begin
  Adjust(@Self);
  Result := nil;
  with GetPythonEngine() do begin
    if PyArg_ParseTuple(AArgs, 'Yi:Create', @LValue, @LCount) <> 0 then
      if PyByteArray_Check(LValue) then begin
        LBuffer := TEncoding.Default.GetBytes(String(PyByteArray_AsString(LValue)));
        Result := PyLong_FromLong(DelphiObject.Write(LBuffer, LCount));
      end;
  end;
end;

function TPyDelphiStream.WriteFloat_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LValue: single;
begin
  Adjust(@Self);
  Result := nil;
  with GetPythonEngine() do begin
    if PyArg_ParseTuple(AArgs, 'f:Create', @LValue) <> 0 then
      Result := PyLong_FromLong(DelphiObject.Write(LValue, SizeOf(Single)));
  end;
end;

function TPyDelphiStream.WriteInt_Wrapper(const AArgs: PPyObject): PPyObject;
var
  LValue: integer;
begin
  Adjust(@Self);
  Result := nil;
  with GetPythonEngine() do begin
    if PyArg_ParseTuple(AArgs, 'i:Create', @LValue) <> 0 then
      Result := PyLong_FromLong(DelphiObject.Write(LValue, SizeOf(Integer)));
  end;
end;

function TPyDelphiStream.WriteString_Wrapper(
  const AArgs: PPyObject): PPyObject;
var
  LValue: PAnsiChar;
  LCount: integer;
  LStr: string;
begin
  Adjust(@Self);
  Result := nil;
  with GetPythonEngine() do begin
    if PyArg_ParseTuple(AArgs, 'si:Create', @LValue, @LCount) <> 0 then begin
      LStr := String(LValue);
      Result := PyLong_FromLong(DelphiObject.Write(Pointer(LStr)^, LCount * SizeOf(Char)));
    end;
  end;
end;

{ TPyDelphiHandleStream }

constructor TPyDelphiHandleStream.CreateWith(APythonType: TPythonType;
  args, kwds: PPyObject);
type
  THandleStreamClass = class of THandleStream;
var
  LParamCount: NativeInt;
  LHandle: THandle;
begin
  inherited;
  //Clear unsuccessful overloaded constructor error
  APythonType.Engine.PyErr_Clear();

  LParamCount := APythonType.Engine.PyTuple_Size(args);
  if (LParamCount = 1) then
    {$IFDEF CPUX64}
    if APythonType.Engine.PyArg_ParseTuple(args, 'K:Create', @LHandle) <> 0 then
    {$ELSE}
    if APythonType.Engine.PyArg_ParseTuple(args, 'I:Create', @LHandle) <> 0 then
    {$ENDIF}
      DelphiObject := THandleStreamClass(DelphiObjectClass).Create(LHandle);
end;

class function TPyDelphiHandleStream.DelphiObjectClass: TClass;
begin
  Result := THandleStream;
end;

function TPyDelphiHandleStream.GetDelphiObject: THandleStream;
begin
  Result := THandleStream(inherited DelphiObject);
end;

procedure TPyDelphiHandleStream.SetDelphiObject(const Value: THandleStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFileStream }

constructor TPyDelphiFileStream.CreateWith(APythonType: TPythonType;
  args, kwds: PPyObject);
type
  TFileStreamClass = class of TFileStream;
var
  LParamCount: NativeInt;
  LMode: Word;
  LRights: Cardinal;
  LFileName: PAnsiChar;
begin
  inherited;
  //Clear unsuccessful overloaded constructor error
  APythonType.Engine.PyErr_Clear();

  if (APythonType.PyObjectClass <> TPyDelphiFileStream) then
    Exit;

  LParamCount := APythonType.Engine.PyTuple_Size(args);
  if (LParamCount = 2) then begin
    if APythonType.Engine.PyArg_ParseTuple(args, 'sH:Create', @LFileName, @LMode) <> 0 then
      DelphiObject := TFileStreamClass(DelphiObjectClass).Create(String(LFileName), LMode);
  end else if (LParamCount = 3) then begin
    if APythonType.Engine.PyArg_ParseTuple(args, 'sHI:Create', @LFileName, @LMode, @LRights) <> 0 then
      DelphiObject := TFileStreamClass(DelphiObjectClass).Create(String(LFileName), LMode, LRights);
  end;

  //Maybe it was created on the next attempt...
  if Assigned(DelphiObject) then
     APythonType.Engine.PyErr_Clear();
end;

class function TPyDelphiFileStream.DelphiObjectClass: TClass;
begin
  Result := TFileStream;
end;

function TPyDelphiFileStream.GetDelphiObject: TFileStream;
begin
  Result := TFileStream(inherited DelphiObject);
end;

procedure TPyDelphiFileStream.SetDelphiObject(const Value: TFileStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBufferedFileStream }

constructor TPyDelphiBufferedFileStream.CreateWith(APythonType: TPythonType;
  args, kwds: PPyObject);
type
  TBufferedFileStreamClass = class of TBufferedFileStream;
const
  LKwArgs1: array[0..3] of PAnsiChar = ('FileName', 'Mode', 'BufferSize', PAnsiChar(nil));
  LKwArgs2: array[0..4] of PAnsiChar = ('FileName', 'Mode', 'Rights', 'BufferSize', PAnsiChar(nil));
var
  LArgCount: NativeInt;
  LMode: Word;
  LRights: Cardinal;
  LFileName: PAnsiChar;
  LBufferSize: integer;
begin
  inherited; //We MUST use the overloaded constructor
  //Clear unsuccessful overloaded constructor error
  APythonType.Engine.PyErr_Clear();

  //We need kwargs here due to constructor overloads with default value (BufferSize)
  LBufferSize := 32768;
  LArgCount := APythonType.Engine.PyTuple_Size(args);
  if (LArgCount = 2) then begin
    if (APythonType.Engine.PyArg_ParseTupleAndKeywords(args, kwds, 'sH|i:Create', @LKwArgs1[0], @LFileName, @LMode, @LBufferSize) <> 0) then
      DelphiObject := TBufferedFileStreamClass(DelphiObjectClass).Create(String(LFileName), LMode, LBufferSize);
  end else if (LArgCount = 3) then begin
    if (APythonType.Engine.PyArg_ParseTupleAndKeywords(args, kwds, 'sHI|i:Create', @LKwArgs2[0], @LFileName, @LMode, @LRights, @LBufferSize) <> 0) then
      DelphiObject := TBufferedFileStreamClass(DelphiObjectClass).Create(String(LFileName), LMode, LRights, LBufferSize);
  end;

  //Maybe it was created on the next attempt...
  if Assigned(DelphiObject) then
     APythonType.Engine.PyErr_Clear();
end;

class function TPyDelphiBufferedFileStream.DelphiObjectClass: TClass;
begin
  Result := TBufferedFileStream;
end;

function TPyDelphiBufferedFileStream.GetDelphiObject: TBufferedFileStream;
begin
  Result := TBufferedFileStream(inherited DelphiObject);
end;

procedure TPyDelphiBufferedFileStream.SetDelphiObject(
  const Value: TBufferedFileStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomMemoryStream }

class function TPyDelphiCustomMemoryStream.DelphiObjectClass: TClass;
begin
  Result := TCustomMemoryStream;
end;

function TPyDelphiCustomMemoryStream.GetDelphiObject: TCustomMemoryStream;
begin
  Result := TCustomMemoryStream(inherited DelphiObject);
end;

procedure TPyDelphiCustomMemoryStream.SetDelphiObject(
  const Value: TCustomMemoryStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMemoryStream }

class function TPyDelphiMemoryStream.DelphiObjectClass: TClass;
begin
  Result := TMemoryStream;
end;

function TPyDelphiMemoryStream.GetDelphiObject: TMemoryStream;
begin
  Result := TMemoryStream(inherited DelphiObject);
end;

procedure TPyDelphiMemoryStream.SetDelphiObject(const Value: TMemoryStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiByteStream }

constructor TPyDelphiByteStream.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
type
  TBytesStreamClass = class of TBytesStream;
var
  LBytes: PPyObject;
begin
  inherited;
  //Clear unsuccessful overloaded constructor error
  APythonType.Engine.PyErr_Clear();

  if (APythonType.Engine.PyTuple_Size(args) = 1) then
    if APythonType.Engine.PyArg_ParseTuple(args, 'Y:Create', @LBytes) <> 0 then begin
      if APythonType.Engine.PyByteArray_Check(LBytes) then begin
        DelphiObject := TBytesStreamClass(DelphiObjectClass).Create(
          TEncoding.Default.GetBytes(
            String(APythonType.Engine.PyByteArray_AsString(LBytes))));
      end;
    end else if APythonType.Engine.PyArg_ParseTuple(args, 'S:Create', @LBytes) <> 0 then begin
      if APythonType.Engine.PyBytes_Check(LBytes) then begin
        DelphiObject := TBytesStreamClass(DelphiObjectClass).Create(
          TEncoding.Default.GetBytes(
            String(APythonType.Engine.PyBytes_AsString(LBytes))));
      end;
    end;

  //Maybe it was created on the next attempt...
  if Assigned(DelphiObject) then
    APythonType.Engine.PyErr_Clear();
end;

class function TPyDelphiByteStream.DelphiObjectClass: TClass;
begin
  Result := TBytesStream;
end;

function TPyDelphiByteStream.GetDelphiObject: TBytesStream;
begin
  Result := TBytesStream(inherited DelphiObject);
end;

procedure TPyDelphiByteStream.SetDelphiObject(const Value: TBytesStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStringStream }

constructor TPyDelphiStringStream.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
type
  TStringStreamClass = class of TStringStream;
var
  LDataString: PAnsiChar;
begin
  inherited;
  //Clear unsuccessful overloaded constructor error
  APythonType.Engine.PyErr_Clear();

  if (APythonType.Engine.PyTuple_Size(args) = 1) then
    if APythonType.Engine.PyArg_ParseTuple(args, 's:Create', @LDataString) <> 0 then
      DelphiObject := TStringStreamClass(DelphiObjectClass).Create(String(LDataString));
end;

class function TPyDelphiStringStream.DelphiObjectClass: TClass;
begin
  Result := TStringStream;
end;

function TPyDelphiStringStream.GetDelphiObject: TStringStream;
begin
  Result := TStringStream(inherited DelphiObject);
end;

procedure TPyDelphiStringStream.SetDelphiObject(const Value: TStringStream);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiResourceStream }

constructor TPyDelphiResourceStream.CreateWith(APythonType: TPythonType; args,
  kwds: PPyObject);
type
  TResourceStreamClass = class of TResourceStream;
var
  LHandle: THandle;
  LResName: PAnsiChar;
  LResId: Integer;
  LResType: PAnsiChar;
begin
  inherited;
  //Clear unsuccessful overloaded constructor error
  APythonType.Engine.PyErr_Clear();

  try
    {$IFDEF CPUX64}
    if APythonType.Engine.PyArg_ParseTuple(args, 'Kss:Create', @LHandle, @LResName, @LResType) <> 0 then
    {$ELSE}
    if APythonType.Engine.PyArg_ParseTuple(args, 'Iss:Create', @LHandle, @LResName, @LResType) <> 0 then
    {$ENDIF}
      DelphiObject := TResourceStreamClass(DelphiObjectClass).Create(LHandle, String(LResName), PWideChar(String(LResType)))
    else
    {$IFDEF CPUX64}
    if APythonType.Engine.PyArg_ParseTuple(args, 'Kis:Create', @LHandle, @LResId, @LResType) <> 0 then
    {$ELSE}
    if APythonType.Engine.PyArg_ParseTuple(args, 'Iis:Create', @LHandle, @LResId, @LResType) <> 0 then
    {$ENDIF}
      DelphiObject := TResourceStreamClass(DelphiObjectClass).CreateFromID(LHandle, LResId, PWideChar(String(LResType)));
  except
    on E: Exception do
      with GetPythonEngine do
        PyErr_SetString(PyExc_RuntimeError^, PAnsiChar(AnsiString(E.Message)));
  end;

  //Maybe it was created on the next attempt...
  if Assigned(DelphiObject) then
    APythonType.Engine.PyErr_Clear();
end;

class function TPyDelphiResourceStream.DelphiObjectClass: TClass;
begin
  Result := TResourceStream;
end;

function TPyDelphiResourceStream.GetDelphiObject: TResourceStream;
begin
  Result := TResourceStream(inherited DelphiObject);
end;

procedure TPyDelphiResourceStream.SetDelphiObject(const Value: TResourceStream);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TClassesRegistration.Create);

end.
