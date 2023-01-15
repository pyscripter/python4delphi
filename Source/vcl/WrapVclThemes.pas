{$I ..\Definition.Inc}
unit WrapVclThemes;

interface

uses
  PythonEngine, WrapDelphi, Vcl.Themes, Vcl.Styles;

  type
  TPyDelphiStyleInfo = class(TPyObject)
  private
    FValue: TStyleInfo;
  protected
    // Exposed Getters
    function Get_Name(Acontext: Pointer): PPyObject; cdecl;
    function Get_Author(Acontext: Pointer): PPyObject; cdecl;
    function Get_AuthorEMail(Acontext: Pointer): PPyObject; cdecl;
    function Get_AuthorUrl(Acontext: Pointer): PPyObject; cdecl;
    function Get_Version(Acontext: Pointer): PPyObject; cdecl;
    // Exposed Setters
    function Set_Name(AValue: PPyObject; AContext: Pointer) : Integer; cdecl;
    function Set_Author(AValue: PPyObject; AContext: Pointer) : Integer; cdecl;
    function Set_AuthorEMail(AValue: PPyObject; AContext: Pointer) : Integer; cdecl;
    function Set_AuthorUrl(AValue: PPyObject; AContext: Pointer) : Integer; cdecl;
    function Set_Version(AValue: PPyObject; AContext: Pointer) : Integer; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    function Compare(AObj: PPyObject) : Integer; override;
    function Repr: PPyObject; override;
    class procedure RegisterGetSets(APythonType: TPythonType ); override;
    class procedure SetupType(APythonType: TPythonType ); override;
    property Value: TStyleInfo read fValue write fValue;
  end;

  TPyDelphiCustomStyleServices = class(TPyDelphiObject)
  private
    function GetDelphiObject: TCustomStyleServices;
    procedure SetDelphiObject(const Value: TCustomStyleServices);
  public
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    class function DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TCustomStyleServices read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStyleManager = class(TPyDelphiObject)
  private
    function GetDelphiObject: TStyleManager;
    procedure SetDelphiObject(const Value: TStyleManager);
  protected
    function Get_StyleNames(AContext: Pointer): PPyObject; cdecl;
    function Get_ActiveStyle(AContext: Pointer): PPyObject; cdecl;
    // Exposed Methods
    function LoadFromFileName_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
    function IsValidStyle_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
  public
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    class function DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    property DelphiObject: TStyleManager read GetDelphiObject write SetDelphiObject;
  end;

  TStyleManagerStyleNamesAccess = class(TContainerAccess)
  private
    function GetContainer: TStyleManager;
  public
    class function ExpectedContainerClass: TClass; override;
    class function Name: string; override;
    function GetItem(AIndex: Integer): PPyObject; override;
    function GetSize: Integer; override;
    property Container : TStyleManager read GetContainer;
  end;

{ Helper functions }
function WrapStyleInfo(APyDelphiWrapper: TPyDelphiWrapper; [ref] AStyleInfo: TStyleInfo): PPyObject;

implementation

uses
  System.SysUtils, Vcl.Controls, MethodCallBack;

{ Global Functions }
function StyleServices_Wrapper(pself, args: PPyObject): PPyObject; cdecl;
var
  LPyObj: PPyObject;
  LControl: TControl;
begin
  LControl := nil;
  with GetPythonEngine do
  begin
    if PyTuple_Check(args) then begin
      if PyTuple_Size(args) = 0 then
        Result := GlobalDelphiWrapper.Wrap(StyleServices())
{$IF CompilerVersion > 33}
      else if (PyArg_ParseTuple(args, 'O:StyleServices', @LPyObj) <> 0)
        and CheckObjAttribute(LPyObj, 'AControl', TControl, TObject(LControl))
      then
        Result := GlobalDelphiWrapper.Wrap(StyleServices(LControl))
{$IFEND}
      else
        Result := nil;
    end else
      Result := nil;
  end;
end;

{ Helper functions }
function WrapStyleInfo(APyDelphiWrapper: TPyDelphiWrapper; [ref] AStyleInfo: TStyleInfo) : PPyObject;
var
  LType: TPythonType;
begin
  LType := APyDelphiWrapper.GetHelperType('StyleInfoType');
  Result := LType.CreateInstance();
  (PythonToDelphi(Result) as TPyDelphiStyleInfo).Value := AStyleInfo;
end;

{ Register the wrappers, the globals and the constants }
type
  TVclThemesRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

  { TVclThemesRegistration }
procedure TVclThemesRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterFunction(PAnsiChar('StyleServices'),
    StyleServices_Wrapper,
    PAnsiChar('StyleServices_Wrapper()'#10 +
    'Get a StyleServices instance.'));
end;

procedure TVclThemesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TVclThemesRegistration.Name: string;
begin
  Result := 'VclThemes';
end;

procedure TVclThemesRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStyleManager);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomStyleServices);
  APyDelphiWrapper.RegisterHelperType(TPyDelphiStyleInfo);
end;

{ TPyDelphiStyleManager }
constructor TPyDelphiStyleManager.CreateWith(APythonType: TPythonType; args:
    PPyObject);
begin
  inherited;
  DelphiObject := TStyleManager.Create();
end;

class function TPyDelphiStyleManager.DelphiObjectClass: TClass;
begin
  Result := TStyleManager;
end;

function TPyDelphiStyleManager.GetDelphiObject: TStyleManager;
begin
  Result := TStyleManager(inherited DelphiObject);
end;

function TPyDelphiStyleManager.Get_ActiveStyle(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.ActiveStyle);
end;

function TPyDelphiStyleManager.Get_StyleNames(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper,
    TStyleManagerStyleNamesAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiStyleManager.IsValidStyle_Wrapper(AArgs: PPyObject): PPyObject;
var
  LFileName: PAnsiChar;
  LIsValid: Boolean;
  LStyleInfo: TStyleInfo;
begin
  Adjust(@Self);
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple(AArgs, 's:IsValidStyle', @LFileName) <> 0 then
    begin
      LIsValid := TStyleManager.IsValidStyle(string(LFileName), LStyleInfo);
      Result := PyTuple_New(2);
      PyTuple_SetItem(Result, 0, PyBool_FromLong(Ord(LIsValid)));
      PyTuple_SetItem(Result, 1, WrapStyleInfo(PyDelphiWrapper, LStyleInfo));
    end else
      Result := nil;
  end;
end;

function TPyDelphiStyleManager.LoadFromFileName_Wrapper(
  AArgs: PPyObject): PPyObject;
var
  LFileName: PAnsiChar;
begin
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple(AArgs, 's:LoadFromFile', @LFileName) <> 0 then
    begin
      TStyleManager.LoadFromFile(string(LFileName));
      Result := GetPythonEngine().ReturnNone();
    end else
      Result := nil;
  end;
end;

class procedure TPyDelphiStyleManager.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do begin
    AddGetSet('StyleNames', @TPyDelphiStyleManager.Get_StyleNames, nil,
        'Provides access to the VCL style names.', nil);
    AddGetSet('ActiveStyle', @TPyDelphiStyleManager.Get_ActiveStyle, nil,
        'Returns the current style.', nil);
  end;
end;

class procedure TPyDelphiStyleManager.RegisterMethods(PythonType: TPythonType);
begin
  PythonType.AddMethod('LoadFromFile', @TPyDelphiStyleManager.LoadFromFileName_Wrapper,
    'TStyleManager.LoadFromFile()'#10 +
    'Loads a VCL style from a file');
  PythonType.AddMethod('IsValidStyle', @TPyDelphiStyleManager.IsValidStyle_Wrapper,
    'TStyleManager.IsValidStyle()'#10 +
    'Check if a Vcl Style file is valid');
end;

procedure TPyDelphiStyleManager.SetDelphiObject(const Value: TStyleManager);
begin
  inherited DelphiObject := Value;
end;

{ TStyleManagerStyleNamesAccess }
class function TStyleManagerStyleNamesAccess.ExpectedContainerClass: TClass;
begin
  Result := TStyleManager;
end;

function TStyleManagerStyleNamesAccess.GetContainer: TStyleManager;
begin
  Result := TStyleManager(inherited Container);
end;

function TStyleManagerStyleNamesAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(Container.StyleNames[AIndex]);
end;

function TStyleManagerStyleNamesAccess.GetSize: Integer;
begin
  Result := Length(Container.StyleNames);
end;

class function TStyleManagerStyleNamesAccess.Name: string;
begin
  Result := 'TStyleManager.StyleNames';
end;

{ TPyDelphiStyleInfo }
constructor TPyDelphiStyleInfo.CreateWith(APythonType: TPythonType; args:
    PPyObject);
var
  LName: PAnsiChar;
  LAuthor: PAnsiChar;
  LAuthorEMail: PAnsiChar;
  LAuthorURL: PAnsiChar;
  LVersion: PAnsiChar;
begin
  inherited;
  if APythonType.Engine.PyArg_ParseTuple(args, 'sssss:Create', @LName, @LAuthor, @LAuthorEMail, @LAuthorURL, @LVersion) <> 0 then
  begin
    FValue.Name := string(LName);
    FValue.Author := string(LAuthor);
    FValue.AuthorEMail := string(LAuthorEMail);
    FValue.AuthorURL := string(LAuthorURL);
    FValue.Version := string(LVersion);
  end
end;

function TPyDelphiStyleInfo.Compare(AObj: PPyObject): Integer;
var
  LOther : TPyDelphiStyleInfo;
begin
  if IsDelphiObject(AObj) and (PythonToDelphi(AObj) is TPyDelphiStyleInfo) then
  begin
    LOther := TPyDelphiStyleInfo(PythonToDelphi(AObj));
    Result := Ord(
      (LOther.Value.Name = Value.Name)
      and (LOther.Value.Author = Value.Author)
      and (LOther.Value.AuthorEMail = Value.AuthorEMail)
      and (LOther.Value.AuthorURL = Value.AuthorURL)
      and (LOther.Value.Version = Value.Version)
    );
  end
  else
    Result := 1;
end;

function TPyDelphiStyleInfo.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(Format(
    '<StyleInfo (%s, %s, %s, %s, %s)>',
    [Value.Name, Value.Author, Value.AuthorEMail, Value.AuthorURL, Value.Version]));
end;

function TPyDelphiStyleInfo.Get_Name(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(Value.Name);
end;

function TPyDelphiStyleInfo.Get_Author(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(Value.Author);
end;

function TPyDelphiStyleInfo.Get_AuthorEMail(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(Value.AuthorEMail);
end;

function TPyDelphiStyleInfo.Get_AuthorUrl(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(Value.AuthorURL);
end;

function TPyDelphiStyleInfo.Get_Version(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyUnicodeFromString(Value.Version);
end;

function TPyDelphiStyleInfo.Set_Name(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  LValue : string;
begin
  if CheckStrAttribute(AValue, 'Name', LValue) then
  with GetPythonEngine do begin
    Adjust(@Self);
    FValue.Name := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiStyleInfo.Set_Author(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  LValue : string;
begin
  if CheckStrAttribute(AValue, 'Author', LValue) then
  with GetPythonEngine do begin
    Adjust(@Self);
    FValue.Author := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiStyleInfo.Set_AuthorEMail(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  LValue : string;
begin
  if CheckStrAttribute(AValue, 'AuthorEMAil', LValue) then
  with GetPythonEngine do begin
    Adjust(@Self);
    FValue.AuthorEMAil := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiStyleInfo.Set_AuthorUrl(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  LValue : string;
begin
  if CheckStrAttribute(AValue, 'AuthorUrl', LValue) then
  with GetPythonEngine do begin
    Adjust(@Self);
    FValue.AuthorUrl := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiStyleInfo.Set_Version(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  LValue : string;
begin
  if CheckStrAttribute(AValue, 'Version', LValue) then
  with GetPythonEngine do begin
    Adjust(@Self);
    FValue.Version := LValue;
    Result := 0;
  end
  else
    Result := -1;
end;

class procedure TPyDelphiStyleInfo.RegisterGetSets(APythonType: TPythonType);
begin
  with APythonType do
  begin
    AddGetSet('Name', @TPyDelphiStyleInfo.Get_Name, @TPyDelphiStyleInfo.Set_Name,
      'Provides access to the Name of a Style Info', nil);
    AddGetSet('Author', @TPyDelphiStyleInfo.Get_Author, @TPyDelphiStyleInfo.Set_Author,
      'Provides access to the Author of a Style Info', nil);
    AddGetSet('AuthorEMail', @TPyDelphiStyleInfo.Get_AuthorEMail, @TPyDelphiStyleInfo.Set_AuthorEMail,
      'Provides access to the Author E-Mail of a Style Info', nil);
    AddGetSet('AuthorUrl', @TPyDelphiStyleInfo.Get_AuthorUrl, @TPyDelphiStyleInfo.Set_AuthorUrl,
      'Provides access to the Author URL of a Style Info', nil);
    AddGetSet('Version', @TPyDelphiStyleInfo.Get_Version, @TPyDelphiStyleInfo.Set_Version,
      'Provides access to the Version of a Style Info', nil);
  end;
end;

class procedure TPyDelphiStyleInfo.SetupType(APythonType: TPythonType);
begin
  inherited;
  APythonType.TypeName := 'StyleInfo';
  APythonType.Name := string(APythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  APythonType.TypeFlags := APythonType.TypeFlags + [tpfBaseType];
  APythonType.GenerateCreateFunction := False;
  APythonType.DocString.Text := 'wrapper for Delphi TStyleInfo type';
  APythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

{ TPyDelphiStyleServices }
constructor TPyDelphiCustomStyleServices.Create(APythonType: TPythonType);
begin
  inherited;
end;

constructor TPyDelphiCustomStyleServices.CreateWith(APythonType: TPythonType;
    args: PPyObject);
begin
  inherited;
  DelphiObject := StyleServices();
end;

class function TPyDelphiCustomStyleServices.DelphiObjectClass: TClass;
begin
  Result := TCustomStyleServices;
end;

function TPyDelphiCustomStyleServices.GetDelphiObject: TCustomStyleServices;
begin
  Result := TCustomStyleServices(inherited DelphiObject);
end;

procedure TPyDelphiCustomStyleServices.SetDelphiObject(
  const Value: TCustomStyleServices);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiCustomStyleServices.RegisterMethods(
  PythonType: TPythonType);
begin
end;

initialization
  RegisteredUnits.Add(TVclThemesRegistration.Create());
end.
