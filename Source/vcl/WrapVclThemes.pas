{$I ..\Definition.Inc}
unit WrapVclThemes;

interface

uses
  PythonEngine, WrapDelphi, Vcl.Themes, Vcl.Styles;

type
  TPyDelphiStyleManager = class(TPyDelphiObject)
  private
    function GetDelphiObject: TStyleManager;
    procedure SetDelphiObject(const Value: TStyleManager);
  protected
    function Get_StyleNames(AContext: Pointer): PPyObject; cdecl;
    function Get_ActiveStyle(AContext: Pointer): PPyObject; cdecl;
  public
    constructor Create( APythonType : TPythonType ); override;
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

implementation

uses
  Vcl.Controls;

{ Register the wrappers, the globals and the constants }
type
  TVclThemesRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TVclThemesRegistration }

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
end;

{ TPyDelphiStyleManager }

constructor TPyDelphiStyleManager.Create(APythonType: TPythonType);
begin
  inherited;
end;

constructor TPyDelphiStyleManager.CreateWith(APythonType: TPythonType;
  args: PPyObject);
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

class procedure TPyDelphiStyleManager.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do begin
    AddGetSet('StyleNames', @TPyDelphiStyleManager.Get_StyleNames, nil,
        'Provides access to the VCL style names.', nil);
    AddGetSet('ActiveStyle', @TPyDelphiStyleManager.Get_ActiveStyle, nil,
        'Returns the current style.', nil);
  end;
end;

class procedure TPyDelphiStyleManager.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
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

initialization
  RegisteredUnits.Add(TVclThemesRegistration.Create());

end.
