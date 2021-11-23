{$I ..\Definition.Inc}

unit WrapVclMenus;

interface

uses
  Vcl.Menus,
  PythonEngine,
  WrapDelphi,
  WrapDelphiClasses;

type
  TMenuItemAccess = class(TContainerAccess)
  private
    function GetContainer: TMenuItem;
  public
    function GetItem(AIndex: Integer): PPyObject; override;
    function GetSize: Integer; override;
    function IndexOf(AValue: PPyObject): Integer; override;
    class function ExpectedContainerClass: TClass; override;
    class function SupportsIndexOf: Boolean; override;
    class function Name: string; override;
    property Container: TMenuItem read GetContainer;
  end;

  TPyDelphiMenuItem = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TMenuItem;
    procedure SetDelphiObject(const Value: TMenuItem);
  public
    class function DelphiObjectClass: TClass; override;
    class function GetContainerAccessClass: TContainerAccessClass; override;
    property DelphiObject: TMenuItem read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMenu = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TMenu;
    procedure SetDelphiObject(const Value: TMenu);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TMenu read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPopupMenu = class(TPyDelphiMenu)
  private
    function GetDelphiObject: TPopupMenu;
    procedure SetDelphiObject(const Value: TPopupMenu);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TPopupMenu read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMainMenu = class(TPyDelphiMenu)
  private
    function GetDelphiObject: TMainMenu;
    procedure SetDelphiObject(const Value: TMainMenu);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TMainMenu read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TMenusRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TMenusRegistration }

function TMenusRegistration.Name: string;
begin
  Result := 'Vcl.Menus';
end;

procedure TMenusRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMenuItem);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMenu);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPopupMenu);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMainMenu);
end;

{ TPyDelphiMenuItem }

class function TPyDelphiMenuItem.DelphiObjectClass: TClass;
begin
  Result := TMenuItem;
end;

class function TPyDelphiMenuItem.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TMenuItemAccess;
end;

function TPyDelphiMenuItem.GetDelphiObject: TMenuItem;
begin
  Result := TMenuItem(inherited DelphiObject);
end;

procedure TPyDelphiMenuItem.SetDelphiObject(const Value: TMenuItem);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPopupMenu }

class function TPyDelphiPopupMenu.DelphiObjectClass: TClass;
begin
  Result := TPopupMenu;
end;

function TPyDelphiPopupMenu.GetDelphiObject: TPopupMenu;
begin
  Result := TPopupMenu(inherited DelphiObject);
end;

procedure TPyDelphiPopupMenu.SetDelphiObject(const Value: TPopupMenu);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMenu }

class function TPyDelphiMenu.DelphiObjectClass: TClass;
begin
  Result := TMenu;
end;

function TPyDelphiMenu.GetDelphiObject: TMenu;
begin
  Result := TMenu(inherited DelphiObject);
end;

procedure TPyDelphiMenu.SetDelphiObject(const Value: TMenu);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMainMenu }

class function TPyDelphiMainMenu.DelphiObjectClass: TClass;
begin
  Result := TMainMenu;
end;

function TPyDelphiMainMenu.GetDelphiObject: TMainMenu;
begin
  Result := TMainMenu(inherited DelphiObject);
end;

procedure TPyDelphiMainMenu.SetDelphiObject(const Value: TMainMenu);
begin
  inherited DelphiObject := Value;
end;

{ TMenuItemAccess }

class function TMenuItemAccess.ExpectedContainerClass: TClass;
begin
  Result := TMenuItem;
end;

function TMenuItemAccess.GetContainer: TMenuItem;
begin
  Result := TMenuItem(inherited Container);
end;

function TMenuItemAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.Items[AIndex]);
end;

function TMenuItemAccess.GetSize: Integer;
begin
  Result := Container.Count;
end;

function TMenuItemAccess.IndexOf(AValue: PPyObject): Integer;
var
  _obj: TPyObject;
  _item: TMenuItem;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if (_obj is TPyDelphiObject) and
        (TPyDelphiObject(_obj).DelphiObject is TMenuItem) then
      begin
        _item := TMenuItem(TPyDelphiObject(_obj).DelphiObject);
        Result := Container.IndexOf(_item);
      end;
    end;
  end;
end;

class function TMenuItemAccess.Name: string;
begin
  Result := 'TMenuItem.Items';
end;

class function TMenuItemAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

initialization
  RegisteredUnits.Add(TMenusRegistration.Create());

end.
