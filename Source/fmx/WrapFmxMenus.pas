{$I ..\Definition.Inc}

unit WrapFmxMenus;

interface

uses
  WrapFmxControls, FMX.Menus, WrapFmxTypes;

type
  TPyDelphiMenuItem = class(TPyDelphiTextControl)
  private
    function GetDelphiObject: TMenuItem;
    procedure SetDelphiObject(const Value: TMenuItem);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TMenuItem read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPopupMenu = class(TPyDelphiCustomPopupMenu)
  private
    function GetDelphiObject: TPopupMenu;
    procedure SetDelphiObject(const Value: TPopupMenu);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TPopupMenu read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMenuBar = class(TPyDelphiStyledControl)
  private
    function GetDelphiObject: TMenuBar;
    procedure SetDelphiObject(const Value: TMenuBar);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TMenuBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMainMenu = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TMainMenu;
    procedure SetDelphiObject(const Value: TMainMenu);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TMainMenu read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TMenusRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TMenusRegistration }

function TMenusRegistration.Name: string;
begin
  Result := 'Menus';
end;

procedure TMenusRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TMenusRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMenuItem);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPopupMenu);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMenuBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMainMenu);
end;

{ TPyDelphiMenuItem }

class function TPyDelphiMenuItem.DelphiObjectClass: TClass;
begin
  Result := TMenuItem;
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

{ TPyDelphiMenuBar }

class function TPyDelphiMenuBar.DelphiObjectClass: TClass;
begin
  Result := TMenuBar;
end;

function TPyDelphiMenuBar.GetDelphiObject: TMenuBar;
begin
  Result := TMenuBar(inherited DelphiObject);
end;

procedure TPyDelphiMenuBar.SetDelphiObject(const Value: TMenuBar);
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

initialization
  RegisteredUnits.Add(TMenusRegistration.Create());

end.
