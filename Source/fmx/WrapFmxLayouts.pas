unit WrapFmxLayouts;

interface

uses
  FMX.Layouts, WrapDelphi, WrapFmxControls;

type
  TPyDelphiLayout = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TLayout;
    procedure SetDelphiObject(const Value: TLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TLayout read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiScaledLayout = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TScaledLayout;
    procedure SetDelphiObject(const Value: TScaledLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TScaledLayout read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomScrollBox = class(TPyDelphiStyledControl)
  private
    function  GetDelphiObject: TCustomScrollBox;
    procedure SetDelphiObject(const Value: TCustomScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiScrollBox = class(TPyDelphiCustomScrollBox)
  private
    function  GetDelphiObject: TScrollBox;
    procedure SetDelphiObject(const Value: TScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiVertScrollBox = class(TPyDelphiCustomScrollBox)
  private
    function  GetDelphiObject: TVertScrollBox;
    procedure SetDelphiObject(const Value: TVertScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TVertScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiHorzScrollBox = class(TPyDelphiCustomScrollBox)
  private
    function  GetDelphiObject: THorzScrollBox;
    procedure SetDelphiObject(const Value: THorzScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: THorzScrollBox read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TLayoutsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TLayoutsRegistration }

procedure TLayoutsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TLayoutsRegistration.Name: string;
begin
  Result := 'Layouts';
end;

procedure TLayoutsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiLayout);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScaledLayout);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiVertScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiHorzScrollBox);
end;

{ TPyDelphiLayout }

class function TPyDelphiLayout.DelphiObjectClass: TClass;
begin
  Result := TLayout;
end;

function TPyDelphiLayout.GetDelphiObject: TLayout;
begin
  Result := TLayout(inherited DelphiObject);
end;

procedure TPyDelphiLayout.SetDelphiObject(const Value: TLayout);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiScaledLayout }

class function TPyDelphiScaledLayout.DelphiObjectClass: TClass;
begin
  Result := TScaledLayout;
end;

function TPyDelphiScaledLayout.GetDelphiObject: TScaledLayout;
begin
  Result := TScaledLayout(inherited DelphiObject);
end;

procedure TPyDelphiScaledLayout.SetDelphiObject(const Value: TScaledLayout);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomScrollBox }

class function TPyDelphiCustomScrollBox.DelphiObjectClass: TClass;
begin
  Result := TCustomScrollBox;
end;

function TPyDelphiCustomScrollBox.GetDelphiObject: TCustomScrollBox;
begin
  Result := TCustomScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomScrollBox.SetDelphiObject(
  const Value: TCustomScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiScrollBox }

class function TPyDelphiScrollBox.DelphiObjectClass: TClass;
begin
  Result := TScrollBox;
end;

function TPyDelphiScrollBox.GetDelphiObject: TScrollBox;
begin
  Result := TScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiScrollBox.SetDelphiObject(const Value: TScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiVertScrollBox }

class function TPyDelphiVertScrollBox.DelphiObjectClass: TClass;
begin
  Result := TVertScrollBox;
end;

function TPyDelphiVertScrollBox.GetDelphiObject: TVertScrollBox;
begin
  Result := TVertScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiVertScrollBox.SetDelphiObject(const Value: TVertScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiHorzScrollBox }

class function TPyDelphiHorzScrollBox.DelphiObjectClass: TClass;
begin
  Result := THorzScrollBox;
end;

function TPyDelphiHorzScrollBox.GetDelphiObject: THorzScrollBox;
begin
  Result := THorzScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiHorzScrollBox.SetDelphiObject(const Value: THorzScrollBox);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TLayoutsRegistration.Create);

end.
