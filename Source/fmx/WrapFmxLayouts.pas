{$I ..\Definition.Inc}

unit WrapFmxLayouts;

interface

uses
  FMX.Layouts, {$IFNDEF LINUX}FMX.BufferedLayout,{$ENDIF LINUX} WrapDelphi, WrapFmxControls;

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

  TPyDelphiFramedScrollBox = class(TPyDelphiCustomScrollBox)
  private
    function  GetDelphiObject: TFramedScrollBox;
    procedure SetDelphiObject(const Value: TFramedScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TFramedScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiFramedVertScrollBox = class(TPyDelphiVertScrollBox)
  private
    function  GetDelphiObject: TFramedVertScrollBox;
    procedure SetDelphiObject(const Value: TFramedVertScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TFramedVertScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiGridLayout = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TGridLayout;
    procedure SetDelphiObject(const Value: TGridLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TGridLayout read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiGridPanelLayout = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TGridPanelLayout;
    procedure SetDelphiObject(const Value: TGridPanelLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TGridPanelLayout read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiFlowLayout = class(TPyDelphiControl)
  private
    function  GetDelphiObject: TFlowLayout;
    procedure SetDelphiObject(const Value: TFlowLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TFlowLayout read GetDelphiObject write SetDelphiObject;
  end;

  {$IFNDEF LINUX}
  TPyDelphiCustomBufferedLayout = class(TPyDelphiLayout)
  private
    function  GetDelphiObject: TCustomBufferedLayout;
    procedure SetDelphiObject(const Value: TCustomBufferedLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomBufferedLayout read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBufferedLayout = class(TPyDelphiCustomBufferedLayout)
  private
    function  GetDelphiObject: TBufferedLayout;
    procedure SetDelphiObject(const Value: TBufferedLayout);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBufferedLayout read GetDelphiObject write SetDelphiObject;
  end;
  {$ENDIF LINUX}

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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFramedScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFramedVertScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGridLayout);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiGridPanelLayout);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFlowLayout);
  {$IFNDEF LINUX}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomBufferedLayout);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBufferedLayout);
  {$ENDIF LINUX}
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

{ TPyDelphiFramedScrollBox }

class function TPyDelphiFramedScrollBox.DelphiObjectClass: TClass;
begin
  Result := TFramedScrollBox;
end;

function TPyDelphiFramedScrollBox.GetDelphiObject: TFramedScrollBox;
begin
  Result := TFramedScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiFramedScrollBox.SetDelphiObject(
  const Value: TFramedScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFramedVertScrollBox }

class function TPyDelphiFramedVertScrollBox.DelphiObjectClass: TClass;
begin
  Result := TFramedVertScrollBox;
end;

function TPyDelphiFramedVertScrollBox.GetDelphiObject: TFramedVertScrollBox;
begin
  Result := TFramedVertScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiFramedVertScrollBox.SetDelphiObject(
  const Value: TFramedVertScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiGridLayout }

class function TPyDelphiGridLayout.DelphiObjectClass: TClass;
begin
  Result := TGridLayout;
end;

function TPyDelphiGridLayout.GetDelphiObject: TGridLayout;
begin
  Result := TGridLayout(inherited DelphiObject);
end;

procedure TPyDelphiGridLayout.SetDelphiObject(const Value: TGridLayout);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiGridPanelLayout }

class function TPyDelphiGridPanelLayout.DelphiObjectClass: TClass;
begin
  Result := TGridPanelLayout;
end;

function TPyDelphiGridPanelLayout.GetDelphiObject: TGridPanelLayout;
begin
  Result := TGridPanelLayout(inherited DelphiObject);
end;

procedure TPyDelphiGridPanelLayout.SetDelphiObject(
  const Value: TGridPanelLayout);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFlowLayout }

class function TPyDelphiFlowLayout.DelphiObjectClass: TClass;
begin
  Result := TFlowLayout;
end;

function TPyDelphiFlowLayout.GetDelphiObject: TFlowLayout;
begin
  Result := TFlowLayout(inherited DelphiObject);
end;

procedure TPyDelphiFlowLayout.SetDelphiObject(const Value: TFlowLayout);
begin
  inherited DelphiObject := Value;
end;

{$IFNDEF LINUX}
{ TPyDelphiCustomBufferedLayout }

class function TPyDelphiCustomBufferedLayout.DelphiObjectClass: TClass;
begin
  Result := TCustomBufferedLayout;
end;

function TPyDelphiCustomBufferedLayout.GetDelphiObject: TCustomBufferedLayout;
begin
  Result := TCustomBufferedLayout(inherited DelphiObject);
end;

procedure TPyDelphiCustomBufferedLayout.SetDelphiObject(
  const Value: TCustomBufferedLayout);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBufferedLayout }

class function TPyDelphiBufferedLayout.DelphiObjectClass: TClass;
begin
  Result := TBufferedLayout;
end;

function TPyDelphiBufferedLayout.GetDelphiObject: TBufferedLayout;
begin
  Result := TBufferedLayout(inherited DelphiObject);
end;

procedure TPyDelphiBufferedLayout.SetDelphiObject(const Value: TBufferedLayout);
begin
  inherited DelphiObject := Value;
end;
{$ENDIF LINUX}

initialization
  RegisteredUnits.Add(TLayoutsRegistration.Create);

end.
