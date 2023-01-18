unit WrapFmxScrollBox;

interface

uses
  FMX.ScrollBox, WrapFmxControls;

type
  TPyDelphiCustomPresentedScrollBox = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TCustomPresentedScrollBox;
    procedure SetDelphiObject(const Value: TCustomPresentedScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomPresentedScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPresentedScrollBox = class(TPyDelphiCustomPresentedScrollBox)
  private
    function GetDelphiObject: TPresentedScrollBox;
    procedure SetDelphiObject(const Value: TPresentedScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPresentedScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPresentedVertScrollBox = class(TPyDelphiCustomPresentedScrollBox)
  private
    function GetDelphiObject: TCustomPresentedVertScrollBox;
    procedure SetDelphiObject(const Value: TCustomPresentedVertScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomPresentedVertScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPresentedVertScrollBox = class(TPyDelphiCustomPresentedVertScrollBox)
  private
    function GetDelphiObject: TPresentedVertScrollBox;
    procedure SetDelphiObject(const Value: TPresentedVertScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPresentedVertScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPresentedHorzScrollBox = class(TPyDelphiCustomPresentedScrollBox)
  private
    function GetDelphiObject: TCustomPresentedHorzScrollBox;
    procedure SetDelphiObject(const Value: TCustomPresentedHorzScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomPresentedHorzScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPresentedHorzScrollBox = class(TPyDelphiCustomPresentedHorzScrollBox)
  private
    function GetDelphiObject: TPresentedHorzScrollBox;
    procedure SetDelphiObject(const Value: TPresentedHorzScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPresentedHorzScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPresentedFrameScrollBox = class(TPyDelphiCustomPresentedScrollBox)
  private
    function GetDelphiObject: TCustomPresentedFramedScrollBox;
    procedure SetDelphiObject(const Value: TCustomPresentedFramedScrollBox);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomPresentedFramedScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPresentedFrameScrollBox = class(TPyDelphiCustomPresentedFrameScrollBox)
  private
    function GetDelphiObject: TPresentedFramedScrollBox;
    procedure SetDelphiObject(const Value: TPresentedFramedScrollBox);
  public
    class function DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPresentedFramedScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPresentedFramedVertScrollBox = class(TPyDelphiCustomPresentedVertScrollBox)
  private
    function GetDelphiObject: TCustomPresentedFramedVertScrollBox;
    procedure SetDelphiObject(const Value: TCustomPresentedFramedVertScrollBox);
  public
    class function DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomPresentedFramedVertScrollBox read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiPresentedFramedVertScrollBox = class(TPyDelphiCustomPresentedFramedVertScrollBox)
  private
    function GetDelphiObject: TPresentedFramedVertScrollBox;
    procedure SetDelphiObject(const Value: TPresentedFramedVertScrollBox);
  public
    class function DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TPresentedFramedVertScrollBox read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TScrollBoxRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TScrollBoxRegistration }

procedure TScrollBoxRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TScrollBoxRegistration.Name: string;
begin
  Result := 'ScrollBox';
end;

procedure TScrollBoxRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPresentedScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPresentedVertScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedVertScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPresentedHorzScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedHorzScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPresentedFrameScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedFrameScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPresentedFramedVertScrollBox);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPresentedFramedVertScrollBox);
end;

{ TPyDelphiCustomPresentedScrollBox }

class function TPyDelphiCustomPresentedScrollBox.DelphiObjectClass: TClass;
begin
  Result := TCustomPresentedScrollBox;
end;

function TPyDelphiCustomPresentedScrollBox.GetDelphiObject: TCustomPresentedScrollBox;
begin
  Result := TCustomPresentedScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomPresentedScrollBox.SetDelphiObject(
  const Value: TCustomPresentedScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedScrollBox }

class function TPyDelphiPresentedScrollBox.DelphiObjectClass: TClass;
begin
  Result := TPresentedScrollBox;
end;

function TPyDelphiPresentedScrollBox.GetDelphiObject: TPresentedScrollBox;
begin
  Result := TPresentedScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiPresentedScrollBox.SetDelphiObject(
  const Value: TPresentedScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPresentedVertScrollBox }

class function TPyDelphiCustomPresentedVertScrollBox.DelphiObjectClass: TClass;
begin
  Result := TCustomPresentedVertScrollBox;
end;

function TPyDelphiCustomPresentedVertScrollBox.GetDelphiObject: TCustomPresentedVertScrollBox;
begin
  Result := TCustomPresentedVertScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomPresentedVertScrollBox.SetDelphiObject(
  const Value: TCustomPresentedVertScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedVertScrollBox }

class function TPyDelphiPresentedVertScrollBox.DelphiObjectClass: TClass;
begin
  Result := TPresentedVertScrollBox;
end;

function TPyDelphiPresentedVertScrollBox.GetDelphiObject: TPresentedVertScrollBox;
begin
  Result := TPresentedVertScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiPresentedVertScrollBox.SetDelphiObject(
  const Value: TPresentedVertScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPresentedHorzScrollBox }

class function TPyDelphiCustomPresentedHorzScrollBox.DelphiObjectClass: TClass;
begin
  Result := TCustomPresentedHorzScrollBox;
end;

function TPyDelphiCustomPresentedHorzScrollBox.GetDelphiObject: TCustomPresentedHorzScrollBox;
begin
  Result := TCustomPresentedHorzScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomPresentedHorzScrollBox.SetDelphiObject(
  const Value: TCustomPresentedHorzScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedHorzScrollBox }

class function TPyDelphiPresentedHorzScrollBox.DelphiObjectClass: TClass;
begin
  Result := TPresentedHorzScrollBox;
end;

function TPyDelphiPresentedHorzScrollBox.GetDelphiObject: TPresentedHorzScrollBox;
begin
  Result := TPresentedHorzScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiPresentedHorzScrollBox.SetDelphiObject(
  const Value: TPresentedHorzScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPresentedFrameScrollBox }

class function TPyDelphiCustomPresentedFrameScrollBox.DelphiObjectClass: TClass;
begin
  Result := TCustomPresentedFramedScrollBox;
end;

function TPyDelphiCustomPresentedFrameScrollBox.GetDelphiObject: TCustomPresentedFramedScrollBox;
begin
  Result := TCustomPresentedFramedScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomPresentedFrameScrollBox.SetDelphiObject(
  const Value: TCustomPresentedFramedScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedFrameScrollBox }

class function TPyDelphiPresentedFrameScrollBox.DelphiObjectClass: TClass;
begin
  Result := TPresentedFramedScrollBox;
end;

function TPyDelphiPresentedFrameScrollBox.GetDelphiObject: TPresentedFramedScrollBox;
begin
  Result := TPresentedFramedScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiPresentedFrameScrollBox.SetDelphiObject(
  const Value: TPresentedFramedScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPresentedFramedVertScrollBox }

class function TPyDelphiCustomPresentedFramedVertScrollBox.DelphiObjectClass: TClass;
begin
  Result := TCustomPresentedFramedVertScrollBox;
end;

function TPyDelphiCustomPresentedFramedVertScrollBox.GetDelphiObject: TCustomPresentedFramedVertScrollBox;
begin
  Result := TCustomPresentedFramedVertScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiCustomPresentedFramedVertScrollBox.SetDelphiObject(
  const Value: TCustomPresentedFramedVertScrollBox);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiPresentedFramedVertScrollBox }

class function TPyDelphiPresentedFramedVertScrollBox.DelphiObjectClass: TClass;
begin
  Result := TPresentedFramedVertScrollBox;
end;

function TPyDelphiPresentedFramedVertScrollBox.GetDelphiObject: TPresentedFramedVertScrollBox;
begin
  Result := TPresentedFramedVertScrollBox(inherited DelphiObject);
end;

procedure TPyDelphiPresentedFramedVertScrollBox.SetDelphiObject(
  const Value: TPresentedFramedVertScrollBox);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TScrollBoxRegistration.Create);

end.
