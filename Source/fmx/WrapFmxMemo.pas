unit WrapFmxMemo;

interface

uses
  WrapFmxScrollBox, FMX.Memo;

type
  TPyDelphiCustomMemo = class(TPyDelphiCustomPresentedScrollBox)
  private
    function GetDelphiObject: TCustomMemo;
    procedure SetDelphiObject(const Value: TCustomMemo);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomMemo read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiMemo = class(TPyDelphiCustomMemo)
  private
    function GetDelphiObject: TMemo;
    procedure SetDelphiObject(const Value: TMemo);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TMemo read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphi;

{ Register the wrappers, the globals and the constants }
type
  TMemoRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TPyDelphiCustomMemo }

class function TPyDelphiCustomMemo.DelphiObjectClass: TClass;
begin
  Result := TCustomMemo;
end;

function TPyDelphiCustomMemo.GetDelphiObject: TCustomMemo;
begin
  Result := TCustomMemo(inherited DelphiObject);
end;

procedure TPyDelphiCustomMemo.SetDelphiObject(const Value: TCustomMemo);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMemo }

class function TPyDelphiMemo.DelphiObjectClass: TClass;
begin
  Result := TMemo;
end;

function TPyDelphiMemo.GetDelphiObject: TMemo;
begin
  Result := TMemo(inherited DelphiObject);
end;

procedure TPyDelphiMemo.SetDelphiObject(const Value: TMemo);
begin
  inherited DelphiObject := Value;
end;

{ TMemoRegistration }

function TMemoRegistration.Name: string;
begin
  Result := 'Memo';
end;

procedure TMemoRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomMemo);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMemo);
end;

initialization
  RegisteredUnits.Add(TMemoRegistration.Create());

end.
