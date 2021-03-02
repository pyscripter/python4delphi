{$I ..\Definition.Inc}

unit WrapVclButtons;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses,
  WrapVclControls, Buttons;

type
  TPyDelphiSpeedButton = class (TPyDelphiControl)
  private
    function  GetDelphiObject: TSpeedButton;
    procedure SetDelphiObject(const Value: TSpeedButton);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TSpeedButton read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiBitBtn = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TBitBtn;
    procedure SetDelphiObject(const Value: TBitBtn);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TBitBtn read GetDelphiObject write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TButtonsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TButtonsRegistration }

procedure TButtonsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('bkCustom',      bkCustom);
  APyDelphiWrapper.DefineVar('bkOK',          bkOK);
  APyDelphiWrapper.DefineVar('bkCancel',      bkCancel);
  APyDelphiWrapper.DefineVar('bkHelp',        bkHelp);
  APyDelphiWrapper.DefineVar('bkYes',         bkYes);
  APyDelphiWrapper.DefineVar('bkNo',          bkNo);
  APyDelphiWrapper.DefineVar('bkClose',       bkClose);
  APyDelphiWrapper.DefineVar('bkAbort',       bkAbort);
  APyDelphiWrapper.DefineVar('bkRetry',       bkRetry);
  APyDelphiWrapper.DefineVar('bkIgnore',      bkIgnore);
  APyDelphiWrapper.DefineVar('bkAll',         bkAll);
end;

function TButtonsRegistration.Name: string;
begin
  Result := 'Buttons';
end;

procedure TButtonsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSpeedButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiBitBtn);
end;

{ TPyDelphiSpeedButton }

class function TPyDelphiSpeedButton.DelphiObjectClass: TClass;
begin
  Result := TSpeedButton;
end;

function TPyDelphiSpeedButton.GetDelphiObject: TSpeedButton;
begin
  Result := TSpeedButton(inherited DelphiObject);
end;

procedure TPyDelphiSpeedButton.SetDelphiObject(const Value: TSpeedButton);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiBitBtn }

class function TPyDelphiBitBtn.DelphiObjectClass: TClass;
begin
  Result := TBitBtn;
end;

function TPyDelphiBitBtn.GetDelphiObject: TBitBtn;
begin
  Result := TBitBtn(inherited DelphiObject);
end;

procedure TPyDelphiBitBtn.SetDelphiObject(const Value: TBitBtn);
begin
  inherited DelphiObject := Value;
end;


initialization
  RegisteredUnits.Add( TButtonsRegistration.Create );
end.
