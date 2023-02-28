unit WrapFmxDateTime;

interface

uses
  Classes,
  FMX.DateTimeCtrls, FMX.Calendar,
  PythonEngine, WrapDelphi, WrapFmxControls;

type
  {|||| FMX.DateTimeCtrls ||||}

  TPyDelphiCustomDateTimeEdit = class(TPyDelphiTextControl)
  private
    function GetDelphiObject: TCustomDateTimeEdit;
    procedure SetDelphiObject(const Value: TCustomDateTimeEdit);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomDateTimeEdit read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomTimeEdit = class(TPyDelphiCustomDateTimeEdit)
  private
    function GetDelphiObject: TCustomTimeEdit;
    procedure SetDelphiObject(const Value: TCustomTimeEdit);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomTimeEdit read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTimeEdit = class(TPyDelphiCustomTimeEdit)
  private
    function GetDelphiObject: TTimeEdit;
    procedure SetDelphiObject(const Value: TTimeEdit);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTimeEdit read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomDateEdit = class(TPyDelphiCustomDateTimeEdit)
  private
    function GetDelphiObject: TCustomDateEdit;
    procedure SetDelphiObject(const Value: TCustomDateEdit);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomDateEdit read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiDateEdit = class(TPyDelphiCustomDateEdit)
  private
    function GetDelphiObject: TDateEdit;
    procedure SetDelphiObject(const Value: TDateEdit);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TDateEdit read GetDelphiObject write SetDelphiObject;
  end;

  {|||| FMX.Calendar ||||}

  TPyDelphiCustomCalendar = class(TPyDelphiPresentedControl)
  private
    function GetDelphiObject: TCustomCalendar;
    procedure SetDelphiObject(const Value: TCustomCalendar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomCalendar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCalendar = class(TPyDelphiCustomCalendar)
  private
    function GetDelphiObject: TCalendar;
    procedure SetDelphiObject(const Value: TCalendar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCalendar read GetDelphiObject write SetDelphiObject;
  end;

implementation

type
{ Register the wrappers, the globals and the constants }
  TDateTimeRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TDateTimeRegistration }

function TDateTimeRegistration.Name: string;
begin
  Result := 'DateTimeCtrls';
end;

procedure TDateTimeRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomDateTimeEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomTimeEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTimeEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomDateEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDateEdit);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomCalendar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCalendar);
end;

{ TPyDelphiCustomDateTimeEdit }

class function TPyDelphiCustomDateTimeEdit.DelphiObjectClass: TClass;
begin
  Result := TCustomDateTimeEdit;
end;

function TPyDelphiCustomDateTimeEdit.GetDelphiObject: TCustomDateTimeEdit;
begin
  Result := TCustomDateTimeEdit(inherited DelphiObject);
end;

procedure TPyDelphiCustomDateTimeEdit.SetDelphiObject(
  const Value: TCustomDateTimeEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomTimeEdit }

class function TPyDelphiCustomTimeEdit.DelphiObjectClass: TClass;
begin
  Result := TCustomTimeEdit;
end;

function TPyDelphiCustomTimeEdit.GetDelphiObject: TCustomTimeEdit;
begin
  Result := TCustomTimeEdit(inherited DelphiObject);
end;

procedure TPyDelphiCustomTimeEdit.SetDelphiObject(const Value: TCustomTimeEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTimeEdit }

class function TPyDelphiTimeEdit.DelphiObjectClass: TClass;
begin
  Result := TTimeEdit;
end;

function TPyDelphiTimeEdit.GetDelphiObject: TTimeEdit;
begin
  Result := TTimeEdit(inherited DelphiObject);
end;

procedure TPyDelphiTimeEdit.SetDelphiObject(const Value: TTimeEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomDateEdit }

class function TPyDelphiCustomDateEdit.DelphiObjectClass: TClass;
begin
  Result := TCustomDateEdit;
end;

function TPyDelphiCustomDateEdit.GetDelphiObject: TCustomDateEdit;
begin
  Result := TCustomDateEdit(inherited DelphiObject);
end;

procedure TPyDelphiCustomDateEdit.SetDelphiObject(const Value: TCustomDateEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiDateEdit }

class function TPyDelphiDateEdit.DelphiObjectClass: TClass;
begin
  Result := TDateEdit;
end;

function TPyDelphiDateEdit.GetDelphiObject: TDateEdit;
begin
  Result := TDateEdit(inherited DelphiObject);
end;

procedure TPyDelphiDateEdit.SetDelphiObject(const Value: TDateEdit);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomCalendar }

class function TPyDelphiCustomCalendar.DelphiObjectClass: TClass;
begin
  Result := TCustomCalendar;
end;

function TPyDelphiCustomCalendar.GetDelphiObject: TCustomCalendar;
begin
  Result := TCustomCalendar(inherited DelphiObject);
end;

procedure TPyDelphiCustomCalendar.SetDelphiObject(const Value: TCustomCalendar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCalendar }

class function TPyDelphiCalendar.DelphiObjectClass: TClass;
begin
  Result := TCalendar;
end;

function TPyDelphiCalendar.GetDelphiObject: TCalendar;
begin
  Result := TCalendar(inherited DelphiObject);
end;

procedure TPyDelphiCalendar.SetDelphiObject(const Value: TCalendar);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TDateTimeRegistration.Create);

end.
