{$I ..\Definition.Inc}
unit WrapFmxStyles;

interface

uses
  WrapDelphi, FMX.Styles, PythonEngine;

type
  TPyDelphiStyleStreaming = class(TPyDelphiObject)
  private
    function GetDelphiObject: TStyleStreaming;
    procedure SetDelphiObject(const Value: TStyleStreaming);
  public
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith(APythonType: TPythonType; args: PPyObject); override;
    class function DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    property DelphiObject: TStyleStreaming read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStyleManager = class(TPyDelphiObject)
  private
    function GetDelphiObject: TStyleManager;
    procedure SetDelphiObject(const Value: TStyleManager);
  public
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
    class function DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    property DelphiObject: TStyleManager read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  FMX.Types;

{ Register the wrappers, the globals and the constants }
type
  TFmxStylesRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TFmxStylesRegistration }
procedure TFmxStylesRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TFmxStylesRegistration.Name: string;
begin
  Result := 'Styles';
end;

procedure TFmxStylesRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStyleStreaming);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStyleManager);
end;

{ TPyDelphiStyleStreaming }
constructor TPyDelphiStyleStreaming.Create(APythonType: TPythonType);
begin
  inherited;
end;

constructor TPyDelphiStyleStreaming.CreateWith(APythonType: TPythonType; args: PPyObject);
begin
  inherited;
  DelphiObject := TStyleStreaming.Create();
end;

class function TPyDelphiStyleStreaming.DelphiObjectClass: TClass;
begin
  Result := TStyleStreaming;
end;

function TPyDelphiStyleStreaming.GetDelphiObject: TStyleStreaming;
begin
  Result := TStyleStreaming(inherited DelphiObject);
end;

class procedure TPyDelphiStyleStreaming.RegisterGetSets(
  PythonType: TPythonType);
begin
end;

class procedure TPyDelphiStyleStreaming.RegisterMethods(
  PythonType: TPythonType);
begin
end;

procedure TPyDelphiStyleStreaming.SetDelphiObject(const Value: TStyleStreaming);
begin
  inherited DelphiObject := Value;
end;
{ TPyDelphiStyleManager }
constructor TPyDelphiStyleManager.Create(APythonType: TPythonType);
begin
  inherited;
end;
constructor TPyDelphiStyleManager.CreateWith(APythonType: TPythonType; args,
    kwds: PPyObject);
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
class procedure TPyDelphiStyleManager.RegisterGetSets(PythonType: TPythonType);
begin
end;

class procedure TPyDelphiStyleManager.RegisterMethods(PythonType: TPythonType);
begin
end;

procedure TPyDelphiStyleManager.SetDelphiObject(const Value: TStyleManager);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TFmxStylesRegistration.Create());
end.
