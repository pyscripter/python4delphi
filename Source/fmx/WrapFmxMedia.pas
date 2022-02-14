{$I ..\Definition.Inc}
unit WrapFmxMedia;

interface

uses
  System.TypInfo,  FMX.Media,
  PythonEngine, WrapDelphi, WrapFmxTypes, WrapFmxControls;

type
  TSampleBufferReadyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; const ATime: TMediaTime);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;
    class function GetTypeInfo: PTypeInfo; override;
  end;

  TPyDelphiCameraComponent = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TCameraComponent;
    procedure SetDelphiObject(const Value: TCameraComponent);
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
  public
    property DelphiObject: TCameraComponent read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayer = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TMediaPlayer;
    procedure SetDelphiObject(const Value: TMediaPlayer);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayer read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerControl = class(TPyDelphiControl)
  private
    function GetDelphiObject: TMediaPlayerControl;
    procedure SetDelphiObject(const Value: TMediaPlayerControl);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerControl read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

type
  TFMXMediaRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

  { TFMXMediaRegistration }

function TFMXMediaRegistration.Name: string;
begin
  Result := 'Media';
end;

procedure TFMXMediaRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TFMXMediaRegistration.RegisterWrappers(APyDelphiWrapper
  : TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCameraComponent);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayer);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerControl);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TSampleBufferReadyEventHandler);
end;

{ TSampleBufferReadyEventHandler }

constructor TSampleBufferReadyEventHandler.Create(PyDelphiWrapper
  : TPyDelphiWrapper; Component: TObject; PropertyInfo: PPropInfo;
  Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TSampleBufferReadyEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TSampleBufferReadyEventHandler.DoEvent(Sender: TObject;
  const ATime: TMediaTime);
var
  PySender, PyTuple, PyResult, PyTime : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PySender := PyDelphiWrapper.Wrap(Sender);
      PyTime := PyLong_FromLong(ATime);
      PyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PySender);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyTime);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        Py_XDECREF(PyResult);
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TSampleBufferReadyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TSampleBufferReadyEvent);
end;

{ TPyDelphiCameraComponent }

class function TPyDelphiCameraComponent.DelphiObjectClass: TClass;
begin
  Result := TCameraComponent;
end;

class procedure TPyDelphiCameraComponent.RegisterGetSets
  (PythonType: TPythonType);
begin
  inherited;
end;

class procedure TPyDelphiCameraComponent.RegisterMethods
  (PythonType: TPythonType);
begin
  inherited;
end;

function TPyDelphiCameraComponent.GetDelphiObject: TCameraComponent;
begin
  Result := TCameraComponent(inherited DelphiObject);
end;

procedure TPyDelphiCameraComponent.SetDelphiObject
  (const Value: TCameraComponent);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayer }

class function TPyDelphiMediaPlayer.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayer;
end;

function TPyDelphiMediaPlayer.GetDelphiObject: TMediaPlayer;
begin
  Result := TMediaPlayer(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayer.SetDelphiObject(const Value: TMediaPlayer);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerControl }

class function TPyDelphiMediaPlayerControl.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerControl;
end;

function TPyDelphiMediaPlayerControl.GetDelphiObject: TMediaPlayerControl;
begin
  Result := TMediaPlayerControl(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerControl.SetDelphiObject(
  const Value: TMediaPlayerControl);
begin
  inherited DelphiObject := Value;
end;

initialization

RegisteredUnits.Add(TFMXMediaRegistration.Create());

end.
