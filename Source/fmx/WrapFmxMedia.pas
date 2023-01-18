{$I ..\Definition.Inc}
unit WrapFmxMedia;

interface

uses
  System.TypInfo, FMX.Media,
  PythonEngine, WrapDelphi,
  WrapFmxTypes, WrapFmxControls, WrapFmxActnList, WrapFmxStdActns;

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

  //Media player wrappers
  TPyDelphiCustomMediaCodec = class(TPyDelphiObject)
  private
    function GetDelphiObject: TCustomMediaCodec;
    procedure SetDelphiObject(const Value: TCustomMediaCodec);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TCustomMediaCodec read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMedia = class(TPyDelphiObject)
  private
    function GetDelphiObject: TMedia;
    procedure SetDelphiObject(const Value: TMedia);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMedia read GetDelphiObject
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

  TPyDelphiCustomMediaPlayerAction = class(TPyDelphiCustomAction)
  private
    function GetDelphiObject: TCustomMediaPlayerAction;
    procedure SetDelphiObject(const Value: TCustomMediaPlayerAction);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TCustomMediaPlayerAction read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerStart = class(TPyDelphiCustomMediaPlayerAction)
  private
    function GetDelphiObject: TMediaPlayerStart;
    procedure SetDelphiObject(const Value: TMediaPlayerStart);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerStart read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerStop = class(TPyDelphiCustomMediaPlayerAction)
  private
    function GetDelphiObject: TMediaPlayerStop;
    procedure SetDelphiObject(const Value: TMediaPlayerStop);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerStop read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerPause = class(TPyDelphiCustomMediaPlayerAction)
  private
    function GetDelphiObject: TMediaPlayerPlayPause;
    procedure SetDelphiObject(const Value: TMediaPlayerPlayPause);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerPlayPause read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerValue = class(TPyDelphiCustomValueRangeAction)
  private
    function GetDelphiObject: TMediaPlayerValue;
    procedure SetDelphiObject(const Value: TMediaPlayerValue);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerValue read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerCurrentTime = class(TPyDelphiMediaPlayerValue)
  private
    function GetDelphiObject: TMediaPlayerCurrentTime;
    procedure SetDelphiObject(const Value: TMediaPlayerCurrentTime);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerCurrentTime read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiMediaPlayerVolume = class(TPyDelphiMediaPlayerValue)
  private
    function GetDelphiObject: TMediaPlayerVolume;
    procedure SetDelphiObject(const Value: TMediaPlayerVolume);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TMediaPlayerVolume read GetDelphiObject
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
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomMediaCodec);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayer);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMedia);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomMediaPlayerAction);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerStart);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerStop);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerPause);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerValue);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerCurrentTime);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayerVolume);

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
end;

class procedure TPyDelphiCameraComponent.RegisterMethods
  (PythonType: TPythonType);
begin
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

{ TPyDelphiCustomMediaCodec }

class function TPyDelphiCustomMediaCodec.DelphiObjectClass: TClass;
begin
  Result := TCustomMediaCodec;
end;

function TPyDelphiCustomMediaCodec.GetDelphiObject: TCustomMediaCodec;
begin
  Result := TCustomMediaCodec(inherited DelphiObject);
end;

procedure TPyDelphiCustomMediaCodec.SetDelphiObject(
  const Value: TCustomMediaCodec);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMedia }

class function TPyDelphiMedia.DelphiObjectClass: TClass;
begin
  Result := TMedia;
end;

function TPyDelphiMedia.GetDelphiObject: TMedia;
begin
  Result := TMedia(inherited DelphiObject);
end;

procedure TPyDelphiMedia.SetDelphiObject(const Value: TMedia);
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

{ TPyDelphiCustomMediaPlayerAction }

class function TPyDelphiCustomMediaPlayerAction.DelphiObjectClass: TClass;
begin
  Result := TCustomMediaPlayerAction;
end;

function TPyDelphiCustomMediaPlayerAction.GetDelphiObject: TCustomMediaPlayerAction;
begin
  Result := TCustomMediaPlayerAction(inherited DelphiObject);
end;

procedure TPyDelphiCustomMediaPlayerAction.SetDelphiObject(
  const Value: TCustomMediaPlayerAction);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerStart }

class function TPyDelphiMediaPlayerStart.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerStart;
end;

function TPyDelphiMediaPlayerStart.GetDelphiObject: TMediaPlayerStart;
begin
  Result := TMediaPlayerStart(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerStart.SetDelphiObject(
  const Value: TMediaPlayerStart);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerStop }

class function TPyDelphiMediaPlayerStop.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerStop;
end;

function TPyDelphiMediaPlayerStop.GetDelphiObject: TMediaPlayerStop;
begin
  Result := TMediaPlayerStop(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerStop.SetDelphiObject(
  const Value: TMediaPlayerStop);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerPause }

class function TPyDelphiMediaPlayerPause.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerPlayPause;
end;

function TPyDelphiMediaPlayerPause.GetDelphiObject: TMediaPlayerPlayPause;
begin
  Result := TMediaPlayerPlayPause(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerPause.SetDelphiObject(
  const Value: TMediaPlayerPlayPause);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerValue }

class function TPyDelphiMediaPlayerValue.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerValue;
end;

function TPyDelphiMediaPlayerValue.GetDelphiObject: TMediaPlayerValue;
begin
  Result := TMediaPlayerValue(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerValue.SetDelphiObject(
  const Value: TMediaPlayerValue);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerCurrentTime }

class function TPyDelphiMediaPlayerCurrentTime.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerCurrentTime;
end;

function TPyDelphiMediaPlayerCurrentTime.GetDelphiObject: TMediaPlayerCurrentTime;
begin
  Result := TMediaPlayerCurrentTime(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerCurrentTime.SetDelphiObject(
  const Value: TMediaPlayerCurrentTime);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiMediaPlayerVolume }

class function TPyDelphiMediaPlayerVolume.DelphiObjectClass: TClass;
begin
  Result := TMediaPlayerVolume;
end;

function TPyDelphiMediaPlayerVolume.GetDelphiObject: TMediaPlayerVolume;
begin
  Result := TMediaPlayerVolume(inherited DelphiObject);
end;

procedure TPyDelphiMediaPlayerVolume.SetDelphiObject(
  const Value: TMediaPlayerVolume);
begin
  inherited DelphiObject := Value;
end;

initialization

RegisteredUnits.Add(TFMXMediaRegistration.Create());

end.
