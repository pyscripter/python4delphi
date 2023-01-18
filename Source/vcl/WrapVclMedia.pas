{$I ..\Definition.Inc}
unit WrapVclMedia;

interface

uses
  System.TypInfo, Vcl.MPlayer,
  PythonEngine, WrapDelphi, WrapVclControls;

type
  TEMPNotifyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Button: TMPBtnType;
      var DoDefault: Boolean);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TEMPPostNotifyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Button: TMPBtnType);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TPyDelphiMediaPlayer = class (TPyDelphiCustomControl)
  private
    function  GetDelphiObject: TMediaPlayer;
    procedure SetDelphiObject(const Value: TMediaPlayer);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TMediaPlayer read GetDelphiObject write SetDelphiObject;
  end;

implementation

type
  TMediaRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TVclMediaRegistration }

function TMediaRegistration.Name: string;
begin
  Result := 'Media';
end;

procedure TMediaRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TMediaRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMediaPlayer);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TEMPNotifyEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TEMPPostNotifyEventHandler);
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

{ TEMPNotifyEventHandler }

constructor TEMPNotifyEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TEMPNotifyEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TEMPNotifyEventHandler.DoEvent(Sender: TObject; Button: TMPBtnType;
  var DoDefault: Boolean);
var
  LPyObject: PPyObject;
  LPyTuple: PPyObject;
  LPyResult: PPyObject;
  LPyButton: PPyObject;
  LDoDefault: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyButton := PyLong_FromLong(Integer(Button));
      LDoDefault := CreateVarParam(PyDelphiWrapper, DoDefault);
      LVarParam := PythonToDelphi(LDoDefault) as TPyDelphiVarParameter;

      LPyTuple := PyTuple_New(3);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyButton);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 2, LDoDefault);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DoDefault := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

class function TEMPNotifyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(EMPNotify);
end;

{ TEMPPostNotifyEventHandler }

constructor TEMPPostNotifyEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TEMPPostNotifyEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TEMPPostNotifyEventHandler.DoEvent(Sender: TObject;
  Button: TMPBtnType);
var
  LPyObject: PPyObject;
  LPyTuple: PPyObject;
  LPyResult: PPyObject;
  LPyButton: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyButton := PyLong_FromLong(Integer(Button));

      LPyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyButton);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

class function TEMPPostNotifyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(EMPPostNotify);
end;

initialization
  RegisteredUnits.Add(TMediaRegistration.Create);

end.
