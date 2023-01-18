{$I ..\Definition.Inc}

unit WrapFmxForms;

interface

uses
  System.Classes, System.SysUtils, FMX.Forms,
  PythonEngine, WrapFmxTypes, WrapDelphiClasses, WrapFmxControls, WrapDelphi,
  System.TypInfo, System.UITypes;

type
  TPyDelphiApplication = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TApplication;
    procedure SetDelphiObject(const Value: TApplication);
  protected
    // Class methods
    function Initialize_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
    function Run_Wrapper(AArgs: PPyObject): PPyObject; cdecl;
  public
    constructor Create( APythonType : TPythonType ); override;
    // Class methods
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterMethods(APythonType: TPythonType); override;
    // Properties
    property DelphiObject: TApplication read GetDelphiObject write SetDelphiObject;
  end;

  TCloseQueryEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var CanClose : Boolean);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TCloseEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TPyDelphiCommonCustomForm = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TCommonCustomForm;
    procedure SetDelphiObject(const Value: TCommonCustomForm);
    function HasFormRes(const AClass: TClass): boolean;
  protected
    //Load properties from .pydfm file
    function LoadProps_Wrapper(args : PPyObject) : PPyObject; cdecl;
  public
    function CreateComponent(AOwner: TComponent): TComponent; override;
    // Class methods
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    // Properties
    property DelphiObject: TCommonCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomForm = class(TPyDelphiCommonCustomForm)
  private
    function GetDelphiObject: TCustomForm;
    procedure SetDelphiObject(const Value: TCustomForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPopupForm = class(TPyDelphiCustomForm)
  private
    function GetDelphiObject: TCustomPopupForm;
    procedure SetDelphiObject(const Value: TCustomPopupForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomPopupForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiForm = class(TPyDelphiCustomForm)
  private
    function GetDelphiObject: TForm;
    procedure SetDelphiObject(const Value: TForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiFrame = class(TPyDelphiControl)
  private
    function GetDelphiObject: TFrame;
    procedure SetDelphiObject(const Value: TFrame);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TFrame read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiScreen = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TScreen;
    procedure SetDelphiObject(const Value: TScreen);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TScreen read GetDelphiObject write SetDelphiObject;
  end;

  EInvalidFormClass = class(Exception);

implementation

uses
  {$IFDEF OSX}
  Macapi.AppKit,
  FMX.Platform.Mac,
  {$ENDIF OSX}
  System.Types,
  System.IOUtils,
  System.Rtti,
  System.Messaging;

{$IFDEF OSX}
var
  gDelphiMainForm: TCommonCustomForm;
  gPythonMainForm: TCommonCustomForm;
  gFormsCreatedSubscription: integer;
{$ENDIF OSX}

{ Register the wrappers, the globals and the constants }
type
  TFormsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{$IFDEF OSX}
  TInternalMainForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;
{$ENDIF}

{ TFormsRegistration }

procedure TFormsRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TFormsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  // Singletons
  APyDelphiWrapper.DefineVar('Application', Application);
  APyDelphiWrapper.DefineVar('Screen', Screen);
end;

function TFormsRegistration.Name: string;
begin
  Result := 'Forms';
end;

procedure TFormsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiApplication);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCommonCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPopupForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFrame);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScreen);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TCloseQueryEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TCloseEventHandler);
end;

{ TPyDelphiApplication }

constructor TPyDelphiApplication.Create(APythonType: TPythonType);
begin
  inherited;
  {$IFDEF OSX}
  gFormsCreatedSubscription := TMessageManager.DefaultManager.SubscribeToMessage(
    TFormsCreatedMessage,
    procedure(const Sender: TObject; const M: TMessage) begin
      if (Sender = Application) and Assigned(gDelphiMainForm) then begin
        if Assigned(gPythonMainForm) then begin
          Application.MainForm := gPythonMainForm;
          gPythonMainForm.Hide();
          gPythonMainForm.Show();
        end;
        FreeAndNil(gDelphiMainForm);
        gPythonMainForm := nil;
        TMessageManager.DefaultManager.Unsubscribe(TFormsCreatedMessage,
          gFormsCreatedSubscription);
      end;
    end);
  {$ENDIF OSX}
end;

class function TPyDelphiApplication.DelphiObjectClass: TClass;
begin
  Result := TApplication;
end;

function TPyDelphiApplication.GetDelphiObject: TApplication;
begin
  Result := TApplication(inherited DelphiObject);
end;

procedure TPyDelphiApplication.SetDelphiObject(const Value: TApplication);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiApplication.Initialize_Wrapper(AArgs: PPyObject): PPyObject;
{$IFDEF OSX}
var
  App: NSApplication;
{$ENDIF OSX}
begin
  Application.Initialize();
  {$IFDEF OSX}
  // #397
  App := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
   if App.ActivationPolicy > 0 then
     App.setActivationPolicy(0);
  //the main form standard creation way,
  //due to MainMenu creation and others.
  Application.CreateForm(TInternalMainForm, gDelphiMainForm);
  {$ENDIF OSX}
  Result := GetPythonEngine().ReturnNone();
end;

function TPyDelphiApplication.Run_Wrapper(AArgs: PPyObject): PPyObject;
begin
  {$IFDEF OSX}
  gPythonMainForm := Application.MainForm;
  Application.MainForm := nil;
  {$ENDIF OSX}
  Application.Run();
  Result := GetPythonEngine().ReturnNone();
end;

class procedure TPyDelphiApplication.RegisterMethods(APythonType: TPythonType);
begin
  with APythonType do begin
    AddMethod('Initialize', @TPyDelphiApplication.Initialize_Wrapper,
      'TApplication.Initialize()'#10 +
      'Initialize the application');
    AddMethod('Run', @TPyDelphiApplication.Run_Wrapper,
      'TApplication.Run()'#10 +
      'Run the application');
  end;
end;

{ TCloseQueryEventHandler }

constructor TCloseQueryEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TCloseQueryEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TCloseQueryEventHandler.DoEvent(Sender: TObject;
  var CanClose: Boolean);
var
  LPyObject, LPyTuple, LPyResult, LPyCanClose : PPyObject;
  LVarParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyCanClose := CreateVarParam(PyDelphiWrapper, CanClose);
      LVarParam := PythonToDelphi(LPyCanClose) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyCanClose);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
          CanClose := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TCloseQueryEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TCloseQueryEvent);
end;

{ TCloseEventHandler }

constructor TCloseEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod : TMethod;
begin
  inherited;
  LMethod.Code := @TCloseEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TCloseEventHandler.DoEvent(Sender: TObject; var Action: TCloseAction);
var
  LPyObject, LPyTuple, LPyResult, LPyAction : PPyObject;
  LVarParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyAction := CreateVarParam(PyDelphiWrapper, NativeInt(Action));
      LVarParam := PythonToDelphi(LPyAction) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 0, LPyObject);
      GetPythonEngine.PyTuple_SetItem(LPyTuple, 1, LPyAction);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
        begin
          Py_DECREF(LPyResult);
          if PyLong_Check(LVarParam.Value) and
             CheckEnum('TCloseAction', PyLong_AsLong(LVarParam.Value), Ord(Low(TCloseAction)), Ord(High(TCloseAction))) then
            Action := TCloseAction(PyLong_AsLong(LVarParam.Value));
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError;
    end;
end;

class function TCloseEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TCloseEvent);
end;

{ TPyDelphiCommonCustomForm }

function TPyDelphiCommonCustomForm.CreateComponent(
  AOwner: TComponent): TComponent;
type
  TCommonCustomFormClass = class of TCommonCustomForm;
var
  LClass: TClass;
  LFormClass: TCommonCustomFormClass;
  LClassName: string;
begin
  LFormClass := nil;
  //get de default form class
  if DelphiObjectClass.InheritsFrom(TCommonCustomForm) then
    LFormClass := TCommonCustomFormClass(DelphiObjectClass);

  //if we have a subclass of our Form wrapper, then check if we can find a
  //Delphi class that would have the same name as the Python class.
  //This would allow Python to instanciate an existing Delphi form class,
  //insted of only using a blank form.
  if (ob_type <> PythonType.TheTypePtr) then begin
    LClassName := string(ob_type.tp_name);
    LClass := GetClass(LClassName);
    if not Assigned(LClass) then
      LClass := GetClass('T' + LClassName);
    if Assigned(LClass) and LClass.InheritsFrom(TCommonCustomForm) then
      LFormClass := TCommonCustomFormClass(LClass);
  end;

  if not Assigned(LFormClass) then
    raise EInvalidFormClass.CreateFmt('Type %s is not a valid form class', [
      DelphiObjectClass.ClassName]);

  //if it's not a design form, so we create it as a non-resourced form,
  //using the non-resourced constructor.
  //if the Owner is TApplication, then we have to call its CreateForm method,
  //otherwise we won't have a mian form defined, as the main form is the first
  //created form. Of course, this is a concern only when Python controls all the
  //GUI and calls Apllication.Run by itself.
  if not HasFormRes(LFormClass) then
    Result := LFormClass.CreateNew(AOwner)
  else if (AOwner = Application) then
    Application.CreateForm(LFormClass, Result)
  else
    Result := LFormClass.Create(AOwner);
end;

class function TPyDelphiCommonCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCommonCustomForm;
end;

function TPyDelphiCommonCustomForm.GetDelphiObject: TCommonCustomForm;
begin
  Result := TCommonCustomForm(inherited DelphiObject);
end;

function TPyDelphiCommonCustomForm.HasFormRes(const AClass: TClass): boolean;
begin
  Result := FindResource(
    FindResourceHInstance(FindClassHInstance(AClass)),
    PChar(AClass.ClassName), PChar(RT_RCDATA)) <> 0;
end;

function TPyDelphiCommonCustomForm.LoadProps_Wrapper(
  args: PPyObject): PPyObject;

  function FindResource(): string;
  var
    LStr: PAnsiChar;
  begin
    with GetPythonEngine() do begin
      if PyArg_ParseTuple(args, 's:LoadProps', @LStr) <> 0 then begin
        Result := string(LStr);
      end else
        Result := String.Empty;
    end;
  end;

begin
  Adjust(@Self);
  try
    if InternalReadComponent(FindResource(), DelphiObject) then
      Exit(GetPythonEngine().ReturnTrue);
  except
    on E: Exception do
      with GetPythonEngine do
        PyErr_SetString(PyExc_RuntimeError^, PAnsiChar(AnsiString(E.Message)));
  end;
  Result := nil;
end;

class procedure TPyDelphiCommonCustomForm.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('LoadProps', @TPyDelphiCustomForm.LoadProps_Wrapper,
    'TCommonCustomForm.LoadProps()'#10 +
    'Load properties from a .pydfm file');
end;

procedure TPyDelphiCommonCustomForm.SetDelphiObject(
  const Value: TCommonCustomForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomForm }

class function TPyDelphiCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCustomForm;
end;

function TPyDelphiCustomForm.GetDelphiObject: TCustomForm;
begin
  Result := TCustomForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomForm.SetDelphiObject(const Value: TCustomForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPopupForm }

class function TPyDelphiCustomPopupForm.DelphiObjectClass: TClass;
begin
  Result := TCustomPopupForm;
end;

function TPyDelphiCustomPopupForm.GetDelphiObject: TCustomPopupForm;
begin
  Result := TCustomPopupForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomPopupForm.SetDelphiObject(
  const Value: TCustomPopupForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiForm }

class function TPyDelphiForm.DelphiObjectClass: TClass;
begin
  Result := TForm;
end;

function TPyDelphiForm.GetDelphiObject: TForm;
begin
  Result := TForm(inherited DelphiObject);
end;

procedure TPyDelphiForm.SetDelphiObject(const Value: TForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFrame }

class function TPyDelphiFrame.DelphiObjectClass: TClass;
begin
  Result := TFrame;
end;

function TPyDelphiFrame.GetDelphiObject: TFrame;
begin
  Result := TFrame(inherited DelphiObject);
end;

procedure TPyDelphiFrame.SetDelphiObject(const Value: TFrame);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiScreen }

class function TPyDelphiScreen.DelphiObjectClass: TClass;
begin
  Result := TScreen;
end;

function TPyDelphiScreen.GetDelphiObject: TScreen;
begin
  Result := TScreen(inherited DelphiObject);
end;

procedure TPyDelphiScreen.SetDelphiObject(const Value: TScreen);
begin
  inherited DelphiObject := Value;
end;

{$IFDEF OSX}

{ TInternalMainForm }

constructor TInternalMainForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  Name := '_InternalDelphiMainForm';
  Left := -10;
  ClientHeight := 1;
  ClientWidth := 1;
end;

{$ENDIF OSX}

Initialization
  RegisteredUnits.Add(TFormsRegistration.Create);

end.
