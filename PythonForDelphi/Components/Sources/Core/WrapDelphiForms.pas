{$I Definition.Inc}

unit WrapDelphiForms;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses, WrapDelphiControls,
  Windows, Forms, Graphics, TypInfo;

type
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

  {
     PyObject wrapping TCustomForm
     Exposes methods ShowModal, Close, CloseQuery, Release
     Exposes property ModalResult
  }
  TPyDelphiCustomForm = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TCustomForm;
    procedure SetDelphiObject(const Value: TCustomForm);
  protected
    function  CreateComponent(AOwner : TComponent) : TComponent; override;
    // Exposed Methods
    function Close_Wrapper(args: PPyObject): PPyObject; cdecl;
    function CloseQuery_Wrapper(args: PPyObject): PPyObject; cdecl;
    function ShowModal_Wrapper(args: PPyObject): PPyObject; cdecl;
    function Release_Wrapper(args : PPyObject) : PPyObject; cdecl;
    // Property Getters
    function Get_ModalResult(AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_ModalResult(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiForm = class (TPyDelphiCustomForm)
  private
    function  GetDelphiObject: TForm;
    procedure SetDelphiObject(const Value: TForm);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TForm read GetDelphiObject write SetDelphiObject;
  end;

  {
     PyObject wrapping TMonitor
     Exposes properties Handle, MonitorNum, Left, Height, Top, Width, BoundsRect, WorkareaRect, Primary
  }
  TPyDelphiMonitor = class (TPyDelphiObject)
  private
    function  GetDelphiObject: TMonitor;
    procedure SetDelphiObject(const Value: TMonitor);
  protected
    function Get_Handle(AContext : Pointer): PPyObject; cdecl;
    function Get_MonitorNum(AContext : Pointer): PPyObject; cdecl;
    function Get_Left(AContext : Pointer): PPyObject; cdecl;
    function Get_Height(AContext : Pointer): PPyObject; cdecl;
    function Get_Top(AContext : Pointer): PPyObject; cdecl;
    function Get_Width(AContext : Pointer): PPyObject; cdecl;
    function Get_BoundsRect(AContext : Pointer): PPyObject; cdecl;
    function Get_WorkareaRect(AContext : Pointer): PPyObject; cdecl;
    function Get_Primary(AContext : Pointer): PPyObject; cdecl;
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TMonitor read GetDelphiObject write SetDelphiObject;
  end;

  TBaseScreenAccess = class(TContainerAccess)
  private
    function GetContainer: TScreen;
  public
    class function ExpectedContainerClass : TClass; override;

    property Container : TScreen read GetContainer;
  end;

  {
    Access to the custom forms of the Screen singleton.
  }
  TScreenCustomFormsAccess = class(TBaseScreenAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    //function IndexOf(AValue : PPyObject) : Integer; override;

    //class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;
  end;

  {
    Access to the forms of the Screen singleton.
  }
  TScreenFormsAccess = class(TBaseScreenAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    //function IndexOf(AValue : PPyObject) : Integer; override;

    //class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;
  end;

  {
    Access to the TDataModule items of the Screen singleton.
  }
  TScreenDataModulesAccess = class(TBaseScreenAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    //function IndexOf(AValue : PPyObject) : Integer; override;

    //class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;
  end;

  {
    Access to the Cursors of the Screen singleton.
  }
  TScreenCursorsAccess = class(TBaseScreenAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    //function IndexOf(AValue : PPyObject) : Integer; override;

    //class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;
  end;

  {
    Access to the Cursors of the Screen singleton.
  }
  TScreenMonitorsAccess = class(TBaseScreenAccess)
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    //function IndexOf(AValue : PPyObject) : Integer; override;

    //class function SupportsIndexOf : Boolean; override;
    class function Name : String; override;
  end;

  {
     PyObject wrapping TScreen
     Exposes methods DisableAlign, EnableAlign, Realign, ResetFonts
     Exposes all properties of TScreen
  }
  TPyDelphiScreen = class (TPyDelphiComponent)
  private
    fOnActiveControlChange: PPyObject;
    fOnActiveFormChange: PPyObject;
    function  GetDelphiObject: TScreen;
    procedure SetDelphiObject(const Value: TScreen);
    procedure HandleOnActiveControlChange(Sender : TObject);
    procedure HandleOnActiveFormChange(Sender : TObject);
  protected
    // Exposed Methods
    {$IFNDEF FPC}
    function DisableAlign_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function EnableAlign_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function MonitorFromPoint_Wrapper(args : PPyObject) : PPyObject; cdecl;
    //TODO: implementation
    //function MonitorFromRect(const Rect: TRect; MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    //function MonitorFromWindow(const Handle: THandle; MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    {$IFNDEF FPC}
    function Realign_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ResetFonts_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    // Property Getters
    function Get_ActiveControl(AContext : Pointer) : PPyObject; cdecl;
    function Get_ActiveCustomForm(AContext : Pointer) : PPyObject; cdecl;
    function Get_ActiveForm(AContext : Pointer) : PPyObject; cdecl;
    function Get_CustomFormCount(AContext : Pointer) : PPyObject; cdecl;
    function Get_CustomForms(AContext : Pointer) : PPyObject; cdecl;
    function Get_Cursor(AContext : Pointer) : PPyObject; cdecl;
    function Get_Cursors(AContext : Pointer) : PPyObject; cdecl;
    function Get_DataModules(AContext : Pointer) : PPyObject; cdecl;
    function Get_DataModuleCount(AContext : Pointer) : PPyObject; cdecl;
    function Get_MonitorCount(AContext : Pointer) : PPyObject; cdecl;
    function Get_Monitors(AContext : Pointer) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_DesktopRect(AContext : Pointer) : PPyObject; cdecl;
    function Get_DesktopLeft(AContext : Pointer) : PPyObject; cdecl;
    function Get_DesktopTop(AContext : Pointer) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_DesktopWidth(AContext : Pointer) : PPyObject; cdecl;
    function Get_DesktopHeight(AContext : Pointer) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_WorkAreaRect(AContext : Pointer) : PPyObject; cdecl;
    function Get_WorkAreaHeight(AContext : Pointer) : PPyObject; cdecl;
    function Get_WorkAreaLeft(AContext : Pointer) : PPyObject; cdecl;
    function Get_WorkAreaTop(AContext : Pointer) : PPyObject; cdecl;
    function Get_WorkAreaWidth(AContext : Pointer) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_HintFont(AContext : Pointer) : PPyObject; cdecl;
    function Get_IconFont(AContext : Pointer) : PPyObject; cdecl;
    function Get_MenuFont(AContext : Pointer) : PPyObject; cdecl;
    function Get_Fonts(AContext : Pointer) : PPyObject; cdecl;
    function Get_FormCount(AContext : Pointer) : PPyObject; cdecl;
    function Get_Forms(AContext : Pointer) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_Imes(AContext : Pointer) : PPyObject; cdecl;
    function Get_DefaultIme(AContext : Pointer) : PPyObject; cdecl;
    function Get_DefaultKbLayout(AContext : Pointer) : PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_Height(AContext : Pointer) : PPyObject; cdecl;
    function Get_PixelsPerInch(AContext : Pointer) : PPyObject; cdecl;
    function Get_Width(AContext : Pointer) : PPyObject; cdecl;
    function Get_OnActiveControlChange(AContext : Pointer) : PPyObject; cdecl;
    function Get_OnActiveFormChange(AContext : Pointer) : PPyObject; cdecl;
    // Property Setters
    function Set_Cursor(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_HintFont(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_IconFont(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_MenuFont(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_OnActiveControlChange(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
    function Set_OnActiveFormChange(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    destructor Destroy; override;
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TScreen read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiApplication = class (TPyDelphiComponent)
  private
    function  GetDelphiObject: TApplication;
    procedure SetDelphiObject(const Value: TApplication);
  protected
    // Exposed Methods
    function ActivateHint_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function BringToFront_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function CancelHint_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ExecuteAction_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HandleException_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HandleMessage_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HelpCommand_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HelpContext_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function HelpJump_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function HelpKeyword_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function HideHint_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Initialize_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function IsRightToLeft_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function MessageBox_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Minimize_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ModalStarted_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ModalFinished_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function NormalizeAllTopMosts_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function NormalizeTopMosts_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function RestoreTopMosts_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    function ProcessMessages_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Restore_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Run_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ShowException_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function Terminate_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function UpdateAction_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$IFNDEF FPC}
    function UseRightToLeftAlignment_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function UseRightToLeftReading_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function UseRightToLeftScrollBar_Wrapper(args : PPyObject) : PPyObject; cdecl;
    {$ENDIF FPC}
    // property getters
    function Get_Active(AContext : Pointer): PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_AllowTesting(AContext : Pointer): PPyObject; cdecl;
    function Get_AutoDragDocking(AContext : Pointer): PPyObject; cdecl;
    function Get_CurrentHelpFile(AContext : Pointer): PPyObject; cdecl;
    function Get_DialogHandle(AContext : Pointer): PPyObject; cdecl;
    function Get_Handle(AContext : Pointer): PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_ExeName(AContext : Pointer): PPyObject; cdecl;
    function Get_HelpFile(AContext : Pointer): PPyObject; cdecl;
    function Get_Hint(AContext : Pointer): PPyObject; cdecl;
    function Get_HintColor(AContext : Pointer): PPyObject; cdecl;
    function Get_HintHidePause(AContext : Pointer): PPyObject; cdecl;
    function Get_HintPause(AContext : Pointer): PPyObject; cdecl;
    function Get_HintShortCuts(AContext : Pointer): PPyObject; cdecl;
    function Get_HintShortPause(AContext : Pointer): PPyObject; cdecl;
    function Get_Icon(AContext : Pointer): PPyObject; cdecl;
    function Get_MainForm(AContext : Pointer): PPyObject; cdecl;
    function Get_BiDiMode(AContext : Pointer): PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_BiDiKeyboard(AContext : Pointer): PPyObject; cdecl;
    function Get_NonBiDiKeyboard(AContext : Pointer): PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_ShowHint(AContext : Pointer): PPyObject; cdecl;
    function Get_ShowMainForm(AContext : Pointer): PPyObject; cdecl;
    function Get_Terminated(AContext : Pointer): PPyObject; cdecl;
    function Get_Title(AContext : Pointer): PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_UpdateFormatSettings(AContext : Pointer): PPyObject; cdecl;
    function Get_UpdateMetricSettings(AContext : Pointer): PPyObject; cdecl;
    {$ENDIF FPC}
    // property setters
    {$IFNDEF FPC}
    function Set_AllowTesting(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_AutoDragDocking(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_DialogHandle(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_Handle(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    {$ENDIF FPC}
    function Set_HelpFile(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_Hint(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_HintColor(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_HintHidePause(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_HintPause(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_HintShortCuts(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_HintShortPause(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_Icon(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_BiDiMode(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    {$IFNDEF FPC}
    function Set_BiDiKeyboard(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_NonBiDiKeyboard(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    {$ENDIF FPC}
    function Set_ShowHint(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_ShowMainForm(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_Title(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    {$IFNDEF FPC}
    function Set_UpdateFormatSettings(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_UpdateMetricSettings(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    {$ENDIF FPC}
  public
    // Class methods
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TApplication read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphiTypes;

{ Global Functions }
function FreeConsole_Wrapper(pself, args: PPyObject): PPyObject; cdecl;
begin
  FreeConsole;
  Result := GetPythonEngine.ReturnNone;
end;

{ Register the wrappers, the globals and the constants }
type
  TFormsRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TFormsRegistration }

procedure TFormsRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterFunction(PAnsiChar('FreeConsole'), FreeConsole_Wrapper,
       PAnsiChar('FreeConsole_Wrapper()'#10 +
       'Frees the MS-DOS console associated with the process.'));
end;

procedure TFormsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  // Singletons
  APyDelphiWrapper.DefineVar('Application', Application);
  APyDelphiWrapper.DefineVar('Screen',      Screen);

  // MessageBox flags
  APyDelphiWrapper.DefineVar('MB_ABORTRETRYIGNORE', MB_ABORTRETRYIGNORE);
  APyDelphiWrapper.DefineVar('MB_OK',               MB_OK);
  APyDelphiWrapper.DefineVar('MB_OKCANCEL',         MB_OKCANCEL);
  APyDelphiWrapper.DefineVar('MB_RETRYCANCEL',      MB_RETRYCANCEL);
  APyDelphiWrapper.DefineVar('MB_YESNO',            MB_YESNO);
  APyDelphiWrapper.DefineVar('MB_YESNOCANCEL',      MB_YESNOCANCEL);

  // MessageBox results
  APyDelphiWrapper.DefineVar('IDOK',      IDOK);
  APyDelphiWrapper.DefineVar('IDCANCEL',  IDCANCEL);
  APyDelphiWrapper.DefineVar('IDABORT',   IDABORT);
  APyDelphiWrapper.DefineVar('IDRETRY',   IDRETRY);
  APyDelphiWrapper.DefineVar('IDIGNORE',  IDIGNORE);
  APyDelphiWrapper.DefineVar('IDYES',     IDYES);
  APyDelphiWrapper.DefineVar('IDNO',      IDNO);

  // TCloseAction enum
  APyDelphiWrapper.DefineVar('caNone',      caNone);
  APyDelphiWrapper.DefineVar('caHide',      caHide);
  APyDelphiWrapper.DefineVar('caFree',      caFree);
  APyDelphiWrapper.DefineVar('caMinimize',  caMinimize);

  // TMonitorDefaultTo enum
  APyDelphiWrapper.DefineVar('mdNearest',   mdNearest);
  APyDelphiWrapper.DefineVar('mdNull',      mdNull);
  APyDelphiWrapper.DefineVar('mdPrimary',   mdPrimary);
end;

function TFormsRegistration.Name: String;
begin
  Result := 'Forms';
end;

procedure TFormsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiApplication);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScreen);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMonitor);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TCloseQueryEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TCloseEventHandler);
end;

{ TPyDelphiCustomForm }

function TPyDelphiCustomForm.Close_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if GetPythonEngine.PyArg_ParseTuple( args, ':Close') <> 0 then begin
      DelphiObject.Close;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiCustomForm.CloseQuery_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':CloseQuery') <> 0 then begin
      Result := VariantAsPyObject(DelphiObject.CloseQuery)
    end else
      Result := nil;
  end;
end;

function TPyDelphiCustomForm.Release_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple( args, ':Release') <> 0 then
    begin
      if Owned then begin
        (DelphiObject as TForm).Release;
        DelphiObject := nil;
        Owned := False;
        Result := ReturnNone;
      end else begin
        PyErr_SetString (PyExc_AttributeError^,
          PAnsiChar('The Delphi form cannot be released, since it is not Owned'));
        Result := nil;
      end;
    end
    else
      Result := nil;
  end;
end;

function TPyDelphiCustomForm.ShowModal_Wrapper(args: PPyObject): PPyObject;
Var
  ModalResult : integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ShowModal') <> 0 then begin
      ModalResult := (DelphiObject as TForm).ShowModal;
      Result := PyInt_FromLong(ModalResult);
    end else
      Result := nil;
  end;
end;

function TPyDelphiCustomForm.GetDelphiObject: TCustomForm;
begin
  Result := TCustomForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomForm.SetDelphiObject(const Value: TCustomForm);
begin
  inherited DelphiObject := Value;
end;

class procedure TPyDelphiCustomForm.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('ModalResult', @TPyDelphiCustomForm.Get_ModalResult, @TPyDelphiCustomForm.Set_ModalResult,
        'Represents the return value of a form that is used as a modal dialog.', nil);
    end;
end;

class procedure TPyDelphiCustomForm.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Close', @TPyDelphiCustomForm.Close_Wrapper,
    'TForm.Close()'#10 +
    'Closes the wrapped Form');
  PythonType.AddMethod('CloseQuery', @TPyDelphiCustomForm.CloseQuery_Wrapper,
    'TForm.CloseQuery()'#10 +
    'Asked the wrapped Form if it can close');
  PythonType.AddMethod('ShowModal', @TPyDelphiCustomForm.ShowModal_Wrapper,
    'TForm.ShowModal()'#10 +
    'Shows the wrapped Form as a modal form');
  PythonType.AddMethod('Release', @TPyDelphiCustomForm.Release_Wrapper,
    'TForm.Release()'#10 +
    'Releases (destroys) the wrapped Form');
end;

class function TPyDelphiCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCustomForm;
end;

function TPyDelphiCustomForm.Get_ModalResult(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.ModalResult);
end;

function TPyDelphiCustomForm.Set_ModalResult(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _modalResult : Integer;
begin
  if CheckIntAttribute(AValue, 'ModalResult', _modalResult) then
    with GetPythonEngine do begin
      Adjust(@Self);
      DelphiObject.ModalResult := _modalResult;
      Result := 0;
    end
    else
      Result := -1;
end;

function TPyDelphiCustomForm.CreateComponent(AOwner: TComponent): TComponent;
var
  _class : TClass;
  _compClass : TComponentClass;
  _className : String;
begin
  // get the default form class
  _compClass := TComponentClass(DelphiObjectClass);
  // if we have a subclass of our Form wrapper, then check if we can find
  // a Delphi class that would have the same name as the Python class.
  // This would allow Python to instanciate an existing Delphi form class,
  // instead of only using a blank form.
  if ob_type <> PythonType.TheTypePtr then
  begin
    _className := string(ob_type.tp_name);
    _class := GetClass(_className);
    if not Assigned(_class) then
      _class := GetClass('T'+_className);
    if Assigned(_class) and _class.InheritsFrom(TCustomForm) then
      _compClass := TComponentClass(_class);
  end;
  // if the Owner is Application, then we have to call its CreateForm method,
  // otherwise we won't have a main form defined, as the main form is the
  // first created form. Of course, this is a concern only when Python
  // controls all the GUI and calls Application.Run by itself.
  if AOwner = Application then
    Application.CreateForm(_compClass, Result)
  else
    Result := _compClass.Create(AOwner);
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

{ TPyDelphiScreen }

class function TPyDelphiScreen.DelphiObjectClass: TClass;
begin
  Result := TScreen;
end;

destructor TPyDelphiScreen.Destroy;
begin
  if PythonOK then
  begin
    GetPythonEngine.Py_XDECREF(fOnActiveControlChange);
    GetPythonEngine.Py_XDECREF(fOnActiveFormChange);
  end;
  inherited;
end;

{$IFNDEF FPC}
function TPyDelphiScreen.DisableAlign_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':DisableAlign') <> 0 then begin
      DelphiObject.DisableAlign;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiScreen.EnableAlign_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':EnableAlign') <> 0 then begin
      DelphiObject.EnableAlign;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiScreen.GetDelphiObject: TScreen;
begin
  Result := TScreen(inherited DelphiObject);
end;

function TPyDelphiScreen.Get_ActiveControl(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.ActiveControl);
end;

function TPyDelphiScreen.Get_ActiveCustomForm(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.ActiveCustomForm);
end;

function TPyDelphiScreen.Get_ActiveForm(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.ActiveForm);
end;

function TPyDelphiScreen.Get_Cursor(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Cursor);
end;

function TPyDelphiScreen.Get_Cursors(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TScreenCursorsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiScreen.Get_CustomFormCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.CustomFormCount);
end;

function TPyDelphiScreen.Get_CustomForms(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TScreenCustomFormsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiScreen.Get_DataModuleCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DataModuleCount);
end;

function TPyDelphiScreen.Get_DataModules(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TScreenDataModulesAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

{$IFNDEF FPC}
function TPyDelphiScreen.Get_DefaultIme(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.DefaultIme);
end;

function TPyDelphiScreen.Get_DefaultKbLayout(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DefaultKbLayout);
end;
{$ENDIF FPC}

function TPyDelphiScreen.Get_DesktopHeight(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DesktopHeight);
end;

{$IFNDEF FPC}
function TPyDelphiScreen.Get_DesktopLeft(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DesktopLeft);
end;

function TPyDelphiScreen.Get_DesktopRect(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRect(PyDelphiWrapper, DelphiObject.DesktopRect);
end;

function TPyDelphiScreen.Get_DesktopTop(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DesktopTop);
end;
{$ENDIF FPC}

function TPyDelphiScreen.Get_DesktopWidth(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DesktopWidth);
end;

function TPyDelphiScreen.Get_Fonts(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Fonts);
end;

function TPyDelphiScreen.Get_FormCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.FormCount);
end;

function TPyDelphiScreen.Get_Forms(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TScreenFormsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiScreen.Get_Height(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Height);
end;

function TPyDelphiScreen.Get_HintFont(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.HintFont);
end;

function TPyDelphiScreen.Get_IconFont(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.IconFont);
end;

{$IFNDEF FPC}
function TPyDelphiScreen.Get_Imes(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Imes);
end;
{$ENDIF FPC}

function TPyDelphiScreen.Get_MenuFont(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.MenuFont);
end;

function TPyDelphiScreen.Get_MonitorCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.MonitorCount);
end;

function TPyDelphiScreen.Get_Monitors(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TScreenMonitorsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiScreen.Get_OnActiveControlChange(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if Assigned(fOnActiveControlChange) then
    begin
      Result := fOnActiveControlChange;
      Py_XINCREF(Result);
    end
    else
      Result := ReturnNone;
  end;
end;

function TPyDelphiScreen.Get_OnActiveFormChange(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if Assigned(fOnActiveFormChange) then
    begin
      Result := fOnActiveFormChange;
      Py_XINCREF(Result);
    end
    else
      Result := ReturnNone;
  end;
end;

function TPyDelphiScreen.Get_PixelsPerInch(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.PixelsPerInch);
end;

function TPyDelphiScreen.Get_Width(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Width);
end;

{$IFNDEF FPC}
function TPyDelphiScreen.Get_WorkAreaHeight(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.WorkAreaHeight);
end;

function TPyDelphiScreen.Get_WorkAreaLeft(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.WorkAreaLeft);
end;

function TPyDelphiScreen.Get_WorkAreaRect(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRect(PyDelphiWrapper, DelphiObject.WorkAreaRect);
end;

function TPyDelphiScreen.Get_WorkAreaTop(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.WorkAreaTop);
end;

function TPyDelphiScreen.Get_WorkAreaWidth(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.WorkAreaWidth);
end;
{$ENDIF FPC}

procedure TPyDelphiScreen.HandleOnActiveControlChange(Sender: TObject);
begin
  RaiseNotifyEvent(PyDelphiWrapper, fOnActiveControlChange, Sender);
end;

procedure TPyDelphiScreen.HandleOnActiveFormChange(Sender: TObject);
begin
  RaiseNotifyEvent(PyDelphiWrapper, fOnActiveFormChange, Sender);
end;

function TPyDelphiScreen.MonitorFromPoint_Wrapper(args: PPyObject): PPyObject;
var
  p : TPoint;
  _pointObj : PPyObject;
  _monitorDefault : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'Oi:MonitorFromPoint',@_pointObj, @_monitorDefault ) <> 0 then
  begin
    if CheckPointAttribute(_pointObj, 'First parameter', p) and
       CheckEnum('TMonitorDefaultTo', _monitorDefault, Ord(Low(TMonitorDefaultTo)), Ord(High(TMonitorDefaultTo))) then
    begin
      Result := Wrap(DelphiObject.MonitorFromPoint(p, TMonitorDefaultTo(_monitorDefault)));
    end
    else
      Result := nil;
  end
  else
    Result := nil;
end;

{$IFNDEF FPC}
function TPyDelphiScreen.Realign_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Realign') <> 0 then begin
      DelphiObject.Realign;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiScreen.ResetFonts_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ResetFonts') <> 0 then begin
      DelphiObject.ResetFonts;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

class procedure TPyDelphiScreen.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('ActiveControl', @TPyDelphiScreen.Get_ActiveControl, nil,
        'Indicates which control currently has input focus on the screen.', nil);
      AddGetSet('ActiveCustomForm', @TPyDelphiScreen.Get_ActiveCustomForm, nil,
        'Indicates the descendant of TCustomForm that currently has focus.', nil);
      AddGetSet('ActiveForm', @TPyDelphiScreen.Get_ActiveForm, nil,
        'Indicates which form currently has focus.', nil);
      AddGetSet('CustomFormCount', @TPyDelphiScreen.Get_CustomFormCount, nil,
        'Indicates the number of forms or property pages displayed on the screen.', nil);
      AddGetSet('CustomForms', @TPyDelphiScreen.Get_CustomForms, nil,
        'Lists all the forms and property pages that are currently displayed in the application.', nil);
      AddGetSet('Cursor', @TPyDelphiScreen.Get_Cursor, @TPyDelphiScreen.Set_Cursor,
        'Controls the mouse cursor image on a global level.', nil);
      AddGetSet('Cursors', @TPyDelphiScreen.Get_Cursors, nil,
        'Lists the cursors available to the application.', nil);
      AddGetSet('DataModules', @TPyDelphiScreen.Get_DataModules, nil,
        'Lists all the data modules that are currently instantiated in the application.', nil);
      AddGetSet('DataModuleCount', @TPyDelphiScreen.Get_DataModuleCount, nil,
        'Indicates the number of data modules that are currently instantiated in the application.', nil);
      AddGetSet('MonitorCount', @TPyDelphiScreen.Get_MonitorCount, nil,
        'Specifies the number of monitors used to comprise the desktop.', nil);
      AddGetSet('Monitors', @TPyDelphiScreen.Get_Monitors, nil,
        'Provides access to an individual monitor used to comprise the desktop.', nil);
      {$IFNDEF FPC}
      AddGetSet('DesktopRect', @TPyDelphiScreen.Get_DesktopRect, nil,
        'Specifies the boundaries of the virtual desktop relative to the upper-left corner of the primary monitor.', nil);
      AddGetSet('DesktopLeft', @TPyDelphiScreen.Get_DesktopLeft, nil,
        'Specifies the x-coordinate of the desktop’s left edge relative to the upper-left corner of the primary monitor.', nil);
      AddGetSet('DesktopTop', @TPyDelphiScreen.Get_DesktopTop, nil,
        'Specifies the y-coordinate of the entire desktop’s top edge relative to the upper-left corner of the primary monitor.', nil);
      {$ENDIF FPC}
      AddGetSet('DesktopWidth', @TPyDelphiScreen.Get_DesktopWidth, nil,
        'Specifies the width of the entire virtual desktop.', nil);
      AddGetSet('DesktopHeight', @TPyDelphiScreen.Get_DesktopHeight, nil,
        'Specifies the height of the entire virtual desktop.', nil);
      {$IFNDEF FPC}
      AddGetSet('WorkAreaRect', @TPyDelphiScreen.Get_WorkAreaRect, nil,
        'Specifies the boundaries of the usable work area on the primary monitor.', nil);
      AddGetSet('WorkAreaHeight', @TPyDelphiScreen.Get_WorkAreaHeight, nil,
        'Specifies the height of the work area on the primary monitor.', nil);
      AddGetSet('WorkAreaLeft', @TPyDelphiScreen.Get_WorkAreaLeft, nil,
        'Specifies the left edge of the work area on the primary monitor.', nil);
      AddGetSet('WorkAreaTop', @TPyDelphiScreen.Get_WorkAreaTop, nil,
        'Specifies the top edge of the work area on the primary monitor.', nil);
      AddGetSet('WorkAreaWidth', @TPyDelphiScreen.Get_WorkAreaWidth, nil,
        'Specifies the width of the work area on the primary monitor.', nil);
      {$ENDIF FPC}
      AddGetSet('HintFont', @TPyDelphiScreen.Get_HintFont, @TPyDelphiScreen.Set_HintFont,
        'Specifies the font used to display help hints.', nil);
      AddGetSet('IconFont', @TPyDelphiScreen.Get_IconFont, @TPyDelphiScreen.Set_IconFont,
        'Specifies the font used to display the labels on the icons in file selection dialogs.', nil);
      AddGetSet('MenuFont', @TPyDelphiScreen.Get_MenuFont, @TPyDelphiScreen.Set_MenuFont,
        'Specifies the font used for menu commands.', nil);
      AddGetSet('Fonts', @TPyDelphiScreen.Get_Fonts, nil,
        'Lists the face names for all fonts supported by the screen.', nil);
      AddGetSet('FormCount', @TPyDelphiScreen.Get_FormCount, nil,
        'Indicates the number of forms or property pages displayed on the screen.', nil);
      AddGetSet('Forms', @TPyDelphiScreen.Get_Forms, nil,
        'Lists all the forms currently displayed in the application.', nil);
      {$IFNDEF FPC}
      AddGetSet('Imes', @TPyDelphiScreen.Get_Imes, nil,
        'Lists all input method editors (IMEs) installed on the system.', nil);
      AddGetSet('DefaultIme', @TPyDelphiScreen.Get_DefaultIme, nil,
        'Indicates which input method editor (IME) is active when the TScreen object is created.', nil);
      AddGetSet('DefaultKbLayout', @TPyDelphiScreen.Get_DefaultKbLayout, nil,
        'Indicates the Windows handle to the keyboard layout that was active when the application started running.', nil);
      {$ENDIF FPC}
      AddGetSet('Height', @TPyDelphiScreen.Get_Height, nil,
        'Indicates the vertical size of the screen in pixels.', nil);
      AddGetSet('PixelsPerInch', @TPyDelphiScreen.Get_PixelsPerInch, nil,
        'Indicates the number of screen pixels that make up a logical inch in the vertical direction.', nil);
      AddGetSet('Width', @TPyDelphiScreen.Get_Width, nil,
        'Indicates the horizontal size of the screen in pixels.', nil);
      AddGetSet('OnActiveControlChange', @TPyDelphiScreen.Get_OnActiveControlChange, @TPyDelphiScreen.Set_OnActiveControlChange,
        'Occurs immediately after input focus changes to a new windowed control.', nil);
      AddGetSet('OnActiveFormChange', @TPyDelphiScreen.Get_OnActiveFormChange, @TPyDelphiScreen.Set_OnActiveFormChange,
        'Occurs immediately after a new form becomes active in a multi-form application.', nil);
    end;
end;

class procedure TPyDelphiScreen.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  {$IFNDEF FPC}
  PythonType.AddMethod('DisableAlign', @TPyDelphiScreen.DisableAlign_Wrapper,
    'TScreen.DisableAlign()'#10 +
    'Prevents forms from being aligned in the screen.');
  PythonType.AddMethod('EnableAlign', @TPyDelphiScreen.EnableAlign_Wrapper,
    'TScreen.EnableAlign()'#10 +
    'Allows forms to be aligned in the screen.');
  PythonType.AddMethod('Realign', @TPyDelphiScreen.Realign_Wrapper,
    'TScreen.Realign()'#10 +
    'Realigns the screen’s forms according to their Align properties.');
  PythonType.AddMethod('ResetFonts', @TPyDelphiScreen.ResetFonts_Wrapper,
    'TScreen.ResetFonts()'#10 +
    'Reinitializes the fonts listed in the Fonts property.');
  {$ENDIF FPC}
  PythonType.AddMethod('MonitorFromPoint', @TPyDelphiScreen.MonitorFromPoint_Wrapper,
    'TScreen.MonitorFromPoint(APoint, AMonitorDefault)'#10 +
    'Returns the monitor where a specified point is located.');
end;

procedure TPyDelphiScreen.SetDelphiObject(const Value: TScreen);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiScreen.Set_Cursor(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _cursor : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Cursor', _cursor) then
  begin
    DelphiObject.Cursor := _cursor;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiScreen.Set_HintFont(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _font : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'HintFont', TFont, _font) then
  begin
    DelphiObject.HintFont := TFont(_font);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiScreen.Set_IconFont(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _font : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'IconFont', TFont, _font) then
  begin
    DelphiObject.IconFont := TFont(_font);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiScreen.Set_MenuFont(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _font : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'MenuFont', TFont, _font) then
  begin
    DelphiObject.MenuFont := TFont(_font);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiScreen.Set_OnActiveControlChange(AValue: PPyObject;
  AContext: Pointer): Integer;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if CheckCallableAttribute(AValue, 'OnActiveControlChange') then
    begin
      Py_XDECREF(fOnActiveControlChange);
      if AValue = Py_None then
      begin
        DelphiObject.OnActiveControlChange := nil;
        fOnActiveControlChange := nil;
      end
      else
      begin
        fOnActiveControlChange := AValue;
        Py_XINCREF(fOnActiveControlChange);
        DelphiObject.OnActiveControlChange := HandleOnActiveControlChange;
      end;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

function TPyDelphiScreen.Set_OnActiveFormChange(AValue: PPyObject;
  AContext: Pointer): Integer;
begin
  Adjust(@Self);
  with GetPythonEngine do begin
    if CheckCallableAttribute(AValue, 'OnActiveFormChange') then
    begin
      Py_XDECREF(fOnActiveFormChange);
      if AValue = Py_None then
      begin
        DelphiObject.OnActiveFormChange := nil;
        fOnActiveFormChange := nil;
      end
      else
      begin
        fOnActiveFormChange := AValue;
        Py_XINCREF(fOnActiveFormChange);
        DelphiObject.OnActiveFormChange := HandleOnActiveFormChange;
      end;
      Result := 0;
    end
    else
      Result := -1;
  end;
end;

{ TBaseScreenAccess }

class function TBaseScreenAccess.ExpectedContainerClass: TClass;
begin
  Result := TScreen;
end;

function TBaseScreenAccess.GetContainer: TScreen;
begin
  Result := TScreen(inherited Container);
end;

{ TScreenCustomFormsAccess }

function TScreenCustomFormsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.CustomForms[AIndex]);
end;

function TScreenCustomFormsAccess.GetSize: Integer;
begin
  Result := Container.CustomFormCount;
end;

class function TScreenCustomFormsAccess.Name: String;
begin
  Result := 'TScreen.CustomForms';
end;

{ TScreenFormsAccess }

function TScreenFormsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.Forms[AIndex]);
end;

function TScreenFormsAccess.GetSize: Integer;
begin
  Result := Container.FormCount;
end;

class function TScreenFormsAccess.Name: String;
begin
  Result := 'TScreen.Forms';
end;

{ TScreenDataModulesAccess }

function TScreenDataModulesAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.DataModules[AIndex]);
end;

function TScreenDataModulesAccess.GetSize: Integer;
begin
  Result := Container.DataModuleCount;
end;

class function TScreenDataModulesAccess.Name: String;
begin
  Result := 'TScreen.DataModules';
end;

{ TScreenCursorsAccess }

function TScreenCursorsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := GetPythonEngine.PyInt_FromLong( Container.Cursors[AIndex] );
end;

function TScreenCursorsAccess.GetSize: Integer;
begin
  Result := 32767;
end;

class function TScreenCursorsAccess.Name: String;
begin
  Result := 'TScreen.Cursors';
end;

{ TScreenMonitorsAccess }

function TScreenMonitorsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap(Container.Monitors[AIndex]);
end;

function TScreenMonitorsAccess.GetSize: Integer;
begin
  Result := Container.MonitorCount;
end;

class function TScreenMonitorsAccess.Name: String;
begin
  Result := 'TScreen.Monitors';
end;

{ TPyDelphiMonitor }

function TPyDelphiMonitor.Get_BoundsRect(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRect(PyDelphiWrapper, DelphiObject.BoundsRect);
end;

function TPyDelphiMonitor.Get_Handle(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Handle);
end;

function TPyDelphiMonitor.Get_Height(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Height);
end;

function TPyDelphiMonitor.Get_Left(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Left);
end;

function TPyDelphiMonitor.Get_MonitorNum(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.MonitorNum);
end;

function TPyDelphiMonitor.Get_Primary(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Primary);
end;

function TPyDelphiMonitor.Get_Top(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Top);
end;

function TPyDelphiMonitor.Get_Width(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Width);
end;

function TPyDelphiMonitor.Get_WorkareaRect(AContext : Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRect(PyDelphiWrapper, DelphiObject.WorkareaRect);
end;

function TPyDelphiMonitor.GetDelphiObject: TMonitor;
begin
  Result := TMonitor(inherited DelphiObject);
end;

class procedure TPyDelphiMonitor.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
  begin
    AddGetSet('Handle', @TPyDelphiMonitor.Get_Handle, nil,
              'Indicates the Windows handle for the monitor.', nil);
    AddGetSet('MonitorNum', @TPyDelphiMonitor.Get_MonitorNum, nil,
              'Specifies the index of the monitor in the global screen object’s Monitors list.', nil);
    AddGetSet('Left', @TPyDelphiMonitor.Get_Left, nil,
              'Indicates the logical position of the left edge of the monitor.', nil);
    AddGetSet('Height', @TPyDelphiMonitor.Get_Height, nil,
              'Indicates the vertical size of the monitor in pixels.', nil);
    AddGetSet('Top', @TPyDelphiMonitor.Get_Top, nil,
              'Indicates the logical position of the top edge of the monitor.', nil);
    AddGetSet('Width', @TPyDelphiMonitor.Get_Width, nil,
              'Indicates the horizontal size of the monitor in pixels.', nil);
    AddGetSet('BoundsRect', @TPyDelphiMonitor.Get_BoundsRect, nil,
              'Indicates the dimensions of the monitor in pixels.', nil);
    AddGetSet('WorkareaRect', @TPyDelphiMonitor.Get_WorkareaRect, nil,
              'Indicates the dimensions of the usable area on the monitor.', nil);
    AddGetSet('Primary', @TPyDelphiMonitor.Get_Primary, nil,
              'Indicates whether this is the primary monitor.', nil);
  end;
end;

procedure TPyDelphiMonitor.SetDelphiObject(const Value: TMonitor);
begin
  inherited DelphiObject := Value;
end;

class function TPyDelphiMonitor.DelphiObjectClass: TClass;
begin
  Result := TMonitor;
end;

{ TPyDelphiApplication }

function TPyDelphiApplication.ActivateHint_Wrapper(
  args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  _point : TPoint;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:ActivateHint',@_obj ) <> 0 then begin
      if CheckPointAttribute(_obj, 'The first argument of ActivateHint', _point) then
      begin
        DelphiObject.ActivateHint(_point);
        Result := ReturnNone;
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.BringToFront_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':BringToFront') <> 0 then begin
      DelphiObject.BringToFront;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.CancelHint_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':CancelHint') <> 0 then begin
      DelphiObject.CancelHint;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.ExecuteAction_Wrapper(
  args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  _value : TObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:ExecuteAction',@_obj ) <> 0 then begin
      if CheckObjAttribute(_obj, 'The first argument of ExecuteAction', TBasicAction, _value) then
      begin
        Result := VariantAsPyObject( DelphiObject.ExecuteAction(TBasicAction(_value)) );
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.GetDelphiObject: TApplication;
begin
  Result := TApplication(inherited DelphiObject);
end;

function TPyDelphiApplication.HandleException_Wrapper(
  args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  _sender : TObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:HandleException',@_obj ) <> 0 then begin
      if CheckObjAttribute(_obj, 'The first argument of HandleException', TObject, _sender) then
      begin
        DelphiObject.HandleException(_sender);
        Result := ReturnNone;
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.HandleMessage_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':HandleMessage') <> 0 then begin
      DelphiObject.HandleMessage;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.HelpCommand_Wrapper(
  args: PPyObject): PPyObject;
var
  _command : Integer;
  _data : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'i:HelpCommand',@_command, @_data ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.HelpCommand(_command, _data) );
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.HelpContext_Wrapper(
  args: PPyObject): PPyObject;
var
  _context : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'i:HelpContext',@_context ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.HelpContext(_context) );
    end else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.HelpJump_Wrapper(args: PPyObject): PPyObject;
var
  _jumpID : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 's:HelpJump',@_jumpID ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.HelpJump(String(_jumpID)) );
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiApplication.HelpKeyword_Wrapper(
  args: PPyObject): PPyObject;
var
  _keyword : PAnsiChar;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 's:HelpKeyword',@_keyword ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.HelpKeyword(String(_keyword)) );
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.HideHint_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':HideHint') <> 0 then begin
      DelphiObject.HideHint;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.Initialize_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Initialize') <> 0 then begin
      DelphiObject.Initialize;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.IsRightToLeft_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':IsRightToLeft') <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.IsRightToLeft );
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiApplication.MessageBox_Wrapper(
  args: PPyObject): PPyObject;
var
  _text : PAnsiChar;
  _caption : PAnsiChar;
  _flags : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'ssi:MessageBox',@_text, @_caption, @_flags ) <> 0 then begin
      Result := PyInt_FromLong(
        DelphiObject.MessageBox(PChar(string(_text)), PChar(string(_caption)), _flags) );
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.Minimize_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Minimize') <> 0 then begin
      DelphiObject.Minimize;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.ModalFinished_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ModalFinished') <> 0 then begin
      DelphiObject.ModalFinished;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.ModalStarted_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ModalStarted') <> 0 then begin
      DelphiObject.ModalStarted;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.NormalizeAllTopMosts_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':NormalizeAllTopMosts') <> 0 then begin
      DelphiObject.NormalizeAllTopMosts;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.NormalizeTopMosts_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':NormalizeTopMosts') <> 0 then begin
      DelphiObject.NormalizeTopMosts;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.RestoreTopMosts_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':RestoreTopMosts') <> 0 then begin
      DelphiObject.RestoreTopMosts;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiApplication.ProcessMessages_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':ProcessMessages') <> 0 then begin
      DelphiObject.ProcessMessages;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

class procedure TPyDelphiApplication.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
  begin
    AddGetSet('Active', @TPyDelphiApplication.Get_Active, nil,
      'Specifies whether the application is active and has focus.', nil);
    {$IFNDEF FPC}
    AddGetSet('AllowTesting', @TPyDelphiApplication.Get_AllowTesting, @TPyDelphiApplication.Set_AllowTesting,
      'Represents information for the IDE.', nil);
    AddGetSet('AutoDragDocking', @TPyDelphiApplication.Get_AutoDragDocking, @TPyDelphiApplication.Set_AutoDragDocking,
      'Indicates whether dragged windows are automatically docked.', nil);
    AddGetSet('CurrentHelpFile', @TPyDelphiApplication.Get_CurrentHelpFile, nil,
      'Indicates the current help file.', nil);
    {$ENDIF FPC}
    AddGetSet('ExeName', @TPyDelphiApplication.Get_ExeName, nil,
      'Contains the file name of the application’s executable file including path information.', nil);
    {$IFNDEF FPC}
    AddGetSet('DialogHandle', @TPyDelphiApplication.Get_DialogHandle, @TPyDelphiApplication.Set_DialogHandle,
      'Provides a mechanism for using non-VCL dialog boxes in an application.', nil);
    AddGetSet('Handle', @TPyDelphiApplication.Get_Handle, @TPyDelphiApplication.Set_Handle,
      'Provides access to the window handle of the main form (window) of the application.', nil);
    {$ENDIF FPC}
    AddGetSet('HelpFile', @TPyDelphiApplication.Get_HelpFile, @TPyDelphiApplication.Set_HelpFile,
      'Specifies the name of the default file the application uses to display Help.', nil);
    AddGetSet('Hint', @TPyDelphiApplication.Get_Hint, @TPyDelphiApplication.Set_Hint,
      'Specifies the text string that appears in the Help Hint box.', nil);
    AddGetSet('HintColor', @TPyDelphiApplication.Get_HintColor, @TPyDelphiApplication.Set_HintColor,
      'color of the hint boxes for the Help Hints for the application.', nil);
    AddGetSet('HintPause', @TPyDelphiApplication.Get_HintPause, @TPyDelphiApplication.Set_HintPause,
      'Specifies the time interval that passes before the control''s Help Hint appears when the user places the mouse pointer on a control or menu item.', nil);
    AddGetSet('HintHidePause', @TPyDelphiApplication.Get_HintHidePause, @TPyDelphiApplication.Set_HintHidePause,
      'Specifies the time interval to wait before hiding the Help Hint if the mouse has not moved from the control or menu item.', nil);
    AddGetSet('HintShortCuts', @TPyDelphiApplication.Get_HintShortCuts, @TPyDelphiApplication.Set_HintShortCuts,
      'Specifies whether the application’s pop-up hints include shortcut information.', nil);
    AddGetSet('HintShortPause', @TPyDelphiApplication.Get_HintShortPause, @TPyDelphiApplication.Set_HintShortPause,
      'Specifies the time period to wait before bringing up a hint if another hint has already been shown.', nil);
    AddGetSet('Icon', @TPyDelphiApplication.Get_Icon, @TPyDelphiApplication.Set_Icon,
      'Determines which icon represents the application on the Windows task bar.', nil);
    AddGetSet('MainForm', @TPyDelphiApplication.Get_MainForm, nil,
      'Identifies which form in the application is the main window.', nil);
    AddGetSet('BiDiMode', @TPyDelphiApplication.Get_BiDiMode, @TPyDelphiApplication.Set_BiDiMode,
      'Indicates the layout of the application when running under Middle Eastern versions of Windows.', nil);
    {$IFNDEF FPC}
    AddGetSet('BiDiKeyboard', @TPyDelphiApplication.Get_BiDiKeyboard, @TPyDelphiApplication.Set_BiDiKeyboard,
      'Specifies the name of the keyboard map that should be used in middle-east locales when text reads from right to left.', nil);
    AddGetSet('NonBiDiKeyboard', @TPyDelphiApplication.Get_NonBiDiKeyboard, @TPyDelphiApplication.Set_NonBiDiKeyboard,
      'Specifies the name of the keyboard map that should be used unless text reads from right to left.', nil);
    {$ENDIF FPC}
    AddGetSet('ShowHint', @TPyDelphiApplication.Get_ShowHint, @TPyDelphiApplication.Set_ShowHint,
      'Determines whether Help Hints are enabled or disabled for the entire application.', nil);
    AddGetSet('ShowMainForm', @TPyDelphiApplication.Get_ShowMainForm, @TPyDelphiApplication.Set_ShowMainForm,
      'Determines whether the application shows its main form on startup.', nil);
    AddGetSet('Terminated', @TPyDelphiApplication.Get_Terminated, nil,
      'Reports whether the application is in the process of shutting down.', nil);
    AddGetSet('Title', @TPyDelphiApplication.Get_Title, @TPyDelphiApplication.Set_Title,
      'Contains the text that appears below the icon representing the application when it is minimized.', nil);
    {$IFNDEF FPC}
    AddGetSet('UpdateFormatSettings', @TPyDelphiApplication.Get_UpdateFormatSettings, @TPyDelphiApplication.Set_UpdateFormatSettings,
      'Specifies whether format settings are updated automatically when the user alters the system configuration.', nil);
    AddGetSet('UpdateMetricSettings', @TPyDelphiApplication.Get_UpdateMetricSettings, @TPyDelphiApplication.Set_UpdateMetricSettings,
      'Specifies whether the settings used for the hint window font and the icon title font are updated.', nil);
    {$ENDIF FPC}
  end;
end;

class procedure TPyDelphiApplication.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('ActivateHint', @TPyDelphiApplication.ActivateHint_Wrapper,
    'TApplication.ActivateHint()'#10 +
    'Displays a hint window for the control at a specified position.');
  PythonType.AddMethod('BringToFront', @TPyDelphiApplication.BringToFront_Wrapper,
    'TApplication.BringToFront()'#10 +
    'Sets the last active window as the topmost window on the desktop above all other applications.');
  PythonType.AddMethod('CancelHint', @TPyDelphiApplication.CancelHint_Wrapper,
    'TApplication.CancelHint()'#10 +
    'Cancels the display of a hint for a control.');
  PythonType.AddMethod('ExecuteAction', @TPyDelphiApplication.ExecuteAction_Wrapper,
    'TApplication.ExecuteAction()'#10 +
    'Generates an OnActionExecute event.');
  PythonType.AddMethod('HandleException', @TPyDelphiApplication.HandleException_Wrapper,
    'TApplication.HandleException()'#10 +
    'Provides default handling of exceptions for the application.');
  PythonType.AddMethod('HandleMessage', @TPyDelphiApplication.HandleMessage_Wrapper,
    'TApplication.HandleMessage()'#10 +
    'Interrupts the execution of an application while Windows processes a message in the Windows message queue. ');
  PythonType.AddMethod('HelpCommand', @TPyDelphiApplication.HelpCommand_Wrapper,
    'TApplication.HelpCommand()'#10 +
    'Provides quick access to any of the Help commands in the WinHelp API (application programming interface).');
  PythonType.AddMethod('HelpContext', @TPyDelphiApplication.HelpContext_Wrapper,
    'TApplication.HelpContext()'#10 +
    'Displays a specified help topic.');
  {$IFNDEF FPC}
  PythonType.AddMethod('HelpJump', @TPyDelphiApplication.HelpJump_Wrapper,
    'TApplication.HelpJump()'#10 +
    'Displays a specified help topic.');
  {$ENDIF FPC}
  PythonType.AddMethod('HelpKeyword', @TPyDelphiApplication.HelpKeyword_Wrapper,
    'TApplication.HelpKeyword()'#10 +
    'Displays a specified help topic.');
  PythonType.AddMethod('HideHint', @TPyDelphiApplication.HideHint_Wrapper,
    'TApplication.HideHint()'#10 +
    'Hides the current hint.');
  PythonType.AddMethod('Initialize', @TPyDelphiApplication.Initialize_Wrapper,
    'TApplication.Initialize()'#10 +
    'Provides an opportunity to initialize subsystems.');
  {$IFNDEF FPC}
  PythonType.AddMethod('IsRightToLeft', @TPyDelphiApplication.IsRightToLeft_Wrapper,
    'TApplication.IsRightToLeft()'#10 +
    'Specifies whether the application adjusts its interface for Middle Eastern locales.');
  {$ENDIF FPC}
  PythonType.AddMethod('MessageBox', @TPyDelphiApplication.MessageBox_Wrapper,
    'TApplication.MessageBox()'#10 +
    'Displays a specified message to the user.');
  PythonType.AddMethod('Minimize', @TPyDelphiApplication.Minimize_Wrapper,
    'TApplication.Minimize()'#10 +
    'Shrinks an application to the Windows task bar.');
  PythonType.AddMethod('ModalStarted', @TPyDelphiApplication.ModalStarted_Wrapper,
    'TApplication.ModalStarted()'#10 +
    'Track opening of modal form.');
  PythonType.AddMethod('ModalFinished', @TPyDelphiApplication.ModalFinished_Wrapper,
    'TApplication.ModalFinished()'#10 +
    'Track closing of modal form.');
  {$IFNDEF FPC}
  PythonType.AddMethod('NormalizeAllTopMosts', @TPyDelphiApplication.NormalizeAllTopMosts_Wrapper,
    'TApplication.NormalizeAllTopMosts()'#10 +
    'Makes forms that have been designated as topmost forms (their FormStyle is fsStayOnTop) behave as if they were not topmost forms. ');
  PythonType.AddMethod('RestoreTopMosts', @TPyDelphiApplication.RestoreTopMosts_Wrapper,
    'TApplication.RestoreTopMosts()'#10 +
    'Restores forms designated as fsStayOnTop to be topmost again.');
  {$ENDIF FPC}
  PythonType.AddMethod('ProcessMessages', @TPyDelphiApplication.ProcessMessages_Wrapper,
    'TApplication.ProcessMessages()'#10 +
    'Interrupts the execution of an application so that it can process the message queue.');
  PythonType.AddMethod('Restore', @TPyDelphiApplication.Restore_Wrapper,
    'TApplication.Restore()'#10 +
    'Restores a minimized application to its normal size.');
  PythonType.AddMethod('Run', @TPyDelphiApplication.Run_Wrapper,
    'TApplication.Run()'#10 +
    'Executes the application.');
  PythonType.AddMethod('ShowException', @TPyDelphiApplication.ShowException_Wrapper,
    'TApplication.ShowException()'#10 +
    'Displays a message box for exceptions that are not caught by application code.');
  PythonType.AddMethod('Terminate', @TPyDelphiApplication.Terminate_Wrapper,
    'TApplication.Terminate()'#10 +
    'Ends application execution.');
  PythonType.AddMethod('UpdateAction', @TPyDelphiApplication.UpdateAction_Wrapper,
    'TApplication.UpdateAction()'#10 +
    'Generates an OnActionUpdate event.');
  {$IFNDEF FPC}
  PythonType.AddMethod('UseRightToLeftAlignment', @TPyDelphiApplication.UseRightToLeftAlignment_Wrapper,
    'TApplication.UseRightToLeftAlignment()'#10 +
    'Specifies whether the application object is in a right-to-left alignment mode.');
  PythonType.AddMethod('UseRightToLeftReading', @TPyDelphiApplication.UseRightToLeftReading_Wrapper,
    'TApplication.UseRightToLeftReading()'#10 +
    'Specifies whether the application object is in a right-to-left text mode.');
  PythonType.AddMethod('UseRightToLeftScrollBar', @TPyDelphiApplication.UseRightToLeftScrollBar_Wrapper,
    'TApplication.UseRightToLeftScrollBar()'#10 +
    'Specifies whether the vertical scroll bar appears on the left side controls.');
  {$ENDIF FPC}
end;

function TPyDelphiApplication.Restore_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Restore') <> 0 then begin
      DelphiObject.Restore;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.Run_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Run') <> 0 then
    begin
      DelphiObject.Run;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

procedure TPyDelphiApplication.SetDelphiObject(const Value: TApplication);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiApplication.ShowException_Wrapper(
  args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  _value : TObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:ShowException',@_obj ) <> 0 then begin
      if CheckObjAttribute(_obj, 'The first argument of ShowException', Exception, _value) then
      begin
        DelphiObject.ShowException(Exception(_value));
        Result := ReturnNone;
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.Terminate_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Terminate') <> 0 then begin
      DelphiObject.Terminate;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.UpdateAction_Wrapper(
  args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  _value : TObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'O:UpdateAction',@_obj ) <> 0 then begin
      if CheckObjAttribute(_obj, 'The first argument of UpdateAction', TBasicAction, _value) then
      begin
        DelphiObject.UpdateAction(TBasicAction(_value));
        Result := ReturnNone;
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.UseRightToLeftAlignment_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':UseRightToLeftAlignment') <> 0 then begin
      DelphiObject.UseRightToLeftAlignment;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.UseRightToLeftReading_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':UseRightToLeftReading') <> 0 then begin
      DelphiObject.UseRightToLeftReading;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;

function TPyDelphiApplication.UseRightToLeftScrollBar_Wrapper(
  args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':UseRightToLeftScrollBar') <> 0 then begin
      DelphiObject.UseRightToLeftScrollBar;
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiApplication.Get_Active(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Active);
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Get_AllowTesting(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.AllowTesting);
end;

function TPyDelphiApplication.Get_AutoDragDocking(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.AutoDragDocking);
end;

function TPyDelphiApplication.Get_BiDiKeyboard(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.BiDiKeyboard);
end;
{$ENDIF FPC}

function TPyDelphiApplication.Get_BiDiMode(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(Ord(DelphiObject.BiDiMode));
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Get_CurrentHelpFile(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.CurrentHelpFile);
end;

function TPyDelphiApplication.Get_DialogHandle(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.DialogHandle);
end;
{$ENDIF FPC}

function TPyDelphiApplication.Get_ExeName(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.ExeName);
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Get_Handle(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Handle);
end;
{$ENDIF FPC}

function TPyDelphiApplication.Get_HelpFile(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.HelpFile);
end;

function TPyDelphiApplication.Get_Hint(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.Hint);
end;

function TPyDelphiApplication.Get_HintColor(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.HintColor);
end;

function TPyDelphiApplication.Get_HintHidePause(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.HintHidePause);
end;

function TPyDelphiApplication.Get_HintPause(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.HintPause);
end;

function TPyDelphiApplication.Get_HintShortCuts(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.HintShortCuts);
end;

function TPyDelphiApplication.Get_HintShortPause(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.HintShortPause);
end;

function TPyDelphiApplication.Get_Icon(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Icon);
end;

function TPyDelphiApplication.Get_MainForm(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.MainForm);
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Get_NonBiDiKeyboard(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.NonBiDiKeyboard);
end;
{$ENDIF FPC}

function TPyDelphiApplication.Get_ShowHint(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.ShowHint);
end;

function TPyDelphiApplication.Get_ShowMainForm(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.ShowMainForm);
end;

function TPyDelphiApplication.Get_Terminated(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.Terminated);
end;

function TPyDelphiApplication.Get_Title(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyString_FromDelphiString(DelphiObject.Title);
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Get_UpdateFormatSettings(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.UpdateFormatSettings);
end;

function TPyDelphiApplication.Get_UpdateMetricSettings(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.UpdateMetricSettings);
end;

function TPyDelphiApplication.Set_AllowTesting(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'AllowTesting', _value) then
  begin
    DelphiObject.AllowTesting := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_AutoDragDocking(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'AutoDragDocking', _value) then
  begin
    DelphiObject.AutoDragDocking := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_BiDiKeyboard(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'BiDiKeyboard', _value) then
  begin
    DelphiObject.BiDiKeyboard := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

function TPyDelphiApplication.Set_BiDiMode(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'BiDiMode', _value) then
  begin
    DelphiObject.BiDiMode := TBiDiMode(_value);
    Result := 0;
  end
  else
    Result := -1;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Set_DialogHandle(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'DialogHandle', _value) then
  begin
    DelphiObject.DialogHandle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_Handle(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Handle', _value) then
  begin
    DelphiObject.Handle := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

function TPyDelphiApplication.Set_HelpFile(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'HelpFile', _value) then
  begin
    DelphiObject.HelpFile := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_Hint(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'Hint', _value) then
  begin
    DelphiObject.Hint := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_HintColor(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'HintColor', _value) then
  begin
    DelphiObject.HintColor := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_HintHidePause(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'HintHidePause', _value) then
  begin
    DelphiObject.HintHidePause := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_HintPause(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'HintPause', _value) then
  begin
    DelphiObject.HintPause := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_HintShortCuts(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'HintShortCuts', _value) then
  begin
    DelphiObject.HintShortCuts := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_HintShortPause(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'HintShortPause', _value) then
  begin
    DelphiObject.HintShortPause := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_Icon(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'Icon', TIcon, _value) then
  begin
    DelphiObject.Icon := TIcon(_value);
    Result := 0;
  end
  else
    Result := -1;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Set_NonBiDiKeyboard(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'NonBiDiKeyboard', _value) then
  begin
    DelphiObject.NonBiDiKeyboard := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

function TPyDelphiApplication.Set_ShowHint(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'ShowHint', _value) then
  begin
    DelphiObject.ShowHint := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_ShowMainForm(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'ShowMainForm', _value) then
  begin
    DelphiObject.ShowMainForm := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_Title(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : String;
begin
  Adjust(@Self);
  if CheckStrAttribute(AValue, 'Title', _value) then
  begin
    DelphiObject.Title := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

{$IFNDEF FPC}
function TPyDelphiApplication.Set_UpdateFormatSettings(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'UpdateFormatSettings', _value) then
  begin
    DelphiObject.UpdateFormatSettings := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiApplication.Set_UpdateMetricSettings(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'UpdateMetricSettings', _value) then
  begin
    DelphiObject.UpdateMetricSettings := _value;
    Result := 0;
  end
  else
    Result := -1;
end;
{$ENDIF FPC}

class function TPyDelphiApplication.DelphiObjectClass: TClass;
begin
  Result := TApplication;
end;

{ TCloseQueryEventHandler }

constructor TCloseQueryEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TCloseQueryEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TCloseQueryEventHandler.DoEvent(Sender: TObject;
  var CanClose: Boolean);
Var
  PyObject, PyTuple, PyResult, PyCanClose : PPyObject;
  _varParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyCanClose := CreateVarParam(PyDelphiWrapper, CanClose);
      _varParam := PythonToDelphi(PyCanClose) as TPyDelphiVarParameter;
      PyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyObject);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyCanClose);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        if Assigned(PyResult) then
        begin
          Py_DECREF(PyResult);
          CanClose := PyObject_IsTrue(_varParam.Value) = 1;
        end;
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TCloseQueryEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TCloseQueryEvent);
end;

{ TCloseActionEventHandler }

constructor TCloseEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TCloseEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TCloseEventHandler.DoEvent(Sender: TObject;
  var Action: TCloseAction);
Var
  PyObject, PyTuple, PyResult, PyAction : PPyObject;
  _varParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyAction := CreateVarParam(PyDelphiWrapper, NativeInt(Action));
      _varParam := PythonToDelphi(PyAction) as TPyDelphiVarParameter;
      PyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyObject);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyAction);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        if Assigned(PyResult) then
        begin
          Py_DECREF(PyResult);
          if PyInt_Check(_varParam.Value) and
             CheckEnum('TCloseAction', PyInt_AsLong(_varParam.Value), Ord(Low(TCloseAction)), Ord(High(TCloseAction))) then
            Action := TCloseAction(PyInt_AsLong(_varParam.Value));
        end;
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TCloseEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TCloseEvent);
end;

initialization
  RegisteredUnits.Add( TFormsRegistration.Create );
  Classes.RegisterClasses([TForm, TScreen, TApplication]);
end.
