(* ************************************************************************ *)
(*                                                                          *)
(* Module:  Unit 'MainForm'     Copyright (c) 2021                          *)
(*                                                                          *)
(* Lucas Moura Belo - lmbelo                                                *)
(* lucas.belo@live.com                                                      *)
(* Brazil                                                                   *)
(*                                                                          *)
(* PyScripter                                                               *)
(* e-mail: pyscripter@gmail.com                                             *)
(*                                                                          *)
(* Project pages:      https://github.com/Embarcadero/python4delphi         *)
(* https://github.com/pyscripter/python4delphi                              *)
(* ************************************************************************ *)
(* Functionality:  It creates a form at run-time and adds a                 *)
(* TCameraComponent on it. Also creates a                                   *)
(* Start/Stop camera for a previewer.                                       *)
(*                                                                          *)
(* ************************************************************************ *)
(* This source code is distributed with no WARRANTY, for no reason or use.  *)
(* Everyone is allowed to use and change this code free for his own tasks   *)
(* and projects, as long as this header and its copyright text is intact.   *)
(* For changed versions of this code, which are public distributed the      *)
(* following additional conditions have to be fullfilled:                   *)
(* 1) The header has to contain a comment on the change and the author of   *)
(* it.                                                                      *)
(* 2) A copy of the changed source has to be sent to the above E-Mail       *)
(* address or my then valid address, if this is possible to the             *)
(* author.                                                                  *)
(* The second condition has the target to maintain an up to date central    *)
(* version of the component. If this condition is not acceptable for        *)
(* confidential or legal reasons, everyone is free to derive a component    *)
(* or to generate a diff file to my or other original sources.              *)
(* ************************************************************************ *)
{$I Definition.inc}
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Ani, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PythonEngine, FMX.PythonGUIInputOutput, System.Actions, System.Permissions,
  FMX.ActnList, FMX.Objects, FMX.Layouts, FMX.Platform, FMX.Media,
  AppEnvironment,
  ProgressFrame, System.Threading, WrapDelphi, WrapDelphiFmx;

type
  TPyMainForm = class(TForm)
    spInputOutput: TSplitter;
    tbTop: TToolBar;
    tbBottom: TToolBar;
    btnRun: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    ActionList1: TActionList;
    actRun: TAction;
    Layout1: TLayout;
    mmOutput: TMemo;
    actClearOutput: TAction;
    Layout2: TLayout;
    btnClearOutput: TButton;
    Circle1: TCircle;
    Layout3: TLayout;
    mmInput: TMemo;
    Layout4: TLayout;
    Circle2: TCircle;
    btnClearInput: TButton;
    actClearInput: TAction;
    Label1: TLabel;
    PythonModule1: TPythonModule;
    PyDelphiWrapper1: TPyDelphiWrapper;
    PythonDelphiVar1: TPythonDelphiVar;
    PythonDelphiVar2: TPythonDelphiVar;
    procedure actRunExecute(Sender: TObject);
    procedure actClearOutputExecute(Sender: TObject);
    procedure actClearInputExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
    procedure PythonDelphiVar1ExtSetData(Sender: TObject; Data: PPyObject);
    procedure PythonDelphiVar1ExtGetData(Sender: TObject; var Data: PPyObject);
    procedure PythonDelphiVar2ExtSetData(Sender: TObject; Data: PPyObject);
    procedure PythonDelphiVar2ExtGetData(Sender: TObject; var Data: PPyObject);
  private
    FAppEnv: TAppEnvironment;
    FSavedCameraActive : boolean;
    FPermissionCamera: string;
    FPyCamera: PPyObject;
    FPyImage: PPyObject;
    procedure DisableComponents();
    procedure EnableComponents();
    //App launch
    procedure AppEventHandler(const Sender: TObject; const AMessage: TMessage);
    //Python environment
    procedure ConfigurePython();
    //Device handlers
    procedure ActivateCameraPermissionRequestResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
    procedure DisplayRationale(Sender: TObject;
      const APermissions: TArray<string>; const APostRationaleProc: TProc);
    //Interop variables
    function GetCameraComponent: TCameraComponent;
    function GetImage: TImage;
    //Interop methods
    function AskDelphiForCameraPermission(PSelf, AArgs: PPyObject)
      : PPyObject; cdecl;
    function AskDelphiToShowFrame(PSelf, AArgs: PPyObject): PPyObject; cdecl;
  protected
    property Camera: TCameraComponent read GetCameraComponent;
    property Image: TImage read GetImage;
  public
    { Public declarations }
  end;

var
  PyMainForm: TPyMainForm;

implementation

uses
  PythonLoad,
  System.IOUtils,
  FMX.DialogService,
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  WrapFMXMedia,
  WrapFmxShapes;

{$R *.fmx}

{ TPyMainForm }

procedure TPyMainForm.FormCreate(Sender: TObject);
begin
  DisableComponents();
  FAppEnv := TAppEnvironment.Create(TProgressViewFrame.Create(Self, Self));
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, AppEventHandler);
  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.Camera);
end;

procedure TPyMainForm.FormDestroy(Sender: TObject);
begin
  FAppEnv.Free();
end;

function TPyMainForm.GetCameraComponent: TCameraComponent;
begin
  if IsDelphiObject(FPyCamera) then begin
    Result := TPyDelphiCameraComponent(PythonToDelphi(FPyCamera)).DelphiObject
  end else
    Result := nil;
end;

function TPyMainForm.GetImage: TImage;
begin
  if IsDelphiObject(FPyImage) then
    Result := TPyDelphiImage(PythonToDelphi(FPyImage)).DelphiObject
  else
    Result := nil;
end;

procedure TPyMainForm.PythonDelphiVar1ExtGetData(Sender: TObject;
  var Data: PPyObject);
begin
  Data := FPyCamera;
  PythonEngine1.Py_XINCREF(FPyCamera);
end;

procedure TPyMainForm.PythonDelphiVar1ExtSetData(Sender: TObject;
  Data: PPyObject);
begin
  PythonEngine1.Py_XDECREF(FPyCamera);
  FPyCamera := Data;
  PythonEngine1.Py_XINCREF(FPyCamera);
end;

procedure TPyMainForm.PythonDelphiVar2ExtGetData(Sender: TObject;
  var Data: PPyObject);
begin
  Data := FPyImage;
  PythonEngine1.Py_XINCREF(FPyImage);
end;

procedure TPyMainForm.PythonDelphiVar2ExtSetData(Sender: TObject;
  Data: PPyObject);
begin
  PythonEngine1.Py_XDECREF(FPyImage);
  FPyImage := Data;
  PythonEngine1.Py_XINCREF(FPyImage);
end;

procedure TPyMainForm.PythonModule1Initialization(Sender: TObject);
begin
  PythonModule1.AddDelphiMethod(
    'AskDelphiForCameraPermission',
    AskDelphiForCameraPermission,
    'Check if camera permission has been granted.');

  PythonModule1.AddDelphiMethod(
    'AskDelphiToShowFrame',
    AskDelphiToShowFrame,
    'Displays sample buffer.');
end;

procedure TPyMainForm.actClearInputExecute(Sender: TObject);
begin
  mmInput.Lines.Clear();
end;

procedure TPyMainForm.actClearOutputExecute(Sender: TObject);
begin
  mmOutput.Lines.Clear();
end;

procedure TPyMainForm.ActivateCameraPermissionRequestResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
begin
  if (Length(AGrantResults) = 1) and
    (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    if Assigned(Camera) then
      Camera.Active := True;
    FSavedCameraActive := True;
  end
  else
    TDialogService.ShowMessage
      ('Cannot start the camera because the required permission has not been granted');
end;

procedure TPyMainForm.actRunExecute(Sender: TObject);
begin
  var
  LMainForm := PyDelphiWrapper1.Wrap(Self);
  PythonModule1.SetVar('MainForm', LMainForm);
  PythonEngine1.Py_DECREF(LMainForm);
  PythonEngine1.CheckError();
  PythonEngine1.ExecString(AnsiString(mmInput.Lines.Text));
end;

procedure TPyMainForm.AppEventHandler(const Sender: TObject; const AMessage: TMessage);
begin
  case TApplicationEventMessage(AMessage).Value.Event of
    TApplicationEvent.WillBecomeInactive:
    begin
      if Assigned(Camera) then
      begin
        FSavedCameraActive := Camera.Active;
        Camera.Active := False;
      end;
    end;
    TApplicationEvent.BecameActive:
    begin
      if Assigned(Camera) then
      begin
        Camera.Active := FSavedCameraActive;
      end;
    end;
    TApplicationEvent.FinishedLaunching:
      ConfigurePython();
  end;
end;

function TPyMainForm.AskDelphiForCameraPermission(PSelf, AArgs: PPyObject)
  : PPyObject; cdecl;
begin
  if not PermissionsService.IsPermissionGranted(FPermissionCamera) then
  begin
    Result := PythonEngine1.ReturnFalse;
    PermissionsService.RequestPermissions([FPermissionCamera],
      ActivateCameraPermissionRequestResult, DisplayRationale);
  end
  else
    Result := PythonEngine1.ReturnTrue;
end;

function TPyMainForm.AskDelphiToShowFrame(PSelf, AArgs: PPyObject): PPyObject;
begin
  TThread.Synchronize(TThread.Current,
    procedure begin
      if Camera.Active then
        Camera.SampleBufferToBitmap(Image.Bitmap, true);
    end);
  Result := PythonEngine1.ReturnNone;
end;

procedure TPyMainForm.ConfigurePython;
begin
  FAppEnv.InitializeEnvironmentAsync(PythonEngine1, True,
    procedure(const AInitialized: boolean; const ALastErrorMsg: string)
    begin
      if AInitialized then
        EnableComponents()
      else
        TThread.Synchronize(nil,
          procedure
          begin
            ShowMessage(ALastErrorMsg);
          end);
    end);
end;

procedure TPyMainForm.DisableComponents;
begin
  btnRun.Enabled := false;
  btnClearOutput.Enabled := false;
  btnClearInput.Enabled := false;
end;

procedure TPyMainForm.DisplayRationale(Sender: TObject;
const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  TDialogService.ShowMessage
    ('The app needs to access the camera in order to work',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TPyMainForm.EnableComponents;
begin
  btnRun.Enabled := True;
  btnClearOutput.Enabled := True;
  btnClearInput.Enabled := True;
end;

end.
