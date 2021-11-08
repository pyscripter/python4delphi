(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'         Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*                                  PyScripter                            *)
(*                                  e-mail: pyscripter@gmail.com          *)
(*                                                                        *)
(*  Project pages:      https://github.com/Embarcadero/python4delphi      *)
(*                      https://github.com/pyscripter/python4delphi       *)
(**************************************************************************)
(*  Functionality: Use PIP on Android.                                    *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Messaging, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Ani, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PythonEngine, FMX.PythonGUIInputOutput, System.Actions, System.Permissions,
  FMX.ActnList, FMX.Objects, FMX.Layouts, FMX.Platform,
  AppEnvironment,
  ProgressFrame;

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
    procedure actRunExecute(Sender: TObject);
    procedure actClearOutputExecute(Sender: TObject);
    procedure actClearInputExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAppEnv: TAppEnvironment;
    procedure DisableComponents();
    procedure EnableComponents();
    //App launch
    procedure AppEventHandler(const Sender: TObject; const AMessage: TMessage);
    //App permission
    procedure DoPermissionGranted();
    //Python environment
    procedure ConfigurePython();
    procedure ActivatePermissionRequestResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject;
      const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure AskForPermissions();
    //Python interpreter
    procedure CheckInterpreter();
    function BuildEnvDict(): variant;
    procedure ExecCmd(const ACmd: variant);
    //PIP commands                                         
    procedure EnsurePIP();
    procedure PIPInstallPkg();    
  public
    { Public declarations }
  end;

var
  PyMainForm: TPyMainForm;

implementation

uses
  PythonLoad,
  VarPyth,
  System.IOUtils,
  FMX.DialogService,
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

{$R *.fmx}

{ TPyMainForm }

procedure TPyMainForm.FormCreate(Sender: TObject);
begin
  DisableComponents();
  FAppEnv := TAppEnvironment.Create(TProgressViewFrame.Create(Self, Self));
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, AppEventHandler);
end;

procedure TPyMainForm.FormDestroy(Sender: TObject);
begin
  FAppEnv.Free();
end;

procedure TPyMainForm.CheckInterpreter;
begin
  var LPythonPath := TPythonLoad.GetPyInterpreter();
  if not TFile.Exists(LPythonPath) then
    raise Exception.Create('Python interpreter not found.');
end;

procedure TPyMainForm.PIPInstallPkg;
begin
  mmOutput.Lines.Add(sLineBreak);
  mmOutput.Lines.Add('# Executing PIP install command.');
  ExecCmd(VarArrayOf([TPythonLoad.GetPyInterpreter(), '-m', 'pip', 'install', 'atilo']));
end;

procedure TPyMainForm.actClearInputExecute(Sender: TObject);
begin
  mmInput.Lines.Clear();
end;

procedure TPyMainForm.actClearOutputExecute(Sender: TObject);
begin
  mmOutput.Lines.Clear();
end;

procedure TPyMainForm.ActivatePermissionRequestResult(
  Sender: TObject; const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
var
  I: integer;
begin
  for I := Low(AGrantResults) to High(AGrantResults)  do begin
    if AGrantResults[I] in [TPermissionStatus.Denied, TPermissionStatus.PermanentlyDenied] then
      raise Exception.CreateFmt('The required permission "%s" has not been granted', [APermissions[I]]);
  end;
  DoPermissionGranted();
end;

procedure TPyMainForm.actRunExecute(Sender: TObject);
begin  
  PythonEngine1.CheckError();
  PythonEngine1.ExecString(AnsiString(mmInput.Lines.Text));
end;

procedure TPyMainForm.AppEventHandler(const Sender: TObject; const AMessage: TMessage);
begin
  case TApplicationEventMessage(AMessage).Value.Event of
    TApplicationEvent.FinishedLaunching:
    begin
      AskForPermissions();      
    end;
  end;
end;

procedure TPyMainForm.AskForPermissions;
  procedure RequestPermission(const APermissions: TArray<string>);
  begin
    if not PermissionsService.IsEveryPermissionGranted(APermissions) then
      PermissionsService.RequestPermissions(APermissions,
        ActivatePermissionRequestResult, DisplayRationale)
    else
      DoPermissionGranted();
  end;
begin
  RequestPermission([
    JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE),
    JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE),
    JStringToString(TJManifest_permission.JavaClass.INTERNET)]);
end;

function TPyMainForm.BuildEnvDict: variant;
begin
  Result := NewPythonDict();
  Result.SetItem('LD_LIBRARY_PATH', TPath.GetDirectoryName(TPythonLoad.GetPyInterpreter()));
  Result.SetItem('PYTHONPATH', TPythonLoad.GetPyBin());
  Result.SetItem('PYTHONHOME', TPythonLoad.GetPyHome());
  Result.SetItem('TMPDIR', TPath.GetTempPath());
end;

procedure TPyMainForm.ConfigurePython;
begin
  FAppEnv.InitializeEnvironmentAsync(PythonEngine1, True,
    procedure(const AInitialized: boolean; const ALastErrorMsg: string)
    begin
      if AInitialized then begin        
        TThread.Synchronize(nil,
          procedure
          begin            
            CheckInterpreter();
            EnsurePIP();
            PIPInstallPkg();
            EnableComponents();
          end);
      end else
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
  const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
begin
  TDialogService.ShowMessage
    ('This app needs some permissions in order to work',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TPyMainForm.DoPermissionGranted;
begin
  ConfigurePython();
end;

procedure TPyMainForm.EnableComponents;
begin
  btnRun.Enabled := True;
  btnClearOutput.Enabled := True;
  btnClearInput.Enabled := True;
end;

procedure TPyMainForm.EnsurePIP;
begin
  mmOutput.Lines.Text := '# Executing ensurepip command.';    
  ExecCmd(VarArrayOf([TPythonLoad.GetPyInterpreter(), '-m', 'ensurepip']));
end;

procedure TPyMainForm.ExecCmd(const ACmd: variant);
begin
  var sp := Import('subprocess');
  var proc := sp.Popen(ACmd, stdout := sp.PIPE, stderr := sp.STDOUT, text := True, env := BuildEnvDict());
  while true do begin
    var line := proc.stdout.readline();
    if VarIsPythonString(line) and (VarToStr(line) <> String.Empty) then begin
      BuiltinModule.print(line.rstrip());
    end else break;      
  end;
  proc.stdout.close();
  var returncode := proc.wait();
  if StrToInt(VarToStr(returncode)) > 0 then
    raise Exception.Create('Process exit with code ' + VarToStr(returncode));
end;

end.
