(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'     Copyright (c) 2021                        *)
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
(*  Functionality:  MainForm of PyLoad                                    *)
(*                  Python4Delphi on Android                              *)
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Ani, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PythonEngine, FMX.PythonGUIInputOutput, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.Layouts, FMX.Platform, AppEnvironment, ProgressFrame,
  System.Threading;

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
    function AppEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): boolean;
    procedure ConfigurePython();
    procedure DisableComponents();
    procedure EnableComponents();
  public
    { Public declarations }
  end;

var
  PyMainForm: TPyMainForm;

implementation

uses
  PythonLoad, System.IOUtils;

{$R *.fmx}

{ TPyMainForm }

procedure TPyMainForm.FormCreate(Sender: TObject);
begin
  DisableComponents();
  FAppEnv := TAppEnvironment.Create(TProgressViewFrame.Create(Self, Self));

  var LAppEventService := IFMXApplicationEventService(nil);
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXApplicationEventService, IInterface(LAppEventService)) then
      LAppEventService.SetApplicationEventHandler(AppEventHandler)
  else begin
    Log.d('Platform service "IFMXApplicationEventService" not supported.');
    Halt(1);
  end;
end;

procedure TPyMainForm.FormDestroy(Sender: TObject);
begin
  FAppEnv.Free();
end;

procedure TPyMainForm.actClearInputExecute(Sender: TObject);
begin
  mmInput.Lines.Clear();
end;

procedure TPyMainForm.actClearOutputExecute(Sender: TObject);
begin
  mmOutput.Lines.Clear();
end;

procedure TPyMainForm.actRunExecute(Sender: TObject);
begin
  PythonEngine1.ExecString(AnsiString(mmInput.Lines.Text));
end;

function TPyMainForm.AppEventHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): boolean;
begin
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: ConfigurePython();
  end;
  Result := true;
end;

procedure TPyMainForm.ConfigurePython;
begin
  FAppEnv.InitializeEnvironmentAsync(PythonEngine1, true,
    procedure(const AInitialized: boolean; const ALastErrorMsg: string) begin
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

procedure TPyMainForm.EnableComponents;
begin
  btnRun.Enabled := true;
  btnClearOutput.Enabled := true;
  btnClearInput.Enabled := true;
end;

end.
