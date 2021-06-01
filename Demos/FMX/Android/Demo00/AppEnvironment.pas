(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'AppEnvironment'    Copyright (c) 2021                   *)
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
(*  Functionality:  App environment set up                                *)
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
unit AppEnvironment;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.Zip, PythonEngine;

type
  IProgressNotifier = interface
    ['{7A2D1743-D4D8-4093-B372-04D814536708}']
    procedure Start(const ADescription, AFirstAction: string; const ATotal: Int64);
    procedure Update(const ACurrentAction: string; const AProgress: Int64);
    procedure Stop();
  end;

  TAppEnvInit = reference to procedure(const AInitialized: boolean; const ALastErrorMsg: string);

  TAppEnvironment = class
  private
    FInitialized: boolean;
    FProgressNotifier: IProgressNotifier;
    procedure OnZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    procedure DoInitializeEnvironmentAsync(const APythonEngine: TPythonEngine;
      const ACheckPyLib: boolean = true);
  public
    constructor Create(const AProgressNotifier: IProgressNotifier);

    procedure InitializeEnvironmentAsync(const APythonEngine: TPythonEngine;
      const ACheckPyLib: boolean; const AAppEnvInit: TAppEnvInit);

    property Initialized: boolean read FInitialized;
  end;

implementation

uses
  System.IOUtils, FMX.Dialogs, PythonLoad;

{ TAppEnvironment }

constructor TAppEnvironment.Create(const AProgressNotifier: IProgressNotifier);
begin
  Assert(Assigned(AProgressNotifier), '"AProgressNotifier" undefined');
  FInitialized := false;
  FProgressNotifier := AProgressNotifier;
end;

procedure TAppEnvironment.DoInitializeEnvironmentAsync(const APythonEngine: TPythonEngine;
  const ACheckPyLib: boolean = true);
begin
  try
    //Lock user iteractions
    TThread.Synchronize(nil, procedure() begin
       FProgressNotifier.Start('Searching for Python installation', String.Empty, 0);
      end);

    Sleep(200);

    //Python distibution unzip
    TPythonLoad.Extract(
      procedure(const AFolderExists: boolean; var AReplaceFiles: boolean) begin
        if not AFolderExists then begin
          TThread.Synchronize(nil, procedure() begin
             FProgressNotifier.Start('Installing Python', String.Empty, 0);
          end);
        end;
        AReplaceFiles := false;
      end, OnZipProgressEvent);

    //Configure Python for Android
    TThread.Synchronize(nil, procedure() begin
        FProgressNotifier.Start('Configuring Python for Android', String.Empty, 3);
        FProgressNotifier.Update('Check for files', 1)
      end);

    TPythonLoad.Configure(APythonEngine);
    Sleep(1000);

    //Load python library
    TThread.Synchronize(nil, procedure() begin
        FProgressNotifier.Start('Loading Python', String.Empty, 3);
        FProgressNotifier.Update('Loading and mapping library', 2)
      end);

    TThread.Synchronize(nil, procedure() begin
        APythonEngine.LoadDll();
      end);
    Sleep(1000);

    //All done notification
    TThread.Synchronize(nil, procedure() begin
        FProgressNotifier.Start('Loading environment', String.Empty, 3);
        FProgressNotifier.Update('All ready', 3)
      end);
    Sleep(1000);
  finally
    TThread.Synchronize(nil, procedure() begin
      FProgressNotifier.Stop();
    end);
  end;
end;

procedure TAppEnvironment.InitializeEnvironmentAsync(
  const APythonEngine: TPythonEngine; const ACheckPyLib: boolean;
  const AAppEnvInit: TAppEnvInit);
begin
  TTask.Run(
    procedure() begin
      try
        DoInitializeEnvironmentAsync(APythonEngine, ACheckPyLib);
        FInitialized := true;
        if Assigned(AAppEnvInit) then
          AAppEnvInit(true, String.Empty);
      except
        on E: Exception do begin
          if Assigned(AAppEnvInit) then
            AAppEnvInit(false, E.Message);
        end;
      end;
    end);
end;

procedure TAppEnvironment.OnZipProgressEvent(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  TThread.Queue(nil, procedure() begin
    FProgressNotifier.Start('Extracting files', String.Empty, Header.UncompressedSize);
    FProgressNotifier.Update(TPath.GetFileName(FileName), Position);
  end);
end;

end.
