(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'ProgressFrame'    Copyright (c) 2021                    *)
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
(*  Functionality:  Environment loading progress report                   *)
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
unit ProgressFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, AppEnvironment;

type
  TProgressViewFrame = class(TFrame, IProgressNotifier)
    pnlContent: TPanel;
    lbDescription: TLabel;
    lblCurrentAction: TLabel;
    pbProgress: TProgressBar;
    pnlBackground: TPanel;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; const AForm: TCustomForm); reintroduce;

    procedure Start(const ADescription, AFirstAction: string; const ATotal: Int64);
    procedure Update(const ACurrentAction: string; const AProgress: Int64);
    procedure Stop();
  end;

implementation

uses
  Math;

{$R *.fmx}

{ TProgressViewFrame }

constructor TProgressViewFrame.Create(AOwner: TComponent;
  const AForm: TCustomForm);
begin
  inherited Create(AOwner);
  lblCurrentAction.Text := String.Empty;
  Parent := AForm;
  Align := TAlignLayout.Contents;
  pnlContent.Width := Math.Min(AForm.Width - 40, 410);
end;

procedure TProgressViewFrame.Start(const ADescription, AFirstAction: string;
  const ATotal: Int64);
begin
  lbDescription.Text := ADescription;
  lblCurrentAction.Text := AFirstAction;
  pbProgress.Min := 0;
  pbProgress.Max := ATotal;
  pbProgress.Value := 0;
  pnlContent.Visible := not ADescription.IsEmpty();
end;

procedure TProgressViewFrame.Stop;
begin
  Self.Visible := false;
end;

procedure TProgressViewFrame.Update(const ACurrentAction: string; const AProgress: Int64);
begin
  lblCurrentAction.Text := ACurrentAction;
  pbProgress.Value := AProgress;
end;

end.
