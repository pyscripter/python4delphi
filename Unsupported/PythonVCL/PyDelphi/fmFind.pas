unit fmFind;

(**************************************************************************)
(*                                                                        *)
(* PyDelphi                            Copyright (c) 1997                 *)
(*                                                                        *)
(*                                     Morgan Martinet                    *)
(*                                     23 rue du 14 juillet               *)
(*                                     94270 le Kremlin-Bicetre           *)
(*                                     Phone (Work): 01 47 25 70 77       *)
(*                                     e-mail: mmm@imaginet.fr            *)
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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFormFind = class(TForm)
    Label1: TLabel;
    EdFind: TEdit;
    BtnNext: TButton;
    BtnCancel: TButton;
    CBMatchCase: TCheckBox;
    CBWholeWord: TCheckBox;
    procedure BtnNextClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    RichEdit : TRichEdit;

    procedure FindNext;
  end;

var
  FormFind: TFormFind;

implementation
uses Misc;
{$R *.DFM}

procedure TFormFind.FindNext;
var
  Options : TSearchTypes;
  start, len, result : Integer;
begin
  if not Assigned(RichEdit) then
    raise Exception.Create('You must set a RichEdit before call the FindDialog');
  if EdFind.Text <> '' then
    begin
      Options := [];
      if CBMatchCase.Checked then
        Include( Options, stMatchCase );
      if CBWholeWord.Checked then
        Include( Options, stWholeWord);
      if RichEdit.SelStart = 0 then
        start := GetLineIndex( RichEdit, GetLineNumber(RichEdit) )
      else
        start := RichEdit.SelStart+RichEdit.SelLength;
      len := GetLineIndex( RichEdit, GetLineCount( RichEdit )-1 );
      result := RichEdit.FindText( EdFind.Text, start, len, Options );
      if result < 0 then
        begin
          MessageBeep( 0 );
        end
      else
        begin
          RichEdit.SelStart := result;
          RichEdit.SelLength := length(EdFind.Text);
          GetScrollCaret( RichEdit );
        end;
    end
  else
    MessageDlg( 'Warning ! You must enter some text to find !', mtWarning, [mbOk], 0 );
end;

procedure TFormFind.BtnNextClick(Sender: TObject);
begin
  FindNext;
end;

procedure TFormFind.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

end.
