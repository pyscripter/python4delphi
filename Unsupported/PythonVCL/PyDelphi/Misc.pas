unit Misc;

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
uses Windows, Classes, Graphics, Forms, Controls, Menus, StdCtrls, ComCtrls,
     Messages;

function  GetLineNumber( memo : TRichEdit ) : Integer;
function  GetColNumber( memo : TRichEdit ) : Integer;
function  GetLineCount( memo : TRichEdit ) : Integer;
function  GetLineIndex( memo : TRichEdit; line : Word ) : Integer;
function  GetLineLength( memo : TRichEdit; line : Word ) : Integer;
procedure GetScrollCaret( memo : TRichEdit );

implementation

function GetLineNumber( memo : TRichEdit ) : Integer;
begin
   result := Memo.Perform(em_LineFromChar, -1, 0) + 1;
end;

function GetColNumber( memo : TRichEdit ) : Integer;
begin
   result := (memo.SelStart + memo.SelLength) - Memo.Perform(em_LineIndex, -1, 0) + 1;
end;

function GetLineCount( memo : TRichEdit ) : Integer;
begin
   result := Memo.Perform(em_GetLineCount, 0, 0);
end;

function GetLineIndex( memo : TRichEdit; line : Word ) : Integer;
begin
   result := Memo.Perform(em_LineIndex, line, 0);
end;

function GetLineLength( memo : TRichEdit; line : Word ) : Integer;
begin
   result := Memo.Perform(em_LineLength, line, 0);
end;

procedure GetScrollCaret( memo : TRichEdit );
begin
   Memo.Perform(em_ScrollCaret, 0, 0);
end;

end.
