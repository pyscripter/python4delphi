
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


unit qrPrintText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, quickrpt, Qrctrls;

type
  TPrintText = class(TForm)
    QRBand1: TQRBand;
    Rep: TQuickRep;
    QRLabel1: TQRLabel;
    QRBand2: TQRBand;
    QRSysData3: TQRSysData;
    QRSysData1: TQRSysData;
    QRSysData2: TQRSysData;
    QRShape1: TQRShape;
    procedure RepNeedData(Sender: TObject; var MoreData: Boolean);
    procedure RepBeforePrint(Sender: TQuickRep; var PrintReport: Boolean);
  private
    { Private declarations }
    line : Integer;
  public
    { Public declarations }
    RichEdit : TRichEdit;
  end;

var
  PrintText: TPrintText;

implementation

{$R *.DFM}



procedure TPrintText.RepNeedData(Sender: TObject; var MoreData: Boolean);
var
 tabs, i : Integer;
 s : String;
begin
  MoreData := line < RichEdit.Lines.Count;
  if MoreData then
    begin
      tabs := 0;
      s := RichEdit.lines.Strings[line];
      for i := 1 to Length(s) do
        if s[i] <> #9 then
          Break
        else
          Inc(tabs);
      s := Trim(s);
      for i := 0 to tabs - 1 do
        Insert( '  ', s, 1 );
      QRLabel1.Caption := s;
      Inc(line);
    end;
end;

procedure TPrintText.RepBeforePrint(Sender: TQuickRep;
  var PrintReport: Boolean);
begin
  line := 0;
end;

end.
