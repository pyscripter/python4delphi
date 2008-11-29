unit fmOutput;

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

{$I Definition.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  PythonEngine,
  PythonVclStd,
  PythonGUIInputOutput;


type
  TOutputForm = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    RichEdit1: TRichEdit;
    procedure PythonEngine1AfterInit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  OutputForm: TOutputForm;

implementation
{$R *.DFM}

procedure TOutputForm.PythonEngine1AfterInit(Sender: TObject);
begin
  with GetPythonEngine, PythonGUIInputOutput1 do
    begin
      RichEdit1.Lines.Clear;
      WriteLine( Py_GetCopyright );
      WriteLine( 'Version '+Py_GetVersion );
      //WriteLine( 'Portions copyright 1997 Morgan Martinet (mmm@imaginet.fr)' );
      WriteLine( '' );
    end;
end;

procedure TOutputForm.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  with TPythonVclStd.Create(nil) do
    Engine := PythonEngine1;
  with GetPythonEngine do
    begin
      for i := 0 to ClientCount - 1 do
        if not Clients[i].Initialized then
          Clients[i].Initialize;
      AutoFinalize := True;
      FreeClients := False;
    end;
end;

end.
