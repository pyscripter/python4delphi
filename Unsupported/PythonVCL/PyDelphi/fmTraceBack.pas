unit fmTraceBack;

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
  ComCtrls, Grids, StdCtrls, Buttons, ExtCtrls;

type
  TTraceBack = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    StringGrid1: TStringGrid;
    BtnRefresh: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure Clear;
    procedure BuildTraceBack;
    procedure FindTrace( const filename : String; lineno : Integer; const context : String );
    function  FindTraceInWindows( const filename : String; lineno : Integer; const context : String ) : Boolean;
    procedure FindTraceInFiles( const filename : String; lineno : Integer; const context : String );
  end;

procedure GetTraceBack;

var
  TraceBack: TTraceBack;


implementation
uses PythonEngine, ChildWin, Misc, Main;
{$R *.DFM}

procedure GetTraceBack;
begin
  if Assigned(TraceBack) then
    TraceBack.BringToFront
  else
    begin
      Screen.Cursor := crHourGlass;
      try
        TraceBack := TTraceBack.Create(Application);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
end;

procedure TTraceBack.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TraceBack := nil;
  Action := caFree;
end;

procedure TTraceBack.Clear;
begin
  with StringGrid1 do
    begin
      RowCount := 2;
      FixedRows := 1;
      Cells[0,0] := 'FileName';
      Cells[1,0] := 'Line';
      Cells[2,0] := 'Context';
      Cells[0,1] := '';
      Cells[1,1] := '';
      Cells[2,1] := '';
    end;
end;

procedure TTraceBack.BuildTraceBack;
var
  tb, tb1  : PPyTraceBackObject;
  filename : String;
  lineno   : Integer;
  depth    : Integer;
  context  : String;
  limitv   : PPyObject;
  limit    : Integer;
begin
  with GetPythonEngine do
    begin
      // get the limit of the traceback
      limit := 1000;
      limitv := PySys_GetObject('tracebacklimit');
      if Assigned(limitv) and PyInt_Check(limitv) then
        limit := PyInt_AsLong(limitv);
      with StringGrid1 do
        begin
          RowCount := 1;
          Cells[0,0] := 'FileName';
          Cells[1,0] := 'Line';
          Cells[2,0] := 'Context';
          tb := PPyTraceBackObject(PySys_GetObject('last_traceback'));
          tb1 := tb;
          depth := 0;
          // Evaluate the depth of the traceback
          while Assigned(tb1) do
            begin
              Inc(depth);
              tb1 := tb1^.tb_next;
            end;
          // build the trace back
          while Assigned(tb) do
            begin
              if depth <= limit then
                begin
                  //if Py_OptimizeFlag^ <> 0 then
                  //tb^.tb_lineno := PyCode_Addr2Line( tb^.tb_frame^.f_code, tb^.tb_lasti );
                  RowCount := RowCount+1;
                  filename := PyString_AsString( tb^.tb_frame^.f_code^.co_filename );
                  lineno   := tb^.tb_lineno;
                  context  := PyString_AsString( tb^.tb_frame^.f_code^.co_name );
                  Cells[0, RowCount-1] := filename;
                  Cells[1, RowCount-1] := IntToStr(lineno);
                  Cells[2, RowCount-1] := context;
                end;
              Dec( depth );
              tb := tb^.tb_next;
            end;
          if RowCount = 1 then
            RowCount := 2;
          FixedRows := 1;
        end;
    end;
end;

procedure TTraceBack.FormCreate(Sender: TObject);
begin
  BuildTraceBack;
end;

procedure TTraceBack.StringGrid1DblClick(Sender: TObject);
begin
  with StringGrid1 do
    begin
      if Cells[0, Row] <> '' then
        begin
          FindTrace( Cells[0,Row], StrToInt(Cells[1,Row]), Cells[2,Row] );
        end;
    end;
end;

procedure TTraceBack.FindTrace( const filename : String; lineno : Integer; const context : String );
begin
  if not FindTraceInWindows( filename, lineno, context) then
    FindTraceInFiles( filename, lineno, context);
end;

function TTraceBack.FindTraceInWindows( const filename : String; lineno : Integer; const context : String ) : Boolean;
var
  i : Integer;
  f : TMDIChild;
begin
  result := False;
  // Look for the good form containing our source code
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is TMDIChild then
      begin
        f := TMDIChild(Screen.Forms[i]);
        // If we find a form whose caption is the same as the Filename
        if f.Caption = filename then
          begin
            // We found our form !
            // We restore the state, in needed
            if f.WindowState = wsMinimized then
              f.WindowState := wsNormal;
            // We bring the form to the front
            f.BringToFront;
            f.RichEdit1.SelStart := GetLineIndex(f.RichEdit1,lineno-1);
            f.RichEdit1.SelLength := GetLineLength(f.RichEdit1,f.RichEdit1.SelStart);
            GetScrollCaret( f.RichEdit1 );
            // That's done !
            result := True;
            Exit;
          end;
      end;
end;

procedure TTraceBack.FindTraceInFiles( const filename : String; lineno : Integer; const context : String );
var
  form : TForm;
begin
  form := MainForm.CreateMDIChild(filename);
  (form as TMDIChild).OpenItem(filename);
  FindTraceInWindows( filename, lineno, context);
end;

procedure TTraceBack.BtnRefreshClick(Sender: TObject);
begin
  BuildTraceBack;
end;

end.
