unit Childwin;

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
     Messages, Dialogs, SysUtils, PythonEngine;

type
  TMDIChild = class(TForm)
    StatusBar1: TStatusBar;
    RichEdit1: TRichEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Memo1Click(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RichEdit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RichEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RichEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RichEdit1Change(Sender: TObject);
  private
    { Private declarations }
    FFileName : String;

  public
    { Public declarations }
    procedure OpenItem( const filename : String );
    procedure SaveItem;
    procedure SaveItemAs;
    procedure CutItem;
    procedure CopyItem;
    procedure PasteItem;
    procedure ExecItem;
    procedure CheckSyntaxItem;
    procedure CompileItem;
    procedure WriteItem;
    procedure ShowLineNumber;
    procedure ShowSyntaxError( E : EPySyntaxError );
    procedure AutoIndent;
    procedure ResetChanged;
    procedure SetChanged;
  end;

implementation
uses Main, fmOutput, Misc, fmTraceBack;
{$R *.DFM}


procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TMDIChild.OpenItem( const filename : String );
begin
  FFileName := filename;
  RichEdit1.Lines.LoadFromFile(FFileName);
  RichEdit1.Modified := False;
  ResetChanged;
end;

procedure TMDIChild.SaveItem;
begin
  if FFileName <> '' then
    begin
      RichEdit1.Lines.SaveToFile( FFileName );
      RichEdit1.Modified := False;
      ResetChanged;
    end
  else
    SaveItemAs;
end;

procedure TMDIChild.SaveItemAs;
begin
  with MainForm.SaveDialog1 do
    begin
      if Execute then
        begin
          RichEdit1.Lines.SaveToFile( FileName );
          RichEdit1.Modified := False;
          FFileName := FileName;
          Caption := FFileName;
          MainForm.RecentFiles1.LatestFile := FileName;
          ResetChanged;
        end;
    end;
end;



procedure TMDIChild.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  id : Integer;
begin
  if RichEdit1.Modified then
    begin
      id := MessageDlg('File "'+Caption+'" has changed. Do you want to save it ?', mtConfirmation, [mbYes,mbNo,mbCancel],0);
      CanClose := id <> mrCancel;
      if id = mrYes then
        SaveItem;
    end;
end;

procedure TMDIChild.CutItem;
begin
  RichEdit1.CutToClipboard;
end;

procedure TMDIChild.CopyItem;
begin
  RichEdit1.CopyToClipboard;
end;

procedure TMDIChild.PasteItem;
begin
  RichEdit1.PasteFromClipboard;
end;

procedure TMDIChild.ExecItem;
var
  co, res : PPyObject;
begin
  res := nil;
  with GetPythonEngine do
    begin
      // We don't use PyExec_Command because we want to specify
      // a filename for the source code, in order to have
      // trace back that mentions the filename where the
      // error occurred.

      // Clear old traceback
      if Assigned( TraceBack ) then
        TraceBack.Clear;

      // Compile source code
      co := Py_CompileString(PChar(CleanString(RichEdit1.Lines.Text)), PChar(Caption), file_input );
      try
        // if successfully compiled
        if Assigned( co ) then
          begin
            // Execute ByteCode, in the __main__ context
            res := PyImport_ExecCodeModule( '__main__', co );
          end;
      finally
        // Cleanup ByteCode
        Py_XDECREF(co);
      end;
      // If Error occurred
      if not Assigned(res) then
        begin
          // Display error
          PyErr_Print;
          // Update Traceback
          if Assigned(TraceBack) then
            TraceBack.BuildTraceBack;
          // Throw exception for the error
          try
            RaiseError;
          except
            on E: EPySyntaxError do
              begin
                if E.EFileName = '???' then
                  E.EFileName := Caption;
                ShowSyntaxError( E );
                raise;
              end;
          end;
        end
      else
        begin
          // Cleanup result
          Py_XDECREF(res);
        end;
    end;
end;

procedure TMDIChild.CheckSyntaxItem;
begin
  with GetPythonEngine do
    begin
      if CheckExecSyntax( CleanString(RichEdit1.Text) ) then
        ShowMessage('Syntax ok.')
      else
        begin
          ShowMessage('Syntax failed.');
          PyErr_Print;
          RaiseError;
        end;
    end;
end;

procedure TMDIChild.CompileItem;
var
  co : PPyObject;
begin
  with GetPythonEngine do
    begin
      co := Py_CompileString(PChar(CleanString(RichEdit1.Lines.Text)), 'essai', file_input );
      if Assigned(co) then
        begin
          ShowMessage('Compilation OK' );
          Py_XDECREF(co);
        end
      else
        begin
          ShowMessage('Compilation failed !');
          PyErr_Print;
          RaiseError;
        end;
    end;
end;

procedure TMDIChild.WriteItem;
var
  args, co, co2, f, str, res : PPyObject;
begin
{  with GetPythonEngine do
    begin
      f := PyModule_GetSymbol( 'marshal', 'dumps' );
      co := Py_CompileString(PChar(CleanString(RichEdit1.Lines.Text)), 'rub_BRUT', file_input );
      if not Assigned(co) then
        raise EPythonError.Create('Could not compile string');
      try
        args := Py_MakeArgs( [co] );
        try
          str := PyEval_CallObject(f,args);
          try
            co2 := PyMarshal_ReadObjectFromString(PyString_AsString(str), PyString_Size(str));
            if not Assigned(co2) then
              raise EPythonError.Create('Could not read object from string');

            try
              res := PyImport_ExecCodeModule( '__main__', co2 );
              if not Assigned(res) then
                PyErr_Print;
              PyDECREF(res);
              PyDict_SetItemString(PyMain_GetDict, 'essai', co2 );
            finally
              PyDECREF(co2);
            end;
          finally
            PyDECREF(str);
          end;
        finally
          PyDECREF(args);
        end;
      finally
        PyDECREF(co);
      end;
    end;}
end;

procedure TMDIChild.FormKeyPress(Sender: TObject; var Key: Char);
begin
  ShowLineNumber;
end;

procedure TMDIChild.ShowLineNumber;
begin
  StatusBar1.Panels[0].Text := Format('%6d:%3d',[GetLineNumber( RichEdit1 ), GetColNumber(RichEdit1)]);
end;

procedure TMDIChild.Memo1Click(Sender: TObject);
begin
  ShowLineNumber;
end;

procedure TMDIChild.Memo1DblClick(Sender: TObject);
begin
  ShowLineNumber;
end;

procedure TMDIChild.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ShowLineNumber;
end;

procedure TMDIChild.ShowSyntaxError( E : EPySyntaxError );
var
  i : Integer;
  f : TMDIChild;
begin
  if (E.EFileName = '<string>') then
    Exit;
  f := nil;
  // Look for the good form containing our source code
  for i := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] is TMDIChild then
      begin
        f := TMDIChild(Screen.Forms[i]);
        // If we find a form whose caption is the same as the Filename
        if f.Caption = E.EFileName then
          begin
            // We found our form !
            // We restore the state, in needed
            if f.WindowState = wsMinimized then
              f.WindowState := wsNormal;
            // We bring the form to the front
            f.BringToFront;
            Break;
          end;
        f := nil;
      end;
  if not Assigned( f ) then
    begin
      // Not found in the windows, so load the file
      f := TMDIChild( MainForm.CreateMDIChild(E.EFilename) );
      f.OpenItem(E.EFilename);
    end;
  with f do
    begin
      // We put the error message in the status bar
      //StatusBar1.Panels[1].Text := E.EName+': '+E.EValue;
      // We select the line that contains the error
      RichEdit1.SelStart := GetLineIndex(RichEdit1,E.ELineNumber-1);
      RichEdit1.SelLength := E.EOffset;
      // We make the selection visible
      GetScrollCaret( RichEdit1 );
    end;
  // That's done !
end;

procedure TMDIChild.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ShowLineNumber;
end;

procedure TMDIChild.RichEdit1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowLineNumber;
end;

procedure TMDIChild.RichEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowLineNumber;
end;

procedure TMDIChild.RichEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ShowLineNumber;
end;

procedure TMDIChild.RichEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ShowLineNumber;
  if Key = VK_RETURN then
    AutoIndent;
end;

procedure TMDIChild.AutoIndent;
var
  cur_line : Integer;
  prev     : String;
  indent   : String;
  i        : Integer;
  cursor   : Integer;
begin
  cur_line := GetLineNumber( RichEdit1 ) - 1;
  if cur_line > 0 then
    begin
      prev := RichEdit1.Lines.Strings[cur_line-1];
      indent := '';
      for i := 1 to length(prev) do
        begin
          if not ( prev[i] in [' ', #9]) then
            Break;
          Insert( prev[i], indent, i+1 );
        end;
      cursor := RichEdit1.SelStart;
      RichEdit1.Lines.Strings[cur_line] := indent + TrimLeft(RichEdit1.Lines.Strings[cur_line]);
      RichEdit1.SelStart := cursor + length(indent);
      RichEdit1.SelLength := 0
    end;
end;

procedure TMDIChild.RichEdit1Change(Sender: TObject);
begin
  SetChanged;
end;

procedure TMDIChild.ResetChanged;
begin
  StatusBar1.Panels[1].Text := '';
end;

procedure TMDIChild.SetChanged;
begin
  StatusBar1.Panels[1].Text := 'Modified';
end;

end.
