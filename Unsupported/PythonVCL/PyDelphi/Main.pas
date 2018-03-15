unit Main;

(**************************************************************************)
(*                                                                        *)
(* PyDelphi                            Copyright (c) 1997                 *)
(*                                                                        *)
(* Version: 1.0                        Morgan Martinet                    *)
(* Sub-Version: 0.1                    23 rue du 14 juillet               *)
(* Date: 27/01/1998                    94270 le Kremlin-Bicetre           *)
(*                                     Phone (Work): 01 47 25 70 77       *)
(*                                     e-mail: mmm@imaginet.fr            *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality:  PyDelphi is a Python IDE.                             *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
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

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, MRUFList;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileCloseItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    SpeedPanel: TPanel;
    OpenBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    CutBtn: TSpeedButton;
    CopyBtn: TSpeedButton;
    PasteBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    StatusBar: TStatusBar;
    SaveDialog1: TSaveDialog;
    SBExecute: TSpeedButton;
    Execute1: TMenuItem;
    View1: TMenuItem;
    Execute2: TMenuItem;
    Compile1: TMenuItem;
    Checksyntax1: TMenuItem;
    SpeedButton2: TSpeedButton;
    Write1: TMenuItem;
    Traceback1: TMenuItem;
    N2: TMenuItem;
    Find1: TMenuItem;
    Findnext1: TMenuItem;
    Replace1: TMenuItem;
    N3: TMenuItem;
    Printersetup1: TMenuItem;
    Print1: TMenuItem;
    Preview1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Clear1: TMenuItem;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FileNewItemClick(Sender: TObject);
    procedure WindowCascadeItemClick(Sender: TObject);
    procedure UpdateMenuItems(Sender: TObject);
    procedure WindowTileItemClick(Sender: TObject);
    procedure WindowArrangeItemClick(Sender: TObject);
    procedure FileCloseItemClick(Sender: TObject);
    procedure FileOpenItemClick(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure FileSaveItemClick(Sender: TObject);
    procedure FileSaveAsItemClick(Sender: TObject);
    procedure CutItemClick(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure PasteItemClick(Sender: TObject);
    procedure WindowMinimizeItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Execute2Click(Sender: TObject);
    procedure Compile1Click(Sender: TObject);
    procedure Checksyntax1Click(Sender: TObject);
    procedure Write1Click(Sender: TObject);
    procedure Traceback1Click(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure RecentFiles1Click(Sender: TObject; LatestFile: string);
    procedure Find1Click(Sender: TObject);
    procedure Findnext1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Printersetup1Click(Sender: TObject);
    procedure Preview1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    FHWND_NEXT : HWND;

    procedure ShowHint(Sender: TObject);
    procedure WMCHANGECBCHAIN( var message : TWMCHANGECBCHAIN ); message WM_CHANGECBCHAIN;
    procedure WMDRAWCLIPBOARD( var message : TWMDRAWCLIPBOARD ); message WM_DRAWCLIPBOARD;

  public
    { Public declarations }
    RecentFiles1 : TRecentFiles;

    function CreateMDIChild(const Name: string) : TForm;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses ChildWin, fmTraceBack, fmAbout, fmFind, fmReplace,
     qrPrintText, ClipBrd, PyDelphiAssoc;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
  Screen.OnActiveFormChange := UpdateMenuItems;
  FHWND_NEXT := SetClipboardViewer( Handle );
  //if FHWND_NEXT = 0 then
  //  MessageDlg('Could not add a Clipboard viewer', mtError, [mbOk], 0);
  RecentFiles1 := TRecentFiles.Create(Self);
  RecentFiles1.DeleteEntry := False;
  RecentFiles1.MaxFiles := 9;
  RecentFiles1.Menu := File1;
  RecentFiles1.RegistryKey := 'Software\Python\PyDelphi';
  RecentFiles1.StandAloneMenu := False;
  RecentFiles1.LoadFromRegistry;
  RecentFiles1.OnClick := RecentFiles1Click;
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  StatusBar.SimpleText := Application.Hint;
end;

function TMainForm.CreateMDIChild(const Name: string) : TForm;
var
  Child: TMDIChild;
begin
  { create a new MDI child window }
  Child := TMDIChild.Create(Application);
  Child.Caption := Name;
  result := Child;
end;

procedure TMainForm.FileNewItemClick(Sender: TObject);
begin
  CreateMDIChild('NONAME' + IntToStr(MDIChildCount + 1));
end;

procedure TMainForm.FileOpenItemClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    begin
      RecentFiles1.LatestFile := OpenDialog.FileName;
      CreateMDIChild(OpenDialog.FileName);
      (ActiveMDIChild as TMDIChild).OpenItem(OpenDialog.FileName);
    end;
end;

procedure TMainForm.FileCloseItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    ActiveMDIChild.Close;
end;

procedure TMainForm.FileSaveItemClick(Sender: TObject);
begin
  (ActiveMDIChild as TMDIChild).SaveItem;
end;

procedure TMainForm.FileSaveAsItemClick(Sender: TObject);
begin
  (ActiveMDIChild as TMDIChild).SaveItemAs;
end;

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.CutItemClick(Sender: TObject);
begin
  //(ActiveMDIChild as TMDIChild).CutItem;
  SendMessage( Screen.ActiveControl.Handle, WM_CUT, 0, 0 );
end;

procedure TMainForm.CopyItemClick(Sender: TObject);
begin
  //(ActiveMDIChild as TMDIChild).CopyItem;
  SendMessage( Screen.ActiveControl.Handle, WM_COPY, 0, 0 );
end;

procedure TMainForm.PasteItemClick(Sender: TObject);
begin
  //(ActiveMDIChild as TMDIChild).PasteItem;
  SendMessage( Screen.ActiveControl.Handle, WM_PASTE, 0, 0 );
end;

procedure TMainForm.WindowCascadeItemClick(Sender: TObject);
begin
  Cascade;
end;

procedure TMainForm.WindowTileItemClick(Sender: TObject);
begin
  Tile;
end;

procedure TMainForm.WindowArrangeItemClick(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TMainForm.WindowMinimizeItemClick(Sender: TObject);
var
  I: Integer;
begin
  { Must be done backwards through the MDIChildren array }
  for I := MDIChildCount - 1 downto 0 do
    MDIChildren[I].WindowState := wsMinimized;
end;

procedure TMainForm.UpdateMenuItems(Sender: TObject);
var
  is_mdichild : Boolean;
begin
  is_mdichild := ActiveMDIChild is TMDIChild;
  FileCloseItem.Enabled := (MDIChildCount > 0) and is_mdichild;
  FileSaveItem.Enabled := (MDIChildCount > 0) and is_mdichild;
  FileSaveAsItem.Enabled := (MDIChildCount > 0) and is_mdichild;
  Print1.Enabled := (MDIChildCount > 0) and is_mdichild;
  Preview1.Enabled := (MDIChildCount > 0) and is_mdichild;
  Find1.Enabled := (MDIChildCount > 0) and is_mdichild;
  FindNext1.Enabled := (MDIChildCount > 0) and is_mdichild;
  Replace1.Enabled := (MDIChildCount > 0) and is_mdichild;
  Execute1.Enabled := (MDIChildCount > 0) and is_mdichild;
  SaveBtn.Enabled := (MDIChildCount > 0) and is_mdichild;
  SBExecute.Enabled := (MDIChildCount > 0) and is_mdichild;
  WindowCascadeItem.Enabled := MDIChildCount > 0;
  WindowTileItem.Enabled := MDIChildCount > 0;
  WindowArrangeItem.Enabled := MDIChildCount > 0;
  WindowMinimizeItem.Enabled := MDIChildCount > 0;
  CutBtn.Enabled := (MDIChildCount > 0);
  CopyBtn.Enabled := (MDIChildCount > 0);
  CutItem.Enabled := (MDIChildCount > 0);
  CopyItem.Enabled := (MDIChildCount > 0);
  Clear1.Enabled := (MDIChildCount > 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FHWND_NEXT <> 0 then
    ChangeClipboardChain( Handle, FHWND_NEXT );
  Screen.OnActiveFormChange := nil;
  RecentFiles1.SaveToRegistry;
end;

procedure TMainForm.Execute2Click(Sender: TObject);
begin
  (ActiveMDIChild as TMDIChild).ExecItem;
end;

procedure TMainForm.Compile1Click(Sender: TObject);
begin
  (ActiveMDIChild as TMDIChild).CompileItem;
end;

procedure TMainForm.Checksyntax1Click(Sender: TObject);
begin
  (ActiveMDIChild as TMDIChild).CheckSyntaxItem;
end;

procedure TMainForm.Write1Click(Sender: TObject);
begin
  (ActiveMDIChild as TMDIChild).WriteItem;
end;

procedure TMainForm.Traceback1Click(Sender: TObject);
begin
  GetTraceBack;
end;

procedure TMainForm.HelpAboutItemClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.RecentFiles1Click(Sender: TObject; LatestFile: string);
begin
  CreateMDIChild(LatestFile);
  (ActiveMDIChild as TMDIChild).OpenItem(LatestFile);
end;

procedure TMainForm.Find1Click(Sender: TObject);
begin
  FormFind.RichEdit := (ActiveMDIChild as TMDIChild).RichEdit1;
  FormFind.ShowModal;
end;

procedure TMainForm.Findnext1Click(Sender: TObject);
begin
  FormFind.RichEdit := (ActiveMDIChild as TMDIChild).RichEdit1;
  FormFind.FindNext;
end;

procedure TMainForm.Replace1Click(Sender: TObject);
begin
  FormReplace.RichEdit := (ActiveMDIChild as TMDIChild).RichEdit1;
  FormReplace.ShowModal;
end;

procedure TMainForm.Printersetup1Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TMainForm.Preview1Click(Sender: TObject);
var
  fm : TMDIChild;
begin
  fm := (ActiveMDIChild as TMDIChild);
  PrintText.Rep.ReportTitle := fm.Caption;
  PrintText.RichEdit := fm.RichEdit1;
  PrintText.Rep.Preview;
end;

procedure TMainForm.Print1Click(Sender: TObject);
var
  fm : TMDIChild;
begin
  fm := (ActiveMDIChild as TMDIChild);
  PrintText.Rep.ReportTitle := fm.Caption;
  PrintText.RichEdit := fm.RichEdit1;
  PrintText.Rep.Print;
end;

procedure TMainForm.Clear1Click(Sender: TObject);
begin
  SendMessage( Screen.ActiveControl.Handle, WM_CLEAR, 0, 0 );
end;

procedure TMainForm.WMCHANGECBCHAIN( var message : TWMCHANGECBCHAIN );
begin
  with message do
    begin
      if Remove = FHWND_NEXT then
        FHWND_NEXT := Next
      else
        SendMessage( FHWND_NEXT, WM_CHANGECBCHAIN, Remove, Next );
    end;
end;

procedure TMainForm.WMDRAWCLIPBOARD( var message : TWMDRAWCLIPBOARD );
begin
  PasteBtn.Enabled := Clipboard.HasFormat( CF_TEXT );
  PasteItem.Enabled := PasteBtn.Enabled;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  ShowMessage( Format('Assoc:   %d',[gCount]));{+#13+
               Format('NewMem:  %d',[GetMemCount])+#13+
               Format('FreeMem: %d',[FreeMemCount])+#13+
               Format('Realloc: %d',[ReallocMemCount]));}
end;

end.
