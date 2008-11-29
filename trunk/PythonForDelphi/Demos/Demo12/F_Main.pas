unit F_Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, PythonEngine, PythonGUIInputOutput, PythonAtom;

type

  TFRM_Main = class(TForm)
    MEM_Script: TRichEdit;
    MEM_Console: TRichEdit;
    BTN_Exec: TButton;
    PythonEngine: TPythonEngine;
    PythonGUIInputOutput: TPythonGUIInputOutput;
    BTN_DelphiVarIntoAtom: TButton;
    PYV_DelphiVar: TPythonDelphiVar;
    BTN_AtomMyMethod: TButton;
    BTN_AtomMyMethod2_toto: TButton;
    BTN_myMethod3_6: TButton;
    BTN_MySelf: TButton;
    BTN_AtomMyProperty: TButton;
    BTN_AtomMyProperty_Bar: TButton;
    BTN_AtomMyList_1: TButton;
    BTN_AtomMyListProperty_2: TButton;
    BTN_AtomNonExistentMethod: TButton;
    PrintOfAnotherAtomBtn: TButton;
    procedure BTN_ExecClick(Sender: TObject);
    procedure BTN_DelphiVarIntoAtomClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PYV_DelphiVarChange(Sender: TObject);
    procedure BTN_AtomMyMethodClick(Sender: TObject);
    procedure MEM_ConsoleChange(Sender: TObject);
    procedure BTN_AtomMyMethod2Click(Sender: TObject);
    procedure BTN_AtomMyMethod2_totoClick(Sender: TObject);
    procedure BTN_myMethod3_6Click(Sender: TObject);
    procedure BTN_MySelfClick(Sender: TObject);
    procedure BTN_AtomMyPropertyClick(Sender: TObject);
    procedure BTN_AtomMyProperty_BarClick(Sender: TObject);
    procedure BTN_AtomMyList_1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BTN_AtomMyListProperty_2Click(Sender: TObject);
    procedure BTN_AtomNonExistentMethodClick(Sender: TObject);
    procedure PrintOfAnotherAtomBtnClick(Sender: TObject);
  private
    { Déclarations privées }
    myAtom : OleVariant;
  public
    { Déclarations publiques }
  end;

var
  FRM_Main: TFRM_Main;

implementation


{$R *.DFM}

procedure TFRM_Main.BTN_ExecClick(Sender: TObject);
begin
  PythonEngine.ExecStrings(MEM_Script.Lines);
end;

procedure TFRM_Main.BTN_DelphiVarIntoAtomClick(Sender: TObject);
var
  value : PPyObject;
begin
  value := PYV_DelphiVar.ValueObject;
  try
    myAtom := getAtom(value);
  finally
    GetPythonEngine.Py_XDecRef(value);
  end;
end;

procedure TFRM_Main.FormShow(Sender: TObject);
begin
  {$IFDEF DEBUG}DebugStrings:=MEM_Console.Lines;{$ENDIF}
end;

procedure TFRM_Main.PYV_DelphiVarChange(Sender: TObject);
begin
  MEM_Console.Lines.Add('delphiVar changed !');
end;

procedure TFRM_Main.BTN_AtomMyMethodClick(Sender: TObject);
begin
  myAtom.myMethod();
end;

procedure TFRM_Main.MEM_ConsoleChange(Sender: TObject);
begin
  SendMessage(MEM_Console.Handle, EM_SCROLL, SB_LINEDOWN, 0);
end;

procedure TFRM_Main.BTN_AtomMyMethod2Click(Sender: TObject);
begin
  myAtom.myMethod2;
end;

procedure TFRM_Main.BTN_AtomMyMethod2_totoClick(Sender: TObject);
begin
  myAtom.myMethod2('toto');
end;

procedure TFRM_Main.BTN_myMethod3_6Click(Sender: TObject);
begin
  ShowMessage(myAtom.myMethod3(6));
end;

procedure TFRM_Main.BTN_MySelfClick(Sender: TObject);
begin
  ShowMessage(myAtom.mySelf.mySelf.mySelf.mySelf.mySelf.mySelf.mySelf.mySelf.myMethod3(18));
end;

procedure TFRM_Main.BTN_AtomMyPropertyClick(Sender: TObject);
begin
  ShowMessage(myAtom.myProperty);
end;

procedure TFRM_Main.BTN_AtomMyProperty_BarClick(Sender: TObject);
begin
  myAtom.myProperty:='bar';
end;

procedure TFRM_Main.BTN_AtomMyList_1Click(Sender: TObject);
begin
  ShowMessage(myAtom.myList()[1]);
end;

procedure TFRM_Main.FormDestroy(Sender: TObject);
begin
  {$IFDEF DEBUG}DebugStrings := nil;{$ENDIF}
end;

procedure TFRM_Main.BTN_AtomMyListProperty_2Click(Sender: TObject);
begin
  ShowMessage(myAtom.myListProperty[2]);
end;

procedure TFRM_Main.BTN_AtomNonExistentMethodClick(Sender: TObject);
begin
  myAtom.nonExistentMethod();
end;

procedure TFRM_Main.PrintOfAnotherAtomBtnClick(Sender: TObject);
begin
  myAtom.printMyPropertyOtherAtom(myAtom);
end;

end.
