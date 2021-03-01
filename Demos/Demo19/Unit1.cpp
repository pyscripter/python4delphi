//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PythonEngine"
#pragma link "Vcl.PythonGUIInputOutput"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
//  Use MaskFPUExceptions(true) if you intend to use mathematical python package like numpy
//  MaskFPUExceptions(true);
//  PythonEngine1->SetPythonHome(PythonEngine1->DllPath);
//  PythonEngine1->LoadDll();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  PythonEngine1->ExecStrings( Memo1->Lines );
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  if ( OpenDialog1->Execute() )
    Memo1->Lines->LoadFromFile( OpenDialog1->FileName );
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject *Sender)
{
  if ( SaveDialog1->Execute() )
    Memo1->Lines->SaveToFile( SaveDialog1->FileName );
}
//---------------------------------------------------------------------------
PPyObject spam_foo( PPyObject self, PPyObject args )
{
  // normally, you would use the function PyArg_ParseTuple for
  // parsing the given arguments.
  TPythonEngine * eng = GetPythonEngine();
  ShowMessage( "args of foo: "+eng->PyObjectAsString(args) );
  return eng->ReturnNone();
}

void __fastcall TForm1::PythonModule1Initialization(TObject *Sender)
{
  PythonModule1->AddMethod( "foo", spam_foo, "foo" );
}
//---------------------------------------------------------------------------
