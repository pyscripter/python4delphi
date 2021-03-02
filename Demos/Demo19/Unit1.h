//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "PythonEngine.hpp"
#include "Vcl.PythonGUIInputOutput.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
    TMemo *Memo1;
    TPanel *Panel1;
    TButton *Button1;
    TButton *Button2;
    TButton *Button3;
    TPythonEngine *PythonEngine1;
    TPythonModule *PythonModule1;
    TOpenDialog *OpenDialog1;
    TSaveDialog *SaveDialog1;
    TPythonGUIInputOutput *PythonGUIInputOutput1;
    TMemo *Memo2;
    TSplitter *Splitter1;
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall Button3Click(TObject *Sender);
    void __fastcall PythonModule1Initialization(TObject *Sender);
private:	// Déclarations de l'utilisateur
public:		// Déclarations de l'utilisateur
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
