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
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
// New Python type Point
class TPyPoint : public TPyObject
{
public:
	typedef TPyObject inherited;

    int x, y;

    // Constructors & Destructors
    __fastcall TPyPoint( TPythonType* APythonType );
    __fastcall TPyPoint( TPythonType* APythonType, PPyObject args );

    // Type services
    ////////////////

    // Basic services
	virtual PPyObject __fastcall GetAttr(char * key);
	virtual int __fastcall SetAttr(char * key, PPyObject value);
	virtual PPyObject __fastcall Repr(void);
	virtual int __fastcall Print(System::file &f, int i){ return 0;};

	/* virtual class method */ virtual void __fastcall RegisterMethods(TPythonType* PythonType);

    // Methods of TPyPoint
    void OffsetBy( int dx, int dy );
    void RaiseError(void);

};
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// Composants gérés par l'EDI
    TPythonEngine *PythonEngine1;
    TPythonGUIInputOutput *PythonGUIInputOutput1;
    TMemo *Memo1;
    TSplitter *Splitter1;
    TMemo *Memo2;
    TPanel *Panel1;
    TButton *Button1;
    TPythonModule *PythonModule1;
    TPythonType *PythonType1;
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall PythonType1Initialization(TObject *Sender);
private:	// Déclarations de l'utilisateur
public:		// Déclarations de l'utilisateur
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
