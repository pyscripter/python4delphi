// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PythonGUIInputOutput.pas' rev: 6.00

#ifndef PythonGUIInputOutputHPP
#define PythonGUIInputOutputHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <PythonEngine.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pythonguiinputoutput
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPythonGUIInputOutput;
class PASCALIMPLEMENTATION TPythonGUIInputOutput : public Pythonengine::TPythonInputOutput 
{
	typedef Pythonengine::TPythonInputOutput inherited;
	
private:
	Stdctrls::TCustomMemo* FCustomMemo;
	HWND FWinHandle;
	
protected:
	void __fastcall pyGUIOutputWndProc(Messages::TMessage &Message);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall SendData(const AnsiString Data);
	virtual AnsiString __fastcall ReceiveData();
	virtual void __fastcall AddPendingWrite(void);
	void __fastcall WriteOutput(void);
	
public:
	__fastcall virtual TPythonGUIInputOutput(Classes::TComponent* AOwner);
	__fastcall virtual ~TPythonGUIInputOutput(void);
	void __fastcall DisplayString(const AnsiString str);
	
__published:
	__property Stdctrls::TCustomMemo* Output = {read=FCustomMemo, write=FCustomMemo};
};


//-- var, const, procedure ---------------------------------------------------
static const Word WM_WriteOutput = 0x401;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Pythonguiinputoutput */
using namespace Pythonguiinputoutput;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PythonGUIInputOutput
