// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MethodCallBack.pas' rev: 6.00

#ifndef MethodCallBackHPP
#define MethodCallBackHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Methodcallback
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TCallType { ctSTDCALL, ctCDECL };
#pragma option pop

typedef void __fastcall (__closure *TCallBack)(void);

typedef int __stdcall (__closure *TDDEAPIfunc)(int CallType, int Fmt, int Conv, int hsz1, int hsz2, int Data, int Data1, int Data2);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void * __fastcall GetDDECallBack(TDDEAPIfunc method);
extern PACKAGE void * __fastcall GetOfObjectCallBack(TCallBack CallBack, int argnum, TCallType calltype);
extern PACKAGE void * __fastcall GetCallBack(System::TObject* self, void * method, int argnum, TCallType calltype);
extern PACKAGE void __fastcall DeleteCallBack(void * Proc);

}	/* namespace Methodcallback */
using namespace Methodcallback;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MethodCallBack
