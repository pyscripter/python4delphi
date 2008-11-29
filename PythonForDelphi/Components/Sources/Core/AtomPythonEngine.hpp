// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'AtomPythonEngine.pas' rev: 6.00

#ifndef AtomPythonEngineHPP
#define AtomPythonEngineHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <PythonEngine.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Atompythonengine
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TAtomPythonEngine;
class PASCALIMPLEMENTATION TAtomPythonEngine : public Pythonengine::TPythonEngine 
{
	typedef Pythonengine::TPythonEngine inherited;
	
public:
	virtual Variant __fastcall PyObjectAsVariant(Pythonengine::PPyObject obj);
public:
	#pragma option push -w-inl
	/* TPythonEngine.Create */ inline __fastcall virtual TAtomPythonEngine(Classes::TComponent* AOwner) : Pythonengine::TPythonEngine(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPythonEngine.Destroy */ inline __fastcall virtual ~TAtomPythonEngine(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Atompythonengine */
using namespace Atompythonengine;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// AtomPythonEngine
