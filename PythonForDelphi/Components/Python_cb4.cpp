//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Python_cb4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("Sources\Core\PythonGUIInputOutput.pas");
USEUNIT("Sources\Core\PythonEngine.pas");
USERES("Sources\Core\PythonEngine.dcr");
USEUNIT("Sources\Core\MethodCallBack.pas");
USEUNIT("Sources\Core\AtomPythonEngine.pas");
USEUNIT("Sources\Core\PythonAtom.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
