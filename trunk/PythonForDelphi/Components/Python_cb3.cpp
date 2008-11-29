//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Python_cb3.res");
USEPACKAGE("VCL35.bpi");
USEUNIT("Sources\Core\PythonGUIInputOutput.pas");
USEUNIT("Sources\Core\PythonEngine.pas");
USERES("Sources\Core\PythonEngine.dcr");
USEUNIT("Sources\Core\MethodCallBack.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Source du paquet.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
