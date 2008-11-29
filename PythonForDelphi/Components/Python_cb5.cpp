//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Python_cb5.res");
USEUNIT("Sources\Core\PythonGUIInputOutput.pas");
USEUNIT("Sources\Core\PythonEngine.pas");
USERES("Sources\Core\PythonEngine.dcr");
USEUNIT("Sources\Core\MethodCallBack.pas");
USEPACKAGE("vcl50.bpi");
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
