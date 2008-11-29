//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("PythonVCL_cb4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("Sources\VCL\PythonDatabase.pas");
USERES("Sources\VCL\PythonDatabase.dcr");
USEUNIT("Sources\VCL\pyDBTables.pas");
USEUNIT("Sources\VCL\pyDB.pas");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("Python_cb4.bpi");
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
