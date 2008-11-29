//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("PythonVCL_cb5.res");
USEUNIT("Sources\VCL\PythonDatabase.pas");
USERES("Sources\VCL\PythonDatabase.dcr");
USEUNIT("Sources\VCL\pyDBTables.pas");
USEUNIT("Sources\VCL\pyDB.pas");
USEPACKAGE("Python_cb5.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
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
