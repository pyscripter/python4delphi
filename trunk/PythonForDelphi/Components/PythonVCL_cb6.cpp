//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("PythonVCL_cb6.res");
USEUNIT("Sources\VCL\PythonDatabase.pas");
USERES("Sources\VCL\PythonDatabase.dcr");
USEUNIT("Sources\VCL\pyDBTables.pas");
USEUNIT("Sources\VCL\pyDB.pas");
USEPACKAGE("Python_cb6.bpi");
USEPACKAGE("vcl60.bpi");
//USEPACKAGE("vcldb60.bpi");
//USEPACKAGE("vclbde60.bpi");
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
