//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("PythonVCL_cb3.res");
USEPACKAGE("VCL35.bpi");
USEPACKAGE("VCLDB35.bpi");
USEPACKAGE("Python_cb3.bpi");
USEUNIT("Sources\VCL\PythonDatabase.pas");
USERES("Sources\VCL\PythonDatabase.dcr");
USEUNIT("Sources\VCL\pyDBTables.pas");
USEUNIT("Sources\VCL\pyDB.pas");
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
