
#pragma hdrstop
#include <condefs.h>
#include <python.h>
USELIB("..\..\Components\py21.lib");
//---------------------------------------------------------------------------
using namespace Py;

//---------------------------------------------------------------------------
#pragma argsused

int ExecString( char * str )
{
  PyObject *m, *d;
  m = PyImport_AddModule("__main__");
  if ( ! m )
    return -1;
  d = PyModule_GetDict(m);
  PyObject * Result = PyRun_String(str, Py_file_input, d, d);
  Py_DECREF(Result);
  Py_FlushLine();
  if ( PyErr_Occurred() )
    PyErr_Print();
  if ( Result )
    return 0;
  else
    return -1;
}


int main(int argc, char **argv)
{
  Py_SetProgramName(argv[0]);
  // Init Python
  Py_Initialize();
  // Execute a script from a string
  //if ( ExecString("for i in range(100):\n  print 'Line #',i\n") )
  if ( PyRun_SimpleString("for i in range(100):\n  print 'Line #',i\n") )
    fprintf(stderr, "Could not execute script !\n");
  // Print some Python informations
  fprintf(stderr, "Python %s on %s\n%s\n",
			Py_GetVersion(), Py_GetPlatform(), Py_GetCopyright());
  // Finalize Python
  Py_Finalize();
  // Terminate
  return 0;
}
