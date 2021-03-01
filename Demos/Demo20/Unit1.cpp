//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//#pragma comment (lib,"Python_D")
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PythonEngine"
#pragma link "Vcl.PythonGUIInputOutput"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
// New Python type Point
//
// Constructors & Destructors

// We override the constructors
__fastcall TPyPoint::TPyPoint( TPythonType* APythonType )
  : TPyObject(APythonType)
{
  x = 0;
  y = 0;
}

// Don't call the Create constructor of TPyPoint, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyPoint.Create will be automatically be called.
__fastcall TPyPoint::TPyPoint( TPythonType* APythonType, PPyObject args )
  : TPyObject(APythonType, args)
{
  TPythonEngine* eng = GetPythonEngine();
  if (! eng->PyArg_ParseTuple(args, "ii:CreatePoint", &x, &y ))
    return;
}

// Type services
////////////////

// Basic services
PPyObject __fastcall TPyPoint::GetAttr(char * key)
{
  TPythonEngine* eng = GetPythonEngine();
  if ( strcmp(key, "x") == 0 )
    return eng->VariantAsPyObject( x );
    // Or  return eng->PyInt_FromLong( x );
  else if ( strcmp(key, "y" ) == 0 )
    return eng->VariantAsPyObject( y );
    // or  return PyInt_FromLong( y );
  else
    return inherited::GetAttr(key);
}

int __fastcall TPyPoint::SetAttr(char * key, PPyObject value)
{
  TPythonEngine* eng = GetPythonEngine();
  PyObject * val = (PyObject *)value;
  if ( strcmp(key, "x") == 0 ) {
    if ( eng->PyArg_Parse( val, "i:Point.SetAttr", &x ) )
      return 0;
    else
      return -1;
  } else if ( strcmp(key, "y") == 0 ) {
    if ( eng->PyArg_Parse( val, "i:Point.SetAttr", &y ) )
      return 0;
    else
      return -1;
  } else
    return inherited::SetAttr(key, value);
}

PPyObject __fastcall TPyPoint::Repr(void)
{
  TPythonEngine* eng = GetPythonEngine();
  return eng->VariantAsPyObject(Format("(%d, %d)",ARRAYOFCONST((x, y))));
    // or return eng->PyString_FromString( (char *)Format("(%d, %d)", ARRAYOFCONST((x, y))) );
}

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

PPyObject __cdecl DoOffsetBy( PPyObject self, PPyObject args )
{
  TPythonEngine* eng = GetPythonEngine();
  TPyPoint * p = (TPyPoint *)PythonToDelphi(self);
  int dx, dy;
  if (! eng->PyArg_ParseTuple( (PyObject *)args, "ii:OffsetBy", &dx, &dy ))
    return NULL;
  p->OffsetBy(dx, dy);
  return eng->ReturnNone();
}

PPyObject __cdecl DoRaiseError( PPyObject self, PPyObject args )
{
  TPyPoint * p = (TPyPoint *)PythonToDelphi(self);
  p->RaiseError();
  return NULL; // we return NULL because RaiseError raised a Python error.
}

// Class method for registering the type interface methods
void __fastcall TPyPoint::RegisterMethods(TPythonType* PythonType)
{
  inherited::RegisterMethods(PythonType);
  PythonType->AddMethod( "OffsetBy", DoOffsetBy, "Point.OffsetBy( dx, dy )" );
  PythonType->AddMethod( "RaiseError", DoRaiseError, "Point.RaiseError()" );
}

// Methods of TPyPoint
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

void TPyPoint::OffsetBy( int dx, int dy )
{
  x += dx;
  y += dy;
}

// Here's an example of how you can raise errors defined
// in the module linked to our type.
void TPyPoint::RaiseError(void)
{
  TPythonEngine* eng = GetPythonEngine();
  // This is a simple call:
  // GetModule->RaiseError( "PointError", "this is an example of raising an error !" );
  // This is an advanced call:
  // We provide the instance vars as a dictionary, so that we can intercept the
  // error with "except" and extract informations from the error object.
  // ArrayToPyDict needs a list of pairs: varName (string), varValue (anything)
  GetModule()->RaiseErrorObj( "EBadPoint", "this is an example of raising an error !",
                            eng->ArrayToPyDict( ARRAYOFCONST(("a", 1, "b", 2, "c", 3)) ) );
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
//  Use MaskFPUExceptions(true) if you intend to use mathematical python package like numpy
  MaskFPUExceptions(true);
  PythonEngine1->SetPythonHome(PythonEngine1->DllPath);
  PythonEngine1->LoadDll();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  PythonEngine1->ExecStrings( Memo2->Lines );
}
//---------------------------------------------------------------------------
// We need to initialize the property PyObjectClass with
// the class of our Type object
void __fastcall TForm1::PythonType1Initialization(TObject *Sender)
{
  PythonType1->PyObjectClass = __classid(TPyPoint);
}
//---------------------------------------------------------------------------
