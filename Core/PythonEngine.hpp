// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PythonEngine.pas' rev: 6.00

#ifndef PythonEngineHPP
#define PythonEngineHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <MethodCallBack.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SyncObjs.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pythonengine
{
//-- type declarations -------------------------------------------------------
#pragma pack(push, 1)
struct TPythonVersionProp
{
	AnsiString DllName;
	AnsiString RegVersion;
	int APIVersion;
	bool CanUseLatest;
} ;
#pragma pack(pop)

typedef TPythonVersionProp PythonEngine__1[7];

#pragma option push -b-
enum TRichComparisonOpcode { pyLT, pyLE, pyEQ, pyNE, pyGT, pyGE };
#pragma option pop

#pragma option push -b-
enum TPFlag { tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfGC, tpfHaveInplaceOps, tpfCheckTypes, tpfHaveRichCompare, tpfHaveWeakRefs, tpfHaveIter, tpfHaveClass, tpfHeapType, tpfBaseType, tpfReady, tpfReadying, tpfHaveGC };
#pragma option pop

typedef Set<TPFlag, tpfHaveGetCharBuffer, tpfHaveGC>  TPFlags;

typedef char *TPChar[16001];

typedef char * *PPChar;

typedef int *PInt;

typedef double *PDouble;

typedef double *PFloat;

typedef int *PLong;

typedef Shortint *PShort;

typedef char * *PString;

struct _frozen;
typedef _frozen *P_frozen;

typedef P_frozen *PP_frozen;

struct PyObject;
typedef PyObject *PPyObject;

typedef PPyObject *PPPyObject;

typedef PPPyObject *PPPPyObject;

struct PyIntObject;
typedef PyIntObject *PPyIntObject;

struct PyTypeObject;
typedef PyTypeObject *PPyTypeObject;

struct PySliceObject;
typedef PySliceObject *PPySliceObject;

typedef void __fastcall (*AtExitProc)(void);

typedef PPyObject __cdecl (*PyCFunction)(PPyObject self, PPyObject args);

typedef PPyObject __cdecl (*unaryfunc)(PPyObject ob1);

typedef PPyObject __cdecl (*binaryfunc)(PPyObject ob1, PPyObject ob2);

typedef PPyObject __cdecl (*ternaryfunc)(PPyObject ob1, PPyObject ob2, PPyObject ob3);

typedef int __cdecl (*inquiry)(PPyObject ob1);

typedef int __cdecl (*coercion)(PPPyObject ob1, PPPyObject ob2);

typedef PPyObject __cdecl (*intargfunc)(PPyObject ob1, int i);

typedef PPyObject __cdecl (*intintargfunc)(PPyObject ob1, int i1, int i2);

typedef int __cdecl (*intobjargproc)(PPyObject ob1, int i, PPyObject ob2);

typedef int __cdecl (*intintobjargproc)(PPyObject ob1, int i1, int i2, PPyObject ob2);

typedef int __cdecl (*objobjargproc)(PPyObject ob1, PPyObject ob2, PPyObject ob3);

typedef void __cdecl (*pydestructor)(PPyObject ob);

typedef int __cdecl (*printfunc)(PPyObject ob, file &f, int i);

typedef PPyObject __cdecl (*getattrfunc)(PPyObject ob1, char * name);

typedef int __cdecl (*setattrfunc)(PPyObject ob1, char * name, PPyObject ob2);

typedef int __cdecl (*cmpfunc)(PPyObject ob1, PPyObject ob2);

typedef PPyObject __cdecl (*reprfunc)(PPyObject ob);

typedef int __cdecl (*hashfunc)(PPyObject ob);

typedef PPyObject __cdecl (*getattrofunc)(PPyObject ob1, PPyObject ob2);

typedef int __cdecl (*setattrofunc)(PPyObject ob1, PPyObject ob2, PPyObject ob3);

typedef int __cdecl (*getreadbufferproc)(PPyObject ob1, int i, void * ptr);

typedef int __cdecl (*getwritebufferproc)(PPyObject ob1, int i, void * ptr);

typedef int __cdecl (*getsegcountproc)(PPyObject ob1, int i);

typedef int __cdecl (*getcharbufferproc)(PPyObject ob1, int i, const char * pstr);

typedef int __cdecl (*objobjproc)(PPyObject ob1, PPyObject ob2);

typedef int __cdecl (*visitproc)(PPyObject ob1, void * ptr);

typedef int __cdecl (*traverseproc)(PPyObject ob1, visitproc proc, void * ptr);

typedef PPyObject __cdecl (*richcmpfunc)(PPyObject ob1, PPyObject ob2, int i);

typedef PPyObject __fastcall (*getiterfunc)(PPyObject ob1);

typedef PPyObject __fastcall (*iternextfunc)(PPyObject ob1);

typedef PPyObject __fastcall (*descrgetfunc)(PPyObject ob1, PPyObject ob2, PPyObject ob3);

typedef int __fastcall (*descrsetfunc)(PPyObject ob1, PPyObject ob2, PPyObject ob3);

typedef int __fastcall (*initproc)(PPyObject ob1, PPyObject ob2, PPyObject ob3);

typedef PPyObject __fastcall (*newfunc)(PPyTypeObject t, PPyObject ob1, PPyObject ob2);

typedef PPyObject __fastcall (*allocfunc)(PPyTypeObject t, int i);

#pragma pack(push, 1)
struct PyNumberMethods
{
	binaryfunc nb_add;
	binaryfunc nb_substract;
	binaryfunc nb_multiply;
	binaryfunc nb_divide;
	binaryfunc nb_remainder;
	binaryfunc nb_divmod;
	ternaryfunc nb_power;
	unaryfunc nb_negative;
	unaryfunc nb_positive;
	unaryfunc nb_absolute;
	inquiry nb_nonzero;
	unaryfunc nb_invert;
	binaryfunc nb_lshift;
	binaryfunc nb_rshift;
	binaryfunc nb_and;
	binaryfunc nb_xor;
	binaryfunc nb_or;
	coercion nb_coerce;
	unaryfunc nb_int;
	unaryfunc nb_long;
	unaryfunc nb_float;
	unaryfunc nb_oct;
	unaryfunc nb_hex;
	binaryfunc nb_inplace_add;
	binaryfunc nb_inplace_subtract;
	binaryfunc nb_inplace_multiply;
	binaryfunc nb_inplace_divide;
	binaryfunc nb_inplace_remainder;
	ternaryfunc nb_inplace_power;
	binaryfunc nb_inplace_lshift;
	binaryfunc nb_inplace_rshift;
	binaryfunc nb_inplace_and;
	binaryfunc nb_inplace_xor;
	binaryfunc nb_inplace_or;
	binaryfunc nb_floor_divide;
	binaryfunc nb_true_divide;
	binaryfunc nb_inplace_floor_divide;
	binaryfunc nb_inplace_true_divide;
} ;
#pragma pack(pop)

typedef PyNumberMethods *PPyNumberMethods;

#pragma pack(push, 1)
struct PySequenceMethods
{
	inquiry sq_length;
	binaryfunc sq_concat;
	intargfunc sq_repeat;
	intargfunc sq_item;
	intintargfunc sq_slice;
	intobjargproc sq_ass_item;
	intintobjargproc sq_ass_slice;
	objobjproc sq_contains;
	binaryfunc sq_inplace_concat;
	intargfunc sq_inplace_repeat;
} ;
#pragma pack(pop)

typedef PySequenceMethods *PPySequenceMethods;

#pragma pack(push, 1)
struct PyMappingMethods
{
	inquiry mp_length;
	binaryfunc mp_subscript;
	objobjargproc mp_ass_subscript;
} ;
#pragma pack(pop)

typedef PyMappingMethods *PPyMappingMethods;

#pragma pack(push, 1)
struct PyBufferProcs
{
	getreadbufferproc bf_getreadbuffer;
	getwritebufferproc bf_getwritebuffer;
	getsegcountproc bf_getsegcount;
	getcharbufferproc bf_getcharbuffer;
} ;
#pragma pack(pop)

typedef PyBufferProcs *PPyBufferProcs;

#pragma pack(push, 1)
struct Py_complex
{
	double real;
	double imag;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PyObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PyIntObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int ob_ival;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct _frozen
{
	char *name;
	Byte *code;
	int size;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PySliceObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyObject *start;
	PyObject *stop;
	PyObject *step;
} ;
#pragma pack(pop)

struct PyMethodDef;
typedef PyMethodDef *PPyMethodDef;

#pragma pack(push, 1)
struct PyMethodDef
{
	char *ml_name;
	PyCFunction ml_meth;
	int ml_flags;
	char *ml_doc;
} ;
#pragma pack(pop)

struct PyMemberDef;
typedef PyMemberDef *PPyMemberDef;

#pragma pack(push, 1)
struct PyMemberDef
{
	char *name;
	int _type;
	int offset;
	int flags;
	char *doc;
} ;
#pragma pack(pop)

typedef PPyObject __fastcall (*getter)(PPyObject ob, void * ptr);

typedef int __fastcall (*setter)(PPyObject ob1, PPyObject ob2, void * ptr);

struct PyGetSetDef;
typedef PyGetSetDef *PPyGetSetDef;

#pragma pack(push, 1)
struct PyGetSetDef
{
	char *name;
	getter get;
	setter _set;
	char *doc;
	void *closure;
} ;
#pragma pack(pop)

typedef PPyObject __fastcall (*wrapperfunc)(PPyObject self, PPyObject args, void * wrapped);

struct wrapperbase;
typedef wrapperbase *pwrapperbase;

#pragma pack(push, 1)
struct wrapperbase
{
	char *name;
	wrapperfunc wrapper;
	char *doc;
} ;
#pragma pack(pop)

struct PyDescrObject;
typedef PyDescrObject *PPyDescrObject;

#pragma pack(push, 1)
struct PyDescrObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyTypeObject *d_type;
	PyObject *d_name;
} ;
#pragma pack(pop)

struct PyMethodDescrObject;
typedef PyMethodDescrObject *PPyMethodDescrObject;

#pragma pack(push, 1)
struct PyMethodDescrObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyTypeObject *d_type;
	PyObject *d_name;
	PyMethodDef *d_method;
} ;
#pragma pack(pop)

struct PyMemberDescrObject;
typedef PyMemberDescrObject *PPyMemberDescrObject;

#pragma pack(push, 1)
struct PyMemberDescrObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyTypeObject *d_type;
	PyObject *d_name;
	PyMemberDef *d_member;
} ;
#pragma pack(pop)

struct PyGetSetDescrObject;
typedef PyGetSetDescrObject *PPyGetSetDescrObject;

#pragma pack(push, 1)
struct PyGetSetDescrObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyTypeObject *d_type;
	PyObject *d_name;
	PyGetSetDef *d_getset;
} ;
#pragma pack(pop)

struct PyWrapperDescrObject;
typedef PyWrapperDescrObject *PPyWrapperDescrObject;

#pragma pack(push, 1)
struct PyWrapperDescrObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyTypeObject *d_type;
	PyObject *d_name;
	wrapperbase *d_base;
	void *d_wrapped;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PyTypeObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int ob_size;
	char *tp_name;
	int tp_basicsize;
	int tp_itemsize;
	pydestructor tp_dealloc;
	printfunc tp_print;
	getattrfunc tp_getattr;
	setattrfunc tp_setattr;
	cmpfunc tp_compare;
	reprfunc tp_repr;
	PyNumberMethods *tp_as_number;
	PySequenceMethods *tp_as_sequence;
	PyMappingMethods *tp_as_mapping;
	hashfunc tp_hash;
	ternaryfunc tp_call;
	reprfunc tp_str;
	getattrofunc tp_getattro;
	setattrofunc tp_setattro;
	PyBufferProcs *tp_as_buffer;
	int tp_flags;
	char *tp_doc;
	traverseproc tp_traverse;
	inquiry tp_clear;
	richcmpfunc tp_richcompare;
	int tp_weaklistoffset;
	getiterfunc tp_iter;
	iternextfunc tp_iternext;
	PyMethodDef *tp_methods;
	PyMemberDef *tp_members;
	PyGetSetDef *tp_getset;
	PyTypeObject *tp_base;
	PyObject *tp_dict;
	descrgetfunc tp_descr_get;
	descrsetfunc tp_descr_set;
	int tp_dictoffset;
	initproc tp_init;
	allocfunc tp_alloc;
	newfunc tp_new;
	pydestructor tp_free;
	inquiry tp_is_gc;
	PyObject *tp_bases;
	PyObject *tp_mro;
	PyObject *tp_cache;
	PyObject *tp_subclasses;
	PyObject *tp_weaklist;
} ;
#pragma pack(pop)

struct PyMethodChain;
typedef PyMethodChain *PPyMethodChain;

#pragma pack(push, 1)
struct PyMethodChain
{
	PyMethodDef *methods;
	PyMethodChain *link;
} ;
#pragma pack(pop)

struct PyClassObject;
typedef PyClassObject *PPyClassObject;

#pragma pack(push, 1)
struct PyClassObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyObject *cl_bases;
	PyObject *cl_dict;
	PyObject *cl_name;
	PyObject *cl_getattr;
	PyObject *cl_setattr;
	PyObject *cl_delattr;
} ;
#pragma pack(pop)

struct PyInstanceObject;
typedef PyInstanceObject *PPyInstanceObject;

#pragma pack(push, 1)
struct PyInstanceObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyClassObject *in_class;
	PyObject *in_dict;
} ;
#pragma pack(pop)

struct PyMethodObject;
typedef PyMethodObject *PPyMethodObject;

#pragma pack(push, 1)
struct PyMethodObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyObject *im_func;
	PyObject *im_self;
	PyObject *im_class;
} ;
#pragma pack(pop)

struct PyCodeObject;
typedef PyCodeObject *PPyCodeObject;

#pragma pack(push, 1)
struct PyCodeObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int co_argcount;
	int co_nlocals;
	int co_stacksize;
	int co_flags;
	PyObject *co_code;
	PyObject *co_consts;
	PyObject *co_names;
	PyObject *co_varnames;
	PyObject *co_freevars;
	PyObject *co_cellvars;
	PyObject *co_filename;
	PyObject *co_name;
	int co_firstlineno;
	PyObject *co_lnotab;
} ;
#pragma pack(pop)

struct PyInterpreterState;
typedef PyInterpreterState *PPyInterpreterState;

struct PyThreadState;
typedef PyThreadState *PPyThreadState;

struct PyFrameObject;
typedef PyFrameObject *PPyFrameObject;

#pragma pack(push, 1)
struct PyInterpreterState
{
	PyInterpreterState *next;
	PyThreadState *tstate_head;
	PyObject *modules;
	PyObject *sysdict;
	PyObject *builtins;
	int checkinterval;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PyThreadState
{
	PyThreadState *next;
	PyInterpreterState *interp;
	PyFrameObject *frame;
	int recursion_depth;
	int ticker;
	int tracing;
	PyObject *sys_profilefunc;
	PyObject *sys_tracefunc;
	PyObject *curexc_type;
	PyObject *curexc_value;
	PyObject *curexc_traceback;
	PyObject *exc_type;
	PyObject *exc_value;
	PyObject *exc_traceback;
	PyObject *dict;
} ;
#pragma pack(pop)

struct PyTryBlock;
typedef PyTryBlock *PPyTryBlock;

#pragma pack(push, 1)
struct PyTryBlock
{
	int b_type;
	int b_handler;
	int b_level;
} ;
#pragma pack(pop)

typedef Shortint CO_MAXBLOCKS;

#pragma pack(push, 1)
struct PyFrameObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int ob_size;
	PyFrameObject *f_back;
	PyCodeObject *f_code;
	PyObject *f_builtins;
	PyObject *f_globals;
	PyObject *f_locals;
	PPyObject *f_valuestack;
	PPyObject *f_stacktop;
	PyObject *f_trace;
	PyObject *f_exc_type;
	PyObject *f_exc_value;
	PyObject *f_exc_traceback;
	PyThreadState *f_tstate;
	int f_lasti;
	int f_lineno;
	int f_restricted;
	int f_iblock;
	PyTryBlock f_blockstack[20];
	int f_nlocals;
	int f_ncells;
	int f_nfreevars;
	int f_stacksize;
	PyObject *f_localsplus[1];
} ;
#pragma pack(pop)

struct PyTraceBackObject;
typedef PyTraceBackObject *PPyTraceBackObject;

#pragma pack(push, 1)
struct PyTraceBackObject
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyTraceBackObject *tb_next;
	PyFrameObject *tb_frame;
	int tb_lasti;
	int tb_lineno;
} ;
#pragma pack(pop)

struct node;
typedef node *PNode;

#pragma pack(push, 1)
struct node
{
	short n_type;
	char *n_str;
	short n_lineno;
	short n_nchildren;
	node *n_child;
} ;
#pragma pack(pop)

struct PyWeakReference;
typedef PyWeakReference *PPyWeakReference;

#pragma pack(push, 1)
struct PyWeakReference
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	PyObject *wr_object;
	PyObject *wr_callback;
	int hash;
	PyWeakReference *wr_prev;
	PyWeakReference *wr_next;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct PyDateTime_Date
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int hashcode;
	Byte data[4];
} ;
#pragma pack(pop)

typedef PyDateTime_Date *PPyDateTime_Date;

#pragma pack(push, 1)
struct PyDateTime_DateTime
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int hashcode;
	Byte data[10];
} ;
#pragma pack(pop)

typedef PyDateTime_DateTime *PPyDateTime_DateTime;

#pragma pack(push, 1)
struct PyDateTime_DateTimeTZ
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int hashcode;
	Byte data[10];
	PyObject *tzinfo;
} ;
#pragma pack(pop)

typedef PyDateTime_DateTimeTZ *PPyDateTime_DateTimeTZ;

#pragma pack(push, 1)
struct PyDateTime_Time
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int hashcode;
	Byte data[6];
} ;
#pragma pack(pop)

typedef PyDateTime_Time *PPyDateTime_Time;

#pragma pack(push, 1)
struct PyDateTime_TimeTZ
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int hashcode;
	Byte data[6];
	PyObject *tzinfo;
} ;
#pragma pack(pop)

typedef PyDateTime_TimeTZ *PPyDateTime_TimeTZ;

#pragma pack(push, 1)
struct PyDateTime_Delta
{
	int ob_refcnt;
	PyTypeObject *ob_type;
	int hashcode;
	int days;
	int seconds;
	int microseconds;
} ;
#pragma pack(pop)

typedef PyDateTime_Delta *PPyDateTime_Delta;

#pragma pack(push, 1)
struct PyDateTime_TZInfo
{
	int ob_refcnt;
	PyTypeObject *ob_type;
} ;
#pragma pack(pop)

typedef PyDateTime_TZInfo *PPyDateTime_TZInfo;

class DELPHICLASS EDLLLoadError;
class PASCALIMPLEMENTATION EDLLLoadError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EDLLLoadError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EDLLLoadError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EDLLLoadError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EDLLLoadError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EDLLLoadError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EDLLLoadError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EDLLLoadError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EDLLLoadError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EDLLLoadError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EDLLImportError;
class PASCALIMPLEMENTATION EDLLImportError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	AnsiString WrongFunc;
	int ErrorCode;
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EDLLImportError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EDLLImportError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EDLLImportError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EDLLImportError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EDLLImportError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EDLLImportError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EDLLImportError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EDLLImportError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EDLLImportError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPythonError;
class PASCALIMPLEMENTATION EPythonError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	AnsiString EName;
	AnsiString EValue;
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPythonError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPythonError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPythonError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPythonError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPythonError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPythonError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPythonError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPythonError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPythonError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyExecError;
class PASCALIMPLEMENTATION EPyExecError : public EPythonError 
{
	typedef EPythonError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyExecError(const AnsiString Msg) : EPythonError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyExecError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPythonError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyExecError(int Ident)/* overload */ : EPythonError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyExecError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPythonError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyExecError(const AnsiString Msg, int AHelpContext) : EPythonError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyExecError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPythonError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyExecError(int Ident, int AHelpContext)/* overload */ : EPythonError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyExecError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPythonError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyExecError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyException;
class PASCALIMPLEMENTATION EPyException : public EPythonError 
{
	typedef EPythonError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyException(const AnsiString Msg) : EPythonError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyException(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPythonError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyException(int Ident)/* overload */ : EPythonError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyException(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPythonError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyException(const AnsiString Msg, int AHelpContext) : EPythonError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyException(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPythonError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyException(int Ident, int AHelpContext)/* overload */ : EPythonError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyException(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPythonError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyException(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyStandardError;
class PASCALIMPLEMENTATION EPyStandardError : public EPyException 
{
	typedef EPyException inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyStandardError(const AnsiString Msg) : EPyException(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyStandardError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyException(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyStandardError(int Ident)/* overload */ : EPyException(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyStandardError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyException(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyStandardError(const AnsiString Msg, int AHelpContext) : EPyException(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyStandardError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyException(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyStandardError(int Ident, int AHelpContext)/* overload */ : EPyException(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyStandardError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyException(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyStandardError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyArithmeticError;
class PASCALIMPLEMENTATION EPyArithmeticError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyArithmeticError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyArithmeticError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyArithmeticError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyArithmeticError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyArithmeticError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyArithmeticError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyArithmeticError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyArithmeticError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyArithmeticError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyLookupError;
class PASCALIMPLEMENTATION EPyLookupError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyLookupError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyLookupError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyLookupError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyLookupError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyLookupError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyLookupError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyLookupError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyLookupError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyLookupError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyAssertionError;
class PASCALIMPLEMENTATION EPyAssertionError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyAssertionError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyAssertionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyAssertionError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyAssertionError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyAssertionError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyAssertionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyAssertionError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyAssertionError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyAssertionError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyAttributeError;
class PASCALIMPLEMENTATION EPyAttributeError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyAttributeError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyAttributeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyAttributeError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyAttributeError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyAttributeError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyAttributeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyAttributeError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyAttributeError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyAttributeError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyEOFError;
class PASCALIMPLEMENTATION EPyEOFError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyEOFError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyEOFError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyEOFError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyEOFError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyEOFError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyEOFError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyEOFError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyEOFError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyEOFError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyFloatingPointError;
class PASCALIMPLEMENTATION EPyFloatingPointError : public EPyArithmeticError 
{
	typedef EPyArithmeticError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyFloatingPointError(const AnsiString Msg) : EPyArithmeticError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyFloatingPointError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyArithmeticError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyFloatingPointError(int Ident)/* overload */ : EPyArithmeticError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyFloatingPointError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyArithmeticError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyFloatingPointError(const AnsiString Msg, int AHelpContext) : EPyArithmeticError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyFloatingPointError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyArithmeticError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyFloatingPointError(int Ident, int AHelpContext)/* overload */ : EPyArithmeticError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyFloatingPointError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyArithmeticError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyFloatingPointError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyEnvironmentError;
class PASCALIMPLEMENTATION EPyEnvironmentError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyEnvironmentError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyEnvironmentError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyEnvironmentError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyEnvironmentError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyEnvironmentError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyEnvironmentError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyEnvironmentError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyEnvironmentError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyEnvironmentError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyIOError;
class PASCALIMPLEMENTATION EPyIOError : public EPyEnvironmentError 
{
	typedef EPyEnvironmentError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyIOError(const AnsiString Msg) : EPyEnvironmentError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyIOError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyEnvironmentError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyIOError(int Ident)/* overload */ : EPyEnvironmentError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyIOError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyEnvironmentError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyIOError(const AnsiString Msg, int AHelpContext) : EPyEnvironmentError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyIOError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyEnvironmentError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyIOError(int Ident, int AHelpContext)/* overload */ : EPyEnvironmentError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyIOError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyEnvironmentError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyIOError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyOSError;
class PASCALIMPLEMENTATION EPyOSError : public EPyEnvironmentError 
{
	typedef EPyEnvironmentError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyOSError(const AnsiString Msg) : EPyEnvironmentError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyOSError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyEnvironmentError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyOSError(int Ident)/* overload */ : EPyEnvironmentError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyOSError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyEnvironmentError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyOSError(const AnsiString Msg, int AHelpContext) : EPyEnvironmentError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyOSError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyEnvironmentError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyOSError(int Ident, int AHelpContext)/* overload */ : EPyEnvironmentError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyOSError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyEnvironmentError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyOSError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyImportError;
class PASCALIMPLEMENTATION EPyImportError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyImportError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyImportError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyImportError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyImportError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyImportError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyImportError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyImportError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyImportError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyImportError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyIndexError;
class PASCALIMPLEMENTATION EPyIndexError : public EPyLookupError 
{
	typedef EPyLookupError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyIndexError(const AnsiString Msg) : EPyLookupError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyIndexError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyLookupError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyIndexError(int Ident)/* overload */ : EPyLookupError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyIndexError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyLookupError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyIndexError(const AnsiString Msg, int AHelpContext) : EPyLookupError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyIndexError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyLookupError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyIndexError(int Ident, int AHelpContext)/* overload */ : EPyLookupError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyIndexError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyLookupError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyIndexError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyKeyError;
class PASCALIMPLEMENTATION EPyKeyError : public EPyLookupError 
{
	typedef EPyLookupError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyKeyError(const AnsiString Msg) : EPyLookupError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyKeyError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyLookupError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyKeyError(int Ident)/* overload */ : EPyLookupError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyKeyError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyLookupError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyKeyError(const AnsiString Msg, int AHelpContext) : EPyLookupError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyKeyError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyLookupError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyKeyError(int Ident, int AHelpContext)/* overload */ : EPyLookupError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyKeyError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyLookupError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyKeyError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyKeyboardInterrupt;
class PASCALIMPLEMENTATION EPyKeyboardInterrupt : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyKeyboardInterrupt(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyKeyboardInterrupt(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyKeyboardInterrupt(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyKeyboardInterrupt(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyKeyboardInterrupt(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyKeyboardInterrupt(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyKeyboardInterrupt(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyKeyboardInterrupt(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyKeyboardInterrupt(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyMemoryError;
class PASCALIMPLEMENTATION EPyMemoryError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyMemoryError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyMemoryError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyMemoryError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyMemoryError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyMemoryError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyMemoryError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyMemoryError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyMemoryError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyMemoryError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyNameError;
class PASCALIMPLEMENTATION EPyNameError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyNameError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyNameError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyNameError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyNameError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyNameError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyNameError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyNameError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyNameError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyNameError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyOverflowError;
class PASCALIMPLEMENTATION EPyOverflowError : public EPyArithmeticError 
{
	typedef EPyArithmeticError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyOverflowError(const AnsiString Msg) : EPyArithmeticError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyOverflowError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyArithmeticError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyOverflowError(int Ident)/* overload */ : EPyArithmeticError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyOverflowError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyArithmeticError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyOverflowError(const AnsiString Msg, int AHelpContext) : EPyArithmeticError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyOverflowError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyArithmeticError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyOverflowError(int Ident, int AHelpContext)/* overload */ : EPyArithmeticError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyOverflowError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyArithmeticError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyOverflowError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyRuntimeError;
class PASCALIMPLEMENTATION EPyRuntimeError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyRuntimeError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyRuntimeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyRuntimeError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyRuntimeError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyRuntimeError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyRuntimeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyRuntimeError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyRuntimeError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyRuntimeError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyNotImplementedError;
class PASCALIMPLEMENTATION EPyNotImplementedError : public EPyRuntimeError 
{
	typedef EPyRuntimeError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyNotImplementedError(const AnsiString Msg) : EPyRuntimeError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyNotImplementedError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyRuntimeError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyNotImplementedError(int Ident)/* overload */ : EPyRuntimeError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyNotImplementedError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyRuntimeError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyNotImplementedError(const AnsiString Msg, int AHelpContext) : EPyRuntimeError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyNotImplementedError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyRuntimeError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyNotImplementedError(int Ident, int AHelpContext)/* overload */ : EPyRuntimeError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyNotImplementedError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyRuntimeError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyNotImplementedError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPySyntaxError;
class PASCALIMPLEMENTATION EPySyntaxError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	AnsiString EFileName;
	AnsiString ELineStr;
	int ELineNumber;
	int EOffset;
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPySyntaxError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPySyntaxError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPySyntaxError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPySyntaxError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPySyntaxError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPySyntaxError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPySyntaxError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPySyntaxError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPySyntaxError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyIndentationError;
class PASCALIMPLEMENTATION EPyIndentationError : public EPySyntaxError 
{
	typedef EPySyntaxError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyIndentationError(const AnsiString Msg) : EPySyntaxError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyIndentationError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPySyntaxError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyIndentationError(int Ident)/* overload */ : EPySyntaxError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyIndentationError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPySyntaxError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyIndentationError(const AnsiString Msg, int AHelpContext) : EPySyntaxError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyIndentationError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPySyntaxError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyIndentationError(int Ident, int AHelpContext)/* overload */ : EPySyntaxError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyIndentationError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPySyntaxError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyIndentationError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyTabError;
class PASCALIMPLEMENTATION EPyTabError : public EPyIndentationError 
{
	typedef EPyIndentationError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyTabError(const AnsiString Msg) : EPyIndentationError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyTabError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyIndentationError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyTabError(int Ident)/* overload */ : EPyIndentationError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyTabError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyIndentationError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyTabError(const AnsiString Msg, int AHelpContext) : EPyIndentationError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyTabError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyIndentationError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyTabError(int Ident, int AHelpContext)/* overload */ : EPyIndentationError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyTabError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyIndentationError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyTabError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPySystemError;
class PASCALIMPLEMENTATION EPySystemError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPySystemError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPySystemError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPySystemError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPySystemError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPySystemError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPySystemError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPySystemError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPySystemError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPySystemError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPySystemExit;
class PASCALIMPLEMENTATION EPySystemExit : public EPyException 
{
	typedef EPyException inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPySystemExit(const AnsiString Msg) : EPyException(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPySystemExit(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyException(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPySystemExit(int Ident)/* overload */ : EPyException(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPySystemExit(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyException(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPySystemExit(const AnsiString Msg, int AHelpContext) : EPyException(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPySystemExit(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyException(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPySystemExit(int Ident, int AHelpContext)/* overload */ : EPyException(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPySystemExit(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyException(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPySystemExit(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyTypeError;
class PASCALIMPLEMENTATION EPyTypeError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyTypeError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyTypeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyTypeError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyTypeError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyTypeError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyTypeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyTypeError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyTypeError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyTypeError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyUnboundLocalError;
class PASCALIMPLEMENTATION EPyUnboundLocalError : public EPyNameError 
{
	typedef EPyNameError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyUnboundLocalError(const AnsiString Msg) : EPyNameError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyUnboundLocalError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyNameError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyUnboundLocalError(int Ident)/* overload */ : EPyNameError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyUnboundLocalError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyNameError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyUnboundLocalError(const AnsiString Msg, int AHelpContext) : EPyNameError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyUnboundLocalError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyNameError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyUnboundLocalError(int Ident, int AHelpContext)/* overload */ : EPyNameError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyUnboundLocalError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyNameError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyUnboundLocalError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyValueError;
class PASCALIMPLEMENTATION EPyValueError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyValueError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyValueError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyValueError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyValueError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyValueError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyValueError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyValueError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyValueError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyValueError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyUnicodeError;
class PASCALIMPLEMENTATION EPyUnicodeError : public EPyValueError 
{
	typedef EPyValueError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyUnicodeError(const AnsiString Msg) : EPyValueError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyUnicodeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyValueError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyUnicodeError(int Ident)/* overload */ : EPyValueError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyUnicodeError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyValueError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyUnicodeError(const AnsiString Msg, int AHelpContext) : EPyValueError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyUnicodeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyValueError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyUnicodeError(int Ident, int AHelpContext)/* overload */ : EPyValueError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyUnicodeError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyValueError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyUnicodeError(void) { }
	#pragma option pop
	
};


class DELPHICLASS UnicodeEncodeError;
class PASCALIMPLEMENTATION UnicodeEncodeError : public EPyUnicodeError 
{
	typedef EPyUnicodeError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall UnicodeEncodeError(const AnsiString Msg) : EPyUnicodeError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall UnicodeEncodeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyUnicodeError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall UnicodeEncodeError(int Ident)/* overload */ : EPyUnicodeError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall UnicodeEncodeError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyUnicodeError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall UnicodeEncodeError(const AnsiString Msg, int AHelpContext) : EPyUnicodeError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall UnicodeEncodeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyUnicodeError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall UnicodeEncodeError(int Ident, int AHelpContext)/* overload */ : EPyUnicodeError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall UnicodeEncodeError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyUnicodeError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~UnicodeEncodeError(void) { }
	#pragma option pop
	
};


class DELPHICLASS UnicodeDecodeError;
class PASCALIMPLEMENTATION UnicodeDecodeError : public EPyUnicodeError 
{
	typedef EPyUnicodeError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall UnicodeDecodeError(const AnsiString Msg) : EPyUnicodeError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall UnicodeDecodeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyUnicodeError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall UnicodeDecodeError(int Ident)/* overload */ : EPyUnicodeError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall UnicodeDecodeError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyUnicodeError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall UnicodeDecodeError(const AnsiString Msg, int AHelpContext) : EPyUnicodeError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall UnicodeDecodeError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyUnicodeError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall UnicodeDecodeError(int Ident, int AHelpContext)/* overload */ : EPyUnicodeError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall UnicodeDecodeError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyUnicodeError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~UnicodeDecodeError(void) { }
	#pragma option pop
	
};


class DELPHICLASS UnicodeTranslateError;
class PASCALIMPLEMENTATION UnicodeTranslateError : public EPyUnicodeError 
{
	typedef EPyUnicodeError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall UnicodeTranslateError(const AnsiString Msg) : EPyUnicodeError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall UnicodeTranslateError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyUnicodeError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall UnicodeTranslateError(int Ident)/* overload */ : EPyUnicodeError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall UnicodeTranslateError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyUnicodeError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall UnicodeTranslateError(const AnsiString Msg, int AHelpContext) : EPyUnicodeError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall UnicodeTranslateError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyUnicodeError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall UnicodeTranslateError(int Ident, int AHelpContext)/* overload */ : EPyUnicodeError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall UnicodeTranslateError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyUnicodeError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~UnicodeTranslateError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyZeroDivisionError;
class PASCALIMPLEMENTATION EPyZeroDivisionError : public EPyArithmeticError 
{
	typedef EPyArithmeticError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyZeroDivisionError(const AnsiString Msg) : EPyArithmeticError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyZeroDivisionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyArithmeticError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyZeroDivisionError(int Ident)/* overload */ : EPyArithmeticError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyZeroDivisionError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyArithmeticError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyZeroDivisionError(const AnsiString Msg, int AHelpContext) : EPyArithmeticError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyZeroDivisionError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyArithmeticError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyZeroDivisionError(int Ident, int AHelpContext)/* overload */ : EPyArithmeticError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyZeroDivisionError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyArithmeticError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyZeroDivisionError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyStopIteration;
class PASCALIMPLEMENTATION EPyStopIteration : public EPyException 
{
	typedef EPyException inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyStopIteration(const AnsiString Msg) : EPyException(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyStopIteration(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyException(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyStopIteration(int Ident)/* overload */ : EPyException(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyStopIteration(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyException(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyStopIteration(const AnsiString Msg, int AHelpContext) : EPyException(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyStopIteration(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyException(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyStopIteration(int Ident, int AHelpContext)/* overload */ : EPyException(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyStopIteration(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyException(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyStopIteration(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyWarning;
class PASCALIMPLEMENTATION EPyWarning : public EPyException 
{
	typedef EPyException inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyWarning(const AnsiString Msg) : EPyException(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyException(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyWarning(int Ident)/* overload */ : EPyException(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyException(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyWarning(const AnsiString Msg, int AHelpContext) : EPyException(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyException(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyWarning(int Ident, int AHelpContext)/* overload */ : EPyException(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyException(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyUserWarning;
class PASCALIMPLEMENTATION EPyUserWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyUserWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyUserWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyUserWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyUserWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyUserWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyUserWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyUserWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyUserWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyUserWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyDeprecationWarning;
class PASCALIMPLEMENTATION EPyDeprecationWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyDeprecationWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyDeprecationWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyDeprecationWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyDeprecationWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyDeprecationWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyDeprecationWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyDeprecationWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyDeprecationWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyDeprecationWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS PendingDeprecationWarning;
class PASCALIMPLEMENTATION PendingDeprecationWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall PendingDeprecationWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall PendingDeprecationWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall PendingDeprecationWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall PendingDeprecationWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall PendingDeprecationWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall PendingDeprecationWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall PendingDeprecationWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall PendingDeprecationWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~PendingDeprecationWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS FutureWarning;
class PASCALIMPLEMENTATION FutureWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall FutureWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall FutureWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall FutureWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall FutureWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall FutureWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall FutureWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall FutureWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall FutureWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~FutureWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPySyntaxWarning;
class PASCALIMPLEMENTATION EPySyntaxWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPySyntaxWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPySyntaxWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPySyntaxWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPySyntaxWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPySyntaxWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPySyntaxWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPySyntaxWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPySyntaxWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPySyntaxWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyOverflowWarning;
class PASCALIMPLEMENTATION EPyOverflowWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyOverflowWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyOverflowWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyOverflowWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyOverflowWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyOverflowWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyOverflowWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyOverflowWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyOverflowWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyOverflowWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyRuntimeWarning;
class PASCALIMPLEMENTATION EPyRuntimeWarning : public EPyWarning 
{
	typedef EPyWarning inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyRuntimeWarning(const AnsiString Msg) : EPyWarning(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyRuntimeWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyWarning(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyRuntimeWarning(int Ident)/* overload */ : EPyWarning(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyRuntimeWarning(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyWarning(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyRuntimeWarning(const AnsiString Msg, int AHelpContext) : EPyWarning(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyRuntimeWarning(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyWarning(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyRuntimeWarning(int Ident, int AHelpContext)/* overload */ : EPyWarning(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyRuntimeWarning(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyWarning(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyRuntimeWarning(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyReferenceError;
class PASCALIMPLEMENTATION EPyReferenceError : public EPyStandardError 
{
	typedef EPyStandardError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyReferenceError(const AnsiString Msg) : EPyStandardError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyReferenceError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyStandardError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyReferenceError(int Ident)/* overload */ : EPyStandardError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyReferenceError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyStandardError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyReferenceError(const AnsiString Msg, int AHelpContext) : EPyStandardError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyReferenceError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyStandardError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyReferenceError(int Ident, int AHelpContext)/* overload */ : EPyStandardError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyReferenceError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyStandardError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyReferenceError(void) { }
	#pragma option pop
	
};


class DELPHICLASS EPyWindowsError;
class PASCALIMPLEMENTATION EPyWindowsError : public EPyOSError 
{
	typedef EPyOSError inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EPyWindowsError(const AnsiString Msg) : EPyOSError(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EPyWindowsError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : EPyOSError(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EPyWindowsError(int Ident)/* overload */ : EPyOSError(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EPyWindowsError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : EPyOSError(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EPyWindowsError(const AnsiString Msg, int AHelpContext) : EPyOSError(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EPyWindowsError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : EPyOSError(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EPyWindowsError(int Ident, int AHelpContext)/* overload */ : EPyOSError(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EPyWindowsError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : EPyOSError(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EPyWindowsError(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TSendDataEvent)(System::TObject* Sender, const AnsiString Data);

typedef void __fastcall (__closure *TReceiveDataEvent)(System::TObject* Sender, AnsiString &Data);

class DELPHICLASS TPythonInputOutput;
class PASCALIMPLEMENTATION TPythonInputOutput : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	int FMaxLines;
	AnsiString FLine_Buffer;
	Classes::TStringList* FLinesPerThread;
	Syncobjs::TCriticalSection* FLock;
	Classes::TStringList* FQueue;
	bool FDelayWrites;
	int FMaxLineLength;
	TSendDataEvent FOnSendData;
	TReceiveDataEvent FOnReceiveData;
	void __fastcall Lock(void);
	void __fastcall Unlock(void);
	void __fastcall AddWrite(const AnsiString str);
	virtual void __fastcall SendData(const AnsiString Data);
	virtual AnsiString __fastcall ReceiveData();
	virtual void __fastcall AddPendingWrite(void);
	int __fastcall GetCurrentThreadSlotIdx(void);
	AnsiString __fastcall GetCurrentThreadLine();
	void __fastcall UpdateCurrentThreadLine(void);
	
public:
	__fastcall virtual TPythonInputOutput(Classes::TComponent* AOwner);
	__fastcall virtual ~TPythonInputOutput(void);
	void __fastcall Write(const AnsiString str);
	void __fastcall WriteLine(const AnsiString str);
	
__published:
	__property int MaxLines = {read=FMaxLines, write=FMaxLines, default=1000};
	__property int MaxLineLength = {read=FMaxLineLength, write=FMaxLineLength, default=256};
	__property bool DelayWrites = {read=FDelayWrites, write=FDelayWrites, default=0};
	__property TSendDataEvent OnSendData = {read=FOnSendData, write=FOnSendData};
	__property TReceiveDataEvent OnReceiveData = {read=FOnReceiveData, write=FOnReceiveData};
};


class DELPHICLASS TDynamicDll;
class PASCALIMPLEMENTATION TDynamicDll : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool __fastcall IsAPIVersionStored(void);
	bool __fastcall IsDllNameStored(void);
	bool __fastcall IsRegVersionStored(void);
	
protected:
	AnsiString FDllName;
	AnsiString FDllPath;
	int FAPIVersion;
	AnsiString FRegVersion;
	bool FAutoLoad;
	bool FAutoUnload;
	bool FFatalMsgDlg;
	bool FFatalAbort;
	unsigned FDLLHandle;
	bool FUseLastKnownVersion;
	Classes::TNotifyEvent FOnBeforeLoad;
	Classes::TNotifyEvent FOnAfterLoad;
	Classes::TNotifyEvent FOnBeforeUnload;
	void * __fastcall Import(const AnsiString funcname);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall BeforeLoad(void);
	virtual void __fastcall AfterLoad(void);
	virtual void __fastcall BeforeUnload(void);
	virtual AnsiString __fastcall GetQuitMessage();
	virtual void __fastcall DoOpenDll(const AnsiString aDllName);
	AnsiString __fastcall GetDllPath();
	
public:
	__fastcall virtual TDynamicDll(Classes::TComponent* AOwner);
	__fastcall virtual ~TDynamicDll(void);
	void __fastcall OpenDll(const AnsiString aDllName);
	bool __fastcall IsHandleValid(void);
	void __fastcall LoadDll(void);
	void __fastcall UnloadDll(void);
	void __fastcall Quit(void);
	
__published:
	__property bool AutoLoad = {read=FAutoLoad, write=FAutoLoad, default=1};
	__property bool AutoUnload = {read=FAutoUnload, write=FAutoUnload, default=1};
	__property AnsiString DllName = {read=FDllName, write=FDllName, stored=IsDllNameStored};
	__property AnsiString DllPath = {read=FDllPath, write=FDllPath};
	__property int APIVersion = {read=FAPIVersion, write=FAPIVersion, stored=IsAPIVersionStored, nodefault};
	__property AnsiString RegVersion = {read=FRegVersion, write=FRegVersion, stored=IsRegVersionStored};
	__property bool FatalAbort = {read=FFatalAbort, write=FFatalAbort, default=1};
	__property bool FatalMsgDlg = {read=FFatalMsgDlg, write=FFatalMsgDlg, default=1};
	__property bool UseLastKnownVersion = {read=FUseLastKnownVersion, write=FUseLastKnownVersion, default=1};
	__property Classes::TNotifyEvent OnAfterLoad = {read=FOnAfterLoad, write=FOnAfterLoad};
	__property Classes::TNotifyEvent OnBeforeLoad = {read=FOnBeforeLoad, write=FOnBeforeLoad};
	__property Classes::TNotifyEvent OnBeforeUnload = {read=FOnBeforeUnload, write=FOnBeforeUnload};
};


class DELPHICLASS TPythonInterface;
class PASCALIMPLEMENTATION TPythonInterface : public TDynamicDll 
{
	typedef TDynamicDll inherited;
	
private:
	int __cdecl (*DLL_PyArg_Parse)(PPyObject args, char * format);
	int __cdecl (*DLL_PyArg_ParseTuple)(PPyObject args, char * format);
	PPyObject __cdecl (*DLL_Py_BuildValue)(char * format);
	int __cdecl (*DLL_PyCode_Addr2Line)(PPyCodeObject co, int addrq);
	char * __cdecl (*DLL_Py_GetBuildInfo)(void);
	
protected:
	bool FInitialized;
	bool FFinalizing;
	bool __fastcall GetInitialized(void);
	virtual void __fastcall AfterLoad(void);
	virtual AnsiString __fastcall GetQuitMessage();
	void __fastcall CheckPython(void);
	AnsiString __fastcall GetUnicodeTypeSuffix();
	
public:
	int *Py_DebugFlag;
	int *Py_VerboseFlag;
	int *Py_InteractiveFlag;
	int *Py_OptimizeFlag;
	int *Py_NoSiteFlag;
	int *Py_UseClassExceptionsFlag;
	int *Py_FrozenFlag;
	int *Py_TabcheckFlag;
	int *Py_UnicodeFlag;
	int *Py_IgnoreEnvironmentFlag;
	int *Py_DivisionWarningFlag;
	P_frozen *PyImport_FrozenModules;
	PyObject *Py_None;
	PyObject *Py_Ellipsis;
	PyIntObject *Py_False;
	PyIntObject *Py_True;
	PyObject *Py_NotImplemented;
	PPyObject *PyExc_AttributeError;
	PPyObject *PyExc_EOFError;
	PPyObject *PyExc_IOError;
	PPyObject *PyExc_ImportError;
	PPyObject *PyExc_IndexError;
	PPyObject *PyExc_KeyError;
	PPyObject *PyExc_KeyboardInterrupt;
	PPyObject *PyExc_MemoryError;
	PPyObject *PyExc_NameError;
	PPyObject *PyExc_OverflowError;
	PPyObject *PyExc_RuntimeError;
	PPyObject *PyExc_SyntaxError;
	PPyObject *PyExc_SystemError;
	PPyObject *PyExc_SystemExit;
	PPyObject *PyExc_TypeError;
	PPyObject *PyExc_ValueError;
	PPyObject *PyExc_ZeroDivisionError;
	PPyObject *PyExc_ArithmeticError;
	PPyObject *PyExc_Exception;
	PPyObject *PyExc_FloatingPointError;
	PPyObject *PyExc_LookupError;
	PPyObject *PyExc_StandardError;
	PPyObject *PyExc_AssertionError;
	PPyObject *PyExc_EnvironmentError;
	PPyObject *PyExc_IndentationError;
	PPyObject *PyExc_MemoryErrorInst;
	PPyObject *PyExc_NotImplementedError;
	PPyObject *PyExc_OSError;
	PPyObject *PyExc_TabError;
	PPyObject *PyExc_UnboundLocalError;
	PPyObject *PyExc_UnicodeError;
	PPyObject *PyExc_WindowsError;
	PPyObject *PyExc_Warning;
	PPyObject *PyExc_DeprecationWarning;
	PPyObject *PyExc_RuntimeWarning;
	PPyObject *PyExc_SyntaxWarning;
	PPyObject *PyExc_UserWarning;
	PPyObject *PyExc_OverflowWarning;
	PPyObject *PyExc_ReferenceError;
	PPyObject *PyExc_StopIteration;
	PPyObject *PyExc_FutureWarning;
	PPyObject *PyExc_PendingDeprecationWarning;
	PPyObject *PyExc_UnicodeDecodeError;
	PPyObject *PyExc_UnicodeEncodeError;
	PPyObject *PyExc_UnicodeTranslateError;
	PyTypeObject *PyType_Type;
	PyTypeObject *PyCFunction_Type;
	PyTypeObject *PyCObject_Type;
	PyTypeObject *PyClass_Type;
	PyTypeObject *PyCode_Type;
	PyTypeObject *PyComplex_Type;
	PyTypeObject *PyDict_Type;
	PyTypeObject *PyFile_Type;
	PyTypeObject *PyFloat_Type;
	PyTypeObject *PyFrame_Type;
	PyTypeObject *PyFunction_Type;
	PyTypeObject *PyInstance_Type;
	PyTypeObject *PyInt_Type;
	PyTypeObject *PyList_Type;
	PyTypeObject *PyLong_Type;
	PyTypeObject *PyMethod_Type;
	PyTypeObject *PyModule_Type;
	PyTypeObject *PyObject_Type;
	PyTypeObject *PyRange_Type;
	PyTypeObject *PySlice_Type;
	PyTypeObject *PyString_Type;
	PyTypeObject *PyTuple_Type;
	PyTypeObject *PyBaseObject_Type;
	PyTypeObject *PyBuffer_Type;
	PyTypeObject *PyCallIter_Type;
	PyTypeObject *PyCell_Type;
	PyTypeObject *PyClassMethod_Type;
	PyTypeObject *PyProperty_Type;
	PyTypeObject *PySeqIter_Type;
	PyTypeObject *PyStaticMethod_Type;
	PyTypeObject *PySuper_Type;
	PyTypeObject *PySymtableEntry_Type;
	PyTypeObject *PyTraceBack_Type;
	PyTypeObject *PyUnicode_Type;
	PyTypeObject *PyWrapperDescr_Type;
	PyTypeObject *_PyWeakref_RefType;
	PyTypeObject *_PyWeakref_ProxyType;
	PyTypeObject *_PyWeakref_CallableProxyType;
	PyTypeObject *PyBaseString_Type;
	PyTypeObject *PyBool_Type;
	PyTypeObject *PyEnum_Type;
	PPyObject __cdecl (*PyComplex_FromCComplex)(const Py_complex c);
	PPyObject __cdecl (*PyComplex_FromDoubles)(double realv, double imag);
	double __cdecl (*PyComplex_RealAsDouble)(PPyObject op);
	double __cdecl (*PyComplex_ImagAsDouble)(PPyObject op);
	Py_complex __cdecl (*PyComplex_AsCComplex)(PPyObject op);
	void * __cdecl (*PyCFunction_GetFunction)(PPyObject ob);
	PPyObject __cdecl (*PyCFunction_GetSelf)(PPyObject ob);
	int __cdecl (*PyCallable_Check)(PPyObject ob);
	PPyObject __cdecl (*PyCObject_FromVoidPtr)(void * cobj, void * destruct);
	void * __cdecl (*PyCObject_AsVoidPtr)(PPyObject ob);
	PPyObject __cdecl (*PyClass_New)(PPyObject ob1, PPyObject ob2, PPyObject ob3);
	int __cdecl (*PyClass_IsSubclass)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*Py_InitModule4)(char * name, PPyMethodDef methods, char * doc, PPyObject passthrough, int Api_Version);
	int __cdecl (*PyErr_BadArgument)(void);
	void __cdecl (*PyErr_BadInternalCall)(void);
	int __cdecl (*PyErr_CheckSignals)(void);
	void __cdecl (*PyErr_Clear)(void);
	void __cdecl (*PyErr_Fetch)(PPPyObject errtype, PPPyObject errvalue, PPPyObject errtraceback);
	PPyObject __cdecl (*PyErr_NoMemory)(void);
	PPyObject __cdecl (*PyErr_Occurred)(void);
	void __cdecl (*PyErr_Print)(void);
	void __cdecl (*PyErr_Restore)(PPyObject errtype, PPyObject errvalue, PPyObject errtraceback);
	PPyObject __cdecl (*PyErr_SetFromErrno)(PPyObject ob);
	void __cdecl (*PyErr_SetNone)(PPyObject value);
	void __cdecl (*PyErr_SetObject)(PPyObject ob1, PPyObject ob2);
	void __cdecl (*PyErr_SetString)(PPyObject ErrorObject, char * text);
	PPyObject __cdecl (*PyImport_GetModuleDict)(void);
	PPyObject __cdecl (*PyInt_FromLong)(int x);
	void __cdecl (*Py_Initialize)(void);
	void __cdecl (*Py_Exit)(int RetVal);
	PPyObject __cdecl (*PyEval_GetBuiltins)(void);
	PPyObject __cdecl (*PyDict_GetItem)(PPyObject mp, PPyObject key);
	int __cdecl (*PyDict_SetItem)(PPyObject mp, PPyObject key, PPyObject item);
	int __cdecl (*PyDict_DelItem)(PPyObject mp, PPyObject key);
	void __cdecl (*PyDict_Clear)(PPyObject mp);
	int __cdecl (*PyDict_Next)(PPyObject mp, PInt pos, PPPyObject key, PPPyObject value);
	PPyObject __cdecl (*PyDict_Keys)(PPyObject mp);
	PPyObject __cdecl (*PyDict_Values)(PPyObject mp);
	PPyObject __cdecl (*PyDict_Items)(PPyObject mp);
	int __cdecl (*PyDict_Size)(PPyObject mp);
	int __cdecl (*PyDict_DelItemString)(PPyObject dp, char * key);
	PPyObject __cdecl (*PyDict_New)(void);
	PPyObject __cdecl (*PyDict_GetItemString)(PPyObject dp, char * key);
	int __cdecl (*PyDict_SetItemString)(PPyObject dp, char * key, PPyObject item);
	PPyObject __cdecl (*PyDictProxy_New)(PPyObject obj);
	PPyObject __cdecl (*PyModule_GetDict)(PPyObject module);
	PPyObject __cdecl (*PyObject_Str)(PPyObject v);
	PPyObject __cdecl (*PyRun_String)(char * str, int start, PPyObject globals, PPyObject locals);
	int __cdecl (*PyRun_SimpleString)(char * str);
	char * __cdecl (*PyString_AsString)(PPyObject ob);
	PPyObject __cdecl (*PyString_FromString)(char * str);
	void __cdecl (*PySys_SetArgv)(int argc, PPChar argv);
	PPyObject __cdecl (*PyCFunction_New)(PPyMethodDef md, PPyObject ob);
	PPyObject __cdecl (*PyEval_CallObject)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyEval_CallObjectWithKeywords)(PPyObject ob1, PPyObject ob2, PPyObject ob3);
	PPyObject __cdecl (*PyEval_GetFrame)(void);
	PPyObject __cdecl (*PyEval_GetGlobals)(void);
	PPyObject __cdecl (*PyEval_GetLocals)(void);
	int __cdecl (*PyEval_GetRestricted)(void);
	void __cdecl (*PyEval_InitThreads)(void);
	void __cdecl (*PyEval_RestoreThread)(PPyThreadState tstate);
	PPyThreadState __cdecl (*PyEval_SaveThread)(void);
	PPyObject __cdecl (*PyFile_FromString)(char * pc1, char * pc2);
	PPyObject __cdecl (*PyFile_GetLine)(PPyObject ob, int i);
	PPyObject __cdecl (*PyFile_Name)(PPyObject ob);
	void __cdecl (*PyFile_SetBufSize)(PPyObject ob, int i);
	int __cdecl (*PyFile_SoftSpace)(PPyObject ob, int i);
	int __cdecl (*PyFile_WriteObject)(PPyObject ob1, PPyObject ob2, int i);
	void __cdecl (*PyFile_WriteString)(char * s, PPyObject ob);
	double __cdecl (*PyFloat_AsDouble)(PPyObject ob);
	PPyObject __cdecl (*PyFloat_FromDouble)(double db);
	PPyObject __cdecl (*PyFunction_GetCode)(PPyObject ob);
	PPyObject __cdecl (*PyFunction_GetGlobals)(PPyObject ob);
	PPyObject __cdecl (*PyFunction_New)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyImport_AddModule)(char * name);
	void __cdecl (*PyImport_Cleanup)(void);
	int __cdecl (*PyImport_GetMagicNumber)(void);
	int __cdecl (*PyImport_ImportFrozenModule)(char * key);
	PPyObject __cdecl (*PyImport_ImportModule)(char * name);
	PPyObject __cdecl (*PyImport_Import)(PPyObject name);
	PPyObject __cdecl (*PyImport_ReloadModule)(PPyObject ob);
	PPyObject __cdecl (*PyInstance_New)(PPyObject obClass, PPyObject obArg, PPyObject obKW);
	int __cdecl (*PyInt_AsLong)(PPyObject ob);
	int __cdecl (*PyList_Append)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyList_AsTuple)(PPyObject ob);
	PPyObject __cdecl (*PyList_GetItem)(PPyObject ob, int i);
	PPyObject __cdecl (*PyList_GetSlice)(PPyObject ob, int i1, int i2);
	int __cdecl (*PyList_Insert)(PPyObject dp, int idx, PPyObject item);
	PPyObject __cdecl (*PyList_New)(int size);
	int __cdecl (*PyList_Reverse)(PPyObject ob);
	int __cdecl (*PyList_SetItem)(PPyObject dp, int idx, PPyObject item);
	int __cdecl (*PyList_SetSlice)(PPyObject ob, int i1, int i2, PPyObject ob2);
	int __cdecl (*PyList_Size)(PPyObject ob);
	int __cdecl (*PyList_Sort)(PPyObject ob);
	double __cdecl (*PyLong_AsDouble)(PPyObject ob);
	int __cdecl (*PyLong_AsLong)(PPyObject ob);
	PPyObject __cdecl (*PyLong_FromDouble)(double db);
	PPyObject __cdecl (*PyLong_FromLong)(int l);
	PPyObject __cdecl (*PyLong_FromString)(char * pc, char * &ppc, int i);
	PPyObject __cdecl (*PyLong_FromUnsignedLong)(unsigned val);
	unsigned __cdecl (*PyLong_AsUnsignedLong)(PPyObject ob);
	PPyObject __cdecl (*PyLong_FromUnicode)(PPyObject ob, int a, int b);
	PPyObject __cdecl (*PyLong_FromLongLong)(__int64 val);
	__int64 __cdecl (*PyLong_AsLongLong)(PPyObject ob);
	int __cdecl (*PyMapping_Check)(PPyObject ob);
	PPyObject __cdecl (*PyMapping_GetItemString)(PPyObject ob, char * key);
	int __cdecl (*PyMapping_HasKey)(PPyObject ob, PPyObject key);
	int __cdecl (*PyMapping_HasKeyString)(PPyObject ob, char * key);
	int __cdecl (*PyMapping_Length)(PPyObject ob);
	int __cdecl (*PyMapping_SetItemString)(PPyObject ob, char * key, PPyObject value);
	PPyObject __cdecl (*PyMethod_Class)(PPyObject ob);
	PPyObject __cdecl (*PyMethod_Function)(PPyObject ob);
	PPyObject __cdecl (*PyMethod_New)(PPyObject ob1, PPyObject ob2, PPyObject ob3);
	PPyObject __cdecl (*PyMethod_Self)(PPyObject ob);
	char * __cdecl (*PyModule_GetName)(PPyObject ob);
	PPyObject __cdecl (*PyModule_New)(char * key);
	PPyObject __cdecl (*PyNumber_Absolute)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Add)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_And)(PPyObject ob1, PPyObject ob2);
	int __cdecl (*PyNumber_Check)(PPyObject ob);
	int __cdecl (*PyNumber_Coerce)(PPyObject &ob1, PPyObject &ob2);
	PPyObject __cdecl (*PyNumber_Divide)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_FloorDivide)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_TrueDivide)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Divmod)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Float)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Int)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Invert)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Long)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Lshift)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Multiply)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Negative)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Or)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Positive)(PPyObject ob);
	PPyObject __cdecl (*PyNumber_Power)(PPyObject ob1, PPyObject ob2, PPyObject ob3);
	PPyObject __cdecl (*PyNumber_Remainder)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Rshift)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Subtract)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyNumber_Xor)(PPyObject ob1, PPyObject ob2);
	void __cdecl (*PyOS_InitInterrupts)(void);
	int __cdecl (*PyOS_InterruptOccurred)(void);
	PPyObject __cdecl (*PyObject_CallObject)(PPyObject ob, PPyObject args);
	int __cdecl (*PyObject_Compare)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyObject_GetAttr)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyObject_GetAttrString)(PPyObject ob, char * c);
	PPyObject __cdecl (*PyObject_GetItem)(PPyObject ob, PPyObject key);
	PPyObject __cdecl (*PyObject_DelItem)(PPyObject ob, PPyObject key);
	int __cdecl (*PyObject_HasAttrString)(PPyObject ob, char * key);
	int __cdecl (*PyObject_Hash)(PPyObject ob);
	int __cdecl (*PyObject_IsTrue)(PPyObject ob);
	int __cdecl (*PyObject_Length)(PPyObject ob);
	PPyObject __cdecl (*PyObject_Repr)(PPyObject ob);
	int __cdecl (*PyObject_SetAttr)(PPyObject ob1, PPyObject ob2, PPyObject ob3);
	int __cdecl (*PyObject_SetAttrString)(PPyObject ob, char * key, PPyObject value);
	int __cdecl (*PyObject_SetItem)(PPyObject ob1, PPyObject ob2, PPyObject ob3);
	PPyObject __cdecl (*PyObject_Init)(PPyObject ob, PPyTypeObject t);
	PPyObject __cdecl (*PyObject_InitVar)(PPyObject ob, PPyTypeObject t, int size);
	PPyObject __cdecl (*PyObject_New)(PPyTypeObject t);
	PPyObject __cdecl (*PyObject_NewVar)(PPyTypeObject t, int size);
	int __cdecl (*PyObject_IsInstance)(PPyObject inst, PPyObject cls);
	int __cdecl (*PyObject_IsSubclass)(PPyObject derived, PPyObject cls);
	PPyObject __cdecl (*PyObject_GC_Malloc)(int size);
	PPyObject __cdecl (*PyObject_GC_New)(PPyTypeObject t);
	PPyObject __cdecl (*PyObject_GC_NewVar)(PPyTypeObject t, int size);
	PPyObject __cdecl (*PyObject_GC_Resize)(PPyObject t, int newsize);
	void __cdecl (*PyObject_GC_Del)(PPyObject ob);
	void __cdecl (*PyObject_GC_Track)(PPyObject ob);
	void __cdecl (*PyObject_GC_UnTrack)(PPyObject ob);
	PPyObject __cdecl (*PyRange_New)(int l1, int l2, int l3, int i);
	int __cdecl (*PySequence_Check)(PPyObject ob);
	PPyObject __cdecl (*PySequence_Concat)(PPyObject ob1, PPyObject ob2);
	int __cdecl (*PySequence_Count)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PySequence_GetItem)(PPyObject ob, int i);
	PPyObject __cdecl (*PySequence_GetSlice)(PPyObject ob, int i1, int i2);
	int __cdecl (*PySequence_In)(PPyObject ob1, PPyObject ob2);
	int __cdecl (*PySequence_Index)(PPyObject ob1, PPyObject ob2);
	int __cdecl (*PySequence_Length)(PPyObject ob);
	PPyObject __cdecl (*PySequence_Repeat)(PPyObject ob, int count);
	int __cdecl (*PySequence_SetItem)(PPyObject ob, int i, PPyObject value);
	int __cdecl (*PySequence_SetSlice)(PPyObject ob, int i1, int i2, PPyObject value);
	int __cdecl (*PySequence_DelSlice)(PPyObject ob, int i1, int i2);
	PPyObject __cdecl (*PySequence_Tuple)(PPyObject ob);
	int __cdecl (*PySequence_Contains)(PPyObject ob, PPyObject value);
	int __cdecl (*PySlice_GetIndices)(PPySliceObject ob, int length, int &start, int &stop, int &step);
	int __cdecl (*PySlice_GetIndicesEx)(PPySliceObject ob, int length, int &start, int &stop, int &step, int &slicelength);
	PPyObject __cdecl (*PySlice_New)(PPyObject start, PPyObject stop, PPyObject step);
	void __cdecl (*PyString_Concat)(PPyObject &ob1, PPyObject ob2);
	void __cdecl (*PyString_ConcatAndDel)(PPyObject &ob1, PPyObject ob2);
	PPyObject __cdecl (*PyString_Format)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyString_FromStringAndSize)(char * s, int i);
	int __cdecl (*PyString_Size)(PPyObject ob);
	PPyObject __cdecl (*PyString_DecodeEscape)(char * s, int len, char * errors, int unicode, char * recode_encoding);
	PPyObject __cdecl (*PyString_Repr)(PPyObject ob, int smartquotes);
	PPyObject __cdecl (*PySys_GetObject)(char * s);
	int __cdecl (*PySys_SetObject)(char * s, PPyObject ob);
	void __cdecl (*PySys_SetPath)(char * path);
	int __cdecl (*PyTraceBack_Here)(void * p);
	int __cdecl (*PyTraceBack_Print)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyTuple_GetItem)(PPyObject ob, int i);
	PPyObject __cdecl (*PyTuple_GetSlice)(PPyObject ob, int i1, int i2);
	PPyObject __cdecl (*PyTuple_New)(int size);
	int __cdecl (*PyTuple_SetItem)(PPyObject ob, int key, PPyObject value);
	int __cdecl (*PyTuple_Size)(PPyObject ob);
	int __cdecl (*PyType_IsSubtype)(PPyTypeObject a, PPyTypeObject b);
	PPyObject __cdecl (*PyUnicode_FromWideChar)(const wchar_t * w, int size);
	int __cdecl (*PyUnicode_AsWideChar)(PPyObject unicode, wchar_t * w, int size);
	PPyObject __cdecl (*PyUnicode_FromOrdinal)(int ordinal);
	PPyObject __cdecl (*PyWeakref_GetObject)(PPyObject ref);
	PPyObject __cdecl (*PyWeakref_NewProxy)(PPyObject ob, PPyObject callback);
	PPyObject __cdecl (*PyWeakref_NewRef)(PPyObject ob, PPyObject callback);
	PPyObject __cdecl (*PyWrapper_New)(PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl (*PyBool_FromLong)(int ok);
	int __cdecl (*Py_AtExit)(AtExitProc proc);
	PPyObject __cdecl (*Py_CompileString)(char * s1, char * s2, int i);
	void __cdecl (*Py_FatalError)(char * s);
	PPyObject __cdecl (*Py_FindMethod)(PPyMethodDef md, PPyObject ob, char * key);
	PPyObject __cdecl (*Py_FindMethodInChain)(PPyMethodChain mc, PPyObject ob, char * key);
	void __cdecl (*Py_FlushLine)(void);
	PPyObject __cdecl (*_PyObject_New)(PPyTypeObject obt, PPyObject ob);
	int __cdecl (*_PyString_Resize)(PPyObject &ob, int i);
	void __cdecl (*Py_Finalize)(void);
	int __cdecl (*PyErr_ExceptionMatches)(PPyObject exc);
	int __cdecl (*PyErr_GivenExceptionMatches)(PPyObject raised_exc, PPyObject exc);
	PPyObject __cdecl (*PyEval_EvalCode)(PPyCodeObject co, PPyObject globals, PPyObject locals);
	char * __cdecl (*Py_GetVersion)(void);
	char * __cdecl (*Py_GetCopyright)(void);
	char * __cdecl (*Py_GetExecPrefix)(void);
	char * __cdecl (*Py_GetPath)(void);
	char * __cdecl (*Py_GetPrefix)(void);
	char * __cdecl (*Py_GetProgramName)(void);
	PNode __cdecl (*PyParser_SimpleParseString)(char * str, int start);
	void __cdecl (*PyNode_Free)(PNode n);
	PPyObject __cdecl (*PyErr_NewException)(char * name, PPyObject base, PPyObject dict);
	void * __fastcall (*Py_Malloc)(int size);
	void * __fastcall (*PyMem_Malloc)(int size);
	PPyObject __cdecl (*PyObject_CallMethod)(PPyObject obj, char * method, char * format);
	void __cdecl (*Py_SetProgramName)(char * name);
	int __cdecl (*Py_IsInitialized)(void);
	char * __cdecl (*Py_GetProgramFullPath)(void);
	PPyThreadState __cdecl (*Py_NewInterpreter)(void);
	void __cdecl (*Py_EndInterpreter)(PPyThreadState tstate);
	void __cdecl (*PyEval_AcquireLock)(void);
	void __cdecl (*PyEval_ReleaseLock)(void);
	void __cdecl (*PyEval_AcquireThread)(PPyThreadState tstate);
	void __cdecl (*PyEval_ReleaseThread)(PPyThreadState tstate);
	PPyInterpreterState __cdecl (*PyInterpreterState_New)(void);
	void __cdecl (*PyInterpreterState_Clear)(PPyInterpreterState interp);
	void __cdecl (*PyInterpreterState_Delete)(PPyInterpreterState interp);
	PPyThreadState __cdecl (*PyThreadState_New)(PPyInterpreterState interp);
	void __cdecl (*PyThreadState_Clear)(PPyThreadState tstate);
	void __cdecl (*PyThreadState_Delete)(PPyThreadState tstate);
	PPyThreadState __cdecl (*PyThreadState_Get)(void);
	PPyThreadState __cdecl (*PyThreadState_Swap)(PPyThreadState tstate);
	void __fastcall Py_INCREF(PPyObject op);
	void __fastcall Py_DECREF(PPyObject op);
	void __fastcall Py_XINCREF(PPyObject op);
	void __fastcall Py_XDECREF(PPyObject op);
	char * __cdecl Py_GetPlatform(void);
	int __cdecl PyArg_Parse(PPyObject args, char * format, const void * const * argp, const int argp_Size);
	int __cdecl PyArg_ParseTuple(PPyObject args, char * format, const void * const * argp, const int argp_Size);
	PPyObject __cdecl Py_BuildValue(char * format, const System::TVarRec * args, const int args_Size);
	int __cdecl PyCode_Addr2Line(PPyCodeObject co, int addrq);
	char * __cdecl Py_GetBuildInfo(void);
	PPyObject __fastcall PyImport_ExecCodeModule(const AnsiString name, PPyObject codeobject);
	bool __fastcall PyString_Check(PPyObject obj);
	bool __fastcall PyString_CheckExact(PPyObject obj);
	bool __fastcall PyFloat_Check(PPyObject obj);
	bool __fastcall PyFloat_CheckExact(PPyObject obj);
	bool __fastcall PyInt_Check(PPyObject obj);
	bool __fastcall PyInt_CheckExact(PPyObject obj);
	bool __fastcall PyLong_Check(PPyObject obj);
	bool __fastcall PyLong_CheckExact(PPyObject obj);
	bool __fastcall PyTuple_Check(PPyObject obj);
	bool __fastcall PyTuple_CheckExact(PPyObject obj);
	bool __fastcall PyInstance_Check(PPyObject obj);
	bool __fastcall PyClass_Check(PPyObject obj);
	bool __fastcall PyMethod_Check(PPyObject obj);
	bool __fastcall PyList_Check(PPyObject obj);
	bool __fastcall PyList_CheckExact(PPyObject obj);
	bool __fastcall PyDict_Check(PPyObject obj);
	bool __fastcall PyDict_CheckExact(PPyObject obj);
	bool __fastcall PyModule_Check(PPyObject obj);
	bool __fastcall PyModule_CheckExact(PPyObject obj);
	bool __fastcall PySlice_Check(PPyObject obj);
	bool __fastcall PyFunction_Check(PPyObject obj);
	bool __fastcall PyUnicode_Check(PPyObject obj);
	bool __fastcall PyUnicode_CheckExact(PPyObject obj);
	bool __fastcall PyType_IS_GC(PPyTypeObject t);
	bool __fastcall PyObject_IS_GC(PPyObject obj);
	bool __fastcall PyWeakref_Check(PPyObject obj);
	bool __fastcall PyWeakref_CheckRef(PPyObject obj);
	bool __fastcall PyWeakref_CheckProxy(PPyObject obj);
	bool __fastcall PyBool_Check(PPyObject obj);
	bool __fastcall PyBaseString_Check(PPyObject obj);
	bool __fastcall PyEnum_Check(PPyObject obj);
	bool __fastcall PyObject_TypeCheck(PPyObject obj, PPyTypeObject t);
	PPyObject __fastcall Py_InitModule(const char * name, PPyMethodDef md);
	__fastcall virtual TPythonInterface(Classes::TComponent* AOwner);
	void __fastcall MapDll(void);
	__property bool Initialized = {read=GetInitialized, nodefault};
	__property bool Finalizing = {read=FFinalizing, nodefault};
public:
	#pragma option push -w-inl
	/* TDynamicDll.Destroy */ inline __fastcall virtual ~TPythonInterface(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TDatetimeConversionMode { dcmToTuple, dcmToDatetime };
#pragma option pop

typedef void __fastcall (__closure *TPathInitializationEvent)(System::TObject* Sender, AnsiString &Path);

#pragma option push -b-
enum TPythonFlag { pfDebug, pfInteractive, pfNoSite, pfOptimize, pfTabcheck, pfUnicode, pfVerbose, pfUseClassExceptionsFlag, pfFrozenFlag, pfIgnoreEnvironmentFlag, pfDivisionWarningFlag };
#pragma option pop

typedef Set<TPythonFlag, pfDebug, pfDivisionWarningFlag>  TPythonFlags;

class DELPHICLASS TTracebackItem;
class PASCALIMPLEMENTATION TTracebackItem : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	AnsiString FileName;
	int LineNo;
	AnsiString Context;
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TTracebackItem(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TTracebackItem(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPythonTraceback;
class PASCALIMPLEMENTATION TPythonTraceback : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	Classes::TList* FItems;
	int FLimit;
	int __fastcall GetItemCount(void);
	TTracebackItem* __fastcall GetItem(int idx);
	
public:
	__fastcall TPythonTraceback(void);
	__fastcall virtual ~TPythonTraceback(void);
	void __fastcall Clear(void);
	void __fastcall Refresh(void);
	__property int ItemCount = {read=GetItemCount, nodefault};
	__property TTracebackItem* Items[int idx] = {read=GetItem};
	__property int Limit = {read=FLimit, write=FLimit, nodefault};
};


class DELPHICLASS TPythonEngine;
class DELPHICLASS TEngineClient;
class PASCALIMPLEMENTATION TPythonEngine : public TPythonInterface 
{
	typedef TPythonInterface inherited;
	
private:
	Classes::TStrings* FInitScript;
	TPythonInputOutput* FIO;
	bool FRedirectIO;
	Classes::TNotifyEvent FOnAfterInit;
	Classes::TList* FClients;
	Syncobjs::TCriticalSection* FLock;
	AnsiString FExecModule;
	bool FAutoFinalize;
	AnsiString FProgramName;
	bool FInitThreads;
	TPathInitializationEvent FOnPathInitialization;
	TPythonTraceback* FTraceback;
	bool FUseWindowsConsole;
	PyObject *FGlobalVars;
	PyObject *FLocalVars;
	TPythonFlags FPyFlags;
	bool FIORedirected;
	TDatetimeConversionMode FDatetimeConversionMode;
	PyObject *FTimeStruct;
	PyObject *FPyDateTime_DateType;
	PyObject *FPyDateTime_DateTimeType;
	PyObject *FPyDateTime_DeltaType;
	PyObject *FPyDateTime_TimeType;
	PyObject *FPyDateTime_TZInfoType;
	PyObject *FPyDateTime_TimeTZType;
	PyObject *FPyDateTime_DateTimeTZType;
	
protected:
	virtual void __fastcall AfterLoad(void);
	virtual void __fastcall BeforeLoad(void);
	virtual void __fastcall DoOpenDll(const AnsiString aDllName);
	void __fastcall SetInitScript(Classes::TStrings* Value);
	PPyThreadState __fastcall GetThreadState(void);
	PPyInterpreterState __fastcall GetInterpreterState(void);
	void __fastcall SetInitThreads(bool Value);
	int __fastcall GetClientCount(void);
	TEngineClient* __fastcall GetClients(int idx);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall CheckRegistry(void);
	void __fastcall SetProgramArgs(void);
	void __fastcall InitWinConsole(void);
	void __fastcall SetUseWindowsConsole(const bool Value);
	void __fastcall SetGlobalVars(const PPyObject Value);
	void __fastcall SetLocalVars(const PPyObject Value);
	void __fastcall SetPyFlags(const TPythonFlags Value);
	void __fastcall AssignPyFlags(void);
	
public:
	__fastcall virtual TPythonEngine(Classes::TComponent* AOwner);
	__fastcall virtual ~TPythonEngine(void);
	void __fastcall Initialize(void);
	void __fastcall Finalize(void);
	void __fastcall Lock(void);
	void __fastcall Unlock(void);
	bool __fastcall IsType(PPyObject ob, PPyTypeObject obt);
	char * __fastcall GetAttrString(PPyObject obj, char * name);
	AnsiString __fastcall CleanString(const AnsiString s);
	AnsiString __fastcall Run_CommandAsString(const AnsiString command, int mode);
	PPyObject __fastcall Run_CommandAsObject(const AnsiString command, int mode);
	void __fastcall ExecString(const AnsiString command);
	PPyObject __fastcall EvalString(const AnsiString command);
	Variant __fastcall EvalPyFunction(PPyObject pyfunc, PPyObject pyargs);
	Variant __fastcall EvalFunction(PPyObject pyfunc, const System::TVarRec * args, const int args_Size);
	Variant __fastcall EvalFunctionNoArgs(PPyObject pyfunc);
	AnsiString __fastcall EvalStringAsStr(const AnsiString command);
	void __fastcall ExecStrings(Classes::TStrings* strings);
	PPyObject __fastcall EvalStrings(Classes::TStrings* strings);
	AnsiString __fastcall EvalStringsAsStr(Classes::TStrings* strings);
	bool __fastcall CheckEvalSyntax(const AnsiString str);
	bool __fastcall CheckExecSyntax(const AnsiString str);
	bool __fastcall CheckSyntax(const AnsiString str, int mode);
	void __fastcall RaiseError(void);
	AnsiString __fastcall PyObjectAsString(PPyObject obj);
	void __fastcall DoRedirectIO(void);
	void __fastcall AddClient(TEngineClient* client);
	void __fastcall RemoveClient(TEngineClient* client);
	TEngineClient* __fastcall FindClient(const AnsiString aName);
	PPyTypeObject __fastcall TypeByName(const AnsiString aTypeName);
	PPyObject __fastcall ModuleByName(const AnsiString aModuleName);
	PPyMethodDef __fastcall MethodsByName(const AnsiString aMethodsContainer);
	virtual PPyObject __fastcall VariantAsPyObject(const Variant &V);
	virtual Variant __fastcall PyObjectAsVariant(PPyObject obj);
	PPyObject __fastcall VarRecAsPyObject(const System::TVarRec &v);
	PPyObject __fastcall MakePyTuple(const PPyObject * objects, const int objects_Size);
	PPyObject __fastcall MakePyList(const PPyObject * objects, const int objects_Size);
	PPyObject __fastcall ArrayToPyTuple(const System::TVarRec * items, const int items_Size);
	PPyObject __fastcall ArrayToPyList(const System::TVarRec * items, const int items_Size);
	PPyObject __fastcall ArrayToPyDict(const System::TVarRec * items, const int items_Size);
	PPyObject __fastcall StringsToPyList(Classes::TStrings* strings);
	PPyObject __fastcall StringsToPyTuple(Classes::TStrings* strings);
	void __fastcall PyListToStrings(PPyObject list, Classes::TStrings* strings);
	void __fastcall PyTupleToStrings(PPyObject tuple, Classes::TStrings* strings);
	WideString __fastcall PyUnicode_AsWideString(PPyObject obj);
	PPyObject __fastcall PyUnicode_FromWideString(const WideString AString);
	PPyObject __fastcall ReturnNone(void);
	PPyObject __fastcall FindModule(const AnsiString ModuleName);
	PPyObject __fastcall FindFunction(AnsiString ModuleName, AnsiString FuncName);
	PPyObject __fastcall SetToList(void * data, int size);
	void __fastcall ListToSet(PPyObject List, void * data, int size);
	void __fastcall CheckError(void);
	PPyObject __fastcall GetMainModule(void);
	bool __fastcall PyTimeStruct_Check(PPyObject obj);
	bool __fastcall PyDate_Check(PPyObject obj);
	bool __fastcall PyDate_CheckExact(PPyObject obj);
	bool __fastcall PyDateTime_Check(PPyObject obj);
	bool __fastcall PyDateTime_CheckExact(PPyObject obj);
	bool __fastcall PyDateTimeTZ_Check(PPyObject obj);
	bool __fastcall PyDateTimeTZ_CheckExact(PPyObject obj);
	bool __fastcall PyTime_Check(PPyObject obj);
	bool __fastcall PyTime_CheckExact(PPyObject obj);
	bool __fastcall PyTimeTZ_Check(PPyObject obj);
	bool __fastcall PyTimeTZ_CheckExact(PPyObject obj);
	bool __fastcall PyDelta_Check(PPyObject obj);
	bool __fastcall PyDelta_CheckExact(PPyObject obj);
	bool __fastcall PyTZInfo_Check(PPyObject obj);
	bool __fastcall PyTZInfo_CheckExact(PPyObject obj);
	int __fastcall PyDateTime_GET_YEAR(PPyObject obj);
	int __fastcall PyDateTime_GET_MONTH(PPyObject obj);
	int __fastcall PyDateTime_GET_DAY(PPyObject obj);
	int __fastcall PyDateTime_DATE_GET_HOUR(PPyObject obj);
	int __fastcall PyDateTime_DATE_GET_MINUTE(PPyObject obj);
	int __fastcall PyDateTime_DATE_GET_SECOND(PPyObject obj);
	int __fastcall PyDateTime_DATE_GET_MICROSECOND(PPyObject obj);
	int __fastcall PyDateTime_TIME_GET_HOUR(PPyObject obj);
	int __fastcall PyDateTime_TIME_GET_MINUTE(PPyObject obj);
	int __fastcall PyDateTime_TIME_GET_SECOND(PPyObject obj);
	int __fastcall PyDateTime_TIME_GET_MICROSECOND(PPyObject obj);
	__property int ClientCount = {read=GetClientCount, nodefault};
	__property TEngineClient* Clients[int idx] = {read=GetClients};
	__property AnsiString ExecModule = {read=FExecModule, write=FExecModule};
	__property PPyThreadState ThreadState = {read=GetThreadState};
	__property PPyInterpreterState InterpreterState = {read=GetInterpreterState};
	__property TPythonTraceback* Traceback = {read=FTraceback};
	__property PPyObject LocalVars = {read=FLocalVars, write=SetLocalVars};
	__property PPyObject GlobalVars = {read=FGlobalVars, write=SetGlobalVars};
	
__published:
	__property bool AutoFinalize = {read=FAutoFinalize, write=FAutoFinalize, default=1};
	__property TDatetimeConversionMode DatetimeConversionMode = {read=FDatetimeConversionMode, write=FDatetimeConversionMode, default=0};
	__property Classes::TStrings* InitScript = {read=FInitScript, write=SetInitScript};
	__property bool InitThreads = {read=FInitThreads, write=SetInitThreads, default=0};
	__property TPythonInputOutput* IO = {read=FIO, write=FIO};
	__property TPythonFlags PyFlags = {read=FPyFlags, write=SetPyFlags, default=0};
	__property bool RedirectIO = {read=FRedirectIO, write=FRedirectIO, default=1};
	__property bool UseWindowsConsole = {read=FUseWindowsConsole, write=FUseWindowsConsole, default=0};
	__property Classes::TNotifyEvent OnAfterInit = {read=FOnAfterInit, write=FOnAfterInit};
	__property TPathInitializationEvent OnPathInitialization = {read=FOnPathInitialization, write=FOnPathInitialization};
};


class PASCALIMPLEMENTATION TEngineClient : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	TPythonEngine* FEngine;
	Classes::TNotifyEvent FOnInitialization;
	Classes::TNotifyEvent FOnFinalization;
	Classes::TNotifyEvent FOnCreate;
	Classes::TNotifyEvent FOnDestroy;
	bool FInitialized;
	virtual void __fastcall SetEngine(TPythonEngine* val);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TEngineClient(Classes::TComponent* AOwner);
	__fastcall virtual ~TEngineClient(void);
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	void __fastcall ClearEngine(void);
	void __fastcall CheckEngine(void);
	__property bool Initialized = {read=FInitialized, nodefault};
	
__published:
	__property TPythonEngine* Engine = {read=FEngine, write=SetEngine};
	__property Classes::TNotifyEvent OnCreate = {read=FOnCreate, write=FOnCreate};
	__property Classes::TNotifyEvent OnDestroy = {read=FOnDestroy, write=FOnDestroy};
	__property Classes::TNotifyEvent OnFinalization = {read=FOnFinalization, write=FOnFinalization};
	__property Classes::TNotifyEvent OnInitialization = {read=FOnInitialization, write=FOnInitialization};
};


typedef PyMethodDef TMethodArray[16001];

typedef PyMethodDef *PMethodArray;

typedef PPyObject __cdecl (__closure *TDelphiMethod)(PPyObject self, PPyObject args);

typedef void __fastcall (__closure *TPythonEvent)(System::TObject* Sender, PPyObject PSelf, PPyObject Args, PPyObject &Result);

class DELPHICLASS TEventDef;
class DELPHICLASS TEventDefs;
class PASCALIMPLEMENTATION TEventDef : public Classes::TCollectionItem 
{
	typedef Classes::TCollectionItem inherited;
	
private:
	AnsiString FName;
	AnsiString FTmpDocString;
	TPythonEvent FOnExecute;
	Classes::TStringList* FDocString;
	void __fastcall SetDocString(const Classes::TStringList* Value);
	
protected:
	virtual AnsiString __fastcall GetDisplayName();
	virtual void __fastcall SetDisplayName(const AnsiString Value);
	
public:
	__fastcall virtual TEventDef(Classes::TCollection* Collection);
	__fastcall virtual ~TEventDef(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	AnsiString __fastcall GetDocString();
	PPyObject __cdecl PythonEvent(PPyObject pself, PPyObject args);
	TEventDefs* __fastcall Owner(void);
	
__published:
	__property AnsiString Name = {read=FName, write=SetDisplayName};
	__property TPythonEvent OnExecute = {read=FOnExecute, write=FOnExecute};
	__property Classes::TStringList* DocString = {read=FDocString, write=SetDocString};
};


class DELPHICLASS TMethodsContainer;
class PASCALIMPLEMENTATION TMethodsContainer : public TEngineClient 
{
	typedef TEngineClient inherited;
	
protected:
	int FMethodCount;
	int FAllocatedMethodCount;
	PyMethodDef *FMethods;
	TEventDefs* FEventDefs;
	void __fastcall AllocMethods(void);
	void __fastcall FreeMethods(void);
	void __fastcall ReallocMethods(void);
	PPyMethodDef __fastcall GetMethods(int idx);
	bool __fastcall StoreEventDefs(void);
	
public:
	__fastcall virtual TMethodsContainer(Classes::TComponent* AOwner);
	__fastcall virtual ~TMethodsContainer(void);
	virtual void __fastcall Initialize(void);
	void __fastcall AddMethod(char * MethodName, PyCFunction Method, char * DocString);
	void __fastcall AddDelphiMethod(char * MethodName, TDelphiMethod DelphiMethod, char * DocString);
	__property int MethodCount = {read=FMethodCount, nodefault};
	__property PPyMethodDef Methods[int idx] = {read=GetMethods};
	__property PPyMethodDef MethodsData = {read=FMethods};
	
__published:
	__property TEventDefs* Events = {read=FEventDefs, write=FEventDefs, stored=StoreEventDefs};
};


class PASCALIMPLEMENTATION TEventDefs : public Classes::TCollection 
{
	typedef Classes::TCollection inherited;
	
protected:
	TMethodsContainer* FMethodsContainer;
	TEventDef* __fastcall GetItems(int idx);
	void __fastcall SetItems(int idx, TEventDef* Value);
	DYNAMIC Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TEventDefs(TMethodsContainer* AMethodsContainer);
	HIDESBASE TEventDef* __fastcall Add(void);
	void __fastcall RegisterEvents(void);
	__property TEventDef* Items[int idx] = {read=GetItems};
	__property TMethodsContainer* Container = {read=FMethodsContainer};
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TEventDefs(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TErrorType { etString, etClass };
#pragma option pop

class DELPHICLASS TParentClassError;
class PASCALIMPLEMENTATION TParentClassError : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
protected:
	AnsiString FName;
	AnsiString FModule;
	
public:
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	
__published:
	__property AnsiString Module = {read=FModule, write=FModule};
	__property AnsiString Name = {read=FName, write=FName};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TParentClassError(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TParentClassError(void) : Classes::TPersistent() { }
	#pragma option pop
	
};


class DELPHICLASS TError;
class DELPHICLASS TErrors;
class PASCALIMPLEMENTATION TError : public Classes::TCollectionItem 
{
	typedef Classes::TCollectionItem inherited;
	
protected:
	AnsiString FName;
	AnsiString FText;
	PyObject *FError;
	TErrorType FErrorType;
	TParentClassError* FParentClass;
	virtual AnsiString __fastcall GetDisplayName();
	void __fastcall SetName(const AnsiString Value);
	void __fastcall SetText(const AnsiString Value);
	void __fastcall SetErrorType(TErrorType Value);
	void __fastcall SetParentClass(TParentClassError* Value);
	
public:
	__fastcall virtual TError(Classes::TCollection* Collection);
	__fastcall virtual ~TError(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	void __fastcall BuildError(const AnsiString ModuleName);
	void __fastcall RaiseError(const AnsiString msg);
	void __fastcall RaiseErrorObj(const AnsiString msg, PPyObject obj);
	TErrors* __fastcall Owner(void);
	__property PPyObject Error = {read=FError, write=FError};
	
__published:
	__property AnsiString Name = {read=FName, write=SetName};
	__property AnsiString Text = {read=FText, write=SetText};
	__property TErrorType ErrorType = {read=FErrorType, write=SetErrorType, nodefault};
	__property TParentClassError* ParentClass = {read=FParentClass, write=SetParentClass};
};


class DELPHICLASS TPythonModule;
class PASCALIMPLEMENTATION TPythonModule : public TMethodsContainer 
{
	typedef TMethodsContainer inherited;
	
protected:
	AnsiString FModuleName;
	PyObject *FModule;
	Classes::TList* FClients;
	TErrors* FErrors;
	Classes::TNotifyEvent FOnAfterInitialization;
	Classes::TStringList* FDocString;
	int __fastcall GetClientCount(void);
	TEngineClient* __fastcall GetClients(int idx);
	void __fastcall SetErrors(TErrors* val);
	void __fastcall SetModuleName(const AnsiString val);
	void __fastcall SetDocString(Classes::TStringList* value);
	
public:
	__fastcall virtual TPythonModule(Classes::TComponent* AOwner);
	__fastcall virtual ~TPythonModule(void);
	void __fastcall MakeModule(void);
	void __fastcall DefineDocString(void);
	virtual void __fastcall Initialize(void);
	void __fastcall InitializeForNewInterpreter(void);
	void __fastcall AddClient(TEngineClient* client);
	TError* __fastcall ErrorByName(const AnsiString name);
	void __fastcall RaiseError(const AnsiString error, const AnsiString msg);
	void __fastcall RaiseErrorFmt(const AnsiString error, const AnsiString format, const System::TVarRec * Args, const int Args_Size);
	void __fastcall RaiseErrorObj(const AnsiString error, const AnsiString msg, PPyObject obj);
	void __fastcall BuildErrors(void);
	void __fastcall SetVar(const AnsiString varName, PPyObject value);
	PPyObject __fastcall GetVar(const AnsiString varName);
	void __fastcall DeleteVar(const AnsiString varName);
	void __fastcall SetVarFromVariant(const AnsiString varName, const Variant &value);
	Variant __fastcall GetVarAsVariant(const AnsiString varName);
	__property PPyObject Module = {read=FModule};
	__property TEngineClient* Clients[int idx] = {read=GetClients};
	__property int ClientCount = {read=GetClientCount, nodefault};
	
__published:
	__property Classes::TStringList* DocString = {read=FDocString, write=SetDocString};
	__property AnsiString ModuleName = {read=FModuleName, write=SetModuleName};
	__property TErrors* Errors = {read=FErrors, write=SetErrors};
	__property Classes::TNotifyEvent OnAfterInitialization = {read=FOnAfterInitialization, write=FOnAfterInitialization};
};


class PASCALIMPLEMENTATION TErrors : public Classes::TCollection 
{
	typedef Classes::TCollection inherited;
	
public:
	TError* operator[](int Index) { return Items[Index]; }
	
private:
	TPythonModule* FModule;
	TError* __fastcall GetError(int Index);
	void __fastcall SetError(int Index, TError* Value);
	
protected:
	DYNAMIC Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall Update(Classes::TCollectionItem* Item);
	
public:
	__fastcall TErrors(TPythonModule* Module);
	HIDESBASE TError* __fastcall Add(void);
	HIDESBASE TPythonModule* __fastcall Owner(void);
	__property TError* Items[int Index] = {read=GetError, write=SetError/*, default*/};
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TErrors(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPyObject;
class DELPHICLASS TPythonType;
typedef TMetaClass*TPyObjectClass;

class DELPHICLASS TTypeServices;
#pragma option push -b-
enum PythonEngine__56 { bsPrint, bsGetAttr, bsSetAttr, bsRepr, bsCompare, bsHash, bsStr, bsGetAttrO, bsSetAttrO, bsCall, bsTraverse, bsClear, bsRichCompare, bsIter, bsIterNext };
#pragma option pop

typedef Set<PythonEngine__56, bsPrint, bsIterNext>  TBasicServices;

#pragma option push -b-
enum PythonEngine__66 { nsAdd, nsSubstract, nsMultiply, nsDivide, nsRemainder, nsDivmod, nsPower, nsNegative, nsPositive, nsAbsolute, nsNonZero, nsInvert, nsLShift, nsRShift, nsAnd, nsXor, nsOr, nsCoerce, nsInt, nsLong, nsFloat, nsOct, nsHex, nsFloorDivide, nsTrueDivide };
#pragma option pop

typedef Set<PythonEngine__66, nsAdd, nsTrueDivide>  TNumberServices;

#pragma option push -b-
enum PythonEngine__86 { ssLength, ssConcat, ssRepeat, ssItem, ssSlice, ssAssItem, ssAssSlice, ssContains, ssInplaceConcat, ssInplaceRepeat };
#pragma option pop

typedef Set<PythonEngine__86, ssLength, ssInplaceRepeat>  TSequenceServices;

#pragma option push -b-
enum PythonEngine__96 { msLength, msSubscript, msAssSubscript };
#pragma option pop

typedef Set<PythonEngine__96, msLength, msAssSubscript>  TMappingServices;

#pragma option push -b-
enum PythonEngine__76 { nsInplaceAdd, nsInplaceSubtract, nsInplaceMultiply, nsInplaceDivide, nsInplaceRemainder, nsInplacePower, nsInplaceLShift, nsInplaceRShift, nsInplaceAnd, nsInplaceXor, nsInplaceOr, nsInplaceFloorDivide, nsInplaceTrueDivide };
#pragma option pop

typedef Set<PythonEngine__76, nsInplaceAdd, nsInplaceTrueDivide>  TInplaceNumberServices;

class PASCALIMPLEMENTATION TTypeServices : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
protected:
	TBasicServices FBasic;
	TNumberServices FNumber;
	TSequenceServices FSequence;
	TMappingServices FMapping;
	TInplaceNumberServices FInplaceNumber;
	
public:
	__fastcall TTypeServices(void);
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	
__published:
	__property TBasicServices Basic = {read=FBasic, write=FBasic, nodefault};
	__property TInplaceNumberServices InplaceNumber = {read=FInplaceNumber, write=FInplaceNumber, nodefault};
	__property TNumberServices Number = {read=FNumber, write=FNumber, nodefault};
	__property TSequenceServices Sequence = {read=FSequence, write=FSequence, nodefault};
	__property TMappingServices Mapping = {read=FMapping, write=FMapping, nodefault};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TTypeServices(void) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TPythonType : public TMethodsContainer 
{
	typedef TMethodsContainer inherited;
	
protected:
	#pragma pack(push, 1)
	PyTypeObject FType;
	#pragma pack(pop)
	
	AnsiString FTypeName;
	TPythonModule* FModule;
	TMetaClass*FPyObjectClass;
	AnsiString FPrefix;
	AnsiString FCreateFuncName;
	TTypeServices* FServices;
	#pragma pack(push, 1)
	PyNumberMethods FNumber;
	#pragma pack(pop)
	
	#pragma pack(push, 1)
	PySequenceMethods FSequence;
	#pragma pack(pop)
	
	#pragma pack(push, 1)
	PyMappingMethods FMapping;
	#pragma pack(pop)
	
	AnsiString FCurrentDocString;
	Classes::TStringList* FDocString;
	AnsiString FCreateFuncDoc;
	int FInstanceCount;
	int FCreateHits;
	int FDeleteHits;
	TPFlags FTypeFlags;
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	PPyTypeObject __fastcall GetTypePtr(void);
	void __fastcall SetPyObjectClass(TMetaClass* val);
	void __fastcall SetModule(TPythonModule* val);
	void __fastcall SetServices(TTypeServices* val);
	void __fastcall SetTypeName(const AnsiString val);
	PPyObject __cdecl CreateMethod(PPyObject pSelf, PPyObject args);
	void __fastcall InitServices(void);
	void __fastcall SetDocString(Classes::TStringList* value);
	int __fastcall TypeFlagsAsInt(void);
	int __cdecl Print(PPyObject pSelf, file &f, int i);
	PPyObject __cdecl GetAttr(PPyObject pSelf, char * key);
	int __cdecl SetAttr(PPyObject pSelf, char * key, PPyObject value);
	PPyObject __cdecl Repr(PPyObject pSelf);
	int __cdecl Compare(PPyObject pSelf, PPyObject obj);
	int __cdecl Hash(PPyObject pSelf);
	PPyObject __cdecl Str(PPyObject pSelf);
	PPyObject __cdecl GetAttrO(PPyObject pSelf, PPyObject key);
	int __cdecl SetAttrO(PPyObject pSelf, PPyObject key, PPyObject value);
	PPyObject __cdecl Call(PPyObject pSelf, PPyObject ob1, PPyObject ob2);
	int __cdecl Traverse(PPyObject pSelf, visitproc proc, void * ptr);
	int __cdecl Clear(PPyObject pSelf);
	PPyObject __cdecl RichCmp(PPyObject pSelf, PPyObject obj, int i);
	PPyObject __cdecl Iter(PPyObject pSelf);
	PPyObject __cdecl IterNext(PPyObject pSelf);
	PPyObject __cdecl NbAdd(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbSubstract(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbMultiply(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbDivide(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbFloorDivide(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbTrueDivide(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbRemainder(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbDivmod(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbPower(PPyObject pSelf, PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl NbNegative(PPyObject pSelf);
	PPyObject __cdecl NbPositive(PPyObject pSelf);
	PPyObject __cdecl NbAbsolute(PPyObject pSelf);
	int __cdecl NbNonZero(PPyObject pSelf);
	PPyObject __cdecl NbInvert(PPyObject pSelf);
	PPyObject __cdecl NbLShift(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbRShift(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbAnd(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbXor(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbOr(PPyObject pSelf, PPyObject obj);
	int __cdecl NbCoerce(PPyObject pSelf, PPPyObject obj);
	PPyObject __cdecl NbInt(PPyObject pSelf);
	PPyObject __cdecl NbLong(PPyObject pSelf);
	PPyObject __cdecl NbFloat(PPyObject pSelf);
	PPyObject __cdecl NbOct(PPyObject pSelf);
	PPyObject __cdecl NbHex(PPyObject pSelf);
	PPyObject __cdecl NbInplaceAdd(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceSubtract(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceMultiply(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceDivide(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceFloorDivide(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceTrueDivide(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceRemainder(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplacePower(PPyObject pSelf, PPyObject ob1, PPyObject ob2);
	PPyObject __cdecl NbInplaceLshift(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceRshift(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceAnd(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceXor(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl NbInplaceOr(PPyObject pSelf, PPyObject obj);
	int __cdecl SqLength(PPyObject pSelf);
	PPyObject __cdecl SqConcat(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl SqRepeat(PPyObject pSelf, int val);
	PPyObject __cdecl SqItem(PPyObject pSelf, int idx);
	PPyObject __cdecl SqSlice(PPyObject pSelf, int idx1, int idx2);
	int __cdecl SqAssItem(PPyObject pSelf, int idx, PPyObject obj);
	int __cdecl SqAssSlice(PPyObject pSelf, int idx1, int idx2, PPyObject obj);
	int __cdecl SqContains(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl SqInplaceConcat(PPyObject pSelf, PPyObject obj);
	PPyObject __cdecl SqInplaceRepeat(PPyObject pSelf, int i);
	int __cdecl MpLength(PPyObject pSelf);
	PPyObject __cdecl MpSubscript(PPyObject pSelf, PPyObject obj);
	int __cdecl MpAssSubscript(PPyObject pSelf, PPyObject obj1, PPyObject obj2);
	
public:
	__fastcall virtual TPythonType(Classes::TComponent* AOwner);
	__fastcall virtual ~TPythonType(void);
	virtual void __fastcall Initialize(void);
	PPyObject __fastcall CreateInstance(void);
	PPyObject __fastcall CreateInstanceWith(PPyObject args);
	void __fastcall AddTypeVar(void);
	__property PyTypeObject TheType = {read=FType, write=FType};
	__property PPyTypeObject TheTypePtr = {read=GetTypePtr};
	__property TMetaClass* PyObjectClass = {read=FPyObjectClass, write=SetPyObjectClass, stored=false};
	__property int InstanceCount = {read=FInstanceCount, nodefault};
	__property int CreateHits = {read=FCreateHits, nodefault};
	__property int DeleteHits = {read=FDeleteHits, nodefault};
	
__published:
	__property Classes::TStringList* DocString = {read=FDocString, write=SetDocString};
	__property AnsiString TypeName = {read=FTypeName, write=SetTypeName};
	__property TPFlags TypeFlags = {read=FTypeFlags, write=FTypeFlags, default=491};
	__property AnsiString Prefix = {read=FPrefix, write=FPrefix};
	__property TPythonModule* Module = {read=FModule, write=SetModule};
	__property TTypeServices* Services = {read=FServices, write=SetServices};
};


class PASCALIMPLEMENTATION TPyObject : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	int ob_refcnt;
	PyTypeObject *ob_type;
	TPythonType* PythonType;
	__fastcall virtual TPyObject(TPythonType* APythonType);
	__fastcall virtual TPyObject(TPythonType* APythonType, PPyObject args);
	__fastcall virtual ~TPyObject(void);
	PPyObject __fastcall GetSelf(void);
	void __fastcall IncRef(void);
	void __fastcall Adjust(void * PyPointer);
	TPythonModule* __fastcall GetModule(void);
	virtual int __fastcall Print(file &f, int i);
	virtual PPyObject __fastcall GetAttr(char * key);
	virtual int __fastcall SetAttr(char * key, PPyObject value);
	virtual PPyObject __fastcall Repr(void);
	virtual int __fastcall Compare(PPyObject obj);
	virtual int __fastcall Hash(void);
	virtual PPyObject __fastcall Str(void);
	virtual PPyObject __fastcall GetAttrO(PPyObject key);
	virtual int __fastcall SetAttrO(PPyObject key, PPyObject value);
	virtual PPyObject __fastcall Call(PPyObject ob1, PPyObject ob2);
	virtual int __fastcall Traverse(visitproc proc, void * ptr);
	virtual int __fastcall Clear(void);
	virtual PPyObject __fastcall RichCompare(PPyObject obj, TRichComparisonOpcode Op);
	virtual PPyObject __fastcall Iter(void);
	virtual PPyObject __fastcall IterNext(void);
	virtual PPyObject __fastcall NbAdd(PPyObject obj);
	virtual PPyObject __fastcall NbSubstract(PPyObject obj);
	virtual PPyObject __fastcall NbMultiply(PPyObject obj);
	virtual PPyObject __fastcall NbDivide(PPyObject obj);
	virtual PPyObject __fastcall NbFloorDivide(PPyObject obj);
	virtual PPyObject __fastcall NbTrueDivide(PPyObject obj);
	virtual PPyObject __fastcall NbRemainder(PPyObject obj);
	virtual PPyObject __fastcall NbDivmod(PPyObject obj);
	virtual PPyObject __fastcall NbPower(PPyObject ob1, PPyObject ob2);
	virtual PPyObject __fastcall NbNegative(void);
	virtual PPyObject __fastcall NbPositive(void);
	virtual PPyObject __fastcall NbAbsolute(void);
	virtual int __fastcall NbNonZero(void);
	virtual PPyObject __fastcall NbInvert(void);
	virtual PPyObject __fastcall NbLShift(PPyObject obj);
	virtual PPyObject __fastcall NbRShift(PPyObject obj);
	virtual PPyObject __fastcall NbAnd(PPyObject obj);
	virtual PPyObject __fastcall NbXor(PPyObject obj);
	virtual PPyObject __fastcall NbOr(PPyObject obj);
	virtual int __fastcall NbCoerce(PPPyObject obj);
	virtual PPyObject __fastcall NbInt(void);
	virtual PPyObject __fastcall NbLong(void);
	virtual PPyObject __fastcall NbFloat(void);
	virtual PPyObject __fastcall NbOct(void);
	virtual PPyObject __fastcall NbHex(void);
	virtual PPyObject __fastcall NbInplaceAdd(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceSubtract(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceMultiply(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceDivide(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceFloorDivide(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceTrueDivide(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceRemainder(PPyObject obj);
	virtual PPyObject __fastcall NbInplacePower(PPyObject ob1, PPyObject ob2);
	virtual PPyObject __fastcall NbInplaceLshift(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceRshift(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceAnd(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceXor(PPyObject obj);
	virtual PPyObject __fastcall NbInplaceOr(PPyObject obj);
	virtual int __fastcall SqLength(void);
	virtual PPyObject __fastcall SqConcat(PPyObject obj);
	virtual PPyObject __fastcall SqRepeat(int val);
	virtual PPyObject __fastcall SqItem(int idx);
	virtual PPyObject __fastcall SqSlice(int idx1, int idx2);
	virtual int __fastcall SqAssItem(int idx, PPyObject obj);
	virtual int __fastcall SqAssSlice(int idx1, int idx2, PPyObject obj);
	virtual int __fastcall SqContains(PPyObject obj);
	virtual PPyObject __fastcall SqInplaceConcat(PPyObject obj);
	virtual PPyObject __fastcall SqInplaceRepeat(int i);
	virtual int __fastcall MpLength(void);
	virtual PPyObject __fastcall MpSubscript(PPyObject obj);
	virtual int __fastcall MpAssSubscript(PPyObject obj1, PPyObject obj2);
	/* virtual class method */ virtual void __fastcall RegisterMethods(TMetaClass* vmt, TPythonType* PythonType);
};


typedef void __fastcall (__closure *TGetDataEvent)(System::TObject* Sender, Variant &Data);

typedef void __fastcall (__closure *TSetDataEvent)(System::TObject* Sender, const Variant &Data);

typedef void __fastcall (__closure *TExtGetDataEvent)(System::TObject* Sender, PPyObject &Data);

typedef void __fastcall (__closure *TExtSetDataEvent)(System::TObject* Sender, PPyObject Data);

class DELPHICLASS TPythonDelphiVar;
class PASCALIMPLEMENTATION TPythonDelphiVar : public TEngineClient 
{
	typedef TEngineClient inherited;
	
protected:
	AnsiString FModule;
	AnsiString FVarName;
	PyObject *FVarObject;
	TPythonType* FVarType;
	TGetDataEvent FOnGetData;
	TSetDataEvent FOnSetData;
	TExtGetDataEvent FOnExtGetData;
	TExtSetDataEvent FOnExtSetData;
	Classes::TNotifyEvent FOnChange;
	void __fastcall CreateVarType(void);
	void __fastcall CreateVar(void);
	Variant __fastcall GetValue();
	void __fastcall SetValue(const Variant &val);
	PPyObject __fastcall GetValueAsPyObject(void);
	void __fastcall SetValueFromPyObject(PPyObject val);
	AnsiString __fastcall GetValueAsString();
	void __fastcall SetVarName(const AnsiString val);
	
public:
	__fastcall virtual TPythonDelphiVar(Classes::TComponent* AOwner);
	virtual void __fastcall Initialize(void);
	virtual void __fastcall Finalize(void);
	bool __fastcall IsVariantOk(const Variant &v);
	__property Variant Value = {read=GetValue, write=SetValue};
	__property PPyObject ValueObject = {read=GetValueAsPyObject, write=SetValueFromPyObject};
	__property AnsiString ValueAsString = {read=GetValueAsString};
	__property PPyObject VarObject = {read=FVarObject, write=FVarObject};
	
__published:
	__property AnsiString Module = {read=FModule, write=FModule};
	__property AnsiString VarName = {read=FVarName, write=SetVarName};
	__property TGetDataEvent OnGetData = {read=FOnGetData, write=FOnGetData};
	__property TSetDataEvent OnSetData = {read=FOnSetData, write=FOnSetData};
	__property TExtGetDataEvent OnExtGetData = {read=FOnExtGetData, write=FOnExtGetData};
	__property TExtSetDataEvent OnExtSetData = {read=FOnExtSetData, write=FOnExtSetData};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	#pragma option push -w-inl
	/* TEngineClient.Destroy */ inline __fastcall virtual ~TPythonDelphiVar(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPyVar;
class PASCALIMPLEMENTATION TPyVar : public TPyObject 
{
	typedef TPyObject inherited;
	
public:
	Variant dv_var;
	TPythonDelphiVar* dv_component;
	PyObject *dv_object;
	__fastcall virtual TPyVar(TPythonType* APythonType);
	__fastcall virtual TPyVar(TPythonType* PythonType, PPyObject args);
	__fastcall virtual ~TPyVar(void);
	virtual PPyObject __fastcall GetAttr(char * key);
	virtual int __fastcall SetAttr(char * key, PPyObject value);
	virtual PPyObject __fastcall Repr(void);
	/* virtual class method */ virtual void __fastcall RegisterMethods(TMetaClass* vmt, TPythonType* PythonType);
	PPyObject __fastcall GetValue(void);
	Variant __fastcall GetValueAsVariant();
	void __fastcall SetValue(PPyObject value);
	void __fastcall SetValueFromVariant(const Variant &value);
};


#pragma option push -b-
enum TThreadExecMode { emNewState, emNewInterpreter };
#pragma option pop

class DELPHICLASS TPythonThread;
class PASCALIMPLEMENTATION TPythonThread : public Classes::TThread 
{
	typedef Classes::TThread inherited;
	
private:
	PyThreadState *f_savethreadstate;
	PyInterpreterState *fInterpreterState;
	PyThreadState *fThreadState;
	TThreadExecMode fThreadExecMode;
	virtual void __fastcall Execute(void);
	
protected:
	virtual void __fastcall ExecuteWithPython(void) = 0 ;
	void __fastcall Py_Begin_Allow_Threads(void);
	void __fastcall Py_End_Allow_Threads(void);
	void __fastcall Py_Begin_Block_Threads(void);
	void __fastcall Py_Begin_Unblock_Threads(void);
	
public:
	__property PPyInterpreterState InterpreterState = {read=fInterpreterState, write=fInterpreterState, default=0};
	__property PPyThreadState ThreadState = {read=fThreadState, write=fThreadState};
	__property TThreadExecMode ThreadExecMode = {read=fThreadExecMode, nodefault};
public:
	#pragma option push -w-inl
	/* TThread.Create */ inline __fastcall TPythonThread(bool CreateSuspended) : Classes::TThread(CreateSuspended) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TThread.Destroy */ inline __fastcall virtual ~TPythonThread(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TPythonVersionProp PYTHON_KNOWN_VERSIONS[7];
static const Shortint COMPILED_FOR_PYTHON_VERSION_INDEX = 0x7;
static const Shortint PYT_METHOD_BUFFER_INCREASE = 0xa;
static const Shortint METH_VARARGS = 0x1;
static const Shortint METH_KEYWORDS = 0x2;
static const Shortint CO_OPTIMIZED = 0x1;
static const Shortint CO_NEWLOCALS = 0x2;
static const Shortint CO_VARARGS = 0x4;
static const Shortint CO_VARKEYWORDS = 0x8;
static const Shortint Py_LT = 0x0;
static const Shortint Py_LE = 0x1;
static const Shortint Py_EQ = 0x2;
static const Shortint Py_NE = 0x3;
static const Shortint Py_GT = 0x4;
static const Shortint Py_GE = 0x5;
static const Shortint Py_TPFLAGS_HAVE_GETCHARBUFFER = 0x1;
static const Shortint Py_TPFLAGS_HAVE_SEQUENCE_IN = 0x2;
static const Shortint Py_TPFLAGS_GC = 0x4;
static const Shortint Py_TPFLAGS_HAVE_INPLACEOPS = 0x8;
static const Shortint Py_TPFLAGS_CHECKTYPES = 0x10;
static const Shortint Py_TPFLAGS_HAVE_RICHCOMPARE = 0x20;
static const Shortint Py_TPFLAGS_HAVE_WEAKREFS = 0x40;
static const Byte Py_TPFLAGS_HAVE_ITER = 0x80;
static const Word Py_TPFLAGS_HAVE_CLASS = 0x100;
static const Word Py_TPFLAGS_HEAPTYPE = 0x200;
static const Word Py_TPFLAGS_BASETYPE = 0x400;
static const Word Py_TPFLAGS_READY = 0x1000;
static const Word Py_TPFLAGS_READYING = 0x2000;
static const Word Py_TPFLAGS_HAVE_GC = 0x4000;
static const Word Py_TPFLAGS_DEFAULT = 0x1eb;
#define TPFLAGS_DEFAULT (System::Set<TPFlag, tpfHaveGetCharBuffer, tpfHaveGC> () << TPFlag(0) << TPFlag(1) << TPFlag(3) << TPFlag(5) << TPFlag(6) << TPFlag(7) << TPFlag(8) )
static const Word single_input = 0x100;
static const Word file_input = 0x101;
static const Word eval_input = 0x102;
static const Word funcdef = 0x103;
static const Word parameters = 0x104;
static const Word varargslist = 0x105;
static const Word fpdef = 0x106;
static const Word fplist = 0x107;
static const Word stmt = 0x108;
static const Word simple_stmt = 0x109;
static const Word small_stmt = 0x10a;
static const Word expr_stmt = 0x10b;
static const Word augassign = 0x10c;
static const Word print_stmt = 0x10d;
static const Word del_stmt = 0x10e;
static const Word pass_stmt = 0x10f;
static const Word flow_stmt = 0x110;
static const Word break_stmt = 0x111;
static const Word continue_stmt = 0x112;
static const Word return_stmt = 0x113;
static const Word raise_stmt = 0x114;
static const Word import_stmt = 0x115;
static const Word import_as_name = 0x116;
static const Word dotted_as_name = 0x117;
static const Word dotted_name = 0x118;
static const Word global_stmt = 0x119;
static const Word exec_stmt = 0x11a;
static const Word assert_stmt = 0x11b;
static const Word compound_stmt = 0x11c;
static const Word if_stmt = 0x11d;
static const Word while_stmt = 0x11e;
static const Word for_stmt = 0x11f;
static const Word try_stmt = 0x120;
static const Word except_clause = 0x121;
static const Word suite = 0x122;
static const Word test = 0x123;
static const Word and_test = 0x123;
static const Word not_test = 0x125;
static const Word comparison = 0x126;
static const Word comp_op = 0x127;
static const Word expr = 0x128;
static const Word xor_expr = 0x129;
static const Word and_expr = 0x12a;
static const Word shift_expr = 0x12b;
static const Word arith_expr = 0x12c;
static const Word term = 0x12d;
static const Word factor = 0x12e;
static const Word power = 0x12f;
static const Word atom = 0x130;
static const Word listmaker = 0x131;
static const Word lambdef = 0x132;
static const Word trailer = 0x133;
static const Word subscriptlist = 0x134;
static const Word subscript = 0x135;
static const Word sliceop = 0x136;
static const Word exprlist = 0x137;
static const Word testlist = 0x138;
static const Word dictmaker = 0x139;
static const Word classdef = 0x13a;
static const Word arglist = 0x13b;
static const Word argument = 0x13c;
static const Word list_iter = 0x13d;
static const Word list_for = 0x13e;
static const Word list_if = 0x13f;
static const short ErrInit = 0xfffffed4;
static const char CR = '\xd';
static const char LF = '\xa';
static const char TAB = '\x9';
#define CRLF "\r\n"
static const Shortint _PyDateTime_DATE_DATASIZE = 0x4;
static const Shortint _PyDateTime_TIME_DATASIZE = 0x6;
static const Shortint _PyDateTime_DATETIME_DATASIZE = 0xa;
static const Word kMaxLines = 0x3e8;
static const Word kMaxLineLength = 0x100;
#define DEFAULT_DATETIME_CONVERSION_MODE (TDatetimeConversionMode)(0)
static const Shortint kObjectOffset = 0x4;
extern PACKAGE TPyObject* __fastcall PythonToDelphi(PPyObject obj);
extern PACKAGE void __cdecl PyObjectDestructor(PPyObject pSelf);
extern PACKAGE PPyObject __cdecl pyio_write(PPyObject self, PPyObject args);
extern PACKAGE PPyObject __cdecl pyio_read(PPyObject self, PPyObject args);
extern PACKAGE PPyObject __cdecl pyio_SetDelayWrites(PPyObject self, PPyObject args);
extern PACKAGE PPyObject __cdecl pyio_SetMaxLines(PPyObject self, PPyObject args);
extern PACKAGE PPyObject __cdecl pyio_GetTypesStats(PPyObject self, PPyObject args);
extern PACKAGE TPythonEngine* __fastcall GetPythonEngine(void);
extern PACKAGE bool __fastcall PythonOK(void);
extern PACKAGE bool __fastcall IsDelphiObject(PPyObject obj);
extern PACKAGE void __fastcall Register(void);
extern PACKAGE bool __fastcall PyType_HasFeature(PPyTypeObject AType, int AFlag);

}	/* namespace Pythonengine */
using namespace Pythonengine;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PythonEngine
