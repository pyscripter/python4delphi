{$I Definition.Inc}
unit PythonTypes;

interface

uses
  Classes;

type
  TPythonVersionProp = record
    DllName      : string;
    RegVersion   : string;
    APIVersion   : Integer;
  end;

type
  TSendDataEvent = procedure (Sender: TObject; const Data : AnsiString ) of object;
  TReceiveDataEvent = procedure (Sender: TObject; var Data : AnsiString ) of object;
  TSendUniDataEvent = procedure (Sender: TObject; const Data : UnicodeString ) of object;
  TReceiveUniDataEvent = procedure (Sender: TObject; var Data : UnicodeString ) of object;
  IOChar = WideChar;
  IOString = UnicodeString;
  TIOStringList = TStringList;

type
  // Delphi equivalent used by TPyObject
  TRichComparisonOpcode = (pyLT, pyLE, pyEQ, pyNE, pyGT, pyGE);

// Delphi equivalent used by TPythonType
type
  TPFlag = (tpfHeapType, tpfBaseType, tpfReady, tpfReadying, tpfHaveGC,
            tpVectorCall, tpMethodDescriptor, tpHaveVersionTag,
            tpValidVersionTag, tpIsAbstract, tpLongSubclass,
            tpListSubClass, tpTupleSubclass, tpBytesSubclass,
            tpBaseExcSubclass, tpTypeSubclass);
  TPFlags = set of TPFlag;

type
  TPyMemberType = (mtShort, mtInt, mtLong, mtFloat, mtDouble, mtString, mtObject,
                   mtChar, mtByte, mtUByte, mtUShort, mtUInt, mtULong,
                   mtStringInplace, mtObjectEx);
  TPyMemberFlag = (mfDefault, mfReadOnly, mfReadRestricted, mfWriteRestricted, mfRestricted);

//#######################################################
//##                                                   ##
//##            Python specific interface              ##
//##                                                   ##
//#######################################################

type
  PP_frozen	    = ^P_frozen;
  P_frozen	    = ^_frozen;
  PPyObject	    = ^PyObject;
  PPPyObject	    = ^PPyObject;
  PPPPyObject	    = ^PPPyObject;
  PPyIntObject	    = ^PyIntObject;
  PPyTypeObject     = ^PyTypeObject;
  PPySliceObject    = ^PySliceObject;

  AtExitProc        = procedure;
  PyCFunction       = function( self, args:PPyObject): PPyObject; cdecl;
  PyCFunctionWithKW = function( self, args, keywords:PPyObject): PPyObject; cdecl;

  unaryfunc         = function( ob1 : PPyObject): PPyObject; cdecl;
  binaryfunc        = function( ob1,ob2 : PPyObject): PPyObject; cdecl;
  ternaryfunc       = function( ob1,ob2,ob3 : PPyObject): PPyObject; cdecl;
  inquiry           = function( ob1 : PPyObject): integer; cdecl;
  lenfunc           = function( ob1 : PPyObject): NativeInt; cdecl;
  coercion          = function( ob1,ob2 : PPPyObject): integer; cdecl;
  ssizeargfunc      = function( ob1 : PPyObject; i: NativeInt): PPyObject; cdecl;
  ssizeobjargproc   = function( ob1 : PPyObject; i: NativeInt; ob2 : PPyObject):
                                integer; cdecl;
  objobjargproc     = function( ob1,ob2,ob3 : PPyObject): integer; cdecl;

  pydestructor      = procedure(ob: PPyObject); cdecl;
  getattrfunc       = function( ob1: PPyObject; name: PAnsiChar): PPyObject; cdecl;
  setattrfunc       = function( ob1: PPyObject; name: PAnsiChar; ob2: PPyObject): integer; cdecl;
  reprfunc          = function( ob: PPyObject): PPyObject; cdecl;
  hashfunc          = function( ob: PPyObject): NativeInt; cdecl; // !! in 2.x it is still a LongInt
  getattrofunc      = function( ob1,ob2: PPyObject): PPyObject; cdecl;
  setattrofunc      = function( ob1,ob2,ob3: PPyObject): integer; cdecl;

  objobjproc        = function ( ob1, ob2: PPyObject): integer; cdecl;
  visitproc         = function ( ob1: PPyObject; ptr: Pointer): integer; cdecl;
  traverseproc      = function ( ob1: PPyObject; proc: visitproc; ptr: Pointer): integer; cdecl;

  richcmpfunc       = function ( ob1, ob2 : PPyObject; i : Integer) : PPyObject; cdecl;
  getiterfunc       = function ( ob1 : PPyObject) : PPyObject; cdecl;
  iternextfunc      = function ( ob1 : PPyObject) : PPyObject; cdecl;
  descrgetfunc      = function ( ob1, ob2, ob3 : PPyObject) : PPyObject; cdecl;
  descrsetfunc      = function ( ob1, ob2, ob3 : PPyObject) : Integer; cdecl;
  initproc          = function ( self, args, kwds : PPyObject) : Integer; cdecl;
  newfunc           = function ( subtype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
  allocfunc         = function ( self: PPyTypeObject; nitems : NativeInt) : PPyObject; cdecl;

  PyNumberMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     nb_add           : binaryfunc;
     nb_subtract      : binaryfunc;
     nb_multiply      : binaryfunc;
     nb_remainder     : binaryfunc;
     nb_divmod        : binaryfunc;
     nb_power         : ternaryfunc;
     nb_negative      : unaryfunc;
     nb_positive      : unaryfunc;
     nb_absolute      : unaryfunc;
     nb_bool          : inquiry;
     nb_invert        : unaryfunc;
     nb_lshift        : binaryfunc;
     nb_rshift        : binaryfunc;
     nb_and           : binaryfunc;
     nb_xor           : binaryfunc;
     nb_or            : binaryfunc;
     nb_int           : unaryfunc;
     nb_reserved      : Pointer;    // not used
     nb_float         : unaryfunc;
     nb_inplace_add       : binaryfunc;
     nb_inplace_subtract  : binaryfunc;
     nb_inplace_multiply  : binaryfunc;
     nb_inplace_remainder : binaryfunc;
     nb_inplace_power     : ternaryfunc;
     nb_inplace_lshift    : binaryfunc;
     nb_inplace_rshift    : binaryfunc;
     nb_inplace_and       : binaryfunc;
     nb_inplace_xor       : binaryfunc;
     nb_inplace_or        : binaryfunc;
     nb_floor_divide         : binaryfunc;
     nb_true_divide          : binaryfunc;
     nb_inplace_floor_divide : binaryfunc;
     nb_inplace_true_divide  : binaryfunc;
     nb_index                   : unaryfunc;
     nb_matrix_multiply         : binaryfunc; // new in python 3.5
     nb_inplace_matrix_multiply : binaryfunc; // new in python 3.5
  end;
  PPyNumberMethods = ^PyNumberMethods;

  PySequenceMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     sq_length    : lenfunc;
     sq_concat    : binaryfunc;
     sq_repeat    : ssizeargfunc;
     sq_item      : ssizeargfunc;
     was_sq_slice : Pointer;  // empty slot in python 3.x
     sq_ass_item  : ssizeobjargproc;
     was_sq_ass_slice  : Pointer; // empty slot in python 3.x
     sq_contains       : objobjproc;
     sq_inplace_concat : binaryfunc;
     sq_inplace_repeat : ssizeargfunc;
  end;
  PPySequenceMethods = ^PySequenceMethods;

  PyMappingMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     mp_length	      : lenfunc;
     mp_subscript     : binaryfunc;
     mp_ass_subscript : objobjargproc;
  end;
  PPyMappingMethods = ^PyMappingMethods;

  Py_complex =  {$IFNDEF CPUX64}packed{$ENDIF} record
     real : double;
     imag : double;
  end;

  PyObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt: NativeInt;
    ob_type:   PPyTypeObject;
  end;

  PyIntObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    ob_ival   : LongInt;
  end;

  _frozen = {$IFNDEF CPUX64}packed{$ENDIF} record
     name	: PAnsiChar;
     code	: PByte;
     size	: Integer;
  end;

  PySliceObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt:          NativeInt;
    ob_type:            PPyTypeObject;
    start, stop, step:  PPyObject;
  end;

  PPyMethodDef = ^PyMethodDef;
  PyMethodDef  = {$IFNDEF CPUX64}packed{$ENDIF} record
     ml_name:  PAnsiChar;
     ml_meth:  PyCFunction;
     ml_flags: Integer;
     ml_doc:   PAnsiChar;
  end;

  // structmember.h
  PPyMemberDef = ^PyMemberDef;
  PyMemberDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    _type : integer;
    offset : NativeInt;
    flags : integer;
    doc : PAnsiChar;
  end;

  // descrobject.h

  // Descriptors

  getter = function ( obj : PPyObject; context : Pointer) : PPyObject; cdecl;
  setter = function ( obj, value : PPyObject; context : Pointer) : integer; cdecl;

  PPyGetSetDef = ^PyGetSetDef;
  PyGetSetDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    get : getter;
    _set : setter;
    doc : PAnsiChar;
    closure : Pointer;
  end;

  wrapperfunc = function (self, args: PPyObject; wrapped : Pointer) : PPyObject; cdecl;

  pwrapperbase = ^wrapperbase;
  wrapperbase = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    wrapper : wrapperfunc;
    doc : PAnsiChar;
  end;

  // Various kinds of descriptor objects

  {#define PyDescr_COMMON \
          PyObject_HEAD \
          PyTypeObject *d_type; \
          PyObject *d_name
  }

  PPyDescrObject = ^PyDescrObject;
  PyDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
  end;

  PPyMethodDescrObject = ^PyMethodDescrObject;
  PyMethodDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_method : PPyMethodDef;
  end;

  PPyMemberDescrObject = ^PyMemberDescrObject;
  PyMemberDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_member : PPyMemberDef;
  end;

  PPyGetSetDescrObject = ^PyGetSetDescrObject;
  PyGetSetDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_getset : PPyGetSetDef;
  end;

  PPyWrapperDescrObject = ^PyWrapperDescrObject;
  PyWrapperDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_base : pwrapperbase;
    d_wrapped : Pointer; // This can be any function pointer
  end;

  PPyModuleDef_Base = ^PyModuleDef_Base;
  PyModuleDef_Base = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    m_init     : function( ) : PPyObject; cdecl;
    m_index     : NativeInt;
    m_copy : PPyObject;
  end;

  PPyModuleDef = ^PyModuleDef;
  PyModuleDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    m_base : PyModuleDef_Base;
    m_name : PAnsiChar;
    m_doc : PAnsiChar;
    m_size : NativeInt;
    m_methods : PPyMethodDef;
    m_reload : inquiry;
    m_traverse : traverseproc;
    m_clear : inquiry;
    m_free : inquiry;
  end;

  // object.h
  PyTypeObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt:      NativeInt;
    ob_type:        PPyTypeObject;
    ob_size:        NativeInt; // Number of items in variable part
    tp_name:        PAnsiChar; // For printing
    tp_basicsize, tp_itemsize: NativeInt; // For allocation

    // Methods to implement standard operations

    tp_dealloc:           pydestructor;
    tp_vectorcall_offset: NativeInt;
    tp_getattr:           getattrfunc;
    tp_setattr:           setattrfunc;
    tp_as_async:          Pointer;  // not implemented
    tp_repr:              reprfunc;

    // Method suites for standard classes

    tp_as_number:   PPyNumberMethods;
    tp_as_sequence: PPySequenceMethods;
    tp_as_mapping:  PPyMappingMethods;

    // More standard operations (here for binary compatibility)

    tp_hash:        hashfunc;
    tp_call:        ternaryfunc;
    tp_str:         reprfunc;
    tp_getattro:    getattrofunc;
    tp_setattro:    setattrofunc;

    // Functions to access object as input/output buffer
    tp_as_buffer:   Pointer; // PPyBufferProcs - not implemented
    // Flags to define presence of optional/expanded features
    tp_flags:       LongInt;

    tp_doc:         PAnsiChar; // Documentation string

    // call function for all accessible objects
    tp_traverse:    traverseproc;

    // delete references to contained objects
    tp_clear:       inquiry;

    // rich comparisons
    tp_richcompare: richcmpfunc;

    // weak reference enabler
    tp_weaklistoffset: NativeInt;
    // Iterators
    tp_iter : getiterfunc;
    tp_iternext : iternextfunc;

    // Attribute descriptor and subclassing stuff
    tp_methods          : PPyMethodDef;
    tp_members          : PPyMemberDef;
    tp_getset           : PPyGetSetDef;
    tp_base             : PPyTypeObject;
    tp_dict             : PPyObject;
    tp_descr_get        : descrgetfunc;
    tp_descr_set        : descrsetfunc;
    tp_dictoffset       : NativeInt;
    tp_init             : initproc;
    tp_alloc            : allocfunc;
    tp_new              : newfunc;
    tp_free             : pydestructor; // Low-level free-memory routine
    tp_is_gc            : inquiry; // For PyObject_IS_GC
    tp_bases            : PPyObject;
    tp_mro              : PPyObject; // method resolution order
    tp_cache            : PPyObject;
    tp_subclasses       : PPyObject;
    tp_weaklist         : PPyObject;
    tp_del              : PyDestructor;
    tp_version_tag      : Cardinal;  // Type attribute cache version tag. Added in version 2.6
    tp_finalize         : PyDestructor;
    tp_vectorcall       : Pointer;   // not implemented
    //More spares
    tp_xxx1             : NativeInt;
    tp_xxx2             : NativeInt;
    tp_xxx3             : NativeInt;
    tp_xxx4             : NativeInt;
    tp_xxx5             : NativeInt;
    tp_xxx6             : NativeInt;
    tp_xxx7             : NativeInt;
    tp_xxx8             : NativeInt;
    tp_xxx9             : NativeInt;
    tp_xxx10            : NativeInt;
  end;

  // from pystate.h
  // the structure of PyInterpreterState and PyThreadState is considered
  // an implementation detail.  It has been changing between python versions
  // and there is no real use of accessing these structures directly.
  PPyInterpreterState = Pointer;
  PPyThreadState = Pointer;

  // Parse tree node interface

  PNode = ^node;
  node = {$IFNDEF CPUX64}packed{$ENDIF} record
    n_type      : smallint;
    n_str       : PAnsiChar;
    n_lineno    : integer;
    n_col_offset: integer;
    n_nchildren : integer;
    n_child     : PNode;
  end;

  PPyCompilerFlags = ^PyCompilerFlags;
  PyCompilerFlags = {$IFNDEF CPUX64}packed{$ENDIF} record
    flags : integer;
    cf_feature_version : integer;  //added in Python 3.8
  end;

  // from datetime.h


{* Fields are packed into successive bytes, each viewed as unsigned and
 * big-endian, unless otherwise noted:
 *
 * byte offset
 *  0 		year     2 bytes, 1-9999
 *  2	  	month    1 byte,  1-12
 *  3 		day      1 byte,  1-31
 *  4     hour     1 byte,  0-23
 *  5 		minute   1 byte,  0-59
 *  6 		second   1 byte,  0-59
 *  7 		usecond  3 bytes, 0-999999
 * 10
 *}

 const
  { # of bytes for year, month, and day. }
  _PyDateTime_DATE_DATASIZE = 4;

  { # of bytes for hour, minute, second, and usecond. }
  _PyDateTime_TIME_DATASIZE = 6;

  { # of bytes for year, month, day, hour, minute, second, and usecond. }
  _PyDateTime_DATETIME_DATASIZE = 10;
type
  PyDateTime_Delta = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode    : NativeInt;  // -1 when unknown
    days        : Integer;  // -MAX_DELTA_DAYS <= days <= MAX_DELTA_DAYS
    seconds     : Integer;  // 0 <= seconds < 24*3600 is invariant
    microseconds: Integer;  // 0 <= microseconds < 1000000 is invariant
  end;
  PPyDateTime_Delta = ^PyDateTime_Delta;

  PyDateTime_TZInfo = {$IFNDEF CPUX64}packed{$ENDIF} record // a pure abstract base clase
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
  end;
  PPyDateTime_TZInfo = ^PyDateTime_TZInfo;

{
/* The datetime and time types have hashcodes, and an optional tzinfo member,
 * present if and only if hastzinfo is true.
 */
#define _PyTZINFO_HEAD		\
	PyObject_HEAD		\
	long hashcode;		\
	char hastzinfo;		/* boolean flag */
}

{* No _PyDateTime_BaseTZInfo is allocated; it's just to have something
 * convenient to cast to, when getting at the hastzinfo member of objects
 * starting with _PyTZINFO_HEAD.
 *}
  _PyDateTime_BaseTZInfo = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
  end;
  _PPyDateTime_BaseTZInfo = ^_PyDateTime_BaseTZInfo;

{* All time objects are of PyDateTime_TimeType, but that can be allocated
 * in two ways, with or without a tzinfo member.  Without is the same as
 * tzinfo == None, but consumes less memory.  _PyDateTime_BaseTime is an
 * internal struct used to allocate the right amount of space for the
 * "without" case.
 *}
{#define _PyDateTime_TIMEHEAD	\
	_PyTZINFO_HEAD		\
	unsigned char data[_PyDateTime_TIME_DATASIZE];
}

  _PyDateTime_BaseTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo false
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
  end;
  _PPyDateTime_BaseTime = ^_PyDateTime_BaseTime;

  PyDateTime_Time = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo true
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
    tzinfo     : PPyObject;
  end;
  PPyDateTime_Time = ^PyDateTime_Time;

{* All datetime objects are of PyDateTime_DateTimeType, but that can be
 * allocated in two ways too, just like for time objects above.  In addition,
 * the plain date type is a base class for datetime, so it must also have
 * a hastzinfo member (although it's unused there).
 *}
  PyDateTime_Date = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATE_DATASIZE)] of Byte;
  end;
  PPyDateTime_Date = ^PyDateTime_Date;

 {
#define _PyDateTime_DATETIMEHEAD	\
	_PyTZINFO_HEAD			\
	unsigned char data[_PyDateTime_DATETIME_DATASIZE];
}

  _PyDateTime_BaseDateTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo false
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
  end;
  _PPyDateTime_BaseDateTime = ^_PyDateTime_BaseDateTime;

  PyDateTime_DateTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo true
    // Start of _PyDateTime_DATETIMEHEAD
      // Start of _PyTZINFO_HEAD
        // Start of the Head of an object
        ob_refcnt  : NativeInt;
        ob_type    : PPyTypeObject;
        // End of the Head of an object
      hashcode   : Integer;
      hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
      data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
    // End of _PyDateTime_DATETIMEHEAD
    tzinfo : PPyObject;
  end;
  PPyDateTime_DateTime = ^PyDateTime_DateTime;

//#######################################################
//##                                                   ##
//##         GIL state                                 ##
//##                                                   ##
//#######################################################
  PyGILState_STATE = (PyGILState_LOCKED, PyGILState_UNLOCKED);

  (*$HPPEMIT 'typedef int __cdecl (*TPyArg_Parse)(void * args, char * format, ...);' *)
  TPyArg_Parse = function( args: PPyObject; format: PAnsiChar {;....}) :  Integer; cdecl varargs;
  {$EXTERNALSYM TPyArg_Parse}

  (*$HPPEMIT 'typedef int __cdecl (*TPyArg_ParseTupleAndKeywords)(void * args, void * kw, char * format, char** kwargs, ...);' *)
  TPyArg_ParseTupleAndKeywords = function( args: PPyObject; kw: PPyObject; format: PAnsiChar; kwargs: PPAnsiChar {;...}): Integer; cdecl varargs;
  {$EXTERNALSYM TPyArg_ParseTupleAndKeywords}

  (*$HPPEMIT 'typedef int __cdecl (*TPy_BuildValue)(char * format, ...);' *)
  TPy_BuildValue = function( format: PAnsiChar {;...}): Pointer; cdecl varargs;
  {$EXTERNALSYM TPy_BuildValue}

type
  TDatetimeConversionMode = (dcmToTuple, dcmToDatetime);

  TPathInitializationEvent = procedure ( Sender : TObject; var Path : string ) of Object;
  TSysPathInitEvent = procedure ( Sender : TObject; PathList : PPyObject ) of Object;
  TPythonFlag = (pfDebug, pfInteractive, pfNoSite, pfOptimize, pfVerbose,
                 pfFrozenFlag, pfIgnoreEnvironmentFlag);
  TPythonFlags = set of TPythonFlag;

  TDelphiMethod = function ( self, args : PPyObject ) : PPyObject of object; cdecl;
  TDelphiMethodWithKW = function ( self, args, keywords : PPyObject ) : PPyObject of object; cdecl;
  TPythonEvent = procedure(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject) of object;

  TErrorType = (etString, etClass);

  TBasicServices     = set of (bsGetAttr, bsSetAttr,
                               bsRepr, bsCompare, bsHash,
                               bsStr, bsGetAttrO, bsSetAttrO,
                               bsCall,
                               // since version 2.0
                               bsTraverse, bsClear,
                               // since version 2.1
                               bsRichCompare,
                               // since version 2.2
                               bsIter, bsIterNext);
  TNumberServices    = set of (nsAdd, nsSubtract, nsMultiply,
                               nsRemainder, nsDivmod,
                               nsPower, nsNegative, nsPositive,
                               nsAbsolute, nsInvert,
                               nsLShift, nsRShift, nsAnd,
                               nsXor, nsOr,
                               nsInt, nsFloat,
                               nsFloorDivide, nsTrueDivide,
                               // since version 3.0
                               nsMatrixMultiply, nsBool);

  // TInplaceNumberServices exists since version 2.0
  TInplaceNumberServices = set of (nsInplaceAdd, nsInplaceSubtract,
                                   nsInplaceMultiply,
                                   nsInplaceRemainder, nsInplacePower,
                                   nsInplaceLShift, nsInplaceRShift,
                                   nsInplaceAnd, nsInplaceXor, nsInplaceOr,
                                   nsInplaceFloorDivide, nsInplaceTrueDivide,
                                   // since version 3.0
                                   nsInplaceMatrixMultiply);

  TSequenceServices  = set of (ssLength, ssConcat, ssRepeat,
                               ssItem, ssAssItem,
                               ssContains, ssInplaceConcat,
                               ssInplaceRepeat
                               );

  TMappingServices   = set of (msLength, msSubscript, msAssSubscript);

  TGetDataEvent = procedure ( Sender : TObject; var Data : Variant ) of Object;
  TSetDataEvent = procedure ( Sender : TObject; Data : Variant ) of Object;
  TExtGetDataEvent = procedure ( Sender : TObject; var Data : PPyObject ) of Object;
  TExtSetDataEvent = procedure ( Sender : TObject; Data : PPyObject) of Object;

  TThreadExecMode = (emNewState, emNewInterpreter);

implementation

end.
