{$I Definition.Inc}
unit PythonConsts;

interface

const
  kMaxLines = 1000;
  kMaxLineLength = 256;

//#######################################################
//##                                                   ##
//##           PYTHON specific constants               ##
//##                                                   ##
//#######################################################
const
  PYT_METHOD_BUFFER_INCREASE = 10;
  PYT_MEMBER_BUFFER_INCREASE = 10;
  PYT_GETSET_BUFFER_INCREASE = 10;

  METH_VARARGS  = $0001;
  METH_KEYWORDS = $0002;

  // Masks for the co_flags field of PyCodeObject
  CO_OPTIMIZED   = $0001;
  CO_NEWLOCALS   = $0002;
  CO_VARARGS     = $0004;
  CO_VARKEYWORDS = $0008;

  // Rich comparison opcodes introduced in version 2.1
  Py_LT = 0;
  Py_LE = 1;
  Py_EQ = 2;
  Py_NE = 3;
  Py_GT = 4;
  Py_GE = 5;

const
{
Type flags (tp_flags)

These flags are used to change expected features and behavior for a
particular type.

Arbitration of the flag bit positions will need to be coordinated among
all extension writers who publicly release their extensions (this will
be fewer than you might expect!).

Most flags were removed as of Python 3.0 to make room for new flags.  (Some
flags are not for backwards compatibility but to indicate the presence of an
optional feature; these flags remain of course.)

Type definitions should use Py_TPFLAGS_DEFAULT for their tp_flags value.

Code can use PyType_HasFeature(type_ob, flag_value) to test whether the
given type object has a specified feature.
}

// Set if the type object is dynamically allocated
  Py_TPFLAGS_HEAPTYPE = (1 shl 9);

// Set if the type allows subclassing
  Py_TPFLAGS_BASETYPE = (1 shl 10);

// Set if the type is 'ready' -- fully initialized
  Py_TPFLAGS_READY = (1 shl 12);

// Set while the type is being 'readied', to prevent recursive ready calls
  Py_TPFLAGS_READYING = (1 shl 13);

// Objects support garbage collection (see objimp.h)
  Py_TPFLAGS_HAVE_GC = (1 shl 14);

// Set if the type implements the vectorcall protocol (PEP 590) */
  _Py_TPFLAGS_HAVE_VECTORCALL = (1 shl 11);

// Objects behave like an unbound method
  Py_TPFLAGS_METHOD_DESCRIPTOR = (1 shl 17);

// Objects support type attribute cache
  Py_TPFLAGS_HAVE_VERSION_TAG = (1 shl 18);
  Py_TPFLAGS_VALID_VERSION_TAG = (1 shl 19);

// Type is abstract and cannot be instantiated
  Py_TPFLAGS_IS_ABSTRACT = (1 shl 20);

// These flags are used to determine if a type is a subclass.
  Py_TPFLAGS_LONG_SUBCLASS       = (1 shl 24);
  Py_TPFLAGS_LIST_SUBCLASS       = (1 shl 25);
  Py_TPFLAGS_TUPLE_SUBCLASS      = (1 shl 26);
  Py_TPFLAGS_BYTES_SUBCLASS      = (1 shl 27);
  Py_TPFLAGS_UNICODE_SUBCLASS    = (1 shl 28);
  Py_TPFLAGS_DICT_SUBCLASS       = (1 shl 29);
  Py_TPFLAGS_BASE_EXC_SUBCLASS   = (1 shl 30);
  Py_TPFLAGS_TYPE_SUBCLASS       = (1 shl 31);

  Py_TPFLAGS_DEFAULT  = Py_TPFLAGS_BASETYPE or Py_TPFLAGS_HAVE_VERSION_TAG;

//-------  Python opcodes  ----------//
Const
   single_input                     = 256;
   file_input                       = 257;
   eval_input                       = 258;

  // structmember.h
const
//* Types */
  T_SHORT                       = 0;
  T_INT                         = 1;
  T_LONG                        = 2;
  T_FLOAT                       = 3;
  T_DOUBLE                      = 4;
  T_STRING                      = 5;
  T_OBJECT                      = 6;
//* XXX the ordering here is weird for binary compatibility */
  T_CHAR                        = 7;	//* 1-character string */
  T_BYTE                        = 8;	//* 8-bit signed int */
//* unsigned variants: */
  T_UBYTE                       = 9;
  T_USHORT                      = 10;
  T_UINT                        = 11;
  T_ULONG                       = 12;

//* Added by Jack: strings contained in the structure */
  T_STRING_INPLACE= 13;

  T_OBJECT_EX                   = 16;{* Like T_OBJECT, but raises AttributeError
                                        when the value is NULL, instead of
                                        converting to None. *}

//* Flags */
  READONLY                      = 1;
  RO                            = READONLY;		//* Shorthand */
  READ_RESTRICTED               = 2;
  PY_WRITE_RESTRICTED           = 4;
  RESTRICTED                    = (READ_RESTRICTED or PY_WRITE_RESTRICTED);

//#######################################################
//##                                                   ##
//##           Non-Python specific constants           ##
//##                                                   ##
//#######################################################

const
  CR              = #13;
  LF              = #10;
  TAB             = #09;
  CRLF            = CR+LF;

implementation

end.
