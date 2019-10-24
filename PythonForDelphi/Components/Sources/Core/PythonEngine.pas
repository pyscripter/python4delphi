(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PythonEngine'     Copyright (c) 1997                    *)
(*                                                                        *)
(* Version: 3.0                     Dr. Dietmar Budelsky                  *)
(* Sub-Version: 0.33                dbudelsky@web.de                      *)
(*                                  Germany                               *)
(*                                                                        *)
(*                                  Morgan Martinet                       *)
(*                                  4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(*  look at the project page at: http://python4Delphi.googlecode.com/     *)
(**************************************************************************)
(*  Functionality:  Delphi Components that provide an interface to the    *)
(*                  Python language (see python.txt for more infos on     *)
(*                  Python itself).                                       *)
(*                                                                        *)
(**************************************************************************)
(*  Contributors:                                                         *)
(*      Grzegorz Makarewicz (mak@mikroplan.com.pl)                        *)
(*      Andrew Robinson (andy@hps1.demon.co.uk)                           *)
(*      Mark Watts(mark_watts@hotmail.com)                                *)
(*      Olivier Deckmyn (olivier.deckmyn@mail.dotcom.fr)                  *)
(*      Sigve Tjora (public@tjora.no)                                     *)
(*      Mark Derricutt (mark@talios.com)                                  *)
(*      Igor E. Poteryaev (jah@mail.ru)                                   *)
(*      Yuri Filimonov (fil65@mail.ru)                                    *)
(*      Stefan Hoffmeister (Stefan.Hoffmeister@Econos.de)                 *)
(*      Michiel du Toit (micdutoit@hsbfn.com) - Lazarus Port              *)
(*      Chris Nicolai (nicolaitanes@gmail.com)                            *)
(*      Kiriakos Vlahos (pyscripter@gmail.com)                            *)
(*      Andrey Gruzdev      (andrey.gruzdev@gmail.com)                    *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(* Dr. Dietmar Budelsky, 1997-11-17                                       *)
(**************************************************************************)

{$I Definition.Inc}

unit PythonEngine;

{ TODO -oMMM : implement tp_as_buffer slot }
{ TODO -oMMM : implement Attribute descriptor and subclassing stuff }

{$IFNDEF FPC}
  {$IFNDEF DELPHI7_OR_HIGHER}
      Error! Delphi 7 or higher is required!
  {$ENDIF}
{$ENDIF}

interface

uses
  Types,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
{$IFDEF FPC}
  dynlibs,
{$ELSE}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  Classes,
  SysUtils,
  SyncObjs,
  Variants,
{$IFDEF DELPHI2005_OR_HIGHER}
{$IFNDEF UNICODE}
  WideStrings,
{$ENDIF}
{$ELSE}
  TinyWideStrings,
{$ENDIF}
  MethodCallBack;

//#######################################################
//##                                                   ##
//##           PYTHON specific constants               ##
//##                                                   ##
//#######################################################

type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
  TUnicodeStringList = TWideStringList;
{$ELSE}
  TUnicodeStringList = TStringList;
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion < 21}
    NativeInt = integer;
    NativeUInt = Cardinal;
  {$IFEND}
  PNativeInt = ^NativeInt;
{$ELSE}
  {$IF DEFINED(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20500)}
  {$ELSE}
    NativeInt = integer;
    NativeUInt = Cardinal;
  {$IFEND}
  PNativeInt = ^NativeInt;
{$ENDIF}

  TPythonVersionProp = packed record
    DllName      : String;
    RegVersion   : String;
    APIVersion   : Integer;
    CanUseLatest : Boolean;
  end;
const
{$IFDEF MSWINDOWS}
  PYTHON_KNOWN_VERSIONS: array[1..10] of TPythonVersionProp =
    (
    (DllName: 'python25.dll'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python26.dll'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python27.dll'; RegVersion: '2.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python32.dll'; RegVersion: '3.2'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python33.dll'; RegVersion: '3.3'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python34.dll'; RegVersion: '3.4'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python35.dll'; RegVersion: '3.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python36.dll'; RegVersion: '3.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python37.dll'; RegVersion: '3.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'python38.dll'; RegVersion: '3.8'; APIVersion: 1013; CanUseLatest: True)
    );
{$ENDIF}
{$IFDEF LINUX}
  PYTHON_KNOWN_VERSIONS: array[1..10] of TPythonVersionProp =
    (
    (DllName: 'libpython2.5.so'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.6.so'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.7.so'; RegVersion: '2.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.2.so'; RegVersion: '3.2'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.3.so'; RegVersion: '3.3'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.4.so'; RegVersion: '3.4'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.5.so'; RegVersion: '3.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.6.so'; RegVersion: '3.6'; APIVersion: 1013; CanUseLatest: True)
    (DllName: 'libpython3.7.so'; RegVersion: '3.7'; APIVersion: 1013; CanUseLatest: True)
    (DllName: 'libpython3.8.so'; RegVersion: '3.8'; APIVersion: 1013; CanUseLatest: True)
    );
{$ENDIF}
{$IFDEF DARWIN}
  PYTHON_KNOWN_VERSIONS: array[1..10] of TPythonVersionProp =
    (
    (DllName: 'libpython2.5.dylib'; RegVersion: '2.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.6.dylib'; RegVersion: '2.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython2.7.dylib'; RegVersion: '2.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.2.dylib'; RegVersion: '3.2'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.3.dylib'; RegVersion: '3.3'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.4.dylib'; RegVersion: '3.4'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.5.dylib'; RegVersion: '3.5'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.6.dylib'; RegVersion: '3.6'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.7.dylib'; RegVersion: '3.7'; APIVersion: 1013; CanUseLatest: True),
    (DllName: 'libpython3.8.dylib'; RegVersion: '3.8'; APIVersion: 1013; CanUseLatest: True)
    );
{$endif}
{$IFDEF PYTHON25}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 1;
{$ENDIF}
{$IFDEF PYTHON26}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 2;
{$ENDIF}
{$IFDEF PYTHON27}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 3;
{$ENDIF}
{$IFDEF PYTHON32}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 4;
{$ENDIF}
{$IFDEF PYTHON33}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 5;
{$ENDIF}
{$IFDEF PYTHON34}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 6;
{$ENDIF}
{$IFDEF PYTHON35}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 7;
{$ENDIF}
{$IFDEF PYTHON36}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 8;
{$ENDIF}
{$IFDEF PYTHON37}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 9;
{$ENDIF}
{$IFDEF PYTHON38}
  COMPILED_FOR_PYTHON_VERSION_INDEX = 10;
{$ENDIF}
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
type
  // Delphi equivalent used by TPyObject
  TRichComparisonOpcode = (pyLT, pyLE, pyEQ, pyNE, pyGT, pyGE);
const
{Type flags (tp_flags) introduced in version 2.0

These flags are used to extend the type structure in a backwards-compatible
fashion. Extensions can use the flags to indicate (and test) when a given
type structure contains a new feature. The Python core will use these when
introducing new functionality between major revisions (to avoid mid-version
changes in the PYTHON_API_VERSION).

Arbitration of the flag bit positions will need to be coordinated among
all extension writers who publically release their extensions (this will
be fewer than you might expect!)..

Python 1.5.2 introduced the bf_getcharbuffer slot into PyBufferProcs.

Type definitions should use Py_TPFLAGS_DEFAULT for their tp_flags value.

Code can use PyType_HasFeature(type_ob, flag_value) to test whether the
given type object has a specified feature.
}

// PyBufferProcs contains bf_getcharbuffer
  Py_TPFLAGS_HAVE_GETCHARBUFFER = (1 shl 0);

// PySequenceMethods contains sq_contains
  Py_TPFLAGS_HAVE_SEQUENCE_IN = (1 shl 1);

// Objects which participate in garbage collection (see objimp.h)
  Py_TPFLAGS_GC = (1 shl 2);

// PySequenceMethods and PyNumberMethods contain in-place operators
  Py_TPFLAGS_HAVE_INPLACEOPS = (1 shl 3);

// PyNumberMethods do their own coercion */
  Py_TPFLAGS_CHECKTYPES = (1 shl 4);

  Py_TPFLAGS_HAVE_RICHCOMPARE = (1 shl 5);

// Objects which are weakly referencable if their tp_weaklistoffset is >0
// XXX Should this have the same value as Py_TPFLAGS_HAVE_RICHCOMPARE?
// These both indicate a feature that appeared in the same alpha release.

  Py_TPFLAGS_HAVE_WEAKREFS = (1 shl 6);

// tp_iter is defined
  Py_TPFLAGS_HAVE_ITER = (1 shl 7);

// New members introduced by Python 2.2 exist
  Py_TPFLAGS_HAVE_CLASS = (1 shl 8);

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

  Py_TPFLAGS_DEFAULT  =      Py_TPFLAGS_HAVE_GETCHARBUFFER
                             or Py_TPFLAGS_HAVE_SEQUENCE_IN
                             or Py_TPFLAGS_HAVE_INPLACEOPS
                             or Py_TPFLAGS_HAVE_RICHCOMPARE
                             or Py_TPFLAGS_HAVE_WEAKREFS
                             or Py_TPFLAGS_HAVE_ITER
                             or Py_TPFLAGS_HAVE_CLASS
                             or Py_TPFLAGS_BASETYPE
                             ;

// See function PyType_HasFeature below for testing the flags.

// Delphi equivalent used by TPythonType
type
  TPFlag = (tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfGC, tpfHaveInplaceOps,
            tpfCheckTypes, tpfHaveRichCompare, tpfHaveWeakRefs
            ,tpfHaveIter, tpfHaveClass, tpfHeapType, tpfBaseType, tpfReady, tpfReadying, tpfHaveGC
            );
  TPFlags = set of TPFlag;
const
  TPFLAGS_DEFAULT = [tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfHaveInplaceOps,
                     tpfHaveRichCompare, tpfHaveWeakRefs, tpfHaveIter,
                     tpfHaveClass, tpfBaseType
                    ];
//-------  Python opcodes  ----------//
Const
   single_input                     = 256;
   file_input                       = 257;
   eval_input                       = 258;
   p4d_funcdef                      = 259;
   p4d_parameters                   = 260;
   p4d_varargslist                  = 261;
   p4d_fpdef                        = 262;
   p4d_fplist                       = 263;
   p4d_stmt                         = 264;
   p4d_simple_stmt                  = 265;
   p4d_small_stmt                   = 266;
   p4d_expr_stmt                    = 267;
   p4d_augassign                    = 268;
   p4d_print_stmt                   = 269;
   p4d_del_stmt                     = 270;
   p4d_pass_stmt                    = 271;
   p4d_flow_stmt                    = 272;
   p4d_break_stmt                   = 273;
   p4d_continue_stmt                = 274;
   p4d_return_stmt                  = 275;
   p4d_raise_stmt                   = 276;
   p4d_import_stmt                  = 277;
   p4d_import_as_name               = 278;
   p4d_dotted_as_name               = 279;
   p4d_dotted_name                  = 280;
   p4d_global_stmt                  = 281;
   p4d_exec_stmt                    = 282;
   p4d_assert_stmt                  = 283;
   p4d_compound_stmt                = 284;
   p4d_if_stmt                      = 285;
   p4d_while_stmt                   = 286;
   p4d_for_stmt                     = 287;
   p4d_try_stmt                     = 288;
   p4d_except_clause                = 289;
   p4d_suite                        = 290;
   p4d_test                         = 291;
   p4d_and_test                     = 291;
   p4d_not_test                     = 293;
   p4d_comparison                   = 294;
   p4d_comp_op                      = 295;
   p4d_expr                         = 296;
   p4d_xor_expr                     = 297;
   p4d_and_expr                     = 298;
   p4d_shift_expr                   = 299;
   p4d_arith_expr                   = 300;
   p4d_term                         = 301;
   p4d_factor                       = 302;
   p4d_power                        = 303;
   p4d_atom                         = 304;
   p4d_listmaker                    = 305;
   p4d_lambdef                      = 306;
   p4d_trailer                      = 307;
   p4d_subscriptlist                = 308;
   p4d_subscript                    = 309;
   p4d_sliceop                      = 310;
   p4d_exprlist                     = 311;
   p4d_testlist                     = 312;
   p4d_dictmaker                    = 313;
   p4d_classdef                     = 314;
   p4d_arglist                      = 315;
   p4d_argument                     = 316;
   p4d_list_iter                    = 317;
   p4d_list_for                     = 318;
   p4d_list_if                      = 319;

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
  WRITE_RESTRICTED              = 4;
  RESTRICTED                    = (READ_RESTRICTED or WRITE_RESTRICTED);
type
  TPyMemberType = (mtShort, mtInt, mtLong, mtFloat, mtDouble, mtString, mtObject,
                   mtChar, mtByte, mtUByte, mtUShort, mtUInt, mtULong,
                   mtStringInplace, mtObjectEx);
  TPyMemberFlag = (mfDefault, mfReadOnly, mfReadRestricted, mfWriteRestricted, mfRestricted);

//#######################################################
//##                                                   ##
//##           Non-Python specific constants           ##
//##                                                   ##
//#######################################################

const
  ErrInit         = -300;
  CR              = #13;
  LF              = #10;
  TAB             = #09;
  CRLF            = CR+LF;



//#######################################################
//##                                                   ##
//##    Global declarations, nothing Python specific   ##
//##                                                   ##
//#######################################################

type
   TPAnsiChar     = array[0..16000] of PAnsiChar;
   TPWideChar = array[0..16000] of PWideChar;
   PPAnsiChar     = ^TPAnsiChar;
   PPWideChar = ^TPWideChar;
   PInt	      = ^Integer;
   PDouble    = ^Double;
   PFloat     = ^Real;
   PLong      = ^LongInt;
   PShort     = ^ShortInt;


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
  ssizessizeargfunc = function( ob1 : PPyObject; i1, i2: NativeInt):
                                PPyObject; cdecl;
  ssizeobjargproc   = function( ob1 : PPyObject; i: NativeInt; ob2 : PPyObject):
                                integer; cdecl;
  ssizessizeobjargproc = function( ob1: PPyObject; i1, i2: NativeInt;
                                ob2: PPyObject): integer; cdecl;
  objobjargproc     = function( ob1,ob2,ob3 : PPyObject): integer; cdecl;

  pydestructor      = procedure(ob: PPyObject); cdecl;
  printfunc         = function( ob: PPyObject; var f: file; i: integer): integer; cdecl;
  getattrfunc       = function( ob1: PPyObject; name: PAnsiChar): PPyObject; cdecl;
  setattrfunc       = function( ob1: PPyObject; name: PAnsiChar; ob2: PPyObject): integer; cdecl;
  cmpfunc           = function( ob1,ob2: PPyObject): integer; cdecl;
  reprfunc          = function( ob: PPyObject): PPyObject; cdecl;
  hashfunc          = function( ob: PPyObject): NativeInt; cdecl; // !! in 2.x it is still a LongInt
  getattrofunc      = function( ob1,ob2: PPyObject): PPyObject; cdecl;
  setattrofunc      = function( ob1,ob2,ob3: PPyObject): integer; cdecl;

  getreadbufferproc = function ( ob1: PPyObject; i: NativeInt; ptr: Pointer): NativeInt; cdecl;
  getwritebufferproc= function ( ob1: PPyObject; i: NativeInt; ptr: Pointer): NativeInt; cdecl;
  getsegcountproc   = function ( ob1: PPyObject; i: NativeInt): NativeInt; cdecl;
  getcharbufferproc = function ( ob1: PPyObject; i: NativeInt; const pstr: PAnsiChar): NativeInt; cdecl;
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
     nb_substract     : binaryfunc;
     nb_multiply      : binaryfunc;
     nb_divide        : binaryfunc;
     nb_remainder     : binaryfunc;
     nb_divmod        : binaryfunc;
     nb_power         : ternaryfunc;
     nb_negative      : unaryfunc;
     nb_positive      : unaryfunc;
     nb_absolute      : unaryfunc;
     nb_nonzero       : inquiry;
     nb_invert        : unaryfunc;
     nb_lshift        : binaryfunc;
     nb_rshift        : binaryfunc;
     nb_and           : binaryfunc;
     nb_xor           : binaryfunc;
     nb_or            : binaryfunc;
     nb_coerce        : coercion;
     nb_int           : unaryfunc;
     nb_long          : unaryfunc;
     nb_float         : unaryfunc;
     nb_oct           : unaryfunc;
     nb_hex           : unaryfunc;

    // Updated for python 2.0
     nb_inplace_add       : binaryfunc;
     nb_inplace_subtract  : binaryfunc;
     nb_inplace_multiply  : binaryfunc;
     nb_inplace_divide    : binaryfunc;
     nb_inplace_remainder : binaryfunc;
     nb_inplace_power     : ternaryfunc;
     nb_inplace_lshift    : binaryfunc;
     nb_inplace_rshift    : binaryfunc;
     nb_inplace_and       : binaryfunc;
     nb_inplace_xor       : binaryfunc;
     nb_inplace_or        : binaryfunc;

     // Added in release 2.2
     // The following require the Py_TPFLAGS_HAVE_CLASS flag
     nb_floor_divide         : binaryfunc;
     nb_true_divide          : binaryfunc;
     nb_inplace_floor_divide : binaryfunc;
     nb_inplace_true_divide  : binaryfunc;
  end;
  PPyNumberMethods = ^PyNumberMethods;

  PySequenceMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     sq_length    : lenfunc;
     sq_concat    : binaryfunc;
     sq_repeat    : ssizeargfunc;
     sq_item      : ssizeargfunc;
     sq_slice     : ssizessizeargfunc;
     sq_ass_item  : ssizeobjargproc;
     sq_ass_slice : ssizessizeobjargproc;
     sq_contains        : objobjproc;
     sq_inplace_concat  : binaryfunc;
     sq_inplace_repeat  : ssizeargfunc;
  end;
  PPySequenceMethods = ^PySequenceMethods;

  PyMappingMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     mp_length	      : lenfunc;
     mp_subscript     : binaryfunc;
     mp_ass_subscript : objobjargproc;
  end;
  PPyMappingMethods = ^PyMappingMethods;

  PyBufferProcs = {$IFNDEF CPUX64}packed{$ENDIF} record
     bf_getreadbuffer   : getreadbufferproc;
     bf_getwritebuffer  : getwritebufferproc;
     bf_getsegcount     : getsegcountproc;
     bf_getcharbuffer   : getcharbufferproc;
  end;
  PPyBufferProcs = ^PyBufferProcs;

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
    tp_name:        PAnsiChar;   // For printing
    tp_basicsize, tp_itemsize: NativeInt; // For allocation

    // Methods to implement standard operations

    tp_dealloc:     pydestructor;
    tp_print:       printfunc; // not available and replaced in Python 3.x
    tp_getattr:     getattrfunc;
    tp_setattr:     setattrfunc;
    tp_compare:     cmpfunc;  // not available and replaced in Python 3.x
    tp_repr:        reprfunc;

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
    tp_as_buffer:   PPyBufferProcs;
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
    tp_version_tag      : NativeUInt;  // Type attribute cache version tag. Added in version 2.6
    tp_finalize         : PyDestructor;
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

  PPyMethodChain = ^PyMethodChain;
  PyMethodChain = {$IFNDEF CPUX64}packed{$ENDIF} record
    methods: PPyMethodDef;
    link:    PPyMethodChain;
  end;

  PPyClassObject = ^PyClassObject;
  PyClassObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    cl_bases   : PPyObject;       // A tuple of class objects
    cl_dict    : PPyObject;       // A dictionary
    cl_name    : PPyObject;       // A string
    // The following three are functions or NULL
    cl_getattr : PPyObject;
    cl_setattr : PPyObject;
    cl_delattr : PPyObject;
  end;

  PPyInstanceObject = ^PyInstanceObject;
  PyInstanceObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    in_class  : PPyClassObject;      // The class object
    in_dict   : PPyObject;           // A dictionary
  end;

{ Instance method objects are used for two purposes:
   (a) as bound instance methods (returned by instancename.methodname)
   (b) as unbound methods (returned by ClassName.methodname)
   In case (b), im_self is NULL
}

  PPyMethodObject = ^PyMethodObject;
  PyMethodObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    im_func  : PPyObject;      // The function implementing the method
    im_self  : PPyObject;      // The instance it is bound to, or NULL
    im_class : PPyObject;      // The class that defined the method
  end;


  // Bytecode object, compile.h
  PPyCodeObject = ^PyCodeObject;
  PyCodeObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt      : NativeInt;
    ob_type        : PPyTypeObject;
    co_argcount    : Integer;         // #arguments, except *args
    co_nlocals     : Integer;         // #local variables
    co_stacksize   : Integer;          // #entries needed for evaluation stack
    co_flags       : Integer;         // CO_..., see below
    co_code        : PPyObject;       // instruction opcodes (it hides a PyStringObject)
    co_consts      : PPyObject;       // list (constants used)
    co_names       : PPyObject;       // list of strings (names used)
    co_varnames    : PPyObject;       // tuple of strings (local variable names)
    co_freevars    : PPyObject;	      // tuple of strings (free variable names)
    co_cellvars    : PPyObject;       // tuple of strings (cell variable names)
    // The rest doesn't count for hash/cmp
    co_filename    : PPyObject;       // string (where it was loaded from)
    co_name        : PPyObject;       // string (name, for reference)
    co_firstlineno : Integer;         // first source line number
    co_lnotab      : PPyObject;       // string (encoding addr<->lineno mapping)
  end;


  // from pystate.h
  PPyInterpreterState = ^PyInterpreterState;
  PPyThreadState = ^PyThreadState;
  PPyFrameObject = ^PyFrameObject;

  // Interpreter environments
  PyInterpreterState = {$IFNDEF CPUX64}packed{$ENDIF} record
    next           : PPyInterpreterState;
    tstate_head    : PPyThreadState;

//  The strucure has changed between versions beyond this point.
//  Not safe to use members
//    modules        : PPyObject;
//    sysdict        : PPyObject;
//    builtins       : PPyObject;

    //Spares
    is_xxx1             : NativeInt;
    is_xxx2             : NativeInt;
    is_xxx3             : NativeInt;
    is_xxx4             : NativeInt;
    is_xxx5             : NativeInt;
    is_xxx6             : NativeInt;
    is_xxx7             : NativeInt;
    is_xxx8             : NativeInt;
    is_xxx9             : NativeInt;
  end;

  // Thread specific information
  PyThreadState = {$IFNDEF CPUX64}packed{$ENDIF} record
    {prev          : PPyThreadState; introduced in python 3.4}
    next           : PPyThreadState;
    interp         : PPyInterpreterState;
    interp34       : PPyInterpreterState;

//  The strucure has changed between versions beyond this point.
//  Not safe to use members
//
//    frame          : PPyFrameObject;
//    recursion_depth: integer;
//    ticker         : integer;
//    tracing        : integer;
//
//    sys_profilefn  : Pointer;           // c-functions for profile/trace
//    sys_tracefn    : Pointer;
//    sys_profilefunc: PPyObject;
//    sys_tracefunc  : PPyObject;
//
//    curexc_type    : PPyObject;
//    curexc_value   : PPyObject;
//    curexc_traceback: PPyObject;
//
//    exc_type       : PPyObject;
//    exc_value      : PPyObject;
//    exc_traceback  : PPyObject;
//
//    dict           : PPyObject;
//    tick_counter      :Integer;
//    gilstate_counter  :Integer;
//
//    async_exc         :PPyObject; { Asynchronous exception to raise }
//    thread_id         :LongInt;   { Thread id where this tstate was created }
    //Spares
    ts_xxx1             : NativeInt;
    ts_xxx2             : NativeInt;
    ts_xxx3             : NativeInt;
    ts_xxx4             : NativeInt;
    ts_xxx5             : NativeInt;
    ts_xxx6             : NativeInt;
    ts_xxx7             : NativeInt;
    ts_xxx8             : NativeInt;
    ts_xxx9             : NativeInt;
    ts_xxx10            : NativeInt;
    ts_xxx11            : NativeInt;
    ts_xxx12            : NativeInt;
    ts_xxx13            : NativeInt;
    ts_xxx14            : NativeInt;
    ts_xxx15            : NativeInt;
    ts_xxx16            : NativeInt;
    ts_xxx17            : NativeInt;
    ts_xxx18            : NativeInt;
    ts_xxx19            : NativeInt;

    { XXX signal handlers should also be here }
  end;

  // from frameobject.h

  PPyTryBlock = ^PyTryBlock;
  PyTryBlock = {$IFNDEF CPUX64}packed{$ENDIF} record
    b_type    : Integer;       // what kind of block this is
    b_handler : Integer;       // where to jump to find handler
    b_level   : Integer;       // value stack level to pop to
  end;

  CO_MAXBLOCKS  = 0..19;
  PyFrameObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the VAR_HEAD of an object.
    ob_refcnt    : NativeInt;
    ob_type      : PPyTypeObject;
    ob_size      : NativeInt;           // Number of items in variable part
    // End of the Head of an object
    f_back       : PPyFrameObject;    // previous frame, or NULL
    f_code       : PPyCodeObject;     // code segment
    f_builtins   : PPyObject;         // builtin symbol table (PyDictObject)
    f_globals    : PPyObject;         // global symbol table (PyDictObject)
    f_locals     : PPyObject;         // local symbol table (PyDictObject)
    f_valuestack : PPPyObject;        // points after the last local
    (* Next free slot in f_valuestack.  Frame creation sets to f_valuestack.
       Frame evaluation usually NULLs it, but a frame that yields sets it
       to the current stack top. *)
    f_stacktop   : PPPyObject;
    f_trace      : PPyObject;         // Trace function
    f_exc_type, f_exc_value, f_exc_traceback: PPyObject;
    f_tstate     : PPyThreadState;
    f_lasti      : Integer;           // Last instruction if called
    f_lineno     : Integer;           // Current line number
    f_iblock     : Integer;           // index in f_blockstack
    f_blockstack : array[CO_MAXBLOCKS] of PyTryBlock; // for try and loop blocks
    f_localsplus : array[0..0] of PPyObject; // locals+stack, dynamically sized
  end;

  // From traceback.c
  PPyTraceBackObject = ^PyTraceBackObject;
  PyTraceBackObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    tb_next   : PPyTraceBackObject;
    tb_frame  : PPyFrameObject;
    tb_lasti  : Integer;
    tb_lineno : Integer;
  end;

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
  end;

  // From weakrefobject.h

  PPyWeakReference = ^PyWeakReference;
  PyWeakReference = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    wr_object   : PPyObject;
    wr_callback : PPyObject;
    hash        : NativeInt;
    wr_prev     : PPyWeakReference;
    wr_next     : PPyWeakReference;
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
    hashcode    : Integer;  // -1 when unknown
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

//#######################################################
//##                                                   ##
//##         New exception classes                     ##
//##                                                   ##
//#######################################################

  // Components' exceptions
  EDLLLoadError  = class(Exception);
  EDLLImportError = class(Exception)
    public
      WrongFunc : AnsiString;
      ErrorCode : Integer;
  end;

  // Python's exceptions
  EPythonError   = class(Exception)
    public
      EName : String;
      EValue : String;
  end;
  EPyExecError   = class(EPythonError);


  // Standard exception classes of Python

{ Hierarchy of Python exceptions, Python 2.3, copied from <INSTALL>\Python\exceptions.c

Exception\n\
 |\n\
 +-- SystemExit\n\
 +-- StopIteration\n\
 +-- StandardError\n\
 |    |\n\
 |    +-- KeyboardInterrupt\n\
 |    +-- ImportError\n\
 |    +-- EnvironmentError\n\
 |    |    |\n\
 |    |    +-- IOError\n\
 |    |    +-- OSError\n\
 |    |         |\n\
 |    |         +-- WindowsError\n\
 |    |         +-- VMSError\n\
 |    |\n\
 |    +-- EOFError\n\
 |    +-- RuntimeError\n\
 |    |    |\n\
 |    |    +-- NotImplementedError\n\
 |    |\n\
 |    +-- NameError\n\
 |    |    |\n\
 |    |    +-- UnboundLocalError\n\
 |    |\n\
 |    +-- AttributeError\n\
 |    +-- SyntaxError\n\
 |    |    |\n\
 |    |    +-- IndentationError\n\
 |    |         |\n\
 |    |         +-- TabError\n\
 |    |\n\
 |    +-- TypeError\n\
 |    +-- AssertionError\n\
 |    +-- LookupError\n\
 |    |    |\n\
 |    |    +-- IndexError\n\
 |    |    +-- KeyError\n\
 |    |\n\
 |    +-- ArithmeticError\n\
 |    |    |\n\
 |    |    +-- OverflowError\n\
 |    |    +-- ZeroDivisionError\n\
 |    |    +-- FloatingPointError\n\
 |    |\n\
 |    +-- ValueError\n\
 |    |    |\n\
 |    |    +-- UnicodeError\n\
 |    |        |\n\
 |    |        +-- UnicodeEncodeError\n\
 |    |        +-- UnicodeDecodeError\n\
 |    |        +-- UnicodeTranslateError\n\
 |    |\n\
 |    +-- ReferenceError\n\
 |    +-- SystemError\n\
 |    +-- MemoryError\n\
 |\n\
 +---Warning\n\
      |\n\
      +-- UserWarning\n\
      +-- DeprecationWarning\n\
      +-- PendingDeprecationWarning\n\
      +-- SyntaxWarning\n\
      +-- RuntimeWarning\n\
      +-- FutureWarning"
}
   EPyException = class (EPythonError);
   EPyStandardError = class (EPyException);
   EPyArithmeticError = class (EPyStandardError);
   EPyLookupError = class (EPyStandardError);
   EPyAssertionError = class (EPyStandardError);
   EPyAttributeError = class (EPyStandardError);
   EPyEOFError = class (EPyStandardError);
   EPyFloatingPointError = class (EPyArithmeticError);
   EPyEnvironmentError = class (EPyStandardError);
   EPyIOError = class (EPyEnvironmentError);
   EPyOSError = class (EPyEnvironmentError);
   EPyImportError = class (EPyStandardError);
   EPyIndexError = class (EPyLookupError);
   EPyKeyError = class (EPyLookupError);
   EPyKeyboardInterrupt = class (EPyStandardError);
   EPyMemoryError = class (EPyStandardError);
   EPyNameError = class (EPyStandardError);
   EPyOverflowError = class (EPyArithmeticError);
   EPyRuntimeError = class (EPyStandardError);
   EPyNotImplementedError = class (EPyRuntimeError);
   EPySyntaxError = class (EPyStandardError)
   public
      EFileName: string;
      ELineStr: string;
      ELineNumber: Integer;
      EOffset: Integer;
   end;
   EPyIndentationError = class (EPySyntaxError);
   EPyTabError = class (EPyIndentationError);
   EPySystemError = class (EPyStandardError);
   EPySystemExit = class (EPyException);
   EPyTypeError = class (EPyStandardError);
   EPyUnboundLocalError = class (EPyNameError);
   EPyValueError = class (EPyStandardError);
   EPyUnicodeError = class (EPyValueError);
   UnicodeEncodeError = class (EPyUnicodeError);
   UnicodeDecodeError = class (EPyUnicodeError);
   UnicodeTranslateError = class (EPyUnicodeError);
   EPyZeroDivisionError = class (EPyArithmeticError);
   EPyStopIteration = class(EPyException);
   EPyWarning = class (EPyException);
   EPyUserWarning = class (EPyWarning);
   EPyDeprecationWarning = class (EPyWarning);
   PendingDeprecationWarning = class (EPyWarning);
   FutureWarning = class (EPyWarning);
   EPySyntaxWarning = class (EPyWarning);
   EPyRuntimeWarning = class (EPyWarning);
   EPyReferenceError = class (EPyStandardError);
 {$IFDEF MSWINDOWS}
   EPyWindowsError = class (EPyOSError);
 {$ENDIF}

//#######################################################
//##                                                   ##
//##                   Components                      ##
//##                                                   ##
//#######################################################

//-------------------------------------------------------
//--                                                   --
//--      class:  TPythonInputOutput                   --
//--      Works as a console for Python outputs        --
//--      It's a virtual Base class                    --
//-------------------------------------------------------

const
  kMaxLines = 1000;
  kMaxLineLength = 256;

type
  TSendDataEvent = procedure (Sender: TObject; const Data : AnsiString ) of object;
  TReceiveDataEvent = procedure (Sender: TObject; var Data : AnsiString ) of object;
  TSendUniDataEvent = procedure (Sender: TObject; const Data : UnicodeString ) of object;
  TReceiveUniDataEvent = procedure (Sender: TObject; var Data : UnicodeString ) of object;
  IOChar = WideChar;
  IOString = UnicodeString;
  TIOStringList = TUnicodeStringList;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonInputOutput = class(TComponent)
  protected
    FMaxLines        : Integer;
    FLine_Buffer     : IOString;
    FLinesPerThread  : TIOStringList;
    FLock            : TCriticalSection;
    FQueue           : TIOStringList;
    FDelayWrites     : Boolean;
    FMaxLineLength   : Integer;
    FOnSendData      : TSendDataEvent;
    FOnReceiveData   : TReceiveDataEvent;
    FOnSendUniData   : TSendUniDataEvent;
    FOnReceiveUniData: TReceiveUniDataEvent;
    FUnicodeIO       : Boolean;
    FRawOutput       : Boolean;

    procedure Lock;
    procedure Unlock;
    procedure AddWrite( const str : IOString );
    // Virtual methods for handling the input/output of text
    procedure SendData( const Data : AnsiString ); virtual;
    function  ReceiveData : AnsiString; virtual;
    procedure SendUniData( const Data : UnicodeString ); virtual;
    function  ReceiveUniData : UnicodeString; virtual;
    procedure AddPendingWrite; virtual;
    function  GetCurrentThreadSlotIdx : Integer;
    function  GetCurrentThreadLine : IOString;
    procedure UpdateCurrentThreadLine;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Write( const str : IOString );
    procedure WriteLine( const str : IOString );

  published
    property MaxLines : Integer read FMaxLines write FMaxLines default kMaxLines;
    property MaxLineLength : Integer read FMaxLineLength write FMaxLineLength default kMaxLineLength;
    property DelayWrites : Boolean read FDelayWrites write FDelayWrites default False;
    property OnSendData    : TSendDataEvent read FOnSendData write FOnSendData;
    property OnReceiveData : TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    property OnSendUniData    : TSendUniDataEvent read FOnSendUniData write FOnSendUniData;
    property OnReceiveUniData : TReceiveUniDataEvent read FOnReceiveUniData write FOnReceiveUniData;
    property UnicodeIO: Boolean read FUnicodeIO write FUnicodeIO;
    property RawOutput: Boolean read FRawOutput write FRawOutput;
  end;

//-------------------------------------------------------
//--                                                   --
//--      Base class:  TDynamicDll                     --
//--                                                   --
//-------------------------------------------------------

type
  TDynamicDll = class(TComponent)
  private
    function IsAPIVersionStored: Boolean;
    function IsDllNameStored: Boolean;
    function IsRegVersionStored: Boolean;
    procedure SetDllName(const Value: String);
  protected
    FDllName            : String;
    FDllPath            : String;
    FAPIVersion         : Integer;
    FRegVersion         : String;
    FAutoLoad           : Boolean;
    FAutoUnload         : Boolean;
    FFatalMsgDlg        : Boolean;
    FFatalAbort         : Boolean;
    FDLLHandle          : THandle;
    FUseLastKnownVersion: Boolean;
    FOnBeforeLoad       : TNotifyEvent;
    FOnAfterLoad        : TNotifyEvent;
    FOnBeforeUnload     : TNotifyEvent;

    function  Import(const funcname: AnsiString; canFail : Boolean = True): Pointer;
    procedure Loaded; override;
    procedure BeforeLoad; virtual;
    procedure AfterLoad; virtual;
    procedure BeforeUnload; virtual;
    function  GetQuitMessage : String; virtual;
    procedure DoOpenDll(const aDllName : String); virtual;
    function  GetDllPath : String;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy;                    override;

    // Public methods
    procedure OpenDll(const aDllName : String);
    function  IsHandleValid : Boolean;
    procedure LoadDll;
    procedure UnloadDll;
    procedure Quit;

    // Public properties
  published
    property AutoLoad : Boolean read FAutoLoad write FAutoLoad default True;
    property AutoUnload : Boolean read FAutoUnload write FAutoUnload default True;
    property DllName : String read FDllName write SetDllName stored IsDllNameStored;
    property DllPath : String read FDllPath write FDllPath;
    property APIVersion : Integer read FAPIVersion write FAPIVersion stored IsAPIVersionStored;
    property RegVersion : String read FRegVersion write FRegVersion stored IsRegVersionStored;
    property FatalAbort :  Boolean read FFatalAbort write FFatalAbort default True;
    property FatalMsgDlg : Boolean read FFatalMsgDlg write FFatalMsgDlg default True;
    property UseLastKnownVersion: Boolean read FUseLastKnownVersion write FUseLastKnownVersion default True;
    property OnAfterLoad : TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeLoad : TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnBeforeUnload : TNotifyEvent read FOnBeforeUnload write FOnBeforeUnload;
  end;

//-------------------------------------------------------
//--                                                   --
//--  class:  TPythonInterface derived from TDynamicDll--
//--      This class maps the functions imported       --
//--      from the Python Dll, and adds some           --
//--      Delphi implementations.                      --
//-------------------------------------------------------

type
  TPythonInterface=class(TDynamicDll)
  private
    DLL_Py_GetBuildInfo:
                     function : PAnsiChar; cdecl;
    DLL_PyCode_Addr2Line:
                     function ( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
    DLL_PyImport_ExecCodeModule:
                     function ( const name : AnsiString; codeobject : PPyObject) : PPyObject; cdecl;

    DLL_PyString_FromString:  function( str: PAnsiChar): PPyObject; cdecl;
    DLL_Py_FlushLine:procedure; cdecl;

  protected
    FInitialized:    Boolean;
    FFinalizing:     Boolean;
    FIsPython3000:   Boolean;
    FMajorVersion:   integer;
    FMinorVersion:   integer;
    FBuiltInModuleName: String;
    function GetInitialized: Boolean;

    procedure AfterLoad; override;
    function  GetQuitMessage : String; override;
    procedure CheckPython;
    function  GetUnicodeTypeSuffix : String;

  public
    // define Python flags. See file pyDebug.h
    Py_DebugFlag: PInt;
    Py_VerboseFlag: PInt;
    Py_InteractiveFlag: PInt;
    Py_OptimizeFlag: PInt;
    Py_NoSiteFlag: PInt;
    Py_UseClassExceptionsFlag: PInt;
    Py_FrozenFlag: PInt;
    Py_TabcheckFlag: PInt;
    Py_UnicodeFlag: PInt;
    Py_IgnoreEnvironmentFlag: PInt;
    Py_DivisionWarningFlag: PInt;
    //_PySys_TraceFunc:    PPPyObject;
    //_PySys_ProfileFunc: PPPPyObject;

    PyImport_FrozenModules: PP_frozen;

    Py_None:            PPyObject;
    Py_Ellipsis:        PPyObject;
    Py_False:           PPyIntObject;
    Py_True:            PPyIntObject;
    Py_NotImplemented:  PPyObject;

    PyExc_AttributeError: PPPyObject;
    PyExc_EOFError: PPPyObject;
    PyExc_IOError: PPPyObject;
    PyExc_ImportError: PPPyObject;
    PyExc_IndexError: PPPyObject;
    PyExc_KeyError: PPPyObject;
    PyExc_KeyboardInterrupt: PPPyObject;
    PyExc_MemoryError: PPPyObject;
    PyExc_NameError: PPPyObject;
    PyExc_OverflowError: PPPyObject;
    PyExc_RuntimeError: PPPyObject;
    PyExc_SyntaxError: PPPyObject;
    PyExc_SystemError: PPPyObject;
    PyExc_SystemExit: PPPyObject;
    PyExc_TypeError: PPPyObject;
    PyExc_ValueError: PPPyObject;
    PyExc_ZeroDivisionError: PPPyObject;
    PyExc_ArithmeticError: PPPyObject;
    PyExc_Exception: PPPyObject;
    PyExc_FloatingPointError: PPPyObject;
    PyExc_LookupError: PPPyObject;
    PyExc_StandardError: PPPyObject;
    PyExc_AssertionError: PPPyObject;
    PyExc_EnvironmentError: PPPyObject;
    PyExc_IndentationError: PPPyObject;
    PyExc_MemoryErrorInst: PPPyObject;
    PyExc_NotImplementedError: PPPyObject;
    PyExc_OSError: PPPyObject;
    PyExc_TabError: PPPyObject;
    PyExc_UnboundLocalError: PPPyObject;
    PyExc_UnicodeError: PPPyObject;
 {$IFDEF MSWINDOWS}
    PyExc_WindowsError: PPPyObject;
 {$ENDIF}
    PyExc_Warning: PPPyObject;
    PyExc_DeprecationWarning: PPPyObject;
    PyExc_RuntimeWarning: PPPyObject;
    PyExc_SyntaxWarning: PPPyObject;
    PyExc_UserWarning: PPPyObject;
    PyExc_ReferenceError: PPPyObject;
    PyExc_StopIteration: PPPyObject;
    PyExc_FutureWarning: PPPyObject;
    PyExc_PendingDeprecationWarning: PPPyObject;
    PyExc_UnicodeDecodeError: PPPyObject;
    PyExc_UnicodeEncodeError: PPPyObject;
    PyExc_UnicodeTranslateError: PPPyObject;

    PyType_Type: PPyTypeObject;
    PyCFunction_Type: PPyTypeObject;
    PyCObject_Type: PPyTypeObject;
    PyClass_Type: PPyTypeObject;
    PyCode_Type: PPyTypeObject;
    PyComplex_Type: PPyTypeObject;
    PyDict_Type: PPyTypeObject;
    PyFile_Type: PPyTypeObject;
    PyFloat_Type: PPyTypeObject;
    PyFrame_Type: PPyTypeObject;
    PyFunction_Type: PPyTypeObject;
    PyInstance_Type: PPyTypeObject;
    PyInt_Type: PPyTypeObject;
    PyList_Type: PPyTypeObject;
    PyLong_Type: PPyTypeObject;
    PyMethod_Type: PPyTypeObject;
    PyModule_Type: PPyTypeObject;
    PyObject_Type: PPyTypeObject;
    PyRange_Type: PPyTypeObject;
    PySlice_Type: PPyTypeObject;
    PyString_Type: PPyTypeObject;
    PyTuple_Type: PPyTypeObject;
    PyBaseObject_Type: PPyTypeObject;
    PyBuffer_Type: PPyTypeObject;
    PyCallIter_Type: PPyTypeObject;
    PyCell_Type: PPyTypeObject;
    PyClassMethod_Type: PPyTypeObject;
    PyProperty_Type: PPyTypeObject;
    PySeqIter_Type: PPyTypeObject;
    PyStaticMethod_Type: PPyTypeObject;
    PySuper_Type: PPyTypeObject;
    PyTraceBack_Type: PPyTypeObject;
    PyUnicode_Type: PPyTypeObject;
    PyWrapperDescr_Type: PPyTypeObject;
    _PyWeakref_RefType: PPyTypeObject;
    _PyWeakref_ProxyType: PPyTypeObject;
    _PyWeakref_CallableProxyType: PPyTypeObject;
    PyBaseString_Type: PPyTypeObject;
    PyBool_Type: PPyTypeObject;
    PyEnum_Type: PPyTypeObject;

    //PyArg_GetObject: function(args : PPyObject; nargs, i: integer; p_a: PPPyObject): integer; cdecl;
    //PyArg_GetLong:   function(args : PPyObject; nargs, i: integer; p_a: PLong): integer; cdecl;
    //PyArg_GetShort:  function(args : PPyObject; nargs, i: integer; p_a: PShort): integer; cdecl;
    //PyArg_GetFloat:  function(args : PPyObject; nargs, i: integer; p_a: PFloat): integer; cdecl;
    //PyArg_GetString: function(args : PPyObject; nargs, i: integer; p_a: PAnsiString): integer; cdecl;
    //PyArgs_VaParse:  function (args : PPyObject; format: PAnsiChar; va_list: array of const): integer; cdecl;
    // Does not work!
    // Py_VaBuildValue: function (format: PAnsiChar; va_list: array of const): PPyObject; cdecl;
    //PyBuiltin_Init:     procedure; cdecl;

    PyComplex_FromCComplex: function(c: Py_complex):PPyObject; cdecl;
    PyComplex_FromDoubles: function(realv,imag : double):PPyObject; cdecl;
    PyComplex_RealAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_ImagAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_AsCComplex: function(op : PPyObject ): Py_complex; cdecl;
    PyCFunction_GetFunction: function(ob : PPyObject): Pointer; cdecl;
    PyCFunction_GetSelf: function(ob : PPyObject): PPyObject; cdecl;
    PyCallable_Check: function(ob	: PPyObject): integer; cdecl;
    PyCObject_FromVoidPtr: function(cobj, destruct : Pointer): PPyObject; cdecl;
    PyCObject_AsVoidPtr: function(ob : PPyObject): Pointer; cdecl;
    PyClass_New: function (ob1,ob2,ob3 :  PPyObject): PPyObject; cdecl;
    PyClass_IsSubclass: function (ob1, ob2 : PPyObject): integer cdecl;

    Py_InitModule4: function( name: PAnsiChar; methods: PPyMethodDef; doc: PAnsiChar;
                              passthrough: PPyObject; Api_Version: Integer):PPyObject; cdecl;
    PyModule_Create2:   function(moduledef: PPyModuleDef; Api_Version: Integer):PPyObject; cdecl;
    PyErr_BadArgument:  function: integer; cdecl;
    PyErr_BadInternalCall: procedure; cdecl;
    PyErr_CheckSignals: function: integer; cdecl;
    PyErr_Clear:        procedure; cdecl;
    PyErr_Fetch:        procedure( errtype, errvalue, errtraceback: PPPyObject); cdecl;
    PyErr_NoMemory:     function: PPyObject; cdecl;
    PyErr_Occurred:     function: PPyObject; cdecl;
    PyErr_Print:        procedure; cdecl;
    PyErr_Restore:      procedure  (errtype, errvalue, errtraceback: PPyObject); cdecl;
    PyErr_SetFromErrno: function (ob :  PPyObject):PPyObject; cdecl;
    PyErr_SetNone:      procedure(value: PPyObject); cdecl;
    PyErr_SetObject:    procedure  (ob1, ob2	: PPyObject); cdecl;
    PyErr_SetString:    procedure( ErrorObject: PPyObject; text: PAnsiChar); cdecl;
    PyImport_GetModuleDict: function: PPyObject; cdecl;
    PyInt_FromLong:     function( x: LongInt):PPyObject; cdecl;
    PyArg_Parse:        function( args: PPyObject; format: PAnsiChar {;....}) :  Integer; cdecl varargs;
    PyArg_ParseTuple:   function( args: PPyObject; format: PAnsiChar {;...}): Integer; cdecl varargs;
    Py_BuildValue:      function( format: PAnsiChar {;...}): PPyObject; cdecl varargs;
    Py_Initialize:      procedure; cdecl;
    Py_Exit:            procedure( RetVal: Integer); cdecl;
    PyEval_GetBuiltins: function: PPyObject; cdecl;
    PyDict_Copy:        function(mp: PPyObject):PPyObject; cdecl;
    PyDict_GetItem:     function(mp, key : PPyObject):PPyObject; cdecl;
    PyDict_SetItem:     function(mp, key, item :PPyObject ):integer; cdecl;
    PyDict_DelItem:     function(mp, key : PPyObject ):integer; cdecl;
    PyDict_Clear:       procedure(mp : PPyObject); cdecl;
    PyDict_Next:        function(mp : PPyObject; pos: PNativeInt; key, value: PPPyObject):integer; cdecl;
    PyDict_Keys:        function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Values:      function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Items:       function(mp: PPyObject):PPyObject; cdecl;
    PyDict_Size:        function(mp: PPyObject):NativeInt; cdecl;
    PyDict_DelItemString: function(dp : PPyObject;key : PAnsiChar ):integer; cdecl;
    PyDict_New: function: PPyObject; cdecl;
    PyDict_GetItemString: function( dp: PPyObject; key: PAnsiChar): PPyObject; cdecl;
    PyDict_SetItemString: function( dp: PPyObject; key: PAnsiChar; item: PPyObject):
                          Integer; cdecl;
    PyDictProxy_New:      function (obj : PPyObject) : PPyObject; cdecl;
    PyModule_GetDict:     function( module:PPyObject): PPyObject; cdecl;
    PyObject_Str:         function( v: PPyObject): PPyObject; cdecl;
    PyRun_String:         function( str: PAnsiChar; start: Integer; globals: PPyObject;
                                    locals: PPyObject): PPyObject; cdecl;
    PyRun_SimpleString:   function( str: PAnsiChar): Integer; cdecl;
    PyString_AsString:    function( ob: PPyObject): PAnsiChar; cdecl;
    PyString_AsStringAndSize: function( ob: PPyObject; var buffer: PAnsiChar; var size: NativeInt): integer; cdecl;
    PySys_SetArgv:        procedure( argc: Integer; argv: PPAnsiChar); cdecl;
    PySys_SetArgv3000:    procedure( argc: Integer; argv: PPWideChar); cdecl;

    PyCFunction_New: function(md:PPyMethodDef;ob:PPyObject):PPyObject; cdecl;
    PyCFunction_NewEx: function(md:PPyMethodDef;self, ob:PPyObject):PPyObject; cdecl;
// Removed.  Use PyEval_CallObjectWithKeywords with third argument nil
//    PyEval_CallObject: function(callable_obj, args:PPyObject):PPyObject; cdecl;
    PyEval_CallObjectWithKeywords:function (callable_obj, args, kw:PPyObject):PPyObject; cdecl;
    PyEval_GetFrame:function :PPyObject; cdecl;
    PyEval_GetGlobals:function :PPyObject; cdecl;
    PyEval_GetLocals:function :PPyObject; cdecl;
    //PyEval_GetOwner:function :PPyObject; cdecl;
    PyEval_GetRestricted:function :integer; cdecl;

    PyEval_InitThreads:procedure; cdecl;
    PyEval_RestoreThread:procedure( tstate: PPyThreadState); cdecl;
    PyEval_SaveThread:function :PPyThreadState; cdecl;

    PyFile_FromString:function (pc1,pc2:PAnsiChar):PPyObject; cdecl;
    PyFile_GetLine:function (ob:PPyObject;i:integer):PPyObject; cdecl;
    PyFile_Name:function (ob:PPyObject):PPyObject; cdecl;
    PyFile_SetBufSize:procedure(ob:PPyObject;i:integer); cdecl;
    PyFile_SoftSpace:function (ob:PPyObject;i:integer):integer; cdecl;
    PyFile_WriteObject:function (ob1,ob2:PPyObject;i:integer):integer; cdecl;
    PyFile_WriteString:procedure(s:PAnsiChar;ob:PPyObject); cdecl;
    PyFloat_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
    PyFloat_FromDouble:function (db:double):PPyObject; cdecl;
    PyFunction_GetCode:function (ob:PPyObject):PPyObject; cdecl;
    PyFunction_GetGlobals:function (ob:PPyObject):PPyObject; cdecl;
    PyFunction_New:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyImport_AddModule:function (name:PAnsiChar):PPyObject; cdecl;
    PyImport_Cleanup:procedure; cdecl;
    PyImport_GetMagicNumber:function :LONGINT; cdecl;
    PyImport_ImportFrozenModule:function (key:PAnsiChar):integer; cdecl;
    PyImport_ImportModule:function (name:PAnsiChar):PPyObject; cdecl;
    PyImport_Import:function (name:PPyObject):PPyObject; cdecl;
    //PyImport_Init:procedure; cdecl;
    PyImport_ReloadModule:function (ob:PPyObject):PPyObject; cdecl;
    PyInstance_New:function (obClass, obArg, obKW:PPyObject):PPyObject; cdecl;
    PyInt_AsLong:function (ob:PPyObject):LONGINT; cdecl;
    PyList_Append:function (ob1,ob2:PPyObject):integer; cdecl;
    PyList_AsTuple:function (ob:PPyObject):PPyObject; cdecl;
    PyList_GetItem:function (ob:PPyObject;i:NativeInt):PPyObject; cdecl;
    PyList_GetSlice:function (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl;
    PyList_Insert:function (dp:PPyObject;idx:NativeInt;item:PPyObject):integer; cdecl;
    PyList_New:function (size:NativeInt):PPyObject; cdecl;
    PyList_Reverse:function (ob:PPyObject):integer; cdecl;
    PyList_SetItem:function (dp:PPyObject;idx:NativeInt;item:PPyObject):integer; cdecl;
    PyList_SetSlice:function (ob:PPyObject;i1,i2:NativeInt;ob2:PPyObject):integer; cdecl;
    PyList_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyList_Sort:function (ob:PPyObject):integer; cdecl;
    PyLong_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
    PyLong_AsLong:function (ob:PPyObject):LONGINT; cdecl;
    PyLong_FromDouble:function (db:double):PPyObject; cdecl;
    PyLong_FromLong:function (l:longint):PPyObject; cdecl;
    PyLong_FromString:function (pc:PAnsiChar;var ppc:PAnsiChar;i:integer):PPyObject; cdecl;
    PyLong_FromUnsignedLong:function(val:cardinal) : PPyObject; cdecl;
    PyLong_AsUnsignedLong:function(ob:PPyObject) : Cardinal; cdecl;
    PyLong_FromUnicode:function(ob:PPyObject; a, b : integer) : PPyObject; cdecl;
    PyLong_FromLongLong:function(val:Int64) : PPyObject; cdecl;
    PyLong_AsLongLong:function(ob:PPyObject) : Int64; cdecl;
    PyMapping_Check:function (ob:PPyObject):integer; cdecl;
    PyMapping_GetItemString:function (ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    PyMapping_HasKey:function (ob,key:PPyObject):integer; cdecl;
    PyMapping_HasKeyString:function (ob:PPyObject;key:PAnsiChar):integer; cdecl;
    PyMapping_Length:function (ob:PPyObject):NativeInt; cdecl;
    PyMapping_SetItemString:function (ob:PPyObject; key:PAnsiChar; value:PPyObject):integer; cdecl;
    PyMethod_Class:function (ob:PPyObject):PPyObject; cdecl;
    PyMethod_Function:function (ob:PPyObject):PPyObject; cdecl;
    PyMethod_New:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
    PyMethod_Self:function (ob:PPyObject):PPyObject; cdecl;
    PyModule_GetName:function (ob:PPyObject):PAnsiChar; cdecl;
    PyModule_New:function (key:PAnsiChar):PPyObject; cdecl;
    PyNumber_Absolute:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Add:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_And:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Check:function (ob:PPyObject):integer; cdecl;
    PyNumber_Coerce:function (var ob1,ob2:PPyObject):integer; cdecl;
    PyNumber_Divide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_FloorDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_TrueDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Divmod:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Float:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Int:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Invert:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Long:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Lshift:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Multiply:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Negative:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Or:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Positive:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Power:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
    PyNumber_Remainder:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Rshift:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Subtract:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Xor:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyOS_InitInterrupts:procedure; cdecl;
    PyOS_InterruptOccurred:function :integer; cdecl;
    PyObject_CallObject:function (ob,args:PPyObject):PPyObject; cdecl;
    PyObject_CallMethod : function ( obj : PPyObject; method, format : PAnsiChar {...}) : PPyObject; cdecl varargs;
    PyObject_Compare: function (ob1,ob2:PPyObject):integer; cdecl;
    PyObject_RichCompare:function (ob1,ob2:PPyObject;opid:integer):PPyObject; cdecl;
    PyObject_RichCompareBool:function (ob1,ob2:PPyObject;opid:integer):Integer; cdecl;
    PyObject_GetAttr:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyObject_GetAttrString:function (ob:PPyObject;c:PAnsiChar):PPyObject; cdecl;
    PyObject_GetItem:function (ob,key:PPyObject):PPyObject; cdecl;
    PyObject_DelItem:function (ob,key:PPyObject):PPyObject; cdecl;
    PyObject_HasAttrString:function (ob:PPyObject;key:PAnsiChar):integer; cdecl;
    PyObject_Hash:function (ob:PPyObject):NativeInt; cdecl;
    PyObject_IsTrue:function (ob:PPyObject):integer; cdecl;
    PyObject_Length:function (ob:PPyObject):NativeInt; cdecl;
    PyObject_Repr:function (ob:PPyObject):PPyObject; cdecl;
    PyObject_SetAttr:function (ob1,ob2,ob3:PPyObject):integer; cdecl;
    PyObject_SetAttrString:function (ob:PPyObject;key:PAnsiChar;value:PPyObject):integer; cdecl;
    PyObject_SetItem:function (ob1,ob2,ob3:PPyObject):integer; cdecl;
    PyObject_Init:function (ob:PPyObject; t:PPyTypeObject):PPyObject; cdecl;
    PyObject_InitVar:function (ob:PPyObject; t:PPyTypeObject; size:NativeInt):PPyObject; cdecl;
    PyObject_New:function (t:PPyTypeObject):PPyObject; cdecl;
    PyObject_NewVar:function (t:PPyTypeObject; size:NativeInt):PPyObject; cdecl;
    PyObject_Free:procedure (ob:PPyObject); cdecl;
    PyObject_GetIter: function (obj: PPyObject) : PPyObject; cdecl;
    PyIter_Next: function (obj: PPyObject) : PPyObject; cdecl;
    PyObject_IsInstance:function (inst, cls:PPyObject):integer; cdecl;
    PyObject_IsSubclass:function (derived, cls:PPyObject):integer; cdecl;
    PyObject_Call:function (ob, args, kw:PPyObject):PPyObject; cdecl;
    PyObject_GenericGetAttr:function (obj, name : PPyObject) : PPyObject; cdecl;
    PyObject_GenericSetAttr:function (obj, name, value : PPyObject) : Integer; cdecl;
    PyObject_GC_Malloc:function (size:NativeUInt):PPyObject; cdecl;
    PyObject_GC_New:function (t:PPyTypeObject):PPyObject; cdecl;
    PyObject_GC_NewVar:function (t:PPyTypeObject; size:NativeInt):PPyObject; cdecl;
    PyObject_GC_Resize:function (t:PPyObject; newsize:NativeInt):PPyObject; cdecl;
    PyObject_GC_Del:procedure (ob:PPyObject); cdecl;
    PyObject_GC_Track:procedure (ob:PPyObject); cdecl;
    PyObject_GC_UnTrack:procedure (ob:PPyObject); cdecl;
    PySequence_Check:function (ob:PPyObject):integer; cdecl;
    PySequence_Concat:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PySequence_Count:function (ob1,ob2:PPyObject):integer; cdecl;
    PySequence_GetItem:function (ob:PPyObject;i:NativeInt):PPyObject; cdecl;
    PySequence_GetSlice:function (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl;
    PySequence_In:function (ob1,ob2:PPyObject):integer; cdecl;
    PySequence_Index:function (ob1,ob2:PPyObject):NativeInt; cdecl;
    PySequence_Length:function (ob:PPyObject):NativeInt; cdecl;
    PySequence_Repeat:function (ob:PPyObject;count:NativeInt):PPyObject; cdecl;
    PySequence_SetItem:function (ob:PPyObject;i:NativeInt;value:PPyObject):integer; cdecl;
    PySequence_SetSlice:function (ob:PPyObject;i1,i2:NativeInt;value:PPyObject):integer; cdecl;
    PySequence_DelSlice:function (ob:PPyObject;i1,i2:NativeInt):integer; cdecl;
    PySequence_Tuple:function (ob:PPyObject):PPyObject; cdecl;
    PySequence_Contains:function (ob, value:PPyObject):integer; cdecl;
    PySeqIter_New: function(obj : PPyObject) : PPyObject; cdecl;
    PySlice_GetIndices:function (ob:PPySliceObject;length:NativeInt;var start,stop,step:NativeInt):integer; cdecl;
    PySlice_GetIndicesEx:function (ob:PPySliceObject;length:NativeInt;var start,stop,step,slicelength:NativeInt):integer; cdecl;
    PySlice_New:function (start,stop,step:PPyObject):PPyObject; cdecl;
    PyString_Concat:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
    PyString_ConcatAndDel:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
    PyString_Format:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyString_FromStringAndSize:function (s:PAnsiChar;i:NativeInt):PPyObject; cdecl;
    PyString_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyString_DecodeEscape:function(s:PAnsiChar; len:NativeInt; errors:PAnsiChar; unicode:NativeInt; recode_encoding:PAnsiChar):PPyObject; cdecl;
    PyString_Repr:function(ob:PPyObject; smartquotes:integer):PPyObject; cdecl;
    PySys_GetObject:function (s:PAnsiChar):PPyObject; cdecl;
    //PySys_Init:procedure; cdecl;
    PySys_SetObject:function (s:PAnsiChar;ob:PPyObject):integer; cdecl;
    PySys_SetPath:procedure(path:PAnsiChar); cdecl;
    //PyTraceBack_Fetch:function :PPyObject; cdecl;
    PyTraceBack_Here:function (p:pointer):integer; cdecl;
    PyTraceBack_Print:function (ob1,ob2:PPyObject):integer; cdecl;
    //PyTraceBack_Store:function (ob:PPyObject):integer; cdecl;
    PyTuple_GetItem:function (ob:PPyObject;i:NativeInt):PPyObject; cdecl;
    PyTuple_GetSlice:function (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl;
    PyTuple_New:function (size:NativeInt):PPyObject; cdecl;
    PyTuple_SetItem:function (ob:PPyObject;key:NativeInt;value:PPyObject):integer; cdecl;
    PyTuple_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyType_IsSubtype:function (a, b : PPyTypeObject):integer; cdecl;
    PyType_GenericAlloc:function(atype: PPyTypeObject; nitems:NativeInt) : PPyObject; cdecl;
    PyType_GenericNew:function(atype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
    PyType_Ready:function(atype: PPyTypeObject) : integer; cdecl;
    PyUnicode_FromWideChar:function (const w:PWideChar; size:NativeInt):PPyObject; cdecl;
    PyUnicode_AsWideChar:function (unicode: PPyObject; w:PWideChar; size:NativeInt):integer; cdecl;
    PyUnicode_Decode:function (const s:PAnsiChar; size: NativeInt; const encoding : PAnsiChar; const errors: PAnsiChar):PPyObject; cdecl;
    PyUnicode_AsEncodedString:function (unicode:PPyObject; const encoding:PAnsiChar; const errors:PAnsiChar):PPyObject; cdecl;
    PyUnicode_FromOrdinal:function (ordinal:integer):PPyObject; cdecl;
    PyUnicode_GetSize:function (unicode:PPyObject):NativeInt; cdecl;
    PyWeakref_GetObject: function ( ref : PPyObject) : PPyObject; cdecl;
    PyWeakref_NewProxy: function ( ob, callback : PPyObject) : PPyObject; cdecl;
    PyWeakref_NewRef: function ( ob, callback : PPyObject) : PPyObject; cdecl;
    PyWrapper_New: function ( ob1, ob2 : PPyObject) : PPyObject; cdecl;
    PyBool_FromLong: function ( ok : Integer) : PPyObject; cdecl;
    PyThreadState_SetAsyncExc: function(t_id :LongInt; exc :PPyObject) : Integer; cdecl;
    Py_AtExit:function (proc: AtExitProc):integer; cdecl;
    //Py_Cleanup:procedure; cdecl;
    Py_CompileStringFlags:function (s1,s2:PAnsiChar;i:integer;flags:PPyCompilerFlags):PPyObject; cdecl;
    Py_CompileStringExFlags:function (s1,s2:PAnsiChar;i:integer;flags:PPyCompilerFlags;optimize:integer):PPyObject; cdecl;
    Py_FatalError:procedure(s:PAnsiChar); cdecl;
    Py_FindMethod:function (md:PPyMethodDef;ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    Py_FindMethodInChain:function (mc:PPyMethodChain;ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    _PyObject_New:function (obt:PPyTypeObject;ob:PPyObject):PPyObject; cdecl;
    _PyString_Resize:function (var ob:PPyObject;i:NativeInt):integer; cdecl;
    Py_Finalize                     : procedure; cdecl;
    PyErr_ExceptionMatches          : function ( exc : PPyObject) : Integer; cdecl;
    PyErr_GivenExceptionMatches     : function ( raised_exc, exc : PPyObject) : Integer; cdecl;
    PyEval_EvalCode                 : function ( co : PPyCodeObject; globals, locals : PPyObject) : PPyObject; cdecl;
    Py_GetVersion                   : function : PAnsiChar; cdecl;
    Py_GetCopyright                 : function : PAnsiChar; cdecl;
    Py_GetExecPrefix                : function : PAnsiChar; cdecl;
    Py_GetPath                      : function : PAnsiChar; cdecl;
    Py_SetPythonHome                : procedure (home : PAnsiChar); cdecl;
    Py_GetPythonHome                : function : PAnsiChar; cdecl;
    Py_SetPythonHome3000            : procedure (home : PWideChar); cdecl;
    Py_GetPythonHome3000            : function : PWideChar; cdecl;
    Py_GetPrefix                    : function : PAnsiChar; cdecl;
    Py_GetProgramName               : function : PAnsiChar; cdecl;

    PyParser_SimpleParseStringFlags : function ( str : PAnsiChar; start, flags : Integer) : PNode; cdecl;
    PyNode_Free                     : procedure( n : PNode ); cdecl;
    PyErr_NewException              : function ( name : PAnsiChar; base, dict : PPyObject ) : PPyObject; cdecl;
    PyMem_Malloc                    : function ( size : NativeInt ) : Pointer;

{New exported Objects in Python 1.5}
    Py_SetProgramName               : procedure( name: PAnsiChar); cdecl;
    Py_SetProgramName3000           : procedure( name: PWideChar); cdecl;
    Py_IsInitialized                : function : integer; cdecl;
    Py_GetProgramFullPath           : function : PAnsiChar; cdecl;
    Py_NewInterpreter               : function : PPyThreadState; cdecl;
    Py_EndInterpreter               : procedure( tstate: PPyThreadState); cdecl;
    PyEval_AcquireLock              : procedure; cdecl;
    PyEval_ReleaseLock              : procedure; cdecl;
    PyEval_AcquireThread            : procedure( tstate: PPyThreadState); cdecl;
    PyEval_ReleaseThread            : procedure( tstate: PPyThreadState); cdecl;
    PyInterpreterState_New          : function : PPyInterpreterState; cdecl;
    PyInterpreterState_Clear        : procedure( interp: PPyInterpreterState); cdecl;
    PyInterpreterState_Delete       : procedure( interp: PPyInterpreterState); cdecl;
    PyThreadState_New               : function ( interp: PPyInterpreterState): PPyThreadState; cdecl;
    PyThreadState_Clear             : procedure( tstate: PPyThreadState); cdecl;
    PyThreadState_Delete            : procedure( tstate: PPyThreadState); cdecl;
    PyThreadState_Get               : function : PPyThreadState; cdecl;
    PyThreadState_Swap              : function ( tstate: PPyThreadState): PPyThreadState; cdecl;
    PyErr_SetInterrupt              : procedure; cdecl;
    PyGILState_Ensure               : function() : PyGILstate_STATE; cdecl;
    PyGILState_Release              : procedure(gilstate : PyGILState_STATE); cdecl;
{Further exported Objects, may be implemented later}
{
    PyCode_New: Pointer;
    PyFile_AsFile: Pointer;
    PyFile_FromFile: Pointer;
    PyFloat_AsString: Pointer;
    PyFrame_BlockPop: Pointer;
    PyFrame_BlockSetup: Pointer;
    PyFrame_ExtendStack: Pointer;
    PyFrame_FastToLocals: Pointer;
    PyFrame_LocalsToFast: Pointer;
    PyFrame_New: Pointer;
    PyGrammar_AddAccelerators: Pointer;
    PyGrammar_FindDFA: Pointer;
    PyGrammar_LabelRepr: Pointer;
    PyInstance_DoBinOp: Pointer;
    PyInt_GetMax: Pointer;
    PyMarshal_Init: Pointer;
    PyMarshal_ReadLongFromFile: Pointer;
    PyMarshal_ReadObjectFromFile: Pointer;
    PyMarshal_ReadObjectFromString: Pointer;
    PyMarshal_WriteLongToFile: Pointer;
    PyMarshal_WriteObjectToFile: Pointer;
    PyMember_Get: Pointer;
    PyMember_Set: Pointer;
    PyNode_AddChild: Pointer;
    PyNode_Compile: Pointer;
    PyNode_New: Pointer;
    PyOS_GetLastModificationTime: Pointer;
    PyOS_Readline: Pointer;
    PyOS_strtol: Pointer;
    PyOS_strtoul: Pointer;
    PyObject_CallFunction: Pointer;
    PyObject_Print: Pointer;
    PyParser_AddToken: Pointer;
    PyParser_Delete: Pointer;
    PyParser_New: Pointer;
    PyParser_ParseFile: Pointer;
    PyParser_ParseString: Pointer;
    PyParser_SimpleParseFile: Pointer;
    PyRun_AnyFile: Pointer;
    PyRun_File: Pointer;
    PyRun_InteractiveLoop: Pointer;
    PyRun_InteractiveOne: Pointer;
    PyRun_SimpleFile: Pointer;
    PySys_GetFile: Pointer;
    PyToken_OneChar: Pointer;
    PyToken_TwoChars: Pointer;
    PyTokenizer_Free: Pointer;
    PyTokenizer_FromFile: Pointer;
    PyTokenizer_FromString: Pointer;
    PyTokenizer_Get: Pointer;
    Py_Main: Pointer;
    _PyParser_Grammar: Pointer;
    _PyParser_TokenNames: Pointer;
    _PyThread_Started: Pointer;
    _Py_c_diff: Pointer;
    _Py_c_neg: Pointer;
    _Py_c_pow: Pointer;
    _Py_c_prod: Pointer;
    _Py_c_quot: Pointer;
    _Py_c_sum: Pointer;
}
  // Not exported in Python 3.8 and implemented as functions
  function PyParser_SimpleParseString( str : PAnsiChar; start : Integer) : PNode; cdecl;
  function Py_CompileString( s1,s2:PAnsiChar;i:integer) : PPyObject; cdecl;

  // functions redefined in Delphi
  procedure   Py_INCREF   ( op: PPyObject);
  procedure   Py_DECREF   ( op: PPyObject);
  procedure   Py_XINCREF  ( op: PPyObject);
  procedure   Py_XDECREF  ( op: PPyObject);

  function PyCode_Addr2Line( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
  function Py_GetBuildInfo: PAnsiChar; cdecl;
  function PyImport_ExecCodeModule( const AName : AnsiString; codeobject : PPyObject) : PPyObject;
  function PyString_Check( obj : PPyObject ) : Boolean;
  function PyString_CheckExact( obj : PPyObject ) : Boolean;
  function PyFloat_Check( obj : PPyObject ) : Boolean;
  function PyFloat_CheckExact( obj : PPyObject ) : Boolean;
  function PyInt_Check( obj : PPyObject ) : Boolean;
  function PyInt_CheckExact( obj : PPyObject ) : Boolean;
  function PyLong_Check( obj : PPyObject ) : Boolean;
  function PyLong_CheckExact( obj : PPyObject ) : Boolean;
  function PyTuple_Check( obj : PPyObject ) : Boolean;
  function PyTuple_CheckExact( obj : PPyObject ) : Boolean;
  function PyInstance_Check( obj : PPyObject ) : Boolean;
  function PyClass_Check( obj : PPyObject ) : Boolean;
  function PyType_CheckExact( obj : PPyObject ) : Boolean;
  function PyMethod_Check( obj : PPyObject ) : Boolean;
  function PyList_Check( obj : PPyObject ) : Boolean;
  function PyList_CheckExact( obj : PPyObject ) : Boolean;
  function PyDict_Check( obj : PPyObject ) : Boolean;
  function PyDict_CheckExact( obj : PPyObject ) : Boolean;
  function PyModule_Check( obj : PPyObject ) : Boolean;
  function PyModule_CheckExact( obj : PPyObject ) : Boolean;
  function PySlice_Check( obj : PPyObject ) : Boolean;
  function PyFunction_Check( obj : PPyObject ) : Boolean;
  function PyIter_Check( obj : PPyObject ) : Boolean;
  function PyUnicode_Check( obj : PPyObject ) : Boolean;
  function PyUnicode_CheckExact( obj : PPyObject ) : Boolean;
  function PyType_IS_GC(t : PPyTypeObject ) : Boolean;
  function PyObject_IS_GC( obj : PPyObject ) : Boolean;
  function PyWeakref_Check( obj : PPyObject ) : Boolean;
  function PyWeakref_CheckRef( obj : PPyObject ) : Boolean;
  function PyWeakref_CheckProxy( obj : PPyObject ) : Boolean;
  function PyBool_Check( obj : PPyObject ) : Boolean;
  function PyBaseString_Check( obj : PPyObject ) : Boolean;
  function PyEnum_Check( obj : PPyObject ) : Boolean;
  function PyObject_TypeCheck(obj:PPyObject; t:PPyTypeObject) : Boolean;
  function Py_InitModule( const AName : PAnsiChar; md : PPyMethodDef) : PPyObject;
  function Py_InitModule3000( const md : PyModuleDef) : PPyObject;
  function PyString_FromString( str: PAnsiChar): PPyObject; virtual; abstract;
  procedure Py_FlushLine; cdecl;

  // Constructors & Destructors
  constructor Create(AOwner: TComponent); override;

  // Public methods
  procedure MapDll;

  // Public properties
  property Initialized : Boolean read GetInitialized;
  property Finalizing : Boolean read FFinalizing;
  property IsPython3000 : Boolean read FIsPython3000;
  property MajorVersion : integer read FMajorVersion;
  property MinorVersion : integer read FMinorVersion;
  property BuiltInModuleName: String read FBuiltInModuleName write FBuiltInModuleName;

end;

//--------------------------------------------------------
//--                                                    --
//-- class:  TPythonEngine derived from TPythonInterface--
//-- Pytrunobject providing interface for               --
//-- running Python into Delphi                         --
//--------------------------------------------------------
type
  TDatetimeConversionMode = (dcmToTuple, dcmToDatetime);
const
  DEFAULT_DATETIME_CONVERSION_MODE = dcmToTuple;
type
  TEngineClient = class;
  TPathInitializationEvent = procedure ( Sender : TObject; var Path : String ) of Object;
  TSysPathInitEvent = procedure ( Sender : TObject; PathList : PPyObject ) of Object;
  TPythonFlag = (pfDebug, pfInteractive, pfNoSite, pfOptimize, pfTabcheck, pfUnicode, pfVerbose,
                 pfUseClassExceptionsFlag, pfFrozenFlag, pfIgnoreEnvironmentFlag, pfDivisionWarningFlag);
  TPythonFlags = set of TPythonFlag;


  TTracebackItem = class
  public
    FileName : String;
    LineNo : Integer;
    Context : String;
  end;

  TPythonTraceback = class
    protected
      FItems : TList;
      FLimit : Integer;

      function GetItemCount : Integer;
      function GetItem( idx : Integer ) : TTracebackItem;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;
      procedure Refresh;

      property ItemCount : Integer read GetItemCount;
      property Items[ idx : Integer ] : TTracebackItem read GetItem;
      property Limit : Integer read FLimit write FLimit;
  end;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonEngine = class(TPythonInterface)
  private
    FVenvPythonExe:              string;
    FInitScript:                 TStrings;
    FIO:                         TPythonInputOutput;
    FRedirectIO:                 Boolean;
    FOnAfterInit:                TNotifyEvent;
    FClients:                    TList;
    FLock:                       TCriticalSection;
    FExecModule:                 AnsiString;
    FAutoFinalize:               Boolean;
    FProgramName:                AnsiString;
    FProgramNameW:               UnicodeString;
    FPythonHome:                 AnsiString;
    FPythonHomeW:                UnicodeString;
    FInitThreads:                Boolean;
    FOnPathInitialization:       TPathInitializationEvent;
    FOnSysPathInit:              TSysPathInitEvent;
    FTraceback:                  TPythonTraceback;
    FUseWindowsConsole:          Boolean;
    FGlobalVars:                 PPyObject;
    FLocalVars:                  PPyObject;
    FPyFlags:                    TPythonFlags;
    FIORedirected:               Boolean;
    FIOPythonModule:             TObject;
    FDatetimeConversionMode:     TDatetimeConversionMode;
    FTimeStruct:                 PPyObject;
    FPyDateTime_DateType:        PPyObject;
    FPyDateTime_DateTimeType:    PPyObject;
    FPyDateTime_DeltaType:       PPyObject;
    FPyDateTime_TimeType:        PPyObject;
    FPyDateTime_TZInfoType:      PPyObject;
    FPyDateTime_TimeTZType:      PPyObject;
    FPyDateTime_DateTimeTZType:  PPyObject;

  protected
    procedure AfterLoad; override;
    procedure BeforeLoad; override;
    procedure DoOpenDll(const aDllName : String); override;
    procedure SetInitScript(Value: TStrings);
    function  GetThreadState: PPyThreadState;
    function  GetInterpreterState: PPyInterpreterState;
    procedure SetInitThreads(Value: Boolean);
    function  GetClientCount : Integer;
    function  GetClients( idx : Integer ) : TEngineClient;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CheckRegistry;
    procedure SetProgramArgs;
    procedure InitWinConsole;
    procedure SetUseWindowsConsole( const Value : Boolean );
    procedure SetGlobalVars(const Value: PPyObject);
    procedure SetLocalVars(const Value: PPyObject);
    procedure SetPyFlags(const Value: TPythonFlags);
    procedure AssignPyFlags;

  public
    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // Public methods
    procedure  Initialize;
    procedure  Finalize;
    procedure  Lock;
    procedure  Unlock;
    procedure  SetPythonHome(const PythonHome: string);
    procedure  SetProgramName(const ProgramName: string);
    function   IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
    function   GetAttrString(obj: PPyObject; AName: PAnsiChar):PAnsiChar;
    function   Run_CommandAsString(const command : AnsiString; mode : Integer) : String;
    function   Run_CommandAsObject(const command : AnsiString; mode : Integer) : PPyObject;
    function   Run_CommandAsObjectWithDict(const command : AnsiString; mode : Integer; locals, globals : PPyObject) : PPyObject;
    function   EncodeString (const str: string): AnsiString;
    function   EncodeWindowsFilePath (const str: string): AnsiString;
    procedure  ExecString(const command : AnsiString); overload;
    procedure  ExecStrings( strings : TStrings ); overload;
    function   EvalString(const command : AnsiString) : PPyObject; overload;
    function   EvalStringAsStr(const command : AnsiString) : String;
    function   EvalStrings( strings : TStrings ) : PPyObject; overload;
    procedure  ExecString(const command : AnsiString; locals, globals : PPyObject ); overload;
    procedure  ExecStrings( strings : TStrings; locals, globals : PPyObject ); overload;
    function   EvalString( const command : AnsiString; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStrings( strings : TStrings; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStringsAsStr( strings : TStrings ) : String;
    function   EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
    function   EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
    function   EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
    function   CheckEvalSyntax( const str : AnsiString ) : Boolean;
    function   CheckExecSyntax( const str : AnsiString ) : Boolean;
    function   CheckSyntax( const str : AnsiString; mode : Integer ) : Boolean;
    procedure  RaiseError;
    function   PyObjectAsString( obj : PPyObject ) : String;
    procedure  DoRedirectIO;
    procedure  AddClient( client : TEngineClient );
    procedure  RemoveClient( client : TEngineClient );
    function   FindClient( const aName : string ) : TEngineClient;
    function   TypeByName( const aTypeName : AnsiString ) : PPyTypeObject;
    function   ModuleByName( const aModuleName : AnsiString ) : PPyObject;
    function   MethodsByName( const aMethodsContainer: string ) : PPyMethodDef;
    function   VariantAsPyObject( const V : Variant ) : PPyObject; virtual;
    function   PyObjectAsVariant( obj : PPyObject ) : Variant; virtual;
    function   VarRecAsPyObject( v : TVarRec ) : PPyObject;
    function   MakePyTuple( const objects : array of PPyObject ) : PPyObject;
    function   MakePyList( const objects : array of PPyObject ) : PPyObject;
    function   ArrayToPyTuple( items : array of const) : PPyObject;
    function   ArrayToPyList( items : array of const) : PPyObject;
    function   ArrayToPyDict( items : array of const) : PPyObject;
    function   StringsToPyList( strings : TStrings ) : PPyObject;
    function   StringsToPyTuple( strings : TStrings ) : PPyObject;
    procedure  PyListToStrings( list : PPyObject; strings : TStrings );
    procedure  PyTupleToStrings( tuple: PPyObject; strings : TStrings );
    function   PyUnicode_AsWideString( obj : PPyObject ) : UnicodeString;
    function   PyUnicode_FromWideString( const AString : UnicodeString) : PPyObject;
    function   ReturnNone : PPyObject;
    function   FindModule( const ModuleName : AnsiString ) : PPyObject;
    function   FindFunction(ModuleName,FuncName: AnsiString): PPyObject;
    function   SetToList( data : Pointer; size : Integer ) : PPyObject;
    procedure  ListToSet( List : PPyObject; data : Pointer; size : Integer );
    procedure  CheckError(ACatchStopEx : Boolean = False);
    function   GetMainModule : PPyObject;
    function   PyTimeStruct_Check( obj : PPyObject ) : Boolean;
    { Date, Time, DateTime and related objects check functions }
    function   PyDate_Check( obj : PPyObject ) : Boolean;
    function   PyDate_CheckExact( obj : PPyObject ) : Boolean;
    function   PyDateTime_Check( obj : PPyObject ) : Boolean;
    function   PyDateTime_CheckExact( obj : PPyObject ) : Boolean;
    function   PyTime_Check( obj : PPyObject ) : Boolean;
    function   PyTime_CheckExact( obj : PPyObject ) : Boolean;
    function   PyDelta_Check( obj : PPyObject ) : Boolean;
    function   PyDelta_CheckExact( obj : PPyObject ) : Boolean;
    function   PyTZInfo_Check( obj : PPyObject ) : Boolean;
    function   PyTZInfo_CheckExact( obj : PPyObject ) : Boolean;
    { end date/time functions }
    function PyString_FromString( str: PAnsiChar): PPyObject; override;
    function PyString_FromDelphiString(str : string): PPyObject;
    function PyString_AsDelphiString( ob: PPyObject): string;
    function PyString_AsAnsiString( ob: PPyObject): AnsiString;
    function PyString_AsWideString( ob: PPyObject): UnicodeString;

    // Public Properties
    property ClientCount : Integer read GetClientCount;
    property Clients[ idx : Integer ] : TEngineClient read GetClients;
    property ExecModule : AnsiString read FExecModule write FExecModule;
    property ThreadState: PPyThreadState read GetThreadState;
    property InterpreterState: PPyInterpreterState read GetInterpreterState;
    property Traceback : TPythonTraceback read FTraceback;
    property LocalVars : PPyObject read FLocalVars Write SetLocalVars;
    property GlobalVars : PPyObject read FGlobalVars Write SetGlobalVars;
    property IOPythonModule: TObject read FIOPythonModule; {TPythonModule}
  published
    property AutoFinalize: Boolean read FAutoFinalize write FAutoFinalize default True;
    property VenvPythonExe: string read FVenvPythonExe write FVenvPythonExe;
    property DatetimeConversionMode: TDatetimeConversionMode read FDatetimeConversionMode write FDatetimeConversionMode default DEFAULT_DATETIME_CONVERSION_MODE;
    property InitScript: TStrings read FInitScript write SetInitScript;
    property InitThreads: Boolean read FInitThreads write SetInitThreads default False;
    property IO: TPythonInputOutput read FIO write FIO;
    property PyFlags: TPythonFlags read FPyFlags write SetPyFlags default [];
    property RedirectIO: Boolean read FRedirectIO write FRedirectIO default True;
    property UseWindowsConsole: Boolean read FUseWindowsConsole write FUseWindowsConsole default False;
    property OnAfterInit: TNotifyEvent read FOnAfterInit write FOnAfterInit;
    property OnPathInitialization: TPathInitializationEvent read FOnPathInitialization write FOnPathInitialization;
    property OnSysPathInit: TSysPathInitEvent read FOnSysPathInit write FOnSysPathInit;
  end;


//-------------------------------------------------------
//--                                                   --
//--      Base class:  TEngineClient                   --
//--                                                   --
//-------------------------------------------------------

  TEngineClient = class(TComponent)
    protected
      FEngine : TPythonEngine;
      FOnInitialization : TNotifyEvent;
      FOnFinalization : TNotifyEvent;
      FOnCreate : TNotifyEvent;
      FOnDestroy : TNotifyEvent;
      FInitialized : Boolean;

      procedure SetEngine( val : TPythonEngine ); virtual;
      procedure Loaded; override;
      procedure Notification( AComponent: TComponent;
                              Operation: TOperation); override;
      procedure ModuleReady(Sender : TObject); virtual;
    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // Public Methods
      procedure Initialize; virtual;
      procedure Finalize; virtual;
      procedure ClearEngine;
      procedure CheckEngine;

      // Public Properties
      property Initialized: Boolean read FInitialized;

    published
      property Engine : TPythonEngine read FEngine write SetEngine;
      property OnCreate : TNotifyEvent read FOnCreate write FOnCreate;
      property OnDestroy : TNotifyEvent read FOnDestroy write FOnDestroy;
      property OnFinalization : TNotifyEvent read FOnFinalization write FOnFinalization;
      property OnInitialization : TNotifyEvent read FOnInitialization write FOnInitialization;
  end;

//-------------------------------------------------------
//--                                                   --
//--class: TMethodsContainer derived from TEngineClient--
//--                                                   --
//-------------------------------------------------------

  TMethodArray = array[ 0 .. 16000 ] of PyMethodDef;
  PMethodArray = ^TMethodArray;
  TDelphiMethod = function ( self, args : PPyObject ) : PPyObject of object; cdecl;
  TDelphiMethodWithKW = function ( self, args, keywords : PPyObject ) : PPyObject of object; cdecl;
  TPythonEvent = procedure(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject) of object;
  TMethodsContainer = class; // forward declaration
  TEventDefs = class; // forward declaration

  // Event Collection Item
  TEventDef = class(TCollectionItem)
  private
    FName: AnsiString;
    FTmpDocString: AnsiString;
    FOnExecute: TPythonEvent;
    FDocString: TStringList;
 	 procedure SetDocString(const Value: TStringList);
  protected
    function  GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function  GetDocString : AnsiString;
    function  PythonEvent(pself, args: PPyObject): PPyObject; cdecl;
    function  Owner : TEventDefs;
  published
    property Name: string read GetDisplayName write SetDisplayName;
    property OnExecute: TPythonEvent read FOnExecute write FOnExecute;
    property DocString: TStringList read FDocString write SetDocString;
  end;

  // Event Collection
  TEventDefs = class(TCollection)
  protected
    FMethodsContainer : TMethodsContainer;

    function  GetItems( idx : Integer ) : TEventDef;
    procedure SetItems( idx : Integer; Value : TEventDef );
    function  GetOwner: TPersistent; override;
  public
    constructor Create( AMethodsContainer : TMethodsContainer );

    function  Add : TEventDef;
    procedure RegisterEvents;

    property Items[ idx : Integer ] : TEventDef read GetItems;
    property Container : TMethodsContainer read FMethodsContainer;
  end;

  // class TMethodsContainer
  TMethodsContainer = class(TEngineClient)
    private
      FMethodCount : Integer;
      FAllocatedMethodCount : Integer;
      FMethods : PPyMethodDef;
      FModuleDef : PyModuleDef;  // for Python 3000
      FEventDefs: TEventDefs;

      procedure AllocMethods;
      procedure FreeMethods;
      function  GetMethods( idx : Integer ) : PPyMethodDef;
      function  StoreEventDefs: Boolean;

    protected
      procedure ReallocMethods; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure Initialize; override;
      procedure Finalize; override;

      function  AddMethod( AMethodName  : PAnsiChar;
                           AMethod  : PyCFunction;
                           ADocString : PAnsiChar ) : PPyMethodDef;
      function  AddMethodWithKeywords( AMethodName  : PAnsiChar;
                                       AMethod  : PyCFunctionWithKW;
                                       ADocString : PAnsiChar ) : PPyMethodDef;
      function  AddDelphiMethod( AMethodName  : PAnsiChar;
                                 ADelphiMethod: TDelphiMethod;
                                 ADocString : PAnsiChar ) : PPyMethodDef;
      function  AddDelphiMethodWithKeywords(  AMethodName  : PAnsiChar;
                                              ADelphiMethod: TDelphiMethodWithKW;
                                              ADocString : PAnsiChar ) : PPyMethodDef;
      procedure ClearMethods;

      // properties
      property MethodCount : Integer read FMethodCount;
      property Methods[ idx : Integer ] : PPyMethodDef read GetMethods;
      property MethodsData : PPyMethodDef read FMethods;
      property ModuleDef : PyModuleDef read FModuleDef;

    published
      property Events: TEventDefs read fEventDefs write fEventDefs stored StoreEventDefs;
  end;


//------------------------------------------------------------
//--                                                        --
//--class: TMembersContainer derived from TMethodsContainer --
//--                                                        --
//------------------------------------------------------------

  TMemberArray = array[ 0 .. 16000 ] of PyMemberDef;
  PMemberArray = ^TMemberArray;

  // class TMembersContainer
  TMembersContainer = class(TMethodsContainer)
    protected
      function  GetMembersStartOffset : Integer; virtual;
    private
      FMemberCount : Integer;
      FAllocatedMemberCount : Integer;
      FMembers : PPyMemberDef;

      procedure AllocMembers;
      procedure FreeMembers;
      function  GetMembers( idx : Integer ) : PPyMemberDef;

    protected
      procedure ReallocMembers; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure AddMember( MemberName  : PAnsiChar;
                           MemberType  : TPyMemberType;
                           MemberOffset : NativeInt;
                           MemberFlags : TPyMemberFlag;
                           MemberDoc : PAnsiChar );
      procedure ClearMembers;
      procedure Finalize; override;

      // properties
      property MemberCount : Integer read FMemberCount;
      property Members[ idx : Integer ] : PPyMemberDef read GetMembers;
      property MembersData : PPyMemberDef read FMembers;
  end;

//------------------------------------------------------------
//--                                                        --
//--class: TGetSetContainer derived from TMembersContainer  --
//--                                                        --
//------------------------------------------------------------

  TGetSetArray = array[ 0 .. 16000 ] of PyGetSetDef;
  PGetSetArray = ^TGetSetArray;

  // class TGetSetContainer
  TGetSetContainer = class(TMembersContainer)
    private
      FGetSetCount : Integer;
      FAllocatedGetSetCount : Integer;
      FGetSets : PPyGetSetDef;

      procedure AllocGetSets;
      procedure FreeGetSets;
      function  GetGetSet( idx : Integer ) : PPyGetSetDef;

    protected
      procedure ReallocGetSets; virtual;

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // public methods
      procedure AddGetSet( AName  : PAnsiChar;
                           AGet : getter;
                           ASet : setter;
                           ADoc : PAnsiChar;
                           AClosure : Pointer);
      procedure ClearGetSets;
      procedure Finalize; override;

      // properties
      property GetSetCount : Integer read FGetSetCount;
      property GetSet[ idx : Integer ] : PPyGetSetDef read GetGetSet;
      property GetSetData : PPyGetSetDef read FGetSets;
  end;

//-------------------------------------------------------
//--                                                   --
//--class: TPythonModule derived from TMethodsContainer--
//--                                                   --
//-------------------------------------------------------

  TPythonModule = class; // forward declaration
  TErrors = class; // forward declaration

  TErrorType = (etString, etClass);

  TParentClassError = class(TPersistent)
    protected
      FName : AnsiString;
      FModule : AnsiString;
    public
      procedure AssignTo( Dest: TPersistent ); override;
    published
      property Module : AnsiString read FModule write FModule;
      property Name : AnsiString read FName write FName;
  end;

  TError = class(TCollectionItem)
  protected
    FName        : AnsiString;
    FText        : AnsiString;
    FError       : PPyObject;
    FErrorType   : TErrorType;
    FParentClass : TParentClassError;

    function GetDisplayName: string; override;
    procedure SetName( const Value : AnsiString );
    procedure SetText( const Value : AnsiString );
    procedure SetErrorType( Value : TErrorType );
    procedure SetParentClass( Value : TParentClassError );
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildError( const ModuleName : AnsiString );
    procedure RaiseError( const msg : AnsiString );
    procedure RaiseErrorObj( const msg : AnsiString; obj : PPyObject );
    function  Owner : TErrors;
    property Error : PPyObject read FError write FError;
  published
    property Name : AnsiString read FName write SetName;
    property Text : AnsiString read FText write SetText;
    property ErrorType : TErrorType read FErrorType write SetErrorType;
    property ParentClass : TParentClassError read FParentClass write SetParentClass;
  end;

  TErrors = class(TCollection)
  private
    FModule: TPythonModule;
    function GetError(Index: Integer): TError;
    procedure SetError(Index: Integer; Value: TError);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Module: TPythonModule);
    function  Add: TError;
    function  Owner : TPythonModule;
    property Items[Index: Integer]: TError read GetError write SetError; default;
  end;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonModule = class(TMethodsContainer)
    protected
      FModuleName : AnsiString;
      FModule : PPyObject;
      FClients : TList;
      FErrors : TErrors;
      FOnAfterInitialization : TNotifyEvent;
      FDocString : TStringList;

      function GetClientCount : Integer;
      function GetClients( idx : Integer ) : TEngineClient;
      procedure SetErrors( val : TErrors );
      procedure SetModuleName( const val : AnsiString );
      procedure SetDocString( value : TStringList );

    public
      // Constructors & destructors
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      // Public methods
      procedure MakeModule;
      procedure DefineDocString;
      procedure Initialize; override;
      procedure InitializeForNewInterpreter;
      procedure AddClient( client : TEngineClient );
      function  ErrorByName( const AName : AnsiString ) : TError;
      procedure RaiseError( const error, msg : AnsiString );
      procedure RaiseErrorFmt( const error, format : AnsiString; Args : array of const );
      procedure RaiseErrorObj( const error, msg : AnsiString; obj : PPyObject );
      procedure BuildErrors;
      procedure SetVar( const varName : AnsiString; value : PPyObject );
      function  GetVar( const varName : AnsiString ) : PPyObject;
      procedure DeleteVar( const varName : AnsiString );
      procedure ClearVars;
      procedure SetVarFromVariant( const varName : AnsiString; const value : Variant );
      function  GetVarAsVariant( const varName: AnsiString ) : Variant;

      // Public properties
      property Module : PPyObject read FModule;
      property Clients[ idx : Integer ] : TEngineClient read GetClients;
      property ClientCount : Integer read GetClientCount;

    published
      property DocString : TStringList read FDocString write SetDocString;
      property ModuleName : AnsiString read FModuleName write SetModuleName;
      property Errors : TErrors read FErrors write SetErrors;
      property OnAfterInitialization : TNotifyEvent read FOnAfterInitialization write FOnAfterInitialization;
  end;


//-------------------------------------------------------
//--                                                   --
//--class:  TPythonType  derived from TGetSetContainer --
//--                                                   --
//-------------------------------------------------------

type
  TPythonType = class; //forward declaration

{
        A                    B                                                      C
        +-------------------++------------------------------------------------------+
        | PyObject header   ||             TPyObject class                          |
        +----------+--------++-----------------+------------+----------+------------+
        |ob_refcnt |ob_type ||hidden Class Ptr |PythonType  |IsSubType |PythonAlloc |
        |integer   |pointer ||pointer          |TPythonType |Boolean   |Boolean     |
        |4 bytes   |4 bytes ||4 bytes          |4 bytes     |1 byte    |1 byte      |
        +----------+--------++-----------------+------------+----------+------------+

        ^                    ^
        |                    |
        ptr returned         ptr returned by Adjust
        by GetSelf

        - a Python object must start at A.
        - a Delphi class class must start at B
        - TPyObject.InstanceSize will return C-B
        - Sizeof(TPyObject) will return C-B
        - The total memory allocated for a TPyObject instance will be C-A,
          even if its InstanceSize is C-B.
        - When turning a Python object pointer into a Delphi instance pointer, PythonToDelphi
          will offset the pointer from A to B.
        - When turning a Delphi instance into a Python object pointer, GetSelf will offset
          Self from B to A.
        - Properties ob_refcnt and ob_type will call GetSelf to access their data.
}
  // The base class of all new Python types
  TPyObject = class
  private
    function  Get_ob_refcnt: NativeInt;
    function  Get_ob_type: PPyTypeObject;
    procedure Set_ob_refcnt(const Value: NativeInt);
    procedure Set_ob_type(const Value: PPyTypeObject);
  public
    PythonType     : TPythonType;
    IsSubtype      : Boolean;
    PythonAlloc    : Boolean;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); virtual;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); virtual;
    destructor  Destroy; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;

    // Misc
    function  GetSelf : PPyObject;
    procedure IncRef;
    procedure Adjust(PyPointer: Pointer);
    function  GetModule : TPythonModule;

    property ob_refcnt : NativeInt read Get_ob_refcnt write Set_ob_refcnt;
    property ob_type   : PPyTypeObject read Get_ob_type write Set_ob_type;

    // Type services
    ////////////////

    // Basic services
    function  Print( var f: file; i: integer) : Integer; virtual;
    function  GetAttr(key : PAnsiChar) : PPyObject; virtual;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; virtual;
    function  Repr : PPyObject; virtual;
    function  Compare( obj: PPyObject) : Integer; virtual;
    function  Hash : NativeInt; virtual;
    function  Str: PPyObject; virtual;
    function  GetAttrO( key: PPyObject) : PPyObject; virtual;
    function  SetAttrO( key, value: PPyObject) : Integer; virtual;
    function  Call( ob1, ob2 : PPyObject) : PPyObject; virtual;
    function  Traverse( proc: visitproc; ptr: Pointer) : integer; virtual;
    function  Clear: integer; virtual;
    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; virtual;
    function  Iter : PPyObject; virtual;
    function  IterNext : PPyObject; virtual;
    function  Init( args, kwds : PPyObject ) : Integer; virtual;

    // Number services
    function  NbAdd( obj : PPyObject) : PPyObject; virtual;
    function  NbSubstract( obj : PPyObject) : PPyObject; virtual;
    function  NbMultiply( obj : PPyObject) : PPyObject; virtual;
    function  NbDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbRemainder( obj : PPyObject) : PPyObject; virtual;
    function  NbDivmod( obj : PPyObject) : PPyObject; virtual;
    function  NbPower( ob1, ob2 : PPyObject) : PPyObject; virtual;
    function  NbNegative : PPyObject; virtual;
    function  NbPositive : PPyObject; virtual;
    function  NbAbsolute : PPyObject; virtual;
    function  NbNonZero : Integer; virtual;
    function  NbInvert : PPyObject; virtual;
    function  NbLShift( obj : PPyObject) : PPyObject; virtual;
    function  NbRShift( obj : PPyObject) : PPyObject; virtual;
    function  NbAnd( obj : PPyObject) : PPyObject; virtual;
    function  NbXor( obj : PPyObject) : PPyObject; virtual;
    function  NbOr( obj : PPyObject) : PPyObject; virtual;
    function  NbCoerce( obj : PPPyObject) : Integer; virtual;
    function  NbInt : PPyObject; virtual;
    function  NbLong : PPyObject; virtual;
    function  NbFloat : PPyObject; virtual;
    function  NbOct : PPyObject; virtual;
    function  NbHex : PPyObject; virtual;
    function  NbInplaceAdd( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceSubtract( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceMultiply( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceDivide( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbInplaceTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbInplaceRemainder( obj : PPyObject): PPyObject; virtual;
    function  NbInplacePower( ob1, ob2 : PPyObject): PPyObject; virtual;
    function  NbInplaceLshift( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceRshift( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceAnd( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceXor( obj : PPyObject): PPyObject; virtual;
    function  NbInplaceOr( obj : PPyObject): PPyObject; virtual;
    // Sequence services
    function  SqLength : NativeInt; virtual;
    function  SqConcat( obj : PPyObject) : PPyObject; virtual;
    function  SqRepeat( val : NativeInt ) : PPyObject; virtual;
    function  SqItem( idx : NativeInt ) : PPyObject; virtual;
    function  SqSlice( idx1, idx2 : NativeInt ) : PPyObject; virtual;
    function  SqAssItem( idx : NativeInt; obj : PPyObject) : Integer; virtual;
    function  SqAssSlice( idx1, idx2 : NativeInt; obj : PPyObject): integer; virtual;
    function  SqContains( obj: PPyObject): integer; virtual;
    function  SqInplaceConcat( obj : PPyObject): PPyObject; virtual;
    function  SqInplaceRepeat( i: NativeInt): PPyObject; virtual;
    // Mapping services
    function  MpLength : NativeInt; virtual;
    function  MpSubscript( obj : PPyObject) : PPyObject; virtual;
    function  MpAssSubscript( obj1, obj2 : PPyObject) : Integer; virtual;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); virtual;
    class procedure RegisterMembers( APythonType : TPythonType ); virtual;
    class procedure RegisterGetSets( APythonType : TPythonType ); virtual;
    class procedure SetupType( APythonType : TPythonType ); virtual;
  end;
  TPyObjectClass = class of TPyObject;

  TBasicServices     = set of (bsPrint, bsGetAttr, bsSetAttr,
                               bsRepr, bsCompare, bsHash,
                               bsStr, bsGetAttrO, bsSetAttrO,
                               bsCall,
                               // since version 2.0
                               bsTraverse, bsClear,
                               // since version 2.1
                               bsRichCompare,
                               // since version 2.2
                               bsIter, bsIterNext);
  TNumberServices    = set of (nsAdd, nsSubstract, nsMultiply,
                               nsDivide, nsRemainder, nsDivmod,
                               nsPower, nsNegative, nsPositive,
                               nsAbsolute, nsNonZero, nsInvert,
                               nsLShift, nsRShift, nsAnd,
                               nsXor, nsOr, nsCoerce,
                               nsInt, nsLong, nsFloat,
                               nsOct, nsHex,
                               // since version 2.2
                               nsFloorDivide, nsTrueDivide);

  // TInplaceNumberServices exists since version 2.0
  TInplaceNumberServices = set of (nsInplaceAdd, nsInplaceSubtract,
                                   nsInplaceMultiply, nsInplaceDivide,
                                   nsInplaceRemainder, nsInplacePower,
                                   nsInplaceLShift, nsInplaceRShift,
                                   nsInplaceAnd, nsInplaceXor, nsInplaceOr,
                                   // since version 2.2
                                   nsInplaceFloorDivide, nsInplaceTrueDivide);

  TSequenceServices  = set of (ssLength, ssConcat, ssRepeat,
                               ssItem, ssSlice, ssAssItem,
                               ssAssSlice,
                               // since version 2.0
                               ssContains,
                               ssInplaceConcat,
                               ssInplaceRepeat
                               );

  TMappingServices   = set of (msLength, msSubscript, msAssSubscript);

  TTypeServices = class(TPersistent)
    protected
      FBasic          : TBasicServices;
      FNumber         : TNumberServices;
      FSequence       : TSequenceServices;
      FMapping        : TMappingServices;
      FInplaceNumber  : TInplaceNumberServices;

    public
      constructor Create;
      procedure AssignTo( Dest: TPersistent ); override;

    published
      property Basic : TBasicServices read FBasic write FBasic;
      property InplaceNumber : TInplaceNumberServices read FInplaceNumber Write FInplaceNumber;
      property Number : TNumberServices read FNumber write FNumber;
      property Sequence : TSequenceServices read FSequence write FSequence;
      property Mapping : TMappingServices read FMapping write FMapping;
  end;

  // The component that initializes the Python type and
  // that creates instances of itself.
  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonType = class(TGetSetContainer)
    protected
      FType : PyTypeObject;
      FTypeName : AnsiString;
      FModule : TPythonModule;
      FPyObjectClass : TPyObjectClass;
      FPrefix : AnsiString;
      FCreateFuncName : AnsiString;
      FServices : TTypeServices;
      FNumber:   PyNumberMethods;
      FSequence: PySequenceMethods;
      FMapping:  PyMappingMethods;
      FCurrentDocString: AnsiString;
      FDocString: TStringList;
      FCreateFuncDoc : AnsiString;
      FInstanceCount : Integer;
      FCreateHits : Integer;
      FDeleteHits : Integer;
      FTypeFlags : TPFlags;
      FCreateFunc : PPyObject;
      FCreateFuncDef : PyMethodDef;
      FGenerateCreateFunction: Boolean;

      procedure Notification( AComponent: TComponent;
                              Operation: TOperation); override;
      function  GetTypePtr : PPyTypeObject;
      procedure SetPyObjectClass( val : TPyObjectClass );
      procedure SetModule( val : TPythonModule );
      procedure SetServices( val : TTypeServices );
      procedure SetTypeName( const val : AnsiString );
      function  CreateMethod( pSelf, args : PPyObject ) : PPyObject; cdecl;
      procedure InitServices;
      procedure SetDocString( value : TStringList );
      function  TypeFlagsAsInt : LongInt;
      function  GetMembersStartOffset : Integer; override;
      procedure ModuleReady(Sender : TObject); override;
      procedure ReallocMethods; override;
      procedure ReallocMembers; override;
      procedure ReallocGetSets; override;

      // Type services
      // They will be all forwarded to the Delphi class that
      // implements the object through the use of virtual
      // methods
      ///////////////////////////////////////
      function  NewSubtypeInst( aType: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;

    public
      constructor Create( AOwner : TComponent ); override;
      destructor  Destroy; override;

      procedure Initialize; override;
      procedure Finalize; override;
      function  CreateInstance : PPyObject;
      function  CreateInstanceWith( args : PPyObject ) : PPyObject;
      procedure AddTypeVar;

      property TheType : PyTypeObject read FType write FType;
      property TheTypePtr : PPyTypeObject read GetTypePtr;
      property PyObjectClass : TPyObjectClass read FPyObjectClass write SetPyObjectClass stored False;
      property InstanceCount : Integer read FInstanceCount;
      property CreateHits : Integer read FCreateHits;
      property DeleteHits : Integer read FDeleteHits;

    published
      property DocString : TStringList read FDocString write SetDocString;
      property TypeName : AnsiString read FTypeName write SetTypeName;
      property TypeFlags : TPFlags read FTypeFlags write FTypeFlags default TPFLAGS_DEFAULT;
      property Prefix : AnsiString read FPrefix write FPrefix;
      property Module : TPythonModule read FModule write SetModule;
      property Services : TTypeServices read FServices write SetServices;
      property GenerateCreateFunction : Boolean read fGenerateCreateFunction write fGenerateCreateFunction default True;
  end;

//-------------------------------------------------------
//--                                                   --
//--  class: TPythonVar derived from TEngineClient     --
//--                                                   --
//-------------------------------------------------------

  TGetDataEvent = procedure ( Sender : TObject; var Data : Variant ) of Object;
  TSetDataEvent = procedure ( Sender : TObject; Data : Variant ) of Object;
  TExtGetDataEvent = procedure ( Sender : TObject; var Data : PPyObject ) of Object;
  TExtSetDataEvent = procedure ( Sender : TObject; Data : PPyObject) of Object;

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPythonDelphiVar = class( TEngineClient )
    protected
      FModule    : AnsiString;
      FVarName   : AnsiString;
      FVarObject : PPyObject;
      FOnGetData : TGetDataEvent;
      FOnSetData : TSetDataEvent;
      FOnExtGetData : TExtGetDataEvent;
      FOnExtSetData : TExtSetDataEvent;
      FOnChange  : TNotifyEvent;

      procedure CreateVarType;
      procedure CreateVar;
      function  GetValue : Variant;
      procedure SetValue( const val : Variant );
      function  GetValueAsPyObject : PPyObject;
      procedure SetValueFromPyObject( val : PPyObject );
      function  GetValueAsString : string;
      procedure SetVarName( const val : AnsiString );

    public
      // Constructors & Destructors
      constructor Create( AOwner : TComponent ); override;

      // Public methods
      procedure Initialize; override;
      procedure Finalize; override;
      function  IsVariantOk( const v : Variant ) : Boolean;

      // Public properties
      property Value : Variant read GetValue write SetValue;
      // Warning: ValueObject returns a preincremented object !
      property ValueObject : PPyObject read GetValueAsPyObject write SetValueFromPyObject;
      property ValueAsString : string read GetValueAsString;
      property VarObject : PPyObject read FVarObject write FVarObject;

    published
      property Module    : AnsiString read FModule write FModule;
      property VarName   : AnsiString read FVarName write SetVarName;
      property OnGetData : TGetDataEvent read FOnGetData write FOnGetData;
      property OnSetData : TSetDataEvent read FOnSetData write FOnSetData;
      property OnExtGetData : TExtGetDataEvent read FOnExtGetData write FOnExtGetData;
      property OnExtSetData : TExtSetDataEvent read FOnExtSetData write FOnExtSetData;
      property OnChange  : TNotifyEvent read FOnChange write FOnChange;
  end;

  TPyVar = class(TPyObject)
  public
    dv_var         : Variant;
    dv_component   : TPythonDelphiVar;
    dv_object      : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
    destructor  Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( APythonType : TPythonType ); override;

    // Methods of TPyVar
    function GetValue : PPyObject;
    function GetValueAsVariant : Variant;
    procedure SetValue( value : PPyObject );
    procedure SetValueFromVariant( const value : Variant );

    // Interface methods
  end;

//#######################################################
//##                                                   ##
//##  Thread Object with Python interpreter lock       ##
//##                                                   ##
//#######################################################
  TThreadExecMode = (emNewState, emNewInterpreter);

{$HINTS OFF}
  TPythonThread = class(TThread)
  private
    fThreadState:      PPyThreadState;
    f_savethreadstate: PPyThreadState;
    fThreadExecMode:   TThreadExecMode;

// Do not overwrite Execute! Use ExecuteWithPython instead!
    procedure Execute; override;
  protected
    procedure ExecuteWithPython; virtual; abstract;

    procedure Py_Begin_Allow_Threads;
    procedure Py_End_Allow_Threads;
// The following procedures are redundant and only for
// compatibility to the C API documentation.
    procedure Py_Begin_Block_Threads;
    procedure Py_Begin_Unblock_Threads;

  public
    property ThreadState : PPyThreadState read  fThreadState;
    property ThreadExecMode: TThreadExecMode read fThreadExecMode write fThreadExecMode;
  end;
{$HINTS ON}

//#######################################################
//##                                                   ##
//##        New Python objects                         ##
//##                                                   ##
//#######################################################

//#######################################################
//##                                                   ##
//##    Methods for new Python objects or modules      ##
//##                                                   ##
//#######################################################

// Module pyio for Python Input/Outputs
function  pyio_write(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_read(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_SetDelayWrites(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_SetMaxLines(self, args : PPyObject) : PPyObject; cdecl;
function  pyio_GetTypesStats(self, args : PPyObject) : PPyObject; cdecl;


//#######################################################
//##                                                   ##
//##        Global procedures                          ##
//##                                                   ##
//#######################################################

function  GetPythonEngine : TPythonEngine;
function  PythonOK : Boolean;
function  PythonToDelphi( obj : PPyObject ) : TPyObject;
function  IsDelphiObject( obj : PPyObject ) : Boolean;
procedure PyObjectDestructor( pSelf : PPyObject); cdecl;
procedure FreeSubtypeInst(ob:PPyObject); cdecl;
procedure Register;
function  PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;
function GetPythonVersionFromDLLName(const DLLFileName : String): String;

{ Helper functions}
(*
    Checks whether the PythonVersion x.x is Registered
*)
{$IFDEF MSWINDOWS}
function IsPythonVersionRegistered(PythonVersion : string;
  out InstallPath: string; out AllUserInstall: Boolean) : Boolean;
{$ENDIF}
(*
  Mask FPU Excptions - Useful for importing SciPy and other Python libs
  See http://bugs.python.org/issue9980 and
  http://stackoverflow.com/questions/3933851/
*)
procedure MaskFPUExceptions(ExceptionsMasked : boolean;
  MatchPythonPrecision : Boolean = True);
(*
  Converts line breaks to LF and optionally adds a line break at the end
*)
function CleanString(const s : AnsiString; AppendLF : Boolean = True) : AnsiString; overload;
function CleanString(const s : string; AppendLF : Boolean = True) : string; overload;

//#######################################################
//##                                                   ##
//##        Global variables                           ##
//##                                                   ##
//#######################################################


implementation

uses
{$IFDEF FPC}
  StrUtils,
{$ELSE}
  AnsiStrings,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Math,
  Registry;
{$ENDIF}


(*******************************************************)
(**                                                   **)
(**            Globals                                **)
(**                                                   **)
(*******************************************************)

var
  gPythonEngine : TPythonEngine;
  gVarType : TPythonType;


(*******************************************************)
(**                                                   **)
(**            class TPythonInputOutput               **)
(**                                                   **)
(*******************************************************)

constructor TPythonInputOutput.Create( AOwner : TComponent );
begin
  inherited;
  FMaxLines      := kMaxLines;
  FQueue         := TIOStringList.Create;
  FDelayWrites   := False;
  FMaxLineLength := kMaxLineLength;
  FLinesPerThread:= TIOStringList.Create;
  FLock          := TCriticalSection.Create;
end;

destructor TPythonInputOutput.Destroy;
begin
  FLinesPerThread.Free;
  FQueue.Free;
  FLock.Free;
  inherited;
end;

procedure TPythonInputOutput.Lock;
begin
  FLock.Enter;
end;

procedure TPythonInputOutput.Unlock;
begin
  FLock.Leave;
end;

procedure TPythonInputOutput.Write( const str : IOString );

  procedure DropLine;
  begin
{$IFDEF MSWINDOWS}
    if DelayWrites then
      AddWrite( FLine_Buffer )
    else
{$ENDIF}
      if UnicodeIO then
        SendUniData( FLine_Buffer )
      else
        SendData( AnsiString(FLine_Buffer) );
    FLine_Buffer := '';
    UpdateCurrentThreadLine;
  end;

var
  i : Integer;
  c : IOChar;
begin
  Lock;
  try
    FLine_Buffer := GetCurrentThreadLine;
    if FRawOutput then begin
      FLine_Buffer := FLine_Buffer  + str;
      DropLine;
    end else begin
      for i := 1 to length(str) do
        begin
          c := str[i];
          if c = #10 then
            DropLine
          else if (c >= ' ') or (c = #09) then
            begin
              Insert( c, FLine_Buffer, length(FLine_Buffer)+1 );
              if Length(FLine_Buffer) > MaxLineLength then
                DropLine;
            end;
        end;
    end;
    UpdateCurrentThreadLine;
  finally
    Unlock;
  end;
end;

procedure TPythonInputOutput.WriteLine( const str : IOString );
begin
  Write( str+#10 );
end;

procedure TPythonInputOutput.AddWrite( const str : IOString );
begin
  FQueue.Add( str );
  if FQueue.Count > FMaxLines then
    FQueue.Delete(0)
  else
    AddPendingWrite;
end;

procedure TPythonInputOutput.SendData( const Data : AnsiString );
begin
  if Assigned(FOnSendData) then
    FOnSendData( Self, Data );
end;

procedure TPythonInputOutput.SendUniData(const Data: UnicodeString);
begin
  if Assigned(FOnSendUniData) then
    FOnSendUniData( Self, Data );
end;

function  TPythonInputOutput.ReceiveData : AnsiString;
begin
  Result := '';
  if Assigned(FOnReceiveData) then
    FOnReceiveData( Self, Result );
end;

function TPythonInputOutput.ReceiveUniData: UnicodeString;
begin
  Result := '';
  if Assigned(FOnReceiveUniData) then
    FOnReceiveUniData( Self, Result );
end;

procedure TPythonInputOutput.AddPendingWrite;
begin
end;

function  TPythonInputOutput.GetCurrentThreadSlotIdx : Integer;
var
  thread_id : Longint;
  i : Integer;
begin
  thread_id := GetCurrentThreadId;
  for i := 0 to FLinesPerThread.Count-1 do
    if Longint(FLinesPerThread.Objects[i]) = thread_id then
      begin
        Result := i;
        Exit;
      end;
  Result := FLinesPerThread.AddObject( '', TObject(thread_id) );
end;

function  TPythonInputOutput.GetCurrentThreadLine : IOString;
begin
  Result := FLinesPerThread.Strings[ GetCurrentThreadSlotIdx ];
end;

procedure TPythonInputOutput.UpdateCurrentThreadLine;
begin
  FLinesPerThread.Strings[ GetCurrentThreadSlotIdx ] := FLine_Buffer;
end;

(*******************************************************)
(**                                                   **)
(**            class TDynamicDll                      **)
(**                                                   **)
(*******************************************************)

procedure TDynamicDll.DoOpenDll(const aDllName : String);
begin
  if not IsHandleValid then
  begin
    FDllName := aDllName;
    FDLLHandle := SafeLoadLibrary(
      {$IFDEF FPC}
        PAnsiChar(AnsiString(GetDllPath+DllName))
      {$ELSE}
        GetDllPath+DllName
      {$ENDIF}
    );
  end;
end;

function  TDynamicDll.GetDllPath : String;
{$IFDEF MSWINDOWS}
var
  AllUserInstall: Boolean;
{$ENDIF}
begin
  Result := DllPath;

  {$IFDEF MSWINDOWS}
  if DLLPath = '' then begin
    IsPythonVersionRegistered(RegVersion, Result, AllUserInstall);
  end;
  {$ENDIF}

  if Result <> '' then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

procedure  TDynamicDll.OpenDll(const aDllName : String);
var
  s : String;
begin
  UnloadDll;

  BeforeLoad;

  FDLLHandle := 0;

  DoOpenDll(aDllName);

  if not IsHandleValid then begin
{$IFDEF MSWINDOWS}
    s := Format('Error %d: Could not open Dll "%s"',[GetLastError, DllName]);
{$ENDIF}
{$IFDEF LINUX}
    s := Format('Error: Could not open Dll "%s"',[DllName]);
{$ENDIF}
    if FatalMsgDlg then
{$IFDEF MSWINDOWS}
      MessageBox( GetActiveWindow, PChar(s), 'Error', MB_TASKMODAL or MB_ICONSTOP );
{$ENDIF}
{$IFDEF LINUX}
      WriteLn(ErrOutput, s);
{$ENDIF}

    if FatalAbort then
      Quit;
  end else
    AfterLoad;
end;

constructor TDynamicDll.Create(AOwner: TComponent);
begin
  inherited;
  FFatalMsgDlg          := True;
  FFatalAbort           := True;
  FAutoLoad             := True;
  FUseLastKnownVersion  := True;
end;

destructor TDynamicDll.Destroy;
begin
  if AutoUnload then
    UnloadDll;
  inherited;
end;

function TDynamicDll.Import(const funcname: AnsiString; canFail : Boolean = True): Pointer;
var
  E : EDllImportError;
begin
  Result := GetProcAddress( FDLLHandle, PAnsiChar(funcname) );
  if (Result = nil) and canFail then begin
    {$IFDEF MSWINDOWS}
    E := EDllImportError.CreateFmt('Error %d: could not map symbol "%s"', [GetLastError, funcname]);
    E.ErrorCode := GetLastError;
    {$ELSE}
    E := EDllImportError.CreateFmt('Error: could not map symbol "%s"', [funcname]);
    {$ENDIF}
    E.WrongFunc := funcname;
    raise E;
  end;
end;

procedure TDynamicDll.Loaded;
begin
  inherited;
  if AutoLoad and not (csDesigning in ComponentState) then
    LoadDll;
end;

function  TDynamicDll.IsHandleValid : Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (FDLLHandle >= 32);
{$ENDIF}
{$IFDEF LINUX}
  Result := FDLLHandle <> 0;
{$ENDIF}
end;

procedure TDynamicDll.LoadDll;
begin
  OpenDll( DllName );
end;

procedure TDynamicDll.UnloadDll;
begin
  if IsHandleValid then begin
    BeforeUnload;
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
  end;
end;

procedure TDynamicDll.BeforeLoad;
begin
  if Assigned( FOnBeforeLoad ) then
    FOnBeforeLoad( Self );
end;

procedure TDynamicDll.AfterLoad;
begin
  if Assigned( FOnAfterLoad ) then
    FOnAfterLoad( Self );
end;

procedure TDynamicDll.BeforeUnload;
begin
  if Assigned( FOnBeforeUnload ) then
    FOnBeforeUnload( Self );
end;

function  TDynamicDll.GetQuitMessage : String;
begin
  Result := Format( 'Dll %s could not be loaded. We must quit.', [DllName]);
end;

procedure TDynamicDll.Quit;
begin
  if not( csDesigning in ComponentState ) then begin
{$IFDEF MSWINDOWS}
    MessageBox( GetActiveWindow, PChar(GetQuitMessage), 'Error', MB_TASKMODAL or MB_ICONSTOP );
    ExitProcess( 1 );
{$ELSE}
    WriteLn(ErrOutput, GetQuitMessage);
{$IFDEF FPC}
    Halt( 1 );
{$ELSE}
    __exit(1);
{$ENDIF}
{$ENDIF}
  end;
end;

function TDynamicDll.IsAPIVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TDynamicDll.IsDllNameStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

function TDynamicDll.IsRegVersionStored: Boolean;
begin
  Result := not UseLastKnownVersion;
end;

procedure TDynamicDll.SetDllName(const Value: String);
begin
  FDllName := Value;
end;


(*******************************************************)
(**                                                   **)
(**            class TPythonInterface                 **)
(**                                                   **)
(*******************************************************)

constructor TPythonInterface.Create(AOwner: TComponent);
var
  i : Integer;
begin
  inherited;
  i := COMPILED_FOR_PYTHON_VERSION_INDEX;
  DllName     := PYTHON_KNOWN_VERSIONS[i].DllName;
  FAPIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
  FRegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
  FAutoUnload := True;
end;

procedure TPythonInterface.AfterLoad;
begin
  inherited;
  FIsPython3000 := Pos('PYTHON3', UpperCase(DLLName)) >  0;
  FMajorVersion := StrToInt(DLLName[7 {$IFDEF LINUX}+3{$ENDIF}]);
  FMinorVersion := StrToInt(DLLName[8{$IFDEF LINUX}+4{$ENDIF}]);


  if FIsPython3000 then
    FBuiltInModuleName := 'builtins'
  else
    FBuiltInModuleName := '__builtin__';

  try
    MapDll;
  except
    on E: Exception do begin
      if FatalMsgDlg then
{$IFDEF MSWINDOWS}
        MessageBox( GetActiveWindow, PChar(E.Message), 'Error', MB_TASKMODAL or MB_ICONSTOP );
{$ELSE}
        WriteLn( ErrOutput, E.Message );
{$ENDIF}
      if FatalAbort then Quit;
    end;
  end;
end;

function  TPythonInterface.GetQuitMessage : String;
begin
  Result := Format( 'Python could not be properly initialized. We must quit.', [DllName]);
end;

function TPythonInterface.GetInitialized: Boolean;
begin
  if Assigned(Py_IsInitialized) then
    Result := Py_IsInitialized() <> 0
  else
    Result := FInitialized;
end;

procedure TPythonInterface.CheckPython;
begin
  if not Initialized then
    raise Exception.Create('Python is not properly initialized' );
end;

function  TPythonInterface.GetUnicodeTypeSuffix : String;
begin
  if (fMajorVersion > 3) or ((fMajorVersion = 3) and (fMinorVersion >= 3)) then
    Result := ''
  else if APIVersion >= 1011 then
    Result :=
      {$IF DEFINED(MSWINDOWS) or DEFINED(DARWIN) or DEFINED(SOLARIS)}
        'UCS2'
      {$ELSE}
        'UCS4'
      {$ENDIF}
  else
    Result := '';
end;

procedure TPythonInterface.MapDll;
Var
  UnicodeSuffix : string;

begin
  UnicodeSuffix := GetUnicodeTypeSuffix;

  Py_DebugFlag               := Import('Py_DebugFlag');
  Py_VerboseFlag             := Import('Py_VerboseFlag');
  Py_InteractiveFlag         := Import('Py_InteractiveFlag');
  Py_OptimizeFlag            := Import('Py_OptimizeFlag');
  Py_NoSiteFlag              := Import('Py_NoSiteFlag');
  Py_FrozenFlag              := Import('Py_FrozenFlag');
  if not IsPython3000 then begin
    Py_UseClassExceptionsFlag  := Import('Py_UseClassExceptionsFlag');
    Py_TabcheckFlag            := Import('Py_TabcheckFlag');
    Py_UnicodeFlag             := Import('Py_UnicodeFlag');
  end;
  if (fMajorVersion < 3) or ((fMajorVersion = 3) and (fMinorVersion <3)) then
    Py_DivisionWarningFlag     := Import('Py_DivisionWarningFlag');

  Py_IgnoreEnvironmentFlag   := Import('Py_IgnoreEnvironmentFlag');

  //_PySys_TraceFunc           := Import('_PySys_TraceFunc');
  //_PySys_ProfileFunc         := Import('_PySys_ProfileFunc');

  Py_None                    := Import('_Py_NoneStruct');
  Py_Ellipsis                := Import('_Py_EllipsisObject');
  if IsPython3000 then
    Py_False                   := Import('_Py_FalseStruct')
  else
    Py_False                   := Import('_Py_ZeroStruct');
  Py_True                    := Import('_Py_TrueStruct');
  Py_NotImplemented          := Import('_Py_NotImplementedStruct');

  PyImport_FrozenModules     := Import('PyImport_FrozenModules');

  PyExc_AttributeError       := Import('PyExc_AttributeError');
  PyExc_EOFError             := Import('PyExc_EOFError');
  PyExc_IOError              := Import('PyExc_IOError');
  PyExc_ImportError          := Import('PyExc_ImportError');
  PyExc_IndexError           := Import('PyExc_IndexError');
  PyExc_KeyError             := Import('PyExc_KeyError');
  PyExc_KeyboardInterrupt    := Import('PyExc_KeyboardInterrupt');
  PyExc_MemoryError          := Import('PyExc_MemoryError');
  PyExc_NameError            := Import('PyExc_NameError');
  PyExc_OverflowError        := Import('PyExc_OverflowError');
  PyExc_RuntimeError         := Import('PyExc_RuntimeError');
  PyExc_SyntaxError          := Import('PyExc_SyntaxError');
  PyExc_SystemError          := Import('PyExc_SystemError');
  PyExc_SystemExit           := Import('PyExc_SystemExit');
  PyExc_TypeError            := Import('PyExc_TypeError');
  PyExc_ValueError           := Import('PyExc_ValueError');
  PyExc_ZeroDivisionError    := Import('PyExc_ZeroDivisionError');
  PyExc_ArithmeticError      := Import('PyExc_ArithmeticError');
  PyExc_Exception            := Import('PyExc_Exception');
  PyExc_FloatingPointError   := Import('PyExc_FloatingPointError');
  PyExc_LookupError          := Import('PyExc_LookupError');
  if not IsPython3000 then begin
    PyExc_StandardError        := Import('PyExc_StandardError');
    PyExc_MemoryErrorInst      := Import('PyExc_MemoryErrorInst');
  end;
  PyExc_AssertionError       := Import('PyExc_AssertionError');
  PyExc_EnvironmentError     := Import('PyExc_EnvironmentError');
  PyExc_IndentationError     := Import('PyExc_IndentationError');
  PyExc_NotImplementedError  := Import('PyExc_NotImplementedError');
  PyExc_OSError              := Import('PyExc_OSError');
  PyExc_TabError             := Import('PyExc_TabError');
  PyExc_UnboundLocalError    := Import('PyExc_UnboundLocalError');
  PyExc_UnicodeError         := Import('PyExc_UnicodeError');
  {$IFDEF MSWINDOWS}
    PyExc_WindowsError       := Import('PyExc_WindowsError');
  {$ENDIF}
  PyExc_Warning              := Import('PyExc_Warning');
  PyExc_DeprecationWarning   := Import('PyExc_DeprecationWarning');
  PyExc_RuntimeWarning       := Import('PyExc_RuntimeWarning');
  PyExc_SyntaxWarning        := Import('PyExc_SyntaxWarning');
  PyExc_UserWarning          := Import('PyExc_UserWarning');
  PyExc_ReferenceError       := Import('PyExc_ReferenceError');
  PyExc_StopIteration        := Import('PyExc_StopIteration');
  PyExc_FutureWarning        := Import('PyExc_FutureWarning');
  PyExc_PendingDeprecationWarning:= Import('PyExc_PendingDeprecationWarning');
  PyExc_UnicodeDecodeError   := Import('PyExc_UnicodeDecodeError');
  PyExc_UnicodeEncodeError   := Import('PyExc_UnicodeEncodeError');
  PyExc_UnicodeTranslateError:= Import('PyExc_UnicodeTranslateError');
  PyType_Type                := Import('PyType_Type');
  PyCFunction_Type           := Import('PyCFunction_Type');
  if not IsPython3000 then begin
    PyCObject_Type             := Import('PyCObject_Type');   // Removed in Python 3.2
    PyClass_Type               := Import('PyClass_Type');
  end;
  PyCode_Type                := Import('PyCode_Type');
  PyComplex_Type             := Import('PyComplex_Type');
  PyDict_Type                := Import('PyDict_Type');
  if not IsPython3000 then
    PyFile_Type                := Import('PyFile_Type');
  PyFloat_Type               := Import('PyFloat_Type');
  PyFrame_Type               := Import('PyFrame_Type');
  PyFunction_Type            := Import('PyFunction_Type');
  if not IsPython3000 then
    PyInstance_Type            := Import('PyInstance_Type');
  if not IsPython3000 then
    PyInt_Type                 := Import('PyInt_Type');
  PyList_Type                := Import('PyList_Type');
  PyLong_Type                := Import('PyLong_Type');
  PyMethod_Type              := Import('PyMethod_Type');
  PyModule_Type              := Import('PyModule_Type');
  PyObject_Type              := Import('PyObject_Type');
  PyRange_Type               := Import('PyRange_Type');
  PySlice_Type               := Import('PySlice_Type');
  if not IsPython3000 then
    PyString_Type              := Import('PyString_Type')
  else
    PyString_Type              := Import('PyBytes_Type');
  PyTuple_Type               := Import('PyTuple_Type');
  PyUnicode_Type             := Import('PyUnicode_Type');
  PyBaseObject_Type          := Import('PyBaseObject_Type');
  if not IsPython3000 then
    PyBuffer_Type              := Import('PyBuffer_Type');
  PyCallIter_Type            := Import('PyCallIter_Type');
  PyCell_Type                := Import('PyCell_Type');
  PyClassMethod_Type         := Import('PyClassMethod_Type');
  PyProperty_Type            := Import('PyProperty_Type');
  PySeqIter_Type             := Import('PySeqIter_Type');
  PyStaticMethod_Type        := Import('PyStaticMethod_Type');
  PySuper_Type               := Import('PySuper_Type');
  PyTraceBack_Type           := Import('PyTraceBack_Type');
  PyWrapperDescr_Type        := Import('PyWrapperDescr_Type');
  _PyWeakref_RefType         := Import('_PyWeakref_RefType');
  _PyWeakref_ProxyType       := Import('_PyWeakref_ProxyType');
  _PyWeakref_CallableProxyType:=Import('_PyWeakref_CallableProxyType');
  if not IsPython3000 then
    PyBaseString_Type          := Import('PyBaseString_Type');
  PyBool_Type                := Import('PyBool_Type');
  PyEnum_Type                := Import('PyEnum_Type');

  //@PyArg_GetObject           := Import('PyArg_GetObject');
  //@PyArg_GetLong             := Import('PyArg_GetLong');
  //@PyArg_GetShort            := Import('PyArg_GetShort');
  //@PyArg_GetFloat            := Import('PyArg_GetFloat');
  //@PyArg_GetString           := Import('PyArg_GetString');
  //@PyArgs_VaParse            := Import('PyArgs_VaParse');
  //@Py_VaBuildValue           := Import('Py_VaBuildValue');
  //@PyBuiltin_Init            := Import('PyBuiltin_Init');
  PyComplex_FromCComplex    := Import('PyComplex_FromCComplex');
  PyComplex_FromDoubles     := Import('PyComplex_FromDoubles');
  PyComplex_RealAsDouble    := Import('PyComplex_RealAsDouble');
  PyComplex_ImagAsDouble    := Import('PyComplex_ImagAsDouble');
  PyComplex_AsCComplex      := Import('PyComplex_AsCComplex');
  PyCFunction_GetFunction   := Import('PyCFunction_GetFunction');
  PyCFunction_GetSelf       := Import('PyCFunction_GetSelf');
  PyCallable_Check          := Import('PyCallable_Check');
  if not IsPython3000 then begin
    PyClass_New               := Import('PyClass_New');
    PyClass_IsSubclass        := Import('PyClass_IsSubclass');
    PyCObject_FromVoidPtr     := Import('PyCObject_FromVoidPtr'); // Removed in Python 3.2
    PyCObject_AsVoidPtr       := Import('PyCObject_AsVoidPtr');
  end;
  PyDict_GetItem            := Import('PyDict_GetItem');
  PyDict_SetItem            := Import('PyDict_SetItem');
  PyDict_DelItem            := Import('PyDict_DelItem');
  PyDict_Clear              := Import('PyDict_Clear');
  PyDict_Next               := Import('PyDict_Next');
  PyDict_Keys               := Import('PyDict_Keys');
  PyDict_Values             := Import('PyDict_Values');
  PyDict_Items              := Import('PyDict_Items');
  PyDict_Size               := Import('PyDict_Size');
  PyDict_DelItemString      := Import('PyDict_DelItemString');
  PyDict_Copy               := Import('PyDict_Copy');
  PyDictProxy_New           := Import('PyDictProxy_New');
  if not IsPython3000 then
    Py_InitModule4            := {$IFDEF CPUX64}Import('Py_InitModule4_64'){$ELSE}Import('Py_InitModule4'){$ENDIF}
  else
    PyModule_Create2           := Import('PyModule_Create2');
  PyErr_Print               := Import('PyErr_Print');
  PyErr_SetNone             := Import('PyErr_SetNone');
  PyErr_SetObject           := Import('PyErr_SetObject');
  PyErr_Restore             := Import('PyErr_Restore');
  PyErr_BadArgument         := Import('PyErr_BadArgument');
  PyErr_NoMemory            := Import('PyErr_NoMemory');
  PyErr_SetFromErrno        := Import('PyErr_SetFromErrno');
  PyErr_BadInternalCall     := Import('PyErr_BadInternalCall');
  PyErr_CheckSignals        := Import('PyErr_CheckSignals');
  PyErr_Occurred            := Import('PyErr_Occurred');
  PyErr_Clear               := Import('PyErr_Clear');
  PyErr_Fetch               := Import('PyErr_Fetch');
  PyErr_SetString           := Import('PyErr_SetString');
  PyEval_GetBuiltins        := Import('PyEval_GetBuiltins');
  PyImport_GetModuleDict    := Import('PyImport_GetModuleDict');
  if IsPython3000 then
    PyInt_FromLong          := Import('PyLong_FromLong')
  else
    PyInt_FromLong          := Import('PyInt_FromLong');
  PyArg_Parse               := Import('PyArg_Parse');
  PyArg_ParseTuple          := Import('PyArg_ParseTuple');
  Py_BuildValue             := Import('Py_BuildValue');
  Py_Initialize             := Import('Py_Initialize');
  PyDict_New                := Import('PyDict_New');
  PyDict_SetItemString      := Import('PyDict_SetItemString');
  PyModule_GetDict          := Import('PyModule_GetDict');
  PyObject_Str              := Import('PyObject_Str');
  PyRun_String              := Import('PyRun_String');
  PyRun_SimpleString        := Import('PyRun_SimpleString');
  PyDict_GetItemString      := Import('PyDict_GetItemString');
  if not IsPython3000 then begin
    PyString_AsString         := Import('PyString_AsString');
    PyString_AsStringAndSize  := Import('PyString_AsStringAndSize')
  end else begin
    PyString_AsString         := Import('PyBytes_AsString');
    PyString_AsStringAndSize  := Import('PyBytes_AsStringAndSize');
  end;
  if not IsPython3000 then
    DLL_PyString_FromString   := Import('PyString_FromString');
  if not IsPython3000 then
    PySys_SetArgv             := Import('PySys_SetArgv')
  else
    PySys_SetArgv3000         := Import('PySys_SetArgv');
  Py_Exit                   := Import('Py_Exit');

  if IsPython3000 then
    PyCFunction_NewEx           :=Import('PyCFunction_NewEx')
  else
    PyCFunction_New           :=Import('PyCFunction_New');


  PyEval_CallObjectWithKeywords:=Import('PyEval_CallObjectWithKeywords');
  PyEval_GetFrame           :=Import('PyEval_GetFrame');
  PyEval_GetGlobals         :=Import('PyEval_GetGlobals');
  PyEval_GetLocals          :=Import('PyEval_GetLocals');
  //@PyEval_GetOwner           :=Import('PyEval_GetOwner');
  if not IsPython3000 then
    PyEval_GetRestricted      :=Import('PyEval_GetRestricted');
  PyEval_InitThreads        :=Import('PyEval_InitThreads');
  PyEval_RestoreThread      :=Import('PyEval_RestoreThread');
  PyEval_SaveThread         :=Import('PyEval_SaveThread');
  if not IsPython3000 then
    PyFile_FromString         :=Import('PyFile_FromString');
  PyFile_GetLine            :=Import('PyFile_GetLine');
  if not IsPython3000 then
    PyFile_Name               :=Import('PyFile_Name');
  if not IsPython3000 then
    PyFile_SetBufSize         :=Import('PyFile_SetBufSize');
  if not IsPython3000 then
    PyFile_SoftSpace          :=Import('PyFile_SoftSpace');
  PyFile_WriteObject        :=Import('PyFile_WriteObject');
  PyFile_WriteString        :=Import('PyFile_WriteString');
  PyFloat_AsDouble          :=Import('PyFloat_AsDouble');
  PyFloat_FromDouble        :=Import('PyFloat_FromDouble');
  PyFunction_GetCode        :=Import('PyFunction_GetCode');
  PyFunction_GetGlobals     :=Import('PyFunction_GetGlobals');
  PyFunction_New            :=Import('PyFunction_New');
  PyImport_AddModule        :=Import('PyImport_AddModule');
  PyImport_Cleanup          :=Import('PyImport_Cleanup');
  PyImport_GetMagicNumber   :=Import('PyImport_GetMagicNumber');
  PyImport_ImportFrozenModule:=Import('PyImport_ImportFrozenModule');
  PyImport_ImportModule     :=Import('PyImport_ImportModule');
  PyImport_Import           :=Import('PyImport_Import');
  //PyImport_Init             :=Import('PyImport_Init');
  PyImport_ReloadModule     :=Import('PyImport_ReloadModule');
  if not IsPython3000 then
    PyInstance_New            :=Import('PyInstance_New');
  if IsPython3000 then
    PyInt_AsLong              :=Import('PyLong_AsLong')
  else
    PyInt_AsLong              :=Import('PyInt_AsLong');
  PyList_Append             :=Import('PyList_Append');
  PyList_AsTuple            :=Import('PyList_AsTuple');
  PyList_GetItem            :=Import('PyList_GetItem');
  PyList_GetSlice           :=Import('PyList_GetSlice');
  PyList_Insert             :=Import('PyList_Insert');
  PyList_New                :=Import('PyList_New');
  PyList_Reverse            :=Import('PyList_Reverse');
  PyList_SetItem            :=Import('PyList_SetItem');
  PyList_SetSlice           :=Import('PyList_SetSlice');
  PyList_Size               :=Import('PyList_Size');
  PyList_Sort               :=Import('PyList_Sort');
  PyLong_AsDouble           :=Import('PyLong_AsDouble');
  PyLong_AsLong             :=Import('PyLong_AsLong');
  PyLong_FromDouble         :=Import('PyLong_FromDouble');
  PyLong_FromLong           :=Import('PyLong_FromLong');
  PyLong_FromString         :=Import('PyLong_FromString');
  PyLong_FromString         :=Import('PyLong_FromString');
  PyLong_FromUnsignedLong   :=Import('PyLong_FromUnsignedLong');
  PyLong_AsUnsignedLong     :=Import('PyLong_AsUnsignedLong');
  PyLong_FromUnicode        :=Import('PyLong_FromUnicode');
  PyLong_FromLongLong       :=Import('PyLong_FromLongLong');
  PyLong_AsLongLong         :=Import('PyLong_AsLongLong');
  PyMapping_Check           :=Import('PyMapping_Check');
  PyMapping_GetItemString   :=Import('PyMapping_GetItemString');
  PyMapping_HasKey          :=Import('PyMapping_HasKey');
  PyMapping_HasKeyString    :=Import('PyMapping_HasKeyString');
  PyMapping_Length          :=Import('PyMapping_Length');
  PyMapping_SetItemString   :=Import('PyMapping_SetItemString');
  if not IsPython3000 then
    PyMethod_Class            :=Import('PyMethod_Class');
  PyMethod_Function         :=Import('PyMethod_Function');
  PyMethod_New              :=Import('PyMethod_New');
  PyMethod_Self             :=Import('PyMethod_Self');
  PyModule_GetName          :=Import('PyModule_GetName');
  PyModule_New              :=Import('PyModule_New');
  PyNumber_Absolute         :=Import('PyNumber_Absolute');
  PyNumber_Add              :=Import('PyNumber_Add');
  PyNumber_And              :=Import('PyNumber_And');
  PyNumber_Check            :=Import('PyNumber_Check');
  if not IsPython3000 then
    PyNumber_Coerce           :=Import('PyNumber_Coerce');
  if IsPython3000 then
    PyNumber_Divide           :=Import('PyNumber_TrueDivide')
  else
    PyNumber_Divide           :=Import('PyNumber_Divide');
  PyNumber_FloorDivide      :=Import('PyNumber_FloorDivide');
  PyNumber_TrueDivide       :=Import('PyNumber_TrueDivide');
  PyNumber_Divmod           :=Import('PyNumber_Divmod');
  PyNumber_Float            :=Import('PyNumber_Float');
  if not IsPython3000 then
    PyNumber_Int              :=Import('PyNumber_Int');
  PyNumber_Invert           :=Import('PyNumber_Invert');
  PyNumber_Long             :=Import('PyNumber_Long');
  PyNumber_Lshift           :=Import('PyNumber_Lshift');
  PyNumber_Multiply         :=Import('PyNumber_Multiply');
  PyNumber_Negative         :=Import('PyNumber_Negative');
  PyNumber_Or               :=Import('PyNumber_Or');
  PyNumber_Positive         :=Import('PyNumber_Positive');
  PyNumber_Power            :=Import('PyNumber_Power');
  PyNumber_Remainder        :=Import('PyNumber_Remainder');
  PyNumber_Rshift           :=Import('PyNumber_Rshift');
  PyNumber_Subtract         :=Import('PyNumber_Subtract');
  PyNumber_Xor              :=Import('PyNumber_Xor');
  PyOS_InitInterrupts       :=Import('PyOS_InitInterrupts');
  PyOS_InterruptOccurred    :=Import('PyOS_InterruptOccurred');
  PyObject_CallObject       :=Import('PyObject_CallObject');
  PyObject_CallMethod       :=Import('PyObject_CallMethod');
  if not IsPython3000 then
    PyObject_Compare          :=Import('PyObject_Compare');
  PyObject_RichCompare      :=Import('PyObject_RichCompare');
  PyObject_RichCompareBool  :=Import('PyObject_RichCompareBool');
  PyObject_GetAttr          :=Import('PyObject_GetAttr');
  PyObject_GetAttrString    :=Import('PyObject_GetAttrString');
  PyObject_GetItem          :=Import('PyObject_GetItem');
  PyObject_DelItem          :=Import('PyObject_DelItem');
  PyObject_HasAttrString    :=Import('PyObject_HasAttrString');
  PyObject_Hash             :=Import('PyObject_Hash');
  PyObject_IsTrue           :=Import('PyObject_IsTrue');
  PyObject_Length           :=Import('PyObject_Length');
  PyObject_Repr             :=Import('PyObject_Repr');
  PyObject_SetAttr          :=Import('PyObject_SetAttr');
  PyObject_SetAttrString    :=Import('PyObject_SetAttrString');
  PyObject_SetItem          :=Import('PyObject_SetItem');
  PyObject_Init             :=Import('PyObject_Init');
  PyObject_InitVar          :=Import('PyObject_InitVar');
  PyObject_New              :=Import('_PyObject_New');
  PyObject_NewVar           :=Import('_PyObject_NewVar');
  PyObject_Free             :=Import('PyObject_Free');
  PyObject_GetIter          :=Import('PyObject_GetIter');
  PyIter_Next               :=Import('PyIter_Next');
  PyObject_IsInstance       :=Import('PyObject_IsInstance');
  PyObject_IsSubclass       :=Import('PyObject_IsSubclass');
  PyObject_Call             :=Import('PyObject_Call');
  PyObject_GenericGetAttr   :=Import('PyObject_GenericGetAttr');
  PyObject_GenericSetAttr   :=Import('PyObject_GenericSetAttr');
  PyObject_GC_Malloc         :=Import('_PyObject_GC_Malloc');
  PyObject_GC_New            :=Import('_PyObject_GC_New');
  PyObject_GC_NewVar         :=Import('_PyObject_GC_NewVar');
  PyObject_GC_Resize         :=Import('_PyObject_GC_Resize');
  PyObject_GC_Del            :=Import('PyObject_GC_Del');
  PyObject_GC_Track          :=Import('PyObject_GC_Track');
  PyObject_GC_UnTrack        :=Import('PyObject_GC_UnTrack');
  PySequence_Check          :=Import('PySequence_Check');
  PySequence_Concat         :=Import('PySequence_Concat');
  PySequence_Count          :=Import('PySequence_Count');
  PySequence_GetItem        :=Import('PySequence_GetItem');
  PySequence_GetSlice       :=Import('PySequence_GetSlice');
  PySequence_In             :=Import('PySequence_In');
  PySequence_Index          :=Import('PySequence_Index');
  PySequence_Length         :=Import('PySequence_Length');
  PySequence_Repeat         :=Import('PySequence_Repeat');
  PySequence_SetItem        :=Import('PySequence_SetItem');
  PySequence_SetSlice       :=Import('PySequence_SetSlice');
  PySequence_DelSlice       :=Import('PySequence_DelSlice');
  PySequence_Tuple          :=Import('PySequence_Tuple');
  PySequence_Contains       :=Import('PySequence_Contains');
  PySlice_GetIndices        :=Import('PySlice_GetIndices');
  PySeqIter_New             :=Import('PySeqIter_New');
  PySlice_GetIndicesEx      :=Import('PySlice_GetIndicesEx');
  PySlice_New               :=Import('PySlice_New');
  if not IsPython3000 then begin
    PyString_Concat           :=Import('PyString_Concat');
    PyString_ConcatAndDel     :=Import('PyString_ConcatAndDel');
    PyString_Format           :=Import('PyString_Format');
    PyString_FromStringAndSize:=Import('PyString_FromStringAndSize');
    PyString_Size             :=Import('PyString_Size');
    PyString_DecodeEscape     :=Import('PyString_DecodeEscape');
    PyString_Repr             :=Import('PyString_Repr');
  end else begin
    PyString_Concat           :=Import('PyBytes_Concat');
    PyString_ConcatAndDel     :=Import('PyBytes_ConcatAndDel');
    PyString_FromStringAndSize:=Import('PyBytes_FromStringAndSize');
    PyString_Size             :=Import('PyBytes_Size');
    PyString_DecodeEscape     :=Import('PyBytes_DecodeEscape');
    PyString_Repr             :=Import('PyBytes_Repr');
  end;
  PySys_GetObject           :=Import('PySys_GetObject');
  //PySys_Init                :=Import('PySys_Init');
  PySys_SetObject           :=Import('PySys_SetObject');
  PySys_SetPath             :=Import('PySys_SetPath');
  //PyTraceBack_Fetch         :=Import('PyTraceBack_Fetch');
  PyTraceBack_Here          :=Import('PyTraceBack_Here');
  PyTraceBack_Print         :=Import('PyTraceBack_Print');
  //@PyTraceBack_Store         :=Import('PyTraceBack_Store');
  PyTuple_GetItem           :=Import('PyTuple_GetItem');
  PyTuple_GetSlice          :=Import('PyTuple_GetSlice');
  PyTuple_New               :=Import('PyTuple_New');
  PyTuple_SetItem           :=Import('PyTuple_SetItem');
  PyTuple_Size              :=Import('PyTuple_Size');
  PyType_IsSubtype          :=Import('PyType_IsSubtype');
  PyType_GenericAlloc       :=Import('PyType_GenericAlloc');
  PyType_GenericNew         :=Import('PyType_GenericNew');
  PyType_Ready              :=Import('PyType_Ready');
  PyUnicode_FromWideChar    :=Import(AnsiString(Format('PyUnicode%s_FromWideChar',[UnicodeSuffix])));
  PyUnicode_AsWideChar      :=Import(AnsiString(Format('PyUnicode%s_AsWideChar',[UnicodeSuffix])));
  PyUnicode_Decode          :=Import(AnsiString(Format('PyUnicode%s_Decode',[UnicodeSuffix])));
  PyUnicode_AsEncodedString :=Import(AnsiString(Format('PyUnicode%s_AsEncodedString',[UnicodeSuffix])));
  PyUnicode_FromOrdinal     :=Import(AnsiString(Format('PyUnicode%s_FromOrdinal',[UnicodeSuffix])));
  PyUnicode_GetSize         :=Import(AnsiString(Format('PyUnicode%s_GetSize',[UnicodeSuffix])));
  PyWeakref_GetObject       :=Import('PyWeakref_GetObject');
  PyWeakref_NewProxy        :=Import('PyWeakref_NewProxy');
  PyWeakref_NewRef          :=Import('PyWeakref_NewRef');
  PyWrapper_New             :=Import('PyWrapper_New');
  PyBool_FromLong           :=Import('PyBool_FromLong');
  PyThreadState_SetAsyncExc :=Import('PyThreadState_SetAsyncExc');
  Py_AtExit                 :=Import('Py_AtExit');
  //Py_Cleanup                :=Import('Py_Cleanup');
  Py_FatalError             :=Import('Py_FatalError');
  if not IsPython3000 then begin
    Py_FindMethod           :=Import('Py_FindMethod');
    Py_FindMethodInChain    :=Import('Py_FindMethodInChain');
    DLL_Py_FlushLine        :=Import('Py_FlushLine');
    _PyString_Resize        :=Import('_PyString_Resize');
    Py_CompileStringFlags   :=Import('Py_CompileStringFlags');
  end else begin
    _PyString_Resize        :=Import('_PyBytes_Resize');
    Py_CompileStringExFlags :=Import('Py_CompileStringExFlags');
  end;
  _PyObject_New               :=Import('_PyObject_New');
  Py_Finalize                :=Import('Py_Finalize');
  if getProcAddress( FDLLHandle, 'PyCode_Addr2Line' ) <> nil then
    DLL_PyCode_Addr2Line     := Import('PyCode_Addr2Line');
  if getProcAddress( FDLLHandle, 'PyImport_ExecCodeModule' ) <> nil then
    DLL_PyImport_ExecCodeModule := Import('PyImport_ExecCodeModule');
  //@PyClass_IsSubclass         :=Import('PyClass_IsSubclass');
  PyErr_ExceptionMatches     :=Import('PyErr_ExceptionMatches');
  PyErr_GivenExceptionMatches:=Import('PyErr_GivenExceptionMatches');
  PyEval_EvalCode            :=Import('PyEval_EvalCode');
  Py_GetVersion              :=Import('Py_GetVersion');
  Py_GetCopyright            :=Import('Py_GetCopyright');
  Py_GetExecPrefix           :=Import('Py_GetExecPrefix');
  Py_GetPath                 :=Import('Py_GetPath');
  if IsPython3000 then
    Py_SetPythonHome3000     :=Import('Py_SetPythonHome')
  else
    Py_SetPythonHome         :=Import('Py_SetPythonHome');
  if IsPython3000 then
    Py_GetPythonHome3000     :=Import('Py_GetPythonHome')
  else
    Py_GetPythonHome         :=Import('Py_GetPythonHome');
  Py_GetPrefix               :=Import('Py_GetPrefix');
  Py_GetProgramName          :=Import('Py_GetProgramName');
  PyParser_SimpleParseStringFlags := Import('PyParser_SimpleParseStringFlags');
  PyNode_Free                :=Import('PyNode_Free');
  PyErr_NewException         :=Import('PyErr_NewException');
  try
    PyMem_Malloc := Import ('PyMem_Malloc');
  except
  end;
  if not IsPython3000 then
    Py_SetProgramName        := Import('Py_SetProgramName')
  else
    Py_SetProgramName3000    := Import('Py_SetProgramName');
  Py_IsInitialized         := Import('Py_IsInitialized');
  Py_GetProgramFullPath    := Import('Py_GetProgramFullPath');
  if getProcAddress( FDLLHandle, 'Py_GetBuildInfo' ) <> nil then
    DLL_Py_GetBuildInfo    := Import('Py_GetBuildInfo');
  Py_NewInterpreter        := Import('Py_NewInterpreter');
  Py_EndInterpreter        := Import('Py_EndInterpreter');
  PyEval_AcquireLock       := Import('PyEval_AcquireLock');
  PyEval_ReleaseLock       := Import('PyEval_ReleaseLock');
  PyEval_AcquireThread     := Import('PyEval_AcquireThread');
  PyEval_ReleaseThread     := Import('PyEval_ReleaseThread');
  PyInterpreterState_New   := Import('PyInterpreterState_New');
  PyInterpreterState_Clear := Import('PyInterpreterState_Clear');
  PyInterpreterState_Delete:= Import('PyInterpreterState_Delete');
  PyThreadState_New        := Import('PyThreadState_New');
  PyThreadState_Clear      := Import('PyThreadState_Clear');
  PyThreadState_Delete     := Import('PyThreadState_Delete');
  PyThreadState_Get        := Import('PyThreadState_Get');
  PyThreadState_Swap       := Import('PyThreadState_Swap');
  PyErr_SetInterrupt       := Import('PyErr_SetInterrupt');
  PyGILState_Ensure        := Import('PyGILState_Ensure');
  PyGILState_Release       := Import('PyGILState_Release');
end;

function TPythonInterface.Py_CompileString( s1,s2:PAnsiChar;i:integer):PPyObject; cdecl;
begin
  if IsPython3000 then
    Result := Py_CompileStringExFlags(s1, s2, i, nil, -1)
  else
    Result := Py_CompileStringFlags(s1, s2, i, nil);
end;

function TPythonInterface.PyParser_SimpleParseString( str : PAnsiChar; start : integer) : PNode; cdecl;
begin
  Result := PyParser_SimpleParseStringFlags(str, start, 0);
end;

procedure TPythonInterface.Py_INCREF(op: PPyObject);
begin
  Inc(op^.ob_refcnt);
end;

procedure TPythonInterface.Py_DECREF(op: PPyObject);
begin
  with op^ do begin
    Dec(ob_refcnt);
    if ob_refcnt = 0 then begin
      ob_type^.tp_dealloc(op);
    end;
  end;
end;

procedure TPythonInterface.Py_XINCREF(op: PPyObject);
begin
  if op <> nil then Py_INCREF(op);
end;

procedure TPythonInterface.Py_XDECREF(op: PPyObject);
begin
  if op <> nil then Py_DECREF(op);
end;

// This function is copied from compile.c because it was not
// exported in the Dll
function TPythonInterface.PyCode_Addr2Line( co: PPyCodeObject; addrq : Integer ) : Integer; cdecl;
var
  size : Integer;
  p    : PAnsiChar;
  line : Integer;
  addr : Integer;
  cpt  : Integer;
begin
  if Assigned(DLL_PyCode_Addr2Line) then
    begin
      Result := DLL_PyCode_Addr2Line( co, addrq );
      Exit;
    end;
  size := PyString_Size(co^.co_lnotab) div 2;
  p    := PyString_AsString(co^.co_lnotab);
  line := co^.co_firstlineno;
  addr := 0;
  cpt  := 0;
  while (size-1) >= 0 do
    begin
      Dec(size);
      Inc( addr, Ord(p[cpt]) );
      Inc(cpt);
      if addr > addrq then
        Break;
      Inc( line, Ord(p[cpt]) );
      Inc(cpt);
    end;
  Result := line;
end;

function TPythonInterface.Py_GetBuildInfo : PAnsiChar; cdecl;
begin
  if Assigned(DLL_Py_GetBuildInfo) then
    begin
      Result := DLL_Py_GetBuildInfo;
      Exit;
    end;
  Result := 'No build info';
end;

function TPythonInterface.PyImport_ExecCodeModule( const AName : AnsiString; codeobject : PPyObject) : PPyObject;
var
  m, d, v, modules : PPyObject;
begin
  if Assigned(DLL_PyImport_ExecCodeModule) then
  begin
    Result := DLL_PyImport_ExecCodeModule(PAnsiChar(AName), codeobject);
    Exit;
  end;
  CheckPython;
  m := PyImport_AddModule(PAnsiChar(AName));
  if not Assigned(m) then
    begin
      Result := nil;
      Exit;
    end;
  d := PyModule_GetDict(m);
  if PyDict_GetItemString(d, '__builtins__') = nil then
    begin
      if PyDict_SetItemString(d, '__builtins__', PyEval_GetBuiltins) <> 0 then
        begin
          Result := nil;
          Exit;
        end;
    end;
  // Remember the fielname as the __file__ attribute
  if PyDict_SetItemString(d, '__file__', PPyCodeObject(codeobject)^.co_filename) <> 0 then
    PyErr_Clear; // Not important enough to report
  v := PyEval_EvalCode(PPyCodeObject(codeobject), d, d); // XXX owner ?
  if not Assigned(v) then
    begin
      Result := nil;
      Exit;
    end;
  Py_XDECREF(v);
  modules := PyImport_GetModuleDict;
  if PyDict_GetItemString(modules, PAnsiChar(AName)) = nil then
    begin
      PyErr_SetString(PyExc_ImportError^,
        PAnsiChar(AnsiString(Format('Loaded module %.200s not found in sys.modules', [AName]))));
      Result := nil;
      Exit;
    end;
  Py_XINCREF(m);
  Result := m;
end;

function TPythonInterface.PyString_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyString_Type);
  if not Result then
    Result := PyObject_TypeCheck(obj, PyUnicode_Type);
end;

function TPythonInterface.PyString_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyString_Type));
end;

function TPythonInterface.PyFloat_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyFloat_Type);
end;

function TPythonInterface.PyFloat_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyFloat_Type));
end;

function TPythonInterface.PyInt_Check( obj : PPyObject ) : Boolean;
begin
  if IsPython3000 then
    Result := PyObject_TypeCheck(obj, PyLong_Type)
  else
    Result := PyObject_TypeCheck(obj, PyInt_Type);
end;

function TPythonInterface.PyInt_CheckExact(obj: PPyObject): Boolean;
begin
  if IsPython3000 then
    Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyLong_Type))
  else
    Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyInt_Type));
end;

function TPythonInterface.PyLong_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyLong_Type);
end;

function TPythonInterface.PyLong_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyLong_Type));
end;

function TPythonInterface.PyTuple_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyTuple_Type);
end;

function TPythonInterface.PyTuple_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyTuple_Type));
end;

function TPythonInterface.PyInstance_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (not IsPython3000) and (obj^.ob_type = PPyTypeObject(PyInstance_Type));
end;

function TPythonInterface.PyClass_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and not IsPython3000 and (obj^.ob_type = PPyTypeObject(PyClass_Type));
end;

function TPythonInterface.PyType_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyType_Type));
end;

function TPythonInterface.PyMethod_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyMethod_Type));
end;

function TPythonInterface.PyList_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyList_Type);
end;

function TPythonInterface.PyList_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyList_Type));
end;

function TPythonInterface.PyDict_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyDict_Type);
end;

function TPythonInterface.PyDict_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyDict_Type));
end;

function TPythonInterface.PyModule_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyModule_Type);
end;

function TPythonInterface.PyModule_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyModule_Type));
end;

function TPythonInterface.PySlice_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PySlice_Type));
end;

function TPythonInterface.PyFunction_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and
    ((obj^.ob_type = PPyTypeObject(PyCFunction_Type)) or
     (obj^.ob_type = PPyTypeObject(PyFunction_Type)));
end;

function TPythonInterface.PyIter_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and
            (IsPython3000 or (PyType_HasFeature(obj^.ob_type, Py_TPFLAGS_HAVE_ITER))
              and Assigned(obj^.ob_type^.tp_iternext));
end;

function TPythonInterface.PyUnicode_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyUnicode_Type);
end;

function TPythonInterface.PyUnicode_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyUnicode_Type));
end;

function TPythonInterface.PyType_IS_GC(t : PPyTypeObject ) : Boolean;
begin
  Result := PyType_HasFeature(t, Py_TPFLAGS_HAVE_GC);
end;

function TPythonInterface.PyObject_IS_GC( obj : PPyObject ) : Boolean;
begin
  Result := PyType_IS_GC(obj^.ob_type) and
            (not Assigned(obj^.ob_type^.tp_is_gc) or (obj^.ob_type^.tp_is_gc(obj) = 1));
end;

function TPythonInterface.PyWeakref_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (PyWeakref_CheckRef(obj) or PyWeakref_CheckProxy(obj));
end;

function TPythonInterface.PyWeakref_CheckRef( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(_PyWeakref_RefType));
end;

function TPythonInterface.PyWeakref_CheckProxy( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and
            ( (obj^.ob_type = PPyTypeObject(_PyWeakref_ProxyType)) or
              (obj^.ob_type = PPyTypeObject(_PyWeakref_CallableProxyType)) );
end;

function TPythonInterface.PyBool_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyBool_Type);
end;

function TPythonInterface.PyBaseString_Check( obj : PPyObject ) : Boolean;
begin
  if IsPython3000 then
    Result:= PyObject_TypeCheck(obj, PyUnicode_Type)
  else
    Result := PyObject_TypeCheck(obj, PyBaseString_Type);
end;

function TPythonInterface.PyEnum_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyEnum_Type));
end;

function TPythonInterface.PyObject_TypeCheck(obj : PPyObject; t : PPyTypeObject) : Boolean;
begin
  Result := Assigned(obj) and (obj^.ob_type = t);
  if not Result and Assigned(obj) and Assigned(t) then
    Result := PyType_IsSubtype(obj^.ob_type, t) = 1;
end;

function TPythonInterface.Py_InitModule( const AName : PAnsiChar; md : PPyMethodDef) : PPyObject;
begin
  CheckPython;
  Result := Py_InitModule4( AName, md, nil, nil, APIVersion );
end;

function TPythonInterface.Py_InitModule3000(const md: PyModuleDef): PPyObject;
Var
  modules  : PPyObject;
begin
  CheckPython;
  Result:= PyModule_Create2(@md, APIVersion);
  if not Assigned(Result) then
    GetPythonEngine.CheckError;
  // To emulate Py_InitModule4 we need to add the module to sys.modules
  modules := PyImport_GetModuleDict;
  if PyDict_SetItemString(modules, md.m_name, Result) <> 0 then
    GetPythonEngine.CheckError;
end;

procedure TPythonInterface.Py_FlushLine; cdecl;
begin
  if Assigned(DLL_Py_FlushLine) then
    DLL_Py_FlushLine;
end;

(*******************************************************)
(**                                                   **)
(**            class TPythonTraceback                 **)
(**                                                   **)
(*******************************************************)

function TPythonTraceback.GetItemCount : Integer;
begin
  Result := FItems.Count;
end;

function TPythonTraceback.GetItem( idx : Integer ) : TTracebackItem;
begin
  Result := TTracebackItem(FItems.Items[idx]);
end;

constructor TPythonTraceback.Create;
begin
  inherited;
  FLimit := 1000;
  FItems := TList.Create;
end;

destructor TPythonTraceback.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TPythonTraceback.Clear;
var
  i : Integer;
begin
  for i := 0 to ItemCount - 1 do
    Items[i].Free;
  FItems.Clear;
end;

{******
 * Warning !
 * This method must be called after the PyErr_Print function,
 * otherwise it can't extract the traceback informations.
 *
 * This method is automatically called by the Exec/Eval methods of
 * TPythonEngine. But if you use the Python core API, then don't
 * forget to refresh the traceback yourself. Or much better,
 * simply use the method CheckError wich will call PyErr_Print,
 * Traceback.Refresh and RaiseError for you.
}
procedure TPythonTraceback.Refresh;
var
//  tb, tb1  : PPyTraceBackObject;
  tb, tb1  : PPyObject;
  obj      : PPyObject;
  frame    : PPyObject;
  code     : PPyObject;
  depth    : Integer;
  limitv   : PPyObject;
  aLimit   : Integer;
  item     : TTracebackItem;
begin
  Clear;
  with GetPythonEngine do
    begin
      // get the limit of the traceback
      alimit := FLimit;
      limitv := PySys_GetObject('tracebacklimit');
      if Assigned(limitv) and PyInt_Check(limitv) then
        alimit := PyInt_AsLong(limitv);
      tb := PySys_GetObject('last_traceback');
      tb1 := tb;
      Py_XIncRef(tb1);
      depth := 0;
      // Evaluate the depth of the traceback
      while Assigned(tb1) and (tb1 <> Py_None) do
        begin
          Inc(depth);
          Py_XDecRef(tb1);
          tb1 := PyObject_GetAttrString(tb1, 'tb_next');
          CheckError(False);
        end;
      Py_XDecRef(tb1);
      // build the trace back
      Py_XIncRef(tb);
      while Assigned(tb) and (tb <> Py_None) do
        begin
          try
            if depth <= alimit then
              begin
                item     := TTracebackItem.Create;
                try
                  obj := PyObject_GetAttrString(tb, 'tb_lineno');
                  CheckError(False);
                  try
                    item.LineNo   := PyObjectAsVariant(obj);
                  finally
                    Py_XDecRef(obj);
                  end;
                  frame := PyObject_GetAttrString(tb, 'tb_frame');
                  CheckError(False);
                  try
                    if Assigned(frame) and (frame <> Py_None) then
                    begin
                      code := PyObject_GetAttrString(frame, 'f_code');
                      CheckError(False);
                      try
                        obj := PyObject_GetAttrString(code, 'co_filename');
                        CheckError(False);
                        try
                          item.Filename := PyObjectAsVariant( obj );
                        finally
                          Py_XDecRef(obj);
                        end;
                        obj := PyObject_GetAttrString(code, 'co_name');
                        CheckError(False);
                        try
                          item.Context  := PyObjectAsVariant( obj );
                        finally
                          Py_XDecRef(obj);
                        end;
                      finally
                        Py_XDecRef(code);
                      end;
                    end;
                  finally
                    Py_XDecRef(frame);
                  end;
                except
                  item.Free;
                  raise;
                end;
                FItems.Add( item );
              end;
            Dec( depth );
          finally
            Py_XDecRef(tb);
          end;
          tb := PyObject_GetAttrString(tb, 'tb_next');
          CheckError(False);
        end;
      Py_XDecRef(tb);
    end;
end;


(*******************************************************)
(**                                                   **)
(**            class TPythonEngine                    **)
(**                                                   **)
(*******************************************************)


constructor TPythonEngine.Create(AOwner: TComponent);
var
  i : Integer;
begin
  inherited;
  FLock                    := TCriticalSection.Create;
  FInitialized             := False;
  FInitScript              := TstringList.Create;
  FClients                 := TList.Create;
  FRedirectIO              := True;
  FExecModule              := '__main__';
  FAutoFinalize            := True;
  FInitThreads             := False;
  FTraceback               := TPythonTraceback.Create;
  FUseWindowsConsole       := False;
  FPyFlags                 := [];
  FDatetimeConversionMode  := DEFAULT_DATETIME_CONVERSION_MODE;
  if csDesigning in ComponentState then
    begin
      for i := 0 to AOwner.ComponentCount - 1 do
        if (AOwner.Components[i] is TPythonEngine) and
           (AOwner.Components[i] <> Self) then
          raise Exception.Create('You can''t drop more than one TPythonEngine component');
    end;
end;

destructor TPythonEngine.Destroy;
begin
  LocalVars := nil;
  GlobalVars := nil;
  Destroying;
  Finalize;
{$IFDEF FPC}
  inherited;
{$ENDIF}  // Free our objects
  FClients.Free;
  FInitScript.Free;
  FTraceback.Free;
  FLock.Free;
{$IFNDEF FPC}
  inherited;
{$ENDIF}
end;

procedure TPythonEngine.Finalize;
var
  i: integer;
  canDetachClients : Boolean;
begin
  // switch off redirection when the component is destroying,
  // because the form or datamodule is beeing closed, and
  // redirecting output may crash the application.
  if FIORedirected and not (csDestroying in ComponentState) and Initialized then
  begin
    RedirectIO := False;
    // restore the initial streams also.
    ExecString('import sys'+LF+
               'if hasattr(sys, "old_stdin"):  sys.stdin=sys.old_stdin'+LF+
               'if hasattr(sys, "old_stdout"): sys.stdout=sys.old_stdout'+LF+
               'if hasattr(sys, "old_stderr"): sys.stderr=sys.old_stderr' );
  end;
  // First finalize our clients
  if Initialized then
    for i := 0 to ClientCount - 1 do
      with Clients[i] do
        begin
          if Initialized then
            Finalize;
        end;
  // Then finalize Python, if we have to
  if Initialized and FAutoFinalize then begin
    try
      try
        FFinalizing := True;
        Py_Finalize;
      finally
        FFinalizing := False;
      end;
    except 
    end;
  end;
  // Detach our clients, when engine is beeing destroyed or one of its clients.
  canDetachClients := csDestroying in ComponentState;
  if not canDetachClients then
    for i := 0 to ClientCount - 1 do
      if csDestroying in Clients[i].ComponentState then
      begin
        canDetachClients := True;
        Break;
      end;
  if canDetachClients then
  begin
    for i := 0 to ClientCount - 1 do
      Clients[i].ClearEngine;
    FClients.Clear;
  end;
  // Free our reference
  gPythonEngine               := nil;
  FTimeStruct                 := nil;
  FPyDateTime_DateType        := nil;
  FPyDateTime_DateTimeType    := nil;
  FPyDateTime_DeltaType       := nil;
  FPyDateTime_TimeType        := nil;
  FPyDateTime_TZInfoType      := nil;
  FPyDateTime_TimeTZType      := nil;
  FPyDateTime_DateTimeTZType  := nil;
end;

procedure TPythonEngine.Lock;
begin
  FLock.Enter;
end;

procedure TPythonEngine.Unlock;
begin
  FLock.Leave;
end;

procedure TPythonEngine.AfterLoad;
begin
  inherited;
  Initialize;
end;

procedure TPythonEngine.BeforeLoad;
begin
  if UseWindowsConsole then
    InitWinConsole;
  inherited;
end;

procedure TPythonEngine.DoOpenDll(const aDllName : String);
var
  i : Integer;
begin
  if UseLastKnownVersion then
    for i:= Integer(COMPILED_FOR_PYTHON_VERSION_INDEX) to High(PYTHON_KNOWN_VERSIONS) do
    begin
      RegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
      FDLLHandle := SafeLoadLibrary(GetDllPath+PYTHON_KNOWN_VERSIONS[i].DllName);
      if IsHandleValid then
      begin
        DllName := PYTHON_KNOWN_VERSIONS[i].DllName;
        APIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
        Exit;
      end;
      if not PYTHON_KNOWN_VERSIONS[i].CanUseLatest then
        Break;
    end
  else
    RegVersion := GetPythonVersionFromDLLName(aDllName);
  inherited;
end;

procedure TPythonEngine.AssignPyFlags;

  procedure SetFlag( AFlag: PInt; AValue : Boolean );
  begin
    if AValue then
      AFlag^ := 1
    else
      AFlag^ := 0;
  end;

begin
  // define each Python flag. See file pyDebug.h
  SetFlag(Py_DebugFlag,       pfDebug in FPyFlags);
  SetFlag(Py_VerboseFlag,     pfVerbose in FPyFlags);
  SetFlag(Py_InteractiveFlag, pfInteractive in FPyFlags);
  SetFlag(Py_OptimizeFlag,    pfOptimize in FPyFlags);
  SetFlag(Py_NoSiteFlag,      pfNoSite in FPyFlags);
  SetFlag(Py_FrozenFlag,      pfFrozenFlag in FPyFlags);
  if not IsPython3000 then begin
    SetFlag(Py_UseClassExceptionsFlag, pfUseClassExceptionsFlag in FPyFlags);
    SetFlag(Py_UnicodeFlag,     pfUnicode in FPyFlags);
    SetFlag(Py_TabcheckFlag,    pfTabcheck in FPyFlags);
  end;
  SetFlag(Py_IgnoreEnvironmentFlag, pfIgnoreEnvironmentFlag in FPyFlags);
  if Assigned(Py_DivisionWarningFlag) then
    SetFlag(Py_DivisionWarningFlag, pfDivisionWarningFlag in FPyFlags);
end;

procedure TPythonEngine.Initialize;

  procedure InitSysPath;
  var
    _path : PPyObject;
  const Script =
    'import sys' + sLineBreak +
    'sys.executable = r"%s"' + sLineBreak +
    'path = sys.path' + sLineBreak +
    'for i in range(len(path)-1, -1, -1):' + sLineBreak +
    '    if path[i].find("site-packages") > 0:' + sLineBreak +
    '        path.pop(i)' + sLineBreak +
    'import site' + sLineBreak +
    'site.main()' + sLineBreak +
    'del sys, path, i, site';
  begin
     if VenvPythonExe <> '' then
       ExecString(AnsiString(Format(Script, [VenvPythonExe])));
    _path := PySys_GetObject('path');
    if Assigned(FOnSysPathInit) then
      FOnSysPathInit(Self, _path);
  end;

  function GetVal(AModule : PPyObject; AVarName : AnsiString) : PPyObject;
  begin
    Result := PyObject_GetAttrString(AModule, PAnsiChar(AVarName));
    if PyErr_Occurred <> nil then
      PyErr_Clear
    else
      Py_XDecRef(Result); // keep a borrowed reference.
  end;

  procedure GetTimeStructType;
  var
    timeModule : PPyObject;
  begin
    timeModule := PyImport_ImportModule('time');
    try
      if Assigned(timeModule) then
        FTimeStruct := GetVal(timeModule, 'struct_time')
      else
        PyErr_Clear;
    finally
      Py_XDecRef(timeModule);
    end;
  end;

  procedure GetDateTimeTypes;
  var
    dateTimeModule : PPyObject;
  begin
    dateTimeModule := PyImport_ImportModule('datetime');
    try
      if Assigned(dateTimeModule) then
      begin
        FPyDateTime_DateType        := GetVal(dateTimeModule, 'date');
        FPyDateTime_DateTimeType    := GetVal(dateTimeModule, 'datetime');
        FPyDateTime_DeltaType       := GetVal(dateTimeModule, 'timedelta');
        FPyDateTime_TimeType        := GetVal(dateTimeModule, 'time');
        FPyDateTime_TZInfoType      := GetVal(dateTimeModule, 'tzinfo');
        FPyDateTime_TimeTZType      := GetVal(dateTimeModule, 'timetz');
        FPyDateTime_DateTimeTZType  := GetVal(dateTimeModule, 'datetimetz');
      end
      else
        PyErr_Clear;
    finally
      Py_XDecRef(dateTimeModule);
    end;
  end;

var
  i : Integer;
begin
  if Assigned(gPythonEngine) then
    raise Exception.Create('There is already one instance of TPythonEngine running' );

  {$IFDEF FPC}
  //this allows you to just call Initialize to create a non-IDE instance with
  //Lazarus.
  if(AutoLoad and not IsHandleValid) then begin
    LoadDLL;//Calls initialize
    exit;
  end;
  {$ENDIF}

  gPythonEngine := Self;
  CheckRegistry;
  if IsPython3000 then begin
    if Assigned(Py_SetProgramName3000) then
    begin
      if FProgramNameW = '' then
        FProgramNameW := ParamStr(0);
      Py_SetProgramName3000(PWideChar(FProgramNameW));
    end
  end else begin
    if Assigned(Py_SetProgramName) then
    begin
      if FProgramName = '' then
        FProgramName := AnsiString(ParamStr(0));
      Py_SetProgramName(PAnsiChar(FProgramName));
    end
  end;
  AssignPyFlags;
  if FPythonHomeW <> '' then begin
    if IsPython3000 then
      Py_SetPythonHome3000(PWideChar(FPythonHomeW))
    else
      Py_SetPythonHome(PAnsiChar(FPythonHome));
  end;
  Py_Initialize;
  FInitialized := True;
  FIORedirected := False;
  InitSysPath;
  SetProgramArgs;
  GetTimeStructType;
  GetDateTimeTypes;
  if InitThreads and Assigned(PyEval_InitThreads) then
    PyEval_InitThreads;
  if RedirectIO and Assigned(FIO) then
    DoRedirectIO;
  for i := 0 to ClientCount - 1 do
    with Clients[i] do
      if not Initialized then
        Initialize;
  if InitScript.Count > 0 then
    ExecStrings( InitScript );
  if Assigned(FOnAfterInit) then
    FOnAfterInit(Self);
end;

procedure TPythonEngine.SetInitScript(Value: TStrings);
begin
  FInitScript.Assign(Value);
end;

function TPythonEngine.GetInterpreterState: PPyInterpreterState;
var
  res: PPyThreadState;
begin
  if Assigned(PyThreadState_Get) then begin
    res:= PyThreadState_Get();
    if (MajorVersion > 3) or ((MajorVersion = 3) and (MinorVersion >= 4)) then
      Result := res^.interp34
    else
      Result := res^.interp;
  end else
    Result := nil;
end;

function TPythonEngine.GetThreadState: PPyThreadState;
begin
  if Assigned(PyThreadState_Get) then
    Result := PyThreadState_Get
  else
    Result := nil;
end;

procedure TPythonEngine.SetInitThreads(Value: Boolean);
begin
  if Value <> FInitThreads then
  begin
    if Value and Assigned(PyEval_InitThreads) then
      PyEval_InitThreads;
    FInitThreads := Value;
  end;
end;

function TPythonEngine.GetClientCount : Integer;
begin
  Result := FClients.Count;
end;

function TPythonEngine.GetClients( idx : Integer ) : TEngineClient;
begin
  Result := TEngineClient( FClients.Items[idx] );
end;

procedure TPythonEngine.Notification( AComponent: TComponent;
                                      Operation: TOperation);
var
  i : Integer;
begin
  inherited;
  if Operation = opRemove then
    begin
      if AComponent = IO then
        IO := nil
      else
        begin
          for i := 0 to ClientCount - 1 do
            if Clients[i] = AComponent then
              begin
                RemoveClient( Clients[i] );
                Break;
              end;
        end;
    end;
end;

procedure TPythonEngine.CheckRegistry;
{$IFDEF MSWINDOWS}
var
  key : string;
  Path : string;
  NewPath : string;
{$IFDEF CPUX86}
  MajorVersion : integer;
  MinorVersion : integer;
{$ENDIF}
  VersionSuffix: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Assigned( FOnPathInitialization ) then
  try
    with TRegistry.Create(KEY_ALL_ACCESS and not KEY_NOTIFY) do
      try
        VersionSuffix := '';
{$IFDEF CPUX86}
        MajorVersion := StrToInt(RegVersion[1]);
        MinorVersion := StrToInt(RegVersion[3]);
        if (MajorVersion > 3) or ((MajorVersion = 3)  and (MinorVersion >= 5)) then
          VersionSuffix := '-32';
{$ENDIF}
        key := Format('\Software\Python\PythonCore\%s%s\PythonPath', [RegVersion, VersionSuffix]);

        RootKey := HKEY_LOCAL_MACHINE;
        if not KeyExists( key ) then
        begin
          // try a current user installation
          RootKey := HKEY_CURRENT_USER;
          if not KeyExists( key ) then  Exit;
        end;
        // Key found
        OpenKey( key, True );
        try
          Path := ReadString('');
          NewPath := Path;
          FOnPathInitialization( Self, NewPath );
          if NewPath <> Path then
          begin
            WriteString( '', NewPath );
          end;
        finally
          CloseKey;
        end;
      finally
        Free;
      end;
  except
    // under WinNT, with a user without admin rights, the access to the
    // LocalMachine keys would raise an exception.
  end;
{$ENDIF}
end;

procedure TPythonEngine.SetProgramArgs;
var
  buff : PAnsiChar;
  argv : PPAnsiChar;
  i, argc : Integer;
  L : array of AnsiString;
  wbuff : PWideChar;
  wargv : PPWideChar;
  WL : array of UnicodeString;
begin
  // we build a string list of the arguments, because ParamStr returns a volatile string
  // and we want to build an array of PAnsiChar, pointing to valid strings.
  argc := ParamCount;
  if not IsPython3000 then begin
    SetLength(L, argc+1);
    GetMem( buff, sizeof(PAnsiChar)*(argc+1) );
    try
      argv := PPAnsiChar(buff);
      // get the strings
      // build the PAnsiChar array
      for i := 0 to argc do begin
        L[i] := AnsiString(ParamStr(i));
        argv^[i] := PAnsiChar(L[i]);
      end;
      // set the argv list of the sys module with the application arguments
      PySys_SetArgv( argc+1, argv );
    finally
      FreeMem( buff );
    end;
  end else begin
    SetLength(WL, argc+1);
    GetMem( wbuff, sizeof(PWideChar)*(argc+1) );
    try
      wargv := PPWideChar(wbuff);
      // get the strings
      // build the PAnsiChar array
      for i := 0 to argc do begin
        WL[i] := ParamStr(i);
        wargv^[i] := PWideChar(WL[i]);
      end;
      // set the argv list of the sys module with the application arguments
      PySys_SetArgv3000( argc + 1, wargv );
    finally
      FreeMem( wbuff );
    end;
  end;
end;

procedure TPythonEngine.InitWinConsole;
begin
{$IFDEF MSWINDOWS}
  FreeConsole;
  AllocConsole;
  SetConsoleTitle( 'Python console' );
{$ENDIF}
end;

procedure TPythonEngine.SetUseWindowsConsole( const Value : Boolean );
begin
  FUseWindowsConsole := Value;
  if (csDesigning in ComponentState) then
    RedirectIO := False;
end;

// GlobalVars contains a dictionary object used by the Run_CommandAsObject method, if not nil.
// Warning ! SetGlobalVars increments the reference count of the dictionary object !
procedure TPythonEngine.SetGlobalVars(const Value: PPyObject);
begin
  Py_XDecRef(FGlobalVars);
  if Assigned(Value) then
    if PyDict_Check(Value) then
      FGlobalVars := Value
    else
      begin
        FGlobalVars := nil;
        raise Exception.Create('You must set a Python dictionary in the GlobalVars property');
      end
  else
    FGlobalVars := nil;
  Py_XIncRef(FGlobalVars);
end;

// LocalVars contains a dictionary object used by the Run_CommandAsObject method, if not nil.
// Warning ! SetLocalVars increments the reference count of the dictionary object !
procedure TPythonEngine.SetLocalVars(const Value: PPyObject);
begin
  Py_XDecRef(FLocalVars);
  if Assigned(Value) then
    if PyDict_Check(Value) then
      FLocalVars := Value
    else
      begin
        FLocalVars := nil;
        raise Exception.Create('You must set a Python dictionary in the LocalVars property');
      end
  else
    FLocalVars := nil;
  Py_XIncRef(FLocalVars);
end;

procedure TPythonEngine.SetPyFlags(const Value: TPythonFlags);
begin
  if FPyFlags <> Value then
  begin
    if Initialized then
      raise Exception.Create('You can''t modify Python flags after it has been initialized');
    FPyFlags := Value;
  end; // of if
end;

procedure TPythonEngine.SetPythonHome(const PythonHome: string);
begin
  FPythonHomeW := PythonHome;
  FPythonHome := EncodeString(PythonHome);
end;

procedure TPythonEngine.SetProgramName(const ProgramName: string);
begin
  FProgramNameW := ProgramName;
  FProgramName := EncodeString(ProgramName);
end;

function TPythonEngine.IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
begin
  result := ob^.ob_type = obt;
end;

function TPythonEngine.GetAttrString(obj: PPyObject; AName: PAnsiChar):PAnsiChar;
var
   attr: PPyObject;
begin
  CheckPython;
  result := nil;
  attr := PyObject_GetAttrString(obj, AName);
  if attr <> nil then begin
    result := PyString_AsString(attr);
    Py_XDECREF(attr);
  end;
  PyErr_Clear;
end;

function   TPythonEngine.EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
var presult :PPyObject;
begin
  CheckPython;
  Result := -1;
  if pyfunc = nil then exit;
  try
    presult := PyEval_CallObjectWithKeywords(pyfunc,pyargs, nil);
    CheckError(False);
    if presult = nil then
      begin
        PyErr_Print;
        RaiseError;
      end
    else
      begin
        try
          if presult = Py_None then
            Result := 0
          else
            Result := PyObjectAsVariant( presult );
        finally
          Py_DECREF(presult);
        end;
      end;
    Py_FlushLine;
  except
    Py_FlushLine;
    if PyErr_Occurred <> nil then
      CheckError(False)
    else
      raise;
  end;
end;

function   TPythonEngine.EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
var pargs: PPyObject;
begin
  CheckPython;
  pargs := ArrayToPyTuple(args);
  try
    Result := EvalPyFunction(pyfunc,pargs);
  finally
    Py_DECREF(pargs);
  end;
end;

function   TPythonEngine.EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
var pargs: PPyObject;
begin
  CheckPython;
  pargs := PyTuple_New(0);
  try
    Result := EvalPyFunction(pyfunc, pargs);
  finally
    Py_DECREF(pargs);
  end;
end;

function   TPythonEngine.EvalStringAsStr(const command : AnsiString) : String;
begin
  Result := Run_CommandAsString( command, eval_input );
end;

function   TPythonEngine.EvalString(const command : AnsiString) : PPyObject;
begin
  Result := Run_CommandAsObject( command, eval_input );
end;

procedure TPythonEngine.ExecString(const command : AnsiString);
begin
  Py_XDecRef( Run_CommandAsObject( command, file_input ) );
end;

function   TPythonEngine.Run_CommandAsString(const command : AnsiString; mode : Integer) : String;
var
  v : PPyObject;
begin
  Result := '';
  v := Run_CommandAsObject( command, mode );
  Result := PyObjectAsString( v );
  Py_XDECREF(v);
end;

function   TPythonEngine.Run_CommandAsObject(const command : AnsiString; mode : Integer) : PPyObject;
begin
  Result := Run_CommandAsObjectWithDict(command, mode, nil, nil);
end;

function TPythonEngine.Run_CommandAsObjectWithDict(const command : AnsiString; mode : Integer; locals, globals : PPyObject) : PPyObject;
var
  m : PPyObject;
  _locals, _globals : PPyObject;
begin
  CheckPython;
  Result := nil;
  Traceback.Clear;
  CheckError(False);

  m := GetMainModule;
  if m = nil then
    raise EPythonError.Create('Run_CommandAsObject: can''t create __main__');

  if Assigned(locals) then
    _locals  := locals
  else if Assigned(FLocalVars) then
    _locals  := LocalVars
  else
    _locals  := PyModule_GetDict(m);

  if Assigned(globals) then
    _globals := globals
  else if Assigned(FGlobalVars) then
    _globals := GlobalVars
  else
    _globals := PyModule_GetDict(m);

  try
    Result := PyRun_String(PAnsiChar(CleanString(command)), mode, _globals, _locals);
    if Result = nil then
      CheckError(False);
    Py_FlushLine;
  except
    Py_FlushLine;
    if PyErr_Occurred <> nil then
      CheckError(False)
    else
      raise;
  end;
end;

procedure TPythonEngine.ExecStrings( strings : TStrings );
begin
  Py_XDecRef( Run_CommandAsObject( EncodeString(strings.Text) , file_input ) );
end;

function TPythonEngine.EvalStrings( strings : TStrings ) : PPyObject;
begin
  Result := Run_CommandAsObject( EncodeString(strings.Text) , eval_input );
end;

procedure TPythonEngine.ExecString(const command : AnsiString; locals, globals : PPyObject );
begin
  Py_XDecRef( Run_CommandAsObjectWithDict( command, file_input, locals, globals ) );
end;

procedure TPythonEngine.ExecStrings( strings : TStrings; locals, globals : PPyObject );
begin
  Py_XDecRef( Run_CommandAsObjectWithDict( EncodeString(strings.Text), file_input, locals, globals ) );
end;

function TPythonEngine.EvalString( const command : AnsiString; locals, globals : PPyObject ) : PPyObject;
begin
  Result := Run_CommandAsObjectWithDict( command, eval_input, locals, globals );
end;

function TPythonEngine.EvalStrings( strings : TStrings; locals, globals : PPyObject ) : PPyObject;
begin
  Result := Run_CommandAsObjectWithDict( EncodeString(strings.Text), eval_input, locals, globals );
end;

function TPythonEngine.EvalStringsAsStr( strings : TStrings ) : String;
begin
  Result := Run_CommandAsString( EncodeString(strings.Text), eval_input );
end;

function TPythonEngine.CheckEvalSyntax( const str : AnsiString ) : Boolean;
begin
  result := CheckSyntax( str, eval_input );
end;

function TPythonEngine.CheckExecSyntax( const str : AnsiString ) : Boolean;
begin
  result := CheckSyntax( str, file_input );
end;

function TPythonEngine.CheckSyntax( const str : AnsiString; mode : Integer ) : Boolean;
var
  n : PNode;
begin
  n := PyParser_SimpleParseString( PAnsiChar(str), mode );
  result := Assigned(n);
  if Assigned( n ) then
    PyNode_Free(n);
end;

procedure TPythonEngine.RaiseError;

  function Define( E : EPythonError; const sType, sValue : String ) : EPythonError;
  begin
    E.EName  := sType;
    E.EValue := sValue;
    if sValue <> '' then
      E.Message := Format('%s: %s',[sType,sValue])
    else
      E.Message := sType;
    Result := E;
  end;

  function DefineSyntaxError( E : EPySyntaxError; const sType, sValue : String; err_type, err_value : PPyObject ) : EPySyntaxError;
  var
    s_value       : String;
    s_line        : String;
    s_filename    : String;
    i_line_number : Integer;
    i_offset      : Integer;
    tmp           : PPyObject;
  begin
    Result := E;
    Result.EName  := sType;
    Result.EValue := sValue;
    s_value := '';
    s_line        := '';
    s_filename    := '';
    i_line_number := 0;
    i_offset      := 0;
    // Sometimes there's a tuple instead of instance...
    if PyTuple_Check( err_value )  and (PyTuple_Size( err_value) >= 2) then
    begin
      s_value := PyString_AsDelphiString(PyTuple_GetItem( err_value, 0));
      err_value := PyTuple_GetItem( err_value, 1);
      if PyTuple_Check( err_value )  and (PyTuple_Size( err_value) >= 4) then
      begin
        i_line_number := PyInt_AsLong(PyTuple_GetItem( err_value, 1));
        i_offset      := PyInt_AsLong(PyTuple_GetItem( err_value, 2));
        s_line := Trim(PyString_AsDelphiString(PyTuple_GetItem( err_value, 3)));
      end;
    end else
    // Is it an instance of the SyntaxError class ?
    if (PyInstance_Check( err_value ) and (PyClass_IsSubclass( PPyObject(PPyInstanceObject(err_value)^.in_class), err_type ) <> 0)) or
       ((PyType_IsSubtype(PPyTypeObject(err_type), PPyTypeObject(PyExc_SyntaxError^)) = 1) and IsType(err_value, PPyTypeObject(err_type))) then
      begin
        // Get the filename
        tmp := PyObject_GetAttrString(err_value, 'filename');
        if tmp <> nil then begin
          if PyString_Check(tmp) then
            s_filename := PyString_AsDelphiString(tmp)
          else if tmp = Py_None then
            s_filename := '???';
          Py_XDECREF(tmp);
        end;
        // Get the text containing the error, cut of carriage return
        tmp := PyObject_GetAttrString(err_value, 'text');
        if Assigned(tmp) and PyString_Check(tmp) then
          s_line := Trim(PyString_AsDelphiString(tmp));
        Py_XDECREF(tmp);
        // Get the offset where the error should appear
        tmp := PyObject_GetAttrString(err_value, 'offset' );
        if Assigned(tmp) and PyInt_Check(tmp) then
          i_offset := PyInt_AsLong(tmp);
        Py_XDECREF(tmp);
        // Get the line number of the error
        tmp := PyObject_GetAttrString(err_value, 'lineno' );
        if Assigned(tmp) and PyInt_Check(tmp) then
          i_line_number := PyInt_AsLong(tmp);
        Py_XDECREF(tmp);
        // Get the message of the error
        tmp := PyObject_GetAttrString(err_value, 'msg' );
        if Assigned(tmp) and PyString_Check(tmp) then
          s_value := PyString_AsDelphiString(tmp);
        Py_XDECREF(tmp);
      end;
    // If all is ok
    if s_value <> '' then
      begin
        with Result do
          begin
            Message     := Format('%s: %s (line %d, offset %d): ''%s''', [sType,s_value,i_line_number, i_offset,s_line]);
            EName       := sType;
            EValue      := s_value;
            EFileName   := s_filename;
            ELineNumber := i_line_number;
            EOffset     := i_offset;
            ELineStr    := s_line;
          end;
      end
    else
      Result.Message := sType;
  end;

  function GetTypeAsString( obj : PPyObject ) : String;
  begin
    if PyClass_Check( obj ) then
      with PPyClassObject(obj)^ do
        Result := PyString_AsDelphiString(cl_name)
    else if PyType_CheckExact( obj ) then
        Result := String(PPyTypeObject(obj).tp_name)
    else
      Result := PyObjectAsString(obj);
  end;

var
  err_type, err_value : PPyObject;
  s_type        : String;
  s_value       : String;
begin
  s_value       := '';

  if PyErr_Occurred <> nil then
    PyErr_Print;
  err_type := PySys_GetObject('last_type');
  err_value := PySys_GetObject('last_value');
  if Assigned(err_type) then
    begin
      s_type := GetTypeAsString(err_type);
      s_value := PyObjectAsString(err_value);

      if (PyErr_GivenExceptionMatches(err_type, PyExc_SystemExit^) <> 0) then
        raise Define( EPySystemExit.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_StopIteration^) <> 0) then
        raise Define( EPyStopIteration.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_KeyboardInterrupt^) <> 0) then
        raise Define( EPyKeyboardInterrupt.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ImportError^) <> 0) then
        raise Define( EPyImportError.Create(''), s_type, s_value )
  {$IFDEF MSWINDOWS}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_WindowsError^) <> 0) then
        raise Define( EPyWindowsError.Create(''), s_type, s_value )
  {$ENDIF}
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_IOError^) <> 0) then
        raise Define( EPyIOError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_OSError^) <> 0) then
        raise Define( EPyOSError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_EnvironmentError^) <> 0) then
        raise Define( EPyEnvironmentError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_EOFError^) <> 0) then
        raise Define( EPyEOFError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_NotImplementedError^) <> 0) then
        raise Define( EPyNotImplementedError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_RuntimeError^) <> 0) then
        raise Define( EPyRuntimeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnboundLocalError^) <> 0) then
        raise Define( EPyUnboundLocalError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_NameError^) <> 0) then
        raise Define( EPyNameError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_AttributeError^) <> 0) then
        raise Define( EPyAttributeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_TabError^) <> 0) then
        raise DefineSyntaxError( EPyTabError.Create(''), s_type, s_value, err_type, err_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_IndentationError^) <> 0) then
        raise DefineSyntaxError( EPyIndentationError.Create(''), s_type, s_value, err_type, err_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_SyntaxError^) <> 0) then
        raise DefineSyntaxError( EPySyntaxError.Create(''), s_type, s_value, err_type, err_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_TypeError^) <> 0) then
        raise Define( EPyTypeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_AssertionError^) <> 0) then
        raise Define( EPyAssertionError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_IndexError^) <> 0) then
        raise Define( EPyIndexError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_KeyError^) <> 0) then
        raise Define( EPyKeyError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_LookupError^) <> 0) then
        raise Define( EPyLookupError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_OverflowError^) <> 0) then
        raise Define( EPyOverflowError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ZeroDivisionError^) <> 0) then
        raise Define( EPyZeroDivisionError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_FloatingPointError^) <> 0) then
        raise Define( EPyFloatingPointError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ArithmeticError^) <> 0) then
        raise Define( EPyArithmeticError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeEncodeError^) <> 0) then
        raise Define( UnicodeEncodeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeDecodeError^) <> 0) then
        raise Define( UnicodeDecodeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeTranslateError^) <> 0) then
        raise Define( UnicodeTranslateError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UnicodeError^) <> 0) then
        raise Define( EPyUnicodeError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ValueError^) <> 0) then
        raise Define( EPyValueError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_ReferenceError^) <> 0) then
        raise Define( EPyReferenceError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_SystemError^) <> 0) then
        raise Define( EPySystemError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_MemoryError^) <> 0) then
        raise Define( EPyMemoryError.Create(''), s_type, s_value )
      else if (not IsPython3000) and (PyErr_GivenExceptionMatches(err_type, PyExc_StandardError^) <> 0) then
        raise Define( EPyStandardError.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_UserWarning^) <> 0) then
        raise Define( EPyUserWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_DeprecationWarning^) <> 0) then
        raise Define( EPyDeprecationWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_SyntaxWarning^) <> 0) then
        raise Define( EPySyntaxWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_RuntimeWarning^) <> 0) then
        raise Define( EPyRuntimeWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_FutureWarning^) <> 0) then
        raise Define( FutureWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_PendingDeprecationWarning^) <> 0) then
        raise Define( PendingDeprecationWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_Warning^) <> 0) then
        raise Define( EPyWarning.Create(''), s_type, s_value )
      else if (PyErr_GivenExceptionMatches(err_type, PyExc_Exception^) <> 0) then
        raise Define( EPyException.Create(''), s_type, s_value )
      else  // Else if no known exception was detected,
            // then build an ExecError exception
        raise Define( EPyExecError.Create(''), s_type, s_value );
    end
  else
    raise EPythonError.Create('RaiseError: could''nt fetch last exception');
end;

function TPythonEngine.PyObjectAsString( obj : PPyObject ) : String;
var
  s : PPyObject;
//  i : Integer;
//  tmp : PAnsiChar;
  w : UnicodeString;
begin
  CheckPython;
  Result := '';
  if not Assigned( obj ) then
    Exit;

  if PyUnicode_Check(obj) then
  begin
    w := PyUnicode_AsWideString(obj);
    Result := w;
    Exit;
  end;
  s := PyObject_Str( obj );
  if Assigned(s) and PyString_Check(s) then
    begin
      Result := PyString_AsDelphiString(s);
//      tmp := PyString_AsString(s);
//      SetLength( Result, PyString_Size(s)+1 );
//      Result := '';
//      for i := 0 to PyString_Size(s) - 1 do
//        Insert( tmp[i], Result, i+1 );
    end;
  Py_XDECREF(s);
end;

procedure TPythonEngine.DoRedirectIO;
const
  code = 'import sys'+LF+
         'class DebugOutput:'+LF+
         '  pyio = __import__("pyio")'+LF+
         '  softspace=0'+LF+
         '  encoding=None'+LF+
         '  def write(self,message):'+LF+
         '     self.pyio.write(message)'+LF+
         '  def readline(self, size=None):'+LF+
         '     return self.pyio.read(size)'+LF+
         '  def flush(self):' + LF +
         '     pass' + LF +
         'sys.old_stdin=sys.stdin'+LF+
         'sys.old_stdout=sys.stdout'+LF+
         'sys.old_stderr=sys.stderr'+LF+
         'sys.stdin=sys.stderr=sys.stdout=DebugOutput()'+LF+#0;
begin
  if csDesigning in ComponentState then
    Exit;
  CheckPython;
  if not Assigned(FIOPythonModule) then
  begin
    // create a new module called pyio
    FIOPythonModule := TPythonModule.Create( Self );
    with FIOPythonModule as TPythonModule do
      begin
        Engine := Self;
        ModuleName := 'pyio';
        AddMethod( 'write', pyio_write, 'write(String) -> None' );
        AddMethod( 'read',  pyio_read,  'read() -> String' );
        AddMethod( 'SetDelayWrites',  pyio_SetDelayWrites,  'SetDelayWrites(Boolean) -> None' );
        AddMethod( 'SetMaxLines',  pyio_SetMaxLines,  'SetMaxLines(Integer) -> None' );
        AddMethod( 'GetTypesStats',  pyio_GetTypesStats,  'GetTypesStats( [type name] ) -> a list of tuple (TypeName, InstanceCount, CreateHits, DeleteHits)' );
      end;
  end;
  with FIOPythonModule as TPythonModule do
    if not Initialized then
      Initialize;
  // execute the code
  ExecString(code);
  FIORedirected := True;
end;

procedure  TPythonEngine.AddClient( client : TEngineClient );
begin
  FClients.Add( client );
end;

procedure  TPythonEngine.RemoveClient( client : TEngineClient );
begin
  // We finalize the PythonEngine, as soon as a client should
  // be freed, because the destroy order of the components
  // is not predictable and may cause some memory crashes !
  if (csDesigning in ComponentState) then
    FClients.Remove( client )
  else if (Initialized) then begin
    FClients.Remove( client );
    if (ClientCount = 0) then
      Finalize;
  end;
end;

function   TPythonEngine.FindClient( const aName : string ) : TEngineClient;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ClientCount - 1 do
    with TPythonType( Clients[i] ) do
      if Name = aName then
        begin
          Result := Clients[i];
          Break;
        end;
end;

function TPythonEngine.EncodeString(const str: string): AnsiString;
begin
  if IsPython3000 then
    Result := UTF8Encode(str)
  else
    Result := AnsiString(str);
end;

function TPythonEngine.EncodeWindowsFilePath(const str: string): AnsiString;
{PEP 529}
begin
  if (MajorVersion > 3) or ((MajorVersion = 3) and (MinorVersion >=6) )then
    Result := UTF8Encode(str)
  else
    Result := AnsiString(str);
end;

function   TPythonEngine.TypeByName( const aTypeName : AnsiString ) : PPyTypeObject;
var
  i : Integer;
begin
  for i := 0 to ClientCount - 1 do
    if Clients[i] is TPythonType then
      with TPythonType( Clients[i] ) do
        if TypeName = aTypeName then
          begin
            Result := TheTypePtr;
            Exit;
          end;
  raise Exception.CreateFmt('Could not find type: %s', [aTypeName]);
end;

function   TPythonEngine.ModuleByName( const aModuleName : AnsiString ) : PPyObject;
var
  i : Integer;
begin
  for i := 0 to ClientCount - 1 do
    if Clients[i] is TPythonModule then
      with TPythonModule( Clients[i] ) do
        if ModuleName = aModuleName then
          begin
            Result := Module;
            Exit;
          end;
  raise Exception.CreateFmt('Could not find module: %s', [aModuleName]);
end;

function   TPythonEngine.MethodsByName( const aMethodsContainer: string ) : PPyMethodDef;
var
  i : Integer;
begin
  for i := 0 to ClientCount - 1 do
    if Clients[i] is TMethodsContainer then
      with TMethodsContainer( Clients[i] ) do
        if Name = aMethodsContainer then
          begin
            Result := MethodsData;
            Exit;
          end;
  raise Exception.CreateFmt('Could not find component: %s', [aMethodsContainer]);
end;

function TPythonEngine.VariantAsPyObject( const V : Variant ) : PPyObject;
Var
  DeRefV : Variant;

  function ArrayVarDim1 : PPyObject;
  var
    i, cpt : Integer;
  begin
    Result := PyList_New( VarArrayHighBound( DeRefV, 1 ) - VarArrayLowBound( DeRefV, 1 ) + 1 );
    cpt := 0;
    for i := VarArrayLowBound( DeRefV, 1 ) to VarArrayHighBound( DeRefV, 1 ) do
      begin
        PyList_SetItem( Result, cpt, VariantAsPyObject(DeRefV[i]) );
        Inc(cpt);
      end;
  end;

  function ArrayVarDim2 : PPyObject;
  var
    i, j, cpt, cpt2 : Integer;
    L : PPyObject;
  begin
    Result := PyList_New( VarArrayHighBound( DeRefV, 1 ) - VarArrayLowBound( DeRefV, 1 ) + 1 );
    cpt := 0;
    for i := VarArrayLowBound( DeRefV, 1 ) to VarArrayHighBound( DeRefV, 1 ) do
      begin
        L := PyList_New( VarArrayHighBound( DeRefV, 2 ) - VarArrayLowBound( DeRefV, 2 ) + 1 );
        PyList_SetItem( Result, cpt, L );
        cpt2 := 0;
        for j := VarArrayLowBound( DeRefV, 2 ) to VarArrayHighBound( DeRefV, 2 ) do
          begin
            PyList_SetItem( L, cpt2, VariantAsPyObject(DeRefV[i, j]) );
            Inc(cpt2);
          end;
        Inc(cpt);
      end;
  end;

  function ArrayVarDim3 : PPyObject;
  var
    i, j, k, cpt, cpt2, cpt3 : Integer;
    L, L2 : PPyObject;
  begin
    Result := PyList_New( VarArrayHighBound( DeRefV, 1 ) - VarArrayLowBound( DeRefV, 1 ) + 1 );
    cpt := 0;
    for i := VarArrayLowBound( DeRefV, 1 ) to VarArrayHighBound( DeRefV, 1 ) do
      begin
        L := PyList_New( VarArrayHighBound( DeRefV, 2 ) - VarArrayLowBound( DeRefV, 2 ) + 1 );
        PyList_SetItem( Result, cpt, L );
        cpt2 := 0;
        for j := VarArrayLowBound( DeRefV, 2 ) to VarArrayHighBound( DeRefV, 2 ) do
          begin
            L2 := PyList_New( VarArrayHighBound( DeRefV, 3 ) - VarArrayLowBound( DeRefV, 3 ) + 1 );
            PyList_SetItem( L, cpt2, L2 );
            cpt3 := 0;
            for k := VarArrayLowBound( DeRefV, 3 ) to VarArrayHighBound( DeRefV, 3 ) do
              begin
                PyList_SetItem( L2, cpt3, VariantAsPyObject(DeRefV[i, j, k]) );
                Inc(cpt3);
              end;
            Inc(cpt2);
          end;
        Inc(cpt);
      end;
  end;

const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}'; // copied from ActiveX.pas
var
  s : AnsiString;
  y, m, d, h, mi, sec, ms, jd, wd : WORD;
  dt : TDateTime;
  dl : Integer;
  myInt : NativeInt;
  wStr : UnicodeString;
  Disp : IDispatch;
  DispID : Integer;
  args : PPyObject;
begin
  Disp := nil;
  //Dereference Variant
  DerefV := V;
  while VarType(DeRefV) = varByRef or varVariant do
    DeRefV := Variant(PVarData(TVarData(DeRefV).VPointer)^);

  case VarType(DeRefV) and (VarTypeMask or VarArray) of
    varBoolean: begin
      if DeRefV = true then
        Result := PPyObject(Py_True)
      else
        Result := PPyObject(Py_False);
      Py_XIncRef(Result);
    end;
    varSmallint,
    varByte,
    varShortInt,
    varWord,
    varLongWord,
    varInteger:  Result := PyInt_FromLong( DeRefV );
    varInt64:    Result := PyLong_FromLongLong( DeRefV );
    varSingle,
    varDouble,
    varCurrency: Result := PyFloat_FromDouble( DeRefV );
    varDate:
      begin
        dt := DeRefV;
        DecodeDate( dt, y, m, d );
        DecodeTime( dt, h, mi, sec, ms );
        if (DatetimeConversionMode = dcmToTuple) then
        begin
          wd := (DayOfWeek( dt ) + 7 - 2) mod 7; // In Python, Monday is the first day (=0)
          jd := Round(EncodeDate(y,m,d)-EncodeDate(y,1,1))+1; // This shoud be the Julian day, the day in a year (0-366)
          dl := -1; // This is daylight save... ?Ξ?Ξ? I don't know what it is...
          Result := ArrayToPyTuple( [y, m, d, h, mi, sec, wd, jd, dl] );
        end
        else if (DatetimeConversionMode = dcmToDatetime) then
        begin
          if not Assigned(FPyDateTime_DateTimeType) then
            raise EPythonError.Create('dcmToDatetime DatetimeConversionMode cannot be used with this version of python. Missing module datetime');
          args := ArrayToPyTuple([y, m, d, h, mi, sec, ms*1000]);
          try
            Result := PyEval_CallObjectWithKeywords(FPyDateTime_DateTimeType, args, nil);
            CheckError(False);
          finally
            Py_DecRef(args);
          end;
        end
        else
          raise EPythonError.Create('Invalid DatetimeConversionMode');
      end;
    varOleStr:
      begin
        if (TVarData(DeRefV).VOleStr = nil) or (TVarData(DeRefV).VOleStr^ = #0) then
          wStr := ''
        else
          wStr := DeRefV;
      {$IFDEF PREFER_UNICODE}
        Result := PyUnicode_FromWideChar( PWideChar(wStr), Length(wStr) );
      {$ELSE}
        s := wStr;
        Result := PyString_FromStringAndSize(PAnsiChar(s), Length(s));
      {$ENDIF}
      end;
    varString:
      begin
        s := AnsiString(DeRefV);
        Result := PyString_FromStringAndSize(PAnsiChar(s), Length(s));
      end;
   {$IFDEF UNICODE}
    varUString:
      begin
       wStr := DeRefV;
      {$IFDEF PREFER_UNICODE}
        Result := PyUnicode_FromWideChar( PWideChar(wStr), Length(wStr) );
      {$ELSE}
        s := wStr;
        Result := PyString_FromStringAndSize(PAnsiChar(s), Length(s));
      {$ENDIF}
      end;
    {$ENDIF}
  else
    if VarType(DeRefV) and varArray <> 0 then
      begin
        case VarArrayDimCount(DeRefV) of
        1: Result := ArrayVarDim1;
        2: Result := ArrayVarDim2;
        3: Result := ArrayVarDim3;
        else
          raise Exception.Create('Can''t convert a variant array of more than 3 dimensions to a Python sequence');
        end;
      end
    else if VarIsNull(DeRefV) or VarIsEmpty(DeRefV) then
      begin
        Result := ReturnNone;
      end
    else
      try
        Disp := DeRefV;
        wStr := '__asPPyObject__';
        // detect if the variant supports this special property
        if Assigned(Disp) and (Disp.GetIDsOfNames(GUID_NULL, @wStr, 1, 0, @DispID) = S_OK) then
        begin
          myInt := DeRefV.__asPPyObject__;  //Returns the address to PPyObject as integer. (See impl. in PythonAtom.pas)
          Result := PPyObject(myInt);
          Py_XIncRef(Result);
        end
        else //If variant don't implement __asPPyObject__, then we have to return nothing.
          Result := ReturnNone;
      except
        // if something went wrong, just return none!
        Result := ReturnNone;
      end; // of try
  end; // of case
end;

function TPythonEngine.PyObjectAsVariant( obj : PPyObject ) : Variant;

  function ExtractDate( var date : Variant ) : Boolean;

    function GetStructMember( obj : PPyObject; const AMember : AnsiString ) : Integer;
    var
      member : PPyObject;
    begin
      member := PyObject_GetAttrString( obj, PAnsiChar(AMember) );
      CheckError(False);
      if PyInt_Check(member) then
        Result := PyInt_AsLong(member)
      else
        raise EPythonError.CreateFmt('Unexpected type found in member %s of a time_struct object', [AMember]);
      Py_XDecRef(member);
    end;

  var
    i, wd, jd, dl : Integer;
    dt : TDateTime;
    y, m, d, h, mi, sec, msec : WORD;
    delta : PPyDateTime_Delta;
  begin
    Result := False;
    if PyTimeStruct_Check( obj ) then
    begin
        y   := GetStructMember( obj, 'tm_year' );
        m   := GetStructMember( obj, 'tm_mon' );
        d   := GetStructMember( obj, 'tm_mday' );
        h   := GetStructMember( obj, 'tm_hour' );
        mi  := GetStructMember( obj, 'tm_min' );
        sec := GetStructMember( obj, 'tm_sec' );
        //wd  := GetStructMember( obj, 'tm_wday' );
        //jd  := GetStructMember( obj, 'tm_yday' );
        //dl  := GetStructMember( obj, 'tm_isdst' );
        dt := EncodeDate( y, m, d ) + EncodeTime( h, mi, sec, 0 );
        Date := dt;
        Result := True;
    end
    else if PyDateTime_Check( obj ) then
    begin
        y   := GetStructMember(obj, 'year');
        m   := GetStructMember(obj, 'month');
        d   := GetStructMember(obj, 'day');
        h := GetStructMember(obj, 'hour');
        mi := GetStructMember(obj, 'minute');
        sec := GetStructMember(obj, 'second');
        msec := GetStructMember(obj, 'microsecond') div 1000;
        dt := EncodeDate( y, m, d ) + EncodeTime( h, mi, sec, msec );
        Date := dt;
        Result := True;
    end
    else if PyDate_Check( obj ) then
    begin
        y   := GetStructMember(obj, 'year');
        m   := GetStructMember(obj, 'month');
        d   := GetStructMember(obj, 'day');
        dt  := EncodeDate( y, m, d );
        Date := dt;
        Result := True;
    end
    else if PyTime_Check( obj ) then
    begin
        h := GetStructMember(obj, 'hour');
        mi := GetStructMember(obj, 'minute');
        sec := GetStructMember(obj, 'second');
        msec := GetStructMember(obj, 'microsecond') div 1000;
        dt  := EncodeTime( h, mi, sec, msec );
        Date := dt;
        Result := True;
    end
    else if PyDelta_Check( obj ) then
    begin
      delta := PPyDateTime_Delta(obj);
      dt := delta^.days + (delta^.seconds / (24*60*60)) + ((delta^.microseconds div 1000) / (24*60*60*1000));
      Date := dt;
      Result := True;
    end
    else if PyTuple_Check( obj ) and (PyTuple_Size(obj) = 9) then
      begin
        for i := 0 to 8 do
          if not PyInt_Check(PyTuple_GetItem(obj, i)) then
            Exit;
        y   := PyInt_AsLong( PyTuple_GetItem(obj, 0) );
        m   := PyInt_AsLong( PyTuple_GetItem(obj, 1) );
        d   := PyInt_AsLong( PyTuple_GetItem(obj, 2) );
        h   := PyInt_AsLong( PyTuple_GetItem(obj, 3) );
        mi  := PyInt_AsLong( PyTuple_GetItem(obj, 4) );
        sec := PyInt_AsLong( PyTuple_GetItem(obj, 5) );
        wd  := PyInt_AsLong( PyTuple_GetItem(obj, 6) );
        jd  := PyInt_AsLong( PyTuple_GetItem(obj, 7) );
        dl  := PyInt_AsLong( PyTuple_GetItem(obj, 8) );
        if not (m   in [1..12]) or
           not (d   in [1..31]) or
           not (h   in [0..23]) or
           not (mi  in [0..59]) or
           not (sec in [0..59]) or
           not (wd  in [0..6]) or
           not ((jd>=0) and (jd<=366)) or
           not ((dl>=-1) and (dl<=1)) then
          Exit;
        try
          dt := EncodeDate( y, m, d );
          dt := dt + EncodeTime( h, mi, sec, 0 );
          Date := dt;
          Result := True;
        except
        end;
      end;
  end;

  function GetSequenceItem( sequence : PPyObject; idx : Integer ) : Variant;
  var
    val : PPyObject;
  begin
    val := PySequence_GetItem( sequence, idx );
    try
      Result := PyObjectAsVariant( val );
    finally
      Py_XDecRef( val );
    end;
  end;

var
  i, seq_length : Integer;
begin
  if PyFloat_Check(obj) then
    Result := PyFloat_AsDouble(obj)
  else if PyBool_Check(obj) then // we must check Bool before Int, as Boolean type inherits from Int.
    Result := PyObject_IsTrue(obj) = 1
  else if PyLong_Check(obj) then
    Result := PyLong_AsLongLong(obj)
  // changed the order of Long and int check (KV)
  else if PyInt_Check(obj) then
    Result := PyInt_AsLong(obj)
  else if PyUnicode_Check(obj) then
    Result := PyUnicode_AsWideString(obj)
  else if PyString_Check(obj) then
    Result := PyObjectAsString(obj)
  else if ExtractDate( Result ) then
    begin
      // Nothing to do
    end
  else if PySequence_Check( obj ) = 1 then
    begin
      seq_length := PySequence_Length( obj );
      // if we have at least one object in the sequence,
      if seq_length > 0 then
        // we try to get the first one, simply to test if the sequence API
        // is really implemented.
        Py_XDecRef( PySequence_GetItem( obj, 0 ) );
      // check if the Python object did really implement the sequence API
      if PyErr_Occurred = nil then
        begin
          // Convert a Python sequence into an array of Variant
          Result := VarArrayCreate( [0, seq_length-1], varVariant );
          for i := 0 to PySequence_Length( obj )-1 do
            Result[i] := GetSequenceItem( obj, i );
        end
      else // the object didn't implement the sequence API, so we return Null
        begin
          PyErr_Clear;
          Result := Null;
        end;
    end
  else
    Result := Null;
end;

function TPythonEngine.VarRecAsPyObject( v : TVarRec ) : PPyObject;
begin
  case v.VType of
    vtInteger:       Result := PyInt_FromLong( v.VInteger );
    vtBoolean:       Result := PyInt_FromLong( Integer(v.VBoolean) );
    vtChar:          Result := PyString_FromString( PAnsiChar(AnsiString(v.VChar)) );
    vtExtended:      Result := PyFloat_FromDouble( v.VExtended^ );
    vtString:
    begin
      if Assigned(v.VString) then
        Result := PyString_FromString(PAnsiChar(AnsiString(v.VString^)))
      else
        Result := PyString_FromString( '' );
    end;
    vtPChar:         Result := PyString_FromString( v.VPChar );
    vtAnsiString:
    begin
      if Assigned(v.VAnsiString) then
        Result := PyString_FromString( v.VAnsiString )
      else
        Result := PyString_FromString('');
    end;
    vtCurrency:      Result := PyFloat_FromDouble( v.VCurrency^ );
    vtVariant:       Result := VariantAsPyObject( v.VVariant^ );
    vtPointer:       Result := v.VPointer;
    vtInt64:         Result := PyLong_FromLongLong( v.VInt64^ );
    vtWideChar:      Result := PyUnicode_FromWideString( v.VWideChar );
    vtPWideChar:
      begin
        if Assigned(v.VPWideChar) then
          Result := PyUnicode_FromWideString( UnicodeString(v.VPWideChar) )
        else
          Result := PyUnicode_FromWideString( '' );
      end;
    vtWideString:
      begin
        if Assigned(v.VWideString) then
          Result := PyUnicode_FromWideString( WideString(v.VWideString) )
        else
          Result := PyUnicode_FromWideString( '' );
      end;
    {$IFDEF UNICODE}
    vtUnicodeString:
      begin
        if Assigned(v.VUnicodeString) then
          Result := PyUnicode_FromWideString( UnicodeString(v.VUnicodeString) )
        else
          Result := PyUnicode_FromWideString( '' );
      end;
    {$ENDIF}
  else
    Raise Exception.Create('Argument type not allowed');
  end;
end;

// This function prevents Python from deleting the objects contained
// when the container will be freed, because we increment each
// object's refcount.
function TPythonEngine.MakePyTuple( const objects : array of PPyObject ) : PPyObject;
var
  i : Integer;
begin
  Result := PyTuple_New( High(objects)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new tuple object');
  for i := Low(objects) to High(objects) do
    begin
      Py_XINCREF( objects[i] );
      PyTuple_SetItem( Result, i, objects[i] );
    end;
end;

// This function prevents Python from deleting the objects contained
// when the container will be freed, because we increment each
// object's refcount.
function TPythonEngine.MakePyList( const objects : array of PPyObject ) : PPyObject;
var
  i : Integer;
begin
  Result := PyList_New( High(objects)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new list object');
  for i := Low(objects) to High(objects) do
    begin
      Py_XIncRef( objects[i] );
      PyList_SetItem( Result, i, objects[i] );
    end;
end;

function TPythonEngine.ArrayToPyTuple( items : array of const) : PPyObject;
var
  i : Integer;
begin
  Result := PyTuple_New( High(items)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new tuple object');
  for i := Low(items) to High(items) do
    PyTuple_SetItem( Result, i, VarRecAsPyObject( items[i] ) );
end;

function TPythonEngine.ArrayToPyList( items : array of const) : PPyObject;
var
  i : Integer;
begin
  Result := PyList_New( High(items)+1 );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new list object');
  for i := Low(items) to High(items) do
    PyList_SetItem( Result, i, VarRecAsPyObject( items[i] ) );
end;

// You must give each entry as a couple key(string)/value
function TPythonEngine.ArrayToPyDict( items : array of const) : PPyObject;

  function VarRecAsString( v : TVarRec ) : AnsiString;
  begin
    case v.VType of
      vtChar:          Result := v.VChar;
      vtString:
        begin
          if Assigned(v.VString) then
            Result := v.VString^
          else
            Result := '';
        end;
      vtPChar:
        begin
          Result := v.VPChar;
        end;
      vtWideChar:
        begin
          Result := AnsiString(v.VWideChar);
        end;
      vtAnsiString:
        begin
          if Assigned(v.VAnsiString) then
            Result := Ansistring(v.VAnsiString)
          else
            Result := '';
        end;
      vtVariant:
        begin
          if Assigned(v.VVariant) then
            Result := AnsiString(v.VVariant^)
          else
            Result := '';
        end;
      vtWideString :
      begin
        if Assigned(v.VWideString) then
          Result := AnsiString(WideString(v.VWideString))
        else
          Result := '';
      end;
      {$IFDEF UNICODE}
      vtUnicodeString:
      begin
        if Assigned(v.VUnicodeString) then
          Result := AnsiString(UnicodeString(v.VUnicodeString))
        else
          Result := '';
      end;
      {$ENDIF}
    else
      Raise Exception.Create('Argument type not allowed');
    end;
  end;

var
  i : Integer;
  s : AnsiString;
  obj : PPyObject;
begin
  if ((High(items)+1) mod 2) <> 0 then
    raise Exception.Create('You must provide an even number of arguments');
  Result := PyDict_New;
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new dict object');
  i := Low(items);
  try
    while i <= High(items) do
      begin
        s := VarRecAsString( items[i] );
        obj := VarRecAsPyObject( items[i+1] );
        if s = '' then
          PyDict_SetItemString( Result, '', obj )
        else
          PyDict_SetItemString( Result, PAnsiChar(s), obj );
        Py_XDecRef(obj);
        Inc( i, 2 );
      end;
  except
    Py_XDECREF( Result );
  end;
end;

function TPythonEngine.StringsToPyList( strings : TStrings ) : PPyObject;
var
  i : Integer;
begin
  Result := PyList_New( strings.Count );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new list object');
  for i := 0 to strings.Count - 1 do
    PyList_SetItem( Result, i,
      PyString_FromDelphiString( strings.Strings[i]) );
end;

function TPythonEngine.StringsToPyTuple( strings : TStrings ) : PPyObject;
var
  i : Integer;
begin
  Result := PyTuple_New( strings.Count );
  if not Assigned(Result) then
    raise EPythonError.Create('Could not create a new tuple object');
  for i := 0 to strings.Count - 1 do
    PyTuple_SetItem( Result, i,
      PyString_FromDelphiString( strings.Strings[i]) );
end;

procedure TPythonEngine.PyListToStrings( list : PPyObject; strings : TStrings );
var
  i : Integer;
begin
  if not PyList_Check(list) then
    raise EPythonError.Create('the python object is not a list');
  strings.Clear;
  for i := 0 to PyList_Size( list ) - 1 do
    strings.Add( PyObjectAsString( PyList_GetItem( list, i ) ) );
end;

procedure TPythonEngine.PyTupleToStrings( tuple: PPyObject; strings : TStrings );
var
  i : Integer;
begin
  if not PyTuple_Check(tuple) then
    raise EPythonError.Create('the python object is not a tuple');
  strings.Clear;
  for i := 0 to PyTuple_Size( tuple ) - 1 do
    strings.Add( PyObjectAsString( PyTuple_GetItem( tuple, i ) ) );
end;

function TPythonEngine.PyUnicode_AsWideString( obj : PPyObject ) : UnicodeString;
var
  _size : Integer;
{$IFDEF LINUX}
  _ucs4Str : UCS4String;
{$ENDIF}
begin
  if PyUnicode_Check(obj) then
  begin
    _size := PyUnicode_GetSize(obj);
    if _size > 0 then
    begin
{$IFDEF LINUX}
      // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
      SetLength(_ucs4Str, _size+1);
      if PyUnicode_AsWideChar(obj, @_ucs4Str[0], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
      Result := UCS4StringToWideString(_ucs4Str);
      // remove trailing zeros (needed by Kylix1)
      while (Length(Result) > 0) and (Result[Length(Result)] = #0) do
        Delete(Result, Length(Result), 1);
{$ELSE}
      SetLength(Result, _size);
      if PyUnicode_AsWideChar(obj, @Result[1], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
{$ENDIF}
    end
    else
      Result := '';
  end
  else
    raise EPythonError.Create('PyUnicode_AsWideString expects a Unicode Python object');
end;

function TPythonEngine.PyUnicode_FromWideString( const AString : UnicodeString) : PPyObject;
{$IFDEF LINUX}
var
  _ucs4Str : UCS4String;
{$ENDIF}
begin
{$IFDEF LINUX}
  // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
  _ucs4Str := WideStringToUCS4String(AString);
  Result := PyUnicode_FromWideChar( {PWideChar}(@_ucs4Str[0]), Length(_ucs4Str)-1 {trim trailing zero});
{$ELSE}
  Result := PyUnicode_FromWideChar( PWideChar(AString), Length(AString) );
{$ENDIF}
end;

function TPythonEngine.ReturnNone : PPyObject;
begin
  Result := Py_None;
  Py_INCREF( Result );
end;

function TPythonEngine.FindModule( const ModuleName : AnsiString ) : PPyObject;
var
  modules, m : PPyObject;
begin
  modules := PyImport_GetModuleDict;
  m := PyDict_GetItemString(modules, PAnsiChar(ModuleName) );
  if (m <> nil) and (PyModule_Check(m)) then
    Result := m
  else
    Result := nil;
end;

function TPythonEngine.FindFunction(ModuleName,FuncName: AnsiString): PPyObject;
var
  module,func: PPyObject;
begin
  module := FindModule(ModuleName);
  if module = nil then result := nil
  else begin
    func := PyObject_GetAttrString(module, PAnsiChar(FuncName));
    if Assigned(func) then begin
       if PyFunction_Check(func) then
         Result := func
       else
       begin
         Py_XDecRef(func);
         Result := nil;
       end;
    end else begin
      Result := nil;
      PyErr_Clear;
    end;
  end;
end;

function TPythonEngine.SetToList( data : Pointer; size : Integer ) : PPyObject;

  function GetBit( idx : Integer ) : Boolean;
  var
    tmp : PAnsiChar;
  begin
    if idx >= size*8 then
      begin
        Result := False;
        Exit;
      end;
    tmp := PAnsiChar(data);
    tmp := tmp + (idx div 8);
    Result := (Ord(tmp^) and (1 shl (idx mod 8))) <> 0;
  end;

var
  i, cpt : Integer;
begin
  cpt := 0;
  for i := 0 to size*8-1 do
    if GetBit(i) then
      Inc(cpt);
  Result := PyList_New( cpt );
  cpt := 0;
  for i := 0 to size*8-1 do
    if GetBit(i) then
      begin
        PyList_SetItem( Result, cpt, PyInt_FromLong(i) );
        Inc(cpt);
      end;
end;

procedure TPythonEngine.ListToSet( List : PPyObject; data : Pointer; size : Integer );

  procedure SetBit( idx : Integer );
  var
    tmp : PAnsiChar;
  begin
    if idx >= size*8 then
      Exit;
    tmp := PAnsiChar(data);
    tmp := tmp + (idx div 8);
    tmp^ := AnsiChar(Chr((Ord(tmp^) or (1 shl (idx mod 8)))));
  end;

var
  i : Integer;
begin
  FillChar( PAnsiChar(data)^, size, #0 );
  for i := 0 to PyList_Size(list)-1 do
    SetBit( PyObjectAsVariant( PyList_GetItem(list, i) ) );
end;

procedure TPythonEngine.CheckError(ACatchStopEx : Boolean = False);
begin
  if PyErr_Occurred <> nil then
  begin
    if ACatchStopEx and (PyErr_GivenExceptionMatches(PyErr_Occurred, PyExc_StopIteration^) <> 0) then
    begin
      PyErr_Clear;
      raise EPyStopIteration.Create('Stop iteration');
    end
    else
    begin
      PyErr_Print;
      Traceback.Refresh;
      RaiseError;
    end;
  end;
end;

function TPythonEngine.GetMainModule : PPyObject;
begin
  Result := PyImport_AddModule(PAnsiChar(ExecModule));
end;

function TPythonEngine.PyTimeStruct_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FTimeStruct) and (Pointer(obj^.ob_type) = FTimeStruct);
end;

function TPythonEngine.PyDate_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_DateType));
end;

function TPythonEngine.PyDate_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_DateType);
end;

function TPythonEngine.PyDateTime_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_DateTimeType));
end;

function TPythonEngine.PyDateTime_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_DateTimeType);
end;

function TPythonEngine.PyTime_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_TimeType));
end;

function TPythonEngine.PyTime_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_TimeType);
end;

function TPythonEngine.PyDelta_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_DeltaType));
end;

function TPythonEngine.PyDelta_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_DeltaType);
end;

function TPythonEngine.PyTZInfo_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PPyTypeObject(FPyDateTime_TZInfoType));
end;

function TPythonEngine.PyTZInfo_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned(FPyDateTime_DateType) and (Pointer(obj^.ob_type) = FPyDateTime_TZInfoType);
end;

function TPythonEngine.PyString_AsDelphiString(ob: PPyObject): string;
begin
  if PyUnicode_Check(ob) then
    Result := string(PyUnicode_AsWideString(ob))
  else
    Result := string(PyString_AsString(ob));
end;

function TPythonEngine.PyString_AsAnsiString( ob: PPyObject): AnsiString;
begin
  if PyUnicode_Check(ob) then
    Result := AnsiString(PyUnicode_AsWideString(ob))
  else
    Result := AnsiString(PyString_AsString(ob));
end;

function TPythonEngine.PyString_AsWideString( ob: PPyObject): UnicodeString;
begin
  if PyUnicode_Check(ob) then
    Result := PyUnicode_AsWideString(ob)
  else
    Result := UnicodeString(PyString_AsString(ob));
end;

function TPythonEngine.PyString_FromDelphiString(str: string): PPyObject;
begin
  if IsPython3000 then
  begin
    Result := PyUnicode_FromWideString(str);
  end
  else
    Result := DLL_PyString_FromString(PAnsiChar(AnsiString(str)));
end;

function TPythonEngine.PyString_FromString( str: PAnsiChar): PPyObject;
var
  _text : UnicodeString;
begin
  if IsPython3000 then
  begin
    _text := UnicodeString(str);
    Result := PyUnicode_FromWideString(_text);
  end
  else
    Result := DLL_PyString_FromString(str);
end;



(*******************************************************)
(**                                                   **)
(**     class TEngineClient                           **)
(**                                                   **)
(*******************************************************)

procedure  TEngineClient.SetEngine( val : TPythonEngine );
begin
  if val <> FEngine then
    begin
      if Assigned(FEngine) {and not(csDesigning in ComponentState)} then
        FEngine.RemoveClient( Self );
      FEngine := val;
      if Assigned(FEngine) {and not(csDesigning in ComponentState)} then
        FEngine.AddClient( Self );
    end;
end;

procedure TEngineClient.ModuleReady(Sender : TObject);
begin
end;

constructor TEngineClient.Create( AOwner : TComponent );
var
  i : Integer;
begin
  inherited;
  if (csDesigning in ComponentState) and Assigned(AOwner) then
    with AOwner do
      for i := 0 to ComponentCount - 1 do
        if Components[i] is TPythonEngine then
          begin
            Self.Engine := TPythonEngine(Components[i]);
            Break;
          end;
end;

destructor TEngineClient.Destroy;
begin
  Engine := nil; // This detaches the client from the Engine.
  if Assigned( FOnDestroy ) then
    FOnDestroy( Self );
  inherited;
end;

procedure TEngineClient.Loaded;
begin
  inherited;
  if Assigned( FOnCreate ) then
    FOnCreate( Self );
end;

procedure TEngineClient.Notification( AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FEngine then
      FEngine := nil;
end;

procedure  TEngineClient.Initialize;
begin
  if FInitialized then
    Exit;
  if Assigned( FOnInitialization ) then
     FOnInitialization( Self );
  FInitialized := True;
end;

procedure TEngineClient.Finalize;
begin
  if not FInitialized then
    Exit;
  if Assigned( FOnFinalization ) then
     FOnFinalization( Self );
  FInitialized := False;
end;

procedure  TEngineClient.ClearEngine;
begin
  FEngine := nil;
end;

procedure  TEngineClient.CheckEngine;
begin
  if not Assigned(FEngine) then
    raise Exception.CreateFmt('No Engine defined for component "%s"', [Name]);
end;


(*******************************************************)
(**                                                   **)
(**     class TMethodsContainer                       **)
(**                                                   **)
(*******************************************************)

////////////////////////////////////////
// class TEventDef

constructor TEventDef.Create(ACollection: TCollection);
begin
  inherited;
  FDocString := TStringList.Create;
  Name := Format('PythonEvent%d',[Collection.Count - 1]);
end;

destructor TEventDef.Destroy;
begin
  FDocString.Free;
  inherited;
end;

function TEventDef.GetDisplayName: string;
begin
  Result := string(FName);
end;

function TEventDef.GetDocString : AnsiString;
begin
  Owner.Container.CheckEngine;
  FTmpDocString :=
    Owner.Container.Engine.EncodeString(CleanString(FDocString.Text, False));
  Result := fTmpDocString;
end;

function TEventDef.PythonEvent(pself,	args: PPyObject): PPyObject;
begin
  Owner.Container.CheckEngine;
  with Owner.Container.Engine do
  begin
    if Assigned(fOnExecute) then
      fOnExecute(Self, pself, args, Result);
  end;
end;

function  TEventDef.Owner : TEventDefs;
begin
  Result := Collection as TEventDefs;
end;

procedure TEventDef.SetDisplayName(const Value: string);
begin
  FName := AnsiString(Value);
  inherited;
end;

procedure TEventDef.Assign(Source: TPersistent);
begin
  if Source is TEventDef then
    begin
      Name := TEventDef(Source).Name;
      DocString := TEventDef(Source).DocString;
    end
  else
    inherited Assign(Source);
end;

procedure TEventDef.SetDocString(const Value: TStringList);
begin
	FDocString.Assign(Value);
end;

////////////////////////////////////////
// class TEventDefs

constructor TEventDefs.Create(AMethodsContainer : TMethodsContainer );
begin
  inherited Create(TEventDef);
  FMethodsContainer := AMethodsContainer;
end;

function TEventDefs.GetItems( idx : Integer ) : TEventDef;
begin
  Result := TEventDef(inherited GetItem(idx));
end;

procedure TEventDefs.SetItems( idx : Integer; Value : TEventDef );
begin
  inherited SetItem( idx, Value );
end;

function  TEventDefs.GetOwner: TPersistent;
begin
  Result := FMethodsContainer;
end;

function TEventDefs.Add : TEventDef;
begin
  Result := TEventDef(inherited Add);
end;

procedure TEventDefs.RegisterEvents;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      FMethodsContainer.AddDelphiMethod(PAnsiChar(FName), PythonEvent, PAnsiChar(GetDocString));
end;

////////////////////////////////////////
// class TMethodsContainer

procedure TMethodsContainer.AllocMethods;
begin
  Assert(FMethods = nil);
  FAllocatedMethodCount := PYT_METHOD_BUFFER_INCREASE;
  FMethodCount := 0;
  FMethods := PPyMethodDef(AllocMem(SizeOf(PyMethodDef)*(FAllocatedMethodCount+1)));
end;

procedure TMethodsContainer.FreeMethods;
begin
  if Assigned(FMethods) then
  begin
    FreeMem(FMethods);
    FMethods := nil;
  end;
  FAllocatedMethodCount := 0;
  FMethodCount := 0;
end;

procedure TMethodsContainer.ReallocMethods;
var
  MethodPtr : PPyMethodDef;
begin
  Inc( FAllocatedMethodCount, PYT_METHOD_BUFFER_INCREASE );
  ReAllocMem( FMethods, SizeOf(PyMethodDef)*(FAllocatedMethodCount+1));
  MethodPtr :=@(PMethodArray(FMethods)^[MethodCount+1]);
  FillChar( MethodPtr^,SizeOf(PyMethodDef)*PYT_METHOD_BUFFER_INCREASE,0);
end;

function TMethodsContainer.GetMethods( idx : Integer ) : PPyMethodDef;
begin
  if (idx < 0) or (idx > MethodCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( PMethodArray(FMethods)^[idx] );
end;

function TMethodsContainer.StoreEventDefs: Boolean;
begin
  Result := (FEventDefs <> nil) and (FEventDefs.Count > 0);
end;

constructor TMethodsContainer.Create( AOwner : TComponent );
begin
  inherited;
  AllocMethods;
  fEventDefs := TEventDefs.Create(Self);
end;

destructor  TMethodsContainer.Destroy;
begin
  fEventDefs.Free;
  fEventDefs := nil;
  FreeMethods;
  inherited;
end;

procedure TMethodsContainer.Initialize;
begin
  inherited;
  Events.RegisterEvents;
end;

procedure TMethodsContainer.Finalize;
begin
  if not (csDestroying in ComponentState) then
    ClearMethods;
  inherited;
end;

function TMethodsContainer.AddMethod( AMethodName  : PAnsiChar;
                                      AMethod  : PyCFunction;
                                      ADocString : PAnsiChar ) : PPyMethodDef;
begin
  if FMethodCount = FAllocatedMethodCount then
    ReallocMethods;
  Result := Methods[ MethodCount ];
  Result^.ml_name  := AMethodName;
  Result^.ml_meth  := AMethod;
  Result^.ml_flags := METH_VARARGS;
  Result^.ml_doc   := ADocString;
  Inc( FMethodCount );
end;

function  TMethodsContainer.AddMethodWithKeywords( AMethodName  : PAnsiChar;
                                                   AMethod  : PyCFunctionWithKW;
                                                   ADocString : PAnsiChar ) : PPyMethodDef;
begin
  Result := AddMethod( AMethodName,
                       PyCFunction(AMethod),
                       ADocString );
  Result^.ml_flags := Result^.ml_flags or METH_KEYWORDS;
end;

function  TMethodsContainer.AddDelphiMethod( AMethodName  : PAnsiChar;
                                             ADelphiMethod: TDelphiMethod;
                                             ADocString : PAnsiChar ) : PPyMethodDef;
begin
  Result := AddMethod( AMethodName,
                       GetOfObjectCallBack( TCallBack(ADelphiMethod), 2, ctCDECL),
                       ADocString );
end;

function  TMethodsContainer.AddDelphiMethodWithKeywords(  AMethodName  : PAnsiChar;
                                                          ADelphiMethod: TDelphiMethodWithKW;
                                                          ADocString : PAnsiChar ) : PPyMethodDef;
begin
  Result := AddMethod( AMethodName,
                       GetOfObjectCallBack( TCallBack(ADelphiMethod), 3, ctCDECL),
                       ADocString );
  Result^.ml_flags := Result^.ml_flags or METH_KEYWORDS;
end;

procedure TMethodsContainer.ClearMethods;
begin
  FMethodCount := 0;
  FillChar(FMethods^, Sizeof(FMethods^)*FAllocatedMethodCount, 0);
end;

////////////////////////////////////////
// class TMembersContainer

function  TMembersContainer.GetMembersStartOffset : Integer;
begin
  Result := 0;
end;

procedure TMembersContainer.AddMember(MemberName: PAnsiChar;  MemberType : TPyMemberType;
  MemberOffset : NativeInt; MemberFlags: TPyMemberFlag; MemberDoc: PAnsiChar);
begin
  if FMemberCount = FAllocatedMemberCount then
    ReallocMembers;
  with Members[ MemberCount ]^ do
    begin
      name      := MemberName;
      case MemberType of
      mtShort:          _type := T_Short;
      mtInt:            _type := T_Int;
      mtLong:           _type := T_Long;
      mtFloat:          _type := T_Float;
      mtDouble:         _type := T_Double;
      mtString:         _type := T_String;
      mtObject:         _type := T_Object;
      mtChar:           _type := T_Char;
      mtByte:           _type := T_Byte;
      mtUByte:          _type := T_UByte;
      mtUShort:         _type := T_UShort;
      mtUInt:           _type := T_UInt;
      mtULong:          _type := T_ULong;
      mtStringInplace:  _type := T_STRING_INPLACE;
      mtObjectEx:       _type := T_OBJECT_EX;
      else
        raise Exception.Create('Unknown member type');
      end;
      offset    := MemberOffset + GetMembersStartOffset;
      case MemberFlags of
      mfDefault:                flags := 0;
      mfReadOnly:               flags := READONLY;
      mfReadRestricted:         flags := READ_RESTRICTED;
      mfWriteRestricted:        flags := WRITE_RESTRICTED;
      mfRestricted:             flags := RESTRICTED;
      else
        raise Exception.Create('Unknown member flag');
      end;
      doc       := MemberDoc;
    end;
  Inc( FMemberCount );
end;

procedure TMembersContainer.AllocMembers;
begin
  FAllocatedMemberCount := PYT_MEMBER_BUFFER_INCREASE;
  Assert(not Assigned(FMembers));
  FMembers := PPyMemberDef(AllocMem(SizeOf(PyMemberDef)*(FAllocatedMemberCount+1)));
end;

procedure TMembersContainer.ClearMembers;
begin
  FMemberCount := 0;
  FillChar(FMembers^, Sizeof(FMembers^)*FAllocatedMemberCount, 0);
end;

constructor TMembersContainer.Create(AOwner: TComponent);
begin
  inherited;
  AllocMembers;
end;

destructor TMembersContainer.Destroy;
begin
  FreeMembers;
  inherited;
end;

procedure TMembersContainer.Finalize;
begin
  if not (csDestroying in ComponentState) then
    ClearMembers;
  inherited;
end;

procedure TMembersContainer.FreeMembers;
begin
  if Assigned(FMembers) then
  begin
    FreeMem(FMembers);
    FMembers := nil;
  end;
  FMemberCount := 0;
  FAllocatedMemberCount := 0;
end;

function TMembersContainer.GetMembers(idx: Integer): PPyMemberDef;
begin
  if (idx < 0) or (idx > MemberCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( PMemberArray(FMembers)^[idx] );
end;

procedure TMembersContainer.ReallocMembers;
var
  MemberPtr : PPyMemberDef;
begin
  Inc( FAllocatedMemberCount, PYT_MEMBER_BUFFER_INCREASE );
  ReAllocMem( FMembers, SizeOf(PyMemberDef)*(FAllocatedMemberCount+1));
  MemberPtr :=@(PMemberArray(FMembers)^[MemberCount+1]);
  FillChar( MemberPtr^,SizeOf(PyMemberDef)*PYT_MEMBER_BUFFER_INCREASE,0);
end;

////////////////////////////////////////
// class TGetSetContainer


procedure TGetSetContainer.AddGetSet(AName: PAnsiChar; AGet: getter;
  ASet: setter; ADoc: PAnsiChar; AClosure: Pointer);
begin
  if FGetSetCount = FAllocatedGetSetCount then
    ReallocGetSets;
  with GetSet[ GetSetCount ]^ do
    begin
      name      := AName;
      get       := AGet;
      _set      := ASet;
      doc       := ADoc;
      closure   := AClosure;
    end;
  Inc( FGetSetCount );
end;

procedure TGetSetContainer.AllocGetSets;
begin
  FAllocatedGetSetCount := PYT_GETSET_BUFFER_INCREASE;
  Assert(not Assigned(FGetSets));
  FGetSets := PPyGetSetDef(AllocMem(SizeOf(PyGetSetDef)*(FAllocatedGetSetCount+1)));
end;

procedure TGetSetContainer.ClearGetSets;
begin
  FGetSetCount := 0;
  FillChar(FGetSets^, Sizeof(FGetSets^)*FAllocatedGetSetCount, 0);
end;

constructor TGetSetContainer.Create(AOwner: TComponent);
begin
  inherited;
  AllocGetSets;
end;

destructor TGetSetContainer.Destroy;
begin
  FreeGetSets;
  inherited;
end;

procedure TGetSetContainer.Finalize;
begin
  if not (csDestroying in ComponentState) then
    ClearGetSets;
  inherited;
end;

procedure TGetSetContainer.FreeGetSets;
begin
  if Assigned(FGetSets) then
  begin
    FreeMem(FGetSets);
    FGetSets := nil;
  end;
  FGetSetCount := 0;
  FAllocatedGetSetCount := 0;
end;

function TGetSetContainer.GetGetSet(idx: Integer): PPyGetSetDef;
begin
  if (idx < 0) or (idx > GetSetCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( PGetSetArray(FGetSets)^[idx] );
end;

procedure TGetSetContainer.ReallocGetSets;
var
  GetSetPtr : PPyGetSetDef;
begin
  Inc( FAllocatedGetSetCount, PYT_GETSET_BUFFER_INCREASE );
  ReAllocMem( FGetSets, SizeOf(PyGetSetDef)*(FAllocatedGetSetCount+1));
  GetSetPtr :=@(PGetSetArray(FGetSets)^[GetSetCount+1]);
  FillChar( GetSetPtr^,SizeOf(PyGetSetDef)*PYT_GETSET_BUFFER_INCREASE,0);
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonModule                           **)
(**                                                   **)
(*******************************************************)

////////////////////////////////////////
// class TParentClassError

procedure TParentClassError.AssignTo( Dest: TPersistent );
begin
  if Dest is TParentClassError then
     with TParentClassError( Dest ) do
       begin
         FName   := Self.FName;
         FModule := Self.FModule;
       end;
  inherited;
end;

////////////////////////////////////////
// class TError

function TError.GetDisplayName: String;
begin
  Result := string(Name);
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TError.SetName( const Value : AnsiString );

  procedure CheckName;
  var
    i : Integer;
    m : TPythonModule;
  begin
    with Collection as TErrors do
      begin
        if GetOwner = nil then
          Exit;
        m := GetOwner as TPythonModule;
        for i := 0 to Count - 1 do
          with Items[i] do
            if Name = Value then
              raise Exception.CreateFmt( 'In module "%s", there''s already an error named "%s"',
                                         [m.ModuleName, Value]);
      end;
  end;

  procedure UpdateDependencies;
  var
    i, j : Integer;
    m : TPythonModule;
  begin
    if FName = '' then
      Exit;
    with Collection as TErrors do
      with GetOwner as TPythonModule do
        begin
          if not Assigned(Engine) then
            Exit;
          m := TPythonModule( TErrors(Self.Collection).GetOwner );
          with Engine do
            begin
              for i := 0 to ClientCount - 1 do
                if Clients[i] is TPythonModule then
                  with TPythonModule(Clients[i]) do
                    begin
                      for j := 0 to Errors.Count - 1 do
                        with Errors.Items[j] do
                          if (ParentClass.Module = m.ModuleName) and
                             (ParentClass.Name = Self.Name) then
                            ParentClass.Name := Value;
                    end;
            end;
        end;
  end;

begin
  if (FName <> Value) and (Value <> '') then
  begin
    CheckName;
    if ErrorType = etClass then
      UpdateDependencies;
    FName := Value;
    Changed(False);
  end;
end;

procedure TError.SetText( const Value : AnsiString );
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

procedure TError.SetErrorType( Value : TErrorType );
begin
  if FErrorType <> Value then
  begin
    FErrorType := Value;
    if FErrorType = etString then
      FParentClass.Name := '';
    Changed(False);
  end;
end;

procedure TError.SetParentClass( Value : TParentClassError );
begin
  FParentClass.Assign( Value );
  Changed(False);
end;

constructor TError.Create(ACollection: TCollection);
begin
  inherited;
  FErrorType := etString;
  FParentClass := TParentClassError.Create;
end;

destructor TError.Destroy;
begin
  FParentClass.Free;
  inherited;
end;

procedure TError.Assign(Source: TPersistent);
begin
  if Source is TError then
  begin
    Name := TError(Source).Name;
    Text := TError(Source).Text;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TError.BuildError( const ModuleName : AnsiString );

  function FindParentClass : PPyObject;
  var
    m, d : PPyObject;
  begin
    Owner.Owner.CheckEngine;
    with Owner.Owner.Engine do
      begin
        if ParentClass.Module <> '' then
          //m := PyImport_ImportModule( PAnsiChar(ParentClass.Module) )
          m := PyImport_AddModule( PAnsiChar(ParentClass.Module) )
        else
          m := FindModule( ModuleName );
        if not Assigned(m) then
          raise Exception.CreateFmt('Could not find module containing the parent class of error "%s"', [Self.Name]);
        d := PyModule_GetDict(m);
        Result := PyDict_GetItemString( d, PAnsiChar(ParentClass.Name) );
        if not Assigned(Result) then
          raise Exception.CreateFmt('Could not find the parent class "%s" of error "%s"', [ParentClass.Name, Self.Name]);
        if not PyClass_Check( Result ) and not PyType_CheckExact( Result ) then
          raise Exception.CreateFmt('The object "%s" in module "%s" is not a class', [ParentClass.Name, ParentClass.Module] );
      end;
  end;

var
  parent : PPyObject;
begin
  if Assigned(Error) then
    Exit;
  if Name = '' then
    with GetOwner as TPythonModule do
      raise Exception.CreateFmt( 'Error without name in module "%s"', [ModuleName] );
  if Text = '' then
    Text := Name;
  Owner.Owner.CheckEngine;
  with Owner.Owner.Engine do
    begin
      if ErrorType = etString then
        Error := PyString_FromString( PAnsiChar(Text) )
      else if ErrorType = etClass then
        begin
          if FParentClass.Name <> '' then
            parent := FindParentClass
          else
            parent := nil;
          Error := PyErr_NewException(
            PAnsiChar(AnsiString(Format('%s.%s', [ModuleName, Self.Name]))),
                                                   parent, nil );
        end;
    end;
  if not Assigned(Error) then
    raise Exception.CreateFmt( 'Could not create error "%s"', [Name] );
end;

procedure TError.RaiseError( const msg : AnsiString );
begin
  Owner.Owner.CheckEngine;
  with Owner.Owner.Engine do
    PyErr_SetString( Error, PAnsiChar(msg) );
end;

procedure TError.RaiseErrorObj( const msg : AnsiString; obj : PPyObject );
var
  args, res, str : PPyObject;
  inst : PPyInstanceObject;
  i : Integer;
  keys : PPyObject;
  key : PPyObject;
  val : PPyObject;
begin
  Owner.Owner.CheckEngine;
  with Owner.Owner.Engine do
    // if we give a dictionary as argument, then we use it for the
    // instance.
    if PyDict_Check( obj ) then
      begin
        args := PyTuple_New(0);
        if not Assigned(args) then
          raise Exception.Create('TError.RaiseErrorObj: Could not create an empty tuple');
        res := PyEval_CallObjectWithKeywords(Error, args, nil);
        Py_DECREF(args);
        if not Assigned(res) then
          raise Exception.CreateFmt('TError.RaiseErrorObj: Could not create an instance of "%s"', [Self.Name]);
        if PyInstance_Check( res ) then
          begin
            inst := PPyInstanceObject(res);
            Py_XDECREF( inst^.in_dict );
            inst^.in_dict := obj;
            str := PyString_FromString( PAnsiChar(msg) );
            PyDict_SetItemString( obj, 'args', str );
            Py_XDecRef(str);
          end
        else if PyObject_TypeCheck(res, PPyTypeObject(PyExc_Exception^)) then
          begin
            args := PyTuple_New(1);
            if not Assigned(args) then
              raise Exception.Create('TError.RaiseErrorObj: Could not create an empty tuple');
            str := PyString_FromString( PAnsiChar(msg) );
            PyTuple_SetItem(args, 0, str);
            res := PyEval_CallObjectWithKeywords(Error, args, nil);
            Py_DECREF(args);
            if not Assigned(res) then
              raise Exception.CreateFmt('TError.RaiseErrorObj: Could not create an instance of "%s"', [Self.Name]);
            keys := PyDict_Keys(obj);
            for i := 0 to PySequence_Length(keys)-1 do
            begin
              key := PySequence_GetItem(keys, i);
              val := PyDict_GetItem(obj, key);
              if Assigned(val) then
              begin
                PyObject_SetAttr(res, key, val);
                Py_DECREF(val);
              end;
              Py_XDECREF(key);
            end;
            Py_XDECREF(keys);
          end
        else
          raise Exception.Create('TError.RaiseErrorObj: I didn''t get an instance' );
        PyErr_SetObject( Error, res );
      end
    else
      PyErr_SetObject( Error, obj );
end;

function  TError.Owner : TErrors;
begin
  Result := GetOwner as TErrors;
end;

////////////////////////////////////////
// class TErrors

function TErrors.GetError(Index: Integer): TError;
begin
  Result := TError(inherited GetItem(Index));
end;

procedure TErrors.SetError(Index: Integer; Value: TError);
begin
  inherited SetItem(Index, Value);
end;

function TErrors.GetOwner: TPersistent;
begin
  Result := FModule;
end;

procedure TErrors.Update(Item: TCollectionItem);
begin
  inherited;
end;

constructor TErrors.Create(Module: TPythonModule );
begin
  inherited Create( TError );
  FModule := Module;
end;

function  TErrors.Add: TError;
begin
  Result := TError(inherited Add);
end;

function  TErrors.Owner : TPythonModule;
begin
  Result := GetOwner as TPythonModule;
end;

////////////////////////////////////////
// class TPythonModule

function TPythonModule.GetClientCount : Integer;
begin
  Result := FClients.Count;
end;

function TPythonModule.GetClients( idx : Integer ) : TEngineClient;
begin
  Result := TEngineClient(FClients.Items[idx]);
end;

procedure TPythonModule.SetErrors( val : TErrors );
begin
  FErrors.Assign( val );
end;

procedure TPythonModule.SetModuleName( const val : AnsiString );

  procedure UpdateDependencies;
  var
    i, j : Integer;
  begin
    if not Assigned(Engine) then
      Exit;
    if FModuleName = '' then
      Exit;
    with Engine do
      for i := 0 to ClientCount - 1 do
        if Clients[i] is TPythonModule then
          with TPythonModule(Clients[i]) do
            for j := 0 to Errors.Count - 1 do
              with Errors.Items[j] do
                if ParentClass.Module = Self.FModuleName then
                  ParentClass.Module := val;
  end;

begin
  if (FModuleName <> val) and (val <> '') then
    begin
      UpdateDependencies;
      FModuleName := val;
    end;
end;

constructor TPythonModule.Create( AOwner : TComponent );
begin
  inherited;
  FClients := TList.Create;
  FErrors  := TErrors.Create(Self);
  FDocString := TStringList.Create;
end;

destructor  TPythonModule.Destroy;
begin
  FDocString.Free;
  FClients.Free;
  FErrors.Free;
  inherited;
end;


procedure TPythonModule.SetDocString( value : TStringList );
begin
  FDocString.Assign( value );
end;

procedure TPythonModule.DefineDocString;
var
  doc : PPyObject;
begin
  with Engine do
    begin
      if DocString.Text <> '' then
        begin
          doc :=
            PyString_FromDelphiString(CleanString(FDocString.Text, False));
          PyObject_SetAttrString( FModule, '__doc__', doc );
          Py_XDecRef(doc);
          CheckError(False);
        end;
    end;
end;

procedure TPythonModule.MakeModule;
begin
  CheckEngine;
  if Assigned(FModule) then
    Exit;
  with Engine do
    begin
      if IsPython3000 then begin
        FillChar(FModuleDef, SizeOf(FModuleDef), 0);
        FModuleDef.m_base.ob_refcnt := 1;
        FModuleDef.m_name := PAnsiChar(ModuleName);
        FModuleDef.m_methods := MethodsData;
        FModuleDef.m_size := -1;
        FModule := Py_InitModule3000( ModuleDef );
      end else
        FModule := Py_InitModule( PAnsiChar(ModuleName), MethodsData );
      DefineDocString;
    end;
end;

procedure TPythonModule.Initialize;
var
  i : Integer;
begin
  inherited;
  FModule := nil;
  MakeModule;
  for i := 0 to ClientCount - 1 do
    Clients[i].ModuleReady(Self);
  BuildErrors;
  if Assigned(FOnAfterInitialization) then
    FOnAfterInitialization( Self );
end;

procedure TPythonModule.InitializeForNewInterpreter;
var
  initialized : Boolean;
  oldModule : PPyObject;
begin
  initialized := FInitialized;
  oldModule := FModule;
  FModule := nil;
  FInitialized := False;
  try
    Initialize;
  finally
    FInitialized := initialized;
    FModule := oldModule;
  end;
end;

procedure TPythonModule.AddClient( client : TEngineClient );
begin
  FClients.Add( client );
end;

function TPythonModule.ErrorByName( const AName : AnsiString ) : TError;
var
  i : Integer;
begin
  for i := 0 to Errors.Count - 1 do
    if CompareText( string(Errors.Items[i].Name), string(AName) ) = 0 then
      begin
        Result := Errors.Items[i];
        Exit;
      end;
  raise Exception.CreateFmt( 'Could not find error "%s"', [AName] );
end;

procedure TPythonModule.RaiseError( const error, msg : AnsiString );
begin
  ErrorByName( error ).RaiseError( msg );
end;

procedure TPythonModule.RaiseErrorFmt( const error, format : AnsiString; Args : array of const );
begin
  RaiseError( error, AnsiString(SysUtils.Format( string(format), Args )) );
end;

procedure TPythonModule.RaiseErrorObj( const error, msg : AnsiString; obj : PPyObject );
begin
  ErrorByName( error ).RaiseErrorObj( msg, obj );
end;

procedure TPythonModule.BuildErrors;
var
  i : Integer;
  d : PPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      d := PyModule_GetDict( Module );
      if not Assigned(d) then
        Exit;
      for i := 0 to Errors.Count - 1 do
        with Errors.Items[i] do
          begin
            BuildError( ModuleName );
            PyDict_SetItemString( d, PAnsiChar(Name), Error );
          end;
    end;
end;

// warning, this function will increase the refcount of value,
// so, if you don't want to keep a link, don't forget to decrement
// the refcount after the SetVar method.
procedure TPythonModule.SetVar( const varName : AnsiString; value : PPyObject );
begin
  if Assigned(FEngine) and Assigned( FModule ) then
    begin
      if Engine.PyObject_SetAttrString(Module, PAnsiChar(varName), value ) <> 0 then
        raise EPythonError.CreateFmt( 'Could not set var "%s" in module "%s"', [varName, ModuleName] );
    end
  else
    raise EPythonError.CreateFmt( 'Can''t set var "%s" in module "%s", because it is not yet initialized', [varName, ModuleName] );
end;

// warning, this function will increase the refcount of value,
// so, if you don't want to keep a link, don't forget to decrement
// the refcount after the GetVar method.
function  TPythonModule.GetVar( const varName : AnsiString ) : PPyObject;
begin
  if Assigned(FEngine) and Assigned( FModule ) then
  begin
    Result := Engine.PyObject_GetAttrString(Module, PAnsiChar(varName) );
    Engine.PyErr_Clear;
  end
  else
    raise EPythonError.CreateFmt( 'Can''t get var "%s" in module "%s", because it is not yet initialized', [varName, ModuleName] );
end;

procedure TPythonModule.DeleteVar( const varName : AnsiString );
var
  dict : PPyObject;
begin
  if Assigned(FEngine) and Assigned( FModule ) then
    with Engine do
    begin
      dict := PyModule_GetDict( Module );
      if not Assigned(dict) then raise EPythonError.CreateFmt( 'Can''t get __dict__ of module "%s"', [ModuleName] );
      PyDict_DelItemString( dict, PAnsiChar(varName) );
    end
  else
    raise EPythonError.CreateFmt( 'Can''t delete var "%s" in module "%s", because it is not yet initialized', [varName, ModuleName] );
end;

procedure TPythonModule.ClearVars;
var
 dict : PPyObject;
begin
 if Assigned(FEngine) and Assigned( FModule ) then
   with Engine do begin
     dict := PyModule_GetDict( Module );
     PyDict_Clear(dict);
   end;
end;

procedure TPythonModule.SetVarFromVariant( const varName : AnsiString; const value : Variant );
var
  obj : PPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := VariantAsPyObject( value );
      try
        SetVar( varName, obj );
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

function  TPythonModule.GetVarAsVariant( const varName : AnsiString ) : Variant;
var
  obj : PPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := GetVar( varName );
      try
        Result := PyObjectAsVariant( obj );
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonType                             **)
(**                                                   **)
(*******************************************************)

//////////////////////////////
//  TPyObject

// Constructors & Destructors
constructor TPyObject.Create( APythonType : TPythonType );
begin
  inherited Create;
  if Assigned(APythonType) then
  begin
    ob_refcnt := 1;
    PythonType := APythonType;
    with APythonType do
    begin
      Inc(FInstanceCount);
      Inc(FCreateHits);
    end;
  end;
end;

constructor TPyObject.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
  Create( APythonType );
end;

destructor TPyObject.Destroy;
begin
  if Assigned(PythonType) then
  begin
    Dec(PythonType.FInstanceCount);
    Inc(PythonType.FDeleteHits);
  end;
  inherited;
end;

class function TPyObject.NewInstance: TObject;
var
  mem : PAnsiChar;
begin
  GetMem(mem, InstanceSize + Sizeof(PyObject));
  PPyObject(mem)^.ob_refcnt := 1;
  PPyObject(mem)^.ob_type := nil;
  Result := InitInstance(Mem+Sizeof(PyObject));
end;

procedure TPyObject.FreeInstance;
begin
  CleanupInstance;
  if not PythonAlloc then
    FreeMem(PAnsiChar(Self)-Sizeof(PyObject));
end;

// Misc
function  TPyObject.GetSelf : PPyObject;
begin
  Result := PPyObject( PAnsiChar(Self)-Sizeof(PyObject) )
end;

procedure TPyObject.IncRef;
begin
  Inc(GetSelf^.ob_refcnt);
end;

procedure TPyObject.Adjust(PyPointer: Pointer);
var
  ptr : PNativeInt;
begin
  ptr := PyPointer;
  ptr^ := NativeInt(PythonToDelphi(PPyObject(ptr^)));
end;

function  TPyObject.GetModule : TPythonModule;
begin
  if Assigned(PythonType) then
    Result := PythonType.Module
  else
    Result := nil;
end;

function TPyObject.Get_ob_refcnt: NativeInt;
begin
  Result := GetSelf^.ob_refcnt;
end;

function TPyObject.Get_ob_type: PPyTypeObject;
begin
  Result := GetSelf^.ob_type;
end;

procedure TPyObject.Set_ob_refcnt(const Value: NativeInt);
begin
  GetSelf^.ob_refcnt := Value;
end;

procedure TPyObject.Set_ob_type(const Value: PPyTypeObject);
begin
  GetSelf^.ob_type := Value;
end;

// Type services
////////////////

// Basic services
function  TPyObject.Print( var f: file; i: integer) : Integer;
begin
  Result := -1;
end;

function  TPyObject.GetAttr(key : PAnsiChar) : PPyObject;
var
  PyKey : PPyObject;
begin
  with GetPythonEngine do
    begin
      PyKey := PyString_FromString(key);
      try
        Result := PyObject_GenericGetAttr(GetSelf, PyKey)
      finally
        Py_XDecRef(PyKey);
      end;
    end;
end;

function  TPyObject.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  with GetPythonEngine do
    begin
      Result := -1;
      PyErr_SetString (PyExc_AttributeError^,
        PAnsiChar(AnsiString(Format('Unknown attribute "%s"',[key]))));
    end;
end;

function  TPyObject.Repr : PPyObject;
begin
  Result :=
    GetPythonEngine.PyString_FromDelphiString( Format('<%s at %x>',
        [PythonType.TypeName, NativeInt(self)]) );
end;

function  TPyObject.Compare( obj: PPyObject) : Integer;
begin
  Result := 0;
end;

function  TPyObject.Hash : NativeInt;
begin
  Result := NativeInt(Self);
end;

function  TPyObject.Str: PPyObject;
begin
  Result := Repr;
end;

function  TPyObject.GetAttrO( key: PPyObject) : PPyObject;
begin
  Result := GetPythonEngine.PyObject_GenericGetAttr(GetSelf, key);
end;

function  TPyObject.SetAttrO( key, value: PPyObject) : Integer;
begin
  Result := GetPythonEngine.PyObject_GenericSetAttr(GetSelf, key, value);
end;

function  TPyObject.Call( ob1, ob2 : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.Traverse( proc: visitproc; ptr: Pointer) : integer;
begin
  Result := 0;
end;

function  TPyObject.Clear: integer;
begin
  Result := 0;
end;

function  TPyObject.RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject;
Var
  Res : Boolean;
begin
  Res := False;
  case Op of
    pyLT: Res := Compare(obj) < 0;
    pyLE: Res := Compare(obj) <= 0;
    pyEQ: Res := Compare(obj) = 0;
    pyNE: Res := Compare(obj) <> 0;
    pyGT: Res := Compare(obj) > 0;
    pyGE: Res := Compare(obj) >= 0;
  end;
  if Res then
    Result := PPyObject(GetPythonEngine.Py_True)
  else
    Result := PPyObject(GetPythonEngine.Py_False);
  GetPythonEngine.Py_INCREF( Result );
end;

function  TPyObject.Iter : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.IterNext : PPyObject;
begin
  Result := nil;
end;

{ Called when an instance of a subtype has been created. Same as __init__ in a class }
function TPyObject.Init( args, kwds : PPyObject ) : Integer;
begin
  Result := 0;
end;

// Number services
function  TPyObject.NbAdd( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbSubstract( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbMultiply( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbFloorDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbTrueDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbRemainder( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbDivmod( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbPower( ob1, ob2 : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbNegative : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbPositive : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbAbsolute : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbNonZero : Integer;
begin
  Result := -1;
end;

function  TPyObject.NbInvert : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbLShift( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbRShift( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbAnd( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbXor( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbOr( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbCoerce( obj : PPPyObject) : Integer;
begin
  Result := 0;
end;

function  TPyObject.NbInt : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbLong : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbFloat : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbOct : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbHex : PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceAdd(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceAnd(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceDivide(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceFloorDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceTrueDivide( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceLshift(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceMultiply(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceOr(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplacePower(ob1, ob2: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceRemainder(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceRshift(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceSubtract(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.NbInplaceXor(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

// Sequence services
function  TPyObject.SqLength : NativeInt;
begin
  Result := 0;
end;

function  TPyObject.SqConcat( obj : PPyObject) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqRepeat( val : NativeInt ) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqItem( idx : NativeInt ) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqSlice( idx1, idx2 : NativeInt ) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.SqAssItem( idx : NativeInt; obj : PPyObject) : Integer;
begin
  Result := -1;
end;

function  TPyObject.SqAssSlice( idx1, idx2 : NativeInt; obj : PPyObject): integer;
begin
  Result := -1;
end;

function TPyObject.SqContains(obj: PPyObject): integer;
begin
  Result := -1;
end;

function TPyObject.SqInplaceConcat(obj: PPyObject): PPyObject;
begin
  Result := nil;
end;

function TPyObject.SqInplaceRepeat(i: NativeInt): PPyObject;
begin
  Result := nil;
end;

// Mapping services
function  TPyObject.MpLength : NativeInt;
begin
  Result := 0;
end;

function  TPyObject.MpSubscript( obj : PPyObject) : PPyObject;
begin
  Result := GetPythonEngine.ReturnNone;
end;

function  TPyObject.MpAssSubscript( obj1, obj2 : PPyObject) : Integer;
begin
  Result := -1;
end;


// Class methods
class procedure TPyObject.RegisterMethods( APythonType : TPythonType );
begin
end;

class procedure TPyObject.RegisterMembers( APythonType : TPythonType );
begin
end;

class procedure TPyObject.RegisterGetSets( APythonType : TPythonType );
begin
end;

class procedure TPyObject.SetupType(APythonType: TPythonType);
begin

end;


//////////////////////////////
//  TTypeServices

constructor TTypeServices.Create;
begin
  inherited;
  FBasic := [bsGetAttr, bsSetAttr, bsRepr, bsStr];
end;

procedure TTypeServices.AssignTo( Dest: TPersistent );
begin
  if Dest is TTypeServices then
     with TTypeServices( Dest ) do
       begin
         FBasic         := Self.FBasic;
         FNumber        := Self.FNumber;
         FSequence      := Self.FSequence;
         FMapping       := Self.FMapping;
         FInplaceNumber := Self.FInplaceNumber;
       end;
  inherited;
end;

//////////////////////////////
//  TPythonType

function  PythonToDelphi( obj : PPyObject ) : TPyObject;
begin
  if IsDelphiObject( obj ) then
    Result := TPyObject(PAnsiChar(obj)+Sizeof(PyObject))
  else
    raise EPythonError.CreateFmt( 'Python object "%s" is not a Delphi class', [GetPythonEngine.PyObjectAsString(obj)] );
end;

procedure PyObjectDestructor( pSelf : PPyObject); cdecl;
var
  call_tp_free : Boolean;
  obj : TPyObject;
begin
  obj := PythonToDelphi(pSelf);
  call_tp_free := obj.PythonAlloc;
  if PythonOk then
    obj.Free;
  if call_tp_free and Assigned(pSelf.ob_type) and Assigned(pSelf.ob_type^.tp_free) then
    pSelf.ob_type^.tp_free(pSelf);
end;

procedure TPythonType.Notification( AComponent: TComponent;
                                    Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FModule then
      FModule := nil;
end;

procedure TPythonType.SetPyObjectClass( val : TPyObjectClass );
begin
  if val <> FPyObjectClass then
    begin
      if Assigned(FPyObjectClass) then
      begin
        ClearMethods;
        ClearMembers;
        ClearGetSets;
      end;
      FPyObjectClass := val;
      if Assigned(val) then
        begin
          FType.tp_basicsize := val.InstanceSize + Sizeof(PyObject);
          val.SetupType( Self );
          val.RegisterMethods( Self );
          val.RegisterMembers( Self );
          val.RegisterGetSets( Self );
        end;
    end;
end;

procedure TPythonType.SetModule( val : TPythonModule );
begin
  if val <> FModule then
    begin
      FModule := val;
      if Assigned(val) then
        if Initialized and not (csLoading in ComponentState) then
          if val.Initialized then
            AddTypeVar
          else
            val.AddClient(Self);
    end;
end;

procedure TPythonType.ModuleReady(Sender : TObject);
begin
  inherited;
  AddTypeVar;
end;

procedure TPythonType.SetServices( val : TTypeServices );
begin
  FServices.Assign( val );
end;

procedure TPythonType.SetTypeName( const val : AnsiString );
begin
  if (FTypeName <> val) and (val <> '') then
    begin
      FTypeName := val;
    end;
end;

function  TPythonType.CreateMethod( pSelf, args : PPyObject ) : PPyObject;
begin
  Result := CreateInstanceWith( args );
end;

procedure TPythonType.ReallocGetSets;
begin
  inherited;
  if tpfBaseType in TypeFlags then
    FType.tp_getset := GetSetData;
end;

procedure TPythonType.ReallocMembers;
begin
  inherited;
  if tpfBaseType in TypeFlags then
    FType.tp_members := MembersData;
end;

procedure TPythonType.ReallocMethods;
begin
  inherited;
  if tpfBaseType in TypeFlags then
    FType.tp_methods := MethodsData;
end;

procedure TPythonType.SetDocString( value : TStringList );
begin
  FDocString.Assign( value );
end;

function  TPythonType.TypeFlagsAsInt : LongInt;
begin
  Result := 0;
  if tpfHaveGetCharBuffer in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_GETCHARBUFFER;
  if tpfHaveSequenceIn in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_SEQUENCE_IN;
  if tpfGC in TypeFlags then
    Result := Result or Py_TPFLAGS_GC;
  if tpfHaveInplaceOps in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_INPLACEOPS;
  if tpfCheckTypes in TypeFlags then
    Result := Result or Py_TPFLAGS_CHECKTYPES;
  if tpfHaveRichCompare in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_RICHCOMPARE;
  if tpfHaveWeakRefs in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_WEAKREFS;
  if tpfHaveIter in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_ITER;
  if tpfHaveClass in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_CLASS;
  if tpfHeapType in TypeFlags then
    Result := Result or Py_TPFLAGS_HEAPTYPE;
  if tpfBaseType in TypeFlags then
    Result := Result or Py_TPFLAGS_BASETYPE;
  if tpfReady in TypeFlags then
    Result := Result or Py_TPFLAGS_READY;
  if tpfReadying in TypeFlags then
    Result := Result or Py_TPFLAGS_READYING;
  if tpfHaveGC in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_GC;
end;

// Type services
// They will be all forwarded to the Delphi class that
// implements the object through the use of virtual
// methods
///////////////////////////////////////

// Basic services

function  TPythonType_Print( pSelf : PPyObject; var f: file; i: integer) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Print( f, i );
end;

function  TPythonType_GetAttr( pSelf : PPyObject; key : PAnsiChar) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).GetAttr( key );
end;

function  TPythonType_SetAttr( pSelf : PPyObject; key : PAnsiChar; value : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SetAttr( key, value );
end;

function  TPythonType_Repr( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Repr;
end;

function  TPythonType_Compare( pSelf, obj : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Compare( obj );
end;

function  TPythonType_Hash( pSelf : PPyObject) : NativeInt; cdecl;
begin
  Result := PythonToDelphi(pSelf).Hash;
end;

function  TPythonType_Str( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Str;
end;

function  TPythonType_GetAttrO( pSelf, key: PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).GetAttrO( key );
end;

function  TPythonType_SetAttrO( pSelf, key, value: PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SetAttrO( key, value );
end;

function  TPythonType_Call( pSelf, ob1, ob2 : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Call( ob1, ob2 );
end;

function  TPythonType_Traverse( pSelf: PPyObject; proc: visitproc; ptr: Pointer) : integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Traverse( proc, ptr );
end;

function  TPythonType_Clear( pSelf: PPyObject): integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Clear;
end;

function  TPythonType_RichCmp( pSelf, obj : PPyObject; i : Integer) : PPyObject; cdecl;
begin
  Assert(i >= Ord(Low(TRichComparisonOpcode)));
  Assert(i <= Ord(High(TRichComparisonOpcode)));
  Result := PythonToDelphi(pSelf).RichCompare( obj, TRichComparisonOpcode(i) );
end;

function  TPythonType_Iter( pSelf: PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).Iter;
end;

function  TPythonType_IterNext( pSelf: PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).IterNext;
end;

function  TPythonType_InitSubtype( pSelf, args, kwds : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).Init(args, kwds);
end;

function  TPythonType.NewSubtypeInst( aType: PPyTypeObject; args, kwds : PPyObject) : PPyObject;
var
  obj : TPyObject;
begin
  Result := aType^.tp_alloc(aType, 0);
  if Assigned(Result) then
  begin
    obj := PythonToDelphi(Result);
    PyObjectClass.InitInstance(obj);
    obj.ob_type := aType;
    obj.IsSubtype := aType <> @FType;
    obj.PythonAlloc := True;
    obj.CreateWith(Self, args);
    if Engine.PyErr_Occurred <> nil then
    begin
      Engine.Py_DECREF(Result);
      Result := nil;
    end;
  end;
end;

function  TPythonType_AllocSubtypeInst( pSelf: PPyTypeObject; nitems : NativeInt) : PPyObject; cdecl;
begin
  Result := GetPythonEngine.PyType_GenericAlloc(pSelf, nitems);
end;

procedure FreeSubtypeInst(ob:PPyObject);
begin
  GetPythonEngine.PyObject_Free(ob);
end;


// Number services

function  TPythonType_NbAdd( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbAdd( obj );
end;

function  TPythonType_NbSubstract( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbSubstract( obj );
end;

function  TPythonType_NbMultiply( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbMultiply( obj );
end;

function  TPythonType_NbDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbDivide( obj );
end;

function  TPythonType_NbFloorDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbFloorDivide( obj );
end;

function  TPythonType_NbTrueDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbTrueDivide( obj );
end;

function  TPythonType_NbRemainder( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbRemainder( obj );
end;

function  TPythonType_NbDivmod( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbDivmod( obj );
end;

function  TPythonType_NbPower( pSelf, ob1, ob2 : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbPower( ob1, ob2 );
end;

function  TPythonType_NbNegative( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbNegative;
end;

function  TPythonType_NbPositive( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbPositive;
end;

function  TPythonType_NbAbsolute( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbAbsolute;
end;

function  TPythonType_NbNonZero( pSelf : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbNonZero;
end;

function  TPythonType_NbInvert( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInvert;
end;

function  TPythonType_NbLShift( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbLShift( obj );
end;

function  TPythonType_NbRShift( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbRShift( obj );
end;

function  TPythonType_NbAnd( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbAnd( obj );
end;

function  TPythonType_NbXor( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbXor( obj );
end;

function  TPythonType_NbOr( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbOr( obj );
end;

function  TPythonType_NbCoerce( pSelf, obj : PPPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf^).NbCoerce( obj );
end;

function  TPythonType_NbInt( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInt;
end;

function  TPythonType_NbLong( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbLong;
end;

function  TPythonType_NbFloat( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbFloat;
end;

function  TPythonType_NbOct( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbOct;
end;

function  TPythonType_NbHex( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbHex;
end;

function TPythonType_NbInplaceAdd(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceAdd( obj );
end;

function TPythonType_NbInplaceAnd(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceAnd( obj );
end;

function TPythonType_NbInplaceDivide(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceDivide( obj );
end;

function  TPythonType_NbInplaceFloorDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceFloorDivide( obj );
end;

function  TPythonType_NbInplaceTrueDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceTrueDivide( obj );
end;

function TPythonType_NbInplaceLshift(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceLshift( obj );
end;

function TPythonType_NbInplaceMultiply(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceMultiply( obj );
end;

function TPythonType_NbInplaceOr(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceOr( obj );
end;

function TPythonType_NbInplacePower(pSelf, ob1, ob2: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplacePower( ob1, ob2 );
end;

function TPythonType_NbInplaceRemainder(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceRemainder( obj );
end;

function TPythonType_NbInplaceRshift(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceRshift( obj );
end;

function TPythonType_NbInplaceSubtract(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceSubtract( obj );
end;

function TPythonType_NbInplaceXor(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceXor( obj );
end;

// Sequence services

function  TPythonType_SqLength( pSelf : PPyObject ) : NativeInt; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqLength;
end;

function  TPythonType_SqConcat( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqConcat( obj );
end;

function  TPythonType_SqRepeat( pSelf : PPyObject; val : NativeInt ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqRepeat( val );
end;

function  TPythonType_SqItem( pSelf : PPyObject; idx : NativeInt ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqItem( idx );
end;

function  TPythonType_SqSlice( pSelf : PPyObject; idx1, idx2 : NativeInt ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqSlice( idx1, idx2 );
end;

function  TPythonType_SqAssItem( pSelf : PPyObject; idx : NativeInt; obj : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqAssItem( idx, obj );
end;

function  TPythonType_SqAssSlice( pSelf : PPyObject; idx1, idx2 : NativeInt; obj : PPyObject): integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqAssSlice( idx1, idx2, obj );
end;


// Mapping services

function  TPythonType_MpLength( pSelf : PPyObject ) : NativeInt; cdecl;
begin
  Result := PythonToDelphi(pSelf).MpLength;
end;

function  TPythonType_MpSubscript( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).MpSubscript( obj );
end;

function  TPythonType_MpAssSubscript( pSelf, obj1, obj2 : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).MpAssSubscript( obj1, obj2 );
end;

function TPythonType_SqContains(pSelf, obj : PPyObject): integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqContains( obj );
end;

function TPythonType_SqInplaceConcat(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqInplaceConcat( obj );
end;

function TPythonType_SqInplaceRepeat(pSelf : PPyObject; i: NativeInt): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqInplaceRepeat( i );
end;

procedure TPythonType.InitServices;
begin
  with FType do
    begin
      // Basic services
      if FDocString.Count > 0 then
        with Engine do
        begin
          FCurrentDocString := EncodeString(CleanString(FDocString.Text, False));
          tp_doc := PAnsiChar(FCurrentDocString);
        end;
      tp_dealloc   := @PyObjectDestructor;
      if bsGetAttr in Services.Basic then
        tp_getattr   := TPythonType_GetAttr;
      if bsSetAttr in Services.Basic then
        tp_setattr   := TPythonType_SetAttr;
      if bsRepr in Services.Basic then
        tp_repr      := TPythonType_Repr;
      if bsStr in Services.Basic then
        tp_str       := TPythonType_Str;
      if bsCompare in Services.Basic then
        tp_compare   := TPythonType_Compare;
      if bsHash in Services.Basic then
        tp_hash      := TPythonType_Hash;
      if bsGetAttrO in Services.Basic then
        tp_getattro  := TPythonType_GetAttrO;
      if bsSetAttrO in Services.Basic then
        tp_setattro  := TPythonType_SetAttrO;
      if bsCall in Services.Basic then
        tp_call      := TPythonType_Call;
      if bsTraverse in Services.Basic then
        tp_traverse := TPythonType_Traverse;
      if bsClear in Services.Basic then
        tp_clear := TPythonType_Clear;
      if bsRichCompare in Services.Basic then
        tp_richcompare := TPythonType_RichCmp;
      if bsIter in Services.Basic then
        tp_iter := TPythonType_Iter;
      if bsIterNext in Services.Basic then
        tp_iternext := TPythonType_IterNext;
      if tpfBaseType in TypeFlags then
      begin
        tp_init             := TPythonType_InitSubtype;
        tp_alloc            := TPythonType_AllocSubtypeInst;
        tp_new              := GetCallBack( Self, @TPythonType.NewSubtypeInst, 3, ctCDECL);
        tp_free             := FreeSubtypeInst;
        tp_methods          := MethodsData;
        tp_members          := MembersData;
        tp_getset           := GetSetData;
      end;

      // Number services
      if Services.Number <> [] then
        tp_as_number := @FNumber;
      with FNumber do
        begin
          if nsAdd in Services.Number then
            nb_add := TPythonType_NbAdd;
          if nsSubstract in Services.Number then
            nb_substract := TPythonType_NbSubstract;
          if nsMultiply in Services.Number then
            nb_multiply := TPythonType_NbMultiply;
          if nsDivide in Services.Number then
            nb_divide := TPythonType_NbDivide;
          if nsFloorDivide in Services.Number then
            nb_floor_divide := TPythonType_NbFloorDivide;
          if nsTrueDivide in Services.Number then
            nb_true_divide := TPythonType_NbTrueDivide;
          if nsRemainder in Services.Number then
            nb_remainder := TPythonType_NbRemainder;
          if nsDivmod in Services.Number then
            nb_divmod := TPythonType_NbDivmod;
          if nsPower in Services.Number then
            nb_power := TPythonType_NbPower;
          if nsNegative in Services.Number then
            nb_negative := TPythonType_NbNegative;
          if nsPositive in Services.Number then
            nb_positive := TPythonType_NbPositive;
          if nsAbsolute in Services.Number then
            nb_absolute := TPythonType_NbAbsolute;
          if nsNonZero in Services.Number then
            nb_nonzero := TPythonType_NbNonZero;
          if nsInvert in Services.Number then
            nb_invert := TPythonType_NbInvert;
          if nsLShift in Services.Number then
            nb_lshift := TPythonType_NbLShift;
          if nsRShift in Services.Number then
            nb_rshift := TPythonType_NbRShift;
          if nsAnd in Services.Number then
            nb_and := TPythonType_NbAnd;
          if nsXor in Services.Number then
            nb_xor := TPythonType_NbXor;
          if nsOr in Services.Number then
            nb_or := TPythonType_NbOr;
          if nsCoerce in Services.Number then
            nb_coerce := TPythonType_NbCoerce;
          if nsInt in Services.Number then
            nb_int := TPythonType_NbInt;
          if nsLong in Services.Number then
            nb_long := TPythonType_NbLong;
          if nsFloat in Services.Number then
            nb_float := TPythonType_NbFloat;
          if nsOct in Services.Number then
            nb_oct := TPythonType_NbOct;
          if nsHex in Services.Number then
            nb_hex := TPythonType_NbHex;
          if nsInplaceAdd in Services.InplaceNumber then
            nb_inplace_add       := TPythonType_NbInplaceAdd;
          if nsInplaceSubtract in Services.InplaceNumber then
            nb_inplace_subtract  := TPythonType_NbInplaceSubtract;
          if nsInplaceMultiply in Services.InplaceNumber then
            nb_inplace_multiply  := TPythonType_NbInplaceMultiply;
          if nsInplaceDivide in Services.InplaceNumber then
            nb_inplace_divide    := TPythonType_NbInplaceDivide;
          if nsInplaceFloorDivide in Services.InplaceNumber then
            nb_inplace_floor_divide := TPythonType_NbInplaceFloorDivide;
          if nsInplaceTrueDivide in Services.InplaceNumber then
            nb_inplace_true_divide := TPythonType_NbInplaceTrueDivide;
          if nsInplaceRemainder in Services.InplaceNumber then
            nb_inplace_remainder := TPythonType_NbInplaceRemainder;
          if nsInplacePower in Services.InplaceNumber then
            nb_inplace_power     := TPythonType_NbInplacePower;
          if nsInplaceLShift in Services.InplaceNumber then
            nb_inplace_lshift    := TPythonType_NbInplaceLShift;
          if nsInplaceRShift in Services.InplaceNumber then
            nb_inplace_rshift    := TPythonType_NbInplaceRShift;
          if nsInplaceAnd in Services.InplaceNumber then
            nb_inplace_and       := TPythonType_NbInplaceAnd;
          if nsInplaceXor in Services.InplaceNumber then
            nb_inplace_xor       := TPythonType_NbInplaceXor;
          if nsInplaceOr in Services.InplaceNumber then
            nb_inplace_or        := TPythonType_NbInplaceOr;
        end;

      // Sequence services
      if Services.Sequence <> [] then
        tp_as_sequence := @FSequence;

      with FSequence do
        begin
          if ssLength in Services.Sequence then
            sq_length := TPythonType_SqLength;
          if ssConcat in Services.Sequence then
            sq_concat := TPythonType_SqConcat;
          if ssRepeat in Services.Sequence then
            sq_repeat := TPythonType_SqRepeat;
          if ssItem in Services.Sequence then
            sq_item := TPythonType_SqItem;
          if ssSlice in Services.Sequence then
            sq_slice := TPythonType_SqSlice;
          if ssAssItem in Services.Sequence then
            sq_ass_item := TPythonType_SqAssItem;
          if ssAssSlice in Services.Sequence then
            sq_ass_slice := TPythonType_SqAssSlice;
          if ssContains in Services.Sequence then
            sq_contains := TPythonType_SqContains;
          if ssInplaceConcat in Services.Sequence then
            sq_inplace_concat  := TPythonType_SqInplaceConcat;
          if ssInplaceRepeat in Services.Sequence then
            sq_inplace_repeat  := TPythonType_SqInplaceRepeat;
        end;

      // Mapping services
      if Services.Mapping <> [] then
        tp_as_mapping := @FMapping;

      with FMapping do
        begin
          if msLength in Services.Mapping then
            mp_length := TPythonType_MpLength;
          if msSubScript in Services.Mapping then
            mp_subscript := TPythonType_MpSubscript;
          if msAssSubscript in Services.Mapping then
            mp_ass_subscript := TPythonType_MpAssSubscript;
        end;
    end;
end;

// Public methods

constructor TPythonType.Create( AOwner : TComponent );
begin
  inherited;
  FPrefix := 'Create';
  FServices := TTypeServices.Create;
  FDocString := TStringList.Create;
  FTypeFlags := TPFLAGS_DEFAULT;
  FGenerateCreateFunction := True;
end;

destructor  TPythonType.Destroy;
begin
  if gVarType = Self then
    gVarType := nil;
  FDocString.Free;
  FServices.Free;
  inherited;
end;

function  TPythonType.GetTypePtr : PPyTypeObject;
begin
  Result := PPyTypeObject(@FType);
end;

procedure TPythonType.Initialize;
begin
  CheckEngine;
  with Engine, FType do
    begin
      ob_type   := PPyTypeObject(PyType_Type);
      ob_refcnt := 1;
      tp_name   := PAnsiChar(FTypeName);
      tp_flags  := TypeFlagsAsInt;
    end;
  if Assigned(FModule) then
    begin
      if Module.Initialized then
        AddTypeVar
      else
        Module.AddClient( Self );
    end;
  InitServices;
  inherited;
end;

procedure TPythonType.Finalize;
begin
  Engine.Py_XDECREF(FCreateFunc);
  FCreateFunc := nil;
  inherited;
end;

function TPythonType.CreateInstance : PPyObject;
var
  obj : TPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := PyObjectClass.Create( Self );
      obj.ob_type := @FType;
      if PyErr_Occurred <> nil then
      begin
        obj.Free;
        Result := nil;
      end
      else
        Result := obj.GetSelf;
    end;
end;

function TPythonType.CreateInstanceWith( args : PPyObject ) : PPyObject;
var
  obj : TPyObject;
begin
  CheckEngine;
  with Engine do
    begin
      obj := PyObjectClass.CreateWith( Self, args );
      obj.ob_type := @FType;
      if PyErr_Occurred <> nil then
      begin
        obj.Free;
        Result := nil;
      end
      else
        Result := obj.GetSelf;
    end;
end;

procedure TPythonType.AddTypeVar;
var
  d : PPyObject;
  meth : TDelphiMethod;
begin
  CheckEngine;
  Assert(Module <> nil);
  Assert(Module.Module <> nil);
  if FGenerateCreateFunction then
  begin
    FCreateFuncName := FPrefix+FTypeName;
    FCreateFuncDoc := AnsiString(Format('Creates a new instance of type %s', [TypeName]));
    if not Assigned(FCreateFunc) then
    begin
      meth := CreateMethod;
      FCreateFuncDef.ml_name  := PAnsiChar(FCreateFuncName);
      FCreateFuncDef.ml_meth  := GetOfObjectCallBack( TCallBack(meth), 2, ctCDECL);
      FCreateFuncDef.ml_flags := METH_VARARGS;
      FCreateFuncDef.ml_doc   := PAnsiChar(FCreateFuncDoc);
      if GetPythonEngine.IsPython3000 then
        FCreateFunc := Engine.PyCFunction_NewEx(@FCreateFuncDef, nil, nil)
      else
        FCreateFunc := Engine.PyCFunction_New(@FCreateFuncDef, nil);
    end;
    Assert(Assigned(FCreateFunc));
  end;
  with Engine do
    begin
      d := PyModule_GetDict( Module.Module );
      Assert(Assigned(d));
      PyDict_SetItemString( d, PAnsiChar(TypeName), PPyObject(TheTypePtr) );
      if FGenerateCreateFunction then
        PyDict_SetItemString( d, PAnsiChar(FCreateFuncName), FCreateFunc );
    end;
end;

function TPythonType.GetMembersStartOffset : Integer;
begin
  Result := Sizeof(PyObject);
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonDelphiVar                        **)
(**                                                   **)
(*******************************************************)

procedure TPythonDelphiVar.CreateVarType;
begin
  if not Assigned(gVarType) then
  begin
    gVarType := TPythonType.Create( Self.Engine );
    with gVarType do
      begin
        TypeName := 'PythonDelphiVar';
        Engine := Self.Engine;
        PyObjectClass := TPyVar;
        Initialize;
      end;
  end;
end;

procedure TPythonDelphiVar.CreateVar;
var
  v : TPyVar;
  m, d : PPyObject;
begin
  if not Assigned(Engine) then
    Exit;
  Assert(Assigned(gVarType), 'missing TPythonType for TPythonDelphiVar');
  with Engine do
    begin
      // Create an instance of PythonDelphiVar
      FVarObject := gVarType.CreateInstance;
      CheckError(False);
      v := TPyVar(PythonToDelphi(FVarObject));
      v.dv_component := Self;
      // Add a reference to this var in the module
      m := PyImport_AddModule(PAnsiChar(Module));
      if m = nil then
        raise EPythonError.CreateFmt('CreateVar: can''t create module "%s"', [Module]);
      d := PyModule_GetDict(m);
      if @PyDict_SetItemString = nil then
        raise Exception.Create('nil');
      PyDict_SetItemString( d, PAnsiChar(VarName), FVarObject );
    end;
end;

function  TPythonDelphiVar.GetValue : Variant;
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      Result := GetValueAsVariant
  else
    raise Exception.Create('No variable was created' );
end;

procedure TPythonDelphiVar.SetValue( const val : Variant );
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      SetValueFromVariant(val)
  else
    raise Exception.Create('No variable was created' );
end;

// Warning: GetValueAsPyObject returns a preincremented object !
function  TPythonDelphiVar.GetValueAsPyObject : PPyObject;
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      Result := GetValue
  else
    raise Exception.Create('No variable was created' );
end;

procedure TPythonDelphiVar.SetValueFromPyObject( val : PPyObject );
begin
  if Assigned( FVarObject ) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      SetValue(val)
  else
    raise Exception.Create('No variable was created' );
end;

function  TPythonDelphiVar.IsVariantOk( const v : Variant ) : Boolean;
var
  t : Integer;
begin
  t := VarType(v) and VarTypeMask;
  Result := (t = varSmallint) or
            (t = varInteger) or
            (t = varSingle) or
            (t = varDouble) or
            (t = varCurrency) or
            (t = varDate) or
            (t = varOleStr) or
            (t = varBoolean) or
            (t = varByte) or
            {$IFDEF UNICODE}
            (t = varUString) or
            {$ENDIF}
            (t = varString);
end;

function  TPythonDelphiVar.GetValueAsString : string;
var
  v : Variant;
  obj : PPyObject;
begin
  v := Value;
  if IsVariantOk( v ) then
    Result := v
  else
    begin
      CheckEngine;
      obj := GetValueAsPyObject;
      try
        Result := Engine.PyObjectAsString( obj );
      finally
        Engine.Py_XDecRef(obj);
      end;
    end;
end;

procedure TPythonDelphiVar.SetVarName( const val : AnsiString );

  procedure CheckVarName;
  var
    i : Integer;
  begin
    if Owner = nil then Exit;
    if (val = FVarName) or (val = '') then Exit;
    for i := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[i] is TPythonDelphiVar then
        with TPythonDelphiVar(Owner.Components[i]) do
          if (VarName = val) and (Module = Self.Module) then
            raise Exception.CreateFmt('A variable "%s" already exists in the module "%s"',[val, Module]);
  end;

begin
  if val <> FVarName then
    begin
      CheckVarName;
      FVarName := val;
    end;
end;

constructor TPythonDelphiVar.Create( AOwner : TComponent );

  procedure AdjustName;
  var
    i, cpt : Integer;
    done : Boolean;
  begin
    if AOwner = nil then Exit;
    cpt := 1;
    done := False;
    while not done do
      begin
        done := True;
        for i := 0 to AOwner.ComponentCount - 1 do
          if AOwner.Components[i] is TPythonDelphiVar then
            with TPythonDelphiVar(AOwner.Components[i]) do
              if (VarName = Self.FVarName+AnsiString(IntToStr(cpt))) and
                 (Module = Self.Module) then
                begin
                  Inc(cpt);
                  done := False;
                  Break;
                end;
      end;
    FVarName := FVarName + AnsiString(IntToStr(cpt));
  end;

begin
  inherited;
  FModule := '__main__';
  FVarName := 'varname';
  if csDesigning in ComponentState then
    AdjustName;
end;

procedure TPythonDelphiVar.Initialize;
begin
  if csDesigning in ComponentState then
    Exit;
  CheckEngine;
  CreateVarType;
  CreateVar;
  inherited;
end;

procedure TPythonDelphiVar.Finalize;
begin
  inherited;
  if not PythonOK then
    Exit;
  if Assigned(FVarObject) then
    with TPyVar(PythonToDelphi(FVarObject)) do
      begin
        dv_component := nil;
        SetValue( nil );
      end;
  with Engine do
    Py_XDECREF( FVarObject );
  FVarObject := nil;
end;


constructor TPyVar.Create( APythonType : TPythonType );
begin
  inherited;
end;

// Don't call the Create constructor of TPyVar, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyVar.Create will be automatically be called.

constructor TPyVar.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'O:CreateVar',@dv_object ) = 0 then
        exit;
    end;
end;

destructor TPyVar.Destroy;
begin
  with GetPythonEngine do
    begin
      if Assigned(dv_object) then
        begin
          Py_DecRef(dv_object);
          dv_object := nil;
        end;
    end;
  inherited;
end;

// Then we override the needed services

function  TPyVar.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( string(key), 'Value') = 0 then
        Result := GetValue
      else
        Result := inherited GetAttr(key);
    end;
end;

function  TPyVar.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := 0;
  with GetPythonEngine do
    begin
      if CompareText( string(key), 'Value' ) = 0 then
        SetValue( value )
      else
        Result := inherited SetAttr(key, value);
    end;
end;

function  TPyVar.Repr : PPyObject;
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := GetValue;
      try
        Result :=
          PyString_FromDelphiString( Format('<%s: %s>',
            [PythonType.TypeName, PyObjectAsString(obj)]) );
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

// Class methods
// We register the methods of our type

class procedure TPyVar.RegisterMethods( APythonType : TPythonType );
begin
  inherited;
  with APythonType do
    begin
      //AddMethod( 'OffsetBy', @TPyPoint.DoOffsetBy, 'Point.OffsetBy( dx, dy )' );
    end;
end;

// Methods of TPyVar


// Warning: GetValue returns a preincremented object !
function TPyVar.GetValue : PPyObject;
var
  v : Variant;
begin
  Result := nil;
  with GetPythonEngine do
    begin
      if Assigned( dv_component ) and
         (@dv_component.OnExtGetData <> nil) then
         begin
           dv_component.OnExtGetData( dv_component, Result );
         end
      else if Assigned( dv_component ) and
         (@dv_component.OnGetData <> nil) then
        begin
           dv_component.OnGetData( dv_component, v );
           Result := VariantAsPyObject(v);
        end
      else if Assigned(dv_object) then
        begin
          Result := dv_object;
          Py_XIncRef(Result);
        end;
      if Result = nil then
        Result := ReturnNone;
    end;
end;

function TPyVar.GetValueAsVariant : Variant;
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := GetValue;
      try
        try
          Result := PyObjectAsVariant( obj );
        except
          Result := PyObjectAsString(obj);
        end;
      finally
        Py_XDecRef(obj);
      end;
    end;
end;

procedure TPyVar.SetValue( value : PPyObject );
begin
  with GetPythonEngine do
    begin
      if Assigned( dv_component ) and
         (@dv_component.OnExtSetData <> nil) then
        begin
          dv_component.OnExtSetData( dv_component, value);
        end
      else if Assigned( dv_component ) and
         (@dv_component.OnSetData <> nil) then
        begin
          dv_component.OnSetData( dv_component, PyObjectAsVariant(value) );
        end;
      Py_XDecRef(dv_object);
      dv_object := value;
      Py_XIncRef(dv_object);
      if Assigned( dv_component ) and
         (@dv_component.OnChange <> nil) then
        dv_component.OnChange( dv_component );
    end;
end;

procedure TPyVar.SetValueFromVariant( const value : Variant );
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := VariantAsPyObject( value );
      SetValue(obj);
      Py_XDecRef(obj);
    end;
end;

(*******************************************************)
(**                                                   **)
(**     class TPythonThread                           **)
(**                                                   **)
(*******************************************************)

procedure TPythonThread.Execute;
var
  global_state : PPyThreadState;
  gilstate : PyGILState_STATE;
begin
  with GetPythonEngine do
  begin
    if fThreadExecMode = emNewState then
    begin
      gilstate := PyGILState_Ensure();
      try
        fThreadState := GetThreadState;
        ExecuteWithPython;
      finally
        PyGILState_Release(gilstate);
      end;
    end else {fThreadExecMode}
    begin
      gilstate := PyGILState_Ensure();
      global_state := PyThreadState_Get;
      PyThreadState_Swap(nil);
      fThreadState := Py_NewInterpreter;

      if Assigned( fThreadState) then
      begin
        PyThreadState_Swap(fThreadState);
        ExecuteWithPython;
        Py_EndInterpreter( fThreadState);
        PyThreadState_Swap(global_state);
        PyGILState_Release(gilstate);
      end else
        raise EPythonError.Create( 'Could not create a new thread state');
    end; {withinterp}
  end;
end;


procedure TPythonThread.Py_Begin_Allow_Threads;
begin
  with GetPythonEngine do
    f_savethreadstate := PyEval_SaveThread;
end;

procedure TPythonThread.Py_End_Allow_Threads;
begin
  with GetPythonEngine do
    PyEval_RestoreThread( f_savethreadstate);
end;

procedure TPythonThread.Py_Begin_Block_Threads;
begin
  Py_End_Allow_Threads;
end;

procedure TPythonThread.Py_Begin_Unblock_Threads;
begin
  Py_Begin_Allow_Threads;
end;


(*******************************************************)
(**                                                   **)
(**     Methods for new Python objects or modules     **)
(**                                                   **)
(*******************************************************)

/////////////////////////////////////////////////////////
// Module pyio for Python Input/Outputs
//

function pyio_write(self, args : PPyObject) : PPyObject;
var
  a1 : PPyObject;
begin
  // Forbid printing for any other thread than the main one
  {$IFNDEF FPC}
  if GetCurrentThreadId <> MainThreadId then
    with GetPythonEngine do
      begin
        if RedirectIO and (IO <> nil) and (IO.ClassName <> 'TPythonInputOutput') and not IO.DelayWrites then
          begin
            Result := GetPythonEngine.ReturnNone;
            Exit;
          end;
      end;
  {$ENDIF}
  with GetPythonEngine do
    begin
      if Assigned(args) and (PyTuple_Size(args) > 0) then
        begin
          a1 := PyTuple_GetItem(args, 0);
          if RedirectIO and (IO <> nil) and Assigned(a1) then
          begin
            if PyUnicode_Check(a1) then
              IO.Write(PyUnicode_AsWideString(a1))
            else
              if PyString_Check(a1) then
                IO.Write(PyObjectAsString(a1));
          end;
          Result := ReturnNone;
        end
      else
        begin
          PyErr_BadArgument;
          Result := nil;
        end;
    end;
end;

function pyio_read(self, args : PPyObject) : PPyObject;
var
  txt : AnsiString;
  Widetxt : UnicodeString;
begin
  with GetPythonEngine do
    begin
      if RedirectIO  then
        begin
          txt := '';
          if Assigned(IO) then
            if IO.UnicodeIO then begin
              Widetxt := IO.ReceiveUniData;
              if PyErr_Occurred <> nil then
                Result := nil
              else
                Result := PyUnicode_FromWideString(PWideChar(Widetxt));
            end else begin
              txt := IO.ReceiveData;
              if PyErr_Occurred <> nil then
                Result := nil
              else
                Result := PyString_FromString(PAnsiChar(txt));
            end
          else
            Result := PyString_FromString(PAnsiChar(txt));
        end
      else
        Result := ReturnNone;
    end;
end;

function pyio_SetDelayWrites(self, args : PPyObject) : PPyObject;
var
  val : Integer;
begin
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'i:SetDelayWrites',@val ) <> 0 then
        begin
          if IO <> nil then
            IO.DelayWrites := val <> 0;
          Result := ReturnNone;
        end
      else
        Result := nil;
    end;
end;

function pyio_SetMaxLines(self, args : PPyObject) : PPyObject;
var
  val : Integer;
begin
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'i:SetMaxLines',@val ) <> 0 then
        begin
          if IO <> nil then
            IO .MaxLines := val;
          Result := ReturnNone;
        end
      else
        Result := nil;
    end;
end;

// With no args, it will look at all types
// With args, it will look only at the types listed in the args.

// It returns a list of tuples. Each tuple contains:
// the Type name, the InstanceCount, the CreateHits and the DeleteHits

function pyio_GetTypesStats(self, args : PPyObject) : PPyObject;

  function HandleType( T : TPythonType ) : PPyObject;
  begin
    with GetPythonEngine do
      begin
        Result := PyTuple_New(4);
        PyTuple_SetItem( Result, 0, PyString_FromString(PAnsiChar(T.TypeName)) );
        PyTuple_SetItem( Result, 1, PyInt_FromLong(T.InstanceCount) );
        PyTuple_SetItem( Result, 2, PyInt_FromLong(T.CreateHits) );
        PyTuple_SetItem( Result, 3, PyInt_FromLong(T.DeleteHits) );
      end;
  end;

  function FindType( const TName : AnsiString ) : TPythonType;
  var
    i : Integer;
  begin
    Result := nil;
    with GetPythonEngine do
      for i := 0 to ClientCount - 1 do
        if Clients[i] is TPythonType then
          with TPythonType(Clients[i]) do
            if TypeName = TName then
              begin
                Result := TPythonType(Clients[i]);
                Break;
              end;
  end;

var
  i : Integer;
  T : TPythonType;
  obj : PPyObject;
  str : AnsiString;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(0);
      if PyTuple_Size(args) > 0 then
        for i := 0 to PyTuple_Size(args)-1 do
          begin
            str := AnsiString(PyObjectAsString( PyTuple_GetItem(args, i) ));
            T := FindType( str );
            if Assigned(T) then
              begin
                obj := HandleType( T );
                PyList_Append( Result, obj );
                Py_XDecRef(obj);
              end;
          end
      else
        for i := 0 to ClientCount - 1 do
          if Clients[i] is TPythonType then
            begin
              obj := HandleType( TPythonType(Clients[i]) );
              PyList_Append( Result, obj );
              Py_XDecRef(obj);
            end;
    end;
end;





(*******************************************************)
(**                                                   **)
(**            Global procedures                      **)
(**                                                   **)
(*******************************************************)

function  GetPythonEngine : TPythonEngine;
begin
  if not Assigned( gPythonEngine ) then
    raise Exception.Create( 'No Python engine was created' );
  if not gPythonEngine.Finalizing then
    if not gPythonEngine.Initialized then
      raise Exception.Create( 'The Python engine is not properly initialized' );
  Result := gPythonEngine;
end;

function  PythonOK : Boolean;
begin
  Result := Assigned( gPythonEngine ) and
            (gPythonEngine.Initialized or gPythonEngine.Finalizing);
end;

function IsDelphiObject( obj : PPyObject ) : Boolean;
var
  t : PPyTypeObject;
begin
  Result := False;
  // Here's a simple trick: we compare the object destructor to
  // our special destructor for Delphi objects, or
  // we check if one of the parent types of obj has a Delphi destructor.
  if Assigned(obj) then
  begin
    t := obj^.ob_type;
    while Assigned(t) do
    begin
      if @t^.tp_dealloc = @PyObjectDestructor then
      begin
        Result := True;
        Break;
      end;
      t := t^.tp_base;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Python',[ TPythonEngine, TPythonInputOutput,
                                TPythonType, TPythonModule, TPythonDelphiVar]);
end;

function GetPythonVersionFromDLLName(const DLLFileName : String): String;
begin
  Result := DLLFileName[{$IFDEF MSWINDOWS}7{$ELSE}10{$ENDIF}] + '.' + DLLFileName[{$IFDEF MSWINDOWS}8{$ELSE}11{$ENDIF}];
end;

function PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;
begin
  //(((t)->tp_flags & (f)) != 0)
  Result := (((AType)^.tp_flags and (AFlag)) <> 0);
end;

procedure MaskFPUExceptions(ExceptionsMasked : boolean;
  MatchPythonPrecision : Boolean);
begin
  if ExceptionsMasked then
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
      exOverflow, exUnderflow, exPrecision])
  else
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
{$IFNDEF NEXTGEN}{$WARN SYMBOL_PLATFORM OFF}
  if MatchPythonPrecision then
      SetPrecisionMode(pmDouble)
    else
      SetPrecisionMode(pmExtended);
{$ENDIF !NEXTGEN}{$WARN SYMBOL_PLATFORM ON}
end;

function CleanString(const s : AnsiString; AppendLF : Boolean) : AnsiString;
var
  i : Integer;
begin
  result := s;
  if s = '' then
    Exit;
  i := Pos(AnsiString(CR),s);
  while i > 0 do
    begin
      Delete( result, i, 1 );
      i := PosEx(AnsiString(CR),result, i);
    end;
  if AppendLF and (result[length(result)] <> LF) then
    Result := Result + LF;
end;

function CleanString(const s : string; AppendLF : Boolean) : string;
begin
  Result := AdjustLineBreaks(s, tlbsLF);
  if AppendLF and (result[length(result)] <> LF) then
    Result := Result + LF;
end;

{$IFDEF MSWINDOWS}
function IsPythonVersionRegistered(PythonVersion : string;
  out InstallPath: string; out AllUserInstall: Boolean) : Boolean;
  // Python provides for All user and Current user installations
  // All User installations place the Python DLL in the Windows System directory
  // and write registry info to HKEY_LOCAL_MACHINE
  // Current User installations place the DLL in the install path and
  // the registry info in HKEY_CURRENT_USER.
  // Hence, for Current user installations we need to try and find the install path
  // since it may not be on the system path.

  // The above convension was changed in Python 3.5.  Now even for all user
  // installations the dll is located at the InstallPath.
  // Also from version 3.5 onwards 32 bit version have a suffix -32 e.g. "3.6-32"
  // See also PEP 514

var
  key: string;
  VersionSuffix: string;
  MajorVersion : integer;
  MinorVersion : integer;
begin
  Result := False;
  InstallPath := '';
  AllUserInstall := False;
  MajorVersion := StrToInt(PythonVersion[1]);
  MinorVersion := StrToInt(PythonVersion[3]);
  VersionSuffix := '';
{$IFDEF CPUX86}
  if (MajorVersion > 3) or ((MajorVersion = 3)  and (MinorVersion >= 5)) then
    VersionSuffix := '-32';
{$ENDIF}
  key := Format('\Software\Python\PythonCore\%s%s\InstallPath', [PythonVersion, VersionSuffix]);

  // First try HKEY_CURRENT_USER as per PEP514
  try
    with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(Key, False) then begin
          InstallPath := ReadString('');
          Result := True;
          Exit;
        end;
      finally
        Free;
      end;
  except
  end;

  //Then try for an all user installation
  try
    with TRegistry.Create(KEY_READ and not KEY_NOTIFY) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey(Key, False) then begin
          AllUserInstall := True;
          if (MajorVersion > 3) or ((MajorVersion = 3)  and (MinorVersion >= 5)) then
            InstallPath := ReadString('');
          Result := True;
        end;
      finally
        Free;
      end;
  except
  end;
end;
{$ENDIF}

end.

