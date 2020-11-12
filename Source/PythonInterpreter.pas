{$I Definition.Inc}
unit PythonInterpreter;

interface

uses
  Classes,
  PythonLoader,
  PythonTypes;

//-------------------------------------------------------
//--                                                   --
//--  class:  TPythonInterface derived from TDynamicDll--
//--      This class maps the functions imported       --
//--      from the Python Dll, and adds some           --
//--      Delphi implementations.                      --
//-------------------------------------------------------

type
  TPythonInterface = class(TDynamicDll)
  protected
    FInitialized:    Boolean;
    FFinalizing:     Boolean;
    FMajorVersion:   integer;
    FMinorVersion:   integer;
    FBuiltInModuleName: string;

    procedure AfterLoad; override;
    function  GetQuitMessage : string; override;
    procedure CheckPython;
    function  GetUnicodeTypeSuffix : string;

  public
    // define Python flags. See file pyDebug.h
    Py_DebugFlag: PInteger;
    Py_VerboseFlag: PInteger;
    Py_InteractiveFlag: PInteger;
    Py_OptimizeFlag: PInteger;
    Py_NoSiteFlag: PInteger;
    Py_FrozenFlag: PInteger;
    Py_IgnoreEnvironmentFlag: PInteger;

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
    PyExc_AssertionError: PPPyObject;
    PyExc_EnvironmentError: PPPyObject;
    PyExc_IndentationError: PPPyObject;
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

    PyCode_Type: PPyTypeObject;
    PyType_Type: PPyTypeObject;
    PyCFunction_Type: PPyTypeObject;
    PyComplex_Type: PPyTypeObject;
    PyDict_Type: PPyTypeObject;
    PyFloat_Type: PPyTypeObject;
    PyFrame_Type: PPyTypeObject;
    PyFunction_Type: PPyTypeObject;
    PyList_Type: PPyTypeObject;
    PyLong_Type: PPyTypeObject;
    PyMethod_Type: PPyTypeObject;
    PyModule_Type: PPyTypeObject;
    PyObject_Type: PPyTypeObject;
    PyRange_Type: PPyTypeObject;
    PySlice_Type: PPyTypeObject;
    PyBytes_Type: PPyTypeObject;
    PyTuple_Type: PPyTypeObject;
    PyBaseObject_Type: PPyTypeObject;
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
    PyBool_Type: PPyTypeObject;
    PyEnum_Type: PPyTypeObject;

    Py_GetBuildInfo: function : PAnsiChar; cdecl;
    PyImport_ExecCodeModule: function ( const name : AnsiString; codeobject : PPyObject) : PPyObject; cdecl;
    PyComplex_FromCComplex: function(c: Py_complex):PPyObject; cdecl;
    PyComplex_FromDoubles: function(realv,imag : double):PPyObject; cdecl;
    PyComplex_RealAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_ImagAsDouble: function(op : PPyObject ): double; cdecl;
    PyComplex_AsCComplex: function(op : PPyObject ): Py_complex; cdecl;
    PyCFunction_GetFunction: function(ob : PPyObject): Pointer; cdecl;
    PyCFunction_GetSelf: function(ob : PPyObject): PPyObject; cdecl;
    PyCallable_Check: function(ob	: PPyObject): integer; cdecl;

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
    PyErr_WarnEx:       function (ob: PPyObject; text: PAnsiChar; stack_level: NativeInt): integer; cdecl;
    PyErr_WarnExplicit: function (ob: PPyObject; text: PAnsiChar; filename: PAnsiChar; lineno: integer; module: PAnsiChar; registry: PPyObject): integer; cdecl;
    PyImport_GetModuleDict: function: PPyObject; cdecl;

    PyArg_Parse:        TPyArg_Parse;
    PyArg_ParseTuple:   TPyArg_Parse;
    PyArg_ParseTupleAndKeywords:   TPyArg_ParseTupleAndKeywords;
    Py_BuildValue:      TPy_BuildValue;

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
    PyDict_Update:      function (a: PPyObject; b: PPyObject):Integer; cdecl;
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
    PyBytes_AsString:    function( ob: PPyObject): PAnsiChar; cdecl;
    PyBytes_AsStringAndSize: function( ob: PPyObject; var buffer: PAnsiChar; var size: NativeInt): integer; cdecl;
    PySys_SetArgv:        procedure( argc: Integer; argv: PPWideChar); cdecl;

    PyCFunction_NewEx: function(md:PPyMethodDef;self, ob:PPyObject):PPyObject; cdecl;
// Removed.  Use PyEval_CallObjectWithKeywords with third argument nil
//    PyEval_CallObject: function(callable_obj, args:PPyObject):PPyObject; cdecl;
    PyEval_CallObjectWithKeywords:function (callable_obj, args, kw:PPyObject):PPyObject; cdecl;
    PyEval_GetFrame:function :PPyObject; cdecl;
    PyEval_GetGlobals:function :PPyObject; cdecl;
    PyEval_GetLocals:function :PPyObject; cdecl;

    PyEval_InitThreads:procedure; cdecl;
    PyEval_RestoreThread:procedure( tstate: PPyThreadState); cdecl;
    PyEval_SaveThread:function :PPyThreadState; cdecl;

    PyFile_GetLine:function (ob:PPyObject;i:integer):PPyObject; cdecl;
    PyFile_WriteObject:function (ob1,ob2:PPyObject;i:integer):integer; cdecl;
    PyFile_WriteString:procedure(s:PAnsiChar;ob:PPyObject); cdecl;
    PyFloat_AsDouble:function (ob:PPyObject):DOUBLE; cdecl;
    PyFloat_FromDouble:function (db:double):PPyObject; cdecl;
    PyFloat_FromString:function (str:PPyObject):PPyObject; cdecl;
    PyFunction_GetCode:function (ob:PPyObject):PPyObject; cdecl;
    PyFunction_GetGlobals:function (ob:PPyObject):PPyObject; cdecl;
    PyFunction_New:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyImport_AddModule:function (name:PAnsiChar):PPyObject; cdecl;
    PyImport_GetMagicNumber:function :LongInt; cdecl;
    PyImport_ImportFrozenModule:function (key:PAnsiChar):integer; cdecl;
    PyImport_ImportModule:function (name:PAnsiChar):PPyObject; cdecl;
    PyImport_Import:function (name:PPyObject):PPyObject; cdecl;
    PyImport_ReloadModule:function (ob:PPyObject):PPyObject; cdecl;
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
    PyLong_AsLong:function (ob:PPyObject):LongInt; cdecl;
    PyLong_FromDouble:function (db:double):PPyObject; cdecl;
    PyLong_FromLong:function (l:LongInt):PPyObject; cdecl;
    PyLong_FromString:function (pc:PAnsiChar;var ppc:PAnsiChar;i:integer):PPyObject; cdecl;
    PyLong_FromUnsignedLong:function(val:LongWord): PPyObject; cdecl;
    PyLong_AsUnsignedLong:function(ob:PPyObject): LongWord; cdecl;
    PyLong_FromUnicode:function(ob:PPyObject; a, b : integer): PPyObject; cdecl;
    PyLong_FromLongLong:function(val:Int64): PPyObject; cdecl;
    PyLong_FromUnsignedLongLong:function(val:UInt64) : PPyObject; cdecl;
    PyLong_AsLongLong:function(ob:PPyObject): Int64; cdecl;
    PyLong_FromVoidPtr:function(p: Pointer): PPyObject; cdecl;
    PyMapping_Check:function (ob:PPyObject):integer; cdecl;
    PyMapping_GetItemString:function (ob:PPyObject;key:PAnsiChar):PPyObject; cdecl;
    PyMapping_HasKey:function (ob,key:PPyObject):integer; cdecl;
    PyMapping_HasKeyString:function (ob:PPyObject;key:PAnsiChar):integer; cdecl;
    PyMapping_Length:function (ob:PPyObject):NativeInt; cdecl;
    PyMapping_SetItemString:function (ob:PPyObject; key:PAnsiChar; value:PPyObject):integer; cdecl;
    PyMethod_Function:function (ob:PPyObject):PPyObject; cdecl;
    PyMethod_New:function (ob1,ob2,ob3:PPyObject):PPyObject; cdecl;
    PyMethod_Self:function (ob:PPyObject):PPyObject; cdecl;
    PyModule_GetName:function (ob:PPyObject):PAnsiChar; cdecl;
    PyModule_New:function (key:PAnsiChar):PPyObject; cdecl;
    PyNumber_Absolute:function (ob:PPyObject):PPyObject; cdecl;
    PyNumber_Add:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_And:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Check:function (ob:PPyObject):integer; cdecl;
    PyNumber_FloorDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_TrueDivide:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Divmod:function (ob1,ob2:PPyObject):PPyObject; cdecl;
    PyNumber_Float:function (ob:PPyObject):PPyObject; cdecl;
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
    PySequence_List:function (o:PPyObject):PPyObject; cdecl;
    PySeqIter_New: function(obj : PPyObject) : PPyObject; cdecl;
    PySlice_GetIndices:function (ob:PPySliceObject;length:NativeInt;var start,stop,step:NativeInt):integer; cdecl;
    PySlice_GetIndicesEx:function (ob:PPySliceObject;length:NativeInt;var start,stop,step,slicelength:NativeInt):integer; cdecl;
    PySlice_New:function (start,stop,step:PPyObject):PPyObject; cdecl;
    PyBytes_Concat:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
    PyBytes_ConcatAndDel:procedure(var ob1:PPyObject;ob2:PPyObject); cdecl;
    PyBytes_FromString:function (s:PAnsiChar):PPyObject; cdecl;
    PyBytes_FromStringAndSize:function (s:PAnsiChar;i:NativeInt):PPyObject; cdecl;
    PyBytes_Size:function (ob:PPyObject):NativeInt; cdecl;
    PyBytes_DecodeEscape:function(s:PAnsiChar; len:NativeInt; errors:PAnsiChar; unicode:NativeInt; recode_encoding:PAnsiChar):PPyObject; cdecl;
    PyBytes_Repr:function(ob:PPyObject; smartquotes:integer):PPyObject; cdecl;
    PySys_GetObject:function (s:PAnsiChar):PPyObject; cdecl;
    PySys_SetObject:function (s:PAnsiChar;ob:PPyObject):integer; cdecl;
    PySys_SetPath:procedure(path:PAnsiChar); cdecl;
    PyTraceBack_Here:function (p:pointer):integer; cdecl;
    PyTraceBack_Print:function (ob1,ob2:PPyObject):integer; cdecl;
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
    PyUnicode_FromString:function (s:PAnsiChar):PPyObject; cdecl;
    PyUnicode_FromStringAndSize:function (s:PAnsiChar;i:NativeInt):PPyObject; cdecl;
    PyUnicode_AsWideChar:function (unicode: PPyObject; w:PWideChar; size:NativeInt):integer; cdecl;
    PyUnicode_AsUTF8:function (unicode: PPyObject):PAnsiChar; cdecl;
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
    Py_CompileStringExFlags:function (s1,s2:PAnsiChar;i:integer;flags:PPyCompilerFlags;optimize:integer):PPyObject; cdecl;
    Py_FatalError:procedure(s:PAnsiChar); cdecl;
    _PyObject_New:function (obt:PPyTypeObject;ob:PPyObject):PPyObject; cdecl;
    _PyBytes_Resize:function (var ob:PPyObject;i:NativeInt):integer; cdecl;
    Py_Finalize                     : procedure; cdecl;
    PyErr_ExceptionMatches          : function ( exc : PPyObject) : Integer; cdecl;
    PyErr_GivenExceptionMatches     : function ( raised_exc, exc : PPyObject) : Integer; cdecl;
    PyEval_EvalCode                 : function ( co : PPyObject; globals, locals : PPyObject) : PPyObject; cdecl;
    Py_GetVersion                   : function : PAnsiChar; cdecl;
    Py_GetCopyright                 : function : PAnsiChar; cdecl;
    Py_GetExecPrefix                : function : PAnsiChar; cdecl;
    Py_GetPath                      : function : PAnsiChar; cdecl;
    Py_SetPythonHome                : procedure (home : PWideChar); cdecl;
    Py_GetPythonHome                : function : PWideChar; cdecl;
    Py_GetPrefix                    : function : PAnsiChar; cdecl;
    Py_GetProgramName               : function : PAnsiChar; cdecl;

    PyParser_SimpleParseStringFlags : function ( str : PAnsiChar; start, flags : Integer) : PNode; cdecl;
    PyNode_Free                     : procedure( n : PNode ); cdecl;
    PyErr_NewException              : function ( name : PAnsiChar; base, dict : PPyObject ) : PPyObject; cdecl;
    PyMem_Malloc                    : function ( size : NativeInt ) : Pointer;

    Py_SetProgramName               : procedure( name: PWideChar); cdecl;
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

    // Not exported in Python 3.8 and implemented as functions - this has been fixed
    // TODO - deal with the following:
    // the PyParser_* functions are deprecated in python 3.9 and will be removed in
    // Python 3.10
    function PyParser_SimpleParseString( str : PAnsiChar; start : Integer) : PNode; cdecl;
    function Py_CompileString( s1,s2:PAnsiChar;i:integer) : PPyObject; cdecl;

    // functions redefined in Delphi
    class procedure Py_INCREF(op: PPyObject); static; inline;
    class procedure Py_DECREF(op: PPyObject); static; inline;
    class procedure Py_XINCREF(op: PPyObject); static; inline;
    class procedure Py_XDECREF(op: PPyObject); static; inline;
    (* Safely decref `op` and set `op` to NULL, especially useful in tp_clear
     * and tp_dealloc implementations.
     *
     * Note that "the obvious" code can be deadly:
     *
     *     Py_XDECREF(op);
     *     op = NULL;
     *
     * Typically, `op` is something like self->containee, and `self` is done
     * using its `containee` member.  In the code sequence above, suppose
     * `containee` is non-NULL with a refcount of 1.  Its refcount falls to
     * 0 on the first line, which can trigger an arbitrary amount of code,
     * possibly including finalizers (like __del__ methods or weakref callbacks)
     * coded in Python, which in turn can release the GIL and allow other threads
     * to run, etc.  Such code may even invoke methods of `self` again, or cause
     * cyclic gc to trigger, but-- oops! --self->containee still points to the
     * object being torn down, and it may be in an insane state while being torn
     * down.  This has in fact been a rich historic source of miserable (rare &
     * hard-to-diagnose) segfaulting (and other) bugs.
     *
     * The safe way is:
     *
     *      Py_CLEAR(op);
     *
     * That arranges to set `op` to NULL _before_ decref'ing, so that any code
     * triggered as a side-effect of `op` getting torn down no longer believes
     * `op` points to a valid object.
     *
     * There are cases where it's safe to use the naive code, but they're brittle.
     * For example, if `op` points to a Python integer, you know that destroying
     * one of those can't cause problems -- but in part that relies on that
     * Python integers aren't currently weakly referencable.  Best practice is
     * to use Py_CLEAR() even if you can't think of a reason for why you need to.
     *)
    class procedure Py_CLEAR(var op: PPyObject); static; inline;

    function PyBytes_Check( obj : PPyObject ) : Boolean;
    function PyBytes_CheckExact( obj : PPyObject ) : Boolean;
    function PyFloat_Check( obj : PPyObject ) : Boolean;
    function PyFloat_CheckExact( obj : PPyObject ) : Boolean;
    function PyLong_Check( obj : PPyObject ) : Boolean;
    function PyLong_CheckExact( obj : PPyObject ) : Boolean;
    function PyTuple_Check( obj : PPyObject ) : Boolean;
    function PyTuple_CheckExact( obj : PPyObject ) : Boolean;
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
    function PyEnum_Check( obj : PPyObject ) : Boolean;
    function PyObject_TypeCheck(obj:PPyObject; t:PPyTypeObject) : Boolean;
    function Py_InitModule( const md : PyModuleDef) : PPyObject;

    // Constructors & Destructors
    constructor Create(AOwner: TComponent); override;

    // Public methods
    procedure MapDll;

    // Public properties
    property Initialized : Boolean read FInitialized;
    property Finalizing : Boolean read FFinalizing;
    property MajorVersion : integer read FMajorVersion;
    property MinorVersion : integer read FMinorVersion;
    property BuiltInModuleName: string read FBuiltInModuleName write FBuiltInModuleName;
  end;

  function  PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;

implementation

uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows, Registry
  {$ENDIF}
  , PythonKnownVersions, PythonConsts, PythonEngine;

  { TODO : PythonEngine reference only used because of Py_InitModule - Check if necessary }

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
  FInitialized := False;
  i := COMPILED_FOR_PYTHON_VERSION_INDEX;
  DllName     := PYTHON_KNOWN_VERSIONS[i].DllName;
  FAPIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
  FRegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
  FAutoUnload := True;
end;

procedure TPythonInterface.AfterLoad;
begin
  inherited;
  FMajorVersion := StrToInt(DLLName[7 {$IFNDEF MSWINDOWS}+3{$ENDIF}]);
  FMinorVersion := StrToInt(DLLName[8{$IFNDEF MSWINDOWS}+4{$ENDIF}]);

  FBuiltInModuleName := 'builtins';

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

function  TPythonInterface.GetQuitMessage : string;
begin
  Result := Format( 'Python could not be properly initialized. We must quit.', [DllName]);
end;

procedure TPythonInterface.CheckPython;
begin
  if not Initialized then
    raise Exception.Create('Python is not properly initialized' );
end;

function  TPythonInterface.GetUnicodeTypeSuffix : string;
begin
  if (fMajorVersion > 3) or ((fMajorVersion = 3) and (fMinorVersion >= 3)) then
    Result := ''
  else if APIVersion >= 1011 then
    Result :=
      {$IF DEFINED(MSWINDOWS) or DEFINED(DARWIN) or DEFINED(SOLARIS)}
        'UCS2'
      {$ELSE}
        'UCS4'
      {$IFEND}
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

  Py_IgnoreEnvironmentFlag   := Import('Py_IgnoreEnvironmentFlag');

  Py_None                    := Import('_Py_NoneStruct');
  Py_Ellipsis                := Import('_Py_EllipsisObject');
  Py_False                   := Import('_Py_FalseStruct');
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
  PyCode_Type                := Import('PyCode_Type');
  PyComplex_Type             := Import('PyComplex_Type');
  PyDict_Type                := Import('PyDict_Type');
  PyFloat_Type               := Import('PyFloat_Type');
  PyFrame_Type               := Import('PyFrame_Type');
  PyFunction_Type            := Import('PyFunction_Type');
  PyList_Type                := Import('PyList_Type');
  PyLong_Type                := Import('PyLong_Type');
  PyMethod_Type              := Import('PyMethod_Type');
  PyModule_Type              := Import('PyModule_Type');
  PyObject_Type              := Import('PyObject_Type');
  PyRange_Type               := Import('PyRange_Type');
  PySlice_Type               := Import('PySlice_Type');
  PyBytes_Type               := Import('PyBytes_Type');
  PyTuple_Type               := Import('PyTuple_Type');
  PyUnicode_Type             := Import('PyUnicode_Type');
  PyBaseObject_Type          := Import('PyBaseObject_Type');
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
  PyBool_Type                := Import('PyBool_Type');
  PyEnum_Type                := Import('PyEnum_Type');

  PyComplex_FromCComplex    := Import('PyComplex_FromCComplex');
  PyComplex_FromDoubles     := Import('PyComplex_FromDoubles');
  PyComplex_RealAsDouble    := Import('PyComplex_RealAsDouble');
  PyComplex_ImagAsDouble    := Import('PyComplex_ImagAsDouble');
  PyComplex_AsCComplex      := Import('PyComplex_AsCComplex');
  PyCFunction_GetFunction   := Import('PyCFunction_GetFunction');
  PyCFunction_GetSelf       := Import('PyCFunction_GetSelf');
  PyCallable_Check          := Import('PyCallable_Check');
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
  PyDict_New                := Import('PyDict_New');
  PyDict_Update             := Import('PyDict_Update');
  PyDict_SetItemString      := Import('PyDict_SetItemString');
  PyDictProxy_New           := Import('PyDictProxy_New');
  PyModule_Create2          := Import('PyModule_Create2');
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
  PyErr_WarnEx              := Import('PyErr_WarnEx');
  PyErr_WarnExplicit        := Import('PyErr_WarnExplicit');
  PyEval_GetBuiltins        := Import('PyEval_GetBuiltins');
  PyImport_GetModuleDict    := Import('PyImport_GetModuleDict');
  PyArg_Parse               := Import('PyArg_Parse');
  PyArg_ParseTuple          := Import('PyArg_ParseTuple');
  PyArg_ParseTupleAndKeywords := Import('PyArg_ParseTupleAndKeywords');
  Py_BuildValue             := Import('Py_BuildValue');
  Py_Initialize             := Import('Py_Initialize');
  PyModule_GetDict          := Import('PyModule_GetDict');
  PyObject_Str              := Import('PyObject_Str');
  PyRun_String              := Import('PyRun_String');
  PyRun_SimpleString        := Import('PyRun_SimpleString');
  PyDict_GetItemString      := Import('PyDict_GetItemString');
  PySys_SetArgv             := Import('PySys_SetArgv');
  Py_Exit                   := Import('Py_Exit');

  PyCFunction_NewEx           := Import('PyCFunction_NewEx');

  PyEval_CallObjectWithKeywords:= Import('PyEval_CallObjectWithKeywords');
  PyEval_GetFrame           := Import('PyEval_GetFrame');
  PyEval_GetGlobals         := Import('PyEval_GetGlobals');
  PyEval_GetLocals          := Import('PyEval_GetLocals');
  PyEval_InitThreads        := Import('PyEval_InitThreads');
  PyEval_RestoreThread      := Import('PyEval_RestoreThread');
  PyEval_SaveThread         := Import('PyEval_SaveThread');
  PyFile_GetLine            := Import('PyFile_GetLine');
  PyFile_WriteObject        := Import('PyFile_WriteObject');
  PyFile_WriteString        := Import('PyFile_WriteString');
  PyFloat_AsDouble          := Import('PyFloat_AsDouble');
  PyFloat_FromDouble        := Import('PyFloat_FromDouble');
  PyFloat_FromString        := Import('PyFloat_FromString');
  PyFunction_GetCode        := Import('PyFunction_GetCode');
  PyFunction_GetGlobals     := Import('PyFunction_GetGlobals');
  PyFunction_New            := Import('PyFunction_New');
  PyImport_AddModule        := Import('PyImport_AddModule');
  PyImport_GetMagicNumber   := Import('PyImport_GetMagicNumber');
  PyImport_ImportFrozenModule:= Import('PyImport_ImportFrozenModule');
  PyImport_ImportModule     := Import('PyImport_ImportModule');
  PyImport_Import           := Import('PyImport_Import');
  PyImport_ReloadModule     := Import('PyImport_ReloadModule');
  PyLong_AsLong             := Import('PyLong_AsLong');
  PyList_Append             := Import('PyList_Append');
  PyList_AsTuple            := Import('PyList_AsTuple');
  PyList_GetItem            := Import('PyList_GetItem');
  PyList_GetSlice           := Import('PyList_GetSlice');
  PyList_Insert             := Import('PyList_Insert');
  PyList_New                := Import('PyList_New');
  PyList_Reverse            := Import('PyList_Reverse');
  PyList_SetItem            := Import('PyList_SetItem');
  PyList_SetSlice           := Import('PyList_SetSlice');
  PyList_Size               := Import('PyList_Size');
  PyList_Sort               := Import('PyList_Sort');
  PyLong_AsDouble           := Import('PyLong_AsDouble');
  PyLong_AsLong             := Import('PyLong_AsLong');
  PyLong_FromDouble         := Import('PyLong_FromDouble');
  PyLong_FromLong           := Import('PyLong_FromLong');
  PyLong_FromString         := Import('PyLong_FromString');
  PyLong_FromString         := Import('PyLong_FromString');
  PyLong_FromUnsignedLong   := Import('PyLong_FromUnsignedLong');
  PyLong_AsUnsignedLong     := Import('PyLong_AsUnsignedLong');
  PyLong_FromUnicode        := Import('PyLong_FromUnicode');
  PyLong_FromLongLong       := Import('PyLong_FromLongLong');
  PyLong_FromUnsignedLongLong := Import('PyLong_FromUnsignedLongLong');
  PyLong_AsLongLong         := Import('PyLong_AsLongLong');
  PyLong_FromVoidPtr        := Import('PyLong_FromVoidPtr');
  PyMapping_Check           := Import('PyMapping_Check');
  PyMapping_GetItemString   := Import('PyMapping_GetItemString');
  PyMapping_HasKey          := Import('PyMapping_HasKey');
  PyMapping_HasKeyString    := Import('PyMapping_HasKeyString');
  PyMapping_Length          := Import('PyMapping_Length');
  PyMapping_SetItemString   := Import('PyMapping_SetItemString');
  PyMethod_Function         := Import('PyMethod_Function');
  PyMethod_New              := Import('PyMethod_New');
  PyMethod_Self             := Import('PyMethod_Self');
  PyModule_GetName          := Import('PyModule_GetName');
  PyModule_New              := Import('PyModule_New');
  PyNumber_Absolute         := Import('PyNumber_Absolute');
  PyNumber_Add              := Import('PyNumber_Add');
  PyNumber_And              := Import('PyNumber_And');
  PyNumber_Check            := Import('PyNumber_Check');
  PyNumber_FloorDivide      := Import('PyNumber_FloorDivide');
  PyNumber_TrueDivide       := Import('PyNumber_TrueDivide');
  PyNumber_Divmod           := Import('PyNumber_Divmod');
  PyNumber_Float            := Import('PyNumber_Float');
  PyNumber_Invert           := Import('PyNumber_Invert');
  PyNumber_Long             := Import('PyNumber_Long');
  PyNumber_Lshift           := Import('PyNumber_Lshift');
  PyNumber_Multiply         := Import('PyNumber_Multiply');
  PyNumber_Negative         := Import('PyNumber_Negative');
  PyNumber_Or               := Import('PyNumber_Or');
  PyNumber_Positive         := Import('PyNumber_Positive');
  PyNumber_Power            := Import('PyNumber_Power');
  PyNumber_Remainder        := Import('PyNumber_Remainder');
  PyNumber_Rshift           := Import('PyNumber_Rshift');
  PyNumber_Subtract         := Import('PyNumber_Subtract');
  PyNumber_Xor              := Import('PyNumber_Xor');
  PyOS_InitInterrupts       := Import('PyOS_InitInterrupts');
  PyOS_InterruptOccurred    := Import('PyOS_InterruptOccurred');
  PyObject_CallObject       := Import('PyObject_CallObject');
  PyObject_CallMethod       := Import('PyObject_CallMethod');
  PyObject_RichCompare      := Import('PyObject_RichCompare');
  PyObject_RichCompareBool  := Import('PyObject_RichCompareBool');
  PyObject_GetAttr          := Import('PyObject_GetAttr');
  PyObject_GetAttrString    := Import('PyObject_GetAttrString');
  PyObject_GetItem          := Import('PyObject_GetItem');
  PyObject_DelItem          := Import('PyObject_DelItem');
  PyObject_HasAttrString    := Import('PyObject_HasAttrString');
  PyObject_Hash             := Import('PyObject_Hash');
  PyObject_IsTrue           := Import('PyObject_IsTrue');
  PyObject_Length           := Import('PyObject_Length');
  PyObject_Repr             := Import('PyObject_Repr');
  PyObject_SetAttr          := Import('PyObject_SetAttr');
  PyObject_SetAttrString    := Import('PyObject_SetAttrString');
  PyObject_SetItem          := Import('PyObject_SetItem');
  PyObject_Init             := Import('PyObject_Init');
  PyObject_InitVar          := Import('PyObject_InitVar');
  PyObject_New              := Import('_PyObject_New');
  PyObject_NewVar           := Import('_PyObject_NewVar');
  PyObject_Free             := Import('PyObject_Free');
  PyObject_GetIter          := Import('PyObject_GetIter');
  PyIter_Next               := Import('PyIter_Next');
  PyObject_IsInstance       := Import('PyObject_IsInstance');
  PyObject_IsSubclass       := Import('PyObject_IsSubclass');
  PyObject_Call             := Import('PyObject_Call');
  PyObject_GenericGetAttr   := Import('PyObject_GenericGetAttr');
  PyObject_GenericSetAttr   := Import('PyObject_GenericSetAttr');
  PyObject_GC_Malloc        := Import('_PyObject_GC_Malloc');
  PyObject_GC_New           := Import('_PyObject_GC_New');
  PyObject_GC_NewVar        := Import('_PyObject_GC_NewVar');
  PyObject_GC_Resize        := Import('_PyObject_GC_Resize');
  PyObject_GC_Del           := Import('PyObject_GC_Del');
  PyObject_GC_Track         := Import('PyObject_GC_Track');
  PyObject_GC_UnTrack       := Import('PyObject_GC_UnTrack');
  PySequence_Check           := Import('PySequence_Check');
  PySequence_Concat          := Import('PySequence_Concat');
  PySequence_Count           := Import('PySequence_Count');
  PySequence_GetItem         := Import('PySequence_GetItem');
  PySequence_GetSlice        := Import('PySequence_GetSlice');
  PySequence_In              := Import('PySequence_In');
  PySequence_Index           := Import('PySequence_Index');
  PySequence_Length          := Import('PySequence_Length');
  PySequence_Repeat          := Import('PySequence_Repeat');
  PySequence_SetItem         := Import('PySequence_SetItem');
  PySequence_SetSlice        := Import('PySequence_SetSlice');
  PySequence_DelSlice        := Import('PySequence_DelSlice');
  PySequence_Tuple           := Import('PySequence_Tuple');
  PySequence_Contains        := Import('PySequence_Contains');
  PySequence_List            := Import('PySequence_List');
  PySlice_GetIndices         := Import('PySlice_GetIndices');
  PySeqIter_New              := Import('PySeqIter_New');
  PySlice_GetIndicesEx       := Import('PySlice_GetIndicesEx');
  PySlice_New                := Import('PySlice_New');
  PyBytes_AsString           := Import('PyBytes_AsString');
  PyBytes_AsStringAndSize    := Import('PyBytes_AsStringAndSize');
  PyBytes_Concat              := Import('PyBytes_Concat');
  PyBytes_ConcatAndDel        := Import('PyBytes_ConcatAndDel');
  PyBytes_FromString          := Import('PyBytes_FromString');
  PyBytes_FromStringAndSize   := Import('PyBytes_FromStringAndSize');
  PyBytes_Size                := Import('PyBytes_Size');
  PyBytes_DecodeEscape        := Import('PyBytes_DecodeEscape');
  PyBytes_Repr                := Import('PyBytes_Repr');
  _PyBytes_Resize             := Import('_PyBytes_Resize');
  PySys_GetObject             := Import('PySys_GetObject');
  PySys_SetObject             := Import('PySys_SetObject');
  PySys_SetPath               := Import('PySys_SetPath');
  PyTraceBack_Here            := Import('PyTraceBack_Here');
  PyTraceBack_Print           := Import('PyTraceBack_Print');
  PyTuple_GetItem             := Import('PyTuple_GetItem');
  PyTuple_GetSlice            := Import('PyTuple_GetSlice');
  PyTuple_New                 := Import('PyTuple_New');
  PyTuple_SetItem             := Import('PyTuple_SetItem');
  PyTuple_Size                := Import('PyTuple_Size');
  PyType_IsSubtype            := Import('PyType_IsSubtype');
  PyType_GenericAlloc         := Import('PyType_GenericAlloc');
  PyType_GenericNew           := Import('PyType_GenericNew');
  PyType_Ready                := Import('PyType_Ready');
  PyUnicode_FromWideChar      := Import(AnsiString(Format('PyUnicode%s_FromWideChar',[UnicodeSuffix])));
  PyUnicode_FromString        := Import(AnsiString(Format('PyUnicode%s_FromString',[UnicodeSuffix])));
  PyUnicode_FromStringAndSize := Import(AnsiString(Format('PyUnicode%s_FromStringAndSize',[UnicodeSuffix])));
  PyUnicode_AsWideChar        := Import(AnsiString(Format('PyUnicode%s_AsWideChar',[UnicodeSuffix])));
  PyUnicode_AsUTF8            := Import(AnsiString(Format('PyUnicode%s_AsUTF8',[UnicodeSuffix])));
  PyUnicode_Decode            := Import(AnsiString(Format('PyUnicode%s_Decode',[UnicodeSuffix])));
  PyUnicode_AsEncodedString   := Import(AnsiString(Format('PyUnicode%s_AsEncodedString',[UnicodeSuffix])));
  PyUnicode_FromOrdinal       := Import(AnsiString(Format('PyUnicode%s_FromOrdinal',[UnicodeSuffix])));
  PyUnicode_GetSize           := Import(AnsiString(Format('PyUnicode%s_GetSize',[UnicodeSuffix])));
  PyWeakref_GetObject         := Import('PyWeakref_GetObject');
  PyWeakref_NewProxy          := Import('PyWeakref_NewProxy');
  PyWeakref_NewRef            := Import('PyWeakref_NewRef');
  PyWrapper_New               := Import('PyWrapper_New');
  PyBool_FromLong             := Import('PyBool_FromLong');
  PyThreadState_SetAsyncExc   := Import('PyThreadState_SetAsyncExc');
  Py_AtExit                   := Import('Py_AtExit');
  Py_FatalError               := Import('Py_FatalError');
  Py_CompileStringExFlags     := Import('Py_CompileStringExFlags');
  _PyObject_New               := Import('_PyObject_New');
  Py_Finalize                 := Import('Py_Finalize');
  PyImport_ExecCodeModule     := Import('PyImport_ExecCodeModule');
  PyErr_ExceptionMatches      := Import('PyErr_ExceptionMatches');
  PyErr_GivenExceptionMatches := Import('PyErr_GivenExceptionMatches');
  PyEval_EvalCode             := Import('PyEval_EvalCode');
  Py_GetVersion               := Import('Py_GetVersion');
  Py_GetCopyright             := Import('Py_GetCopyright');
  Py_GetExecPrefix            := Import('Py_GetExecPrefix');
  Py_GetPath                  := Import('Py_GetPath');
  Py_SetPythonHome            := Import('Py_SetPythonHome');
  Py_GetPythonHome            := Import('Py_GetPythonHome');
  Py_GetPrefix                := Import('Py_GetPrefix');
  Py_GetProgramName           := Import('Py_GetProgramName');
  PyParser_SimpleParseStringFlags := Import('PyParser_SimpleParseStringFlags');
  PyNode_Free                 := Import('PyNode_Free');
  PyErr_NewException          := Import('PyErr_NewException');
  try
    PyMem_Malloc := Import ('PyMem_Malloc');
  except
  end;
  Py_SetProgramName        := Import('Py_SetProgramName');
  Py_IsInitialized         := Import('Py_IsInitialized');
  Py_GetProgramFullPath    := Import('Py_GetProgramFullPath');
  Py_GetBuildInfo          := Import('Py_GetBuildInfo');
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

function TPythonInterface.Py_CompileString(s1,s2:PAnsiChar;i:integer):PPyObject;
begin
  Result := Py_CompileStringExFlags(s1, s2, i, nil, -1);
end;

function TPythonInterface.PyParser_SimpleParseString( str : PAnsiChar; start : integer) : PNode; cdecl;
begin
  Result := PyParser_SimpleParseStringFlags(str, start, 0);
end;

class procedure TPythonInterface.Py_INCREF(op: PPyObject);
begin
  Inc(op^.ob_refcnt);
end;

class procedure TPythonInterface.Py_DECREF(op: PPyObject);
begin
  with op^ do begin
    Dec(ob_refcnt);
    if ob_refcnt = 0 then begin
      ob_type^.tp_dealloc(op);
    end;
  end;
end;

class procedure TPythonInterface.Py_XINCREF(op: PPyObject);
begin
  if op <> nil then Py_INCREF(op);
end;

class procedure TPythonInterface.Py_XDECREF(op: PPyObject);
begin
  if op <> nil then Py_DECREF(op);
end;


class procedure TPythonInterface.Py_CLEAR(var op: PPyObject);
Var
  _py_tmp : PPyObject;
begin
  _py_tmp := op;
  if _py_tmp <> nil then
  begin
    op := nil;
    Py_DECREF(_py_tmp);
  end;
end;

function TPythonInterface.PyBytes_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyBytes_Type);
end;

function TPythonInterface.PyBytes_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyBytes_Type));
end;

function TPythonInterface.PyFloat_Check( obj : PPyObject ) : Boolean;
begin
  Result := PyObject_TypeCheck(obj, PyFloat_Type);
end;

function TPythonInterface.PyFloat_CheckExact(obj: PPyObject): Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(PyFloat_Type));
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

function TPythonInterface.PyClass_Check( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (PyObject_IsInstance(obj, PPyObject(PyType_Type)) <> 0);
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

function TPythonInterface.PyIter_Check(obj: PPyObject): Boolean;
begin
 Result := Assigned(obj) and Assigned(obj^.ob_type^.tp_iternext);
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

function TPythonInterface.Py_InitModule(const md: PyModuleDef): PPyObject;
Var
  modules  : PPyObject;
begin
  CheckPython;
  Result:= PyModule_Create2(@md, APIVersion);

  { TODO : Verify if it is really necessary make ref to GetPythonEngine or Self is last }

  if not Assigned(Result) then
    GetPythonEngine.CheckError;
  // To emulate Py_InitModule4 we need to add the module to sys.modules
  modules := PyImport_GetModuleDict;
  if PyDict_SetItemString(modules, md.m_name, Result) <> 0 then
    GetPythonEngine.CheckError;
end;

function PyType_HasFeature(AType : PPyTypeObject; AFlag : Integer) : Boolean;
begin
  //(((t)->tp_flags & (f)) != 0)
  Result := (((AType)^.tp_flags and (AFlag)) <> 0);
end;

end.
