{$I Definition.Inc}

unit VarPyth;

(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'VarPyth'          Copyright (c) 2001                    *)
(*                                                                        *)
(* Version: 1.0                     Morgan Martinet                       *)
(* Sub-Version: 0.7                 4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality:  This allows you to use Python objects like COM        *)
(*                  automation objects, inside your Delphi source code.   *)
(*                  This is a replacement of the former PythonAtom.pas    *)
(*                  that uses the new custom variant types introduced     *)
(*                  in Delphi6.                                           *)
(**************************************************************************)
(*  Contributors:                                                         *)
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
(**************************************************************************)

interface

uses
  Variants, PythonEngine;

type
  TSequenceType = (stTuple, stList);

{ Python variant creation utils }
function VarPythonCreate( AObject : PPyObject ) : Variant; overload;
function VarPythonCreate( const AValue : Variant ) : Variant; overload;
function VarPythonCreate( const AValues : array of const; ASequenceType : TSequenceType = stList ) : Variant; overload;
function VarPythonEval( const APythonExpression : AnsiString) : Variant;

{ Python variant helper functions }
function VarPython: TVarType;
function VarIsPython(const AValue: Variant): Boolean;
function VarAsPython(const AValue: Variant): Variant;
function ExtractPythonObjectFrom(const AValue : Variant) : PPyObject;
function VarIsSame(const A, B : Variant ) : Boolean; // checks if 2 variants share the same Python object.
function VarIsSameType(const A, B : Variant ) : Boolean; // checks if 2 variants are of the same Python type.
function VarIsPythonSequence(const AValue: Variant): Boolean;
function VarIsPythonMapping(const AValue: Variant): Boolean;
function VarIsPythonNumber(const AValue: Variant): Boolean;
function VarIsPythonString(const AValue: Variant): Boolean;
function VarIsPythonInteger(const AValue: Variant): Boolean;
function VarIsPythonFloat(const AValue: Variant): Boolean;
function VarIsPythonTuple(const AValue: Variant): Boolean;
function VarIsPythonList(const AValue: Variant): Boolean;
function VarIsPythonDict(const AValue: Variant): Boolean;
function VarIsPythonClass(const AValue: Variant): Boolean;
function VarIsPythonMethod(const AValue: Variant): Boolean;
function VarIsPythonFunction(const AValue: Variant): Boolean;
function VarIsPythonModule(const AValue: Variant): Boolean;
function VarIsPythonCallable(const AValue: Variant): Boolean;
function VarIsPythonIterator(const AValue: Variant): Boolean;
function VarIsPythonUnicode(const AValue: Variant): Boolean;
function VarIsPythonDateTime(const AValue: Variant): Boolean;
function VarIsPythonDate(const AValue: Variant): Boolean;
function VarIsPythonTime(const AValue: Variant): Boolean;
function VarIsPythonDateTimeDelta(const AValue: Variant): Boolean;
function VarIsPythonTZInfo(const AValue: Variant): Boolean;
function VarIsBool(const AValue: Variant): Boolean;
function VarIsEnum(const AValue: Variant): Boolean;
function VarIsInstanceOf(const AInstance, AClass : Variant): Boolean;
function VarIsSubclassOf(const ADerived, AClass : Variant): Boolean;
function VarIsSubtypeOf(const ADerived, AType : Variant): Boolean;
function VarIsNone(const AValue : Variant): Boolean;
function VarIsTrue(const AValue : Variant): Boolean;

function VarModuleHasObject(const AModule : Variant; aObj: AnsiString): Boolean;

function NewPythonList( const ASize : Integer = 0 ): Variant;
function NewPythonTuple( const ASize : Integer ): Variant;
function NewPythonDict: Variant;

// Not really needed since you can assign a PythonVariant to a string anyway
// but it is slightly faster and in some places avoids the declaration of a variable
function VarPythonAsString(AValue : Variant) : string;
{$IFDEF FPC}
// to work around http://mantis.freepascal.org/view.php?id=20849)
function VarPythonToVariant(AValue : Variant): Variant;
{$ENDIF}

function None: Variant;
function Ellipsis: Variant;
function MainModule: Variant; // return the main module that's used for executing a script.
function BuiltinModule: Variant; // return the builtin module
function SysModule: Variant; // return the builtin module 'sys'
function DatetimeModule: Variant; // return the builtin module 'datetime'
function Import( const AModule : AnsiString ): Variant; // import a Python module and return the module object.
function len(const AValue : Variant ): NativeInt; // return the length of a Python collection.
function _type(const AValue : Variant ): Variant; // return the type object of a Python object.
function iter(const AValue : Variant ): Variant; // return an iterator for the container AValue. You can call the 'next' method of the iterator until you catch the EPyStopIteration exception.

type
  TVarPyEnumerator = record
  private
    FIterator: Variant;
    FCurrent: Variant;
  public
    constructor Create(const AValue: Variant);
    function MoveNext: Boolean; inline;
    function GetCurrent: Variant; inline;
    property Current: Variant read GetCurrent;
  end;

  TVarPyEnumerateHelper = record
  private
    FIterable: Variant;
  public
    constructor Create(const AValue: Variant);
    function  GetEnumerator: TVarPyEnumerator;
  end;

function VarPyIterate(const AValue: Variant): TVarPyEnumerateHelper;

implementation

uses
  VarUtils, SysUtils, TypInfo, Classes;

type
  TNamedParamDesc = record
    Index : Integer;
    Name : AnsiString;
  end;
  TNamedParamArray = array of TNamedParamDesc;

{$IFDEF DELPHIXE2_OR_HIGHER}
  {$DEFINE USESYSTEMDISPINVOKE}  //Delphi 2010 DispInvoke is buggy
  {$IF defined(OSX64) or defined(LINUX) or not defined(DELPHI10_4_OR_HIGHER)}
    {$DEFINE PATCHEDSYSTEMDISPINVOKE}  //To correct memory leaks
  {$IFEND}
{$ENDIF}
{$IF DEFINED(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20500)}
  {$DEFINE USESYSTEMDISPINVOKE}
{$IFEND}

  { Python variant type handler }
  TPythonVariantType = class(TInvokeableVariantType, IVarInstanceReference)
  protected
    fNamedParams : TNamedParamArray;
    function LeftPromotion(const V: TVarData; const AOperator: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
    function RightPromotion(const V: TVarData; const AOperator: TVarOp;
      out RequiredVarType: TVarType): Boolean; override;
    function GetInstance(const V: TVarData): TObject;
    function EvalPython(const V: TVarData; const AName: AnsiString;
      const Arguments: TVarDataArray): PPyObject;
    function  VarDataToPythonObject( AVarData : TVarData ) : PPyObject;
    procedure PyhonVarDataCreate( var Dest : TVarData; AObject : PPyObject );
    {$IFNDEF USESYSTEMDISPINVOKE}
    procedure DoDispInvoke(Dest: PVarData; var Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); virtual;
    function GetPropertyWithArg(var Dest: TVarData; const V: TVarData;
      const AName: AnsiString; AArg : TVarData): Boolean; virtual;
    {$ENDIF USESYSTEMDISPINVOKE}
    {$IFNDEF FPC}
    function FixupIdent(const AText: string): string; override;
    {$ENDIF FPC}
    {$IFDEF FPC}
    procedure VarDataClear(var Dest: TVarData);
    procedure VarDataCopyNoInd(var Dest: TVarData; const Source: TVarData);
    procedure VarDataCastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); overload;
    {$ENDIF FPC}
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    procedure BinaryOp(var Left: TVarData; const Right: TVarData;
      const AOperator: TVarOp); override;
    procedure UnaryOp(var Right: TVarData; const AOperator: TVarOp); override;
    function CompareOp(const Left: TVarData; const Right: TVarData;
      const AOperator: TVarOp): Boolean; override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const AName: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const AName: string;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const AName: string): Boolean; override;
    function SetProperty({$IFDEF FPC}var{$ELSE}const{$ENDIF} V: TVarData; const AName: string;
      const Value: TVarData): Boolean; override;
    {$IFDEF DELPHIXE7_OR_HIGHER}
    procedure DispInvoke(Dest: PVarData;
      [Ref] const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);override;
    {$ELSE}
    procedure DispInvoke(Dest: PVarData;
       var Source: TVarData; CallDesc: PCallDesc; Params: Pointer);override;
    {$ENDIF}
  end;

var
  { Python variant type handler instance }
  PythonVariantType: TPythonVariantType = nil;

type
  { Python data that the Python variant points to }
  TPythonData = class(TObject)
  private
    fPyObject: PPyObject;
    function GetAsString: string;
    procedure SetPyObject(const Value: PPyObject);
    function GetAsVariant: Variant;
    function GetAsWideString: UnicodeString;
    function GetAsAnsiString: AnsiString;
  public
    constructor Create(AObject : PPyObject);
    destructor Destroy; override;

    // query state
    function IsNone : Boolean;

    // non-destructive operations
    function Equal(const Right: TPythonData): Boolean;
    function LessThan(const Right: TPythonData): Boolean;
    function LessOrEqualThan(const Right: TPythonData): Boolean;
    function GreaterThan(const Right: TPythonData): Boolean;
    function GreaterOrEqualThan(const Right: TPythonData): Boolean;

    // destructive operations
    procedure DoAdd(const Right: TPythonData);
    procedure DoSubtract(const Right: TPythonData);
    procedure DoMultiply(const Right: TPythonData);
    procedure DoDivide(const Right: TPythonData);
    procedure DoIntDivide(const Right: TPythonData);
    procedure DoModulus(const Right: TPythonData);
    procedure DoShiftLeft(const Right: TPythonData);
    procedure DoShiftRight(const Right: TPythonData);
    procedure DoAnd(const Right: TPythonData);
    procedure DoOr(const Right: TPythonData);
    procedure DoXor(const Right: TPythonData);
    procedure DoNegate;
    procedure DoNot;

    // conversion
    property AsString: string read GetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsVariant: Variant read GetAsVariant;
    property AsWideString: UnicodeString read GetAsWideString;

    // data
    property PyObject : PPyObject read fPyObject write SetPyObject;
  end;

type
  { Helper record that helps crack open TVarData }
  TPythonVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VPython: TPythonData;
    Reserved4: Integer;
    {$IFDEF CPUX64}
    Reserved5: Integer;  // size is 24 bytes in 64bit
    {$ENDIF CPUX64}
  end;


resourcestring
  SMultiDimensionalPropsNotSupported = 'Multi-dimensional sequences or mappings are not supported in Python';
  SCantConvertArg = 'Can''t convert argument #%d of %s into a Python object';
  SCantConvertKeyToPythonObject = 'Can''t convert Key into a Python object';
  SCantConvertValueToPythonObject = 'Can''t convert Value into a Python object';
  SCantCreateNewSequenceObject = 'Can''t create a new sequence object';
  SExpectedPythonVariant = 'Expected a Python variant';

{ Python variant creation utils }

function VarPythonCreate( AObject : PPyObject ) : Variant;
begin
  VarClear(Result);
  if Assigned(AObject) then
  begin
    TPythonVarData(Result).VType := VarPython;
    TPythonVarData(Result).VPython := TPythonData.Create(AObject);
  end; // of if
end;

function VarPythonCreate( const AValue : Variant ) : Variant;
var
  _value : PPyObject;
begin
  if VarIsPython(AValue) then
    Result := AValue
  else
    with GetPythonEngine do
    begin
      _value := VariantAsPyObject(AValue);
      try
        Result := VarPythonCreate( _value );
      finally
        Py_XDecRef(_value);
      end;
    end; // of with
end;

function VarPythonCreate( const AValues : array of const; ASequenceType : TSequenceType = stList ) : Variant;
var
  i : Integer;
  _seq, _item : PPyObject;
begin
  with GetPythonEngine do
  begin
    if ASequenceType = stTuple then
      _seq := PyTuple_New( High(AValues)-Low(AValues)+1 )
    else
      _seq := PyList_New( High(AValues)-Low(AValues)+1 );
    if not Assigned(_seq) then
      raise Exception.Create(SCantCreateNewSequenceObject);
    try
      for i := Low(AValues) to High(AValues) do
      begin
        if (AValues[i].VType = vtVariant) and VarIsPython(AValues[i].VVariant^) then
        begin
          _item := ExtractPythonObjectFrom( AValues[i].VVariant^ );
          Py_XIncRef(_item);
        end
        else
          _item := VarRecAsPyObject( AValues[i] );
        if ASequenceType = stTuple then
          PyTuple_SetItem( _seq, i, _item )
        else
          PyList_SetItem( _seq, i, _item );
      end; // of for
      Result := VarPythonCreate( _seq );
    finally
      Py_XDecRef(_seq);
    end; // of try
  end; // of with
end;

function VarPythonEval( const APythonExpression : AnsiString) : Variant;
var
  _obj : PPyObject;
begin
  with GetPythonEngine do
  begin
    _obj := EvalString(APythonExpression);
    try
      Result := VarPythonCreate( _obj  );
    finally
      Py_XDecRef(_obj);
    end;
  end;
end;

function VarPython: TVarType;
begin
  Result := PythonVariantType.VarType;
end;

function VarIsPython(const AValue: Variant): Boolean;
begin
  Result := (TVarData(AValue).VType and varTypeMask) = VarPython;
end;

function VarAsPython(const AValue: Variant): Variant;
begin
  if not VarIsPython(AValue) then
    VarCast(Result, AValue, VarPython)
  else
    Result := AValue;
end;

// note that the returned reference to the Python object is a borrowed reference (not pre-incremented).
function ExtractPythonObjectFrom(const AValue : Variant) : PPyObject;
begin
  if VarIsPython(AValue) then
    Result := TPythonVarData(AValue).VPython.PyObject
  else
    Result := nil;
end;

function VarIsSame(const A, B : Variant ) : Boolean;
begin
  Result := ExtractPythonObjectFrom(A) = ExtractPythonObjectFrom(B);
end;

function VarIsSameType(const A, B : Variant ) : Boolean;
var
  _obj1, _obj2 : PPyObject;
begin
  _obj1 := ExtractPythonObjectFrom(A);
  _obj2 := ExtractPythonObjectFrom(B);
  Result := Assigned(_obj1) and Assigned(_obj2) and (_obj1^.ob_type = _obj2^.ob_type);
end;

//------------------------------------------------------------------------------
{ Python variant helper functions }
function VarIsPythonSequence(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PySequence_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonMapping(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyMapping_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonNumber(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyNumber_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonString(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyUnicode_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonInteger(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyLong_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonFloat(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyFloat_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonTuple(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyTuple_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonList(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyList_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDict(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDict_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonClass(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyClass_Check(ExtractPythonObjectFrom(AValue))
             or (GetPythonEngine.PyObject_HasAttrString(ExtractPythonObjectFrom(AValue), '__bases__') <> 0));
end;

function VarIsPythonMethod(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyMethod_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonFunction(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyFunction_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonModule(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyModule_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonCallable(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyCallable_Check(ExtractPythonObjectFrom(AValue)) <> 0);
end;

function VarIsPythonIterator(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (GetPythonEngine.PyIter_Check(ExtractPythonObjectFrom(AValue)));
end;

function VarIsPythonUnicode(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyUnicode_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDateTime(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDateTime_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDate(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDate_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonTime(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyTime_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonDateTimeDelta(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyDelta_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsPythonTZInfo(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyTZInfo_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsBool(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyBool_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsEnum(const AValue: Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            GetPythonEngine.PyEnum_Check(ExtractPythonObjectFrom(AValue));
end;

function VarIsInstanceOf(const AInstance, AClass : Variant): Boolean;
begin
  with GetPythonEngine do
  begin
    Result := VarIsPython(AInstance) and VarIsPython(AClass) and
              (PyObject_IsInstance( ExtractPythonObjectFrom(AInstance),
                                    ExtractPythonObjectFrom(AClass)) <> 0);
    CheckError;
  end; // of with
end;

function VarIsSubclassOf(const ADerived, AClass : Variant): Boolean;
begin
  with GetPythonEngine do
  begin
    Result := VarIsPython(ADerived) and VarIsPython(AClass) and
              (PyObject_IsSubclass( ExtractPythonObjectFrom(ADerived),
                                    ExtractPythonObjectFrom(AClass)) <> 0);
    CheckError;
  end; // of with
end;

function VarIsSubtypeOf(const ADerived, AType : Variant): Boolean;
begin
  with GetPythonEngine do
  begin
    Result := VarIsPython(ADerived) and VarIsPython(AType) and
              (PyType_IsSubtype( ExtractPythonObjectFrom(ADerived)^.ob_type,
                                 PPyTypeObject(ExtractPythonObjectFrom(AType))) <> 0);
    CheckError;
  end; // of with
end;

function VarIsNone(const AValue : Variant): Boolean;
begin
  Result := VarIsPython(AValue) and
            (ExtractPythonObjectFrom(AValue) = GetPythonEngine.Py_None);
end;

function VarIsTrue(const AValue : Variant): Boolean;
begin
  Result := AValue; // the cast into a boolean will call the PyObject_IsTrue API.
end;

function VarModuleHasObject(const AModule : Variant; aObj: AnsiString): Boolean;
begin
  with GetPythonEngine do
    Result := VarIsPython(AModule) and
              PyModule_Check(ExtractPythonObjectFrom(AModule)) and
              Assigned(PyDict_GetItemString(
                PyModule_GetDict(ExtractPythonObjectFrom(AModule)),PAnsiChar(aObj)));
end;

function NewPythonList( const ASize : Integer = 0 ): Variant;
var
  _list : PPyObject;
begin
  with GetPythonEngine do
  begin
    _list := PyList_New(ASize);
    try
      Result := VarPythonCreate( _list );
    finally
      Py_XDecRef(_list);
    end; // of try
  end; // of with
end;

function NewPythonTuple( const ASize : Integer ): Variant;
var
  _tuple : PPyObject;
begin
  with GetPythonEngine do
  begin
    _tuple := PyTuple_New(ASize);
    try
      Result := VarPythonCreate( _tuple );
    finally
      Py_XDecRef(_tuple);
    end; // of try
  end; // of with
end;

function NewPythonDict: Variant;
var
  _dict : PPyObject;
begin
  with GetPythonEngine do
  begin
    _dict := PyDict_New;
    try
      Result := VarPythonCreate( _dict );
    finally
      Py_XDecRef(_dict);
    end; // of try
  end; // of with
end;

function VarPythonAsString(AValue : Variant) : string;
begin
  if VarIsPython(AValue) then
    Result := TPythonVarData(AValue).VPython.AsString
  else
    Result := AValue;
end;

{$IFDEF FPC}
function VarPythonToVariant(AValue : Variant): Variant;
begin
  if VarIsPython(AValue) then
    Result :=
      GetPythonEngine.PyObjectAsVariant(TPythonVarData(AValue).VPython.PyObject)
  else
    Result := AValue;
end;
{$ENDIF}

function None : Variant;
begin
  with GetPythonEngine do
    Result := VarPythonCreate(Py_None);
end;

function Ellipsis : Variant;
begin
  with GetPythonEngine do
    Result := VarPythonCreate(Py_Ellipsis);
end;

function MainModule : Variant;
var
  _main : PPyObject;
begin
  _main := GetPythonEngine.GetMainModule; // the refcount is not pre-incremented
  Assert(Assigned(_main));
  Result := VarPythonCreate(_main);
end;

function BuiltinModule : Variant;
begin
  Result := Import(AnsiString(GetPythonEngine.BuiltInModuleName));
end;

function SysModule : Variant;
begin
  Result := Import('sys');
end;

function DatetimeModule : Variant; // return the builtin module 'datetime'
begin
  Result := Import('datetime');
end;

function Import( const AModule : AnsiString ) : Variant;
var
  _module : PPyObject;
  _module_name : PPyObject;
begin
  with GetPythonEngine do
  begin
    _module_name := PyUnicodeFromString(AModule);
    try
      _module := PyImport_Import(_module_name);
      CheckError;
    finally
      Py_XDecRef(_module_name);
    end; // of try
    Assert(Assigned(_module));
    try
      Result := VarPythonCreate(_module);
    finally
      Py_XDecRef(_module);
    end; // of try
  end; // of with
end;

function GetObjectLength(AObject: PPyObject): NativeInt;
begin
  with GetPythonEngine do
  begin
    PyErr_Clear;
    Result := PyObject_Length(AObject);
    CheckError;
  end; // of with
end;

// returns the length of a Python collection.
function len(const AValue : Variant ) : NativeInt;
begin
  if VarIsPython(AValue) then
    Result := GetObjectLength( ExtractPythonObjectFrom(AValue) )
  else
    raise Exception.Create(SExpectedPythonVariant);
end;

function _type(const AValue : Variant ) : Variant;
begin
  if VarIsPython(AValue) then
    Result := VarPythonCreate( PPyObject( ExtractPythonObjectFrom(AValue)^.ob_type ) )
  else
    raise Exception.Create(SExpectedPythonVariant);
end;

function iter(const AValue : Variant ) : Variant;
var
  _iter : PPyObject;
begin
  if VarIsPython(AValue) then
    with GetPythonEngine do
    begin
      PyErr_Clear;
      _iter := PyObject_GetIter(ExtractPythonObjectFrom(AValue));
      CheckError;
      try
        Result := VarPythonCreate(_iter);
      finally
        Py_XDecRef(_iter);
      end;
    end
  else
    raise Exception.Create(SExpectedPythonVariant);
end;

//------------------------------------------------------------------------------
{ TPythonVariantType }

procedure TPythonVariantType.BinaryOp(var Left: TVarData;
  const Right: TVarData; const AOperator: TVarOp);
begin
  if Right.VType = VarType then
    case Left.VType of
      varString, varUString:
        case AOperator of
          opAdd:
            Variant(Left) := Variant(Left) + TPythonVarData(Right).VPython.AsString;
        else
          RaiseInvalidOp;
        end; // of varString
    else
      if Left.VType = VarType then
        case AOperator of
          opAdd:
            TPythonVarData(Left).VPython.DoAdd(TPythonVarData(Right).VPython);
          opSubtract:
            TPythonVarData(Left).VPython.DoSubtract(TPythonVarData(Right).VPython);
          opMultiply:
            TPythonVarData(Left).VPython.DoMultiply(TPythonVarData(Right).VPython);
          opDivide:
            TPythonVarData(Left).VPython.DoDivide(TPythonVarData(Right).VPython);
          opIntDivide:
            TPythonVarData(Left).VPython.DoIntDivide(TPythonVarData(Right).VPython);
          opModulus:
            TPythonVarData(Left).VPython.DoModulus(TPythonVarData(Right).VPython);
          opShiftLeft:
            TPythonVarData(Left).VPython.DoShiftLeft(TPythonVarData(Right).VPython);
          opShiftRight:
            TPythonVarData(Left).VPython.DoShiftRight(TPythonVarData(Right).VPython);
          opAnd:
            TPythonVarData(Left).VPython.DoAnd(TPythonVarData(Right).VPython);
          opOr:
            TPythonVarData(Left).VPython.DoOr(TPythonVarData(Right).VPython);
          opXor:
            TPythonVarData(Left).VPython.DoXor(TPythonVarData(Right).VPython);
        else
          RaiseInvalidOp;
        end // of case
      else
        RaiseInvalidOp;
    end // of case
  else
    RaiseInvalidOp;
end;

procedure TPythonVariantType.Cast(var Dest: TVarData;
  const Source: TVarData);
var
  _object : PPyObject;
begin
  _object := VarDataToPythonObject(Source);
  try
    PyhonVarDataCreate( Dest, _object );
  finally
    GetPythonEngine.Py_XDECREF(_object);
  end;
end;

procedure TPythonVariantType.CastTo(var Dest: TVarData;
  const Source: TVarData; const AVarType: TVarType);
var
  V : Variant;
begin
  if Source.VType = VarType then
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TPythonVarData(Source).VPython.AsWideString);
      varUString:
        {$IFDEF FPC}
        VarDataFromOleStr(Dest, TPythonVarData(Source).VPython.AsWideString);
        {$ELSE}
        VarDataFromStr(Dest, TPythonVarData(Source).VPython.AsWideString);
        {$ENDIF}
      varString:
        // Preserve AnsiStrings
        {$IFDEF FPC}
        Variant(Dest) := TPythonVarData(Source).VPython.AsWideString;
        {$ELSE}
        VarDataFromLStr(Dest, TPythonVarData(Source).VPython.AsAnsiString);
        {$ENDIF}
    else
      if AVarType and varTypeMask = varBoolean then
      begin
        Dest.VType := varBoolean;
        Dest.VBoolean := GetPythonEngine.PyObject_IsTrue( TPythonVarData(Source).VPython.PyObject ) = 1;
      end
      else
      begin
        V := TPythonVarData(Source).VPython.AsVariant;
        VarDataCastTo(Dest, TVarData(V), AVarType);
      end;
    end // of case
  else
    inherited;
end;

{$IFDEF FPC}
procedure TPythonVariantType.VarDataClear(var Dest: TVarData);
begin
  VarClear(Variant(Dest));
end;

procedure TPythonVariantType.VarDataCopyNoInd(var Dest: TVarData; const Source: TVarData);
begin
  VarCopyNoInd(Variant(Dest), Variant(Source));
end;

procedure TPythonVariantType.VarDataCastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); overload;
begin
  VarCast(Variant(Dest), Variant(Source), AVarType);
end;
{$ENDIF}

procedure TPythonVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TPythonVarData(V).VPython);
end;

function TPythonVariantType.CompareOp(const Left, Right: TVarData;
  const AOperator: TVarOp): Boolean;
begin
  Result := False;
  if (Left.VType = VarType) and (Right.VType = VarType) then
    case AOperator of
      opCmpEQ:
        Result := TPythonVarData(Left).VPython.Equal(TPythonVarData(Right).VPython);
      opCmpNE:
        Result := not TPythonVarData(Left).VPython.Equal(TPythonVarData(Right).VPython);
      opCmpLT:
        Result := TPythonVarData(Left).VPython.LessThan(TPythonVarData(Right).VPython);
      opCmpLE:
        Result := TPythonVarData(Left).VPython.LessOrEqualThan(TPythonVarData(Right).VPython);
      opCmpGT:
        Result := TPythonVarData(Left).VPython.GreaterThan(TPythonVarData(Right).VPython);
      opCmpGE:
        Result := TPythonVarData(Left).VPython.GreaterOrEqualThan(TPythonVarData(Right).VPython);
    else
      RaiseInvalidOp;
    end // of case
  else
    RaiseInvalidOp;
end;

procedure TPythonVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    PyhonVarDataCreate( Dest, TPythonVarData(Source).VPython.PyObject );
end;

procedure SetClearVarToEmptyParam(var V: TVarData);
begin
  VarClear(Variant(V));
  V.VType := varError;
{$IFNDEF FPC}
  V.VError := VAR_PARAMNOTFOUND;
{$ELSE}
  V.VError := HRESULT($80020004); {DISP_E_PARAMNOTFOUND}
{$ENDIF}
end;

const
  CDoMethod    = $01;
  CPropertyGet = $02;
  CPropertySet = $04;

{$IF defined(PATCHEDSYSTEMDISPINVOKE) and (defined(OSX64) or defined(LINUX))}
{
   Fixes https://quality.embarcadero.com/browse/RSP-28097
}
var
  _EmptyBSTR: PWideChar = nil;

Const
  SDispatchError = 'Variant method calls not supported';

procedure _DispInvokeError;
begin
  raise EVariantDispatchError.Create(SDispatchError);
end;

function GetDispatchInvokeArgs(CallDesc: PCallDesc; Params: Pointer; var Strings: TStringRefList; OrderLTR : Boolean): TVarDataArray;
const
  { Parameter type masks - keep in sync with decl.h/ap* enumerations}
  atString   = $48;
  atUString  = $4A;
  atVarMask  = $3F;
  atTypeMask = $7F;
  atByRef    = $80;
var
  I: Integer;
  ArgType: Byte;
  PVarParm: PVarData;
  StringCount: Integer;
  VAList: TVarArgList;
  Temp: Pointer;
begin
  VAList := TVarArgList(Params^);
  StringCount := 0;
  SetLength(Result, CallDesc^.ArgCount);
  for I := 0 to CallDesc^.ArgCount-1 do
  begin
    ArgType := CallDesc^.ArgTypes[I];

    if OrderLTR then
      PVarParm := @Result[I]
    else
      PVarParm := @Result[CallDesc^.ArgCount-I-1];

    if (ArgType and atByRef) = atByRef then
    begin
      Temp := VarArgGetValue(VAList, Pointer);
      if (ArgType and atTypeMask) = atString then
      begin
        PVarData(PVarParm)^.VType := varByRef or varOleStr;
        PVarData(PVarParm)^.VPointer := Strings[StringCount].FromAnsi( PAnsiString(Temp));
        Inc(StringCount);
      end
      else
      if (ArgType and atTypeMask) = atUString then
      begin
        PVarData(PVarParm)^.VType := varByRef or varOleStr;
        PVarData(PVarParm)^.VPointer := Strings[StringCount].FromUnicode(PUnicodeString(Temp));
        Inc(StringCount);
      end
      else
      begin
        if ((ArgType and atTypeMask) = varVariant) and
          ((PVarData(Temp)^.VType = varString) or (PVarData(Temp)^.VType = varUString)) then
          VarCast(PVariant(Temp)^, PVariant(Temp)^, varOleStr);
        //PVarData(PVarParm)^.VType := varByRef or (ArgType and atTypeMask);

        ArgType := ArgType and atTypeMask;
        if DispatchUnsignedAsSigned then
          case ArgType of
            varUInt64:   ArgType := varInt64;
            varUInt32:   ArgType := varInteger;
            varWord:     ArgType := varSmallint;
            varByte:     ArgType := varShortInt;
          end;
        PVarData(PVarParm)^.VType := varByRef or ArgType;

        PVarData(PVarParm)^.VPointer := Temp;
      end;
    end
    else // ByVal
    begin
      PVarParm^.VType := ArgType;
      case ArgType of
        varEmpty, varNull: ; // Only need to set VType
        varInteger:   PVarParm^.VInteger := VarArgGetValue(VAList, Integer);
        varSingle:    PVarParm^.VSingle := VarArgGetValue(VAList, Single);
        varDouble:    PVarParm^.VDouble :=  VarArgGetValue(VAList, Double);
        varCurrency:  PVarParm^.VCurrency := VarArgGetValue(VAList, Currency);
        varDate:      PVarParm^.VDate := VarArgGetValue(VAList, TDateTime);
        varOleStr:    PVarParm^.VPointer := VarArgGetValue(VAList, Pointer);
        varDispatch:  PVarParm^.VDispatch := VarArgGetValue(VAList, Pointer);
        varError:     PVarParm^.VError := HRESULT($80020004); //DISP_E_PARAMNOTFOUND;
        varBoolean:   PVarParm^.VBoolean := VarArgGetValue(VAList, Boolean);
        varVariant:
          begin
            PVarParm^.VType := varEmpty;
{$IFDEF CPUX64}

//          PVariant(PVarParm)^ := PVariant(Params^)^;
            PVariant(PVarParm)^ := VarArgGetValue(VAList, PVariant)^;
{$ELSE}
//          PVariant(PVarParm)^ := PVariant(Params)^;
            PVariant(PVarParm)^ := VarArgGetValue(VAList, Variant);
{$ENDIF}
          end;
        varUnknown:   PVarParm^.VUnknown := VarArgGetValue(VAList, Pointer);
        varSmallint:  PVarParm^.VSmallInt := VarArgGetValue(VAList, SmallInt);
        varShortInt:  PVarParm^.VShortInt := VarArgGetValue(VAList, ShortInt);
        varByte:      PVarParm^.VByte :=  VarArgGetValue(VAList, Byte);
        varWord:
          begin
            if DispatchUnsignedAsSigned then
            begin
              PVarParm^.VType := varInteger;
              PVarParm^.VInteger := Integer(VarArgGetValue(VAList, Word));
            end else
              PVarParm^.VWord := VarArgGetValue(VAList, Word);
          end;
        varUInt32:
          begin
            if DispatchUnsignedAsSigned then
            begin
              PVarParm^.VType := varInteger;
              PVarParm^.VInteger := Integer(VarArgGetValue(VAList, Cardinal));
            end else
              PVarParm^.VUInt32 := VarArgGetValue(VAList, Cardinal);
          end;
        varInt64:     PVarParm^.VInt64 := VarArgGetValue(VAList, Int64);
        varUInt64:
          begin
            if DispatchUnsignedAsSigned then
            begin
              PVarParm^.VType := varInt64;
              PVarParm^.VInt64 := VarArgGetValue(VAList, Int64); //Int64(PInt64(Params)^);
            end else
              PVarParm^.VUInt64 := VarArgGetValue(VAList, UInt64); //PUInt64(Params)^;
          end;
        atString:
        begin
          PVarParm^.VType := varOleStr;
          Temp := VarArgGetValue(VAList, Pointer);
          if PAnsiString(Temp)^ <> '' then
          begin
            {
            This line causes a crash and is replaced with the one below in line with unicode strings
            PVarParm^.VPointer := PWideChar(Strings[StringCount].FromAnsi(PAnsiString(Temp))^);
            }
            PVarParm^.VPointer := PWideChar(Strings[StringCount].FromAnsi(@AnsiString(Temp))^);
            Strings[StringCount].Ansi := nil;
            Inc(StringCount);
          end
          else
            PVarParm^.VPointer := _EmptyBSTR;
        end;
        atUString:
          begin
            PVarParm^.VType := varOleStr;
            Temp := VarArgGetValue(VAList, Pointer);
            if UnicodeString(Temp) <> '' then
            begin
              PVarParm^.VPointer := PWideChar(Strings[StringCount].FromUnicode(@UnicodeString(Temp))^);
              Strings[StringCount].Unicode := nil;
              Inc(StringCount);
            end
            else
              PVarParm^.VPointer := _EmptyBSTR;
          end;
      else
        // Unsupported Var Types
        //varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
        //varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
        //varRecord   = $0024; { VT_RECORD      36 }
        //varString   = $0100; { Pascal string  256 } {not OLE compatible }
        //varAny      = $0101; { Corba any      257 } {not OLE compatible }
        //varUString  = $0102; { Unicode string 258 } {not OLE compatible }
        _DispInvokeError;
      end;
    end;
  end;
end;
{$IFEND}

{$IFDEF DELPHIXE7_OR_HIGHER}
procedure TPythonVariantType.DispInvoke(Dest: PVarData;
  [Ref] const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
{$ELSE}
procedure TPythonVariantType.DispInvoke(Dest: PVarData;
   var Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
{$ENDIF}
{$IFDEF USESYSTEMDISPINVOKE}
{$IFDEF PATCHEDSYSTEMDISPINVOKE}
  //  Modified to correct memory leak QC102387 / RSP-23093
  procedure PatchedFinalizeDispatchInvokeArgs(CallDesc: PCallDesc; const Args: TVarDataArray; OrderLTR : Boolean);
  const
    atByRef    = $80;
  var
    I: Integer;
    ArgType: Byte;
    PVarParm: PVarData;
    VType: TVarType;
  begin
    for I := 0 to CallDesc^.ArgCount-1 do
    begin
      ArgType := CallDesc^.ArgTypes[I];

      if OrderLTR then
        PVarParm := @Args[I]
      else
        PVarParm := @Args[CallDesc^.ArgCount-I-1];

      VType := PVarParm.VType;

      // Only ByVal Variant or Array parameters have been copied and need to be released
      // Strings have been released via the use of the TStringRefList parameter to GetDispatchInvokeArgs
      // !!Modified to prevent memory leaks!! RSP-23093
      if ((ArgType and atByRef) <> atByRef) and ((ArgType = varVariant) or ((VType and varArray) = varArray)) then
        VarClear(PVariant(PVarParm)^);
    end;
  end;

  procedure PatchedDispInvoke(Dest: PVarData;
    const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
  {$IFNDEF DELPHI10_4_OR_HIGHER}
  type
    PStringRefList = ^TStringRefList;
  {$ENDIF}
  const
    CDoMethod    = $01;
    CPropertyGet = $02;
    CPropertySet = $04;
  var
    I, LArgCount: Integer;
    LIdent: string;
    LTemp: TVarData;
    VarParams : TVarDataArray;
    {$IFNDEF DELPHI10_4_OR_HIGHER}
    Strings: array of TStringRef;
    {$ELSE}
    Strings: TStringRefList;
    {$ENDIF}
    PIdent: PByte;
  begin
    // Grab the identifier
    LArgCount := CallDesc^.ArgCount;
    PIdent := @CallDesc^.ArgTypes[LArgCount];
    LIdent := FixupIdent( UTF8ToString(MarshaledAString(PIdent)) );
    if LArgCount > 0 then begin
      SetLength(Strings, LArgCount);
     {$IFNDEF DELPHI10_4_OR_HIGHER}
     VarParams := GetDispatchInvokeArgs(CallDesc, Params, PStringRefList(Strings)^, true);
     {$ELSE}
     VarParams := GetDispatchInvokeArgs(CallDesc, Params, Strings, true);
     {$ENDIF}
    end;
    try
      // What type of invoke is this?
      case CallDesc^.CallType of
        CDoMethod:
          // procedure with N arguments
          if Dest = nil then
          begin
            if not DoProcedure(Source, LIdent, VarParams) then
            begin

              // ok maybe its a function but first we must make room for a result
              VarDataInit(LTemp);
              try

                // notate that the destination shouldn't be bothered with
                // functions can still return stuff, we just do this so they
                //  can tell that they don't need to if they don't want to
                SetClearVarToEmptyParam(LTemp);

                // ok lets try for that function
                if not DoFunction(LTemp, Source, LIdent, VarParams) then
                  RaiseDispError;
              finally
                VarDataClear(LTemp);
              end;
            end
          end

          // property get or function with 0 argument
          else if LArgCount = 0 then
          begin
            if not GetProperty(Dest^, Source, LIdent) and
               not DoFunction(Dest^, Source, LIdent, VarParams) then
              RaiseDispError;
          end

          // function with N arguments
          else if not DoFunction(Dest^, Source, LIdent, VarParams) then
            RaiseDispError;

        CPropertyGet:
          if not ((Dest <> nil) and                         // there must be a dest
                  (LArgCount = 0) and                       // only no args
                  GetProperty(Dest^, Source, LIdent)) then  // get op be valid
            RaiseDispError;

        CPropertySet:
          if not ((Dest = nil) and                          // there can't be a dest
                  (LArgCount = 1) and                       // can only be one arg
                  SetProperty(Source, LIdent, VarParams[0])) then // set op be valid
            RaiseDispError;
      else
        RaiseDispError;
      end;

    finally
      PatchedFinalizeDispatchInvokeArgs(CallDesc, VarParams, true);
    end;

    for I := 0 to Length(Strings) - 1 do
    begin
      if Pointer(Strings[I].Wide) = nil then
        Break;
      if Strings[I].Ansi <> nil then
        Strings[I].Ansi^ := AnsiString(Strings[I].Wide)
      else
        if Strings[I].Unicode <> nil then
          Strings[I].Unicode^ := UnicodeString(Strings[I].Wide)
    end;
  end;
{$ENDIF PATCHEDSYSTEMDISPINVOKE}

  procedure GetNamedParams;
  var
    LNamePtr: PAnsiChar;
    LNamedArgStart : Integer;     //arg position of 1st named argument (if any)
    I : integer;
  begin
    LNamePtr := PAnsiChar(@CallDesc^.ArgTypes[CallDesc^.ArgCount]);
    LNamedArgStart := CallDesc^.ArgCount - CallDesc^.NamedArgCount;
    SetLength(fNamedParams, CallDesc^.NamedArgCount);
    // Skip function Name
    for I := 0 to CallDesc^.NamedArgCount - 1 do begin
      LNamePtr := LNamePtr + Succ(Length(LNamePtr));
      fNamedParams[I].Index := I+LNamedArgStart;
      fNamedParams[I].Name  := AnsiString(LNamePtr);
    end;
  end;

Var
  NewCallDesc : TCallDesc;
begin
  if CallDesc^.NamedArgCount > 0 then GetNamedParams;
  try
    if (CallDesc^.CallType = CPropertyGet) and (CallDesc^.ArgCount = 1) then begin
      NewCallDesc := CallDesc^;
      NewCallDesc.CallType := CDoMethod;
    {$IFDEF PATCHEDSYSTEMDISPINVOKE}
      PatchedDispInvoke(Dest, Source, @NewCallDesc, Params);
    {$ELSE PATCHEDSYSTEMDISPINVOKE}
      inherited DispInvoke(Dest, Source, @NewCallDesc, Params);
    {$ENDIF PATCHEDSYSTEMDISPINVOKE}
    end else
      {$IFDEF PATCHEDSYSTEMDISPINVOKE}
      PatchedDispInvoke(Dest, Source, CallDesc, Params);
      {$ELSE PATCHEDSYSTEMDISPINVOKE}
      inherited;
      {$ENDIF PATCHEDSYSTEMDISPINVOKE}
  finally
    if CallDesc^.NamedArgCount > 0 then SetLength(fNamedParams, 0);
  end;
end;

{$ELSE USESYSTEMDISPINVOKE}
begin
  DoDispInvoke(Dest, Source, CallDesc, Params);
end;

procedure TPythonVariantType.DoDispInvoke(Dest: PVarData;
  var Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
type
  PParamRec = ^TParamRec;
  TParamRec = array[0..3] of Integer;
  TStringDesc = record
    BStr: WideString;
    PStr: PAnsiString;
  end;
var
  LArguments: TVarDataArray;
  LStrings: array of TStringDesc;
  LStrCount: Integer;
  LParamPtr: Pointer;
  LNamedArgStart : Integer;     //arg position of 1st named argument (if any)
  LNamePtr: PAnsiChar;

  procedure ParseParam(I: Integer);
  const
    CArgTypeMask    = $7F;
    CArgByRef       = $80;
  var
    LArgType: Integer;
    LArgByRef: Boolean;
  begin
    LArgType := CallDesc^.ArgTypes[I] and CArgTypeMask;
    LArgByRef := (CallDesc^.ArgTypes[I] and CArgByRef) <> 0;

    if I >= LNamedArgStart then
    begin
      LNamePtr := LNamePtr + Succ(StrLen(LNamePtr));
      fNamedParams[I-LNamedArgStart].Index := I;
      fNamedParams[I-LNamedArgStart].Name  := AnsiString(LNamePtr);
    end;

    // error is an easy expansion
    if LArgType = varError then
      SetClearVarToEmptyParam(LArguments[I])

    // literal string
    else if LArgType = varStrArg then
    begin
      with LStrings[LStrCount] do
        if LArgByRef then
        begin
          //BStr := StringToOleStr(PAnsiString(ParamPtr^)^);
          BStr := WideString(System.Copy(PAnsiString(LParamPtr^)^, 1, MaxInt));
          PStr := PAnsiString(LParamPtr^);
          LArguments[I].VType := varOleStr or varByRef;
          LArguments[I].VOleStr := @BStr;
        end
        else
        begin
          //BStr := StringToOleStr(PAnsiString(LParamPtr)^);
          BStr := WideString(System.Copy(PAnsiString(LParamPtr)^, 1, MaxInt));
          PStr := nil;
          LArguments[I].VType := varOleStr;
          if BStr = '' then
            LArguments[I].VOleStr := nil
          else
            LArguments[I].VOleStr := PWideChar(BStr);
        end;
      Inc(LStrCount);
    end

    // value is by ref
    else if LArgByRef then
    begin
      if (LArgType = varVariant) and
         (PVarData(LParamPtr^)^.VType = varString)
           or (PVarData(LParamPtr)^.VType = varUString)
      then
        //VarCast(PVariant(ParamPtr^)^, PVariant(ParamPtr^)^, varOleStr);
        VarDataCastTo(PVarData(LParamPtr^)^, PVarData(LParamPtr^)^, varOleStr);
      LArguments[I].VType := LArgType or varByRef;
      LArguments[I].VPointer := Pointer(LParamPtr^);
    end

    // value is a variant
    else if LArgType = varVariant then
      if (PVarData(LParamPtr)^.VType = varString)
        or (PVarData(LParamPtr)^.VType = varUString)
      then
      begin
        with LStrings[LStrCount] do
        begin
          //BStr := StringToOleStr(AnsiString(PVarData(LParamPtr)^.VString));
          if (PVarData(LParamPtr)^.VType = varString) then
            BStr := WideString(System.Copy(AnsiString(PVarData(LParamPtr)^.VString), 1, MaxInt))
          else
            {$IFDEF FPC}
            BStr := System.Copy(UnicodeString(PVarData(LParamPtr)^.VString), 1, MaxInt);
            {$ELSE}
            BStr := System.Copy(UnicodeString(PVarData(LParamPtr)^.VUString), 1, MaxInt);
            {$ENDIF}
          PStr := nil;
          LArguments[I].VType := varOleStr;
          LArguments[I].VOleStr := PWideChar(BStr);
        end;
        Inc(LStrCount);
        Inc(NativeInt(LParamPtr), SizeOf(TVarData) - SizeOf(Pointer));
      end
      else
      begin
        LArguments[I] := PVarData(LParamPtr)^;
        Inc(NativeInt(LParamPtr), SizeOf(TVarData) - SizeOf(Pointer));
      end
    else
    begin
      LArguments[I].VType := LArgType;
      case CVarTypeToElementInfo[LArgType].Size of
        1, 2, 4:
        begin
          LArguments[I].VLongs[1] := PParamRec(LParamPtr)^[0];
        end;
        8:
        begin
          LArguments[I].VLongs[1] := PParamRec(LParamPtr)^[0];
          LArguments[I].VLongs[2] := PParamRec(LParamPtr)^[1];
          Inc(NativeInt(LParamPtr), 8 - SizeOf(Pointer));
        end;
      else
        RaiseDispError;
      end;
    end;
    Inc(NativeInt(LParamPtr), SizeOf(Pointer));
  end;

var
  I, LArgCount: Integer;
  LIdent: AnsiString;
  LTemp: TVarData;
begin
  //------------------------------------------------------------------------------------
  // Note that this method is mostly a copy&paste from  TInvokeableVariantType.DispInvoke
  // because Borland assumes that the names are not case sensitive, whereas Python has
  // case sensitive symbols.
  // We modified the property get to allow the use of indexed properties.
  //------------------------------------------------------------------------------------

  // Grab the identifier
  LArgCount := CallDesc^.ArgCount;
  //After arg types, method name and named arg names are stored
  //Position pointer on method name
  LNamePtr := PAnsiChar(@CallDesc^.ArgTypes[LArgCount]);
  LIdent := AnsiString(LNamePtr);
  //Named params must be after positional params
  LNamedArgStart := CallDesc^.ArgCount - CallDesc^.NamedArgCount;
  SetLength(fNamedParams, CallDesc^.NamedArgCount);

  // Parse the arguments
  LParamPtr := Params;
  SetLength(LArguments, LArgCount);
  LStrCount := 0;
  SetLength(LStrings, LArgCount);
  for I := 0 to LArgCount - 1 do
    ParseParam(I);

  // What type of invoke is this?
  case CallDesc^.CallType of
    CDoMethod:
      // procedure with N arguments
      if Dest = nil then
      begin
        if not DoProcedure(Source, string(LIdent), LArguments) then
        begin

          // ok maybe its a function but first we must make room for a result
          VarDataInit(LTemp);
          try

            // notate that the destination shouldn't be bothered with
            // functions can still return stuff, we just do this so they
            //  can tell that they don't need to if they don't want to
            SetClearVarToEmptyParam(LTemp);

            // ok lets try for that function
            if not DoFunction(LTemp, Source, string(LIdent), LArguments) then
              RaiseDispError;
          finally
            VarDataClear(LTemp);
          end;
        end
      end

      // property get or function with 0 argument
      else if LArgCount = 0 then
      begin
        if not GetProperty(Dest^, Source, string(LIdent)) and
           not DoFunction(Dest^, Source, string(LIdent), LArguments) then
          RaiseDispError;
      end

      // function with N arguments
      else if not DoFunction(Dest^, Source, string(LIdent), LArguments) then
        RaiseDispError;

    CPropertyGet:
    begin
      // here that code has been changed to allow the indexed properties.

      if Dest = nil then // there must be a dest
        RaiseDispError;
      if LArgCount = 0 then // no args
      begin
        if not GetProperty(Dest^, Source, string(LIdent)) then   // get op be valid
          RaiseDispError;
      end
      else if LArgCount = 1 then // only one arg
      begin
        if not GetPropertyWithArg(Dest^, Source, LIdent, LArguments[0]) then   // get op be valid
          RaiseDispError;
      end
      else
        raise Exception.Create( SMultiDimensionalPropsNotSupported );
    end;

    CPropertySet:
      if not ((Dest = nil) and                         // there can't be a dest
              (LArgCount = 1) and                       // can only be one arg
              SetProperty(Source, string(LIdent), LArguments[0])) then // set op be valid
        RaiseDispError;
  else
    RaiseDispError;
  end;

  // copy back the string info
  I := LStrCount;
  while I <> 0 do
  begin
    Dec(I);
    with LStrings[I] do
      if Assigned(PStr) then
        PStr^ := AnsiString(System.Copy(BStr, 1, MaxInt));
  end;
end;

function TPythonVariantType.GetPropertyWithArg(var Dest: TVarData;
  const V: TVarData; const AName: AnsiString; AArg: TVarData): Boolean;
var
  _prop, _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := nil;
    _prop := PyObject_GetAttrString(TPythonVarData(V).VPython.PyObject, PAnsiChar(AName));
    CheckError;
    if Assigned(_prop) then
    begin
      // here we check only sequences, as Delphi does not allow a type different from Integer
      // to be used within brackets.
      // But you can still access a dictionary with parenthesis, like: myObj.MyDict('MyKey')
      // Note that we can't use the brackets on a Python variant that contains a list,
      // because Delphi thinks it's a variant array, whereas it is not, of course!
      // So: myList[0] won't work, but myObj.MyList[0] will!!!
      if PySequence_Check(_prop) <> 0 then
      begin
        _result := PySequence_GetItem(_prop, Variant(AArg));
        CheckError;
      end; // of if
    end; // of if
    Result := Assigned(_result);
    if Result then
      try
        PyhonVarDataCreate(Dest, _result);
      finally
        Py_XDecRef(_prop);
      end; // of try
  end; // of with
end;
{$ENDIF USESYSTEMDISPINVOKE}

function TPythonVariantType.DoFunction(var Dest: TVarData;
  const V: TVarData; const AName: string;
  const Arguments: TVarDataArray): Boolean;
var
  _PyResult : PPyObject;
begin
  // eval the function call
  _PyResult := EvalPython(V, AnsiString(AName), Arguments);
  try
    Result := Assigned(_PyResult);
    // if the evaluation returned a result
    if Result then
      // convert it into a variant
      PyhonVarDataCreate( Dest, _PyResult );
  finally
    GetPythonEngine.Py_XDecRef( _PyResult );
  end; // of try
end;

function TPythonVariantType.DoProcedure(const V: TVarData;
  const AName: string; const Arguments: TVarDataArray): Boolean;
var
  _PyResult : PPyObject;
begin
  _PyResult := EvalPython(V, AnsiString(AName), Arguments);
  Result := Assigned(_PyResult);
  GetPythonEngine.Py_XDecRef( _PyResult );
end;

function TPythonVariantType.EvalPython(const V: TVarData;
  const AName: AnsiString; const Arguments: TVarDataArray): PPyObject;

  function ArgAsPythonObject( AArgIndex : Integer ) : PPyObject;
  begin
    Result := VarDataToPythonObject(Arguments[AArgIndex]);
    // if conversion failed, then too bad ;-)
    if not Assigned(Result) then
      raise Exception.CreateFmt(SCantConvertArg, [AArgIndex, AName]);
  end; // of function

  function GetObjectItem( AObject : PPyObject; const AKey : TVarData ) : PPyObject;
  var
    _key : PPyObject;
  begin
    _key := VarDataToPythonObject(AKey);
    with GetPythonEngine do
    begin
      PyErr_Clear;
      if not Assigned(_key) then
        raise Exception.Create(SCantConvertKeyToPythonObject);
      try
        Result := PyObject_GetItem( AObject, _key );
        CheckError;
      finally
        Py_XDecRef(_key);
      end; // of try
    end; // of with
  end; // of function

  function SetObjectItem( AObject : PPyObject; const AKey, AValue : TVarData ) : PPyObject;
  var
    _key, _value : PPyObject;
    _result : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      _key := VarDataToPythonObject(AKey);
      if not Assigned(_key) then
        raise Exception.Create(SCantConvertKeyToPythonObject);
      try
        _value := VarDataToPythonObject(AValue);
        if not Assigned(_value) then
          raise Exception.Create(SCantConvertValueToPythonObject);
        if PyList_Check(AObject) then
          _result := PyList_SetItem( AObject,
            {$IFDEF FPC}PtrInt(Variant(AKey)){$ELSE}Variant(AKey){$ENDIF}, _value )
        else if PyTuple_Check(AObject) then
          _result := PyTuple_SetItem( AObject,
            {$IFDEF FPC}PtrInt(Variant(AKey)){$ELSE}Variant(AKey){$ENDIF}, _value )
        else
          try
            if PySequence_Check(AObject) <> 0 then
              _result := PySequence_SetItem(AObject,
                {$IFDEF FPC}PtrInt(Variant(AKey)){$ELSE}Variant(AKey){$ENDIF}, _value)
            else
              _result := PyObject_SetItem( AObject, _key, _value );
          finally
            Py_XDecRef(_value);
          end; // of try
        CheckError;
        Result := PyLong_FromLong(_result);
      finally
        Py_XDecRef(_key);
      end; // of try
    end; // of with
  end; // of function

  function DeleteObjectItem( AObject : PPyObject; const AKey : TVarData ) : PPyObject;
  var
    _key : PPyObject;
  begin
    _key := VarDataToPythonObject(AKey);
    with GetPythonEngine do
    begin
      PyErr_Clear;
      if not Assigned(_key) then
        raise Exception.Create(SCantConvertKeyToPythonObject);
      try
        PyObject_DelItem( AObject, _key );
        CheckError;
        Result := ReturnNone;
      finally
        Py_XDecRef(_key);
      end; // of try
    end; // of with
  end; // of function

  procedure ExtractSliceIndexes(AObject : PPyObject; const AStart, AEnd: TVarData; out ASliceStart, ASliceEnd : Integer );
  begin
    with GetPythonEngine do
    begin
      if VarIsSame(Variant(AStart), Ellipsis) then
        ASliceStart := 0
      else
        ASliceStart := Variant(AStart);
      if VarIsSame(Variant(AEnd), Ellipsis) then
        ASliceEnd := PySequence_Length(AObject)
      else
        ASliceEnd := Variant(AEnd);
      CheckError;
    end; // of with
  end;

  function GetSequenceSlice( AObject : PPyObject; const AStart, AEnd: TVarData ) : PPyObject;
  var
    _start, _end : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      ExtractSliceIndexes(AObject, AStart, AEnd, _start, _end);
      Result := PySequence_GetSlice( AObject, _start, _end);
      CheckError;
    end; // of with
  end; // of function

  function SetSequenceSlice( AObject : PPyObject; const AStart, AEnd, AValue : TVarData ) : PPyObject;
  var
    _start, _end : Integer;
    _value : PPyObject;
    _result : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      ExtractSliceIndexes(AObject, AStart, AEnd, _start, _end);
      _value := VarDataToPythonObject(AValue);
      if not Assigned(_value) then
        raise Exception.Create(SCantConvertValueToPythonObject);
      try
        _result := PySequence_SetSlice( AObject, _start, _end, _value);
        CheckError;
        Result := PyLong_FromLong(_result);
      finally
        Py_XDecRef(_value);
      end; // of try
    end; // of with
  end; // of function

  function DelSequenceSlice( AObject : PPyObject; const AStart, AEnd: TVarData ) : PPyObject;
  var
    _start, _end, _result : Integer;
  begin
    with GetPythonEngine do
    begin
      PyErr_Clear;
      ExtractSliceIndexes(AObject, AStart, AEnd, _start, _end);
      _result := PySequence_DelSlice( AObject, _start, _end);
      CheckError;
      Result := PyLong_FromLong(_result);
    end; // of with
  end; // of function

  function SequenceContains( AObject : PPyObject; const AValue : TVarData ) : PPyObject;
  var
    _value : PPyObject;
    _result : Integer;
  begin
    _value := VarDataToPythonObject(AValue);
    with GetPythonEngine do
    begin
      PyErr_Clear;
      if not Assigned(_value) then
        raise Exception.Create(SCantConvertValueToPythonObject);
      try
        _result := PySequence_Contains( AObject, _value );
        CheckError;
        Result := PyLong_FromLong(_result);
      finally
        Py_XDecRef(_value);
      end; // of try
    end; // of with
  end; // of function

var
  i : Integer;
  _container : PPyObject;
  _obj : PPyObject;
  _Arg : PPyObject;
  _Args : PPyObject;
  _ArgLen : Integer;
  _KW : PPyObject;
begin
  Result := nil;
  with GetPythonEngine do
  begin
    // extract the associated Python object
    _container := TPythonVarData(V).VPython.PyObject;

    // extract the key from the container object
    _obj := PyObject_GetAttrString(_container, PAnsiChar(AName));
    try
      try
        // if the container object does not have the key AName
        if PyErr_Occurred <> nil then
        begin
          // here we handle a special case: COM (or Delphi?) doesn't allow you to have index properties
          // on a variant except from a true array variant! So if myVar is a Python variant that holds
          // a Python list, I can't write something like: myVar[0]! But if the list is a member of a
          // Python instance, it will work fine! Like: myInst.myList[0]
          // So, to handle this problem we detect some special names: GetItem, SetItem and Length
          // that will do the same as:
          // myList[0] <-> myList.GetItem(0)
          // myDict['Hello'] := 1 <-> myDict.SetItem('Hello', 1)
          // len(myList) <-> myList.Length()
          // we get some bonus with the slices and in operators also:
          // myList = [0, 1, 2, 3]; myList.GetSlice(1, 2) --> [1, 2]
          // myList.Contains(2) <-> 2 in myList

          if (Length(Arguments) = 1) and SameText(string(AName), 'GetItem') then
            Result := GetObjectItem(_container, Arguments[0])
          else if (Length(Arguments) = 2) and SameText(string(AName), 'SetItem') then
            Result := SetObjectItem(_container, Arguments[0], Arguments[1])
          else if (Length(Arguments) = 1) and SameText(string(AName), 'DeleteItem') then
            Result := DeleteObjectItem(_container, Arguments[0])
          else if (Length(Arguments) = 2) and SameText(string(AName), 'GetSlice') then
            Result := GetSequenceSlice(_container, Arguments[0], Arguments[1])
          else if (Length(Arguments) = 3) and SameText(string(AName), 'SetSlice') then
            Result := SetSequenceSlice(_container, Arguments[0], Arguments[1], Arguments[2])
          else if (Length(Arguments) = 2) and SameText(string(AName), 'DelSlice') then
            Result := DelSequenceSlice(_container, Arguments[0], Arguments[1])
          else if (Length(Arguments) = 1) and SameText(string(AName), 'Contains') then
            Result := SequenceContains(_container, Arguments[0])
          else if SameText(string(AName), 'Length') then
            Result := PyLong_FromLong( GetObjectLength(_container) );
        end; // of if
      finally
        // if the key did not exist, Python generated an exception that we must propagate through CheckError
        CheckError;
      end; // of try
      // exit now if our special functions could apply
      if Assigned(Result) then
        Exit;
      // if we found the key
      if Assigned(_obj) then
        // if the associated object is callable (a method or a function)
        if PyCallable_Check(_obj) <> 0 then
        begin
          // Prepare the Python arguments for calling the func or method.

          // there's a special case: if we call a Python method without any argument (like myList.sort()),
          // the arguments list contains a single error variant.
          if (Length(Arguments) = 1) and VarDataIsEmptyParam(Arguments[0]) then
            _ArgLen := 0
          else
            _ArgLen := Length(Arguments);
          if Length(fNamedParams) > 0 then
          begin
            _KW := PyDict_New;
            _ArgLen := fNamedParams[0].Index;
          end
          else
            _KW := nil;
          try
            _Args := PyTuple_New(_ArgLen);
            try
              for i := 0 to _ArgLen-1 do
                PyTuple_SetItem( _Args, i, ArgAsPythonObject(i) );
              for i := 0 to Length(fNamedParams)-1 do
                PyDict_SetItemString(_KW, PAnsiChar(fNamedParams[i].Name), ArgAsPythonObject(fNamedParams[i].Index));

              // call the func or method, with or without named parameters (KW)
              if Assigned(_KW) then
                Result := PyEval_CallObjectWithKeywords(_obj, _Args, _KW)
              else
                Result := PyEval_CallObjectWithKeywords(_obj, _Args, nil);
              CheckError(True);
            finally
              Py_XDecRef(_Args);
            end; // of try
          finally
            Py_XDecRef(_KW);
          end;
        end // of if
        // if we have one argument and our associated object is not callable
        // it could mean that we're trying to access an item of a sequence or
        // a mapping, like myInst.MyList(1)
        else if Length(Arguments) = 1 then
        begin
          // if we have a sequence or a mapping object
          if (PySequence_Check(_obj) <> 0) or (PyMapping_Check(_obj) <> 0) then
          begin
            // convert the variant argument into a Python object
            _Arg := VarDataToPythonObject(Arguments[0]);
            try
              // get the item
              Result := PyObject_GetItem(_obj, _Arg);
              CheckError;
            finally
              Py_XDecRef(_Arg);
            end; // of try
          end; // of if
        end // of else if
        else
          raise Exception.Create( SMultiDimensionalPropsNotSupported );
    finally
      Py_XDecRef(_obj);
    end; // of try
  end; // of with
end;

{$IFNDEF FPC}
function TPythonVariantType.FixupIdent(const AText: string): string;
begin
  Result := AText;
end;
{$ENDIF FPC}

function TPythonVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result := TPythonVarData(V).VPython;
end;

function TPythonVariantType.GetProperty(var Dest: TVarData;
  const V: TVarData; const AName: string): Boolean;
var
  _prop : PPyObject;
  _len : Integer;
begin
  with GetPythonEngine do
  begin
    _prop := PyObject_GetAttrString(TPythonVarData(V).VPython.PyObject, PAnsiChar(AnsiString(AName)));
    // if we could not find the property
    if (PyErr_Occurred <> nil) or not Assigned(_prop) then
    begin
      // test a special property name for sequences only
      if SameText(AName, 'Length') then
      begin
        // if it's a sequence or a mapping
        if (PySequence_Check(TPythonVarData(V).VPython.PyObject) <> 0) or
           (PyMapping_Check(TPythonVarData(V).VPython.PyObject) <> 0) then
        begin
          // clear the error state of Python
          PyErr_Clear;
          // get the length
          _len := PyObject_Length(TPythonVarData(V).VPython.PyObject);
          CheckError;
          // convert the length into a Python integer
          _prop := PyLong_FromLong( _len );
        end; // of if
      end;
    end; // of if
    CheckError;
    Result := Assigned(_prop);
    if Result then
      try
        PyhonVarDataCreate(Dest, _prop);
      finally
        Py_XDecRef(_prop);
      end; // of try
  end; // of with
end;

function TPythonVariantType.IsClear(const V: TVarData): Boolean;
begin
  Result := (TPythonVarData(V).VPython = nil) or
            TPythonVarData(V).VPython.IsNone;
end;

function TPythonVariantType.LeftPromotion(const V: TVarData;
  const AOperator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  { TypeX Op Python }
  if (AOperator = opAdd) and VarDataIsStr(V) then
    RequiredVarType := varUString
  else
    RequiredVarType := VarType;

  Result := True;
end;

// this method increases the ref count of AObject
procedure TPythonVariantType.PyhonVarDataCreate(var Dest: TVarData;
  AObject: PPyObject);
begin
  VarDataClear(Dest);
  if Assigned(AObject) then
  begin
    TPythonVarData(Dest).VType := VarPython;
    TPythonVarData(Dest).VPython := TPythonData.Create(AObject);
  end;
end;

function TPythonVariantType.RightPromotion(const V: TVarData;
  const AOperator: TVarOp; out RequiredVarType: TVarType): Boolean;
begin
  { Python Op TypeX }
  // Note that the Int64 doesn't is an exception, because the expr Python op Int64,
  // won't cast the Int64 to Python.
  // See unit Variants, function VarCastRare.
  if V.VType <> varInt64 then
  begin
    RequiredVarType := VarType;
    Result := True;
  end
  else
    Result := False;
end;

function TPythonVariantType.SetProperty({$IFDEF FPC}var{$ELSE}const{$ENDIF} V: TVarData;
  const AName: string; const Value: TVarData): Boolean;
var
  _newValue : PPyObject;
begin
  with GetPythonEngine do
  begin
    _newValue := VarDataToPythonObject(Value);
    try
      PyObject_SetAttrString(TPythonVarData(V).VPython.PyObject, PAnsiChar(AnsiString(AName)), _newValue );
      CheckError;
    finally
      Py_XDecRef(_newValue);
    end; // of try
  end; // of with
  Result := True;
end;

procedure TPythonVariantType.UnaryOp(var Right: TVarData;
  const AOperator: TVarOp);
begin
  if Right.VType = VarType then
    case AOperator of
      opNegate:
        TPythonVarData(Right).VPython.DoNegate;
      opNot:
        TPythonVarData(Right).VPython.DoNot;
    else
      RaiseInvalidOp;
    end // of case
  else
    RaiseInvalidOp;
end;

function TPythonVariantType.VarDataToPythonObject(AVarData: TVarData): PPyObject;
var
  _PVarData : PVarData;
begin
  Result := nil;
  // first detect if we have a Python variant as argument and extract its associated Python object
  if AVarData.VType = VarType then
  begin
    Result := TPythonVarData(AVarData).VPython.PyObject;
    GetPythonEngine.Py_XINCREF(Result);
  end // of if
  else if VarDataIsByRef(AVarData) and
          ((AVarData.VType and varTypeMask) = varVariant) then
  begin
    _PVarData := AVarData.VPointer;
    if _PVarData^.VType = VarType then
    begin
      Result := TPythonVarData(_PVarData^).VPython.PyObject;
      GetPythonEngine.Py_XINCREF(Result);
    end; // of if
  end // of if
  else if (AVarData.VType and varTypeMask) = varNull then
    Result := GetPythonEngine.ReturnNone
  else if (AVarData.VType and varTypeMask) = varEmpty then
    Result := GetPythonEngine.ReturnNone;
  // else try to convert the variant into a Python object
  if not Assigned(Result) then
    Result := GetPythonEngine.VariantAsPyObject(Variant(AVarData));
end;

//------------------------------------------------------------------------------
{ TPythonData }

constructor TPythonData.Create(AObject: PPyObject);
begin
  PyObject := AObject;
end;

destructor TPythonData.Destroy;
begin
  if PythonOK then
    PyObject := nil;
  inherited;
end;

procedure TPythonData.DoAdd(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Add(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoAnd(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_And(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoDivide(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_TrueDivide(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoIntDivide(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_FloorDivide( PyObject, Right.PyObject );
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoModulus(const Right: TPythonData);
var
  _item, _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Divmod(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) and (PySequence_Check(_result) <> 0) and
       (PySequence_Length(_result) >= 2) then
    begin
      _item := PySequence_GetItem(_result, 1);
      PyObject := _item;
      Py_XDecRef(_item);
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoMultiply(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Multiply(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoNegate;
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Negative(PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoNot;
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Invert(PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoOr(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Or(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoShiftLeft(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_LShift(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoShiftRight(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_RShift(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoSubtract(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Subtract(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

procedure TPythonData.DoXor(const Right: TPythonData);
var
  _result : PPyObject;
begin
  with GetPythonEngine do
  begin
    _result := PyNumber_Xor(PyObject, Right.PyObject);
    CheckError;
    if Assigned(_result) then
    begin
      PyObject := _result;
      Py_XDecRef(_result);
    end; // of if
  end; // of with
end;

function TPythonData.Equal(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
  begin
    Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_EQ) = 1;
    CheckError;
  end; // of with
end;

function TPythonData.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString);
end;

function TPythonData.GetAsString: string;
begin
  if Assigned(PyObject) then
    Result := GetPythonEngine.PyObjectAsString(PyObject)
  else
    result := '';
end;

function TPythonData.GetAsVariant: Variant;
begin
  if Assigned(PyObject) then
    Result := GetPythonEngine.PyObjectAsVariant(PyObject)
  else
    Result := Null;
end;

function TPythonData.GetAsWideString: UnicodeString;
begin
  if Assigned(PyObject) and GetPythonEngine.PyUnicode_Check(PyObject) then
    Result := GetPythonEngine.PyUnicodeAsString(PyObject)
  else
    Result := UnicodeString(GetAsString);
end;

function TPythonData.GreaterOrEqualThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
    Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_GE) = 1
end;

function TPythonData.GreaterThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
    Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_GT) = 1
end;

function TPythonData.IsNone: Boolean;
begin
  Result := PyObject = GetPythonEngine.Py_None;
end;

function TPythonData.LessOrEqualThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
    Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_LE) = 1
end;

function TPythonData.LessThan(const Right: TPythonData): Boolean;
begin
  with GetPythonEngine do
    Result := PyObject_RichCompareBool(PyObject, Right.PyObject, Py_LT) = 1
end;

procedure TPythonData.SetPyObject(const Value: PPyObject);
begin
  with GetPythonEngine do
  begin
    Py_XDecRef(fPyObject);
    fPyObject := Value;
    Py_XIncRef(fPyObject);
  end;
end;

{ TVarPyEnumerator }

constructor TVarPyEnumerator.Create(const AValue: Variant);
begin
  FIterator := iter(AValue);
end;

function TVarPyEnumerator.GetCurrent: Variant;
begin
  Result := FCurrent;
end;

function TVarPyEnumerator.MoveNext: Boolean;
begin
  Result := True;
  try
    FCurrent := BuiltinModule.next(FIterator);
  except
    on E: EPyStopIteration do
    begin
      Result := False;
    end
    else
      raise;
  end;
end;

function VarPyIterate(const AValue: Variant): TVarPyEnumerateHelper;
begin
  Result.Create(AValue);
end;

{ TVarPyEnumerateHelper }

constructor TVarPyEnumerateHelper.Create(const AValue: Variant);
begin
  FIterable := AValue;
end;

function TVarPyEnumerateHelper.GetEnumerator: TVarPyEnumerator;
begin
  Result.Create(FIterable);
end;

initialization
  PythonVariantType := TPythonVariantType.Create;
finalization
  FreeAndNil(PythonVariantType);
end.
