(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PythonEngine'     Copyright (c) 1997                    *)
(*                                                                        *)
(*                                  Dr. Dietmar Budelsky                  *)
(*                                  dbudelsky@web.de                      *)
(*                                  Germany                               *)
(*                                                                        *)
(*                                  Morgan Martinet                       *)
(*                                  4723 rue Brebeuf                      *)
(*                                  H2J 3L2 MONTREAL (QC)                 *)
(*                                  CANADA                                *)
(*                                  e-mail: p4d@mmm-experts.com           *)
(*                                                                        *)
(*                                  PyScripter                            *)
(*                                  e-mail: pyscripter@gmail.com          *)
(*                                                                        *)
(*  Project page: https://github.com/pyscripter/python4delphi             *)
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
(*      Andrey Gruzdev (andrey.gruzdev@gmail.com)                         *)
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
{$POINTERMATH ON}

unit PythonEngine;

{ TODO -oMMM : implement tp_as_buffer slot }
{ TODO -oMMM : implement Attribute descriptor and subclassing stuff }

{$IFNDEF FPC}
  {$IFNDEF DELPHI2010_OR_HIGHER}
      Error! Delphi 2010 or higher is required!
  {$ENDIF}
{$ENDIF}

{$IF defined(LINUX) or (defined(BSD) and not defined(DARWIN)) or defined(SOLARIS) or defined(HAIKU)}
  {$define _so_files}
{$IFEND}

interface

uses
  Types,
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
{$IFDEF FPC}
  Dl,
  DynLibs,
{$ELSE}
  Posix.DLfcn,
  Posix.Pthread,
{$ENDIF}
{$ENDIF}
  Classes, SysUtils, SyncObjs, Variants,
  MethodCallBack, PythonTypes, PythonExceptions, PythonKnownVersions,
  PythonConsts, PythonInputOutput, PythonInterpreter;

// See function PyType_HasFeature below for testing the flags.

const
  TPFLAGS_DEFAULT = [tpfBaseType, tpHaveVersionTag];

{$IF not Defined(FPC) and (CompilerVersion >= 23)}
const
  PID_SUPPORTED_PLATFORMS = pidWin32 or pidWin64
                         or pidOSX32 or pidOSX64
                         or pidiOSDevice32 or pidiOSDevice64
                         or pidAndroid32Arm or pidAndroid64Arm;
{$IFEND}

//--------------------------------------------------------
//--                                                    --
//-- class:  TPythonEngine derived from TPythonInterface--
//-- Pytrunobject providing interface for               --
//-- running Python into Delphi                         --
//--------------------------------------------------------
const
  DEFAULT_DATETIME_CONVERSION_MODE = dcmToTuple;
type
  TPythonModule = class; // forward declaration
  TEngineClient = class;

  TTracebackItem = class
  public
    FileName : string;
    LineNo : Integer;
    Context : string;
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
  [ComponentPlatformsAttribute(PID_SUPPORTED_PLATFORMS)]
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
    FProgramName:                UnicodeString;
    FPythonHome:                 UnicodeString;
    FInitThreads:                Boolean;
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
    procedure DoOpenDll(const aDllName : string); override;
    procedure SetInitScript(Value: TStrings);
    function  GetThreadState: PPyThreadState;
    procedure SetInitThreads(Value: Boolean);
    function  GetClientCount : Integer;
    function  GetClients( idx : Integer ) : TEngineClient;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
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
    procedure  SetPythonHome(const PythonHome: UnicodeString);
    procedure  SetProgramName(const ProgramName: UnicodeString);
    function   IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
    function   Run_CommandAsString(const command : AnsiString; mode : Integer) : string;
    function   Run_CommandAsObject(const command : AnsiString; mode : Integer) : PPyObject;
    function   Run_CommandAsObjectWithDict(const command : AnsiString; mode : Integer; locals, globals : PPyObject) : PPyObject;
    function   EncodeString (const str: UnicodeString): AnsiString; {$IFDEF FPC}overload;{$ENDIF}
    {$IFDEF FPC}
    overload;
    function   EncodeString (const str: AnsiString): AnsiString; overload;
    {$ENDIF}
    function   EncodeWindowsFilePath (const str: string): AnsiString;
    procedure  ExecString(const command : AnsiString); overload;
    procedure  ExecStrings( strings : TStrings ); overload;
    function   EvalString(const command : AnsiString) : PPyObject; overload;
    function   EvalStringAsStr(const command : AnsiString) : string;
    function   EvalStrings( strings : TStrings ) : PPyObject; overload;
    procedure  ExecString(const command : AnsiString; locals, globals : PPyObject ); overload;
    procedure  ExecStrings( strings : TStrings; locals, globals : PPyObject ); overload;
    function   EvalString( const command : AnsiString; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStrings( strings : TStrings; locals, globals : PPyObject ) : PPyObject; overload;
    function   EvalStringsAsStr( strings : TStrings ) : string;
    function   EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
    function   EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
    function   EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
    function   CheckEvalSyntax( const str : AnsiString ) : Boolean;
    function   CheckExecSyntax( const str : AnsiString ) : Boolean;
    function   CheckSyntax( const str : AnsiString; mode : Integer ) : Boolean;
    procedure  RaiseError;
    function   PyObjectAsString( obj : PPyObject ) : string;
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

    { String conversion }
    function PyUnicodeFromString(const AString : UnicodeString) : PPyObject; overload;
    function PyUnicodeFromString(const AString: AnsiString): PPyObject; overload;
    function PyUnicodeAsString( obj : PPyObject ) : UnicodeString;

    // Public Properties
    property ClientCount : Integer read GetClientCount;
    property Clients[ idx : Integer ] : TEngineClient read GetClients;
    property ExecModule : AnsiString read FExecModule write FExecModule;
    property ThreadState: PPyThreadState read GetThreadState;
    property Traceback : TPythonTraceback read FTraceback;
    property LocalVars : PPyObject read FLocalVars Write SetLocalVars;
    property GlobalVars : PPyObject read FGlobalVars Write SetGlobalVars;
    property IOPythonModule: TObject read FIOPythonModule; {TPythonModule}
    property PythonHome: UnicodeString read FPythonHome write SetPythonHome;
    property ProgramName: UnicodeString read FProgramName write SetProgramName;
  published
    property AutoLoad;
    property AutoUnload;
    property DllName;
    property DllPath;
    property APIVersion;
    property RegVersion;
    property FatalAbort;
    property FatalMsgDlg;
    property UseLastKnownVersion;
    property OnAfterLoad;
    property OnBeforeLoad;
    property OnBeforeUnload;
    property OnPathInitialization;

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
      FModuleDef : PyModuleDef;
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
  TErrors = class; // forward declaration

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
  [ComponentPlatformsAttribute(PID_SUPPORTED_PLATFORMS)]
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
    function  NbSubtract( obj : PPyObject) : PPyObject; virtual;
    function  NbMultiply( obj : PPyObject) : PPyObject; virtual;
    function  NbFloorDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbTrueDivide( obj : PPyObject) : PPyObject; virtual;
    function  NbMatrixMultiply( obj : PPyObject) : PPyObject; virtual;
    function  NbRemainder( obj : PPyObject) : PPyObject; virtual;
    function  NbDivmod( obj : PPyObject) : PPyObject; virtual;
    function  NbPower( ob1, ob2 : PPyObject) : PPyObject; virtual;
    function  NbNegative : PPyObject; virtual;
    function  NbPositive : PPyObject; virtual;
    function  NbAbsolute : PPyObject; virtual;
    function  NbBool : Integer; virtual;
    function  NbInvert : PPyObject; virtual;
    function  NbLShift( obj : PPyObject) : PPyObject; virtual;
    function  NbRShift( obj : PPyObject) : PPyObject; virtual;
    function  NbAnd( obj : PPyObject) : PPyObject; virtual;
    function  NbXor( obj : PPyObject) : PPyObject; virtual;
    function  NbOr( obj : PPyObject) : PPyObject; virtual;
    function  NbInt : PPyObject; virtual;
    function  NbFloat : PPyObject; virtual;
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
    function  NbInplaceMatrixMultiply(obj: PPyObject): PPyObject; virtual;
    // Sequence services
    function  SqLength : NativeInt; virtual;
    function  SqConcat( obj : PPyObject) : PPyObject; virtual;
    function  SqRepeat( val : NativeInt ) : PPyObject; virtual;
    function  SqItem( idx : NativeInt ) : PPyObject; virtual;
    function  SqAssItem( idx : NativeInt; obj : PPyObject) : Integer; virtual;
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

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(PID_SUPPORTED_PLATFORMS)]
  {$IFEND}
  // The component that initializes the Python type and
  // that creates instances of itself.
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

  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(PID_SUPPORTED_PLATFORMS)]
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
      destructor Destroy(); override;

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
function GetPythonVersionFromDLLName(const DLLFileName : string): string;

{ Helper functions}
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
function CleanString(const s : UnicodeString; AppendLF : Boolean = True) : UnicodeString; overload;

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
  {$IFNDEF ANDROID}
  AnsiStrings,
  {$ELSE}
    StrUtils,
  {$ENDIF}
{$ENDIF}
  Math;


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
      if Assigned(limitv) and PyLong_Check(limitv) then
        alimit := PyLong_AsLong(limitv);
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
        FInitialized := False;
      end;
    except
    end;
  end;
  // Detach our clients, when engine is being destroyed or one of its clients.
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

procedure TPythonEngine.DoOpenDll(const aDllName : string);
var
  i : Integer;
begin
  if UseLastKnownVersion then
    for i:= Integer(COMPILED_FOR_PYTHON_VERSION_INDEX) downto 1 do
    begin
      RegVersion := PYTHON_KNOWN_VERSIONS[i].RegVersion;
      inherited DoOpenDll(PYTHON_KNOWN_VERSIONS[i].DllName);
      if IsHandleValid then
      begin
        DllName := PYTHON_KNOWN_VERSIONS[i].DllName;
        APIVersion := PYTHON_KNOWN_VERSIONS[i].APIVersion;
        Exit;
      end;
    end
  else
    RegVersion := GetPythonVersionFromDLLName(aDllName);
  inherited;
end;

procedure TPythonEngine.AssignPyFlags;

  procedure SetFlag( AFlag: PInteger; AValue : Boolean );
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
  SetFlag(Py_IgnoreEnvironmentFlag, pfIgnoreEnvironmentFlag in FPyFlags);
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

  gPythonEngine := Self;
  {$IFDEF WINDOWS}
  CheckRegistry;
  {$ENDIF}
  if Assigned(Py_SetProgramName) then
  begin
    if FProgramName = '' then
      FProgramName := UnicodeString(ParamStr(0));
    Py_SetProgramName(PWideChar(FProgramName));
  end;
  AssignPyFlags;
  if FPythonHome <> '' then
    Py_SetPythonHome(PWideChar(FPythonHome));
  Py_Initialize;
  if Assigned(Py_IsInitialized) then
    FInitialized := Py_IsInitialized() <> 0
  else
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

procedure TPythonEngine.SetProgramArgs;
var
  i, argc : Integer;
  wargv : array of PWideChar;
  {$IFDEF POSIX}
  UCS4L : array of UCS4String;
  {$ELSE}
  WL : array of UnicodeString;
  {$ENDIF}
begin
  // we build a string list of the arguments, because ParamStr returns a volatile string
  // and we want to build an array of PAnsiChar, pointing to valid strings.
  argc := ParamCount;
  SetLength(wargv, argc + 1);
  // build the PWideChar array
  {$IFDEF POSIX}
  // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
  SetLength(UCS4L, argc+1);
  for i := 0 to argc do begin
    UCS4L[i] := WideStringToUCS4String(ParamStr(i));
    wargv[i] := @UCS4L[i][0];
  end;
  {$ELSE}
  SetLength(WL, argc+1);
  for i := 0 to argc do begin
    WL[i] := UnicodeString(ParamStr(i));
    wargv[i] := PWideChar(WL[i]);
  end;
  {$ENDIF}
  // set the argv list of the sys module with the application arguments
  PySys_SetArgv( argc + 1, PPWideChar(wargv) );
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

procedure TPythonEngine.SetPythonHome(const PythonHome: UnicodeString);
begin
  FPythonHome := PythonHome;
end;

procedure TPythonEngine.SetProgramName(const ProgramName: UnicodeString);
begin
  FProgramName := ProgramName;
end;

function TPythonEngine.IsType(ob: PPyObject; obt: PPyTypeObject): Boolean;
begin
  result := ob^.ob_type = obt;
end;

function TPythonEngine.EvalPyFunction(pyfunc, pyargs:PPyObject): Variant;
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
  except
    if PyErr_Occurred <> nil then
      CheckError(False)
    else
      raise;
  end;
end;

function TPythonEngine.EvalFunction(pyfunc:PPyObject; args: array of const): Variant;
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

function TPythonEngine.EvalFunctionNoArgs(pyfunc:PPyObject): Variant;
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

function TPythonEngine.EvalStringAsStr(const command : AnsiString) : string;
begin
  Result := Run_CommandAsString( command, eval_input );
end;

function TPythonEngine.EvalString(const command : AnsiString) : PPyObject;
begin
  Result := Run_CommandAsObject( command, eval_input );
end;

procedure TPythonEngine.ExecString(const command : AnsiString);
begin
  Py_XDecRef( Run_CommandAsObject( command, file_input ) );
end;

function TPythonEngine.Run_CommandAsString(const command : AnsiString; mode : Integer) : string;
var
  v : PPyObject;
begin
  Result := '';
  v := Run_CommandAsObject( command, mode );
  Result := PyObjectAsString( v );
  Py_XDECREF(v);
end;

function TPythonEngine.Run_CommandAsObject(const command : AnsiString; mode : Integer) : PPyObject;
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
    _globals := _locals;

  try
    Result := PyRun_String(PAnsiChar(CleanString(command)), mode, _globals, _locals);
    if Result = nil then
      CheckError(False);
  except
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

function TPythonEngine.EvalStringsAsStr( strings : TStrings ) : string;
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

  function Define( E : EPythonError; const sType, sValue : string ) : EPythonError;
  begin
    E.EName  := sType;
    E.EValue := sValue;
    if sValue <> '' then
      E.Message := Format('%s: %s',[sType,sValue])
    else
      E.Message := sType;
    Result := E;
  end;

  function DefineSyntaxError( E : EPySyntaxError; const sType, sValue : UnicodeString; err_type, err_value : PPyObject ) : EPySyntaxError;
  var
    s_value       : UnicodeString;
    s_line        : UnicodeString;
    s_filename    : UnicodeString;
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
      s_value := PyObjectAsString(PyTuple_GetItem( err_value, 0));
      err_value := PyTuple_GetItem( err_value, 1);
      if PyTuple_Check( err_value )  and (PyTuple_Size( err_value) >= 4) then
      begin
        i_line_number := PyLong_AsLong(PyTuple_GetItem( err_value, 1));
        i_offset      := PyLong_AsLong(PyTuple_GetItem( err_value, 2));
        s_line := Trim(PyObjectAsString(PyTuple_GetItem( err_value, 3)));
      end;
    end else
    // Is it an instance of the SyntaxError class ?
    if (PyType_IsSubtype(PPyTypeObject(err_type), PPyTypeObject(PyExc_SyntaxError^)) = 1)
       and IsType(err_value, PPyTypeObject(err_type))
    then
    begin
      // Get the filename
      tmp := PyObject_GetAttrString(err_value, 'filename');
      if tmp <> nil then begin
        if PyUnicode_Check(tmp) then
          s_filename := PyUnicodeAsString(tmp)
        else if tmp = Py_None then
          s_filename := '???';
        Py_XDECREF(tmp);
      end;
      // Get the text containing the error, cut of carriage return
      tmp := PyObject_GetAttrString(err_value, 'text');
      if Assigned(tmp) and PyUnicode_Check(tmp) then
        s_line := Trim(PyUnicodeAsString(tmp));
      Py_XDECREF(tmp);
      // Get the offset where the error should appear
      tmp := PyObject_GetAttrString(err_value, 'offset' );
      if Assigned(tmp) and PyLong_Check(tmp) then
        i_offset := PyLong_AsLong(tmp);
      Py_XDECREF(tmp);
      // Get the line number of the error
      tmp := PyObject_GetAttrString(err_value, 'lineno' );
      if Assigned(tmp) and PyLong_Check(tmp) then
        i_line_number := PyLong_AsLong(tmp);
      Py_XDECREF(tmp);
      // Get the message of the error
      tmp := PyObject_GetAttrString(err_value, 'msg' );
      if Assigned(tmp) and PyUnicode_Check(tmp) then
        s_value := PyUnicodeAsString(tmp);
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

  function GetTypeAsString( obj : PPyObject ) : string;
  begin
    if PyType_CheckExact( obj ) then
      Result := string(PPyTypeObject(obj).tp_name)
    else
      Result := PyObjectAsString(obj);
  end;

var
  err_type, err_value : PPyObject;
  s_type        : string;
  s_value       : string;
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
    raise EPythonError.Create('RaiseError: couldn''t fetch last exception');
end;

function TPythonEngine.PyObjectAsString( obj : PPyObject ) : string;
var
  S : PPyObject;
  W : UnicodeString;
begin
  Result := '';
  if not Assigned( obj ) then
    Exit;

  if PyUnicode_Check(obj) then
  begin
    W := PyUnicodeAsString(obj);
    Result := string(W);
    Exit;
  end;
  S := PyObject_Str( obj );
  if Assigned(S) and PyUnicode_Check(S) then
    Result := PyUnicodeAsString(S);
  Py_XDECREF(S);
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
        AddMethod( 'write', pyio_write, 'write(string) -> None' );
        AddMethod( 'read',  pyio_read,  'read() -> string' );
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

function TPythonEngine.EncodeString(const str: UnicodeString): AnsiString; {$IFDEF FPC}overload;{$ENDIF}
begin
  Result := UTF8Encode(str)
end;

{$IFDEF FPC}
function TPythonEngine.EncodeString (const str: AnsiString): AnsiString; overload;
begin
  Result := str;
end;
{$ENDIF}

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

var
  s : AnsiString;
  y, m, d, h, mi, sec, ms, jd, wd : WORD;
  dt : TDateTime;
  dl : Integer;
  wStr : UnicodeString;
  args : PPyObject;
begin
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
    varInteger:  Result := PyLong_FromLong( DeRefV );
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
        Result := PyUnicodeFromString(wStr);
      end;
    varString:
      begin
        s := AnsiString(DeRefV);
        Result := PyBytes_FromStringAndSize(PAnsiChar(s), Length(s));
      end;
    varUString:
      begin
        wStr := DeRefV;
        Result := PyUnicodeFromString(wStr);
      end;
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
      // if we cannot get something useful then
      Result := ReturnNone;
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
      if PyLong_Check(member) then
        Result := PyLong_AsLong(member)
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
          if not PyLong_Check(PyTuple_GetItem(obj, i)) then
            Exit;
        y   := PyLong_AsLong( PyTuple_GetItem(obj, 0) );
        m   := PyLong_AsLong( PyTuple_GetItem(obj, 1) );
        d   := PyLong_AsLong( PyTuple_GetItem(obj, 2) );
        h   := PyLong_AsLong( PyTuple_GetItem(obj, 3) );
        mi  := PyLong_AsLong( PyTuple_GetItem(obj, 4) );
        sec := PyLong_AsLong( PyTuple_GetItem(obj, 5) );
        wd  := PyLong_AsLong( PyTuple_GetItem(obj, 6) );
        jd  := PyLong_AsLong( PyTuple_GetItem(obj, 7) );
        dl  := PyLong_AsLong( PyTuple_GetItem(obj, 8) );
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
  else if PyUnicode_Check(obj) then
    Result := PyUnicodeAsString(obj)
  else if PyBytes_Check(obj) then
    Result := AnsiString(PyBytes_AsString(obj))
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
    vtInteger:       Result := PyLong_FromLong( v.VInteger );
    vtBoolean:       Result := PyLong_FromLong( Integer(v.VBoolean) );
    vtChar:          Result := PyUnicodeFromString(AnsiString(v.VChar));
    vtExtended:      Result := PyFloat_FromDouble( v.VExtended^ );
    vtString:
    begin
      {$IFNDEF ANDROID}
      if Assigned(v.VString) then
        Result := PyUnicodeFromString(AnsiString(v.VString^))
      else
      {$ENDIF}
        Result := PyUnicodeFromString('');
    end;
    vtPChar:         Result := PyUnicodeFromString(AnsiString(v.VPChar));
    vtAnsiString:
    begin
      if Assigned(v.VAnsiString) then
        Result := PyUnicodeFromString(PAnsiChar(v.VAnsiString))
      else
        Result := PyUnicodeFromString('');
    end;
    vtCurrency:      Result := PyFloat_FromDouble( v.VCurrency^ );
    vtVariant:       Result := VariantAsPyObject( v.VVariant^ );
    vtPointer:       Result := v.VPointer;
    vtInt64:         Result := PyLong_FromLongLong( v.VInt64^ );
    vtWideChar:      Result := PyUnicodeFromString(UnicodeString(v.VWideChar));
    vtPWideChar:
      begin
        if Assigned(v.VPWideChar) then
          Result := PyUnicodeFromString(UnicodeString(v.VPWideChar))
        else
          Result := PyUnicodeFromString('');
      end;
    vtWideString:
      begin
        if Assigned(v.VWideString) then
          Result := PyUnicodeFromString(WideString(v.VWideString))
        else
          Result := PyUnicodeFromString('');
      end;
    vtUnicodeString:
      begin
        if Assigned(v.VUnicodeString) then
          Result := PyUnicodeFromString(UnicodeString(v.VUnicodeString))
        else
          Result := PyUnicodeFromString('');
      end;
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
      vtUnicodeString:
      begin
        if Assigned(v.VUnicodeString) then
          Result := AnsiString(UnicodeString(v.VUnicodeString))
        else
          Result := '';
      end;
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
      PyUnicodeFromString(strings.Strings[i]));
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
      PyUnicodeFromString(strings.Strings[i]));
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

function TPythonEngine.PyUnicodeAsString( obj : PPyObject ) : UnicodeString;
var
  _size : Integer;
{$IFDEF POSIX}
  _ucs4Str : UCS4String;
{$ENDIF}
begin
  if PyUnicode_Check(obj) then
  begin
    _size := PyUnicode_GetSize(obj);
    if _size > 0 then
    begin
{$IFDEF POSIX}
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
    raise EPythonError.Create('PyUnicodeAsString expects a Unicode Python object');
end;

function TPythonEngine.PyUnicodeFromString(const AString : UnicodeString) : PPyObject;
{$IFDEF POSIX}
var
  _ucs4Str : UCS4String;
{$ENDIF}
begin
{$IFDEF POSIX}
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
        PyList_SetItem( Result, cpt, PyLong_FromLong(i) );
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

function TPythonEngine.PyUnicodeFromString(const AString: AnsiString): PPyObject;
begin
  Result := PyUnicodeFromString(UnicodeString(AString));
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
begin
  Inc( FAllocatedMethodCount, PYT_METHOD_BUFFER_INCREASE );
  ReAllocMem( FMethods, SizeOf(PyMethodDef)*(FAllocatedMethodCount+1));
  FillChar( FMethods[MethodCount+1] ,SizeOf(PyMethodDef)*PYT_METHOD_BUFFER_INCREASE,0);
end;

function TMethodsContainer.GetMethods( idx : Integer ) : PPyMethodDef;
begin
  if (idx < 0) or (idx > MethodCount) then
    raise Exception.CreateFmt('%s: Index %d out of range', [ClassName, idx]);
  Result := @( FMethods[idx] );
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
      mfWriteRestricted:        flags := PY_WRITE_RESTRICTED;
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
  Result := @( FMembers[idx] );
end;

procedure TMembersContainer.ReallocMembers;
begin
  Inc( FAllocatedMemberCount, PYT_MEMBER_BUFFER_INCREASE );
  ReAllocMem( FMembers, SizeOf(PyMemberDef)*(FAllocatedMemberCount+1));
  FillChar( FMembers[MemberCount+1], SizeOf(PyMemberDef)*PYT_MEMBER_BUFFER_INCREASE,0);
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
  Result := @( FGetSets[idx] );
end;

procedure TGetSetContainer.ReallocGetSets;
begin
  Inc( FAllocatedGetSetCount, PYT_GETSET_BUFFER_INCREASE );
  ReAllocMem( FGetSets, SizeOf(PyGetSetDef)*(FAllocatedGetSetCount+1));
  FillChar( FGetSets[GetSetCount+1], SizeOf(PyGetSetDef)*PYT_GETSET_BUFFER_INCREASE,0);
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

function TError.GetDisplayName: string;
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
        Error := PyUnicodeFromString(Text)
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
        if PyObject_TypeCheck(res, PPyTypeObject(PyExc_Exception^)) then
          begin
            args := PyTuple_New(1);
            if not Assigned(args) then
              raise Exception.Create('TError.RaiseErrorObj: Could not create an empty tuple');
            str := PyUnicodeFromString(msg);
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
            PyUnicodeFromString(CleanString(FDocString.Text, False));
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
      FillChar(FModuleDef, SizeOf(FModuleDef), 0);
      FModuleDef.m_base.ob_refcnt := 1;
      FModuleDef.m_name := PAnsiChar(ModuleName);
      FModuleDef.m_methods := MethodsData;
      FModuleDef.m_size := -1;
      FModule := Py_InitModule( ModuleDef );
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
      PyKey := PyUnicodeFromString(key);
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
    GetPythonEngine.PyUnicodeFromString(Format('<%s at %x>',
        [PythonType.TypeName, NativeInt(self)]));
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

function  TPyObject.NbSubtract( obj : PPyObject) : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbMultiply( obj : PPyObject) : PPyObject;
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

function  TPyObject.NbMatrixMultiply( obj : PPyObject) : PPyObject;
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

function  TPyObject.NbBool : Integer;
begin
  Result := 0;
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

function  TPyObject.NbInt : PPyObject;
begin
  Result := nil;
end;

function  TPyObject.NbFloat : PPyObject;
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

function TPyObject.NbInplaceMatrixMultiply(obj: PPyObject): PPyObject;
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

function  TPyObject.SqAssItem( idx : NativeInt; obj : PPyObject) : Integer;
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
  if tpVectorCall in TypeFlags then
    Result := Result or _Py_TPFLAGS_HAVE_VECTORCALL;
  if tpMethodDescriptor in TypeFlags then
    Result := Result or Py_TPFLAGS_METHOD_DESCRIPTOR;
  if tpHaveVersionTag in TypeFlags then
    Result := Result or Py_TPFLAGS_HAVE_VERSION_TAG;
  if tpValidVersionTag in TypeFlags then
    Result := Result or Py_TPFLAGS_VALID_VERSION_TAG;
  if tpIsAbstract in TypeFlags then
    Result := Result or Py_TPFLAGS_IS_ABSTRACT;
  if tpLongSubclass in TypeFlags then
    Result := Result or Py_TPFLAGS_LONG_SUBCLASS;
  if tpListSubClass in TypeFlags then
    Result := Result or Py_TPFLAGS_LIST_SUBCLASS;
  if tpTupleSubclass in TypeFlags then
    Result := Result or Py_TPFLAGS_TUPLE_SUBCLASS;
  if tpBytesSubclass in TypeFlags then
    Result := Result or Py_TPFLAGS_BYTES_SUBCLASS;
  if tpBaseExcSubclass in TypeFlags then
    Result := Result or Py_TPFLAGS_BASE_EXC_SUBCLASS;
  if tpTypeSubclass in TypeFlags then
    Result := Result or Py_TPFLAGS_TYPE_SUBCLASS;
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

function  TPythonType_NbSubtract( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbSubtract( obj );
end;

function  TPythonType_NbMultiply( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbMultiply( obj );
end;

function  TPythonType_NbFloorDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbFloorDivide( obj );
end;

function  TPythonType_NbTrueDivide( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbTrueDivide( obj );
end;

function  TPythonType_NbMatrixMultiply( pSelf, obj : PPyObject) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbMatrixMultiply( obj );
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

function  TPythonType_NbBool( pSelf : PPyObject ) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbBool;
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

function  TPythonType_NbInt( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInt;
end;

function  TPythonType_NbFloat( pSelf : PPyObject ) : PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbFloat;
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

function  TPythonType_NbInplaceMatrixMultiply(pSelf, obj: PPyObject): PPyObject; cdecl;
begin
  Result := PythonToDelphi(pSelf).NbInplaceMatrixMultiply( obj );
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

function  TPythonType_SqAssItem( pSelf : PPyObject; idx : NativeInt; obj : PPyObject) : Integer; cdecl;
begin
  Result := PythonToDelphi(pSelf).SqAssItem( idx, obj );
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
{ Called from TPythonType.Initialize which first calls CheckEngine - FEngine is alread assigned }
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
      begin
        tp_as_number := @FNumber;

        with FNumber do
        begin
          if nsAdd in Services.Number then nb_add := TPythonType_NbAdd; // #3.1
          if nsSubtract in Services.Number then nb_subtract := TPythonType_NbSubtract; // #3.2
          if nsMultiply in Services.Number then nb_multiply := TPythonType_NbMultiply; // #3.3
          if nsFloorDivide in Services.Number then nb_floor_divide := TPythonType_NbFloorDivide; // #3.30
          if nsTrueDivide in Services.Number then nb_true_divide := TPythonType_NbTrueDivide; // #3.31
          if (nsMatrixMultiply in Services.Number) and ((FEngine.MajorVersion > 3)
            or ((FEngine.MajorVersion = 3) and (FEngine.MinorVersion >= 5)))
          then
              nb_matrix_multiply := TPythonType_NbMatrixMultiply; // #3.35
          if nsRemainder in Services.Number then nb_remainder := TPythonType_NbRemainder;  // #3.4
          if nsDivmod in Services.Number then nb_divmod := TPythonType_NbDivmod; // #3.5
          if nsPower in Services.Number then nb_power := TPythonType_NbPower; // #3.6
          if nsNegative in Services.Number then nb_negative := TPythonType_NbNegative;  // #3.7
          if nsPositive in Services.Number then nb_positive := TPythonType_NbPositive;  // #3.8
          if nsAbsolute in Services.Number then nb_absolute := TPythonType_NbAbsolute;   // #3.9
          if nsBool in Services.Number then nb_bool := TPythonType_NbBool; // #3.10
          if nsInvert in Services.Number then nb_invert := TPythonType_NbInvert;  // #3.11
          if nsLShift in Services.Number then nb_lshift := TPythonType_NbLShift;  // #3.12
          if nsRShift in Services.Number then nb_rshift := TPythonType_NbRShift;  // #3.13
          if nsAnd in Services.Number then nb_and := TPythonType_NbAnd;  // #3.14
          if nsXor in Services.Number then nb_xor := TPythonType_NbXor;  // #3.15
          if nsOr in Services.Number then nb_or := TPythonType_NbOr;  // #3.16
          if nsInt in Services.Number then nb_int := TPythonType_NbInt;  // #3.17
          if nsFloat in Services.Number then nb_float := TPythonType_NbFloat;  // #3.19
          if nsInplaceAdd in Services.InplaceNumber then nb_inplace_add := TPythonType_NbInplaceAdd;  // #3.20
          if nsInplaceSubtract in Services.InplaceNumber then nb_inplace_subtract := TPythonType_NbInplaceSubtract;  // #3.21
          if nsInplaceMultiply in Services.InplaceNumber then nb_inplace_multiply := TPythonType_NbInplaceMultiply;  // #3.22
          if nsInplaceFloorDivide in Services.InplaceNumber then nb_inplace_floor_divide := TPythonType_NbInplaceFloorDivide;  // #3.32
          if nsInplaceTrueDivide in Services.InplaceNumber then nb_inplace_true_divide := TPythonType_NbInplaceTrueDivide;  // #3.33
          if nsInplaceRemainder in Services.InplaceNumber then nb_inplace_remainder := TPythonType_NbInplaceRemainder; // #3.23
          if nsInplacePower in Services.InplaceNumber then nb_inplace_power := TPythonType_NbInplacePower;  // #3.24
          if nsInplaceLShift in Services.InplaceNumber then nb_inplace_lshift := TPythonType_NbInplaceLShift;  // #3.25
          if nsInplaceRShift in Services.InplaceNumber then nb_inplace_rshift := TPythonType_NbInplaceRShift;  // #3.26
          if nsInplaceAnd in Services.InplaceNumber then nb_inplace_and := TPythonType_NbInplaceAnd;  // #3.27
          if nsInplaceXor in Services.InplaceNumber then nb_inplace_xor := TPythonType_NbInplaceXor;  // #3.28
          if nsInplaceOr in Services.InplaceNumber then nb_inplace_or := TPythonType_NbInplaceOr;  // #3.29
          if (nsInplaceMatrixMultiply in Services.InplaceNumber) and
            ((FEngine.MajorVersion > 3) or ((FEngine.MajorVersion = 3)
             and (FEngine.MinorVersion >= 5)))
          then
              nb_inplace_matrix_multiply := TPythonType_NbInplaceMatrixMultiply; // #3.36
        end;
      end;

      // Sequence services
      if Services.Sequence <> [] then
      begin
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
          if ssAssItem in Services.Sequence then
            sq_ass_item := TPythonType_SqAssItem;
          if ssContains in Services.Sequence then
            sq_contains := TPythonType_SqContains;
          if ssInplaceConcat in Services.Sequence then
            sq_inplace_concat  := TPythonType_SqInplaceConcat;
          if ssInplaceRepeat in Services.Sequence then
            sq_inplace_repeat  := TPythonType_SqInplaceRepeat;
        end;
      end;

      // Mapping services
      if Services.Mapping <> [] then
      begin
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
      FCreateFunc := Engine.PyCFunction_NewEx(@FCreateFuncDef, nil, nil)
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

destructor TPythonDelphiVar.Destroy;
begin
  inherited;
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
            (t = varUString) or
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
          PyUnicodeFromString(Format('<%s: %s>',
            [PythonType.TypeName, PyObjectAsString(obj)]));
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
        if RedirectIO and (IO <> nil) and (not IO.InheritsFrom(TPythonInputOutput)) and not IO.DelayWrites then
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
              IO.Write(PyUnicodeAsString(a1))
            else
              IO.Write(IOString(PyObjectAsString(a1)));
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
                Result := PyUnicodeFromString(Widetxt);
            end else begin
              txt := IO.ReceiveData;
              if PyErr_Occurred <> nil then
                Result := nil
              else
                Result := PyUnicodeFromString(txt);
            end
          else
            Result := PyUnicodeFromString(txt);
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
        PyTuple_SetItem( Result, 0, PyUnicodeFromString(T.TypeName));
        PyTuple_SetItem( Result, 1, PyLong_FromLong(T.InstanceCount) );
        PyTuple_SetItem( Result, 2, PyLong_FromLong(T.CreateHits) );
        PyTuple_SetItem( Result, 3, PyLong_FromLong(T.DeleteHits) );
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
  if not gPythonEngine.Finalizing and not gPythonEngine.Initialized then
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

function GetPythonVersionFromDLLName(const DLLFileName : string): string;
begin
  Result := DLLFileName[{$IFDEF MSWINDOWS}7{$ELSE}10{$ENDIF}] + '.' + DLLFileName[{$IFDEF MSWINDOWS}8{$ELSE}11{$ENDIF}];
end;

procedure MaskFPUExceptions(ExceptionsMasked : boolean;
  MatchPythonPrecision : Boolean);
begin
  {$IF Defined(CPUX86) or Defined(CPUX64)}
  if ExceptionsMasked then
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
      exOverflow, exUnderflow, exPrecision])
  else
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
  {$WARN SYMBOL_PLATFORM OFF}
  {$IF Defined(FPC) or Defined(MSWINDOWS)}
  if MatchPythonPrecision then
      SetPrecisionMode(pmDouble)
    else
      SetPrecisionMode(pmExtended);
  {$WARN SYMBOL_PLATFORM ON}
  {$IFEND}
  {$IFEND}
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

function CleanString(const s : UnicodeString; AppendLF : Boolean) : UnicodeString;
begin
  {$IFDEF FPC}
  Result := UnicodeString(AdjustLineBreaks(AnsiString(s), tlbsLF));
  {$ELSE}
  Result := AdjustLineBreaks(s, tlbsLF);
  {$ENDIF}
  if AppendLF and (result[length(result)] <> LF) then
    Result := Result + LF;
end;

end.

