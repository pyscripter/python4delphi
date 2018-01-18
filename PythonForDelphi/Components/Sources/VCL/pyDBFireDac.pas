{$REGION 'Licence'}
{
Very important: We need text buffer for AnsiString's
==============================================================================}
{$ENDREGION}
unit pyDBFireDac;

interface

uses
  Classes, SysUtils, Variants, PythonEngine, WrapDelphi, WrapDelphiClasses,
  Windows, System.AnsiStrings, System.Rtti, DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  FireDAC.Comp.UI, FireDAC.Stan.ExprFuncs;

type

TPyDBDataset = class;

TCallBackSplit = record
  method: Pointer;
  self:   TObject;
end;

TPyDBSharedObject = class
protected
  FRefCount : Integer;
  FData : TObject;
  FOwner : Boolean;
  procedure DoDecRef;
  procedure SetData( value : TObject );
public
  constructor Create;
  constructor CreateWith( value : TObject; isOwner : Boolean );
  destructor Destroy; override;
  procedure IncRef;
  procedure DecRef;
  procedure FreeData;
  property Data:  TObject read FData  write SetData;
  property Owner: Boolean read FOwner write FOwner;
end;

TPyDBCommon = class(TPyObject)
  function  GetProperties : PPyObject;
  procedure AppendProperties( List : PPyObject ); virtual;
  procedure AppendProp( List : PPyObject; const prop : String );
  function  GetAttr(key : PAnsiChar) : PPyObject; override;
  procedure RaiseDBError( E : Exception );
  function  EventBelongsToObject( Event : TCallbackSplit ) : Boolean;
end;

TPyDBField = class(TPyDBCommon)
private
  m_arAnsiTextBuf: TArray<AnsiString>;
public
  FField        : TField;
  FOnChange     : PPyObject;
  FOnGetText    : PPyObject;
  FOnSetText    : PPyObject;
  FOnValidate   : PPyObject;
  constructor Create( APythonType : TPythonType ); override;
  constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;
  //
  class var PyDBFieldType: TPythonType;
  //
  destructor  Destroy; override;
  function  GetAttr(key : PAnsiChar) : PPyObject; override;
  function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
  function  Repr : PPyObject; override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  function  CheckField : Boolean;
  procedure AppendProperties( List : PPyObject ); override;
  // Do-Methods
  function Do_Clear( args : PPyObject ) : PPyObject; cdecl;
  function Do_FocusControl( args : PPyObject ) : PPyObject; cdecl;
  function Do_IsValidChar( args : PPyObject ) : PPyObject; cdecl;
  function Do_RefreshLookupList( args : PPyObject ) : PPyObject; cdecl;
  // Events
  procedure OnChange( Sender : TField );
  procedure OnGetText( Sender: TField; var Text: String; DisplayText: Boolean );
  procedure OnSetText( Sender : TField; const Text: String );
  procedure OnValidate( Sender : TField );
end;

TPyDBVarArg = class(TPyObject)
public
  FValue : PPyObject;
  constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
  destructor Destroy; override;
  //
  class var PyDBVarArgType: TPythonType;
  //
  function  GetAttr(key : PAnsiChar) : PPyObject; override;
  function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
  function  Repr : PPyObject; override;
end;

{
TPyDSRowsRegistration = class(TRegisteredUnit)
public
  function Name(): String; override;
  procedure RegisterWrappers(aPyDelphiWrapper: TPyDelphiWrapper); override;
  procedure DefineVars(aPyDelphiWrapper: TPyDelphiWrapper); override;
end;
}

TDSRowsAccess = class(TContainerAccess)
private
  function  GetContainer: TPyDBDataset;
public
  function GetItem(AIndex : Integer) : PPyObject; override;
  function GetSize : Integer; override;
  function IndexOf(AValue : PPyObject) : Integer; override;

  class function ExpectedContainerClass : TClass; override;
  class function SupportsIndexOf : Boolean; override;
  class function Name : String; override;
end;

{DAV>>> $M+ <<<DAV}   // <- Generation of runtime type information (RTTI)
TPyDBDataset = class (TPyDelphiPersistent)
private
  function  GetDelphiObject: TFDDataset;
  procedure SetDelphiObject(const Value: TFDDataset);
protected
  m_arAnsiTextBuf: TArray<AnsiString>;
  // Methods
  procedure RaiseDBError( E : Exception );
public
  FSharedObject:    TPyDBSharedObject;
  constructor Create( aPythonType: TPythonType ); override;
  constructor CreateWith( aPythonType : TPythonType; args : PPyObject ); override;
  destructor  Destroy; override;
  //
  function  CreateContainerAccess : TContainerAccess; override;
  //
  // Class methodes
  class function  DelphiObjectClass : TClass; override;
  class function  GetContainerAccessClass : TContainerAccessClass; override;
  class procedure RegisterGetSets( PythonType : TPythonType ); override;
  class procedure RegisterMembers( PythonType : TPythonType ); override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  //
  // Property Getters
  function Get_RowsCount( AContext : Pointer) : PPyObject; cdecl;
  function Get_Rows( AContext : Pointer) : PPyObject; cdecl;
  //
  // Attributes
  function  GetAttr(key : PAnsiChar) : PPyObject; override;
  function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
  //
  // Methods
  function Do_Fields( args : PPyObject ): PPyObject; cdecl;
  function Do_FieldByName( args : PPyObject ): PPyObject; cdecl;
  function Do_FieldNamesAsTuple( args: PPyObject): PPyObject; cdecl;
  function Do_FieldsAsTuple( args: PPyObject): PPyObject; cdecl;
  function Do_FieldsAsDict( args: PPyObject): PPyObject; cdecl;
  function Do_First( args : PPyObject ) : PPyObject; cdecl;
  function Do_Last( args : PPyObject ) : PPyObject; cdecl;
  function Do_Next( args : PPyObject ) : PPyObject; cdecl;
  function Do_Prior( args : PPyObject ) : PPyObject; cdecl;
  function Do_Locate( args : PPyObject ) : PPyObject; cdecl;
  function Do_Lookup( args : PPyObject ) : PPyObject; cdecl;
  function Do_Refresh( args : PPyObject ) : PPyObject; cdecl;
  function Do_FindKey( args : PPyObject ) : PPyObject; cdecl;
  function Do_FindNearest( args : PPyObject ) : PPyObject; cdecl;
  //
  property DelphiObject: TFDDataset read GetDelphiObject write SetDelphiObject;
end;

{DAV>>> $M+ <<<DAV}   // <- Generation of runtime type information (RTTI)
TPyDBTable = class (TPyDBDataset)
private
  function CheckActiveDBTable(aMustOpen: Boolean): Boolean;
protected
  // Methods
  //-- function Get_FieldNamesAsTuple( args: PPyObject): PPyObject; overload; cdecl;
public
  constructor Create( aPythonType: TPythonType ); override;
  constructor CreateWith( aPythonType : TPythonType; args : PPyObject ); override;
  destructor  Destroy; override;
  //
  class var PyDBTableType:  TPythonType;
  //
  function  GetDelphiObject: TFDTable;
  procedure SetDelphiObject(const Value: TFDTable);
  //
  class function  DelphiObjectClass : TClass; override;
  class procedure RegisterGetSets( PythonType : TPythonType ); override;
  class procedure RegisterMembers( PythonType : TPythonType ); override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  //
  // Attributes
  function  GetAttr(key : PAnsiChar) : PPyObject; override;
  function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
  //
  // Methods
  function Do_Open( args : PPyObject ) : PPyObject; cdecl;
  function Do_Close( args : PPyObject ) : PPyObject; cdecl;
  function Do_Edit( args : PPyObject ) : PPyObject; cdecl;
  function Do_Insert( args : PPyObject ) : PPyObject; cdecl;
  function Do_Append( args : PPyObject ) : PPyObject; cdecl;
  function Do_Post( args : PPyObject ) : PPyObject; cdecl;
  function Do_Cancel( args : PPyObject ) : PPyObject; cdecl;
  function Do_Delete( args : PPyObject ) : PPyObject; cdecl;
  function Do_SetRangeStart( args : PPyObject ) : PPyObject; cdecl;
  function Do_EditRangeStart( args : PPyObject ) : PPyObject; cdecl;
  function Do_SetRangeEnd( args : PPyObject ) : PPyObject; cdecl;
  function Do_EditRangeEnd( args : PPyObject ) : PPyObject; cdecl;
  function Do_ApplyRange( args : PPyObject ) : PPyObject; cdecl;
  function Do_SetRange( args : PPyObject ) : PPyObject; cdecl;
  function Do_CancelRange( args : PPyObject ) : PPyObject; cdecl;
  function Do_GetIndexNames( args : PPyObject ) : PPyObject; cdecl;
  //
  property DelphiObject: TFDTable read GetDelphiObject write SetDelphiObject;
end;

{DAV>>> $M+ <<<DAV}   // <- Generation of runtime type information (RTTI)
TPyDBQuery = class (TPyDBDataset)
private
  function CheckActiveDBQuery(aMustOpen: Boolean): Boolean;
protected
public
  constructor Create( aPythonType: TPythonType ); override;
  constructor CreateWith( aPythonType : TPythonType; args : PPyObject ); override;
  destructor  Destroy; override;
  //
  class var PyDBQueryType:  TPythonType;
  //
  function  GetDelphiObject: TFDQuery;
  procedure SetDelphiObject(const Value: TFDQuery);
  //
  class function  DelphiObjectClass : TClass; override;
  class procedure RegisterGetSets( PythonType : TPythonType ); override;
  class procedure RegisterMembers( PythonType : TPythonType ); override;
  class procedure RegisterMethods( PythonType : TPythonType ); override;
  //
  // Attributes
  function  GetAttr(key : PAnsiChar) : PPyObject; override;
  function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
  //
  // Methods
  function Do_Open( args : PPyObject ) : PPyObject; cdecl;
  function Do_Close( args : PPyObject ) : PPyObject; cdecl;
  function Do_Prepare( args : PPyObject ) : PPyObject; cdecl;
  function Do_Unprepare( args : PPyObject ) : PPyObject; cdecl;
  function Do_ExecSQL( args : PPyObject ) : PPyObject; cdecl;
  //
  property DelphiObject: TFDQuery read GetDelphiObject write SetDelphiObject;
end;

// Global functions
procedure SetEvent( var EventSlot : PPyObject; Event : PPyObject; const EventName, ClassName : String );
procedure ClearEvent( var EventSlot : PPyObject );
function  ReturnEvent( Event : PPyObject ) : PPyObject;
procedure ExecuteEvent( Event : PPyObject; Args : array of Const );
function _PAnsiChar(const aStr: String; var aAnsiStrBuf: AnsiString): PAnsiChar;

// Global variables
var
  g_oDBModule:      TPythonModule = Nil;


  implementation

// ----------------------- Global functions ------------------------------------

procedure SetEvent( var EventSlot : PPyObject; Event : PPyObject; const EventName, ClassName : String );
begin
 with GetPythonEngine do begin
   if Assigned(Event) and not (Event = Py_None) then begin
     if PyFunction_Check(Event) or PyMethod_Check(Event) then begin
       EventSlot := Event;
       Py_IncRef(EventSlot);
     end
     else
       raise Exception.CreateFmt( 'Event %s of class %s needs a function or a method', [EventName, ClassName] );
   end
   else begin
     Py_XDecRef(EventSlot);
     EventSlot := nil;
   end;
 end;
end;

procedure ClearEvent( var EventSlot : PPyObject );
begin
 with GetPythonEngine do
   Py_XDecRef( EventSlot );
 EventSlot := nil;
end;

function ReturnEvent( Event : PPyObject ) : PPyObject;
begin
 with GetPythonEngine do begin
   if Assigned(Event) then begin
     Result := Event;
     Py_IncRef(Event);
    end
    else
      Result := ReturnNone;
 end;
end;

procedure ExecuteEvent( Event : PPyObject; Args : array of Const );
var
 L: PPyObject;
 R: PPyObject;
begin
 R := nil;
 if not Assigned(Event) then
   Exit;
 with GetPythonEngine do begin
   L := ArrayToPyTuple( Args );
   try
     R := PyEval_CallObjectWithKeywords( Event, L, nil );
   finally
     Py_XDecRef(R);
     Py_XDecRef(L);
   end;
   CheckError;
 end;
end;

function _PAnsiChar(const aStr: String; var aAnsiStrBuf: AnsiString): PAnsiChar;
begin
 if Length(aStr) > 0 then begin
   aAnsiStrBuf := AnsiString(aStr);
   Result      := PAnsiChar(aAnsiStrBuf);
 end
 else begin
   aAnsiStrBuf := '';
   Result      := Nil;
 end;
end;

// ----------------------- TPyDBSharedObject -----------------------------------

procedure TPyDBSharedObject.DoDecRef;
begin
 if FRefCount > 0 then
   Dec(FRefCount);
 if (FRefCount = 0) then
   FreeData;
end;

procedure TPyDBSharedObject.SetData( value : TObject );
begin
 DoDecRef;
 FRefCount := 0;
 FData := value;
 IncRef;
end;

constructor TPyDBSharedObject.Create;
begin
 inherited;
 Owner := True;
end;

constructor TPyDBSharedObject.CreateWith( value : TObject; isOwner : Boolean );
begin
 inherited;
 Data := value;
 Owner := isOwner;
end;

destructor TPyDBSharedObject.Destroy;
begin
 FreeData;
 inherited;
end;

procedure TPyDBSharedObject.IncRef;
begin
 Inc(FRefCount);
end;

procedure TPyDBSharedObject.DecRef;
begin
 if not Assigned(Self) then
   Exit;
 DoDecRef;
 if (FRefCount = 0) then
   Free;
end;

procedure TPyDBSharedObject.FreeData;
begin
 if Owner and Assigned(FData) then
   FData.Free;
 FData := nil;
end;

// ----------------------- TPyDBCommon -----------------------------------------

function  TPyDBCommon.GetProperties : PPyObject;
begin
 with GetPythonEngine do begin
   Result := PyList_New(0);
   AppendProperties( Result );
   PyList_Sort(Result);
 end;
end;

procedure TPyDBCommon.AppendProperties( List : PPyObject );
begin
 // Override this method in the subclasses and add your
 // properties to the list
end;

procedure TPyDBCommon.AppendProp( List : PPyObject; const prop : String );
var
 obj : PPyObject;
begin
 with GetPythonEngine do begin
   obj := PyString_FromString(PAnsiChar(AnsiString(prop)));
   PyList_Append( List, obj );
   Py_XDecRef(obj);
 end;
end;

function  TPyDBCommon.GetAttr(key : PAnsiChar) : PPyObject;
begin
 try
   if (CompareText( String(key), '__properties__' ) = 0) or
      (CompareText( String(key), '__members__' ) = 0) then
     Result := GetProperties
   else
     Result := inherited GetAttr(key);
 except // Remap Delphi exception to a Python exception
   on E : Exception do begin
     RaiseDBError( E );
     Result := nil;
   end;
 end;
end;

procedure TPyDBCommon.RaiseDBError( E : Exception );
begin
 if GetModule <> nil then
   GetModule.RaiseError( AnsiString('DBError'), AnsiString(E.Message) );
end;

function TPyDBCommon.EventBelongsToObject( Event : TCallbackSplit ) : Boolean;
begin
 Result := Event.Self = Self;
end;

// ----------------------- TPyDBField ------------------------------------------

constructor TPyDBField.Create( APythonType : TPythonType );
begin
 inherited;
 SetLength(m_arAnsiTextBuf,0);
end;

constructor TPyDBField.CreateWith( PythonType : TPythonType; args : PPyObject );
begin
 inherited;
 SetLength(m_arAnsiTextBuf,0);
end;

destructor  TPyDBField.Destroy;
begin
 SetLength(m_arAnsiTextBuf,0);
 if Assigned(FField) then begin
   if EventBelongsToObject( TCallbackSplit(FField.OnChange) ) then
     FField.OnChange   := nil;
   if EventBelongsToObject( TCallbackSplit(FField.OnGetText) ) then
     FField.OnGetText  := nil;
   if EventBelongsToObject( TCallbackSplit(FField.OnSetText) ) then
     FField.OnSetText  := nil;
   if EventBelongsToObject( TCallbackSplit(FField.OnValidate) ) then
     FField.OnValidate := nil;
 end;
 FField := nil;
 ClearEvent( FOnChange );
 ClearEvent( FOnGetText );
 ClearEvent( FOnSetText );
 ClearEvent( FOnValidate );
 inherited;
end;

// Then we override the needed services

function  TPyDBField.GetAttr(key : PAnsiChar) : PPyObject;
var
 l_pAnsiChar: PAnsiChar;
 l_sUpperKey, l_sStr: String;
begin
 with GetPythonEngine do begin
   if not CheckField then begin
     Result := nil;
     Exit;
   end;
   try
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'ALIGNMENT' ) = 0 then
       Result := VariantAsPyObject( Integer(FField.Alignment) )
     else if CompareText(l_sUpperKey, 'ASBOOLEAN' ) = 0 then
       Result := VariantAsPyObject( FField.AsBoolean )
     else if CompareText(l_sUpperKey, 'ASDATETIME' ) = 0 then
       Result := VariantAsPyObject( FField.AsDateTime )
     else if CompareText(l_sUpperKey, 'ASFLOAT' ) = 0 then
       Result := VariantAsPyObject( FField.AsFloat )
     else if CompareText(l_sUpperKey, 'ASINTEGER' ) = 0 then
       Result := VariantAsPyObject( FField.AsInteger )
     else if CompareText(l_sUpperKey, 'ASSTRING' ) = 0 then
       Result := VariantAsPyObject( FField.AsString )
     else if CompareText(l_sUpperKey, 'CANMODIFY' ) = 0 then
       Result := VariantAsPyObject( FField.CanModify )
     else if CompareText(l_sUpperKey, 'ConstraintErrorMessage' ) = 0 then
       Result := VariantAsPyObject( FField.ConstraintErrorMessage )
     else if CompareText(l_sUpperKey, 'CurValue' ) = 0 then
       Result := VariantAsPyObject( FField.CurValue )
     else if CompareText(l_sUpperKey, 'CustomConstraint' ) = 0 then
       Result := VariantAsPyObject( FField.CustomConstraint )
     else if CompareText(l_sUpperKey, 'DATASIZE' ) = 0 then
       Result := VariantAsPyObject( FField.DataSize )
     else if CompareText(l_sUpperKey, 'DATATYPE' ) = 0 then
       Result := VariantAsPyObject( FField.DataType )
     else if CompareText(l_sUpperKey, 'DefaultExpression' ) = 0 then
       Result := VariantAsPyObject( FField.DefaultExpression )
     else if CompareText(l_sUpperKey, 'DISPLAYLABEL' ) = 0 then
       Result := VariantAsPyObject( FField.DisplayLabel )
     else if CompareText(l_sUpperKey, 'DISPLAYNAME' ) = 0 then
       Result := VariantAsPyObject( FField.DisplayName )
     else if CompareText(l_sUpperKey, 'DISPLAYTEXT' ) = 0 then
       Result := VariantAsPyObject( FField.DisplayText )
     else if CompareText(l_sUpperKey, 'DISPLAYWIDTH' ) = 0 then
       Result := VariantAsPyObject( FField.DisplayWidth )
     else if CompareText(l_sUpperKey, 'EDITMASK' ) = 0 then
       Result := VariantAsPyObject( FField.EditMask )
     else if CompareText(l_sUpperKey, 'FIELDKIND' ) = 0 then
       Result := VariantAsPyObject( FField.FieldKind )
     else if CompareText(l_sUpperKey, 'FIELDNAME' ) = 0 then
       Result := VariantAsPyObject( FField.FieldName)
     else if CompareText(l_sUpperKey, 'FIELDNO' ) = 0 then
       Result := VariantAsPyObject( FField.FieldNo )
     else if CompareText(l_sUpperKey, 'HASCONSTRAINTS' ) = 0 then
       Result := VariantAsPyObject( FField.HasConstraints )
     else if CompareText(l_sUpperKey, 'IMPORTEDCONSTRAINT' ) = 0 then
       Result := VariantAsPyObject( FField.ImportedConstraint )
     else if CompareText(l_sUpperKey, 'INDEX' ) = 0 then
       Result := VariantAsPyObject( FField.Index )
     else if CompareText(l_sUpperKey, 'ISBLOB' ) = 0 then
       Result := VariantAsPyObject( FField.IsBlob)
     else if CompareText(l_sUpperKey, 'ISINDEXFIELD' ) = 0 then
       Result := VariantAsPyObject( FField.IsIndexField )
     else if CompareText(l_sUpperKey, 'ISNULL' ) = 0 then
       Result := VariantAsPyObject( FField.IsNull )
     else if CompareText(l_sUpperKey, 'KEYFFIELDS' ) = 0 then
       Result := VariantAsPyObject( FField.KeyFields )
     else if CompareText(l_sUpperKey, 'LOOKUP' ) = 0 then
       Result := VariantAsPyObject( FField.Lookup )
     else if CompareText(l_sUpperKey, 'LOOKUPCACHE' ) = 0 then
       Result := VariantAsPyObject( FField.LookupCache )
     else if CompareText(l_sUpperKey, 'LOOKUPDATASET' ) = 0 then
       Result := ReturnNone
     else if CompareText(l_sUpperKey, 'LOOKUPKeyFields' ) = 0 then
       Result := VariantAsPyObject( FField.LookupKeyFields )
     else if CompareText(l_sUpperKey, 'LOOKUPLIST' ) = 0 then
       Result := ReturnNone
     else if CompareText(l_sUpperKey, 'LOOKUPRESULTFIELD' ) = 0 then
       Result := VariantAsPyObject( FField.LookupResultField )
     else if CompareText(l_sUpperKey, 'NEWVALUE' ) = 0 then
       Result := VariantAsPyObject( FField.NewValue )
     else if CompareText(l_sUpperKey, 'OFFSET' ) = 0 then
       Result := VariantAsPyObject( FField.Offset )
     else if CompareText(l_sUpperKey, 'OLDVALUE' ) = 0 then
       Result := VariantAsPyObject( FField.OldValue )
     else if CompareText(l_sUpperKey, 'ORIGIN' ) = 0 then
       Result := VariantAsPyObject( FField.Origin )
     else if CompareText(l_sUpperKey, 'READONLY' ) = 0 then
       Result := VariantAsPyObject( FField.ReadOnly )
     else if CompareText(l_sUpperKey, 'REQUIRED' ) = 0 then
       Result := VariantAsPyObject( FField.Required )
     else if CompareText(l_sUpperKey, 'SIZE' ) = 0 then
       Result := VariantAsPyObject( FField.Size )
     else if CompareText(l_sUpperKey, 'TEXT' ) = 0 then
       Result := VariantAsPyObject( FField.Text )
     else if CompareText(l_sUpperKey, 'VALIDCHARS' ) = 0 then
       Result := ReturnNone
     else if CompareText(l_sUpperKey, 'VALUE' ) = 0 then begin
       if (FField.DataType = ftString) or (FField.DataType = ftWideString) then begin
         SetLength(m_arAnsiTextBuf,1);
         l_sStr := FField.AsString;
         l_pAnsiChar := _PAnsiChar(l_sStr, m_arAnsiTextBuf[0]);
         Result := PyString_FromString( l_pAnsiChar );
       end
       else
         Result := VariantAsPyObject( FField.Value );
     end
     else if CompareText(l_sUpperKey, 'VISIBLE' ) = 0 then
       Result := VariantAsPyObject( FField.Visible )
     else if CompareText(l_sUpperKey, 'ONCHANGE' ) = 0 then
       Result := ReturnEvent( FOnChange )
     else if CompareText(l_sUpperKey, 'ONGETTEXT' ) = 0 then
       Result := ReturnEvent( FOnGetText )
     else if CompareText(l_sUpperKey, 'ONSETTEXT' ) = 0 then
       Result := ReturnEvent( FOnSetText )
     else if CompareText(l_sUpperKey, 'ONVALIDATE' ) = 0 then
       Result := ReturnEvent( FOnValidate )
     else
       Result := inherited GetAttr(key);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function  TPyDBField.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
var
 l_sUpperKey: String;
begin
 Result := -1;
 with GetPythonEngine do begin
   if not CheckField then
     Exit;
   try
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'ALIGNMENT' ) = 0 then begin
       FField.Alignment := TAlignment(PyObjectAsVariant( value ));
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ASBOOLEAN' ) = 0 then begin
       FField.AsBoolean := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ASDATETIME' ) = 0 then begin
       FField.AsDateTime := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ASFLOAT' ) = 0 then begin
       FField.AsFloat := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ASINTEGER' ) = 0 then begin
       FField.AsInteger := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ASSTRING' ) = 0 then begin
       FField.AsString := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'CANMODIFY' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'CONSTRAINTERRORMESSAGE' ) = 0 then begin
       FField.ConstraintErrorMessage := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'CURVALUE' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'CUSTOMCONSTRAINT' ) = 0 then begin
       FField.CustomConstraint := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DATASIZE' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DATATYPE' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DEFAULTEXPRESSION' ) = 0 then begin
       FField.DefaultExpression := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DISPLAYLABEL' ) = 0 then begin
       FField.DisplayLabel := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DISPLAYNAME' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DISPLAYTEXT' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'DISPLAYWIDTH' ) = 0 then begin
       FField.DisplayWidth := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'EDITMASK' ) = 0 then begin
       FField.EditMask := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'FIELDKIND' ) = 0 then begin
       FField.FieldKind := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'FIELDNAME' ) = 0 then begin
       FField.FieldName:= PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'FIELDNO' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'HASCONSTRAINTS' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'IMPORTEDCONSTRAINT' ) = 0 then begin
       FField.ImportedConstraint := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'INDEX' ) = 0 then begin
       FField.Index := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ISBLOB' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ISINDEXFIELD' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ISNULL' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'KEYFIELDS' ) = 0 then begin
       FField.KeyFields := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'LOOKUP' ) = 0 then begin
       FField.Lookup := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'LOOKUPCACHE' ) = 0 then begin
       FField.LookupCache := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'LOOKUPDATASET' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'LOOKUPKEYFIELDS' ) = 0 then begin
       FField.LookupKeyFields := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'LOOKUPLIST' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'LOOKUPRESULTFIELD' ) = 0 then begin
       FField.LookupResultField := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'NEWVALUE' ) = 0 then begin
       FField.NewValue := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'OFFSET' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'OLDVALUE' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ORIGIN' ) = 0 then begin
       FField.Origin := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'READONLY' ) = 0 then begin
       FField.ReadOnly := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'REQUIRED' ) = 0 then begin
       FField.Required := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'SIZE' ) = 0 then begin
       FField.Size := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'TEXT' ) = 0 then begin
       FField.Text := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'VALIDCHARS' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'VALUE' ) = 0 then begin
       FField.Value := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'VISIBLE' ) = 0 then begin
       FField.Visible := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ONCHANGE' ) = 0 then begin
       SetEvent( FOnChange, Value, 'OnChange', 'TField' );
       if Assigned(FOnChange) then
         FField.OnChange := OnChange
       else
         FField.OnChange := nil;
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ONGETTEXT' ) = 0 then begin
       SetEvent( FOnGetText, Value, 'OnGetText', 'TField' );
       if Assigned(FOnGetText) then
         FField.OnGetText := OnGetText
       else
         FField.OnGetText := nil;
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ONSETTEXT' ) = 0 then begin
       SetEvent( FOnSetText, Value, 'OnSetText', 'TField' );
       if Assigned(FOnSetText) then
         FField.OnSetText := OnSetText
       else
         FField.OnSetText := nil;
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ONVALIDATE' ) = 0 then begin
       SetEvent( FOnValidate, Value, 'OnValidate', 'TField' );
       if Assigned(FOnValidate) then
         FField.OnValidate := OnValidate
       else
         FField.OnValidate := nil;
       Result := 0;
     end
     else
       Result := inherited SetAttr(key, value);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := -1;
     end;
   end;
 end;
end;

function  TPyDBField.Repr: PPyObject;
begin
 Result := inherited Repr;
end;

class procedure TPyDBField.RegisterMethods( PythonType : TPythonType );
begin
 inherited;
 with PythonType do begin
   AddMethod(AnsiString('Clear'), @TPyDBField.Do_Clear,
             AnsiString('DBField.Clear() -> None') );
   AddMethod(AnsiString('FocusControl'), @TPyDBField.Do_FocusControl,
             AnsiString('DBField.FocusControl() -> None') );
   AddMethod(AnsiString('IsValidChar'), @TPyDBField.Do_IsValidChar,
             AnsiString('DBField.IsValidChar( InputChar : String ) -> True or False') );
   AddMethod(AnsiString('RefreshLookupList'), @TPyDBField.Do_RefreshLookupList,
             AnsiString('DBField.RefreshLookupList() -> None') );
 end;
end;

function TPyDBField.CheckField : Boolean;
begin
 if not Assigned(FField) then begin
   Result := False;
   with GetPythonEngine do
     PyErr_SetString (PyExc_RuntimeError^, PAnsiChar('No field defined !') );
 end
 else
   Result := True;
 Result := Result;
end;

procedure TPyDBField.AppendProperties( List : PPyObject );
begin
 inherited;
 AppendProp( List, 'Alignment');
 AppendProp( List, 'AsBoolean');
 AppendProp( List, 'AsDateTime');
 AppendProp( List, 'AsFloat');
 AppendProp( List, 'AsInteger');
 AppendProp( List, 'AsString');
 AppendProp( List, 'CanModify');
 AppendProp( List, 'ConstraintErrorMessage');
 AppendProp( List, 'CurValue');
 AppendProp( List, 'CustomConstraint');
 AppendProp( List, 'DataSize');
 AppendProp( List, 'DataType');
 AppendProp( List, 'DefaultExpression');
 AppendProp( List, 'DisplayLabel');
 AppendProp( List, 'DisplayName');
 AppendProp( List, 'DisplayText');
 AppendProp( List, 'DisplayWidth');
 AppendProp( List, 'EditMask');
 AppendProp( List, 'FieldKind');
 AppendProp( List, 'FieldName');
 AppendProp( List, 'FieldNo');
 AppendProp( List, 'HasConstraints');
 AppendProp( List, 'ImportedConstraint');
 AppendProp( List, 'Index');
 AppendProp( List, 'IsBlob');
 AppendProp( List, 'IsIndexField');
 AppendProp( List, 'IsNull');
 AppendProp( List, 'KeyFields');
 AppendProp( List, 'Lookup');
 AppendProp( List, 'LookupCache');
 AppendProp( List, 'LookupDataset');
 AppendProp( List, 'LookupKeyFields');
 AppendProp( List, 'LookupList');
 AppendProp( List, 'LookupResultField');
 AppendProp( List, 'NewValue');
 AppendProp( List, 'Offset');
 AppendProp( List, 'OldValue');
 AppendProp( List, 'Origin');
 AppendProp( List, 'ReadOnly');
 AppendProp( List, 'Required');
 AppendProp( List, 'Size');
 AppendProp( List, 'Text');
 AppendProp( List, 'ValidChars');
 AppendProp( List, 'Value');
 AppendProp( List, 'Visible');
end;

function TPyDBField.Do_Clear( args : PPyObject ) : PPyObject;
begin
 with GetPythonEngine do begin
   Adjust(@Self);  // <- adjust the transmitted self argument
   try
     if CheckField then begin
       FField.Clear;
       Result := ReturnNone;
     end
     else
       Result := nil;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBField.Do_FocusControl( args : PPyObject ) : PPyObject;
begin
 with GetPythonEngine do begin
   Adjust(@Self);  // <- adjust the transmitted self argument
   try
     if CheckField then begin
       FField.FocusControl;
       Result := ReturnNone;
     end
     else
       Result := nil;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBField.Do_IsValidChar( args : PPyObject ) : PPyObject;
var
 c : Char;
 s : PAnsiChar;
 str : AnsiString;
begin
 with GetPythonEngine do begin
   Adjust(@Self);   // <- adjust the transmitted self argument
   try
     if CheckField then begin
       if PyArg_ParseTuple( args, 's:TField.IsValidChar',@s ) <> 0 then begin
         str := s;
         if Length(str) > 0 then
           c := Char(str[1])
         else
           c := #0;
         Result := VariantAsPyObject( FField.IsValidChar( c ) )
       end
       else
         Result := nil;
     end
     else
       Result := nil;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBField.Do_RefreshLookupList( args : PPyObject ) : PPyObject;
begin
 with GetPythonEngine do begin
   Adjust(@Self);  // <- adjust the transmitted self argument
   try
     if CheckField then begin
       FField.RefreshLookupList;
       Result := ReturnNone;
     end
     else
       Result := nil;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

procedure TPyDBField.OnChange( Sender : TField );
begin
 IncRef;
 ExecuteEvent( FOnChange, [GetSelf] );
end;

procedure TPyDBField.OnGetText( Sender: TField; var Text: String; DisplayText: Boolean );
var
 v : PPyObject;
begin
 IncRef;
 with GetPythonEngine do begin
   v := TPyDBVarArg.PyDBVarArgType.CreateInstanceWith( VariantAsPyObject( Text ) );
   Py_XIncRef(v);
   try
     ExecuteEvent( FOnGetText, [GetSelf, v, DisplayText] );
     with PythonToDelphi(v) as TPyDBVarArg do
       Text := PyObjectAsVariant(FValue);
   finally
     Py_XDecRef(v);
   end;
 end;
end;

procedure TPyDBField.OnSetText( Sender : TField; const Text: String );
begin
 IncRef;
 ExecuteEvent( FOnSetText, [GetSelf, Text] );
end;

procedure TPyDBField.OnValidate( Sender : TField );
begin
 IncRef;
 ExecuteEvent( FOnValidate, [GetSelf] );
end;

// ----------------------- TPyDBVarArg -----------------------------------------

constructor TPyDBVarArg.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
 inherited;
 with GetPythonEngine do begin
   if PyTuple_Check(args) and (PyTuple_Size(args)>=1) then begin
     FValue := PyTuple_GetItem( args, 0 );
     Py_XIncRef(FValue);
   end;
 end;
end;

destructor TPyDBVarArg.Destroy;
begin
 with GetPythonEngine do
   Py_XDecRef(FValue);
 inherited;
end;

function  TPyDBVarArg.GetAttr(key : PAnsiChar) : PPyObject;
begin
 with GetPythonEngine do begin
   if CompareText(String(key), 'Value' ) = 0 then begin
     Result := FValue;
     if not Assigned(Result) then
       Result := Py_None;
     Py_XIncRef(Result);
   end
   else
     Result := inherited GetAttr(key);
 end;
end;

function  TPyDBVarArg.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
 with GetPythonEngine do begin
   if CompareText(String(key), 'Value' ) = 0 then begin
     Py_XDecRef(FValue);
     FValue := value;
     Py_XIncRef(FValue);
     Result := 0;
   end
   else
     Result := inherited SetAttr(key, value);
 end;
end;

function  TPyDBVarArg.Repr : PPyObject;
begin
 with GetPythonEngine do
   Result := PyString_FromString( PAnsiChar(AnsiString(PyObjectAsString(FValue))) );
end;

// ----------------------- TDSRowsAccess ---------------------------------------

class function TDSRowsAccess.ExpectedContainerClass: TClass;
begin
  Result := TPyDBDataset;
end;

function TDSRowsAccess.GetContainer: TPyDBDataset;
begin
 Result := TPyDBDataset(inherited Container);
end;

function TDSRowsAccess.GetItem(aIndex: Integer): PPyObject;
var
 i : Integer;
 l_sStr: String;
 l_pAnsiChar: PAnsiChar;
 l_oDataset: TFDDataset;
begin
 Result := nil;
 l_oDataset := GetContainer.GetDelphiObject;
 if (aIndex >= 0) and (aIndex < l_oDataset.RecordCount) then begin
   with GetPythonEngine do begin
     try
       l_oDataset.RecNo := AIndex + 1;
       Result   := PyTuple_New(l_oDataset.FieldCount);
       for i := 0 to l_oDataset.FieldCount - 1 do begin
         with l_oDataset.Fields[i] do begin
           PyTuple_SetItem( Result, i,  VariantAsPyObject( Value ) );
         end;
       end;
     except
       on E : Exception do begin
         raise Exception.CreateFmt('Exception in TDSRowsAccess.GetItem - Index=%d', [AIndex]);
       end;
     end;
   end;
 end;
end;

function TDSRowsAccess.GetSize: Integer;
var
 l_oDataset: TFDDataset;
begin
 l_oDataset := GetContainer.GetDelphiObject;
 Result     := l_oDataset.RecordCount;
end;

function TDSRowsAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  S : string;
  _obj : TPyObject;
  _value : TObject;
  _ds : TFDDataset;
begin
  Result := -1;
  {
  with GetPythonEngine do
  begin
    if PyString_Check(AValue) then
    begin
      S := PyString_AsDelphiString(AValue);
      for i := 0 to Container.ControlCount-1 do
        if SameText( Container.Controls[i].Name, S) then
        begin
          Result := i;
          Break;
        end;
    end
    else if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if _obj is TPyDelphiObject then
      begin
        _value := TPyDelphiObject(_obj).DelphiObject;
        if _value is TControl then
        begin
          _ctrl := TControl(_value);
          for i := 0 to Container.ControlCount-1 do
            if Container.Controls[i] = _ctrl then
            begin
              Result := i;
              Break;
            end;
        end;
      end;
    end;
  end;
  }
end;

class function TDSRowsAccess.Name: String;
begin
  Result := 'DSRows';
end;

class function TDSRowsAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

// ----------------------- TPyDBDataset ------------------------------------

constructor TPyDBDataset.Create( aPythonType : TPythonType );
begin
 inherited;
 if not Assigned(DelphiObject) then
   DelphiObject := TFDDataset.Create(Nil);
end;

constructor TPyDBDataset.CreateWith( aPythonType : TPythonType; args : PPyObject );
begin
 inherited;
 if not Assigned(DelphiObject) then
   DelphiObject := TFDDataset.Create(Nil);
end;

destructor TPyDBDataset.Destroy;
var
 l_oDataset: TFDDataset;
begin
 l_oDataset := DelphiObject;
 if Assigned(l_oDataset) then begin
   if not ((l_oDataset is TFDTable) or (l_oDataset is TFDQuery)) then begin
     DelphiObject := Nil;    // <- implizit Free!!!
   end;
 end;
 SetLength(m_arAnsiTextBuf,0);
 inherited;
end;

function  TPyDBDataset.GetDelphiObject: TFDDataset;
begin
 Result := TFDDataset(inherited DelphiObject);
end;

procedure TPyDBDataset.SetDelphiObject(const Value: TFDDataset);
begin
 inherited DelphiObject := Value;
end;

function  TPyDBDataset.CreateContainerAccess : TContainerAccess;
var
 l_ContainerAccessClass : TContainerAccessClass;
begin
 l_ContainerAccessClass := TDSRowsAccess;
 Result := l_ContainerAccessClass.Create(PyDelphiWrapper, self)
end;

class function  TPyDBDataset.DelphiObjectClass : TClass;
begin
 Result := TFDDataset;
end;

class function  TPyDBDataset.GetContainerAccessClass : TContainerAccessClass;
begin
 Result := TDSRowsAccess;
end;

function TPyDBDataset.Get_RowsCount(AContext: Pointer): PPyObject;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   Result := PyInt_FromLong(DelphiObject.RecordCount);
 end;
end;

function TPyDBDataset.Get_Rows(AContext: Pointer): PPyObject;
begin
  with GetPythonEngine do begin
    Adjust(@Self);
    Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiContainer do
      Setup(Self.PyDelphiWrapper, TDSRowsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
  end;
end;

class procedure TPyDBDataset.RegisterGetSets( PythonType : TPythonType );
begin
 inherited;
 PythonType.AddGetSet(PAnsiChar('RowsCount'), @TPyDBDataset.Get_RowsCount, nil,
       PAnsiChar('Returns the count of contained dataset rows'), nil);
 PythonType.AddGetSet(PAnsiChar('Rows'), @TPyDBDataset.Get_Rows, nil,
       PAnsiChar('Returns an iterator over contained dataset rows'), nil);
end;

class procedure TPyDBDataset.RegisterMembers( PythonType : TPythonType );
begin
 inherited;
end;

class procedure TPyDBDataset.RegisterMethods( PythonType : TPythonType );
begin
 inherited;
 with PythonType do begin
   AddMethod(AnsiString('Fields'),        @TPyDBDataset.Do_Fields,
             AnsiString('DBDataset.Fields( index : Integer ) -> TField') );
   AddMethod(AnsiString('FieldByName'),   @TPyDBDataset.Do_FieldByName,
             AnsiString('DBDataset.FieldByName(FieldName: String) -> DBField') );
   AddMethod(AnsiString('FieldNamesAsTuple'), @TPyDBDataset.Do_FieldNamesAsTuple,
             AnsiString('DBDataset.FieldNamesAsTuple() -> a tuple containing all field names') );
   AddMethod(AnsiString('FieldsAsTuple'), @TPyDBDataset.Do_FieldsAsTuple,
             AnsiString('DBDataset.FieldsAsTuple() -> a tuple containing all TFields') );
   AddMethod(AnsiString('FieldsAsDict'),  @TPyDBDataset.Do_FieldsAsDict,
             AnsiString('DBDataset.FieldsAsDict() -> a dictionary containing all TFields, with FieldName as Key, and TField as Value') );
   AddMethod(AnsiString('First'),         @TPyDBDataset.Do_First,
             AnsiString('DBDataset.First() -> None') );
   AddMethod(AnsiString('Last'),          @TPyDBDataset.Do_Last,
             AnsiString('DBDataset.Last() -> None') );
   AddMethod(AnsiString('Next'),          @TPyDBDataset.Do_Next,
             AnsiString('DBDataset.Next() -> None') );
   AddMethod(AnsiString('Prior'),         @TPyDBDataset.Do_Prior,
             AnsiString('DBDataset.Prior() -> None') );
   AddMethod(AnsiString('Locate'),        @TPyDBDataset.Do_Locate,
             AnsiString('DBDataset.Locate( KeyFields : String, KeyValues : Object or Sequence, Options : TLocateOptions ) -> True or False') );
   AddMethod(AnsiString('Lookup'),        @TPyDBDataset.Do_Lookup,
             AnsiString('DBDataset.Lookup( KeyFields : String, KeyValues : Object or Sequence, ResultFields : String ) -> a list containing all Result fields') );
   AddMethod(AnsiString('FindKey'),       @TPyDBDataset.Do_FindKey,
             AnsiString('DBDataset.FindKey( KeyValues : Sequence ) -> True or False') );
   AddMethod(AnsiString('FindNearest'),   @TPyDBDataset.Do_FindNearest,
             AnsiString('DBDataset.FindNearest( KeyValues : Sequence ) -> None') );

 end;
end;

function  TPyDBDataset.GetAttr(key : PAnsiChar) : PPyObject;
var
 l_sUpperKey: String;
 l_oDataset:  TFDDataset;
begin
 Result := nil;
 with GetPythonEngine do begin
   try
     l_oDataset  := DelphiObject;
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'BOF' ) = 0 then
       Result := VariantAsPyObject( l_oDataset.BOF )
     else if CompareText(l_sUpperKey, 'CANMODIFY' ) = 0 then
       Result := VariantAsPyObject( l_oDataset.CanModify )
     else if CompareText(l_sUpperKey, 'EOF' ) = 0 then
       Result := VariantAsPyObject( l_oDataset.EOF )
     else if CompareText( string(key), 'FIELDCOUNT' ) = 0 then
       Result := VariantAsPyObject( l_oDataset.FieldCount )
     else if CompareText( string(key), 'RECNO' ) = 0 then
       Result := VariantAsPyObject( l_oDataset.RecNo )
     else
       Result := inherited GetAttr(key);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := Nil;
     end;
   end;
 end;
end;

function  TPyDBDataset.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
var
 l_sUpperKey: String;
 l_oDataset:  TFDDataset;
begin
 Result := -1;
 with GetPythonEngine do begin
   try
     l_oDataset  := DelphiObject;
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'FILTER' ) = 0 then begin
       l_oDataset.Filter := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'FILTERED' ) = 0 then begin
       l_oDataset.Filtered := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'BOF' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'CanModify' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'EOF' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'RECNO' ) = 0 then begin
       l_oDataset.RecNo := PyObjectAsVariant( value );
       Result := 0;
     end
     else
       Result := inherited SetAttr(key, value);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := -1;
     end;
   end;
 end;
end;

// Methods

procedure TPyDBDataset.RaiseDBError( E : Exception );
begin
 if self.GetModule <> nil then
   GetModule.RaiseError( AnsiString('DBError'), AnsiString(E.Message) );
end;

function TPyDBDataset.Do_Fields( args : PPyObject ): PPyObject;
var
 idx : Integer;
 fld : TField;
 F : TPyDBField;
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   // We adjust the transmitted self argument
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     if PyArg_ParseTuple( args, 'i:DBDataset.Fields',@idx ) <> 0 then begin
       if (idx >= 0) and (idx < l_oDataset.FieldCount) then begin
         Result := TPyDBField.PyDBFieldType.CreateInstance;
         F := PythonToDelphi(Result) as TPyDBField;
         F.FField := l_oDataset.Fields[idx];
       end
       else begin
         Result := nil;
         PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Value out of range : %d', [idx]))));
       end;
     end
     else
       Result := nil;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_FieldByName( args : PPyObject ) : PPyObject;
var
 l_sAStr: AnsiString;
 s : PAnsiChar;
 fld : TField;
 F : TPyDBField;
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   // We adjust the transmitted self argument
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     if (PyArg_ParseTuple( args, PAnsiChar('s:DBDataset.FieldByName'),@s ) <> 0) then begin
       l_sAStr := AnsiString(s);
       fld := l_oDataset.FieldByName(String(l_sAStr));
       if Assigned(fld) then begin
         Result := TPyDBField.PyDBFieldType.CreateInstance;
         F := PythonToDelphi(Result) as TPyDBField;
         F.FField := fld;
       end
       else begin
         Result := nil;
         PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Unknown field "%s"', [String(s)]))) );
       end;
     end
     else
       Result := nil;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_FieldNamesAsTuple( args: PPyObject): PPyObject;
var
 i : Integer;
 l_sStr: String;
 l_pAnsiChar: PAnsiChar;
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     Result   := PyTuple_New(l_oDataset.FieldCount);
     SetLength(m_arAnsiTextBuf,l_oDataset.FieldCount);
     for i := 0 to l_oDataset.FieldCount - 1 do begin
       with l_oDataset.Fields[i] do begin
         l_sStr := FieldName;
         l_pAnsiChar := _PAnsiChar(l_sStr, m_arAnsiTextBuf[i]);
         PyTuple_SetItem( Result, i, PyString_FromString( l_pAnsiChar ) );
       end;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_FieldsAsTuple( args: PPyObject): PPyObject;
var
 i : Integer;
 l_sStr: String;
 l_pAnsiChar: PAnsiChar;
 l_oDataset: TFDDataset;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);     // <- adjust the transmitted self argument
   l_oDataset := DelphiObject;
   try
     Result := PyTuple_New(l_oDataset.FieldCount);
     SetLength(m_arAnsiTextBuf,l_oDataset.FieldCount);
     for i := 0 to l_oDataset.FieldCount - 1 do begin
       if (l_oDataset.Fields[i].DataType = ftString) or
          (l_oDataset.Fields[i].DataType = ftWideString) then begin
         l_sStr := l_oDataset.Fields[i].AsString;
         l_pAnsiChar := _PAnsiChar(l_sStr, m_arAnsiTextBuf[i]);
         PyTuple_SetItem( Result, i, PyString_FromString( l_pAnsiChar ) );
       end
       else
         PyTuple_SetItem( Result, i, VariantAsPyObject( l_oDataset.Fields[i].AsVariant ) );
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBDataset.Do_FieldsAsDict( args: PPyObject): PPyObject;
var
 i : Integer;
 l_sStr: String;
 l_pAnsiChar: PAnsiChar;
 l_oDataset: TFDDataset;
  obj : PPyObject;
  _fieldName : PPyObject;
begin
 with GetPythonEngine do begin
   Adjust(@Self);     // <- adjust the transmitted self argument
   l_oDataset := DelphiObject;
   try
     Result := PyDict_New;
     for i := 0 to l_oDataset.FieldCount - 1 do
       with l_oDataset.Fields[i] do begin
         obj := VariantAsPyObject( AsVariant );
         _fieldName := VariantAsPyObject(Variant(FieldName));
         PyDict_SetItem( Result, _fieldName, obj );
         Py_XDecRef(obj);
         Py_XDecRef(_fieldName);
       end;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_First( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     l_oDataset.First;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_Last( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     l_oDataset.Last;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_Next( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     l_oDataset.Next;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_Prior( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     l_oDataset.Prior;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_Locate( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
 keyFields : PAnsiChar;
 keyValues, options : PPyObject;
 rslt : Boolean;
 vvalues : Variant;
 opt : TLocateOptions;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     if PyArg_ParseTuple( args, 'sOO:DBDataset.Locate',@keyFields, @keyValues, @options ) <> 0 then begin
       if PySequence_Check(options) = 0 then
         PyErr_SetString (PyExc_AttributeError^, 'Third argument of Locate must be a sequence.')
       else begin
         // Prepare the locate options
         ListToSet( options, @opt, sizeof(opt) );
         // Create a variant containing the key values
         vvalues := PyObjectAsVariant( keyValues );
         // Execute the locate
         rslt := l_oDataset.Locate( String(keyFields), vvalues, opt );
         // Return its result
         Result := VariantAsPyObject( rslt );
       end;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_Lookup( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
 keyFields, resultFields : PAnsiChar;
 keyValues : PPyObject;
 rslt : Variant;
 vvalues : Variant;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     if PyArg_ParseTuple( args, 'sOs:DBDataset.Lookup',@keyFields, @keyValues, @resultFields ) <> 0 then begin
       // Create a variant containing the key values
       vvalues := PyObjectAsVariant( keyValues );
       // Execute the lookup
       rslt := l_oDataset.Lookup( String(keyFields), vvalues, String(resultFields) );
       // Return its result
       Result := VariantAsPyObject( rslt );
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_Refresh( args : PPyObject ) : PPyObject;
var
 l_oDataset: TFDDataset;
begin
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     l_oDataset.Refresh();
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function TPyDBDataset.Do_FindKey( args : PPyObject ) : PPyObject;
var
 i : Integer;
 V : Variant;
 l_oDataset: TFDDataSet;
 KeyValues : PPyObject;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     if PyArg_ParseTuple( args, 'O:DBDataset.FindKey',@KeyValues ) <> 0 then begin
       if PySequence_Check( KeyValues ) <> 0 then begin
         V := PyObjectAsVariant( KeyValues );
         l_oDataset.CheckBrowseMode;
         l_oDataset.SetKey;
         for i := 0 to VarArrayHighBound(V, 1) do
           l_oDataset.IndexFields[i].AsVariant := V[i];
         Result := VariantAsPyObject( l_oDataset.GotoKey );
       end
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBDataset.Do_FindNearest( args : PPyObject ) : PPyObject;
var
 i : Integer;
 V : Variant;
 l_oDataset: TFDDataset;
 KeyValues : PPyObject;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oDataset := DelphiObject;
   try
     if PyArg_ParseTuple( args, 'O:DBDataset.FindNearest',@KeyValues ) <> 0 then begin
       if PySequence_Check( KeyValues ) <> 0  then begin
         V := PyObjectAsVariant( KeyValues );
         l_oDataset.CheckBrowseMode;
         l_oDataset.SetKey;
         for i := 0 to VarArrayHighBound(V, 1) do
           l_oDataset.IndexFields[i].AsVariant := V[i];
         l_oDataset.GotoNearest;
       end;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

// ----------------------- TPyDBTable ------------------------------------

constructor TPyDBTable.Create( aPythonType : TPythonType );
begin
 if not Assigned(DelphiObject) then begin
   DelphiObject := TFDTable.Create(Nil);
   self.Owned := True;
 end;
 inherited;
end;

constructor TPyDBTable.CreateWith( aPythonType : TPythonType; args : PPyObject );
begin
 if not Assigned(DelphiObject) then begin
   DelphiObject := TFDTable.Create(Nil);
   self.Owned := True;
 end;
 inherited;
end;

destructor TPyDBTable.Destroy;
var
 l_oTbl: TFDTable;
begin
 l_oTbl := DelphiObject;
 if Assigned(l_oTbl) then begin
   if l_oTbl.Active then
     l_oTbl.Close();
   DelphiObject := Nil;    // <- implizit Free!!!
 end;
 inherited;
end;

function TPyDBTable.GetDelphiObject: TFDTable;
begin
 Result := TFDTable(inherited DelphiObject);
end;

procedure TPyDBTable.SetDelphiObject(const Value: TFDTable);
begin
 inherited DelphiObject := Value;
end;

class function TPyDBTable.DelphiObjectClass: TClass;
begin
 Result := TFDTable;
end;

class procedure TPyDBTable.RegisterMethods( PythonType : TPythonType );
begin
 inherited;
 with PythonType do begin
   AddMethod(AnsiString('Open'), @TPyDBTable.Do_Open,
             AnsiString('FDTable.Open() -> None') );
   AddMethod(AnsiString('Close'), @TPyDBTable.Do_Close,
             AnsiString('FDTable.Close() -> None') );
   AddMethod(AnsiString('Edit'), @TPyDBTable.Do_Edit,
             AnsiString('FDTable.Edit() -> None') );
   AddMethod(AnsiString('Insert'), @TPyDBTable.Do_Insert,
             AnsiString('FDTable.Insert() -> None') );
   AddMethod(AnsiString('Append'), @TPyDBTable.Do_Append,
             AnsiString('FDTable.Append() -> None') );
   AddMethod(AnsiString('Post'), @TPyDBTable.Do_Post,
             AnsiString('FDTable.Post() -> None') );
   AddMethod(AnsiString('Cancel'), @TPyDBTable.Do_Cancel,
             AnsiString('FDTable.Cancel() -> None') );
   AddMethod(AnsiString('Delete'), @TPyDBTable.Do_Delete,
             AnsiString('FDTable.Delete() -> None') );
   AddMethod(AnsiString('SetRangeStart'), @TPyDBTable.Do_SetRangeStart,
             AnsiString('FDTable.SetRangeStart() -> None') );
   AddMethod(AnsiString('EditRangeStart'), @TPyDBTable.Do_EditRangeStart,
             AnsiString('FDTable.EditRangeStart() -> None') );
   AddMethod(AnsiString('SetRangeEnd'), @TPyDBTable.Do_SetRangeEnd,
             AnsiString('FDTable.SetRangeEnd() -> None') );
   AddMethod(AnsiString('EditRangeEnd'), @TPyDBTable.Do_EditRangeEnd,
             AnsiString('FDTable.EditRangeEnd() -> None') );
   AddMethod(AnsiString('ApplyRange'), @TPyDBTable.Do_ApplyRange,
             AnsiString('FDTable.ApplyRange() -> None') );
   AddMethod(AnsiString('SetRange'), @TPyDBTable.Do_SetRange,
             AnsiString('FDTable.SetRange( sequence of RangeStart values, sequence of RangeEnd values ) -> None') );
   AddMethod(AnsiString('CancelRange'), @TPyDBTable.Do_CancelRange,
             AnsiString('FDTable.CancelRange() -> None') );
   AddMethod(AnsiString('GetIndexNames'), @TPyDBTable.Do_GetIndexNames,
             AnsiString('FDTable.GetIndexNames() -> list of Index Names') );
 end;
end;

class procedure TPyDBTable.RegisterMembers( PythonType : TPythonType );
begin
 inherited;
 //-- PythonType.AddMember( 'Active', mtInt, NativeInt(@TPyDBTable(nil).x), mfDefault, 'Active');
end;

class procedure TPyDBTable.RegisterGetSets( PythonType : TPythonType );
begin
 inherited;
 //-- PythonType.AddGetSet(PAnsiChar('Active'), @TPyDBTable.Get_Active, @TPyDBTable.Set_Active,
 //--                      PAnsiChar('Returns/Sets the Active'), nil);
end;

function TPyDBTable.CheckActiveDBTable(aMustOpen: Boolean): Boolean;
begin
 Result := True;
 if GetDelphiObject.Active then begin
   if not aMustOpen then begin
     Result := False;
     with GetPythonEngine do
       PyErr_SetString (PyExc_RuntimeError^, PAnsiChar('DBTable is open!') );
   end;
 end
 else begin
   if aMustOpen then begin
     Result := False;
     with GetPythonEngine do
       PyErr_SetString (PyExc_RuntimeError^, PAnsiChar('DBTable is not open!') );
   end;
 end;
end;

function  TPyDBTable.GetAttr(key : PAnsiChar) : PPyObject;
var
 l_sUpperKey: String;
 l_sConnectionDefName, l_sDatabaseName: String;
 l_oConn: TFDCustomConnection;
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   l_oConn := Nil;
   try
     l_oTable    := DelphiObject;
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'CONNECTIONDEFNAME' ) = 0 then begin
       l_oConn := DelphiObject.Connection;
       l_sConnectionDefName := DelphiObject.ConnectionName;
       if Assigned(l_oConn) then
         l_sConnectionDefName := l_oConn.ConnectionName;
       Result := VariantAsPyObject( l_sConnectionDefName )
     end
     else if CompareText(UpperCase(String(key)), 'DATABASENAME' ) = 0 then begin
       l_oConn := DelphiObject.Connection;
       if Assigned(l_oConn) then
         l_sDatabaseName := l_oConn.Params.Database
       else
         l_sDatabaseName := '';
       Result := VariantAsPyObject( l_sDatabaseName )
    end
    else if CompareText(l_sUpperKey, 'TABLENAME' ) = 0 then
       Result := VariantAsPyObject( l_oTable.TableName )
    else if CompareText(l_sUpperKey, 'ACTIVE' ) = 0 then
      Result := VariantAsPyObject( l_oTable.Active )
    else if CompareText(l_sUpperKey, 'FILTER' ) = 0 then
       Result := VariantAsPyObject( l_oTable.Filter )
    else if CompareText(l_sUpperKey, 'FILTERED' ) = 0 then
       Result := VariantAsPyObject( l_oTable.Filtered )
    else if CompareText(l_sUpperKey, 'STATE' ) = 0 then
       Result := VariantAsPyObject( Integer(l_oTable.State) )
    else if CompareText(l_sUpperKey, 'MODIFIED' ) = 0 then
       Result := VariantAsPyObject( l_oTable.Modified )
     else if CompareText(l_sUpperKey, 'ISRANGED' ) = 0 then
       // Ermöglicht das Ermitteln des aktuellen Bereichsfilterungsmodus.
       Result := VariantAsPyObject( l_oTable.IsRanged )
     else if CompareText(l_sUpperKey, 'KEYEXCLUSIVE' ) = 0 then
       // Ermittelt oder setzt die Einbeziehung der niedrigsten und höchsten Werte in einen gefilterten Bereich.
       Result := VariantAsPyObject( l_oTable.KeyExclusive )
     else if CompareText(l_sUpperKey, 'KEYFIELDCOUNT' ) = 0 then
       // Ermittelt oder setzt die Anzahl der in der Bereichsfilterung zu verwendenden Indexfelder
       Result := VariantAsPyObject( l_oTable.KeyFieldCount )
     else if CompareText(l_sUpperKey, 'INDEXNAME' ) = 0 then
       Result := VariantAsPyObject( l_oTable.IndexName )
    else
      Result := inherited GetAttr(key);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function  TPyDBTable.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
var
 i: Integer;
 l_sUpperKey: String;
 l_sName, l_sConnectionDefName, l_sDatabaseName: String;
 l_oConn: TFDCustomConnection;
 l_oTable: TFDTable;
begin
 Result := -1;
 with GetPythonEngine do begin
   l_oConn := Nil;
   try
     l_oTable    := DelphiObject;
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'CONNECTIONDEFNAME' ) = 0 then begin
       if CheckActiveDBTable(False) then begin
         l_sConnectionDefName := UpperCase(PyObjectAsVariant(value));
         for i := 0 to FDManager.ConnectionCount-1 do begin
           l_sName := UpperCase(FDManager.Connections[i].ConnectionDefName);
           if l_sName = l_sConnectionDefName then begin
             l_oConn := FDManager.Connections[i];
             break;
           end;
         end;
         if Assigned(l_oConn) then begin
           if l_oTable.Active then
             l_oTable.Active := False;
           l_oTable.Connection := l_oConn;
         end;
         Result := 0;
       end;
     end
     else if CompareText(l_sUpperKey, 'DATABASENAME' ) = 0 then begin
       if CheckActiveDBTable(False) then begin
         l_sDatabaseName := PyObjectAsVariant( value );
         l_oConn := DelphiObject.Connection;
         if Assigned(l_oConn) then begin
           l_oConn.Params.Database := l_sDatabaseName;
         end;
         Result := 0;
       end;
     end
     else if CompareText(l_sUpperKey, 'TABLENAME' ) = 0 then begin
       if CheckActiveDBTable(False) then begin
         l_oTable.TableName := PyObjectAsVariant( value );
         Result := 0;
       end;
     end
     else if CompareText(l_sUpperKey, 'ACTIVE' ) = 0 then begin
       l_oTable.Active := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'FILTER' ) = 0 then begin
       l_oTable.Filter := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'FILTERED' ) = 0 then begin
       l_oTable.Filtered := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'STATE' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'MODIFIED' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ISRANGED' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'KEYEXCLUSIVE' ) = 0 then begin
       // Ermittelt oder setzt die Einbeziehung der niedrigsten und höchsten Werte in einen gefilterten Bereich.
       l_oTable.KeyExclusive := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'KEYFIELDCOUNT' ) = 0 then begin
       // Ermittelt oder setzt die Anzahl der in der Bereichsfilterung zu verwendenden Indexfelder
       l_oTable.KeyFieldCount := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'INDEXNAME' ) = 0 then begin
       // Ermittelt oder setzt die Anzahl der in der Bereichsfilterung zu verwendenden Indexfelder
       l_oTable.IndexName := PyObjectAsVariant( value );
       Result := 0;
     end
     else
       Result := inherited SetAttr(key, value);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := -1;
     end;
   end;
 end;
end;

function TPyDBTable.Do_Open( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);   // <- Adjust the transmitted self argument
   l_oTable := DelphiObject;
   try
     l_oTable.Open;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Close( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     l_oTable.Close;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Edit( args : PPyObject ): PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.Edit;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Insert( args : PPyObject ): PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.Insert;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Append( args : PPyObject ): PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.Append;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Post( args : PPyObject ): PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.Post;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Cancel( args : PPyObject ): PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.Cancel;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_Delete( args : PPyObject ): PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.Delete;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_SetRangeStart( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.SetRangeStart;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_EditRangeStart( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.EditRangeStart;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_SetRangeEnd( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.EditRangeEnd;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_EditRangeEnd( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.EditRangeEnd;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_ApplyRange( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.ApplyRange;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_SetRange( args : PPyObject ) : PPyObject;
var
 i: Integer;
 l_oTable: TFDTable;
 l_oPyStartValues, l_oPyEndValues : PPyObject;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active and
       (PyArg_ParseTuple( args, 'OO:FDTable.SetRange',@l_oPyStartValues, @l_oPyEndValues ) <> 0) then begin
       if PySequence_Check(l_oPyStartValues) = 0 then begin
         PyErr_SetString (PyExc_AttributeError^, 'First argument of SetRange must be a sequence.');
       end
       else if PySequence_Check(l_oPyEndValues) = 0 then begin
         PyErr_SetString (PyExc_AttributeError^, 'Second argument of SetRange must be a sequence.');
       end
       else begin
         l_oTable.SetRangeStart;
         for i := 0 to PySequence_Length(l_oPyStartValues)-1 do
           l_oTable.IndexFields[i].Value := PyObjectAsVariant(PySequence_GetItem(l_oPyStartValues, i));
         l_oTable.SetRangeEnd;
         for i := 0 to PySequence_Length(l_oPyEndValues)-1 do
           l_oTable.IndexFields[i].Value := PyObjectAsVariant(PySequence_GetItem(l_oPyEndValues, i));
         l_oTable.ApplyRange;
       end;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_CancelRange( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   try
     if l_oTable.Active then begin
       l_oTable.CancelRange;
       Result := ReturnNone;
     end;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBTable.Do_GetIndexNames( args : PPyObject ) : PPyObject;
var
 l_oTable: TFDTable;
 L : TStringList;
begin
 Result := nil;
 L := Nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oTable := DelphiObject;
   L := TStringList.Create;
   try
     l_oTable.GetIndexNames( L );
     Result := GetPythonEngine.StringsToPyList( L );
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
 if Assigned(L) then
   L.Free;
end;

// --------------------- TPyDBQuery --------------------------------------------

constructor TPyDBQuery.Create( aPythonType : TPythonType );
begin
 if not Assigned(DelphiObject) then begin
   DelphiObject := TFDQuery.Create(Nil);
   self.Owned := True;
 end;
 inherited;
end;

constructor TPyDBQuery.CreateWith( aPythonType : TPythonType; args : PPyObject );
begin
 if not Assigned(DelphiObject) then begin
   DelphiObject := TFDQuery.Create(Nil);
   self.Owned := True;
 end;
 inherited;
end;

destructor TPyDBQuery.Destroy;
var
 l_oQry: TFDQuery;
begin
 l_oQry := DelphiObject;
 if Assigned(l_oQry) then begin
   if l_oQry.Active then
     l_oQry.Close();
   DelphiObject := Nil;    // <- implizit Free!!!
 end;
 inherited;
end;

function TPyDBQuery.GetDelphiObject: TFDQuery;
begin
 Result := TFDQuery(inherited DelphiObject);
end;

procedure TPyDBQuery.SetDelphiObject(const Value: TFDQuery);
begin
 inherited DelphiObject := Value;
end;

class function  TPyDBQuery.DelphiObjectClass : TClass;
begin
 Result := TFDQuery;
end;

class procedure TPyDBQuery.RegisterMethods( PythonType : TPythonType );
begin
 inherited;
 with PythonType do begin
   AddMethod(AnsiString('Open'), @TPyDBQuery.Do_Open,
             AnsiString('FDQuery.Open() -> None') );
   AddMethod(AnsiString('Close'), @TPyDBQuery.Do_Close,
             AnsiString('FDQuery.Close() -> None') );
   AddMethod(AnsiString('Prepare'), @TPyDBQuery.Do_Prepare,
             AnsiString('FDQuery.Prepare() -> None') );
   AddMethod(AnsiString('Unprepare'), @TPyDBQuery.Do_Unprepare,
             AnsiString('FDQuery.Unprepare() -> None') );
   AddMethod(AnsiString('ExecSQL'), @TPyDBQuery.Do_ExecSQL,
             AnsiString('FDQuery.ExecSQL() -> None') );
 end;
end;

class procedure TPyDBQuery.RegisterMembers( PythonType : TPythonType );
begin
 inherited;
 //-- PythonType.AddMember( 'Active', mtInt, NativeInt(@TPyDBTable(nil).x), mfDefault, 'Active');
end;

class procedure TPyDBQuery.RegisterGetSets( PythonType : TPythonType );
begin
 inherited;
 //-- PythonType.AddGetSet(PAnsiChar('Active'), @TPyDBTable.Get_Active, @TPyDBTable.Set_Active,
 //--                      PAnsiChar('Returns/Sets the Active'), nil);
end;

function TPyDBQuery.CheckActiveDBQuery(aMustOpen: Boolean): Boolean;
begin
 Result := True;
 if GetDelphiObject.Active then begin
   if not aMustOpen then begin
     Result := False;
     with GetPythonEngine do
       PyErr_SetString (PyExc_RuntimeError^, PAnsiChar('DBQuery is open!') );
   end;
 end
 else begin
   if aMustOpen then begin
     Result := False;
     with GetPythonEngine do
       PyErr_SetString (PyExc_RuntimeError^, PAnsiChar('DBQuery is not open!') );
   end;
 end;
end;

function  TPyDBQuery.GetAttr(key : PAnsiChar) : PPyObject;
var
 i: Integer;
 l_sUpperKey, l_sStr: String;
 l_sConnectionDefName, l_sDatabaseName: String;
 l_pAnsiChar: PAnsiChar;
 l_oConn: TFDCustomConnection;
 l_oQuery: TFDQuery;
begin
 Result := nil;
 with GetPythonEngine do begin
   l_oConn := Nil;
   try
     l_oQuery    := DelphiObject;
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'CONNECTIONDEFNAME' ) = 0 then begin
       l_oConn := DelphiObject.Connection;
       l_sConnectionDefName := DelphiObject.ConnectionName;
       if Assigned(l_oConn) then
         l_sConnectionDefName := l_oConn.ConnectionName;
       Result := VariantAsPyObject( l_sConnectionDefName )
     end
     else if CompareText(UpperCase(String(key)), 'DATABASENAME' ) = 0 then begin
       l_oConn := DelphiObject.Connection;
       if Assigned(l_oConn) then
         l_sDatabaseName := l_oConn.Params.Database
       else
         l_sDatabaseName := '';
       Result := VariantAsPyObject( l_sDatabaseName )
    end
    else if CompareText(l_sUpperKey, 'PARAMCOUNT' ) = 0 then
       Result := VariantAsPyObject( l_oQuery.ParamCount )
    else if CompareText(l_sUpperKey, 'ACTIVE' ) = 0 then
      Result := VariantAsPyObject( l_oQuery.Active )
    else if CompareText(l_sUpperKey, 'PREPARED' ) = 0 then
       Result := VariantAsPyObject( l_oQuery.Prepared )
    else if CompareText(l_sUpperKey, 'SQL' ) = 0 then begin
      Result   := PyTuple_New(l_oQuery.SQL.Count);
      SetLength(m_arAnsiTextBuf,l_oQuery.SQL.Count);
      for i := 0 to l_oQuery.SQL.Count - 1 do begin
        l_sStr := l_oQuery.SQL[i];
        l_pAnsiChar := _PAnsiChar(l_sStr, m_arAnsiTextBuf[i]);
        PyTuple_SetItem( Result, i, PyString_FromString( l_pAnsiChar ) );
      end;
    end
    else
      Result := inherited GetAttr(key);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := nil;
     end;
   end;
 end;
end;

function  TPyDBQuery.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
var
 i, j: Integer;
 l_sUpperKey, l_sText, l_sStr, l_sSql: String;
 l_sName, l_sConnectionDefName, l_sDatabaseName: String;
 l_oConn: TFDCustomConnection;
 l_oQuery: TFDQuery;
 l_pPyObj: PPyObject;
begin
 Result := -1;
 with GetPythonEngine do begin
   l_oConn := Nil;
   try
     l_oQuery    := DelphiObject;
     l_sUpperKey := UpperCase(String(key));
     if CompareText(l_sUpperKey, 'CONNECTIONDEFNAME' ) = 0 then begin
       if CheckActiveDBQuery(False) then begin
         l_sConnectionDefName := UpperCase(PyObjectAsVariant(value));
         for i := 0 to FDManager.ConnectionCount-1 do begin
           l_sName := UpperCase(FDManager.Connections[i].ConnectionDefName);
           if l_sName = l_sConnectionDefName then begin
             l_oConn := FDManager.Connections[i];
             break;
           end;
         end;
         if Assigned(l_oConn) then begin
           if l_oQuery.Active then
             l_oQuery.Active := False;
           l_oQuery.Connection := l_oConn;
         end;
         Result := 0;
       end;
     end
     else if CompareText(l_sUpperKey, 'DATABASENAME' ) = 0 then begin
       if CheckActiveDBQuery(False) then begin
         l_sDatabaseName := PyObjectAsVariant( value );
         l_oConn := DelphiObject.Connection;
         if Assigned(l_oConn) then begin
           l_oConn.Params.Database := l_sDatabaseName;
         end;
         Result := 0;
       end;
     end
     else if CompareText(l_sUpperKey, 'PARAMCOUNT' ) = 0 then begin
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'ACTIVE' ) = 0 then begin
       l_oQuery.Active := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'PREPARED' ) = 0 then begin
       l_oQuery.Prepared := PyObjectAsVariant( value );
       Result := 0;
     end
     else if CompareText(l_sUpperKey, 'SQL' ) = 0 then begin
       if not PyList_Check(value) then
         raise EPythonError.Create('the python object is not a list');
       l_sSql := '';
       for i := 0 to PyList_Size( value ) - 1 do begin
         l_pPyObj := PyList_GetItem( value, i );
         l_sStr   := PyObjectAsString( l_pPyObj );
         for j := 1 to Length(l_sStr) do begin
           if (l_sStr[j] > #0) and (l_sStr[j] <= #13) then
             l_sStr[j] := ' ';
         end;
         if (Length(l_sSql) > 0) and (l_sSql[Length(l_sSql)] <> ' ') then
           l_sSql := l_sSql + ' ';
         l_sSql := l_sSql + l_sStr;
       end;
       l_oQuery.SQL.Clear;
       l_oQuery.SQL.Add(l_sSql);
       l_sText := l_oQuery.SQL.Text;
       Result := 0;
     end
     else
       Result := inherited SetAttr(key, value);
   except
     on E : Exception do begin
       RaiseDBError( E );
       Result := -1;
     end;
   end;
 end;
end;

function TPyDBQuery.Do_Open( args : PPyObject ) : PPyObject;
var
 l_oQuery: TFDQuery;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);   // <- Adjust the transmitted self argument
   l_oQuery := DelphiObject;
   try
     l_oQuery.Open;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBQuery.Do_Close( args : PPyObject ) : PPyObject;
var
 l_oQuery: TFDQuery;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oQuery := DelphiObject;
   try
     l_oQuery.Close;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBQuery.Do_Prepare( args : PPyObject ) : PPyObject;
var
 l_oQuery: TFDQuery;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oQuery := DelphiObject;
   try
     l_oQuery.Prepare;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBQuery.Do_Unprepare( args : PPyObject ) : PPyObject;
var
 l_oQuery: TFDQuery;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oQuery := DelphiObject;
   try
     l_oQuery.Unprepare;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

function TPyDBQuery.Do_ExecSQL( args : PPyObject ) : PPyObject;
var
 l_oQuery: TFDQuery;
begin
 Result := nil;
 with GetPythonEngine do begin
   Adjust(@Self);
   l_oQuery := DelphiObject;
   try
     l_oQuery.ExecSQL;
     Result := ReturnNone;
   except
     on E : Exception do begin
       RaiseDBError( E );
     end;
   end;
 end;
end;

// -----------------------------------------------------------------------------

initialization
begin
 //-- RegisteredUnits.Add( TPyDSRowsRegistration.Create );
 //-- Classes.RegisterClasses([TPyDBTable]);
end;

finalization
begin
 {
 if Assigned(g_oDBFieldType) then begin
   g_oDBFieldType.Free;
   g_oDBFieldType := Nil;
 end;
 if Assigned(g_oDBVarArgType) then begin
   g_oDBVarArgType.Free;
   g_oDBVarArgType := Nil;
 end;
 if Assigned(g_oDBTableType) then begin
   g_oDBTableType.Free;
   g_oDBTableType := Nil;
 end;
 if Assigned(g_oDBQueryType) then begin
   g_oDBQueryType.Free;
   g_oDBQueryType := Nil;
 end;
 if Assigned(g_oDBModule) then begin
   g_oDBModule.Free;
   g_oDBModule := Nil;
 end;
 }
end;

end.
