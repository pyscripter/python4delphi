{$I Definition.Inc}

unit pyDB;

{-------------------------------------------------------------------------------
  $Header: /P4D/PythonForDelphi/Components/Sources/VCL/pyDB.pas 1     03-04-09 19:25 Morgan $
  Copyright © MMM Inc. 2003 - All Rights Reserved.
  ------------------------------------------------------------------------------
  Author: Morgan Martinet

  Description:

  ------------------------------------------------------------------------------
  $Log: /P4D/PythonForDelphi/Components/Sources/VCL/pyDB.pas $
 * 
 * 1     03-04-09 19:25 Morgan
 * initial check-in

-------------------------------------------------------------------------------}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PythonEngine, StdCtrls, ExtCtrls, ComCtrls, DB;
type
  TCallBackSplit = record
    method: Pointer;
    self:   TObject;
  end;

{*********************************************************************
 *      DB module                                              *
 *********************************************************************}
  TPythonDB = class(TPythonModule)
    public
      constructor Create( AOwner : TComponent ); override;
      procedure Initialize; override;
  end;

{*********************************************************************
 *      Shared object                                                *
 *********************************************************************}

 TSharedObject = class
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

   property Data : TObject read FData write SetData;
   property Owner : Boolean read FOwner write FOwner;
 end;

 {*********************************************************************
  *      Interface of TCommon                                         *
  *********************************************************************}

  TCommon = class(TPyObject)
    function  GetProperties : PPyObject;
    procedure AppendProperties( List : PPyObject ); virtual;
    procedure AppendProp( List : PPyObject; const prop : String );
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    procedure RaiseDBError( E : Exception );
    function  EventBelongsToObject( Event : TCallbackSplit ) : Boolean;
  end;

 {*********************************************************************
  *      Interface of TField                                          *
  *********************************************************************}
  // This is a Delphi class implementing a new Python type
  // it must derive from TPyObject or one of its descendants.
  // Then it must override some methods, like the constructors,
  // the RegisterMethods and the type services' virtual methods.
  TPyField = class(TCommon)
    FSharedObject : TSharedObject;
    FField        : TField;
    FOnChange     : PPyObject;
    FOnGetText    : PPyObject;
    FOnSetText    : PPyObject;
    FOnValidate   : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;
    destructor  Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Methods of TPyField
    function  CheckDataset : Boolean;
    function  CheckField : Boolean;
    procedure AppendProperties( List : PPyObject ); override;

    // Interface methods
    function DoClear( args : PPyObject ) : PPyObject; cdecl;
    function DoFocusControl( args : PPyObject ) : PPyObject; cdecl;
    function DoIsValidChar( args : PPyObject ) : PPyObject; cdecl;
    function DoRefreshLookupList( args : PPyObject ) : PPyObject; cdecl;

    // Events
    ////////////////
    procedure OnChange( Sender : TField );
    procedure OnGetText( Sender: TField; var Text: String; DisplayText: Boolean );
    procedure OnSetText( Sender : TField; const Text: String );
    procedure OnValidate( Sender : TField );
  end;

  TPythonField = class( TPythonType)
    public
      constructor Create( AOwner : TComponent ); override;
  end;

{*********************************************************************
 *      Interface of TDataset                                        *
 *********************************************************************}


  // This is a Delphi class implementing a new Python type
  // it must derive from TPyObject or one of its descendants.
  // Then it must override some methods, like the constructors,
  // the RegisterMethods and the type services' virtual methods.
  TPyDataset = class(TCommon)
    FSharedObject : TSharedObject;
    // Python functions or methods for each event
    FAfterClose     : PPyObject;
    FAfterOpen      : PPyObject;
    FAfterScroll    : PPyObject;
    FBeforeClose    : PPyObject;
    FBeforeOpen     : PPyObject;
    FBeforeScroll   : PPyObject;
    FAfterCancel    : PPyObject;
    FAfterDelete    : PPyObject;
    FAfterEdit      : PPyObject;
    FAfterInsert    : PPyObject;
    FAfterPost      : PPyObject;
    FBeforeCancel   : PPyObject;
    FBeforeDelete   : PPyObject;
    FBeforeEdit     : PPyObject;
    FBeforeInsert   : PPyObject;
    FBeforePost     : PPyObject;
    FOnCalcFields   : PPyObject;
    FOnDeleteError  : PPyObject;
    FOnEditError    : PPyObject;
    FOnFilterRecord : PPyObject;
    FOnNewRecord    : PPyObject;
    FOnPostError    : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;
    destructor  Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;

    // Sequence services
    function  SqLength : NativeInt; override;
    function  SqItem( idx : NativeInt ) : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Methods of TPyDataset
    function  CheckDataset : Boolean;
    function  Dataset : TDataset;
    procedure SetDataset( val : TDataset; owner : Boolean );
    function  FieldNamesAsTuple : PPyObject;
    function  FieldsAsTuple : PPyObject;
    function  FieldsAsDict : PPyObject;
    function  Locate( const keyFields : String; keyValues, options : PPyObject ) : PPyObject;
    function  Lookup( const keyFields : String; keyValues : PPyObject; const resultFields : String ) : PPyObject;
    procedure AppendProperties( List : PPyObject ); override;

    // Interface methods
    function DoOpen( args : PPyObject ) : PPyObject; cdecl;
    function DoClose( args : PPyObject ) : PPyObject; cdecl;
    function DoFirst( args : PPyObject ) : PPyObject; cdecl;
    function DoLast( args : PPyObject ) : PPyObject; cdecl;
    function DoNext( args : PPyObject ) : PPyObject; cdecl;
    function DoPrior( args : PPyObject ) : PPyObject; cdecl;
    function DoEdit( args : PPyObject ) : PPyObject; cdecl;
    function DoInsert( args : PPyObject ) : PPyObject; cdecl;
    function DoAppend( args : PPyObject ) : PPyObject; cdecl;
    function DoPost( args : PPyObject ) : PPyObject; cdecl;
    function DoCancel( args : PPyObject ) : PPyObject; cdecl;
    function DoDisableControls( args : PPyObject ) : PPyObject; cdecl;
    function DoEnableControls( args : PPyObject ) : PPyObject; cdecl;
    function DoLocate( args : PPyObject ) : PPyObject; cdecl;
    function DoLookup( args : PPyObject ) : PPyObject; cdecl;
    function DoMoveBy( args : PPyObject ) : PPyObject; cdecl;
    function DoRefresh( args : PPyObject ) : PPyObject; cdecl;
    function DoFields( args : PPyObject ) : PPyObject; cdecl;
    function DoFieldByName( args : PPyObject ) : PPyObject; cdecl;
    function DoFindField( args : PPyObject ) : PPyObject; cdecl;
    function DoFieldNamesAsTuple( args : PPyObject ) : PPyObject; cdecl;
    function DoFieldsAsTuple( args : PPyObject ) : PPyObject; cdecl;
    function DoFieldsAsDict( args : PPyObject ) : PPyObject; cdecl;

    // Events
    ////////////////
    procedure AfterClose( Dataset : TDataset );
    procedure AfterOpen( Dataset : TDataset );
    procedure AfterScroll( Dataset : TDataset );
    procedure BeforeClose( Dataset : TDataset );
    procedure BeforeOpen( Dataset : TDataset );
    procedure BeforeScroll( Dataset : TDataset );
    procedure AfterCancel    ( Dataset : TDataset );
    procedure AfterDelete    ( Dataset : TDataset );
    procedure AfterEdit      ( Dataset : TDataset );
    procedure AfterInsert    ( Dataset : TDataset );
    procedure AfterPost      ( Dataset : TDataset );
    procedure BeforeCancel   ( Dataset : TDataset );
    procedure BeforeDelete   ( Dataset : TDataset );
    procedure BeforeEdit     ( Dataset : TDataset );
    procedure BeforeInsert   ( Dataset : TDataset );
    procedure BeforePost     ( Dataset : TDataset );
    procedure OnCalcFields   ( Dataset : TDataset );
    procedure OnDeleteError  ( Dataset : TDataset; E: EDatabaseError; var Action: TDataAction );
    procedure OnEditError    ( Dataset : TDataset; E: EDatabaseError; var Action: TDataAction );
    procedure OnFilterRecord ( Dataset : TDataset; var Accept : Boolean );
    procedure OnNewRecord    ( Dataset : TDataset );
    procedure OnPostError    ( Dataset : TDataset; E: EDatabaseError; var Action: TDataAction );
  end;

  TPythonDataset = class( TPythonType)
    public
      constructor Create( AOwner : TComponent ); override;
  end;

 {*********************************************************************
 *      Interface of TVarArg                                         *
 *********************************************************************}

   TVarArg = class(TPyObject)
     FValue : PPyObject;

    // Constructors & Destructors
    constructor CreateWith( APythonType : TPythonType; args : PPyObject ); override;
    destructor Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;
    function  Repr : PPyObject; override;
  end;

  TPythonVarArg = class( TPythonType)
    public
      constructor Create( AOwner : TComponent ); override;
  end;

 {*********************************************************************
  *      Global functions                                             *
  *********************************************************************}
  procedure CreateComponents( AOwner : TComponent );
  procedure SetEvent( var EventSlot : PPyObject; Event : PPyObject; const EventName, ClassName : String );
  function  SetDatasetEvent( EventSlot : PPyObject; dsEvent : TDataSetNotifyEvent ) : TDataSetNotifyEvent;
  function  SetDatasetErrorEvent( EventSlot : PPyObject; dsEvent : TDataSetErrorEvent ) : TDataSetErrorEvent;
  procedure ClearEvent( var EventSlot : PPyObject );
  function  ReturnEvent( Event : PPyObject ) : PPyObject;
  procedure ExecuteEvent( Event : PPyObject; Args : array of Const );
  function  NewDataset( aDataset : TDataset; IsOwner : Boolean ) : PPyObject;

 {********************************************************************
 *      Global variables                                             *
 *********************************************************************}
var
  gDatasetType : TPythonType;
  gFieldType : TPythonType;
  gVarArgType : TPythonType;
  gDBModule : TPythonModule;


implementation

{*********************************************************************
 *      DB module                                              *
 *********************************************************************}

constructor TPythonDB.Create( AOwner : TComponent );
begin
  inherited;
  ModuleName := 'DB';
  Name := 'modDB';
  with DocString do
    begin
      Add( 'This module contains several Object Types that' );
      Add( 'will let you work with the Borland BDE and access' );
      Add( 'a database.' );
      Add( '' );
    end;
  with Errors.Add do
    Name := 'DBError';
end;

procedure TPythonDB.Initialize;
begin
  inherited;
  // Values for type TTableType
  SetVarFromVariant( 'ttDefault', 0 );
  SetVarFromVariant( 'ttParadox', 1 );
  SetVarFromVariant( 'ttDBase', 2 );
  SetVarFromVariant( 'ttASCII', 3 );
  // Values for type TDatasetState
  SetVarFromVariant( 'dsInactive', 0 );
  SetVarFromVariant( 'dsBrowse', 1 );
  SetVarFromVariant( 'dsEdit', 2 );
  SetVarFromVariant( 'dsInsert', 3 );
  SetVarFromVariant( 'dsSetKey', 4 );
  SetVarFromVariant( 'dsCalcFields', 5 );
  SetVarFromVariant( 'dsFilter', 6 );
  SetVarFromVariant( 'dsNewValue', 7 );
  SetVarFromVariant( 'dsOldValue', 8 );
  SetVarFromVariant( 'dsCurValue', 9 );
  // Values for type TFieldType
  SetVarFromVariant( 'ftUnknown', 0 );
  SetVarFromVariant( 'ftString', 1 );
  SetVarFromVariant( 'ftSmallint', 2 );
  SetVarFromVariant( 'ftInteger', 3 );
  SetVarFromVariant( 'ftWord', 4 );
  SetVarFromVariant( 'ftBoolean', 5 );
  SetVarFromVariant( 'ftFloat', 6 );
  SetVarFromVariant( 'ftCurrency', 7 );
  SetVarFromVariant( 'ftBCD', 8 );
  SetVarFromVariant( 'ftDate', 9 );
  SetVarFromVariant( 'ftTime', 10 );
  SetVarFromVariant( 'ftDateTime', 11 );
  SetVarFromVariant( 'ftBytes', 12 );
  SetVarFromVariant( 'ftVarBytes', 13 );
  SetVarFromVariant( 'ftAutoInc', 14 );
  SetVarFromVariant( 'ftBlob', 15 );
  SetVarFromVariant( 'ftMemo', 16 );
  SetVarFromVariant( 'ftGraphic', 17 );
  SetVarFromVariant( 'ftFmtMemo', 18 );
  SetVarFromVariant( 'ftParadoxOle', 19 );
  SetVarFromVariant( 'ftDBaseOle', 20 );
  SetVarFromVariant( 'ftTypedBinary', 21 );
  SetVarFromVariant( 'ftCursor', 22 );
  // Values for type TFieldKind
  SetVarFromVariant( 'fkData', 0 );
  SetVarFromVariant( 'fkCalculated', 1 );
  SetVarFromVariant( 'fkLookup', 2 );
  SetVarFromVariant( 'fkInternalCalc', 3 );
  // Values for type TLocateOption
  SetVarFromVariant( 'loCaseInsensitive', 0 );
  SetVarFromVariant( 'loPartialKey', 1 );
  // Values for type TDataAction
  SetVarFromVariant( 'daFail', 0 );
  SetVarFromVariant( 'daAbort', 1 );
  SetVarFromVariant( 'daRetry', 2 );
  // Values for type TUpdateKind
  SetVarFromVariant( 'ukModify', 0 );
  SetVarFromVariant( 'ukInsert', 1 );
  SetVarFromVariant( 'ukDelete', 2 );
end;

{*********************************************************************
 *      Shared object                                                *
 *********************************************************************}

procedure TSharedObject.DoDecRef;
begin
  if FRefCount > 0 then
    Dec(FRefCount);
  if (FRefCount = 0) then
    FreeData;
end;

procedure TSharedObject.SetData( value : TObject );
begin
  DoDecRef;
  FRefCount := 0;
  FData := value;
  IncRef;
end;

constructor TSharedObject.Create;
begin
  inherited;
  Owner := True;
end;

constructor TSharedObject.CreateWith( value : TObject; isOwner : Boolean );
begin
  inherited;
  Data := value;
  Owner := isOwner;
end;

destructor TSharedObject.Destroy;
begin
  FreeData;
  inherited;
end;

procedure TSharedObject.IncRef;
begin
  Inc(FRefCount);
end;

procedure TSharedObject.DecRef;
begin
  if not Assigned(Self) then
    Exit;
  DoDecRef;
  if (FRefCount = 0) then
    Free;
end;

procedure TSharedObject.FreeData;
begin
  if Owner and Assigned(FData) then
    FData.Free;
  FData := nil;
end;

{*********************************************************************
 *      TCommon                                                      *
 *********************************************************************}

function  TCommon.GetProperties : PPyObject;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(0);
      AppendProperties( Result );
      PyList_Sort(Result);
    end;
end;

procedure TCommon.AppendProperties( List : PPyObject );
begin
  // Override this method in the subclasses and add your
  // properties to the list
end;

procedure TCommon.AppendProp( List : PPyObject; const prop : String );
var
  obj : PPyObject;
begin
  with GetPythonEngine do
    begin
      obj := PyString_FromDelphiString(prop);
      PyList_Append( List, obj );
      Py_XDecRef(obj);
    end;
end;

function  TCommon.GetAttr(key : PAnsiChar) : PPyObject;
begin
  try
    if (CompareText( string(key), '__properties__' ) = 0) or
       (CompareText( string(key), '__members__' ) = 0) then
      Result := GetProperties
    else
      Result := inherited GetAttr(key);
  except // Remap Delphi exception to a Python exception
    on E : Exception do
      begin
        RaiseDBError( E );
        Result := nil;
      end;
  end;
end;

procedure TCommon.RaiseDBError( E : Exception );
begin
  if gDBModule <> nil then
    gDBModule.RaiseError( 'DBError', AnsiString(E.Message) );
end;

function TCommon.EventBelongsToObject( Event : TCallbackSplit ) : Boolean;
begin
  Result := Event.Self = Self;
end;

{*********************************************************************
 *      TField                                                       *
 *********************************************************************}

// We override the constructors

constructor TPyField.Create( APythonType : TPythonType );
begin
  inherited;
end;

// Don't call the Create constructor of TPyField, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyDataset.Create will be automatically be called.

constructor TPyField.CreateWith( PythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      //if PyArg_ParseTuple( args, 'ii:CreateTDataset',@x, @y ) = 0 then
      //  exit;
    end;
end;

destructor  TPyField.Destroy;
begin
  if Assigned(FField) then
    begin
      if EventBelongsToObject( TCallbackSplit(FField.OnChange) ) then
        FField.OnChange   := nil;
      if EventBelongsToObject( TCallbackSplit(FField.OnGetText) ) then
        FField.OnGetText  := nil;
      if EventBelongsToObject( TCallbackSplit(FField.OnSetText) ) then
        FField.OnSetText  := nil;
      if EventBelongsToObject( TCallbackSplit(FField.OnValidate) ) then
        FField.OnValidate := nil;
    end;
  FSharedObject.DecRef;
  FSharedObject := nil;
  FField := nil;
  ClearEvent( FOnChange );
  ClearEvent( FOnGetText );
  ClearEvent( FOnSetText );
  ClearEvent( FOnValidate );
  inherited;
end;

// Then we override the needed services

function  TPyField.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if not CheckField then
        begin
          Result := nil;
          Exit;
        end;
      try
        if CompareText( string(key), 'Alignment' ) = 0 then
          Result := VariantAsPyObject( Integer(FField.Alignment) )
        else if CompareText( string(key), 'AsBoolean' ) = 0 then
          Result := VariantAsPyObject( FField.AsBoolean )
        else if CompareText( string(key), 'AsDateTime' ) = 0 then
          Result := VariantAsPyObject( FField.AsDateTime )
        else if CompareText( string(key), 'AsFloat' ) = 0 then
          Result := VariantAsPyObject( FField.AsFloat )
        else if CompareText( string(key), 'AsInteger' ) = 0 then
          Result := VariantAsPyObject( FField.AsInteger )
        else if CompareText( string(key), 'AsString' ) = 0 then
          Result := VariantAsPyObject( FField.AsString )
        else if CompareText( string(key), 'CanModify' ) = 0 then
          Result := VariantAsPyObject( FField.CanModify )
        else if CompareText( string(key), 'ConstraintErrorMessage' ) = 0 then
          Result := VariantAsPyObject( FField.ConstraintErrorMessage )
        else if CompareText( string(key), 'CurValue' ) = 0 then
          Result := VariantAsPyObject( FField.CurValue )
        else if CompareText( string(key), 'CustomConstraint' ) = 0 then
          Result := VariantAsPyObject( FField.CustomConstraint )
        else if CompareText( string(key), 'DataSize' ) = 0 then
          Result := VariantAsPyObject( FField.DataSize )
        else if CompareText( string(key), 'DataType' ) = 0 then
          Result := VariantAsPyObject( FField.DataType )
        else if CompareText( string(key), 'DefaultExpression' ) = 0 then
          Result := VariantAsPyObject( FField.DefaultExpression )
        else if CompareText( string(key), 'DisplayLabel' ) = 0 then
          Result := VariantAsPyObject( FField.DisplayLabel )
        else if CompareText( string(key), 'DisplayName' ) = 0 then
          Result := VariantAsPyObject( FField.DisplayName )
        else if CompareText( string(key), 'DisplayText' ) = 0 then
          Result := VariantAsPyObject( FField.DisplayText )
        else if CompareText( string(key), 'DisplayWidth' ) = 0 then
          Result := VariantAsPyObject( FField.DisplayWidth )
        else if CompareText( string(key), 'EditMask' ) = 0 then
          Result := VariantAsPyObject( FField.EditMask )
        else if CompareText( string(key), 'FieldKind' ) = 0 then
          Result := VariantAsPyObject( FField.FieldKind )
        else if CompareText( string(key), 'FieldName' ) = 0 then
          Result := VariantAsPyObject( FField.FieldName)
        else if CompareText( string(key), 'FieldNo' ) = 0 then
          Result := VariantAsPyObject( FField.FieldNo )
        else if CompareText( string(key), 'HasConstraints' ) = 0 then
          Result := VariantAsPyObject( FField.HasConstraints )
        else if CompareText( string(key), 'ImportedConstraint' ) = 0 then
          Result := VariantAsPyObject( FField.ImportedConstraint )
        else if CompareText( string(key), 'Index' ) = 0 then
          Result := VariantAsPyObject( FField.Index )
        else if CompareText( string(key), 'IsBlob' ) = 0 then
          Result := VariantAsPyObject( FField.IsBlob)
        else if CompareText( string(key), 'IsIndexField' ) = 0 then
          Result := VariantAsPyObject( FField.IsIndexField )
        else if CompareText( string(key), 'IsNull' ) = 0 then
          Result := VariantAsPyObject( FField.IsNull )
        else if CompareText( string(key), 'KeyFields' ) = 0 then
          Result := VariantAsPyObject( FField.KeyFields )
        else if CompareText( string(key), 'Lookup' ) = 0 then
          Result := VariantAsPyObject( FField.Lookup )
        else if CompareText( string(key), 'LookupCache' ) = 0 then
          Result := VariantAsPyObject( FField.LookupCache )
        else if CompareText( string(key), 'LookupDataset' ) = 0 then
          Result := ReturnNone
        else if CompareText( string(key), 'LookupKeyFields' ) = 0 then
          Result := VariantAsPyObject( FField.LookupKeyFields )
        else if CompareText( string(key), 'LookupList' ) = 0 then
          Result := ReturnNone
        else if CompareText( string(key), 'LookupResultField' ) = 0 then
          Result := VariantAsPyObject( FField.LookupResultField )
        else if CompareText( string(key), 'NewValue' ) = 0 then
          Result := VariantAsPyObject( FField.NewValue )
        else if CompareText( string(key), 'Offset' ) = 0 then
          Result := VariantAsPyObject( FField.Offset )
        else if CompareText( string(key), 'OldValue' ) = 0 then
          Result := VariantAsPyObject( FField.OldValue )
        else if CompareText( string(key), 'Origin' ) = 0 then
          Result := VariantAsPyObject( FField.Origin )
        else if CompareText( string(key), 'ReadOnly' ) = 0 then
          Result := VariantAsPyObject( FField.ReadOnly )
        else if CompareText( string(key), 'Required' ) = 0 then
          Result := VariantAsPyObject( FField.Required )
        else if CompareText( string(key), 'Size' ) = 0 then
          Result := VariantAsPyObject( FField.Size )
        else if CompareText( string(key), 'Text' ) = 0 then
          Result := VariantAsPyObject( FField.Text )
        else if CompareText( string(key), 'ValidChars' ) = 0 then
          Result := ReturnNone
        else if CompareText( string(key), 'Value' ) = 0 then
          Result := VariantAsPyObject( FField.Value )
        else if CompareText( string(key), 'Visible' ) = 0 then
          Result := VariantAsPyObject( FField.Visible )
        else if CompareText( string(key), 'OnChange' ) = 0 then
          Result := ReturnEvent( FOnChange )
        else if CompareText( string(key), 'OnGetText' ) = 0 then
          Result := ReturnEvent( FOnGetText )
        else if CompareText( string(key), 'OnSetText' ) = 0 then
          Result := ReturnEvent( FOnSetText )
        else if CompareText( string(key), 'OnValidate' ) = 0 then
          Result := ReturnEvent( FOnValidate )
        else
          Result := inherited GetAttr(key);
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function  TPyField.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not CheckField then
        Exit;
      try
        if CompareText( string(key), 'Alignment' ) = 0 then
          begin
            FField.Alignment := TAlignment(PyObjectAsVariant( value ));
            Result := 0;
          end
        else if CompareText( string(key), 'AsBoolean' ) = 0 then
          begin
            FField.AsBoolean := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'AsDateTime' ) = 0 then
          begin
            FField.AsDateTime := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'AsFloat' ) = 0 then
          begin
            FField.AsFloat := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'AsInteger' ) = 0 then
          begin
            FField.AsInteger := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'AsString' ) = 0 then
          begin
            FField.AsString := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'CanModify' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'ConstraintErrorMessage' ) = 0 then
          begin
            FField.ConstraintErrorMessage := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'CurValue' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'CustomConstraint' ) = 0 then
          begin
            FField.CustomConstraint := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'DataSize' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'DataType' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'DefaultExpression' ) = 0 then
          begin
            FField.DefaultExpression := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'DisplayLabel' ) = 0 then
          begin
            FField.DisplayLabel := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'DisplayName' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'DisplayText' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'DisplayWidth' ) = 0 then
          begin
            FField.DisplayWidth := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'EditMask' ) = 0 then
          begin
            FField.EditMask := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'FieldKind' ) = 0 then
          begin
            FField.FieldKind := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'FieldName' ) = 0 then
          begin
            FField.FieldName:= PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'FieldNo' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'HasConstraints' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'ImportedConstraint' ) = 0 then
          begin
            FField.ImportedConstraint := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Index' ) = 0 then
          begin
            FField.Index := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'IsBlob' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'IsIndexField' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'IsNull' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'KeyFields' ) = 0 then
          begin
            FField.KeyFields := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Lookup' ) = 0 then
          begin
            FField.Lookup := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'LookupCache' ) = 0 then
          begin
            FField.LookupCache := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'LookupDataset' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'LookupKeyFields' ) = 0 then
          begin
            FField.LookupKeyFields := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'LookupList' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'LookupResultField' ) = 0 then
          begin
            FField.LookupResultField := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'NewValue' ) = 0 then
          begin
            FField.NewValue := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Offset' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'OldValue' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'Origin' ) = 0 then
          begin
            FField.Origin := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'ReadOnly' ) = 0 then
          begin
            FField.ReadOnly := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Required' ) = 0 then
          begin
            FField.Required := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Size' ) = 0 then
          begin
            FField.Size := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Text' ) = 0 then
          begin
            FField.Text := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'ValidChars' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'Value' ) = 0 then
          begin
            FField.Value := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Visible' ) = 0 then
          begin
            FField.Visible := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'OnChange' ) = 0 then
          begin
            SetEvent( FOnChange, Value, 'OnChange', 'TField' );
            if Assigned(FOnChange) then
              FField.OnChange := OnChange
            else
              FField.OnChange := nil;
            Result := 0;
          end
        else if CompareText( string(key), 'OnGetText' ) = 0 then
          begin
            SetEvent( FOnGetText, Value, 'OnGetText', 'TField' );
            if Assigned(FOnGetText) then
              FField.OnGetText := OnGetText
            else
              FField.OnGetText := nil;
            Result := 0;
          end
        else if CompareText( string(key), 'OnSetText' ) = 0 then
          begin
            SetEvent( FOnSetText, Value, 'OnSetText', 'TField' );
            if Assigned(FOnSetText) then
              FField.OnSetText := OnSetText
            else
              FField.OnSetText := nil;
            Result := 0;
          end
        else if CompareText( string(key), 'OnValidate' ) = 0 then
          begin
            SetEvent( FOnValidate, Value, 'OnValidate', 'TField' );
            if Assigned(FOnValidate) then
              FField.OnValidate := OnValidate
            else
              FField.OnValidate := nil;
            Result := 0;
          end
        else
          Result := inherited SetAttr(key, value);
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := -1;
          end;
      end;
    end;
end;

function  TPyField.Repr : PPyObject;
begin
  Result := inherited Repr;
end;


// Class methods
// We register the methods of our type

class procedure TPyField.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      AddMethod( 'Clear',  @TPyField.DoClear,  'TField.Clear() -> None' );
      AddMethod( 'FocusControl',  @TPyField.DoFocusControl,  'TField.FocusControl() -> None' );
      AddMethod( 'IsValidChar',  @TPyField.DoIsValidChar,  'TField.IsValidChar( InputChar : String ) -> True or False' );
      AddMethod( 'RefreshLookupList',  @TPyField.DoRefreshLookupList,  'TField.RefreshLookupList() -> None' );
    end;
end;

// Methods of TPyField
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

function TPyField.CheckDataset : Boolean;
begin
  if (FSharedObject = nil) or (FSharedObject.Data = nil) then
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_RuntimeError^, 'No Dataset defined !');
    end
  else
    Result := True;
end;

function TPyField.CheckField : Boolean;
begin
  if not Assigned(FField) then
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_RuntimeError^, 'No field defined !');
    end
  else
    Result := True;
  Result := Result and CheckDataset;
end;

procedure TPyField.AppendProperties( List : PPyObject );
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

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

function TPyField.DoClear( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // Do action
        if CheckField then
          begin
            FField.Clear;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyField.DoFocusControl( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // Do action
        if CheckField then
          begin
            FField.FocusControl;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyField.DoIsValidChar( args : PPyObject ) : PPyObject;
var
  c : Char;
  s : PAnsiChar;
  str : String;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // Do action
        if CheckField then
          begin
            if PyArg_ParseTuple( args, 's:TField.IsValidChar',@s ) <> 0 then
              begin
                str := string(s);
                if Length(str) > 0 then
                  c := str[1]
                else
                  c := #0;
                Result := VariantAsPyObject( FField.IsValidChar( c ) )
              end
            else
              Result := nil;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyField.DoRefreshLookupList( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // Do action
        if CheckField then
          begin
            FField.RefreshLookupList;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

// Events
////////////////
procedure TPyField.OnChange( Sender : TField );
begin
  IncRef;
  ExecuteEvent( FOnChange, [GetSelf] );
end;

procedure TPyField.OnGetText( Sender: TField; var Text: String; DisplayText: Boolean );
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( Text ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnGetText, [GetSelf, v, DisplayText] );
        with PythonToDelphi(v) as TVarArg do
          Text := PyObjectAsVariant(FValue);
      finally
        Py_XDecRef(v);
      end;
    end;
end;

procedure TPyField.OnSetText( Sender : TField; const Text: String );
begin
  IncRef;
  ExecuteEvent( FOnSetText, [GetSelf, Text] );
end;

procedure TPyField.OnValidate( Sender : TField );
begin
  IncRef;
  ExecuteEvent( FOnValidate, [GetSelf] );
end;

constructor TPythonField.Create( AOwner : TComponent );
begin
  inherited;
  Name := 'typeField';
  TypeName := 'TField';
  Module := gDBModule;
  TypeFlags := TypeFlags + [tpfBaseType];
  PyObjectClass := TPyField;
  with DocString do
    begin
      Add( 'The TField type implements the Delphi TField VCL Object inside Python.' );
      Add( 'The properties and methods are the same as in Delphi.' );
    end;
end;

{*********************************************************************
 *      Implementation of TDataset                                   *
 *********************************************************************}

// We override the constructors

constructor TPyDataset.Create( APythonType : TPythonType );
begin
  inherited;
  FSharedObject := TSharedObject.Create;
  FSharedObject.Owner := True;
end;

// Don't call the Create constructor of TPyDataset, because
// we call the inherited constructor CreateWith that calls
// the Create constructor first, and because the constructors
// are virtual, TPyDataset.Create will be automatically be called.

constructor TPyDataset.CreateWith( PythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      //if PyArg_ParseTuple( args, 'ii:CreateTDataset',@x, @y ) = 0 then
      //  exit;
    end;
end;

destructor  TPyDataset.Destroy;
begin
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterClose) ) then
    Dataset.AfterClose      := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterOpen) ) then
    Dataset.AfterOpen       := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterScroll) ) then
    Dataset.AfterScroll     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeClose) ) then
    Dataset.BeforeClose     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeOpen) ) then
    Dataset.BeforeOpen      := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeScroll) ) then
    Dataset.BeforeScroll    := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterCancel) ) then
    Dataset.AfterCancel     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterDelete) ) then
    Dataset.AfterDelete     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterEdit) ) then
    Dataset.AfterEdit       := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterInsert) ) then
    Dataset.AfterInsert     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.AfterPost) ) then
    Dataset.AfterPost       := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeCancel) ) then
    Dataset.BeforeCancel    := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeDelete) ) then
    Dataset.BeforeDelete    := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeEdit) ) then
    Dataset.BeforeEdit      := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforeInsert) ) then
    Dataset.BeforeInsert    := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.BeforePost) ) then
    Dataset.BeforePost      := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.OnCalcFields) ) then
    Dataset.OnCalcFields    := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.OnDeleteError) ) then
    Dataset.OnDeleteError   := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.OnEditError) ) then
    Dataset.OnEditError     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.OnFilterRecord) ) then
    Dataset.OnFilterRecord  := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.OnNewRecord) ) then
    Dataset.OnNewRecord     := nil;
  if EventBelongsToObject( TCallbackSplit(Dataset.OnPostError) ) then
    Dataset.OnPostError     := nil;
  FSharedObject.FreeData;
  FSharedObject.DecRef;
  FSharedObject := nil;
  ClearEvent( FAfterClose      );
  ClearEvent( FAfterOpen       );
  ClearEvent( FAfterScroll     );
  ClearEvent( FBeforeClose     );
  ClearEvent( FBeforeOpen      );
  ClearEvent( FBeforeScroll    );
  ClearEvent( FAfterCancel     );
  ClearEvent( FAfterDelete     );
  ClearEvent( FAfterEdit       );
  ClearEvent( FAfterInsert     );
  ClearEvent( FAfterPost       );
  ClearEvent( FBeforeCancel    );
  ClearEvent( FBeforeDelete    );
  ClearEvent( FBeforeEdit      );
  ClearEvent( FBeforeInsert    );
  ClearEvent( FBeforePost      );
  ClearEvent( FOnCalcFields    );
  ClearEvent( FOnDeleteError   );
  ClearEvent( FOnEditError     );
  ClearEvent( FOnFilterRecord  );
  ClearEvent( FOnNewRecord     );
  ClearEvent( FOnPostError     );
  inherited;
end;

// Then we override the needed services

function  TPyDataset.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if not CheckDataset then
        begin
          Result := nil;
          Exit;
        end;
      try
        if CompareText( string(key), 'Active' ) = 0 then
          Result := VariantAsPyObject( Dataset.Active )
        else if CompareText( string(key), 'BOF' ) = 0 then
          Result := VariantAsPyObject( Dataset.BOF )
        else if CompareText( string(key), 'CanModify' ) = 0 then
          Result := VariantAsPyObject( Dataset.CanModify )
        else if CompareText( string(key), 'EOF' ) = 0 then
          Result := VariantAsPyObject( Dataset.EOF )
        else if CompareText( string(key), 'FieldCount' ) = 0 then
          Result := VariantAsPyObject( Dataset.FieldCount )
        else if CompareText( string(key), 'Filter' ) = 0 then
          Result := VariantAsPyObject( Dataset.Filter )
        else if CompareText( string(key), 'Filtered' ) = 0 then
          Result := VariantAsPyObject( Dataset.Filtered )
        else if CompareText( string(key), 'Found' ) = 0 then
          Result := VariantAsPyObject( Dataset.Found )
        else if CompareText( string(key), 'Modified' ) = 0 then
          Result := VariantAsPyObject( Dataset.Modified )
        else if CompareText( string(key), 'RecNo' ) = 0 then
          Result := VariantAsPyObject( Dataset.RecNo )
        else if CompareText( string(key), 'RecordCount' ) = 0 then
          Result := VariantAsPyObject( Dataset.RecordCount )
        else if CompareText( string(key), 'RecordSize' ) = 0 then
          Result := VariantAsPyObject( Dataset.RecordSize )
        else if CompareText( string(key), 'State' ) = 0 then
          Result := VariantAsPyObject( Integer(Dataset.State) )
        else if CompareText( string(key), 'AfterClose' ) = 0 then
          Result := ReturnEvent( FAfterClose )
        else if CompareText( string(key), 'AfterOpen' ) = 0 then
          Result := ReturnEvent( FAfterOpen )
        else if CompareText( string(key), 'AfterScroll' ) = 0 then
          Result := ReturnEvent( FAfterScroll )
        else if CompareText( string(key), 'BeforeClose' ) = 0 then
          Result := ReturnEvent( FBeforeClose )
        else if CompareText( string(key), 'BeforeOpen' ) = 0 then
          Result := ReturnEvent( FBeforeOpen )
        else if CompareText( string(key), 'BeforeScroll' ) = 0 then
          Result := ReturnEvent( FBeforeScroll )
        else if CompareText( string(key), 'AfterCancel' ) = 0 then
          Result := ReturnEvent( FAfterCancel)
        else if CompareText( string(key), 'AfterDelete' ) = 0 then
          Result := ReturnEvent( FAfterDelete)
        else if CompareText( string(key), 'AfterEdit' ) = 0 then
          Result := ReturnEvent( FAfterEdit)
        else if CompareText( string(key), 'AfterInsert' ) = 0 then
          Result := ReturnEvent( FAfterInsert)
        else if CompareText( string(key), 'AfterPost' ) = 0 then
          Result := ReturnEvent( FAfterPost)
        else if CompareText( string(key), 'BeforeCancel' ) = 0 then
          Result := ReturnEvent( FBeforeCancel)
        else if CompareText( string(key), 'BeforeDelete' ) = 0 then
          Result := ReturnEvent( FBeforeDelete)
        else if CompareText( string(key), 'BeforeEdit' ) = 0 then
          Result := ReturnEvent( FBeforeEdit)
        else if CompareText( string(key), 'BeforeInsert' ) = 0 then
          Result := ReturnEvent( FBeforeInsert)
        else if CompareText( string(key), 'BeforePost' ) = 0 then
          Result := ReturnEvent( FBeforePost)
        else if CompareText( string(key), 'OnCalcFields' ) = 0 then
          Result := ReturnEvent( FOnCalcFields)
        else if CompareText( string(key), 'OnDeleteError' ) = 0 then
          Result := ReturnEvent( FOnDeleteError)
        else if CompareText( string(key), 'OnEditError' ) = 0 then
          Result := ReturnEvent( FOnEditError)
        else if CompareText( string(key), 'OnFilterRecord' ) = 0 then
          Result := ReturnEvent( FOnFilterRecord)
        else if CompareText( string(key), 'OnNewRecord' ) = 0 then
          Result := ReturnEvent( FOnNewRecord)
        else if CompareText( string(key), 'OnPostError' ) = 0 then
          Result := ReturnEvent( FOnPostError)
        else
          Result := inherited GetAttr(key);
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function  TPyDataset.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not CheckDataset then
        Exit;
      try
        if CompareText( string(key), 'Active' ) = 0 then
          begin
            Dataset.Active := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'BOF' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'CanModify' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'EOF' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'FieldCount' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'Filter' ) = 0 then
          begin
            Dataset.Filter := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Filtered' ) = 0 then
          begin
            Dataset.Filtered := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Found' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'Modified' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'RecNo' ) = 0 then
          begin
            Dataset.RecNo := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'RecordCount' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'RecordSize' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'State' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'AfterClose' ) = 0 then
          begin
            SetEvent( FAfterClose, Value, 'AfterClose', 'TDataset' );
            Dataset.AfterClose := SetDatasetEvent( FAfterClose, AfterClose );
            Result := 0;
          end
        else if CompareText( string(key), 'AfterOpen' ) = 0 then
          begin
            SetEvent( FAfterOpen, Value, 'AfterOpen', 'TDataset' );
            Dataset.AfterOpen := SetDatasetEvent( FAfterOpen, AfterOpen );
            Result := 0;
          end
        else if CompareText( string(key), 'AfterScroll' ) = 0 then
          begin
            SetEvent( FAfterScroll, Value, 'AfterScroll', 'TDataset' );
            Dataset.AfterScroll := SetDatasetEvent( FAfterScroll, AfterScroll );
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeClose' ) = 0 then
          begin
            SetEvent( FBeforeClose, Value, 'BeforeClose', 'TDataset' );
            Dataset.BeforeClose := SetDatasetEvent( FBeforeClose, BeforeClose );
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeOpen' ) = 0 then
          begin
            SetEvent( FBeforeOpen, Value, 'BeforeOpen', 'TDataset' );
            Dataset.BeforeOpen := SetDatasetEvent( FBeforeOpen, BeforeOpen );
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeScroll' ) = 0 then
          begin
            SetEvent( FBeforeScroll, Value, 'BeforeScroll', 'TDataset' );
            Dataset.BeforeScroll := SetDatasetEvent( FBeforeScroll, BeforeScroll );
            Result := 0;
          end
        else if CompareText( string(key), 'AfterCancel' ) = 0 then
          begin
            SetEvent( FAfterCancel, Value, 'AfterCancel', 'TDataset' );
            Dataset.AfterCancel := SetDatasetEvent( FAfterCancel, AfterCancel);
            Result := 0;
          end
        else if CompareText( string(key), 'AfterDelete' ) = 0 then
          begin
            SetEvent( FAfterDelete, Value, 'AfterDelete', 'TDataset' );
            Dataset.AfterDelete := SetDatasetEvent( FAfterDelete, AfterDelete);
            Result := 0;
          end
        else if CompareText( string(key), 'AfterEdit' ) = 0 then
          begin
            SetEvent( FAfterEdit, Value, 'AfterEdit', 'TDataset' );
            Dataset.AfterEdit := SetDatasetEvent( FAfterEdit, AfterEdit);
            Result := 0;
          end
        else if CompareText( string(key), 'AfterInsert' ) = 0 then
          begin
            SetEvent( FAfterInsert, Value, 'AfterInsert', 'TDataset' );
            Dataset.AfterInsert := SetDatasetEvent( FAfterInsert, AfterInsert);
            Result := 0;
          end
        else if CompareText( string(key), 'AfterPost' ) = 0 then
          begin
            SetEvent( FAfterPost, Value, 'AfterPost', 'TDataset' );
            Dataset.AfterPost := SetDatasetEvent( FAfterPost, AfterPost);
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeCancel' ) = 0 then
          begin
            SetEvent( FBeforeCancel, Value, 'BeforeCancel', 'TDataset' );
            Dataset.BeforeCancel := SetDatasetEvent( FBeforeCancel, BeforeCancel);
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeDelete' ) = 0 then
          begin
            SetEvent( FBeforeDelete, Value, 'BeforeDelete', 'TDataset' );
            Dataset.BeforeDelete := SetDatasetEvent( FBeforeDelete, BeforeDelete);
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeEdit' ) = 0 then
          begin
            SetEvent( FBeforeEdit, Value, 'BeforeEdit', 'TDataset' );
            Dataset.BeforeEdit := SetDatasetEvent( FBeforeEdit, BeforeEdit);
            Result := 0;
          end
        else if CompareText( string(key), 'BeforeInsert' ) = 0 then
          begin
            SetEvent( FBeforeInsert, Value, 'BeforeInsert', 'TDataset' );
            Dataset.BeforeInsert := SetDatasetEvent( FBeforeInsert, BeforeInsert);
            Result := 0;
          end
        else if CompareText( string(key), 'BeforePost' ) = 0 then
          begin
            SetEvent( FBeforePost, Value, 'BeforePost', 'TDataset' );
            Dataset.BeforePost := SetDatasetEvent( FBeforePost, BeforePost);
            Result := 0;
          end
        else if CompareText( string(key), 'OnCalcFields' ) = 0 then
          begin
            SetEvent( FOnCalcFields, Value, 'OnCalcFields', 'TDataset' );
            Dataset.OnCalcFields := SetDatasetEvent( FOnCalcFields, OnCalcFields);
            Result := 0;
          end
        else if CompareText( string(key), 'OnDeleteError' ) = 0 then
          begin
            SetEvent( FOnDeleteError, Value, 'OnDeleteError', 'TDataset' );
            Dataset.OnDeleteError := SetDatasetErrorEvent( FOnDeleteError, OnDeleteError );
            Result := 0;
          end
        else if CompareText( string(key), 'OnEditError' ) = 0 then
          begin
            SetEvent( FOnEditError, Value, 'OnEditError', 'TDataset' );
            Dataset.OnEditError := SetDatasetErrorEvent( FOnEditError, OnEditError );
            Result := 0;
          end
        else if CompareText( string(key), 'OnFilterRecord' ) = 0 then
          begin
            SetEvent( FOnFilterRecord, Value, 'OnFilterRecord', 'TDataset' );
            if Assigned(FOnFilterRecord) then
              Dataset.OnFilterRecord := OnFilterRecord
            else
              Dataset.OnFilterRecord := nil;
            Result := 0;
          end
        else if CompareText( string(key), 'OnNewRecord' ) = 0 then
          begin
            SetEvent( FOnNewRecord, Value, 'OnNewRecord', 'TDataset' );
            Dataset.OnNewRecord := SetDatasetEvent( FOnNewRecord, OnNewRecord);
            Result := 0;
          end
        else if CompareText( string(key), 'OnPostError' ) = 0 then
          begin
            SetEvent( FOnPostError, Value, 'OnPostError', 'TDataset' );
            Dataset.OnPostError := SetDatasetErrorEvent( FOnPostError, OnPostError );
            Result := 0;
          end
        else
          Result := inherited SetAttr(key, value);
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := -1;
          end;
      end;
    end;
end;

function  TPyDataset.Repr : PPyObject;
begin
  Result := inherited Repr;
end;

// Sequence services

function  TPyDataset.SqLength : NativeInt;
begin
  if CheckDataset then
    Result := Dataset.RecordCount
  else
    Result := -1;
end;

function  TPyDataset.SqItem( idx : NativeInt ) : PPyObject;
begin
  if CheckDataset then
    begin
      // Check if empty
      if Dataset.RecordCount = 0 then
        begin
          Result := nil;
          with GetPythonEngine do
            PyErr_SetString (PyExc_IndexError^, PAnsiChar('Table is empty'));
          Exit;
        end;
      // Check range
      if (idx < 0) or (idx+1 > Dataset.RecordCount) then
        begin
          Result := nil;
          with GetPythonEngine do
            PyErr_SetString (PyExc_IndexError^, PAnsiChar(AnsiString(Format('Index %d out of range (%d,%d)',[idx, 0, Dataset.RecordCount-1]))));
          Exit;
        end;
      try
        Dataset.RecNo := idx+1;
        Result := FieldsAsTuple
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end
  else
    Result := nil;
end;

// Class methods
// We register the methods of our type

class procedure TPyDataset.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      AddMethod( 'Open',  @TPyDataset.DoOpen,  'TDataset.Open() -> None' );
      AddMethod( 'Close', @TPyDataset.DoClose, 'TDataset.Close() -> None' );
      AddMethod( 'First', @TPyDataset.DoFirst, 'TDataset.First() -> None' );
      AddMethod( 'Last',  @TPyDataset.DoLast,  'TDataset.Last() -> None' );
      AddMethod( 'Next',  @TPyDataset.DoNext,  'TDataset.Next() -> None' );
      AddMethod( 'Prior', @TPyDataset.DoPrior, 'TDataset.Prior() -> None' );
      AddMethod( 'Edit',  @TPyDataset.DoEdit,  'TDataset.Edit() -> None' );
      AddMethod( 'Insert', @TPyDataset.DoInsert, 'TDataset.Insert() -> None' );
      AddMethod( 'Append', @TPyDataset.DoAppend, 'TDataset.Append() -> None' );
      AddMethod( 'Post',   @TPyDataset.DoPost,   'TDataset.Post() -> None' );
      AddMethod( 'Cancel', @TPyDataset.DoCancel, 'TDataset.Cancel() -> None' );
      AddMethod( 'DisableControls', @TPyDataset.DoDisableControls,   'TDataset.DisableControls() -> None' );
      AddMethod( 'EnableControls',  @TPyDataset.DoEnableControls,   'TDataset.EnableControls() -> None' );
      AddMethod( 'Locate', @TPyDataset.DoLocate, 'TDataset.Locate( KeyFields : String, KeyValues : Object or Sequence, Options : TLocateOptions ) -> True or False' );
      AddMethod( 'Lookup', @TPyDataset.DoLookup, 'TDataset.Lookup( KeyFields : String, KeyValues : Object or Sequence, ResultFields : String ) -> a list containing all Result fields' );
      AddMethod( 'MoveBy', @TPyDataset.DoMoveBy, 'TDataset.MoveBy( Distance : Integer ) : Integer' );
      AddMethod( 'Refresh',@TPyDataset.DoRefresh,'TDataset.Refresh() -> None' );
      AddMethod( 'Fields', @TPyDataset.DoFields, 'TDataset.Fields( index : Integer ) -> TField' );
      AddMethod( 'FieldByName', @TPyDataset.DoFieldByName, 'TDataset.FieldByName( FieldName : String ) -> TField' );
      AddMethod( 'FindField',   @TPyDataset.DoFindField, 'TDataset.FindField( FieldName : String ) -> TField' );
      AddMethod( 'FieldNamesAsTuple', @TPyDataset.DoFieldNamesAsTuple, 'TDataset.FieldNamesAsTuple() -> a tuple containing all field names' );
      AddMethod( 'FieldsAsTuple', @TPyDataset.DoFieldsAsTuple, 'TDataset.FieldsAsTuple() -> a tuple containing all TFields' );
      AddMethod( 'FieldsAsDict', @TPyDataset.DoFieldsAsDict, 'TDataset.FieldsAsDict() -> a dictionary containing all TFields, with FieldName as Key, and TField as Value' );
    end;
end;

// Methods of TPyDataset
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

function TPyDataset.CheckDataset : Boolean;
begin
  if (FSharedObject = nil) or (FSharedObject.Data = nil) then
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetString (PyExc_RuntimeError^, 'No Dataset defined !');
    end
  else
    Result := True;
end;

function  TPyDataset.Dataset : TDataset;
begin
  Result := FSharedObject.Data as TDataset;
end;

procedure TPyDataset.SetDataset( val : TDataset; owner : Boolean );
begin
  FSharedObject.Data := val;
  FSharedObject.Owner := owner;
end;

function  TPyDataset.FieldNamesAsTuple : PPyObject;
var
  i : Integer;
begin
  with GetPythonEngine do
    begin
      Result := PyTuple_New(Dataset.FieldCount);
      for i := 0 to Dataset.FieldCount - 1 do
        with Dataset.Fields[i] do
          PyTuple_SetItem( Result, i, VariantAsPyObject(Variant(FieldName)) );
    end;
end;

function  TPyDataset.FieldsAsTuple : PPyObject;
var
  i : Integer;
begin
  with GetPythonEngine do
    begin
      Result := PyTuple_New(Dataset.FieldCount);
      for i := 0 to Dataset.FieldCount - 1 do
        with Dataset.Fields[i] do
          PyTuple_SetItem( Result, i, VariantAsPyObject( AsVariant ) );
    end;
end;

function  TPyDataset.FieldsAsDict : PPyObject;
var
  i : Integer;
  obj : PPyObject;
  _fieldName : PPyObject;
begin
  with GetPythonEngine do
    begin
      Result := PyDict_New;
      for i := 0 to Dataset.FieldCount - 1 do
        with Dataset.Fields[i] do
        begin
          obj := VariantAsPyObject( AsVariant );
          _fieldName := VariantAsPyObject(Variant(FieldName));
          PyDict_SetItem( Result, _fieldName, obj );
          Py_XDecRef(obj);
          Py_XDecRef(_fieldName);
        end;
    end;
end;

function  TPyDataset.Locate( const keyFields : String; keyValues, options : PPyObject ) : PPyObject;
var
  rslt : Boolean;
  vvalues : Variant;
  opt : TLocateOptions;
begin
  Result := nil;
  with GetPythonEngine do
    begin
      if PySequence_Check(options) = 0 then
        begin
          PyErr_SetString (PyExc_AttributeError^, 'Third argument of Locate must be a sequence.');
          Exit;
        end;
      // Prepare the locate options
      ListToSet( options, @opt, sizeof(opt) );
      // Create a variant containing the key values
      vvalues := PyObjectAsVariant( keyValues );
      // Execute the locate
      rslt := Dataset.Locate( keyFields, vvalues, opt );
      // Return its result
      Result := VariantAsPyObject( rslt );
    end;
end;

function  TPyDataset.Lookup( const keyFields : String; keyValues : PPyObject; const resultFields : String ) : PPyObject;
var
  rslt : Variant;
  vvalues : Variant;
begin
  with GetPythonEngine do
    begin
      // Create a variant containing the key values
      vvalues := PyObjectAsVariant( keyValues );
      // Execute the lookup
      rslt := Dataset.Lookup( keyFields, vvalues, resultFields );
      // Return its result
      Result := VariantAsPyObject( rslt );
    end;
end;

procedure TPyDataset.AppendProperties( List : PPyObject );
begin
  inherited;
  AppendProp( List, 'Active');
  AppendProp( List, 'BOF');
  AppendProp( List, 'CanModify');
  AppendProp( List, 'EOF');
  AppendProp( List, 'FieldCount');
  AppendProp( List, 'Filter');
  AppendProp( List, 'Filtered');
  AppendProp( List, 'Found');
  AppendProp( List, 'Modified');
  AppendProp( List, 'RecNo');
  AppendProp( List, 'RecordCount');
  AppendProp( List, 'RecordSize');
  AppendProp( List, 'State');
  AppendProp( List, 'AfterClose');
  AppendProp( List, 'AfterOpen');
  AppendProp( List, 'AfterScroll');
  AppendProp( List, 'BeforeClose');
  AppendProp( List, 'BeforeOpen');
  AppendProp( List, 'BeforeScroll');
  AppendProp( List, 'AfterCancel');
  AppendProp( List, 'AfterDelete');
  AppendProp( List, 'AfterEdit');
  AppendProp( List, 'AfterInsert');
  AppendProp( List, 'AfterPost');
  AppendProp( List, 'BeforeCancel');
  AppendProp( List, 'BeforeDelete');
  AppendProp( List, 'BeforeEdit');
  AppendProp( List, 'BeforeInsert');
  AppendProp( List, 'BeforePost');
  AppendProp( List, 'OnCalcFields');
  AppendProp( List, 'OnDeleteError');
  AppendProp( List, 'OnEditError');
  AppendProp( List, 'OnFilterRecord');
  AppendProp( List, 'OnNewRecord');
  AppendProp( List, 'OnPostError');
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

function TPyDataset.DoOpen( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Open;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoClose( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Close;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFirst( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.First;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoLast( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Last;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoNext( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Next;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoPrior( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Prior;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoEdit( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Edit;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoInsert( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Insert;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoAppend( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Append;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoPost( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Post;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoCancel( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Cancel;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoDisableControls( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.DisableControls;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoEnableControls( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.EnableControls;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoLocate( args : PPyObject ) : PPyObject;
var
  keyFields : PAnsiChar;
  keyValues, options : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'sOO:TDataset.Locate',@keyFields, @keyValues, @options ) <> 0) then
          begin
            // Do action
            Result := Locate( String(keyFields), keyValues, options );
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoLookup( args : PPyObject ) : PPyObject;
var
  keyFields, resultFields : PAnsiChar;
  keyValues : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'sOs:TDataset.Lookup',@keyFields, @keyValues, @resultFields ) <> 0) then
          begin
            // Do action
            Result := Lookup( String(keyFields), keyValues, String(resultFields) );
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;
function TPyDataset.DoMoveBy( args : PPyObject ) : PPyObject;
var
  dist, rslt : Integer;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'i:TDataset.MoveBy',@dist ) <> 0) then
          begin
            // Do action
            rslt := Dataset.MoveBy( dist );
            // Finally, we return nothing
            Result := PyInt_FromLong(rslt);
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoRefresh( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Dataset.Refresh;
            // Finally, we return nothing
            Result := ReturnNone;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFields( args : PPyObject ) : PPyObject;
var
  idx : Integer;
  F : TPyField;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // first we extract the arguments
        if CheckDataset and (PyArg_ParseTuple( args, 'i:TDataset.Fields',@idx ) <> 0) then
          begin
            if (idx >= 0) and (idx < Dataset.FieldCount) then
              begin
                Result := gFieldType.CreateInstance;
                F := PythonToDelphi(Result) as TPyField;
                F.FField := Dataset.Fields[idx];
                F.FSharedObject := FSharedObject;
                F.FSharedObject.IncRef;
              end
            else
              begin
                Result := nil;
                PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Value out of range : %d', [idx]))));
              end;
          end
        else // the arguments were not right
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFieldByName( args : PPyObject ) : PPyObject;
var
  s : PAnsiChar;
  fld : TField;
  F : TPyField;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // first we extract the arguments
        if CheckDataset and (PyArg_ParseTuple( args, 's:TDataset.FieldByName',@s ) <> 0) then
          begin
            fld := Dataset.FindField(String(s));
            if Assigned(fld) then
              begin
                Result := gFieldType.CreateInstance;
                F := PythonToDelphi(Result) as TPyField;
                F.FField := fld;
                F.FSharedObject := FSharedObject;
                F.FSharedObject.IncRef;
              end
            else
              begin
                Result := nil;
                PyErr_SetString (PyExc_AttributeError^, PAnsiChar(AnsiString(Format('Unknown field "%s"', [String(s)]))));
              end;
          end
        else // the arguments were not right
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFindField( args : PPyObject ) : PPyObject;
var
  s : PAnsiChar;
  fld : TField;
  F : TPyField;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        // first we extract the arguments
        if CheckDataset and (PyArg_ParseTuple( args, 's:TDataset.FindField',@s ) <> 0) then
          begin
            fld := Dataset.FindField(String(s));
            if Assigned(fld) then
              begin
                Result := gFieldType.CreateInstance;
                F := PythonToDelphi(Result) as TPyField;
                F.FField := fld;
                F.FSharedObject := FSharedObject;
                F.FSharedObject.IncRef;
              end
            else
              begin
                Result := ReturnNone;
              end;
          end
        else // the arguments were not right
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFieldNamesAsTuple( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Result := FieldNamesAsTuple;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFieldsAsTuple( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Result := FieldsAsTuple;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

function TPyDataset.DoFieldsAsDict( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Result := FieldsAsDict;
          end
        else
          Result := nil;
      except // Remap Delphi exception to a Python exception
        on E : Exception do
          begin
            RaiseDBError( E );
            Result := nil;
          end;
      end;
    end;
end;

// Events
////////////////

procedure TPyDataset.AfterClose( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterClose, [GetSelf] );
end;

procedure TPyDataset.AfterOpen( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterOpen, [GetSelf] );
end;

procedure TPyDataset.AfterScroll( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterScroll, [GetSelf] );
end;

procedure TPyDataset.BeforeClose( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeClose, [GetSelf] );
end;

procedure TPyDataset.BeforeOpen( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeOpen, [GetSelf] );
end;

procedure TPyDataset.BeforeScroll( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeScroll, [GetSelf] );
end;

procedure TPyDataset.AfterCancel( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterCancel, [GetSelf] );
end;

procedure TPyDataset.AfterDelete( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterDelete, [GetSelf] );
end;

procedure TPyDataset.AfterEdit( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterEdit, [GetSelf] );
end;

procedure TPyDataset.AfterInsert( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterInsert, [GetSelf] );
end;

procedure TPyDataset.AfterPost( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FAfterPost, [GetSelf] );
end;

procedure TPyDataset.BeforeCancel( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeCancel, [GetSelf] );
end;

procedure TPyDataset.BeforeDelete( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeDelete, [GetSelf] );
end;

procedure TPyDataset.BeforeEdit( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeEdit, [GetSelf] );
end;

procedure TPyDataset.BeforeInsert( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforeInsert, [GetSelf] );
end;

procedure TPyDataset.BeforePost( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FBeforePost, [GetSelf] );
end;

procedure TPyDataset.OnCalcFields( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FOnCalcFields, [GetSelf] );
end;

procedure TPyDataset.OnDeleteError( Dataset : TDataset; E: EDatabaseError; var Action: TDataAction );
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( NativeInt(Action) ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnDeleteError, [GetSelf, ArrayToPyTuple([E.ClassName, E.Message]), v] );
        with PythonToDelphi(v) as TVarArg do
          Action := TDataAction(NativeInt(PyObjectAsVariant(FValue)));
      finally
        Py_XDecRef(v);
      end;
    end;
end;

procedure TPyDataset.OnEditError( Dataset : TDataset; E: EDatabaseError; var Action: TDataAction );
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( NativeInt(Action) ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnEditError, [GetSelf, ArrayToPyTuple([E.ClassName, E.Message]), v] );
        with PythonToDelphi(v) as TVarArg do
          Action := TDataAction(NativeInt(PyObjectAsVariant(FValue)));
      finally
        Py_XDecRef(v);
      end;
    end;
end;

procedure TPyDataset.OnFilterRecord( Dataset : TDataset; var Accept : Boolean );
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( Accept ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnFilterRecord, [GetSelf, v] );
        with PythonToDelphi(v) as TVarArg do
          Accept := PyObjectAsVariant(FValue);
      finally
        Py_XDecRef(v);
      end;
    end;
end;

procedure TPyDataset.OnNewRecord( Dataset : TDataset );
begin
  IncRef;
  ExecuteEvent( FOnNewRecord, [GetSelf] );
end;

procedure TPyDataset.OnPostError( Dataset : TDataset; E: EDatabaseError; var Action: TDataAction );
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( NativeInt(Action) ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnPostError, [GetSelf, ArrayToPyTuple([E.ClassName, E.Message]), v] );
        with PythonToDelphi(v) as TVarArg do
          Action := TDataAction(NativeInt(PyObjectAsVariant(FValue)));
      finally
        Py_XDecRef(v);
      end;
    end;
end;

constructor TPythonDataset.Create( AOwner : TComponent );
begin
  inherited;
  Name := 'typeDataset';
  TypeName := 'TDataset';
  Services.Sequence := [ssLength, ssItem];
  Module := gDBModule;
  TypeFlags := TypeFlags + [tpfBaseType];
  PyObjectClass := TPyDataset;
  with DocString do
    begin
      Add( 'The TDataset type implements the Delphi TDataset VCL Object inside Python.' );
      Add( 'The properties and methods are the same as in Delphi.' );
      Add( 'A TDataset can not be instancied, because it''s a virtual class.' );
      Add( 'It''s used as an interface to any Dataset subclass.');
    end;
end;


{*********************************************************************
 *      Implementation of TVarArg                                    *
 *********************************************************************}

// Constructors & Destructors
constructor TVarArg.CreateWith( APythonType : TPythonType; args : PPyObject );
begin
  inherited;
  with GetPythonEngine do
    begin
      if PyTuple_Check(args) and (PyTuple_Size(args)>=1) then
        begin
          FValue := PyTuple_GetItem( args, 0 );
          Py_XIncRef(FValue);
        end;
    end;
end;

destructor TVarArg.Destroy;
begin
  with GetPythonEngine do
    Py_XDecRef(FValue);
  inherited;
end;


// Basic services
function  TVarArg.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if CompareText( string(key), 'Value' ) = 0 then
        begin
          Result := FValue;
          if not Assigned(Result) then
            Result := Py_None;
          Py_XIncRef(Result);
        end
      else
        Result := inherited GetAttr(key);
    end;
end;

function  TVarArg.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  with GetPythonEngine do
    begin
      if CompareText( string(key), 'Value' ) = 0 then
        begin
          Py_XDecRef(FValue);
          FValue := value;
          Py_XIncRef(FValue);
          Result := 0;
        end
      else
        Result := inherited SetAttr(key, value);
    end;
end;

function  TVarArg.Repr : PPyObject;
begin
  with GetPythonEngine do
    Result := PyString_FromDelphiString( PyObjectAsString(FValue) );
end;

constructor TPythonVarArg.Create( AOwner : TComponent );
begin
  inherited;
  Name := 'typeVarArg';
  TypeName := 'VarArg';
  Module := gDBModule;
  PyObjectClass := TVarArg;
  with DocString do
    begin
      Add( 'a VarArg object contains a single property Value,' );
      Add( 'which you can read and write in order to change' );
      Add( 'an argument in an Event.' );
      Add( 'if you try to print the VarArg, it will print the Value' );
      Add( 'property.' );
    end;
end;

{*********************************************************************
*      Global functions                                             *
*********************************************************************}

procedure CreateComponents( AOwner : TComponent );
begin
  if Assigned(gDBModule) then
    Exit;
  gDBModule := TPythonDB.Create(AOwner);
  gDatasetType := TPythonDataset.Create(AOwner);
  gFieldType := TPythonField.Create(AOwner);
  gVarArgType := TPythonVarArg.Create(AOwner);
end;

procedure SetEvent( var EventSlot : PPyObject; Event : PPyObject; const EventName, ClassName : String );
begin
  with GetPythonEngine do
    begin
      if Assigned(Event) and not (Event = Py_None) then
        begin
          if PyFunction_Check(Event) or PyMethod_Check(Event) then
            begin
              EventSlot := Event;
              Py_IncRef(EventSlot);
            end
          else
            raise Exception.CreateFmt( 'Event %s of class %s needs a function or a method', [EventName, ClassName] );
        end
      else
        begin
          Py_XDecRef(EventSlot);
          EventSlot := nil;
        end;
    end;
end;

function SetDatasetEvent( EventSlot : PPyObject; dsEvent : TDataSetNotifyEvent ) : TDataSetNotifyEvent;
begin
  if Assigned(EventSlot) then
    Result := dsEvent
  else
    Result := nil;
end;

function SetDatasetErrorEvent( EventSlot : PPyObject; dsEvent : TDataSetErrorEvent ) : TDataSetErrorEvent;
begin
  if Assigned(EventSlot) then
    Result := dsEvent
  else
    Result := nil;
end;

procedure ClearEvent( var EventSlot : PPyObject );
begin
  with GetPythonEngine do
    Py_XDecRef( EventSlot );
  EventSlot := nil;
end;

function ReturnEvent( Event : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if Assigned(Event) then
        begin
          Result := Event;
          Py_IncRef(Event);
        end
      else
        Result := ReturnNone;
    end;
end;

procedure ExecuteEvent( Event : PPyObject; Args : array of Const );
var
  L : PPyObject;
  Result : PPyObject;
begin
  Result := nil;
  if not Assigned(Event) then
    Exit;
  with GetPythonEngine do
    begin
      L := ArrayToPyTuple( Args );
      try
        Result := PyEval_CallObjectWithKeywords( Event, L, nil );
      finally
        Py_XDecRef(Result);
        Py_XDecRef(L);
      end;
      CheckError;
    end;
end;

function  NewDataset( aDataset : TDataset; IsOwner : Boolean ) : PPyObject;
begin
  if not Assigned(gDBModule) then
    begin
      CreateComponents(nil);
      gDBModule.Engine := GetPythonEngine;
      gDBModule.Initialize;
      gDatasetType.Engine := GetPythonEngine;
      gDatasetType.Initialize;
      gFieldType.Engine := GetPythonEngine;
      gFieldType.Initialize;
      gVarArgType.Engine := GetPythonEngine;
      gVarArgType.Initialize;
    end;
  Result := gDatasetType.CreateInstance;
  with PythonToDelphi(Result) as TPyDataset do
    begin
      FSharedObject.Owner := IsOwner;
      FSharedObject.Data := aDataset;
    end;
end;

end.
