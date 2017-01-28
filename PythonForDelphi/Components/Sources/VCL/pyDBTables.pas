{$I Definition.Inc}

unit pyDBTables;

{-------------------------------------------------------------------------------
  $Header: /P4D/PythonForDelphi/Components/Sources/VCL/pyDBTables.pas 1     03-04-09 19:25 Morgan $
  Copyright © MMM Inc. 2003 - All Rights Reserved.
  ------------------------------------------------------------------------------
  Author: Morgan Martinet

  Description:

  ------------------------------------------------------------------------------
  $Log: /P4D/PythonForDelphi/Components/Sources/VCL/pyDBTables.pas $
 * 
 * 1     03-04-09 19:25 Morgan
 * initial check-in

-------------------------------------------------------------------------------}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  Variants, PythonEngine, StdCtrls, ExtCtrls, ComCtrls,
  DB, DBTables, pyDB;
type

{*********************************************************************
 *      DBTables module                                              *
 *********************************************************************}
  TPythonDBTables = class(TPythonModule)
    public
      constructor Create( AOwner : TComponent ); override;
      procedure Initialize; override;
  end;

{*********************************************************************
 *      Interface of TBDEDataset                                     *
 *********************************************************************}
   TPyBDEDataset = class(TPyDataset)
     FOnUpdateError : PPyObject;
     FOnUpdateRecord : PPyObject;

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    destructor Destroy; override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Methods of TPyDBDataset
    function BDEDataset : TBDEDataset;
    procedure AppendProperties( List : PPyObject ); override;

    // Interface methods

    // Events
    ////////////////
    procedure OnUpdateError( Dataset : TDataset; E: EDatabaseError; UpdateKind: TUpdateKind; var ua:TUpdateAction);
    procedure OnUpdateRecord( Dataset : TDataset; UpdateKind: TUpdateKind; var ua:TUpdateAction);
  end;

 {*********************************************************************
 *      Interface of TDBDataset                                      *
 *********************************************************************}
   TPyDBDataset = class(TPyBDEDataset)

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Methods of TPyDBDataset
    function DBDataset : TDBDataset;
    procedure AppendProperties( List : PPyObject ); override;

    // Interface methods
  end;

 {*********************************************************************
 *      Interface of TTable                                          *
 *********************************************************************}
   TPyTable = class(TPyDBDataset)

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Methods of TPyTTable
    function  Table : TTable;
    function  SetRange( startValues, endValues : PPyObject ) : PPyObject;
    procedure AddIndex(const Name, Fields: string; Options: PPyObject);
    function  GetIndexNames : PPyObject;
    procedure GotoCurrent( tbl : PPyObject );
    function  FindKey( KeyValues : PPyObject ) : Boolean;
    procedure FindNearest( KeyValues : PPyObject );
    procedure AppendProperties( List : PPyObject ); override;

    // Interface methods
    function DoSetKey( args : PPyObject ) : PPyObject; cdecl;
    function DoSetRangeStart( args : PPyObject ) : PPyObject; cdecl;
    function DoSetRangeEnd( args : PPyObject ) : PPyObject; cdecl;
    function DoSetRange( args : PPyObject ) : PPyObject; cdecl;
    function DoEditKey( args : PPyObject ) : PPyObject; cdecl;
    function DoEditRangeStart( args : PPyObject ) : PPyObject; cdecl;
    function DoEditRangeEnd( args : PPyObject ) : PPyObject; cdecl;
    function DoApplyRange( args : PPyObject ) : PPyObject; cdecl;
    function DoCancelRange( args : PPyObject ) : PPyObject; cdecl;
    function DoLockTable( args : PPyObject ) : PPyObject; cdecl;
    function DoUnlockTable( args : PPyObject ) : PPyObject; cdecl;
    function DoAddIndex( args : PPyObject ) : PPyObject; cdecl;
    function DoCloseIndexFile( args : PPyObject ) : PPyObject; cdecl;
    function DoCreateTable( args : PPyObject ) : PPyObject; cdecl;
    function DoDeleteIndex( args : PPyObject ) : PPyObject; cdecl;
    function DoDeleteTable( args : PPyObject ) : PPyObject; cdecl;
    function DoEmptyTable( args : PPyObject ) : PPyObject; cdecl;
    function DoGetIndexNames( args : PPyObject ) : PPyObject; cdecl;
    function DoOpenIndexFile( args : PPyObject ) : PPyObject; cdecl;
    function DoRenameTable( args : PPyObject ) : PPyObject; cdecl;
    function DoGotoCurrent( args : PPyObject ) : PPyObject; cdecl;
    function DoGotoNearest( args : PPyObject ) : PPyObject; cdecl;
    function DoGotoKey( args : PPyObject ) : PPyObject; cdecl;
    function DoFindKey( args : PPyObject ) : PPyObject; cdecl;
    function DoFindNearest( args : PPyObject ) : PPyObject; cdecl;
  end;

  TPythonTable = class( TPythonType)
    public
      constructor Create( AOwner : TComponent ); override;
  end;

 {*********************************************************************
 *      Interface of TQuery                                          *
 *********************************************************************}

   TPyQuery = class(TPyDBDataset)

    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;

    // Type services
    ////////////////

    // Basic services
    function  GetAttr(key : PAnsiChar) : PPyObject; override;
    function  SetAttr(key : PAnsiChar; value : PPyObject) : Integer; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Methods of TPyTTable
    function Query : TQuery;
    procedure AppendProperties( List : PPyObject ); override;

    // Interface methods
    function DoPrepare( args : PPyObject ) : PPyObject; cdecl;
    function DoUnPrepare( args : PPyObject ) : PPyObject; cdecl;
    function DoExecSQL( args : PPyObject ) : PPyObject; cdecl;
  end;

  TPythonQuery = class( TPythonType)
    public
      constructor Create( AOwner : TComponent ); override;
  end;

 {*********************************************************************
  *      Global functions                                             *
  *********************************************************************}
  procedure CreateComponents( AOwner : TComponent );

 {********************************************************************
 *      Global variables                                             *
 *********************************************************************}
var
  gDBTablesModule : TPythonModule;
  gTableType : TPythonType;
  gQueryType : TPythonType;

implementation

{*********************************************************************
 *      DBTables module                                              *
 *********************************************************************}

constructor TPythonDBTables.Create( AOwner : TComponent );
begin
  inherited;
  ModuleName := 'DBTables';
  Name := 'modDBTables';
  with DocString do
    begin
      Add( 'This module contains several Object Types that' );
      Add( 'will let you work with the Borland BDE and access' );
      Add( 'a database.' );
      Add( '' );
      Add( 'CreateTTable() -> creates a TTable instance' );
      Add( 'CreateTQuery() -> creates a TQuery instance' );
    end;
end;

procedure TPythonDBTables.Initialize;
begin
  inherited;
  // Values for type TLockType
  SetVarFromVariant( 'ltReadLock', 0 );
  SetVarFromVariant( 'ltWriteLock', 1 );
  // Values for type TIndexOptions
  SetVarFromVariant( 'ixPrimary', 0 );
  SetVarFromVariant( 'ixUnique', 1 );
  SetVarFromVariant( 'ixDescending', 2 );
  SetVarFromVariant( 'ixCaseInsensitive', 3 );
  SetVarFromVariant( 'ixExpression', 4 );
  // Values for type TUpdateAction
  SetVarFromVariant( 'uaFail', 0 );
  SetVarFromVariant( 'uaAbort', 1 );
  SetVarFromVariant( 'uaSkip', 2 );
  SetVarFromVariant( 'uaRetry', 3 );
  SetVarFromVariant( 'uaApplied', 4 );
end;

{*********************************************************************
 *      Implementation of TBDEDataset                                *
 *********************************************************************}

// We override the constructors

constructor TPyBDEDataset.Create( APythonType : TPythonType );
begin
  inherited;
end;

destructor TPyBDEDataset.Destroy;
begin
  if EventBelongsToObject( TCallbackSplit(BDEDataset.OnUpdateError) ) then
    BDEDataset.OnUpdateError  := nil;
  if EventBelongsToObject( TCallbackSplit(BDEDataset.OnUpdateRecord) ) then
    BDEDataset.OnUpdateRecord := nil;
  ClearEvent( FOnUpdateError );
  ClearEvent( FOnUpdateRecord );
  inherited;
end;


// Then we override the needed services

function  TPyBDEDataset.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if not CheckDataset then
        begin
          Result := nil;
          Exit;
        end;
      try
        if CompareText( string(key), 'OnUpdateError' ) = 0 then
          Result := ReturnEvent( FOnUpdateError )
        else if CompareText( string(key), 'OnUpdateRecord' ) = 0 then
          Result := ReturnEvent( FOnUpdateRecord )
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

function  TPyBDEDataset.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not CheckDataset then
        Exit;
      try
        if CompareText( string(key), 'OnUpdateError' ) = 0 then
          begin
            SetEvent( FOnUpdateError, Value, 'OnUpdateError', 'TField' );
            if Assigned(FOnUpdateError) then
              BDEDataset.OnUpdateError := OnUpdateError
            else
              BDEDataset.OnUpdateError := nil;
            Result := 0;
          end
        else if CompareText( string(key), 'OnUpdateRecord' ) = 0 then
          begin
            SetEvent( FOnUpdateRecord, Value, 'OnUpdateRecord', 'TField' );
            if Assigned(FOnUpdateRecord) then
              BDEDataset.OnUpdateRecord := OnUpdateRecord
            else
              BDEDataset.OnUpdateRecord := nil;
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


// Class methods
// We register the methods of our type

class procedure TPyBDEDataset.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      //AddMethod( 'First', @TPyDataset.DoFirst, 'TDataset.First' );
    end;
end;

// Methods of TPyBDEDataset
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

function TPyBDEDataset.BDEDataset : TBDEDataset;
begin
  Result := Dataset as TBDEDataset;
end;

procedure TPyBDEDataset.AppendProperties( List : PPyObject );
begin
  inherited;
  //AppendProp( List, '');
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

// Events
////////////////
procedure TPyBDEDataset.OnUpdateError( Dataset : TDataset; E: EDatabaseError; UpdateKind: TUpdateKind; var ua:TUpdateAction);
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( NativeInt(ua) ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnUpdateError, [GetSelf, ArrayToPyTuple([E.ClassName, E.Message]), Integer(UpdateKind), v] );
        with PythonToDelphi(v) as TVarArg do
          ua := TUpdateAction(NativeInt(PyObjectAsVariant(FValue)));
      finally
        Py_XDecRef(v);
      end;
    end;
end;

procedure TPyBDEDataset.OnUpdateRecord( Dataset : TDataset; UpdateKind: TUpdateKind; var ua:TUpdateAction);
var
  v : PPyObject;
begin
  IncRef;
  with GetPythonEngine do
    begin
      v := gVarArgType.CreateInstanceWith( VariantAsPyObject( NativeInt(ua) ) );
      Py_XIncRef(v);
      try
        ExecuteEvent( FOnUpdateError, [GetSelf, Integer(UpdateKind), v] );
        with PythonToDelphi(v) as TVarArg do
          ua := TUpdateAction(NativeInt(PyObjectAsVariant(FValue)));
      finally
        Py_XDecRef(v);
      end;
    end;
end;


{*********************************************************************
 *      Implementation of TDBDataset                                 *
 *********************************************************************}

// We override the constructors

constructor TPyDBDataset.Create( APythonType : TPythonType );
begin
  inherited;
end;



// Then we override the needed services

function  TPyDBDataset.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if not CheckDataset then
        begin
          Result := nil;
          Exit;
        end;
      try
        if CompareText( string(key), 'DatabaseName' ) = 0 then
          Result := VariantAsPyObject( DBDataset.DatabaseName )
        else if CompareText( string(key), 'SessionName' ) = 0 then
          Result := VariantAsPyObject( DBDataset.SessionName )
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

function  TPyDBDataset.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not CheckDataset then
        Exit;
      try
        if CompareText( string(key), 'DatabaseName' ) = 0 then
          begin
            DBDataset.DatabaseName := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'SessionName' ) = 0 then
          begin
            DBDataset.SessionName := PyObjectAsVariant( value );
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


// Class methods
// We register the methods of our type

class procedure TPyDBDataset.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      //AddMethod( 'First', @TPyDataset.DoFirst, 'TDataset.First' );
    end;
end;

// Methods of TPyDBDataset
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

function TPyDBDataset.DBDataset : TDBDataset;
begin
  Result := Dataset as TDBDataset;
end;

procedure TPyDBDataset.AppendProperties( List : PPyObject );
begin
  inherited;
  AppendProp( List, 'DatabaseName');
  AppendProp( List, 'SessionName');
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.


{*********************************************************************
 *      Implementation of TTable                                     *
 *********************************************************************}

// We override the constructors

constructor TPyTable.Create( APythonType : TPythonType );
begin
  inherited;
  FSharedObject.Data := TTable.Create(APythonType);
end;



// Then we override the needed services

function  TPyTable.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if not CheckDataset then
        begin
          Result := nil;
          Exit;
        end;
      try
        if CompareText( string(key), 'IndexName' ) = 0 then
          Result := VariantAsPyObject( Table.IndexName )
        else if CompareText( string(key), 'ReadOnly' ) = 0 then
          Result := VariantAsPyObject( Table.ReadOnly )
        else if CompareText( string(key), 'TableLevel' ) = 0 then
          Result := VariantAsPyObject( Table.TableLevel )
        else if CompareText( string(key), 'TableName' ) = 0 then
          Result := VariantAsPyObject( Table.TableName )
        else if CompareText( string(key), 'TableType' ) = 0 then
          Result := VariantAsPyObject( Integer(Table.TableType) )
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

function  TPyTable.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not CheckDataset then
        Exit;
      try
        if CompareText( string(key), 'IndexName' ) = 0 then
          begin
            Table.IndexName := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'ReadOnly' ) = 0 then
          begin
            Table.ReadOnly := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'TableLevel' ) = 0 then
          begin
            Table.TableLevel := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'TableName' ) = 0 then
          begin
            Table.TableName := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'TableType' ) = 0 then
          begin
            Table.TableType := TTableType(PyObjectAsVariant( value ));
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

// Class methods
// We register the methods of our type

class procedure TPyTable.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      AddMethod( 'SetKey', @TPyTable.DoSetKey, 'TTable.SetKey() -> None' );
      AddMethod( 'SetRangeStart', @TPyTable.DoSetRangeStart, 'TTable.SetRangeStart() -> None' );
      AddMethod( 'SetRangeEnd', @TPyTable.DoSetRangeEnd, 'TTable.SetRangeEnd() -> None' );
      AddMethod( 'SetRange', @TPyTable.DoSetRange, 'TTable.SetRange( sequence of RangeStart values, sequence of RangeEnd values ) -> None' );
      AddMethod( 'EditKey', @TPyTable.DoEditKey, 'TTable.EditKey() -> None' );
      AddMethod( 'EditRangeStart', @TPyTable.DoEditRangeStart, 'TTable.EditRangeStart() -> None' );
      AddMethod( 'EditRangeEnd', @TPyTable.DoEditRangeEnd, 'TTable.EditRangeEnd() -> None' );
      AddMethod( 'ApplyRange', @TPyTable.DoApplyRange, 'TTable.ApplyRange() -> None' );
      AddMethod( 'CancelRange', @TPyTable.DoCancelRange, 'TTable.CancelRange() -> None' );
      AddMethod( 'LockTable', @TPyTable.DoLockTable, 'TTable.LockTable( LockType : TLockType ) -> None' );
      AddMethod( 'UnlockTable', @TPyTable.DoUnlockTable, 'TTable.UnlockTable( LockType : TLockType ) -> None' );
      AddMethod( 'AddIndex', @TPyTable.DoAddIndex, 'TTable.AddIndex( Name : String, Fields : String, Options : TIndexOptions ) -> None' );
      AddMethod( 'CloseIndexFile', @TPyTable.DoCloseIndexFile, 'TTable.CloseIndexFile( IndexFileName : String ) -> None' );
      AddMethod( 'CreateTable', @TPyTable.DoCreateTable, 'TTable.CreateTable() -> None' );
      AddMethod( 'DeleteIndex', @TPyTable.DoDeleteIndex, 'TTable.DeleteIndex( Name : String ) -> None' );
      AddMethod( 'DeleteTable', @TPyTable.DoDeleteTable, 'TTable.DeleteTable() -> None' );
      AddMethod( 'EmptyTable', @TPyTable.DoEmptyTable, 'TTable.EmptyTable() -> None' );
      AddMethod( 'GetIndexNames', @TPyTable.DoGetIndexNames, 'TTable.GetIndexNames() -> list of Index Names' );
      AddMethod( 'OpenIndexFile', @TPyTable.DoOpenIndexFile, 'TTable.OpenIndexFile( IndexFileName : String ) -> None' );
      AddMethod( 'RenameTable', @TPyTable.DoRenameTable, 'TTable.RenameTable( NewName : String ) -> None' );
      AddMethod( 'GotoCurrent', @TPyTable.DoGotoCurrent, 'TTable.GotoCurrent( table : TTable ) -> None' );
      AddMethod( 'GotoNearest', @TPyTable.DoGotoNearest, 'TTable.GotoNearest() -> None' );
      AddMethod( 'GotoKey', @TPyTable.DoGotoKey, 'TTable.GotoKey() -> True or False' );
      AddMethod( 'FindKey', @TPyTable.DoFindKey, 'TTable.FindKey( KeyValues : Sequence ) -> True or False' );
      AddMethod( 'FindNearest', @TPyTable.DoFindNearest, 'TTable.FindNearest( KeyValues : Sequence ) -> None' );
    end;
end;

// Methods of TPyTable
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

function TPyTable.Table : TTable;
begin
  Result := FSharedObject.Data as TTable;
end;

function TPyTable.SetRange( startValues, endValues : PPyObject ) : PPyObject;
var
  i : Integer;
begin
  Result := nil;
  with GetPythonEngine do
    begin
      if PySequence_Check(startValues) = 0 then
        begin
          PyErr_SetString (PyExc_AttributeError^, 'First argument of SetRange must be a sequence.');
          Exit;
        end;
      if PySequence_Check(endValues) = 0 then
        begin
          PyErr_SetString (PyExc_AttributeError^, 'Second argument of SetRange must be a sequence.');
          Exit;
        end;
      with Table do
        begin
          SetRangeStart;
          for i := 0 to PySequence_Length(startValues)-1 do
            begin
              IndexFields[i].Value := PyObjectAsVariant(PySequence_GetItem(startValues, i));
            end;
          SetRangeEnd;
          for i := 0 to PySequence_Length(endValues)-1 do
            begin
              IndexFields[i].Value := PyObjectAsVariant(PySequence_GetItem(endValues, i));
            end;
          ApplyRange;
        end;
      Result := ReturnNone;
    end;
end;

procedure TPyTable.AddIndex(const Name, Fields: string; Options: PPyObject);
var
  opt : TIndexOptions;
begin
  with GetPythonEngine do
    begin
      ListToSet( Options, @opt, sizeof(opt) );
      Table.AddIndex( Name, Fields, opt );
    end;
end;

function TPyTable.GetIndexNames : PPyObject;
var
  L : TStringList;
begin
  L := TStringList.Create;
  try
    Table.GetIndexNames( L );
    Result := GetPythonEngine.StringsToPyList( L );
  finally
    L.Free;
  end;
end;

procedure TPyTable.GotoCurrent( tbl : PPyObject );
var
  obj : TPyObject;
begin
  with GetPythonEngine do
    begin
      if IsDelphiObject( tbl ) then
        begin
          obj := PythonToDelphi(tbl);
          if obj is TPyTable then
            begin
              Table.GotoCurrent( TPyTable(obj).Table );
            end
          else
            raise EPythonError.Create( 'TTable.GotoCurrent: the Delphi Type class is not a TPyTable, or one of its descendants' );
        end
      else
        raise EPythonError.Create( 'TTable.GotoCurrent: need a TTable object as argument' );
    end;
end;

function TPyTable.FindKey( KeyValues : PPyObject ) : Boolean;
var
  V : Variant;
  i : Integer;
begin
  with GetPythonEngine do
    begin
      if PySequence_Check( KeyValues ) <> 0 then
        begin
          V := PyObjectAsVariant( KeyValues );
          Table.CheckBrowseMode;
          Table.SetKey;
          for i := 0 to VarArrayHighBound(V, 1) do
            Table.IndexFields[i].AsVariant := V[i];
          Result := Table.GotoKey;
        end
      else
        Result := False;
    end;
end;

procedure TPyTable.FindNearest( KeyValues : PPyObject );
var
  V : Variant;
  i : Integer;
begin
  with GetPythonEngine do
    begin
      if PySequence_Check( KeyValues ) <> 0  then
        begin
          V := PyObjectAsVariant( KeyValues );
          Table.CheckBrowseMode;
          Table.SetKey;
          for i := 0 to VarArrayHighBound(V, 1) do
            Table.IndexFields[i].AsVariant := V[i];
          Table.GotoNearest;
        end;
    end;
end;

procedure TPyTable.AppendProperties( List : PPyObject );
begin
  inherited;
  AppendProp( List, 'IndexName');
  AppendProp( List, 'ReadOnly');
  AppendProp( List, 'TableLevel');
  AppendProp( List, 'TableName');
  AppendProp( List, 'TableType');
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.
function TPyTable.DoSetKey( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.SetKey;
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

function TPyTable.DoSetRangeStart( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.SetRangeStart;
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

function TPyTable.DoSetRangeEnd( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.SetRangeEnd;
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

function TPyTable.DoSetRange( args : PPyObject ) : PPyObject; cdecl;
var
  startValues, endValues : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'OO:TDataset.SetRange',@startValues, @endValues ) <> 0) then
          begin
            // Do action
            Result := SetRange( startValues, endValues );
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

function TPyTable.DoEditKey( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.EditKey;
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

function TPyTable.DoEditRangeStart( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.EditRangeStart;
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

function TPyTable.DoEditRangeEnd( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.EditRangeEnd;
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

function TPyTable.DoApplyRange( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.ApplyRange;
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

function TPyTable.DoCancelRange( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.CancelRange;
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

function TPyTable.DoLockTable( args : PPyObject ) : PPyObject; cdecl;
var
  t : Integer;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'i:TTable.LockTable',@t ) <> 0) then
          begin
            // Do action
            Table.LockTable(TLockType(t));
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

function TPyTable.DoUnlockTable( args : PPyObject ) : PPyObject; cdecl;
var
  t : Integer;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'i:TTable.LockTable',@t ) <> 0) then
          begin
            // Do action
            Table.UnlockTable(TLockType(t));
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

function TPyTable.DoAddIndex( args : PPyObject ) : PPyObject; cdecl;
var
  aName, fields : PAnsiChar;
  options : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'ssO:TTable.AddIndex',@aName, @fields, @options ) <> 0) then
          begin
            // Do action
            AddIndex( string(aName), string(fields), options );
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

function TPyTable.DoCloseIndexFile( args : PPyObject ) : PPyObject; cdecl;
var
  aName : PAnsiChar;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 's:TTable.CloseIndexFile',@aName ) <> 0) then
          begin
            // Do action
            Table.CloseIndexFile( string(aName) );
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

function TPyTable.DoCreateTable( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.CreateTable;
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

function TPyTable.DoDeleteIndex( args : PPyObject ) : PPyObject; cdecl;
var
  aName : PAnsiChar;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 's:TTable.DeleteIndex',@aName ) <> 0) then
          begin
            // Do action
            Table.DeleteIndex( string(aName) );
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

function TPyTable.DoDeleteTable( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.DeleteTable;
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

function TPyTable.DoEmptyTable( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.EmptyTable;
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

function TPyTable.DoGetIndexNames( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Result := GetIndexNames;
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

function TPyTable.DoOpenIndexFile( args : PPyObject ) : PPyObject; cdecl;
var
  aName : PAnsiChar;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 's:TTable.OpenIndexFile',@aName ) <> 0) then
          begin
            // Do action
            Table.OpenIndexFile( string(aName) );
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

function TPyTable.DoRenameTable( args : PPyObject ) : PPyObject; cdecl;
var
  newName : PAnsiChar;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 's:TTable.RenameTable',@newName ) <> 0) then
          begin
            // Do action
            Table.RenameTable( string(newName) );
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

function TPyTable.DoGotoCurrent( args : PPyObject ) : PPyObject; cdecl;
var
  tbl : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'O:TTable.GotoCurrent',@tbl ) <> 0) then
          begin
            // Do action
            GotoCurrent( tbl );
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

function TPyTable.DoGotoNearest( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Table.GotoNearest;
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

function TPyTable.DoGotoKey( args : PPyObject ) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Result := VariantAsPyObject( Table.GotoKey );
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

function TPyTable.DoFindKey( args : PPyObject ) : PPyObject; cdecl;
var
  KeyValues : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'O:TTable.FindKey',@KeyValues ) <> 0) then
          begin
            // Do action
            Result := VariantAsPyObject( FindKey( KeyValues ) );
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

function TPyTable.DoFindNearest( args : PPyObject ) : PPyObject; cdecl;
var
  KeyValues : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset and (PyArg_ParseTuple( args, 'O:TTable.FindNearest',@KeyValues ) <> 0) then
          begin
            // Do action
            FindNearest(KeyValues);
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

constructor TPythonTable.Create( AOwner : TComponent );
begin
  inherited;
  Name := 'typeTable';
  TypeName := 'TTable';
  Services.Sequence := [ssLength, ssItem];
  Module := gDBTablesModule;
  TypeFlags := TypeFlags + [tpfBaseType];
  PyObjectClass := TPyTable;
  with DocString do
    begin
      Add( 'The TTable type implements the Delphi TTable VCL Object inside Python.' );
      Add( 'The properties and methods are the same as in Delphi.' );
    end;
end;

{*********************************************************************
 *      Implementation of TQuery                                     *
 *********************************************************************}

// We override the constructors

constructor TPyQuery.Create( APythonType : TPythonType );
begin
  inherited;
  FSharedObject.Data := TQuery.Create(APythonType);
end;



// Then we override the needed services

function  TPyQuery.GetAttr(key : PAnsiChar) : PPyObject;
begin
  with GetPythonEngine do
    begin
      if not CheckDataset then
        begin
          Result := nil;
          Exit;
        end;
      try
        if CompareText( string(key), 'Constrained' ) = 0 then
          Result := VariantAsPyObject( Query.Constrained )
        else if CompareText( string(key), 'Local' ) = 0 then
          Result := VariantAsPyObject( Query.Local)
        else if CompareText( string(key), 'ParamCheck' ) = 0 then
          Result := VariantAsPyObject( Query.ParamCheck )
        else if CompareText( string(key), 'ParamCount' ) = 0 then
          Result := VariantAsPyObject( Query.ParamCount )
        else if CompareText( string(key), 'Prepared' ) = 0 then
          Result := VariantAsPyObject( Query.Prepared )
        else if CompareText( string(key), 'RequestLive' ) = 0 then
          Result := VariantAsPyObject( Query.RequestLive )
        else if CompareText( string(key), 'RowsAffected' ) = 0 then
          Result := VariantAsPyObject( Query.RowsAffected )
        else if CompareText( string(key), 'SQL' ) = 0 then
          Result := StringsToPyList( Query.SQL)
        else if CompareText( string(key), 'UniDirectional' ) = 0 then
          Result := VariantAsPyObject( Query.UniDirectional )
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

function  TPyQuery.SetAttr(key : PAnsiChar; value : PPyObject) : Integer;
begin
  Result := -1;
  with GetPythonEngine do
    begin
      if not CheckDataset then
        Exit;
      try
        if CompareText( string(key), 'Constrained' ) = 0 then
          begin
            Query.Constrained := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'Local' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'ParamCheck' ) = 0 then
          begin
            Query.ParamCheck := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'ParamCount' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'Prepared' ) = 0 then
          begin
            Query.Prepared := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'RequestLive' ) = 0 then
          begin
            Query.RequestLive := PyObjectAsVariant( value );
            Result := 0;
          end
        else if CompareText( string(key), 'RowsAffected' ) = 0 then
          begin
            Result := 0;
          end
        else if CompareText( string(key), 'SQL' ) = 0 then
          begin
            PyListToStrings( value, Query.SQL );
            Result := 0;
          end
        else if CompareText( string(key), 'UniDirectional' ) = 0 then
          begin
            Query.UniDirectional := PyObjectAsVariant( value );
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


// Class methods
// We register the methods of our type

class procedure TPyQuery.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  with PythonType do
    begin
      AddMethod( 'Prepare', @TPyQuery.DoPrepare, 'TQuery.Prepare() -> None' );
      AddMethod( 'Unprepare', @TPyQuery.DoUnprepare, 'TQuery.Unprepare() -> None' );
      AddMethod( 'ExecSQL', @TPyQuery.DoExecSQL, 'TQuery.ExecSQL() -> None' );
    end;
end;

// Methods of TPyQuery
// They do the real actions on the object
// It's better to split the functions that interface
// Delphi to Python and the functions that do the
// real implementation.

function TPyQuery.Query : TQuery;
begin
  Result := FSharedObject.Data as TQuery;
end;

procedure TPyQuery.AppendProperties( List : PPyObject );
begin
  inherited;
  AppendProp( List, 'Constrained');
  AppendProp( List, 'Local');
  AppendProp( List, 'ParamCheck');
  AppendProp( List, 'ParamCount');
  AppendProp( List, 'Prepared');
  AppendProp( List, 'RequestLive');
  AppendProp( List, 'RowsAffected');
  AppendProp( List, 'SQL');
  AppendProp( List, 'UniDirectional');
end;

// Interface methods
// They will be called directly by Python, so we extract the
// python arguments and we call the method that will really do
// the action.

function TPyQuery.DoPrepare( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Query.Prepare;
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

function TPyQuery.DoUnPrepare( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Query.UnPrepare;
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

function TPyQuery.DoExecSQL( args : PPyObject ) : PPyObject;
begin
  with GetPythonEngine do
    begin
      // We adjust the transmitted self argument
      Adjust(@Self);
      try
        if CheckDataset then
          begin
            // Do action
            Query.ExecSQL;
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

constructor TPythonQuery.Create( AOwner : TComponent );
begin
  inherited;
  Name := 'typeQuery';
  TypeName := 'TQuery';
  Services.Sequence := [ssLength, ssItem];
  Module := gDBTablesModule;
  PyObjectClass := TPyQuery;
  with DocString do
    begin
      Add( 'The TQuery type implements the Delphi TQuery VCL Object inside Python.' );
      Add( 'The properties and methods are the same as in Delphi.' );
    end;
end;

{*********************************************************************
*      Global functions                                             *
*********************************************************************}

procedure CreateComponents( AOwner : TComponent );
begin
  if Assigned(gDBTablesModule) then
    Exit;
  gDBTablesModule := TPythonDBTables.Create(AOwner);
  gTableType := TPythonTable.Create(AOwner);
  gQueryType := TPythonQuery.Create(AOwner);
end;

end.
